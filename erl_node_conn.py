import sys
import types
import string
import socket
import random
import md5


import erl_opts
import erl_term
import erl_common
import erl_async_conn
import erl_eventhandler

M = "erl_node_conn"

def CheckDigest(digest, challenge, cookie):
    expectedDigest = GenDigest(challenge, cookie)
    return expectedDigest == digest
        
def GenDigest(challenge, cookie):
    challengeStr = str(challenge)
    if challengeStr[-1] == 'L':
        challengeStr = challengeStr[:-1]
    return md5.new(cookie + challengeStr).digest()

def GenChallenge():
    return int(random.random() * 0x7fffffff)


class ErlNodeOutConnection(erl_async_conn.ErlAsyncClientConnection):
    _STATE_DISCONNECTED = -1
    _STATE_HANDSHAKE_RECV_STATUS = 2
    _STATE_HANDSHAKE_RECV_CHALLENGE = 4
    _STATE_HANDSHAKE_RECV_CHALLENGE_ACK = 6
    _STATE_CONNECTED = 7

    def __init__(self, nodeName, opts):
        erl_async_conn.ErlAsyncClientConnection.__init__(self)
        self._recvdata = ""
        self._hostName = None
        self._portNum = None
        self._nodeName = nodeName
        self._opts = opts
        self._peerName = None
        self._state = self._STATE_DISCONNECTED
        # 2 bytes for the packet length during the handshake, then 4 bytes
        self._packetLenSize = 2         

    def InitiateConnection(self, hostName, portNum,
                           connectOkCb, connectFailedCb, connectionBrokenCb,
                           passThroughMsgCb):
        self._hostName = hostName
        self._portNum = portNum
        self._connectOkCb = connectOkCb
        self._connectFailedCb = connectFailedCb
        self._connectionBrokenCb = connectionBrokenCb
        self._passThroughMsgCb = passThroughMsgCb
        if self.Connect(hostName, portNum):
            self._SendName()
            self._state = self._STATE_HANDSHAKE_RECV_STATUS
        else:
            return 0

    def GetPeerNodeName(self):
        return self._peerName

    def SendMsg(self, ctrlMsg, msg=None):
        if msg == None:
            packet = "p" + erl_term.TermToBinary(ctrlMsg)
        else:
            packet = "p" + (erl_term.TermToBinary(ctrlMsg) + \
                            erl_term.TermToBinary(msg))
        self._SendPacket(packet)


    ##
    ## Internal routines
    ##

    def _In(self):
        """Callback routine, which is called when data is available
        on the connection."""
        connection = self.GetConnection()
        newData = connection.recv(100000)
        if len(newData) == 0:
            self.Close()
            if self._state != self._STATE_CONNECTED:
                self._state = self._STATE_DISCONNECTED
                self._connectFailedCb()
            else:
                self._state = self._STATE_DISCONNECTED
                self._connectionBrokenCb()
            return

        self._recvdata = self._recvdata + newData
        remainingUnhandledData = self._HandleData(self._recvdata)
        self._recvdata = remainingUnhandledData

    def _HandleData(self, data):
        remainingInput = data
        while 1:
            if len(remainingInput) < self._packetLenSize:
                return remainingInput

            if self._packetLenSize == 2:
                packetLen = self.ReadInt2(remainingInput[0:2])
                packetOffset = 2
            else:
                packetLen = self.ReadInt4(remainingInput[0:4])
                packetOffset = 4

            if len(remainingInput) < self._packetLenSize + packetLen:
                return remainingInput

            packetData = remainingInput[packetOffset:packetOffset+packetLen]
            self._HandlePacket(packetData)
            remainingInput = remainingInput[packetOffset+packetLen:]


    def _HandlePacket(self, data):
        if self._state == self._STATE_HANDSHAKE_RECV_STATUS:
            # First check that the correct message came in
            if data[0] != "s":
                erl_common.DebugHex(M, "handshake:recv_status: got", data)
                self.Close()
                self._state = self._STATE_DISCONNECTED
                self._connectFailedCb()
            status = data[1:]
            if status == "ok" or status == "ok_simultaneous":
                self._state = self._STATE_HANDSHAKE_RECV_CHALLENGE
            elif status == "nok" or status == "not_allowed":
                self.Close()
                self._state = self._STATE_DISCONNECTED
                self._connectFailedCb()
            elif status == "alive":
                self._SendStatusAliveTrue()
                self._state = self._STATE_HANDSHAKE_RECV_CHALLENGE
            else:
                erl_common.DebugHex(M, "handshake:recv_status", data)
        elif self._state == self._STATE_HANDSHAKE_RECV_CHALLENGE:
            # First check that the correct message came in
            if data[0] != "n":
                erl_common.DebugUnrecognizedMsg(M, "handshake:recv_cha", data)
                self.Close()
                self._state = self._STATE_DISCONNECTED
                self._connectFailedCb()
            self._peerVersion = self.ReadInt2(data[1:3])
            self._peerFlags = self.ReadInt4(data[3:7])
            challenge = self.ReadInt4(data[7:11])
            self._peerName = data[11:]
            self._SendChallengeReply(challenge)
            self._state = self._STATE_HANDSHAKE_RECV_CHALLENGE_ACK
        elif self._state == self._STATE_HANDSHAKE_RECV_CHALLENGE_ACK:
            # First check that the correct message came in
            if data[0] != "a":
                erl_common.DebugUnrecognizedMsg(M, "handshake:recv_cha_ack",
                                                data)
                self.Close()
                self._state = self._STATE_DISCONNECTED
                self._connectFailedCb()
            digest = data[1:]
            ownCookie = self._opts.GetCookie()
            if CheckDigest(digest, self._challengeToPeer, ownCookie):
                self._packetLenSize = 4
                self._state = self._STATE_CONNECTED
                self._connectOkCb()
            else:
                erl_common.Debug(M,
                                 "Connection attempt to disallowed node %s" %
                                 self._peerName)
                self.Close()
                self._state = self._STATE_DISCONNECTED
                self._connectFailedCb()
        elif self._state == self._STATE_CONNECTED:
            if len(data) == 0:
                # tick. Answer with another tick
                erl_common.Debug(M, "Tick")
                self._SendPacket("")
            else:
                msgType = data[0]
                if msgType == "p":
                    terms = erl_term.BinariesToTerms(data[1:])
                    if len(terms) == 2:
                        controlMsg = terms[0]
                        msg = terms[1]
                        self._passThroughMsgCb(self, self.GetPeerNodeName(),
                                               controlMsg, msg)
                    elif len(terms) == 1:
                        controlMsg = terms[0]
                        self._passThroughMsgCb(self, self.GetPeerNodeName(),
                                               controlMsg, msg)
                    else:
                        debugTxt = "PassThrough-msg: terms=%s" % `terms`
                        erl_common.DebugHex(M, debugTxt, data)
                else:
                    debugTxt = "msgType=%c" % msgType
                    erl_common.DebugHex(M, debugTxt, data)
        else:
            erl_common.DebugUnrecognizedMsg(M, "state=%d" % self._state, data)


    def _SendName(self):
        packet = "n" + \
                 self.PackInt2(self._opts.GetDistrVersion()) + \
                 self.PackInt4(self._opts.GetDistrFlags()) + \
                 self._nodeName
        self._SendHandshakeMsg(packet)

    def _SendStatusAliveTrue(self):
        self._SendHandshakeMsg("true")

    def _SendChallengeReply(self, challenge):
        digest = GenDigest(challenge, self._opts.GetCookie())
        challengeToPeer = GenChallenge()
        self._challengeToPeer = challengeToPeer
        packet = "r" + self.PackInt4(challengeToPeer) + digest
        self._SendHandshakeMsg(packet)

    def _SendHandshakeMsg(self, packet):
        msg = self.PackInt2(len(packet)) + packet
        erl_common.Debug(M, "Sending handshake")
        self.Send(msg)

    def _SendPacket(self, packet):
        msg = self.PackInt4(len(packet)) + packet
        erl_common.Debug(M, "Sending msg")
        self.Send(msg)


class ErlNodeServerSocket(erl_async_conn.ErlAsyncServer):
    def __init__(self, nodeName, opts):
        erl_async_conn.ErlAsyncServer.__init__(self)
        self._nodeName = nodeName
        self._opts = opts
        self._passThroughMsgCb = self._Sink
        self._nodeUpCb = self._Sink
        self._nodeDownCb = self._Sink

    def Start(self, nodeUpCb, nodeDownCb, passThroughMsgCb):
        self._nodeUpCb = nodeUpCb
        self._nodeDownCb = nodeDownCb
        self._passThroughMsgCb = passThroughMsgCb
        return erl_async_conn.ErlAsyncServer.Start(self)

    def _NewConnection(self, s, remoteAddr):
        erl_common.Debug(M, "new connection from %s" % `remoteAddr`)
        inConn = ErlNodeInConnection(s,
                                     self._nodeName, self._opts,
                                     self._nodeUpCb, self._nodeDownCb,
                                     self._passThroughMsgCb)

    def _Sink(self, *a, **kw):
        pass

class ErlNodeInConnection(erl_async_conn.ErlAsyncPeerConnection):
    _STATE_DISCONNECTED = -1
    _STATE_HANDSHAKE_RECV_NAME = 1
    _STATE_HANDSHAKE_RECV_STATUS = 3
    _STATE_HANDSHAKE_RECV_CHALLENGE_REPLY = 5
    _STATE_CONNECTED = 7

    def __init__(self, sock, nodeName, opts,
                 newConnectionUpCb, connectionBrokenCb,
                 passThroughMsgCb):
        erl_async_conn.ErlAsyncPeerConnection.__init__(self, sock)
        self._recvdata = ""
        self._hostName = None
        self._portNum = None
        self._nodeName = nodeName
        self._opts = opts
        self._newConnectionUpCb = newConnectionUpCb
        self._connectionBrokenCb = connectionBrokenCb
        self._passThroughMsgCb = passThroughMsgCb
        self._state = self._STATE_HANDSHAKE_RECV_NAME
        self._peerName = None
        # 2 bytes for the packet length during the handshake, then 4 bytes
        self._packetLenSize = 2         

    def GetPeerNodeName(self):
        return self._peerName

    def SendMsg(self, ctrlMsg, msg=None):
        if msg == None:
            packet = "p" + erl_term.TermToBinary(ctrlMsg)
        else:
            packet = "p" + (erl_term.TermToBinary(ctrlMsg) + \
                            erl_term.TermToBinary(msg))
        self._SendPacket(packet)


    ##
    ## Internal routines
    ##

    def _In(self):
        """Callback routine, which is called when data is available
        on the connection."""
        connection = self.GetConnection()
        newData = connection.recv(100000)
        if len(newData) == 0:
            self.Close()
            if self._state != self._STATE_CONNECTED:
                self._state = self._STATE_DISCONNECTED
            else:
                erl_common.Debug(M, "InConnection: Connection broken")
                self._state = self._STATE_DISCONNECTED
                self._connectionBrokenCb(self, self.GetPeerNodeName())
            return

        self._recvdata = self._recvdata + newData
        remainingUnhandledData = self._HandleData(self._recvdata)
        self._recvdata = remainingUnhandledData

    def _HandleData(self, data):
        remainingInput = data
        while 1:
            if len(remainingInput) < self._packetLenSize:
                return remainingInput

            if self._packetLenSize == 2:
                packetLen = self.ReadInt2(remainingInput[0:2])
                packetOffset = 2
            else:
                packetLen = self.ReadInt4(remainingInput[0:4])
                packetOffset = 4

            if len(remainingInput) < self._packetLenSize + packetLen:
                return remainingInput

            packetData = remainingInput[packetOffset:packetOffset+packetLen]
            self._HandlePacket(packetData)
            remainingInput = remainingInput[packetOffset+packetLen:]

    def _HandlePacket(self, data):
        if self._state == self._STATE_HANDSHAKE_RECV_NAME:
            # First check that the correct message came in
            if data[0] != "n":
                erl_common.DebugHex(M, "handshake:recv_name", data)
                self.Close()
                self._state = self._STATE_DISCONNECTED
            self._peerDistrVersion = self.ReadInt2(data[1:3])
            self._peerFlags = self.ReadInt4(data[3:7])
            self._peerName = self.ReadInt4(data[7:])
            # FIXME: check for connections _to_ this node:
            #        check whether nodeName > ownNodeName (or check < ?)
            self._SendStatusOk()
            self._SendChallenge()
            self._state = self._STATE_HANDSHAKE_RECV_CHALLENGE_REPLY
        elif self._state == self._STATE_HANDSHAKE_RECV_CHALLENGE_REPLY:
            # First check that the correct message came in
            if data[0] != "r":
                erl_common.DebugHex(M, "handshake:recv_chreply", data)
                self.Close()
                self._state = self._STATE_DISCONNECTED
            peersChallenge = self.ReadInt4(data[1:5])
            peersDigest = data[5:]
            ownCookie = self._opts.GetCookie()
            if CheckDigest(peersDigest, self._challengeToPeer, ownCookie):
                self._SendChallengeAck(peersChallenge)
                self._packetLenSIze = 4
                self._state = self._STATE_CONNECTED
                self._newConnectionUpCb(self, self.GetPeerNodeName())
            else:
                erl_common.Debug(M,
                                 "Connection attempt from disallowed node %s" %
                                 self._peerName)
                self.Close()
                self._state = self._STATE_DISCONNECTED
        elif self._state == self._STATE_CONNECTED:
            if len(data) == 0:
                # tick. Answer with another tick
                erl_common.Debug(M, "Tick")
                self._SendPacket("")
            else:
                msgType = data[0]
                if msgType == "p":
                    terms = erl_term.BinariesToTerms(data[1:])
                    if len(terms) == 2:
                        controlMsg = terms[0]
                        msg = terms[1]
                        self._passThroughMsgCb(self, self.GetPeerNodeName(),
                                               controlMsg, msg)
                    elif len(terms) == 1:
                        controlMsg = terms[0]
                        self._passThroughMsgCb(self, self.GetPeerNodeName(),
                                               controlMsg)
                    else:
                        debugTxt = "PassThrough-msg: terms=%s" % `terms`
                        erl_common.DebugHex(M, debugTxt, data)
                else:
                    debugTxt = "msgType=%c" % msgType
                    erl_common.DebugHex(M, debugTxt, data)
        else:
            erl_common.DebugHex(M, "state=%d" % self._state, data)
            

    def _SendStatusOk(self):
        self._SendHandshakeMsg("sok")

    def _SendChallenge(self):
        challenge = GenChallenge()
        self._challengeToPeer = challenge
        packet = "n" + \
                 self.PackInt2(self._opts.GetDistrVersion()) + \
                 self.PackInt4(self._opts.GetDistrFlags()) + \
                 self.PackInt4(challenge) + \
                 self._nodeName
        self._SendHandshakeMsg(packet)

    def _SendChallengeAck(self, challenge):
        packet = "a" + GenDigest(challenge, self._opts.GetCookie())
        self._SendHandshakeMsg(packet)

    def _SendHandshakeMsg(self, packet):
        msg = self.PackInt2(len(packet)) + packet
        erl_common.Debug(M, "Sending handshake")
        self.Send(msg)

    def _SendPacket(self, packet):
        msg = self.PackInt4(len(packet)) + packet
        erl_common.Debug(M, "Sending msg")
        self.Send(msg)
