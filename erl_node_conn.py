import sys
import types
import string
import socket
import random
import getopt
import md5


import erl_common
import eventhandler
import erl_async_conn

def NodeNameMaybeAddDomain(nodeName):
    if "@" in nodeName:
        return nodeName
    domainName = erl_common.GetHostName()
    return nodeName + "@" + domainName

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

    def __init__(self, nodeName, cookie, distrVersion, flags):
        erl_async_conn.ErlAsyncClientConnection.__init__(self)
        self._recvdata = ""
        self._hostName = None
        self._portNum = None
        self._nodeName = nodeName
        self._cookie = cookie
        self._distrVersion = distrVersion
        self._flags = flags
        self._state = self._STATE_DISCONNECTED
        # 2 bytes for the packet length during the handshake, then 4 bytes
        self._packetLenSize = 2         

    def InitiateConnection(self, hostName, portNum,
                           connectOkCb, connectFailedCb, connectBrokenCb):
        self._hostName = hostName
        self._portNum = portNum
        self._connectOkCb = connectOkCb
        self._connectFailedCb = connectFailedCb
        self._connectBrokenCb = connectBrokenCb
        if self.Connect(hostName, portNum):
            self._SendName()
            self._state = self._STATE_HANDSHAKE_RECV_STATUS
        else:
            return 0

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
                self._connectBrokenCb()
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
        erl_common.Debug("Incoming message:")
        erl_common.HexDump(data)

        if self._state == self._STATE_HANDSHAKE_RECV_STATUS:
            # First check that the correct message came in
            if data[0] != "s":
                erl_common.DebugUnrecognizedMsg("handshake recv_status", data)
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
                erl_common.DebugUnrecognizedMsg("handshake recv_status", data)
        elif self._state == self._STATE_HANDSHAKE_RECV_CHALLENGE:
            # First check that the correct message came in
            if data[0] != "n":
                erl_common.DebugUnrecognizedMsg("handshake recv_cha", data)
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
                erl_common.DebugUnrecognizedMsg("handshake recv_cha_ack", data)
                self.Close()
                self._state = self._STATE_DISCONNECTED
                self._connectFailedCb()
            digest = data[1:]
            if CheckDigest(digest, self._challengeToPeer, self._cookie):
                self._packetLenSize = 4
                self._state = self._STATE_CONNECTED
            else:
                erl_common.Debug("Connection attempt to disallowed node %s" %
                                 self._peerName)
                self.Close()
                self._state = self._STATE_DISCONNECTED
                self._connectFailedCb()
        elif self._state == self._STATE_CONNECTED:
            if len(data) == 0:
                # tick. Answer with another tick
                erl_common.Debug("Tick")
                self._SendMsg("")
            else:
                erl_common.DebugUnrecognizedMsg("connected", data)
        else:
            erl_common.DebugUnrecognizedMsg("state=%d" % self._state, data)


    def _SendName(self):
        packet = "n" + \
                 self.PackInt2(self._distrVersion) + \
                 self.PackInt4(self._flags) + \
                 self._nodeName
        self._SendHandshakeMsg(packet)

    def _SendStatusAliveTrue(self):
        self._SendHandshakeMsg("true")

    def _SendChallengeReply(self, challenge):
        digest = GenDigest(challenge, self._cookie)
        print "challenge = %s cookie = %s ==> digest = %s" % \
              (`challenge`, self._cookie, `map(lambda x: ord(x), digest)`)
        challengeToPeer = GenChallenge()
        self._challengeToPeer = challengeToPeer
        packet = "r" + self.PackInt4(challengeToPeer) + digest
        self._SendHandshakeMsg(packet)

    def _SendHandshakeMsg(self, packet):
        msg = self.PackInt2(len(packet)) + packet
        erl_common.Debug("Sending handshake:")
        erl_common.HexDump(msg)
        self.Send(msg)

    def _SendMsg(self, packet):
        msg = self.PackInt4(len(packet)) + packet
        erl_common.Debug("Sending msg:")
        erl_common.HexDump(msg)
        self.Send(msg)


class ErlNode:
    def __init__(self, nodeName, cookie, distrVersion, flags):
        self._nodeName = NodeNameMaybeAddDomain(nodeName)
        self._cookie = cookie
        self._distrVersion = distrVersion
        self._flags = flags
        self._server = ServerSocket(nodeName, cookie, distrVersion, flags)
        self._portNum = self.serverSocket.Start()
        self._isServerPublished = 0

    def CreateMBox(self, msgCallback=None):
        pass

    def Ping(self, remoteNodeName):
        pass

    def Publish(self):
        pass

    def Unpublish(self):
        pass


class ErlMBox:
    def __init__(self):
        pass

    def Send(self, dest, msg):
        pass

    def Link(self, otherEnd):
        pass

    def Unlink(self, otherEnd):
        pass


class ServerSocket(erl_async_conn.ErlAsyncServer):
    def __init__(self, nodeName, cookie, distrVersion, flags):
        erl_async_conn.ErlAsyncServer.__init__(self)
        self._nodeName = nodeName
        self._cookie = cookie
        self._distrVersion = distrVersion
        self._flags = flags

    def _NewConnection(self, s, remoteAddr):
        inConn = ErlNodeInConnection(s,
                                     self._nodeName, self._cookie,
                                     self._distrVersion, self._flags)

class ErlNodeInConnection(erl_async_conn.ErlAsyncPeerConnection):
    _STATE_DISCONNECTED = -1
    _STATE_HANDSHAKE_RECV_NAME = 1
    _STATE_HANDSHAKE_RECV_STATUS = 3
    _STATE_HANDSHAKE_RECV_CHALLENGE_REPLY = 5
    _STATE_CONNECTED = 7

    def __init__(self, sock, nodeName, cookie, distrVersion, flags):
        erl_async_conn.ErlAsyncPeerConnection.__init__(self, sock)
        self._recvdata = ""
        self._hostName = None
        self._portNum = None
        self._nodeName = nodeName
        self._cookie = cookie
        self._distrVersion = distrVersion
        self._flags = flags
        self._state = self._STATE_DISCONNECTED
        # 2 bytes for the packet length during the handshake, then 4 bytes
        self._packetLenSize = 2         

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
                self._connectBrokenCb()
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
        erl_common.Debug("Incoming message:")
        erl_common.HexDump(data)

        if self._state == self._STATE_HANDSHAKE_RECV_NAME:
            # First check that the correct message came in
            if data[0] != "n":
                erl_common.DebugUnrecognizedMsg("handshake recv_name", data)
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
                erl_common.DebugUnrecognizedMsg("handshake recv_chreply", data)
                self.Close()
                self._state = self._STATE_DISCONNECTED
            peersChallenge = self.ReadInt4(data[1:5])
            peersDigest = data[5:]
            if CheckDigest(peersDigest, self._challengeToPeer, self._cookie):
                self._SendChallengeAck(peersChallenge)
                self._packetLenSIze = 4
                self._state = self._STATE_CONNECTED
            else:
                erl_common.Debug("Connection attempt from disallowed node %s" %
                                 self._peerName)
                self.Close()
                self._state = self._STATE_DISCONNECTED
        elif self._state == self._STATE_CONNECTED:
            if len(data) == 0:
                # tick. Answer with another tick
                erl_common.Debug("Tick")
                self._SendMsg("")
            else:
                erl_common.DebugUnrecognizedMsg("connected", data)
        else:
            erl_common.DebugUnrecognizedMsg("state=%d" % self._state, data)
            

    def _SendStatusOk(self):
        self._SendHandshakeMsg("ok")

    def _SendChallenge(self):
        challenge = GenChallenge()
        self._challengeToPeer = challenge
        packet = "n" + \
                 self.PackInt2(self._distrVersion) + \
                 self.PackInt4(self._flags) + \
                 self.PackInt4(challenge) + \
                 self._nodeName
        self._SendHandshakeMsg(packet)

    def _SendChallengeAck(self, challenge):
        packet = "a" + GenDigest(challenge, self._cookie)
        self._SendHandshakeMsg(packet)

    def _SendHandshakeMsg(self, packet):
        msg = self.PackInt2(len(packet)) + packet
        erl_common.Debug("Sending handshake:")
        erl_common.HexDump(msg)
        self.Send(msg)

    def _SendMsg(self, packet):
        msg = self.PackInt4(len(packet)) + packet
        erl_common.Debug("Sending msg:")
        erl_common.HexDump(msg)
        self.Send(msg)



def __TestConnectOk():
    print "ConnectOk"

def __TestConnectFailed():
    print "ConnectFailed"

def __TestConnectBroken():
    print "ConnectBroken"

def main(argv):
    global e

    try:
        opts, args = getopt.getopt(argv[1:], "?n:c:d:f:")
    except getopt.error, info:
        print info

    hostName = "localhost"
    ownNodeName = "py_interface_test"
    cookie = "cookie"
    ownDistrVersion = 4
    ownFlags = 4

    for (optchar, optarg) in opts:
        if optchar == "-?":
            print "Usage: %s host [port]" % argv[0]
            sys.exit(1)
        elif optchar == "-c":
            cookie = optarg
        elif optchar == "-n":
            ownNodeName = optarg
        elif optchar == "-d":
            ownDistrVersion = string.atoi(optarg)
        elif optchar == "-f":
            ownFlags = string.atoi(optarg)

    if len(args) >= 2:
        hostName = args[0]
        portNum = string.atoi(args[1])
    elif len(args) >= 1:
        portNum = string.atoi(args[0])
    else:
        sys.exit(1)

    ownNodeName = NodeNameMaybeAddDomain(ownNodeName)

    print "Connecting to %s:%d"
    print "  ownNodeName=\"%s\"" % ownNodeName
    print "  cookie=\"%s\"" % cookie
    print "  ownDistrVersion=%d" % ownDistrVersion
    print "  ownFlags=%d" % ownFlags

    c = ErlNodeOutConnection(ownNodeName, cookie, ownDistrVersion, ownFlags)
    c.InitiateConnection(hostName, portNum,
                         __TestConnectOk,
                         __TestConnectFailed,
                         __TestConnectBroken)
    evhandler = eventhandler.GetEventHandler()
    evhandler.Loop()


if __name__ == '__main__':
    main(sys.argv)

