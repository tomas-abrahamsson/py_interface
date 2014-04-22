### py_interface -- A Python-implementation of an Erlang node
###
### $Id$
###
### Copyright (C) 2002  Tomas Abrahamsson
###
### Author: Tomas Abrahamsson <tab@lysator.liu.se>
###
### This file is part of the Py-Interface library
###
### This library is free software; you can redistribute it and/or
### modify it under the terms of the GNU Library General Public
### License as published by the Free Software Foundation; either
### version 2 of the License, or (at your option) any later version.
###
### This library is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### Library General Public License for more details.
###
### You should have received a copy of the GNU Library General Public
### License along with this library; if not, write to the Free
### Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

### erl_node_conn.py -- Handle inter-node communication

import sys
import time
import types
import string
import socket
import random
try:
  from hashlib import md5
except ImportError:
  # Pre-Python2.5 library for md5
  from md5 import new as md5


from py_interface import erl_opts
from py_interface import erl_term
from py_interface import erl_common
from py_interface import erl_async_conn
from py_interface import erl_eventhandler

M = "erl_node_conn"

def CheckDigest(digest, challenge, cookie):
    """Checks that a digest is correct.
    DIGEST      = string
    CHALLENGE   = integer | longinteger
    COOKIE      = string

    Returns: 1 | 0
    Throws:  nothing
    """
    expectedDigest = GenDigest(challenge, cookie)
    return expectedDigest == digest

def GenDigest(challenge, cookie):
    """Generates a digest from a CHALLENGE and a COOKIE.
    CHALLENGE   = integer | longinteger
    COOKIE      = string

    Returns: string
    Throws:  nothing
    """
    challengeStr = str(challenge)
    if challengeStr[-1] == 'L':
        challengeStr = challengeStr[:-1]
    return md5(cookie + challengeStr).digest()

def GenChallenge():
    """Generates a challenge.
    No arguments.
    Returns: integer
    Throws:  nothing
    """
    return int(random.random() * 0x7fffffff)


class Ticker:
    """This class is used for keeping track of the net-ticks:
    * when a remote node has been silent for too long
    * when it's time to send a tick so the other part won't think
      we've been silent for to long (to `tick').
    """

    def __init__(self, netTickTime, timeToTickCb, noResponseCb):
        """Constructor
        NET-TICK-TIME   = integer
                        the net-tick-time (in seconds).
        TIME-TO-TICK-CB = <function(): void>
                        callback to call when it's time to tick.
        NO-RESPONSE-CB  = <function(): void>
                        callback to call when the other end has been
                        silent for too long.

        Throws:  nothing
        """
        self._netTickTime = netTickTime
        self._evhandler = erl_eventhandler.GetEventHandler()
        self._InitStartResponseTimer(netTickTime, noResponseCb)
        self._InitStartTickTimer(netTickTime, timeToTickCb)

    def _InitStartResponseTimer(self, netTickTime, noResponseCb):
        self._noResponseCb = noResponseCb
        self._noResponseTimeout = netTickTime * 1.25
        self._responseCheckTimeout = netTickTime * 0.25
        self._responseDoCheck = 1
        self._timeForLastResponse = time.time()
        self._StartResponseTimer()

    def _StartResponseTimer(self):
        if self._responseDoCheck:
            timeout = self._responseCheckTimeout
            cb = self._CheckResponse
            timerId = self._evhandler.AddTimerEvent(timeout, cb)
            self._checkResponseTimerId = timerId

    def _StopResponseTimer(self):
        self._responseDoCheck = 0

    def GotResonse(self):
        """To be called whenever data has been received from the other end.
        No arguments.
        Returns: void
        Throws:  nothing
        """
        self._timeForLastResponse = time.time()

    def _CheckResponse(self):
        if self._responseDoCheck:
            now = time.time()
            if now > self._timeForLastResponse + self._noResponseTimeout:
                self._responseDoCheck = 0
                self._noResponseCb()
            else:
                self._StartResponseTimer()

    def _InitStartTickTimer(self, netTickTime, timeToTickCb):
        self._timeToTickCb = timeToTickCb
        self._tickTimeout = netTickTime * 0.25
        self._tickCheckTimeout = netTickTime * 0.125
        self._tickDoCheck = 1
        self._timeForLastTick = time.time()
        self._StartTickTimer()

    def _StartTickTimer(self):
        if self._tickDoCheck:
            timeout = self._tickCheckTimeout
            cb = self._Tick
            timerId = self._evhandler.AddTimerEvent(timeout, cb)
            self._tickTimerId = timerId

    def _StopTickTimer(self):
        self._tickDoCheck = 0

    def RestartTick(self):
        """To be called whenever something has been sent to the other end.
        No arguments.
        Returns: void
        Throws:  nothing
        """
        self._timeForLastTick = time.time()

    def _Tick(self):
        if self._tickDoCheck:
            self._StartTickTimer()
            now = time.time()
            if now > self._timeForLastTick + self._tickTimeout:
                self._timeToTickCb()
                self._timeForLastTick = time.time()

    def Stop(self):
        """Stop the timers.
        No arguments.
        Returns: void
        Throws:  nothing
        """
        self._StopResponseTimer()
        self._StopTickTimer()

class ErlNodeOutConnection(erl_async_conn.ErlAsyncClientConnection):
    """This class handles a connection _to_ another node,
    initiated by this node.

    Inheritance: erl_async_conn.ErlAsyncClientConnection

    This is intended to be used by the erl_node.ErlNode class.
    """
    _STATE_DISCONNECTED = -1
    _STATE_HANDSHAKE_RECV_STATUS = 2
    _STATE_HANDSHAKE_RECV_CHALLENGE = 4
    _STATE_HANDSHAKE_RECV_CHALLENGE_ACK = 6
    _STATE_CONNECTED = 7

    def __init__(self, nodeName, opts):
        """Constructor.
        NODE-NAME = string
        OPTS      = <instance of erl_opts.ErlNodeOpts>

        Throws:  nothing
        """
        erl_async_conn.ErlAsyncClientConnection.__init__(self)
        self._recvdata = ""
        self._hostName = None
        self._portNum = None
        self._nodeName = nodeName
        self._opts = opts
        self._peerName = None
        self._peerFlags = 0xffffFFFF
        self._state = self._STATE_DISCONNECTED
        # 2 bytes for the packet length during the handshake, then 4 bytes
        self._packetLenSize = 2
        # These are started once the connection is up
        self._tickTimers = None

    def InitiateConnection(self, hostName, portNum,
                           connectOkCb, connectFailedCb, connectionBrokenCb,
                           passThroughMsgCb):
        """Initiates a connection to another erlang-node.
        HOST-NAME            = string
                             The node to connect to.
        PORT-NUM             = integer
                             The port on that node. Use the EPMD to find
                             the port number, given a node name.
        CONNECT-OK-CB        = <function(): void>
                             To be called when a connection has been
                             successfully established.
        CONNECT-FAILED-CB    = <function(CONNECTION, PEER-NAME): void>
                             CONNECTION = ErlNodeOutConnection
                             PEER-NAME = string (the node name for the peer)
                             To be called when a connection establishment
                             failed.
        CONNECTION-BROKEN-CB = <function(CONNECTION, PEER-NAME): void>
                             CONNECTION = ErlNodeOutConnection
                             PEER-NAME = string (the node name for the peer)
                             To be called when an established connection
                             has been broken.

        Returns: void
        Throws:  <<to-be-documented>>
        """
        self._hostName = hostName
        self._portNum = portNum
        self._connectOkCb = connectOkCb
        self._connectFailedCb = connectFailedCb
        self._connectionBrokenCb = connectionBrokenCb
        self._passThroughMsgCb = passThroughMsgCb
        self._peerName = "(unknown)@%s" % hostName
        if self.Connect(hostName, portNum):
            self._SendName()
            self._state = self._STATE_HANDSHAKE_RECV_STATUS
        else:
            return 0

    def GetPeerNodeName(self):
        """Retrieves the node name for the peer.
        No arguments.
        Returns: string | ""
        Throws:  nothing
        """
        return self._peerName

    def SendMsg(self, ctrlMsg, msg=None):
        """Sends a message to the other end.
        CTRL-MSG   = term
        MSG        (optional) = None | term

        Returns: void
        Throws:  nothing

        For information on the CTRL-MSG and the MSG, please refer to the file
        erl_ext_dist.txt in the Erlang distribution.
        """
        if msg == None:
            packet = "p" + erl_term.TermToBinary(ctrlMsg)
        else:
            packet = "p" + (erl_term.TermToBinary(ctrlMsg) + \
                            erl_term.TermToBinary(msg, self._peerFlags))
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
                if self._tickTimers != None:
                    self._tickTimers.Stop()
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
                erl_common.DebugHex(M, "handshake:recv_cha", data)
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
                erl_common.DebugHex(M, "handshake:recv_cha_ack", data)
                self.Close()
                self._state = self._STATE_DISCONNECTED
                self._connectFailedCb()
            digest = data[1:]
            ownCookie = self._opts.GetCookie()
            if CheckDigest(digest, self._challengeToPeer, ownCookie):
                self._packetLenSize = 4
                self._state = self._STATE_CONNECTED
                t = self._opts.GetNetTickTime()
                self._tickTimers = Ticker(t, self._Tick, self._NoResponse)
                self._connectOkCb()
            else:
                erl_common.Debug(M,
                                 "Connection attempt to disallowed node %s" %
                                 self._peerName)
                self.Close()
                self._state = self._STATE_DISCONNECTED
                self._connectFailedCb()
        elif self._state == self._STATE_CONNECTED:
            self._tickTimers.GotResonse()
            if len(data) == 0:
                # A tick
                return

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
                erl_common.DebugHex(M, "msgType=%c" % msgType, data)
        else:
            erl_common.DebugHex(M, "state=%d" % self._state, data)


    def _Tick(self):
        """This callback is called by the Ticker class instance
        when it is time to send a tick to the other end, to indicate that
        we are still alive.
        """
        self._SendPacket("")

    def _NoResponse(self):
        """This callback is called by the Ticker class instance
        when nothing has been received from the other end for too long.
        """
        erl_common.Debug(M, "InConnection: Connection broken")
        self._state = self._STATE_DISCONNECTED
        self._tickTimers.Stop()
        self._connectionBrokenCb()


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
        self._tickTimers.RestartTick()
        self.Send(msg)


class ErlNodeServerSocket(erl_async_conn.ErlAsyncServer):
    """This class opens a socket and for incoming connections from other
    Erlang nodes. When a remote node connects, an new instance of
    the ErlNodeInConnection is created for handling the new connection.

    This class is indended to be used by the erl_node.ErlNode.
    """
    def __init__(self, nodeName, opts):
        """Constructor
        NODE-NAME = string
                  The name of this node
        OPTS      = <instance of erl_opts.ErlNodeOpts>

        Throws:  nothing
        """
        erl_async_conn.ErlAsyncServer.__init__(self)
        self._nodeName = nodeName
        self._opts = opts
        self._passThroughMsgCb = self._Sink
        self._nodeUpCb = self._Sink
        self._nodeDownCb = self._Sink

    def Start(self, nodeUpCb, nodeDownCb, passThroughMsgCb):
        """Setup and start to listen for incoming connections.

        NODE-UP-CB          = <function(CONNECTION, PEER-NAME): void>
                            Callback to call when a new connection has been
                            established.
        NODE-DOWN-CB        = <function(CONNECTION, PEER-NAME): void>
                            Callback to call when an established connection
                            has been broken.
        PASS-THROUGH-MSG-CB = <function(CONNECTION, PEER-NAME, CTRL-MSG, [MSG])
                                       : void>
                            Callback to call for pass-through messages.
                            Currently, all messages incoming messages are
                            of this type.
        (Sub)Types:

          CONNECTION = <instance of ErlNodeInConnection>
                     The instance of the class that handles the connection
          PEER-NAME  = string
                     The node name for the peer node
          CTRL-MSG   = term
          MSG        = term
                     For information on CTRL-MSG and MSG, see the
                     file erl_ext_dist.txt, which is included in the
                     Erlang distribution.

        Returns: void
        Throws:  <<to-be-documented>>
        """
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
    """This class handles incoming connections from other Erlang nodes.

    This class is indended to be used by the ErlNodeSocketServer
    (and thus indirectly by the erl_node.ErlNode).
    """

    ## XXX TODO: This node duplicates too much functionality
    ##     from ErlNodeOutConnection, still there are differences.
    ##
    ##     The differences are in the setting up of the connection;
    ##     during the handshake sequence, the connecting side
    ##     (ErlNodeOutConnection) acts the client, while the connected
    ##     side (ErlNodeInConnection) acts as server.
    ##
    ##     One idea is to maybe separate the state-machine
    ##     into its own class.
    ##
    ##     Need to think about this one...

    _STATE_DISCONNECTED = -1
    _STATE_HANDSHAKE_RECV_NAME = 1
    _STATE_HANDSHAKE_RECV_STATUS = 3
    _STATE_HANDSHAKE_RECV_CHALLENGE_REPLY = 5
    _STATE_CONNECTED = 7

    def __init__(self, sock, nodeName, opts,
                 newConnectionUpCb, connectionBrokenCb,
                 passThroughMsgCb):
        """Constructor.
        SOCK                 = <socket>
                               The socket for the incoming connection.
        NODE-NAME            = string
                               The node name of the node to which this
                               connection belongs.
        OPTS                 = <instance of erl_opts.ErlNodeOpts>
                               Options for the node
        NEW-CONNECTION-UP-CB = <function(CONNECTION, PEER-NAME): void>
                               Callback to call when a new connection has been
                               established.
        CONNECTION-BROKEN-CB = <function(CONNECTION, PEER-NAME): void>
                               Callback to call when an established connection
                               has been broken.
        PASS-THROUGH-MSG-CB  = <function(CONNECTION, PEER-NAME,
                                         CTRL-MSG, [MSG]): void>
                               Callback to call for pass-through messages.
                               Currently, all messages incoming messages are
                               of this type.
        """
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
        self._peerName = nodeName
        self._peerFlags = 0xffffFFFF
        # 2 bytes for the packet length during the handshake, then 4 bytes
        self._packetLenSize = 2
        # These are started once the connection is up
        self._tickTimers = None

    def GetPeerNodeName(self):
        """Retrieves the node name for the peer.
        No arguments.
        Returns: string | ""
        Throws:  nothing
        """
        return self._peerName

    def SendMsg(self, ctrlMsg, msg=None):
        """Sends a message to the other end.
        CTRL-MSG   = term
        MSG        (optional) = None | term

        Returns: void
        Throws:  nothing

        For information on the CTRL-MSG and the MSG, please refer to the file
        erl_ext_dist.txt in the Erlang distribution.
        """
        if msg == None:
            packet = "p" + erl_term.TermToBinary(ctrlMsg)
        else:
            packet = "p" + (erl_term.TermToBinary(ctrlMsg) + \
                            erl_term.TermToBinary(msg, self._peerFlags))
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
                if self._tickTimers != None:
                    self._tickTimers.Stop()
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
            self._peerName = data[7:]
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
                self._packetLenSize = 4
                self._state = self._STATE_CONNECTED
                t = self._opts.GetNetTickTime()
                self._tickTimers = Ticker(t, self._Tick, self._NoResponse)
                self._newConnectionUpCb(self, self.GetPeerNodeName())
            else:
                erl_common.Debug(M,
                                 "Connection attempt from disallowed node %s" %
                                 self._peerName)
                self.Close()
                self._state = self._STATE_DISCONNECTED
        elif self._state == self._STATE_CONNECTED:
            self._tickTimers.GotResonse()
            if len(data) == 0:
                # A tick
                return

            msgType = data[0]
            if msgType == "p":
                terms = erl_term.BinariesToTerms(data[1:])
                if len(terms) == 2:
                    controlMsg = terms[0]
                    msg = terms[1]
                    peerName = self.GetPeerNodeName()
                    self._passThroughMsgCb(self, peerName, controlMsg, msg)
                elif len(terms) == 1:
                    controlMsg = terms[0]
                    peerName = self.GetPeerNodeName()
                    self._passThroughMsgCb(self, peerName, controlMsg)
                else:
                    debugTxt = "PassThrough-msg: terms=%s" % `terms`
                    erl_common.DebugHex(M, debugTxt, data)
            else:
                erl_common.DebugHex(M, "msgType=%c" % msgType, data)
        else:
            erl_common.DebugHex(M, "state=%d" % self._state, data)


    def _Tick(self):
        """This callback is called by the Ticker class instance
        when it is time to send a tick to the other end, to indicate that
        we are still alive.
        """
        self._SendPacket("")

    def _NoResponse(self):
        """This callback is called by the Ticker class instance
        when nothing has been received from the other end for too long.
        """
        erl_common.Debug(M, "InConnection: Connection broken")
        self._state = self._STATE_DISCONNECTED
        self._tickTimers.Stop()
        self._connectionBrokenCb(self, self.GetPeerNodeName())


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
        self._tickTimers.RestartTick()
        self.Send(msg)
