import sys
import types
import string
import socket
import random
import getopt


import otp_epmd
import erl_term
import erl_common
import eventhandler
import erl_node_conn


DISTR_FLAGS_PUBLISHED = 1;
DISTR_FLAGS_ATOMCACHE = 2;
DISTR_FLAGS_EXTENDEDREFERENCES = 4;
DISTR_FLAGS_DISTMONITOR = 8;
DISTR_FLAGS_FUNTAGS = 16;


_pidCount = 1
_serial = 0

class ErlMBox:
    def __init__(self, node, pid, msgCallback):
        self._node = node
        self._msgCallback = msgCallback
        self._pid = pid

    def Self(self):
        return self._pid

    def Send(self, dest, msg):
        pass

    def Link(self, otherEnd):
        pass

    def Unlink(self, otherEnd):
        pass


class ErlNode:
    def __init__(self, nodeName, cookie,
                 distrVersionLo = 5,
                 distrVersionHi = 5,
                 distrFlags = DISTR_FLAGS_EXTENDEDREFERENCES):
        self._nodeName = erl_common.NodeNameMaybeAddHostName(nodeName)
        self._cookie = cookie
        self._distrVersion = distrVersion
        self._flags = flags
        self._connections = {}
        self._server = erl_node_conn.ServerSocket(nodeName, cookie,
                                                  distrVersion, flags)
        self._portNum = self.server.Start(self._NodeUp, self._NodeDown)
        self._isServerPublished = 0
        self._mboxes = {}
        self._epmd = otp_epmd.OtpEpmd()
        self.Publish()
        self._ongoingPings = {}

        
    def CreateMBox(self, msgCallback=None):
        mboxPid = self._CreatePid()
        mbox = ErlMBox(self, mboxPid, msgCallback)
        self._mboxes[mboxPid] = mbox
        return mboxPid

    def Ping(self, remoteNodeName, pingCallback):
        if not "@" in remoteNodeName:
            raise "Bad node name for remote node"
        if self._ongoingPings.has_key(remoteNodeName):
            pingCallbacks = self._ongoingPings[remoteNodeName]
            self._ongoingPings[remoteNodeName] = pingCallbacks + [pingCallback]
        else:
            self._ongoingPings[remoteNodeName] = [pingCallback]
            [nodeName, hostName] = string.split(remoteNodeName, "@")
            e = epmd(hostName)
            cb = common.VCallback(self._PingEpmdResonse, remoteNodeName)
            e.PortPlease2Req(nodeName, cb)

    def Publish(self):
        if not self._isServerPublished:
            self._epmd.Connect(self._EpmdConnectedOk, self._EpmdConnectFailed)

    def Unpublish(self):
        if self._isServerPublished:
            self._epmd.Close()
            self._isServerPublished = 0


    ##
    ## Internal routines
    ##

    def _CreatePid(self):
        ## Code stolen from com/ericsson/otp/erlang/OtpLocalNode.java
        global _serial, _pidCount
        newPid = erl_term.OtpPid(erl_term.OtpAtom(self._nodeName),
                                 _pidCount, _serial, self._creation)
        _pidCount = _pidCount + 1
        if _pidCount > 0x7fff:
            _pidCount = 0
            _serial = _serial + 1
            if _serial > 0x07:
                _serial = 0
        return newPid

    def _EpmdConnectedOk(self, creation):
        self._isServerPublished = 1
        self._creation = creation

    def _EpmdConnectFailed(self, errorResult):
        raise "Failed to connect to epmd (%d)" % errorResult

    def _NodeUp(self, connection, nodeName):
        self._connections[nodeName] = connection

    def _NodeDown(self, connection, nodeName):
        if self._connections.has_key(nodeName):
            del self._connections[nodeName]
    
    def _PingEpmdResponse(self, result, portNum, nodeType, proto,
                          distVSNRange, nodeNameNoHost, extra,
                          remoteNodeName):
        if result != 0:
            callbacks = self._ongoingPings[remoteNodeName]
            del self._ongoingPings[remoteNodeName]
            for cb in callbacks:
                cb("pang")
        else:
            [otherNode, otherHost] = string.split(remoteNodeName, "@")
            out = erl_node_conn.ErlNodeOutConnection(self._nodeName,
                                                     self._cookie,
                                                     self._distrVersionHi,
                                                     self._flags)
            connectedCb = common.VCallback(self._PingSucceeded,
                                           out, remoteNodeName)
            connectFailedCb = common.VCallback(self._PingFailed,
                                               out, remoteNodeName)
            connectionBrokenCb = common.VCallback(self._OutConnectionBroken,
                                                  out, remoteNodeName)
            out.InitiateConnection(otherHost, portNum,
                                   connectedOkCb,
                                   connectFailedCb,
                                   connectionBrokenCb)

    def _PingSucceeded(self, connection, remoteNodeName):
        callbacks = self._ongoingPings[remoteNodeName]
        del self._ongoingPings[remoteNodeName]
        self.connections[remoteNodeName] = connection
        for cb in callbacks:
            cb("pong")

    def _PingFailed(self, connection, remoteNodeName):
        callbacks = self._ongoingPings[remoteNodeName]
        del self._ongoingPings[remoteNodeName]
        for cb in callbacks:
            cb("pang")
        
    def _OutConnectionBroken(self, connection, remoteNodeName):
        if self.connections.has_key(remoteNodeName):
            del self.connections[remoteNodeName]
