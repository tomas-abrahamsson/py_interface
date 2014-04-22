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

### erl_epmd.py -- handle the communication with the epmd
###                (the Erlang portmapper daemon)

import sys
import types
import string
import socket
import getopt

from py_interface import erl_common
from py_interface import erl_async_conn
from py_interface import erl_eventhandler

NODETYPE_NORMAL = 77
NODETYPE_HIDDEN = 72

M = "erl_epmd"

class ErlEpmdError(Exception):
    def __init__(self, reason):
        self.reason = reason

class ErlEpmd:
    """This class provides a connection to the Erlang Portmapper Daemon, EPMD.
    """
    def __init__(self, hostName="localhost", portNum=4369):
        """Constructor.

        HOST-NAME = string
                    Default is "localhost"
        PORT-NUM  = integer
                    Default is 4369

        Creates an instance for communicating with the EPMD on HOST-NAME
        on PORT-NUM.
        Use the Connect method to establish the actual connection to the EPMD.
        """
        self._hostName = hostName
        self._portNum = portNum
        self.connection = None

    def SetOwnPortNum(self, ownPortNum):
        """Specify the port number for the node

        OWN-PORT-NUM = integer

        Specify the port number for the socket that serves incoming
        connections to the node. This is the portnumber to be published
        to the EPMD. Other nodes looks up the nodes portnumber in the
        EPMD, then establishes connections to that port.
        """
        self._ownPortNum = ownPortNum

    def SetOwnNodeName(self, nodeName):
        """Specify the node name for the node

        NODE-NAME = string
                    Must be on the form alivename@hostname

        Sets the nodes name. This is the node name that is published in
        the EPMD.
        """
        ## XXX Ought to change the scheme: get the host from the node-name
        ##     instead of from an argument to the constructor?
        self._ownNodeName = nodeName

    def Connect(self, connectedCb, connectFailedCb):
        """Connect to the EPMD and issue an Alive2 request.

        CONNECTED-CB      = <function(CREATION): void>
                            CREATION = integer
        CONNECT-FAILED-CB = <function(RESULT): void>
                            RESULT = integer

        Connects to the EPMD specified in the constructor, then publishes
        the own node name and port by issuing an Alive2 request.

        The SetOwnPortNum and SetOwnNodeName methods must have been called
        prior to calling this method.
        """
        self._connectedCb = connectedCb
        self._connectFailedCb = connectFailedCb
        self._epmdConn = ErlEPMDStdConnection()
        if not self._epmdConn.Connect(self._hostName, self._portNum):
            raise ErlEpmdError("Connection to EPMD failed")
        self.Alive2Req(self._ownPortNum, NODETYPE_HIDDEN,
                       (5, 5), self._ownNodeName, "", self._Alive2RespCb)

    def Close(self):
        """Close the connection to the EPMD."""
        self.AliveCloseReq(self._AliveCloseSink)

    ## Requests
    ##

    def AliveReq(self, portNum, nodeName, cb):
        """Issue an Alive request.
        PORT-NUM        = integer
                          The port number of the socket that serves incoming
                          connections to the node.
        NODE-NAME       = string
                          the name of the node (on the form alive@host)

        This request has no callback.
        """
        self._epmdConn.AliveReq(portNum, nodeName, cb)

    def Alive2Req(self, portNum, nodeType, distrVSNRange, nodeName, extra, cb):
        """Issue an Alive2 request.
        PORT-NUM        = integer
                          The port number of the socket that serves incoming
                          connections to the node.
        NODE-TYPE       = integer
        DISTR-VSN-RANGE = tuple(LO, HI)
                          LO = HI = integer
        NODE-NAME       = string
                          the name of the node (on the form alive@host)
        EXTRA           = string
        CB              = <function(CREATION): void>
                          CREATION = integer

        Calls the callback CB when the answer is available.
        See the file distribution_handshake.txt in the erlang distribution
        for more info on the NODE-TYPE, DISTR-VSN-RANGE and EXTRA arguments.
        """
        self._epmdConn.Alive2Req(portNum, nodeType, distrVSNRange,
                                 nodeName, extra, cb)

    def AliveCloseReq(self, cb):
        """Issue an AliveClose request.
        CB = <function(): void>

        This effectively closes the socket connection to the EPMD.
        """
        self._epmdConn.AliveCloseReq(cb)

    def PortPleaseReq(self, nodeName, callback):
        """Issue a PortPlease request.
        NODE-NAME = string
        CALLBACK  = <function(PORT-NUMBER): void>
                    PORT-NUMBER = integer
        Calls the CALLBACK function with argument PORT-NUMBER when
        the answer is available.
        """
        e = ErlEPMDOneShotConnection(self._hostName, self._portNum)
        e.PortPleaseReq(nodeName, callback)

    def PortPlease2Req(self, nodeName, callback):
        """Issue a PortPlease2 request.
        NODE-NAME = string
        CALLBACK  = <function(RESULT, NODE-TYPE, PROTOCOL, DISTR-VSN-RANGE,
                              RNODE-NAME, EXTRA): void>
                    RESULT = 1 | 0
                    NODE-TYPE = integer
                    PROTOCOL = integer
                    DISTR-VSN-RANGE = tuple(LO, HI)
                    LO = HI = integer
                    RNODE-NAME = string
                    EXTRA = string
        Calls the CALLBACK function when the answer is available from the EPMD.
        If the RESULT is 0, then the values of the rest of the arguments
        are undefined. If the result is 1, then the rest of the arguments
        have the values as reported from the EPMD.

        Calls the CALLBACK function with argument PORT-NUMBER when
        the answer is available.
        """
        e = ErlEPMDOneShotConnection(self._hostName, self._portNum)
        e.PortPlease2Req(nodeName, callback)

    def NamesReq(self, callback):
        """Issue a Names request
        CALLBACK = <function(EPMD-PORT-NUM, NODE-INFO): void
                   EPMD-PORT-NUM = NODE-INFO = integer
        Calls the CALLBACK when the answer is available.
        """
        e = ErlEPMDOneShotConnection(self._hostName, self._portNum)
        e.NamesReq(callback)

    def DumpReq(self, callback):
        """Issue a Dump request
        CALLBACK = <function(EPMD-PORT-NUM, NODE-INFO): void
                   EPMD-PORT-NUM = NODE-INFO = integer
        Calls the CALLBACK when the answer is available.
        """
        e = ErlEPMDOneShotConnection(self._hostName, self._portNum)
        e.DumpReq(callback)

    def KillReq(self, callback):
        """Issue a Kill request
        CALLBACK = <function(RESPONSE): void
                   RESPONSE = string
        Calls the CALLBACK when the answer is available.
        """
        e = ErlEPMDOneShotConnection(self._hostName, self._portNum)
        e.KillReq(callback)

    def StopReq(self, nodeName, callback):
        e = ErlEPMDOneShotConnection(self._hostName, self._portNum)
        e.StopReq(callback)

    ##
    ## Internal routines
    ##

    def _Alive2RespCb(self, result, creation):
        if result == 0:
            # ok:
            self._connectedCb(creation)
        else:
            self._connectFailedCb(result)

    def _AliveCloseSink(self):
        pass

class ErlEPMDOneShotConnection(erl_async_conn.ErlAsyncClientConnection):
    """This class is intended to be used by the instances of the ErlEPMD class.

    This class handles one-shot connections to the Erlang Portmapper
    Daemon, EPMD. All requests except ALIVE and ALIVE2 result in one-shot
    connections to the EPMD, meaning that a new TCP/IP connection is set
    up for the request, and then teared down when the answer has arrived.
    """

    _PORT_PLEASE_REQ = 112
    _PORT_PLEASE2_REQ = 122
    _NAMES_REQ = 110
    _DUMP_REQ = 100
    _KILL_REQ = 107
    _STOP_REQ = 115
    _PORT2_RESP = 119

    def __init__(self, hostName, portNum):
        """Constructor
        HOST-NAME = string
                    The host to connect to
        PORT-NUM  = integer
                    The port number for the EPMD.
        Set up an object for a one-shot connection to host:port.
        """
        erl_async_conn.ErlAsyncClientConnection.__init__(self)
        self._recvdata = ""
        self._oneShotCallback = None
        self._hostName = hostName
        self._portNum = portNum

    def PortPleaseReq(self, nodeName, callback):
        """Issue a PortPlease request.
        NODE-NAME = string
        CALLBACK  = <function(PORT-NUMBER): void>
                    PORT-NUMBER = integer
        Calls the CALLBACK function with argument PORT-NUMBER when
        the answer is available.
        """
        msg = self.PackInt1(self._PORT_PLEASE_REQ) + nodeName
        unpackcb = erl_common.Callback(self._UnpackPortPleaseResp, callback)
        self._SendOneShotReq(msg, unpackcb)

    def PortPlease2Req(self, nodeName, callback):
        """Issue a PortPlease2 request.
        NODE-NAME = string
        CALLBACK  = <function(RESULT, NODE-TYPE, PROTOCOL, DISTR-VSN-RANGE,
                              RNODE-NAME, EXTRA): void>
                    RESULT = 1 | 0
                    NODE-TYPE = integer
                    PROTOCOL = integer
                    DISTR-VSN-RANGE = tuple(LO, HI)
                    LO = HI = integer
                    RNODE-NAME = string
                    EXTRA = string
        Calls the CALLBACK function when the answer is available from the EPMD.
        If the RESULT is 0, then the values of the rest of the arguments
        are undefined. If the result is 1, then the rest of the arguments
        have the values as reported from the EPMD.

        Calls the CALLBACK function with argument PORT-NUMBER when
        the answer is available.
        """
        msg = self.PackInt1(self._PORT_PLEASE2_REQ) + nodeName
        unpackcb = erl_common.Callback(self._UnpackPortPlease2Resp, callback)
        self._SendOneShotReq(msg, unpackcb)

    def NamesReq(self, callback):
        """Issue a Names request
        CALLBACK = <function(EPMD-PORT-NUM, NODE-INFO): void
                   EPMD-PORT-NUM = NODE-INFO = integer
        Calls the CALLBACK when the answer is available.
        """
        msg = self.PackInt1(self._NAMES_REQ)
        unpackcb = erl_common.Callback(self._UnpackNamesResp, callback)
        self._SendOneShotReq(msg, unpackcb)

    def DumpReq(self, callback):
        """Issue a Dump request
        CALLBACK = <function(EPMD-PORT-NUM, NODE-INFO): void
                   EPMD-PORT-NUM = NODE-INFO = integer
        Calls the CALLBACK when the answer is available.
        """
        msg = self.PackInt1(self._DUMP_REQ)
        unpackcb = erl_common.Callback(self._UnpackDumpResp, callback)
        self._SendOneShotReq(msg, unpackcb)

    def KillReq(self, callback):
        """Issue a Kill request
        CALLBACK = <function(RESPONSE): void
                   RESPONSE = string
        Calls the CALLBACK when the answer is available.
        """
        msg = self.PackInt1(self._KILL_REQ)
        unpackcb = erl_common.Callback(self._UnpackKillResp, callback)
        self._SendOneShotReq(msg, unpackcb)

    def StopReq(self, nodeName, callback):
        msg = self.PackInt1(self._STOP_REQ) + nodeName
        unpackcb = erl_common.Callback(self._UnpackStopResp, callback)
        self._SendOneShotReq(msg, unpackcb)

    ##
    ## Internal routines
    ##


    ##
    ## Send oneshot
    ##

    def _SendOneShotReq(self, req, unpackcb):
        if self.Connect(self._hostName, self._portNum):
            self._oneShotCallback = unpackcb
            msg = self.PackInt2(len(req)) + req
            self.Send(msg)
            return 1
        else:
            return 0

    ##
    ## Incoming data and unpack routines
    ##

    def _In(self):
        """Callback routine, which is called when data is available
        on the connection."""
        connection = self.GetConnection()
        newData = connection.recv(100000)
        if len(newData) == 0:
            self.Close()
            if self._oneShotCallback != None:
                self._oneShotCallback(self._recvdata)
        else:
            self._recvdata = self._recvdata + newData


    def _UnpackPortPleaseResp(self, resp, cb):
        portNum = self.ReadInt2(resp[0:2])
        cb(portNum)

    def _UnpackPortPlease2Resp(self, resp, cb):
        if len(resp) == 2:
            result = self.ReadInt1(resp[1])
            cb(result, None, None, None, None, None, None)
        else:
            res = self.ReadInt1(resp[1])
            portNum = self.ReadInt2(resp[2:4])
            nodeType = self.ReadInt1(resp[4])
            protocol = self.ReadInt1(resp[5])
            distrVSNLo = self.ReadInt2(resp[6:8])
            distrVSNHi = self.ReadInt2(resp[8:10])
            distrVSNRng = (distrVSNLo, distrVSNHi)
            nLen = self.ReadInt2(resp[10:12])
            nodeName = resp[12:12 + nLen]
            if len(resp) == 12 + nLen + 1 and \
               self.ReadInt1(resp[-1]) == 0:
                extra = ""
            else:
                eLen = self.ReadInt2(resp[12 + nLen:12 + nLen + 2])
                if eLen == 2:
                    extra = self.ReadInt2(resp[12 + nLen + 2:])
                else:
                    extra = ""
            cb(res, portNum, nodeType, protocol, distrVSNRng, nodeName, extra)

    def _UnpackNamesResp(self, resp, cb):
        epmdPortNum = self.ReadInt4(resp[0:4])
        nodeInfo = self.ReadInt4(resp[4:])
        cb(epmdPortNum, nodeInfo)

    def _UnpackDumpResp(self, resp, cb):
        epmdPortNum = self.ReadInt4(resp[0:4])
        nodeInfo = self.ReadInt4(resp[4:])
        cb(epmdPortNum, nodeInfo)

    def _UnpackKillResp(self, resp, cb):
        cb(resp)

    def _UnpackStopResp(self, resp, cb):
        cb(resp)


class ErlEPMDStdConnection(erl_async_conn.ErlAsyncClientConnection):
    """This class is intended to be used by the instances of the ErlEPMD class.

    This class provides a long-lasting connection to the Erlang Portmapper
    Daemon, EPMD. This type of connection is used for ALIVE and ALIVE2
    requests, where the connection is expected to live as long as the
    node is alive.
    """
    _ALIVE_REQ = 97
    _ALIVE2_REQ = 120
    _ALIVE_OK_RESP = 89
    _ALIVE2_RESP = 121

    def __init__(self):
        """Constructor."""
        erl_async_conn.ErlAsyncClientConnection.__init__(self)
        self._currentRequests = []
        self._pendingInput = ""

    ##
    ## Requests to the EPMD
    ##

    def Alive2Req(self, portNum, nodeType, distrVSNRange, nodeName, extra, cb):
        """Issue an Alive2 request.
        PORT-NUM        = integer
                          The port number of the socket that serves incoming
                          connections to the node.
        NODE-TYPE       = integer
        DISTR-VSN-RANGE = tuple(LO, HI)
                          LO = HI = integer
        NODE-NAME       = string
                          the name of the node (on the form alive@host)
        EXTRA           = string
        CB              = <function(CREATION): void>
                          CREATION = integer

        Calls the callback CB when the answer is available.
        See the file distribution_handshake.txt in the erlang distribution
        for more info on the NODE-TYPE, DISTR-VSN-RANGE and EXTRA arguments.
        """
        aliveName = string.split(nodeName, "@")[0]
        msg = (self.PackInt1(self._ALIVE2_REQ) +
               self.PackInt2(portNum) +
               self.PackInt1(nodeType) +
               self.PackInt1(0) +      # protocol: 0 = tcp/ip-v4
               self.PackInt2(distrVSNRange[0]) +
               self.PackInt2(distrVSNRange[1]) +
               self.PackInt2(len(aliveName)) + aliveName +
               self.PackInt2(len(extra)) + extra)
        self._SendReq(msg, cb)


    def AliveReq(self, portNum, nodeName):
        """Issue an Alive request.
        PORT-NUM        = integer
                          The port number of the socket that serves incoming
                          connections to the node.
        NODE-NAME       = string
                          the name of the node (on the form alive@host)

        This request has no callback.
        """
        msg = (self.PackInt1(self._ALIVE_REQ) +
               self.PackInt2(portNum) +
               nodeName)
        self._SendReq(msg, cb)

    def AliveCloseReq(self, cb):
        """Issue an AliveClose request.
        CB = <function(): void>

        This effectively closes the socket connection to the EPMD.
        """
        self.Close()
        cb()


    ##
    ## Internal routines
    ##

    ##
    ## Sending
    ##

    def _SendReq(self, req, cb):
        if not self._isConnected:
            raise ErlEpmdError("not connected to epmd")
        self._NewCurrReq(ord(req[0]), cb)
        msg = self.PackInt2(len(req)) + req
        self.Send(msg)


    def _NewCurrReq(self, reqId, cb):
        self._currentRequests.append((reqId, cb))


    def _GetCurrReqId(self):
        if len(self._currentRequests) == 0:
            return None
        else:
            return self._currentRequests[0][0]

    def _GetCurrReqCb(self):
        if len(self._currentRequests) == 0:
            return None
        else:
            return self._currentRequests[0][1]


    def _CurrReqDone(self):
        self._currentRequests = self._currentRequests[1:]


    ##
    ## Handling incoming data
    ##

    def _In(self):
        is_closed = 0
        data = self._connection.recv(100000)
        if len(data) == 0:
            # closed connection
            is_closed = 1

        newInput = self._pendingInput + data

        # split into chunks separated by newline sequence
        # call the callback for each of these chunks

        newPendingInput = self._HandleMsgs(newInput)
        self._pendingInput = newPendingInput

        if is_closed:
            self._OtherEndClosedConnection()


    def _OtherEndClosedConnection(self):
        if self._GetCurrReqId() == self._ALIVE_REQ:  # alive_req
            self.AliveNotOkResp()
            self._CurrReqDone()
        else:
            self.ConnectionClosed()
        ## close our end
        self.Close()


    def _HandleMsgs(self, input):
        toBeUnpacked = input
        while 1:
            (parsedOk, remainingInput) = self._HandleMsg(toBeUnpacked)
            if not parsedOk:
                return remainingInput
            else:
                self._CurrReqDone()
            toBeUnpacked = remainingInput


    def _HandleMsg(self, data):
        dataLen = len(data)
        if dataLen < 3:
            return (0, data)

        data0 = ord(data[0])
        if data0 == self._ALIVE_OK_RESP and \
           self._GetCurrReqId() == self._ALIVE_REQ:
            if dataLen < 3:
                return (0, data)
            creation = self.ReadInt2(data[1:3])
            cb = self._GetCurrReqCb()
            cb(creation)
            self._CurrReqDone()
            return (1, data[3:])
        elif data0 == self._ALIVE2_RESP and \
             self._GetCurrReqId() == self._ALIVE2_REQ:
            if dataLen < 4:
                return (0, data)
            result = self.ReadInt1(data[1])
            creation = self.ReadInt2(data[2:4])
            cb = self._GetCurrReqCb()
            cb(result, creation)
            self._CurrReqDone()
            return (1, data[4:])

        currReqTxt = "current request is %s" % `self._GetCurrReqId()`
        erl_common.DebugHex(M, "unexpected msg, trown away, "+currReqTxt, data)
        return (0, "")
