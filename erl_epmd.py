import sys
import types
import string
import socket
import getopt

import erl_common
import erl_async_conn
import erl_eventhandler

NODETYPE_NORMAL = 77
NODETYPE_HIDDEN = 72

M = "erl_epmd"

class ErlEPMDOneShotConnection(erl_async_conn.ErlAsyncClientConnection):
    _PORT_PLEASE_REQ = 112
    _PORT_PLEASE2_REQ = 122
    _NAMES_REQ = 110
    _DUMP_REQ = 100
    _KILL_REQ = 107
    _STOP_REQ = 115
    _PORT2_RESP = 119

    def __init__(self, hostName, portNum):
        erl_async_conn.ErlAsyncClientConnection.__init__(self)
        self._recvdata = ""
        self._oneShotCallback = None
        self._hostName = hostName
        self._portNum = portNum

    def PortPleaseReq(self, nodeName, callback):
        msg = self.PackInt1(self._PORT_PLEASE_REQ) + nodeName
        unpackcb = erl_common.Callback(self._UnpackPortPleaseResp, callback)
        self._SendOneShotReq(msg, unpackcb)

    def PortPlease2Req(self, nodeName, callback):
        msg = self.PackInt1(self._PORT_PLEASE2_REQ) + nodeName
        unpackcb = erl_common.Callback(self._UnpackPortPlease2Resp, callback)
        self._SendOneShotReq(msg, unpackcb)

    def NamesReq(self, callback):
        msg = self.PackInt1(self._NAMES_REQ)
        unpackcb = erl_common.Callback(self._UnpackNamesResp, callback)
        self._SendOneShotReq(msg, unpackcb)

    def DumpReq(self, callback):
        msg = self.PackInt1(self._DUMP_REQ)
        unpackcb = erl_common.Callback(self._UnpackDumpResp, callback)
        self._SendOneShotReq(msg, unpackcb)

    def KillReq(self, callback):
        msg = self.PackInt1(self._KILL_REQ)
        unpackcb = erl_common.Callback(self._UnpackKillResp, callback)
        self._SendOneShotReq(msg, unpackcb)

    def StopReq(self, nodeName, callback):
        raise "Not used"
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
                extra = self.ReadInt2(resp[12 + nLen + 2:])
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
    _ALIVE_REQ = 97
    _ALIVE2_REQ = 120
    _ALIVE_OK_RESP = 89
    _ALIVE2_RESP = 121

    def __init__(self):
        erl_async_conn.ErlAsyncClientConnection.__init__(self)
        self._currentRequests = []
        self._pendingInput = ""

    ##
    ## Requests to the EPMD
    ##

    def Alive2Req(self, portNum, nodeType, distrVSNRange, nodeName, extra, cb):
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
        msg = (self.PackInt1(self._ALIVE_REQ) +
               self.PackInt2(portNum) +
               nodeName)
        self._SendReq(msg, cb)

    def AliveCloseReq(self, cb):
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
            raise "not connected to epmd"
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


class ErlEpmd:
    def __init__(self, hostName="localhost", portNum=4369):
        self._hostName = hostName
        self._portNum = portNum
        self.connection = None

    def SetOwnPortNum(self, ownPortNum):
        self._ownPortNum = ownPortNum

    def SetOwnNodeName(self, nodeName):
        self._ownNodeName = nodeName

    def Connect(self, connectedCb, connectFailedCb):
        self._connectedCb = connectedCb
        self._connectFailedCb = connectFailedCb
        self._epmdConn = ErlEPMDStdConnection()
        if not self._epmdConn.Connect(self._hostName, self._portNum):
            raise "Connection to EPMD failed"
        self.Alive2Req(self._ownPortNum, NODETYPE_HIDDEN,
                       (5, 5), self._ownNodeName, "", self._Alive2RespCb)

    def Close(self):
        self.AliveCloseReq()

    ## Requests
    ##

    def AliveReq(self, portNum, nodeName, cb):
        self._epmdConn.AliveReq(portNum, nodeName, cb)

    def Alive2Req(self, portNum, nodeType, distrVSNRange, nodeName, extra, cb):
        self._epmdConn.Alive2Req(portNum, nodeType, distrVSNRange,
                                 nodeName, extra, cb)

    def AliveCloseReq(self, cb):
        self._epmdConn.AliveCloseReq(cb)

    def PortPleaseReq(self, nodeName, callback):
        e = ErlEPMDOneShotConnection(self._hostName, self._portNum)
        e.PortPleaseReq(nodeName, callback)

    def PortPlease2Req(self, nodeName, callback):
        e = ErlEPMDOneShotConnection(self._hostName, self._portNum)
        e.PortPlease2Req(nodeName, callback)

    def NamesReq(self, callback):
        e = ErlEPMDOneShotConnection(self._hostName, self._portNum)
        e.NamesReq(callback)

    def DumpReq(self, callback):
        e = ErlEPMDOneShotConnection(self._hostName, self._portNum)
        e.DumpReq(callback)

    def KillReq(self, callback):
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

e = None

def TestAliveOkResp(creation):
    print "AliveOkResp creation=%d" % creation

def TestAliveNotOkResp(self):
    print "AliveNotOkResp"

def TestAlive2Resp(result, creation):
    print "Alive2Resp, result=%d, creation=%d" % (result, creation)

def TestAlive2RespConnected(creation):
    print "Alive2RespConnected, creation=%d" % creation
    nodeToCheckFor = "flerp"
    print "Checking for node named \"%s\"." % nodeToCheckFor
    e.PortPlease2Req(nodeToCheckFor, TestPort2Resp)

def TestAlive2RespConnectFailed(result):
    print "Alive2RespConnectFailed, result=%d" % result

def TestPortOkResp(portNum):
    print "PortOkResp, portNum=%d" % portNum

def TestPortNotOkResp(self):
    print "PortNotOkResp"

def TestPort2Resp(result, portNum, nodeType, proto, distr, nodeName, extra):
    if result == 0:
        # found
        print ("Port2Resp, result=ok, portNum=%d, nodeType=%d, protocol=%d," +
               " distrVSNRange=%s, nodeName=\"%s\", extra=\"%s\"") % \
               (portNum, nodeType, proto, `distr`, nodeName, extra)
    else:
        # not found
        print "Port2Resp, result=%d" % result
        

def TestNamesResp(epmdPortNum, nodeInfo):
    print "NamesResp, epmdPortNum=%d nodeInfo:\n%s" % \
          (epmdPortNum, nodeInfo)

def TestDumpResp(epmdPortNum, nodeInfo):
    print "DumpResp, epmdPortNum=%d nodeInfo:\n%s" % \
          (epmdPortNum, nodeInfo)

def TestKillResp(resp):
    print "KillResp, resp=%s" % resp

def TestStopResp(resp):
    print "StopResp, resp=%s" % resp

def TestConnectionClosed():
    print "Connection to epmd has been closed."

def main(argv):
    global e

    try:
        opts, args = getopt.getopt(argv[1:], "?p:n:")
    except getopt.error, info:
        print info

    hostName = "localhost"
    portNum = 4369
    ownPortNum = 1234
    ownNodeName = "py_interface_test"

    for (optchar, optarg) in opts:
        if optchar == "-?":
            print "Usage: %s host [port]" % argv[0]
            sys.exit(1)
        elif optchar == "-p":
            ownPortNum = string.atoi(optarg)
        elif optchar == "-n":
            ownNodeName = optarg

    if len(args) >= 2:
        hostName = args[0]
        portNum = string.atoi(args[1])
    elif len(args) == 1:
        hostName = args[0]

    e = ErlEpmd(hostName, portNum)
    e.SetOwnPortNum(ownPortNum)
    e.SetOwnNodeName(ownNodeName)
    e.Connect(TestAlive2RespConnected, TestAlive2RespConnectFailed)
    evhandler = erl_eventhandler.GetEventHandler()
    evhandler.Loop()


if __name__ == '__main__':
    main(sys.argv)

