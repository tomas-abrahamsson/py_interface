import sys
import types
import string
import socket

import erl_common
import eventhandler
import erl_async_conn

NODETYPE_NORMAL = 77
NODETYPE_HIDDEN = 72

class OtpEpmdConnection(erl_async_conn.AsyncClientConnection):
    def In(self, data):
        pass
        


class OtpEpmd:
    _ALIVE_REQ = 97
    _ALIVE2_REQ = 120
    _PORT_PLEASE_REQ = 112
    _PORT_PLEASE2_REQ = 122
    _NAMES_REQ = 110
    _DUMP_REQ = 100
    _KILL_REQ = 107
    _STOP_REQ = 115
    _ALIVE_OK_RESP = 89
    _ALIVE2_RESP = 121
    _PORT2_RESP = 119


    def __init__(self):
        self._isConnected = 0
        self._Init()
        self.evhandler = eventhandler.GetEventHandler()
        self._SetConnectionClosed()
    

    def Connect(self, hostname="localhost", portNum=4369):
        pass
    
    def Close(self):
        pass

    ## Requests
    ##

    def AliveReq(self, portNum, nodeName):
        msg = (self._PackInt1(self._ALIVE_REQ) +
               self._PackInt2(portNum) +
               nodeName)
        self._SendReq(msg)

    def Alive2Req(self, portNum, nodeType, distrVSNRange, nodeName, extra):
        msg = (self._PackInt1(self._ALIVE2_REQ) +
               self._PackInt2(portNum) +
               self._PackInt1(nodeType) +
               self._PackInt1(0) +      # protocol: 0 = tcp/ip-v4
               self._PackInt2(distrVSNRange[0]) + 
               self._PackInt2(distrVSNRange[1]) +
               self._PackInt2(len(nodeName)) +
               nodeName +
               self._PackInt2(len(extra)) +
               extra)
        self._SendReq(msg)

    def AliveCloseReq(self):
        self.Close()

    def PortPleaseReq(self, nodeName):
        msg = self._PackInt1(self._PORT_PLEASE_REQ) + nodeName
        self._SendReq(msg)

    def PortPlease2Req(self, nodeName):
        msg = self._PackInt1(self._PORT_PLEASE2_REQ) + nodeName
        self._SendReq(msg)

    def NamesReq(self):
        msg = self._PackInt1(self._NAMES_REQ)
        self._SendReq(msg)

    def DumpReq(self):
        msg = self._PackInt1(self._DUMP_REQ)
        self._SendReq(msg)

    def KillReq(self):
        msg = self._PackInt1(self._KILL_REQ)
        self._SendReq(msg)

    def StopReq(self, nodeName):
        raise "Not used"
        msg = self._PackInt1(self._STOP_REQ) + nodeName
        self._SendReq(msg)

    ## Responses
    ##

    def AliveOkResp(self, creation):
        print "AliveOkResp creation=%d" % creation

    def AliveNotOkResp(self):
        print "AliveNotOkResp"

    def Alive2Resp(self, result, creation):
        print "Alive2Resp, result=%d, creation=%d" % (result, creation)

    def PortOkResp(self, portNum):
        print "PortOkResp, portNum=%d" % portNum

    def PortNotOkResp(self):
        print "PortNotOkResp"

    def Port2OkResp(self, portNum, nodeType, proto, distr, nodeName, extra):
        print ("Port2OkResp, portNum=%d nodeType=%d protocol=%d" +
               " distrVSNRange=%s nodeName=%s extra=%s") % \
               (portNum, nodeType, proto, `distr`, nodeName, extra)

    def Port2NotOkResp(self, result):
        print "Port2NotOkResp result=%d" % result


    def NamesResp(self, epmdPortNum, nodeInfo):
        print "NamesResp, epmdPortNum=%d nodeInfo:\n%s" % \
              (epmdPortNum, nodeInfo)

    def DumpResp(self, epmdPortNum, nodeInfo):
        print "DumpResp, epmdPortNum=%d nodeInfo:\n%s" % \
              (epmdPortNum, nodeInfo)

    def KillResp(self, resp):
        print "KillResp, resp=%s" % resp

    def StopResp(self, resp):
        print "StopResp, resp=%s" % resp

    def ConnectionClosed(self):
        print "Connection to epmd has been closed."


    ##
    ## Internal routines
    ##

    def _Init(self):
        self._currentRequests = []
        self._pendingInput = ""

    def _SetConnectionClosed(self):
        if self._isConnected:
            self.evhandler.PopReadEvent(self._connection)
            self._connection = None
            self._isConnected = 0
            self._Init()

    def _SetConnectionOpen(self, sock):
        if not self._isConnected:
            self._connection = sock
            self._isConnected = 1
            self.evhandler.PushReadEvent(self._connection, self._In)


    def _GetCurrReq(self):
        if len(self._currentRequests) == 0:
            return None
        else:
            return self._currentRequests[0]

    def _NewCurrReq(self, reqNum):
        self._currentRequests.append(reqNum)

    def _CurrReqDone(self):
        self._currentRequests = self._currentRequests[1:]


    def _SendReq(self, req):
        if not self._isConnected:
            raise "not connected to epmd"
        self._NewCurrReq(req[0])
        msg = self._PackInt2(len(req)) + req
        print "Sending:"
        erl_common.HexDump(msg)
        print
        self._SendOrQueueStr(msg)
        
        
    def _SendOrQueueStr(self, erlMsg):
        numBytesToSend = len(erlMsg)
        try:
            numBytesSent = self._connection.send(erlMsg)
            if numBytesSent < numBytesToSend:
                remaining = erlMsg[numBytesSent:]
                self._Queue(remaining)
        except socket.error, (errNum, errText):
            if errNum == 11:
                self._Queue(erlMsg)
            else:
                raise

    def _Queue(self, strToQueue):
        if self._pendingOutput == "":
            self.evhandler.PushWriteEvent(self._connection, self._QueuedWrite)
        self._pendingOutput = self._pendingOutput + strToQueue

    def _QueuedWrite(self):
        numBytesToSend = len(self._pendingOutput)
        try:
            numBytesSent = self._connection.send(erlStr)
            if numBytesSent == numBytesToSend:
                self._pendingOutput = ""
                self.evhandler.PopWriteEvent(self._connection)
            else:
                self._pendingOutput = self._pendingOutput[numBytesSent:]
        except socket.error, (errNum, errText):
            if errNum == 11:
                # still not possible to send...
                # wait a bit more
                pass


    def _In(self):
        is_closed = 0
        print "_In: getting data..."
        data = self._connection.recv(100000)
        print "_In: got data:"
        erl_common.HexDump(data)
        print

        if len(data) == 0:
            # closed connection
            is_closed = 1

        newInput = self._pendingInput + data

        # split into chunks separated by newline sequence
        # call the callback for each of these chunks

        newPendingInput = self._HandleMsgs(newInput)
        self._pendingInput = newPendingInput

        if is_closed:
            self._ConnectionClosed()

    def _ConnectionClosed(self):
        if self._GetCurrReq() == 97:  # alive_req
            self.AliveNotOkResp()
            self._CurrReqDone()
        elif self._GetCurrReq() == 112: # port_please_req
            self.PortNotOkResp()
            self._CurrReqDone()
        else:
            self.ConnectionClosed()
        self._SetConnectionClosed()


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
        if dataLen < 2:
            return (0, data)

        respLen = self._ReadInt2(data[0:2])
        if dataLen < 2 + respLen:
            return (0, data)

        print "respLen=%d" % respLen


        resp = data[2:2+respLen]
        remaining = data[2 + respLen:]
        resp0 = resp[0]
        if resp0 == self._ALIVE_RESP and \
           self._GetCurrReq() == self._ALIVE_REQ:
            creation = self._ReadInt2(resp[1:3])
            self.AliveOkResp(creation)
            return (1, remaining)
        elif resp0 == self._ALIVE2_RESP and \
           self._GetCurrReq() == self._ALIVE2_REQ:
            result = self._ReadInt1(resp[1])
            creation = self._ReadInt2(resp[2:4])
            self.Alive2Resp(result, creation)
            return (1, remaining)
        elif resp0 == self._PORT2_RESP and \
           self._GetCurrReq() == self._PORT_PLEASE2_REQ:
            if len(resp) == 2:
                result = self._ReadInt1(resp[1])
                self.Port2NotOkResp(result)
                return (1, remaining)
            else:
                result = self._ReadInt1(resp[1])
                portNum = self._ReadInt2(resp[2:4])
                nodeType = self._ReadInt1(resp[4])
                protocol = self._ReadInt1(resp[5])
                distrVSNLo = self._ReadInt2(resp[6:8])
                distrVSNHi = self._ReadInt2(resp[8:10])
                distrVSNRange = (distrVSNLo, distrVSNHi)
                nLen = self._ReadInt2(resp[10:12])
                nodeName = self._ReadInt2(resp[12:12 + nLen])
                eLen = self._ReadInt2(resp[12 + nLen:12 + nLen + 2])
                extra = self._ReadInt2(resp[12 + nLen + 2:])
                self.Port2OkResp(portNum, nodeType, protocol, distrVSNRange,
                                 nodeName, extra)
                return (1, remaining)
        elif self._GetCurrReq() == self._PORT_REQ:
            portNum = self._ReadInt2(resp[0:2])
            self.PortOkResp(portNum)
            return (1, remaining)
        elif self._GetCurrReq() == self._NAMES_REQ:
            epmdPortNum = self._ReadInt4(resp[0:4])
            nodeInfo = self._ReadInt4(resp[4:])
            self.NamesResp(epmdPortNum, nodeInfo)
            return (1, remaining)
        elif self._GetCurrReq() == self._DUMP_REQ:
            epmdPortNum = self._ReadInt4(resp[0:4])
            nodeInfo = self._ReadInt4(resp[4:])
            self.DumpResp(epmdPortNum, nodeInfo)
            return (1, remaining)
        elif self._GetCurrReq() == self._KILL_REQ:
            self.KillResp(resp)
            return (1, remaining)
        elif self._GetCurrReq() == self._STOP_REQ:
            self.StopResp(resp)
            return (1, remaining)

        currReqTxt = "current request is %s" % `self._GetCurrReq()`
        erl_common.DebugUnrecognizedMsg("EPMD, trown away "+currReqTxt, resp)
        return (None, remaining)
        


    def _ReadInt1(self, s):
        return erl_common.ReadInt1(s)
    
    def _ReadInt2(self, s):
        return erl_common.ReadInt2(s)
    
    def _ReadInt4(self, s):
        return erl_common.ReadInt4(s)

    def _PackInt1(self, i):
        return erl_common.PackInt1(i)

    def _PackInt2(self, i):
        return erl_common.PackInt2(i)

    def _PackInt4(self, i):
        return erl_common.PackInt4(i)



def main(argv):
    o = OtpEpmd()
    print o.Connect()
    o.PortPlease2Req("server__19313")
    evhandler = eventhandler.GetEventHandler()
    evhandler.Loop()


if __name__ == '__main__':
    main(sys.argv)
