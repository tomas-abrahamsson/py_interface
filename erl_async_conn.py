import socket

import erl_common
import eventhandler


class ErlAsyncClientConnection:
    def __init__(self):
        self._isConnected = 0
        self._Init()
        self.evhandler = eventhandler.GetEventHandler()
        

    def Connect(self, hostname, portNum):
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            s.connect((hostname, portNum))
            s.setblocking(0)
            self.hostname = hostname
            self.portNum = portNum
            self._SetConnectionOpen(s)
            return 1
        except socket.error, errInfo:
            print "socket error:", errInfo
            self._SetConnectionClosed()
            return 0

    def Close(self):
        if not self._isConnected:
            return
        self._connection.close()
        self._SetConnectionClosed()

    
    def Send(self, data):
        self._SendOrQueue(data)


    def GetConnection(self):
        if not self._isConnected:
            return None
        else:
            return self._connection


    ##
    ## Internal routines
    ##


    def _Init(self):
        self._currentRequests = []
        self._pendingOutput = ""

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

    def _SendOrQueue(self, data):
        numBytesToSend = len(data)
        try:
            numBytesSent = self._connection.send(data)
            if numBytesSent < numBytesToSend:
                remaining = data[numBytesSent:]
                self._Queue(remaining)
        except socket.error, (errNum, errText):
            if errNum == 11:
                self._Queue(data)
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
        print "erl_sync_conn: please override me!"
        newData = self._connection.recv(10000)

    def ReadInt1(self, s):
        return erl_common.ReadInt1(s)
    
    def ReadInt2(self, s):
        return erl_common.ReadInt2(s)
    
    def ReadInt4(self, s):
        return erl_common.ReadInt4(s)

    def PackInt1(self, i):
        return erl_common.PackInt1(i)

    def PackInt2(self, i):
        return erl_common.PackInt2(i)

    def PackInt4(self, i):
        return erl_common.PackInt4(i)
    
