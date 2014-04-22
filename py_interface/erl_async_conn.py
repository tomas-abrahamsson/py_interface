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

### erl_async_conn.py -- general async communication over tcp/ip

import socket
try:
    import SOCKET
except ImportError:
    SOCKET = socket
import errno

from py_interface import erl_common
from py_interface import erl_eventhandler


class ErlAsyncPeerConnection:
    def __init__(self, openSocket=None):
        self.evhandler = erl_eventhandler.GetEventHandler()
        if openSocket == None:
            self._Init()
        else:
            self._Init()
            self._SetConnectionOpen(openSocket)


    def Close(self):
        if not self._isConnected:
            return
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
        self._isConnected = 0
        self._pendingOutput = ""

    def _SetConnectionClosed(self):
        if self._isConnected:
            self.evhandler.PopReadEvent(self._connection)
            self._connection.close()
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
            if errNum == errno.EAGAIN or errNum == errno.EWOULDBLOCK:
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
            numBytesSent = self._connection.send(self._pendingOutput)
            if numBytesSent == numBytesToSend:
                self._pendingOutput = ""
                self.evhandler.PopWriteEvent(self._connection)
            else:
                self._pendingOutput = self._pendingOutput[numBytesSent:]
        except socket.error, (errNum, errText):
            if errNum == errno.EAGAIN or errNum == errno.EWOULDBLOCK:
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


class ErlAsyncClientConnection(ErlAsyncPeerConnection):
    def __init__(self):
        ErlAsyncPeerConnection.__init__(self)


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


class ErlAsyncServer(ErlAsyncPeerConnection):
    def __init__(self):
        ErlAsyncPeerConnection.__init__(self)

    def Start(self, portNum=0, iface=""):
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            s.setsockopt(SOCKET.SOL_SOCKET, SOCKET.SO_REUSEADDR, 1)
            s.bind((iface, portNum))
            (ipNum, resultPortNum) = s.getsockname()
            s.listen(5)
            s.setblocking(0)
            self.hostname = iface
            self.portNum = resultPortNum
            self._SetConnectionOpen(s)
            return resultPortNum
        except socket.error, errInfo:
            print "socket error:", errInfo
            return None

    def _In(self):
        s = self.GetConnection()
        (s2, (remoteHost, remotePort)) = s.accept()
        self._NewConnection(s2, (remoteHost, remotePort))

    def _NewConnection(self, sockConn, remoteAddr):
        asyncConnection = ErlAsyncPeerConnection(s2)
        pass
