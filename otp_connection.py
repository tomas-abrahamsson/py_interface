### erlcom.py -- communication with the erlang
###              and the Callback and VCallback classes
###
### $Id$
###
### Copyright (C) 2000 Tomas Abrahamsson
### 
### Author: Tomas Abrahamsson <tab@lysator.liu.se>
### 
### This file is part of the Albertina caller id displayer
###
### The Albertina program is free software; you can redistribute it
### and/or modify it under the terms of the GNU Library General Public License
### as published by the Free Software Foundation; either version 2 of the
### License, or (at your option) any later version.
###
### The Albertina program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library
### General Public License for more details.
### 
### You should have received a copy of the GNU Library General Public License
### along with the Albertina program; see the file COPYING.LIB.  If not,
### write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
### Boston, MA 02111-1307, USA. */
### 


import os
import sys
import types
import string
import socket
from   SOCKET import *                  # socket constants


import eventhandler
import common
import erlterm


class ErlCom:
    def __init__(self, erlHost=None, erlPort=None):
        self.serNum = 0
        self._Init()
        if erlHost != None and erlPort != None:
            self.connection = self._Connect(erlHost, erlPort)

        
    def Connect(self, erlHost, erlPort):
        """Try to connect to an erlang host."""
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        # fcntl(s.fileno(), F_SETFL, O_NONBLOCK)
        try:
            s.connect(erlHost, erlPort)
            s.setblocking(0)
            self.erlHost = erlHost
            self.erlPort = erlPort
            self.connection = s
            self._isConnected = 1
            self.evhandler = eventhandler.GetEventHandler()
            self.evhandler.PushReadEvent(self.connection, self._In)
            return 1
        except socket.error, errInfo:
            print "socket error:", errInfo
            self._isConnected = 0
            return 0


    def Disconnect(self):
        """Disconnect from an erlang host"""
        if not self._isConnected:
            return
        self.evhandler.PopReadEvent(self.connection)
        if self._pendingOutput != "":
            self.evhandler.PopWriteEvent(self.connection)
        self.connection.close()
        self._Init()
        self.connection = None
        self._isConnected = 0


    def SendMsgStr(self, erlPidAsStr, erlMsg):
        """Send a message to an erlang process."""
        erlReq = self._FormatSendReq(erlPidAsStr, self._Backslashify(erlMsg))
        self._SendOrQueueStr(erlReq)


    def Rpc(self, module, fn, expr, cbfunction, *optArgs, **namedArgs):
        erlArgsStr = erlterm.Format(expr)
        apply(self.RpcStr,
              (module, fn, erlArgsStr, cbfunction) + optArgs,
              namedArgs)


    def RpcStr(self, module, fn, args, cbfunction, *optArgs, **namedArgs):
        """Call an erlang MODULE, and FUNCTION with ARGS (a python list).
        When the return value arrives, the CB-FUNCTION is called with
        the erlang term (converted to a python value) as first
        argument. Any EXTRA-ARGUMENTS, named as well as unnamed,
        will be appended to every call to the CB-FUNCTION."""
        if not self._isConnected:
            raise "not connected"
        if len(args) == 0:
            raise "empty string"

        serNum = self.serNum
        self.serNum = serNum + 1
        erlArgsStr = self._Backslashify(args)
        erlReq = self._FormatRpc(serNum, module, fn, erlArgsStr)
        self._SendOrQueueStr(erlReq)
        cb = common.VCallback(cbfunction, optArgs, namedArgs)
        self._pendingCalls[serNum] = cb


    def SetDict(self, dict):
        """Set dictinary to use for atoms in erlang terms in incoming messages.
        If an atom is not found in the dictionary, then its string
        representation will be used."""
        self.dict = dict


    def GetDict(self):
        return self.dict


    def SetMsgHandler(self, handlerfn, *optArgs, **namedArgs):
        """Set handler function to call for every erlang messages that is
        not an answers to an rpc call.
        Args: HANDLER-FN, [EXTRA-ARGUMENTS]
        the HANDLER-FN will be called with the erlang term as first
        argument. Any EXTRA-ARGUMENTS, named as well as unnamed,
        will be appended to every call to the HANDLER-FN."""
        self.msgHandler = common.VCallback(handlerfn, optArgs, namedArgs)


    ##
    ## Internal methods below this point
    ##

    def _Init(self):
        self._pendingCalls = {}
        self._pendingOutput = ""
        self._pendingInput = ""
        self.msgHandler = self._MsgHandlerSink
        self.SetDict({})

    def _FormatSend(self, erlPidAsStr, erlMsgStr):
        return "{send_req,\"%s\",%s}\r\n" % (erlPidAsStr, erlMsgStr)

    def _FormatRpc(self, n, module, fn, args):
        return "{rpc_req,%d,'%s','%s',%s}\r\n" % (n, module, fn, args)

    def _Backslashify(self, s):
        res = ""
        for c in s:
            if c == "\n":
                res = res + "\\n"
            elif c == "\r":
                res = res + "\\r"
            else:
                res = res + c
        return res

    def _SendOrQueueStr(self, erlMsg):
        numBytesToSend = len(erlMsg)
        try:
            numBytesSent = self.connection.send(erlMsg)
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
            self.evhandler.PushWriteEvent(self.connection, self._QueuedWrite)
        self._pendingOutput = self._pendingOutput + strToQueue

    def _QueuedWrite(self):
        numBytesToSend = len(self._pendingOutput)
        try:
            numBytesSent = self.connection.send(erlStr)
            if numBytesSent == numBytesToSend:
                self._pendingOutput = ""
                self.evhandler.PopWriteEvent(self.connection)
            else:
                self._pendingOutput = self._pendingOutput[numBytesSent:]
        except socket.error, (errNum, errText):
            if errNum == 11:
                # still not possible to send...
                # wait a bit more
                pass


    def _In(self):
        data = self.connection.recv(100000)
        newInput = self._pendingInput + data
        dict = self.GetDict()

        # split into chunks separated by newline sequence
        # call the callback for each of these chunks
        splitted = string.split(newInput, "\r\n", 1)
        while len(splitted) > 1:
            [firstLine, restOfInput] = splitted
            self._ParseAndEvalErlTerm(firstLine, dict)
            splitted = string.split(restOfInput, "\r\n", 1)

        # assign what's left over from the split to the self._pendingInput
        [remaining] = splitted
        self._pendingInput = remaining

        
    def _ParseAndEvalErlTerm(self, erlTermAsString, dict):
        try:
            result = erlterm.Parse(erlTermAsString)
            self._Eval(result)
        except erlterm.error:
            # ignore
            print "Parseerror"
            pass

    def _Eval(self, erlMsg):
        if type(erlMsg) == types.TupleType \
           and len(erlMsg) > 0 and erlMsg[0] == "rpc_resp":
            reqNum = erlMsg[1]
            answer = erlMsg[2]
            if self._pendingCalls.has_key(reqNum):
                cb = self._pendingCalls[reqNum]
                del self._pendingCalls[reqNum]
                cb(answer)
            else:
                print "No pending call with reqNum = %d" % reqNum
        else:
            msgHandler = self.msgHandler
            msgHandler(erlMsg)

    def _MsgHandlerSink(self, ignoredMsg):
        # ignore anything
        pass

def MakeString(str):
    if type(str) == types.ListType:
        return string.join(map(lambda n: chr(n), str), "")
    else:
        return str

class Unpacker:
    INCOMPLETE = 0

    def __init__(self):
        self.Reset()

    def Reset(self):
        self.inputBuf = ""
        self.state = 0

    def Unpack(self, data):
        self.inputBuf = self.inputBuf + data
        (unpackedTerms, pendingData) = self._UnpackInput(self.inputBuf)
        self.inputBuf = pendingData
        return unpackedTerms

    def _UnpackInput(self, data):
        done = 0
        unpackedTerms = []
        inputData = data
        while not done:
            (unpackedTerm, remainingData) = self._UnpackOneTerm(inputData)
            if unpackedTerm == None:
                return (unpackedTerms, remainingData)
            unpackedTerms.append(unpackedTerm)
            inputData = remainingData

    def _UnpackOneTerm(self, data):
        dataLen = len(data)

        if len(data) == 0:
            return (None, data)

        if data[0] == 97:               # small_integer_ext
            if dataLen >= 2:
                i = OtpInt(ord(data[1]))
                return (i, data[2:])

        elif data[0] == 98:             # integer_ext
            if dataLen >= 5:
                i = OtpInt((ord(data[1]) << 24) +
                           (ord(data[2]) << 16) +
                           (ord(data[3]) <<  8) +
                           (ord(data[4]) <<  0))
                return (i, data[5:])

        elif data[0] == 98:             # float_ext
            if dataLen >= 27:
                floatData = data[1:27]
                f = OtpFloat(string.atof(floatData))
                return (f, data[27:])
        elif data[0] == 100:            # atom_ext
            if dataLen >= 3:
                atomLen = (ord(data[1]) << 8) + ord(data[2])
                if dataLen >= 3 + atomLen:
                    atomText = data[3:3 + atomLen]
                    a = OtpAtom(atomText)
                    return (a, data[3 + atomLen:])

        return (None, data)
