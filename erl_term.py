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


import common
import erl_common

def OtpNumber(number):
    return number

_atom_cache = {}
class OtpAtom:
    def __init__(self, atomText, cache=-1):
        global _atom_cache
        if atomText == None and cache != -1:
            if _atom_cache.has_key(cache):
                self.atomText = _atom_cache[cache]
            else:
                raise "No such cached atom: %s" % `cache`
        elif atomText != None and cache != -1:
            self.atomText = atomText
            _atom_cache[cache] = atomText
        else:
            self.atomText = atomText
    def __repr__(self):
        return "<erl-atom: %s>" % `self.atomText`

            
class OtpRef:
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id                    # id is either an int or a list of ints
        self.creation = creation
    def __repr__(self):
        return "<erl-ref, node=%s, id=%d, creation=%d>" % \
               (`self.node`, self.id, self.creation)

class OtpPort:
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id
        self.creation = creation
    def __repr__(self):
        return "<erl-port, node=%s, id=%d, creation=%d>" % \
               (`self.node`, self.id, self.creation)

class OtpPid:
    def __init__(self, node, id, serial, creation):
        self.node = node
        self.id = id
        self.serial = serial
        self.creation = creation
    def __repr__(self):
        return "<erl-pid, node=%s, id=%d, serial=%d, creation=%d>" % \
               (`self.node`, self.id, self.serial, self.creation)

def OtpTuple(elementsAsList):
    return tuple(elementsAsList)

def OtpList(elements):
    return elements

class OtpBinary:
    def __init__(self, contets):
        self._contents = contets        #  a string
    def __repr__(self):
        return "<erl-binary, size=%d>" % len(self._contents)

def OtpString(s):
    return s

class OtpFun:
    def __init__(self, pid, module, index, uniq, freeVars):
        self.pid = pid
        self.module = module
        self.index = index
        self.uniq = uniq
        self.freeVars = freeVars
    def __repr__(self):
        return "<erl-fun, pid=%s, module=%s, index=%d, uniq=%d, freeVars=%s>"%\
               (`self.pid`, `self.module`, self.index, self.uniq,
                `self.freeVars`)

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
        while 1:
            (unpackedTerm, remainingData) = self._UnpackOneTermTop(inputData)
            if unpackedTerm == None:
                return (unpackedTerms, remainingData)
            unpackedTerms.append(unpackedTerm)
            inputData = remainingData

    def _UnpackOneTermTop(self, data):
        if len(data) == 0: 
            return (None, data)
        if data[0] != chr(131):
            return (None, data)
        return self._UnpackOneTerm(data[1:])


    def _UnpackOneTerm(self, data):
        dataLen = len(data)

        if len(data) == 0:
            return (None, data)

        data0 = ord(data[0])
        if data0 == 97:                 # small_integer_ext
            if dataLen < 2:
                return (None, data)
            n = self._ReadInt1(data[1])
            return (OtpNumber(n), data[2:])

        elif data0 == 98:               # integer_ext
            if dataLen < 5:
                return (None, data)
            n = self._ReadInt4(data[1:5])
            return (OtpNumber(i), data[5:])

        elif data0 == 99:               # float_ext
            if dataLen < 32:
                return (None, data)
            floatData = data[1:32]
            try:
                nullIndex = string.index(floatData, chr(0))
                floatStr = floatData[0:nullIndex]
            except ValueError:
                floatStr = floatData
            f = string.atof(floatStr)
            return (OtpNumber(f), data[32:])

        elif data0 == 100:              # atom_ext
            if dataLen < 3:
                return (None, data)
            atomLen = self._ReadInt2(data[1:3])
            if dataLen < 3 + atomLen:
                return (None, data)
            atomText = data[3:3 + atomLen]
            return (OtpAtom(atomText), data[3 + atomLen:])

        elif data0 == 101:              # reference_ext
            (node, remainingData) = self._UnpackOneTerm(data[1:])
            if node == None:
                return (None, data)
            if len(remainingData) < 5:
                return (None, data)
            id = self._ReadId(remainingData[0:4])
            creation = self._ReadCreation(remainingData[4])
            return (OtpRef(node, id, creation), remainingData[5:])

        elif data0 == 102:              # port_ext
            (node, remainingData) = self._UnpackOneTerm(data[1:])
            if node == None:
                return (None, data)
            if len(remainingData) < 5:
                return (None, data)
            id = self._ReadId(remainingData[0:4])
            creation = self._ReadCreation(remainingData[4])
            return (OtpPort(node, id, creation), remainingData[5:])

        elif data0 == 103:              # pid_ext
            (node, remainingData) = self._UnpackOneTerm(data[1:])
            if node == None:
                return (None, data)
            if len(remainingData) < 9:
                return (None, data)
            id = self._ReadId(remainingData[0:4], 15)
            serial = self._ReadInt4(remainingData[4:8])
            creation = self._ReadCreation(remainingData[8])
            return (OtpPid(node, id, serial, creation), remainingData[9:])

        elif data0 == 104:              # small_tuple_ext
            if dataLen < 2:
                return (None, data)
            arity = self._ReadInt1(data[1])
            (elements, remainingData) = self._UnpackTermSeq(arity, data[2:])
            if elements == None:
                return (None, data)
            return (OtpTuple(elements), remainingData)

        elif data0 == 105:              # large_tuple_ext
            if dataLen < 5:
                return (None, data)
            arity = self._ReadInt4(data[1:5])
            (elements, remainingData) = self._UnpackTermSeq(arity, data[5:])
            if elements == None:
                return (None, data)
            return (OtpTuple(elements), remainingData)

        elif data0 == 106:              # nil_ext:
            return (OtpList([]), data[1:])
        
        elif data0 == 107:              # string_ext
            if dataLen < 3:
                return (None, data)
            strlen = self._ReadInt2(data[1:3])
            if dataLen < 3 + strlen:
                return (None, data)
            s = data[3:3 + strlen]
            return (OtpString(s), data[3 + strlen:])

        elif data0 == 108:              # list_ext
            if dataLen < 5:
                return (None, data)
            arity = self._ReadInt4(data[1:5])
            (elements, remainingData) = self._UnpackTermSeq(arity, data[5:])
            if elements == None:
                return (None, data)
            return (OtpList(elements), remainingData)

        elif data0 == 109:              # binary_ext
            if dataLen < 5:
                return (None, data)
            binlen = self._ReadInt4(data[1:5])
            if dataLen < 5 + binlen:
                return (None, data)
            s = data[5:5 + binlen]
            return (OtpBinary(s), data[5 + binlen:])

        elif data0 == 110:              # small_big_ext
            if dataLen < 2:
                return (None, data)
            n = self._ReadInt1(data[1])
            if dataLen < 2 + 1 + n:
                return (None, data)
            sign = self._ReadInt1(data[2])
            bignum = 0L
            for i in range(n):
                d = self._ReadInt1(data[3 + n - i - 1])
                bignum = bignum * 256L + long(d)
            if sign:
                bignum = bignum * -1L
            return (OtpNumber(bignum), data[3 + n:])

        elif data0 == 111:              # large_big_ext
            if dataLen < 5:
                return (None, data)
            n = self._ReadInt4(data[1:5])
            if dataLen < 5 + 1 + n:
                return (None, data)
            sign = self._ReadInt1(data[5])
            bignum = 0L
            for i in range(n):
                d = self._ReadInt1(data[6 + n - i - 1])
                bignum = bignum * 256L + long(d)
            if sign:
                bignum = bignum * -1L
            return (OtpNumber(bignum), data[6 + n:])

        elif data0 == 78:               # new_cache
            if dataLen < 4:
                return (None, data)
            index = self._ReadInt1(data[1])
            atomLen = self._ReadInt2(data[2:4])
            if dataLen < 4 + atomLen:
                return (None, data)
            atomText = data[4:4 + atomLen]
            return (OtpAtom(atomText, cache=index), data[4 + atomLen:])

        elif data0 == 67:               # cached_atom
            if dataLen < 2:
                return (None, data)
            index = self._ReadInt1(data[1])
            return (OtpAtom(None, cache=index), data[2:])

        elif data0 == 114:              # new_reference_ext
            if dataLen < 3:
                return (None, data)
            idLen = self._ReadInt2(data[1:3])
            (node, remainingData) = self._UnpackOneTerm(data[3:])
            if node == None:
                return (None, data)
            nprim = 4 * idLen
            if len(remainingData) < 1 + nprim:
                return (None, data)
            creation = self._ReadCreation(remainingData[0])
            remainingData = remainingData[1:]
            id0 = self._ReadId(remainingData[0:4])
            id = [id0]
            remainingData = remainingData[4:]
            for i in idLen:
                i = self._ReadInt4(remainingData[0:4])
                remainingData = remainingData[4:]
            return (OtpRef(node, creation, id), remainingData)

        elif data0 == 117:              # fun_ext
            if dataLen < 5:
                return (None, data)
            freevarsLen = self._ReadInt4(data[1:5])
            (pid, remainingData1) = self._UnpackOneTerm(data[5:])
            if pid == None:
                return (None, data)
            (module, remainingData2) = self._UnpackOneTerm(remainingData1)
            if module == None:
                return (None, data)
            (index, remainingData3)  = self._UnpackOneTerm(remainingData2)
            if index == None:
                return (None, data)
            (uniq, remainingData4) = self._UnpackOneTerm(remainingData3)
            if uniq == None:
                return (None, data)
            (freeVars, remainingData5) = self._UnpackTermSeq(freevarsLen,
                                                             remainingData4)
            if freeVars == None:
                return (None, data)
            return (OtpFun(pid, module, index, uniq, freeVars),
                    remainingData5)

        else:
            print "Bad tag %s" % `data0`

            
        return (None, data)

    def _UnpackTermSeq(self, numTerms, data):
        seq = []
        remainingData = data
        for i in range(numTerms):
            (term, newRemainingData) = self._UnpackOneTerm(remainingData)
            if term == None:
                return (None, data)
            seq.append(term)
            remainingData = newRemainingData
        return (seq, remainingData)

    def _PackOneTerm(self, term):
        pass

    def _ReadId(self, s, maxSignificantBits = 18):
        return self._ReadInt4(s) & ((1 << maxSignificantBits) - 1)

    def _ReadCreation(self, s):
        return self._ReadInt1(s) & ((1 << 2) - 1)

    def _ReadInt1(self, s):
        return erl_common.ReadInt1(s)
    
    def _ReadInt2(self, s):
        return erl_common.ReadInt2(s)
    
    def _ReadInt4(self, s):
        return erl_common.ReadInt4(s)

class Packer:
    def __init__(self):
        self.Reset()

    def Reset(self):
        self.state = 0


    def Pack(self, term):
        return self._PackInt1(131) + self._PackOneTerm(term)

    def _PackOneTerm(self, term):
        if type(term) == types.StringType:
            return self._PackString(term)
        elif type(term) == types.ListType:
            return self._PackList(term)
        elif type(term) == types.TupleType:
            return self._PackTuple(term)
        elif type(term) == types.LongType:
            return self._PackLong(term)
        elif type(term) == types.FloatType:
            return self._PackFloat(term)
        elif type(term) == types.IntType:
            return self._PackInt(term)
        elif type(term) == types.ClassType:
            if isinstance(term, OtpAtom):
                return self._PackAtom(term)
            elif isinstance(term, OtpRef):
                return self._PackRef(term)
            elif isinstance(term, OtpPort):
                return self._PackPort(term)
            elif isinstance(term, OtpPid):
                return self._PackPid(term)
            elif isinstance(term, OtpBinary):
                return self._PackBinary(term)
            elif isinstance(term, OtpFun):
                return self._PackFun(term)
            else:
                raise "Can't pack instance of type %s" % `type(term)`
        else:
            raise "Can't pack value of type %s" % `type(term)`

    
    def _PackString(self, term):
        if len(term) == 0:
            return self.PackList([])
        elif len(term) <= 65535:
            return self._PackInt1(107) + self._PackInt2(len(term)) + term
        else:
            return self.PackList(map(lambda c: ord(c), term))

    def _PackList(self, term):
        if len(term) == 0:
            return self._PackInt1(106)
        else:
            packedData = ""
            for elem in term:
                packedData = packedData + self._PackOneTerm(elem)
            return self._PackInt1(108) + self._PackInt4(len(term)) + packedData

    def _PackTuple(self, term):
        if len(term) < 256:
            head = self._PackInt1(104) + self._PackInt1(len(term))
        else:
            head = self._PackInt1(105) + self._PackInt4(len(term))
        packedData = head
        for elem in term:
            packedData = packedData + self._PackOneTerm(elem)
        return packedData


    def _PackLong(self, term):
        if -long(0x7fffffff) - 1 <= term <= long(0x7fffffff):
            return self._PackInt(term)
        else:
            numBytesNeeded = int(math.log(term) / math.log(256)) + 1
            if numBytesNeeded > 1:
                return self._PackInt1(111) + self._PackInt4(numBytesNeeded) + \
                       self._PackLongBytes(term, numBytesNeeded)
            else:
                return self._PackInt1(110) + self._PackInt1(numBytesNeeded) + \
                       self._PackLongBytes(term, numBytesNeeded)

    def _PackLongBytes(self, term, numBytesNeeded):
        if term < 0:
            sign = self._PackInt(1)
        else:
            sign = self._PackInt(0)
        bignum = term
        bignumBytes = sign
        for i in range(numBytesNeeded):
            bignumBytes = bignumBytes + self._PackInt1(bignum & 255)
            bignum = bignum >> 8
        return bignumBytes

    def _PackFloat(self, term):
        floatStr = "%.20e" % term
        nullPadStr = self._PackInt1(0) * (31 - len(floatStr))
        return self._PackInt1(99) + floatStr + nullPadStr

    def _PackInt(self, term):
        if 0 <= term < 256:
            return self._PackInt1(97) + self._PackInt1(term)
        else:
            return self._PackInt1(98) + self._PackInt4(term)

    def _PackAtom(self, term):
        pass

    def _PackRef(self, term):
        if type(term.id) == types.ListType:
            return self._PackNewReferenceExt(term)
        else:
            return self._PackOldReferenceExt(term)

    def _PackNewReferenceExt(self, term):
        node = self._PackOneTerm(term.node)
        creation = self._PackCreation(term.creation)
        id0 = self._PackId(term.id[0])
        ids = id0
        for id in term.id[1:]:
            ids = ids + self._PackInt4(id)
        return self._PackInt1(114) + self._PackInt2(len(term.id)) + \
               node + creation + ids

    def _PackNewReferenceExt(self, term):
        node = self._PackOneTerm(term.node)
        id = self._PackId(term.id)
        creation = self._PackCreation(term.creation)
        return self._PackInt1(101) + node + id + creation

    def _PackPort(self, term):
        node = self._PackOneTerm(term.node)
        id = self._PackId(term.id)
        creation = self._PackCreation(term.creation)
        return self._PackInt1(102) + node + id + creation

    def _PackPid(self, term):
        node = self._PackOneTerm(term.node)
        id = self._PackId(term.id, 15)
        serial = self._PackInt4(term.serial)
        creation = self._PackCreation(term.creation)
        return self._PackInt1(102) + node + id + serial + creation

    def _PackBinary(self, term):
        return self._PackInt1(109) + self._PackInt4(len(term.contents)) + \
               term.contents

    def _PackFun(self, term):
        numFreeVars = self._PackInt4(len(term.freeVars))
        pid = self._PackPid(term.pid)
        module = self._PackAtom(term.module)
        index = self._PackInt(term.index)
        uniq = self._PackInt(term.uniq)
        freeVars = ""
        for freeVar in term.freeVars:
            freeVars = freeVars + self._PackOneTerm(freeVar)
        return self._PackInt4(117) + numFreeVars + \
               pid + module + index + uniq + freeVars


    def _PackId(self, i, maxSignificantBits=18):
        return self._PackInt4(i & ((1 << maxSignificantBits) - 1))

    def _PackCreation(self, i):
        return self._PackInt1(i & ((1 << 2) - 1))

    def _PackInt1(self, i):
        return erl_common.PackInt1(i)

    def _PackInt2(self, i):
        return erl_common.PackInt2(i)

    def _PackInt4(self, i):
        return erl_common.PackInt4(i)

