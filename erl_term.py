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

def ErlNumber(number):
    return number

_atom_cache = {}
class ErlAtom:
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

def IsErlAtom(term):
    return type(term) == types.InstanceType and isinstance(term, ErlAtom)


            
class ErlRef:
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id                    # id is either an int or a list of ints
        self.creation = creation
    def __repr__(self):
        return "<erl-ref: node=%s, id=%d, creation=%d>" % \
               (`self.node`, self.id, self.creation)

def IsErlRef(term):
    return type(term) == types.InstanceType and isinstance(term, ErlRef)


class ErlPort:
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id
        self.creation = creation
    def __repr__(self):
        return "<erl-port: node=%s, id=%d, creation=%d>" % \
               (`self.node`, self.id, self.creation)

def IsErlPort(term):
    return type(term) == types.InstanceType and isinstance(term, ErlPort)

class ErlPid:
    def __init__(self, node, id, serial, creation):
        self.node = node
        self.id = id
        self.serial = serial
        self.creation = creation
    def __repr__(self):
        return "<erl-pid: node=%s, id=%d, serial=%d, creation=%d>" % \
               (`self.node`, self.id, self.serial, self.creation)

def IsErlPid(term):
    return type(term) == types.InstanceType and isinstance(term, ErlPid)

def ErlTuple(elementsAsList):
    return tuple(elementsAsList)

def ErlList(elements):
    return elements

class ErlBinary:
    def __init__(self, contets):
        self._contents = contets        #  a string
    def __repr__(self):
        return "<erl-binary: size=%d>" % len(self._contents)

def IsErlBinary(term):
    return type(term) == types.InstanceType and isinstance(term, ErlBinary)

def ErlString(s):
    return s

class ErlFun:
    def __init__(self, pid, module, index, uniq, freeVars):
        self.pid = pid
        self.module = module
        self.index = index
        self.uniq = uniq
        self.freeVars = freeVars
    def __repr__(self):
        return "<erl-fun: pid=%s, module=%s, index=%d, uniq=%d, freeVars=%s>"%\
               (`self.pid`, `self.module`, self.index, self.uniq,
                `self.freeVars`)


def IsErlFun(term):
    return type(term) == types.InstanceType and isinstance(term, ErlFun)

###
### UNPACKING
###
def BinaryToTerm(binary):
    (term, remaining) = _UnpackOneTerm(binary)
    if len(remaining) != 0:
        raise "BinaryToTerm: Extraneous data in binary"
    return term

def BinariesToTerms(binary):
    (term, remaining) = BufToTerm(binary)
    if len(remaining) != 0:
        raise "BinaryToTerm: Extraneous data in binary"
    return term

def BufToTerm(data):
    unpackedTerms = []
    inputData = data
    while 1:
        (unpackedTerm, remainingData) = _UnpackOneTermTop(inputData)
        if unpackedTerm == None:
            return (unpackedTerms, remainingData)
        unpackedTerms.append(unpackedTerm)
        inputData = remainingData

def _UnpackOneTermTop(data):
    if len(data) == 0: 
        return (None, data)
    if data[0] != chr(131):
        return (None, data)
    return _UnpackOneTerm(data[1:])


def _UnpackOneTerm(data):
    dataLen = len(data)

    if len(data) == 0:
        return (None, data)

    data0 = ord(data[0])
    if data0 == 97:                 # small_integer_ext
        if dataLen < 2:
            return (None, data)
        n = _ReadInt1(data[1])
        return (ErlNumber(n), data[2:])

    elif data0 == 98:               # integer_ext
        if dataLen < 5:
            return (None, data)
        n = _ReadInt4(data[1:5])
        return (ErlNumber(i), data[5:])

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
        return (ErlNumber(f), data[32:])

    elif data0 == 100:              # atom_ext
        if dataLen < 3:
            return (None, data)
        atomLen = _ReadInt2(data[1:3])
        if dataLen < 3 + atomLen:
            return (None, data)
        atomText = data[3:3 + atomLen]
        return (ErlAtom(atomText), data[3 + atomLen:])

    elif data0 == 101:              # reference_ext
        (node, remainingData) = _UnpackOneTerm(data[1:])
        if node == None:
            return (None, data)
        if len(remainingData) < 5:
            return (None, data)
        id = _ReadId(remainingData[0:4])
        creation = _ReadCreation(remainingData[4])
        return (ErlRef(node, id, creation), remainingData[5:])

    elif data0 == 102:              # port_ext
        (node, remainingData) = _UnpackOneTerm(data[1:])
        if node == None:
            return (None, data)
        if len(remainingData) < 5:
            return (None, data)
        id = _ReadId(remainingData[0:4])
        creation = _ReadCreation(remainingData[4])
        return (ErlPort(node, id, creation), remainingData[5:])

    elif data0 == 103:              # pid_ext
        (node, remainingData) = _UnpackOneTerm(data[1:])
        if node == None:
            return (None, data)
        if len(remainingData) < 9:
            return (None, data)
        id = _ReadId(remainingData[0:4], 15)
        serial = _ReadInt4(remainingData[4:8])
        creation = _ReadCreation(remainingData[8])
        return (ErlPid(node, id, serial, creation), remainingData[9:])

    elif data0 == 104:              # small_tuple_ext
        if dataLen < 2:
            return (None, data)
        arity = _ReadInt1(data[1])
        (elements, remainingData) = _UnpackTermSeq(arity, data[2:])
        if elements == None:
            return (None, data)
        return (ErlTuple(elements), remainingData)

    elif data0 == 105:              # large_tuple_ext
        if dataLen < 5:
            return (None, data)
        arity = _ReadInt4(data[1:5])
        (elements, remainingData) = _UnpackTermSeq(arity, data[5:])
        if elements == None:
            return (None, data)
        return (ErlTuple(elements), remainingData)

    elif data0 == 106:              # nil_ext:
        return (ErlList([]), data[1:])

    elif data0 == 107:              # string_ext
        if dataLen < 3:
            return (None, data)
        strlen = _ReadInt2(data[1:3])
        if dataLen < 3 + strlen:
            return (None, data)
        s = data[3:3 + strlen]
        return (ErlString(s), data[3 + strlen:])

    elif data0 == 108:              # list_ext
        if dataLen < 5:
            return (None, data)
        arity = _ReadInt4(data[1:5])
        (elements, remainingData) = _UnpackTermSeq(arity, data[5:])
        if elements == None:
            return (None, data)
        return (ErlList(elements), remainingData)

    elif data0 == 109:              # binary_ext
        if dataLen < 5:
            return (None, data)
        binlen = _ReadInt4(data[1:5])
        if dataLen < 5 + binlen:
            return (None, data)
        s = data[5:5 + binlen]
        return (ErlBinary(s), data[5 + binlen:])

    elif data0 == 110:              # small_big_ext
        if dataLen < 2:
            return (None, data)
        n = _ReadInt1(data[1])
        if dataLen < 2 + 1 + n:
            return (None, data)
        sign = _ReadInt1(data[2])
        bignum = 0L
        for i in range(n):
            d = _ReadInt1(data[3 + n - i - 1])
            bignum = bignum * 256L + long(d)
        if sign:
            bignum = bignum * -1L
        return (ErlNumber(bignum), data[3 + n:])

    elif data0 == 111:              # large_big_ext
        if dataLen < 5:
            return (None, data)
        n = _ReadInt4(data[1:5])
        if dataLen < 5 + 1 + n:
            return (None, data)
        sign = _ReadInt1(data[5])
        bignum = 0L
        for i in range(n):
            d = _ReadInt1(data[6 + n - i - 1])
            bignum = bignum * 256L + long(d)
        if sign:
            bignum = bignum * -1L
        return (ErlNumber(bignum), data[6 + n:])

    elif data0 == 78:               # new_cache
        if dataLen < 4:
            return (None, data)
        index = _ReadInt1(data[1])
        atomLen = _ReadInt2(data[2:4])
        if dataLen < 4 + atomLen:
            return (None, data)
        atomText = data[4:4 + atomLen]
        return (ErlAtom(atomText, cache=index), data[4 + atomLen:])

    elif data0 == 67:               # cached_atom
        if dataLen < 2:
            return (None, data)
        index = _ReadInt1(data[1])
        return (ErlAtom(None, cache=index), data[2:])

    elif data0 == 114:              # new_reference_ext
        if dataLen < 3:
            return (None, data)
        idLen = _ReadInt2(data[1:3])
        (node, remainingData) = _UnpackOneTerm(data[3:])
        if node == None:
            return (None, data)
        nprim = 4 * idLen
        if len(remainingData) < 1 + nprim:
            return (None, data)
        creation = _ReadCreation(remainingData[0])
        remainingData = remainingData[1:]
        id0 = _ReadId(remainingData[0:4])
        id = [id0]
        remainingData = remainingData[4:]
        for i in idLen:
            i = _ReadInt4(remainingData[0:4])
            remainingData = remainingData[4:]
        return (ErlRef(node, creation, id), remainingData)

    elif data0 == 117:              # fun_ext
        if dataLen < 5:
            return (None, data)
        freevarsLen = _ReadInt4(data[1:5])
        (pid, remainingData1) = _UnpackOneTerm(data[5:])
        if pid == None:
            return (None, data)
        (module, remainingData2) = _UnpackOneTerm(remainingData1)
        if module == None:
            return (None, data)
        (index, remainingData3)  = _UnpackOneTerm(remainingData2)
        if index == None:
            return (None, data)
        (uniq, remainingData4) = _UnpackOneTerm(remainingData3)
        if uniq == None:
            return (None, data)
        (freeVars, remainingData5) = _UnpackTermSeq(freevarsLen,
                                                         remainingData4)
        if freeVars == None:
            return (None, data)
        return (ErlFun(pid, module, index, uniq, freeVars),
                remainingData5)

    else:
        print "Bad tag %s" % `data0`


    return (None, data)

def _UnpackTermSeq(numTerms, data):
    seq = []
    remainingData = data
    for i in range(numTerms):
        (term, newRemainingData) = _UnpackOneTerm(remainingData)
        if term == None:
            return (None, data)
        seq.append(term)
        remainingData = newRemainingData
    return (seq, remainingData)

def _ReadId(s, maxSignificantBits = 18):
    return _ReadInt4(s) & ((1 << maxSignificantBits) - 1)

def _ReadCreation(s):
    return _ReadInt1(s) & ((1 << 2) - 1)

def _ReadInt1(s):
    return erl_common.ReadInt1(s)

def _ReadInt2(s):
    return erl_common.ReadInt2(s)

def _ReadInt4(s):
    return erl_common.ReadInt4(s)



###
### PACKING
###
def TermToBinary(term):
    return chr(131) + _PackOneTerm(term)

def _PackOneTerm(term):
    if type(term) == types.StringType:
        return _PackString(term)
    elif type(term) == types.ListType:
        return _PackList(term)
    elif type(term) == types.TupleType:
        return _PackTuple(term)
    elif type(term) == types.LongType:
        return _PackLong(term)
    elif type(term) == types.FloatType:
        return _PackFloat(term)
    elif type(term) == types.IntType:
        return _PackInt(term)
    elif IsErlAtom(term):
        return _PackAtom(term)
    elif IsErlRef(term):
        return _PackRef(term)
    elif IsErlPort(term):
        return _PackPort(term)
    elif IsErlPid(term):
        return _PackPid(term)
    elif IsErlBinary(term):
        return _PackBinary(term)
    elif IsErlFun(term):
        return _PackFun(term)
    else:
        print "Term=%s" % `term`
        raise "Can't pack value of type %s" % `type(term)`

    
def _PackString(term):
    if len(term) == 0:
        return PackList([])
    elif len(term) <= 65535:
        return _PackInt1(107) + _PackInt2(len(term)) + term
    else:
        return PackList(map(lambda c: ord(c), term))

def _PackList(term):
    if len(term) == 0:
        return _PackInt1(106)
    else:
        packedData = ""
        for elem in term:
            packedData = packedData + _PackOneTerm(elem)
        return _PackInt1(108) + _PackInt4(len(term)) + packedData

def _PackTuple(term):
    if len(term) < 256:
        head = _PackInt1(104) + _PackInt1(len(term))
    else:
        head = _PackInt1(105) + _PackInt4(len(term))
    packedData = head
    for elem in term:
        packedData = packedData + _PackOneTerm(elem)
    return packedData


def _PackLong(term):
    if -long(0x7fffffff) - 1 <= term <= long(0x7fffffff):
        return _PackInt(term)
    else:
        numBytesNeeded = int(math.log(term) / math.log(256)) + 1
        if numBytesNeeded > 1:
            return _PackInt1(111) + _PackInt4(numBytesNeeded) + \
                   _PackLongBytes(term, numBytesNeeded)
        else:
            return _PackInt1(110) + _PackInt1(numBytesNeeded) + \
                   _PackLongBytes(term, numBytesNeeded)

def _PackLongBytes(term, numBytesNeeded):
    if term < 0:
        sign = _PackInt(1)
    else:
        sign = _PackInt(0)
    bignum = term
    bignumBytes = sign
    for i in range(numBytesNeeded):
        bignumBytes = bignumBytes + _PackInt1(bignum & 255)
        bignum = bignum >> 8
    return bignumBytes

def _PackFloat(term):
    floatStr = "%.20e" % term
    nullPadStr = _PackInt1(0) * (31 - len(floatStr))
    return _PackInt1(99) + floatStr + nullPadStr

def _PackInt(term):
    if 0 <= term < 256:
        return _PackInt1(97) + _PackInt1(term)
    else:
        return _PackInt1(98) + _PackInt4(term)

def _PackAtom(term):
    atomText = term.atomText
    return _PackInt1(100) + _PackInt2(len(atomText)) + atomText

def _PackRef(term):
    if type(term.id) == types.ListType:
        return _PackNewReferenceExt(term)
    else:
        return _PackOldReferenceExt(term)

def _PackNewReferenceExt(term):
    node = _PackOneTerm(term.node)
    creation = _PackCreation(term.creation)
    id0 = _PackId(term.id[0])
    ids = id0
    for id in term.id[1:]:
        ids = ids + _PackInt4(id)
    return _PackInt1(114) + _PackInt2(len(term.id)) + \
           node + creation + ids

def _PackNewReferenceExt(term):
    node = _PackOneTerm(term.node)
    id = _PackId(term.id)
    creation = _PackCreation(term.creation)
    return _PackInt1(101) + node + id + creation

def _PackPort(term):
    node = _PackOneTerm(term.node)
    id = _PackId(term.id)
    creation = _PackCreation(term.creation)
    return _PackInt1(102) + node + id + creation

def _PackPid(term):
    node = _PackOneTerm(term.node)
    id = _PackId(term.id, 15)
    serial = _PackInt4(term.serial)
    creation = _PackCreation(term.creation)
    return _PackInt1(102) + node + id + serial + creation

def _PackBinary(term):
    return _PackInt1(109) + _PackInt4(len(term.contents)) + term.contents

def _PackFun(term):
    numFreeVars = _PackInt4(len(term.freeVars))
    pid = _PackPid(term.pid)
    module = _PackAtom(term.module)
    index = _PackInt(term.index)
    uniq = _PackInt(term.uniq)
    freeVars = ""
    for freeVar in term.freeVars:
        freeVars = freeVars + _PackOneTerm(freeVar)
    return _PackInt4(117) + numFreeVars + \
           pid + module + index + uniq + freeVars


def _PackId(i, maxSignificantBits=18):
    return _PackInt4(i & ((1 << maxSignificantBits) - 1))

def _PackCreation(i):
    return _PackInt1(i & ((1 << 2) - 1))

def _PackInt1(i):
    return erl_common.PackInt1(i)

def _PackInt2(i):
    return erl_common.PackInt2(i)

def _PackInt4(i):
    return erl_common.PackInt4(i)

