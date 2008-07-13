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

### erl_term.py -- python types/classes for all erlang types.
###                Also: packing and unpacking all types to/from
###                      the erlang external binary format

### Log of fixes:
###
### * integer unpacking by Jimmy Olgeni <olgeni@uli.it>
###
### * string packing by Jimmy Olgeni <olgeni@uli.it>
###
### * reference unpacking, independently by Nigel Head <nigel.head@esa.int>
###                                and Tomas Abrahamsson <tab@lysator.liu.se>
###
### * bignum packing by Tomas Abrahamsson <tab@lysator.liu.se>
###
### * error handling changed a bit: throws exceptions instead of silently
###   hiding some nasties by Nigel Head <nigel.head@esa.int>
###
### * deals with received list tails better for non-proper lists
###                       by Nigel Head <nigel.head@esa.int>

import os
import sys
import math
import types
import string


from py_interface import erl_common

def ErlNumber(number):
    return number

_atom_cache = {}
class ErlAtom:
    """An Erlang atom. The following attributes are defined:
    atomText = string
    """
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
    def equals(self, other):
        return self.atomText == other.atomText
    def __str__(self):
        return self.atomText

def IsErlAtom(term):
    """Checks whether a term is an Erlang atom or not."""
    return type(term) == types.InstanceType and isinstance(term, ErlAtom)

            
class ErlRef:
    """An Erlang reference. The following attributes are defined:
    node     = <ErlAtom>
    id       = integer | list(integer)
    creation = integer
    """
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id                    # id is either an int or a list of ints
        self.creation = creation
    def __repr__(self):
        return "<erl-ref: node=%s, id=%s, creation=%d>" % \
               (`self.node`, `self.id`, self.creation)
    def equals(self, other):
        return self.node.equals(other.node) and \
               self.id == other.id and \
               self.creation == other.creation

def IsErlRef(term):
    """Checks whether a term is an Erlang reference or not."""
    return type(term) == types.InstanceType and isinstance(term, ErlRef)


class ErlPort:
    """An Erlang port. The following attributes are defined:
    node     = <ErlAtom>
    id       = integer
    creation = integer
    """
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id
        self.creation = creation
    def __repr__(self):
        return "<erl-port: node=%s, id=%d, creation=%d>" % \
               (`self.node`, self.id, self.creation)
    def equals(self, other):
        return self.node.equals(other.node) and \
               self.id == other.id and \
               self.creation == other.creation

def IsErlPort(term):
    """Checks whether a term is an Erlang reference or not."""
    return type(term) == types.InstanceType and isinstance(term, ErlPort)

class ErlPid:
    """An Erlang process id. The following attributes are defined:
    node     = <ErlAtom>
    id       = integer
    serial   = integer
    creation = integer
    """
    def __init__(self, node, id, serial, creation):
        self.node = node
        self.id = id
        self.serial = serial
        self.creation = creation
    def __repr__(self):
        return "<erl-pid: node=%s, id=%d, serial=%d, creation=%d>" % \
               (`self.node`, self.id, self.serial, self.creation)
    def equals(self, other):
        return self.node.equals(other.node) and \
               self.id == other.id and \
               self.serial == other.serial and \
               self.creation == other.creation

def IsErlPid(term):
    """Checks whether a term is an Erlang process id or not."""
    return type(term) == types.InstanceType and isinstance(term, ErlPid)

def ErlTuple(elementsAsList):
    """An Erlang tuple. This maps to a python tuple."""
    return tuple(elementsAsList)

def ErlList(elements):
    """An Erlang list. This maps to a python list."""
    return elements
    
class ErlImproperList:
    """An improper erlang list (one where the tail is not []).
    Can be iterated over to get the elements, by default will include
    the tail as the last element."""
    def __init__(self,elements,tail,useTail=1):
        self.elements = elements
        self.tail = tail
        # if true, we include tail element in iterations on this list
        self.iterOnTail = useTail
    def __repr__(self):
        return "<erl-improper-list: head=%s, tail=%s>" % (`self.elements`,`self.tail`)
    def equals(self,other):
        return self.elements==other.elements and self.tail==other.tail
    def __getitem__(self,key):
        try:
            return self.elements[key]
        except IndexError:
            if self.iterOnTail and key==len(self.elements):
                return self.tail
            raise IndexError
        
def IsErlImproperList(term):
    return type(term)== types.InstanceType and isinstance(term, ErlImproperList)
    
class ErlBinary:
    """An Erlang binary. The following attributes are defined:
    contents = string
    """
    def __init__(self, contents):
        self.contents = contents        #  a string
    def __repr__(self):
        return "<erl-binary: size=%d>" % len(self.contents)
    def equals(self, other):
        return self.contents == other.contents

def IsErlBinary(term):
    """Checks whether a term is an Erlang binary or not."""
    return type(term) == types.InstanceType and isinstance(term, ErlBinary)

def ErlString(s):
    """An Erlang list. This maps to a python string."""
    return s

class ErlFun:
    """An Erlang process id. The following attributes are defined:
    pid      = <ErlPid>
    module   = <ErlAtom>
    index    = integer
    uniq     = integer
    freeVars = list(term)
    """
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
    def equals(self, other):
        return self.pid.equals(other.pid) and \
               self.module.equals(other.module) and \
               self.index == other.index and \
               self.uniq == other.uniq and \
               self.freeVars == other.freeVars


def IsErlFun(term):
    """Checks whether a term is an Erlang function or not."""
    return type(term) == types.InstanceType and isinstance(term, ErlFun)

###
### MAGIC tags used in packing/unpacking. See erl_ext_dist.txt
###
MAGIC_VERSION = 131
MAGIC_STRING = 107
MAGIC_NIL = 106
MAGIC_LIST = 108
MAGIC_SMALL_TUPLE = 104
MAGIC_LARGE_TUPLE = 105
MAGIC_LARGE_BIG = 111
MAGIC_SMALL_BIG = 110
MAGIC_FLOAT = 99
MAGIC_SMALL_INTEGER = 97
MAGIC_INTEGER = 98
MAGIC_ATOM = 100
MAGIC_NEW_REFERENCE = 114
MAGIC_REFERENCE = 101
MAGIC_PORT = 102
MAGIC_PID = 103
MAGIC_BINARY = 109
MAGIC_FUN = 117
MAGIC_NEW_CACHE = 78
MAGIC_CACHED_ATOM = 67

###
### UNPACKING
###
def BinaryToTerm(binary):
    """Unpack a binary to a term.

    BINARY = string

    Returns: term
    Throws:  "BinaryToTerm: Extraneous data in binary"
    """
    try:
        (term, remaining) = _UnpackOneTermTop(binary)
    except:
        raise "BinaryToTerm: Panic -- invalid binary received?"
    if len(remaining) != 0:
        raise "BinaryToTerm: Extraneous data in binary"
    return term

def BinariesToTerms(binary):
    """Unpack a binary/binaries to term(s).
    This is mainly for use by the erl_node_conn, where, in some cases,
    two or more terms are packed together.

    BINARY = string

    Returns: list(term)
    Throws:  "BinaryToTerm: Extraneous data in binary"
    """
    try:
        (terms, remaining) = BufToTerm(binary)
    except:
        raise "BinariesToTerms: Panic -- invalid binary received?"
    if len(remaining) != 0:
        raise "BinariesToTerms: Extraneous data in binary"
    return terms

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
    if data[0] != chr(MAGIC_VERSION):
        return (None, data)
    return _UnpackOneTerm(data[1:])


def _UnpackOneTerm(data):
    dataLen = len(data)

    if len(data) == 0:
        return (None, data)

    data0 = ord(data[0])

    if data0 == MAGIC_SMALL_INTEGER:
        n = _ReadInt1(data[1])
        return (ErlNumber(n), data[2:])

    elif data0 == MAGIC_INTEGER:
        n = _ReadInt4(data[1:5])
        return (ErlNumber(n), data[5:])

    elif data0 == MAGIC_FLOAT:
        floatData = data[1:32]
        try:
            nullIndex = string.index(floatData, chr(0))
            floatStr = floatData[0:nullIndex]
        except ValueError:
            floatStr = floatData
        f = string.atof(floatStr)
        return (ErlNumber(f), data[32:])

    elif data0 == MAGIC_ATOM:
        atomLen = _ReadInt2(data[1:3])
        atomText = data[3:3 + atomLen]
        return (ErlAtom(atomText), data[3 + atomLen:])

    elif data0 == MAGIC_REFERENCE:
        (node, remainingData) = _UnpackOneTerm(data[1:])
        id = _ReadId(remainingData[0:4])
        creation = _ReadCreation(remainingData[4])
        return (ErlRef(node, id, creation), remainingData[5:])

    elif data0 == MAGIC_PORT:
        (node, remainingData) = _UnpackOneTerm(data[1:])
        id = _ReadId(remainingData[0:4], 28)
        creation = _ReadCreation(remainingData[4])
        return (ErlPort(node, id, creation), remainingData[5:])

    elif data0 == MAGIC_PID:
        (node, remainingData) = _UnpackOneTerm(data[1:])
        id = _ReadId(remainingData[0:4], 28)
        serial = _ReadInt4(remainingData[4:8])
        creation = _ReadCreation(remainingData[8])
        return (ErlPid(node, id, serial, creation), remainingData[9:])

    elif data0 == MAGIC_SMALL_TUPLE:
        arity = _ReadInt1(data[1])
        (elements, remainingData) = _UnpackTermSeq(arity, data[2:])
        return (ErlTuple(elements), remainingData)

    elif data0 == MAGIC_LARGE_TUPLE:
        arity = _ReadInt4(data[1:5])
        (elements, remainingData) = _UnpackTermSeq(arity, data[5:])
        return (ErlTuple(elements), remainingData)

    elif data0 == MAGIC_NIL:
        return (ErlList([]), data[1:])

    elif data0 == MAGIC_STRING:
        strlen = _ReadInt2(data[1:3])
        s = data[3:3 + strlen]
        return (ErlString(s), data[3 + strlen:])

    elif data0 == MAGIC_LIST:
        # get the list head
        arity = _ReadInt4(data[1:5])
        (elements, remainingData) = _UnpackTermSeq(arity, data[5:])
        # now get the list tail (usually this is [] but
        # for not well formed lists it may be any term).
        (tail, newRemainingData) = _UnpackOneTerm(remainingData)
        if tail <> []:
            return (ErlImproperList(elements,tail), newRemainingData)
        return (ErlList(elements), newRemainingData)

    elif data0 == MAGIC_BINARY:
        binlen = _ReadInt4(data[1:5])
        s = data[5:5 + binlen]
        return (ErlBinary(s), data[5 + binlen:])

    elif data0 == MAGIC_SMALL_BIG:
        n = _ReadInt1(data[1])
        sign = _ReadInt1(data[2])
        bignum = 0L
        for i in range(n):
            d = _ReadInt1(data[3 + n - i - 1])
            bignum = bignum * 256L + long(d)
        if sign:
            bignum = bignum * -1L
        return (ErlNumber(bignum), data[3 + n:])

    elif data0 == MAGIC_LARGE_BIG:
        n = _ReadInt4(data[1:5])
        sign = _ReadInt1(data[5])
        bignum = 0L
        for i in range(n):
            d = _ReadInt1(data[6 + n - i - 1])
            bignum = bignum * 256L + long(d)
        if sign:
            bignum = bignum * -1L
        return (ErlNumber(bignum), data[6 + n:])

    elif data0 == MAGIC_NEW_CACHE:
        index = _ReadInt1(data[1])
        atomLen = _ReadInt2(data[2:4])
        atomText = data[4:4 + atomLen]
        return (ErlAtom(atomText, cache=index), data[4 + atomLen:])

    elif data0 == MAGIC_CACHED_ATOM:
        index = _ReadInt1(data[1])
        return (ErlAtom(None, cache=index), data[2:])

    elif data0 == MAGIC_NEW_REFERENCE:
        idLen = _ReadInt2(data[1:3])
        (node, remainingData) = _UnpackOneTerm(data[3:])
        nprim = 4 * idLen
        creation = _ReadCreation(remainingData[0])
        remainingData = remainingData[1:]
        id0 = _ReadId(remainingData[0:4])
        ids = [id0]
        remainingData = remainingData[4:]
        for i in range(idLen-1):
            id = _ReadInt4(remainingData[0:4])
            remainingData = remainingData[4:]
            ids.append(id)
        return (ErlRef(node, ids, creation), remainingData)

    elif data0 == MAGIC_FUN:
        freevarsLen = _ReadInt4(data[1:5])
        (pid, remainingData1) = _UnpackOneTerm(data[5:])
        (module, remainingData2) = _UnpackOneTerm(remainingData1)
        (index, remainingData3)  = _UnpackOneTerm(remainingData2)
        (uniq, remainingData4) = _UnpackOneTerm(remainingData3)
        (freeVars, remainingData5) = _UnpackTermSeq(freevarsLen,remainingData4)
        print "MAGIC_FUN"
        print pid
        print module
        print index
        print uniq
        print freeVars
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
    """Pack a term to a binary.

    TERM = term

    Returns: string
    Throws:  \"Can't pack value of type ...\"
    """
    return chr(MAGIC_VERSION) + _PackOneTerm(term)

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
        return _PackList([])
    elif len(term) <= 65535:
        return _PackInt1(MAGIC_STRING) + _PackInt2(len(term)) + term
    else:
        return _PackList(map(lambda c: ord(c), term))

def _PackList(term):
    if len(term) == 0:
        return _PackInt1(MAGIC_NIL)
    else:
        packedData = ""
        for elem in term:
            packedData = packedData + _PackOneTerm(elem)
        return _PackInt1(MAGIC_LIST) + _PackInt4(len(term)) + packedData + \
               _PackList([])

def _PackTuple(term):
    if len(term) < 256:
        head = _PackInt1(MAGIC_SMALL_TUPLE) + _PackInt1(len(term))
    else:
        head = _PackInt1(MAGIC_LARGE_TUPLE) + _PackInt4(len(term))
    packedData = head
    for elem in term:
        packedData = packedData + _PackOneTerm(elem)
    return packedData


def _PackLong(term):
    if -long(0x7fffffff) - 1 <= term <= long(0x7fffffff):
        return _PackInt(term)
    else:
        numBytesNeeded = int(math.log(term) / math.log(256)) + 1
        if numBytesNeeded > 255:
            return _PackInt1(MAGIC_LARGE_BIG) + \
                   _PackInt4(numBytesNeeded) + \
                   _PackLongBytes(term, numBytesNeeded)
        else:
            return _PackInt1(MAGIC_SMALL_BIG) + \
                   _PackInt1(numBytesNeeded) + \
                   _PackLongBytes(term, numBytesNeeded)

def _PackLongBytes(term, numBytesNeeded):
    if term < 0:
        sign = _PackInt1(1)
    else:
        sign = _PackInt1(0)
    bignum = term
    bignumBytes = sign
    for i in range(numBytesNeeded):
        bignumBytes = bignumBytes + _PackInt1(bignum & 255)
        bignum = bignum >> 8
    return bignumBytes

def _PackFloat(term):
    floatStr = "%.20e" % term
    nullPadStr = _PackInt1(0) * (31 - len(floatStr))
    return _PackInt1(MAGIC_FLOAT) + floatStr + nullPadStr

def _PackInt(term):
    if 0 <= term < 256:
        return _PackInt1(MAGIC_SMALL_INTEGER) + _PackInt1(term)
    else:
        return _PackInt1(MAGIC_INTEGER) + _PackInt4(term)

def _PackAtom(term):
    atomText = term.atomText
    return _PackInt1(MAGIC_ATOM) + _PackInt2(len(atomText)) + atomText

def _PackRef(term):
    if type(term.id) == types.ListType:
        return _PackNewReferenceExt(term)
    else:
        return _PackReferenceExt(term)

def _PackNewReferenceExt(term):
    node = _PackOneTerm(term.node)
    creation = _PackCreation(term.creation)
    id0 = _PackId(term.id[0])
    ids = id0
    for id in term.id[1:]:
        ids = ids + _PackInt4(id)
    return _PackInt1(MAGIC_NEW_REFERENCE) + \
           _PackInt2(len(term.id)) + \
           node + creation + ids

def _PackReferenceExt(term):
    node = _PackOneTerm(term.node)
    id = _PackId(term.id)
    creation = _PackCreation(term.creation)
    return _PackInt1(MAGIC_REFERENCE) + node + id + creation

def _PackPort(term):
    node = _PackOneTerm(term.node)
    id = _PackId(term.id, 28)
    creation = _PackCreation(term.creation)
    return _PackInt1(MAGIC_PORT) + node + id + creation

def _PackPid(term):
    node = _PackOneTerm(term.node)
    id = _PackId(term.id, 28)
    serial = _PackInt4(term.serial)
    creation = _PackCreation(term.creation)
    return _PackInt1(MAGIC_PID) + node + id + serial + creation

def _PackBinary(term):
    return _PackInt1(MAGIC_BINARY) + \
           _PackInt4(len(term.contents)) + \
           term.contents

def _PackFun(term):
    numFreeVars = _PackInt4(len(term.freeVars))
    pid = _PackPid(term.pid)
    module = _PackAtom(term.module)
    index = _PackInt(term.index)
    uniq = _PackInt(term.uniq)
    freeVars = ""
    for freeVar in term.freeVars:
        freeVars = freeVars + _PackOneTerm(freeVar)
    return _PackInt4(MAGIC_FUN) + numFreeVars + \
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

