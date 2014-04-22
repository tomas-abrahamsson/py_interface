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
import struct
import pickle
import UserDict

from py_interface import erl_common
from py_interface import erl_opts

class HMarker: # a hash (value )marker
    def __init__(self, discr):
        self.discr=discr
    def __repr__(self):
        return "<hmarker/%s" % self.discr
hAtom   = HMarker("atom")
hRef    = HMarker("ref")
hPort   = HMarker("port")
hPid    = HMarker("pid")
hBinary = HMarker("binary")
hBitBinary = HMarker("bit-binary")
hFun    = HMarker("fun")
hFunExport = HMarker("fun-export")
hMapKey = HMarker("map-key")

class ErlTermError(Exception):
    def __init__(self, reason):
        self.reason = reason

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
                raise ErlTermError("No such cached atom: %s" % `cache`)
        elif atomText != None and cache != -1:
            self.atomText = atomText
            _atom_cache[cache] = atomText
        else:
            self.atomText = atomText
    def __repr__(self):
        return "<erl-atom: %s>" % `self.atomText`
    def equals(self, other):
        return IsErlAtom(other) and self.atomText == other.atomText
    __eq__ = equals
    def __ne__(self, other):
        return not self.__eq__(other)
    def __hash__(self):
        return hash((hAtom, self.atomText))
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
        return IsErlRef(other) and \
            self.node.equals(other.node) and \
            self.id == other.id and \
            self.creation == other.creation
    __eq__ = equals
    def __ne__(self, other):
        return not self.__eq__(other)
    def __hash__(self):
        return hash((hRef, self.node, self.id, self.creation))

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
        return IsErlPort(other) and \
            self.node.equals(other.node) and \
            self.id == other.id and \
            self.creation == other.creation
    __eq__ = equals
    def __ne__(self, other):
        return not self.__eq__(other)
    def __hash__(self):
        return hash((hPort, self.node, self.id, self.creation))

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
        return IsErlPid(other) and \
            self.node.equals(other.node) and \
            self.id == other.id and \
            self.serial == other.serial and \
            self.creation == other.creation
    __eq__ = equals
    def __ne__(self, other):
        return not self.__eq__(other)
    def __hash__(self):
        return hash((hPid, self.node, self.id, self.serial, self.creation))

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
        return IsErlImproperList(other) and \
            self.elements==other.elements and self.tail==other.tail
    __eq__ = equals
    def __ne__(self, other):
        return not self.__eq__(other)
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
        self.contents = contents
    def __repr__(self):
        return "<erl-binary: size=%d>" % len(self.contents)
    def equals(self, other):
        return IsErlBinary(other) and self.contents == other.contents
    __eq__ = equals
    def __ne__(self, other):
        return not self.__eq__(other)
    def __hash__(self):
        return hash((hBinary, self.contents))

def IsErlBinary(term):
    """Checks whether a term is an Erlang binary or not."""
    return type(term) == types.InstanceType and isinstance(term, ErlBinary)

class ErlBitBinary:
    """An Erlang bitstring: a possibly un-even number of octets
    contents = string
    numBitsInLastOctet = 1..8: the number of bits in the last octet of contents
                               counting from the most significant bit
    """
    def __init__(self, contents, bits):
        self.contents = contents
        self.bits = bits
    def __repr__(self):
        return "<erl-bit-binary: size=%d octets + %d bits>" % \
               (len(self.contents)-1, self.bits)
    def equals(self, other):
        return IsErlBitBinary(other) and self.eqContents(other)
    def eqContents(self, other):
        if len(self.contents) == 0 and len(other.contents) == 0:
            return True
        if len(self.contents) > 0 and len(other.contents) > 0:
            lastBits1 = ord(self.contents[-1]) & 255 << (8-self.bits)
            lastBits2 = ord(other.contents[-1]) & 255 << (8-other.bits)
            return self.contents[0:-1] == other.contents[0:-1] and \
                   lastBits1 == lastBits2
        else:
            return False
    __eq__ = equals
    def __ne__(self, other):
        return not self.__eq__(other)
    def __hash__(self):
        return hash((hBitBinary, self.contents, self.bits))

def IsErlBitBinary(term):
    """Checks whether a term is an Erlang bit-binary or not."""
    return type(term) == types.InstanceType and isinstance(term, ErlBitBinary)

def ErlString(s):
    """An Erlang list. This maps to a python string."""
    return s

class ErlFun:
    """An Erlang process id. The following attributes are defined:
    pid      = <ErlPid>
    module   = <ErlAtom>
    index    = integer
    uniq     = integer | 16 bytes
    oldIndex = integer
    oldUniq  = integer
    arity    = 0..255
    freeVars = list(term)
    """
    def __init__(self, pid, module, index, uniq,
                 oldIndex, oldUniq, arity, freeVars):
        self.pid = pid
        self.module = module
        self.index = index
        self.uniq = uniq
        self.oldIndex = oldIndex
        self.oldUniq = oldUniq
        self.arity = arity
        self.freeVars = freeVars
    def __repr__(self):
        return ("<erl-fun: pid=%s, module=%s, index=%d, uniq=%s, " +
                "oldIndex=%d, oldUniq=%d, arity=%d, freeVars=%s>") % \
               (`self.pid`, `self.module`, self.index, `self.uniq`,
                self.oldIndex, self.oldUniq, self.arity, `self.freeVars`)
    def equals(self, other):
        return IsErlFun(other) and \
            self.pid.equals(other.pid) and \
            self.module.equals(other.module) and \
            self.index == other.index and \
            self.uniq == other.uniq and \
            self.oldIndex == other.oldIndex and \
            self.oldUniq == other.oldUniq and \
            self.arity == other.arity and \
            self.freeVars == other.freeVars
    __eq__ = equals
    def __ne__(self, other):
        return not self.__eq__(other)
    def __hash__(self):
        return hash((hFun, hash(self.pid), hash(self.module),
                     self.index, self.uniq,
                     self.oldIndex, self.oldUniq,
                     self.arity,
                     tuple(self.freeVars)))

def IsErlFun(term):
    """Checks whether a term is an Erlang function or not."""
    return type(term) == types.InstanceType and isinstance(term, ErlFun)

class ErlFunExport:
    """An Erlang fun M:F/A. The following attributes are defined:
    module = an atom
    function = an atom
    arity = an integer
    """
    def __init__(self, module, function, arity):
        self.module = module
        self.function = function
        self.arity = arity
    def __repr__(self):
        return "<erl-export: module=%s, function=%s, arity=%d>" % \
               (`self.module`, `self.function`, self.arity)
    def equals(self, other):
        return IsErlFunExport(other) and \
               self.module == other.module and \
               self.function == other.function and \
               self.arity == other.arity
    __eq__ = equals
    def __ne__(self, other):
        return not self.__eq__(other)
    def __hash__(self):
        return hash((hFunExport, self.module, self.function, self.arity))

def IsErlFunExport(term):
    """Checks whether a term is an Erlang function export or not."""
    return type(term) == types.InstanceType and isinstance(term, ErlFunExport)

## About the Erlang map type vs Python Dict:
##
## In Python, it is not possible to have a list as an dict key,
## because it is mutable (it is unhashable: does not have __hash__),
## but Erlang lists are translated to Python lists (if proper),
## and in Erlang, it is possible to have any type as a Map key --
## even a(nother) Map can act as key -- and may well occur as Map keys
## (also, strings are lists, and even while they will often get
## encoded as string types, this might not be guaranteed, if they
## contain odd chars or integer values)
##
## So we make a dictionary imitation, which can have lists and
## atoms and whatnot among the Erlang types as keys.
##
## So we wrap unhashable terms as ErlMapKey instances, and
## hash a serialized form of the term. This also mimics key immutability.
##
##
## We go for pickle rather than cPickle, because it appears that
## pickle.dumps(pickle.loads(pickle.dumps(term))) == pickle.dumps(term)
## which is a desirable property for keys, but does not hold for cPickle.

def MakeErlMapKey(term):
    try:
        hash(term)
        return term
    except TypeError, x:
        return ErlMapKey(term)

def UnmakeErlMapKey(k):
    if IsErlMapKey(k):
        return k.GetElem()
    else:
        return k

class ErlMapKey:
    def __init__(self, elem):
        self.h = hash((hMapKey, type(elem), pickle.dumps(elem)))
        self.e = elem
    def __hash__(self):
        return self.h
    def __eq__(self, other):
        if IsErlMapKey(other):
            return self.e == other.e
        else:
            return False
    def __ne__(self, other):
        return not self.__eq__(other)
    def __repr__(self):
        return repr(self.e)
    def GetElem(self):
        return self.e

def IsErlMapKey(x):
    return type(x) == types.InstanceType and isinstance(x, ErlMapKey)

class ErlMap(UserDict.DictMixin):
    def __init__(self, dict=None, **kwargs):
        # fixme: handle extra and kv
        self.d = {}
        if dict != None:
            self.update(dict)
        if len(kwargs):
            self.update(kwargs)
    def __getitem__(self, key):
        return self.d[MakeErlMapKey(key)]
    def __setitem__(self, key, value):
        self.d[MakeErlMapKey(key)] = value
    def __delitem__(self, key):
        del self.d[MakeErlMapKey(key)]
    def keys(self):
        return [UnmakeErlMapKey(x) for x in self.d.keys()]
    def __eq__(self, other):
        if IsErlMap(other):
            ks1 = self.d.keys()
            ks2 = other.d.keys()
            if len(ks1) != len(ks2):
                return False
            for k in ks1:
                if k not in ks2:
                    return False
                if self.d[k] != other.d[k]:
                    return False
            return True
    def __ne__(self, other):
        return not self.__eq__(other)
    def __cmp__(self, other):
        # UserDict.DictMixin defines a __cmp__ which is unusable
        # to us, because the keys might be lists or other non-hashable objects,
        # so override __cmp__ with something reasonable.
        #
        # return a negative integer if self < other
        # return a positive integer if self > other
        if other is None:
            return 1
        if IsErlMap(other):
            ks1 = self.d.keys()
            ks2 = other.d.keys()
            if len(ks1) < len(ks2): return -1
            if len(ks1) > len(ks2): return 1
            if len(ks1) == len(ks2) == 0: return 0
            k2min = ks2[0]
            for k in ks2:
                if k < k2min: k2min = k
            for k in ks1:
                if k not in ks2:
                    if k < k2min: return -1
                    else: return 1
                if self.d[k] < other.d[k]: return -1
                if self.d[k] > other.d[k]: return 1
            return 0
        return 1
    def __repr__(self):
        r = "{"
        for k in self.d:
            r += repr(k) + ": " + repr(self.d[k])
        r += "}"
        return r

def IsErlMap(term):
    """Checks whether a term is an Erlang function or not."""
    return type(term) == types.InstanceType and isinstance(term, ErlMap)

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
MAGIC_BIT_BINARY = 77
MAGIC_FUN = 117
MAGIC_NEW_FUN = 112
MAGIC_NEW_CACHE = 78
MAGIC_CACHED_ATOM = 67
MAGIC_MAP = 116
MAGIC_EXPORT = 113
MAGIC_NEW_FLOAT = 70

###
### UNPACKING
###
def BinaryToTerm(binary):
    """Unpack a binary to a term.

    BINARY = string

    Returns: term
    Throws:  "BinaryToTerm: Extraneous data in binary"
    """
    (term, remaining) = _UnpackOneTermTop(binary)
    if len(remaining) != 0:
        raise ErlTermError("BinaryToTerm: Extraneous data in binary")
    return term

def BinariesToTerms(binary):
    """Unpack a binary/binaries to term(s).
    This is mainly for use by the erl_node_conn, where, in some cases,
    two or more terms are packed together.

    BINARY = string

    Returns: list(term)
    Throws:  "BinaryToTerm: Extraneous data in binary"
    """
    (terms, remaining) = BufToTerm(binary)
    if len(remaining) != 0:
        raise ErlTermError("BinariesToTerms: Extraneous data in binary")
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
        n = _ReadSignedInt4(data[1:5])
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

    elif data0 == MAGIC_NEW_FLOAT:
        (f,) = struct.unpack(">d", data[1:9])
        return (ErlNumber(f), data[9:])

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

    elif data0 == MAGIC_BIT_BINARY:
        binlen = _ReadInt4(data[1:5])
        bits = _ReadInt1(data[5])
        s = data[6:6 + binlen]
        return (ErlBitBinary(s, bits), data[6 + binlen:])

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
        return (ErlFun(pid, module, index, uniq, 0, 0, 0, freeVars),
                remainingData5)

    elif data0 == MAGIC_NEW_FUN:
        size = _ReadInt4(data[1:5]) # total number of bytes including the size
        funData = _ReadInt4(data[1:size+1])
        arity = _ReadInt1(data[5])
        uniq = data[6:22]
        index = _ReadInt4(data[22:26])
        numFree = _ReadInt4(data[26:30])
        (module, remainingData2) = _UnpackOneTerm(data[30:])
        (oldIndex, remainingData3) = _UnpackOneTerm(remainingData2)
        (oldUniq, remainingData4) = _UnpackOneTerm(remainingData3)
        (pid, remainingData5) = _UnpackOneTerm(remainingData4)
        (freeVars, remainingData6) = _UnpackTermSeq(numFree, remainingData5)
        return (ErlFun(pid, module, index, uniq,
                       oldIndex, oldUniq, arity,
                       freeVars),
                remainingData6)

    elif data0 == MAGIC_EXPORT:
        (module, remainingData2) = _UnpackOneTerm(data[1:])
        (function, remainingData3) = _UnpackOneTerm(remainingData2)
        (arity, remainingData4) = _UnpackOneTerm(remainingData3)
        return (ErlFunExport(module, function, arity), remainingData4)

    elif data0 == MAGIC_MAP:
        arity = _ReadInt4(data[1:5])
        remainingData1 = data[5:]
        m = ErlMap()
        (pairs, remainingData2) = _UnpackPairs(arity, remainingData1)
        for (k, v) in pairs:
            m[k] = v
        return (m, remainingData2)

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

def _UnpackPairs(numPairs, data):
    pairs = []
    remainingData = data
    for i in range(numPairs):
        (k, remainingData2) = _UnpackOneTerm(remainingData)
        (v, remainingData3) = _UnpackOneTerm(remainingData2)
        pairs.append((k, v))
        remainingData = remainingData3
    return (pairs, remainingData)


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

def _ReadSignedInt4(s):
    n = erl_common.ReadInt4(s)
    if n <= 0x7fffffff:
        return n
    else:
        return -(0x100000000-n)

###
### PACKING
###

def TermToBinary(term, flags=0xffffFFFF):
    """Pack a term to a binary.

    TERM = term
    FLAGS = flags, see distribution flags on
            http://erlang.org/doc/apps/erts/erl_dist_protocol.html

    Returns: string
    Throws:  \"Can't pack value of type ...\"
    """
    return chr(MAGIC_VERSION) + _PackOneTerm(term, flags)

def _PackOneTerm(term, flags):
    if type(term) == types.StringType:
        return _PackString(term, flags)
    elif type(term) == types.ListType:
        return _PackList(term, flags)
    elif type(term) == types.TupleType:
        return _PackTuple(term, flags)
    elif type(term) == types.LongType:
        return _PackLong(term, flags)
    elif type(term) == types.FloatType:
        return _PackFloat(term, flags)
    elif type(term) == types.IntType:
        return _PackInt(term, flags)
    elif IsErlAtom(term):
        return _PackAtom(term, flags)
    elif IsErlRef(term):
        return _PackRef(term, flags)
    elif IsErlPort(term):
        return _PackPort(term, flags)
    elif IsErlPid(term):
        return _PackPid(term, flags)
    elif IsErlBinary(term):
        return _PackBinary(term, flags)
    elif IsErlBitBinary(term):
        return _PackBitBinary(term, flags)
    elif IsErlFun(term):
        return _PackFun(term, flags)
    elif IsErlFunExport(term):
        return _PackFunExport(term, flags)
    elif IsErlMap(term):
        return _PackMap(term, flags)
    else:
        raise ErlTermError("Can't pack value of type %s: %s" %
                           (`type(term)`, `term`))


def _PackString(term, flags):
    if len(term) == 0:
        return _PackList([])
    elif len(term) <= 65535:
        return _PackInt1(MAGIC_STRING) + _PackInt2(len(term)) + term
    else:
        return _PackList(map(lambda c: ord(c), term), flags)

def _PackList(term, flags):
    if len(term) == 0:
        return _PackInt1(MAGIC_NIL)
    else:
        packedData = ""
        for elem in term:
            packedData = packedData + _PackOneTerm(elem, flags)
        return _PackInt1(MAGIC_LIST) + _PackInt4(len(term)) + packedData + \
               _PackList([], flags)

def _PackTuple(term, flags):
    if len(term) < 256:
        head = _PackInt1(MAGIC_SMALL_TUPLE) + _PackInt1(len(term))
    else:
        head = _PackInt1(MAGIC_LARGE_TUPLE) + _PackInt4(len(term))
    packedData = head
    for elem in term:
        packedData = packedData + _PackOneTerm(elem, flags)
    return packedData


def _PackLong(term, flags):
    if -long(0x7fffffff) - 1 <= term <= long(0x7fffffff):
        return _PackInt(term, flags)
    else:
        numBytesNeeded = int(math.log(abs(term)) / math.log(256)) + 1
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
    bignum = abs(term)
    bignumBytes = sign
    for i in range(numBytesNeeded):
        bignumBytes = bignumBytes + _PackInt1(bignum & 255)
        bignum = bignum >> 8
    return bignumBytes

def _PackFloat(term, flags):
    if flags & erl_opts.DISTR_FLAG_NEWFLOATS:
        floatData = struct.pack(">d", term)
        return _PackInt1(MAGIC_NEW_FLOAT) + floatData
    else:
        floatStr = "%.20e" % term
        nullPadStr = _PackInt1(0) * (31 - len(floatStr))
        return _PackInt1(MAGIC_FLOAT) + floatStr + nullPadStr

def _PackInt(term, flags):
    if 0 <= term < 256:
        return _PackInt1(MAGIC_SMALL_INTEGER) + _PackInt1(term)
    else:
        return _PackInt1(MAGIC_INTEGER) + _PackInt4(term)

def _PackAtom(term, flags):
    atomText = term.atomText
    return _PackInt1(MAGIC_ATOM) + _PackInt2(len(atomText)) + atomText

def _PackRef(term, flags):
    if type(term.id) == types.ListType:
        return _PackNewReferenceExt(term, flags)
    else:
        return _PackReferenceExt(term, flags)

def _PackNewReferenceExt(term, flags):
    node = _PackOneTerm(term.node, flags)
    creation = _PackCreation(term.creation)
    id0 = _PackId(term.id[0])
    ids = id0
    for id in term.id[1:]:
        ids = ids + _PackInt4(id)
    return _PackInt1(MAGIC_NEW_REFERENCE) + \
           _PackInt2(len(term.id)) + \
           node + creation + ids

def _PackReferenceExt(term, flags):
    node = _PackOneTerm(term.node, flags)
    id = _PackId(term.id)
    creation = _PackCreation(term.creation)
    return _PackInt1(MAGIC_REFERENCE) + node + id + creation

def _PackPort(term, flags):
    node = _PackOneTerm(term.node, flags)
    id = _PackId(term.id, 28)
    creation = _PackCreation(term.creation)
    return _PackInt1(MAGIC_PORT) + node + id + creation

def _PackPid(term, flags):
    node = _PackOneTerm(term.node, flags)
    id = _PackId(term.id, 28)
    serial = _PackInt4(term.serial)
    creation = _PackCreation(term.creation)
    return _PackInt1(MAGIC_PID) + node + id + serial + creation

def _PackBinary(term, flags):
    return _PackInt1(MAGIC_BINARY) + \
           _PackInt4(len(term.contents)) + \
           term.contents

def _PackBitBinary(term, flags):
    return _PackInt1(MAGIC_BIT_BINARY) + \
           _PackInt4(len(term.contents)) + \
           _PackInt1(term.bits) + \
           term.contents

def _PackFun(term, flags):
    if flags & erl_opts.DISTR_FLAG_NEWFUNTAGS and \
       type(term.uniq) == types.StringType:
        arity = _PackInt1(term.arity)
        uniq = term.uniq
        index = _PackInt4(term.index)
        numFree = _PackInt4(len(term.freeVars))
        module = _PackOneTerm(term.module, flags)
        # oldIndex and oldUniq must be SMALL_INTEGER_EXT or INTEGER_EXT
        # FIXME: should either somehow force them to be that,
        # or verify/assert that they end up being that
        oldIndex = _PackOneTerm(term.oldIndex, flags)
        oldUniq = _PackOneTerm(term.oldUniq, flags)
        pid = _PackOneTerm(term.pid, flags)
        freeVars = "".join([_PackOneTerm(x, flags) for x in term.freeVars])
        info = arity + uniq + index + numFree + \
               module + oldIndex + oldUniq + pid + freeVars
        size = _PackInt4(4 + len(info)) # size includes the size field
        return _PackInt1(MAGIC_NEW_FUN) + size + info
    else:
        numFreeVars = _PackInt4(len(term.freeVars))
        pid = _PackPid(term.pid)
        module = _PackAtom(term.module)
        index = _PackInt(term.index)
        uniq = _PackInt(term.uniq)
        freeVars = ""
        for freeVar in term.freeVars:
            freeVars = freeVars + _PackOneTerm(freeVar, flags)
        return _PackInt1(MAGIC_FUN) + numFreeVars + \
               pid + module + index + uniq + freeVars

def _PackFunExport(term, flags):
    module = _PackOneTerm(term.module, flags)
    function = _PackOneTerm(term.function, flags)
    arity = _PackOneTerm(term.arity, flags)
    return _PackInt1(MAGIC_EXPORT) + module + function + arity

def _PackMap(term, flags):
    arity = _PackInt4(len(term))
    pairs = [_PackOneTerm(k, flags) + _PackOneTerm(v, flags) \
             for (k,v) in term.iteritems()]
    return _PackInt1(MAGIC_MAP) + arity + "".join(pairs)

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
