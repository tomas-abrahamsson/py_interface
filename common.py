### common.py -- common utility functions
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


def getenv(e):
    if os.environ.has_key(e):
        return os.environ[e]
    else:
        return ""



def HexDump(string):
    def dump_chars(addr, s):
        hexString = ""
        ascString = ""
        for i, c in map(None, range(len(s)), s):
            if i > 0:
                hexString = hexString + " "
            hexString = hexString + ("%02x" % ord(c))
            if (c < " "):
                ascString = ascString + "."
            else:
                ascString = ascString + c
        numFill = 16 - len(s)
        hexString = hexString + "   " * numFill
        addrString = "%04x" % addr
        return addrString + ": " + hexString + "   " + ascString

    remaining_chars = string;
    addr = 0
    while len(remaining_chars) > 0:
        if len(remaining_chars) < 16:
            print dump_chars(addr, remaining_chars)
            remaining_chars = ""
        else:
            print dump_chars(addr, remaining_chars[:16])
            remaining_chars = remaining_chars[16:]
            addr = addr + 16

def IndexSeq(seq):
    """Given a sequence, return a list of tuples (i, elem[i]).
Example: IndexSeq(["a", "b", "c"]) ==> [(0, "a"), (1, "b"), (2, "c")]."""
    return map(None, range(len(seq)), seq)


_logfile = None
def SetLogFile(fileName):
    global _logfile
    _logfile = open(fileName, "w")

def LogFileClose():
    global _logfile
    if _logfile != None:
        _logfile.close()

def Log(str):
    global _logfile
    if _logfile != None:
        _logfile.write(str)
        _logfile.write("\n")
        _logfile.flush()
    

_modulesToDebug = []
_debugAllModules = 0

def DebugOnAll():
    global _modulesToDebug, _debugAllModules
    _debugAllModules = 1


def DebugOn(moduleList):
    global _modulesToDebug, _debugAllModules
    _debugAllModules = 0
    _modulesToDebug = moduleList


def Debug(module, txt):
    global _modulesToDebug, _debugAllModules
    if _debugAllModules or module in _modulesToDebug:
        print "%s: %s" % (module, txt)
                                                

class VCallback:
    def __init__(self, callback, optArgs, namedArgs):
        self.callback = callback
        self.optArgs = optArgs
        self.namedArgs = namedArgs

    def __call__(self, *extraArgs):
        return apply(self.callback, extraArgs + self.optArgs, self.namedArgs)

    def __repr__(self):
        return "<VCallback to %s>" % `self.callback`

class Callback:
    def __init__(self, callback, *optArgs, **namedArgs):
        self.callback = callback
        self.optArgs = optArgs
        self.namedArgs = namedArgs

    def __call__(self, *extraArgs):
        return apply(self.callback, extraArgs + self.optArgs, self.namedArgs)
