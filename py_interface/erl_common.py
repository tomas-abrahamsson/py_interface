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

### erl_common.py -- common utility functions

import os
import string
import socket

def ReadInt1(s):
    """Convert first byte from a string to an unsigned 8-bit integer."""
    return ord(s[0])

def ReadInt2(s):
    """Convert first two bytes from a string to an unsigned 16-bit integer."""
    return (ord(s[0]) << 8) + \
           (ord(s[1]) << 0)

def ReadInt4(s):
    """Convert first four bytes from a string to an unsigned 32-bit integer.

    Returns integer | long

    In Python on a 32-bit machine, integers are signed and within
    the range -2^31 .. (2^31 - 1). If the 32-bit integer fits within this
    range, an integer is returned, otherwise a long is returned."""
    l4 = (long(ord(s[0])) << 24) + \
         (ord(s[1]) << 16) + \
         (ord(s[2]) <<  8) + \
         (ord(s[3]) <<  0)
    try:
        i4 = int(l4)
        return i4
    except OverflowError:
        return l4


def PackInt1(i):
    """Converts an unsigned 8-bit integer/long into a string, 1 byte long."""
    return chr(i & 255)

def PackInt2(i):
    """Converts an unsigned 16-bit integer/long into a string, 2 byte long."""
    return chr((i >> 8) & 255) + \
           chr((i >> 0) & 255)

def PackInt4(i):
    """Converts an unsigned 32-bit integer/long into a string, 4 byte long."""
    return chr((i >> 24) & 255) + \
           chr((i >> 16) & 255) + \
           chr((i >>  8) & 255) + \
           chr((i >>  0) & 255)


def AlignNodeName(nodeName, useShortNodeNames=1):
    """Make sure the node name is a valid node name.

    NODE-NAME            = string
                         A node name, possibly containing an "@"
    USE-SHORT-NODE-NAMES = bool
                         Whether to align using short node names
                         or not. If not, then long node names are
                         assumed.

    Returns: string
    Throws:  nothing

    The returned node name:

    1. If the NODE-NAME contains an `@', then NODE-NAME is returned unchanged
    2. If the NODE-NAME does not contain an `@', then the returned node name
       is constructed as NODE-NAME + "@" + host-name, where
       host-name is either on short or long form, depending on
       USE-SHORT-NODE-NAMES.
    """
    if useShortNodeNames:
        return AlignShortNodeName(nodeName)
    else:
        return AlignLongNodeName(nodeName)

def AlignShortNodeName(nodeName):
    """Align a node, use short hostname if needed. See doc for AlignNodeName"""
    if "@" in nodeName:
        return nodeName
    fqdn = GetFullyQualifiedHostName()
    shortHostName = string.split(fqdn, ".")[0]
    return nodeName + "@" + shortHostName

def AlignLongNodeName(nodeName):
    """Align a node, use long hostname if needed. See doc for AlignNodeName"""
    if "@" in nodeName:
        return nodeName
    fqdn = GetFullyQualifiedHostName()
    return nodeName + "@" + fqdn

def GetFullyQualifiedHostName(name=""):
    """Get fully qualified domain name from name.

    An empty argument is interpreted as meaning the local host.

    First the hostname returned by gethostbyaddr() is checked, then
    possibly existing aliases. In case no FQDN is available, hostname
    is returned.
    """
    try:
        # Try the builtin version if it exists.
        # It does in Python 2.0
        return socket.getfqdn(name)
    except AttributeError, info:
        # This fallback code is provided for Python 1.5.2 and earlier
        # It is stolen with pride from Python 2.0
        name = string.strip(name)
        if not name or name == '0.0.0.0':
            name = socket.gethostname()
        try:
            hostname, aliases, ipaddrs = socket.gethostbyaddr(name)
        except error:
            pass
        else:
            aliases.insert(0, hostname)
            for name in aliases:
                if '.' in name:
                    break
            else:
                name = hostname
        return name

def getenv(envName):
    """Read an environment variable.
    ENV-NAME = string
             The name of the environment variable
    Returns: "" | string
             The env-value, or "" if the env-name isn't defined
    Throws:  nothing
    """
    if os.environ.has_key(envName):
        return os.environ[envName]
    else:
        return ""



def IndexSeq(seq):
    """Given a sequence, return a list of tuples (i, elem[i]).
Example: IndexSeq(["a", "b", "c"]) ==> [(0, "a"), (1, "b"), (2, "c")]."""
    return map(None, range(len(seq)), seq)


_logfilename = None
def SetLogFile(fileName):
    """Setup a file name to log to
    FILE-NAME = string
              The name of a file to log to
    Returns: void

    Sets up a log file.
    """
    global _logfilename

def Log(str):
    """Log a string to the log file
    STR = string
        The string to log (without trailing new-line)
    Returns: void
    Throws:  nothing

    Logs a string to the file set up by logfilename.
    The file is opened only during the logging. It is kept closed between
    calls to this function.
    """
    global _logfilename
    if _logfilename != None:
        try:
            f = open(_logfilename, "w")
            f.write(str)
            f.write("\n")
            f.flush()
            f.close()
        except IOError, info:
            # Silently ignore
            pass


_modulesToDebug = []
_debugAllModules = 0
_debugFileName = None

def DebugToFile(fileName):
    """Setup a file name to print debugging messages to
    FILE-NAME = string
              The name of a file to log to
    Returns: void
    Throws:  nothing

    Sets up a file to print debugging messages to.
    """
    global _debugFileName
    _debugFileName = fileName

def DebugOnAll():
    """Turn on debugging for all modules
    No arguments
    Returns: void
    Throws:  nothing

    Turns on debugging flag for all modules
    """
    global _modulesToDebug, _debugAllModules
    _debugAllModules = 1


def DebugOn(moduleList):
    """Turn on debugging for selected modules
    MODULE-LIST = list(string)

    Returns: void
    Throws:  nothing

    Turns on debugging flag for selected modules
    """
    global _modulesToDebug, _debugAllModules
    _debugAllModules = 0
    _modulesToDebug = moduleList

def Debug(module, txt):
    """Print a debug text
    MODULE = string
           Name of the module that is printing the debug text
    TXT    = string
           The text to print as debugging message
    Returns: void
    Throws:  nothing

    Prints a debugging text. The text is printed if debugging for the
    module is turned on, see DebugOnAll and DebugOn.

    The debug message is printed to stdout, except if debugging has
    been set to go to a file, see DebugToFile.
    """
    global _modulesToDebug, _debugAllModules
    if _debugAllModules or module in _modulesToDebug:
        _DebugEmitText("%s: %s" % (module, txt))

def DebugHex(module, txt, msg):
    """Print a debug text and a hexdump of a message
    MODULE = string
           Name of the module that is printing the debug text
    TXT    = string
           The text to print as debugging message
    MSG    = string
           The message to hexdump
    Returns: void
    Throws:  nothing

    Prints a debugging text. The text is printed if debugging for the
    module is turned on, see DebugOnAll and DebugOn.

    The debug message is printed to stdout, except if debugging has
    been set to go to a file, see DebugToFile.
    """
    hexMsg = _HexDumpFormat(msg)
    _DebugEmitText("%s: %s\n%s" % (module, txt, hexMsg))

def _DebugEmitText(txt):
    global _debugFileName
    if _debugFileName == None:
        print txt
    else:
        try:
            f = open(_debugFileName, "w")
            f.write(txt)
            f.flush()
            f.close()
        except IOError, info:
            # Silently ignore
            pass


def _HexDumpFormat(string):
    def dump_chars(addr, s):
        hexString = ""
        ascString = ""
        for i, c in map(None, range(len(s)), s):
            if i > 0:
                hexString = hexString + " "
            hexString = hexString + ("%02x" % ord(c))
            if (c < " ") or ((ord(c) >= 128) and (ord(c) < 160)):
                ascString = ascString + "."
            else:
                ascString = ascString + c
        numFill = 16 - len(s)
        hexString = hexString + "   " * numFill
        addrString = "%04x" % addr
        return addrString + ": " + hexString + "   " + ascString + "\n"

    remaining_chars = string;
    addr = 0
    result = ""
    while len(remaining_chars) > 0:
        if len(remaining_chars) < 16:
            result = result + dump_chars(addr, remaining_chars)
            remaining_chars = ""
        else:
            result = result + dump_chars(addr, remaining_chars[:16])
            remaining_chars = remaining_chars[16:]
            addr = addr + 16
    return result

class Callback:
    """This class provides a callback object.
    Here's how to use it:

    def MyFunction1(...):
        pass
    def MyFunction2(..., arg1, arg2, arg3):
        pass
    def MyFunction3(..., arg1, arg2, arg3, kw1=None, kw2=None):
        pass

    cb1 = Callback(MyFunction1)
    cb2 = Callback(MyFunction2, 1, 2, 3)
    cb3 = Callback(MyFunction3, 1, 2, 3, kw1=4, kw2=5)

    RegisterCallback(cb1)
    RegisterCallback(cb2)
    RegisterCallback(cb3)

    Any arguments that the invoker uses are tacked on before the first
    callback-argument, hence the `...' in the function definitions above.
    This example might clarify:

    def MyFunction(s1, s2, arg1, arg2):
        print "s1=%s, s2=%s, arg1=%s, arg2=%s" % (`s1`, `s2`, `arg1`, `arg2`)
    mycb = Callback(MyFunction, 3, 4)
    RegisterCallback(mycb)
    ...
    #someone calls:
    cb(1, 2)
    # This will cause MyFunction to be invoked as MyFuntion(1,2,3,4),
    # the arguments 1 and 2 tacked on to the front,
    # coming from the callback invoker, and arguments 3 and 4
    # come from the creation of the callback object
    # Thus, the following will be printed:
    s1=1, s2=2, arg1=3, arg2=4
    """
    def __init__(self, callback, *optArgs, **namedArgs):
        self.callback = callback
        self.optArgs = optArgs
        self.namedArgs = namedArgs

    def __call__(self, *extraArgs):
        try:
            return apply(self.callback, extraArgs+self.optArgs, self.namedArgs)
        except KeyboardInterrupt:
            raise
        except:
            print "Error in VCallback %s" % self.__repr__()
            raise

    def __repr__(self):
        return "<Callback to %s>" % `self.callback`

class VCallback:
    """This class provides a callback object.
    It is similar to the Callback (see the doc for this class),
    but is intended to be used in a situation where you have
    already collected the optional args and the keyword args.

    Here's an example of when to use this class instead of the Callback class:

    def DefineRegisterCallback(cbfn, *optArgs, *namedArgs):
        cb = VCallback(cbfn, optArgs namedArgs)
        RegisterCallback(cb)

    ...

    DefineRegisterCallback(MyFunction, 1, 2)
    """
    def __init__(self, callback, optArgs, namedArgs):
        self.callback = callback
        self.optArgs = optArgs
        self.namedArgs = namedArgs

    def __call__(self, *extraArgs):
        try:
            return apply(self.callback, extraArgs+self.optArgs, self.namedArgs)
        except KeyboardInterrupt:
            raise
        except:
            print "Error in VCallback %s" % self.__repr__()
            print "  extraArgs=%s" % `extraArgs`
            print "  self.optArgs=%s" % `self.optArgs`
            print "  self.namedArgs=%s" % `self.namedArgs`
            raise

    def __repr__(self):
        return "<VCallback to %s>" % `self.callback`
