import os
import string
import socket

def ReadInt1(s):
    return ord(s[0])
    
def ReadInt2(s):
    return (ord(s[0]) << 8) + \
           (ord(s[1]) << 0)
    
def ReadInt4(s):
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
    return chr(i & 255)

def PackInt2(i):
    return chr((i >> 8) & 255) + \
           chr((i >> 0) & 255)

def PackInt4(i):
    return chr((i >> 24) & 255) + \
           chr((i >> 16) & 255) + \
           chr((i >>  8) & 255) + \
           chr((i >>  0) & 255)


def NodeNameMaybeAddHostName(nodeName):
    if "@" in nodeName:
        return nodeName
    hostName = GetHostName()
    return nodeName + "@" + hostName

def GetFullyQualifiedHostName():
    raise "Not implemented"
    
def GetHostName():
    hostName = socket.gethostname()
    if "." in hostName:
        components = string.split(hostName, ".")
        return components[0]
    else:
        return hostName


def getenv(e):
    if os.environ.has_key(e):
        return os.environ[e]
    else:
        return ""



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
        _DebugEmitText("%s: %s" % (module, txt))

def DebugHex(module, txt, msg):
    hexMsg = HexDumpFormat(msg)
    _DebugEmitText("%s: %s\n%s" % (module, txt, hexMsg))

def _DebugEmitText(txt):
    print txt


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
        return addrString + ": " + hexString + "   " + ascString

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

class VCallback:
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

class Callback:
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
