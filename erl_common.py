def ReadInt1(s):
    return ord(s[0])
    
def ReadInt2(s):
    return (ord(s[0]) << 8) + \
           (ord(s[1]) << 0)
    
def ReadInt4(s):
    return (ord(s[0]) << 24) + \
           (ord(s[1]) << 16) + \
           (ord(s[2]) <<  8) + \
           (ord(s[3]) <<  0)


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


def Debug(s):
    print s

def DebugUnrecognizedMsg(txt, msg):
    print "Unrecognized message", s
    HexDump(msg)


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
