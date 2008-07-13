#! /usr/bin/env python

###
###
### Test code
###
###
import sys
import string
import getopt

from py_interface import erl_opts
from py_interface import erl_common
from py_interface import erl_node_conn
from py_interface import erl_eventhandler


def __TestConnectOk():
    print "ConnectOk"

def __TestConnectFailed():
    print "ConnectFailed"

def __TestConnectionBroken(connection, nodeName):
    print "ConnectionBroken for %s" % nodeName

def __TestPassThroughMsg(controlMsg, msg=None):
    print "passThrough:"
    print "  controlMsg=%s" % `controlMsg`
    print "  msg=%s" % `msg`

def main(argv):
    global e

    try:
        opts, args = getopt.getopt(argv[1:], "?n:c:d:f:")
    except getopt.error, info:
        print info
        sys.exit(1)

    hostName = "localhost"
    ownNodeName = "py_interface_test"
    cookie = "cookie"
    ownDistrVersion = 5
    ownFlags = 4

    for (optchar, optarg) in opts:
        if optchar == "-?":
            print "Usage: %s [host] port" % argv[0]
            sys.exit(1)
        elif optchar == "-c":
            cookie = optarg
        elif optchar == "-n":
            ownNodeName = optarg
        elif optchar == "-d":
            ownDistrVersion = string.atoi(optarg)
        elif optchar == "-f":
            ownFlags = string.atoi(optarg)

    if len(args) >= 2:
        hostName = args[0]
        portNum = string.atoi(args[1])
    elif len(args) >= 1:
        portNum = string.atoi(args[0])
    else:
        sys.exit(1)

    ownNodeName = erl_common.AlignNodeName(ownNodeName, 1)

    print "Connecting to %s:%d"
    print "  ownNodeName=\"%s\"" % ownNodeName
    print "  cookie=\"%s\"" % cookie
    print "  ownDistrVersion=%d" % ownDistrVersion
    print "  ownFlags=%d" % ownFlags

    opts = erl_opts.ErlNodeOpts(cookie=cookie)
    c = erl_node_conn.ErlNodeOutConnection(ownNodeName, opts)
    c.InitiateConnection(hostName, portNum,
                         __TestConnectOk,
                         __TestConnectFailed,
                         __TestConnectionBroken,
                         __TestPassThroughMsg)
    evhandler = erl_eventhandler.GetEventHandler()
    evhandler.Loop()


main(sys.argv)

