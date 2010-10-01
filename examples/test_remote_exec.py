#! /usr/bin/env python

import sys
import types
import string
import socket
import getopt


from py_interface import erl_term
from py_interface import erl_node
from py_interface import erl_opts
from py_interface import erl_common
from py_interface import erl_eventhandler

eh = None


def print_usage():
    print "Usage: %s [opts] remote-node mod fn arg ..." % sys.argv[0]
    print "Opts:"
    print "-?              print usage"
    print "-d              enable debugging info"
    print "-t <seconds>    timeout: exit after this many seconds"
    print "-n <name|sname> set the python node's name"
    print "-c <cookie>     set cookie"

def _TestMBoxCallback(msg, *x, **kw):
    print "Incoming msg=%s" % `msg`

def _TestMBoxRPCResponse(msg):
    global eh
    print "RPC answer: %s" % `msg`
    eh.StopLooping()


def __TestMBoxCallback(msg):
    print "Incoming msg=%s" % `msg`

def SendTheRPC(mbox, remoteNode, m, f, a):
    print "Sending:"
    print "  remoteNode=%s" % `remoteNode`
    print "  m=%s" % `m`
    print "  f=%s" % `f`
    print "  a=%s" % `a`
    mbox.SendRPC(remoteNode, m, f, a, _TestMBoxRPCResponse)

def DoTimeout():
    global eh
    print "Timeout!"
    eh.StopLooping()

def ExprRebuildAtoms(expr):
    if type(expr) == types.StringType:
        if len(expr) >= 2 and expr[0] == expr[-1] == "'":
            atomText = expr[1:-1]
            return erl_term.ErlAtom(atomText)
        else:
            return expr
    elif type(expr) == types.ListType:
        rebuiltList = []
        for elem in expr:
            rebuiltElem = ExprRebuildAtoms(elem)
            rebuiltList.append(rebuiltElem)
        return rebuiltList
    elif type(expr) == types.TupleType:
        rebuiltList = []
        for elem in list(expr):
            rebuiltElem = ExprRebuildAtoms(elem)
            rebuiltList.append(rebuiltElem)
        return tuple(rebuiltList)
    else:
        return expr

def main(argv):
    global eh

    try:
        opts, args = getopt.getopt(argv[1:], "?dn:c:t:", ['help'])
    except getopt.error, info:
        print info
        sys.exit(1)

    hostName = "localhost"
    ownNodeName = "py_interface_test"
    cookie = "cookie"
    doDebug = 0
    timeout = 10

    for (optchar, optarg) in opts:
        if optchar == "-?" or optchar == "--help":
            print_usage()
            sys.exit(0)
        elif optchar == "-c":
            cookie = optarg
        elif optchar == "-d":
            doDebug = 1
        elif optchar == "-n":
            ownNodeName = optarg
        elif optchar == "-t":
            timeout = eval(optarg)

    if doDebug:
        erl_common.DebugOnAll()

    if len(args) < 3:
        print_usage()
        sys.exit(1)

    [remoteNode, module, function] = args[0:3]
    fargs = args[3:]

    print "Creating node..."
    n = erl_node.ErlNode(ownNodeName, erl_opts.ErlNodeOpts(cookie=cookie))
    print "Publishing node..."
    n.Publish()
    print "Creating mbox..."
    m = n.CreateMBox(__TestMBoxCallback)
    print "Registering mbox as p..."
    m.RegisterName("p")

    evhand = erl_eventhandler.GetEventHandler()
    eh = evhand

    evhand.AddTimerEvent(timeout, DoTimeout)


    # Schedule to run the RPC after we've started looping
    evhand.AddTimerEvent(0.001,
                         erl_common.Callback(
                             SendTheRPC, m, remoteNode,
                             module, function,
                             map(lambda x: ExprRebuildAtoms(eval(x)),
                                 fargs)))

    print "Looping..."
    evhand.Loop()
    sys.exit(0)

try:
    main(sys.argv)
except KeyboardInterrupt:
    print "Interrupted. Exiting."
    sys.exit(1)
