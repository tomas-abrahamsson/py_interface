#! /usr/bin/env python

import sys
import string
import getopt

import erl_node
import erl_opts
import erl_eventhandler

import erl_common

###
###
###
### TEST CODE
###
###

def __TestMBoxCallback(msg):
    print "msg=%s" % `msg`

def GoCall(l, i, destNode):
    if i < len(l):
        (j, n, m) = l[i]
        print "Calling %d" % j
        m.Send(("echo_server", destNode), j)
        evhand = erl_eventhandler.GetEventHandler()
        evhand.AddTimerEvent(0.5, GoCall, l, i+1, destNode)
    else:
        print "Done"
        

def main(argv):
    try:
        opts, args = getopt.getopt(argv[1:], "?0:d:n:c:")
    except getopt.error, info:
        print info
        sys.exit(1)

    hostName = "localhost"
    numNodes = 1
    startIndex = 0
    destNode = "destNode@localhost"
    cookie = "cookie"

    print "erl> F = fun(FF) -> receive X -> io:format(\"F:~p~n\", [X]), FF(FF) end end."
    print "erl> register(echo_server, spawn(fun() -> F(F) end))."

    for (optchar, optarg) in opts:
        if optchar == "-?":
            print "Usage: %s erlnode" % argv[0]
            sys.exit(1)
        elif optchar == "-c":
            cookie = optarg
        elif optchar == "-d":
            destNode = optarg
        elif optchar == "-n":
            numNodes = string.atoi(optarg)
        elif optchar == "-0":
            startIndex = string.atoi(optarg)

    l = []
    o = erl_opts.ErlNodeOpts(cookie=cookie)
    n=None
    m=None

    erl_common.DebugOnAll()

    for i0 in range(numNodes):
        i = startIndex + i0
        print "Node %d..." % i
        n = erl_node.ErlNode("flerp%d"%i, o)
        n.Publish()
        m = n.CreateMBox(__TestMBoxCallback)
        m.RegisterName("p")

        l.append((i, n, m))

    evhand = erl_eventhandler.GetEventHandler()

    print "Starting in 2 seconds..." 
    evhand.AddTimerEvent(2, GoCall, l, 0, destNode)

    evhand.Loop()


main(sys.argv)
