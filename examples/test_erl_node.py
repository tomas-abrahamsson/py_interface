#! /usr/bin/env python

import sys
import getopt

from py_interface import erl_node
from py_interface import erl_opts
from py_interface import erl_eventhandler

###
###
###
### TEST CODE
###
###

def __TestMBoxCallback(msg):
    print "msg=%s" % `msg`

n=None
m=None
def main(argv):
    try:
        opts, args = getopt.getopt(argv[1:], "?n:c:")
    except getopt.error, info:
        print info
        sys.exit(1)

    hostName = "localhost"
    ownNodeName = "py_interface_test"
    cookie = "cookie"

    for (optchar, optarg) in opts:
        if optchar == "-?":
            print "Usage: %s erlnode" % argv[0]
            sys.exit(1)
        elif optchar == "-c":
            cookie = optarg
        elif optchar == "-n":
            ownNodeName = optarg

    print "Creating node..."
    n = erl_node.ErlNode(ownNodeName, erl_opts.ErlNodeOpts(cookie=cookie))
    print "Publishing node..."
    n.Publish()
    print "Creating mbox..."
    m = n.CreateMBox(__TestMBoxCallback)
    print "Registering mbox as p..."
    m.RegisterName("p")

    print "Looping..."
    evhand = erl_eventhandler.GetEventHandler()
    evhand.Loop()


main(sys.argv)
