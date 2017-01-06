#! /usr/bin/env python3

## A test case for testing packing/unpacking of erlang-terms:
##
## See run_test_erl_node_pingpong.sh for how to run it.
##
## A message is sent from an erlang node to a python node.
## That message is echoed back to the erlang node, which checks
## if the received message matches the original message.
##

import os
import sys
import types
import string
import socket
import getopt

# expecting current dir to be test/
# so that we can find py_interface in ..
sys.path.insert(0, "..")

from py_interface import erl_term
from py_interface import erl_node
from py_interface import erl_opts
from py_interface import erl_common
from py_interface import erl_eventhandler

mb = None
quiet = False

def __TestMBoxCallback(msg, *k, **kw):
    global mb, quiet
    if not quiet:
        print("Incoming msg=%s (k=%s, kw=%s)" % (repr(msg), repr(k), repr(kw)))
    if type(msg) == tuple:
        if len(msg) == 2:
            if erl_term.IsErlPid(msg[0]):
                dest = msg[0]
                if not quiet:
                    print("Sending it back to %s" % (dest,))
                reply = msg[1]
                mb.Send(dest, (mb.Self(), reply))

def __FlushStdout():
    input = sys.stdin.readline()
    if input == "": # master died
        sys.exit(0)
    print("-FLUSH-")
    sys.stdout.flush()

def __EpmdConnectedOk(dest, atom_to_send):
    global mb
    msg = (mb.Self(), erl_term.ErlAtom(atom_to_send))
    mb.Send(dest, msg)

def main(argv):
    global mb, quiet

    try:
        opts, args = getopt.getopt(argv[1:], "?dn:c:q")
    except getopt.error as info:
        print(info)
        sys.exit(1)

    hostName = "localhost"
    ownNodeName = "py_interface_test"
    cookie = "cookie"
    doDebug = 0

    for (optchar, optarg) in opts:
        if optchar == "-?":
            print("Usage: %s erlnode" % argv[0])
            sys.exit(1)
        elif optchar == "-c":
            cookie = optarg
        elif optchar == "-d":
            doDebug = 1
        elif optchar == "-q":
            quiet = 1
        elif optchar == "-n":
            ownNodeName = optarg

    if len(args) >= 3:
        pid_name_to_connect_to = args[0]
        node_to_connect_to = args[1]
        atom_to_send = args[2]
    else:
        print("Error: missing args: " +
              "pid_name_to_connect_to node_to_connect_to atom_to_send")
        sys.exit(1)

    if doDebug:
        erl_common.DebugOnAll()

    dest = (pid_name_to_connect_to, node_to_connect_to)

    print("Creating node...")
    n = erl_node.ErlNode(ownNodeName, erl_opts.ErlNodeOpts(cookie=cookie))
    n.SetEpmdConnectedOkCb(erl_common.Callback(__EpmdConnectedOk,
                                               dest, atom_to_send))
    print("Publishing node...")
    n.Publish()
    print("Creating mbox...")
    mb = n.CreateMBox(None)
    m = n.CreateMBox(__TestMBoxCallback)

    evhand = erl_eventhandler.GetEventHandler()
    evhand.PushReadEvent(sys.stdin, __FlushStdout)

    print("-GOING LOOPING-") # Erlang side will pick this text up
    sys.stdout.flush()

    evhand.Loop()

try:
    main(sys.argv)
except KeyboardInterrupt:
    print("Interrupted. Exiting.")
    sys.exit(1)
