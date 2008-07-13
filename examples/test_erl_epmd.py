#! /usr/bin/env python

import sys
import types
import string
import socket
import getopt

from py_interface import erl_epmd
from py_interface import erl_common
from py_interface import erl_async_conn
from py_interface import erl_eventhandler

e = None

def TestAliveOkResp(creation):
    print "AliveOkResp creation=%d" % creation

def TestAliveNotOkResp(self):
    print "AliveNotOkResp"

def TestAlive2Resp(result, creation):
    print "Alive2Resp, result=%d, creation=%d" % (result, creation)

def TestAlive2RespConnected(creation):
    print "Alive2RespConnected, creation=%d" % creation
    nodeToCheckFor = "flerp"
    print "Checking for node named \"%s\"." % nodeToCheckFor
    e.PortPlease2Req(nodeToCheckFor, TestPort2Resp)

def TestAlive2RespConnectFailed(result):
    print "Alive2RespConnectFailed, result=%d" % result

def TestPortOkResp(portNum):
    print "PortOkResp, portNum=%d" % portNum

def TestPortNotOkResp(self):
    print "PortNotOkResp"

def TestPort2Resp(result, portNum, nodeType, proto, distr, nodeName, extra):
    if result == 0:
        # found
        print ("Port2Resp, result=ok, portNum=%d, nodeType=%d, protocol=%d," +
               " distrVSNRange=%s, nodeName=\"%s\", extra=\"%s\"") % \
               (portNum, nodeType, proto, `distr`, nodeName, extra)
    else:
        # not found
        print "Port2Resp, result=%d" % result
        

def TestNamesResp(epmdPortNum, nodeInfo):
    print "NamesResp, epmdPortNum=%d nodeInfo:\n%s" % \
          (epmdPortNum, nodeInfo)

def TestDumpResp(epmdPortNum, nodeInfo):
    print "DumpResp, epmdPortNum=%d nodeInfo:\n%s" % \
          (epmdPortNum, nodeInfo)

def TestKillResp(resp):
    print "KillResp, resp=%s" % resp

def TestStopResp(resp):
    print "StopResp, resp=%s" % resp

def TestConnectionClosed():
    print "Connection to epmd has been closed."

def main(argv):
    global e

    try:
        opts, args = getopt.getopt(argv[1:], "?p:n:")
    except getopt.error, info:
        print info

    hostName = "localhost"
    portNum = 4369
    ownPortNum = 1234
    ownNodeName = "py_interface_test"

    for (optchar, optarg) in opts:
        if optchar == "-?":
            print "Usage: %s host [port]" % argv[0]
            sys.exit(1)
        elif optchar == "-p":
            ownPortNum = string.atoi(optarg)
        elif optchar == "-n":
            ownNodeName = optarg

    if len(args) >= 2:
        hostName = args[0]
        portNum = string.atoi(args[1])
    elif len(args) == 1:
        hostName = args[0]

    e = erl_epmd.ErlEpmd(hostName, portNum)
    e.SetOwnPortNum(ownPortNum)
    e.SetOwnNodeName(ownNodeName)
    e.Connect(TestAlive2RespConnected, TestAlive2RespConnectFailed)
    evhandler = erl_eventhandler.GetEventHandler()
    evhandler.Loop()


main(sys.argv)

