#! /usr/bin/env python

import sys
import types
import string
import socket
import getopt
from Tkinter import *


from py_interface import erl_term
from py_interface import erl_node
from py_interface import erl_opts
from py_interface import erl_common
from py_interface import erl_eventhandler

class TkTest:
    def __init__(self, top, node, mbox):
        self.node = node
        self.mbox = mbox

        self.evhand = erl_eventhandler.GetEventHandler()
        if sys.stdin.isatty():
            # only useful if stdin might generate KeyboardInterrupt
            self.__CheckKbdInterrupt()
    
        f1 = Frame(top)
        self.destVar = StringVar()
        self.msgVar = StringVar()
        self.destVar.set("(\"xxsh\",\"address@%s\")" % socket.gethostname())
        self.msgVar.set("\"hej\"")
        b = Button(f1, text="Send", borderwidth=1,
                   command=self.Send)
        b.pack(side=LEFT)
        dg = Label(f1, text="Dest:")
        de = Entry(f1, borderwidth=1, textvariable=self.destVar)
        mg = Label(f1, text="Msg:")
        me = Entry(f1, borderwidth=1, textvariable=self.msgVar)
        dg.pack(side=LEFT)
        de.pack(side=LEFT)
        mg.pack(side=LEFT)
        me.pack(side=LEFT)
        f1.pack()

        f2 = Frame(top)
        b2 = Button(f2, text="SendRPC", borderwidth=1, command=self.SendRPC)
        b2.pack(side=LEFT)
        self.remoteNodeVar = StringVar()
        self.modVar = StringVar()
        self.funVar = StringVar()
        self.argsVar = StringVar()
        self.remoteNodeVar.set("\"'address@%s'\"" % socket.gethostname())
        self.modVar.set("\"'io'\"")
        self.funVar.set("\"'format'\"")
        self.argsVar.set("[\"hej!~nhopp!~n\", []]")
        rnl = Label(f2, text="remotenode:")
        rne = Entry(f2, borderwidth=1, textvariable=self.remoteNodeVar)
        modl = Label(f2, text="mod:")
        mode = Entry(f2, borderwidth=1, width=10, textvariable=self.modVar)
        funl = Label(f2, text="fun:")
        fune = Entry(f2, borderwidth=1, width=10, textvariable=self.funVar)
        argsl = Label(f2, text="args:")
        argse = Entry(f2, borderwidth=1, textvariable=self.argsVar)
        rnl.pack(side=LEFT)
        rne.pack(side=LEFT)
        modl.pack(side=LEFT)
        mode.pack(side=LEFT)
        funl.pack(side=LEFT)
        fune.pack(side=LEFT)
        argsl.pack(side=LEFT)
        argse.pack(side=LEFT)
        f2.pack()

        f3 = Frame(top)
        b3 = Button(f3, text="Dump node connections",
                    borderwidth=1, command=self.NDump)
        b3.pack(side=LEFT)
        f3.pack()

    def __CheckKbdInterrupt(self):
        # Exercise the Python interpreter regularly so keyboard
        # interrupts get through
        self.evhand.AddTimerEvent(0.25, self.__CheckKbdInterrupt)        

    def Send(self, event=None):
        dest = eval(self.destVar.get())
        msg = ExprRebuildAtoms(eval(self.msgVar.get()))
        msgCooked = ExprRebuildAtoms(msg)
        print "Sending:"
        print "  dest=%s" % `dest`
        print "  msg =%s" % `msg`
        self.mbox.Send(dest, msg)

    def SendRPC(self, event=None):
        n = ExprRebuildAtoms(eval(self.remoteNodeVar.get()))
        m = ExprRebuildAtoms(eval(self.modVar.get()))
        f = ExprRebuildAtoms(eval(self.funVar.get()))
        a = ExprRebuildAtoms(eval(self.argsVar.get()))
        print "Sending:"
        print "  n=%s" % `n`
        print "  m=%s" % `m`
        print "  f=%s" % `f`
        print "  a=%s" % `a`
        self.mbox.SendRPC(n, m, f, a, self._TestMBoxRPCResponse)


    def _TestMBoxCallback(self, msg, *x, **kw):
        print "Incoming msg=%s" % `msg`

    def _TestMBoxRPCResponse(self, msg):
        print "RPC answer: %s" % `msg`

    def NDump(self, event=None):
        self.node.DumpConnections()



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
        



def __TestMBoxCallback(msg):
    print "Incoming msg=%s" % `msg`


def main(argv):
    try:
        opts, args = getopt.getopt(argv[1:], "?dn:c:")
    except getopt.error, info:
        print info
        sys.exit(1)

    hostName = "localhost"
    ownNodeName = "py_interface_test"
    cookie = "cookie"
    doDebug = 0

    for (optchar, optarg) in opts:
        if optchar == "-?":
            print "Usage: %s erlnode" % argv[0]
            sys.exit(1)
        elif optchar == "-c":
            cookie = optarg
        elif optchar == "-d":
            doDebug = 1
        elif optchar == "-n":
            ownNodeName = optarg

    top = Tk()
    erl_eventhandler.SetEventHandlerStateTk(top)


    if doDebug:
        erl_common.DebugOnAll()

    print "Creating node..."
    n = erl_node.ErlNode(ownNodeName, erl_opts.ErlNodeOpts(cookie=cookie))
    print "Publishing node..."
    n.Publish()
    print "Creating mbox..."
    m = n.CreateMBox(__TestMBoxCallback)
    print "Registering mbox as p..."
    m.RegisterName("p")

    TkTest(top, n, m)

    print "Looping..."
    evhand = erl_eventhandler.GetEventHandler()
    evhand.Loop()

try:
    main(sys.argv)
except KeyboardInterrupt:
    print "Interrupted. Exiting."
    sys.exit(1)
