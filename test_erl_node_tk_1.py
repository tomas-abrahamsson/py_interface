#! /usr/bin/env python

import sys
import types
import string
import getopt
from Tkinter import *


import erl_term
import erl_node
import eventhandler

class TkTest:
    def __init__(self, top, node, mbox):
        self.node = node
        self.mbox = mbox

        self.evhand = eventhandler.GetEventHandler()
        if sys.stdin.isatty():
            # only useful if stdin might generate KeyboardInterrupt
            self.__CheckKbdInterrupt()
    
        f = Frame(top)
        b = Button(f, text="Send", borderwidth=1,
                   command=self.Send)
        b.pack(side=LEFT)
        dg = Label(f, text="Dest:")
        dg.pack(side=LEFT)
        self.destVar = StringVar()
        self.destVar.set("(\"xxsh\",\"address@xanadu\")")
        de = Entry(f, borderwidth=1, textvariable=self.destVar)
        de.pack(side=LEFT)
        mg = Label(f, text="Dest:")
        mg.pack(side=LEFT)
        self.msgVar = StringVar()
        self.msgVar.set("\"hej\"")
        me = Entry(f, borderwidth=1, textvariable=self.msgVar)
        me.pack(side=LEFT)
        f.pack()

    def __CheckKbdInterrupt(self):
        # Exercise the Python interpreter regularly so keyboard
        # interrupts get through
        self.evhand.AddTimerEvent(0.25, self.__CheckKbdInterrupt)        

    def Send(self, event=None):
        dest = eval(self.destVar.get())
        msg = eval(self.msgVar.get())
        print "Sending:"
        print "  dest=%s" % `dest`
        print "  msg =%s" % `msg`
        self.mbox.Send(dest, msg)


def __TestMBoxCallback(msg):
    print "Incoming msg=%s" % `msg`


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

    top = Tk()
    eventhandler.SetEventHandlerStateTk(top)


    print "Creating node..."
    n = erl_node.ErlNode(ownNodeName, cookie)
    print "Publishing node..."
    n.Publish()
    print "Creating mbox..."
    m = n.CreateMBox(__TestMBoxCallback)
    print "Registering mbox as p..."
    m.RegisterName("p")

    TkTest(top, n, m)

    print "Looping..."
    evhand = eventhandler.GetEventHandler()
    evhand.Loop()

try:
    main(sys.argv)
except KeyboardInterrupt:
    print "Interrupted. Exiting."
    sys.exit(1)
