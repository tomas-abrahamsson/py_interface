import sys
import types
import string
import getopt


import erl_epmd
import erl_term
import erl_common
import common
import eventhandler
import erl_node_conn


DISTR_FLAGS_PUBLISHED = 1;
DISTR_FLAGS_ATOMCACHE = 2;
DISTR_FLAGS_EXTENDEDREFERENCES = 4;
DISTR_FLAGS_DISTMONITOR = 8;
DISTR_FLAGS_FUNTAGS = 16;


_pidCount = 1
_serial = 0

class ErlMBox:
    def __init__(self, node, pid, msgCallback):
        self._node = node
        if msgCallback == None:
            msgCallback = self._Sink
        self._msgCallback = msgCallback
        self._pid = pid

    def Self(self):
        return self._pid

    def Send(self, dest, msg):
        self._node.SendMsgFromMBox(self, dest, msg)

    def RegisterName(self, name):
        self._node.RegisterNameForMBox(self, name)

    def UnregisterName(self):
        self._node.UnregisterNameForMBox(self)

    def Link(self, otherEnd):
        pass

    def Unlink(self, otherEnd):
        pass

    def SendRPC(self, remote_node, mod, fun, args, cb):
        if type(mod) == types.StringType:
            mod = erl_term.ErlAtom(mod)
        if type(fun) == types.StringType:
            fun = erl_term.ErlAtom(fun)
        self.Send(("rex", remote_node),
                  (self.Self(),
                   (erl_term.ErlAtom("call"),
                    mod, fun, args, erl_term.ErlAtom("user"))))

    ##
    ## Routines to be called from the node only
    ##
    def Msg(self, msg):
        self._msgCallback(msg)

    def _Sink(self, *a, **kw):
        pass


class ErlNode:
    # Early operations 
    CTRLMSGOP_LINK = 1
    CTRLMSGOP_SEND = 2
    CTRLMSGOP_EXIT = 3
    CTRLMSGOP_UNLINK = 4
    CTRLMSGOP_NODE_LINK = 5
    CTRLMSGOP_REG_SEND = 6
    CTRLMSGOP_GROUP_LEADER = 7
    CTRLMSGOP_EXIT2 = 8
    # New operations in destrvsn = 1 (OTP R4)
    CTRLMSGOP_SEND_TT = 12
    CTRLMSGOP_EXIT_TT = 13
    CTRLMSGOP_REG_SEND_TT = 16
    CTRLMSGOP_EXIT2_TT = 18
    # New operations in destrvsn = 4 (OTP R6)
    CTRLMSGOP_MONITOR_P = 19
    CTRLMSGOP_DEMONITOR_P = 20
    CTRLMSGOP_MONITOR_P_EXIT = 21
    # end of operations
    
    def __init__(self, nodeName, cookie="",
                 distrVersion = 5,
                 distrFlags = DISTR_FLAGS_EXTENDEDREFERENCES):
        self._nodeName = erl_common.NodeNameMaybeAddHostName(nodeName)
        self._cookie = cookie
        self._distrVersion = distrVersion
        self._distrFlags = distrFlags

        self._creation = 0
        self._connections = {}

        self._epmd = erl_epmd.ErlEpmd()
        self._ongoingPings = {}

        self._isServerPublished = 0
        self._pids = {}                 # mapping pid     --> ErlMBox()
        self._mboxes = {}               # mapping ErlMBox --> pid
        self._registeredNames = {}      # mapping name    --> pid
        self._registeredPids = {}       # mapping pid     --> name


        self._server = erl_node_conn.ErlNodeServerSocket(self._nodeName,
                                                         self._cookie,
                                                         self._distrVersion,
                                                         self._distrFlags)
        self._portNum = self._server.Start(self._NodeUp, self._NodeDown,
                                           self._PassThroughMsg)
        self._epmd.SetOwnPortNum(self._portNum)
        self._epmd.SetOwnNodeName(self._nodeName)

        
    def CreateMBox(self, msgCallback=None):
        mboxPid = self._CreatePid()
        mbox = ErlMBox(self, mboxPid, msgCallback)
        self._pids[mboxPid] = mbox
        self._mboxes[mbox] = mboxPid
        return mbox

    def Ping(self, remoteNodeName, pingCallback):
        if not "@" in remoteNodeName:
            raise "Bad node name for remote node"
        if self._ongoingPings.has_key(remoteNodeName):
            pingCallbacks = self._ongoingPings[remoteNodeName]
            self._ongoingPings[remoteNodeName] = pingCallbacks + [pingCallback]
        else:
            self._ongoingPings[remoteNodeName] = [pingCallback]
            [nodeName, hostName] = string.split(remoteNodeName, "@")
            e = erl_epmd.ErlEpmd(hostName)
            cb = common.Callback(self._PingEpmdResponse, remoteNodeName)
            e.PortPlease2Req(nodeName, cb)

    def Publish(self):
        if not self._isServerPublished:
            self._epmd.Connect(self._EpmdConnectedOk, self._EpmdConnectFailed)

    def Unpublish(self):
        if self._isServerPublished:
            self._epmd.Close()
            self._isServerPublished = 0

    def DumpConnections(self):
        print "Connections:"
        for k in self._connections.keys():
            print "  %s --> %s" % (`k`, `self._connections[k]`)
        print "--"


    ##
    ## Routines to be called only by mboxes
    ##

    def RegisterNameForMBox(self, mbox, name):
        mboxPid = mbox.Self()
        if self._registeredNames.has_key(name):
            raise "IsRegistered"
        if self._registeredPids.has_key(mboxPid):
            raise "IsRegistered"
        self._registeredNames[name] = mboxPid
        self._registeredPids[mboxPid] = name

    def UnregisterNameForMBox(self, mbox):
        mboxPid = mbox.Self()
        if not self._registeredPids.has_key(mboxPid):
            raise "NotRegistered"
        name = self._registeredPids[mboxPid]
        del self._registeredPids[mboxPid]
        del self._registeredNames[name]


    def SendMsgFromMBox(self, sourceMBox, dest, msg):
        ## Possible dest types:
        ## - A tuple: (registered_name, node_name)
        ## - An atom: registered_name
        ## - A pid:   <erlpid ...>  (note: the pid contains the pid's node)

        sourcePid = self._mboxes[sourceMBox]

        ## First check for strings in the dest argument.
        ## Convert any strings to to atoms.
        if type(dest) == types.StringType:
            dest = erl_term.ErlAtom(dest)
        elif type(dest) == types.TupleType:
            destPidName = dest[0]
            destNode = dest[1]
            if type(destPidName) == types.StringType:
                destPidName = erl_term.ErlAtom(destPidName)
            if type(destNode) == types.StringType:
                destNode = erl_term.ErlAtom(destNode)
            dest = (destPidName, destNode)

        ## Then split the dest into:
        ##    destPid/destPidName   and   destNode
        ## depending on its type.
        if type(dest) == types.TupleType:
            destPid = dest[0]
            destNode = dest[1]
        elif erl_term.IsErlAtom(dest):
            destNode = self
            name = dest.atomText
            if not self._registeredNames.has_key(name):
                return
            destPid = self._registeredNames[name]
        elif erl_term.IsErlPid(dest):
            destPid = dest
            destNode = dest.node
        else:
            return

        ## Now do the sending...
        if destNode == self:
            if not self._registeredPids.has_key(destPid):
                return
            mbox = self._registredPids[destPid]
            mbox.Msg(msg)
        else:                           # dest node is remote
            # First make sure we are online
            # FIXME: Will this really work???
            #        Publish might register callbacks, but
            #        this code continues after the Publish...
            self.Publish()
            destNodeName = destNode.atomText
            if not self._connections.has_key(destNodeName):
                # We don't have a connection to the destination
                # We must open a connection.
                # This is done by pinging with the ping-callback
                # being a function that sends the message.

                cb = common.Callback(self._SendMsgToRemoteNode,
                                     sourcePid, destNode, destPid, msg)
                destNodeName = destNode.atomText
                self.Ping(destNodeName, cb)
            else:
                ## We have contact with the remote node. Send at once!
                self._SendMsgToRemoteNode("pong",
                                          sourcePid, destNode, destPid, msg)

    ##
    ## Internal routines
    ##

    def _CreatePid(self):
        ## Code stolen from com/ericsson/otp/erlang/OtpLocalNode.java
        global _serial, _pidCount
        newPid = erl_term.ErlPid(erl_term.ErlAtom(self._nodeName),
                                 _pidCount, _serial, self._creation)
        _pidCount = _pidCount + 1
        if _pidCount > 0x7fff:
            _pidCount = 0
            _serial = _serial + 1
            if _serial > 0x07:
                _serial = 0
        return newPid

    def _EpmdConnectedOk(self, creation):
        self._isServerPublished = 1
        self._creation = creation

    def _EpmdConnectFailed(self, errorResult):
        raise "Failed to connect to epmd (%d)" % errorResult

    def _NodeUp(self, connection, nodeName):
        erl_common.Debug("NODEUP: nodeName=%s connection=%s" % \
                         (nodeName, connection))
        self._connections[nodeName] = connection

    def _NodeDown(self, connection, nodeName):
        erl_common.Debug("NODENOWN: nodeName=%s connection=%s" % \
                         (nodeName, connection))
        if self._connections.has_key(nodeName):
            del self._connections[nodeName]
    
    def _PingEpmdResponse(self, result, portNum, nodeType, proto,
                          distVSNRange, nodeNameNoHost, extra,
                          remoteNodeName):
        if result != 0:
            callbacks = self._ongoingPings[remoteNodeName]
            del self._ongoingPings[remoteNodeName]
            for cb in callbacks:
                cb("pang")
        else:
            [otherNode, otherHost] = string.split(remoteNodeName, "@")
            out = erl_node_conn.ErlNodeOutConnection(self._nodeName,
                                                     self._cookie,
                                                     self._distrVersion,
                                                     self._distrFlags)
            connectedOkCb = common.Callback(self._PingSucceeded,
                                            out, remoteNodeName)
            connectFailedCb = common.Callback(self._PingFailed,
                                              out, remoteNodeName)
            connectionBrokenCb = common.Callback(self._NodeDown,
                                                 out, remoteNodeName)
            passThroughMsgCb = common.Callback(self._PassThroughMsg,
                                               out, remoteNodeName)
            out.InitiateConnection(otherHost, portNum,
                                   connectedOkCb,
                                   connectFailedCb,
                                   connectionBrokenCb,
                                   self._PassThroughMsg)

    def _PingSucceeded(self, connection, remoteNodeName):
        callbacks = self._ongoingPings[remoteNodeName]
        del self._ongoingPings[remoteNodeName]
        self._NodeUp(connection, remoteNodeName)
        for cb in callbacks:
            cb("pong")

    def _PingFailed(self, connection, remoteNodeName):
        callbacks = self._ongoingPings[remoteNodeName]
        del self._ongoingPings[remoteNodeName]
        for cb in callbacks:
            cb("pang")
        
    def _OutConnectionBroken(self, connection, remoteNodeName):
        if self._connections.has_key(remoteNodeName):
            del self._connections[remoteNodeName]

    def _PassThroughMsg(self, connection, remoteNodeName, ctrlMsg, msg=None):
        erl_common.Debug("ctrlMsg=%s" % `ctrlMsg`)

        ctrlMsgOp = ctrlMsg[0]
        if ctrlMsgOp == self.CTRLMSGOP_LINK:
            fromPid = ctrlMsg[1]
            toPid = ctrlMsg[2]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_SEND:
            cookie = ctrlMsg[1]
            toPid = ctrlMsg[2]
            msg = msg
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_EXIT:
            fromPid = ctrlMsg[1]
            toPid = ctrlMsg[2]
            reason = ctrlMsg[3]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_UNLINK:
            fromPid = ctrlMsg[1]
            toPid = ctrlMsg[2]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_NODE_LINK:
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_REG_SEND:
            fromPid = ctrlMsg[1]
            cookie = ctrlMsg[2]
            toNameAtom = ctrlMsg[3]
            toName = toNameAtom.atomText
            msg = msg
            if self._registeredNames.has_key(toName):
                mboxPid = self._registeredNames[toName]
                mbox = self._pids[mboxPid]
                mbox.Msg(msg)
            else:
                erl_common.Debug("Got REG_SEND with no dest mbox: \"%s\": %s" %
                                 (toName, msg))
        elif ctrlMsgOp == self.CTRLMSGOP_GROUP_LEADER:
            fromPid = ctrlMsg[1]
            toPid = ctrlMsg[2]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_EXIT2:
            fromPid = ctrlMsg[1]
            toPid = ctrlMsg[2]
            reason = ctrlMsg[3]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_SEND_TT:
            cookie = ctrlMsg[1]
            toPid = ctrlMsg[2]
            traceToken = ctrlMsg[3]
            msg = msg
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_EXIT_TT:
            fromPid = ctrlMsg[1]
            toPid = ctrlMsg[2]
            traceToken = ctrlMsg[3]
            reason = ctrlMsg[4]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_REG_SEND_TT:
            fromPid = ctrlMsg[1]
            cookie = ctrlMsg[2]
            toName = ctrlMsg[3]
            traceToken = ctrlMsg[4]
            msg = msg
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_EXIT2_TT:
            fromPid = ctrlMsg[1]
            toPid = ctrlMsg[2]
            traceToken = ctrlMsg[3]
            reason = ctrlMsg[4]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_MONITOR_P:
            fromPid = ctrlMsg[1]
            toPid = ctrlMsg[2]
            ref = ctrlMsg[3]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_DEMONITOR_P:
            fromPid = ctrlMsg[1]
            toPid = ctrlMsg[2]
            ref = ctrlMsg[3]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_MONITOR_P_EXIT:
            fromPid = ctrlMsg[1]
            toPid = ctrlMsg[2]
            ref = ctrlMsg[3]
            pass
        else:
            erl_common.Debug("Unknown controlmsg: %s" % `ctrlMsg`)

    def _SendMsgToRemoteNode(self, pingResult, srcPid, destNode, destPid, msg):
        if pingResult != "pong":
            return
        destNodeName = destNode.atomText
        if not self._connections.has_key(destNodeName):
            return
        conn = self._connections[destNodeName]
        cookie = erl_term.ErlAtom("")
        if erl_term.IsErlAtom(destPid):
            ctrlMsg = (self.CTRLMSGOP_REG_SEND, srcPid, cookie, destPid)
        else:
            ctrlMsg = (self.CTRLMSGOP_SEND, cookie, destPid)
        conn.SendMsg(ctrlMsg, msg)


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
def testmain(argv):
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
    n = ErlNode(ownNodeName, cookie)
    print "Publishing node..."
    n.Publish()
    print "Creating mbox..."
    m = n.CreateMBox(__TestMBoxCallback)
    print "Registering mbox as p..."
    m.RegisterName("p")

    print "Looping..."
    evhand = eventhandler.GetEventHandler()
    evhand.Loop()


if __name__ == '__main__':
    testmain(sys.argv)
