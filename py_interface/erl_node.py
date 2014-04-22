### py_interface -- A Python-implementation of an Erlang node
###
### $Id$
###
### Copyright (C) 2002  Tomas Abrahamsson
###
### Author: Tomas Abrahamsson <tab@lysator.liu.se>
###
### This file is part of the Py-Interface library
###
### This library is free software; you can redistribute it and/or
### modify it under the terms of the GNU Library General Public
### License as published by the Free Software Foundation; either
### version 2 of the License, or (at your option) any later version.
###
### This library is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### Library General Public License for more details.
###
### You should have received a copy of the GNU Library General Public
### License along with this library; if not, write to the Free
### Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

### erl_node.py -- the node

import sys
import types
import string


from py_interface import erl_epmd
from py_interface import erl_term
from py_interface import erl_opts
from py_interface import erl_common
from py_interface import erl_node_conn
from py_interface import erl_eventhandler



M = "erl_node"

py_interface_version = "0.9"

_pidCount = 1
_serial = 0

class ErlNodeBadPeerNameError(Exception):
    def __init__(self, reason):
        self.reason = reason

class ErlNodeIsRegisteredError(Exception):
    def __init__(self, reason):
        self.reason = reason

class ErlNodeNotRegisteredError(Exception):
    def __init__(self, reason):
        self.reason = reason

class ErlNodeConnectionFailedError(Exception):
    def __init__(self, reason):
        self.reason = reason


class ErlMBox:
    """This class provides an mbox, which is equivalent to an
    erlang-process. It is intenteded to be used in a single-threaded
    application. You must provide a callback routine for incoming
    messages."""

    def __init__(self, node, pid, msgCallback):
        """The recommended way to create mboxes is through the method
        CreateMBox in the ErlNode class.

        NODE            = <instance of ErlNode>
                        The node to which this mbox belongs.
        PID             = <instance of ErlPid>
                        The pid for this mbox
        MSG-CALLBACK    = <function(MSG): void>
                        A callback to call for incoming messages
                        to this mbox. The callback should take one
                        argument: the message. Its return value is
                        ignored.
        """
        self._node = node
        if msgCallback == None:
            msgCallback = self._Sink
        self._msgCallback = msgCallback
        self._pid = pid
        self._pendingRPCs = {}

    def Self(self):
        """Return the pid for this mbox.

        Returns: <instance of ErlPid>
        Throws:  nothing
        """
        return self._pid

    def Send(self, dest, msg):
        """Send, to DEST, the message MSG.

        DEST    = <instance of ErlPid> | string | <instance of ErlAtom> |
                  tuple(PROC-NAME, NODE)
                  PROC-NAME = NODE = string | <instance of ErlAtom>
                The destination to which the message is to be sent.
                This can is either a pid for an mbox (on the same node or on a
                different node), or a registered name (as string or atom) on
                the same node, or a tuple specifying a node and a registered
                name on that node.

        MSG     = <term>
                The message to be sent.

        Returns: void
        Throws:  <<to-be-documented>>

        Send a message. There is no result indicating whether the delivery
        of the message was successful or not. If the DEST is located on
        another node, a connection to that node is automatically set up
        and maintained, unless already connected.

        This method returns immediately. Any necessarry queuing,
        possibly due to network traffic congestion, is handled
        automatically by the node.
        """
        self._node.SendMsgFromMBox(self, dest, msg)

    def RegisterName(self, name):
        """Register a NAME for this mbox.

        NAME    = string | <instance of ErlAtom>
                The name to register this mbox as.

        Returns: void
        Throws:  <<to-be-documented>>

        Only on name can be registered for each mbox.
        """
        self._node.RegisterNameForMBox(self, name)

    def UnregisterName(self):
        """Unregister the name that was registered for this mbox.
        Returns: void
        Throws:  <<to-be-documented>>
        """
        self._node.UnregisterNameForMBox(self)

    def Link(self, otherEnd):
        """Link this mbox to another pid/mbox.
        Currently, this is not implemented."""
        pass

    def Unlink(self, otherEnd):
        """Unlink another pid/mbox.
        Currently, this is not implemented."""
        pass

    def SendRPC(self, remoteNode, mod, fun, args, cb):
        """Send an rpc to REMOTE-NODE, MOD, FUN, ARGS. Call CB for the answer.

        REMOTE-NODE     = string | <instance of ErlAtom>
                        The node to send the call to
        MOD             =  string | <instance of ErlAtom>
                        The module
        FUN             = string | <instance of ErlAtom>
                        The name of the function to call
        ARGS            = list(<term>)
                        The argument list
        CB              = <function(RESULT): void>
                          REMOTE-NODE = string
                          RESULT = <term>
                        A callback function to be called when the answer
                        to the rpc callback receives. The callback is called
                        with one arg: the result. Its return value is ignored.
                        If the remote node goes down during execution, the
                        callback is called with this value:
                          tuple(<ErlAtom("EXIT")>,
                                tuple(<ErlAtom("nodedown")>,
                                      <ErlAtom(<REMOTE-NODE-NAME>)>))

        Returns: void
        Throws:  <<to-be-documented>>

        Send an request to execute a function in a module on a remote node.
        As for the Send method, this method returns immediately.
        """
        rexMBox = self._node.WhereisMBox("rex")
        rexMBox.SendRPC(remoteNode, mod, fun, args, cb)

    ##
    ## Routines to be called from the node only
    ##
    def Msg(self, sourceNodeName, msg):
        """This routine is intended to be called only from mbox's node.

        SOURCE-NODE-NAME        = string
        MSG                     = <term>

        Returns: void
        Throws:  nothing

        An incoming message
        """
        self._msgCallback(msg)

    def _Sink(self, *a, **kw):
        """Message sink."""
        pass


class _ErlRexMBox(ErlMBox):
    """This class implements what's needed for the rpc (remote procedure call)
    functionality. It is an mbox that registers itself as "rex".
    """

    def __init__(self, node, pid):
        ErlMBox.__init__(self, node, pid, None)
        self._nodeDownSubscriptions = {}

    def Start(self):
        self.RegisterName("rex")


    def SendRPC(self, remoteNode, mod, fun, args, cb):
        """Send an rpc to REMOTE-NODE, MOD, FUN, ARGS. Call CB for the answer.

        REMOTE-NODE     = string | <instance of ErlAtom>
                        The node to send the call to
        MOD             =  string | <instance of ErlAtom>
                        The module
        FUN             = string | <instance of ErlAtom>
                        The name of the function to call
        ARGS            = list(<term>)
                        The argument list
        CB              = <function(RESULT): void>
                          REMOTE-NODE = string
                          RESULT = <term>
                        A callback function to be called when the answer
                        to the rpc callback receives. The callback is called
                        with one arg: the result. Its return value is ignored.
                        If the remote node goes down during execution, the
                        callback is called with this value:
                          tuple(<ErlAtom("EXIT")>,
                                tuple(<ErlAtom("nodedown")>,
                                      <ErlAtom(<REMOTE-NODE-NAME>)>))

        Returns: void
        Throws:  <<to-be-documented>>

        Send an request to execute a function in a module on a remote node.
        As for the Send method, this method returns immediately.
        """
        if type(mod) == types.StringType:
            mod = erl_term.ErlAtom(mod)
        if type(fun) == types.StringType:
            fun = erl_term.ErlAtom(fun)

        # Handle queue of pending callbacks for the remote node
        if type(remoteNode) == types.StringType:
            remoteNodeName = remoteNode
        elif erl_term.IsErlAtom(remoteNode):
            remoteNodeName = remoteNode.atomText
        if self._pendingRPCs.has_key(remoteNodeName):
            self._pendingRPCs[remoteNodeName].append(cb)
        else:
            self._pendingRPCs[remoteNodeName] = [cb]

        # Register a nodedown-callback for this node, so
        # we can call any rpc callbacks in case the node goes down
        if not self._nodeDownSubscriptions.has_key(remoteNodeName):
            id = self._node.NodeDownSubscribe(self._NodeDown)
            self._nodeDownSubscriptions[remoteNodeName] = id

        # Now send the rpc-message
        self.Send(("rex", remoteNode),
                  (self.Self(),
                   (erl_term.ErlAtom("call"),
                    mod, fun, args, erl_term.ErlAtom("user"))))


    ##
    ## Routines to be called from the node only
    ##
    def Msg(self, sourceNodeName, msg):
        """This routine is intended to be called only from mbox's node.

        SOURCE-NODE-NAME        = string
        MSG                     = <term>

        Returns: void
        Throws:  nothing

        An incoming message
        """
        if type(msg) == types.TupleType and \
           len(msg) == 2 and \
           erl_term.IsErlAtom(msg[0]) and \
           msg[0].atomText == "rex" and \
           len(self._pendingRPCs) > 0:
            self._RPCAnswer(sourceNodeName, msg[1])
        else:
            erl_common.Debug("REX: Unexpected msg: %s" % `msg`)

    def _RPCAnswer(self, sourceNodeName, answer):
        # Does this assumption always hold:
        # first answer is for first rpc-call?
        # Maybe this holds for calls/answers for one single node.
        # So the pendingRPCs is one queue per node.
        if self._pendingRPCs.has_key(sourceNodeName):
            pendingRPCs = self._pendingRPCs[sourceNodeName]
            cb = pendingRPCs[0]
            self._pendingRPCs[sourceNodeName] = pendingRPCs[1:]
            cb(answer)

    def _NodeDown(self, nodeStatus, nodeName):
        if self._pendingRPCs.has_key(nodeName):
            callbacks = self._pendingRPCs[nodeName]
            self._pendingRPCs[nodeName] = []
            for cb in callbacks:
                cb((erl_term.ErlAtom("EXIT"),
                    (erl_term.ErlAtom("nodedown"),
                     erl_term.ErlAtom(nodeName))))


class ErlNode:
    """This class implements a node, which is equivaluent to an erlang node.
    It is intended to be used in a single-threaded applucation.

    MBoxes, which corresponds to erlang processes, can be created for
    communication.
    """
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

    def __init__(self, nodeName, opts=erl_opts.ErlNodeOpts()):
        """Constructor.

        NODE-NAME       = string
                        The name for this node.

        OPTS            = <instance of ErlNodeOpts>

        Creates an ErlNode. The name of the node is determined by NODE-NAME
        as described below. A node-name consists of two parts: an alive-name
        and a host-name, separated by an `@'. The host-name can be short
        (not including the domain) or long (including the domain). Short and
        long node-names must not be mixed among the nodes in a system, see
        the erlang documentation for further details.
          1. If the NODE-NAME contains an `@', then NODE-NAME is used
             unchanged as name for the node.
          2. If the NODE-NAME does not contain an `@', then the node's name
             is constructed as NODE-NAME + "@" + host-name, where
             host-name is either on short or long form, depending on what is
             specified in the OPTS.
        """
        shortNodeNames = opts.GetShortNodeNames()
        self._nodeName = erl_common.AlignNodeName(nodeName, shortNodeNames)
        self._opts = opts

        self._creation = 0
        self._connections = {}

        self._epmd = erl_epmd.ErlEpmd()
        self._ongoingPings = {}

        self._isServerPublished = 0
        self._pids = {}                 # mapping pid     --> ErlMBox()
        self._mboxes = {}               # mapping ErlMBox --> pid
        self._registeredNames = {}      # mapping name    --> pid
        self._registeredPids = {}       # mapping pid     --> name

        self._nodeUpCb = []             # stores (id, callback)
        self._nodeDownCb = []           # stores (id, callback)
        self._cbId = 0

        self._server = erl_node_conn.ErlNodeServerSocket(self._nodeName,
                                                         self._opts)
        self._portNum = self._server.Start(self._NodeUp, self._NodeDown,
                                           self._PassThroughMsg)
        self._epmd.SetOwnPortNum(self._portNum)
        self._epmd.SetOwnNodeName(self._nodeName)
        self._CreateRex()

    def CreateMBox(self, msgCallback=None):
        """Creates an mbox, which is equivalent to an erlang process.

        MSG-CALLBACK    = <function(MSG): void>
                        A callback function to call for incoming messages.

        Returns: <instance of ErlMBox>
        Throws:  nothing

        This creates an mbox, which is equivalent to an erlang process.
        Messages to the mbox are delivered as callbacks to the callback
        function.
        """
        mboxPid = self._CreatePid()
        mbox = ErlMBox(self, mboxPid, msgCallback)
        self._pids[mboxPid] = mbox
        self._mboxes[mbox] = mboxPid
        return mbox

    def Ping(self, remoteNodeName, pingCallback):
        """Ping a remote node.

        REMOTE-NODE-NAME        = string | <instance of ErlAtom>
                                The node to ping
        PING-CALLBACK           = <function(RESULT): void>
                                A callback to call for deliverance of the
                                ping result. RESULT = "pong" | "pang".

        Returns: void
        Throws:  <<to-be-documented>>

        Try to ping a remote node. A connection to that node is established
        unless already connected. Whether or not the remote node is up or
        down is indicated as by the argument to the callback function:
          "pong": the remote node is alive and there is a connection to it
          "pang": the remote node is down.
        """
        if not "@" in remoteNodeName:
            raise ErlNodeBadPeerNameError(remoteNodeName)
        if self._ongoingPings.has_key(remoteNodeName):
            pingCallbacks = self._ongoingPings[remoteNodeName]
            self._ongoingPings[remoteNodeName] = pingCallbacks + [pingCallback]
        else:
            self._ongoingPings[remoteNodeName] = [pingCallback]
            [nodeName, hostName] = string.split(remoteNodeName, "@")
            e = erl_epmd.ErlEpmd(hostName)
            cb = erl_common.Callback(self._PingEpmdResponse, remoteNodeName)
            e.PortPlease2Req(nodeName, cb)

    def Publish(self):
        """Publish this node to the EPMD, the Erlang Portmapper Daemon.
        Returns: void
        Throws:  nothing.

        Publishing must be done for mboxes/processes on other nodes
        to be able to establish contact with mboxes on this node.
        The node is published automatically when it tries to contact
        another node, for example due to a message to send or an rpc call.
        """
        if not self._isServerPublished:
            self._epmd.Connect(self._EpmdConnectedOk, self._EpmdConnectFailed)

    def Unpublish(self):
        """Removes the publication of this node to the EPMD.
        Returns: void
        Throws:  nothing
        """
        if self._isServerPublished:
            self._epmd.Close()
            self._isServerPublished = 0

    def NodeUpSubscribe(self, cb):
        """Subscribe to nodeup-information

        CB        = <function("nodeup", NODE-NAME): void>

        Returns:  ID = <id>
        Throws:   nothing

        Subscribe to nodeup-information, that is, to connections to
        new nodes. The new connections may initiated by, for example,
        a messages begin sent from this node to another one (a new
        connection is automatically set up), or by another node
        connection to this.

        The ID returned can be used to unsubscribe the callback from further
        nodeup-informations.
        """
        id = self._cbId
        self._cbId = self._cbId + 1
        self._nodeUpCb.append((id, cb))
        return id

    def NodeUpUnsubscribe(self, id):
        """Unsubscribe to nodeup-information

        ID              = <id>

        Returns: void
        Throws:  nothing

        Unsubscribes to nodeup-information.
        """
        cbs = self._nodeUpCb
        for (index, (cbid, cb)) in map(None, range(len(cbs)), cbs):
            if id == cbid:
                del self._nodeUpCb[index]
                return

    def NodeDownSubscribe(self, cb):
        """Subscribe to nodedown-information for a certain node

        CB        = <function("nodedown", NODE-NAME): void>

        Returns:  ID = <id>
        Throws:   nothing

        Subscribe to nodedown-information, that is, to broken connections to
        other nodes.

        The ID returned can be used to unsubscribe the callback from further
        nodedown-informations.
        """
        id = self._cbId
        self._cbId = self._cbId + 1
        self._nodeDownCb.append((id, cb))
        return id

    def NodeDownUnsubscribe(self, id):
        """Unsubscribe to nodedown-information

        ID              = <id>

        Returns: void
        Throws:  nothing

        Unsubscribes to nodedown-information.
        """
        cbs = self._nodeDownCb
        for (index, (cbid, cb)) in map(None, range(len(cbs)), cbs):
            if id == cbid:
                del self._nodeDownCb[index]
                return

    def DumpConnections(self):
        """Dump connections. This method is intended for debugging purposes.
        Returns: void
        Throws:  nothing
        """
        print "Connections:"
        for k in self._connections.keys():
            print "  %s --> %s" % (`k`, `self._connections[k]`)
        print "--"


    ##
    ## Routines to be called only by mboxes
    ##

    def RegisterNameForMBox(self, mbox, name):
        """This routine is intended to be called from an ErlMBox instance.
        MBOX = <instance of ErlMBox>
        NAME = string
        Returns: void
        Throws:  <<IsRegistered>>
        """
        mboxPid = mbox.Self()
        if self._registeredNames.has_key(name):
            raise ErlNodeIsRegisteredError(name)
        if self._registeredPids.has_key(mboxPid):
            raise ErlNodeIsRegisteredError(name)
        self._registeredNames[name] = mboxPid
        self._registeredPids[mboxPid] = name

    def UnregisterNameForMBox(self, mbox):
        """This routine is intended to be called from an ErlMBox instance
        MBOX = <instance of ErlMBox>
        Returns: void
        Throws:  <<NotRegistered>>
        """
        mboxPid = mbox.Self()
        if not self._registeredPids.has_key(mboxPid):
            raise ErlNodeNotRegisteredError("pid not registered %s" % `mbox`)
        name = self._registeredPids[mboxPid]
        del self._registeredPids[mboxPid]
        del self._registeredNames[name]


    def WhereisMBox(self, name):
        """Lookup an mbox that is registered under a name.
        NAME = string
        Returns: <instance of ErlMBox>
        Throws:  nothing
        """
        mboxPid = self.WhereisPid(name)
        if mboxPid == None:
            return None
        if not self._pids.has_key(mboxPid):
            return None
        return self._pids[mboxPid]

    def WhereisPid(self, name):
        """Lookup an mbox that is registered under a name.
        NAME = string
        Returns: <instance of ErlPid>
        Throws:  nothing
        """
        if not self._registeredNames.has_key(name):
            return None
        return self._registeredNames[name]


    def SendMsgFromMBox(self, sourceMBox, dest, msg):
        """This routine is intended to be called from an ErlMBox instance
        SOURCE-MBOX = <instance of ErlMBox>
        DEST        = <instance of ErlPid> |
                      string |
                      <instance of ErlAtom> |
                      tuple(DEST-REGNAME, DEST-NODE)
                      DEST-REGNAME = DEST-NODE = string | <instance of ErlAtom>
        MSG         = <term>
        Returns: void
        THrows:  <<to-be-documented>>
        """
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

                cb = erl_common.Callback(self._SendMsgToRemoteNode,
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

    def _CreateRex(self):
        mboxPid = self._CreatePid()
        mbox = _ErlRexMBox(self, mboxPid)
        self._pids[mboxPid] = mbox
        self._mboxes[mbox] = mboxPid
        mbox.Start()

    def _CreatePid(self):
        """Returns: <instance of ErlPid>"""
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
        """This callback is called when the publish to EPMD has successfully
        completed."""
        self._isServerPublished = 1
        self._creation = creation

    def _EpmdConnectFailed(self, errorResult):
        raise ErlNodeConnectionFailedError(errorResult)

    def _NodeUp(self, connection, nodeName):
        """This callback is called from the in/out connection object
        when a new connection has been established."""
        erl_common.Debug(M, "NODEUP: nodeName=%s connection=%s" % \
                         (nodeName, connection))
        self._connections[nodeName] = connection
        for (id, cb) in self._nodeUpCb:
            cb("nodeup", nodeName)

    def _NodeDown(self, connection, nodeName):
        """This callback is called from the in/out connection object when a
        connection has been broken."""
        erl_common.Debug(M, "NODENOWN: nodeName=%s connection=%s" % \
                         (nodeName, connection))
        if self._connections.has_key(nodeName):
            del self._connections[nodeName]
            for (id, cb) in self._nodeDownCb:
                cb("nodedown", nodeName)

    def _PingEpmdResponse(self, result, portNum, nodeType, proto,
                          distVSNRange, nodeNameNoHost, extra,
                          remoteNodeName):
        """This callback is called when the lookup for a nodename at a host
        has returned, whether it was successful or not.
        If it was successful, then RESULT != 0 and the other parameters
        are valid. If it was not successful, then the other parameters have
        undefined values."""
        if result != 0:
            callbacks = self._ongoingPings[remoteNodeName]
            del self._ongoingPings[remoteNodeName]
            for cb in callbacks:
                cb("pang")
        else:
            [otherNode, otherHost] = string.split(remoteNodeName, "@")
            out = erl_node_conn.ErlNodeOutConnection(self._nodeName,
                                                     self._opts)
            connectedOkCb = erl_common.Callback(self._PingSucceeded,
                                                out, remoteNodeName)
            connectFailedCb = erl_common.Callback(self._PingFailed,
                                                  out, remoteNodeName)
            connectionBrokenCb = erl_common.Callback(self._NodeDown,
                                                     out, remoteNodeName)
            passThroughMsgCb = erl_common.Callback(self._PassThroughMsg,
                                                   out, remoteNodeName)
            out.InitiateConnection(otherHost, portNum,
                                   connectedOkCb,
                                   connectFailedCb,
                                   connectionBrokenCb,
                                   self._PassThroughMsg)

    def _PingSucceeded(self, connection, remoteNodeName):
        """This internal routine signals that the ping of another node
        was successul."""
        callbacks = self._ongoingPings[remoteNodeName]
        del self._ongoingPings[remoteNodeName]
        self._NodeUp(connection, remoteNodeName)
        for cb in callbacks:
            cb("pong")

    def _PingFailed(self, connection, remoteNodeName):
        """This internal routine signals that the ping of another node
        failed."""
        if self._ongoingPings.has_key(remoteNodeName):
            callbacks = self._ongoingPings[remoteNodeName]
            del self._ongoingPings[remoteNodeName]
            for cb in callbacks:
                cb("pang")


    def _PassThroughMsg(self, connection, remoteNodeName, ctrlMsg, msg=None):
        """This callback is called when a connection recevies a message of
        type passthrough. Currently all messages are of type passthrough."""
        erl_common.Debug(M, "ctrlMsg=%s" % `ctrlMsg`)

        ctrlMsgOp = ctrlMsg[0]
        if ctrlMsgOp == self.CTRLMSGOP_LINK:
            fromPid = ctrlMsg[1]
            toPid = self._InternPid(ctrlMsg[2])
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_SEND:
            cookie = ctrlMsg[1]
            toPid = self._InternPid(ctrlMsg[2])
            msg = msg
            erl_common.Debug(M, "SEND: msg=%s" % `msg`)
            if self._pids.has_key(toPid):
                mbox = self._pids[toPid]
                mbox.Msg(remoteNodeName, msg)
            else:
                erl_common.Debug(M, "Got SEND with no dest pid: %s" % toPid)
                erl_common.Debug(M, "Pids:\n%s" % `self._pids`)
        elif ctrlMsgOp == self.CTRLMSGOP_EXIT:
            fromPid = ctrlMsg[1]
            toPid = self._InternPid(ctrlMsg[2])
            reason = ctrlMsg[3]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_UNLINK:
            fromPid = ctrlMsg[1]
            toPid = self._InternPid(ctrlMsg[2])
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
                mbox.Msg(remoteNodeName, msg)
            else:
                erl_common.Debug(M,
                                 "Got REG_SEND with no dest mbox: \"%s\": %s" %
                                 (toName, msg))
        elif ctrlMsgOp == self.CTRLMSGOP_GROUP_LEADER:
            fromPid = ctrlMsg[1]
            toPid = self._InternPid(ctrlMsg[2])
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_EXIT2:
            fromPid = ctrlMsg[1]
            toPid = self._InternPid(ctrlMsg[2])
            reason = ctrlMsg[3]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_SEND_TT:
            cookie = ctrlMsg[1]
            toPid = self._InternPid(ctrlMsg[2])
            traceToken = ctrlMsg[3]
            msg = msg
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_EXIT_TT:
            fromPid = ctrlMsg[1]
            toPid = self._InternPid(ctrlMsg[2])
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
            toPid = self._InternPid(ctrlMsg[2])
            traceToken = ctrlMsg[3]
            reason = ctrlMsg[4]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_MONITOR_P:
            fromPid = ctrlMsg[1]
            toPid = self._InternPid(ctrlMsg[2])
            ref = ctrlMsg[3]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_DEMONITOR_P:
            fromPid = ctrlMsg[1]
            toPid = self._InternPid(ctrlMsg[2])
            ref = ctrlMsg[3]
            pass
        elif ctrlMsgOp == self.CTRLMSGOP_MONITOR_P_EXIT:
            fromPid = ctrlMsg[1]
            toPid = self._InternPid(ctrlMsg[2])
            ref = ctrlMsg[3]
            pass
        else:
            erl_common.Debug(M, "Unknown controlmsg: %s" % `ctrlMsg`)

    def _SendMsgToRemoteNode(self, pingResult, srcPid, destNode, destPid, msg):
        """This internal routine performs the actual sending."""
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

    def _InternPid(self, newPid):
        """This is like intern() for strings, but for pids.
        The purpose is so that we'll be able to lookup pids
        in self._pids and self._registredPids.
        """
        for existingPid in self._pids.keys():
            if existingPid.equals(newPid):
                return existingPid
        return newPid
