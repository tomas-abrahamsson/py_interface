### eventhandler.py -- An event handler
###
### $Id$
###
### Copyright (C) 2000 Tomas Abrahamsson
### 
### Author: Tomas Abrahamsson <tab@lysator.liu.se>
### 
### This file is part of the Albertina caller id displayer
###
### The Albertina program is free software; you can redistribute it
### and/or modify it under the terms of the GNU Library General Public License
### as published by the Free Software Foundation; either version 2 of the
### License, or (at your option) any later version.
###
### The Albertina program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library
### General Public License for more details.
### 
### You should have received a copy of the GNU Library General Public License
### along with the Albertina program; see the file COPYING.LIB.  If not,
### write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
### Boston, MA 02111-1307, USA. */
### 


import os
import sys
import string
import select
import time
from Tkinter import tkinter

import common

_evhandler = None

### ---------------------------------------------------
### API
###

def GetEventHandler():
    global _evhandler

    if _evhandler == None:
        _evhandler = _EventHandler()
    return _evhandler

###
### End of API
### ---------------------------------------------------

_nextTimerId = 0
def GetNewTimerId():
    global _nextTimerId

    idToReturn = _nextTimerId
    _nextTimerId = _nextTimerId + 1
    return idToReturn

class _TimerEvent:
    def __init__(self, timeLeft, cb):
        self.addedWhen = time.time()
        self.id = GetNewTimerId()
        self.timeOutTime = self.addedWhen + timeLeft
        self.cb = cb

    def __cmp__(self, other):
        if self.timeOutTime == other.timeOutTime:
            return 0
        elif self.timeOutTime < other.timeOutTime:
            return -1
        else:
            return 1

class _EventHandler:
    STATE_STANDALONE = 1
    STATE_TK = 2

    def __init__(self):
        # mappings of connection --> callback
        self.readEvents = {}
        self.writeEvents = {}
        self.exceptEvents = {}

        # A sorted list of timers: the timer with least time to run is first
        self.timerEvents = []

        # State:        STATE_STANDALONE     -- Tk is not involved
        #               STATE_TK             -- use the eventhandler in Tkinter
        self.state = self.STATE_STANDALONE
        self.tkTopLevel = None

    def _SetStateTk(self, topLevel):
        """Not supported for now.
        State that we are using eventhandler in Tkinter.
        When using the Tkinter eventhandler, you cannot:
        * add a read-handler, then add a write-handler for the same connection,
          and then delete one of the read or write handler: both gets deleted.
        * delete timer-events."""
        self.state = self.STATE_TK
        self.tkTopLevel = topLevel

    def SetStateStandAlone(self):
        """State that we are implementing our own eventhandler."""
        self.state = self.STATE_STANDALONE


    def PushReadEvent(self, connection, cbfunction, *optArgs, **namedArgs):
        cb = common.VCallback(cbfunction, optArgs, namedArgs)
        if self.readEvents.has_key(connection):
            handlers = self.readEvents[connection]
        else:
            handlers = []
        newHandlers = self._PushHandler(cb, handlers)
        self.readEvents[connection] = newHandlers

    def PopReadEvent(self, connection):
        handlers = self.readEvents[connection]
        newHandlers = self._PopHandler(handlers)
        if len(newHandlers) == 0:
            del self.readEvents[connection]
        else:
            self.readEvents[connection] = newHandlers


    def PushWriteEvent(self, connection, cbfunction, *optArgs, **namedArgs):
        cb = common.VCallback(cbfunction, optArgs, namedArgs)
        if self.writeEvents.has_key(connection):
            handlers = self.writeEvents[connection]
        else:
            handlers = []
        newHandlers = self._PushHandler(cb, handlers)
        self.writeEvents[connection] = newHandlers

    def PopWriteEvent(self, connection):
        handlers = self.writeEvents[connection]
        newHandlers = self._PopHandler(handlers)
        if len(newHandlers) == 0:
            del self.writeEvents[connection]
        else:
            self.writeEvents[connection] = newHandlers


    def PushExceptEvent(self, connection, cbfunction, *optArgs, **namedArgs):
        cb = common.VCallback(cbfunction, optArgs, namedArgs)
        if self.exceptEvents.has_key(connection):
            handlers = self.exceptEvents[connection]
        else:
            handlers = []
        newHandlers = self._PushHandler(cb, handlers)
        self.exceptEvents[connection] = newHandlers

    def PopExceptEvent(self, connection):
        handlers = self.exceptEvents[connection]
        newHandlers = self._PopHandler(handlers)
        if len(newHandlers) == 0:
            del self.exceptEvents[connection]
        else:
            self.exceptEvents[connection] = newHandlers


    def AddTimerEvent(self, timeLeft, cbfunction, *optArgs, **namedArgs):
        cb = common.VCallback(cbfunction, optArgs, namedArgs)
        newTimerEvent = _TimerEvent(timeLeft, cb)
        self.timerEvents.append(newTimerEvent)
        self.timerEvents.sort()
        return newTimerEvent.id

    def DelTimerEvent(self, id):
        indexForId = None
        it = map(None, range(len(self.timerEvents)), self.timerEvents)
        for i, ev in it:
            if ev.id == id:
                indexForId = i
                break
        if indexForId != None:
            del self.timerEvents[indexForId]

    def Loop(self):
        self.continueLooping = 1
        while self.continueLooping:
            rList = self.readEvents.keys()
            wList = self.writeEvents.keys()
            eList = self.exceptEvents.keys()

            timeout = None
            if len(self.timerEvents) > 0:
                firstTimerEv = self.timerEvents[0]
                now = time.time()
                timeout = firstTimerEv.timeOutTime - now

            if timeout == None:
                # No timer to wait for
                try:
                    reads, writes, excepts = select.select(rList, wList, eList)
                except select.error, info:
                    (errno, errText) = info
                    if errno == 4:
                        # 'Interrupted system call'
                        # ignore this one.
                        # loop again in the while loop
                        continue
                    else:
                        # Other error: serious. Raise again.
                        raise

            elif timeout < 0:
                # Signal timeout
                (reads, writes, excepts) = ([], [], [])
            else:
                # Select and wait for timer
                reads, writes, excepts = select.select(rList, wList, eList,
                                                       timeout)

            # Check for handles that are clear for reading
            for readable in reads:
                cb = self.readEvents[readable][0]
                cb()

            # Check for handles that are clear for writing
            for writable in writes:
                cb = self.writeEvents[writable][0]
                cb()

            # Check for handles that are in exception situation
            for exceptable in excepts:
                cb = self.exceptEvents[exceptable][0]
                cb()

            # Check for timers that have timed out
            while 1:
                expiredTimers = filter(lambda ev, now=time.time():
                                       ev.timeOutTime <= now,
                                       self.timerEvents)
                if len(expiredTimers) == 0:
                    break               # skip the while loop
                
                for expiredTimer in expiredTimers:
                    expiredTimer.cb()
                    self.DelTimerEvent(expiredTimer.id)


    def StopLooping(self):
        """Clients can use this in callbacks to stop the event handler loop"""
        self.continueLooping = 0

    def _PushHandler(self, cb, handlers):
        return [cb] + handlers

    def _PopHandler(self, handlers):
        return handlers[1:]
