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

### erl_opts.py -- holder class for options for the node

### Added a patch from Luke Gorrie, to support Erlang/OTP R10B, see
### http://article.gmane.org/gmane.comp.lang.erlang.general/9751


DISTR_FLAG_PUBLISHED          = 0x01
DISTR_FLAG_ATOMCACHE          = 0x02
DISTR_FLAG_EXTENDEDREFERENCES = 0x04
DISTR_FLAG_DISTMONITOR        = 0x08
DISTR_FLAG_FUNTAGS            = 0x10
DISTR_FLAG_DISTMONITORNAME    = 0x20
DISTR_FLAG_HIDDENATOMCACHE    = 0x40
DISTR_FLAG_NEWFUNTAGS         = 0x80
DISTR_FLAG_EXTENDEDPIDSPORTS  = 0x100
DISTR_FLAG_EXPORTPTRTAG       = 0x200
DISTR_FLAG_BITBINARIES        = 0x400
DISTR_FLAG_NEWFLOATS          = 0x800
DISTR_FLAG_UNICODEIO          = 0x1000
DISTR_FLAG_DISTHDRATOMCACHE   = 0x2000
DISTR_FLAG_SMALLATOMTAGS      = 0x4000
DISTR_FLAG_INTERNALTAGS       = 0x8000
DISTR_FLAG_UTF8ATOMS          = 0x10000
DISTR_FLAG_MAPTAG             = 0x20000

class ErlNodeOpts:
    def __init__(self,
                 netTickTime=60,
                 shortNodeNames=1,
                 cookie="",
                 distrVersion=5,
                 distrFlags=(DISTR_FLAG_EXTENDEDREFERENCES|
                             DISTR_FLAG_EXTENDEDPIDSPORTS|
                             DISTR_FLAG_FUNTAGS|
                             DISTR_FLAG_NEWFUNTAGS|
                             DISTR_FLAG_EXPORTPTRTAG|
                             DISTR_FLAG_BITBINARIES|
                             DISTR_FLAG_NEWFLOATS|
                             DISTR_FLAG_MAPTAG)
                 ):
        self._netTickTime = netTickTime
        self._shortNodeNames = shortNodeNames
        self._cookie = cookie
        self._distrVersion = distrVersion
        self._distrFlags = distrFlags

    def GetNetTickTime(self):
        return self._netTickTime
    def SetNetTickTime(self, netTickTime):
        self._netTickTime = netTickTime

    def GetShortNodeNames(self):
        return self._shortNodeNames
    def SetShortNodeNames(self, shortNodeNames):
        self._shortNodeNames = shortNodeNames

    def GetCookie(self):
        return self._cookie
    def SetCookie(self, cookie):
        self._cookie = cookie

    def GetDistrVersion(self):
        return self._distrVersion
    def SetDistrVersion(self, distrVersion):
        self._distrVersion = distrVersion

    def GetDistrFlags(self):
        return self._distrFlags
    def SetDistrFlags(self, distrFlags):
        self._distrFlags = distrFlags
