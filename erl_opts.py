class ErlNodeOpts:
    def __init__(self,
                 netTickTime=60,
                 shortNodeNames=1,
                 cookie="",
                 distrVersion=5,
                 distrFlags=4):
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
