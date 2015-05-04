module World.Constants where

cHEAL_FOOD :: Fractional a => a
cHEAL_FOOD = 0.5

cHUNGER_RATE :: Fractional a => a
cHUNGER_RATE = 0.01

cFATIGUE_RESTORE :: Fractional a => a
cFATIGUE_RESTORE = 0.1

cEDGE_FATIGUE :: Fractional a => a
cEDGE_FATIGUE = 0.05

cDEFAULT_AGENT_HEALTH :: Fractional a => a
cDEFAULT_AGENT_HEALTH = 1

cDEFAULT_WUMPUS_HEALTH :: Fractional a => a
cDEFAULT_WUMPUS_HEALTH = 1

cMAX_AGENT_HEALTH :: Fractional a => a
cMAX_AGENT_HEALTH = 2

cMAX_AGENT_FATIGUE :: Fractional a => a
cMAX_AGENT_FATIGUE = 1

cPLANT_REGROWTH :: Fractional a => a
cPLANT_REGROWTH = 0.1

cDAY_LENGTH :: Integral a => a
cDAY_LENGTH = 50

cSTART_TIME :: Integral a => a
cSTART_TIME = 0

cPLANT_HARVEST :: Fractional a => a
cPLANT_HARVEST = 1

cPLANT_MAX :: Fractional a => a
cPLANT_MAX = 1

cSTENCH_RADIUS :: Integral a => a
cSTENCH_RADIUS = 3

cBREEZE_RADIUS :: Integral a => a
cBREEZE_RADIUS = 3

cAGENT_FILTER_ROUNDS :: Integral a => a
cAGENT_FILTER_ROUNDS = 100

cAGENT_FILTER_MAX_SIGNIFICANCE :: Integral a => a
cAGENT_FILTER_MAX_SIGNIFICANCE = 100
