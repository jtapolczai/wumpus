module World.Statistics where

import Control.Lens

import Types

agentDied :: AgentIndex -> WorldStats -> WorldStats
agentDied i = numAlive . ix i -~ 1

wumpusDied :: WorldStats -> WorldStats
wumpusDied = numWumpuses -~ 1

plantHarvested :: WorldStats -> WorldStats
plantHarvested = numHarvests +~ 1

itemGiven :: WorldStats -> WorldStats
itemGiven = numItemsGiven +~ 1

gestureSent :: WorldStats -> WorldStats
gestureSent = numGesturesSent +~ 1

attackPerformed :: WorldStats -> WorldStats
attackPerformed = numAttacksPerformed +~ 1
