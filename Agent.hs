module Agent where

import qualified Data.Map as M

import Types

data HormoneStorage = HS {
   hAnger :: Rational,
   hContentment :: Rational,
   hEnthusiasm :: Rational,
   hFear :: Rational
   }

data SocialStorage = SJS {
   sSympathy :: Rational,
   sTrust :: Rational,
   sRespect :: Rational
}

type Memory s = (M.Map CellInd (CellData s), M.Map EdgeInd EdgeData)

data AgentState = AS {
   hs :: HormoneStorage,
   sjs :: M.Map EntityName SocialStorage,
   memory :: Memory AgentState
   }

-- |Represents a perception that an agent may receive from the world.
data Perception = EdgeP EdgeInd EdgeData
                  | CellP CellInd VisualCellData
                  | AgentP CellInd VisualAgent
                  | WumpusP CellInd Wumpus
                  | TemperatureP Temperature
                  | TimeP Int
