{-# LANGUAGE UnicodeSyntax #-}

module Agent where

import qualified Data.Map as M

import Types

data HormoneStorage = HS {
   hAnger :: ℝ,
   hContentment :: ℝ,
   hEnthusiasm :: ℝ,
   hFear :: ℝ
   }

data SocialStorage = SJS {
   sSympathy :: ℝ,
   sTrust :: ℝ,
   sRespect :: ℝ
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
                  | TimeP ℕ
