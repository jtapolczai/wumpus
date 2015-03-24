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
