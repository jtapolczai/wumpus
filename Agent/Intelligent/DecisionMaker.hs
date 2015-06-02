module Agent.Intelligent.DecisionMaker where

import Control.Lens
import qualified Data.Map as M

import Types

-- |A function which selects an action, given a certain emotion.
type ActionSelector =
   CellInd -- ^The agent's current position.
   -> CellInd -- ^The target cell's position
   -> CellData -- ^The target cell's data.
   -- |The social emotions felt towards the entity on the target
   --  cell, if one exists. If Nothing is passed, a neutral stance
   --  is assumed.
   -> Maybe (M.Map SocialEmotionName HormoneLevel)
   -> Action

-- |Makes a decision based on the affective evaluation of the world.
--  If
makeDecision :: AgentComponent IO
makeDecision = undefined

-- |Selects an anger-actions
angerAction :: ActionSelector
angerAction = undefined



enthusiasmActions :: [Action]
enthusiasmActions = undefined

fearActions :: [Action]
fearActions = undefined

-- |Actions associated with contentment.
contentmentActions :: [Action]
contentmentActions = [NoOp]
