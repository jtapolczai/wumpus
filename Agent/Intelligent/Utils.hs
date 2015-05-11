{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent.Utils where

import Control.Lens

import Types

-- |Filters the message space of an agent by counter (messages have to have
--  a counter value >= the given one).
aboveCounter :: AgentState -> Counter -> [AgentMessage]
aboveCounter as c = map snd $ filter ((c<=).fst) $ as ^. messageSpace

