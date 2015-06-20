-- |Contains pre-made affective fragments from which one can piece together an
--  agent's personality.
module Agent.Intelligent.Affect.Fragments where

import Types

psbcFragmentType :: String -> PSBCFragmentType
psbcFragmentType "weak" = Weak
psbcFragmentType "strong" = Strong
psbcFragmentType x = error $ "psbcFragmentType called with unspported type " ++ x

sjsFragmentType :: String -> SJSFragmentType
sjsFragmentType "hostile" = Hostile
sjsFragmentType "friendly" = Friendly
sjsFragmentType x = error $ "sjsFragmentType called with unspported type " ++ x

-- |Returns the personality fragment belonging to an emotion and
--  a type (currently supported: weak/strong).
personalityFragment :: EmotionName -> String -> Filter AgentMessage
personalityFragment Anger "weak" = weakAnger
personalityFragment Anger "strong" = strongAnger

personalityFragment Fear "weak" = weakFear
personalityFragment Fear "strong" = strongFear

personalityFragment Enthusiasm "weak" = weakEnthusiasm
personalityFragment Enthusiasm "strong" = strongEnthusiasm

personalityFragment Contentment "weak" = weakContentment
personalityFragment Contentment "strong" = strongContentment

personalityFragment _ x = error $ "personalityFragment called with unsupported type "++x

sympathyFragment Sympathy "hostile" = hostileSocial
sympathyFragment Sympathy "friendly" = friendlySocial
sympathyFragment _ x = error $ "sympathyFragment called with unsupported type "++x

weakAnger :: Filter AgentMessage
weakAnger = todo "affectFragments"
strongAnger :: Filter AgentMessage
strongAnger = todo "affectFragments"

weakFear :: Filter AgentMessage
weakFear = todo "affectFragments"
strongFear :: Filter AgentMessage
strongFear = todo "affectFragments"

weakEnthusiasm :: Filter AgentMessage
weakEnthusiasm = todo "affectFragments"
strongEnthusiasm :: Filter AgentMessage
strongEnthusiasm = todo "affectFragments"

weakContentment :: Filter AgentMessage
weakContentment = todo "affectFragments"
strongContentment :: Filter AgentMessage
strongContentment = todo "affectFragments"

hostileSocial :: Filter AgentMessage
hostileSocial = todo "affectFragments"

friendlySocial :: Filter AgentMessage
friendlySocial = todo "affectFragments"

