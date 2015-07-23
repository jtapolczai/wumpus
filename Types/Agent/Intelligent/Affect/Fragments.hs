module Types.Agent.Intelligent.Affect.Fragments where

-- |Type of a PSBC-fragment.
data PSBCFragmentType = Weak | Strong
   deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- |Type of an SJS-fragment
data SJSFragmentType = Friendly | Hostile
   deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- |Settings for a sympathy template.
data SocialSettings = SocialSettings {
   _socialSettingsAttackedVal :: Rational,
   _socialSettingsHostileGestureVal :: Rational,
   _socialSettingsReceivedGoldVal :: Rational, 
   _socialSettingsReceivedMeatVal :: Rational, 
   _socialSettingsReceivedFruitVal :: Rational,
   _socialSettingsFriendlyGestureVal :: Rational,
   _socialSettingsGrudgeMinVal :: Rational,   
   _socialSettingsGrudgeMaxVal :: Rational,
   _socialSettingsGrudgeImproveVal :: Rational
} 
