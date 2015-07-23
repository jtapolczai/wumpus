module Types.Agent.Intelligent.Affect.Fragments where

-- |Type of a PSBC-fragment.
data PSBCFragmentType = Weak | Strong
   deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- |Type of an SJS-fragment
data SJSFragmentType = Friendly | Hostile
   deriving (Show, Eq, Ord, Read, Enum, Bounded)

data FearSettings = FS
data EnthusiasmSettings = ES
data ContentmentSettings = CS

-- |Settings for a sympathy template.
data AngerSettings = AngerSettings {
   _angerSettingsWumpusDiedVal :: Rational,
   _angerSettingsAgentDiedVal :: Rational,
   _angerSettingsHighTempVal :: Rational,
   _angerSettingsGoodHealthVal :: Rational, 
   _angerSettingsHighHealthVal :: Rational, 
   _angerSettingsHighStaminaVal :: Rational,
   _angerSettingsWumpusRadiusVal :: Rational,
   _angerSettingsWumpusIntensityVal :: Rational,   
   _angerSettingsAgentRadiusVal :: Rational,
   _angerSettingsAgentIntensityVal :: Rational
} 

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
