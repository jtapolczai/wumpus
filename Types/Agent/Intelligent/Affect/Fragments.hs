module Types.Agent.Intelligent.Affect.Fragments where

-- |Type of a PSBC-fragment.
data PSBCFragmentType = Weak | Strong
   deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- |Type of an SJS-fragment
data SJSFragmentType = Friendly | Hostile
   deriving (Show, Eq, Ord, Read, Enum, Bounded)

data EnthusiasmSettings = ES
data ContentmentSettings = CS

-- |Settings for an anger template.
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

-- |Settings for a fear template.
data FearSettings = FearSettings {
   _fearSettingsQuarterHealthLossVal :: Rational,
   _fearSettingsHalfHealthLossVal :: Rational,
   _fearSettingsThreeQuarterHealthLossVal :: Rational,
   _fearSettingsDiedVal :: Rational,
   _fearSettingsHighTempVal :: Rational,
   _fearSettingsLowTempVal :: Rational,
   _fearSettingsLowHealthVal :: Rational, 
   _fearSettingsBadHealthVal :: Rational, 
   _fearSettingsVeryBadHealthVal :: Rational,
   _fearSettingsCriticalHealthVal :: Rational,
   _fearSettingsGoodHealthVal :: Rational,
   _fearSettingsHealthGainVal :: Rational,
   _fearSettingsLowStaminaVal :: Rational,
   _fearSettingsWumpusRadiusVal :: Rational,
   _fearSettingsWumpusIntensityVal :: Rational,  
   _fearSettingsWeakEnemyRadiusVal :: Rational,
   _fearSettingsWeakEnemyIntensityVal :: Rational,
   _fearSettingsNormalEnemyRadiusVal :: Rational,
   _fearSettingsNormalEnemyIntensityVal :: Rational,
   _fearSettingsStrongEnemyRadiusVal :: Rational,
   _fearSettingsStrongEnemyIntensityVal :: Rational,
   _fearSettingsVeryStrongEnemyRadiusVal :: Rational,
   _fearSettingsVeryStrongEnemyIntensityVal :: Rational,
   _fearSettingsFriendRadiusVal :: Rational,
   _fearSettingsFriendIntensityVal :: Rational,
   _fearSettingsPitRadiusVal :: Rational,
   _fearSettingsPitIntensityVal :: Rational
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
