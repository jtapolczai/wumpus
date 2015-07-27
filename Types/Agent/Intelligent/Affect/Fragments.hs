module Types.Agent.Intelligent.Affect.Fragments where

-- |Type of a PSBC-fragment.
data PSBCFragmentType = Weak | Strong
   deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- |Type of an SJS-fragment
data SJSFragmentType = Friendly | Hostile
   deriving (Show, Eq, Ord, Read, Enum, Bounded)

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
   _angerSettingsAgentIntensityVal :: Rational,
   _angerSettingsStench1Val :: Rational,
   _angerSettingsStench2Val :: Rational
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
   _fearSettingsPitIntensityVal :: Rational,
   _fearSettingsStench1Val :: Rational,
   _fearSettingsStench2Val :: Rational,
   _fearSettingsBreeze1Val :: Rational,
   _fearSettingsBreeze2Val :: Rational
}

-- |Settings for an enthusiasm template.
data EnthusiasmSettings = EnthusiasmSettings {
   _enthusiasmSettingsQuarterHealthLossVal :: Rational,
   _enthusiasmSettingsHalfHealthLossVal :: Rational,
   _enthusiasmSettingsHighTempVal :: Rational,
   _enthusiasmSettingsLowTempVal :: Rational,
   _enthusiasmSettingsLowStaminaVal :: Rational,
   _enthusiasmSettingsGaveGoldVal :: Rational,
   _enthusiasmSettingsGaveMeatVal :: Rational,
   _enthusiasmSettingsGaveFruitVal :: Rational,
   _enthusiasmSettingsPlantHarvestedVal :: Rational,
   _enthusiasmSettingsHunger1Val :: Rational,
   _enthusiasmSettingsHunger2Val :: Rational,
   _enthusiasmSettingsHunger3Val :: Rational,
   _enthusiasmSettingsHunger4Val :: Rational,
   _enthusiasmSettingsHunger5Val :: Rational,
   _enthusiasmSettingsStrongFriendRadiusVal :: Rational,
   _enthusiasmSettingsStrongFriendIntensityVal :: Rational,
   _enthusiasmSettingsNormalFriendRadiusVal :: Rational,
   _enthusiasmSettingsNormalFriendIntensityVal :: Rational,
   _enthusiasmSettingsWeakFriendRadiusVal :: Rational,
   _enthusiasmSettingsWeakFriendIntensityVal :: Rational,
   _enthusiasmSettingsPlant1RadiusVal :: Rational,
   _enthusiasmSettingsPlant1IntensityVal :: Rational,
   _enthusiasmSettingsPlant2RadiusVal :: Rational,
   _enthusiasmSettingsPlant2IntensityVal :: Rational,
   _enthusiasmSettingsPlant3RadiusVal :: Rational,
   _enthusiasmSettingsPlant3IntensityVal :: Rational,
   _enthusiasmSettingsPlant4RadiusVal :: Rational,
   _enthusiasmSettingsPlant4IntensityVal :: Rational,
   _enthusiasmSettingsPlant5RadiusVal :: Rational,
   _enthusiasmSettingsPlant5IntensityVal :: Rational,
   _enthusiasmSettingsGoldRadiusVal :: Rational,
   _enthusiasmSettingsGoldIntensityVal :: Rational,
   _enthusiasmSettingsMeatRadiusVal :: Rational,
   _enthusiasmSettingsMeatIntensityVal :: Rational,
   _enthusiasmSettingsFruitRadiusVal :: Rational,
   _enthusiasmSettingsFruitIntensityVal :: Rational
}

-- |Settings for a contentment template.
data ContentmentSettings = ContentmentSettings {
   _contentmentSettingsQuarterHealthLossVal :: Rational,
   _contentmentSettingsHalfHealthLossVal :: Rational,
   _contentmentSettingsBadHealthVal :: Rational,
   _contentmentSettingsVeryBadHealthVal :: Rational,
   _contentmentSettingsCriticalHealthVal :: Rational,
   _contentmentSettingsStaminaLossVal :: Rational,
   _contentmentSettingsHighHealthVal :: Rational,
   _contentmentSettingsVeryHighHealthVal :: Rational,
   _contentmentSettingsExcellentHealthVal :: Rational,
   _contentmentSettingsHaveGoldVal :: Rational,
   _contentmentSettingsHaveFruitVal :: Rational,
   _contentmentSettingsHaveMuchFruitVal :: Rational,
   _contentmentSettingsHaveMeatVal :: Rational,
   _contentmentSettingsHaveMuchMeatVal :: Rational,
   _contentmentSettingsPlantRadiusVal :: Rational,
   _contentmentSettingsPlantIntensityVal :: Rational
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
