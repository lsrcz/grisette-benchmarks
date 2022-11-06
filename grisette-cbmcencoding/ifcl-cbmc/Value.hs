{-# LANGUAGE TemplateHaskell #-}

module Value where

import Control.DeepSeq
import Generics.Deriving
import Grisette

data PCValue = PCValue {int :: SymInteger, label :: SymBool}
  deriving (Show, Eq, Generic, NFData)
  deriving (GSimpleMergeable SymBool, GSEq SymBool, GMergeable SymBool, GEvaluateSym Model) via (Default PCValue)

$(makeUnionMWrapper "u" ''PCValue)

instance GGenSym SymBool () PCValue

instance GenSymSimple () PCValue where
  genSymSimpleFresh = derivedNoSpecGenSymSimpleFresh

instance GGenSym SymBool PCValue PCValue

instance GenSymSimple PCValue PCValue where
  genSymSimpleFresh = derivedSameShapeGenSymSimpleFresh

zeroLow :: PCValue
zeroLow = PCValue 0 $ conc False

data MemValue
  = MPCValue PCValue
  | ReturnAddr PCValue PCValue
  deriving (Show, Eq, Generic, NFData)
  deriving (GMergeable SymBool, GEvaluateSym Model, GSEq SymBool) via (Default MemValue)
