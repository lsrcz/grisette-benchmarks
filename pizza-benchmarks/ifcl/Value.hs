{-# LANGUAGE TemplateHaskell #-}

module Value where

import Control.DeepSeq
import Generics.Deriving
import Pizza

data PCValue = PCValue {int :: SymInteger, label :: SymBool}
  deriving (Show, Eq, Generic, NFData)
  deriving (SimpleMergeable SymBool, SEq SymBool, Mergeable SymBool, EvaluateSym Model) via (Default PCValue)

$(makeUnionMWrapper "u" ''PCValue)

instance GenSym SymBool () PCValue

instance GenSymSimple () PCValue where
  genSymSimpleFresh = derivedNoSpecGenSymSimpleFresh

instance GenSym SymBool PCValue PCValue

instance GenSymSimple PCValue PCValue where
  genSymSimpleFresh = derivedSameShapeGenSymSimpleFresh

zeroLow :: PCValue
zeroLow = PCValue 0 $ conc False

data MemValue
  = MPCValue PCValue
  | ReturnAddr PCValue PCValue
  deriving (Show, Eq, Generic, NFData)
  deriving (Mergeable SymBool, EvaluateSym Model, SEq SymBool) via (Default MemValue)
