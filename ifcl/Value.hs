{-# LANGUAGE TemplateHaskell #-}

module Value where

import Control.DeepSeq
import Generics.Deriving
import Grisette

data PCValue = PCValue {int :: SymInteger, label :: SymBool}
  deriving (Show, Eq, Generic, NFData)
  deriving (SimpleMergeable, SEq, Mergeable, EvaluateSym) via (Default PCValue)

$(makeUnionWrapper "u" ''PCValue)

instance GenSym () PCValue

instance GenSymSimple () PCValue where
  simpleFresh = derivedNoSpecSimpleFresh

instance GenSym PCValue PCValue

instance GenSymSimple PCValue PCValue where
  simpleFresh = derivedSameShapeSimpleFresh

zeroLow :: PCValue
zeroLow = PCValue 0 $ con False

data MemValue
  = MPCValue PCValue
  | ReturnAddr PCValue PCValue
  deriving (Show, Eq, Generic, NFData)
  deriving (Mergeable, EvaluateSym, SEq) via (Default MemValue)
