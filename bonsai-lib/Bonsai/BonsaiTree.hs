{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Bonsai.BonsaiTree where

import Bonsai.SyntaxSpec
import Control.DeepSeq
import qualified Data.ByteString as B
import Data.Hashable
import Data.Maybe
import GHC.TypeNats
import Generics.Deriving
import Grisette

data ConcBonsaiTree leaf
  = ConcBonsaiLeaf leaf
  | ConcBonsaiNode (ConcBonsaiTree leaf) (ConcBonsaiTree leaf)
  deriving (Show, Generic)

data BonsaiTree leaf
  = BonsaiLeaf leaf
  | BonsaiNode (UnionM (BonsaiTree leaf)) (UnionM (BonsaiTree leaf))
  deriving (Generic, Show, Eq, Hashable, NFData)

deriving via (Default (BonsaiTree leaf)) instance (SEq leaf) => SEq (BonsaiTree leaf)

deriving via (Default (BonsaiTree leaf)) instance (Mergeable leaf) => Mergeable (BonsaiTree leaf)

deriving via (Default (BonsaiTree leaf)) instance (Mergeable leaf, EvaluateSym leaf) => EvaluateSym (BonsaiTree leaf)

deriving via (Default (ConcBonsaiTree cleaf)) instance (ToCon leaf cleaf) => ToCon (BonsaiTree leaf) (ConcBonsaiTree cleaf)

deriving via
  (Default (BonsaiTree leaf))
  instance
    (Mergeable leaf, ToSym cleaf leaf) =>
    ToSym (ConcBonsaiTree cleaf) (BonsaiTree leaf)

$(makeUnionWrapper "u" ''BonsaiTree)

showConcTree :: OptimSyntaxSpec n -> ConcBonsaiTree (WordN n) -> Maybe B.ByteString
showConcTree stx (ConcBonsaiLeaf sym) = bvToTerminal stx sym
showConcTree stx (ConcBonsaiNode l r) = do
  ls <- showConcTree stx l
  rs <- showConcTree stx r
  return $ B.append "[ " (B.append ls (B.append " " (B.append rs " ]")))

instance (KnownNat n, 1 <= n) => GenSym Int (BonsaiTree (SymWordN n)) where
  fresh depth =
    if depth <= 1
      then uBonsaiLeaf <$> simpleFresh ()
      else do
        l <- fresh $ depth - 1
        r <- fresh $ depth - 1
        sym <- simpleFresh ()
        chooseFresh [BonsaiLeaf sym, BonsaiNode l r]

unsafeLeaf :: (KnownNat n, 1 <= n) => OptimSyntaxSpec n -> B.ByteString -> BonsaiTree (SymWordN n)
unsafeLeaf stx nm = BonsaiLeaf $ con $ fromJust $ terminalToBV stx nm

data BonsaiError
  = BonsaiTypeError
  | BonsaiExecError
  | BonsaiRecursionError
  deriving (Show, Eq, Generic, NFData)
  deriving (Mergeable, ToCon BonsaiError, EvaluateSym) via (Default BonsaiError)

instance TransformError BonsaiError BonsaiError where
  transformError = id

verifyTyperTranslation :: Either BonsaiError a -> SymBool
verifyTyperTranslation (Left BonsaiExecError) = con True
verifyTyperTranslation _ = con False
