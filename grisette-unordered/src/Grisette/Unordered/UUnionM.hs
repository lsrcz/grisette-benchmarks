{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Grisette.Unordered.UUnionM where

import Control.DeepSeq
import Control.Monad.Identity
import Data.Bifunctor
import Data.Functor.Classes
import Data.Hashable
import Data.IORef
import Data.String
import GHC.IO hiding (evaluate)
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Function
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.PrimWrapper
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solver
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Grisette.IR.SymPrim
import Grisette.Unordered.UUnion
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Compat

data UUnionMBase bool a where
  UUAny ::
    IORef (Either (UUnionBase bool a) (UUnionMBase bool a)) ->
    UUnionBase bool a ->
    UUnionMBase bool a
  UUMrg ::
    GMergingStrategy bool a ->
    UUnionBase bool a ->
    UUnionMBase bool a

instance (NFData bool, NFData a) => NFData (UUnionMBase bool a) where
  rnf = rnf1

instance (NFData bool) => NFData1 (UUnionMBase bool) where
  liftRnf = liftRnf2 rnf

instance NFData2 UUnionMBase where
  liftRnf2 _bool _a (UUAny i m) = rnf i `seq` liftRnf2 _bool _a m
  liftRnf2 _bool _a (UUMrg _ m) = liftRnf2 _bool _a m

instance (Lift bool, Lift a) => Lift (UUnionMBase bool a) where
  liftTyped (UUAny _ v) = [||freshUUAny v||]
  liftTyped (UUMrg _ v) = [||freshUUAny v||]
  lift = unTypeSplice . liftTyped

freshUUAny :: UUnionBase bool a -> UUnionMBase bool a
freshUUAny v = UUAny (unsafeDupablePerformIO $ newIORef $ Left v) v
{-# NOINLINE freshUUAny #-}

instance (Show a, Show bool) => (Show (UUnionMBase bool a)) where
  showsPrec = showsPrec1

instance (Show b) => Show1 (UUnionMBase b) where
  liftShowsPrec sp sl i (UUAny _ a) = showsUnaryWith (liftShowsPrec sp sl) "UUAny" i a
  liftShowsPrec sp sl i (UUMrg _ a) = showsUnaryWith (liftShowsPrec sp sl) "UUMrg" i a

underlyingUnion :: UUnionMBase bool a -> UUnionBase bool a
underlyingUnion (UUAny _ a) = a
underlyingUnion (UUMrg _ a) = a

isMerged :: UUnionMBase bool a -> Bool
isMerged UUAny {} = False
isMerged UUMrg {} = True

instance SymBoolOp bool => GUnionPrjOp bool (UUnionMBase bool) where
  singleView = singleView . underlyingUnion
  ifView = undefined
  leftMost = undefined

instance (SymBoolOp bool) => Functor (UUnionMBase bool) where
  fmap f fa = fa >>= return . f

instance (SymBoolOp bool) => Applicative (UUnionMBase bool) where
  pure = single
  f <*> a = f >>= (\xf -> a >>= (return . xf))

bindUnion :: forall bool a b. SymBoolOp bool => UUnionBase bool a -> (a -> UUnionMBase bool b) -> UUnionMBase bool b
bindUnion (UUnionBase l1) f = go $ fmap (second f) l1
  where
    cc (Nothing, UUnionBase l) (UUAny _ (UUnionBase r)) = (Nothing, UUnionBase $ l ++ r)
    cc (Just s, UUnionBase l) (UUAny _ (UUnionBase r)) = (Just s, UUnionBase $ l ++ r)
    cc (_, UUnionBase l) (UUMrg s (UUnionBase r)) = (Just s, UUnionBase $ l ++ r)
    ins :: (bool, UUnionMBase bool b) -> UUnionMBase bool b
    ins (b, UUAny _ (UUnionBase l)) = freshUUAny $ UUnionBase $ fmap (first (b &&~)) l
    ins (b, UUMrg s (UUnionBase l)) = UUMrg s $ UUnionBase $ fmap (first (b &&~)) l
    go :: [(bool, UUnionMBase bool b)] -> UUnionMBase bool b
    go v = case foldl cc (Nothing, UUnionBase []) $ fmap ins v of
      (Just s, r) -> mergeWithStrategy s $ freshUUAny r
      (_, r) -> freshUUAny r

instance (SymBoolOp bool) => Monad (UUnionMBase bool) where
  a >>= f = bindUnion (underlyingUnion a) f

instance (SymBoolOp bool, GMergeable bool a) => GMergeable bool (UUnionMBase bool a) where
  gmergingStrategy = SimpleStrategy $ \cond t f -> unionIf cond t f >>= mrgSingle @bool

instance (SymBoolOp bool, GMergeable bool a) => GSimpleMergeable bool (UUnionMBase bool a) where
  gmrgIte = mrgIf

instance (SymBoolOp bool) => GMergeable1 bool (UUnionMBase bool) where
  liftGMergingStrategy m = SimpleStrategy $ \cond t f -> unionIf cond t f >>= (UUMrg m . single)

instance SymBoolOp bool => GSimpleMergeable1 bool (UUnionMBase bool) where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)

instance SymBoolOp bool => GUnionLike bool (UUnionMBase bool) where
  mergeWithStrategy _ m@(UUMrg _ _) = m
  mergeWithStrategy s (UUAny ref u) = unsafeDupablePerformIO $
    atomicModifyIORef' ref $ \case
      x@(Right r) -> (x, r)
      Left _ -> (Right r, r)
        where
          !r = UUMrg s $ fullReconstructUnordered s u -- m >>= mrgSingle
  {-# NOINLINE mergeWithStrategy #-}
  mrgIfWithStrategy s (Conc c) l r = if c then mergeWithStrategy s l else mergeWithStrategy s r
  mrgIfWithStrategy s cond l r =
    mergeWithStrategy s $ unionIf cond l r
  single = freshUUAny . single
  unionIf cond (UUAny _ a) (UUAny _ b) = freshUUAny $ unionIf cond a b
  unionIf cond (UUMrg m a) (UUAny _ b) = UUMrg m $ mrgIfWithStrategy m cond a b
  unionIf cond a (UUMrg m b) = UUMrg m $ mrgIfWithStrategy m cond (underlyingUnion a) b

instance (SymBoolOp bool, GSEq bool a) => GSEq bool (UUnionMBase bool a) where
  x `gsymeq` y = getSingle $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 `gsymeq` y1

liftToMonadUnion :: (SymBoolOp bool, GMergeable bool a, GMonadUnion bool u) => UUnionMBase bool a -> u a
liftToMonadUnion u = go (underlyingUnion u)
  where
    go (UUnionBase l) = go1 l
    go1 [(_, l)] = mrgSingle l
    go1 ((b1, a1) : l) = mrgIf b1 (mrgSingle a1) $ go1 l
    go1 _ = undefined

instance (SymBoolOp bool, GSOrd bool a) => GSOrd bool (UUnionMBase bool a) where
  x `gsymle` y = getSingle $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 `gsymle` y1
  x `gsymlt` y = getSingle $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 `gsymlt` y1
  x `gsymge` y = getSingle $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 `gsymge` y1
  x `gsymgt` y = getSingle $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 `gsymgt` y1
  x `gsymCompare` y = liftToMonadUnion $ do
    x1 <- x
    y1 <- y
    x1 `gsymCompare` y1

instance {-# OVERLAPPABLE #-} (SymBoolOp bool, ToSym a b, GMergeable bool b) => ToSym a (UUnionMBase bool b) where
  toSym = mrgSingle . toSym

instance {-# OVERLAPPING #-} (SymBoolOp bool, ToSym a b, GMergeable bool b) => ToSym (UUnionMBase bool a) (UUnionMBase bool b) where
  toSym = merge . fmap toSym

instance {-# OVERLAPPING #-} (SymBoolOp bool, ToSym a b, GMergeable bool b) => ToSym (Identity a) (UUnionMBase bool b) where
  toSym (Identity x) = toSym x

instance (SymBoolOp bool, ToCon a b) => ToCon (UUnionMBase bool a) b where
  toCon v = go $ underlyingUnion v
    where
      go (UUnionBase [(_, x)]) = toCon x
      go _ = Nothing

instance (SymBoolOp bool, GMergeable bool a, GEvaluateSym model a, GEvaluateSym model bool) => GEvaluateSym model (UUnionMBase bool a) where
  gevaluateSym fillDefault model x = go $ underlyingUnion x
    where
      go :: UUnionBase bool a -> UUnionMBase bool a
      go (UUnionBase l) = merge $ freshUUAny $ UUnionBase $ go1 l
      go1 [] = []
      go1 ((b, a) : l) =
        let cond = gevaluateSym fillDefault model b
         in if cond == conc False then go1 l else (cond, gevaluateSym fillDefault model a) : go1 l

instance
  (Monoid symbolSet, SymBoolOp bool, GExtractSymbolics symbolSet a, GExtractSymbolics symbolSet bool) =>
  GExtractSymbolics symbolSet (UUnionMBase bool a)
  where
  gextractSymbolics v = go $ underlyingUnion v
    where
      go (UUnionBase l) = go1 l
      go1 [] = mempty
      go1 ((b, a) : l) = gextractSymbolics b <> gextractSymbolics a <> go1 l

instance (Hashable bool, Hashable a) => Hashable (UUnionMBase bool a) where
  s `hashWithSalt` (UUAny _ u) = s `hashWithSalt` (0 :: Int) `hashWithSalt` u
  s `hashWithSalt` (UUMrg _ u) = s `hashWithSalt` (1 :: Int) `hashWithSalt` u

instance (Eq bool, Eq a) => Eq (UUnionMBase bool a) where
  UUAny _ l == UUAny _ r = l == r
  UUMrg _ l == UUMrg _ r = l == r
  _ == _ = False

instance (Eq bool) => Eq1 (UUnionMBase bool) where
  liftEq e l r = liftEq e (underlyingUnion l) (underlyingUnion r)

instance (SymBoolOp bool, Num a, GMergeable bool a) => Num (UUnionMBase bool a) where
  fromInteger = mrgSingle . fromInteger
  negate x = x >>= (mrgSingle . negate)
  x + y = x >>= \x1 -> y >>= \y1 -> mrgSingle $ x1 + y1
  x - y = x >>= \x1 -> y >>= \y1 -> mrgSingle $ x1 - y1
  x * y = x >>= \x1 -> y >>= \y1 -> mrgSingle $ x1 * y1
  abs x = x >>= mrgSingle . abs
  signum x = x >>= mrgSingle . signum

instance (SymBoolOp bool, ITEOp bool a, GMergeable bool a) => ITEOp bool (UUnionMBase bool a) where
  ites = mrgIf

instance (SymBoolOp bool, LogicalOp a, GMergeable bool a) => LogicalOp (UUnionMBase bool a) where
  a ||~ b = do
    a1 <- a
    b1 <- b
    mrgSingle $ a1 ||~ b1
  a &&~ b = do
    a1 <- a
    b1 <- b
    mrgSingle $ a1 &&~ b1
  nots x = do
    x1 <- x
    mrgSingle $ nots x1
  xors a b = do
    a1 <- a
    b1 <- b
    mrgSingle $ a1 `xors` b1
  implies a b = do
    a1 <- a
    b1 <- b
    mrgSingle $ a1 `implies` b1

instance (SymBoolOp bool, PrimWrapper t c, GMergeable bool t) => PrimWrapper (UUnionMBase bool t) c where
  conc = mrgSingle . conc
  ssymb = mrgSingle . ssymb
  isymb i s = mrgSingle $ isymb i s
  sinfosymb s info = mrgSingle $ sinfosymb s info
  iinfosymb i s info = mrgSingle $ iinfosymb i s info
  concView (SingleU (Conc b)) = Just b
  concView _ = Nothing

instance
  (SymBoolOp bool, Function f, GMergeable bool f, GMergeable bool a, Ret f ~ a) =>
  Function (UUnionMBase bool f)
  where
  type Arg (UUnionMBase bool f) = Arg f
  type Ret (UUnionMBase bool f) = UUnionMBase bool (Ret f)
  f # a = do
    f1 <- f
    mrgSingle $ f1 # a

instance (SymBoolOp bool, IsString a, GMergeable bool a) => IsString (UUnionMBase bool a) where
  fromString = mrgSingle . fromString

instance (SymBoolOp bool, GGenSym bool spec a, GMergeable bool a) => GGenSym bool spec (UUnionMBase bool a)

instance (SymBoolOp bool, GGenSym bool spec a) => GenSymSimple spec (UUnionMBase bool a) where
  genSymSimpleFresh spec = do
    res <- ggenSymFresh spec
    if not (isMerged res) then error "Not merged" else return res

instance
  (SymBoolOp bool, GGenSym bool a a, GenSymSimple () bool, GMergeable bool a) =>
  GGenSym bool (UUnionMBase bool a) a
  where
  ggenSymFresh spec = go (underlyingUnion $ merge spec)
    where
      go (UUnionBase l) = go1 l
      go1 [(_, x)] = ggenSymFresh x
      go1 ((_, x1) : xs) = mrgIf <$> genSymSimpleFresh () <*> ggenSymFresh x1 <*> go1 xs
      go1 _ = undefined

type UUnionM = UUnionMBase SymBool

instance ExtractUnionEither (UUnionMBase bool (Either e v)) (UUnionMBase bool) e v where
  extractUnionEither = id
