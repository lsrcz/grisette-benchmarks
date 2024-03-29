module Bonsai.MatchSyntaxNoMemo where

import Bonsai.BonsaiTree
import Bonsai.SyntaxSpec
import qualified Data.ByteString as B
import qualified Data.HashSet as S
import GHC.TypeLits
import Grisette

matchSyntax ::
  (KnownNat n, 1 <= n) =>
  OptimSyntaxSpec n ->
  (Rule -> BonsaiTree (SymWordN n) -> SymBool) ->
  BonsaiTree (SymWordN n) ->
  B.ByteString ->
  SymBool
matchSyntax stx fR tree sym = case getRules stx sym of
  Nothing -> con False
  Just rus -> foldl (\acc rule -> acc .|| fR rule tree) (con False) rus
{-# INLINE matchSyntax #-}

matchRule ::
  (KnownNat n, 1 <= n) =>
  OptimSyntaxSpec n ->
  (BonsaiTree (SymWordN n) -> B.ByteString -> SymBool) ->
  (Rule -> BonsaiTree (SymWordN n) -> SymBool) ->
  Rule ->
  BonsaiTree (SymWordN n) ->
  SymBool
matchRule stx fS fR rule tree = case (tree, rule) of
  (_, SymRule sym) | sym `S.member` nonTerminals stx -> fS tree sym
  (BonsaiNode left right, PairRule first second) ->
    simpleMerge (fR first <$> left)
      .&& simpleMerge (fR second <$> right)
  (BonsaiLeaf sym, SymRule sym1) -> Just sym .== (con <$> terminalToBV stx sym1)
  _ -> con False
{-# INLINE matchRule #-}
