{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bonsai.BonsaiTree
import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.Trans.Class
import Data.Proxy
import Grisette
-- import Bonsai.Match
-- import Bonsai.Pattern
import STLC
-- import Bonsai.SyntaxSpec
import Utils.Timing

f8 :: UnionM STLCTree
f8 = genSym (8 :: Int) "h"

main :: IO ()
main = timeItAll "Overall" $ do
  let result = runExceptT $ lift f8 >>= execStlc
  _ <- timeItAll "evaluate" $ result `deepseq` return ()
  r <- timeItAll "Lowering/Solving" $ solveExcept (approx (Proxy @6) boolector) verifyTyperTranslation result
  case r of
    Left _ -> putStrLn "Verified"
    Right mo -> do
      putStrLn "Found bad"
      print $ showConcTree stlcSyntax <$> (toCon $ evaluateSym True mo f8 :: Maybe ConcSTLCTree)
