{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bonsai.BonsaiTree
import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.Trans.Class
import Data.Proxy
import Grisette
import NanoScala
import Utils.Timing

f10 :: UnionM DotTree
f10 = genSym (10 :: Int) "a"

main :: IO ()
main = timeItAll "Overall" $ do
  let result = runExceptT $ lift f10 >>= execDot
  _ <- timeItAll "evaluate" $ result `deepseq` return ()
  r <- timeItAll "Lowering/Solving" $ solveExcept (approx (Proxy @6) boolector) verifyTyperTranslation result
  case r of
    Left _ -> putStrLn "Verified"
    Right mo -> do
      putStrLn "Found bad"
      print $ showConcTree dotSyntax <$> (toCon $ evaluateSym True mo f10 :: Maybe ConcDotTree)
