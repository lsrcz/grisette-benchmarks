{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bonsai.BonsaiTree
import Control.DeepSeq
import Control.Monad.Except
import LetPoly
import Pizza
import Pizza.Unordered.UUnionM
import Utils.Timing

f7 :: UUnionM LetPolyTree
f7 = genSym (7 :: Int) "a"

main :: IO ()
main = timeItAll "Overall" $ do
  let result = runExceptT $ lift f7 >>= execLetPoly
  _ <- timeItAll "evaluate" $ result `deepseq` return ()
  r <- timeItAll "Lowering/Solving" $ solveFallable (BoundedReasoning @7 boolector {verbose = False}) verifyTyperTranslation result
  case r of
    Left _ -> putStrLn "Verified"
    Right mo -> do
      putStrLn "Found bad"
      print $ showConcTree letPolySyntax <$> (toCon $ evaluateSym True mo f7 :: Maybe ConcLetPolyTree)
      print $ evaluateSym True mo result
      print $ evaluateSym False mo f7
      print $ evaluateSym True mo (lift f7) >>= execLetPoly
