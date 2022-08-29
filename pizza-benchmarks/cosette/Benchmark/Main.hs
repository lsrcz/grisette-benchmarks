{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Benchmark.Queries
import Control.DeepSeq
import Denotation
import Equal
import Pizza
import Utils.Timing

main :: IO ()
main = timeItAll "Overall" $ do
  let r1 = $$(denoteSql q4)
  let r1r = $$(denoteSql q4r)
  let cond = $$(verifCondition q4 q4r)
  _ <- timeItAll "Evaluate" $ cond `deepseq` return ()
  r <- timeItAll "Lowering/Solving" $ solveFormula (UnboundedReasoning z3 {verbose = False, timing = PrintTiming}) cond
  case r of
    Left _ -> putStrLn "Verified"
    Right m -> do
      putStrLn "Found counter example:"
      putStrLn "t:"
      print $ evaluateSym True m t
      putStrLn "tr:"
      print $ evaluateSym True m tr
      putStrLn "ta:"
      print $ evaluateSym True m ta
      putStrLn "tb:"
      print $ evaluateSym True m tb
      putStrLn "q1 execution result:"
      print $ evaluateSym True m $$(denoteSql q1)
      putStrLn "q1r execution result:"
      print $ evaluateSym True m $$(denoteSql q1r)
      putStrLn "q4 execution result:"
      print $ evaluateSym True m r1
      putStrLn "q4r execution result:"
      print $ evaluateSym True m r1r
