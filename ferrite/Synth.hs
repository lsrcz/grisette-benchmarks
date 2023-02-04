{-# LANGUAGE OverloadedStrings #-}

module Synth where

import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Maybe
import Fs
import Grisette
import Interpret
import Lang
import Litmus
import Utils.Timing

syncCost :: [SysCall] -> SymInteger
syncCost (Efsync _ e : xs) = ites e 1 0 + syncCost xs
syncCost (_ : xs) = syncCost xs
syncCost [] = 0

synthVCTranslation :: Either AssertionError () -> CEGISCondition
synthVCTranslation (Left _) = cegisPrePost (con True) (con False)
synthVCTranslation _ = cegisPrePost (con True) (con True)

synth ::
  forall b con fs.
  (FileSystem con fs, Mergeable fs, Show fs) =>
  GrisetteSMTConfig b ->
  Litmus con fs ->
  IO (Maybe [SysCall])
synth config (Litmus fsBound make setupProc prog allowCond) =
  let fs = make
      newfs
        | not (null setupProc) = fromJust $ interpretConc setupProc fs
        | otherwise = fs
      progWithSyncs = runFresh (insertSynthSyncs fsBound prog) "syncs"
      prog1 = crack newfs progWithSyncs

      order =
        genSymSimple
          (SimpleListSpec (fromIntegral $ length prog1) (EnumGenUpperBound @Integer (fromIntegral $ length prog1)))
          "order"
      -- order = [0,1,2,4,5,3,6]
      (synthFs, crashes) = runFresh (runStateT (interpretOrderOps prog1 order (mrgReturn (toSym newfs :: fs))) []) "crash"
      allowed = allowCond (toSym newfs) #~ synthFs

      cost = syncCost progWithSyncs
      go sol currCost =
        let costConstraint = con (currCost == fromIntegral (length progWithSyncs)) ||~ cost <~ currCost
            synthCond :: UnionM (Either AssertionError ())
            synthCond = runExceptT $ symAssert ((validOrdering fs prog1 order `implies` allowed) &&~ costConstraint)
         in do
              _ <- timeItAll "evaluate" $ synthCond `deepseq` return ()
              m <- timeItAll "Lowering/Solving" $ cegisExcept config (crashes, order) synthVCTranslation synthCond
              case m of
                Left _ -> return sol
                Right (_, mo) -> go (Just $ evaluateSym True mo progWithSyncs) $ evaluateSym True mo cost
   in go Nothing (fromIntegral $ length progWithSyncs)
