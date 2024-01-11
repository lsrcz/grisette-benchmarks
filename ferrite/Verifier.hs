{-# LANGUAGE OverloadedStrings #-}

module Verifier where

import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Maybe
import Fs
import Grisette
import Interpret
import Litmus
import Utils.Timing

verifyTranslation :: Either AssertionError () -> SymBool
verifyTranslation (Left _) = con True
verifyTranslation (Right _) = con False

verify ::
  forall b con fs.
  (FileSystem con fs, Mergeable fs, Show fs) =>
  GrisetteSMTConfig b ->
  Litmus con fs ->
  IO (Maybe con)
verify config (Litmus _ make setupProc prog allowCond) =
  let fs = make
      newfs =
        if not (null setupProc)
          then fromJust $ interpretConc setupProc fs
          else fs
      prog1 = crack newfs prog
      order =
        genSymSimple
          (SimpleListSpec (fromIntegral $ length prog1) (EnumGenUpperBound @Integer (fromIntegral $ length prog1)))
          "order"

      (verifFs, _) = runFresh (runStateT (interpretOrderOps prog1 order (mrgReturn (toSym newfs :: fs))) []) "crash"
      allowed = allowCond (toSym newfs) .# verifFs

      verifCond :: UnionM (Either AssertionError ())
      verifCond = runExceptT $ symAssert (validOrdering fs prog1 order `symImplies` allowed)
   in do
        _ <- timeItAll "evaluate" $ verifCond `deepseq` return ()
        r <- timeItAll "Lowering/Solving" $ solveExcept config verifyTranslation verifCond
        case r of
          Left _ -> return Nothing
          Right mo -> return $ (case evaluateSym True mo verifFs of Single v -> Just v; _ -> Nothing) >>= toCon
