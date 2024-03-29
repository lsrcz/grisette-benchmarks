module Main where

import Ext4
import Fs
import Grisette
import Lang
import Litmus
import Synth
import Utils.Timing
import Verifier

renameBound :: Integer
renameBound = 2

renameBlockSize :: Integer
renameBlockSize = 4096

createRenameFs :: ConcExt4Fs
createRenameFs = ext4fs renameBound renameBlockSize False

createRenameSetup :: [SysCall]
createRenameSetup = []

createRenameTestIncorrect :: [SysCall]
createRenameTestIncorrect = [Creat 0, Write 0 [True, True], Rename 0 1]

createRenameTestCorrect :: [SysCall]
createRenameTestCorrect = [Creat 0, Write 0 [True, True], Efsync 0 (con True), Rename 0 1]

createRenameAllow :: Ext4Fs -> Ext4Fs -> SymBool
createRenameAllow _ fs2 =
  let new1 = ondisk fs2 1
   in new1 .== mrgNothing .|| new1 .== mrgJust [con True, con True]

main :: IO ()
main = timeItAll "Overall" $ do
  {-
  print fs
  print $ crack fs incorrect
  print $ crack fs synthResult
  print $ interpretConc incorrect fs
  -}
  let origTest = Litmus renameBound createRenameFs createRenameSetup createRenameTestIncorrect createRenameAllow

  maybeProg <- synth (precise z3 {verbose = False}) origTest
  case maybeProg of
    Nothing -> putStrLn "Failed to synthesis"
    Just synthProg -> do
      putStrLn $ "Before: " ++ show createRenameTestIncorrect
      putStrLn $ "Synthesized: " ++ show (removeDisabledSyncs synthProg)
      let testSynth = Litmus renameBound createRenameFs createRenameSetup synthProg createRenameAllow
      cex <- verify (precise z3 {verbose = False}) testSynth
      case cex of
        Nothing -> putStrLn "Verified"
        Just _ -> putStrLn "Failed to verify"
