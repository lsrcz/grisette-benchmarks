module Interpret where

import Control.Monad.State.Strict
import Fs
import Grisette
import Lang

interpretOps :: (FileSystem con fs, Mergeable fs) => [UnionM InodeOp] -> fs -> UnionM fs
interpretOps [] fs = mrgReturn fs
interpretOps (x : xs) fs = do
  i <- x
  r <- execute fs i
  interpretOps xs r

interpretConc :: forall con fs. (FileSystem con fs, Mergeable fs) => [SysCall] -> con -> Maybe con
interpretConc s fs =
  ( case interpretOps (crack fs s) (toSym fs) :: UnionM fs of
      Single x -> Just x
      _ -> Nothing
  )
    >>= toCon

zoomy :: Fresh a -> StateT [SymBool] Fresh a
zoomy f = StateT $ \s -> (,s) <$> f {-do
                                    (inner, l) <- get
                                    (a, newInner) <- lift $ runStateT s inner
                                    put (newInner, l)
                                    return a-}

nonDet :: StateT [SymBool] Fresh SymBool
nonDet = do
  v <- zoomy (simpleFresh ())
  modify (v :)
  return v

interpretOrderOps ::
  forall con fs.
  (FileSystem con fs, Mergeable fs, Show fs) =>
  [UnionM InodeOp] ->
  [UnionM Integer] ->
  UnionM fs ->
  StateT [SymBool] Fresh (UnionM fs)
interpretOrderOps _ [] fs = return fs
interpretOrderOps l (x : xs) fs = do
  let fs1 = do
        f <- fs
        x1 <- x
        i <- l !! fromInteger x1
        merge $ execute f i
  r <- interpretOrderOps l xs fs1
  cond <- nonDet
  return $ mrgIf cond fs r

isPermutation :: [UnionM Integer] -> SymBool
isPermutation l = go [0 .. fromIntegral (length l) - 1]
  where
    go [] = con True
    go (x : xs) = go1 x l .== 1 .&& go xs
    go1 :: Integer -> [UnionM Integer] -> SymInteger
    go1 _ [] = 0
    go1 n (x : xs) = symIte (x .== mrgReturn n) 1 0 + go1 n xs

reorderOk :: forall con fs. (FileSystem con fs) => con -> [UnionM InodeOp] -> [UnionM Integer] -> SymBool
reorderOk fs iops = go
  where
    go [] = con True
    go (x : xs) = go1 x xs .&& go xs
    go1 _ [] = con True
    go1 x (l : ls) =
      let opx = (\v -> iops !! fromInteger v) =<< x
          opl = (\v -> iops !! fromInteger v) =<< l
       in go1 x ls
            .&& ( (x .> l)
                    `symImplies` ( ((\xv lv -> con (reorder fs xv lv)) .# opx .# opl)
                                     .&& (\xv lv -> con (reorder fs xv lv))
                                       .# opl
                                       .# opx
                                 )
                )

validOrdering :: forall con fs. (FileSystem con fs) => con -> [UnionM InodeOp] -> [UnionM Integer] -> SymBool
validOrdering fs iops ordering = isPermutation ordering .&& reorderOk fs iops ordering

insertSynthSyncs :: Integer -> [SysCall] -> Fresh [SysCall]
insertSynthSyncs i [] = do
  e <- simpleFresh (GenEfsync i)
  return [e]
insertSynthSyncs i (x : xs) = do
  e <- simpleFresh (GenEfsync i)
  tl <- insertSynthSyncs i xs
  return $ e : x : tl
