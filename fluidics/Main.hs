{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import Data.Maybe (isJust)
import GHC.Generics
import Grisette
import Utils.Timing

type Grid = [[UnionM (Maybe B.ByteString)]]

makeGrid :: Integer -> Integer -> Grid
makeGrid height width = [[mrgReturn Nothing | _ <- [0 .. width - 1]] | _ <- [0 .. height - 1]]

unsafeReplaceNth :: Int -> a -> [a] -> [a]
unsafeReplaceNth _ _ [] = error "Failed"
unsafeReplaceNth p v (x : xs) = if p == 0 then v : xs else x : unsafeReplaceNth (p - 1) v xs

replaceNth :: (Mergeable a, MonadUnion m, MonadError () m) => SymInteger -> a -> [a] -> m [a]
replaceNth pos val ls = go ls 0
  where
    go [] i = mrgIf (pos .>= i) (throwError ()) (return [])
    go (x : xs) i = do
      hd <- mrgIf (pos .== i) (mrgReturn val) (mrgReturn x)
      tl <- go xs (i + 1)
      mrgReturn (hd : tl)

data ConcPoint = ConcPoint Integer Integer
  deriving (Show, Generic)
  deriving (ToCon Point) via (Default ConcPoint)

data Point = Point SymInteger SymInteger
  deriving (Show, Generic, GenSym ())
  deriving (EvaluateSym, Mergeable) via (Default Point)

instance GenSymSimple () Point where
  simpleFresh () = do
    x <- simpleFresh ()
    y <- simpleFresh ()
    return $ Point x y

gridRef :: Point -> StateT Grid (ExceptT () UnionM) (UnionM (Maybe B.ByteString))
gridRef (Point x y) = do
  g <- get
  l1 <- g .!! x
  l1 .!! y

gridSet :: Point -> UnionM (Maybe B.ByteString) -> StateT Grid (ExceptT () UnionM) ()
gridSet (Point x y) v = do
  g <- get
  xlist <- g .!! x
  r <- replaceNth y v xlist
  g1 <- replaceNth x r g
  merge $ put g1

unsafeSet :: Grid -> Int -> Int -> UnionM (Maybe B.ByteString) -> Grid
unsafeSet g x y v =
  let xlist = g !! x
      r = unsafeReplaceNth y v xlist
   in unsafeReplaceNth x r g

data Dir = N | S | W | E
  deriving (Show, Generic)
  deriving (Mergeable, EvaluateSym, ToCon Dir) via (Default Dir)

translatePoint :: Point -> Dir -> Point
translatePoint (Point x y) N = Point (x - 1) y
translatePoint (Point x y) S = Point (x + 1) y
translatePoint (Point x y) W = Point x (y - 1)
translatePoint (Point x y) E = Point x (y + 1)

instance GenSym () Dir where
  fresh () = chooseFresh [N, S, W, E]

move :: Point -> Dir -> StateT Grid (ExceptT () UnionM) ()
move p d = do
  droplet <- gridRef p
  let newpoint = translatePoint p d
  gridSet p (mrgReturn Nothing)
  gridSet newpoint droplet

mix :: Point -> StateT Grid (ExceptT () UnionM) ()
mix p = do
  let e = translatePoint p E
  let se = translatePoint e S
  let s = translatePoint p S
  a <- gridRef p
  b <- gridRef e
  symAssert .# mrgFmap (con . isJust) a
  symAssert .# mrgFmap (con . isJust) b
  gridSet p (mrgJust "c")
  gridSet e (mrgJust "c")
  merge $
    replicateM_ 3 $ do
      move p E
      move e S
      move se W
      move s N

data ConcInstruction = ConcMove ConcPoint Dir | ConcMix ConcPoint
  deriving (Show, Generic)
  deriving (ToCon Instruction) via (Default ConcInstruction)

data Instruction = Move Point (UnionM Dir) | Mix Point
  deriving (Show, Generic)
  deriving (Mergeable, EvaluateSym) via (Default Instruction)

instance GenSym () Instruction where
  fresh _ = do
    p <- simpleFresh ()
    d <- fresh ()
    chooseFresh [Move p d, Mix p]

interpretInstruction :: Instruction -> StateT Grid (ExceptT () UnionM) ()
interpretInstruction (Move p ud) = (lift . lift) ud >>= move p
interpretInstruction (Mix p) = mix p

synthTranslation :: Either () SymBool -> SymBool
synthTranslation (Left _) = con False
synthTranslation (Right v) = v

synthesizeProgram ::
  GrisetteSMTConfig n ->
  Int ->
  Grid ->
  (Grid -> ExceptT () UnionM SymBool) ->
  IO (Maybe [ConcInstruction])
synthesizeProgram config i initst f = go 0 (mrgReturn initst)
  where
    lst = genSymSimple (SimpleListSpec i ()) "a"
    go num st
      | num == i = return Nothing
      | otherwise =
          let newst = do
                t1 <- st
                ins <- lift (lst !! num)
                merge $ execStateT (interpretInstruction ins) t1
              cond = runExceptT $ newst >>= f
           in do
                print num
                _ <- timeItAll "evaluate" $ cond `deepseq` return cond
                r <- timeItAll "Lowering/Solving" $ solveExcept config synthTranslation cond
                case r of
                  Left _ -> go (num + 1) newst
                  Right m -> return $ toCon $ evaluateSym True m $ take (num + 1) lst

initSt :: Grid
initSt = unsafeSet (unsafeSet (makeGrid 5 5) 0 0 (mrgJust "a")) 0 2 (mrgJust "b")

spec :: StateT Grid (ExceptT () UnionM) SymBool
spec = do
  r <- gridRef (Point 4 2)
  r2 <- gridRef (Point 0 0)
  return $ r .== mrgJust "a" .&& r2 .== mrgJust "b"

main :: IO ()
main = timeItAll "Overall" $ do
  let (x :: UnionM Instruction) = genSym () "a"
  print x
  synthr <- synthesizeProgram (precise z3) 20 initSt (merge . evalStateT spec)
  print synthr
