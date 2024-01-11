{-# LANGUAGE TemplateHaskell #-}

module Instructions where

import Data.List.Extra
import GHC.Generics
import Grisette
import Value

data Instruction
  = Halt
  | Noop
  | Push PCValue
  | Pop
  | Store
  | Load
  | Add
  | Add1
  | Load1
  | Store1AB
  | Store1B
  | Jump
  | Jump1AB
  | Jump1B
  | StoreCR
  | PopCR
  | Return
  | Call PCValue PCValue
  | Call1B PCValue
  | Return1B PCValue
  | Return1AB PCValue
  deriving (Show, Eq, Generic)
  deriving (SEq, Mergeable, EvaluateSym) via (Default Instruction)

$(makeUnionWrapper "u" ''Instruction)

data InstructionSpec
  = HaltIns
  | NoopIns
  | PushIns
  | PopIns
  | StoreIns
  | LoadIns
  | AddIns
  | Add1Ins
  | Load1Ins
  | Store1ABIns
  | Store1BIns
  | JumpIns
  | Jump1ABIns
  | Jump1BIns
  | StoreCRIns
  | PopCRIns
  | ReturnIns
  | CallIns
  | Call1BIns
  | Return1BIns
  | Return1ABIns
  deriving (Show, Eq, Ord)

instance GenSym InstructionSpec Instruction

instance GenSymSimple InstructionSpec Instruction where
  simpleFresh HaltIns = return Halt
  simpleFresh NoopIns = return Noop
  simpleFresh PushIns = Push <$> simpleFresh ()
  simpleFresh PopIns = return Pop
  simpleFresh StoreIns = return Store
  simpleFresh LoadIns = return Load
  simpleFresh AddIns = return Add
  simpleFresh Add1Ins = return Add1
  simpleFresh Load1Ins = return Load1
  simpleFresh Store1ABIns = return Store1AB
  simpleFresh Store1BIns = return Store1B
  simpleFresh JumpIns = return Jump
  simpleFresh Jump1ABIns = return Jump1AB
  simpleFresh Jump1BIns = return Jump1B
  simpleFresh StoreCRIns = return StoreCR
  simpleFresh PopCRIns = return PopCR
  simpleFresh ReturnIns = return Return
  simpleFresh CallIns =
    Call <$> simpleFresh () <*> simpleFresh ()
  simpleFresh Call1BIns = Call1B <$> simpleFresh ()
  simpleFresh Return1BIns = Return1B <$> simpleFresh ()
  simpleFresh Return1ABIns = Return1AB <$> simpleFresh ()

sortUniq :: (Ord a) => [a] -> [a]
sortUniq = sort . nubOrd

instance GenSym [InstructionSpec] Instruction where
  fresh spec =
    let uniqSpec = sortUniq spec
     in do
          l <- traverse simpleFresh uniqSpec
          chooseFresh l

instance GenSym Instruction Instruction

instance GenSymSimple Instruction Instruction where
  simpleFresh = derivedSameShapeSimpleFresh
