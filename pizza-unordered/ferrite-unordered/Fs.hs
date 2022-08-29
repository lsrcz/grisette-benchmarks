module Fs where

import Lang
import Pizza
import Pizza.Unordered.UUnionM

class (ToSym conc symb, ToCon symb conc, EvaluateSym Model symb) => FileSystem conc symb | conc -> symb, symb -> conc where
  crack :: conc -> [SysCall] -> [UUnionM InodeOp]
  execute :: symb -> InodeOp -> UUnionM symb
  ondisk :: symb -> Name -> UUnionM (Maybe [SymBool])

  -- conc is ignored here
  reorder :: conc -> InodeOp -> InodeOp -> Bool
