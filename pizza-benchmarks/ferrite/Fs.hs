module Fs where

import Lang
import Pizza

class (ToSym conc symb, ToCon symb conc, EvaluateSym Model symb) => FileSystem conc symb | conc -> symb, symb -> conc where
  crack :: conc -> [SysCall] -> [UnionM InodeOp]
  execute :: symb -> InodeOp -> UnionM symb
  ondisk :: symb -> Name -> UnionM (Maybe [SymBool])

  -- conc is ignored here
  reorder :: conc -> InodeOp -> InodeOp -> Bool
