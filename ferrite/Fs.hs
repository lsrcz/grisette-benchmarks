module Fs where

import Grisette
import Lang

class (ToSym con sym, ToCon sym con, EvaluateSym sym) => FileSystem con sym | con -> sym, sym -> con where
  crack :: con -> [SysCall] -> [UnionM InodeOp]
  execute :: sym -> InodeOp -> UnionM sym
  ondisk :: sym -> Name -> UnionM (Maybe [SymBool])

  -- con is ignored here
  reorder :: con -> InodeOp -> InodeOp -> Bool
