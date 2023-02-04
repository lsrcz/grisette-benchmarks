module Litmus where

import Grisette
import Lang

data Litmus con fs = Litmus
  { bound :: Integer,
    makeFs :: con,
    setup :: [SysCall],
    test :: [SysCall],
    allow :: fs -> fs -> SymBool
  }
