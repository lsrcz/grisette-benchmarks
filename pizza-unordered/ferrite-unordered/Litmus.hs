module Litmus where

import Lang
import Pizza

data Litmus conc fs = Litmus
  { bound :: Integer,
    makeFs :: conc,
    setup :: [SysCall],
    test :: [SysCall],
    allow :: fs -> fs -> SymBool
  }
