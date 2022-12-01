module Core.Optimize where

import Core.Def

optCore :: CoreProgram -> CoreProgram
optCore = id