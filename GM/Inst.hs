module GM.Inst where

import Utils

data GMInst
  = PushG  String
  | Push   Int
  | Pop    Int
  | MkApp 
  | Update Int
  | Pack   Int Int
  | Split
  | Jump   (Map Int [GMInst])
  | Slide  Int
  | Eval
  | Alloc  Int
  | Unwind

-- data Env
--   = VarEnv String Env
--   | OffsetEnv Int Env
--   | EmptyEnv

-- hasVar :: Env -> String -> Bool 
-- hasVar EmptyEnv _ = False
-- hasVar (VarEnv n' p) n = (n == n') || hasVar p n
-- hasVar (OffsetEnv _ p) n = hasVar p n

-- compile :: TypeEnv -> Expr -> [GMInst]
-- compile _ (ILitE n) = [PushI n]


