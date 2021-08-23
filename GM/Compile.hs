module GM.Compile where

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
