module Core.Optimize (optCore) where

import qualified Data.Map as M

import Common.Def
import Core.Def

type ConstrMap = M.Map Ident (Int, Int)

optCore :: CoreProgram -> CoreProgram
optCore (constrs, fns) =
  (constrs, map (optFn cMap) fns)
  where
    cMap = M.fromList $ map
      (\(name, arity, tag) -> (getIdent name, (arity, tag)))
      constrs

optFn :: ConstrMap -> CoreFn -> CoreFn
optFn cMap (name, params, body) =
  (name, params, optExpr cMap body)

optExpr :: ConstrMap -> CoreExpr -> CoreExpr
optExpr = hnfExpr

hnfExpr :: ConstrMap -> CoreExpr -> CoreExpr
hnfExpr cMap e =
  case e of
    GConstrCE name ->
      let (arity, tag) = cMap M.! getIdent name
      in if arity == 0
          then HNFCE (name, arity, tag) []
          else e
    AppCE e1 e2 ->
      case hnfApp cMap e1 e2 of
        Just e -> e
        Nothing -> AppCE (hnfExpr cMap e1) (hnfExpr cMap e2)
    HNFCE c es -> HNFCE c (map (hnfExpr cMap) es)
    CaseCE e brs -> CaseCE (hnfExpr cMap e) (map hnfBr brs)
    LetCE bind e -> LetCE (hnfBind bind) (hnfExpr cMap e)
    LetRecCE binds e -> LetRecCE (map hnfBind binds) (hnfExpr cMap e)
    LambdaCE params e -> LambdaCE params (hnfExpr cMap e)
    _ -> e
  where
    hnfBr (tag, binds, body) = (tag, binds, hnfExpr cMap body)
    hnfBind (id, e) = (id, hnfExpr cMap e)

hnfApp :: ConstrMap -> CoreExpr -> CoreExpr -> Maybe CoreExpr
hnfApp cMap f last =
  case appToList f of
    GConstrCE name : es ->
      let (arity, tag) = cMap M.! getIdent name
      in if arity == 1 + length es
          then Just $ HNFCE (name, arity, tag) (es ++ [last])
          else Nothing
    _ -> Nothing
  where
    appToList :: CoreExpr -> [CoreExpr]
    appToList (AppCE e1 e2) = appToList e1 ++ [e2]
    appToList e = [e]