{-# LANGUAGE GADTs, RankNTypes #-}
module TypeCheck where

import qualified Syntax as S

data Symbol = Symbol { name :: String, sort :: Sort } deriving Show
data Sort = O | Sort :-> Sort  deriving(Show)
data Term = Var Symbol | Abst Symbol Term | Apply Term Term deriving(Show)

infixr 4 :->
toTSort :: Sort -> (forall ty. S.TSort ty -> a) -> a
toTSort O k = k S.TO
toTSort (t1 :-> t2) k = 
    toTSort t1 (\ty1 -> toTSort t2 (\ty2 -> k (S.TFun ty1 ty2)))

typeCheck :: Term -> a -> (forall ty. S.Term ty -> a) -> a
typeCheck (Var x) _ k = 
    toTSort (sort x) $ \ty -> k (S.Var (S.Symbol (name x) ty))
typeCheck (Abst x t) k' k =
    toTSort (sort x) $ \ty ->
        typeCheck t k' $ \t' -> k (S.Abst (S.Symbol (name x) ty) t')
typeCheck (Apply t1 t2) k' k =
    typeCheck t1 k' $ \t1' ->
        typeCheck t2 k' $ \t2' ->
        case S.getSort t1' of
            S.TFun ty1 _ -> 
                case S.tyComp ty1 (S.getSort t2') of
                    S.TyEq -> k (S.apply t1' t2')
                    _ -> k'
            _ -> k'

fromTSort :: S.TSort ty -> Sort
fromTSort S.TO = O
fromTSort (S.TFun t1 t2) = fromTSort t1 :-> fromTSort t2

fromSymbol :: S.Symbol ty -> Symbol
fromSymbol x = Symbol (S.name x) (fromTSort $ S.sort x)
fromTerm :: S.Term ty -> Term
fromTerm (S.Var x) = Var $ fromSymbol x
fromTerm (S.Abst x t) = Abst (fromSymbol x) $ fromTerm t
fromTerm (S.Apply t1 t2 _) = Apply (fromTerm t1) (fromTerm t2)
