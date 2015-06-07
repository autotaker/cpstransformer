{-# LANGUAGE GADTs, TypeFamilies, DataKinds, TypeOperators #-}
module Syntax where

data Sort = O | Sort :-> Sort
infixr 4 :->

data TSort :: Sort -> * where
    TFun :: TSort ty -> TSort ty' -> TSort (ty :-> ty')
    TO   :: TSort O

data Symbol ty = Symbol { name :: String, sort :: TSort ty }
    
data Term ty where
    Var   :: Symbol ty -> Term ty
    Abst  :: Symbol ty -> Term ty' -> Term (ty :-> ty')
    Apply :: Term (ty' :-> ty) -> Term ty' -> TSort ty -> Term ty

apply :: Term (ty' :-> ty) -> Term ty' -> Term ty
apply t1 t2 = 
    case getSort t1 of TFun _ ty -> Apply t1 t2 ty

data TyEq :: Sort -> Sort -> *  where
    TyEq    :: TyEq ty ty
    TyNotEq :: TyEq ty1 ty2

tyComp :: TSort ty -> TSort ty' -> TyEq ty ty'
tyComp TO TO = TyEq
tyComp (TFun t1 t2) (TFun t3 t4) =
    case tyComp t1 t3 of
        TyEq -> case tyComp t2 t4 of
            TyEq -> TyEq
            _ -> TyNotEq
        _ -> TyNotEq
tyComp _ _ = TyNotEq

class HasSort f where
    getSort :: f a -> TSort a

instance HasSort Symbol where
    getSort = sort

instance HasSort Term where
    getSort (Var x) = getSort x
    getSort (Abst x t) = TFun (getSort x) (getSort t)
    getSort (Apply _ _ ty) = ty

type family CPSSort (t :: Sort) :: Sort
type instance CPSSort O = O
type instance CPSSort (ty :-> ty') = 
    CPSSort ty :-> (CPSSort ty' :-> O) :-> O

cpsSort :: TSort ty -> TSort (CPSSort ty)
cpsSort TO = TO
cpsSort (TFun ty ty') = TFun (cpsSort ty) (TFun (TFun (cpsSort ty') TO) TO)

cpsS :: Symbol ty -> Symbol (CPSSort ty)
cpsS sym = Symbol (name sym) (cpsSort (sort sym))

newSym :: String -> TSort ty -> Symbol ty
newSym x ty = Symbol x ty

cpsT :: Term ty -> Term (CPSSort ty :-> O) -> Term O 
cpsT (Var x) k = apply k (Var (cpsS x))
cpsT (Abst x t) k = 
    let k' = newSym "k" (TFun (cpsSort (getSort t)) TO)  in
    apply k (Abst (cpsS x) (Abst k' (cpsT t (Var k'))))
cpsT (Apply t1 t2 _) k =
    let f = newSym "f" (cpsSort (getSort t1)) in
    let x = newSym "x" (cpsSort (getSort t2)) in
    cpsT t1 $ Abst f $
        cpsT t2 $ Abst x $ apply (apply (Var f) (Var x)) k
