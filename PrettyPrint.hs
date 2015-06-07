module PrettyPrint where

import Text.PrettyPrint
import TypeCheck

pprintSort :: Sort -> Doc
pprintSort O = text "o"
pprintSort (t1@(_ :-> _) :-> t2) = 
    parens (pprintSort t1) <+> text "->" <+> pprintSort t2
pprintSort (t1 :-> t2) =
    pprintSort t1  <+> text "->" <+> pprintSort t2

pprintTerm :: Term -> Doc
pprintTerm (Var x) = text (name x)
pprintTerm (Abst x t) =
    text "\\" <>
    parens (text (name x) <+> text "::" <+> pprintSort (sort x)) <+>
    text "->" <+>
    pprintTerm t
pprintTerm (Apply t1 t2) =
    let d1 = pprintTerm t1
        d2 = pprintTerm t2 in
    let d1' = case t1 of Abst _ _ -> parens d1
                         _ -> d1
        d2' = case t2 of Var _ -> d2
                         _ -> parens d2 in
    d1' <+> d2'

