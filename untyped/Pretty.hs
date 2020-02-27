{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Pretty where

import Text.PrettyPrint
import Prelude hiding ((<>))

import AST

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

viewVars :: Expr -> [Name]
viewVars (Lam n a) = n : viewVars a
viewVars _ = []

viewBody :: Expr -> Expr
viewBody (Lam _ a) = viewBody a
viewBody x = x

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

instance Pretty Name where
  ppr _ s = text s

instance Pretty Expr where
  ppr p e = case e of
    Lit (LInt a)  -> text (show a)
    Lit (LBool b) -> text (show b)
    Var x   -> text x
    App a b -> parensIf (p>0) $ (ppr (p+1) a) <+> (ppr p b)
    Lam _ _ -> parensIf (p>0) $
         char '\\'
      <> hsep (fmap pp (viewVars e))
      <+> "->"
      <+> ppr (p+1) (viewBody e)

ppexpr :: Expr -> String
ppexpr = render . ppr 0