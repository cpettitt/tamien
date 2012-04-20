{-
 - A pretty printer for the Core language.
 -}

module Tamien.Core.Printer
    ( printProgram
    , printExpr
    ) where

import Tamien.Core.Language

import Text.PrettyPrint

printProgram :: CoreProgram -> String
printProgram = render . docProgram

printExpr :: CoreExpr -> String
printExpr = render . docExpr

docProgram :: CoreProgram -> Doc
docProgram = vcat . punctuate (space <> semi) . map docScDefn

docScDefn :: CoreScDefn -> Doc
docScDefn (ScDefn name vs e)
    = text name <+> hsep (map text vs) <+> equals $+$ nest 4 (docExpr e)

docExpr :: CoreExpr -> Doc
docExpr (Var v)      = text v
docExpr (Num n)      = int n
docExpr (Constr t a) = text "Pack" <> lbrace <> int t <> comma <> int a <> rbrace
docExpr (App e1 e2)  = docAExpr e1 <+> docAExpr e2
docExpr (Let isrec defns expr)
    =   text isrecstr
        $$ nest 4 (docDefns defns)
        $+$ text "in" <+> docExpr expr
    where 
        isrecstr | isrec     = "letrec"
                 | not isrec = "let"
docExpr (Case e as)
    = text "case" <+> docExpr e <+> text "of" <+> char '{' $+$ nest 4 (docAlts as) $+$ char '}'
docExpr (Lam vs e)
    = char '\\' <+> hsep (map text vs) <+> hang arrow 4 (docExpr e)

docAExpr :: CoreExpr -> Doc
docAExpr e | isAtomicExpr e = docExpr e
           | otherwise      = lparen <> docExpr e <> rparen

docDefns :: [(Name, CoreExpr)] -> Doc
docDefns = vcat . punctuate semi . map docDefn

docDefn :: (Name, CoreExpr) -> Doc
docDefn (name, expr) = text name <> text " = " <> docExpr expr

docAlts :: [CoreAlt] -> Doc
docAlts = vcat . punctuate semi . map docAlt

docAlt :: CoreAlt -> Doc
docAlt (Alt tag vs e)
    = angles (int tag) <+> hsep (map text vs) <+> arrow $$ nest 4 (docExpr e)

arrow :: Doc
arrow = text "->"

angles :: Doc -> Doc
angles doc = char '<' <> doc <> char '>'

-- | True if the expression has no internal structure
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (Var _)      = True
isAtomicExpr (Num _)      = True
isAtomicExpr (Constr _ _) = True
isAtomicExpr _            = False

