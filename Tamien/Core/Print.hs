{-
 - A pretty printer for the Core language.
 -}

module Tamien.Core.Print where

import Tamien.Core.Language

import Text.PrettyPrint

printProgram :: CoreProgram -> String
printProgram = render . docProgram

printExpr :: CoreExpr -> String
printExpr = render . docExpr

docProgram :: CoreProgram -> Doc
docProgram = vcat . punctuate (text "" $$ text "") . map docScDefn

docScDefn :: CoreScDefn -> Doc
docScDefn (name, vs, e)
    = text name <+> hsep (map text vs) <+> equals $+$ nest 4 (docExpr e)

docExpr :: CoreExpr -> Doc
docExpr (Var v)      = text v
docExpr (Num n)      = int n
docExpr (Constr t a) = undefined
docExpr (App e1 e2)  = docExpr e1 <+> docAExpr e2
docExpr (Let isrec defns expr)
    =   text isrecstr
        $$ nest 4 (docDefns defns)
        $+$ text "in" <+> docExpr expr
    where 
        isrecstr | isrec     = "letrec"
                 | not isrec = "let"
docExpr (Case e as)
    = text "case" <+> docExpr e <+> text "of" $+$ nest 4 (docAlts as)
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
docAlt (tag, vs, e)
    = int tag <+> hsep (map text vs) <+> arrow $$ nest 4 (docExpr e)

arrow :: Doc
arrow = text ("->")
