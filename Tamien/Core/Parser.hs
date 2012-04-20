{-
 - A parser for the Core language.
 -}

module Tamien.Core.Parser
    ( parse
    , parseExpr
    , isKeyword
    ) where

import Tamien.Core.Language

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.List (foldl1')
import Text.Parsec hiding (parse, runParser)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as PT

-- PARSER

parse:: String -> Either String CoreProgram
parse = runParser pProgram

parseExpr :: String -> Either String CoreExpr
parseExpr = runParser pExpr

runParser :: Parser a -> String -> Either String a
runParser p str
    = case P.parse p "" str of
        Left err -> Left $ show err
        Right x  -> Right x

pProgram :: Parser CoreProgram
pProgram = semiSep1 pScDefn

pScDefn :: Parser CoreScDefn
pScDefn = ScDefn <$> identifier <*> many identifier <*> (equals *> pExpr)

pExpr :: Parser CoreExpr
pExpr = choice [ pApp
               , pLet
               , pLetRec
               , pCase
               , pLam
               , pAExpr
               ]

pApp :: Parser CoreExpr
pApp = foldl1' App <$> many1 pAExpr

pLam :: Parser CoreExpr
pLam = Lam <$> (backslash *> many1 identifier) <*> (arrow *> pExpr)

pLet :: Parser CoreExpr
pLet
    = Let False <$> (reserved "let" *> pDefns) <*> (reserved "in" *> pExpr)

pLetRec :: Parser CoreExpr
pLetRec
    = Let True <$> (reserved "letrec" *> pDefns) <*> (reserved "in" *> pExpr)

pDefns :: Parser [(Name, CoreExpr)]
pDefns = semiSep1 pDefn

pDefn :: Parser (Name, CoreExpr)
pDefn = (,) <$> identifier <* equals <*> pExpr

pCase :: Parser CoreExpr
pCase = Case <$> (reserved "case" *> pExpr) <*> (reserved "of" *> braces pAlts)

pAlts :: Parser [CoreAlt]
pAlts = sepBy1 pAlt semi 

pAlt :: Parser CoreAlt
pAlt = Alt <$> angles natural <*> many identifier <*> (arrow *> pExpr)

pAExpr :: Parser CoreExpr
pAExpr = choice [ pVar
                    , pNum
                    , pConstr
                    , pPExpr
                    ]

pVar :: Parser CoreExpr
pVar = Var <$> identifier

-- TODO overflow check
pNum :: Parser CoreExpr
pNum = Num <$> natural

pConstr :: Parser CoreExpr
pConstr = reserved "Pack" *> braces (Constr <$> natural <* comma  <*> natural)

pPExpr :: Parser CoreExpr
pPExpr = parens pExpr

-- LEXER

lexer :: PT.TokenParser st
lexer = PT.makeTokenParser lexerDef

lexerDef :: PT.LanguageDef st
lexerDef
    = PT.LanguageDef
        { PT.commentStart    = "{-"
        , PT.commentEnd      = "-}"
        , PT.commentLine     = "--"
        , PT.nestedComments  = True
        , PT.identStart      = letter <|> char '_'
        , PT.identLetter     = alphaNum <|> oneOf "_'"
        , PT.opStart         = PT.opLetter lexerDef
        , PT.opLetter        = oneOf ":!@#$%^&*-+./<=>?|~"
        , PT.reservedOpNames = []
        , PT.reservedNames   = [ "let", "letrec", "in", "case", "of", "Pack" ]
        , PT.caseSensitive   = True
        }

isKeyword :: String -> Bool
isKeyword =  (`elem` PT.reservedNames lexerDef)

angles     = PT.angles lexer
arrow      = symbol "->"
backslash  = symbol "\\"
braces     = PT.braces lexer
comma      = PT.comma lexer
equals     = symbol "="
identifier = PT.identifier lexer
natural    = fromIntegral <$> PT.natural lexer
parens     = PT.parens lexer
reserved   = PT.reserved lexer
semi       = PT.semi lexer
semiSep1   = PT.semiSep1 lexer
symbol     = PT.symbol lexer
