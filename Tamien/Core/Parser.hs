{-
 - A parser for the Core language.
 -}

module Tamien.Core.Parser where

import Tamien.Core.Language

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.List (foldl1')
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as PT

-- PARSER

parseExpr :: Parser CoreExpr
parseExpr = choice [ parseApp
                   , parseLet
                   , parseLetRec
--                   , parseCase
                   , parseLam
                   , parseAExpr
                   ]

parseApp :: Parser CoreExpr
parseApp = foldl1' App <$> many1 parseAExpr

parseLam :: Parser CoreExpr
parseLam = Lam <$> (backslash *> many1 identifier) <*> (arrow *> parseExpr)

parseLet :: Parser CoreExpr
parseLet
    = Let False <$> (reserved "let" *> parseDefns) <*> (reserved "in" *> parseExpr)

parseLetRec :: Parser CoreExpr
parseLetRec
    = Let True <$> (reserved "letrec" *> parseDefns) <*> (reserved "in" *> parseExpr)

parseDefns :: Parser [(Name, CoreExpr)]
parseDefns = semiSep1 parseDefn

parseDefn :: Parser (Name, CoreExpr)
parseDefn = (,) <$> identifier <* equals <*> parseExpr

parseCase :: Parser CoreExpr
parseCase = undefined

parseAExpr :: Parser CoreExpr
parseAExpr = choice [ parseVar
                    , parseNum
                    , parseConstr
                    , parsePExpr
                    ]

parseVar :: Parser CoreExpr
parseVar = Var <$> identifier

-- TODO overflow check
parseNum :: Parser CoreExpr
parseNum = Num <$> natural

parseConstr :: Parser CoreExpr
parseConstr = reserved "Pack" *> braces (Constr <$> natural <* comma  <*> natural)

parsePExpr :: Parser CoreExpr
parsePExpr = parens parseExpr

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

arrow      = symbol "->"
backslash  = symbol "\\"
braces     = PT.braces lexer
comma      = PT.comma lexer
equals     = symbol "="
identifier = PT.identifier lexer
natural    = fromIntegral <$> PT.natural lexer
parens     = PT.parens lexer
reserved   = PT.reserved lexer
semiSep1   = PT.semiSep1 lexer
symbol     = PT.symbol lexer
