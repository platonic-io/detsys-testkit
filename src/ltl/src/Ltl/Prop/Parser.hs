{-# LANGUAGE OverloadedStrings #-}

module Ltl.Prop.Parser (parse) where

import Control.Applicative
import qualified Control.Monad.Combinators.Expr as PE
import qualified Data.Char as Char
import Data.Text (Text)
import Data.Void (Void)
import Ltl.Json (Json)
import qualified Ltl.Json as Json
import Ltl.Prop
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PL

type Parser = Parsec Void Text

space = PL.space P.space1 (PL.skipLineComment "//") (PL.skipBlockComment "/*" "*/")

parens = P.between (PL.symbol space "(") (PL.symbol space ")")

stringVar = ((:) <$> P.letterChar <*> many P.alphaNumChar P.<?> "variable")

decimal = toInteger <$> PL.signed space (PL.lexeme space PL.decimal)

pJQ =
  P.choice
    [ P.try $
        P.choice
          [ flip Json.Lookup <$ PL.symbol space "." <*> P.takeWhileP Nothing Char.isAlphaNum <*> pJQ,
            flip Json.Index <$ PL.symbol space "[" <*> decimal <* PL.symbol space "]" <*> pJQ
          ],
      pure Json.This
    ]

pVar = f <$ (P.satisfy (== '@')) <*> stringVar <*> PL.lexeme space (P.option Before (After <$ P.char '\'')) <*> PL.lexeme space pJQ
  where
    f x temp = Var temp (VariableNode x)

pEvent = PL.symbol space "$" *> PL.lexeme space pJQ

pInt =
  P.choice
    [ IConst <$> decimal,
      IVarAdd <$> PL.lexeme space stringVar <*> (PL.symbol space "+" *> decimal <|> pure 0)
    ]

pConst :: Parser Json
pConst = do
  t <- P.char '`' *> P.takeWhileP (Just "json") (/= '`') <* PL.symbol space "`"
  case Json.decode t of
    Left reason -> fail reason
    Right x -> pure x

pExpr :: Parser Expr
pExpr =
  P.choice
    [ Variable <$> pVar,
      Constant <$> pConst,
      IntLang <$> pInt,
      EEvent <$> pEvent
    ]

pPredicate :: Parser Formula
pPredicate = f <$> pExpr <* PL.symbol space "=" <*> pExpr
  where
    f :: Expr -> Expr -> Formula
    f x y = P (Eq x y)

pTrue = TT <$ PL.symbol space "TT"

pFalse = FF <$ PL.symbol space "FF"

pTerm =
  P.choice
    [ pTrue,
      pFalse,
      pPredicate,
      parens parser
    ]

intList = P.between (PL.symbol space "[") (PL.symbol space "]") $ P.sepBy decimal (PL.symbol space ",")

pDomainExists = PE.Prefix (flip ExistsInt <$ PL.symbol space "exists" <*> PL.lexeme space stringVar <* PL.symbol space "in" <*> intList <* PL.symbol space ".")

pDomainForall = PE.Prefix (flip ForallInt <$ PL.symbol space "forall" <*> PL.lexeme space stringVar <* PL.symbol space "in" <*> intList <* PL.symbol space ".")

operatorTable =
  [ [prefix ["!", "~"] Neg],
    [ binary ["&&", "/\\"] And,
      binary ["||", "\\/"] Or,
      PE.Postfix (flip Imp <$ PL.symbol space "->" <*> parser)
    ]
  ]

prefix names f = PE.Prefix (f <$ P.choice [PL.symbol space name | name <- names])

binary names f = PE.InfixR (f <$ P.choice [PL.symbol space name | name <- names])

parser :: Parser Formula
parser =
  P.choice
    [ P.try $ flip ExistsInt <$ PL.symbol space "exists" <*> PL.lexeme space stringVar <* PL.symbol space "in" <*> intList <* PL.symbol space "." <*> parser,
      P.try $ flip ForallInt <$ PL.symbol space "forall" <*> PL.lexeme space stringVar <* PL.symbol space "in" <*> intList <* PL.symbol space "." <*> parser,
      ExistsNode <$ PL.symbol space "exists" <*> PL.lexeme space stringVar <* PL.symbol space "." <*> parser,
      ForallNode <$ PL.symbol space "forall" <*> PL.lexeme space stringVar <* PL.symbol space "." <*> parser,
      Always <$ P.choice [PL.symbol space name | name <- ["always", "[]"]] <*> parser,
      Eventually <$ P.choice [PL.symbol space name | name <- ["atSomePoint", "eventually", "<>"]] <*> parser,
      PE.makeExprParser pTerm operatorTable
    ]

parse :: Text -> Either String Formula
parse t = case P.parse (parser <* P.eof) "" t of
  Left bundle -> Left (P.errorBundlePretty bundle)
  Right f -> Right f
