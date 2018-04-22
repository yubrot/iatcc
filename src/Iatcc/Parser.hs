module Iatcc.Parser
  ( parse
  , Parser
  , program
  , expr
  , constant
  ) where

import Control.Monad
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isLetter)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Iatcc.AST

type Parser = Parsec Void Text

-- Parser

table :: [[Operator Parser Expr]]
table =
  [ [ unaryPrefix "dec" UnaryFxSub1
    , unaryPrefix "inc" UnaryFxAdd1
    , unaryPrefix "char->fixnum" UnaryCharToFx
    , unaryPrefix "fixnum->char" UnaryFxToChar
    , unaryPrefix "fixnum?" UnaryIsFixnum
    , unaryPrefix "boolean?" UnaryIsBoolean
    , unaryPrefix "char?" UnaryIsChar
    , unaryPrefix "zero?" UnaryIsFxZero
    , unaryPrefix "null?" UnaryIsNull
    , unaryPrefix "pair?" UnaryIsPair
    , unaryPrefix "!" UnaryNot
    , unaryPrefix "~" UnaryLogicalNot
    , unaryPrefix "car" UnaryCar
    , unaryPrefix "cdr" UnaryCdr
    ]
  , [ binaryInfixl "*" BinaryMul
    ]
  , [ binaryInfixl "+" BinaryAdd
    , binaryInfixl "-" BinarySub
    ]
  , [ binaryInfixl "." BinaryCons
    ]
  , [ binaryInfixl "<=" BinaryLesserEqual
    , binaryInfixl ">=" BinaryGreaterEqual
    , binaryInfixl "<"  BinaryLesser
    , binaryInfixl ">"  BinaryGreater
    ]
  , [ binaryInfixl "==" BinaryEqual
    , binaryInfixl "!=" BinaryNotEqual
    ]
  , [ binaryInfixl "&" BinaryLogicalAnd ]
  , [ binaryInfixl "|" BinaryLogicalOr ]
  , [ binaryInfixr "&&" BinaryAnd ]
  , [ binaryInfixr "||" BinaryOr ]
  , [ binaryInfixr "car=" BinarySetCar
    , binaryInfixr "cdr=" BinarySetCdr
    ]
  , [ binaryInfixr ";" BinarySeq ]
  ]
 where
  unaryPrefix name op = Prefix (EUnary op <$ reserved name)
  binaryInfixl name op = InfixL (EBinary op <$ reserved name)
  binaryInfixr name op = InfixR (EBinary op <$ reserved name)

parse :: Parser a -> String -> Text -> Either (ParseError (Token Text) Void) a
parse p = runParser (amb *> p <* eof)

program :: Parser Program
program = label "program" $ choice
  [ PLetRec <$> (reserved "letrec" *> fun `sepBy1` reserved "and") <*> (reserved "in" *> expr)
  , PExpr <$> expr
  ]

fun :: Parser Fun
fun = Fun <$> identifier <*> parens (identifier `sepBy` symbol ",") <*> (symbol "=" *> expr)

expr :: Parser Expr
expr = label "expr" $ makeExprParser term table

term :: Parser Expr
term = label "term" $ choice
  [ EConst <$> constant
  , parens expr
  , EIf <$> (reserved "if" *> expr) <*> (reserved "then" *> expr) <*> (reserved "else" *> expr)
  , termLet
  , varOrApp
  ]
 where
  varOrApp = do
    i <- identifier
    (EApp i <$> parens (expr `sepBy` symbol ",")) <|> return (EVar i)
  termLet = reserved "let" >> do
    let var = (,) <$> identifier <*> (symbol "=" *> expr)
    vars <- var `sepBy1` reserved "and"
    body <- reserved "in" *> expr
    return $ foldr (\(id, init) body -> ELet id init body) body vars

constant :: Parser Constant
constant = label "constant" $ choice
  [ CFixnum <$> signed integer
  , CBoolean True <$ reserved "true"
  , CBoolean False <$ reserved "false"
  , CChar <$> lexeme (char '\'' *> L.charLiteral <* char '\'') -- TODO: Remove host-language dependency
  , CNull <$ symbol "()"
  ]

-- Lexer

amb :: Parser ()
amb = L.space (void spaceChar) lineComment blockComment
 where
  lineComment = L.skipLineComment "//"
  blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme amb

signed :: Num a => Parser a -> Parser a
signed = L.signed amb

integer :: Parser Integer
integer = lexeme L.decimal

identifier :: Parser Text
identifier = lexeme $ fmap T.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

symbol :: Text -> Parser ()
symbol = lexeme . void . string

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reserved :: Text -> Parser ()
reserved s
  | isLetter (T.head s) = lexeme $ try $ void (string s) >> notFollowedBy alphaNumChar
  | otherwise = lexeme $ try $ void (string s) >> notFollowedBy (satisfy (`elem` T.unpack s))
