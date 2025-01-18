module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)

-- Лексер
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style = emptyDef {
        Tok.commentLine = "//",
        Tok.reservedOpNames = ["+", "-", "*", "/", "<", ">", "=", ":", ",", "(", ")", "{", "}"],
        Tok.reservedNames = ["func", "if", "else", "while", "for", "i32", "u32", "i16", "u16", "float", "double"]
    }

-- Простые парсеры для базовых лексем
parens :: Parser a -> Parser a
parens = Tok.parens lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

-- Пример использования лексера:
-- parseExpr, parseFunction, etc. в вашем парсере
