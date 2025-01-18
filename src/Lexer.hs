module Lexer (
    lexer,
    parens,
    identifier,
    reserved,
    reservedOp,
    int',
    commaSep,
    braces,
    decimal',
    angles
) where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style = emptyDef {
        Tok.commentLine = "//",
        Tok.reservedOpNames = ["+", "*", "-", "/", "=", ":", "(", ")", "{", "}", "<", ">", ",", "[", "]"],
        Tok.reservedNames = ["func", "if", "else", "while", "return", "vec", "i32", "u32", "i16", "u16", "float", "double"]
    }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

angles :: Parser a -> Parser a
angles = Tok.angles lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

int' :: Parser Integer
int' = Tok.integer lexer

decimal' :: Parser Double
decimal' = Tok.float lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer