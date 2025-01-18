module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import AST
import Lexer (lexer, parens, identifier, reserved, reservedOp, int', commaSep, braces, decimal', angles)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex
import Control.Monad.Identity
import Debug.Trace

-- Добавляем отладочный парсер
debugParser :: String -> Parser a -> Parser a
debugParser label p = do
    pos <- getPosition
    traceM $ "Attempting " ++ label ++ " at " ++ show pos
    result <- p
    traceM $ "Succeeded " ++ label
    return result

-- Парсер для числа
parseNumber :: Parser Expr
parseNumber = debugParser "number" $ do
    spaces
    num <- many1 digit
    spaces
    return $ Number (read num)

-- Парсер для переменной
parseVariable :: Parser Expr
parseVariable = debugParser "variable" $ Variable <$> identifier

-- Парсер для типа вектора
parseVectorType :: Parser Type
parseVectorType = debugParser "vector type" $ do
    reserved "vec"
    angles $ do
        size <- int'
        reservedOp ","
        Primitive elemType <- parseType
        return $ VectType (fromInteger size) elemType

-- Парсер для типа
parseType :: Parser Type
parseType = debugParser "type" $ choice [
    try parseVectorType,
    (reserved "i32" >> return (Primitive I32)),
    (reserved "u32" >> return (Primitive U32)),
    (reserved "i16" >> return (Primitive I16)),
    (reserved "u16" >> return (Primitive U16)),
    (reserved "float" >> return (Primitive FLOAT)),
    (reserved "double" >> return (Primitive DOUBLE))
    ]

-- Парсер для объявления переменной
parseDefVar :: Parser Expr
parseDefVar = debugParser "variable definition" $ do
    name <- identifier
    reservedOp ":"
    typ <- parseType
    reservedOp "="
    value <- parseExpr
    return $ DefVar name typ value

-- Парсер для функции
parseFunction :: Parser Expr
parseFunction = debugParser "function" $ do
    reserved "func"
    name <- identifier
    args <- parens (return [])
    body <- Tok.braces lexer (many parseExpr)
    return $ Function name args body

-- Добавляем парсер для return
parseReturn :: Parser Expr
parseReturn = debugParser "return" $ do
    reserved "return"
    expr <- parseExpr
    return $ Return expr

-- Определяем таблицу операторов
operators :: [[Ex.Operator String () Identity Expr]]
operators = [
    [ binary "*" (BinOp Times) Ex.AssocLeft
    , binary "/" (BinOp Divide) Ex.AssocLeft
    ],
    [ binary "+" (BinOp Plus) Ex.AssocLeft
    , binary "-" (BinOp Minus) Ex.AssocLeft
    ]
    ]

-- Вспомогательная функция для создания бинарного оператора
binary :: String -> (Expr -> Expr -> Expr) -> Ex.Assoc -> Ex.Operator String () Identity Expr
binary name f assoc = Ex.Infix (reservedOp name >> return f) assoc

-- Обновляем parseExpr для поддержки операторов
parseExpr :: Parser Expr
parseExpr = Ex.buildExpressionParser operators parseTerm

parseTerm :: Parser Expr
parseTerm = choice [
    try parseFunction,
    try parseDefVar,
    try parseReturn,
    try parseNumber,
    try parseVariable,
    parens parseExpr
    ]

-- Главный парсер
parseProgram :: Parser [Expr]
parseProgram = debugParser "program" $ do
    spaces
    exprs <- many1 parseExpr
    eof
    return exprs
