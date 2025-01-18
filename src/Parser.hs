module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import AST
import Lexer
import qualified Text.Parsec.Token as Tok
-- Парсер для числа
parseNumber :: Parser Expr
parseNumber = do
    spaces  -- Добавляем пропуск пробелов
    num <- many1 digit
    spaces  -- Добавляем пропуск пробелов
    return $ Number (read num)

-- Парсер для переменной
parseVariable :: Parser Expr
parseVariable = Variable <$> many1 letter

-- Парсер для типа (например, i32, float)
parseType :: Parser Type
parseType = (reserved "i32" >> return (Primitive I32))
        <|> (reserved "u32" >> return (Primitive U32))
        <|> (reserved "i16" >> return (Primitive I16))
        <|> (reserved "u16" >> return (Primitive U16))
        <|> (reserved "double" >> return (Primitive DOUBLE))
        <|> (reserved "float" >> return (Primitive FLOAT))

-- Парсер для объявления переменной с типом
parseDefVar :: Parser Expr
parseDefVar = do
    name <- identifier
    _ <- reservedOp ":"
    typ <- parseType
    _ <- reservedOp "="
    expr <- parseExpr
    return $ DefVar name typ expr

-- Парсер для вектора
parseVect :: Parser Expr
parseVect = do
    _ <- string "vec<"
    size <- many1 digit
    _ <- string ", "
    typ <- many1 letter
    _ <- char '>'
    return $ VectAdd (Number (read size)) (Number (read typ))

-- Парсер для векторного сложения
parseVectAdd :: Parser Expr
parseVectAdd = do
    v1 <- parseExpr
    reservedOp "+"
    v2 <- parseExpr
    return $ VectAdd v1 v2

-- Парсер для условного оператора (if)
parseIf :: Parser Expr
parseIf = do
    reserved "if"
    cond <- parseExpr
    reserved "then"
    blockTrue <- many parseExpr
    reserved "else"
    blockFalse <- many parseExpr
    return $ If cond blockTrue blockFalse

-- Парсер для цикла (while)
parseWhile :: Parser Expr
parseWhile = do
    reserved "while"
    cond <- parseExpr
    block <- many parseExpr
    return $ While cond block

-- Парсер для функции
parseFunction :: Parser Expr
parseFunction = do
    reserved "func"
    name <- identifier
    params <- parens (commaSep parseVariable)
    body <- Tok.braces lexer (many parseExpr)
    return $ Function name (params ++ body)

-- Парсер для вызова функции
parseCall :: Parser Expr
parseCall = do
    funcName <- many1 letter
    spaces
    args <- between (char '(') (char ')') (parseExpr `sepBy` (char ',' >> spaces))
    return $ Call funcName args

-- Парсер для выражений
parseExpr :: Parser Expr
parseExpr = try parseNumber  -- Перемещаем parseNumber в начало
        <|> try parseFunction 
        <|> try parseCall 
        <|> try parseIf 
        <|> try parseWhile 
        <|> try parseVectAdd
        <|> try parseDefVar
        <|> parseVariable
