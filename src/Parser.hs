module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import AST
import Lexer (lexer, parens, identifier, reserved, reservedOp, int', commaSep, braces, decimal', angles)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex
import Control.Monad.Identity
import Debug.Trace





-- Отладочный парсер
debugParser :: String -> Parser a -> Parser a
debugParser label p = do
    pos <- getPosition
    traceM $ "Attempting " ++ label ++ " at " ++ show pos
    result <- p
    traceM $ "Succeeded " ++ label
    return result

-- Парсер числа
parseNumber :: Parser Expr
parseNumber = debugParser "number" $ do
    spaces
    num <- many1 digit
    spaces
    return $ Number (read num)

-- Парсер переменной
parseVariable :: Parser Expr
parseVariable = debugParser "variable" $ Variable <$> identifier

-- Парсер типа
parseType :: Parser Type
parseType = debugParser "type" $ choice [
    (reserved "i32" >> return (Primitive I32)),
    (reserved "u32" >> return (Primitive U32)),
    (reserved "i16" >> return (Primitive I16)),
    (reserved "u16" >> return (Primitive U16)),
    (reserved "float" >> return (Primitive FLOAT)),
    (reserved "double" >> return (Primitive DOUBLE))
    ]

-- Парсер объявления переменной
parseDefVar :: Parser Expr
parseDefVar = debugParser "variable definition" $ do
    name <- identifier
    reservedOp ":"
    typ <- parseType
    reservedOp "="
    value <- parseExpr
    return $ DefVar name typ value


-- Парсер условного оператора
parseIf :: Parser Expr
parseIf = do
    reserved "if"
    cond <- parens parseExpr <|> parseExpr -- Позволяем отсутствие скобок
    trueBranch <- braces (many parseExpr)
    reserved "else"
    falseBranch <- braces (many parseExpr)
    return $ If cond trueBranch falseBranch



-- Парсер цикла
parseWhile :: Parser Expr
parseWhile = debugParser "while" $ do
    reserved "while"
    cond <- parens parseExpr <|> parseExpr  -- Позволяем условия без скобок
    body <- braces (many parseExpr)
    return $ While cond body


-- Парсер функции
parseFunction :: Parser Expr
parseFunction = debugParser "function" $ do
    reserved "func"
    name <- identifier
    args <- parens (commaSep identifier)
    body <- braces (many parseExpr)
    return $ Function name (map Variable args) body

-- Таблица операторов

operators :: [[Ex.Operator String () Identity Expr]]
operators =
    [ [ binary "*" (BinOp Times) Ex.AssocLeft
      , binary "/" (BinOp Divide) Ex.AssocLeft
      ]
    , [ binary "+" (BinOp Plus) Ex.AssocLeft
      , binary "-" (BinOp Minus) Ex.AssocLeft
      ]
    , [ binary "<" (BinOp Less) Ex.AssocLeft
      , binary ">" (BinOp Greater) Ex.AssocLeft
      , binary "==" (BinOp Equal) Ex.AssocLeft
      ]
    , [ binary "=" (BinOp Assign) Ex.AssocRight ]  -- Добавляем присваивание

    ]

-- Вспомогательная функция для создания бинарного оператора
binary :: String -> (Expr -> Expr -> Expr) -> Ex.Assoc -> Ex.Operator String () Identity Expr
binary name f assoc = Ex.Infix (reservedOp name >> return f) assoc

-- Парсер выражений
parseExpr :: Parser Expr
parseExpr = Ex.buildExpressionParser operators parseTerm

parseAssignment :: Parser Expr
parseAssignment = debugParser "assignment" $ do
    varName <- identifier
    reservedOp "="
    expr <- parseExpr
    return $ BinOp Assign (Variable varName) expr

parseTerm :: Parser Expr
parseTerm = choice [

    try parseDefVar,
    try parseAssignment, 
    try parseIf,      -- Добавлено разбор if
    try parseWhile,
    try parseFunction,
    try parseReturn,
    try parseNumber,
    try parseVariable,
    parens parseExpr
    
    ]

-- Главный парсер программы
parseProgram :: Parser [Expr]
parseProgram = debugParser "program" $ do
    spaces
    exprs <- many1 parseExpr
    eof
    return exprs

parseReturn :: Parser Expr
parseReturn = do
    reserved "return"
    expr <- parseExpr
    return $ Return expr