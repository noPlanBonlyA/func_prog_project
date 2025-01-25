module AST where

type Name = String

-- Основные типы выражений
data Expr
    = Variable Name        -- Переменная
    | Number Int           -- Число
    | Function Name [Expr] [Expr]  -- Имя, аргументы, тело функции
    | Call Name [Expr]     -- Вызов функции
    | VectAdd Expr Expr    -- Сложение двух векторов
    | VectMul Expr Expr    -- Умножение двух векторов (или скалярное произведение)
    | MatrixMul Expr Expr  -- Умножение матриц
    | If Expr [Expr] [Expr] -- Условный оператор
    | While Expr [Expr]     -- Цикл
    | DefVar Name Type Expr  -- Объявление переменной с типом и значением
    | Cast Type Expr        -- Приведение типа
    | IndexOp Expr Expr    -- Операция индексации
    | MemOp MemoryOp       -- Операции с памятью
    | Return Expr  -- Добавляем конструктор Return
    | BinOp Op Expr Expr
    | Block [Expr]  
    deriving (Show, Eq)

-- Типы данных
data Type = Primitive PrimitiveType 
          | Ptr Type 
          | VectType Int PrimitiveType 
          | MatrixType Int Int PrimitiveType  -- Для матриц
          deriving (Eq, Ord, Show)

data PrimitiveType = I32 | U32 | I16 | U16 | DOUBLE | FLOAT 
                   | VECTOR_I32 | VECTOR_FLOAT  -- Типы для векторных данных
                   deriving (Eq, Ord, Show)

-- Операции
data Op = Plus | Minus | Times | Divide | Assign | 
          Less | Greater | Equal | NotEqual | OR | 
          AND | VectAddOp | VectMulOp | MatrixMulOp  -- Операции для векторов и матриц
          deriving (Eq, Ord, Show)

-- Унарные операции
data UnaryOp = Increment | Decrement | UnMinus deriving (Eq, Ord, Show)

-- Операции с памятью
data MemoryOp = Load Type Expr Expr | Store Type Expr Expr deriving (Eq, Show)
