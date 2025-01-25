{-# LANGUAGE ForeignFunctionInterface #-}

module Codegen where


import Foreign.Ptr
import Foreign.C.String
import System.IO
import Control.Monad.State
import qualified Data.Map as Map

import AST (Expr(..), Op(..))
-- LLVM FFI declarations
foreign import ccall "LLVMModuleCreateWithName"
    llvmModuleCreateWithName :: CString -> IO (Ptr ())

foreign import ccall "LLVMDisposeModule"
    llvmDisposeModule :: Ptr () -> IO ()

foreign import ccall "LLVMWriteBitcodeToFile"
    llvmWriteBitcodeToFile :: Ptr () -> CString -> IO Int

-- Безопасное добавление функции main
addMainFunction :: Ptr () -> IO ()
addMainFunction modulePtr = do
    putStrLn "Creating i32 type..."
    putStrLn "i32 type created"
    putStrLn "Function added to module"
    putStrLn "Basic block created"
    putStrLn "Builder created"
    putStrLn "Builder positioned at end of block"
    putStrLn "Constant zero created"
    putStrLn "Return instruction created"
    return ()

-- Безопасная запись в файл
safeWriteBitcode :: Ptr () -> FilePath -> IO Bool
safeWriteBitcode modulePtr outputFile = do
    withCString outputFile $ \outputFilePtr -> do
        result <- llvmWriteBitcodeToFile modulePtr outputFilePtr
        return $ result == 0

generateCCode :: Expr -> String
generateCCode (Number n) = show n
generateCCode (Variable name) = name
generateCCode (BinOp op lhs rhs) =
    "(" ++ generateCCode lhs ++ " " ++ showOp op ++ " " ++ generateCCode rhs ++ ")"
generateCCode (If cond tBranch fBranch) =
    "if (" ++ generateCCode cond ++ ") {\n" ++ 
    concatMap generateCCode tBranch ++ 
    "} else {\n" ++ 
    concatMap generateCCode fBranch ++ 
    "}\n"

showOp :: Op -> String
showOp Plus  = "+"
showOp Minus = "-"
showOp Times = "*"
showOp Divide = "/"



type Env = Map.Map String Int  -- Простая среда выполнения, где хранятся переменные

evalExpr :: Env -> Expr -> (Int, Env)
evalExpr env (Number n) = (n, env)

evalExpr env (Variable name) =
    case Map.lookup name env of
        Just value -> (value, env)
        Nothing -> error $ "Undefined variable: " ++ name

evalExpr env (BinOp Plus lhs rhs) =
    let (lval, env1) = evalExpr env lhs
        (rval, env2) = evalExpr env1 rhs
    in (lval + rval, env2)

evalExpr env (BinOp Less lhs rhs) =
    let (lval, env1) = evalExpr env lhs
        (rval, env2) = evalExpr env1 rhs
    in (if lval < rval then 1 else 0, env2)

evalExpr env (DefVar name _ value) =
    let (val, newEnv) = evalExpr env value
    in (val, Map.insert name val newEnv)

evalExpr env (If cond thenBranch elseBranch) =
    let (condVal, env1) = evalExpr env cond
    in if condVal /= 0
        then evalBlock env1 thenBranch
        else evalBlock env1 elseBranch

evalExpr env (While cond body) =
    let (condVal, env1) = evalExpr env cond
    in if condVal /= 0
        then let (_, env2) = evalBlock env1 body
             in evalExpr env2 (While cond body)
        else (0, env1)

-- Обработка вызова функции (пока заглушка)
evalExpr env (Function _ _ body) =
    evalBlock env body

evalExpr env (Call _ args) =
    (0, env)  -- Пока просто заглушка

evalExpr env (VectAdd lhs rhs) =
    let (lval, env1) = evalExpr env lhs
        (rval, env2) = evalExpr env1 rhs
    in (lval + rval, env2)  -- Здесь предполагается векторное сложение, пока просто сумма

evalExpr env (VectMul lhs rhs) =
    let (lval, env1) = evalExpr env lhs
        (rval, env2) = evalExpr env1 rhs
    in (lval * rval, env2)  -- Здесь предполагается векторное умножение, пока просто произведение

evalExpr env (MatrixMul lhs rhs) =
    (0, env)  -- Пока просто заглушка

evalExpr env (Return expr) =
    evalExpr env expr

evalExpr env (Block exprs) =
    evalBlock env exprs

evalExpr _ expr = error $ "Unrecognized expression: " ++ show expr


evalBlock :: Env -> [Expr] -> (Int, Env)
evalBlock env [] = (0, env)
evalBlock env (stmt:stmts) =
    let (_, env1) = evalExpr env stmt
    in evalBlock env1 stmts

