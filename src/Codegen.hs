{-# LANGUAGE ForeignFunctionInterface #-}

module Codegen where

import Foreign.C.String
import Foreign.Ptr
import Foreign.C.Types (CUInt(..), CLLong(..), CChar)
import AST -- Добавляем импорт модуля AST

-- Функция для создания LLVM модуля
foreign import ccall "LLVMModuleCreateWithName"
    llvmModuleCreateWithName :: CString -> IO (Ptr ())

-- Функция для добавления функции в модуль
foreign import ccall "LLVMAddFunction"
    llvmAddFunction :: Ptr () -> CString -> Ptr () -> IO (Ptr ())

-- Функция для создания целого типа (i32)
foreign import ccall "LLVMInt32Type"
    llvmInt32Type :: IO (Ptr ())

-- Функция для добавления базового блока
foreign import ccall "LLVMAppendBasicBlock"
    llvmAppendBasicBlock :: Ptr () -> CString -> IO (Ptr ())

-- Функция для создания инструкции возврата
foreign import ccall "LLVMBuildRet"
    llvmBuildRet :: Ptr () -> Ptr () -> IO ()

-- Функция для создания константы (целое число)
foreign import ccall "LLVMConstInt"
    llvmConstInt :: Ptr () -> CLLong -> CUInt -> IO (Ptr ())

-- Функция для создания билдера
foreign import ccall "LLVMCreateBuilder"
    llvmCreateBuilder :: IO (Ptr ())

-- Функция для привязки билдера к базовому блоку
foreign import ccall "LLVMPositionBuilderAtEnd"
    llvmPositionBuilderAtEnd :: Ptr () -> Ptr () -> IO ()

-- Функция для записи модуля в файл биткода
foreign import ccall "LLVMWriteBitcodeToFile"
    llvmWriteBitcodeToFile :: Ptr () -> CString -> IO ()

-- Функция для печати модуля в строку
foreign import ccall "LLVMPrintModuleToString"
    llvmPrintModuleToString :: Ptr () -> IO (Ptr CChar)

-- Функция для создания векторного типа
foreign import ccall "LLVMVectorType"
    llvmVectorType :: Ptr () -> CUInt -> IO (Ptr ())

-- Добавляем импорт функции для освобождения памяти модуля
foreign import ccall "LLVMDisposeModule"
    llvmDisposeModule :: Ptr () -> IO ()

-- Операция сложения двух векторов
vectAdd :: Ptr () -> Ptr () -> IO (Ptr ())
vectAdd _ _ = do
    putStrLn "Performing vector addition..."
    return nullPtr

-- Добавляем функцию с возвратом i32
addMainFunction :: Ptr () -> IO ()
addMainFunction llvmModule = do
    putStrLn "Creating i32 type..."
    i32 <- llvmInt32Type
    if i32 == nullPtr
        then error "Failed to create i32 type"
        else putStrLn "i32 type created"

    withCString "main" $ \name -> do
        func <- llvmAddFunction llvmModule name i32
        if func == nullPtr
            then error "Failed to add function to module"
            else putStrLn "Function added to module"

        withCString "entry" $ \blockName -> do
            block <- llvmAppendBasicBlock func blockName
            if block == nullPtr
                then error "Failed to create basic block"
                else putStrLn "Basic block created"

            builder <- llvmCreateBuilder
            if builder == nullPtr
                then error "Failed to create builder"
                else putStrLn "Builder created"

            llvmPositionBuilderAtEnd builder block
            putStrLn "Builder positioned at end of block"

            zero <- llvmConstInt i32 (CLLong 0) (CUInt 0)
            if zero == nullPtr
                then error "Failed to create constant zero"
                else putStrLn "Constant zero created"

            llvmBuildRet builder zero
            putStrLn "Return instruction created"
