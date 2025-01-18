{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc (free)
import Codegen
import Parser
import AST
import Text.Parsec
import Text.Parsec.String (parseFromFile)
import qualified Text.Parsec.Token as Tok
import System.IO (writeFile)
import Control.Exception (SomeException, catch)

-- Функция для форматированного вывода LLVM IR
formatLLVMOutput :: String -> String
formatLLVMOutput str = unlines [
    "=== Generated LLVM IR ===",
    str,
    "======================="
    ]

-- Функция для вывода AST
formatAST :: Show a => a -> String
formatAST ast = unlines [
    "=== Generated AST ===",
    show ast,
    "===================="
    ]

-- Функция для сохранения результата в файл
saveToFile :: String -> String -> IO ()
saveToFile filename content = do
    writeFile filename content
    putStrLn $ "Results saved to " ++ filename

main :: IO ()
main = do
    let moduleName = "example"
    let outputFile = "output.ll"
    let astFile = "ast_output.txt"
    let inputFile = "input.txt"
    
    putStrLn $ "Attempting to read from " ++ inputFile ++ "..."
    
    parseResult <- parseFromFile parseExpr inputFile
    case parseResult of
        Left err -> do
            putStrLn "=== Parsing Error ==="
            print err
            putStrLn "===================="
            
        Right ast -> do
            putStrLn $ formatAST ast
            saveToFile astFile (formatAST ast)

            withCString moduleName $ \modName -> do
                llvmModule <- llvmModuleCreateWithName modName
                if llvmModule == nullPtr
                    then error "Failed to create LLVM module"
                    else do
                        putStrLn "Created module successfully"
                        
                        -- Добавляем функцию main
                        putStrLn "Adding main function..."
                        addMainFunction llvmModule
                        putStrLn "Main function added successfully"

                        -- Печать модуля в строку для отладки
                        putStrLn "Generating LLVM IR..."
                        moduleStr <- llvmPrintModuleToString llvmModule
                        if moduleStr == nullPtr
                            then do
                                llvmDisposeModule llvmModule
                                error "Failed to print module to string"
                            else do
                                str <- peekCString moduleStr
                                putStrLn $ formatLLVMOutput str
                                saveToFile outputFile str
                                free moduleStr
                                
                                -- Сохранение модуля в биткод
                                putStrLn "Saving bitcode..."
                                result <- withCString "output.bc" $ \file -> 
                                    llvmWriteBitcodeToFile llvmModule file
                                
                                -- Освобождаем модуль после всех операций
                                llvmDisposeModule llvmModule

                                if result /= 0
                                    then putStrLn "Warning: Failed to save bitcode"
                                    else do
                                        putStrLn "Bitcode saved successfully"
                                        putStrLn "\nCompilation completed successfully!"
                                        putStrLn $ unlines [
                                            "Generated files:",
                                            "- AST: " ++ astFile,
                                            "- LLVM IR: " ++ outputFile,
                                            "- Bitcode: output.bc"
                                            ]

-- Вспомогательная функция для проверки успешности операции
checkSuccess :: String -> Bool -> IO ()
checkSuccess message success = 
    if success 
        then putStrLn $ "Success: " ++ message
        else putStrLn $ "Error: " ++ message
