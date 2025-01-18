{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment (getArgs)
import Text.Parsec (parse)
import Parser (parseProgram)
import Foreign.C.String (withCString, peekCString)
import Codegen
import System.IO (writeFile)

main :: IO ()
main = do
    let inputFile = "input.txt"  -- Фиксированное имя входного файла
    putStrLn "=== Refal LLVM Compiler ==="
    putStrLn $ "Processing file: " ++ inputFile
    putStrLn "=========================="
    
    content <- readFile inputFile
    case parse parseProgram inputFile content of
        Left err -> do
            putStrLn "\n❌ Parsing Error:"
            print err
        Right ast -> do
            putStrLn "\n✅ Parsing successful!"
            
            -- Сохраняем AST в файл
            let astOutput = inputFile ++ ".ast"
            writeFile astOutput (show ast)
            putStrLn $ "AST saved to: " ++ astOutput
            
            putStrLn "\n🔨 Generating LLVM IR..."
            
            -- Генерация LLVM IR
            withCString "module" $ \moduleName -> do
                llvmModule <- llvmModuleCreateWithName moduleName
                addMainFunction llvmModule
                
                -- Сохранение в файл
                withCString (inputFile ++ ".ll") $ \outputFile -> do
                    llvmWriteBitcodeToFile llvmModule outputFile
                    putStrLn "✅ LLVM IR generation completed!"
                    putStrLn "\n✨ Compilation completed successfully!"
                    putStrLn "\nGenerated files:"
                    putStrLn $ "  📄 " ++ astOutput
                    putStrLn $ "  📄 " ++ inputFile ++ ".ll"
                    
                -- Освобождение памяти
                llvmDisposeModule llvmModule
