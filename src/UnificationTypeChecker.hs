module Main where

import System.IO
import System.Environment
import System.Exit

import Language.Python.Common
import Language.Python.Version3

import qualified Language.Python.Common.AST as AST

import Unification

main :: IO()
main = do
    args <- getArgs
    case args of
        [path] -> readFile path >>= run path
        []     -> putStrLn "Reading from STDIN" >> getContents >>= run ""
        _      -> die "Usage:\nreading from file - ./main path_to_file\nreading from STDIN - ./main"

printNicely :: String -> String
printNicely = printWithSplits 0 where
    indent :: Int -> String
    indent n | n <= 0 = ""
             | otherwise = "|   " ++ indent (n - 4)

    printWithSplits :: Int -> String -> String
    printWithSplits _ [] = ""
    printWithSplits tabs (x : xs) =
        if x == '[' || x == '{' then
            "\n" ++ indent tabs ++ [x] ++ printWithSplits (tabs + 4) xs
        else if x == ']' || x == '}' then
            [x] ++ "\n" ++ indent (tabs - 8) ++ printWithSplits (tabs - 4) xs
        else
            [x] ++ printWithSplits tabs xs

run :: String -> String -> IO ()
run path input = do
    putStrLn path
    putStrLn input
    let file = ((take (length path - 3) path) ++ ".out")
    case parseModule input path of
        Left err -> do
            hPutStrLn stderr $ show err
            exitFailure
        Right (Module mod, tokens) -> do
            writeFile "./syntax.txt" $ printNicely $ show mod
            result <- typecheck mod
            case result of
                Left errT -> do
                    hPutStrLn stderr $ "ERROR\nType check error " ++ errT
                    exitFailure
                Right out   -> do
                    hPutStrLn stderr $ "Program typechecks successfully"
                    writeFile file out

typecheck :: [AST.StatementSpan] -> IO (Either String String)
typecheck stmts = do
    case runUnificationMonad (initContext 0 testEnv) (infer stmts) of
        Left err -> return $ Left err
        Right ((inf, _), ctx) -> return $ Right (show inf)