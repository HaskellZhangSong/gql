module Main (main) where
import System.Environment

import Lib
import AST
import PathSerialize
import Control.Monad
import Parser
import Text.Printf
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do 
    p : args <- getArgs
    str <- readFile p
    let index_table = getAllTable str
    putStrLn "#ifndef GQL_ENCODE_H"
    putStrLn "#define GQL_ENCODE_H"
    printf "uint32_t indexTable[%d] = " (length index_table) 
    printfIndexTable index_table
    putStr ";"
    putStrLn ""
    let paths = str2Paths str
    printf "uint32_t pathCodes[%d] = " (length paths)
    let ws = map (encode index_table) paths
    printEncoding ws
    putStr ";"
    putStrLn ""
    putStrLn "#endif"
    forM_ ((map.map) unT paths) $ \p -> do
        putStr "// "
        print p
    when (length args > 0) $ pPrint $ parseGQL str
