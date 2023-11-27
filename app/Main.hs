module Main (main) where
import System.Environment

import Lib
import AST
import PathSerialize
import Control.Monad
import Parser
import Text.Printf
import Text.Pretty.Simple (pPrint)
import System.Exit
main :: IO ()
main = do 
    ars <- getArgs
    when (null ars) $ do
        putStrLn "Usage: gql <graphfile>"
        exitWith $ ExitFailure 1
    let p : args = ars
    str <- readFile p
    let graph = case parseGQL str of
                    Left er -> error $ show er
                    Right g -> g
    let index_table = getAllTable graph
    putStrLn "#ifndef GQL_ENCODE_H"
    putStrLn "#define GQL_ENCODE_H"
    putStrLn "#include <stdint.h>"
    putStrLn ""
    printf "#define EDGE_INDEX_LEN %d\n" (numOfEdges (path graph))
    printf "uint32_t edgeIndexArr[EDGE_INDEX_LEN] = "
    printEdges (getEdges (path graph))
    putStr ";"
    putStrLn "\n"
    printf "#define TABLE_INDEX_LEN %d\n" (length index_table)
    printf "uint32_t tableIndexArr[TABLE_INDEX_LEN] = "
    printIndexTable index_table
    putStr ";"
    putStrLn "\n"
    let paths = str2Paths graph
    printf "#define PATH_CODES_LEN %d" (length paths)
    putStrLn ""
    printf "uint32_t pathCodes[PATH_CODES_LEN] = "
    let ws = map (encode index_table) paths
    printEncoding ws
    putStr ";"
    putStrLn ""
    putStrLn "#endif"
    forM_ ((map.map) unT paths) $ \p -> do
        putStr "// "
        print p
    when (length args > 0) $ pPrint $ parseGQL str
