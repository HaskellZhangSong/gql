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

hfile :: GQL -> String
hfile graph = let paths = str2Paths graph
                  index_table = getAllTable graph
                  ws = map (encode index_table) paths
                in unlines [
    "#ifndef GQL_ENCODE_H",
    "#define GQL_ENCODE_H",
    "#include <stdint.h>",
    printf "#define EDGE_INDEX_LEN %d" (numOfEdges (path graph)),
    "extern uint32_t edgeIndexArr[EDGE_INDEX_LEN];",
    printf "#define TABLE_INDEX_LEN %d" (length index_table),
    "extern uint32_t tableIndexArr[TABLE_INDEX_LEN] ;",
    printf "#define PATH_CODES_LEN %d" (length paths) , 
    "extern uint32_t pathCodes[PATH_CODES_LEN];",
    "#endif"
    ]

cfile :: GQL -> String
cfile graph = let paths = str2Paths graph
                  index_table = getAllTable graph
                  ws = map (encode index_table) paths
                in unlines [
    "#include <stdint.h>",
    printf "#define EDGE_INDEX_LEN %d" (numOfEdges (path graph)),
    "uint32_t edgeIndexArr[EDGE_INDEX_LEN] = " ++ printEdges (getEdges (path graph)) ++ ";",
    printf "#define TABLE_INDEX_LEN %d" (length index_table),
    "uint32_t tableIndexArr[TABLE_INDEX_LEN] = " ++ printIndexTable index_table ++ ";",
    printf "#define PATH_CODES_LEN %d" (length paths), 
    printf "uint32_t pathCodes[PATH_CODES_LEN] = " ++ printEncoding ws ++ ";"
    ]

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
    putStrLn "// header file"
    putStr $ hfile graph
    putStrLn "// C file"
    putStr $ cfile graph
    let index_table = getAllTable graph
    let paths = str2Paths graph
    forM_ ((map.map) unT paths) $ \p -> do
        putStr "// "
        print p
    when (length args > 0) $ pPrint $ parseGQL str
