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
import Prettyprinter
import Data.Function

hfile :: GQL -> String
hfile graph = let paths = gql2Paths graph
                  index_table = getAllTable graph
                  p = path graph
                  tis = pathToTableInfo p
                  ws = map (encode index_table) paths
                in unlines [
    "#pragma once",
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

table_info_hfile :: GQL -> String
table_info_hfile graph = let p = path graph
                             tis = pathToTableInfo p
                        in unlines
    ["#pragma once",
     "#ifndef VERTEX_EDGES_INFO_H",
     "#define VERTEX_EDGES_INFO_H",
     "#include <stdint.h>",
     printf "#define EDGE_IDS_SIZE %d" (numOfEdges p),
     "extern IdHashPair g_edge_id_hash_pairs[EDGE_IDS_SIZE];",

     idHashPair,
     vertexEdgesInfo,
     printf "#define VERTEX_EDGES_INFO_SIZE %d" (length tis),
     printf "extern VertexEdgesInfo g_vertex_edges_info[VERTEX_EDGES_INFO_SIZE];",
     "#endif"]

cfile :: GQL -> String
cfile graph = let paths = gql2Paths graph
                  p = path graph
                  tis = pathToTableInfo p
                  index_table = getAllTable graph
                  ws = map (encode index_table) paths
                in unlines [
    "#include <stdint.h>",
    "#include \"path_config.h\"",
    "uint32_t edgeIndexArr[EDGE_INDEX_LEN] = " ++ printEdges (getEdges (path graph)) ++ ";",
    "uint32_t tableIndexArr[TABLE_INDEX_LEN] = " ++ printIndexTable index_table ++ ";",
    printf "uint32_t pathCodes[PATH_CODES_LEN] = " ++ printEncoding ws ++ ";"
    ]

table_info_cfile :: GQL -> String
table_info_cfile graph = let paths = gql2Paths graph
                             p = path graph
                             tis = pathToTableInfo p
                         in unlines [
    "#include \"vertex_edges_info.h\"",
    "IdHashPair g_edge_id_hash_pairs[EDGE_IDS_SIZE] = " 
        ++ (show $ (getEdges p)
                 & map (\(E e) -> indent 4 (braces ((pretty (drop 1 e)) <> pretty "," <+> (unsafeViaShow (hash32 e)))))
                 & concatWith (surround (comma <> line))
                 & \x -> line <> x
                 & braces 
                 )  ++ ";",
    printf ("VertexEdgesInfo g_vertex_edges_info[VERTEX_EDGES_INFO_SIZE] = \n" ++ 
            show (indent 4 $ braces $ map pretty tis & concatWith (surround (line <+> pretty ","))) ++ ";") 
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
    writeFile "./path_config.h" (hfile graph)
    writeFile "./path_config.c" (cfile graph)
    writeFile "./vertex_edges_info.h" (table_info_hfile graph)
    writeFile "./vertex_edges_info.c" (table_info_cfile graph)
    let paths = gql2Paths graph
    when (length args > 0) $ pPrint $ parseGQL str
