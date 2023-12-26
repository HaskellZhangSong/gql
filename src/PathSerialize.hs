module PathSerialize where

import AST
import Parser
import System.IO.Unsafe
import Data.Either
import Data.List
import Data.Bits
import Data.Word
import Text.Printf (printf)

getPaths :: Path -> [[Table]]
getPaths Null            = [[]]
getPaths (Node t)        = [[t]]
getPaths (PathOr p1 p2)  = sort $ nub $ (getPaths p1 ++ getPaths p2)
getPaths (PathAnd p1 p2) = sort $ [nub (x ++ y) | x <- getPaths p1, y <- getPaths p2]
getPaths (Path t es p)   = sort [t : ps | ps <- getPaths p]

example1 :: String
example1 = unsafePerformIO $ readFile "./example/example2.gql"

printIndexTable :: [(Table, Int)] -> String
printIndexTable [] = "{}"
printIndexTable xs = "{" ++ go tables
                    where 
                        tables = map fst xs
                        go [] = "}"
                        go [(T x)] = (drop 1 x) ++ "}"
                        go (T t:tx) = (drop 1 t) ++  ", " ++ go tx

printEdges :: Edges -> String
printEdges [] = "{}"
printEdges es = "{" ++ go (sort es)
                    where 
                        go [] = "}"
                        go [(E x)] = (drop 1 x) ++ "}"
                        go (E t:tx) = (drop 1 t) ++ ", " ++ go tx

gql2Paths :: GQL -> [[Table]]
gql2Paths gq =  nub $ getPaths $ path gq

getAllTable :: GQL -> [(Table, Int)]
getAllTable gq =  zip (nub $ concat $ getPaths (path gq)) [0..]

encode :: [(Table, Int)] -> [Table] -> Word32
encode t [] = 0
encode t (p:ps) = case lookup p t of
                    Just i -> (1 `shiftL` i) .|. encode t ps

printEncoding :: [Word32] -> String
printEncoding [] = "{}"
printEncoding xs = "{\n" ++ go xs
                where go [] = "}"
                      go [x] = printf "    0b%b" x ++ "}"
                      go (w:ws) = printf "    0b%b" w ++ ",\n" ++ go ws


