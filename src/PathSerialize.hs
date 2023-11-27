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

printIndexTable :: [(Table, Int)] -> IO ()
printIndexTable [] = putStr "{}"
printIndexTable xs = do 
                    putStr "{" 
                    go tables
                    where 
                        tables = map fst xs
                        go [] = putStr "}"
                        go [(T x)] = putStr (drop 1 x) >> putStr "}"
                        go (T t:tx) = putStr (drop 1 t) >> putStr ", " >> go tx

printEdges :: Edges -> IO ()
printEdges [] = putStr "{}"
printEdges es = do 
            putStr "{"
            go (sort es)
                    where 
                        go [] = putStr "}"
                        go [(E x)] = putStr (drop 1 x) >> putStr "}"
                        go (E t:tx) = putStr (drop 1 t) >> putStr ", " >> go tx

str2Paths :: GQL -> [[Table]]
str2Paths gq =  nub $ getPaths (path gq)

getAllTable :: GQL -> [(Table, Int)]
getAllTable gq =  zip (nub $ concat $ getPaths (path gq)) [0..]

encode :: [(Table, Int)] -> [Table] -> Word32
encode t [] = 0
encode t (p:ps) = case lookup p t of
                    Just i -> (1 `shiftL` i) .|. encode t ps

printEncoding :: [Word32] -> IO () 
printEncoding [] = putStr "{}"
printEncoding xs = do 
                putStrLn "{"
                go xs
                where go [] = putStr "}"
                      go [x] = printf "    0b%b" x >> putStr "}"
                      go (w:ws) = printf "    0b%b" w >> putStr ",\n" >> go ws


