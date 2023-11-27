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
getPaths (PathOr p1 p2)  = getPaths p1 ++ getPaths p2
getPaths (PathAnd p1 p2) = [nub $ ((concat $ getPaths p1) ++ (concat $ getPaths p2))]
getPaths (Path t es p)   = [t : ps | ps <- getPaths p]

example1 :: String
example1 = unsafePerformIO $ readFile "./example/example2.gql"

printfIndexTable :: [(Table, Int)] -> IO ()
printfIndexTable [] = putStr "{}"
printfIndexTable xs = do 
                    putStr "{" 
                    go tables
                    where 
                        tables = map fst xs
                        go [] = putStr "}"
                        go [(T x)] = putStr (drop 1 x) >> putStr "}"
                        go (T t:tx) = putStr (drop 1 t) >> putStr ", " >> go tx

str2Paths :: String -> [[Table]]
str2Paths str = case parseGQL str of
                    Left x -> error $ show x
                    Right x -> nub $ getPaths (path x)

getAllTable :: String -> [(Table, Int)]
getAllTable str = case parseGQL str of
                    Left x -> error $ show x
                    Right x -> zip (nub $ concat $ getPaths (path x)) [0..]

encode :: [(Table, Int)] -> [Table] -> Word32
encode t [] = 0
encode t (p:ps) = case lookup p t of
                    Just i -> (1 `shiftL` i) .|. encode t ps

printEncoding :: [Word32] -> IO () 
printEncoding [] = putStr "{}"
printEncoding xs = do 
                putStr "{"
                go xs
                where go [] = putStr "}"
                      go [x] = printf "0b%b" x >> putStr "}"
                      go (w:ws) = printf "0b%b" w >> putStr ", " >> go ws


