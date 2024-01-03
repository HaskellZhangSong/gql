{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module AST where
import Data.Char
import Data.Either (fromRight)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import System.IO.Unsafe
import Text.RawString.QQ
import Prettyprinter
import Data.Function
import Data.List
import qualified Data.Map as M

data Table = T_Null
           | T { unT :: String }
    deriving (Eq)

instance Show Table where
    show (T_Null) = "T_Null"
    show (T s) = s

instance Ord Table where
    (<=) T_Null _ = True
    (<=) (T s1) (T s2) = let i1 = read (dropWhile isAlpha s1) :: Int 
                             i2 = read (dropWhile isAlpha s2) :: Int
                         in i1 <= i2

newtype EdgeId = E { unE :: String }
    deriving (Eq)
instance Show EdgeId where
    show (E str) = str

instance Ord EdgeId where
    (<=) (E e1) (E e2) = let i1 = read (dropWhile isAlpha e1) :: Int 
                             i2 = read (dropWhile isAlpha e2) :: Int
                         in i1 <= i2

idHashPair :: String
idHashPair = [r|
typedef struct {
    uint32_t id;
    uint32_t hash;
} IdHashPair;
|]

vertexEdgesInfo :: String
vertexEdgesInfo = [r|
#define MAX_EDGE_SIZE 16
typedef struct {
    IdHashPair tableIdHashPair;
    uint32_t edgeCount;
    uint32_t edgeIds[MAX_EDGE_SIZE];
} VertexEdgesInfo;
|]

newtype PathId = P String
    deriving (Eq, Show)

type Edges = [EdgeId]

data GQL = GQL {pathId :: PathId,
                path :: Path}
        deriving (Eq, Show)

data Path = Null
          | Node Table
          | PathOr Path Path
          | PathAnd Path Path
          | Path Table Edges Path
        deriving (Eq, Show)

pathOrSize :: Path -> Int
pathOrSize (PathOr p1 p2) = 1 + pathOrSize p2
pathOrSize _ = 0

foreign import ccall "XXH32" xxh32 :: CString -> CSize -> CUInt -> IO CUInt

hash32 str = unsafePerformIO $ withCString str (\x -> xxh32 x (genericLength str) 10)

tableAdjacent :: Path -> Edges -> [(Table, Edges)]
tableAdjacent Null _ = []
tableAdjacent (Node t) [e] = [(t,[e])]
tableAdjacent (PathOr t@(Node _) p2) (e:es) = tableAdjacent t [e] ++ tableAdjacent p2 es
tableAdjacent (PathOr Null p2) (e:es) = tableAdjacent p2 es
tableAdjacent (PathOr a@(PathOr p1 p2) p3) es = error "impossible case, since | is right assoc"
tableAdjacent (PathOr a p3) (e:es) = tableAdjacent a [e] ++ tableAdjacent p3 es
tableAdjacent (PathAnd p1 p2) es = tableAdjacent p1 es ++ tableAdjacent p2 es
tableAdjacent (Path t es Null) es2 = [(t, es2)]
tableAdjacent (Path t es p) es2 = if isPathOr p 
                                    then 
                                        let ps = flattenOrPath p
                                            ets' = filter (\(e, t) -> t /= Null) (zip es ps)
                                            (es', por') = unzip ets'
                                            por = foldr1 PathOr por'
                                        in (t, nub $ es' ++ es2) : tableAdjacent por es'
                                    else 
                                        (t,nub $ es ++ es2) : tableAdjacent p es

isPathOr (PathOr _ _ ) = True
isPathOr _             = False

flattenOrPath :: Path -> [Path]
flattenOrPath (PathOr p1 p2) = p1 : flattenOrPath p2
flattenOrPath x = [x]

data IdHashPair = IdHashPair { 
                    id :: CUInt, 
                    hash :: CUInt 
                }

instance Pretty IdHashPair where
    pretty (IdHashPair a b) = braces (unsafeViaShow a <> 
                                      pretty ", " <>  
                                      unsafeViaShow b)

data TableInfo = TableInfo {
                    table     :: IdHashPair, 
                    edgeCount :: Int,
                    edges     :: [CUInt]
                }

instance Pretty TableInfo where
    pretty (TableInfo t ec es) = braces
                                 (pretty t <>
                                  pretty ", " <>
                                  unsafeViaShow ec <>
                                  pretty "," <>
                                  line <> (indent 4 $
                                    (braces (map unsafeViaShow es & concatWith (surround (pretty ","))))))

tableInfos :: (Table, Edges) -> TableInfo
tableInfos (T t, es) = TableInfo (IdHashPair (read (drop 1 t) :: CUInt) 
                                          (hash32 t))
                              (length es)
                              (map (\(E x) -> (read (drop 1 x) :: CUInt)) es)

pathToTableInfo :: Path -> [TableInfo]
pathToTableInfo = map tableInfos 
                    . map (\(x,y) -> (x, nub y))
                    . filter (\(x,y) -> not $ null y) 
                    . M.toList 
                    . M.fromListWith (++) 
                    . \p -> tableAdjacent p []

numOfEdges :: Path -> Int
numOfEdges Null = 0
numOfEdges (Node _) = 0
numOfEdges (PathOr p1 p2) = numOfEdges p1 + numOfEdges p2
numOfEdges (PathAnd p1 p2) = numOfEdges p1 + numOfEdges p2
numOfEdges (Path _ es p) = length es + numOfEdges p

getEdges :: Path -> Edges
getEdges Null = []
getEdges (Node _) = []
getEdges (PathOr p1 p2) = getEdges p1 ++ getEdges p2
getEdges (PathAnd p1 p2) = getEdges p1 ++ getEdges p2
getEdges (Path _ es p) = es ++ getEdges p

sample1 :: Path
sample1 = Path (T "T0") 
                [E "E0", E "E1" , E "E2"] 
                (PathAnd 
                    (PathAnd (Path (T "T1") 
                                   [E "E3", E "E4"]
                                   (PathOr (Node (T "T4"))
                                           (Node (T "T5"))))
                             (Node (T "T2")))
                    (Node (T "T3")))

sample2 = (Path (T "T1") 
                [E "E3", E "E4"]
                (PathOr (Node (T "T4"))
                        (Node (T "T5"))))

sample3 = (PathAnd (Path (T "T1")
                    [E "E3", E "E4"]
                    (PathOr (Node (T "T4"))
                            (Node (T "T5"))))
                   (Node (T "T2")))