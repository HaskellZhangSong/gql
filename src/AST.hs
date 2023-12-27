{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}


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

tableInfo :: String
tableInfo = [r|
#define MAX_EDGE_SIZE 16
typedef struct {
    IdHashPair tableIdHashPair;
    uin32_t edgeCount;
    IdHashPair edgeIdHashPairs[MAX_EDGE_SIZE];
} TableInfo;
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

foreign import ccall "XXH32" xxh32 :: CString -> CSize -> CUInt -> IO CUInt

hash32 str = unsafePerformIO $ withCString str (\x -> xxh32 x (genericLength str) 10)

tableEdges :: Path -> [(Table, Edges)]
tableEdges Null = []
tableEdges (Node _) = []
tableEdges (PathOr p1 p2) = tableEdges p1 ++ tableEdges p2
tableEdges (PathAnd p1 p2) = tableEdges p1 ++ tableEdges p2
tableEdges (Path t es p) = (t,es) : tableEdges p

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
                    edges     :: [IdHashPair]
                }

instance Pretty TableInfo where
    pretty (TableInfo t ec es) = braces
                                 (pretty t <>
                                  pretty ", " <>
                                  unsafeViaShow ec <>
                                  pretty ", " <>
                                  line <> (indent 4 $
                                    (braces (map pretty es & concatWith (surround (pretty ", " <> line))))))

tableInfos :: (Table, Edges) -> TableInfo
tableInfos (T t, es) = let es' = es ++ replicate (8 - length es) (E "E0")
                        in TableInfo (IdHashPair (read (drop 1 t) :: CUInt) 
                                          (hash32 t))
                              (length es)
                              (map (\(E x) -> (IdHashPair (read (drop 1 x) :: CUInt) 
                                          (hash32 x))) es')

pathToTableInfo :: Path -> [TableInfo]
pathToTableInfo = map tableInfos . M.toList . M.fromListWith (++) . tableEdges

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