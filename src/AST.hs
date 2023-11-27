module AST where

data Table = T_Null
           | T { unT :: String}
    deriving (Eq, Ord, Show)

newtype EdgeId = E String
    deriving (Eq, Show)
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





