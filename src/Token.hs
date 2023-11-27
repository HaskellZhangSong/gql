module Token where

data Token = CREATE
           | PATH
           | PId String
           | MATCH
           | LBrace
           | RBrace
           | LSquareBrace
           | RSquareBrace
           | Ident String
           | Dash
           | Arrow
           | NULL
           | Bar
           | Comma
           | Eof
           deriving (Eq, Show)

