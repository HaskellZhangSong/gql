{
module Lexer where
import Token
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$nl = [\r\n\f]

@identifier  = \:[A-Za-z][0-9A-Za-z]*
@p_id  = [A-Za-z][0-9A-Za-z]*
@null = "NULL"

tokens :- 
    $white+ ;
    $nl+    ;
    CREATE      {tok (\p _ -> Tok CREATE p)}
    PATH        {tok (\p _ -> Tok PATH p)}
    MATCH       {tok (\p _ -> Tok MATCH p)}
    @null       {tok (\p _ -> Tok NULL p)}
    @p_id       {tok (\p s -> Tok (PId s) p)}
    \(          {tok (\p _ -> Tok LBrace p)}
    \)          {tok (\p _ -> Tok RBrace p)}
    \[          {tok (\p _ -> Tok LSquareBrace p)}
    \]          {tok (\p _ -> Tok RSquareBrace p)}
    @identifier {tok (\p s -> Tok (Ident (drop 1 s)) p)}
    \-          {tok (\p _ -> Tok Dash p)}
    \-\>        {tok (\p _ -> Tok Arrow p)}
    \|          {tok (\p _ -> Tok Bar p)}
    \,          {tok (\p _ -> Tok Comma p)}

{
data PosToken = Tok Token AlexPosn
    deriving (Show, Eq)

getIdentStr (Tok (Ident str) p) = str
getPidStr (Tok (PId str) p) = str

tok :: (AlexPosn -> String -> PosToken) -> AlexPosn -> String -> PosToken
tok f p s = f p s

token_pos (Tok _ p) = p

get_pos_line (AlexPn _ l _) = l
get_pos_col  (AlexPn c _ _) = c
get_col = get_pos_col . token_pos

token (Tok t p) = t

tokenize :: String -> [PosToken]
tokenize s = let tokens = alexScanTokens s
                in if null tokens
                    then []
                    else tokens ++ [Tok Eof (token_pos $ last tokens)]
}