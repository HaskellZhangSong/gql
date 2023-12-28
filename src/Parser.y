{
module Parser where
import Lexer
import AST
import Token
import Text.Parsec.String
import Text.Parsec
}


%monad { GenParser PosToken () } { >>= } { return }
%lexer { scanner } {Tok Eof _ }
%tokentype { PosToken }


%name gql
%name p

%token
    create      { Tok CREATE p }
    path        { Tok PATH p }
    match       { Tok MATCH p }
    null        { Tok NULL p }
    p_id        { Tok (PId $$) p }
    '('         { Tok LBrace p }
    ')'         { Tok RBrace p }
    '['         { Tok LSquareBrace p }
    ']'         { Tok RSquareBrace p }
    ident       { Tok (Ident $$) p }
    '-'         { Tok Dash p }
    '->'        { Tok Arrow p }
    '|'         { Tok Bar p }
    ','         { Tok Comma p }

%right '|'
%right ','
%left '->'

%%

gql :: { GQL }
gql : create path p_id '(' match p ')'
    { GQL (P $3) $6 }

p :: { Path }
p : 
    '(' ident ')' '-' '[' edges ']' '->' p
            { Path (T $2) $6 $9 }
  | '(' ident ')' '-' '[' edges ']' '->' '(' p ')'
            { Path (T $2) $6 $10 }
  | p ',' p  { PathAnd $1 $3 }
  | p '|' p  { PathOr  $1 $3 }
  | ident { Node (T $1) }
  | '(' null ')' { Null }
  | '(' ident ')' { Node (T $2) }


edges :: { Edges }
edges : ident { [E $1] }
      | ident '|' edges { (E $1) : $3 }

{
happyError :: GenParser PosToken () a
happyError = do
    t <- getInput
    parserTrace "trace"
    fail $ show t

scanner :: (PosToken -> GenParser PosToken () a) -> GenParser PosToken () a
scanner cont = do
    t <- getInput
    let fst_tk@(Tok _ tp) = head t
    setInput (tail t)
    pos <- getPosition
    let pos' = setSourceLine pos (get_pos_line tp)
    let sourceCol = setSourceColumn pos' (get_pos_col tp)
    setPosition sourceCol
    cont $ fst_tk

parseGQL str = parse gql "" (tokenize str)
parsePath str = parse p "" (tokenize str)
}