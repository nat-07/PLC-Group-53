
-- ALEX FILE --
{ 
module Tokens where -- Preamble : Haskell code copied directly to the output
}

%wrapper "basic" -- Wrap declaration : (this just says to use a basic wrapper)

-- Macro Definitions --
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
$alphanum = [a-zA-Z0-9_]

@string = \"$alphanum+\"
@filename = $alphanum+".csv"
@column_index = COLUMN $digit+
-- alphabetic characters

tokens :- -- Delimiter begins the rules (the name is irrelevant)
-- PATTERN   { ACTION }--

  $white+       ; -- ignores whitespace
  "--".*        ; -- ignores '--' followed by any characters (for comments)
  "SELECT"         { \_ -> TokenSelect }
  "FROM"           { \_ -> TokenFrom }
  "WHERE"          { \_ -> TokenWhere }
  "GET"            { \_ -> TokenGet }
  "COLUMN"         { \_ -> TokenColumn }
  "OF"             { \_ -> TokenOf }
  "AND"            { \_ -> TokenAnd }
  "OR"             { \_ -> TokenOr }
  "DO"             { \_ -> TokenDo }
  "THEN"           { \_ -> TokenThen }
  "PRODUCT"        { \_ -> TokenProduct }
  "TIMES"          { \_ -> TokenTimes }
  "DROP"           { \_ -> TokenDrop }
  "PERMUTE"      { \_ -> TokenPermute }
  "COPY"           { \_ -> TokenCopy }
  "WITH"           { \_ -> TokenWith }
  "LEFT"     { \_ -> TokenLeft }
  "MERGE"     { \_ -> TokenMerge }
  "TO"             { \_ -> TokenTo }
  "IN"             { \_ -> TokenIn }
  "IS"             { \_ -> TokenIs}
  "NOT"              { \_ -> TokenNot}
  "EMPTY"              { \_ -> TokenEmpty}
  "RESULT"          { \_ -> TokenResult }
  "ON"              { \_ -> TokenOn }

  "!="             { \_ -> TokenNeq }
  \=               { \s -> TokenEq }
  \(               { \s -> TokenLParen }
  \)               { \s -> TokenRParen }
  \,               { \s -> TokenComma }
  $digit+          { \s -> TokenInt (read s) }
  $alpha($alphanum*) { \s -> TokenVar s }
  @column_index       { \s -> TokenColumnIndex (read (drop 7 s)) }
  @filename           { \s -> TokenFileName s }
  @string             { \s -> TokenString s }
  
  -- matches variables
  -- must start with a letter number of letters, digits, underscores (_), or apostrophes (')
  -- The entire matched string is passed as a TokenVar with the variable name.

{
data Token
  = TokenSelect
  | TokenFrom
  | TokenWhere
  | TokenGet
  | TokenColumn
  | TokenOf
  | TokenAnd
  | TokenOr
  | TokenDo
  | TokenThen
  | TokenProduct
  | TokenTimes
  | TokenDrop
  | TokenPermute
  | TokenCopy
  | TokenWith
  | TokenLeft
  | TokenMerge
  | TokenTo
  | TokenIn
  | TokenResult
  | TokenEq
  | TokenNeq
  | TokenLParen
  | TokenRParen
  | TokenIs
  | TokenNot
  | TokenEmpty
  | TokenOn
  | TokenVar String
  | TokenInt Int
  | TokenComma
  | TokenColumnIndex Int
  | TokenFileName String
  | TokenString String
  deriving (Show, Eq)
}
