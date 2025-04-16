{
    module Grammar where 

import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }

%token 
    SELECT { TokenSelect }
    WHERE { TokenWhere }
    DO { TokenDo }
    ',' { TokenComma }
    GET { TokenGet }
    COLUMN { TokenColumn }
    OF { TokenOf }
    '!=' { TokenNeq }
    IS { TokenIs }
    NOT { TokenNot }
    EMPTY { TokenEmpty }
    AND { TokenAnd }
    OR { TokenOr }
    THEN { TokenThen }
    PRODUCT { TokenProduct }
    TIMES { TokenTimes }
    DROP { TokenDrop }
    IN { TokenIn }
    PERMUTE { TokenPermute }
    COPY { TokenCopy }
    WITH { TokenWith }
    LEFT { TokenLeft }
    MERGE { TokenMerge }
    TO { TokenTo }
    ON { TokenOn }
    FILENAME { TokenFileName $$ }
    RESULT { TokenResult }
    int { TokenInt $$ } 
    var { TokenVar $$ } 
    '=' { TokenEq } 
    '(' { TokenLParen } 
    ')' { TokenRParen }

%nonassoc SELECT WHERE FROM DO 
%right IN
%left '+' '-' 
%left '*' '/' 
%left NEG 
%right '^'
%left OR
%left AND


%%

Query : SELECT '(' selectList ')' { SELECT $3 }
      | SELECT '(' selectList ')' WHERE '(' condition ')' { SELECTWHERE $3 $7 }
      | DO tasks { DO $2 }
      | DO tasks WHERE '(' condition ')' { DOWHERE $2 $5 }

condition : selectItem '=' string { CONDITIONEQSTRING $1 $3 }
          | selectItem '!=' string { CONDITIONNEQSTRING $1 $3 }
          | selectItem '=' selectItem { CONDITIONEQITEM $1 $3 }
          | selectItem '!=' selectItem { CONDITIONNEQITEM $1 $3 }
          | selectItem IS EMPTY { CONDITIONISEMPTY $1 }
          | selectItem IS NOT EMPTY { CONDITIONISNOTEMPTY $1 }
          | condition AND condition { AND $1 $3 }
          | condition OR condition { OR $1 $3 }

selectList : selectItem selectListRest { foldl ANDSELECT $1 $2 }

selectListRest : ',' selectItem selectListRest { $2 : $3 }
               |                 { [] }

selectItem : GET columnIndex OF csv { GET $2 $4 }


tasks : task { $1 }
      | task THEN tasks { THENTASK $1 $3 }

task : product { $1 }
     | permutation { $1 }
     | copy { $1 }
     | leftMerge { $1 }
     | drop { $1 }

product : PRODUCT '(' selectList ')' TIMES '(' selectList ')' { PRODUCT $3 $7 }

drop : DROP columnIndex IN csv { DROP $2 $4 }

permutation : PERMUTE '(' selectList ')' IN csv { PERMUTE $3 $6 }

copy : COPY csv WITH string { COPY $2 $4 }
     | COPY columnIndex WITH string IN csv { COPYIN $2 $4 $6 }

leftMerge : LEFT MERGE csv TO csv ON columnIndex { LEFTMERGEON $3 $5 $7 }
          | LEFT MERGE csv TO csv { LEFTMERGECSV $3 $5 }
          | LEFT MERGE '(' selectList ')' TO '(' selectList ')' ON columnIndex { LEFTMERGE $4 $8 $11 }

csv : filename { $1 }
    | RESULT OF '(' tasks ')' { RESULTOF $4 }

columnIndex : COLUMN int { COLUMN $2 }

string : '(' var ')' { STR $2 }

filename : FILENAME  { FILENAME $1 }

{ 
parseError :: [Token] -> a
parseError tokens = error $ "Parse error. Remaining tokens: " ++ show tokens

data Exp = Let String Exp Exp 
         | Plus Exp Exp 
         | Minus Exp Exp 
         | Times Exp Exp 
         | Div Exp Exp 
         | Negate Exp
         | Int Int 
         | Var String 
         | Expon Exp Exp
         deriving Show 

data Exp2 = SELECT Exp2 
          | SELECTWHERE Exp2 Exp2 
          | CONDITIONEQSTRING Exp2 Exp2
          | CONDITIONNEQSTRING Exp2 Exp2
          | CONDITIONNEQITEM Exp2 Exp2
          | CONDITIONEQITEM Exp2 Exp2
          | CONDITIONISEMPTY Exp2 
          | CONDITIONISNOTEMPTY Exp2
          | ANDSELECT Exp2 Exp2
          | THENTASK Exp2 Exp2
          | AND Exp2 Exp2
          | OR Exp2 Exp2
          | DO Exp2
          | GET Exp2 Exp2
          | DOWHERE Exp2 Exp2 
          | PRODUCT Exp2 Exp2 
          | DROP Exp2 Exp2
          | PERMUTE Exp2 Exp2 
          | COPY Exp2 Exp2
          | COPYIN Exp2 Exp2 Exp2 
          | LEFTMERGE Exp2 Exp2 Exp2
          | LEFTMERGEON Exp2 Exp2 Exp2 
          | LEFTMERGECSV Exp2 Exp2 
          | FILENAME String 
          | RESULTOF Exp2 
          | COLUMN Int 
          | STR String 
          | NAME String 
          deriving Show
} 