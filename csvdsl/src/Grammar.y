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
    EMPTYCOL {TokenEmptyCol}
    EMPTY { TokenEmpty }
    READ { TokenRead }
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
    PRINT {TokenPrint}
    int { TokenInt $$ } 
    var { TokenVar $$ } 
    '=' { TokenEq } 
    '==' { TokenEqString } 
    '!==' { TokenNEqString } 
    '(' { TokenLParen } 
    ')' { TokenRParen }
    ';' {TokenSemiColon}

%nonassoc SELECT WHERE FROM DO 
%right IN
%left '+' '-' 
%left '*' '/' 
%left NEG 
%right '^'
%left OR
%left AND


%%

Queries : query ';' queriesRest { foldl ANDQUERY $1 $3}
        

queriesRest : query ';' queriesRest {$1 : $3}
            |                         { [] }

query : SELECT '(' selectList ')' TO filename { SELECT $3 $6} 
      | SELECT '(' selectList ')' WHERE '(' condition ')' TO filename { SELECTWHERE $3 $7 $10 }
      | DO task TO filename { DO $2 $4 }
      | DO task WHERE '(' condition ')' TO filename { DOWHERE $2 $5 $8 }
      | PRINT filename {PRINT $2}

condition : columnIndex '==' string { CONDITIONEQSTRING $1 $3 }
          | columnIndex '!==' string { CONDITIONNEQSTRING $1 $3 }
          | columnIndex '=' columnIndex {CONDITIONEQCOLUMN $1 $3}
          | columnIndex '!=' columnIndex { CONDITIONNEQCOLUMN $1 $3 }
          | columnIndex EMPTY { CONDITIONISEMPTY $1 }
          | columnIndex NOT EMPTY { CONDITIONISNOTEMPTY $1 }
          | condition AND condition { AND $1 $3 }
          | condition OR condition { OR $1 $3 }

selectList : selectItem selectListRest { foldl ANDSELECT $1 $2 }

selectListRest : ',' selectItem selectListRest { $2 : $3 }
               |                 { [] }

selectItem : GET columnIndex OF csv { GET $2 $4 }
           | READ csv {GETCSV $2}

task : product { $1 }
     | copy { $1 }
     | leftMerge { $1 }
     | drop { $1 }

product : PRODUCT '(' selectList ')' TIMES '(' selectList ')' { PRODUCT $3 $7 }

drop : DROP columnIndex IN '(' selectList ')' { DROP $2 $5 }

copy : COPY '(' selectList ')' WITH string { COPY $3 $6 }
     | COPY '(' selectList ')' WITH EMPTYCOL {COPYEMPTYCOL $3}

leftMerge : LEFT MERGE '(' selectList ')' TO '(' selectList ')' ON columnIndex { LEFTMERGE $4 $8 $11 }

csv : filename { $1 }

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

data Exp2 = SELECT Exp2 Exp2
          | SELECTWHERE Exp2 Exp2 Exp2
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
          | DO Exp2 Exp2
          | GET Exp2 Exp2
          | DOWHERE Exp2 Exp2 Exp2
          | PRODUCT Exp2 Exp2 
          | DROP Exp2 Exp2
          | PERMUTE Exp2 
          | COPY Exp2 Exp2
          | COPYEMPTYCOL Exp2 
          | LEFTMERGE Exp2 Exp2 Exp2
          | LEFTMERGEON Exp2 Exp2 Exp2 
          | LEFTMERGECSV Exp2 Exp2 
          | FILENAME String 
          | RESULTOF Exp2 
          | CONDITIONEQCOLUMN Exp2 Exp2
          | CONDITIONNEQCOLUMN Exp2 Exp2
          | COLUMN Int 
          | STR String 
          | NAME String 
          | PRINT Exp2
          | GETCSV Exp2
          | ANDQUERY Exp2 Exp2
          deriving Show
} 