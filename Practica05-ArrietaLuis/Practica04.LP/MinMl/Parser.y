{
module MinMl.Parser where
import MinMl.Lexer as LexMl
}

%name parser
%tokentype {Token}
%error {parseError}

%token 
    boolean              { LexMl.TokenBool $$ }
    var                  { LexMl.TokenVar $$ }
    digit                { LexMl.TokenDigit $$ }
    fun                  { LexMl.TokenFun $$ }
    '('                  { LexMl.TokenOB }
    ')'                  { LexMl.TokenCB }
    '-'		             { LexMl.TokenSub }
    '+'                  { LexMl.TokenAdd }
    '*'                  { LexMl.TokenMul }
    minmlor              { LexMl.TokenOr }           
    minmland             { LexMl.TokenAnd }
    minmllet             { LexMl.TokenLet }
    minmleq	             { LexMl.TokenEq }
    minmlgt      	     { LexMl.TokenGt }
    minmllt              { LexMl.TokenLt }
    minmlgte 		     { LexMl.TokenGtE }
    minmllte             { LexMl.TokenLtE }
    minmlif		         { LexMl.TokenIf }
    minmlletrec	         { LexMl.TokenLetRec }  
    minmlfunc            { LexMl.TokenFunc }


%right minmlor
%right minmland
%right minmleq minmllt minmlgt minmllte minmlgte        
%left minmllet minmlif 
%left minmlfunc
%left minmlletrec
%left '+' '-'
%left '*' 

%%
MinMl   : boolean                             { LexMl.MinMlBool $1 }
      | var                                   { LexMl.MinMlVar $1 }
      | digit                                 { LexMl.MinMlDigit $1 }
      | fun                                   { LexMl.MinMlFun $1 }
      | '(' MinMl ')'                         { $2 }
      | MinMl '+' MinMl                       { LexMl.MinMlAdd $1 $3 }
      | MinMl '*' MinMl                       { LexMl.MinMlMul $1 $3 }
      | MinMl '-' MinMl                       { LexMl.MinMlSub $1 $3 }
      | MinMl minmlor MinMl                   { LexMl.MinMlOr $1 $3 }
      | MinMl minmland MinMl                  { LexMl.MinMlAnd $1 $3} 
      | MinMl minmleq MinMl                   { LexMl.MinMlEq $1 $3 }
      | MinMl minmlgt MinMl                   { LexMl.MinMlGt $1 $3 }
      | MinMl minmllt MinMl                   { LexMl.MinMlLt $1 $3 }
      | MinMl minmlgte MinMl                  { LexMl.MinMlGtE $1 $3 }
      | MinMl minmllte MinMl                  { LexMl.MinMlLtE $1 $3 }
      | minmlif MinMl MinMl MinMl             { LexMl.MinMlIf $2 $3 $4 }
      | minmllet MinMl MinMl MinMl            { LexMl.MinMlLet $2 $3 $4 }
      | minmlletrec MinMl MinMl               { LexMl.MinMlLetRec $2 $3 }
      | minmlfunc MinMl MinMl                 { LexMl.MinMlFunc $2 $3 }




{
parseError :: [Token] -> a
parseError _ = error "Parse error"

mainInter = getContents >>= print . parser . lexer

}