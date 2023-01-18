{
module MinHs.Parser where
import MinHs.Lexer as LexHs
}

%name parser
%tokentype {Token}
%error {parseError}

%token 
      boolean              { LexHs.TokenBool $$ }
      var                  { LexHs.TokenVar $$ }
      digit                { LexHs.TokenDigit $$ }
      fun                  { LexHs.TokenFun $$ }
      '('                  { LexHs.TokenOB }
      ')'                  { LexHs.TokenCB }
      '-'		         { LexHs.TokenSub }
      '+'                  { LexHs.TokenAdd }
      '*'                  { LexHs.TokenMul }
      expror               { LexHs.TokenOr }           
      exprand              { LexHs.TokenAnd }
      exprlet              { LexHs.TokenLet }
      expreq	         { LexHs.TokenEq }
      exprgt               { LexHs.TokenGt }
      exprlt               { LexHs.TokenLt }
      exprgte              { LexHs.TokenGtE }
      exprlte              { LexHs.TokenLtE }
      exprif	         { LexHs.TokenIf }
      exprrecfun	         { LexHs.TokenRecFun }  
      exprfunc             { LexHs.TokenFunc }
      exprrecfun2          { LexHs.TokenRecFun2 }
      exprfunc2            { LexHs.TokenFunc2 }
      arrow                { LexHs.TokenTypeArrow }
      nat                  { LexHs.TokenTypeNat }
      bool                 { LexHs.TokenTypeBool }
      void                 { LexHs.TokenVoid }

%nonassoc arrow nat bool void
%right expror
%right exprand
%right expreq exprlt exprgt exprlte exprgte        
%left exprfunc exprfunc2
%left exprlet exprif exprrecfun exprrecfun2
%left '+' '-'
%left '*' 



%%
Expr   : boolean                                               { LexHs.ExprBool $1 }
      | var                                                    { LexHs.ExprVar $1 }
      | digit                                                  { LexHs.ExprDigit $1 }
      | fun                                                    { LexHs.ExprFun $1 }
      | void                                                   { LexHs.ExprVoid }
      | '(' Expr ')'                                           { $2 }
      | Expr '+' Expr                                          { LexHs.ExprAdd $1 $3 }
      | Expr '*' Expr                                          { LexHs.ExprMul $1 $3 }
      | Expr '-' Expr                                          { LexHs.ExprSub $1 $3 }
      | Expr expror Expr                                       { LexHs.ExprOr $1 $3 }
      | Expr exprand Expr                                      { LexHs.ExprAnd $1 $3} 
      | Expr expreq Expr                                       { LexHs.ExprEq $1 $3 }
      | Expr exprgt Expr                                       { LexHs.ExprGt $1 $3 }
      | Expr exprlt Expr                                       { LexHs.ExprLt $1 $3 }
      | Expr exprgte Expr                                      { LexHs.ExprGtE $1 $3 }
      | Expr exprlte Expr                                      { LexHs.ExprLtE $1 $3 }
      | exprif Expr Expr Expr                                  { LexHs.ExprIf $2 $3 $4 }
      | exprlet Expr Expr Expr                                 { LexHs.ExprLet $2 $3 $4 }
      | exprrecfun Expr Expr Expr Expr Expr                    { LexHs.ExprRecFun $2 $3 $4 $5 $6 }
      | exprfunc Expr Expr                                     { LexHs.ExprFunc $2 $3 }
      | exprrecfun2 Expr Expr Expr Expr Expr Expr Expr         { LexHs.ExprRecFun2 $2 $3 $4 $5 $6 $7 $8 }
      | exprfunc2 Expr Expr Expr                               { LexHs.ExprFunc2 $2 $3 $4 }
      | nat                                                    { LexHs.Nat }
      | bool                                                   { LexHs.Bool }
      | Expr arrow Expr                                        { LexHs.Arrow $1 $3 }



{
parseError :: [Token] -> a
parseError _ = error "Parse error"

mainInter = getContents >>= print . parser . lexer

data Type 
        = Nat
        | Boolean
        | Arrow Type Type
        deriving (Show, Eq)

}