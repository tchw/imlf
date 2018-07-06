{
module Parser (parseExpr, parseProg) where

import Records
import Token
import Id
import Ast
import Type
}

%name parseExpr Expr
%name parseProg Prog
%tokentype { Token }
%error { parseError }

%token
'(' { TkLParen }
')' { TkRParen }
'{' { TkLCurly }
'}' { TkRCurly }
'[' { TkLSquare }
']' { TkRSquare }
'=' { TkAssign }
':' { TkColon }
'::' { TkDoubleColon }
';' { TkSemicolon }
',' { TkComma }
'_' { TkUnderscore }
'->' { TkArrow }
'=>' { TkImply }
'|' { TkVBar }
'+' { TkId "+" }
'-' { TkId "-" }
'*' { TkId "*" }
'/' { TkId "/" }
'<' { TkId "<" }
'<=' { TkId "<=" }
'==' { TkId "==" }
'>=' { TkId ">=" }
'>' { TkId ">" }
'and' { TkId "and" }
'or' { TkId "or" }
'not' { TkId "not" }
id { TkId $$ }
upperId { TkUpperId $$ }
'if' { TkIf }
'else' { TkElse }
'then' { TkThen }
'let' { TkLet }
'in' { TkIn }
'data' { TkData }
'case' { TkCase }
'of' { TkOf }
'over' { TkOver }
'inst' { TkInst }
'main' { TkMain }
int { TkInt $$ }

%%

List(X)
: { [] }
| X List(X) { $1:$2 }

List1(X)
: X List(X) { $1:$2 }

Snd(A,B)
: A B { $2 }

SepBy(S,X)
: X List(Snd(S,X)) { $1:$2 }

Id
: id { Id $1 }
| '+' { IAdd }
| '-' { ISub }
| '*' { IMul }
| '/' { IDiv }
| '==' { IEq }

UpperId
: upperId { Id $1 }

Prog
: 'main' '=' Expr { AProg { _overs = [], _insts = [], _binds = [], _datas = [], _main = $3 } }
| Over ';' Prog { over #overs ($1 :) $3 }
| Inst ';' Prog { over #insts ($1 :) $3 }
| Bind ';' Prog { over #binds ($1 :) $3 }
| Data ';' Prog { over #datas ($1 :) $3 }

Branch
: UpperId '->' Expr { ABranch $1 [] $3 }
| UpperId '(' SepBy(',',Id) ')' '->' Expr { ABranch $1 $3 $6 }

Expr
: '(' Expr ')' { $2 }
| int { AInt $1 }
| Id { AVar $1 }
| UpperId { AVar $1 }
| Expr '(' SepBy(',',Expr) ')' { AApp $1 $3 }
| '(' SepBy(',',Id) ')' '->' Expr { AFn $2 $5 }
| 'let' SepBy(';',Bind) 'in' Expr { ALet True $2 $4 }
| 'case' Expr 'of' SepBy('|',Branch) { ACase $2 $4 }

FunDep
: List1(Id) '->' Id { AFunDep $1 $3 }

FunDeps
: { [] }
| '|' List1(FunDep) { $2 }

Over
: 'over' Id List(Id) FunDeps '::' Type { AOver $2 $3 $4 $6 }

Pred
: Id List(Type) { TyPred $1 $2 }

Preds
: { [] }
| Pred '=>' { [$1] }
| '{' SepBy(',',Pred) '}' '=>' { $2 }

Inst
: 'inst' Preds Id List1(UpperId) '=' Expr { AInst $2 $3 $4 $6 }

Bind
: Id '=' Expr { ABind $1 $3 }

TypeFactor
: Id { TyVar $1 }
| UpperId { TyConst $1 }
| '(' Type ')' { $2 }

Type1
: TypeFactor { $1 }
| Id List1(TypeFactor) { mkTyApp (TyVar $1) $2 }
| UpperId List1(TypeFactor) { mkTyApp (TyConst $1) $2 }

Type
: Type1 { $1 }
| Type1 '->' Type { mkTyFn [$1] $3 }

Constr
: UpperId { AConstr $1 [] }
| UpperId '(' SepBy(',',Type) ')' { AConstr $1 $3 }

Data
: 'data' UpperId List(Id) '=' SepBy('|',Constr) { AData $2 $3 $5 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
