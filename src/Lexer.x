{
module Lexer(lexe) where
import Token
}

%wrapper "basic"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$symbol = [\+\-\*\/\<\>\=]

tokens :-
$white+ ;
\( { \_ -> TkLParen }
\) { \_ -> TkRParen }
\{ { \_ -> TkLCurly }
\} { \_ -> TkRCurly }
\[ { \_ -> TkLSquare }
\] { \_ -> TkRSquare }
\= { \_ -> TkAssign }
\: { \_ -> TkColon }
\:: { \_ -> TkDoubleColon }
\; { \_ -> TkSemicolon }
\, { \_ -> TkComma }
\_ { \_ -> TkUnderscore }
\-\> { \_ -> TkArrow }
\=\> { \_ -> TkImply }
\| { \_ -> TkVBar }
if { \_ -> TkIf }
then { \_ -> TkThen }
else { \_ -> TkElse }
let { \_ -> TkLet }
in { \_ -> TkIn }
data { \_ -> TkData }
case { \_ -> TkCase }
of { \_ -> TkOf }
over { \_ -> TkOver }
inst { \_ -> TkInst }
main { \_ -> TkMain }
$upper [$lower $upper $digit \_]* { \s -> TkUpperId s }
$lower [$lower $upper $digit \_]* | $symbol+ { \s -> TkId s }
$digit+ { \s -> TkInt (read s) }

{
lexe = alexScanTokens
}
