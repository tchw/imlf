module Token where

import PPrint

data Token
  = TkLParen
  | TkRParen
  | TkLCurly
  | TkRCurly
  | TkLSquare
  | TkRSquare
  | TkAssign
  | TkColon
  | TkDoubleColon
  | TkSemicolon
  | TkComma
  | TkUnderscore
  | TkArrow
  | TkImply
  | TkVBar
  | TkId String
  | TkUpperId String
  | TkIf
  | TkThen
  | TkElse
  | TkLet
  | TkIn
  | TkData
  | TkCase
  | TkOf
  | TkAnd
  | TkOr
  | TkOver
  | TkInst
  | TkMain
  | TkInt Int
  deriving (Eq)

instance Show Token where
  show TkLParen = "("
  show TkRParen = ")"
  show TkLCurly = "{"
  show TkRCurly = "}"
  show TkLSquare = "]"
  show TkRSquare = "["
  show TkAssign = "="
  show TkColon = ":"
  show TkDoubleColon = "::"
  show TkSemicolon = ";"
  show TkComma = ","
  show TkUnderscore = "_"
  show TkArrow = "->"
  show TkImply = "=>"
  show TkVBar = "|"
  show (TkId name) = name
  show (TkUpperId name) = name
  show TkIf = "if"
  show TkThen = "then"
  show TkElse = "else"
  show TkLet = "let"
  show TkIn = "in"
  show TkData = "data"
  show TkCase = "case"
  show TkOf = "of"
  show TkAnd = "and"
  show TkOr = "or"
  show TkOver = "over"
  show TkInst = "inst"
  show TkMain = "main"
  show (TkInt x) = show x

instance PPrint Token where
  pprint x = text (show x)
