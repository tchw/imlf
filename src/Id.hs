module Id where

import PPrint
import Token

data Id
  = Id String
  -- {constructor name}
  -- case translation unpack function 
  | ICase [Id]
  -- (->) type constructor
  | IFn
  -- {n,k}
  -- (n,k) fix operator
  | IFix Int Int
  -- {class name, instance types}
  -- overloaded function
  | IMth Id [Id]
  -- arithmetic operators
  | IAdd
  | ISub
  | IMul
  | IDiv
  | IEq
  -- overloaded ap function
  | IAp
  -- overloaded pack function
  | IPack
  -- P data type
  | IP
  -- E data type
  | IE
  -- P and E unpack functions
  | IUnpP
  | IUnpE
  -- translated data constructor
  | ITrans Id
  deriving (Eq, Ord)

instance Show Id where
  show (Id x) = x
  show (ICase is) = "case" ++ concat (map (\i -> "-" ++ show i) is)
  show IFn = show TkArrow
  show (IFix n k) = "fix-" ++ show n ++ "-" ++ show k
  show (IMth c ts) = show c ++ concat (map (\t -> "-" ++ show t) ts)
  show IAdd = "+"
  show ISub = "-"
  show IMul = "*"
  show IDiv = "/"
  show IEq = "=="  
  show IAp = "ap"
  show IPack = "pack"
  show IP = "P"
  show IE = "E"
  show IUnpP = "unp-P"
  show IUnpE = "unp-E"
  show (ITrans i) = "_" ++ show i

instance PPrint Id where
  pprint i = text (show i)
