module Main where

import System.Environment
import Control.Monad.State
import Data.Map
import PPrint hiding (text, empty)
import Ast
import Lexer
import Parser
import Typechecker
import Type
import Lambda
import Effects

uncomment _ []              = []
uncomment flag ('-':'-':xs) = uncomment (not flag) xs
uncomment True (x:xs)       = uncomment True xs
uncomment False (x:xs)      = x : uncomment False xs

main :: IO ()
main =
  do (file:_) <- getArgs
     source <- readFile file
     let e = parseProg (lexe (uncomment False source))
         (e_,g) = translate 0 e
         (phs,s,t,e',g') = typecheck g (Effects.addDefs e_)
         e'' = toLambda e'
         e''' = Lambda.addDefs $ toLambda e'
     reduce infinity e''' g'
     return ()
