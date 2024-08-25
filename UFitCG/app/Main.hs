module Main (main) where

import Teste

main :: IO ()
main = do
    entrada <- getLine
    let num = read entrada :: Int
    print (funcao num)
