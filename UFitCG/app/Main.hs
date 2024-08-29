module Main (main) where

import Database.SQLite.Simple
import Data.String (fromString)

import Loja
import Assinatura
import Usuario 

main :: IO ()
main = do
    mensagem <- (removeProduto 1)
    putStrLn mensagem
    
