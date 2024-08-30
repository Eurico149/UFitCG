module Main (main) where

import Database.SQLite.Simple
import Data.String (fromString)

import Loja
import Assinatura
import Usuario 
import AvaliacaoFisica

main :: IO ()
main = do
    mensagem <- (removeAssinatura "gode")
    putStrLn mensagem
    
