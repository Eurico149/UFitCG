module Main (main) where

import Database.SQLite.Simple
import Data.String (fromString)

import Loja
import Assinatura
import Usuario 
import AvaliacaoFisica
import Login
import Navegabilidade

main :: IO ()
main = do
    abaLogin
    
    
