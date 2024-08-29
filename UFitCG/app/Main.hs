module Main (main) where

import Database.SQLite.Simple
import Data.String (fromString)

import Loja
import Assinatura
import Usuario

main :: IO ()
main = do
    mensagem <- (cadastraVendaAssinatura "eurico" "LEP" "A" 0 "29/08/2004")
    putStrLn mensagem
    
