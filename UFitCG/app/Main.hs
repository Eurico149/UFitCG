module Main (main) where

import Database.SQLite.Simple

import Loja
import Assinatura
import Usuario

main :: IO ()
main = do
    mensagem <- (cadastraVendaAssinatura "eurico" "GOD" "M" 0 "28/08/2004")
    putStrLn mensagem
    
