module Main (main) where

import Database.SQLite.Simple
import Data.String (fromString)

import Loja
import Assinatura
import Usuario

main :: IO ()
main = do
    mensagem <- (cadastraUsuario "CM4545154" "12345678" "CLI" "Clara" "00/00/0000" "123" 0)
    putStrLn mensagem
    
