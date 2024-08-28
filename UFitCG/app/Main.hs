module Main (main) where

import Database.SQLite.Simple
import Data.String (fromString)

import Loja
import Assinatura
import Usuario

main :: IO ()
main = do
    mensagem <- (cadastraUsuario "ClaraM" "12345678" "ADM" "Clara" "00/00/0000" "   " 1)
    putStrLn mensagem
    
