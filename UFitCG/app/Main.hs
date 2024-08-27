module Main (main) where

import Database.SQLite.Simple
import Data.String (fromString)

import Loja
import Assinatura

data Usuario = Usuario String String String String String String Float

cadastraUsuatio :: Connection -> Usuario -> IO ()
cadastraUsuatio conn (Usuario usr senha tipo_usr nome data_nascimento tipo_assinatura salario) = do
    let query = fromString "INSERT INTO usuario(usr, senha, tipo_usr, nome, data_nascimento, tipo_assinatura, salario) VALUES(?,?,?,?,?,?,?);"
    execute conn query (usr, senha, tipo_usr, nome, data_nascimento, tipo_assinatura, salario)
    putStrLn $ "Usuario: " ++ nome ++ " cadastrado com sucesso."

main :: IO ()
main = do
    mensagem <- (cadastraAssinatura "GOD" 150.9 850.0 1600.0 0 1 "")
    putStrLn mensagem
    
