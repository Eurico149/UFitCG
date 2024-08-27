module Main (main) where

import Database.SQLite.Simple
import Data.String (fromString)

data Produto = Produto String Float String String
data Usuario = Usuario String String String String String String Float

cadastroProduto :: Connection -> Produto -> IO ()
cadastroProduto conn (Produto nome valor descricao categorias) = do
    let query = fromString "INSERT INTO loja (nome, valor, descricao, categorias) VALUES (?, ?, ?, ?);"
    execute conn query (nome, valor, descricao, categorias)
    putStrLn "Produto inserido com sucesso!"

cadastraUsuatio :: Connection -> Usuario -> IO ()
cadastraUsuatio  conn (Usuario usr senha tipo_usr nome data_nascimento tipo_assinatura salario) = do
    let query = fromString "INSERT INTO usuario(usr, senha, tipo_usr, nome, data_nascimento, tipo_assinatura, salario) VALUES(?,?,?,?,?,?,?);"
    execute conn query (usr, senha, tipo_usr, nome, data_nascimento, tipo_assinatura, salario)
    putStrLn $ "Usuario: " ++ nome ++ " cadastrado com sucesso."

-- Função principal
main :: IO ()
main = do
    conn <- open "data/DataBase.db"
    let prod = Produto "Abacate" 12.50 "Fruta fresca" "Alimentos"
    cadastroProduto conn prod
    close conn
