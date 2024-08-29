{-# LANGUAGE OverloadedStrings #-}

module Loja (cadastroProduto, removeProduto) where

import Database.SQLite.Simple
import Data.String (fromString)

data Produto = Produto String Float String String

cadastroProduto :: String -> Float -> String -> String -> IO String
cadastroProduto nome valor descricao categorias = do
    if valor < 0 then return "Valor Negativo Invalido!"
    else if null categorias then return "Produto Precisa de Pelo Menos Uma Categoria!"
    else if null nome then return "Nome Invalido"
    else do 
        conn <- open "data/DataBase.db"
        let prod = Produto nome valor descricao categorias
        insertProduto conn prod
        close conn
        return "Produto Inserido!"

insertProduto :: Connection -> Produto -> IO ()
insertProduto conn (Produto nome valor descricao categorias) = do
    let query = fromString "INSERT INTO loja (nome, valor, descricao, categorias) VALUES (?, ?, ?, ?);"
    execute conn query (nome, valor, descricao, categorias)
    return ()

removeProduto :: Int -> IO String
removeProduto id = do
    conn <- open "data/DataBase.db"
    quant <- verificaExistencia conn id
    if quant >= 1 then do 
        deletarTupla conn id
        close conn
        return "Produto removido!" 
    else do 
        close conn
        return "Produto Inexistente!" 

deletarTupla :: Connection -> Int -> IO String
deletarTupla conn id = do
    execute conn "DELETE FROM loja WHERE id=?" (Only id)
    return ""

verificaExistencia :: Connection -> Int -> IO Int
verificaExistencia conn id = do
    [Only count] <- query conn "SELECT COUNT(*) FROM loja WHERE id=?" (Only id)
    return count