{-# LANGUAGE OverloadedStrings #-}

module Loja (listarProdutosCategorias, cadastroProduto, removeProduto, listarProdutos, verificaExistenciaProduto, listarProdutosCategorias) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.String (fromString)

data Produto = Produto String Float String String
data ProdutoMostrar = ProdutoMostrar Int String Float String String deriving (Show)

instance FromRow ProdutoMostrar where
    fromRow = ProdutoMostrar <$> field <*> field <*> field <*> field <*> field

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
    quant <- verificaExistenciaProduto conn id
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

verificaExistenciaProduto :: Connection -> Int -> IO Int
verificaExistenciaProduto conn id = do
    [Only count] <- query conn "SELECT COUNT(*) FROM loja WHERE id=?" (Only id)
    return count

listarProdutos :: IO()
listarProdutos = do 
    conn <- open "data/DataBase.db"

    produtos <- query_ conn "SELECT * FROM loja" :: IO [ProdutoMostrar]

    mapM_ printProdutos produtos

    close conn

listarProdutosCategorias :: String -> IO()
listarProdutosCategorias categoria = do 
    conn <- open "data/DataBase.db"

    let likePattern = "%" ++ categoria ++ "%"
    produtos <- query conn "SELECT * FROM loja WHERE categorias LIKE ?" (Only likePattern) :: IO [ProdutoMostrar]

    mapM_ printProdutos produtos

    close conn

printProdutos :: ProdutoMostrar -> IO()
printProdutos(ProdutoMostrar id nome valor descricao categorias) = do
    putStrLn $ "Id: " ++ show id
    putStrLn $ "Nome: " ++ nome
    putStrLn $ "Valor: " ++ show valor
    putStrLn $ "Descricao: " ++ descricao
    putStrLn $ "Categoria: " ++ categorias
    putStrLn $ ""