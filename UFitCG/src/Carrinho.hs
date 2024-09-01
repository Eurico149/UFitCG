{-# LANGUAGE OverloadedStrings #-}

module Carrinho where

import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)

import Usuario
import Loja
import Distribution.Compat.Prelude (Double)

data Carrinho = Carrinho String Int
data ProdutosCarrinho = ProdutosCarrinho Int String Float String String deriving (Show)

instance FromRow ProdutosCarrinho where
    fromRow = ProdutosCarrinho <$> field <*> field <*> field <*> field <*> field


adicionaProdutoCarrinho :: String -> Int -> IO String
adicionaProdutoCarrinho usr_cli id_prod = do
    conn <- open "data/DataBase.db"
    verificaUsr <- verificaExistencia conn usr_cli
    verificaProduto <- verificaExistenciaProduto conn id_prod
    close conn
    if verificaUsr < 1 then return "Usuario Inexistente!"
    else if verificaProduto < 1 then return "Produto Inexistente!"
    else do
        conn <- open "data/DataBase.db"
        let produto = Carrinho usr_cli id_prod
        insertProdutoCarrinho conn produto
        close conn
        return "Produto Adicionado ao Carrinho!"

insertProdutoCarrinho :: Connection -> Carrinho -> IO ()
insertProdutoCarrinho conn (Carrinho usr_cli id_prod) = do
    let codigo = fromString "INSERT INTO carrinho(usr, id_prod) VALUES(?,?);"
    execute conn codigo (usr_cli, id_prod)
    putStrLn $ ""

deletarCarinho :: String -> IO()
deletarCarinho usr_cli = do
    conn <- open "data/DataBase.db"
    execute conn "DELETE FROM carrinho WHERE usr=?" (Only usr_cli)
    putStrLn "Todos os Produtos Foram Excluidos do Seu Carrinho"

deletarProdutoCarrinho :: String -> Int -> IO()
deletarProdutoCarrinho usr_cli id_prod = do
    conn <- open "data/DataBase.db"
    let codigo = fromString "DELETE FROM carrinho WHERE usr=? AND id_prod=?;"
    execute conn codigo (usr_cli, id_prod)
    putStrLn "Produto Excluido do Seu Carrinho!"

listarProdutosCarrinho :: String -> IO()
listarProdutosCarrinho usr_cli = do
    conn <- open "data/DataBase.db"

    prod <- query conn "SELECT l.id, l.nome, l.valor, l.descricao, l.categorias FROM carrinho as c, loja as j WHERE c.id_prod=l.id AND c.usr=?" (Only usr_cli) :: IO [ProdutosCarrinho]

    mapM_ printProdutos prod

    close conn

printProdutos :: ProdutosCarrinho -> IO()
printProdutos (ProdutosCarrinho id nome valor descricao categorias) = do
    putStrLn $ "Id: " ++ show id
    putStrLn $ "Nome do Produto: " ++ nome
    putStrLn $ "Preço: " ++ show valor
    putStrLn $ "Descricao: " ++ descricao
    putStrLn $ "Categoria: " ++ descricao
    putStrLn $ ""

valorCompra :: String -> IO Float
valorCompra usr_cli = do
    conn <- open "data/DataBase.db"
    [Only total] <- query conn "SELECT SUM(l.valor) FROM loja AS l, carrinho AS c WHERE c.id_prod = l.id AND c.usr=?" (Only usr_cli) :: IO [Only Float]
    close conn
    return total

finalizaCompra :: String -> IO String
finalizaCompra usr_cli = do
    conn <- open "data/DataBase.db"
    nomesProdutos <- query conn 
        "SELECT l.nome FROM loja AS l, carrinho AS c WHERE c.id_prod = l.id AND c.usr=?" (Only usr_cli) :: IO [Only String]
    close conn
    let todosProd = concatMap (\(Only nome) -> nome ++ ", ") nomesProdutos
    if null todosProd
        then return ""
        else return (init(init todosProd))

dataFinalizaCompra :: IO String
dataFinalizaCompra = do
    dataAtual <- getCurrentTime
    let dataString = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" dataAtual
    return dataString
