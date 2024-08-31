{-# LANGUAGE OverloadedStrings #-}

module Carrinho where

import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow

import Usuario
import Loja

data Carrinho = Carrinho String String 
data ProdutosCarrinho = ProdutosCarrinho Int String Float String String deriving (Show)

instance FromRow ProdutosCarrinho where
    fromRow = ProdutosCarrinho <$> field <*> field <*> field <*> field <*> field


adicionaProdutoCarrinho :: String -> String -> IO String
adicionaProdutoCarrinho usr_cli id_prod = do
    conn <- open "data/DataBase.db"
    verificaUsr <- verificaExistencia usr_cli
    verificaProduto <- verificaExistenciaProduto id_prod
    close conn

    if verificaUsr < 1 then return "Usuario Inexistente!"
    else if verificaExistenciaProduto then return "Produto Inexistente!"
    else do 
        conn <- open "data/DataBase.db"
        let produto = Carrinho usr_cli id_prod
        insertProdutoCarrinho conn produto
        close conn 
        return "Produto Adicionado ao Carrinho!"

insertProdutoCarrinho :: Connection -> Carrinho -> IO ()
insertProdutoCarrinho conn (Carrinho usr_cli id_prod) = do
    let codigo = fromString "INSERT INTO carrinho(usr, id_prod) VALUES(?,?);"
    execute conn codigo (usr_cl, id_prod)
    putStrLn $ ""

deletarCarinho :: String -> IO()
deletarCarinho usr_cli = do
    conn <- open "data/DataBase.db"
    execute conn "DELETE FROM carrinho WHERE usr=?" (Only usr)
    return "Todos os Produtos Foram Excluidos do Seu Carrinho"

deletarProdutoCarrinho :: String -> IO()
deletarProdutoCarrinho usr_cli id_prod = do 
    conn <- open "data/DataBase.db"
    let codigo = fromString "DELETE FROM carrinho WHERE usr=? AND id_prod=?;"
    execute conn codigo (usr, id_aula)
    return "Produto Excluido do Seu Carrinho!"

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