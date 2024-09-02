{-# LANGUAGE OverloadedStrings #-}

module Venda where

import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow

import Carrinho

data Venda = Venda Int String String String Float deriving (Show)

instance FromRow Venda where
    fromRow = Venda <$> field <*> field <*> field <*> field <*> field

cadastrarVenda :: String -> IO String
cadastrarVenda usr = do
    veri_car <- verificaProdutoCarrinho usr
    if (veri_car == 0) then return "Carrinho Vazio"
    else do
        produtos <- finalizaCompra usr
        dataAtual <- dataFinalizaCompra
        valorTotal <- valorCompra usr
        conn <- open "data/DataBase.db"
        let venda = fromString "INSERT INTO vendas_loja(produtos, usr, data_horario, valor_total) VALUES(?,?,?,?);"
        execute conn venda (produtos, usr, dataAtual, valorTotal)
        close conn 
        deletarCarinho usr
        return "Compra Efetuada"
        
verificaProdutoCarrinho :: String -> IO Int
verificaProdutoCarrinho usr = do
    conn <- open "data/DataBase.db"
    [Only count] <- query conn "SELECT COUNT(*) FROM carrinho WHERE usr=?" (Only usr)
    close conn
    return count

filtrarVendas :: String -> IO()
filtrarVendas usr = do
    conn <- open "data/DataBase.db"

    vendasUsr <- query conn "SELECT * FROM vendas_loja WHERE usr=?" (Only usr) :: IO [Venda]

    close conn

    mapM_ formatVenda vendasUsr 

quantidadeVendasLoja :: Connection -> String -> IO Int
quantidadeVendasLoja conn id_ven = do
    [Only count] <- query conn (fromString "SELECT COUNT(*) FROM vendas_loja WHERE id=?") (Only id_ven)
    return count

verificaVendaLoja :: String -> IO Bool
verificaVendaLoja id_ven = do
    conn <- open "data/DataBase.db"
    quant <- quantidadeVendasLoja conn id_ven
    close conn
    return (quant == 1)

removeVendaLoja :: String -> IO String
removeVendaLoja id_ven = do
    verivenda <- verificaVendaLoja id_ven
    if verivenda then do
        conn <- open "data/DataBase.db"
        delVendaLoja conn id_ven
        close conn
        return "Venda Removida Com Sucesso"
    else return "Venda Não Cadastrada"

delVendaLoja :: Connection -> String -> IO ()
delVendaLoja conn id_ven = do
    execute conn (fromString "DELETE FROM vendas_loja WHERE id=?") (Only id_ven)
    return ()

listarVendas :: IO()
listarVendas = do
    conn <- open "data/DataBase.db"
    vendas <- query_ conn "SELECT * FROM vendas_loja" :: IO [Venda]
    close conn
    mapM_ formatVenda vendas

formatVenda :: Venda -> IO ()
formatVenda (Venda id produto usr data_venda valor) = do
    putStrLn $ "Id: " ++ show id
    putStrLn $ "Produto: " ++ produto
    putStrLn $ "Usuario: " ++ usr
    putStrLn $ "Data: " ++ data_venda
    putStrLn $ "Valor: " ++ show valor
    putStrLn $ ""     


    