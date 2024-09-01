{-# LANGUAGE OverloadedStrings #-}

module Venda where

import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow

import Carrinho

data Venda = Venda String String String Float

cadastrarVenda :: String -> IO()
cadastrarVenda usr = do
    conn <- open "data/DataBase.db"
    produtos <- finalizaCompra usr
    dataAtual <- dataFinalizaCompra
    valorTotal <- valorCompra usr
    if produtos == ""
        then putStrLn ""
        else do
            let venda = fromString "INSERT INTO vendas_loja(produtos, usr, data_horario, valor_total) VALUES(?,?,?,?);"
            execute conn venda (produtos, usr, dataAtual, valorTotal)
            putStrLn ""
            close conn 
            deletarCarinho usr

filtrarVendas :: String -> IO()
filtrarVendas usr = do
    conn <- open "data/DataBase.db"
    vendasUsr <- query conn 
        "SELECT * FROM vendas_loja WHERE usr=?" (Only usr) :: IO [Venda]
    close conn
    mapM_ (putStrLn . formatVenda) vendasUsr 

listarVendas :: IO()
listarVendas = do
    conn <- open "data/DataBase.db"
    vendas <- query conn 
        "SELECT * FROM vendas_loja" (Only usr) :: IO [Venda]
    close conn
    mapM_ (putStrLn . formatVenda) vendas
    