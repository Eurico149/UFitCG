{-# LANGUAGE OverloadedStrings #-}

module FichaTreino (cadastrarFicha, removeFichaTreino, listarFichaCliente, listarFichaPersonal) where

import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow

import Usuario

data FichaTreino = FichaTreino String String String String
data FichaMostrar = FichaMostrar Int String String String String deriving (Show)

instance FromRow FichaMostrar where
    fromRow = FichaMostrar <$> field <*> field <*> field <*> field <*> field

cadastrarFicha :: String -> String -> String -> String -> IO String 
cadastrarFicha usr_cli usr_per exercicios observacoes = do
     conn <- open "data/DataBase.db"
     quant <- verificaExistencia conn usr_cli
     close conn

     if quant < 1 then return "Usuario Inexistente!"
     else if (length observacoes) > 250 then return "Capacidade das Observacoes Excedida!"
     else do
        conn <- open "data/DataBase.db"
        let ficha = FichaTreino usr_cli usr_per exercicios observacoes
        insertFicha conn ficha
        close conn
        return "Ficha Cadastrada!"

insertFicha :: Connection -> FichaTreino -> IO()
insertFicha conn (FichaTreino usr_cli usr_per exercicios observacoes) = do
    let codigo = fromString "INSERT INTO ficha_treino(usr_cli, usr_per, exercicios, observacoes) VALUES(?,?,?,?);"
    execute conn codigo (usr_cli, usr_per, exercicios, observacoes)
    putStrLn $ ""

verificaExistenciaFicha :: Connection -> String -> IO Int
verificaExistenciaFicha conn id = do
    [Only count] <- query conn "SELECT COUNT(*) FROM ficha_treino WHERE id=?" (Only id)
    return count

removeFichaTreino :: String -> IO String
removeFichaTreino id = do 
    conn <- open "data/DataBase.db"
    quant <- verificaExistenciaFicha conn id
    if quant >= 1 then do 
        deletarTupla conn id
        close conn
        return "Ficha removida!" 
    else do 
        close conn
        return "Ficha Inexistente!" 


deletarTupla :: Connection -> String -> IO String
deletarTupla conn id = do
    execute conn "DELETE FROM ficha_treino WHERE id=?" (Only id)
    return ""

listarFichaCliente :: String -> IO()
listarFichaCliente usr_cli = do 
    conn <- open "data/DataBase.db"

    fichas <- query conn "SELECT * FROM ficha_treino WHERE usr_cli=?" (Only usr_cli) :: IO [FichaMostrar]

    mapM_ printFichas fichas

    close conn

listarFichaPersonal :: String -> IO()
listarFichaPersonal usr_per = do 
    conn <- open "data/DataBase.db"

    fichas <- query conn "SELECT * FROM ficha_treino WHERE usr_per=?" (Only usr_per) :: IO [FichaMostrar]

    mapM_ printFichas fichas

    close conn

printFichas :: FichaMostrar -> IO()
printFichas (FichaMostrar id usr_cli usr_per exercicios observacoes) = do
    putStrLn $ "Id: " ++ show id
    putStrLn $ "Usuario: " ++ usr_cli
    putStrLn $ "Personal: " ++ usr_per
    putStrLn $ "Exercicios: " ++ exercicios
    putStrLn $ "Observacoes: " ++ observacoes
    putStrLn $ ""