{-# LANGUAGE OverloadedStrings #-}

module ClienteAula where

import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow

import AulaExtra

data Aula = Aula String String

adicionarAulaExtra :: String -> String -> IO String
adicionarAulaExtra usr_cli id_aula = do 
    conn <- open "data/DataBase.db"
    quant <- verificaExistenciaAula conn id_aula
    close conn

    if quant < 1 then return "Aula inexistente!"
    else do 
        let aula = Aula usr_cli id_aula
        conn <- open "data/DataBase.db"
        insertClienteAula conn aula
        close conn
        return "Aula Adicionada!"

insertClienteAula :: Connection -> Aula -> IO String
insertClienteAula  conn (Aula usr_cli id_aula) = do
    let codigo = fromString "INSERT INTO cliente_aula(usr_cli, id_aula) VALUES(?,?);"
    execute conn codigo (usr_cli, id_aula)
    return ""

cancelarAula :: String -> String -> IO String
cancelarAula usr_cli id_aula = do
    conn <- open "data/DataBase.db"
    quant <- verificaExistenciaAula id_aula
    close conn 

    if quant < 1 then return "Aula inexistente!"
    else do
        let aula = Aula usr_cli id_aula 
        conn <- open "data/DataBase.db"
        deleteAula conn usr_cli id_aula
        close conn
        return "Aula Cancelada!"

deleteAula :: Connection -> Aula -> IO String
deleteAula conn (Aula usr_cli id_aula) = do 
    let codigo = fromString "DELETE FROM clientes_aulas WHERE usr_cli=? and id_aula=?;"
    execute conn codigo (usr_cli, id_aula)
    return ""