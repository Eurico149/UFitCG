{-# LANGUAGE OverloadedStrings #-}

module ClienteAula (adicionarAulaExtra, cancelarAula, listarAulasCliente) where

import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow

import AulaExtra

data Aula = Aula String String
data AulaExtra = AulaExtra String String String deriving (Show)

instance FromRow AulaExtra where
  fromRow = AulaExtra <$> field <*> field <*> field

adicionarAulaExtra :: String -> String -> IO String
adicionarAulaExtra usr_cli id_aula = do 
    conn <- open "data/DataBase.db"
    quant <- quantAulas conn id_aula
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
    let codigo = fromString "INSERT INTO clientes_aulas(usr_cli, id_aula) VALUES(?,?);"
    execute conn codigo (usr_cli, id_aula)
    return ""

quantAulas :: Connection -> String -> IO Int
quantAulas conn id_aula = do
    [Only count] <- query conn "SELECT COUNT (*) FROM aula_extra WHERE id=?" (Only id_aula)
    return count 

verificaExistenciaAula :: Connection -> String -> IO Int
verificaExistenciaAula conn id_aula = do
    [Only count] <- query conn "SELECT COUNT(*) FROM aula_extra WHERE id = ?" (Only id_aula)
    return count

cancelarAula :: String -> String -> IO String
cancelarAula usr_cli id_aula = do
    conn <- open "data/DataBase.db"
    quant <- verificaExistenciaAula conn id_aula
    close conn 

    if quant < 1 then return "Aula inexistente!"
    else do
        let aula = Aula usr_cli id_aula 
        conn <- open "data/DataBase.db"
        deleteAula conn aula
        close conn
        return "Aula Cancelada!"

deleteAula :: Connection -> Aula -> IO String
deleteAula conn (Aula usr_cli id_aula) = do 
    let codigo = fromString "DELETE FROM clientes_aulas WHERE usr_cli=? and id_aula=?;"
    execute conn codigo (usr_cli, id_aula)
    return ""

listarAulasCliente :: String -> IO()
listarAulasCliente usr_cli = do 
    conn <- open "data/DataBase.db"

    aulas <- query conn "SELECT a.materia, a.usr_per, a.data_horario FROM clientes_aulas AS c, aula_extra AS a WHERE c.id_aula=a.id AND c.usr_cli=?" (Only usr_cli) :: IO [AulaExtra]

    mapM_ printAulas aulas

    close conn

printAulas :: AulaExtra -> IO()
printAulas (AulaExtra materia usr_per data_horario) = do
    putStrLn $ "Materia: " ++ materia
    putStrLn $ "Personal: " ++ usr_per
    putStrLn $ "Data e Horario: " ++ data_horario
    putStrLn $ ""