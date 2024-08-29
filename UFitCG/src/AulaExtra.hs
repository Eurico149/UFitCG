{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module AulaExtra (cadastraAula) where

import Data.String (fromString)
import Database.SQLite.Simple

data AulaExtra = AulaExtra String String String Int

cadastraAula :: String -> String -> String -> Int -> IO String
cadastraAula materia usr_per data_horario limite = do
    conn <- open "data/DataBase.db"
    aula <- verificaExistenciaAula conn data_horario
    personal <- verificaExistenciaPersonal conn usr_per
    resultado <- verificaCondicoes conn aula personal limite materia usr_per data_horario
    close conn
    return resultado

verificaCondicoes :: Connection -> Int -> Int -> Int -> String -> String -> String -> IO String
verificaCondicoes conn aula personal limite materia usr_per data_horario
    | personal == 0  = return "Usuario Invalido!"
    | aula >= 1      = return "O horário está ocupado!"
    | limite <= 0    = return "Limite Inválido"
    | length data_horario /= 22 = return "Horario Invalido"
    | otherwise = do
        let aulaExtra = AulaExtra materia usr_per data_horario limite
        conn <- open "data/DataBase.db"
        insertAula conn aulaExtra
        close conn
        return "Aula Cadastrada Com Sucesso!"

insertAula :: Connection -> AulaExtra -> IO ()
insertAula conn (AulaExtra materia usr_per data_horario limite) = do
    let query = fromString "INSERT INTO aula_extra(materia, usr_per, data_horario, limite) VALUES(?,?,?,?);"
    execute conn query (materia, usr_per, data_horario, limite)
    return ()

verificaExistenciaAula :: Connection -> String -> IO Int
verificaExistenciaAula conn dataH = do
    [Only count] <- query conn "SELECT COUNT(*) FROM aula_extra WHERE data_horario = ?" (Only dataH)
    return count

verificaExistenciaPersonal :: Connection -> String -> IO Int
verificaExistenciaPersonal conn personal = do
    [Only count] <- query conn "SELECT COUNT(*) FROM usuario WHERE usr = ? AND (tipo_usr = 'PER' OR tipo_usr = 'ADM') " (Only personal)
    return count