{-# LANGUAGE BlockArguments #-}
module AulaExtra (cadastraAula) where

import Data.String (fromString)
import Database.SQLite.Simple 
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField (Only(..)) 

data AulaExtra = AulaExtra Int String String String Int

cadastraAula :: Int -> String -> String -> String -> String -> Int -> IO String
cadastraAula id modalidade urs_per data_horario limite = do
    conn <- open "data/DataBase.db"
    aula <- verificaExistenciaAula conn data_horario
    personal <- verificaExistenciaPersonal conn urs_per
    close conn
    resultado <- verificaCondicoes conn aula personal limite id modalidade urs_per data_horario
    close conn
    return resultado

verificaCondicoes :: Connection -> Int -> Int -> Int -> Int -> String -> String -> String -> IO String
verificaCondicoes conn aula personal limite id modalidade urs_per data_horario
    | personal == 0  = return "Usuario Invalido!"
    | aula >= 1      = return "O horário está ocupado!"
    | limite <= 0    = return "Limite Inválido"
    | otherwise      = do
        let aulaExtra = AulaExtra id modalidade urs_per data_horario limite
        insertAula conn aulaExtra
        return "Aula Cadastrada Com Sucesso!"

insertAula :: Connection -> AulaExtra -> IO ()
insertAula conn aulaExtra = do
    let query = "INSERT INTO aula_extra(id, modalidade, urs_per, data_horario, limite) VALUES(?,?,?,?,?);"
        params = ( idAula aulaExtra
                 , modalidade aulaExtra
                 , usrPer aulaExtra
                 , dataHorario aulaExtra
                 , limite aulaExtra )
    execute conn query params
    putStrLn "Aula inserida com sucesso."

verificaExistenciaAula :: Connection -> String -> IO Int
verificaExistenciaAula conn dataH = do
    [Only count] <- query conn "SELECT COUNT(*) FROM aula_extra WHERE data_horario = ?" (Only dataH)
    return count

verificaExistenciaPersonal :: Connection -> String -> IO Int
verificaExistenciaPersonal conn personal = do
    [Only count] <- query conn "SELECT COUNT(*) FROM usuario WHERE usr = ? AND (tipo_usr = 'per' OR tipo_usr = 'adm') " (Only personal)
    return count