{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module AulaExtra (cadastraAula, verificaExistenciaAula, removeAula, listarAulas, listarAulasPersonal) where

import Data.String (fromString)
import Database.SQLite.Simple

import Usuario

data AulaExtra = AulaExtra String String String Int
data AulaMostrar = AulaMostrar Int String String String Int deriving (Show)

instance FromRow AulaMostrar where
    fromRow = AulaMostrar <$> field <*> field <*> field <*> field <*> field

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

verificaAulaId :: Connection -> String -> IO Int
verificaAulaId conn id = do
    [Only count] <- query conn "SELECT COUNT(*) FROM aula_extra WHERE id=?" (Only id)
    return count

removeAula :: String -> IO String
removeAula id = do
    conn <- open "data/DataBase.db"
    quant <- verificaAulaId conn id
    if quant >= 1 then do 
        deletarTupla conn id
        close conn
        return "Aula removida!" 
    else do 
        close conn
        return "Aula Inexistente!"

deletarTupla :: Connection -> String -> IO String
deletarTupla conn id = do
    execute conn "DELETE FROM aula_extra WHERE usr=?" (Only id)
    return ""

listarAulasPersonal :: String -> IO()
listarAulasPersonal usr_per = do 
    conn <- open "data/DataBase.db"

    aulas <- query conn "SELECT * FROM aula_extra WHERE usr_per=?" (Only usr_per) :: IO [AulaMostrar]

    mapM_ printAulas aulas

    close conn

listarAulas :: IO()
listarAulas = do 
    conn <- open "data/DataBase.db"

    aulas <- query_ conn "SELECT * FROM aula_extra" :: IO [AulaMostrar]

    mapM_ printAulas aulas

    close conn

printAulas :: AulaMostrar -> IO()
printAulas (AulaMostrar id materia usr_per data_horario limite) = do
    putStrLn $ "Id: " ++ show id
    putStrLn $ "Materia: " ++ materia
    putStrLn $ "Personal: " ++ usr_per
    putStrLn $ "Data e Horario: " ++ data_horario
    putStrLn $ ""