module Login (login) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.String (fromString, String)
import Data.Time

import Usuario (verificaUsr)
import Data.Bool (Bool (True))

login :: String -> String -> IO String 
login usr senha = do
    conn <- open "data/DataBase.db"
    veri_dados <- verificaDadosLogin conn usr senha
    if veri_dados then do
        veri_horario <- verificaHorario conn usr
        if veri_horario then do
            tipo <- veriUsuario conn usr senha
            close conn
            return tipo
        else return "h"
    else return ""

veriUsuario :: Connection -> String -> String -> IO String
veriUsuario conn usr senha = do
    [Only tipo_usr] <- query conn (fromString "SELECT tipo_usr FROM usuario WHERE usr=? and senha=?") (usr, senha)
    return tipo_usr

verificaDadosLogin :: Connection -> String -> String -> IO Bool
verificaDadosLogin conn usr senha_usr = do
    veri_usr <- verificaUsr usr
    if veri_usr then do
        [Only senha] <- query conn (fromString "SELECT senha FROM usuario WHERE usr=?") (Only usr)
        if senha_usr == senha then return True
        else return False
    else return False

verificaHorario :: Connection -> String -> IO Bool
verificaHorario conn usr = do
    [Only tipo_assinatura] <- query conn (fromString "SELECT tipo_assinatura FROM usuario WHERE usr=?") (Only usr)
    horario_atual <- getZonedTime
    let timeInBrazil = zonedTimeToLocalTime horario_atual
        (TimeOfDay hour _ _) = localTimeOfDay timeInBrazil
    case tipo_assinatura of
        Nothing -> return True
        Just "SIL" -> 
            if hour >= 6 && hour <= 14
               then return True
               else return False
        _ -> return True