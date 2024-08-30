module Login (login) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.String (fromString)

import Usuario (verificaUsr)

login :: String -> String -> IO String 
login usr senha = do
    conn <- open "data/DataBase.db"
    veri_dados <- verificaDadosLogin conn usr senha
    if veri_dados then do
        tipo <- veriUsuario conn usr senha
        close conn
        return tipo
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
