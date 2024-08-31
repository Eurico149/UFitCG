{-# LANGUAGE OverloadedStrings #-}

module Usuario (cadastraUsuario, temAssinatura, verificaUsr, removeUsuario, verificaExistencia, mostrarPerfil) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.String (fromString)


data Usuario = Usuario String String String String String String Float deriving (Show)

instance FromRow Usuario where
    fromRow = Usuario <$> field <*> field <*> field <*> field <*> field <*> field <*> field

cadastraUsuario :: String -> String -> String -> String -> String -> String -> Float -> IO String
cadastraUsuario usr senha tipo_usr nome data_nascimento tipo_assinatura salario = do
    conn <- open "data/DataBase.db"
    quant <- verificaExistencia conn usr
    close conn

    if quant >= 1 then return "Usuario Existente!" 
    else if (length usr) > 40 then return "Nome de Usuario Deve Ter no Maximo 40 Caracteres!"
    else if (length senha) /= 8 then return "Senha Deve Ter Exatamente 8 Caracteres!"
    else if (length data_nascimento) /= 10 then return "Data de Nascimento Deve Ter Exataente 10 Caracteres(dd/mm/aaaa)!"
    else if salario < 0 then return "Erro: Salario Negavivo!"
    else do
        conn <- open "data/DataBase.db"
        if tipo_usr == "ADM" || tipo_usr == "PER" then do
            let user = Usuario usr senha tipo_usr nome data_nascimento "   " salario
            insertUsuario conn user
            close conn
            return "Usuario Cadastrado Com Sucesso!"
        else if tipo_usr == "CLI" then do
            temAssinaturaBool <- temAssinatura tipo_assinatura
            if temAssinaturaBool then do
                let user = Usuario usr senha tipo_usr nome data_nascimento tipo_assinatura salario
                insertUsuario conn user
                close conn
                return "Usuario Cadastrado Com Sucesso!"
            else return "Tipo de Assinatura Invalida!"
        else return "Tipo de Usuario Invalido!"


insertUsuario :: Connection -> Usuario -> IO ()
insertUsuario conn (Usuario usr senha tipo_usr nome data_nascimento tipo_assinatura salario) = do
    let codigo = fromString "INSERT INTO usuario(usr, senha, tipo_usr, nome, data_nascimento, tipo_assinatura, salario) VALUES(?,?,?,?,?,?,?);"
    execute conn codigo (usr, senha, tipo_usr, nome, data_nascimento, tipo_assinatura, salario)
    putStrLn $ ""

verificaExistencia :: Connection -> String -> IO Int
verificaExistencia conn usr = do
    [Only count] <- query conn "SELECT COUNT(*) FROM usuario WHERE usr=?" (Only usr)
    return count

quantAssinatura :: Connection -> String -> IO Int
quantAssinatura conn sigla = do
    [Only count] <- query conn "SELECT COUNT (*) FROM assinatura WHERE sigla=?" (Only sigla)
    return count
 

temAssinatura :: String -> IO Bool
temAssinatura sigla = do 
    conn <- open "data/DataBase.db"
    quant <- quantAssinatura conn sigla
    close conn

    if quant >= 1 then return True
        else return False

removeUsuario :: String -> IO String
removeUsuario usr = do 
    conn <- open "data/DataBase.db"
    quant <- verificaExistencia conn usr
    if quant >= 1 then do 
        deletarTupla conn usr
        close conn
        return "Usuario removido!" 
    else do 
        close conn
        return "Usuario Inexistente!" 


deletarTupla :: Connection -> String -> IO String
deletarTupla conn usr = do
    execute conn "DELETE FROM usuario WHERE usr=?" (Only usr)
    return ""

verificaUsr :: String -> IO Bool
verificaUsr usr = do
    conn <- open "data/DataBase.db"
    quant <- verificaExistencia conn usr
    close conn
    return (quant == 1)

mostrarPerfil :: String -> IO()
mostrarPerfil usr = do 
    conn <- open "data/DataBase.db"

    user <- query conn "SELECT * FROM usuario WHERE usr=?" (Only usr) :: IO [Usuario]

    mapM_ printPerfil user

    close conn

printPerfil :: Usuario -> IO()
printPerfil (Usuario usr senha tipo_usr nome data_nascimento tipo_assinatura salario) = do
    putStrLn $ "Usuario: " ++ usr
    if tipo_usr =="CLI" then putStrLn $ "Tipo de Usuario: Cliente"
    else if tipo_usr == "ADM" then putStrLn $ "Tipo de Usuario: Administrador"
    else putStrLn $ "Tipo de Usuario: Personal"

    putStrLn $ "Nome: " ++ nome
    putStrLn $ "Data de Nascimento: " ++ data_nascimento

    if tipo_usr == "CLI" then putStrLn $ "Tipo de Assinatura: " ++ tipo_assinatura
    else putStrLn $ "Salario: " ++ show salario
    putStrLn $ "" 
