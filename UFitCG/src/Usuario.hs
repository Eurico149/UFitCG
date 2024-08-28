{-# LANGUAGE OverloadedStrings #-}

module Usuario (cadastraUsuario) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.String (fromString)
import Data.Char (toUpper)


data Usuario = Usuario String String String String String String Float

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
                                let user = Usuario usr senha tipo_usr nome data_nascimento tipo_assinatura salario
                                insertUsuario conn user
                                close conn
                                return "Usuario Cadastrado Com Sucesso!"
                            else return "Tipo de Usuario Invalido!"


insertUsuario :: Connection -> Usuario -> IO ()
insertUsuario conn (Usuario usr senha tipo_usr nome data_nascimento tipo_assinatura salario) = do
    let query = fromString "INSERT INTO usuario(usr, senha, tipo_usr, nome, data_nascimento, tipo_assinatura, salario) VALUES(?,?,?,?,?,?,?);"
    execute conn query (usr, senha, tipo_usr, nome, data_nascimento, tipo_assinatura, salario)
    putStrLn $ ""

verificaExistencia :: Connection -> String -> IO Int
verificaExistencia conn usr = do
    [Only count] <- query conn "SELECT COUNT(*) FROM usuario WHERE usr=?" (Only usr)
    return count