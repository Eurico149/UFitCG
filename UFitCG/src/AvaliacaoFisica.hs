module AvaliacaoFisica where

import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow

import Usuario

data AvaliacaoFisica = AvaliacaoFisica String String String String String

cadastraAvaliacao :: String -> String -> String -> String -> String -> IO String
cadastraAvaliacao usr_cli usr_per avaliacao observacoes data_ava = do
    conn <- open "data/DataBase.db"
    quantCli <- verificaExistencia conn usr_cli
    close conn

    if quantCli < 1 then return "Usuario Inexistente!"
    else if (length data_ava) /= 10 then return "Data Invalida!"
    else do
        let ava = AvaliacaoFisica usr_cli usr_per avaliacao observacoes data_ava

        conn <- open "data/DataBase.db"
        insertAvaliacao conn ava
        close conn
        return "Avaliacao Adicionada!"


insertAvaliacao :: Connection -> AvaliacaoFisica -> IO String
insertAvaliacao conn (AvaliacaoFisica usr_cli usr_per avaliacao observacoes data_ava) = do 
    let codigo = fromString "INSERT INTO avaliacao_fisica(usr_cli, usr_per, avaliacao, observacoes, data_ava) VALUES(?,?,?,?,?);"
    execute conn codigo (usr_cli, usr_per, avaliacao, observacoes, data_ava)
    return ""
