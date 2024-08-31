{-# LANGUAGE OverloadedStrings #-}

module AvaliacaoFisica where

import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow

import Usuario

data AvaliacaoFisica = AvaliacaoFisica String String String String String
data AvaliacaoMostar = AvaliacaoMostar Int String String String String String deriving (Show)

instance FromRow AvaliacaoMostar where
    fromRow = AvaliacaoMostar <$> field <*> field <*> field <*> field <*> field <*> field

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

listarAvaliacoesCliente :: String -> IO()
listarAvaliacoesCliente usr_cli = do 
    conn <- open "data/DataBase.db"

    ava <- query conn "SELECT * FROM avaliacao_fisica WHERE usr_cli=?" (Only usr_cli) :: IO [AvaliacaoMostar]

    mapM_ printAvaliacoes ava

    close conn

listarAvaliacoesPersonal :: String -> IO()
listarAvaliacoesPersonal usr_per = do 
    conn <- open "data/DataBase.db"

    ava <- query conn "SELECT * FROM avaliacao_fisica WHERE usr_per=?" (Only usr_per) :: IO [AvaliacaoMostar]

    mapM_ printAvaliacoes ava

    close conn

printAvaliacoes :: AvaliacaoMostar -> IO()
printAvaliacoes (AvaliacaoMostar id usr_cli usr_per avaliacao observacoes data_ava) = do
    putStrLn $ "Id: " ++ show id
    putStrLn $ "Usuario: " ++ usr_cli
    putStrLn $ "Personal: " ++ usr_per
    putStrLn $ "Avaliacao: " ++ avaliacao
    putStrLn $ "Descricao: " ++ observacoes
    putStrLn $ "Data da Avaliacao: " ++ data_ava
    putStrLn $ ""

removeAvaliacao :: String -> IO String
removeAvaliacao id = do 

    conn <- open "data/DataBase.db"
    quant <- verificaExistenciaAvaliacao conn id
    if quant >= 1 then do 
        deletarTupla conn id
        close conn
        return "Avaliacao Removida!" 
    else do 
        close conn
        return "Avaliacao Inexistente!" 


deletarTupla :: Connection -> String -> IO String
deletarTupla conn id = do
    execute conn "DELETE FROM avaliacao_fisica WHERE id=?" (Only id)
    return ""

verificaExistenciaAvaliacao :: Connection -> String -> IO Int
verificaExistenciaAvaliacao conn id = do
    [Only count] <- query conn "SELECT COUNT(*) FROM avaliacao_fisica WHERE id=?" (Only id)
    return count