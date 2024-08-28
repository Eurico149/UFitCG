{-# LANGUAGE OverloadedStrings #-}

module Assinatura (cadastraAssinatura) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.String (fromString)
import Data.Char (toUpper)

data Assinatura = Assinatura String Float Float Float Int Int String

cadastraAssinatura :: String -> Float -> Float -> Float -> Int -> Int -> String -> IO String
cadastraAssinatura sigla mensal semestral anual desconto aulas acesso = do
    if mensal < 0 || semestral < 0 || anual < 0 then return "Valores Negativos Invalidos!"
        else if desconto < 0 then return "Desconto Negativo Invalido!"
            else if aulas < 0 then return "Numero de Aulas Invalidas!"
                else if (length sigla) /= 3 then return "Sigla Com Tamanho Diferente de 3 Invalida!"
                    else if (length acesso) > 23 then return "Acesso Com Mais de 23 Caracteres Invalido"
                        else do
                            conn <- open "data/DataBase.db"
                            let ass = Assinatura (map toUpper sigla) mensal semestral anual desconto aulas acesso
                            insertAssinatura conn ass
                            close conn
                            return "Assinatura Inserida!"

insertAssinatura :: Connection -> Assinatura -> IO ()
insertAssinatura conn (Assinatura sigla mensal semestral anual desconto aulas acesso) = do
    let query = fromString "INSERT INTO assinatura (sigla, valor_mensal, valor_semestral, valor_anual, desconto, aulas_gratis, acesso) VALUES (?, ?, ?, ?, ?, ?, ?);"
    execute conn query (sigla, mensal, semestral, anual, desconto, aulas, acesso)
    return ()

