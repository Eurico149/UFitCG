module Main (main) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow ()
import Control.Exception (bracket)
import Data.String (fromString) -- Certifique-se de importar fromString

-- Definindo o tipo de dados que será retornado pela consulta
data Usuario = Usuario
  { tipoUsr :: String
  } deriving (Show)

-- Definindo como o resultado da consulta será convertido para o tipo Usuario
instance FromRow Usuario where
  fromRow = Usuario <$> field

-- Função para fazer a consulta ao banco de dados
consultarUsuario :: String -> String -> IO ()
consultarUsuario matricula senha = do
  let queryStr = "SELECT tipo_usr FROM usuario WHERE matricula = ? AND senha = ?;"
  bracket (open "data/DataBase.db") close $ \conn -> do
    let querySql = Query $ fromString queryStr -- Convertendo a string para Query
    results <- query conn querySql (matricula, senha) :: IO [Usuario]
    mapM_ print results

-- Função principal
main :: IO ()
main = do
  matricula <- getLine
  senha <- getLine
  consultarUsuario matricula senha


