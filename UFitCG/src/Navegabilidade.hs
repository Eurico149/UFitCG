module Navegabilidade (abaLogin) where

import Login

abaLogin :: IO ()
abaLogin = do
    putStrLn "Usuario :"
    usr <- getLine
    putStrLn "Senha: "
    senha <- getLine

    veri <- login usr senha
    if null veri then abaLogin
    else tipoMenu veri usr

tipoMenu :: String -> String -> IO ()
tipoMenu tipo_usr usr
    | tipo_usr == "ADM" = menuAdm usr
    | tipo_usr == "PER" = menuPer usr
    | tipo_usr == "CLI" = menuCli usr
    | otherwise = putStrLn "Erro"

menuAdm :: String -> IO ()
menuAdm usr = do
    putStrLn "acessar lepra"

menuPer :: String -> IO ()
menuPer usr = do
    putStrLn "acessar cancer"

menuCli :: String -> IO ()
menuCli usr = do
    putStrLn "acessar alzaimer"
