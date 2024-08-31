module Navegabilidade (abaLogin) where

import Login
import Assinatura
import ClienteAula
import System.IO (hFlush, stdout)
import System.Process (callCommand)

abaLogin :: IO ()
abaLogin = do
    callCommand "clear"

    putStr "Usuario: "
    hFlush stdout
    usr <- getLine
    putStr "Senha: "
    hFlush stdout
    senha <- getLine

    callCommand "clear"

    veri <- login usr senha
    if null veri then do
        putStrLn "Usuario ou Senha Invalido"
        putStrLn "Aperte Enter Para Fazer Login Novamente ou '-' Para Sair"
        comando <- getLine
        if comando == "-" then return ()
        else abaLogin
    else tipoMenu veri usr

tipoMenu :: String -> String -> IO ()
tipoMenu tipo_usr usr
    | tipo_usr == "ADM" = menuAdm usr
    | tipo_usr == "PER" = menuPer usr
    | tipo_usr == "CLI" = menuCli usr
    | otherwise = putStrLn "Erro"

menuAdm :: String -> IO ()
menuAdm usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Listas Vendas de Assinaturas UFitCG"
    putStrLn "-. Sair"
    comando <- getLine
    callCommand "clear"
    acaoMenuADM comando usr

acaoMenuADM :: String -> String -> IO()
acaoMenuADM comando usr
    | comando == "1" = do
        saida <- listarVendasAssinaturas
        putStrLn saida
        espera
        menuAdm usr
    | comando == "-" = return ()
    | otherwise = menuAdm usr

menuPer :: String -> IO ()
menuPer usr = do
    putStrLn "acessar cancer"

menuCli :: String -> IO ()
menuCli usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Listas Assinaturas UFitCG"
    putStrLn "2. Listar Aulas Extras"
    putStrLn "-. Sair"
    comando <- getLine
    callCommand "clear"
    acaoMenuCli comando usr

acaoMenuCli :: String -> String -> IO ()
acaoMenuCli comando usr
    | comando == "1" = do
        saida <- listarAssinaturas
        putStrLn saida
        espera
        menuCli usr
    | comando == "2"= do
        listarAulasCliente usr
        espera
        menuCli usr
    | comando == "-" = return ()
    | otherwise = menuCli usr

espera :: IO ()
espera = do
    _ <- getLine
    callCommand "clear"
    return ()