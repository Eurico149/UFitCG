module Assinatura (cadastraAssinatura, cadastraVendaAssinatura) where

import Database.SQLite.Simple
import Data.String (fromString)
import Data.Char (toUpper)

import Usuario (temAssinatura, verificaExistencia)

data Assinatura = Assinatura String Float Float Float Int Int String
data VendaAssinatura = VendaAssinatura String String String Int String

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
    let codigo = fromString "INSERT INTO assinatura(sigla, valor_mensal, valor_semestral, valor_anual, desconto, aulas_gratis, acesso) VALUES (?, ?, ?, ?, ?, ?, ?);"
    execute conn codigo (sigla, mensal, semestral, anual, desconto, aulas, acesso)
    return ()



cadastraVendaAssinatura :: String -> String -> String -> Int -> String -> IO String
cadastraVendaAssinatura usr tipo_assinatura tipo_parcela parcelas_pagas data_inicio = do
    if (length usr) > 40 then return "Nome de Usuario Deve Ter no Maximo 40 Caracteres!" 
    else if (length tipo_assinatura) /= 3 then return "Tipo de Assinatura Deve ter 3 Letras!"
    else if (length tipo_parcela) /= 1 then return "O Tipo de Parcela Deve Ter 1 Letra!"
    else if (length data_inicio) /= 10 then return "Formato de data Invalido!"
    else if parcelas_pagas < 0 then return "Numero de Parcelas Invalido!"
    else if (map toUpper tipo_parcela) /= "M" && (map toUpper tipo_parcela) /= "S" && (map toUpper tipo_parcela) /= "A" then return "Tipo de Parcela Invalido!"
    else do
        veriUsr <- verificaUsr usr
        veriAss <- temAssinatura tipo_assinatura
        if (not veriUsr) then return "Usuario Não Existe!"
        else if (not veriAss) then return "Tipo de Assinatura Não Cadastrada!"
        else do
            conn <- open "data/DataBase.db"
            let venass = VendaAssinatura usr tipo_assinatura tipo_parcela parcelas_pagas data_inicio
            insertVendaAssinatura conn venass
            close conn
            return "Venda Inserida!"

insertVendaAssinatura :: Connection -> VendaAssinatura -> IO()
insertVendaAssinatura conn (VendaAssinatura usr tipo_assinatura tipo_parcela parcelas_pagas data_inicio) = do
    let codigo = fromString "INSERT INTO vendas_assinatura(usr, tipo_assinatura, tipo_parcela, parcelas_pagas, data_inicio) VALUES (?, ?, ?, ?, ?);"
    execute conn codigo (usr, tipo_assinatura, tipo_parcela, parcelas_pagas, data_inicio)
    return ()

verificaUsr :: String -> IO Bool
verificaUsr usr = do
    conn <- open "data/DataBase.db"
    quant <- verificaExistencia conn usr
    close conn
    return (quant == 1)
