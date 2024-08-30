module Assinatura (cadastraAssinatura, cadastraVendaAssinatura, removeAssinatura, removeVendasAssinatura, listarAssinaturas, listarVendasAssinaturas) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.String (fromString)
import Data.Char (toUpper)

import Usuario (temAssinatura, verificaExistencia)

data Assinatura = Assinatura String Float Float Float Int Int String deriving (Show)
data VendaAssinatura = VendaAssinatura String String String Int String
data VendaAssinaturaLis = VendaAssinaturaLis Int String String String Int String
instance FromRow Assinatura where
  fromRow = Assinatura <$> field <*> field <*> field <*> field <*> field <*> field <*> field
instance FromRow VendaAssinaturaLis where
  fromRow = VendaAssinaturaLis <$> field <*> field <*> field <*> field <*> field <*> field

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

removeAssinatura :: String -> IO String
removeAssinatura sigla = do
    if (length sigla) /= 3 then return "Formato De Assinatura Invalido!"
    else do
        let siglaupper = (map toUpper sigla)
        veriAss <- temAssinatura siglaupper
        if veriAss then do
            conn <- open "data/DataBase.db"
            delAssinatura conn siglaupper
            close conn
            return "Assinatura Dletada!"
        else return "Essa Assinatura Não Esta Cadastrada!"

delAssinatura :: Connection -> String -> IO()
delAssinatura conn sigla = do
    execute conn (fromString "DELETE FROM assinatura WHERE sigla=?") (Only sigla)
    return ()

formatAssinatura :: Int -> Assinatura -> String
formatAssinatura id_ass (Assinatura sigla mensal semestral anual desconto aulas acesso) =
    if null acesso then show id_ass ++ ". " ++ sigla ++ ", Valor_Mensal: R$" ++ show mensal ++ ", Valor_Semestral: R$" ++ show semestral ++ ", Valor_Anual: R$" ++ show anual ++ ", Desconto_Aulas: " ++ show desconto ++ "%, Aulas: " ++ show aulas ++ ", Acesso: Iimitado"
    else show id_ass ++ ". " ++ sigla ++ ", Valor_Mensal: R$" ++ show mensal ++ ", Valor_Semestral: R$" ++ show semestral ++ ", Valor_Anual: R$" ++ show anual ++ ", Desconto_Aulas: " ++ show desconto ++ "%, Aulas: " ++ show aulas ++ ", Acesso: " ++ acesso

listarAssinaturas :: IO String
listarAssinaturas = do 
    conn <- open "data/DataBase.db"
    assinaturas <- query_ conn (fromString "SELECT * FROM assinatura") :: IO [Assinatura]
    close conn
    let resultado = unlines $ zipWith formatAssinatura [1..] assinaturas
    return resultado

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

quantVendaAssinatura :: Connection -> Int -> IO Int
quantVendaAssinatura conn id_ven = do
    [Only count] <- query conn (fromString "SELECT COUNT (*) FROM vendas_assinatura WHERE id=?") (Only id_ven)
    return count

verificaVendaAssinatura :: Int -> IO Bool
verificaVendaAssinatura id_ven = do
    conn <- open "data/DataBase.db"
    quant <- quantVendaAssinatura conn id_ven
    close conn
    return (quant == 1)

removeVendasAssinatura :: Int -> IO String
removeVendasAssinatura id_ven = do
    verivenda <- verificaVendaAssinatura id_ven
    conn <- open "data/DataBase.db"
    if verivenda then do
        delVendaAssinatura conn id_ven
        close conn
        return "Venda Removida Com Sucesso!"
    else return "Venda Não Cadastrada!"

delVendaAssinatura :: Connection -> Int -> IO ()
delVendaAssinatura conn id_ven = do
    execute conn (fromString "DELETE FROM vendas_assinatura WHERE id=?") (Only id_ven)
    return ()

formatVendaAssinatura :: VendaAssinaturaLis -> String
formatVendaAssinatura (VendaAssinaturaLis id_ven usr tipo_assinatura tipo_parcela parcelas_pagas data_inicio) =
    show id_ven ++ ". " ++ show usr ++ ", Tipo_Assinatura: " ++ show tipo_assinatura ++ ", Tipo_Parcela: " ++ show tipo_parcela ++ ", Parcelas_pagas: " ++ show parcelas_pagas ++ ", Data_Inicio: " ++ show data_inicio

listarVendasAssinaturas :: IO String
listarVendasAssinaturas = do 
    conn <- open "data/DataBase.db"
    vendas <- query_ conn (fromString "SELECT * FROM vendas_assinatura") :: IO [VendaAssinaturaLis]
    close conn
    let resultado = unlines $ map formatVendaAssinatura vendas
    return resultado
