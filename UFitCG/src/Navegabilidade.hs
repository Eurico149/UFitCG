module Navegabilidade (telaInicial) where

import Login
import Usuario
import Assinatura
import ClienteAula
import Loja
import AvaliacaoFisica
import FichaTreino
import AulaExtra
import Carrinho
import Venda
import System.IO (hFlush, stdout, readFile)
import System.Process (callCommand)

telaInicial :: IO ()
telaInicial = do 
    callCommand "clear"

    putStrLn "|------------------------------|\n|      Bem vindo à UFitCG      |\n|                              |\n|    Tecle ENTER para Login    |\n|      Digite - para Sair      |\n|------------------------------|\n"    
    input <- getLine
    acaoTelaInicial input
    
acaoTelaInicial :: String -> IO ()
acaoTelaInicial input
    | input == "-" = return ()
    | otherwise = abaLogin

abaLogin :: IO ()
abaLogin = do
    callCommand "clear"

    putStrLn "LOGIN"
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
        if comando == "-" then telaInicial
        else abaLogin
    else if "h" == veri then do  
            putStrLn "Usuario fora de Horario de Acesso"
            putStrLn "Aperte Enter Para Fazer Login Novamente ou '-' Para Sair"
            comando <- getLine
            if comando == "-" then telaInicial
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
    putStrLn "1. Controle de Usuario\n2. Controle da Loja\n3. Controle Assinaturas\n4. Vendas Assinaturas\n5. Vendas Loja\n6. Perfil\n-. Sair"
    comando <- getLine
    callCommand "clear"
    acaoMenuADM comando usr

acaoMenuADM :: String -> String -> IO()
acaoMenuADM comando usr
    | comando == "1" = menuUsuarioAdm usr
    | comando == "2" = menuLojaAdm usr
    | comando == "3" = menuAssAdm usr
    | comando == "4" = menuVendasAdm usr
    | comando == "5" = menuVendasLojaAdm usr
    | comando == "6" = do
        mostrarPerfil usr
        espera
        menuAdm usr
    | comando == "-" = telaInicial
    | otherwise = menuAdm usr

menuVendasLojaAdm :: String -> IO()
menuVendasLojaAdm usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Debito Cliente\n2. Apagar Venda Loja\n3. Listar Vendas\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuVendasLojaAdm comando usr

acaoMenuVendasLojaAdm :: String -> String -> IO()
acaoMenuVendasLojaAdm comando usr
    | comando == "1" = do
        putStr "Cliente: "
        hFlush stdout
        usr_cli <- getLine

        filtrarVendas usr_cli
        espera
        menuVendasLojaAdm usr
    | comando == "2" = do
        putStr "Id: "
        hFlush stdout
        id_ven <- getLine

        mensagem <- removeVendaLoja id_ven
        putStrLn mensagem
        espera
        menuVendasLojaAdm usr
    | comando == "3" = do
        listarVendas
        espera
        menuVendasLojaAdm usr
    | comando == "-" = menuAdm usr
    | otherwise = menuVendasLojaAdm usr

menuVendasAdm :: String -> IO ()
menuVendasAdm usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Cadastrar Venda\n2. Cancelar Venda\n3. Listar Vendas\n4. Adicionar Parcela Paga\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuVendasAdm comando usr

acaoMenuVendasAdm :: String -> String -> IO ()
acaoMenuVendasAdm comando usr
    | comando == "1" = do
        putStr "Usuario: "
        hFlush stdout
        usr <- getLine
        putStr "Tipo de Assinatura: "
        hFlush stdout
        tipo_ass <- getLine
        putStr "Tipo Parcela: "
        hFlush stdout
        tipo_parcela <- getLine
        putStr "Parcelas Pagas: "
        hFlush stdout
        parcelas_pagasstr <- getLine
        let parcelas_pagas = read parcelas_pagasstr :: Int
        putStr "Data: "
        hFlush stdout
        data_inicio <- getLine

        mensagem <- (cadastraVendaAssinatura usr tipo_ass tipo_parcela parcelas_pagas data_inicio)
        putStrLn mensagem
        espera
        menuVendasAdm usr
    | comando == "2" = do
        putStr "Id: "
        hFlush stdout
        id_venstr <- getLine
        let id_ven = read id_venstr :: Int
        
        mensagem <- (removeVendasAssinatura id_ven)
        putStrLn mensagem
        espera
        menuVendasAdm usr
    | comando == "3" = do
        listarVendasAssinaturas
        espera
        menuVendasAdm usr
    | comando == "4" = do
        putStr "Cliente: "
        hFlush stdout
        cliente <- getLine

        mensagem <- adicionarParcelaPaga cliente
        putStrLn mensagem
        espera
        menuVendasAdm usr
    | comando == "-" = menuAdm usr
    | otherwise = menuVendasAdm usr

menuUsuarioAdm :: String -> IO () 
menuUsuarioAdm usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Cadastrar Usuario\n2. Apagar Usuario\n3. Listar Usuarios\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuUsuarioAdm comando usr

acaoMenuUsuarioAdm :: String -> String -> IO ()
acaoMenuUsuarioAdm comando usr
    | comando == "1" = do
        putStr "Usuario: "
        hFlush stdout
        usuario <- getLine
        putStr "Senha: "
        hFlush stdout
        senha <- getLine
        putStr "Tipo de Usuario: "
        hFlush stdout
        tipo_usr <- getLine
        putStr "Nome: "
        hFlush stdout
        nome <- getLine
        putStr "Data de Nascimento: "
        hFlush stdout
        data_nas <- getLine
        putStr "Tipo de Assinatura: "
        hFlush stdout
        tipo_assinatura <- getLine
        putStr "Salario: "
        hFlush stdout
        salariostr <- getLine
        let salario = read salariostr :: Float

        mensagem <- (cadastraUsuario usuario senha tipo_usr nome data_nas tipo_assinatura salario)
        putStrLn mensagem
        espera
        menuUsuarioAdm usr
    | comando == "2" = do
        putStr "Usuario: "
        hFlush stdout
        usuario <- getLine
        
        mensagem <- (removeUsuario usuario)
        putStrLn mensagem
        espera
        menuUsuarioAdm usr
    | comando == "3" = menuUsuarioListarAdm usr
    | comando == "-" = menuAdm usr
    | otherwise = menuUsuarioAdm usr

menuUsuarioListarAdm :: String -> IO ()
menuUsuarioListarAdm usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Listar Todos\n2. Listar Por Tipo\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuUsuarioListarAdm comando usr

acaoMenuUsuarioListarAdm :: String -> String -> IO ()
acaoMenuUsuarioListarAdm comando usr
    | comando == "1" = do
        mostrarUsuarios
        espera
        menuUsuarioListarAdm usr
    | comando == "2" = do
        putStr "Tipo: "
        hFlush stdout
        tipo <- getLine
        mostrarUsuariosTipo tipo
        espera
        menuUsuarioListarAdm usr
    | comando == "-" = menuUsuarioAdm usr
    | otherwise = menuUsuarioListarAdm usr

menuLojaAdm :: String -> IO ()
menuLojaAdm usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Cadastrar Produto\n2. Apagar Produto\n3. Listar Produtos\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuLojaAdm comando usr

acaoMenuLojaAdm :: String -> String -> IO ()
acaoMenuLojaAdm comando usr
    | comando == "1" = do
        putStr "Nome do Produto: "
        hFlush stdout
        nome <- getLine
        putStr "Valor: "
        hFlush stdout
        valorstr <- getLine
        let valor = read valorstr :: Float
        putStr "Descricao: "
        hFlush stdout
        descricao <- getLine
        putStr "Categorias: "
        hFlush stdout
        categorias <- getLine

        mensagem <- (cadastroProduto nome valor descricao categorias)
        putStrLn mensagem
        espera
        menuLojaAdm usr
    | comando == "2" = do
        putStr "Id: "
        hFlush stdout
        id_prodstr <- getLine
        let id_prod = read id_prodstr :: Int

        mensagem <- (removeProduto id_prod)
        putStrLn mensagem
        espera
        menuLojaAdm usr
    | comando == "3" = menuLojaListarAdm usr
    | comando == "-" = menuAdm usr
    | otherwise = menuLojaAdm usr

menuLojaListarAdm :: String -> IO ()
menuLojaListarAdm usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Listar Todos\n2. Listar Por Categoria\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuLojaListarAdm comando usr

acaoMenuLojaListarAdm :: String -> String -> IO()
acaoMenuLojaListarAdm comando usr
    | comando == "1" = do 
        listarProdutos
        espera
        menuLojaListarAdm usr
    | comando == "2" = do
        putStr "Categoria: "
        hFlush stdout
        categoria <- getLine
        listarProdutosCategorias categoria
        espera
        menuLojaListarAdm usr
    | comando == "-" = menuLojaAdm usr
    | otherwise = menuLojaListarAdm usr

menuAssAdm :: String -> IO () 
menuAssAdm usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Cadastrar Assinatura\n2. Apagar Assinatura\n3. Listar Assinaturas\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuAssAdm comando usr

acaoMenuAssAdm :: String -> String -> IO ()
acaoMenuAssAdm comando usr
    | comando == "1" = do
        putStr "Sigla: "
        hFlush stdout
        sigla <- getLine
        putStr "Valor Mensal: "
        hFlush stdout
        mensalstr <- getLine
        let mensal = read mensalstr :: Float
        putStr "Valor Semestral: "
        hFlush stdout
        semestralstr <- getLine
        let semestral = read semestralstr :: Float
        putStr "Valor Anual: "
        hFlush stdout
        anualstr <- getLine
        let anual = read anualstr :: Float
        putStr "Desconto em Aulas Extras: "
        hFlush stdout
        descontostr <- getLine
        let desconto = read descontostr :: Int
        putStr "Numero Aulas Gratis: "
        hFlush stdout
        aulasstr <- getLine
        let aulas = read aulasstr :: Int
        putStr "Acesso: "
        hFlush stdout
        acesso <- getLine
        
        mensagem <- (cadastraAssinatura sigla mensal semestral anual desconto aulas acesso)
        putStrLn mensagem
        espera
        menuAssAdm usr
    | comando == "2" = do
        putStr "Sigla da Assinatura a Apagar: "
        hFlush stdout
        sigla <- getLine

        mensagem <- removeAssinatura sigla
        putStrLn mensagem
        espera
        menuAssAdm usr
    | comando == "3" = do
        saida <- listarAssinaturas
        putStrLn saida
        espera
        menuAssAdm usr
    | comando == "-" = menuAdm usr
    | otherwise = menuAssAdm usr

menuPer :: String -> IO ()
menuPer usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Controle de Aulas\n2. Controle Avalições Fisicas\n3. Controle Ficha de Treino\n4. Perfil\n-. Sair"
    comando <- getLine
    callCommand "clear"
    acaoMenuPer comando usr

acaoMenuPer :: String -> String -> IO ()
acaoMenuPer comando usr
    | comando == "1" = menuAulasPer usr
    | comando == "2" = menuAvaliacaoPer usr
    | comando == "3" = menuTreinoPer usr
    | comando == "4" = do 
        mostrarPerfil usr
        espera
        menuPer usr
    | comando == "-" = telaInicial
    | otherwise = menuPer usr

menuTreinoPer :: String -> IO ()
menuTreinoPer usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Cadastrar Ficha de Treino\n2. Apagar Ficha de treino\n3. Listar Fichas de Treino\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuTreinoPer comando usr

acaoMenuTreinoPer :: String -> String -> IO ()
acaoMenuTreinoPer comando usr
    | comando == "1" = do
        putStr "Cliente: "
        hFlush stdout
        usr_cli <- getLine
        putStr "Exercicios: "
        hFlush stdout
        exercicios <- getLine
        putStr "Observacoes: "
        hFlush stdout
        observacoes <- getLine

        mensagem <- cadastrarFicha usr_cli usr exercicios observacoes
        putStrLn mensagem
        espera
        menuTreinoPer usr
    | comando == "2" = do 
        putStr "Id: "
        hFlush stdout
        id_ficha <- getLine
        
        mensagem <- removeFichaTreino id_ficha
        putStrLn mensagem
        espera
        menuTreinoPer usr
    | comando == "3" = do
        listarFichaPersonal usr
        espera
        menuTreinoPer usr
    | comando == "-" = menuPer usr
    | otherwise = menuTreinoPer usr

menuAvaliacaoPer :: String -> IO ()
menuAvaliacaoPer usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Cadastrar Avaliacao Fisica\n2. Apagar Avaliacao Fisica\n3. Listar Avaliações Fisicas\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuAvaliacaoPer comando usr

acaoMenuAvaliacaoPer :: String -> String -> IO ()
acaoMenuAvaliacaoPer comando usr
    | comando == "1" = do
        putStr "Cliente: "
        hFlush stdout
        cliente <- getLine
        putStr "Avaliação: "
        hFlush stdout
        avaliacao <- getLine
        putStr "Observações: "
        hFlush stdout
        observacoes <- getLine
        putStr "Data: "
        hFlush stdout
        data_ava <- getLine 
        
        mensagem <- (cadastraAvaliacao cliente usr avaliacao observacoes data_ava)
        putStrLn mensagem
        espera
        menuAvaliacaoPer usr
    | comando == "2" = do
        putStr "Id: "
        hFlush stdout
        id_ava <- getLine
        mensagem <- removeAvaliacao id_ava
        putStrLn mensagem
        espera
        menuAvaliacaoPer usr
    | comando == "3" = do
        listarAvaliacoesPersonal usr
        espera
        menuAvaliacaoPer usr
    | comando == "-" = menuPer usr
    | otherwise = menuAvaliacaoPer usr

menuAulasPer :: String -> IO ()
menuAulasPer usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Cadastrar Aula\n2. Apagar Aula\n3. Listar Aulas\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuAulasPer comando usr

acaoMenuAulasPer :: String -> String -> IO ()
acaoMenuAulasPer comando usr
    | comando == "1" = do
        putStr "Materia: "
        hFlush stdout
        materia <- getLine
        putStr "Data e Horario: "
        hFlush stdout
        data_horario <- getLine
        putStr "Limite de Alunos: "
        hFlush stdout
        limiteStr <- getLine
        let limite = read limiteStr :: Int
        
        mensagem <- (cadastraAula materia usr data_horario limite)
        putStrLn mensagem
        espera
        menuAulasPer usr
    | comando == "2" = do
        putStr "Id: "
        hFlush stdout
        id_aula <- getLine

        mensagem <- (removeAula id_aula)
        putStrLn mensagem
        espera
        menuAulasPer usr
    | comando == "3" = menuListaAulaPer usr
    | comando == "-" = menuPer usr
    | otherwise = menuAulasPer usr

menuListaAulaPer :: String -> IO()
menuListaAulaPer usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Listar Minhas Aulas\n2. Listar Todas as Aulas\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuListaAulaPer comando usr

acaoMenuListaAulaPer :: String -> String -> IO ()
acaoMenuListaAulaPer comando usr
    | comando == "1" = do
        listarAulasPersonal usr
        espera
        menuListaAulaPer usr
    | comando == "2" = do 
        listarAulas
        espera
        menuListaAulaPer usr
    | comando == "-" = menuAulasPer usr
    | otherwise = menuListaAulaPer usr

menuCli :: String -> IO ()
menuCli usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Controle Aulas\n2. Listar Minhas Fichas de Trieno\n3. Minhas Avaliações Fisicas\n4. MarcketPlace\n5. Suporte\n6. Perfil\n-. Sair"
    comando <- getLine
    callCommand "clear"
    acaoMenuCli comando usr

acaoMenuCli :: String -> String -> IO ()
acaoMenuCli comando usr
    | comando == "1" = menuAulasCli usr
    | comando == "2" = do
        listarFichaCliente usr
        espera
        menuCli usr
    | comando == "3" = do 
        listarAvaliacoesCliente usr
        espera
        menuCli usr
    | comando == "4" = menuMarcketPlaceCli usr
    | comando == "5" = do 
        texto <- readFile "data/Suporte.txt"
        putStr texto
        espera
        menuCli usr
    | comando == "6" = do
        mostrarPerfil usr
        espera
        menuCli usr
    | comando == "-" = telaInicial
    | otherwise = menuCli usr

menuAulasCli :: String -> IO ()
menuAulasCli usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Entrar em Aula\n2. Sair da Aula\n3. Listar Aulas\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuAulasCli comando usr

acaoMenuAulasCli :: String -> String -> IO ()
acaoMenuAulasCli comando usr
    | comando == "1" = do
        putStr "Id: "
        hFlush stdout
        id_aula <- getLine
        
        mensagem <- adicionarAulaExtra usr id_aula
        putStrLn mensagem
        espera
        menuAulasCli usr
    | comando == "2" = do
        putStr "Id: "
        hFlush stdout
        id_aula <- getLine

        mensagem <- cancelarAula usr id_aula
        putStrLn mensagem
        espera
        menuAulasCli usr
    | comando == "3" = menuAulasListarCli usr
    | comando == "-" = menuCli usr
    | otherwise = menuAulasCli usr

menuAulasListarCli :: String -> IO ()
menuAulasListarCli usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Listar Minhas Aulas\n2. Listar Todas as Aulas\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuAulasListarCli comando usr

acaoMenuAulasListarCli :: String -> String -> IO ()
acaoMenuAulasListarCli comando usr
    | comando == "1" = do
        listarAulasCliente usr
        espera
        menuAulasListarCli usr
    | comando == "2" = do
        listarAulas
        espera
        menuAulasListarCli usr
    | comando == "-" = menuAulasCli usr
    | otherwise = menuAulasListarCli usr

menuMarcketPlaceCli :: String -> IO ()
menuMarcketPlaceCli usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Listar Produtos\n2. Carrinho\n3. Adicionar Produto ao Carrinho\n4. Pagar\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuMarcketPlaceCli comando usr

acaoMenuMarcketPlaceCli :: String -> String -> IO ()
acaoMenuMarcketPlaceCli comando usr
    | comando == "1" = menuMarcketPlaceListarProdCli usr
    | comando == "2" = do
        listarProdutosCarrinho usr
        espera
        menuMarcketPlaceCli usr
    | comando == "3" = do
        putStr "Id: "
        hFlush stdout
        id_prodstr <- getLine
        let id_prod = read id_prodstr :: Int

        mensagem <- adicionaProdutoCarrinho usr id_prod
        putStrLn mensagem
        espera
        menuMarcketPlaceCli usr
    | comando == "4" = do         
        putStrLn "Digite Enter Para Confirmar a Compra\nDigite - Para Voltar"         
        comando <- getLine         
        if comando == "-" then menuMarcketPlaceCli usr        
        else do             
            mensagem <- cadastrarVenda usr   
            putStrLn mensagem          
            espera             
            menuMarcketPlaceCli usr
    | comando == "-" = menuCli usr
    | otherwise = menuMarcketPlaceCli usr

menuMarcketPlaceListarProdCli :: String -> IO()
menuMarcketPlaceListarProdCli usr = do
    putStrLn "Digite O Numero Do Comando A Sua Escolha"
    putStrLn "1. Listar Todos os Produtos\n2. Listar Produto por Categoria\n-. Voltar"
    comando <- getLine
    callCommand "clear"
    acaoMenuMarcketPlaceListarProdCli comando usr

acaoMenuMarcketPlaceListarProdCli :: String -> String -> IO ()
acaoMenuMarcketPlaceListarProdCli comando usr
    | comando == "1" = do 
        listarProdutos
        espera
        menuMarcketPlaceListarProdCli usr
    | comando == "2" = do
        putStr "Categoria: "
        hFlush stdout
        categoria <- getLine
        listarProdutosCategorias categoria
        espera
        menuMarcketPlaceListarProdCli usr
    | comando == "-" = menuMarcketPlaceCli usr
    | otherwise = menuMarcketPlaceListarProdCli usr

espera :: IO ()
espera = do
    _ <- getLine
    callCommand "clear"
    return ()

