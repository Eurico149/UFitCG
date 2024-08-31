-- Formato acesso: hh:mm hh:mm,hh:mm hh:mm 
CREATE TABLE assinatura(
        sigla CHAR(3) PRIMARY KEY,
        valor_mensal REAL,
        valor_semestral REAL,
        valor_anual REAL,
        desconto INTEGER,
        aulas_gratis INTEGER,
        acesso VARCHAR(23),
        CHECK (aulas_gratis >= 0),
        CHECK (desconto >= 0),
        CHECK (length(sigla) = 3)
);

-- Formato data_nascimento: dd-mm-aaaa 
CREATE TABLE usuario(
        usr VARCHAR(40) PRIMARY KEY,
        senha CHAR(8) NOT NULL,
        tipo_usr CHAR(3) NOT NULL,
        nome VARCHAR(50) NOT NULL,
        data_nascimento VARCHAR(10),
        tipo_assinatura CHAR(3),
        salario REAL,
        CHECK (salario >= 0),
        CHECK (length(senha) = 8),
        CHECK (tipo_usr in ('ADM', 'PER', 'CLI')),
        FOREIGN KEY(tipo_assinatura) REFERENCES assinatura(sigla)
);

-- Formato data: dd-mm-aaaa
CREATE TABLE vendas_assinatura(
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        usr VARCHAR(40) NOT NULL,
        tipo_assinatura CHAR(3) NOT NULL,
        tipo_parcela CHAR(1) NOT NULL,
        parcelas_pagas INTEGER NOT NULL,
        data_inicio CHAR(10) NOT NULL,
        CHECK (tipo_parcela IN ('M', 'S', 'A')),
        CHECK (length(tipo_assinatura) = 3),
        FOREIGN KEY(tipo_assinatura) REFERENCES assinatura(sigla),
        FOREIGN KEY(usr) REFERENCES usuario(usr)
);


-- Formato categorias: bebida suplemento barra_proteina,...
CREATE TABLE loja(
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        nome VARCHAR(60) NOT NULL,
        valor REAL NOT NULL,
        descricao VARCHAR(250),
        categorias VARCHAR(50),
        CHECK (valor>=0)
);

CREATE TABLE carrinho(
        usr VARCHAR(40),
        id_prod INTEGER,
        FOREIGN KEY(usr) REFERENCES usuario(usr),
        FOREIGN KEY(id_prod) REFERENCES loja(id)
);

-- Formato produtos: id_produto1 id_produto2 ...
-- Formato data: dd-mm-aaaa hh:mm
CREATE TABLE vendas_loja(
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        produtos TEXT,
        usr VARCHAR(40) NOT NULL,
        data_horario VARCHAR(16),
        valor_total REAL NOT NULL,
        CHECK (valor_total >= 0),
        FOREIGN KEY(usr) REFERENCES usuario(usr)
);

-- Formato exercicios: nome_exercicio seriesXrepeticoes,...
CREATE TABLE ficha_treino(
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        usr_cli VARCHAR(40) NOT NULL,
        usr_per VARCHAR(40) NOT NULL,
        exercicios TEXT,
        observacoes VARCHAR(250),
        FOREIGN KEY(usr_cli) REFERENCES usuario(usr),
        FOREIGN KEY(usr_per) REFERENCES usuario(usr)
);

-- Formato data: dd/mm/aaaa
-- formato avalicao: parte_corpo medida,gordura_corporal num%,...
CREATE TABLE avaliacao_fisica(
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        usr_cli VARCHAR(40) NOT NULL,
        usr_per VARCHAR(40) NOT NULL,
        avalicao TEXT,
        observacoes TEXT,
        data_ava VARCHAR(10),
        FOREIGN KEY(usr_cli) REFERENCES usuario(usr),
        FOREIGN KEY(usr_per) REFERENCES usuario(usr)
);

-- Formato de data: dd-mm--aa horainicio,horafim EX: 23-08-2004 12:30,13:00
CREATE TABLE aula_extra(
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        materia VARCHAR(40) NOT NULL,
        usr_per VARCHAR(40) NOT NULL,
        data_horario VARCHAR(22),
        limite INTEGER,
        CHECK (limite > 0),
        FOREIGN KEY(usr_per) REFERENCES usuario(usr)
);

CREATE TABLE clientes_aulas(
        usr_cli VARCHAR(40) NOT NULL,
        id_aula INTEGER,
        FOREIGN KEY(usr_cli) REFERENCES usuario(usr),
        FOREIGN KEY(id_aula) REFERENCES aula_extra(id)
);

INSERT INTO usuario(usr, senha, tipo_usr, nome, data_nascimento, tipo_assinatura, salario) VALUES ('eurico', '12345678', 'ADM', 'Eurico Gabriel Vasconcelos Pereira', '13/02/2000', '', 3500.00);
