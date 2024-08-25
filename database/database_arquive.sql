CREATE TABLE assinatura(
        sigla CHAR(3) PRIMARY KEY,
        valor_mensal REAL,
        valor_semestral REAL,
        valor_anual REAL,
        desconto INTEGER,
        aulas_gratis INTEGER,
        acesso VARCHAR(24)
);

CREATE TABLE usuario(
        matricula CHAR(9) PRIMARY KEY,
        senha VARCHAR(8) NOT NULL,
        tipo_usr CHAR(3) NOT NULL
);

CREATE TABLE cliente(
        matricula CHAR(9) PRIMARY KEY,
        nome VARCHAR(50) NOT NULL,
        idade INTEGER,
        tipo_assinatura CHAR(1) NOT NULL
);

CREATE TABLE personal(
        matricula CHAR(9) PRIMARY KEY,
        nome VARCHAR(50) NOT NULL,
        idade INTEGER,
        salario REAL
);

CREATE TABLE admin(
        matricula CHAR(9) PRIMARY KEY,
        nome VARCHAR(50) NOT NULL ,
        idade INTEGER,
        salario REAL
);

CREATE TABLE vendas_assinatura(
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        matricula CHAR(9) NOT NULL,
        tipo_assinatura CHAR(3) NOT NULL,
        tipo_parcela CHAR(1) NOT NULL,
        parcelas_pagas INTEGER NOT NULL,
        data_inicio CHAR(10) NOT NULL,
        CHECK (tipo_parcela IN ('M', 'S', 'A')),
        FOREIGN KEY(tipo_assinatura) REFERENCES assinatura(sigla),
        FOREIGN KEY(matricula) REFERENCES cliente(matricula)
);

CREATE TABLE loja(
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        nome VARCHAR(60),
        valor REAL NOT NULL,
        descricao VARCHAR(250),
        categorias VARCHAR(50),
        CHECK (valor>=0)
);

CREATE TABLE vendas_loja(
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        produtos TEXT,
        matricula_usr CHAR(9),
        data VARCHAR(16),
        valor_total REAL,
        FOREIGN KEY(matricula_usr) REFERENCES cliente(matricula)
);

CREATE TABLE ficha_treino(
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        matricula_cli CHAR(9),
        matricula_per CHAR(9),
        exercicios TEXT,
        observacoes VARCHAR(250),
        FOREIGN KEY(matricula_cli) REFERENCES cliente(matricula),
        FOREIGN KEY(matricula_per) REFERENCES personal(matricula)
);

CREATE TABLE suporte(
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        matricula_cli CHAR(9) NOT NULL,
        matricula_fun CHAR(9) NOT NULL,
        comentario TEXT,
        FOREIGN KEY(matricula_cli) REFERENCES cliente(matricula)
);

CREATE TABLE avaliacao_fisica(
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        matricula_cli CHAR(9),
        matricula_per CHAR(9),
        avalicao TEXT,
        observacoes TEXT,
        data VARCHAR(10),
        FOREIGN KEY(matricula_cli) REFERENCES cliente(matricula),
        FOREIGN KEY(matricula_per) REFERENCES personal(matricula)
);

-- Formato de data: dd-mm--aa horainicio,horafim EX: 23-08-2004 12:30,13:00
CREATE TABLE aula_extra(
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        materia VARCHAR(40),
        matricula_per CHAR(9),
        data VARCHAR(22) NOT NULL,
        limite INTEGER,
        FOREIGN KEY(matricula_per) REFERENCES personal(matricula)
);

CREATE TABLE clientes_aulas(
        matricula_cli CHAR(9),
        id_aula INTEGER,
        FOREIGN KEY(matricula_cli) REFERENCES cliente(matricula),
        FOREIGN KEY(id_aula) REFERENCES aula_extra(id)
);

INSERT INTO usuario(matricula, senha, tipo_usr) VALUES ('111111111', '12345678', 'ADM');
