CREATE TABLE assinatura(
	sigla CHAR(3) PRIMARY KEY,
	valor_mensal REAL,
	valor_semestral REAL,
	valor_anual REAL,
	desconto INTEGER,
	aulas_gratis INTEGER,
	acesso TEXT,
);

CREATE TABLE aluno(
	matricula INTEGER PRIMARY KEY AUTOINCREMENT,
	nome VARCHAR(200) NOT NULL,
	email VARCHAR(300),
	tipo_assinatura CHAR(3),
	situacao_assinatura VARCHAR(8) NOT NULL,
	CHECK (email LIKE '%@%'),
	CHECK (situacao_assinatura IN ('PENDENTE', 'PAGO')),
	FOREIGN KEY(tipo_assinatura) REFERENCES assinatura(sigla)
);

INSERT INTO alunos(nome, email, tipo_assinatura, situacao_assinatura) VALUES(
	'Eurico Gabriel Vasconcelos Pereira',
	'euricogabriel149@gmail.com',
	'GOLD',
	'PAGO'
);

