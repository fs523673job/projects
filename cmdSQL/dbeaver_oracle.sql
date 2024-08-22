/* Testando a instalação */
SELECT * FROM USER_TABLES

/*Verificar a versão do Oracle*/
SELECT * FROM v$version;
SELECT banner FROM v$version WHERE banner LIKE 'Oracle%';
SELECT version FROM v$instance;

/* Criação de Usuários */
DROP ROLE C##APDATA;
DROP ROLE C##Apdata;


CREATE USER C##apdata IDENTIFIED BY apdata;
GRANT CONNECT TO C##apdata;
GRANT RESOURCE TO C##apdata;
GRANT CREATE ANY VIEW TO C##apdata;
GRANT CREATE VIEW TO C##apdata;
GRANT UNLIMITED TABLESPACE TO C##apdata;
GRANT EXECUTE ON DBMS_RLS TO C##apdata WITH GRANT OPTION;
ALTER USER C##apdata ACCOUNT UNLOCK;
ALTER USER C##apdata IDENTIFIED BY apdata;

COMMIT;

SELECT * FROM dba_users
SELECT username FROM dba_users WHERE username = 'C##APDATA';


/* Usuários */

SELECT * FROM user_users;


/* Verificar data oracle*/
SELECT value FROM v$nls_parameters WHERE parameter ='NLS_DATE_FORMAT';
SELECT * FROM NLS_DATABASE_PARAMETERS p WHERE p.PARAMETER = 'NLS_DATE_FORMAT';

/* Verificar nome do database*/
SELECT NAME, OPEN_MODE FROM v$database;


/* Comandos Base ApData Normais */

SELECT * FROM USUARIOS u 

SELECT AVQ_DtdTesteBancoDados FROM DefSisConfiguracoesGerais WHERE AVQ_CdiSistema = 1
SELECT * FROM DefSisConfiguracoesGerais WHERE AVQ_CdiSistema = 1

DROP TABLE CONTRSSINDSPATRSESOCIALITS
DROP TABLE LANCAMENTOSCONTABEISB1
DROP TABLE CONULTIMOSCALCULOS 
DROP TABLE CONVARSSALARIAIS