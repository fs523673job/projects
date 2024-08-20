SELECT * FROM USER_TABLES

/*Verificar a versão do Oracle*/
SELECT * FROM v$version;
SELECT banner FROM v$version WHERE banner LIKE 'Oracle%';
SELECT version FROM v$instance;


/* Criação de Usuários */

CREATE USER C##apdata IDENTIFIED BY apdata;
GRANT CONNECT TO C##apdata;
GRANT RESOURCE TO C##apdata;
GRANT CREATE ANY VIEW TO C##apdata;
GRANT CREATE VIEW TO C##apdata;
GRANT UNLIMITED TABLESPACE TO C##apdata;
GRANT EXECUTE ON DBMS_RLS TO C##apdata WITH GRANT OPTION;
ALTER USER C##apdata ACCOUNT UNLOCK;
ALTER USER C##apdata IDENTIFIED BY apdata;
