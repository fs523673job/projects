SELECT ID, PT_NOME, DESCRICAO, PALAVRAS_CHAVE, DATA_INICIO, DATA_FINAL FROM CHAMADOS c ORDER BY 1 DESC;	

SELECT * FROM DESVIOS d ORDER BY 1 DESC;

SELECT * FROM CHAMADOS c ORDER BY 1 DESC; 

SELECT * FROM FEATURE_CRIADAS fc ORDER BY 1 DESC;

SELECT * FROM FEATURE_CRIADAS fc WHERE fc.LINK_MR <> '' ORDER BY 1 DESC;

SELECT * FROM FEATURE_CRIADAS WHERE REGEX_SEARCH  LIKE '%Cifras%'

SELECT * FROM FEATURE_CRIADAS WHERE NAME_PT LIKE '%874359%'

SELECT * FROM FEATURE_CRIADAS WHERE DATETIME_FIELD BETWEEN '2025-07-21' AND '2025-07-25'

SELECT DISTINCT PT_NAME, DESC_PT FROM FEATURE_CRIADAS WHERE DATETIME_FIELD BETWEEN '2025-07-28' AND '2025-08-02'

--INSERT INTO FEATURE_CRIADAS (NAME_FEATURE, NAME_BRANCH, SHA_ORIGEM, REGEX_SEARCH, DESC_PT, NAME_PT, TYPE_FEATURE, DATETIME_FIELD) 
--VALUES ('PT_831437_MR', 'Liberacao_559', '', 'Access Violation, Portal do Candidato, Workflow', 'Mensagem de access violation quando o candidato finaliza o lançamento no portal', '831437', 'MR', '2024-11-25 16:51:00.000')

--DELETE FROM FEATURE_CRIADAS WHERE ID = 305

--UPDATE FEATURE_CRIADAS fc SET LINK_MR = 'https://gitlab.com/apdata/global-antares/-/merge_requests/2232' WHERE ID = 307

/***************************************************************************************************************************/

SELECT * FROM FEATURE_CRIADAS fc WHERE REGEX_SEARCH LIKE '%823321%'
SELECT * FROM FEATURE_CRIADAS fc WHERE NAME_PT LIKE '%823321%'

UPDATE FEATURE_CRIADAS fc SET LINK_MR = 'https://gitlab.com/apdata/global-antares/-/merge_requests/2983' WHERE SHA_ORIGEM = '3e07dfe4f21'
UPDATE FEATURE_CRIADAS fc SET STATUS_MR = 'Merged' WHERE ID in (183, 184, 185, 186, 187, 188)
UPDATE FEATURE_CRIADAS SET NAME_FEATURE = 'PT_864782_MR' WHERE ID = 252 
 
/***************************************************************************************************************************/




