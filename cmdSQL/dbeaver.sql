SELECT ID, PT_NOME, DESCRICAO, PALAVRAS_CHAVE, DATA_INICIO, DATA_FINAL FROM CHAMADOS c ORDER BY 1 DESC;	

SELECT * FROM DESVIOS d ORDER BY 1 DESC;

SELECT * FROM CHAMADOS c ORDER BY 1 DESC; 

SELECT * FROM FEATURE_CRIADAS fc ORDER BY 1 DESC;

INSERT INTO FEATURE_CRIADAS (NAME_FEATURE, NAME_BRANCH, SHA_ORIGEM, REGEX_SEARCH, DESC_PT, NAME_PT, TYPE_FEATURE, DATETIME_FIELD) 
VALUES ('PT_550463_MR', 'V559', 'ebc5d8a6', 'Login By Ticket', ' LoginByTicket (Utilizando Direct)', '550463', 'MR', '2020-06-18 19:07:00.000')

/***************************************************************************************************************************/

SELECT * FROM FEATURE_CRIADAS fc WHERE REGEX_SEARCH LIKE '%OR,%'

 


