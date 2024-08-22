SELECT banner FROM v$version WHERE banner LIKE 'Oracle%';

SELECT * FROM USUARIOS u WHERE USR_CDIUSUARIO = 1
SELECT * FROM USUARIOSDESABILITACOES u2 WHERE USD_CDIUSUARIO = 1

/*Alteraçoes - Realizadas*/
UPDATE USUARIOS SET USD_CDIUSUARIO = 0 WHERE USD_CDIUSUARIODESABILITACAO =  3513

/*Reversões - Realizadas*/
UPDATE USUARIOS SET USD_CDIUSUARIO = 1 WHERE USD_CDIUSUARIODESABILITACAO =  3513

/*Outros Comandos*/

select *
  from ConCCustosDisciplinas 
  where PFM_CdiContratado = 1 
  and PFM_CdiCentroCusto = 1