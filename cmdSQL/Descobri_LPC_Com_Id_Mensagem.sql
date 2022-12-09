SELECT AIC_CdiCalculoLPC,ACL_D1sCalculoLPC FROM CalculosLPCItens
INNER JOIN TransacoesCalcsLPCCFlexs on (TCX_CdiCalculoLPC = AIC_CdiCalculoLPC)
INNER JOIN FormulariosWF on (TCX_CdiTransacao = FWF_CdiTransacao)
INNER JOIN CalculosLPC on (ACL_CdiCalculoLPC = AIC_CdiCalculoLPC)
WHERE AIC_CdiOperacaoLPC in(11)
AND AIC_NuiCampoLPCF2 in(68) /*Id_Mensagem*/
AND FWF_CdiFormularioWF in (286) /*Id_Formulario_Workflow*/

--Transacao (20702) Calculos (409, 1284)
--(401, 409, 1276, 1284)