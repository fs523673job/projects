Update LancamentosWF Set LWF_CdiEtapaWorkflow = 1153, LWF_OplEfetivado = 0, LWF_OplFinalizado = 0 Where LWF_CdiLancamentoWF in (3581)
Delete PendenciasWFTransacoesUsus  Where PFU_CdiLancamentoWF in (3581)
Delete PendenciasWFTransacoes  Where PFT_CdiLancamentoWF in (3581)

insert into PendenciasWFTransacoesUsus	values	(3581,1014728,30)
insert into PendenciasWFTransacoes	values	(3581,1014728,'2022/01/28 17:00')
