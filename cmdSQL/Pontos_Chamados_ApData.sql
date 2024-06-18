Select HFE_CdiContratoItemxPessoa
, PDD_Dssnome
, SUM(Coalesce(BRR_QtiPeso, 1)) Qtd
From ContratosItensxPessoas
inner join PessoasContratos           on (PDD_CdiPessoaContrato      = HFE_CdiPessoaContrato)
left  join ( Select Distinct AHX_CdiChamada
, AHX_CdiContratoItemxPessoa_At
, Coalesce(NullIf(BRR_QtiPeso, 0), 1) BRR_QtiPeso
From Chamadas
Inner Join ChamadasOcorrencias on (AHX_CdiChamada = CHA_CdiChamada)
Inner JOin PesosChamadas on (BRR_CdiPesoChamada = CHA_CdiPesoChamada)
Where AHX_DtdChamadaOcorrencia_Ini  = (Select MIN(M.AHX_DtdChamadaOcorrencia_Ini) From ChamadasOcorrencias M
Where M.AHX_CdiChamada = ChamadasOcorrencias.AHX_CdiChamada and
M.AHX_CdiContratoItemxPessoa_At = ChamadasOcorrencias.AHX_CdiContratoItemxPessoa_At and
M.AHX_CdiContratoItemxPessoa_At in ( 13679, 23265, 30112, 30032, 5019, 20508, 29293, 27995, 35360,
11212, 3949, 26280, 27918, 30288, 13692, 30506) and
M.AHX_CdiOcorrenciaChamada in (32, 71, 30, 1020) and
M.AHX_DtdChamadaOcorrencia_Ini >= '05/01/2023' )
and AHX_CdiOcorrenciaChamada in (32, 71, 30, 1020)
and Coalesce(Month(AHX_DtdChamadaOcorrencia_Ini), 0) = 6
and Coalesce(Year(AHX_DtdChamadaOcorrencia_Ini), 0) = 2024
) ChamadasOcorrencias on (AHX_CdiContratoItemxPessoa_At = HFE_CdiContratoItemxPessoa)
Where HFE_CdiContratoItemxPessoa in ( 13679, 23265, 30112, 30032, 5019, 20508, 29293, 27995, 35360,
11212, 3949, 26280, 27918, 30288, 13692, 30506)
Group By PDD_Dssnome
, HFE_CdiContratoItemxPessoa
Order by PDD_Dssnome
, 3