--USE [XXXXXX]
--GO

/****** Object:  StoredProcedure [dbo].[pr_LimpaTabelasIntermediarias]    Script Date: 29/03/2018 09:17:22 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[pr_LimpaTabelasIntermediarias] as 
--DELETE CONTROLE PROCESSOS MENSAGERIA
DELETE FROM ProcseSocEventos
DELETE FROM ProcseSocPreparacaoLotes
DELETE FROM ProcseSocTransmissaoLotes
--DELETE LOTES
DELETE FROM LotesEnvioseSocialEvOcorr
DELETE FROM LotesEnvioseSocialEventos
DELETE FROM LotesEnvioseSocialOcorr
DELETE FROM LotesEnvioseSocial
--DELETA /* Evento S-1000 */
DELETE FROM EmpregadoreseSFornsSistInf WHERE SMI_CdiEmpregadoreSFornSistInf > 0;
DELETE FROM EmpregadoreseS WHERE SGC_CdiEmpregadoreS > 0;
--DELETA /* Evento S-1005 */
DELETE FROM EstabelecseSocialEntsEds WHERE IFR_CdiEstabeleceSocialEntEd > 0;
DELETE FROM EstabelecseSocial WHERE SGJ_CdiEstabeleceSocial > 0;
--DELETA /* Evento S-1010 */
DELETE FROM RubricaseSocialProcsCSind WHERE ICH_CdiRubricaeSocialProcCSind > 0;
DELETE FROM RubricaseSocialProcsFGTS WHERE ICG_CdiRubricaeSocialProcFGTS > 0;
DELETE FROM RubricaseSocialProcsIRRF WHERE ICB_CdiRubricaeSocialProcIRRF > 0;
DELETE FROM RubricaseSocialProcsCP WHERE IBZ_CdiRubricaeSocialProcCP > 0;
DELETE FROM RubricaseSocial WHERE SGD_CdiRubricaeSocial > 0;
--DELETA /* Evento S-1020 */
DELETE FROM LotacoeseSocialItens WHERE FHG_CdiLotacaoeSocialItem > 0;
DELETE FROM LotacoeseSocial WHERE SGE_CdiLotacaoeSocial > 0;
--DELETA /* Evento S-1030 */
DELETE FROM CargoseSocial WHERE SGF_CdiCargoeSocial > 0;
--DELETA /* Evento S-1035 */
DELETE FROM CarreirasPublicaseSocial WHERE IBY_CdiCarreiraPublicaeSocial > 0;
--DELETA /* Evento S-1040 */
DELETE FROM FuncoeseSocial WHERE SGG_CdiFuncaoeSocial > 0;
--DELETA /* Evento S-1050 */
DELETE FROM JornadaseSocialItens WHERE SGI_CdiJornadaeSocialItem > 0;
DELETE FROM JornadaseSocial WHERE SGH_CdiJornadaeSocial > 0;
--DELETA /* Evento S-1060 */
DELETE FROM AmbientesTrabeSocialRiscos WHERE FKN_CdiAmbienteTrabeSocialRisc > 0;
DELETE FROM AmbientesTrabeSocial WHERE FKM_CdiAmbienteTrabeSocial > 0;
--DELETA /* Evento S-1070 */
DELETE FROM ProcessoseSocialSusps WHERE IDV_CdiProcessoeSocialSusp > 0;
DELETE FROM ProcessoseSocial WHERE SGK_CdiProcessoeSocial > 0;
--DELETA /* Evento S-1080 */
DELETE FROM OpsPortuarioseSocial WHERE SGL_CdiOpPortuarioeSocial > 0;
--DELETA /* Evento S-1200 */
DELETE FROM RemseSocRecsDDPsEstsRemsIs WHERE SGX_CdiRemeSocRecDDPEstRemI > 0;
DELETE FROM RemseSocRecsDDPsEstsRems WHERE SGW_CdiRemeSocRecDDPEstRem > 0;
DELETE FROM RemseSocRecsDDPsEsts WHERE SGV_CdiRemeSocRecDDPEst > 0;
DELETE FROM RemseSocRecsDDPs WHERE SGU_CdiRemeSocRecDDP > 0;
DELETE FROM RemseSocRecsDDs WHERE SGT_CdiRemeSocRecDD > 0;
DELETE FROM RemseSocRecsEstsRemsDsMsDs WHERE FJC_CdiRemeSocRecEstRemDMD > 0;
DELETE FROM RemseSocRecsEstsRemsDsMs WHERE FJB_CdiRemeSocRecEstRemDM > 0;
DELETE FROM RemseSocRecsEstsRemsIts WHERE SGS_CdiRemeSocRecEstRemIt > 0;
DELETE FROM RemseSocRecsEstsRems WHERE SGR_CdiRemeSocRecEstRem > 0;
DELETE FROM RemseSocRecsEsts WHERE SGQ_CdiRemeSocRecEst > 0;
DELETE FROM RemseSocRecs WHERE FIZ_CdiRemeSocRec > 0;
DELETE FROM RemseSocProcessosJudiciais WHERE EOI_CdiRemeSocProcessoJudicial > 0;
DELETE FROM RemseSocMultsVincs WHERE SGO_CdiRemeSocMultVinc > 0;
DELETE FROM RemseSoc WHERE SGN_CdiRemeSoc > 0;
--DELETA /* Evento S-1202 */
DELETE FROM RemsRPPSeSRecsDDsPsEsRsIts WHERE FOV_CdiRemRPPSeSRecDDPERIt > 0;
DELETE FROM RemsRPPSeSRecsDDsPsEsRs WHERE FOX_CdiRemRPPSeSRecDDPER > 0;
DELETE FROM RemsRPPSeSRecsDDsPsEs WHERE ICZ_CdiRemRPPSeSRecDDPE > 0;
DELETE FROM RemsRPPSeSRecsDDsPs WHERE FOU_CdiRemRPPSeSRecDDP > 0;
DELETE FROM RemsRPPSeSRecsDDs WHERE FOT_CdiRemRPPSeSRecDD > 0;
DELETE FROM RemsRPPSeSRecsEstsRsDsMsDs WHERE FOP_CdiRemRPPSeSRecEstRDMD > 0;
DELETE FROM RemsRPPSeSRecsEstsRsDsMs WHERE FON_CdiRemRPPSeSRecEstRDM > 0;
DELETE FROM RemsRPPSeSRecsEstsRsIts WHERE FOM_CdiRemRPPSeSRecEstRIt > 0;
DELETE FROM RemsRPPSeSRecsEstsRs WHERE FOK_CdiRemRPPSeSRecEstR > 0;
DELETE FROM RemsRPPSeSRecsEsts WHERE IDD_CdiRemRPPSeSRecEst > 0;
DELETE FROM RemsRPPSeSRecs WHERE FOJ_CdiRemRPPSeSRec > 0;
DELETE FROM RemsRPPSeSProcsJudiciais WHERE FOI_CdiRemRPPSeSProcJudicial > 0;
DELETE FROM RemsRPPSeS WHERE FOH_CdiRemRPPSeS > 0;
--DELETA /* Evento S-1210 */
DELETE FROM PagseSPagsCompsAntsValores WHERE ICY_CdiPageSPagCompAntValor > 0;
DELETE FROM PagseSPagsCompsAnts WHERE ICX_CdiPageSPagCompAnt > 0;
DELETE FROM PagseSPagsFeriasRubricasPA WHERE ICW_CdiPageSPagFeriasRubricaPA > 0;
DELETE FROM PagseSPagsFeriasRubricas WHERE ICU_CdiPageSPagFeriasRubrica > 0;
DELETE FROM PagseSPagsFerias WHERE ICQ_CdiPageSPagFerias > 0;
DELETE FROM PagseSPagsBsPrevsRetsParc WHERE ICO_CdiPageSPagBPrevRetParc > 0;
DELETE FROM PagseSPagsBsPrevsRetsTot WHERE ICM_CdiPageSPagBPrevRetTot > 0;
DELETE FROM PagseSPagsNormaisRetsParc WHERE ICL_CdiPageSPagNormalRetParc > 0;
DELETE FROM PagseSPagsNormaisRetsTotPA WHERE ICK_CdiPageSPagNormalRetTotPA > 0;
DELETE FROM PagseSPagsNormaisRetsTot WHERE ICJ_CdiPageSPagNormalRetTot > 0;
DELETE FROM PagseSPagsNormais WHERE EOY_CdiPageSPagNormal > 0;
DELETE FROM PagseSPags WHERE SGZ_CdiPageSPag > 0;
DELETE FROM PagseS WHERE SGY_CdiPageS > 0;
--DELETA /* Evento S-1220 */
DELETE FROM BenefsNaoIdentifseSocsIts WHERE FJY_CdiBenefNaoIdentifeSocIt > 0;
DELETE FROM BenefsNaoIdentifseSocs WHERE FJX_CdiBenefNaoIdentifeSoc > 0;
--DELETA /* Evento S-1250 */
DELETE FROM AquissPreSTpsTotsProcsJuds WHERE HTF_CdiAquisPreSTpTotProcJud > 0;
DELETE FROM AquissPreSTpsTotsIts WHERE SHV_CdiAquisPreSTpTotIt > 0;
DELETE FROM AquissPreSTpsTots WHERE SHU_CdiAquisPreSTpTot > 0;
DELETE FROM AquissPreSTps WHERE EVJ_CdiAquisPreSTp > 0;
DELETE FROM AquissPreS WHERE SHS_CdiAquisPreS > 0;
--DELETA /* Evento S-1260 */
DELETE FROM ComercsProdeSocTpsPrs WHERE FNU_CdiComercProdeSocTpPr > 0;
DELETE FROM ComercsProdeSocTpsIsNFs WHERE IDX_CdiComercProdeSocTpItNF > 0;
DELETE FROM ComercsProdeSocTpsIs WHERE SHY_CdiComercProdeSocTpIt > 0;
DELETE FROM ComercsProdeSocTps WHERE EVK_CdiComercProdeSocTp > 0;
DELETE FROM ComercsProdeSoc WHERE SHW_CdiComercProdeSoc > 0;
--DELETA /* Evento S-1270 */
DELETE FROM TrabsAvulsosNPseSEstabs WHERE FJN_CdiTrabAvulsoNPeSEstab > 0;
DELETE FROM TrabsAvulsosNPseS WHERE FJH_CdiTrabAvulsoNPeS > 0;
--DELETA /* Evento S-1280 */
DELETE FROM TrabsAvulsosNPseSEstabs WHERE FJN_CdiTrabAvulsoNPeSEstab > 0;
DELETE FROM TrabsAvulsosNPseS WHERE FJH_CdiTrabAvulsoNPeS > 0;
--DELETA /* Evento S-1298 */
DELETE FROM AberturaMensaleSocial WHERE SGM_CdiAberturaMensaleSocial > 0;
--DELETA /* Evento S-1299 */
DELETE FROM FechamentoMensaleSocial WHERE EQT_CdiFechamentoMensaleSocial > 0;
--DELETA /* Evento S-1300 */
DELETE FROM ContrsSindsPatrseSocialIts WHERE FJU_CdiContrSindPatreSocialIt > 0;
DELETE FROM ContrsSindsPatrseSocial WHERE FJT_CdiContrSindPatreSocial > 0;
--DELETA /* Evento S-2100 */
DELETE FROM ContratadoseSocFilsSinds WHERE IFS_CdiContratadoeSocFilSind > 0;
DELETE FROM ContratadoseSocJornadas WHERE EQN_CdiContratadoeSocJorn > 0;
DELETE FROM ContratadoseSocSubstsTemps WHERE IDE_CdiContratadoeSocSubstTemp > 0;
DELETE FROM ContratadoseSocDepends WHERE SJF_CdiContratadoeSocDepend > 0;
DELETE FROM ContratadoseSocObservs WHERE IQD_CdiContratadoeSocObserv > 0;
DELETE FROM ContratadoseSoc WHERE SJE_CdiContratadoeSoc > 0;
--DELETA /* Evento S-2190 */
DELETE FROM ContratadosPrelimseSoc WHERE FIP_CdiContratadoPrelimeSoc > 0;
--DELETA /* Evento S-2200 */
DELETE FROM ContratadoseSocFilsSinds WHERE IFS_CdiContratadoeSocFilSind > 0;
DELETE FROM ContratadoseSocJornadas WHERE EQN_CdiContratadoeSocJorn > 0;
DELETE FROM ContratadoseSocSubstsTemps WHERE IDE_CdiContratadoeSocSubstTemp > 0;
DELETE FROM ContratadoseSocDepends WHERE SJF_CdiContratadoeSocDepend > 0;
DELETE FROM ContratadoseSoc WHERE SJE_CdiContratadoeSoc > 0;
--DELETA /* Evento S-2205 */
DELETE FROM AlteracoesCadastrseSocDeps WHERE SJI_CdiAlteracaoCadastreSocDep > 0;
DELETE FROM AlteracoesCadastrseSoc WHERE SJH_CdiAlteracaoCadastreSoc > 0;
--DELETA /* Evento S-2206 */
DELETE FROM AltsContratuaiseSocFsSinds WHERE IFT_CdiAltContratualSocFSind > 0;
DELETE FROM AltsContratuaiseSocJorns WHERE EQM_CdiAltContratualeSocJorn > 0;
DELETE FROM AltsContratuaiseSoc WHERE SJJ_CdiAltContratualeSoc > 0;
--DELETA /* Evento S-2210 */
DELETE FROM CATseSocialPartesCorpos WHERE EUU_CdiCATeSocialParteCorpo > 0;
DELETE FROM CATseSocialAgentes WHERE EUV_CdiCATeSocialAgente > 0;
DELETE FROM CATseSocial WHERE SJK_CdiCATeSocial > 0;
--DELETA /* Evento S-2220 */
DELETE FROM ASOseSocialExames WHERE SJN_CdiASOeSocialExame > 0;
DELETE FROM ASOseSocial WHERE SJM_CdiASOeSocial > 0;
--DELETA /* Evento S-2230 */
DELETE FROM AfastamentoseSocial WHERE SJQ_CdiAfastamentoeSocial > 0;
--DELETA /* Evento S-2240 */
DELETE FROM CondsAmbseSRespsRegsAmbs WHERE FLJ_CdiCondAmbeSRespRegAmb > 0;
DELETE FROM CondsAmbseSAmbsRsEPIs WHERE SJX_CdiCondAmbeSAmbREPI > 0;
DELETE FROM CondsAmbseSAmbsRsEPCs WHERE IEA_CdiCondAmbeSAmbREPC > 0;
DELETE FROM CondsAmbseSAmbsRs WHERE SJW_CdiCondAmbeSAmbR > 0;
DELETE FROM CondsAmbseSAmbs WHERE FLI_CdiCondAmbeSAmb > 0;
DELETE FROM CondsAmbseS WHERE SJV_CdiCondAmbeS > 0;
--DELETA /* Evento S-2241 */
DELETE FROM ExpseSocApEspAmbsRiscos WHERE FLD_CdiExpeSocApEspAmbRisco > 0;
DELETE FROM ExpseSocApEspAmbs WHERE FLB_CdiExpeSocApEspAmb > 0;
DELETE FROM ExpseSocInsPerAmbsRiscos WHERE FLA_CdiExpeSocInsPerAmbRisco > 0;
DELETE FROM ExpseSocInsPerAmbs WHERE FKZ_CdiExpeSocInsPerAmb > 0;
DELETE FROM ExpseSoc WHERE FKW_CdiExpeSoc > 0;
--DELETA /* Evento S-2250 */
DELETE FROM ComunicsAvisoPrevioeSoc WHERE SKA_CdiComunicAvisoPrevioeSoc > 0;
--DELETA /* Evento S-2298 */
DELETE FROM ReintegracoeseSocial WHERE SKM_CdiReintegracaoeSocial > 0;
--DELETA /* Evento S-2299 */
DELETE FROM ConsComVincTeSMultsVincs WHERE IDO_CdiConComVincTeSMulVinc > 0;
DELETE FROM ConsComVincTeSProcs WHERE FNZ_CdiConComVincTeSProc > 0;
DELETE FROM ConsComVincTeSRsDDsPsEsIts WHERE IDN_CdiConComVincTeSRDDPEIt > 0;
DELETE FROM ConsComVincTeSRsDDsPsEs WHERE IDM_CdiConComVincTeSRDDPE > 0;
DELETE FROM ConsComVincTeSRsDDsPs WHERE IDL_CdiConComVincTeSRDDP > 0;
DELETE FROM ConsComVincTeSRsDDs WHERE IDK_CdiConComVincTeSRDD > 0;
DELETE FROM ConsComVincTeSRsEstsDsMsDs WHERE IDJ_CdiConComVincTeSREstDMD > 0;
DELETE FROM ConsComVincTeSRsEstsDsMs WHERE IDH_CdiConComVincTeSREstDM > 0;
DELETE FROM ConsComVincTeSRsEstsIts WHERE SKL_CdiConComVincTeSREstIt > 0;
DELETE FROM ConsComVincTeSRsEsts WHERE FNY_CdiConComVincTeSREst > 0;
DELETE FROM ConsComVincTeSRs WHERE FIW_CdiConComVincTeSR > 0;
DELETE FROM ConsComVincTeS WHERE SKK_CdiConComVincTeS > 0;
--DELETA /* Evento S-2300 */
DELETE FROM ContrsSemVinculoeSocDeps WHERE SKG_CdiContrSemVinculoeSocDep > 0;
DELETE FROM ContrsSemVinculoeSoc WHERE SKF_CdiContrSemVinculoeSoc > 0;
--DELETA /* Evento S-2306 */
DELETE FROM ContrsSemVincAltContreS WHERE SKH_CdiContrSemVincAltContreS > 0;
--DELETA /* Evento S-2399 */
DELETE FROM ConsSemVincTeSMultsVincs WHERE IDU_CdiConSemVincTeSMulVinc > 0;
DELETE FROM ConsSemVincTeSProcs WHERE IDT_CdiConSemVincTeProc > 0;
DELETE FROM ConsSemVincTeSRsEstsDsMsDs WHERE IDR_CdiConSemVincTeSREstDMD > 0;
DELETE FROM ConsSemVincTeSRsEstsDsMs WHERE IDQ_CdiConSemVincTeSREstDM > 0;
DELETE FROM ConsSemVincTeSRsEstsIts WHERE SKJ_CdiConSemVincTeSREstIt > 0;
DELETE FROM ConsSemVincTeSRsEsts WHERE FOB_CdiConSemVincTeSREst > 0;
DELETE FROM ConsSemVincTeSRs WHERE FIX_CdiConSemVincTeSR > 0;
DELETE FROM ConsSemVincTeS WHERE SKI_CdiConSemVincTeS > 0;
--DELETA /* Evento S-3000 */
DELETE FROM ExclusoesEventoseSocial WHERE SKN_CdiExclusaoEventoeSocial > 0;
--DELETA /* Evento S-5001 */
DELETE FROM RetseSCSTrabsEstsTrabsTs WHERE FPW_CdiReteSCSTrabEstTrabT > 0;
DELETE FROM RetseSCSTrabsEstsTrabsIts WHERE FPV_CdiReteSCSTrabEstTrabIt > 0;
DELETE FROM RetseSCSTrabsEstsTrabs WHERE FPU_CdiReteSCSTrabEstTrab > 0;
DELETE FROM RetseSCSTrabsEsts WHERE FPQ_CdiReteSCSTrabEst > 0;
DELETE FROM RetseSCSTrabsTpsCalcs WHERE FPN_CdiReteSCSTrabTpCalc > 0;
DELETE FROM RetseSCSTrabsProcsJuds WHERE FPM_CdiReteSCSTrabProcJud > 0;
DELETE FROM RetseSCSTrabs WHERE FPL_CdiReteSCSTrab > 0;
--DELETA /* Evento S-5002 */
DELETE FROM RetseSIRTrabsCategsTpsRets WHERE FQB_CdiReteSIRTrabCategTPRet > 0;
DELETE FROM RetseSIRTrabsCategsTpsVals WHERE FQA_CdiReteSIRTrabCategTpVal > 0;
DELETE FROM RetseSIRTrabsCategs WHERE FPZ_CdiReteSIRTrabCateg > 0;
DELETE FROM RetseSIRTrabs WHERE FPY_CdiReteSIRTrab > 0;
--DELETA /* Evento S-5011 */
DELETE FROM RetseSCSContsVlsConss WHERE FQK_CdiReteSCSContVlCons > 0;
DELETE FROM RetseSCSContsEstsVlsConss WHERE FQJ_CdiReteSCSContEstVlCons > 0;
DELETE FROM RetseSCSContsEstsAPRs WHERE ILW_CdiReteSCSContEstAPR > 0;
DELETE FROM RetseSCSContsEstsCPRs WHERE FQI_CdiReteSCSContEstCPR > 0;
DELETE FROM RetseSCSContsEstsLotsOGMOs WHERE FQH_CdiReteSCSContEstLotOGMO > 0;
DELETE FROM RetseSCSContsEstsLotsIncs WHERE FQG_CdiReteSCSContEstLotIncid > 0;
DELETE FROM RetseSCSContsEstsLotsSsTs WHERE FQF_CdiReteSCSContEstLotsST > 0;
DELETE FROM RetseSCSContsEstsLots WHERE FQE_CdiReteSCSContEstLot > 0;
DELETE FROM RetseSCSContsEsts WHERE FQD_CdiReteSCSContEst > 0;
DELETE FROM RetseSCSConts WHERE FQC_CdiReteSCSCont > 0;
--DELETA /* Evento S-5012 */
DELETE FROM ReteSIRContsTpsRets WHERE FQM_CdiReteSIRContTpRet > 0;
DELETE FROM ReteSIRConts WHERE FQL_CdiReteSIRCont > 0;
GO


