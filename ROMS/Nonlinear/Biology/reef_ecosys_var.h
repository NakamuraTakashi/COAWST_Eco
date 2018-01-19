/*
** svn $Id: fennel_var.h 618 2012-05-17 20:12:51Z arango $
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2012 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
**  Assigns metadata indices for the Fennel et al. (2006) ecosystem   **
**  model variables that are used in input and output NetCDF files.   **
**  The metadata information is read from "varinfo.dat".              **
**                                                                    **
**  This file is included in file "mod_ncparam.F", routine            **
**  "initialize_ncparm".                                              **
**                                                                    **
************************************************************************
*/

/*
**  Model state biological tracers.
*/

!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add

              CASE ('idCrl1')
                idCrl1=varid
              CASE ('idCrl2')
                idCrl2=varid
              CASE ('idSgrs')
                idSgrs=varid
              CASE ('idAlga')
                idAlga=varid
              CASE ('idSand')
                idAlga=varid
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
              CASE ('idTvar(iTIC_)')
                idTvar(iTIC_)=varid
              CASE ('idTvar(iTAlk)')
                idTvar(iTAlk)=varid
              CASE ('idTvar(iOxyg)')
                idTvar(iOxyg)=varid
#if defined ORGANIC_MATTER
              CASE ('idTvar(iDOC_)')
                idTvar(iDOC_)=varid
              CASE ('idTvar(iPOC_)')
                idTvar(iPOC_)=varid
              CASE ('idTvar(iPhy1)')
                idTvar(iPhy1)=varid
              CASE ('idTvar(iPhy2)')
                idTvar(iPhy2)=varid
              CASE ('idTvar(iZoop)')
                idTvar(iZoop)=varid
#endif
#if defined CARBON_ISOTOPE
              CASE ('idTvar(iT13C)')
                idTvar(iT13C)=varid
# if defined ORGANIC_MATTER
              CASE ('idTvar(iDO13)')
                idTvar(iDO13)=varid
              CASE ('idTvar(iPO13)')
                idTvar(iPO13)=varid
              CASE ('idTvar(iP113)')
                idTvar(iP113)=varid
              CASE ('idTvar(iP213)')
                idTvar(iP213)=varid
              CASE ('idTvar(iZo13)')
                idTvar(iZo13)=varid
# endif
#endif
#if defined NUTRIENTS
              CASE ('idTvar(iNO3_)')
                idTvar(iNO3_)=varid
              CASE ('idTvar(iNO2_)')
                idTvar(iNO2_)=varid
              CASE ('idTvar(iNH4_)')
                idTvar(iNH4_)=varid
              CASE ('idTvar(iPO4_)')
                idTvar(iPO4_)=varid
# if defined ORGANIC_MATTER
              CASE ('idTvar(iDON_)')
                idTvar(iDON_)=varid
              CASE ('idTvar(iPON_)')
                idTvar(iPON_)=varid
              CASE ('idTvar(iDOP_)')
                idTvar(iDOP_)=varid
              CASE ('idTvar(iPOP_)')
                idTvar(iPOP_)=varid
# endif
#endif
#if defined COT_STARFISH
              CASE ('idTvar(iCOTe)')
                idTvar(iCOTe)=varid
              CASE ('idTvar(iCOTl)')
                idTvar(iCOTl)=varid
#endif

              CASE ('iHbio2(ipHt_)')
                iHbio2(ipHt_)=varid
              CASE ('iHbio2(iWarg)')
                iHbio2(iWarg)=varid

              CASE ('iHbio2(iCOfx)')
                iHbio2(iCOfx)=varid
              CASE ('iHbio2(ipCO2)')
                iHbio2(ipCO2)=varid
              CASE ('iHbio2(iO2fx)')
                iHbio2(iO2fx)=varid

              CASE ('iHbio2(iPARb)')
                iHbio2(iPARb)=varid

              CASE ('iHbio2(iTau_)')
                iHbio2(iTau_)=varid
#ifdef CORAL_POLYP
              CASE ('iHbio2(iC1Pg)')
                iHbio2(iC1Pg)=varid
              CASE ('iHbio2(iC1_R)')
                iHbio2(iC1_R)=varid
              CASE ('iHbio2(iC1Pn)')
                iHbio2(iC1Pn)=varid
              CASE ('iHbio2(iC1_G)')
                iHbio2(iC1_G)=varid
              CASE ('iHbio2(iC1OC)')
                iHbio2(iC1OC)=varid
              CASE ('iHbio2(iC2Pg)')
                iHbio2(iC2Pg)=varid
              CASE ('iHbio2(iC2_R)')
                iHbio2(iC2_R)=varid
              CASE ('iHbio2(iC2Pn)')
                iHbio2(iC2Pn)=varid
              CASE ('iHbio2(iC2_G)')
                iHbio2(iC2_G)=varid
              CASE ('iHbio2(iC2OC)')
                iHbio2(iC2OC)=varid
# ifdef CORAL_CARBON_ISOTOPE
              CASE ('iHbio2(iC113)')
                iHbio2(iC113)=varid
              CASE ('iHbio2(iC213)')
                iHbio2(iC213)=varid
# endif
# ifdef CORAL_ZOOXANTHELLAE
              CASE ('iHbio2(iC1zx)')
                iHbio2(iC1zx)=varid
              CASE ('iHbio2(iC2zx)')
                iHbio2(iC2zx)=varid
# endif
# ifdef CORAL_SIZE_DYNAMICS
              CASE ('iHbio2(iC1mt)')
                iHbio2(iC1mt)=varid
              CASE ('iHbio2(iC1gr)')
                iHbio2(iC1gr)=varid
              CASE ('iHbio2(iC2mt)')
                iHbio2(iC2mt)=varid
              CASE ('iHbio2(iC2gr)')
                iHbio2(iC2gr)=varid
# endif
#endif
#ifdef SEAGRASS
              CASE ('iHbio2(iSgPg)')
                iHbio2(iSgPg)=varid
              CASE ('iHbio2(iSg_R)')
                iHbio2(iSg_R)=varid
              CASE ('iHbio2(iSgPn)')
                iHbio2(iSgPn)=varid
#endif
#ifdef MACROALGAE
              CASE ('iHbio2(iAgPg)')
                iHbio2(iAgPg)=varid
              CASE ('iHbio2(iAg_R)')
                iHbio2(iAg_R)=varid
              CASE ('iHbio2(iAgPn)')
                iHbio2(iAgPn)=varid
#endif
#ifdef SEDIMENT_ECOSYS
              CASE ('iHbio2(iSdPg)')
                iHbio2(iSdPg)=varid
              CASE ('iHbio2(iSd_R)')
                iHbio2(iSd_R)=varid
              CASE ('iHbio2(iSdPn)')
                iHbio2(iSdPn)=varid
              CASE ('iHbio2(iSd_G)')
                iHbio2(iSd_G)=varid
#endif

              CASE ('iHbio3(iPPro)')
                iHbio3(iPPro)=varid
# ifdef CARBON_ISOTOPE
              CASE ('iHbio3(id13C)')
                iHbio3(id13C)=varid
# endif
# ifdef NUTRIENTS
              CASE ('iHbio3(iNO3u)')
                iHbio3(iNO3u)=varid
#  ifdef DENITRIFICATION
              CASE ('iHbio2(iDNIT)')
                iHbio2(iDNIT)=varid
#  endif
# endif

/*
**  Adjoint sensitivity state biological tracers.
*/

#if defined AD_SENSITIVITY   || defined IS4DVAR_SENSITIVITY || \
    defined OPT_OBSERVATIONS || defined SENSITIVITY_4DVAR   || \
    defined SO_SEMI
              CASE ('idTads(iTIC_)')
                idTads(iTIC_)=varid
              CASE ('idTads(iTAlk)')
                idTads(iTAlk)=varid
              CASE ('idTads(iOxyg)')
                idTads(iOxyg)=varid
# if defined ORGANIC_MATTER
              CASE ('idTads(iDOC_)')
                idTads(iDOC_)=varid
              CASE ('idTads(iPOC_)')
                idTads(iPOC_)=varid
              CASE ('idTads(iPhy1)')
                idTads(iPhy1)=varid
              CASE ('idTads(iPhy2)')
                idTads(iPhy2)=varid
              CASE ('idTads(iZoop)')
                idTads(iZoop)=varid
# endif
# if defined CARBON_ISOTOPE
              CASE ('idTads(iT13C)')
                idTads(iT13C)=varid
#  if defined ORGANIC_MATTER
              CASE ('idTads(iDO13)')
                idTads(iDO13)=varid
              CASE ('idTads(iPO13)')
                idTads(iPO13)=varid
              CASE ('idTads(iP113)')
                idTads(iP113)=varid
              CASE ('idTads(iP213)')
                idTads(iP213)=varid
              CASE ('idTads(iZo13)')
                idTads(iZo13)=varid
#  endif
# endif
# if defined NUTRIENTS
              CASE ('idTads(iNO3_)')
                idTads(iNO3_)=varid
              CASE ('idTads(iNO2_)')
                idTads(iNO2_)=varid
              CASE ('idTads(iNH4_)')
                idTads(iNH4_)=varid
              CASE ('idTads(iPO4_)')
                idTads(iPO4_)=varid
#  if defined ORGANIC_MATTER
              CASE ('idTads(iDON_)')
                idTads(iDON_)=varid
              CASE ('idTads(iPON_)')
                idTads(iPON_)=varid
              CASE ('idTads(iDOP_)')
                idTads(iDOP_)=varid
              CASE ('idTads(iPOP_)')
                idTads(iPOP_)=varid
#  endif
# endif
# if defined COT_STARFISH
              CASE ('idTads(iCOTe)')
                idTads(iCOTe)=varid
              CASE ('idTads(iCOTl)')
                idTads(iCOTl)=varid
# endif
#endif

/*
**  Biological tracers open boundary conditions.
*/
              CASE ('idTbry(iwest,iTIC_)')
                idTbry(iwest,iTIC_)=varid
              CASE ('idTbry(ieast,iTIC_)')
                idTbry(ieast,iTIC_)=varid
              CASE ('idTbry(isouth,iTIC_)')
                idTbry(isouth,iTIC_)=varid
              CASE ('idTbry(inorth,iTIC_)')
                idTbry(inorth,iTIC_)=varid

              CASE ('idTbry(iwest,iTAlk)')
                idTbry(iwest,iTAlk)=varid
              CASE ('idTbry(ieast,iTAlk)')
                idTbry(ieast,iTAlk)=varid
              CASE ('idTbry(isouth,iTAlk)')
                idTbry(isouth,iTAlk)=varid
              CASE ('idTbry(inorth,iTAlk)')
                idTbry(inorth,iTAlk)=varid

              CASE ('idTbry(iwest,iOxyg)')
                idTbry(iwest,iOxyg)=varid
              CASE ('idTbry(ieast,iOxyg)')
                idTbry(ieast,iOxyg)=varid
              CASE ('idTbry(isouth,iOxyg)')
                idTbry(isouth,iOxyg)=varid
              CASE ('idTbry(inorth,iOxyg)')
                idTbry(inorth,iOxyg)=varid

#if defined ORGANIC_MATTER
              CASE ('idTbry(iwest,iDOC_)')
                idTbry(iwest,iDOC_)=varid
              CASE ('idTbry(ieast,iDOC_)')
                idTbry(ieast,iDOC_)=varid
              CASE ('idTbry(isouth,iDOC_)')
                idTbry(isouth,iDOC_)=varid
              CASE ('idTbry(inorth,iDOC_)')
                idTbry(inorth,iDOC_)=varid

              CASE ('idTbry(iwest,iPOC_)')
                idTbry(iwest,iPOC_)=varid
              CASE ('idTbry(ieast,iPOC_)')
                idTbry(ieast,iPOC_)=varid
              CASE ('idTbry(isouth,iPOC_)')
                idTbry(isouth,iPOC_)=varid
              CASE ('idTbry(inorth,iPOC_)')
                idTbry(inorth,iPOC_)=varid

              CASE ('idTbry(iwest,iPhy1)')
                idTbry(iwest,iPhy1)=varid
              CASE ('idTbry(ieast,iPhy1)')
                idTbry(ieast,iPhy1)=varid
              CASE ('idTbry(isouth,iPhy1)')
                idTbry(isouth,iPhy1)=varid
              CASE ('idTbry(inorth,iPhy1)')
                idTbry(inorth,iPhy1)=varid

              CASE ('idTbry(iwest,iPhy2)')
                idTbry(iwest,iPhy2)=varid
              CASE ('idTbry(ieast,iPhy2)')
                idTbry(ieast,iPhy2)=varid
              CASE ('idTbry(isouth,iPhy2)')
                idTbry(isouth,iPhy2)=varid
              CASE ('idTbry(inorth,iPhy2)')
                idTbry(inorth,iPhy2)=varid

              CASE ('idTbry(iwest,iZoop)')
                idTbry(iwest,iZoop)=varid
              CASE ('idTbry(ieast,iZoop)')
                idTbry(ieast,iZoop)=varid
              CASE ('idTbry(isouth,iZoop)')
                idTbry(isouth,iZoop)=varid
              CASE ('idTbry(inorth,iZoop)')
                idTbry(inorth,iZoop)=varid
#endif
#if defined CARBON_ISOTOPE
              CASE ('idTbry(iwest,iT13C)')
                idTbry(iwest,iT13C)=varid
              CASE ('idTbry(ieast,iT13C)')
                idTbry(ieast,iT13C)=varid
              CASE ('idTbry(isouth,iT13C)')
                idTbry(isouth,iT13C)=varid
              CASE ('idTbry(inorth,iT13C)')
                idTbry(inorth,iT13C)=varid

# if defined ORGANIC_MATTER
              CASE ('idTbry(iwest,iDO13)')
                idTbry(iwest,iDO13)=varid
              CASE ('idTbry(ieast,iDO13)')
                idTbry(ieast,iDO13)=varid
              CASE ('idTbry(isouth,iDO13)')
                idTbry(isouth,iDO13)=varid
              CASE ('idTbry(inorth,iDO13)')
                idTbry(inorth,iDO13)=varid

              CASE ('idTbry(iwest,iPO13)')
                idTbry(iwest,iPO13)=varid
              CASE ('idTbry(ieast,iPO13)')
                idTbry(ieast,iPO13)=varid
              CASE ('idTbry(isouth,iPO13)')
                idTbry(isouth,iPO13)=varid
              CASE ('idTbry(inorth,iPO13)')
                idTbry(inorth,iPO13)=varid

              CASE ('idTbry(iwest,iP113)')
                idTbry(iwest,iP113)=varid
              CASE ('idTbry(ieast,iP113)')
                idTbry(ieast,iP113)=varid
              CASE ('idTbry(isouth,iP113)')
                idTbry(isouth,iP113)=varid
              CASE ('idTbry(inorth,iP113)')
                idTbry(inorth,iP113)=varid
                
              CASE ('idTbry(iwest,iP213)')
                idTbry(iwest,iP213)=varid
              CASE ('idTbry(ieast,iP213)')
                idTbry(ieast,iP213)=varid
              CASE ('idTbry(isouth,iP213)')
                idTbry(isouth,iP213)=varid
              CASE ('idTbry(inorth,iP213)')
                idTbry(inorth,iP213)=varid

              CASE ('idTbry(iwest,iZo13)')
                idTbry(iwest,iZo13)=varid
              CASE ('idTbry(ieast,iZo13)')
                idTbry(ieast,iZo13)=varid
              CASE ('idTbry(isouth,iZo13)')
                idTbry(isouth,iZo13)=varid
              CASE ('idTbry(inorth,iZo13)')
                idTbry(inorth,iZo13)=varid
# endif
#endif

#if defined NUTRIENTS
              CASE ('idTbry(iwest,iNO3_)')
                idTbry(iwest,iNO3_)=varid
              CASE ('idTbry(ieast,iNO3_)')
                idTbry(ieast,iNO3_)=varid
              CASE ('idTbry(isouth,iNO3_)')
                idTbry(isouth,iNO3_)=varid
              CASE ('idTbry(inorth,iNO3_)')
                idTbry(inorth,iNO3_)=varid

              CASE ('idTbry(iwest,iNO2_)')
                idTbry(iwest,iNO2_)=varid
              CASE ('idTbry(ieast,iNO2_)')
                idTbry(ieast,iNO2_)=varid
              CASE ('idTbry(isouth,iNO2_)')
                idTbry(isouth,iNO2_)=varid
              CASE ('idTbry(inorth,iNO2_)')
                idTbry(inorth,iNO2_)=varid

              CASE ('idTbry(iwest,iNH4_)')
                idTbry(iwest,iNH4_)=varid
              CASE ('idTbry(ieast,iNH4_)')
                idTbry(ieast,iNH4_)=varid
              CASE ('idTbry(isouth,iNH4_)')
                idTbry(isouth,iNH4_)=varid
              CASE ('idTbry(inorth,iNH4_)')
                idTbry(inorth,iNH4_)=varid

              CASE ('idTbry(iwest,iPO4_)')
                idTbry(iwest,iPO4_)=varid
              CASE ('idTbry(ieast,iPO4_)')
                idTbry(ieast,iPO4_)=varid
              CASE ('idTbry(isouth,iPO4_)')
                idTbry(isouth,iPO4_)=varid
              CASE ('idTbry(inorth,iPO4_)')
                idTbry(inorth,iPO4_)=varid

# if defined ORGANIC_MATTER
              CASE ('idTbry(iwest,iDON_)')
                idTbry(iwest,iDON_)=varid
              CASE ('idTbry(ieast,iDON_)')
                idTbry(ieast,iDON_)=varid
              CASE ('idTbry(isouth,iDON_)')
                idTbry(isouth,iDON_)=varid
              CASE ('idTbry(inorth,iDON_)')
                idTbry(inorth,iDON_)=varid

              CASE ('idTbry(iwest,iPON_)')
                idTbry(iwest,iPON_)=varid
              CASE ('idTbry(ieast,iPON_)')
                idTbry(ieast,iPON_)=varid
              CASE ('idTbry(isouth,iPON_)')
                idTbry(isouth,iPON_)=varid
              CASE ('idTbry(inorth,iPON_)')
                idTbry(inorth,iPON_)=varid

              CASE ('idTbry(iwest,iDOP_)')
                idTbry(iwest,iDOP_)=varid
              CASE ('idTbry(ieast,iDOP_)')
                idTbry(ieast,iDOP_)=varid
              CASE ('idTbry(isouth,iDOP_)')
                idTbry(isouth,iDOP_)=varid
              CASE ('idTbry(inorth,iDOP_)')
                idTbry(inorth,iDOP_)=varid

              CASE ('idTbry(iwest,iPOP_)')
                idTbry(iwest,iPOP_)=varid
              CASE ('idTbry(ieast,iPOP_)')
                idTbry(ieast,iPOP_)=varid
              CASE ('idTbry(isouth,iPOP_)')
                idTbry(isouth,iPOP_)=varid
              CASE ('idTbry(inorth,iPOP_)')
                idTbry(inorth,iPOP_)=varid
# endif
#endif

#if defined COT_STARFISH
              CASE ('idTbry(iwest,iCOTe)')
                idTbry(iwest,iCOTe)=varid
              CASE ('idTbry(ieast,iCOTe)')
                idTbry(ieast,iCOTe)=varid
              CASE ('idTbry(isouth,iCOTe)')
                idTbry(isouth,iCOTe)=varid
              CASE ('idTbry(inorth,iCOTe)')
                idTbry(inorth,iCOTe)=varid

              CASE ('idTbry(iwest,iCOTl)')
                idTbry(iwest,iCOTl)=varid
              CASE ('idTbry(ieast,iCOTl)')
                idTbry(ieast,iCOTl)=varid
              CASE ('idTbry(isouth,iCOTl)')
                idTbry(isouth,iCOTl)=varid
              CASE ('idTbry(inorth,iCOTl)')
                idTbry(inorth,iCOTl)=varid
#endif

/*
**  Biological tracers point Source/Sinks (river runoff).
*/
              CASE ('idRtrc(iTIC_)')
                idRtrc(iTIC_)=varid
              CASE ('idRtrc(iTAlk)')
                idRtrc(iTAlk)=varid
              CASE ('idRtrc(iOxyg)')
                idRtrc(iOxyg)=varid
#if defined ORGANIC_MATTER
              CASE ('idRtrc(iDOC_)')
                idRtrc(iDOC_)=varid
              CASE ('idRtrc(iPOC_)')
                idRtrc(iPOC_)=varid
              CASE ('idRtrc(iPhy1)')
                idRtrc(iPhy1)=varid
              CASE ('idRtrc(iPhy2)')
                idRtrc(iPhy2)=varid
              CASE ('idRtrc(iZoop)')
                idRtrc(iZoop)=varid
#endif
#if defined CARBON_ISOTOPE
              CASE ('idRtrc(iT13C)')
                idRtrc(iT13C)=varid
# if defined ORGANIC_MATTER
              CASE ('idRtrc(iDO13)')
                idRtrc(iDO13)=varid
              CASE ('idRtrc(iPO13)')
                idRtrc(iPO13)=varid
              CASE ('idRtrc(iPh13)')
                idRtrc(iPh13)=varid
              CASE ('idRtrc(iZo13)')
                idRtrc(iZo13)=varid
# endif
#endif
#if defined NUTRIENTS
              CASE ('idRtrc(iNO3_)')
                idRtrc(iNO3_)=varid
              CASE ('idRtrc(iNO2_)')
                idRtrc(iNO2_)=varid
              CASE ('idRtrc(iNH4_)')
                idRtrc(iNH4_)=varid
              CASE ('idRtrc(iPO4_)')
                idRtrc(iPO4_)=varid
# if defined ORGANIC_MATTER
              CASE ('idRtrc(iDON_)')
                idRtrc(iDON_)=varid
              CASE ('idRtrc(iPON_)')
                idRtrc(iPON_)=varid
              CASE ('idRtrc(iDOP_)')
                idRtrc(iDOP_)=varid
              CASE ('idRtrc(iPOP_)')
                idRtrc(iPOP_)=varid
# endif
#endif
#if defined COT_STARFISH
              CASE ('idRtrc(iCOTe)')
                idRtrc(iCOTe)=varid
              CASE ('idRtrc(iCOTl)')
                idRtrc(iCOTl)=varid
#endif


