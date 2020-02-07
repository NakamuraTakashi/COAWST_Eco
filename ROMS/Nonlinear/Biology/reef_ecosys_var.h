/*
** svn $Id: fennel_var.h 618 2012-05-17 20:12:51Z arango $
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2012 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
*************************************************** Takashi Nakamura ***
**                                                                    **
**  Assigns metadata indices for the coral reef ecosystem             **
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
              CASE ('idTvar(iTIC_)')
                idTvar(iTIC_)=varid
              CASE ('idTvar(iTAlk)')
                idTvar(iTAlk)=varid
              CASE ('idTvar(iOxyg)')
                idTvar(iOxyg)=varid
#if defined ORGANIC_MATTER
              CASE ('idTvar(iDOC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTvar(iDOC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iPOC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTvar(iPOC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iPhyt(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_phyt
                  varid=varid+1
                  idTvar(iPhyt(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iZoop(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_zoop
                  varid=varid+1
                  idTvar(iZoop(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iPIC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pim
                  varid=varid+1
                  idTvar(iPIC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
#endif
#if defined CARBON_ISOTOPE
              CASE ('idTvar(iT13C)')
                idTvar(iT13C)=varid
# if defined ORGANIC_MATTER
              CASE ('idTvar(iDO13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTvar(iDO13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iPO13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTvar(iPO13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iPhyt13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_phyt
                  varid=varid+1
                  idTvar(iPhyt13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iZoop13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_zoop
                  varid=varid+1
                  idTvar(iZoop13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iPI13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pim
                  varid=varid+1
                  idTvar(iPI13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
# endif
#endif
#if defined NUTRIENTS
              CASE ('idTvar(iNO3_)')
                idTvar(iNO3_)=varid
!              CASE ('idTvar(iNO2_)')
!                idTvar(iNO2_)=varid
              CASE ('idTvar(iNH4_)')
                idTvar(iNH4_)=varid
              CASE ('idTvar(iPO4_)')
                idTvar(iPO4_)=varid
# if defined ORGANIC_MATTER
              CASE ('idTvar(iDON(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTvar(iDON(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iPON(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTvar(iPON(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iDOP(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTvar(iDOP(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iPOP(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTvar(iPOP(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
# endif
# if defined NITROGEN_ISOTOPE
              CASE ('idTvar(i15NO3)')
                idTvar(i15NO3)=varid
!              CASE ('idTvar(i15NO2)')
!                idTvar(i15NO2)=varid
              CASE ('idTvar(i15NH4)')
                idTvar(i15NH4)=varid
#  if defined ORGANIC_MATTER
              CASE ('idTvar(iDO15N(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTvar(iDO15N(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iPO15N(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTvar(iPO15N(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iPhyt15N(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_phyt
                  varid=varid+1
                  idTvar(iPhyt15N(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iZoop15N(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_zoop
                  varid=varid+1
                  idTvar(iZoop15N(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
#  endif
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

# ifdef CARBON_ISOTOPE
              CASE ('iHbio3(id13C)')
                iHbio3(id13C)=varid
# endif

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
              CASE ('idTbry(iwest,iDOC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(iwest,iDOC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iDOC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(ieast,iDOC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iDOC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(isouth,iDOC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iDOC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(inorth,iDOC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iPOC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(iwest,iPOC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iPOC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(ieast,iPOC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iPOC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(isouth,iPOC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iPOC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(inorth,iPOC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iPhyt(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_phyt
                  varid=varid+1
                  idTbry(iwest,iPhyt(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iPhyt(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_phyt
                  varid=varid+1
                  idTbry(ieast,iPhyt(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iPhyt(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_phyt
                  varid=varid+1
                  idTbry(isouth,iPhyt(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iPhyt(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_phyt
                  varid=varid+1
                  idTbry(inorth,iPhyt(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iZoop(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_zoop
                  varid=varid+1
                  idTbry(iwest,iZoop(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iZoop(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_zoop
                  varid=varid+1
                  idTbry(ieast,iZoop(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iZoop(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_zoop
                  varid=varid+1
                  idTbry(isouth,iZoop(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iZoop(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_zoop
                  varid=varid+1
                  idTbry(inorth,iZoop(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
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
              CASE ('idTbry(iwest,iDO13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(iwest,iDO13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iDO13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(ieast,iDO13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iDO13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(isouth,iDO13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iDO13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(inorth,iDO13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iPO13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(iwest,iPO13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iPO13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(ieast,iPO13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iPO13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(isouth,iPO13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iPO13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(inorth,iPO13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iPhyt13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_phyt
                  varid=varid+1
                  idTbry(iwest,iPhyt13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iPhyt13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_phyt
                  varid=varid+1
                  idTbry(ieast,iPhyt13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iPhyt13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_phyt
                  varid=varid+1
                  idTbry(isouth,iPhyt13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iPhyt13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_phyt
                  varid=varid+1
                  idTbry(inorth,iPhyt13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iZoop13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_zoop
                  varid=varid+1
                  idTbry(iwest,iZoop13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iZoop13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_zoop
                  varid=varid+1
                  idTbry(ieast,iZoop13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iZoop13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_zoop
                  varid=varid+1
                  idTbry(isouth,iZoop13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iZoop13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_zoop
                  varid=varid+1
                  idTbry(inorth,iZoop13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
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

!              CASE ('idTbry(iwest,iNO2_)')
!                idTbry(iwest,iNO2_)=varid
!              CASE ('idTbry(ieast,iNO2_)')
!                idTbry(ieast,iNO2_)=varid
!              CASE ('idTbry(isouth,iNO2_)')
!                idTbry(isouth,iNO2_)=varid
!              CASE ('idTbry(inorth,iNO2_)')
!                idTbry(inorth,iNO2_)=varid

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
              CASE ('idTbry(iwest,iDON(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(iwest,iDON(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iDON(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(ieast,iDON(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iDON(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(isouth,iDON(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iDON(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(inorth,iDON(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iPON(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(iwest,iPON(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iPON(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(ieast,iPON(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iPON(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(isouth,iPON(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iPON(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(inorth,iPON(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iDOP(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(iwest,iDOP(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iDOP(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(ieast,iDOP(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iDOP(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(isouth,iDOP(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iDOP(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idTbry(inorth,iDOP(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iPOP(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(iwest,iPOP(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iPOP(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(ieast,iPOP(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iPOP(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(isouth,iPOP(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iPOP(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idTbry(inorth,iPOP(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
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
              CASE ('idRtrc(iDOC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idRtrc(iDOC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idRtrc(iPOC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idRtrc(iPOC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idRtrc(iPhyt(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_phyt
                  varid=varid+1
                  idRtrc(iPhyt(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idRtrc(iZoop(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_zoop
                  varid=varid+1
                  idRtrc(iZoop(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idRtrc(iPIC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pim
                  varid=varid+1
                  idRtrc(iPIC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
#endif
#if defined CARBON_ISOTOPE
              CASE ('idRtrc(iT13C)')
                idRtrc(iT13C)=varid
# if defined ORGANIC_MATTER
              CASE ('idRtrc(iDO13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idRtrc(iDO13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idRtrc(iPO13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idRtrc(iPO13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idRtrc(iPhyt13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_phyt
                  varid=varid+1
                  idRtrc(iPhyt13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idRtrc(iZoop13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_zoop
                  varid=varid+1
                  idRtrc(iZoop13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idRtrc(iPI13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pim
                  varid=varid+1
                  idRtrc(iPI13C(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
# endif
#endif
#if defined NUTRIENTS
              CASE ('idRtrc(iNO3_)')
                idRtrc(iNO3_)=varid
!              CASE ('idRtrc(iNO2_)')
!                idRtrc(iNO2_)=varid
              CASE ('idRtrc(iNH4_)')
                idRtrc(iNH4_)=varid
              CASE ('idRtrc(iPO4_)')
                idRtrc(iPO4_)=varid
# if defined ORGANIC_MATTER
              CASE ('idRtrc(iDON(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idRtrc(iDON(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idRtrc(iPON(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idRtrc(iPON(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idRtrc(iDOP(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_dom
                  varid=varid+1
                  idRtrc(iDOP(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idRtrc(iPOP(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_pom
                  varid=varid+1
                  idRtrc(iPOP(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
# endif
#endif
#if defined COT_STARFISH
              CASE ('idRtrc(iCOTe)')
                idRtrc(iCOTe)=varid
              CASE ('idRtrc(iCOTl)')
                idRtrc(iCOTl)=varid
#endif


