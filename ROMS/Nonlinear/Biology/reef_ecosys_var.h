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
                DO i=1,Ndom
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
                DO i=1,Npom
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
                DO i=1,Nphy
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
                DO i=1,Nzoo
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
                DO i=1,Npim
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
                DO i=1,Ndom
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
                DO i=1,Npom
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
                DO i=1,Nphy
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
                DO i=1,Nzoo
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
                DO i=1,Npim
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
                DO i=1,Ndom
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
                DO i=1,Npom
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
                DO i=1,Ndom
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
                DO i=1,Npom
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
                DO i=1,Ndom
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
                DO i=1,Npom
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
                DO i=1,Nphy
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
                DO i=1,Nzoo
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

#ifdef CARBON_ISOTOPE
              CASE ('iHbio3(id13C)')  !!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Remove??
                iHbio3(id13C)=varid
#endif
#if defined DIAGNOSTICS_BIO

              CASE ('iDbio2(ipHt_)')
                iDbio2(ipHt_)=varid
              CASE ('iDbio2(iWarg)')
                iDbio2(iWarg)=varid

              CASE ('iDbio2(iCO2fx)')
                iDbio2(iCO2fx)=varid
              CASE ('iDbio2(ipCO2)')
                iDbio2(ipCO2)=varid
              CASE ('iDbio2(iO2fx)')
                iDbio2(iO2fx)=varid

              CASE ('iDbio2(iPARb)')
                iDbio2(iPARb)=varid

              CASE ('iDbio2(iTau_)')
                iDbio2(iTau_)=varid

# ifdef CARBON_ISOTOPE
              CASE ('iDbio3(id13C)')
                iDbio3(id13C)=varid
# endif
#endif

! ==== Coral parameters ===============================================
#ifdef CORAL_POLYP
              CASE ('iHbio2(iClTAcal(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iClTAcal(i))=varid
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
              CASE ('iHbio2(iClTAcoe(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iClTAcoe(i))=varid
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
              CASE ('iHbio2(iClDICcal(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iClDICcal(i))=varid
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
              CASE ('iHbio2(iClDICcoe(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iClDICcoe(i))=varid
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
              CASE ('iHbio2(iClDOcoe(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iClDOcoe(i))=varid
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
              CASE ('iHbio2(iClQC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iClQC(i))=varid
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
# if defined CORAL_NONE_CO2_EQ
              CASE ('iHbio2(iClCO2cal(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iClCO2cal(i))=varid
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
              CASE ('iHbio2(iClCO2coe(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iClCO2coe(i))=varid
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
# ifdef CORAL_CARBON_ISOTOPE
              CASE ('iHbio2(iClDI13Ccal(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iClDI13Ccal(i))=varid
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
              CASE ('iHbio2(iClDI13Ccoe(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iClDI13Ccoe(i))=varid
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
              CASE ('iHbio2(iClQ13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iClQ13C(i))=varid
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
#  if defined CORAL_NONE_CO2_EQ
              CASE ('iHbio2(iCl13CO2cal(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iCl13CO2cal(i))=varid
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
              CASE ('iHbio2(iCl13CO2coe(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iCl13CO2coe(i))=varid
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
# ifdef CORAL_ZOOXANTHELLAE
              CASE ('iHbio2(iClROS(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iClROS(i))=varid
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
              CASE ('iHbio2(iZxDns(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iZxDns(i))=varid
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
              CASE ('iHbio2(iZxQC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iZxQC(i))=varid
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
              CASE ('iHbio2(iZxChl(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iZxChl(i))=varid
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
              CASE ('iHbio2(iZxQAo(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iZxQAo(i))=varid
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
              CASE ('iHbio2(iZxQAr(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iZxQAr(i))=varid
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
              CASE ('iHbio2(iZxQAi(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iZxQAi(i))=varid
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
              CASE ('iHbio2(iZxQAid(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iZxQAid(i))=varid
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
#  ifdef CORAL_CARBON_ISOTOPE
              CASE ('iHbio2(iZxQ13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iHbio2(iZxQ13C(i))=varid
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
# ifdef CORAL_SIZE_DYNAMICS

# endif
# if defined DIAGNOSTICS_BIO
              CASE ('iDbio2(iClPg(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iDbio2(iClPg(i))=varid
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
              CASE ('iDbio2(iCl_R(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iDbio2(iCl_R(i))=varid
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
              CASE ('iDbio2(iCl_G(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iDbio2(iCl_G(i))=varid
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
              CASE ('iDbio2(iClPn(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iDbio2(iClPn(i))=varid
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
#  ifdef CORAL_CARBON_ISOTOPE
              CASE ('iDbio2(iClQCd13C(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iDbio2(iClQCd13C(i))=varid
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
#  ifdef CORAL_ZOOXANTHELLAE
              CASE ('iDbio2(iZxPg(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iDbio2(iZxPg(i))=varid
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
              CASE ('iDbio2(iZx_R(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iDbio2(iZx_R(i))=varid
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
              CASE ('iDbio2(iZxPn(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iDbio2(iZxPn(i))=varid
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
#  ifdef CORAL_SIZE_DYNAMICS
              CASE ('iDbio2(iClmt(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iDbio2(iClmt(i))=varid
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
              CASE ('iDbio2(iClgw(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Ncl
                  varid=varid+1
                  iDbio2(iClgw(i))=varid
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
! ==== Seagrass parameters ===============================================
#ifdef SEAGRASS
              CASE ('iHbio2(iSgSgCBm(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nsg
                  varid=varid+1
                  iHbio2(iSgSgCBm(i))=varid
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
              CASE ('iHbio2(iSgLfCBm(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nsg
                  varid=varid+1
                  iHbio2(iSgLfCBm(i))=varid
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
              CASE ('iHbio2(iSgRtCBm(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nsg
                  varid=varid+1
                  iHbio2(iSgRtCBm(i))=varid
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
              CASE ('iHbio2(iSgTotSgCBm(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nsg
                  varid=varid+1
                  iHbio2(iSgTotSgCBm(i))=varid
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
              CASE ('iHbio2(iSgTotLfCBm(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nsg
                  varid=varid+1
                  iHbio2(iSgTotLfCBm(i))=varid
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
              CASE ('iHbio2(iSgTotRtCBm(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nsg
                  varid=varid+1
                  iHbio2(iSgTotRtCBm(i))=varid
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
              CASE ('iHbio2(iSgTotLA(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nsg
                  varid=varid+1
                  iHbio2(iSgTotLA(i))=varid
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
              CASE ('iHbio2(iSgGridELAP(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nsg
                  varid=varid+1
                  iHbio2(iSgGridELAP(i))=varid
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
              CASE ('iHbio2(iSgGridPhot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nsg
                  varid=varid+1
                  iHbio2(iSgGridPhot(i))=varid
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
              CASE ('iHbio2(iSgPhotLim(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nsg
                  varid=varid+1
                  iHbio2(iSgPhotLim(i))=varid
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
              CASE ('iHbio2(iSgGridResp(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nsg
                  varid=varid+1
                  iHbio2(iSgGridResp(i))=varid
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
              CASE ('iHbio2(iSgGridNetPhot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nsg
                  varid=varid+1
                  iHbio2(iSgGridNetPhot(i))=varid
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
              CASE ('iHbio2(iSgGridDieoff(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nsg
                  varid=varid+1
                  iHbio2(iSgGridDieoff(i))=varid
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

! ==== Macroalgae parameters ===============================================
#ifdef MACROALGAE


# if defined DIAGNOSTICS_BIO
              CASE ('iDbio2(iAgPg(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nag
                  varid=varid+1
                  iDbio2(iAgPg(i))=varid
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
              CASE ('iDbio2(iAg_R(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nag
                  varid=varid+1
                  iDbio2(iAg_R(i))=varid
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
              CASE ('iDbio2(iAgPn(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,Nag
                  varid=varid+1
                  iDbio2(iAgPn(i))=varid
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

! ==== Sediment parameters ===============================================
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
#ifdef SEDIMENT_ECOSYS
              CASE ('iHbiosed3(iSdporo)')
                iHbiosed3(iSdporo)=varid
              CASE ('iHbiosed3(iSdTmp)')
                iHbiosed3(iSdTmp)=varid
              CASE ('iHbiosed3(iSdSal)')
                iHbiosed3(iSdSal)=varid
              ! CASE ('iHbiosed3(iSdpH)')
              !   iHbiosed3(iSdpH)=varid
              CASE ('iHbiosed3(iSdTA)')
                iHbiosed3(iSdTA)=varid
              ! CASE ('iHbiosed3(iSdDIC)')
              !   iHbiosed3(iSdDIC)=varid
              CASE ('iHbiosed3(iSdO2)')
                iHbiosed3(iSdO2)=varid
              CASE ('iHbiosed3(iSdCO2)')
                iHbiosed3(iSdCO2)=varid
              CASE ('iHbiosed3(iSdN2)')
                iHbiosed3(iSdN2)=varid
# if defined ORGANIC_MATTER
              CASE ('iHbiosed3(iSdDOCf)')
                iHbiosed3(iSdDOCf)=varid
              CASE ('iHbiosed3(iSdDOCs)')
                iHbiosed3(iSdDOCs)=varid
              CASE ('iHbiosed3(iSdPOCf)')
                iHbiosed3(iSdPOCf)=varid
              CASE ('iHbiosed3(iSdPOCs)')
                iHbiosed3(iSdPOCs)=varid
              CASE ('iHbiosed3(iSdPOCn)')
                iHbiosed3(iSdPOCn)=varid
# endif
# if defined NUTRIENTS
              CASE ('iHbiosed3(iSdNO3)')
                iHbiosed3(iSdNO3)=varid
              CASE ('iHbiosed3(iSdNH4)')
                iHbiosed3(iSdNH4)=varid
              CASE ('iHbiosed3(iSdPO4)')
                iHbiosed3(iSdPO4)=varid
#  if defined ORGANIC_MATTER
              CASE ('iHbiosed3(iSdDONf)')
                iHbiosed3(iSdDONf)=varid
              CASE ('iHbiosed3(iSdDONs)')
                iHbiosed3(iSdDONs)=varid
              CASE ('iHbiosed3(iSdPONf)')
                iHbiosed3(iSdPONf)=varid
              CASE ('iHbiosed3(iSdPONs)')
                iHbiosed3(iSdPONs)=varid
              CASE ('iHbiosed3(iSdPONn)')
                iHbiosed3(iSdPONn)=varid
              CASE ('iHbiosed3(iSdDOPf)')
                iHbiosed3(iSdDOPf)=varid
              CASE ('iHbiosed3(iSdDOPs)')
                iHbiosed3(iSdDOPs)=varid
              CASE ('iHbiosed3(iSdPOPf)')
                iHbiosed3(iSdPOPf)=varid
              CASE ('iHbiosed3(iSdPOPs)')
                iHbiosed3(iSdPOPs)=varid
              CASE ('iHbiosed3(iSdPOPn)')
                iHbiosed3(iSdPOPn)=varid
#  endif
# endif
# if defined SULFATE
              CASE ('iHbiosed3(iSdMn2)')
                iHbiosed3(iSdMn2)=varid
              CASE ('iHbiosed3(iSdMnO2)')
                iHbiosed3(iSdMnO2)=varid
              CASE ('iHbiosed3(iSdFe2)')
                iHbiosed3(iSdFe2)=varid
              CASE ('iHbiosed3(iSdFeS)')
                iHbiosed3(iSdFeS)=varid
              CASE ('iHbiosed3(iSdFeS2)')
                iHbiosed3(iSdFeS2)=varid
              CASE ('iHbiosed3(iSdFeOOH)')
                iHbiosed3(iSdFeOOH)=varid
              CASE ('iHbiosed3(iSdFeOOH_PO4)')
                iHbiosed3(iSdFeOOH_PO4)=varid
              CASE ('iHbiosed3(iSdH2S)')
                iHbiosed3(iSdH2S)=varid
              CASE ('iHbiosed3(iSdSO4)')
                iHbiosed3(iSdSO4)=varid
              CASE ('iHbiosed3(iSdS0)')
                iHbiosed3(iSdS0)=varid
# endif
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

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
                DO i=1,Ndom
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
                DO i=1,Ndom
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
                DO i=1,Ndom
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
                DO i=1,Ndom
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
                DO i=1,Npom
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
                DO i=1,Npom
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
                DO i=1,Npom
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
                DO i=1,Npom
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
                DO i=1,Nphy
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
                DO i=1,Nphy
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
                DO i=1,Nphy
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
                DO i=1,Nphy
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
                DO i=1,Nzoo
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
                DO i=1,Nzoo
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
                DO i=1,Nzoo
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
                DO i=1,Nzoo
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
                DO i=1,Ndom
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
                DO i=1,Ndom
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
                DO i=1,Ndom
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
                DO i=1,Ndom
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
                DO i=1,Npom
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
                DO i=1,Npom
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
                DO i=1,Npom
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
                DO i=1,Npom
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
                DO i=1,Nphy
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
                DO i=1,Nphy
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
                DO i=1,Nphy
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
                DO i=1,Nphy
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
                DO i=1,Nzoo
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
                DO i=1,Nzoo
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
                DO i=1,Nzoo
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
                DO i=1,Nzoo
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
                DO i=1,Ndom
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
                DO i=1,Ndom
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
                DO i=1,Ndom
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
                DO i=1,Ndom
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
                DO i=1,Npom
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
                DO i=1,Npom
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
                DO i=1,Npom
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
                DO i=1,Npom
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
                DO i=1,Ndom
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
                DO i=1,Ndom
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
                DO i=1,Ndom
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
                DO i=1,Ndom
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
                DO i=1,Npom
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
                DO i=1,Npom
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
                DO i=1,Npom
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
                DO i=1,Npom
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
                DO i=1,Ndom
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
                DO i=1,Npom
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
                DO i=1,Nphy
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
                DO i=1,Nzoo
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
                DO i=1,Npim
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
                DO i=1,Ndom
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
                DO i=1,Npom
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
                DO i=1,Nphy
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
                DO i=1,Nzoo
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
                DO i=1,Npim
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
                DO i=1,Ndom
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
                DO i=1,Npom
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
                DO i=1,Ndom
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
                DO i=1,Npom
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


