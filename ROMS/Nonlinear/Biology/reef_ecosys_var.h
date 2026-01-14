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
              CASE ('idTvar(iDO)')
                idTvar(iDO)=varid
              CASE ('idTvar(iTA)')
                idTvar(iTA)=varid

              CASE ('idTvar(iDIC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Csp
                  varid=varid+1
                  idTvar(iDIC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iNO3(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  idTvar(iNO3(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iNH4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  idTvar(iNH4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('idTvar(iPO4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Psp
                  varid=varid+1
                  idTvar(iPO4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1

              CASE ('idTvar(iDOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Csp
                    varid=varid+1
                    idTvar(iDOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTvar(iPOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Csp
                    varid=varid+1
                    idTvar(iPOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTvar(iDON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTvar(iDON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTvar(iPON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTvar(iPON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTvar(iDOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Psp
                    varid=varid+1
                    idTvar(iDOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTvar(iPOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Psp
                    varid=varid+1
                    idTvar(iPOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTvar(iPhyC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Csp
                    varid=varid+1
                    idTvar(iPhyC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTvar(iZooC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Csp
                    varid=varid+1
                    idTvar(iZooC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTvar(iPhyN(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTvar(iPhyN(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTvar(iZooN(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTvar(iZooN(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTvar(iPhyP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Psp
                    varid=varid+1
                    idTvar(iPhyP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTvar(iZooP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Psp
                    varid=varid+1
                    idTvar(iZooP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTvar(iPIC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npim
                  DO i=1,N_Csp
                    varid=varid+1
                    idTvar(iPIC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
#if defined BLUE_TIDE         
              CASE ('idTvar(iH2S(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  idTvar(iH2S(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1

              CASE ('idTvar(iS0(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  idTvar(iS0(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
#endif
#if defined COT_STARFISH
              CASE ('idTvar(iCOTe)')
                idTvar(iCOTe)=varid
              CASE ('idTvar(iCOTl)')
                idTvar(iCOTl)=varid
#endif

!#ifdef CARBON_ISOTOPE
!              CASE ('iHbio3(id13C)')  !!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Remove??
!                iHbio3(id13C)=varid
!#endif
#if defined DIAGNOSTICS_BIO
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

              CASE ('iDbio3(ipHt_)')
                iDbio3(ipHt_)=varid
              CASE ('iDbio3(iWarg)')
                iDbio3(iWarg)=varid
              CASE ('iDbio3(iWcal)')
                iDbio3(iWcal)=varid

!!! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
              CASE ('iDbio3(iDOCTot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Csp
                  varid=varid+1
                  iDbio3(iDOCTot(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio3(iPOCTot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Csp
                  varid=varid+1
                  iDbio3(iPOCTot(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio3(iDONTot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  iDbio3(iDONTot(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio3(iPONTot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  iDbio3(iPONTot(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio3(iDOPTot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Psp
                  varid=varid+1
                  iDbio3(iDOPTot(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio3(iPOPTot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Psp
                  varid=varid+1
                  iDbio3(iPOPTot(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio3(iPhyCTot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Csp
                  varid=varid+1
                  iDbio3(iPhyCTot(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio3(iZooCTot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Csp
                  varid=varid+1
                  iDbio3(iZooCTot(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio3(iPhyNTot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  iDbio3(iPhyNTot(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio3(iZooNTot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  iDbio3(iZooNTot(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio3(iPhyPTot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Psp
                  varid=varid+1
                  iDbio3(iPhyPTot(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio3(iZooPTot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Psp
                  varid=varid+1
                  iDbio3(iZooPTot(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio3(iPICTot(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Csp
                  varid=varid+1
                  iDbio3(iPICTot(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

!!! mons light model >>>>>s>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>KM:Add
# if defined LIGHT_MODEL
              CASE ('iDbio3(iLight)')
                iDbio3(iLight)=varid
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<KM:Add

!# ifdef CARBON_ISOTOPE
!              CASE ('iDbio3(id13C)')
!                iDbio3(id13C)=varid
!# endif
#endif

! ==== Coral parameters ===============================================
#ifdef CORAL_POLYP
              CASE ('iHbio2(iClTAcal(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iHbio2(iClTAcal(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iClTAcoe(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iHbio2(iClTAcoe(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iClDICcal(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  DO i=1,N_Csp
                    varid=varid+1
                    iHbio2(iClDICcal(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbio2(iClDICcoe(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  DO i=1,N_Csp
                    varid=varid+1
                    iHbio2(iClDICcoe(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbio2(iClDOcoe(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iHbio2(iClDOcoe(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iClQC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  DO i=1,N_Csp
                    varid=varid+1
                    iHbio2(iClQC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
# if defined CORAL_NONE_CO2_EQ
              CASE ('iHbio2(iClCO2cal(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  DO i=1,N_Csp
                    varid=varid+1
                    iHbio2(iClCO2cal(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbio2(iClCO2coe(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  DO i=1,N_Csp
                    varid=varid+1
                    iHbio2(iClCO2coe(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
# endif
# ifdef CORAL_ZOOXANTHELLAE
              CASE ('iHbio2(iClROS(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iHbio2(iClROS(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iZxDns(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iHbio2(iZxDns(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iZxQC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  DO i=1,N_Csp
                    varid=varid+1
                    iHbio2(iZxQC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbio2(iZxChl(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iHbio2(iZxChl(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iZxQAo(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iHbio2(iZxQAo(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iZxQAr(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iHbio2(iZxQAr(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iZxQAi(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iHbio2(iZxQAi(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iZxQAid(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iHbio2(iZxQAid(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
# endif
# ifdef CORAL_SIZE_DYNAMICS

# endif
# if defined DIAGNOSTICS_BIO
              CASE ('iDbio2(iClPg(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iDbio2(iClPg(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio2(iCl_R(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iDbio2(iCl_R(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio2(iCl_G(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iDbio2(iCl_G(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio2(iClPn(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iDbio2(iClPn(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
#  ifdef CORAL_CARBON_ISOTOPE
              CASE ('iDbio2(iClQCd13C(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iDbio2(iClQCd13C(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio2(iCl_Gd13C(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iDbio2(iCl_Gd13C(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
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
              CASE ('iDbio2(iZxPg(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iDbio2(iZxPg(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio2(iZx_R(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iDbio2(iZx_R(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio2(iZxPn(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iDbio2(iZxPn(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
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
              CASE ('iDbio2(iClmt(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iDbio2(iClmt(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio2(iClgw(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ncl
                  varid=varid+1
                  iDbio2(iClgw(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
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
              CASE ('iHbio2(iSgSgCBm(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  DO i=1,N_Csp
                    varid=varid+1
                    iHbio2(iSgSgCBm(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgSgNBm(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  DO i=1,N_Nsp
                    varid=varid+1
                    iHbio2(iSgSgNBm(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgSgPBm(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  DO i=1,N_Psp
                    varid=varid+1
                    iHbio2(iSgSgPBm(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgDICstock(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  DO i=1,N_Csp
                    varid=varid+1
                    iHbio2(iSgDICstock(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgCH2Ostock(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  DO i=1,N_Csp
                    varid=varid+1
                    iHbio2(iSgCH2Ostock(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgDOstock(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgDOstock(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgNO3stock(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  DO i=1,N_Nsp
                    varid=varid+1
                    iHbio2(iSgNO3stock(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgNH4stock(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  DO i=1,N_Nsp
                    varid=varid+1
                    iHbio2(iSgNH4stock(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgPO4stock(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  DO i=1,N_Psp
                    varid=varid+1
                    iHbio2(iSgPO4stock(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgLfCBm(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgLfCBm(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgRtCBm(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgRtCBm(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgTotSgCBmS(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgTotSgCBmS(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgTotSgCBm(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgTotSgCBm(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgTotLfCBm(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgTotLfCBm(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgTotRtCBm(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgTotRtCBm(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgLAI(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgLAI(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgTotLA(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgTotLA(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgGridELAP(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgGridELAP(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgGridPhot(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgGridPhot(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgGridGrow(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgGridGrow(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgPhotLim(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgPhotLim(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgGrowLim(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgGrowLim(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgGridResp(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgGridResp(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgGridNetPhot(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgGridNetPhot(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgGridDieoff(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgGridDieoff(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgInitC(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgInitC(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgInitN(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgInitN(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSgInitP(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nsg
                  varid=varid+1
                  iHbio2(iSgInitP(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
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
              CASE ('iDbio2(iAgPg(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nag
                  varid=varid+1
                  iDbio2(iAgPg(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio2(iAg_R(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nag
                  varid=varid+1
                  iDbio2(iAg_R(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iDbio2(iAgPn(j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nag
                  varid=varid+1
                  iDbio2(iAgPn(j))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,i2.2)')                   &
   &                    TRIM(ADJUSTL(Vinfo(1))), j
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', functional group ', j
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
              CASE ('iHbiosed3(iSdO2)')
                iHbiosed3(iSdO2)=varid
              CASE ('iHbiosed3(iSdTA)')
                iHbiosed3(iSdTA)=varid
              CASE ('iHbiosed3(iSdDIC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Csp
                  varid=varid+1
                  iHbiosed3(iSdDIC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdNO3(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  iHbiosed3(iSdNO3(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdNH4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  iHbiosed3(iSdNH4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdPO4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Psp
                  varid=varid+1
                  iHbiosed3(iSdPO4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdDOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Csp
                    varid=varid+1
                    iHbiosed3(iSdDOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdPOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Csp
                    varid=varid+1
                    iHbiosed3(iSdPOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdDON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Nsp
                    varid=varid+1
                    iHbiosed3(iSdDON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdPON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Nsp
                    varid=varid+1
                    iHbiosed3(iSdPON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdDOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Psp
                    varid=varid+1
                    iHbiosed3(iSdDOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdPOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Psp
                    varid=varid+1
                    iHbiosed3(iSdPOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdPIC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Csp
                  varid=varid+1
                  iHbiosed3(iSdPIC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdMn2)')
                iHbiosed3(iSdMn2)=varid
              CASE ('iHbiosed3(iSdMnO2)')
                iHbiosed3(iSdMnO2)=varid
              CASE ('iHbiosed3(iSdFe2)')
                iHbiosed3(iSdFe2)=varid
              CASE ('iHbiosed3(iSdFeS(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  iHbiosed3(iSdFeS(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdFeS2(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  iHbiosed3(iSdFeS2(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdFeOOH)')
                iHbiosed3(iSdFeOOH)=varid
              CASE ('iHbiosed3(iSdFeOOH_PO4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Psp
                  varid=varid+1
                  iHbiosed3(iSdFeOOH_PO4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdH2S(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  iHbiosed3(iSdH2S(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdSO4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  iHbiosed3(iSdSO4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbiosed3(iSdS0(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  iHbiosed3(iSdS0(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO
                varid=varid+1
              CASE ('iHbio2(iSdEfDO)')
                iHbio2(iSdEfDO)=varid
              CASE ('iHbio2(iSdEfTA)')
                iHbio2(iSdEfTA)=varid
              CASE ('iHbio2(iSdEfDIC)')
                iHbio2(iSdEfDIC)=varid
              CASE ('iHbio2(iSdEfNO3)')
                iHbio2(iSdEfNO3)=varid
              CASE ('iHbio2(iSdEfNH4)')
                iHbio2(iSdEfNH4)=varid
              CASE ('iHbio2(iSdEfPO4)')
                iHbio2(iSdEfPO4)=varid
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

/*
**  Biological tracers open boundary conditions.
*/
              CASE ('idTbry(iwest,iDO)')
                idTbry(iwest,iDO)=varid
              CASE ('idTbry(ieast,iDO)')
                idTbry(ieast,iDO)=varid
              CASE ('idTbry(isouth,iDO)')
                idTbry(isouth,iDO)=varid
              CASE ('idTbry(inorth,iDO)')
                idTbry(inorth,iDO)=varid

              CASE ('idTbry(iwest,iTA)')
                idTbry(iwest,iTA)=varid
              CASE ('idTbry(ieast,iTA)')
                idTbry(ieast,iTA)=varid
              CASE ('idTbry(isouth,iTA)')
                idTbry(isouth,iTA)=varid
              CASE ('idTbry(inorth,iTA)')
                idTbry(inorth,iTA)=varid

              CASE ('idTbry(iwest,iDIC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Csp
                  varid=varid+1
                  idTbry(iwest,iDIC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iDIC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Csp
                  varid=varid+1
                  idTbry(ieast,iDIC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iDIC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Csp
                  varid=varid+1
                  idTbry(isouth,iDIC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iDIC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Csp
                  varid=varid+1
                  idTbry(inorth,iDIC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iNO3(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  idTbry(iwest,iNO3(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iNO3(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  idTbry(ieast,iNO3(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iNO3(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  idTbry(isouth,iNO3(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iNO3(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  idTbry(inorth,iNO3(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iNH4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  idTbry(iwest,iNH4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iNH4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  idTbry(ieast,iNH4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iNH4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  idTbry(isouth,iNH4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iNH4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  idTbry(inorth,iNH4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iPO4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Psp
                  varid=varid+1
                  idTbry(iwest,iPO4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iPO4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Psp
                  varid=varid+1
                  idTbry(ieast,iPO4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iPO4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Psp
                  varid=varid+1
                  idTbry(isouth,iPO4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iPO4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Psp
                  varid=varid+1
                  idTbry(inorth,iPO4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iDOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(iwest,iDOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
               CASE ('idTbry(ieast,iDOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(ieast,iDOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(isouth,iDOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(isouth,iDOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(inorth,iDOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(inorth,iDOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idTbry(iwest,iPOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(iwest,iPOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(ieast,iPOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(ieast,iPOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(isouth,iPOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(isouth,iPOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(inorth,iPOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(inorth,iPOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idTbry(iwest,iDON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(iwest,iDON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
               CASE ('idTbry(ieast,iDON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(ieast,iDON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(isouth,iDON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(isouth,iDON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(inorth,iDON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(inorth,iDON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idTbry(iwest,iPON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(iwest,iPON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(ieast,iPON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(ieast,iPON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(isouth,iPON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(isouth,iPON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(inorth,iPON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(inorth,iPON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idTbry(iwest,iDOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(iwest,iDOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
               CASE ('idTbry(ieast,iDOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(ieast,iDOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(isouth,iDOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(isouth,iDOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(inorth,iDOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(inorth,iDOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idTbry(iwest,iPOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(iwest,iPOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(ieast,iPOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(ieast,iPOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(isouth,iPOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(isouth,iPOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(inorth,iPOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(inorth,iPOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idTbry(iwest,iPhyC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(iwest,iPhyC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(ieast,iPhyC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(ieast,iPhyC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(isouth,iPhyC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(isouth,iPhyC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(inorth,iPhyC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(inorth,iPhyC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idTbry(iwest,iZooC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(iwest,iZooC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(ieast,iZooC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(ieast,iZooC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(isouth,iZooC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(isouth,iZooC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(inorth,iZooC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(inorth,iZooC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idTbry(iwest,iPhyN(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(iwest,iPhyN(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(ieast,iPhyN(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(ieast,iPhyN(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(isouth,iPhyN(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(isouth,iPhyN(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(inorth,iPhyN(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(inorth,iPhyN(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idTbry(iwest,iZooN(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(iwest,iZooN(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(ieast,iZooN(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(ieast,iZooN(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(isouth,iZooN(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(isouth,iZooN(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(inorth,iZooN(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Nsp
                    varid=varid+1
                    idTbry(inorth,iZooN(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idTbry(iwest,iPhyP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(iwest,iPhyP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(ieast,iPhyP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(ieast,iPhyP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(isouth,iPhyP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(isouth,iPhyP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(inorth,iPhyP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(inorth,iPhyP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idTbry(iwest,iZooP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(iwest,iZooP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(ieast,iZooP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(ieast,iZooP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(isouth,iZooP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(isouth,iZooP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(inorth,iZooP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Psp
                    varid=varid+1
                    idTbry(inorth,iZooP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idTbry(iwest,iPIC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npim
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(iwest,iPIC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(ieast,iPIC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npim
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(ieast,iPIC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(isouth,iPIC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npim
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(isouth,iPIC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idTbry(inorth,iPIC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npim
                  DO i=1,N_Csp
                    varid=varid+1
                    idTbry(inorth,iPIC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
#if defined BLUE_TIDE         
              CASE ('idTbry(iwest,iH2S(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  idTbry(iwest,iH2S(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iH2S(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  idTbry(ieast,iH2S(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iH2S(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  idTbry(isouth,iH2S(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iH2S(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  idTbry(inorth,iH2S(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idTbry(iwest,iS0(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  idTbry(iwest,iS0(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(ieast,iS0(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  idTbry(ieast,iS0(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(isouth,iS0(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  idTbry(isouth,iS0(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idTbry(inorth,iS0(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  idTbry(inorth,iS0(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
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
              CASE ('idRtrc(iDO)')
                idRtrc(iDO)=varid
              CASE ('idRtrc(iTA)')
                idRtrc(iTA)=varid
              CASE ('idRtrc(iDIC(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Csp
                  varid=varid+1
                  idRtrc(iDIC(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idRtrc(iNO3(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  idRtrc(iNO3(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idRtrc(iNH4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Nsp
                  varid=varid+1
                  idRtrc(iNH4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
              CASE ('idRtrc(iPO4(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Psp
                  varid=varid+1
                  idRtrc(iPO4(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idRtrc(iDOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Csp
                    varid=varid+1
                    idRtrc(iDOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idRtrc(iPOC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Csp
                    varid=varid+1
                    idRtrc(iPOC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idRtrc(iDON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Nsp
                    varid=varid+1
                    idRtrc(iDON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idRtrc(iPON(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Nsp
                    varid=varid+1
                    idRtrc(iPON(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idRtrc(iDOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Ndom
                  DO i=1,N_Psp
                    varid=varid+1
                    idRtrc(iDOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idRtrc(iPOP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npom
                  DO i=1,N_Psp
                    varid=varid+1
                    idRtrc(iPOP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idRtrc(iPhyC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Csp
                    varid=varid+1
                    idRtrc(iPhyC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idRtrc(iZooC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Csp
                    varid=varid+1
                    idRtrc(iZooC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idRtrc(iPhyN(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Nsp
                    varid=varid+1
                    idRtrc(iPhyN(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idRtrc(iZooN(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Nsp
                    varid=varid+1
                    idRtrc(iZooN(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

              CASE ('idRtrc(iPhyP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nphy
                  DO i=1,N_Psp
                    varid=varid+1
                    idRtrc(iPhyP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1
              CASE ('idRtrc(iZooP(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Nzoo
                  DO i=1,N_Psp
                    varid=varid+1
                    idRtrc(iZooP(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1


              CASE ('idRtrc(iPIC(i,j))')
                load=.FALSE.
                varid=varid-1
                DO j=1,Npim
                  DO i=1,N_Csp
                    varid=varid+1
                    idRtrc(iPIC(i,j))=varid
                    DO ng=1,Ngrids
                      Fscale(varid,ng)=scale
                      Iinfo(1,varid,ng)=gtype
                    END DO
                    WRITE (Vname(1,varid),'(a,i2.2,a,i2.2)')            &
   &                      TRIM(ADJUSTL(Vinfo(1))), j,'_',i
                    WRITE (Vname(2,varid),'(a,a,i2.2,a,i2.2)')          &
   &                      TRIM(ADJUSTL(Vinfo(2)))                       &
                          ,', functional group ', j, ', tracer ', i
                    WRITE (Vname(3,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(3)))
                    WRITE (Vname(4,varid),'(a,a)')                      &
   &                      TRIM(Vname(1,varid)), ', scalar, series'
                    WRITE (Vname(5,varid),'(a)')                        &
   &                      TRIM(ADJUSTL(Vinfo(5)))
                  END DO
                END DO
                varid=varid+1

#if defined BLUE_TIDE         
              CASE ('idRtrc(iH2S(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  idRtrc(iH2S(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1

              CASE ('idRtrc(iS0(i))')
                load=.FALSE.
                varid=varid-1
                DO i=1,N_Ssp
                  varid=varid+1
                  idRtrc(iS0(i))=varid
                  DO ng=1,Ngrids
                    Fscale(varid,ng)=scale
                    Iinfo(1,varid,ng)=gtype
                  END DO
                  WRITE (Vname(1,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(1))), '_',i
                  WRITE (Vname(2,varid),'(a,a,i2.2)')                 &
   &                    TRIM(ADJUSTL(Vinfo(2))), ', tracer ', i
                  WRITE (Vname(3,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(3)))
                  WRITE (Vname(4,varid),'(a,a)')                      &
   &                    TRIM(Vname(1,varid)), ', scalar, series'
                  WRITE (Vname(5,varid),'(a)')                        &
   &                    TRIM(ADJUSTL(Vinfo(5)))
                END DO             
                varid=varid+1
#endif

#if defined COT_STARFISH
              CASE ('idRtrc(iCOTe)')
                idRtrc(iCOTe)=varid
              CASE ('idRtrc(iCOTl)')
                idRtrc(iCOTl)=varid
#endif


