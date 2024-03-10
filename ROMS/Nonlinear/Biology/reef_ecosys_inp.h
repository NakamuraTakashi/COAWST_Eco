      SUBROUTINE read_BioPar (model, inp, out, Lwrite)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2013 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!================================================== Takashi Nakamura ===
!                                                                      !
!  This routine reads in Coral Reef ecosystem model input parameters.  !
!  They are specified in input script "bio_reef_ecosys.in".            !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_biology
      USE mod_ncparam
      USE mod_scalars
!
      implicit none
!
!  Imported variable declarations
!
      logical, intent(in) :: Lwrite
      integer, intent(in) :: model, inp, out
!
!  Local variable declarations.
!
      integer :: Npts, Nval
      integer :: iTrcStr, iTrcEnd
      integer :: i, ifield, igrid, itracer, itrc, ng, nline, status

      integer :: decode_line, load_i, load_l, load_lbc, load_r

      logical, dimension(Ngrids) :: Lbio
      logical, dimension(NBT,Ngrids) :: Ltrc
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
      logical, dimension(NHbio2d,Ngrids) :: LHbio2
      logical, dimension(NHbio3d,Ngrids) :: LHbio3
#if defined DIAGNOSTICS_BIO && defined REEF_ECOSYS /*!!!<<< TN:add REEF_ECOSYS*/
      logical, dimension(NDbio2d,Ngrids) :: LDbio2
      logical, dimension(NDbio3d,Ngrids) :: LDbio3
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
#if defined SEDIMENT_ECOSYS
      logical, dimension(NHbiosed3d,Ngrids) :: LHbiosed3
      integer, dimension(Ngrids) :: NsedTemporary
      real(r8), allocatable :: SedEcoTemp(:), SedEcoTemp1(:), SedEcoTemp2(:)
      integer :: k
      ! integer :: yt_debug
!!! yuta_seagrass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# if defined SEAGRASS
      real(r8), allocatable :: SgRtProfTemp(:), SgRtProfTemp1(:), SgRtProfTemp2(:)
      integer :: iSgSpecies
# endif
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

      real(r8), dimension(NBT,Ngrids) :: Rbio

      real(r8), dimension(100) :: Rval
#if defined ORGANIC_MATTER
      real(r8), dimension(Nphy,Ngrids) :: Rphyt
      real(r8), dimension(Nzoo,Ngrids) :: Rzoop
      real(r8), dimension(Ndom,Ngrids) :: Rdom
      real(r8), dimension(Npom,Ngrids) :: Rpom
      real(r8), dimension(Npim,Ngrids) :: Rpim
#endif
      character (len=40 ) :: KeyWord
      character (len=256) :: line
      character (len=256), dimension(200) :: Cval

      ! yt_debug = 0
!
!-----------------------------------------------------------------------
!  Initialize.
!-----------------------------------------------------------------------
!
      igrid=1                            ! nested grid counter
      itracer=0                          ! LBC tracer counter
      iTrcStr=1                          ! first LBC tracer to process
      iTrcEnd=NBT                        ! last  LBC tracer to process
      nline=0                            ! LBC multi-line counter
!
!-----------------------------------------------------------------------
!  Read in Coral Reef ecosystem parameters.
!-----------------------------------------------------------------------
!
      DO WHILE (.TRUE.)
        READ (inp,'(a)',ERR=10,END=20) line
        status=decode_line(line, KeyWord, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          SELECT CASE (TRIM(KeyWord))
            CASE ('Lbiology')
              Npts=load_l(Nval, Cval, Ngrids, Lbiology)
            CASE ('LReadBioINI')
              Npts=load_l(Nval, Cval, 2*Ngrids, LReadBioINI)
            CASE ('CrlIter')
              Npts=load_i(Nval, Rval, Ngrids, CrlIter)
            CASE ('SedIter')
              Npts=load_i(Nval, Rval, Ngrids, SedIter)
            CASE ('PARfrac')
              Npts=load_r(Nval, Rval, Ngrids, PARfrac)
            CASE ('pCO2air')
              Npts=load_r(Nval, Rval, Ngrids, pCO2air)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
            CASE ('TAlk0')
              Npts=load_r(Nval, Rval, Ngrids, TAlk0)
            CASE ('TIC_0')
              Npts=load_r(Nval, Rval, Ngrids, TIC_0)
            CASE ('Oxyg0')
              Npts=load_r(Nval, Rval, Ngrids, Oxyg0)
#if defined ORGANIC_MATTER
            CASE ('DOC_0')
              Npts=load_r(Nval, Rval, Ndom*Ngrids, Rdom)
              DO ng=1,Ngrids
                DO itrc=1,Ndom
                  DOC_0(itrc,ng)=Rdom(itrc,ng)
                END DO
              END DO
            CASE ('POC_0')
              Npts=load_r(Nval, Rval, Npom*Ngrids, Rpom)
              DO ng=1,Ngrids
                DO itrc=1,Npom
                  POC_0(itrc,ng)=Rpom(itrc,ng)
                END DO
              END DO
            CASE ('Phyt_0')
              Npts=load_r(Nval, Rval, Nphy*Ngrids, Rphyt)
              DO ng=1,Ngrids
                DO itrc=1,Nphy
                  Phyt_0(itrc,ng)=Rphyt(itrc,ng)
                END DO
              END DO
            CASE ('Zoop_0')
              Npts=load_r(Nval, Rval, Nzoo*Ngrids, Rzoop)
              DO ng=1,Ngrids
                DO itrc=1,Nzoo
                  Zoop_0(itrc,ng)=Rzoop(itrc,ng)
                END DO
              END DO
            CASE ('PIC_0')
              Npts=load_r(Nval, Rval, Npim*Ngrids, Rpim)
              DO ng=1,Ngrids
                DO itrc=1,Npim
                  PIC_0(itrc,ng)=Rpim(itrc,ng)
                END DO
              END DO
#endif
#if defined CARBON_ISOTOPE
            CASE ('d13C_TIC0')
              Npts=load_r(Nval, Rval, Ngrids, d13C_TIC0)
# if defined ORGANIC_MATTER
            CASE ('d13C_DOC_0')
              Npts=load_r(Nval, Rval, Ndom*Ngrids, Rdom)
              DO ng=1,Ngrids
                DO itrc=1,Ndom
                  d13C_DOC_0(itrc,ng)=Rdom(itrc,ng)
                END DO
              END DO
            CASE ('d13C_POC_0')
              Npts=load_r(Nval, Rval, Npom*Ngrids, Rpom)
              DO ng=1,Ngrids
                DO itrc=1,Npom
                  d13C_POC_0(itrc,ng)=Rpom(itrc,ng)
                END DO
              END DO
            CASE ('d13C_Phyt_0')
              Npts=load_r(Nval, Rval, Nphy*Ngrids, Rphyt)
              DO ng=1,Ngrids
                DO itrc=1,Nphy
                  d13C_Phyt_0(itrc,ng)=Rphyt(itrc,ng)
                END DO
              END DO
            CASE ('d13C_Zoop_0')
              Npts=load_r(Nval, Rval, Nzoo*Ngrids, Rzoop)
              DO ng=1,Ngrids
                DO itrc=1,Nzoo
                  d13C_Zoop_0(itrc,ng)=Rzoop(itrc,ng)
                END DO
              END DO
            CASE ('d13C_PIC_0')
              Npts=load_r(Nval, Rval, Npim*Ngrids, Rpom)
              DO ng=1,Ngrids
                DO itrc=1,Npim
                  d13C_PIC_0(itrc,ng)=Rpim(itrc,ng)
                END DO
              END DO
# endif
#endif
#if defined NUTRIENTS
            CASE ('NO3_0')
              Npts=load_r(Nval, Rval, Ngrids, NO3_0)
!            CASE ('NO2_0')
!              Npts=load_r(Nval, Rval, Ngrids, NO2_0)
            CASE ('NH4_0')
              Npts=load_r(Nval, Rval, Ngrids, NH4_0)
            CASE ('PO4_0')
              Npts=load_r(Nval, Rval, Ngrids, PO4_0)
# if defined ORGANIC_MATTER
            CASE ('DON_0')
              Npts=load_r(Nval, Rval, Ndom*Ngrids, Rdom)
              DO ng=1,Ngrids
                DO itrc=1,Ndom
                  DON_0(itrc,ng)=Rdom(itrc,ng)
                END DO
              END DO
            CASE ('PON_0')
              Npts=load_r(Nval, Rval, Npom*Ngrids, Rpom)
              DO ng=1,Ngrids
                DO itrc=1,Npom
                  PON_0(itrc,ng)=Rpom(itrc,ng)
                END DO
              END DO
            CASE ('DOP_0')
              Npts=load_r(Nval, Rval, Ndom*Ngrids, Rdom)
              DO ng=1,Ngrids
                DO itrc=1,Ndom
                  DOP_0(itrc,ng)=Rdom(itrc,ng)
                END DO
              END DO
            CASE ('POP_0')
              Npts=load_r(Nval, Rval, Npom*Ngrids, Rpom)
              DO ng=1,Ngrids
                DO itrc=1,Npom
                  POP_0(itrc,ng)=Rpom(itrc,ng)
                END DO
              END DO
# endif
# if defined NITROGEN_ISOTOPE
            CASE ('d15N_NO3_0')
              Npts=load_r(Nval, Rval, Ngrids, d15N_NO3_0)
!            CASE ('d15N_NO2_0')
!              Npts=load_r(Nval, Rval, Ngrids, d15N_NO2_0)
            CASE ('d15N_NH4_0')
              Npts=load_r(Nval, Rval, Ngrids, d15N_NH4_0)
#  if defined ORGANIC_MATTER
            CASE ('d15N_DON_0')
              Npts=load_r(Nval, Rval, Ndom*Ngrids, Rdom)
              DO ng=1,Ngrids
                DO itrc=1,Ndom
                  d15N_DON_0(itrc,ng)=Rdom(itrc,ng)
                END DO
              END DO
            CASE ('d15N_PON_0')
              Npts=load_r(Nval, Rval, Npom*Ngrids, Rpom)
              DO ng=1,Ngrids
                DO itrc=1,Npom
                  d15N_PON_0(itrc,ng)=Rpom(itrc,ng)
                END DO
              END DO
            CASE ('d15N_Phyt_0')
              Npts=load_r(Nval, Rval, Nphy*Ngrids, Rphyt)
              DO ng=1,Ngrids
                DO itrc=1,Nphy
                  d15N_Phyt_0(itrc,ng)=Rphyt(itrc,ng)
                END DO
              END DO
            CASE ('d15N_Zoop_0')
              Npts=load_r(Nval, Rval, Nzoo*Ngrids, Rzoop)
              DO ng=1,Ngrids
                DO itrc=1,Nzoo
                  d15Nzoo_0(itrc,ng)=Rzoop(itrc,ng)
                END DO
              END DO
#  endif
# endif
#endif
#if defined COT_STARFISH
            CASE ('COTe0')
              Npts=load_r(Nval, Rval, Ngrids, COTe0)
            CASE ('COTl0')
              Npts=load_r(Nval, Rval, Ngrids, COTl0)
#endif
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# ifdef SEDIMENT_ECOSYS
            CASE ('Nsed')
              Npts=load_i(Nval, Rval, Ngrids, NsedTemporary)
              ! write(*,*) 'yt_debug1: line = ', line
              DO ng=1,Ngrids
                Nsed(ng) = NsedTemporary(ng)
              ENDDO
              allocate ( SedEcoLayerDepths(Ngrids, maxval(Nsed)) )
              allocate ( SedEcoTemp(0) )
              allocate ( SeagrassRootProf(Ngrids, Nsg, maxval(Nsed)) )
              allocate ( SgRtProfTemp(0) )
            CASE ('SedEcoLayerDepths')
              write(*,*) 'yt_debug: SedEcoLayerDepths loading line: ', line
              ! yt_debug = yt_debug+1
              ! write(*,*) 'yt_debug2',yt_debug,':'
              SedEcoTemp1 = (/SedEcoTemp/)
              ! write(*,*) 'yt_debug3',yt_debug,': SedEcoTemp1 = ', SedEcoTemp1
              allocate (SedEcoTemp2(Nval))
              ! write(*,*) 'yt_debug4',yt_debug,': Nval = ', Nval
              Npts=load_r(Nval, Rval, Nval, SedEcoTemp2)
              ! write(*,*) 'yt_debug5',yt_debug,': SedEcoTemp2 = ', SedEcoTemp2
              SedEcoTemp = (/SedEcoTemp1, SedEcoTemp2/)
              ! write(*,*) 'yt_debug6',yt_debug,': SedEcoTemp = ', SedEcoTemp
              deallocate (SedEcoTemp1)
              deallocate (SedEcoTemp2)
              if(size(SedEcoTemp)==sum(Nsed)) then
                ! write(*,*) 'yt_debug7',yt_debug,':'
                i=0
                DO ng=1,Ngrids
                  DO k=1, Nsed(ng)
                    i=i+1
                    SedEcoLayerDepths(ng, k) = SedEcoTemp(i)
                  ENDDO
                ENDDO
                deallocate(SedEcoTemp)
                ! write(*,*) 'yt_debug8',yt_debug,': SedEcoLayerDepths = ',SedEcoLayerDepths
                write(*,*) 'yt_debug: SedEcoLayerDepths finished loading array.'
              elseif(size(SedEcoTemp)>sum(Nsed)) then
                write(*,*) 'yt_debug: SedEcoLayerDepths input array size does not match Nsed!'
                error stop
              endif
!!! yuta_seagrass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
#  ifdef SEAGRASS
            CASE ('SeagrassRootProf')
              write(*,*) 'yt_debug: SeagrassRootProf loading line: ', line
              SgRtProfTemp1 = (/SgRtProfTemp/)
              allocate (SgRtProfTemp2(Nval))
              Npts=load_r(Nval, Rval, Nval, SgRtProfTemp2)
              SgRtProfTemp = (/SgRtProfTemp1, SgRtProfTemp2/)
              deallocate (SgRtProfTemp1)
              deallocate (SgRtProfTemp2)
              if(size(SgRtProfTemp)==sum(Nsed)) then
                i=0
                DO ng=1,Ngrids
                  DO iSgSpecies=1,Nsg
                    DO k=1, Nsed(ng)
                      i=i+1
                      SeagrassRootProf(ng, iSgSpecies, k) = SgRtProfTemp(i)
                    ENDDO
                  ENDDO
                ENDDO
                deallocate(SgRtProfTemp)
                write(*,*) 'yt_debug: SeagrassRootProf finished loading array.'
              elseif(size(SgRtProfTemp)>sum(Nsed)) then
                write(*,*) 'yt_debug: SeagrassRootProf input array size does not match Nsed!'
                error stop
              endif
#  endif
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
            CASE ('TNU2')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  nl_tnu2(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('TNU4')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  nl_tnu4(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_TNU2')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_tnu2(i,ng)=Rbio(itrc,ng)
                  tl_tnu2(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_TNU4')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_tnu4(i,ng)=Rbio(itrc,ng)
                  ad_tnu4(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('AKT_BAK')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  Akt_bak(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_AKT_fac')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_Akt_fac(i,ng)=Rbio(itrc,ng)
                  tl_Akt_fac(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('TNUDG')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  Tnudg(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('LBC(isTvar)')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              ifield=isTvar(idbio(itracer))
              Npts=load_lbc(Nval, Cval, line, nline, ifield, igrid,     &
     &                      idbio(iTrcStr), idbio(iTrcEnd),             &
     &                      Vname(1,idTvar(idbio(itracer))), LBC)
#if defined ADJOINT || defined TANGENT || defined TL_IOMS
            CASE ('ad_LBC(isTvar)')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              ifield=isTvar(idbio(itracer))
              Npts=load_lbc(Nval, Cval, line, nline, ifield, igrid,     &
     &                      idbio(iTrcStr), idbio(iTrcEnd),             &
     &                      Vname(1,idTvar(idbio(itracer))), ad_LBC)
#endif
            CASE ('LtracerSrc')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerSrc(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('LtracerCLM')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerCLM(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('LnudgeTCLM')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LnudgeTCLM(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Hout(idTvar)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idTvar(idbio(', itrc, '))'
                    exit_flag=5
                    RETURN
                  END IF
                  Hout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Hout(idTsur)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTsur(idbio(itrc))
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idTsur(idbio(', itrc, '))'
                    exit_flag=5
                    RETURN
                  END IF
                  Hout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO

!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
            CASE ('Hout(iHbio2)')
              Npts=load_l(Nval, Cval, NHbio2d*Ngrids, LHbio2)
              DO ng=1,Ngrids
                DO itrc=1,NHbio2d
                  i=iHbio2(itrc)
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'iHbio2(', itrc, ')'
                    exit_flag=5
                    RETURN
                  END IF
                  Hout(i,ng)=LHbio2(itrc,ng)
                END DO
              END DO
            CASE ('Hout(iHbio3)')
              Npts=load_l(Nval, Cval, NHbio3d*Ngrids, LHbio3)
              DO ng=1,Ngrids
                DO itrc=1,NHbio3d
                  i=iHbio3(itrc)
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'iHbio3(', itrc, ')'
                    exit_flag=5
                    RETURN
                  END IF
                  Hout(i,ng)=LHbio3(itrc,ng)
                END DO
              END DO
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
            CASE ('Hout(iHbiosed3)')
              Npts=load_l(Nval, Cval, NHbiosed3d*Ngrids, LHbiosed3)
              DO ng=1,Ngrids
                DO itrc=1,NHbiosed3d
                  i=iHbiosed3(itrc)
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'iHbiosed3(', itrc, ')'
                    exit_flag=5
                    RETURN
                  END IF
                  Hout(i,ng)=LHbiosed3(itrc,ng)
                END DO
              END DO
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add
            CASE ('Qout(idTvar)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  Qout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Qout(idsurT)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idsurT(idbio(itrc))
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idsurT(idbio(', itrc, '))'
                    exit_flag=5
                    RETURN
                  END IF
                  Qout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Qout(idTsur)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTsur(idbio(itrc))
                  Qout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
            CASE ('Qout(iHbio2)')
              Npts=load_l(Nval, Cval, NHbio2d*Ngrids, LHbio2)
              DO ng=1,Ngrids
                DO itrc=1,NHbio2d
                  i=iHbio2(itrc)
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'iHbio2(', itrc, ')'
                    exit_flag=5
                    RETURN
                  END IF
                  Qout(i,ng)=LHbio2(itrc,ng)
                END DO
              END DO
            CASE ('Qout(iHbio3)')
              Npts=load_l(Nval, Cval, NHbio3d*Ngrids, LHbio3)
              DO ng=1,Ngrids
                DO itrc=1,NHbio3d
                  i=iHbio3(itrc)
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'iHbio3(', itrc, ')'
                    exit_flag=5
                    RETURN
                  END IF
                  Qout(i,ng)=LHbio3(itrc,ng)
                END DO
              END DO
            CASE ('Qout(iHbiosed3)')
              Npts=load_l(Nval, Cval, NHbiosed3d*Ngrids, LHbiosed3)
              DO ng=1,Ngrids
                DO itrc=1,NHbiosed3d
                  i=iHbiosed3(itrc)
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'iHbiosed3(', itrc, ')'
                    exit_flag=5
                    RETURN
                  END IF
                  Qout(i,ng)=LHbiosed3(itrc,ng)
                END DO
              END DO
#if defined DIAGNOSTICS_BIO && defined REEF_ECOSYS /*!!!<<< TN:add REEF_ECOSYS*/
            CASE ('Dout(iDbio2)')
              Npts=load_l(Nval, Cval, NDbio2d*Ngrids, LDbio2)
              DO ng=1,Ngrids
                DO itrc=1,NDbio2d
                  i=iDbio2(itrc)
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'iDbio2(', itrc, ')'
                    exit_flag=5
                    RETURN
                  END IF
                  Dout(i,ng)=LDbio2(itrc,ng)
                END DO
              END DO
            CASE ('Dout(iDbio3)')
              Npts=load_l(Nval, Cval, NDbio3d*Ngrids, LDbio3)
              DO ng=1,Ngrids
                DO itrc=1,NDbio3d
                  i=iDbio3(itrc)
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'iDbio3(', itrc, ')'
                    exit_flag=5
                    RETURN
                  END IF
                  Dout(i,ng)=LDbio3(itrc,ng)
                END DO
              END DO
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
            CASE ('Aout(idTvar)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idTTav)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idUTav)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idUTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idVTav)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idVTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(iHUTav)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=iHUTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(iHVTav)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=iHVTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
#ifdef DIAGNOSTICS_TS
            CASE ('Dout(iTrate)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTrate),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iThadv)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iThadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTxadv)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTxadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTyadv)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTyadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTvadv)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTvadv),ng)=Ltrc(i,ng)
                END DO
              END DO
# if defined TS_DIF2 || defined TS_DIF4
            CASE ('Dout(iThdif)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iThdif),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTxdif)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTxdif),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTydif)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTydif),ng)=Ltrc(i,ng)
                END DO
              END DO
#  if defined MIX_GEO_TS || defined MIX_ISO_TS
            CASE ('Dout(iTsdif)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTsdif),ng)=Ltrc(i,ng)
                END DO
              END DO
#  endif
# endif
            CASE ('Dout(iTvdif)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTvdif),ng)=Ltrc(i,ng)
                END DO
              END DO
#endif
          END SELECT
        END IF
      END DO
  10  IF (Master) WRITE (out,50) line
      exit_flag=4
      RETURN
  20  CONTINUE
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
!
!  Set various parameters.
!
      DO ng=1,Ngrids
!
!  Set switch to create history NetCDF file.
!
        IF ((nHIS(ng).gt.0).and.ANY(Hout(:,ng))) THEN
          LdefHIS(ng)=.TRUE.
        END IF
!
!  Set switch to create quicksave NetCDF file.
!
        IF ((nQCK(ng).gt.0).and.ANY(Qout(:,ng))) THEN
          LdefQCK(ng)=.TRUE.
        END IF
      END DO

!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
!
!-----------------------------------------------------------------------
!  Report input parameters.
!-----------------------------------------------------------------------
!
      IF (Lwrite) THEN
        DO ng=1,Ngrids
          IF (Lbiology(ng)) THEN
            WRITE (out,60) ng
            WRITE (out,70) CrlIter(ng), 'CrlIter',                      &
     &            'Number of iterations for nonlinear convergence.'
            WRITE (out,70) SedIter(ng), 'SedIter',                      &
     &            'Number of iterations for nonlinear convergence.'
            WRITE (out,90) PARfrac(ng), 'PARfrac',                      &
     &            'Fraction of shortwave radiation that is',            &
     &            'photosynthetically active (nondimensional).'
            WRITE (out,80) pCO2air(ng), 'pCO2air',                      &
     &            'CO2 partial pressure in air (ppm by volume).'
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
            WRITE (out,80) TAlk0(ng), 'TAlk0',                          &
     &            'Total alkalinity  (umol/kg).'
            WRITE (out,80) TIC_0(ng), 'TIC_0',                          &
     &            'Total dissolved inorganic carbon (umol/kg).'
            WRITE (out,80) Oxyg0(ng), 'Oxyg0',                          &
     &            'Dissolved oxygen (umol/L).'
#if defined ORGANIC_MATTER
            DO itrc=1,Ndom
              WRITE (out,140) DOC_0(itrc,ng), 'DOC_0', itrc,                          &
     &            'Dissolved organic carbon (umolC/L).'
            END DO
            DO itrc=1,Npom
              WRITE (out,140)  POC_0(itrc,ng), 'POC_0', itrc,                          &
     &            'Particulate organic carbon (umolC/L).'
            END DO
            DO itrc=1,Nphy
              WRITE (out,140) Phyt_0(itrc,ng), 'Phyt_0', itrc,                          &
     &            'Phytoplankton (umolC/L).'
            END DO
            DO itrc=1,Nzoo
              WRITE (out,140) Zoop_0(itrc,ng), 'Zoop_0', itrc,                          &
     &            'Zooplankton (umolC/L).'
            END DO
#endif
#if defined CARBON_ISOTOPE
            WRITE (out,80) d13C_TIC0(ng), 'd13C_TIC0',                      &
     &            'd13C of DIC (permil VPDB).'
# if defined ORGANIC_MATTER
            DO itrc=1,Ndom
              WRITE (out,140) d13C_DOC_0(itrc,ng), 'd13C_DOC_0', itrc,                      &
     &            'd13C of DOC (permil VPDB).'
            END DO
            DO itrc=1,Npom
              WRITE (out,140) d13C_POC_0(itrc,ng), 'd13C_POC_0', itrc,                      &
     &            'd13C of POC (permil VPDB).'
            END DO
            DO itrc=1,Nphy
              WRITE (out,140) d13C_Phyt_0(itrc,ng), 'd13C_Phyt_0', itrc,                      &
     &            'd13C of phytoplankton (permil VPDB).'
            END DO
            DO itrc=1,Nzoo
              WRITE (out,140) d13C_Zoop_0(itrc,ng), 'd13C_Zoop_0', itrc,                      &
     &            'd13C of zooplankton (permil VPDB).'
            END DO
            DO itrc=1,Npim
              WRITE (out,140) d13C_Zoop_0(itrc,ng), 'd13C_Zoop_0', itrc,                      &
     &            'd13C of zooplankton (permil VPDB).'
            END DO
# endif
#endif
#if defined NUTRIENTS
            WRITE (out,80) NO3_0(ng), 'NO3_0',                          &
     &            'NO3 (umol/L).'
!            WRITE (out,80) NO2_0(ng), 'NO2_0',                          &
!     &            'NO2 (umol/L).'
            WRITE (out,80) NH4_0(ng), 'NH4_0',                          &
     &            'NH4 (umol/L).'
            WRITE (out,80) PO4_0(ng), 'PO4_0',                          &
     &            'PO4 (umol/L).'
# if defined ORGANIC_MATTER
            DO itrc=1,Ndom
              WRITE (out,140) DON_0(itrc,ng), 'DON_0', itrc,                          &
     &            'DON (umolN/L).'
            END DO
            DO itrc=1,Npom
              WRITE (out,140) PON_0(itrc,ng), 'PON_0', itrc,                          &
     &            'PON (umolN/L).'
            END DO
            DO itrc=1,Ndom
              WRITE (out,140) DOP_0(itrc,ng), 'DOP_0', itrc,                          &
     &            'DOP (umolP/L).'
            END DO
            DO itrc=1,Npom
              WRITE (out,140) POP_0(itrc,ng), 'POP_0', itrc,                          &
     &            'POP (umolP/L).'
            END DO
# endif
# if defined NITROGEN_ISOTOPE
            WRITE (out,80) d15N_NO3_0(ng), 'd15N_NO3_0',                      &
     &            'd15N of NO3 (permil).'
!            WRITE (out,80) d15N_NO2_0(ng), 'd15N_NO2_0',                      &
!     &            'd15N of NO2 (permil).'
            WRITE (out,80) d15N_NH4_0(ng), 'd15N_NH4_0',                      &
     &            'd15N of NH4 (permil).'
#  if defined ORGANIC_MATTER
            DO itrc=1,Ndom
              WRITE (out,140) d15N_DON_0(itrc,ng), 'd15N_DOC_0', itrc,                      &
     &            'd15N of DOC (permil).'
            END DO
            DO itrc=1,Npom
              WRITE (out,140) d15N_PON_0(itrc,ng), 'd15N_POC_0', itrc,                      &
     &            'd15N of POC (permil).'
            END DO
            DO itrc=1,Ndom
              WRITE (out,140) d15N_Phyt_0(itrc,ng), 'd15N_Phyt_0', itrc,                      &
     &            'd15N of phytoplankton (permil).'
            END DO
            DO itrc=1,Npom
              WRITE (out,140) d15N_Zoop_0(itrc,ng), 'd15N_Zoop_0', itrc,                      &
     &            'd15N of zooplankton (permil).'
            END DO
#  endif
# endif
#endif
#if defined COT_STARFISH
            WRITE (out,80) COTe0(ng), 'COTe0',                          &
     &            'Eggs of crown-of thorns starfish (umolC/L).'
            WRITE (out,80) COTl0(ng), 'COTl0',                          &
     &            'Larvae of crown-of thorns starfish (umol/L).'
#endif
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# ifdef SEDIMENT_ECOSYS
            WRITE (out,70) Nsed(ng), 'Nsed',                      &
     &            'Number of biological sediment layers.'
            DO k=1,Nsed(ng)
              WRITE (out,140) SedEcoLayerDepths(ng,k), 'SedEcoLayerDepths', k,                      &
      &            'Depth (cm) from surface at bottom of each biological sediment layer.'
            ENDDO
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
#ifdef TS_DIF2
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,100) nl_tnu2(i,ng), 'nl_tnu2', i,              &
     &              'NLM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# ifdef ADJOINT
              WRITE (out,100) ad_tnu2(i,ng), 'ad_tnu2', i,              &
     &              'ADM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,100) tl_tnu2(i,ng), 'tl_tnu2', i,              &
     &              'TLM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
#ifdef TS_DIF4
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,100) nl_tnu4(i,ng), 'nl_tnu4', i,              &
     &              'NLM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# ifdef ADJOINT
              WRITE (out,100) ad_tnu4(i,ng), 'ad_tnu4', i,              &
     &              'ADM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,100) tl_tnu4(i,ng), 'tl_tnu4', i,              &
     &              'TLM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE(out,100) Akt_bak(i,ng), 'Akt_bak', i,               &
     &             'Background vertical mixing coefficient (m2/s)',     &
     &             'for tracer ', i, TRIM(Vname(1,idTvar(i)))
            END DO
#ifdef FORWARD_MIXING
            DO itrc=1,NBT
              i=idbio(itrc)
# ifdef ADJOINT
              WRITE (out,100) ad_Akt_fac(i,ng), 'ad_Akt_fac', i,        &
     &              'ADM basic state vertical mixing scale factor',     &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,100) tl_Akt_fac(i,ng), 'tl_Akt_fac', i,        &
     &              'TLM basic state vertical mixing scale factor',     &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,100) Tnudg(i,ng), 'Tnudg', i,                  &
     &              'Nudging/relaxation time scale (days)',             &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
            END DO
#ifdef TCLIMATOLOGY
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,110) LtracerCLM(i,ng), 'LtracerCLM',           &
     &              i, 'Processing climatology on tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
            END DO
#endif
#ifdef TS_PSOURCE
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,110) LtracerSrc(i,ng), 'LtracerSrc',           &
     &              i, 'Processing point sources/Sink on tracer ', i,   &
     &              TRIM(Vname(1,idTvar(i)))
            END DO
#endif
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (Hout(idTvar(i),ng)) WRITE (out,120)                   &
     &            Hout(idTvar(i),ng), 'Hout(idTvar)',                   &
     &            'Write out tracer ', i, TRIM(Vname(1,idTvar(i)))
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (Hout(idTsur(i),ng)) WRITE (out,120)                   &
     &            Hout(idTsur(i),ng), 'Hout(idTsur)',                   &
     &            'Write out tracer flux ', i, TRIM(Vname(1,idTvar(i)))
            END DO
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
            IF (NHbio2d.gt.0) THEN
              DO itrc=1,NHbio2d
                i=iHbio2(itrc)
                IF (Hout(i,ng)) WRITE (out,130)                         &
     &              Hout(i,ng), 'Hout(iHbio2)',                         &
     &              'Write out', TRIM(Vname(1,i))
              END DO
            END IF
            DO itrc=1,NHbio3d
              i=iHbio3(itrc)
              IF (Hout(i,ng)) WRITE (out,130)                           &
     &            Hout(i,ng), 'Hout(iHbio3)',                           &
     &            'Write out', TRIM(Vname(1,i))
            END DO
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
            DO itrc=1,NHbiosed3d
              i=iHbiosed3(itrc)
              IF (Hout(i,ng)) WRITE (out,130)                           &
     &            Hout(i,ng), 'Hout(iHbiosed3)',                        &
     &            'Write out', TRIM(Vname(1,i))
            END DO
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (Qout(idTvar(i),ng)) WRITE (out,120)                   &
     &            Qout(idTvar(i),ng), 'Qout(idTvar)',                   &
     &            'Write out tracer ', i, TRIM(Vname(1,idTvar(i)))
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (Qout(idTsur(i),ng)) WRITE (out,120)                   &
     &            Qout(idTsur(i),ng), 'Qout(idTsur)',                   &
     &            'Write out tracer flux ', i, TRIM(Vname(1,idTvar(i)))
            END DO
            IF (NHbio2d.gt.0) THEN
              DO itrc=1,NHbio2d
                i=iHbio2(itrc)
                IF (Qout(i,ng)) WRITE (out,130)                         &
     &              Qout(i,ng), 'Qout(iHbio2)',                         &
     &              'Write out', TRIM(Vname(1,i))
              END DO
            END IF
            DO itrc=1,NHbio3d
              i=iHbio3(itrc)
              IF (Qout(i,ng)) WRITE (out,130)                           &
     &            Qout(i,ng), 'Qout(iHbio3)',                           &
     &            'Write out', TRIM(Vname(1,i))
            END DO
            DO itrc=1,NHbiosed3d
              i=iHbiosed3(itrc)
              IF (Qout(i,ng)) WRITE (out,130)                           &
     &            Qout(i,ng), 'Qout(iHbiosed3)',                        &
     &            'Write out', TRIM(Vname(1,i))
            END DO
#if defined DIAGNOSTICS_BIO && defined REEF_ECOSYS /*!!!<<< TN:add REEF_ECOSYS*/
            IF (NDbio2d.gt.0) THEN
              DO itrc=1,NDbio2d
                i=iDbio2(itrc)
                IF (Dout(i,ng)) WRITE (out,130)                         &
     &              Dout(i,ng), 'Dout(iDbio2)',                         &
     &              'Write out', TRIM(Vname(1,i))
              END DO
            END IF
            DO itrc=1,NDbio3d
              i=iDbio3(itrc)
              IF (Dout(i,ng)) WRITE (out,130)                           &
     &            Dout(i,ng), 'Dout(iDbio3)',                           &
     &            'Write out', TRIM(Vname(1,i))
            END DO
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add

#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
            WRITE (out,'(1x)')
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (Aout(idTvar(i),ng)) WRITE (out,120)                   &
     &            Aout(idTvar(i),ng), 'Aout(idTvar)',                   &
     &            'Write out averaged tracer ', i,                      &
     &            TRIM(Vname(1,idTvar(i)))
            END DO
#endif
#ifdef DIAGNOSTICS_TS
            WRITE (out,'(1x)')
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTrate),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTrate)',                 &
     &              'Write out rate of change of tracer ', itrc,        &
     &              TRIM(Vname(1,idTvar(itrc)))
            END DO
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iThadv),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iThadv)',                 &
     &              'Write out horizontal advection, tracer ', itrc,    &
     &              TRIM(Vname(1,idTvar(itrc)))
            END DO
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTxadv),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTxadv)',                 &
     &              'Write out horizontal X-advection, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
            END DO
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTyadv),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTyadv)',                 &
     &              'Write out horizontal Y-advection, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
            END DO
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTvadv),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTvadv)',                 &
     &              'Write out vertical advection, tracer ', itrc,      &
     &              TRIM(Vname(1,idTvar(itrc)))
            END DO
# if defined TS_DIF2 || defined TS_DIF4
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iThdif),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iThdif)',                 &
     &              'Write out horizontal diffusion, tracer ', itrc,    &
     &              TRIM(Vname(1,idTvar(itrc)))
            END DO
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(i,iTxdif),ng))                            &
     &          WRITE (out,120) .TRUE., 'Dout(iTxdif)',                 &
     &              'Write out horizontal X-diffusion, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
            END DO
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTydif),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTydif)',                 &
     &              'Write out horizontal Y-diffusion, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
            END DO
#  if defined MIX_GEO_TS || defined MIX_ISO_TS
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTsdif),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTsdif)',                 &
     &              'Write out horizontal S-diffusion, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
            END DO
#  endif
# endif
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTvdif),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTvdif)',                 &
     &              'Write out vertical diffusion, tracer ', itrc,      &
     &              TRIM(Vname(1,idTvar(itrc)))
            END DO
#endif
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Rescale biological tracer parameters.
!-----------------------------------------------------------------------
!
!  Take the square root of the biharmonic coefficients so it can
!  be applied to each harmonic operator.
!
      DO ng=1,Ngrids
        DO itrc=1,NBT
          i=idbio(itrc)
          nl_tnu4(i,ng)=SQRT(ABS(nl_tnu4(i,ng)))
#ifdef ADJOINT
          ad_tnu4(i,ng)=SQRT(ABS(ad_tnu4(i,ng)))
#endif
#if defined TANGENT || defined TL_IOMS
          tl_tnu4(i,ng)=SQRT(ABS(tl_tnu4(i,ng)))
#endif
!
!  Compute inverse nudging coefficients (1/s) used in various tasks.
!
          IF (Tnudg(i,ng).gt.0.0_r8) THEN
            Tnudg(i,ng)=1.0_r8/(Tnudg(i,ng)*86400.0_r8)
          ELSE
            Tnudg(i,ng)=0.0_r8
          END IF
        END DO
      END DO

  30  FORMAT (/,' read_BioPar - variable info not yet loaded, ',        &
     &        a,i2.2,a)
  40  FORMAT (/,' read_BioPar - variable info not yet loaded, ',a)
  50  FORMAT (/,' read_BioPar - Error while processing line: ',/,a)
  60  FORMAT (/,/,' Reef Ecosys Model Parameters, Grid: ',i2.2,              &
     &        /,  ' =================================',/)
  70  FORMAT (1x,i10,2x,a,t30,a)
  80  FORMAT (1p,e11.4,2x,a,t30,a)
  90  FORMAT (1p,e11.4,2x,a,t30,a,/,t32,a)
 100  FORMAT (1p,e11.4,2x,a,'(',i2.2,')',t30,a,/,t32,a,i2.2,':',1x,a)
 110  FORMAT (10x,l1,2x,a,'(',i2.2,')',t30,a,i2.2,':',1x,a)
 120  FORMAT (10x,l1,2x,a,t30,a,i2.2,':',1x,a)
 130  FORMAT (10x,l1,2x,a,t30,a,1x,a)
 140  FORMAT (1p,e11.4,2x,a,': ',i2.2,t30,a)

      RETURN
      END SUBROUTINE read_BioPar
