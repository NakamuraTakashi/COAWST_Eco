      SUBROUTINE ana_psource (ng, tile, model)
!
!! svn $Id: ana_psource.h 830 2017-01-24 21:21:11Z arango $
!!======================================================================
!! Copyright (c) 2002-2018 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine sets analytical tracer and mass point Sources       !
!  and/or Sinks.  River runoff can be consider as a point source.      !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping
!
      integer, intent(in) :: ng, tile, model

#include "tile.h"
!
      CALL ana_psource_tile (ng, tile, model,                           &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
     &                       nnew(ng), knew(ng),                        &
     &                       OCEAN(ng) % zeta,                          &
     &                       OCEAN(ng) % ubar,                          &
     &                       OCEAN(ng) % vbar,                          &
#ifdef SOLVE3D
     &                       OCEAN(ng) % u,                             &
     &                       OCEAN(ng) % v,                             &
     &                       GRID(ng) % z_w,                            &
#endif
     &                       GRID(ng) % h,                              &
     &                       GRID(ng) % on_u,                           &
     &                       GRID(ng) % om_v)
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME(20)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_psource
!
!***********************************************************************
      SUBROUTINE ana_psource_tile (ng, tile, model,                     &
     &                             LBi, UBi, LBj, UBj,                  &
     &                             IminS, ImaxS, JminS, JmaxS,          &
     &                             nnew, knew,                          &
     &                             zeta, ubar, vbar,                    &
#ifdef SOLVE3D
     &                             u, v, z_w,                           &
#endif
     &                             h, on_u, om_v)
!***********************************************************************
!
      USE mod_param
      USE mod_parallel
      USE mod_scalars
#ifdef SEDIMENT
      USE mod_sediment
#endif
      USE mod_sources

#ifdef DISTRIBUTE
!
      USE distribute_mod, ONLY : mp_bcastf, mp_bcasti
      USE distribute_mod, ONLY : mp_collect, mp_reduce
#endif
!!! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> TN:Add
#if defined REEF_ECOSYS
      USE mod_biology
      USE mod_reef_ecosys_param
      USE mod_geochem
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< TN:Add
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nnew, knew
!
#ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: zeta(LBi:,LBj:,:)
      real(r8), intent(in) :: ubar(LBi:,LBj:,:)
      real(r8), intent(in) :: vbar(LBi:,LBj:,:)
# ifdef SOLVE3D
      real(r8), intent(in) :: u(LBi:,LBj:,:,:)
      real(r8), intent(in) :: v(LBi:,LBj:,:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
# endif
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(in) :: on_u(LBi:,LBj:)
      real(r8), intent(in) :: om_v(LBi:,LBj:)
#else
      real(r8), intent(in) :: zeta(LBi:UBi,LBj:UBj,3)
      real(r8), intent(in) :: ubar(LBi:UBi,LBj:UBj,3)
      real(r8), intent(in) :: vbar(LBi:UBi,LBj:UBj,3)
# ifdef SOLVE3D
      real(r8), intent(in) :: u(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(in) :: v(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:N(ng))
# endif
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: om_v(LBi:UBi,LBj:UBj)
#endif
!
!  Local variable declarations.
!
      integer :: Npts, NSUB, is, i, j, k, ised
      integer :: m  !!! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< TN:Add

      real(r8) :: Pspv = 0.0_r8
      real(r8), save :: area_east, area_west
      real(r8) :: cff, fac, my_area_east, my_area_west

#if defined DISTRIBUTE && defined SOLVE3D
      real(r8), dimension(Msrc(ng)*N(ng)) :: Pwrk
#endif
#if defined DISTRIBUTE
      real(r8), dimension(2) :: buffer

      character (len=3), dimension(2) :: io_handle
#endif

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  If initialization, set point Sources and/or Sinks locations.
!-----------------------------------------------------------------------
!
      IF (iic(ng).eq.ntstart(ng)) THEN
!
!  Set-up point Sources/Sink number (Nsrc), direction (Dsrc), I- and
!  J-grid locations (Isrc,Jsrc). Currently, the direction can be along
!  XI-direction (Dsrc = 0) or along ETA-direction (Dsrc > 0).  The
!  mass sources are located at U- or V-points so the grid locations
!  should range from 1 =< Isrc =< L  and  1 =< Jsrc =< M.
!
#if defined RIVERPLUME1
        IF (Master.and.DOMAIN(ng)%SouthWest_Test(tile)) THEN
          Nsrc(ng)=1
          SOURCES(ng)%Dsrc(Nsrc(ng))=0.0_r8
          SOURCES(ng)%Isrc(Nsrc(ng))=1
          SOURCES(ng)%Jsrc(Nsrc(ng))=50
        END IF
#elif defined RIVERPLUME2
        IF (Master.and.DOMAIN(ng)%SouthWest_Test(tile)) THEN
          Nsrc(ng)=1+Lm(ng)*2
          DO is=1,(Nsrc(ng)-1)/2
            SOURCES(ng)%Dsrc(is)=1.0_r8
            SOURCES(ng)%Isrc(is)=is
            SOURCES(ng)%Jsrc(is)=1
          END DO
          DO is=(Nsrc(ng)-1)/2+1,Nsrc(ng)-1
            SOURCES(ng)%Dsrc(is)=1.0_r8
            SOURCES(ng)%Isrc(is)=is-Lm(ng)
            SOURCES(ng)%Jsrc(is)=Mm(ng)+1
          END DO
          SOURCES(ng)%Dsrc(Nsrc(ng))=0.0_r8
          SOURCES(ng)%Isrc(Nsrc(ng))=1
          SOURCES(ng)%Jsrc(Nsrc(ng))=60
        END IF
#elif defined SED_TEST1
        IF (Master.and.DOMAIN(ng)%SouthWest_Test(tile)) THEN
          Nsrc(ng)=Mm(ng)*2
          DO is=1,Nsrc(ng)/2
            SOURCES(ng)%Dsrc(is)=0.0_r8
            SOURCES(ng)%Isrc(is)=1
            SOURCES(ng)%Jsrc(is)=is
          END DO
          DO is=Nsrc(ng)/2+1,Nsrc(ng)
            SOURCES(ng)%Dsrc(is)=0.0_r8
            SOURCES(ng)%Isrc(is)=Lm(ng)+1
            SOURCES(ng)%Jsrc(is)=is-Mm(ng)
          END DO
        END IF
!!! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> TN:Add
#elif defined SHIRAHO_REEF
        IF (Master.and.DOMAIN(ng)%SouthWest_Test(tile)) THEN
          Nsrc(ng)=2
          is = 1
          SOURCES(ng)%Dsrc(is)=0.0_r8
          SOURCES(ng)%Isrc(is)=9
          SOURCES(ng)%Jsrc(is)=58
          is = 2  ! Used for SGD input if SGD_ON is activated
          SOURCES(ng)%Dsrc(is)=0.0_r8
          SOURCES(ng)%Isrc(is)=-1 ! Set out of the domain
          SOURCES(ng)%Jsrc(is)=-1 ! Set out of the domain
        END IF
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< TN:Add
#else
        ana_psource.h: No values provided for Nsrc, Dsrc, Isrc, Jsrc.
#endif

#ifdef DISTRIBUTE
!
!  Broadcast point sources/sinks information to all nodes.
!
        CALL mp_bcasti (ng, iNLM, Nsrc(ng))
        CALL mp_bcasti (ng, iNLM, SOURCES(ng)%Isrc)
        CALL mp_bcasti (ng, iNLM, SOURCES(ng)%Jsrc)
        CALL mp_bcastf (ng, iNLM, SOURCES(ng)%Dsrc)
#endif
      END IF
!
!-----------------------------------------------------------------------
!  Set momentum point Sources and/or Sinks.
!-----------------------------------------------------------------------
!
      MOMENTUM : IF (LuvSrc(ng).or.LwSrc(ng)) THEN

#ifdef SOLVE3D
!
!  If appropriate, set-up nondimensional shape function to distribute
!  mass point sources/sinks vertically.  It must add to unity!!.
!
# ifdef DISTRIBUTE
        SOURCES(ng)%Qshape=Pspv
# endif
        Npts=Msrc(ng)*N(ng)

!$OMP BARRIER

# if defined SED_TEST1
        DO k=1,N(ng)
          DO is=1,Nsrc(ng)
            i=SOURCES(ng)%Isrc(is)
            j=SOURCES(ng)%Jsrc(is)
            IF (((IstrT.le.i).and.(i.le.IendT)).and.                    &
     &           ((JstrT.le.j).and.(j.le.JendT))) THEN
              IF (ubar(i,j,knew).ne.0.0_r8) THEN
                cff=ABS(u(i,j,k,nnew)/ubar(i,j,knew))
              ELSE
                cff=1.0_r8
              END IF
              SOURCES(ng)%Qshape(is,k)=cff*                             &
     &                                 (z_w(i-1,j,k    )-               &
     &                                  z_w(i-1,j,k-1  )+               &
     &                                  z_w(i  ,j,k    )-               &
     &                                  z_w(i  ,j,k-1  ))/              &
     &                                 (z_w(i-1,j,N(ng))-               &
     &                                  z_w(i-1,j,0    )+               &
     &                                  z_w(i  ,j,N(ng))-               &
     &                                  z_w(i  ,j,0    ))
            END IF
          END DO
        END DO
#  ifdef DISTRIBUTE
        Pwrk=RESHAPE(SOURCES(ng)%Qshape,(/Npts/))
        CALL mp_collect (ng, iNLM, Npts, Pspv, Pwrk)
        SOURCES(ng)%Qshape=RESHAPE(Pwrk,(/Msrc(ng),N(ng)/))
#  endif

# elif defined RIVERPLUME2
        DO k=1,N(ng)
          DO is=1,Nsrc(ng)-1
            i=SOURCES(ng)%Isrc(is)
            j=SOURCES(ng)%Jsrc(is)
            IF (((IstrT.le.i).and.(i.le.IendT)).and.                    &
     &          ((JstrT.le.j).and.(j.le.JendT))) THEN
              IF (vbar(i,j,knew).ne.0.0_r8) THEN
                cff=ABS(v(i,j,k,nnew)/vbar(i,j,knew))
              ELSE
                cff=1.0_r8
              END IF
              SOURCES(ng)%Qshape(is,k)=cff*                             &
     &                                 (z_w(i,j-1,k    )-               &
     &                                  z_w(i,j-1,k-1  )+               &
     &                                  z_w(i,j  ,k    )-               &
     &                                  z_w(i,j  ,k-1  ))/              &
     &                                 (z_w(i,j-1,N(ng))-               &
     &                                  z_w(i,j-1,0    )+               &
     &                                  z_w(i,j  ,N(ng))-               &
     &                                  z_w(i,j  ,0    ))
            END IF
          END DO
        END DO
        IF (Master.and.DOMAIN(ng)%SouthWest_Test(tile)) THEN
          DO k=1,N(ng)
            SOURCES(ng)%Qshape(Nsrc(ng),k)=1.0_r8/REAL(N(ng),r8)
          END DO
        END IF
#  ifdef DISTRIBUTE
        Pwrk=RESHAPE(SOURCES(ng)%Qshape,(/Npts/))
        CALL mp_collect (ng, iNLM, Npts, Pspv, Pwrk)
        SOURCES(ng)%Qshape=RESHAPE(Pwrk,(/Msrc(ng),N(ng)/))
#  endif

# else
!!
!!  Notice that there is not need for distributed-memory communications
!!  here since the computation below does not have a parallel tile
!!  dependency. All the nodes are computing this simple statement.
!!
        IF (DOMAIN(ng)%NorthEast_Test(tile)) THEN
          DO k=1,N(ng)
            DO is=1,Nsrc(ng)
              SOURCES(ng)%Qshape(is,k)=1.0_r8/REAL(N(ng),r8)
            END DO
          END DO
        END IF
# endif
#endif
!
!  Set-up vertically integrated mass transport (m3/s) of point
!  Sources/Sinks (positive in the positive U- or V-direction and
!  viceversa).
!
#ifdef DISTRIBUTE
        SOURCES(ng)%Qbar=Pspv
#endif

!$OMP BARRIER

#if defined RIVERPLUME1
        IF ((tdays(ng)-dstart).lt.0.5_r8) THEN
          fac=1.0_r8+TANH((time(ng)-43200.0_r8)/43200.0_r8)
        ELSE
          fac=1.0_r8
        END IF
        DO is=1,Nsrc(ng)
          SOURCES(ng)%Qbar(is)=fac*1500.0_r8
        END DO

#elif defined RIVERPLUME2
        DO is=1,(Nsrc(ng)-1)/2               ! North end
          i=SOURCES(ng)%Isrc(is)
          j=SOURCES(ng)%Jsrc(is)
          IF (((IstrT.le.i).and.(i.le.IendT)).and.                      &
     &        ((JstrT.le.j).and.(j.le.JendT))) THEN
            SOURCES(ng)%Qbar(is)=-0.05_r8*om_v(i,j)*                    &
     &                           (0.5_r8*(zeta(i,j-1,knew)+h(i,j-1)+    &
     &                                    zeta(i  ,j,knew)+h(i  ,j)))
          END IF
        END DO
        DO is=(Nsrc(ng)-1)/2+1,Nsrc(ng)-1    ! South end
          i=SOURCES(ng)%Isrc(is)
          j=SOURCES(ng)%Jsrc(is)
          IF (((IstrT.le.i).and.(i.le.IendT)).and.                      &
     &        ((JstrT.le.j).and.(j.le.JendT))) THEN
            SOURCES(ng)%Qbar(is)=-0.05_r8*om_v(i,j)*                    &
     &                           (0.5_r8*(zeta(i,j-1,knew)+h(i,j-1)+    &
     &                                    zeta(i  ,j,knew)+h(i  ,j)))
          END IF
        END DO
        IF (Master.and.DOMAIN(ng)%SouthWest_Test(tile)) THEN
          SOURCES(ng)%Qbar(Nsrc)=1500.0_r8   ! West wall
        END IF
# ifdef DISTRIBUTE
        CALL mp_collect (ng, iNLM, Msrc(ng), Pspv, SOURCES(ng)%Qbar)
# endif

#elif defined SED_TEST1
        my_area_west=0.0_r8                  ! West end
        fac=-36.0_r8*10.0_r8*1.0_r8
        DO is=1,Nsrc(ng)/2
          i=SOURCES(ng)%Isrc(is)
          j=SOURCES(ng)%Jsrc(is)
          IF (((IstrT.le.i).and.(i.le.IendT)).and.                      &
     &        ((JstrT.le.j).and.(j.le.JendT))) THEN
            cff=0.5_r8*(zeta(i-1,j,knew)+h(i-1,j)+                      &
     &                  zeta(i  ,j,knew)+h(i  ,j))*on_u(i,j)
            SOURCES(ng)%Qbar(is)=fac*cff
            my_area_west=my_area_west+cff
          END IF
        END DO
!
        my_area_east=0.0_r8                  ! East end
        fac=-36.0_r8*10.0_r8*1.0_r8
        DO is=Nsrc(ng)/2+1,Nsrc(ng)
          i=SOURCES(ng)%Isrc(is)
          j=SOURCES(ng)%Jsrc(is)
          IF (((IstrT.le.i).and.(i.le.IendT)).and.                      &
     &        ((JstrT.le.j).and.(j.le.JendT))) THEN
            cff=0.5_r8*(zeta(i-1,j,knew)+h(i-1,j)+                      &
     &                  zeta(i  ,j,knew)+h(i  ,j))*on_u(i,j)
            SOURCES(ng)%Qbar(is)=fac*cff
            my_area_east=my_area_east+cff
          END IF
        END DO
!
# ifdef DISTRIBUTE
        NSUB=1                           ! distributed-memory
# else
        IF (DOMAIN(ng)%SouthWest_Corner(tile).and.                      &
     &    DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          NSUB=1                         ! non-tiled application
        ELSE
          NSUB=NtileX(ng)*NtileE(ng)     ! tiled application
        END IF
# endif
!$OMP CRITICAL (PSOURCE)
        IF (tile_count.eq.0) THEN
          area_west=0.0_r8
          area_east=0.0_r8
        END IF
        area_west=area_west+my_area_west
        area_east=area_east+my_area_east
        tile_count=tile_count+1
        IF (tile_count.eq.NSUB) THEN
          tile_count=0
# ifdef DISTRIBUTE
          buffer(1)=area_west
          buffer(2)=area_east
          io_handle(1)='SUM'
          io_handle(2)='SUM'
          CALL mp_reduce (ng, iNLM, 2, buffer, io_handle)
          area_west=buffer(1)
          area_east=buffer(1)
# endif
          DO is=1,Nsrc(ng)/2
            SOURCES(ng)%Qbar(is)=Sources(ng)%Qbar(is)/area_west
          END DO
          DO is=Nsrc(ng)/2+1,Nsrc(ng)
            SOURCES(ng)%Qbar(is)=Sources(ng)%Qbar(is)/area_east
          END DO
        END IF
!$OMP END CRITICAL (PSOURCE)

# ifdef DISTRIBUTE
        CALL mp_collect (ng, iNLM, Msrc(ng), Pspv, SOURCES(ng)%Qbar)
# endif
!!! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> TN:Add
#elif defined SHIRAHO_REEF
        is=1  ! Todoroki river flow rate (m3/s)
        SOURCES(ng)%Qbar(is)=1.0_r8   ! ~1 m3/s in normal condition
        is=2  ! SGD rate (m3/s) in each grid cell (50m x 50m) area
        SOURCES(ng)%Qbar(is)=0.001_r8 !  SGD volume flux in each grid is calculated by:
                                      !   Qbar*pm(i,j)*pn(i,j)*sgd_src(i,j) 
                                      !   (sgd_src(i,j) map should be included in the grd file)
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< TN:Add
#else
        ana_psource.h: No values provided for Qbar.
#endif

#ifdef SOLVE3D
!
!  Set-up mass transport profile (m3/s) of point Sources/Sinks.
!
!$OMP BARRIER

        IF (DOMAIN(ng)%NorthEast_Test(tile)) THEN
          DO k=1,N(ng)
            DO is=1,Nsrc(ng)
              SOURCES(ng)%Qsrc(is,k)=SOURCES(ng)%Qbar(is)*              &
     &                               SOURCES(ng)%Qshape(is,k)
            END DO
          END DO
        END IF
#endif
      END IF MOMENTUM

#ifdef SOLVE3D
!
!-----------------------------------------------------------------------
!  Set tracers point Sources and/or Sinks.
!-----------------------------------------------------------------------
!
      TRACERS : IF (ANY(LtracerSrc(:,ng))) THEN
        SOURCES(ng)%Tsrc=0.0_r8                      ! initialize
!
!  Set-up tracer (tracer units) point Sources/Sinks.
!
# if defined RIVERPLUME1
#  ifdef ONE_TRACER_SOURCE
        IF (DOMAIN(ng)%NorthEast_Test(tile)) THEN
          SOURCES(ng)%Tsrc(itemp)=T0(ng)
          SOURCES(ng)%Tsrc(isalt)=0.0_r8
        END IF
#  elif defined TWO_D_TRACER_SOURCE
        IF (DOMAIN(ng)%NorthEast_Test(tile)) THEN
          SOURCES(ng)%Tsrc(is,itemp)=T0(ng)
          SOURCES(ng)%Tsrc(is,isalt)=0.0_r8
        END IF
#  else
        IF (DOMAIN(ng)%NorthEast_Test(tile)) THEN
          DO k=1,N(ng)
            DO is=1,Nsrc(ng)
              SOURCES(ng)%Tsrc(is,k,itemp)=T0(ng)
              SOURCES(ng)%Tsrc(is,k,isalt)=0.0_r8
            END DO
          END DO
        END IF
#  endif

# elif defined RIVERPLUME2
#  ifdef ONE_TRACER_SOURCE
        IF (DOMAIN(ng)%NorthEast_Test(tile)) THEN
          SOURCES(ng)%Tsrc(itemp)=T0(ng)
          SOURCES(ng)%Tsrc(isalt)=S0(ng)
        END IF
#  elif defined TWO_D_TRACER_SOURCE
        IF (DOMAIN(ng)%NorthEast_Test(tile)) THEN
          DO is=1,Nsrc(ng)-1
            SOURCES(ng)%Tsrc(is,itemp)=T0(ng)
            SOURCES(ng)%Tsrc(is,isalt)=S0(ng)
          END DO
        END IF
#  else
        IF (DOMAIN(ng)%NorthEast_Test(tile)) THEN
          DO k=1,N(ng)
            DO is=1,Nsrc(ng)-1
              SOURCES(ng)%Tsrc(is,k,itemp)=T0(ng)
              SOURCES(ng)%Tsrc(is,k,isalt)=S0(ng)
            END DO
            SOURCES(ng)%Tsrc(Nsrc(ng),k,itemp)=T0(ng)
            SOURCES(ng)%Tsrc(Nsrc(ng),k,isalt)=S0(ng)
          END DO
        END IF
#  endif
!!! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> TN:Add
# elif defined SHIRAHO_REEF
#  ifdef ONE_TRACER_SOURCE
        IF (DOMAIN(ng)%NorthEast_Test(tile)) THEN
          SOURCES(ng)%Tsrc(itemp)=T0(ng)
          SOURCES(ng)%Tsrc(isalt)=1.0_r8
        END IF
#  elif defined TWO_D_TRACER_SOURCE
        IF (DOMAIN(ng)%NorthEast_Test(tile)) THEN
          SOURCES(ng)%Tsrc(is,itemp)=T0(ng)
          SOURCES(ng)%Tsrc(is,isalt)=1.0_r8
        END IF
#  else
        IF (DOMAIN(ng)%NorthEast_Test(tile)) THEN
          DO k=1,N(ng)
!! ========= Todoroki river ================================================
            is=1
            SOURCES(ng)%Tsrc(is,k,itemp)=27.0_r8
            SOURCES(ng)%Tsrc(is,k,isalt)=1.0_r8
#   if defined SEDIMENT
            ised=1
            SOURCES(ng)%Tsrc(is,k,idsed(ised))=0.1_r8  ! SS ~100 mg/L in normal condition = 0.1 kg/m3  
            DO ised=2,NST
              SOURCES(ng)%Tsrc(is,k,idsed(ised))=0.0_r8
            END DO
#   endif
#   if defined REEF_ECOSYS
          ! Initialize all tracer values to be zero
            SOURCES(ng)%Tsrc(is,k,iDIC (1)  :iDIC (N_Csp)     ) = 0.0_r8
            SOURCES(ng)%Tsrc(is,k,iNO3 (1)  :iNO3 (N_Nsp)     ) = 0.0_r8
            SOURCES(ng)%Tsrc(is,k,iNH4 (1)  :iNH4 (N_Nsp)     ) = 0.0_r8
            SOURCES(ng)%Tsrc(is,k,iPO4 (1)  :iPO4 (N_Psp)     ) = 0.0_r8
            SOURCES(ng)%Tsrc(is,k,iDOC (1,1):iDOC (N_Csp,Ndom)) = 0.0_r8     ! umolC L-1
            SOURCES(ng)%Tsrc(is,k,iPOC (1,1):iPOC (N_Csp,Npom)) = 0.0_r8     ! umolC L-1
            SOURCES(ng)%Tsrc(is,k,iDON (1,1):iDON (N_Nsp,Ndom)) = 0.0_r8     ! umolN L-1
            SOURCES(ng)%Tsrc(is,k,iPON (1,1):iPON (N_Nsp,Npom)) = 0.0_r8     ! umolN L-1
            SOURCES(ng)%Tsrc(is,k,iDOP (1,1):iDOP (N_Psp,Ndom)) = 0.0_r8     ! umolP L-1
            SOURCES(ng)%Tsrc(is,k,iPOP (1,1):iPOP (N_Psp,Npom)) = 0.0_r8     ! umolP L-1
            SOURCES(ng)%Tsrc(is,k,iPhyC(1,1):iPhyC(N_Csp,Nphy)) = 0.0_r8     ! umolC L-1
            SOURCES(ng)%Tsrc(is,k,iZooC(1,1):iZooC(N_Csp,Nzoo)) = 0.0_r8     ! umolC L-1
            SOURCES(ng)%Tsrc(is,k,iPhyN(1,1):iPhyN(N_Nsp,Nphy)) = 0.0_r8     ! umolN L-1
            SOURCES(ng)%Tsrc(is,k,iZooN(1,1):iZooN(N_Nsp,Nzoo)) = 0.0_r8     ! umolN L-1
            SOURCES(ng)%Tsrc(is,k,iPhyP(1,1):iPhyP(N_Psp,Nphy)) = 0.0_r8     ! umolP L-1
            SOURCES(ng)%Tsrc(is,k,iZooP(1,1):iZooP(N_Psp,Nzoo)) = 0.0_r8     ! umolP L-1
            SOURCES(ng)%Tsrc(is,k,iPIC (1,1):iPIC (N_Csp,Npim)) = 0.0_r8     ! umolC L-1
  
          ! TA  
            SOURCES(ng)%Tsrc(is,k,iTA) = 3000.0_r8
          ! DIC  
            SOURCES(ng)%Tsrc(is,k,iDIC(iCt))  = 3000.0_r8
            SOURCES(ng)%Tsrc(is,k,iDIC(iC13)) = SOURCES(ng)%Tsrc(is,k,iDIC(iCt))  !! River Carbon tracer
          ! DO  
            SOURCES(ng)%Tsrc(is,k,iDO) =  200.0_r8
          ! NO3  
            SOURCES(ng)%Tsrc(is,k,iNO3(iNt))  = 150.0_r8 ! ~2 mg/L = 150 umol/L
            SOURCES(ng)%Tsrc(is,k,iNO3(iN15)) = SOURCES(ng)%Tsrc(is,k,iNO3(iNt))  !! River nitrogen tracer
          ! NH4   
            SOURCES(ng)%Tsrc(is,k,iNH4(iNt))  = 1.5_r8   ! 0.02 mg/L umol L-1
            SOURCES(ng)%Tsrc(is,k,iNH4(iN15)) = SOURCES(ng)%Tsrc(is,k,iNH4(iNt))  !! River nitrogen tracer
          ! PO4
            SOURCES(ng)%Tsrc(is,k,iPO4(iPt))  = 1.6_r8   !~0.05 mg/L= 1.6 umolP L-1
            SOURCES(ng)%Tsrc(is,k,iPO4(iP1))  = SOURCES(ng)%Tsrc(is,k,iPO4(iPt))  !! River phosphorus tracer
          ! DOC
            SOURCES(ng)%Tsrc(is,k,iDOC(iCt,iLDOM)) = 50.0_r8  ! ~2 mg/L = 170 umolC L-1
            SOURCES(ng)%Tsrc(is,k,iDOC(iCt,iRDOM)) = 120.0_r8     ! umolC L-1
          ! POC
            SOURCES(ng)%Tsrc(is,k,iPOC(iCt,iLPOM)) = 1.0_r8   ! ~0.5 mg/L = 4 umolC L-1
            SOURCES(ng)%Tsrc(is,k,iPOC(iCt,iRPOM)) = 4.0_r8     ! umolC L-1
            SOURCES(ng)%Tsrc(is,k,iPOC(iCt,iCPOM)) = 1.7_r8   ! ~1% of DOC+POC ! umolC L-1
          ! DON
            DO m=1,Ndom
              SOURCES(ng)%Tsrc(is,k,iDON(iNt,m)) = SOURCES(ng)%Tsrc(is,k,iDOC(iCt,m))*16.0_r8/106.0_r8     ! umolN L-1
            END DO
          ! PON
            DO m=1,Npom
              SOURCES(ng)%Tsrc(is,k,iPON(iNt,m)) = SOURCES(ng)%Tsrc(is,k,iPOC(iCt,m))*16.0_r8/106.0_r8     ! umolN L-1
            END DO
          ! DOP
            DO m=1,Ndom
              SOURCES(ng)%Tsrc(is,k,iDOP(iPt,m)) = SOURCES(ng)%Tsrc(is,k,iDOC(iCt,m))/106.0_r8     ! umolP L-1
            END DO
          ! POP
            DO m=1,Npom
              SOURCES(ng)%Tsrc(is,k,iPOP(iPt,m)) = SOURCES(ng)%Tsrc(is,k,iPOC(iCt,m))/106.0_r8     ! umolP L-1
            END DO
          ! PhyC        
            DO m=1,Nphy
              SOURCES(ng)%Tsrc(is,k,iPhyC(iCt,m)) = 0.0_r8
            END DO
          ! ZooC
            DO m=1,Nzoo
              SOURCES(ng)%Tsrc(is,k,iZooC(iCt,m)) = 0.0_r8
            END DO
          ! PhyN        
            DO m=1,Nphy
              SOURCES(ng)%Tsrc(is,k,iPhyN(iNt,m)) = 0.0_r8
            END DO
          ! ZooN
            DO m=1,Nzoo
              SOURCES(ng)%Tsrc(is,k,iZooN(iNt,m)) = 0.0_r8
            END DO
          ! PhyP      
            DO m=1,Nphy
              SOURCES(ng)%Tsrc(is,k,iPhyP(iPt,m)) = 0.0_r8
            END DO
          ! ZooP
            DO m=1,Nzoo
              SOURCES(ng)%Tsrc(is,k,iZooP(iPt,m)) = 0.0_r8
            END DO
          ! PIC
            SOURCES(ng)%Tsrc(is,k,iPIC(iCt,iLive)) = 0.0_r8 ! PIC_0(iLive,ng)     ! umolC L-1
            DO m=2,Npim
              SOURCES(ng)%Tsrc(is,k,iPIC(iCt,m)) = 0.0_r8     ! umolC L-1
            END DO
  
#    if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
            SOURCES(ng)%Tsrc(is,k,iDIC(iC13))                                              &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iDIC(iCt)),   d13C_DIC_0(ng), R13C_VPDB )
            DO m=1,Ndom
              SOURCES(ng)%Tsrc(is,k,iDOC(iC13,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iDOC(iCt,m)), d13C_DOC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Npom
              SOURCES(ng)%Tsrc(is,k,iPOC(iC13,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPOC(iCt,m)), d13C_POC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Nphy
              SOURCES(ng)%Tsrc(is,k,iPhyC(iC13,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPhyC(iCt,m)), d13C_PhyC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Nzoo
              SOURCES(ng)%Tsrc(is,k,iZooC(iC13,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iZooC(iCt,m)), d13C_ZooC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Npim
              SOURCES(ng)%Tsrc(is,k,iPIC(iC13,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPIC(iCt,m)), d13C_PIC_0(m,ng), R13C_VPDB )
            END DO
#     if defined CLUMPED_ISOTOPE
!**************** Under developpment *************************
            SOURCES(ng)%Tsrc(is,k,iDIC(iD47))                                              &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iDIC(iCt)),   D47_DIC_0(ng), R47D_???? )
            DO m=1,Ndom
              SOURCES(ng)%Tsrc(is,k,iDOC(iD47,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iDOC(iCt,m)), D47_DOC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Npom
              SOURCES(ng)%Tsrc(is,k,iPOC(iD47,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPOC(iCt,m)), D47_POC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Nphy
              SOURCES(ng)%Tsrc(is,k,iPhyC(iD47,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPhyC(iCt,m)), D47_PhyC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Nzoo
              SOURCES(ng)%Tsrc(is,k,iZooC(iD47,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iZooC(iCt,m)), D47_ZooC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Npim
              SOURCES(ng)%Tsrc(is,k,iPIC(iD47,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPIC(iCt,m)), D47_PIC_0(m,ng), R47D_???? )
            END DO
#     endif
#    endif
#    if defined NITROGEN_ISOTOPE
            SOURCES(ng)%Tsrc(is,k,iNO3(iN15))                                              &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iNO3(iNt)),   d15N_NO3_0(ng), R15N_AIR )
            SOURCES(ng)%Tsrc(is,k,iNH4(iN15))                                              &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iNH4(iNt)),   d15N_NH4_0(ng), R15N_AIR )
            DO m=1,Ndom
              SOURCES(ng)%Tsrc(is,k,iDON(iN15,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iDON(iNt,m)), d15N_DON_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Npom
              SOURCES(ng)%Tsrc(is,k,iPON(iN15,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPON(iNt,m)), d15N_PON_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Nphy
              SOURCES(ng)%Tsrc(is,k,iPhyN(iN15,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPhyN(iNt,m)), d15N_PhyN_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Nzoo
              SOURCES(ng)%Tsrc(is,k,iZooN(iN15,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iZooN(iNt,m)), d15N_ZooN_0(m,ng), R15N_AIR )
            END DO
#    endif
#    if defined COT_STARFISH
            SOURCES(ng)%Tsrc(is,k,iCOTe) = 0.0_r8     ! umolC L-1
            SOURCES(ng)%Tsrc(is,k,iCOTl) = 0.0_r8     ! umolC L-1
#    endif
#   endif

!! ========= SGD ================================================
            is=2
            SOURCES(ng)%Tsrc(is,k,itemp)=T0(ng)
            SOURCES(ng)%Tsrc(is,k,isalt)=1.0_r8
#   if defined SEDIMENT
            ised=1
            SOURCES(ng)%Tsrc(is,k,idsed(ised))=0.0_r8
            DO ised=2,NST
              SOURCES(ng)%Tsrc(is,k,idsed(ised))=0.0_r8
            END DO
#   endif
#   if defined REEF_ECOSYS
          ! Initialize all tracer values to be zero
            SOURCES(ng)%Tsrc(is,k,iDIC (1)  :iDIC (N_Csp)     ) = 0.0_r8
            SOURCES(ng)%Tsrc(is,k,iNO3 (1)  :iNO3 (N_Nsp)     ) = 0.0_r8
            SOURCES(ng)%Tsrc(is,k,iNH4 (1)  :iNH4 (N_Nsp)     ) = 0.0_r8
            SOURCES(ng)%Tsrc(is,k,iPO4 (1)  :iPO4 (N_Psp)     ) = 0.0_r8
            SOURCES(ng)%Tsrc(is,k,iDOC (1,1):iDOC (N_Csp,Ndom)) = 0.0_r8     ! umolC L-1
            SOURCES(ng)%Tsrc(is,k,iPOC (1,1):iPOC (N_Csp,Npom)) = 0.0_r8     ! umolC L-1
            SOURCES(ng)%Tsrc(is,k,iDON (1,1):iDON (N_Nsp,Ndom)) = 0.0_r8     ! umolN L-1
            SOURCES(ng)%Tsrc(is,k,iPON (1,1):iPON (N_Nsp,Npom)) = 0.0_r8     ! umolN L-1
            SOURCES(ng)%Tsrc(is,k,iDOP (1,1):iDOP (N_Psp,Ndom)) = 0.0_r8     ! umolP L-1
            SOURCES(ng)%Tsrc(is,k,iPOP (1,1):iPOP (N_Psp,Npom)) = 0.0_r8     ! umolP L-1
            SOURCES(ng)%Tsrc(is,k,iPhyC(1,1):iPhyC(N_Csp,Nphy)) = 0.0_r8     ! umolC L-1
            SOURCES(ng)%Tsrc(is,k,iZooC(1,1):iZooC(N_Csp,Nzoo)) = 0.0_r8     ! umolC L-1
            SOURCES(ng)%Tsrc(is,k,iPhyN(1,1):iPhyN(N_Nsp,Nphy)) = 0.0_r8     ! umolN L-1
            SOURCES(ng)%Tsrc(is,k,iZooN(1,1):iZooN(N_Nsp,Nzoo)) = 0.0_r8     ! umolN L-1
            SOURCES(ng)%Tsrc(is,k,iPhyP(1,1):iPhyP(N_Psp,Nphy)) = 0.0_r8     ! umolP L-1
            SOURCES(ng)%Tsrc(is,k,iZooP(1,1):iZooP(N_Psp,Nzoo)) = 0.0_r8     ! umolP L-1
            SOURCES(ng)%Tsrc(is,k,iPIC (1,1):iPIC (N_Csp,Npim)) = 0.0_r8     ! umolC L-1
  
          ! TA  
            SOURCES(ng)%Tsrc(is,k,iTA) = 3400.0_r8
          ! DIC  
            SOURCES(ng)%Tsrc(is,k,iDIC(iCt))  = 3400.0_r8
            SOURCES(ng)%Tsrc(is,k,iDIC(iC13)) = SOURCES(ng)%Tsrc(is,k,iDIC(iCt))  !! River Carbon tracer
          ! DO  
            SOURCES(ng)%Tsrc(is,k,iDO) =  10.0_r8
          ! NO3  
            SOURCES(ng)%Tsrc(is,k,iNO3(iNt))  = 260.0_r8 ! Umezawa et al., 2002
            SOURCES(ng)%Tsrc(is,k,iNO3(iN15)) = SOURCES(ng)%Tsrc(is,k,iNO3(iNt))  !! River nitrogen tracer
          ! NH4   
            SOURCES(ng)%Tsrc(is,k,iNH4(iNt))  = 0.1_r8   ! umol L-1
            SOURCES(ng)%Tsrc(is,k,iNH4(iN15)) = SOURCES(ng)%Tsrc(is,k,iNH4(iNt))  !! River nitrogen tracer
          ! PO4
            SOURCES(ng)%Tsrc(is,k,iPO4(iPt))  = 0.25_r8     ! umol L-1
            SOURCES(ng)%Tsrc(is,k,iPO4(iP1))  = SOURCES(ng)%Tsrc(is,k,iPO4(iPt))  !! River phosphorus tracer
          ! DOC
            SOURCES(ng)%Tsrc(is,k,iDOC(iCt,iLDOM)) = 10.0_r8     ! umolC L-1
            SOURCES(ng)%Tsrc(is,k,iDOC(iCt,iRDOM)) = 100.0_r8     ! umolC L-1
          ! POC
            DO m=1,Npom
              SOURCES(ng)%Tsrc(is,k,iPOC(iCt,m)) = 0.0_r8     ! umolC L-1
            END DO
          ! DON
            DO m=1,Ndom
              SOURCES(ng)%Tsrc(is,k,iDON(iNt,m)) = SOURCES(ng)%Tsrc(is,k,iDOC(iCt,m))*16.0_r8/106.0_r8     ! umolN L-1
            END DO
          ! PON
            DO m=1,Npom
              SOURCES(ng)%Tsrc(is,k,iPON(iNt,m)) = SOURCES(ng)%Tsrc(is,k,iPOC(iCt,m))*16.0_r8/106.0_r8     ! umolN L-1
            END DO
          ! DOP
            DO m=1,Ndom
              SOURCES(ng)%Tsrc(is,k,iDOP(iPt,m)) = SOURCES(ng)%Tsrc(is,k,iDOC(iCt,m))/106.0_r8     ! umolP L-1
            END DO
          ! POP
            DO m=1,Npom
              SOURCES(ng)%Tsrc(is,k,iPOP(iPt,m)) = SOURCES(ng)%Tsrc(is,k,iPOC(iCt,m))/106.0_r8     ! umolP L-1
            END DO
          ! PhyC        
            DO m=1,Nphy
              SOURCES(ng)%Tsrc(is,k,iPhyC(iCt,m)) = 0.0_r8
            END DO
          ! ZooC
            DO m=1,Nzoo
              SOURCES(ng)%Tsrc(is,k,iZooC(iCt,m)) = 0.0_r8
            END DO
          ! PhyN        
            DO m=1,Nphy
              SOURCES(ng)%Tsrc(is,k,iPhyN(iNt,m)) = 0.0_r8
            END DO
          ! ZooN
            DO m=1,Nzoo
              SOURCES(ng)%Tsrc(is,k,iZooN(iNt,m)) = 0.0_r8
            END DO
          ! PhyP      
            DO m=1,Nphy
              SOURCES(ng)%Tsrc(is,k,iPhyP(iPt,m)) = 0.0_r8
            END DO
          ! ZooP
            DO m=1,Nzoo
              SOURCES(ng)%Tsrc(is,k,iZooP(iPt,m)) = 0.0_r8
            END DO
          ! PIC
            SOURCES(ng)%Tsrc(is,k,iPIC(iCt,iLive)) = 0.0_r8 ! PIC_0(iLive,ng)     ! umolC L-1
            DO m=2,Npim
              SOURCES(ng)%Tsrc(is,k,iPIC(iCt,m)) = 0.0_r8     ! umolC L-1
            END DO
  
#    if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
            SOURCES(ng)%Tsrc(is,k,iDIC(iC13))                                              &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iDIC(iCt)),   d13C_DIC_0(ng), R13C_VPDB )
            DO m=1,Ndom
              SOURCES(ng)%Tsrc(is,k,iDOC(iC13,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iDOC(iCt,m)), d13C_DOC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Npom
              SOURCES(ng)%Tsrc(is,k,iPOC(iC13,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPOC(iCt,m)), d13C_POC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Nphy
              SOURCES(ng)%Tsrc(is,k,iPhyC(iC13,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPhyC(iCt,m)), d13C_PhyC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Nzoo
              SOURCES(ng)%Tsrc(is,k,iZooC(iC13,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iZooC(iCt,m)), d13C_ZooC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Npim
              SOURCES(ng)%Tsrc(is,k,iPIC(iC13,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPIC(iCt,m)), d13C_PIC_0(m,ng), R13C_VPDB )
            END DO
#     if defined CLUMPED_ISOTOPE
!**************** Under developpment *************************
            SOURCES(ng)%Tsrc(is,k,iDIC(iD47))                                              &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iDIC(iCt)),   D47_DIC_0(ng), R47D_???? )
            DO m=1,Ndom
              SOURCES(ng)%Tsrc(is,k,iDOC(iD47,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iDOC(iCt,m)), D47_DOC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Npom
              SOURCES(ng)%Tsrc(is,k,iPOC(iD47,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPOC(iCt,m)), D47_POC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Nphy
              SOURCES(ng)%Tsrc(is,k,iPhyC(iD47,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPhyC(iCt,m)), D47_PhyC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Nzoo
              SOURCES(ng)%Tsrc(is,k,iZooC(iD47,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iZooC(iCt,m)), D47_ZooC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Npim
              SOURCES(ng)%Tsrc(is,k,iPIC(iD47,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPIC(iCt,m)), D47_PIC_0(m,ng), R47D_???? )
            END DO
#     endif
#    endif
#    if defined NITROGEN_ISOTOPE
            SOURCES(ng)%Tsrc(is,k,iNO3(iN15))                                              &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iNO3(iNt)),   6.7_r8, R15N_AIR ) ! Umezawa et al., 2002
            SOURCES(ng)%Tsrc(is,k,iNH4(iN15))                                              &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iNH4(iNt)),   d15N_NH4_0(ng), R15N_AIR )
            DO m=1,Ndom
              SOURCES(ng)%Tsrc(is,k,iDON(iN15,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iDON(iNt,m)), d15N_DON_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Npom
              SOURCES(ng)%Tsrc(is,k,iPON(iN15,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPON(iNt,m)), d15N_PON_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Nphy
              SOURCES(ng)%Tsrc(is,k,iPhyN(iN15,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iPhyN(iNt,m)), d15N_PhyN_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Nzoo
              SOURCES(ng)%Tsrc(is,k,iZooN(iN15,m))                                          &
       &      = Ci_from_Ct_delta(SOURCES(ng)%Tsrc(is,k,iZooN(iNt,m)), d15N_ZooN_0(m,ng), R15N_AIR )
            END DO
#    endif
#    if defined COT_STARFISH
              SOURCES(ng)%Tsrc(is,k,iCOTe) = 0.0_r8     ! umolC L-1
              SOURCES(ng)%Tsrc(is,k,iCOTl) = 0.0_r8     ! umolC L-1
#    endif
#   endif
          END DO
        END IF
#  endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< TN:Add
# else
        ana_psource.h: No values provided for Tsrc.
# endif
      END IF TRACERS
#endif

      RETURN
      END SUBROUTINE ana_psource_tile
