      SUBROUTINE ana_tobc_bio (ng, tile, model)
!
!! svn $Id$
!!======================================================================
!! Copyright (c) 2002-2015 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!=======================================================================
!================================================== Takashi Nakamura ===
!                                                                      !
!  This routine sets tracer-type variables open boundary conditions    !
!  using analytical expressions.                                       !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_boundary
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping
!
! Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model

#include "tile.h"
!
      CALL ana_tobc_bio_tile (ng, tile, model,                          &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    IminS, ImaxS, JminS, JmaxS,                   &
     &                    nstp(ng),                                     &
     &                    GRID(ng) % z_r,                               &
     &                    OCEAN(ng) % t)
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME(34)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_tobc_bio
!
!***********************************************************************
      SUBROUTINE ana_tobc_bio_tile (ng, tile, model,                    &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          nstp,                                   &
     &                          z_r, t)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
      USE mod_boundary
      USE mod_ncparam
      USE mod_ocean
#ifdef SEDIMENT
      USE mod_sediment
#endif
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
#ifdef REEF_ECOSYS
      USE mod_biology
      USE mod_reef_ecosys_param
      USE mod_geochem
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp

#ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: t(LBi:,LBj:,:,:,:)
#else
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#endif
!
!  Local variable declarations.
!
      integer :: i, ised, ip, j, k, isp, m
      real(r8) :: cff

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Tracers open boundary conditions.
!-----------------------------------------------------------------------
!
#if defined REEF_ECOSYS

! ---- Eastern boundary -----------------------------------------------
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
# if defined SEDIMENT && defined ANA_TOBC_SED
            DO ised=1,NST
              BOUNDARY(ng)%t_east(j,k,idsed(ised))=0.0_r8
            END DO
# endif

            CALL set_reef_ecosys_vertical_profile(                      &
     &             ng, -z_r(Iend+1,j,k), BOUNDARY(ng)%t_east(j,k,:) )

          END DO
        END DO
      END IF

! ---- Western boundary -----------------------------------------------
      IF (ANY(LBC(iwest,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Western_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
# if defined SEDIMENT && defined ANA_TOBC_SED
            DO ised=1,NST
              BOUNDARY(ng)%t_west(j,k,idsed(ised))=0.0_r8
            END DO
# endif

            CALL set_reef_ecosys_vertical_profile(                      &
     &             ng, -z_r(Istr-1,j,k), BOUNDARY(ng)%t_west(j,k,:) )

          END DO
        END DO
      END IF

! ---- Southern boundary -----------------------------------------------
      IF (ANY(LBC(isouth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Southern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
# if defined SEDIMENT && defined ANA_TOBC_SED
            DO ised=1,NST
              BOUNDARY(ng)%t_south(i,k,idsed(ised))=0.0_r8
            END DO
# endif

            CALL set_reef_ecosys_vertical_profile(                      &
     &             ng, -z_r(i,Jstr-1,k), BOUNDARY(ng)%t_south(i,k,:) )

          END DO
        END DO
      END IF

! ---- Northern boundary -----------------------------------------------
      IF (ANY(LBC(inorth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Northern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
# if defined SEDIMENT && defined ANA_TOBC_SED
            DO ised=1,NST
              BOUNDARY(ng)%t_north(i,k,idsed(ised))=0.0_r8
            END DO
# endif

            CALL set_reef_ecosys_vertical_profile(                      &
     &             ng, -z_r(i,Jend+1,k), BOUNDARY(ng)%t_north(i,k,:) )

          END DO
        END DO
      END IF

!==========================================================================      
#else
! ---- Eastern boundary -----------------------------------------------
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
# if defined T_PASSIVE && defined ANA_TOBC_PASSIVE
            DO ip=1,NPT
              BOUNDARY(ng)%t_east(j,k,inert(ip))=0.0_r8
            END DO
# endif
# if defined SEDIMENT && defined ANA_TOBC_SED
            DO ised=1,NST
              BOUNDARY(ng)%t_east(j,k,idsed(ised))=0.0_r8
            END DO
# endif
          END DO
        END DO
      END IF

! ---- Western boundary -----------------------------------------------
      IF (ANY(LBC(iwest,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Western_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
# if defined T_PASSIVE && defined ANA_TOBC_PASSIVE
            DO ip=1,NPT
              BOUNDARY(ng)%t_west(j,k,inert(ip))=0.0_r8
            END DO
# endif
# if defined SEDIMENT && defined ANA_TOBC_SED
            DO ised=1,NST
              BOUNDARY(ng)%t_west(j,k,idsed(ised))=0.0_r8
            END DO
# endif
          END DO
        END DO
      END IF

! ---- Southern boundary -----------------------------------------------
      IF (ANY(LBC(isouth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Southern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
# if defined T_PASSIVE && defined ANA_TOBC_PASSIVE
            DO ip=1,NPT
              BOUNDARY(ng)%t_south(i,k,inert(ip))=0.0_r8
            END DO
# endif
# if defined SEDIMENT && defined ANA_TOBC_SED
            DO ised=1,NST
              BOUNDARY(ng)%t_south(i,k,idsed(ised))=0.0_r8
            END DO
# endif
          END DO
        END DO
      END IF

! ---- Northern boundary -----------------------------------------------
      IF (ANY(LBC(inorth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Northern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
# if defined T_PASSIVE && defined ANA_TOBC_PASSIVE
            DO ip=1,NPT
              BOUNDARY(ng)%t_north(i,k,inert(ip))=0.0_r8
            END DO
# endif
# if defined SEDIMENT && defined ANA_TOBC_SED
            DO ised=1,NST
              BOUNDARY(ng)%t_north(i,k,idsed(ised))=0.0_r8
            END DO
# endif
          END DO
        END DO
      END IF
#endif

      RETURN
      END SUBROUTINE ana_tobc_bio_tile
