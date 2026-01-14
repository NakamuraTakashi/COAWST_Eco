      SUBROUTINE ana_passive (ng, tile, model)
!
!! svn $Id: ana_passive.h 830 2017-01-24 21:21:11Z arango $
!!======================================================================
!! Copyright (c) 2002-2018 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets initial conditions for passive inert tracers      !
!  using analytical expressions.                                       !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_ncparam
      USE mod_ocean
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MY:add
      USE mod_grid
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MY:add
!
! Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model

#include "tile.h"
!
      CALL ana_passive_tile (ng, tile, model,                           &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MY:add
     &                       GRID(ng) % Hz,                             &
     &                       GRID(ng) % h,                              &
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MY:add
     &                       OCEAN(ng) % t)
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME(18)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_passive
!
!***********************************************************************
      SUBROUTINE ana_passive_tile (ng, tile, model,                     &
     &                             LBi, UBi, LBj, UBj,                  &
     &                             IminS, ImaxS, JminS, JmaxS,          &
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MY:add
     &                             Hz,                                  &
     &                             h,                                   &
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MY:add
     &                             t)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
#ifdef ASSUMED_SHAPE
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MY:add
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)                                    ! Thicknesses (m) of vertical RHO-points.
      real(r8), intent(in) ::  h(LBi:,LBj:)                                      ! Bottom depth (m) at RHO-points.
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MY:add
      real(r8), intent(out) :: t(LBi:,LBj:,:,:,:)
#else
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MY:add
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,N(ng))                         ! Thicknesses (m) of vertical RHO-points.
      real(r8), intent(in) ::  h(LBi:UBi,LBj:UBj)                               ! Bottom depth (m) at RHO-points.
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MY:add
      real(r8), intent(out) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#endif
!
!  Local variable declarations.
!
      integer :: i, iage, ip, itrc, j, k
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MY:add
      real(r8), parameter :: crit_dep = 5.0_r8                                  ! Critical water depth to add dye tracer (m)
      real(r8), parameter :: Inival = 0.0_r8
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,0:N(ng)) :: dab               ! Depth from the water surface (m)
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MY:add

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Set analytical initial conditions for passive inert tracers.
!-----------------------------------------------------------------------
!
#if defined DYE_RELEASE
# ifdef AGE_MEAN
      DO ip=1,NPT,2
        itrc=inert(ip)
        iage=inert(ip+1)
        DO k=1,N(ng)
          DO j=JstrT,JendT
            DO i=IstrT,IendT
              t(i,j,k,1,itrc)=0.001_r8
              t(i,j,k,2,itrc)=t(i,j,k,1,itrc)
              t(i,j,k,1,iage)=0.0_r8
              t(i,j,k,2,iage)=t(i,j,k,1,iage)
            END DO
          END DO
        END DO
      END DO
# else
      DO ip=1,NPT
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MY:add
! Initialization of depth from the water surface (m)
        dab = Inival
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MY:add
        itrc=inert(ip)
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MY:rm
!        DO k=1,N(ng)
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MY:rm
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MY:add
        DO k=N(ng),1,-1
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MY:add
          DO j=JstrT,JendT
            DO i=IstrT,IendT
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MY:add
! Calculate depth from the water surface (m)
              if (k == N(ng)) then
                dab(i,j,k)=Hz(i,j,k)
              else
                dab(i,j,k)=dab(i,j,k+1)+Hz(i,j,k)
              end if
! Place dye at grids with water depth below crit_dep
              if (dab(i,j,k) > crit_dep .and. h(i,j) > crit_dep) then
                t(i,j,k,1,itrc)=1.0_r8
              else
                t(i,j,k,1,itrc)=Inival
              end if
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MY:add
              t(i,j,k,2,itrc)=t(i,j,k,1,itrc)
            END DO
          END DO
        END DO
      END DO
# endif
#else
      ana_passive.h: no values provided for t(:,:,:,1,inert(itrc))
#endif

      RETURN
      END SUBROUTINE ana_passive_tile
