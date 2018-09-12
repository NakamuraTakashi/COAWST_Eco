#include "cppdefs.h"
      MODULE bndwavebc_mod
#ifdef SOLVE3D
!
!svn $Id: bndwavebc_im.F 732 2008-09-07 01:55:51Z jcwarner $
!=======================================================================
!  Copyright (c) 2002-2018 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                           Hernan G. Arango   !
!                                                   John C. Warner     !
!                                                                      !
!  This subroutine sets lateral boundary conditions for the            !
!  water levels by adding the bound wave to zeta.                      !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: bndwavebc_tile

      CONTAINS
!
!***********************************************************************
      SUBROUTINE bndwavebc (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_inwave_swan
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: Insteps

# include "tile.h"

      Insteps=WAVES(ng)%Insteps
!
      CALL bndwavebc_tile (ng, tile,                                    &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     IminS, ImaxS, JminS, JmaxS,                  &
     &                     Insteps)
!     ,                                     &
!     &                     WAVES(ng)%bndwave)
      RETURN
      END SUBROUTINE bndwavebc

!
!***********************************************************************
      SUBROUTINE bndwavebc_tile (ng, tile,                              &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          Insteps)
!     ,                                &
!     &                          bndwave)
!***********************************************************************

      USE mod_ncparam
      USE mod_boundary
      USE mod_scalars
      USE mod_inwave_vars
      USE mod_inwave_swan
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, Insteps
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
# ifdef ASSUMED_SHAPE
!      real(r8), intent(in) :: bndwave(:,:)
# else
!      real(r8), intent(in) :: bndwave(MAX(Lm(ng),Mm(ng))+2,1:Insteps)
# endif
!
!  Local variable declarations.
!
      integer :: i, j, tidx

# include "set_bounds.h"

      tidx=MOD(iic(ng),Insteps)+1
!
!-----------------------------------------------------------------------
!  Lateral boundary conditions at the western edge.
!-----------------------------------------------------------------------
!
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
        IF (LBC(iwest,isFsur,ng)%acquire) THEN
          DO j=Jstr,Jend
            BOUNDARY(ng)%zeta_west(j)=BOUNDARY(ng)%zeta_west(j)+        &
     &                                WAVES(ng)%bndwave(j,tidx)
          END DO
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Lateral boundary conditions at the eastern edge.
!-----------------------------------------------------------------------
!
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
        IF (LBC(ieast,isFsur,ng)%acquire) THEN
          DO j=Jstr,Jend
            BOUNDARY(ng)%zeta_east(j)=BOUNDARY(ng)%zeta_east(j)+        &
     &                                WAVES(ng)%bndwave(j,tidx)
          END DO
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Lateral boundary conditions at the southern edge.
!-----------------------------------------------------------------------
!
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
        IF (LBC(isouth,isFsur,ng)%acquire) THEN
          DO i=Istr,Iend
            BOUNDARY(ng)%zeta_south(i)=BOUNDARY(ng)%zeta_south(i)+      &
     &                                 WAVES(ng)%bndwave(i,tidx)
          END DO
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Lateral boundary conditions at the northern edge.
!-----------------------------------------------------------------------
!
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
        IF (LBC(inorth,isFsur,ng)%acquire) THEN
          DO i=Istr,Iend
            BOUNDARY(ng)%zeta_north(i)=BOUNDARY(ng)%zeta_north(i)+      &
     &                                 WAVES(ng)%bndwave(i,tidx)
          END DO
        END IF
      END IF
!
      RETURN
      END SUBROUTINE bndwavebc_tile
#endif
      END MODULE bndwavebc_mod
