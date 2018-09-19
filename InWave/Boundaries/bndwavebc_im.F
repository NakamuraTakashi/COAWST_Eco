#include "cppdefs.h"
#define VARY_ACBC
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
      PUBLIC  :: bndwavebc

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

      USE mod_grid
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
!  Local variable declarations.
!
      integer :: i, j, d, nt1, nt2, tidx
      real(r8):: cff, cff1, cff2, cff3, ramp
      real(r8):: phiw, cw, dist, toff, bndtot
      real(r8), parameter :: eps =1.0E-20_r8

# include "set_bounds.h"
# ifdef RAMP_INWAVE
      ramp=TANH((tdays(ng)-dstart)/0.14_r8)
# else
      ramp=1.0_r8
# endif

      tidx=MOD(iic(ng),Insteps)+1
!
!-----------------------------------------------------------------------
!  Lateral boundary conditions at the western edge.
!-----------------------------------------------------------------------
!
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
        IF (LBC(iwest,isFsur,ng)%acquire) THEN
          DO j=Jstr,Jend
# ifdef VARY_ACBC
            bndtot=0.0_r8
            DO d=1,ND
              cff1=0.5_r8*(WAVEP(ng)%cy(Istr-1,j  ,d)+                  &
                           WAVEP(ng)%cy(Istr-1,j+1,d))
              cff2=WAVEP(ng)%cx(Istr-1,j,d)
              cw=sqrt(cff1**2+cff2**2)
              dist=REAL(j,r8)/GRID(ng)%pn(Istr-1,j)*                    &
     &             sin(WAVEG(ng)%wd(d)-GRID(ng)%angler(Istr-1,j))
              toff=ABS(dist/(cw+eps))
              cff1=toff/dt(ng)
              nt1=tidx+INT(cff1)
              IF (nt1.le.0) nt1=MOD(nt1,WAVES(ng)%Insteps)
              IF (nt1.le.0) nt1=nt1+WAVES(ng)%Insteps
              nt2=nt1+1
              IF (nt2>WAVES(ng)%Insteps) nt2=nt2-WAVES(ng)%Insteps
              phiw=toff/dt(ng)-INT(toff/dt(ng))
              cff2=(1.0_r8-phiw)*WAVES(ng)%bndwave(nt1)+                &
     &                     phiw*WAVES(ng)%bndwave(nt2)
              cff2=cff2*WAVES(ng)%SDD(d)/WAVES(ng)%int
              bndtot=bndtot+cff2
            END DO
# else
            bndtot=WAVES(ng)%bndwave(tidx)
# endif
            BOUNDARY(ng)%zeta_west(j)=BOUNDARY(ng)%zeta_west(j)+        &
     &                                bndtot*ramp
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
#ifdef VARY_ACBC
            bndtot=0.0_r8
            DO d=1,ND
              cff1=0.5_r8*(WAVEP(ng)%cy(Iend+1,j  ,d)+                  &
                           WAVEP(ng)%cy(Iend+1,j+1,d))
              cff2=WAVEP(ng)%cx(Iend+1,j,d)
              cw=sqrt(cff1**2+cff2**2)
              dist=REAL(j,r8)/GRID(ng)%pn(Iend+1,j)*                    &
     &             sin(WAVEG(ng)%wd(d)-GRID(ng)%angler(Iend+1,j))
              toff=ABS(dist/(cw+eps))
              cff1=toff/dt(ng)
              nt1=tidx+INT(cff1)
              IF (nt1.le.0) nt1=MOD(nt1,WAVES(ng)%Insteps)
              IF (nt1.le.0) nt1=nt1+WAVES(ng)%Insteps
              nt2=nt1+1
              IF (nt2>WAVES(ng)%Insteps) nt2=nt2-WAVES(ng)%Insteps
              phiw=toff/dt(ng)-INT(toff/dt(ng))
              cff2=(1.0_r8-phiw)*WAVES(ng)%bndwave(nt1)+                &
     &                     phiw*WAVES(ng)%bndwave(nt2)
              cff2=cff2*WAVES(ng)%SDD(d)/WAVES(ng)%int
              bndtot=bndtot+cff2
            END DO
# else
            bndtot=WAVES(ng)%bndwave(tidx)
# endif
            BOUNDARY(ng)%zeta_east(j)=BOUNDARY(ng)%zeta_east(j)+        &
     &                                bndtot*ramp
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
#ifdef VARY_ACBC
            bndtot=0.0_r8
            DO d=1,ND
              cff1=0.5_r8*(WAVEP(ng)%cx(i  ,Jstr-1,d)+                  &
                           WAVEP(ng)%cx(i+1,Jstr-1,d))
              cff2=WAVEP(ng)%cy(i  ,Jstr-1,d)
              cw=sqrt(cff1**2+cff2**2)
              dist=REAL(i,r8)/GRID(ng)%pm(i,Jstr-1)*                    &
     &             sin(WAVEG(ng)%wd(d)-GRID(ng)%angler(i,Jstr-1))
              toff=ABS(dist/(cw+eps))
              cff1=toff/dt(ng)
              nt1=tidx+INT(cff1)
              IF (nt1.le.0) nt1=MOD(nt1,WAVES(ng)%Insteps)
              IF (nt1.le.0) nt1=nt1+WAVES(ng)%Insteps
              nt2=nt1+1
              IF (nt2>WAVES(ng)%Insteps) nt2=nt2-WAVES(ng)%Insteps
              phiw=toff/dt(ng)-INT(toff/dt(ng))
              cff2=(1.0_r8-phiw)*WAVES(ng)%bndwave(nt1)+                &
     &                     phiw*WAVES(ng)%bndwave(nt2)
              cff2=cff2*WAVES(ng)%SDD(d)/WAVES(ng)%int
              bndtot=bndtot+cff2
            END DO
# else
            bndtot=WAVES(ng)%bndwave(tidx)
# endif
            BOUNDARY(ng)%zeta_south(i)=BOUNDARY(ng)%zeta_south(i)+      &
     &                                bndtot*ramp
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
#ifdef VARY_ACBC
            bndtot=0.0_r8
            DO d=1,ND
              cff1=0.5_r8*(WAVEP(ng)%cx(i  ,Jend+1,d)+                  &
                           WAVEP(ng)%cx(i+1,Jend+1,d))
              cff2=WAVEP(ng)%cy(i  ,Jend+1,d)
              cw=sqrt(cff1**2+cff2**2)
              dist=REAL(i,r8)/GRID(ng)%pm(i,Jend+1)*                    &
     &             sin(WAVEG(ng)%wd(d)-GRID(ng)%angler(i,Jend+1))
              toff=ABS(dist/(cw+eps))
              cff1=toff/dt(ng)
              nt1=tidx+INT(cff1)
              IF (nt1.le.0) nt1=MOD(nt1,WAVES(ng)%Insteps)
              IF (nt1.le.0) nt1=nt1+WAVES(ng)%Insteps
              nt2=nt1+1
              IF (nt2>WAVES(ng)%Insteps) nt2=nt2-WAVES(ng)%Insteps
              phiw=toff/dt(ng)-INT(toff/dt(ng))
              cff2=(1.0_r8-phiw)*WAVES(ng)%bndwave(nt1)+                &
     &                     phiw*WAVES(ng)%bndwave(nt2)
              cff2=cff2*WAVES(ng)%SDD(d)/WAVES(ng)%int
              bndtot=bndtot+cff2
            END DO
# else
            bndtot=WAVES(ng)%bndwave(tidx)
# endif
            BOUNDARY(ng)%zeta_north(i)=BOUNDARY(ng)%zeta_north(i)+      &
     &                                bndtot*ramp
          END DO
        END IF
      END IF
!
      RETURN
      END SUBROUTINE bndwavebc_tile
#endif
      END MODULE bndwavebc_mod
