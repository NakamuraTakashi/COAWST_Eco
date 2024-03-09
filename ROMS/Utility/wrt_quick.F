#include "cppdefs.h"
      SUBROUTINE wrt_quick (ng, tile)
!
!svn $Id: wrt_quick.F 858 2017-07-31 23:02:30Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2018 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine writes requested model fields at requested levels      !
!  into ROMS QUICKSAVE NetCDF file.                                    !
!                                                                      !
!  Notice that only momentum is affected by the full time-averaged     !
!  masks.  If applicable, these mask contains information about        !
!  river runoff and time-dependent wetting and drying variations.      !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
#ifdef BBL_MODEL
      USE mod_bbl
#endif
#ifdef ADJUST_BOUNDARY
      USE mod_boundary
#endif
#ifdef SOLVE3D
      USE mod_coupling
#endif
      USE mod_forces
      USE mod_grid
#ifdef ICE_MODEL
      USE mod_ice
#endif
      USE mod_iounits
      USE mod_mixing
      USE mod_ncparam
      USE mod_netcdf
      USE mod_ocean
      USE mod_scalars
#if defined SEDIMENT || defined BBL_MODEL
      USE mod_sedbed
      USE mod_sediment
#endif
#if defined VEGETATION
      USE mod_vegetation
      USE mod_vegarr
#endif
#if defined BIOLOGY
      USE mod_biology
#endif
      USE mod_stepping
!
      USE nf_fwrite2d_mod,     ONLY : nf_fwrite2d
#ifdef ADJUST_BOUNDARY
      USE nf_fwrite2d_bry_mod, ONLY : nf_fwrite2d_bry
#endif
#ifdef SOLVE3D
      USE nf_fwrite3d_mod,     ONLY : nf_fwrite3d
# ifdef ADJUST_BOUNDARY
      USE nf_fwrite3d_bry_mod, ONLY : nf_fwrite3d_bry
# endif
      USE omega_mod,           ONLY : scale_omega
#endif
      USE uv_rotate_mod,       ONLY : uv_rotate2d
#ifdef SOLVE3D
      USE uv_rotate_mod,       ONLY : uv_rotate3d
#endif
      USE strings_mod,         ONLY : FoundError
#ifdef INWAVE_MODEL
      USE mod_inwave_params
      USE mod_inwave_vars
#endif
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: LBi, UBi, LBj, UBj
#ifdef ADJUST_BOUNDARY
      integer :: LBij, UBij
#endif
      integer :: Fcount, gfactor, gtype, status
#ifdef SOLVE3D
      integer :: i, itrc, j, k
#endif
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
# ifdef REEF_ECOSYS
      integer :: ifield
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
      real(r8) :: scale

      real(r8), allocatable :: Ur2d(:,:)
      real(r8), allocatable :: Vr2d(:,:)
#ifdef SOLVE3D
      real(r8), allocatable :: Ur3d(:,:,:)
      real(r8), allocatable :: Vr3d(:,:,:)
      real(r8), allocatable :: Wr3d(:,:,:)
#endif
#ifdef BBL_MODEL
      real(r8), allocatable :: wrk2(:,:)
#endif
# include "set_bounds.h"
!
      SourceFile=__FILE__
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
#ifdef ADJUST_BOUNDARY
      LBij=BOUNDS(ng)%LBij
      UBij=BOUNDS(ng)%UBij
#endif
!
!-----------------------------------------------------------------------
!  Write out quicksave fields.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
!
!  Set grid type factor to write full (gfactor=1) fields or water
!  points (gfactor=-1) fields only.
!
#if defined WRITE_WATER && defined MASKING
      gfactor=-1
#else
      gfactor=1
#endif
!
!  Set time record index.
!
      QCK(ng)%Rindex=QCK(ng)%Rindex+1
      Fcount=QCK(ng)%Fcount
      QCK(ng)%Nrec(Fcount)=QCK(ng)%Nrec(Fcount)+1
!
!  Write out model time (s).
!
      CALL netcdf_put_fvar (ng, iNLM, QCK(ng)%name,                     &
     &                      TRIM(Vname(idtime,ng)), time(ng:),          &
     &                      (/QCK(ng)%Rindex/), (/1/),                  &
     &                      ncid = QCK(ng)%ncid,                        &
     &                      varid = QCK(ng)%Vid(idtime))
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

#if defined SEDIMENT && defined SED_MORPH
!
!  Write out time-dependent bathymetry (m)
!
      IF (Qout(idBath,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idbath), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     GRID(ng) % h,                                &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idbath)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#ifdef WET_DRY
!
!  Write out wet/dry mask at PSI-points.
!
      scale=1.0_r8
      gtype=gfactor*p2dvar
      status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idPwet),   &
     &                   QCK(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % pmask,                              &
# endif
     &                   GRID(ng) % pmask_wet,                          &
     &                   SetFillVal = .FALSE.)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
     &               __FILE__)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idPwet)), QCK(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out wet/dry mask at RHO-points.
!
      scale=1.0_r8
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idRwet),   &
     &                   QCK(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % rmask,                              &
# endif
     &                   GRID(ng) % rmask_wet,                          &
     &                   SetFillVal = .FALSE.)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
     &               __FILE__)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idRwet)), QCK(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out wet/dry mask at U-points.
!
      scale=1.0_r8
      gtype=gfactor*u2dvar
      status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUwet),   &
     &                   QCK(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % umask,                              &
# endif
     &                   GRID(ng) % umask_wet,                          &
     &                   SetFillVal = .FALSE.)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
     &               __FILE__)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idUwet)), QCK(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out wet/dry mask at V-points.
!
      scale=1.0_r8
      gtype=gfactor*v2dvar
      status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVwet),   &
     &                   QCK(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % vmask,                              &
# endif
     &                   GRID(ng) % vmask_wet,                          &
     &                   SetFillVal = .FALSE.)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
     &               __FILE__)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idVwet)), QCK(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
#endif
#ifdef SOLVE3D
!
!  Write time-varying depths of RHO-points.
!
      IF (Qout(idpthR,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idpthR), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     GRID(ng) % z_r)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idpthR)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write time-varying depths of U-points.
!
      IF (Qout(idpthU,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u3dvar
        DO k=1,N(ng)
          DO j=Jstr-1,Jend+1
            DO i=IstrU-1,Iend+1
              GRID(ng)%z_v(i,j,k)=0.5_r8*(GRID(ng)%z_r(i-1,j,k)+        &
     &                                    GRID(ng)%z_r(i  ,j,k))
            END DO
          END DO
        END DO
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idpthU), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % umask,                            &
# endif
     &                     GRID(ng) % z_v)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idpthU)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write time-varying depths of V-points.
!
      IF (Qout(idpthV,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v3dvar
        DO k=1,N(ng)
          DO j=JstrV-1,Jend+1
            DO i=Istr-1,Iend+1
              GRID(ng)%z_v(i,j,k)=0.5_r8*(GRID(ng)%z_r(i,j-1,k)+       &
     &                                    GRID(ng)%z_r(i,j  ,k))
            END DO
          END DO
        END DO
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idpthV), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % vmask,                            &
# endif
     &                     GRID(ng) % z_v)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idpthV)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write time-varying depths of W-points.
!
      IF (Qout(idpthW,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idpthW), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     GRID(ng) % z_w)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idpthW)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
!
!  Write out free-surface (m)
!
      IF (Qout(idFsur,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idFsur), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#endif
#ifdef WET_DRY
     &                     OCEAN(ng) % zeta(:,:,KOUT),                  &
     &                     SetFillVal = .FALSE.)
#else
     &                     OCEAN(ng) % zeta(:,:,KOUT))
#endif
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idFsur)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
#if defined FORWARD_WRITE && defined FORWARD_RHS
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idRzet), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     OCEAN(ng) % rzeta(:,:,KOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idRzet)), QCK(ng)%Rindex
      END IF
exit_flag=3
          ioerror=status
          RETURN
        END IF
#endif
      END IF
#ifdef ADJUST_BOUNDARY
!
!  Write out free-surface open boundaries.
!
      IF (ANY(Lobc(:,isFsur,ng))) THEN
        scale=1.0_r8
        status=nf_fwrite2d_bry (ng, iNLM, QCK(ng)%name, QCK(ng)%ncid,   &
     &                          Vname(1,idSbry(isFsur)),                &
     &                          QCK(ng)%Vid(idSbry(isFsur)),            &
     &                          QCK(ng)%Rindex, r2dvar,                 &
     &                          LBij, UBij, Nbrec(ng), scale,           &
     &                          BOUNDARY(ng) % zeta_obc(LBij:,:,:,      &
     &                                                  Lbout(ng)))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSbry(isFsur))),            &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
!
!  Write out 2D U-momentum component (m/s).
!
      IF (Qout(idUbar,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUbar), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#ifdef MASKING
     &                     GRID(ng) % umask_full,                       &
#endif
     &                     OCEAN(ng) % ubar(:,:,KOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbar)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
#ifdef FORWARD_WRITE
# ifdef FORWARD_RHS
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idRu2d), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask_full,                       &
#  endif
     &                     OCEAN(ng) % rubar(:,:,KOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idRu2d)), QCK(ng)%Rindex
      END IF
exit_flag=3
          ioerror=status
          RETURN
        END IF
# endif
# ifdef SOLVE3D
#  ifdef FORWARD_RHS
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idRuct), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % umask_full,                       &
#   endif
     &                     COUPLING(ng) % rufrc)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idRuct)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
#  endif
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUfx1), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask_full,                       &
#  endif
     &                     COUPLING(ng) % DU_avg1)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUfx1)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUfx2), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask_full,                       &
#  endif
     &                     COUPLING(ng) % DU_avg2)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUfx2)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
# endif
#endif
      END IF
#ifdef ADJUST_BOUNDARY
!
!  Write out 2D U-momentum component open boundaries.
!
      IF (ANY(Lobc(:,isUbar,ng))) THEN
        scale=1.0_r8
        status=nf_fwrite2d_bry (ng, iNLM, QCK(ng)%name, QCK(ng)%ncid,   &
     &                          Vname(1,idSbry(isUbar)),                &
     &                          QCK(ng)%Vid(idSbry(isUbar)),            &
     &                          QCK(ng)%Rindex, u2dvar,                 &
     &                          LBij, UBij, Nbrec(ng), scale,           &
     &                          BOUNDARY(ng) % ubar_obc(LBij:,:,:,      &
     &                                                  Lbout(ng)))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSbry(isUbar))),            &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
!
!  Write out 2D V-momentum component (m/s).
!
      IF (Qout(idVbar,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVbar), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#ifdef MASKING
     &                     GRID(ng) % vmask_full,                       &
#endif
     &                     OCEAN(ng) % vbar(:,:,KOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbar)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
#ifdef FORWARD_WRITE
# ifdef FORWARD_RHS
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idRv2d), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask_full,                       &
#  endif
     &                     OCEAN(ng) % rvbar(:,:,KOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idRv2d)), QCK(ng)%Rindex
      END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
# endif
# ifdef SOLVE3D
#  ifdef FORWARD_RHS
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idRvct), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % vmask_full,                       &
#   endif
     &                     COUPLING(ng) % rvfrc)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idRvct)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
#  endif
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVfx1), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask_full,                       &
#  endif
     &                     COUPLING(ng) % DV_avg1)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVfx1)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVfx2), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask_full,                       &
#  endif
     &                     COUPLING(ng) % DV_avg2)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVfx2)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
# endif
#endif
      END IF
#ifdef ADJUST_BOUNDARY
!
!  Write out 2D V-momentum component open boundaries.
!
      IF (ANY(Lobc(:,isVbar,ng))) THEN
        scale=1.0_r8
        status=nf_fwrite2d_bry (ng, iNLM, QCK(ng)%name, QCK(ng)%ncid,   &
     &                          Vname(1,idSbry(isVbar)),                &
     &                          QCK(ng)%Vid(idSbry(isVbar)),            &
     &                          QCK(ng)%Rindex, v2dvar,                 &
     &                          LBij, UBij, Nbrec(ng), scale,           &
     &                          BOUNDARY(ng) % vbar_obc(LBij:,:,:,      &
     &                                                  Lbout(ng)))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSbry(isVbar))),            &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
!
!  Write out 2D Eastward and Northward momentum components (m/s) at
!  RHO-points.
!
      IF (Qout(idu2dE,ng).and.Qout(idv2dN,ng)) THEN
        IF (.not.allocated(Ur2d)) THEN
          allocate (Ur2d(LBi:UBi,LBj:UBj))
            Ur2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
        IF (.not.allocated(Vr2d)) THEN
          allocate (Vr2d(LBi:UBi,LBj:UBj))
            Vr2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
        CALL uv_rotate2d (ng, tile, .FALSE., .TRUE.,                    &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    GRID(ng) % CosAngler,                         &
     &                    GRID(ng) % SinAngler,                         &
#ifdef MASKING
     &                    GRID(ng) % rmask_full,                        &
#endif
     &                    OCEAN(ng) % ubar(:,:,KOUT),                   &
     &                    OCEAN(ng) % vbar(:,:,KOUT),                   &
     &                    Ur2d, Vr2d)

        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idu2dE), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#endif
     &                     Ur2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idu2dE)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF

        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idv2dN), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#endif
     &                     Vr2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idv2dN)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
        deallocate (Ur2d)
        deallocate (Vr2d)
      END IF

#ifdef SOLVE3D
!
!  Write out 3D U-momentum component (m/s).
!
      IF (Qout(idUvel,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUvel), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % umask_full,                       &
# endif
     &                     OCEAN(ng) % u(:,:,:,NOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUvel)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
# if defined FORWARD_WRITE && defined FORWARD_RHS
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idRu3d), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % umask_full,                       &
#  endif
     &                     OCEAN(ng) % ru(:,:,:,NOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idRu3d)), QCK(ng)%Rindex
      END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
# endif
      END IF
# ifdef ADJUST_BOUNDARY
!
!  Write out 3D U-momentum component open boundaries.
!
      IF (ANY(Lobc(:,isUvel,ng))) THEN
        scale=1.0_r8
        status=nf_fwrite3d_bry (ng, iNLM, QCK(ng)%name, QCK(ng)%ncid,   &
     &                          Vname(1,idSbry(isUvel)),                &
     &                          QCK(ng)%Vid(idSbry(isUvel)),            &
     &                          QCK(ng)%Rindex, u3dvar,                 &
     &                          LBij, UBij, 1, N(ng), Nbrec(ng), scale, &
     &                          BOUNDARY(ng) % u_obc(LBij:,:,:,:,       &
     &                                               Lbout(ng)))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSbry(isUvel))),            &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
!
!  Write out 3D V-momentum component (m/s).
!
      IF (Qout(idVvel,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVvel), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % vmask_full,                       &
# endif
     &                     OCEAN(ng) % v(:,:,:,NOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVvel)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
# if defined FORWARD_WRITE && defined FORWARD_RHS
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idRv3d), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % vmask_full,                       &
#  endif
     &                     OCEAN(ng) % rv(:,:,:,NOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUsur)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
# endif
      END IF
# ifdef ADJUST_BOUNDARY
!
!  Write out 3D V-momentum component open boundaries.
!
      IF (ANY(Lobc(:,isVvel,ng))) THEN
        scale=1.0_r8
        status=nf_fwrite3d_bry (ng, iNLM, QCK(ng)%name, QCK(ng)%ncid,   &
     &                          Vname(1,idSbry(isVvel)),                &
     &                          QCK(ng)%Vid(idSbry(isVvel)),            &
     &                          QCK(ng)%Rindex, v3dvar,                 &
     &                          LBij, UBij, 1, N(ng), Nbrec(ng), scale, &
     &                          BOUNDARY(ng) % v_obc(LBij:,:,:,:,       &
     &                                               Lbout(ng)))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSbry(isVvel))),            &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
!
!  Write out 3D Eastward and Northward momentum components (m/s) at
!  RHO-points.
!
      IF (Qout(idu3dE,ng).and.Qout(idv3dN,ng)) THEN
        IF (.not.allocated(Ur3d)) THEN
          allocate (Ur3d(LBi:UBi,LBj:UBj,N(ng)))
          Ur3d(LBi:UBi,LBj:UBj,1:N(ng))=0.0_r8
        END IF
        IF (.not.allocated(Vr3d)) THEN
          allocate (Vr3d(LBi:UBi,LBj:UBj,N(ng)))
          Vr3d(LBi:UBi,LBj:UBj,1:N(ng))=0.0_r8
        END IF
        CALL uv_rotate3d (ng, tile, .FALSE., .TRUE.,                    &
     &                    LBi, UBi, LBj, UBj, 1, N(ng),                 &
     &                    GRID(ng) % CosAngler,                         &
     &                    GRID(ng) % SinAngler,                         &
# ifdef MASKING
     &                    GRID(ng) % rmask_full,                        &
# endif
     &                    OCEAN(ng) % u(:,:,:,NOUT),                    &
     &                    OCEAN(ng) % v(:,:,:,NOUT),                    &
     &                    Ur3d, Vr3d)

          scale=1.0_r8
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idu3dE), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
# endif
     &                     Ur3d)
          IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idu3dE)), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF

          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idv3dN), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
# endif
     &                     Vr3d)
          IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idv3dN)), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
                  END IF
        deallocate (Ur3d)
        deallocate (Vr3d)
      END IF
!
!  Write out S-coordinate omega vertical velocity (m/s).
!
      IF (Qout(idOvel,ng)) THEN
        IF (.not.allocated(Wr3d)) THEN
          allocate (Wr3d(LBi:UBi,LBj:UBj,0:N(ng)))
          Wr3d(LBi:UBi,LBj:UBj,0:N(ng))=0.0_r8
        END IF
        scale=1.0_r8
        gtype=gfactor*w3dvar
        CALL scale_omega (ng, tile, LBi, UBi, LBj, UBj, 0, N(ng),       &
     &                    GRID(ng) % pm,                                &
     &                    GRID(ng) % pn,                                &
     &                    OCEAN(ng) % W,                                &
     &                    Wr3d)
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idOvel), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     Wr3d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idOvel)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
        deallocate (Wr3d)
      END IF
!
!  Write out vertical velocity (m/s).
!
      IF (Qout(idWvel,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWvel), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     OCEAN(ng) % wvel)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWvel)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out tracer type variables.
!
      DO itrc=1,NT(ng)
        IF (Qout(idTvar(itrc),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Tid(itrc), &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
# ifdef MASKING
     &                       GRID(ng) % rmask,                          &
# endif
     &                       OCEAN(ng) % t(:,:,:,NOUT,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTvar(itrc))),            &
     &                          QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
# ifdef ADJUST_BOUNDARY
!
!  Write out 3D tracers open boundaries.
!
      DO itrc=1,NT(ng)
        IF (ANY(Lobc(:,isTvar(itrc),ng))) THEN
          scale=1.0_r8
          status=nf_fwrite3d_bry (ng, iNLM, QCK(ng)%name, QCK(ng)%ncid, &
     &                            Vname(1,idSbry(isTvar(itrc))),        &
     &                            QCK(ng)%Vid(idSbry(isTvar(itrc))),    &
     &                            QCK(ng)%Rindex, r3dvar,               &
     &                            LBij, UBij, 1, N(ng), Nbrec(ng),      &
     &                            scale,                                &
     &                            BOUNDARY(ng) % t_obc(LBij:,:,:,:,     &
     &                                                 Lbout(ng),itrc))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idSbry(isTvar(itrc)))),    &
     &                          QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
# endif

# ifdef BEST_NPZ
#  ifdef STATIONARY
!--------------------------------------
!  Write out 3D stationary variable array
!--------------------------------------
      DO itrc=1,NTS(ng)
        IF (Qout(idTSvar(itrc),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*r3dvar
!           print *,'st=',OCEAN(ng) % st(:,:,:,NOUT,itrc)
!          if (hisTSid(itrc,ng) .lt. 0) hisTSid(itrc,ng) = 0
          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%TSid(itrc),   &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#   ifdef MASKING
     &                       GRID(ng) % rmask_full,                     &
#   endif
     &                       OCEAN(ng) % st(:,:,:,NOUT,itrc))
          !---------------------
          !Zero the output array
          !---------------------

!           print *,'st=',OCEAN(ng) % st(:,:,:,1,1)
!           print *,'NOUT=',NOUT
!           print *,'itrc=',itrc

          OCEAN(ng) % st(:,:,:,NOUT,itrc) = 0.0_r8
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN

              WRITE (stdout,10) TRIM(Vname(1,idTSvar(itrc))),           &
     &                          QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
#  endif

#  ifdef STATIONARY2
!--------------------------------------
!  Write out 2D stationary variable array
!--------------------------------------
      DO itrc=1,NTS2(ng)
        IF (Qout(idTS2var(itrc),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*r2dvar
          if (hisTS2id(itrc,ng) .lt. 0) hisTS2id(itrc,ng) = 0
          status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%TS2id(itrc),                       &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj,  scale,                &
#   ifdef MASKING
     &                       GRID(ng) % rmask_full,                     &
#   endif
     &                       OCEAN(ng) % st2(:,:,NOUT,itrc))
          !---------------------
          !Zero the output array
          !---------------------
          OCEAN(ng) % st2(:,:,NOUT,itrc) = 0.0_r8
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTS2var(itrc))),          &
     &                          QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
#  endif

!--------------------------------------
!  Write out 2D production array
!--------------------------------------

#  ifdef PROD2
      DO itrc=1,NPT2(ng)
        IF (Qout(idPT2var(itrc),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*r2dvar
          if (hisPT2id(itrc,ng) .lt. 0) hisPT2id(itrc,ng) = 0
          status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%PT2id(itrc),                       &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#   ifdef MASKING
     &                       GRID(ng) % rmask_full,                     &
#   endif
     &                       OCEAN(ng) % pt2(:,:,NOUT,itrc))
          !---------------------
          !Zero the output array
          !---------------------
          OCEAN(ng) % pt2(:,:,NOUT,itrc) = 0.0_r8
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idPT2var(itrc))),          &
     &                          QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
#  endif
!--------------------------------------
!  Write out 3D production array
!--------------------------------------

#  ifdef PROD3
      DO itrc=1,NPT3(ng)
        IF (Qout(idPT3var(itrc),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*r3dvar
          if (hisPT3id(itrc,ng) .lt. 0) hisPT3id(itrc,ng) = 0
          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%PT3id(itrc),                       &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#   ifdef MASKING
     &                       GRID(ng) % rmask_full,                     &
#   endif
     &                       OCEAN(ng) % pt3(:,:,:,NOUT,itrc))
          !---------------------
          !Zero the output array
          !---------------------
          OCEAN(ng) % pt3(:,:,:,NOUT,itrc) = 0.0_r8
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idPT3var(itrc))),          &
     &                          QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
#  endif
!------------------------------
!Write out benthic variables
!-----------------------------

#  if defined BENTHIC
              DO itrc=1,NBeT(ng)
              IF (Qout(idBeTvar(itrc),ng)) THEN
                 scale=1.0_r8
                 gtype=gfactor*r2dvar
             status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                 &
     &                          QCK(ng)%Bid(itrc),                      &
     &                          QCK(ng)%Rindex, gtype,                  &
     &                          LBi, UBi, LBj, UBj, scale,              &

!Will need to switch this if have more than one depth level for benthos
!      status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Bid(itrc),       &
!     &                          QCK(ng)%Rindex, gtype,                 &
!     &                          LBi, UBi, LBj, UBj, 1, NBL(ng), scale, &

#   ifdef MASKING
     &                       GRID(ng) % rmask_full,                     &
#   endif
     &                       OCEAN(ng) % bt(:,:,1,NOUT,itrc))
!             print *,OCEAN(ng) % bt(LBi,LBj,1,NOUT,itrc)
!             print *, 'LBi=',LBi,'LBj=',LBj,'NOUT=',NOUT,'itrc=',itrc

          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
           IF (Master) THEN
            WRITE (stdout,10)TRIM(Vname(1,idBeTvar(itrc))),             &
     &                       QCK(ng)%Rindex
           END IF
              exit_flag=3
              ioerror=status
            RETURN
          END IF
        END IF
      END DO
#  endif

!------------------------------
!Write out ice bio variables
!-----------------------------

#  if defined ICE_BIO
#   ifdef CLIM_ICE_1D
      DO itrc=1,NIceT(ng)
        IF (Qout(idIceBvar(itrc),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*r2dvar
          status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                    &
     &                          QCK(ng)%IceBid(itrc),                   &
     &                          QCK(ng)%Rindex, gtype,                  &
     &                          LBi, UBi, LBj, UBj, scale,              &

!Will need to switch this if have more than one depth level for benthos
!      status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%IceBid(itrc), &
!     &                          QCK(ng)%Rindex, gtype,                  &
!     &                          LBi, UBi, LBj, UBj, 1, NBL(ng), scale,  &

#   ifdef MASKING
     &                       GRID(ng) % rmask_full,                     &
#   endif
     &                       OCEAN(ng) % it(:,:,NOUT,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10)TRIM(Vname(1,idIceBvar(itrc))),          &
     &                       QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
#   else
!
!  Write out Ice Algae
!
      IF (Qout(idIcePhL,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIcePhL),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % IcePhL(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIcePhL)),                  &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

!
!  Write out Ice Nitrate
!
      IF (Qout(idIceNO3,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIceNO3),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % IceNO3(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIceNO3)),                  &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

!  Write out Ice Ammonium
!
      IF (Qout(idIceNH4,ng)) THEN
!      IF (Qout(idIceBvar,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIceNH4),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % IceNH4(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIceNH4)),                  &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

!  Write out IceLog (doesn't work with IceLog being int)
!
#    ifdef FOO
      IF (Qout(idIceLog,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIceLog),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % IceLog(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIceLog)),                  &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#    endif
#   endif
#  endif
# endif
!
!  Write out density anomaly.
!----------------------------
      IF (Qout(idDano,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idDano), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
# endif
     &                     OCEAN(ng) % rho)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idDano)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# ifdef NEMURO_SED1
!
!  Write out PON in sediment.
!
      IF (Qout(idPONsed,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idPONsed),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     OCEAN(ng) % PONsed)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idPONsed)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out OPAL in sediment.
!
      IF (Qout(idOPALsed,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idOPALsed),                      &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     OCEAN(ng) % OPALsed)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idOPALsed)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out DENIT in sediment.
!
      IF (Qout(idDENITsed,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idDENITsed),                     &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     OCEAN(ng) % DENITsed)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idDENITsed)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out buried PON in sediment.
!
      IF (Qout(idPONbur,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idPONbur),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     OCEAN(ng) % PON_burial)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idPONbur)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out buried OPAL in sediment.
!
      IF (Qout(idOPALbur,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idOPALbur),                      &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     OCEAN(ng) % OPAL_burial)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idOPALbur)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# ifdef PRIMARY_PROD
!
!  Write out Net primary production.
!
      IF (Qout(idNPP,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idNPP),                          &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     OCEAN(ng) % Bio_NPP)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idNPP)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# ifdef LMD_SKPP
!
!  Write out depth surface boundary layer.
!
      IF (Qout(idHsbl,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idHsbl), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     MIXING(ng) % hsbl)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHsbl)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# ifdef LMD_BKPP
!
!  Write out depth surface boundary layer.
!
      IF (Qout(idHbbl,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idHbbl), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     MIXING(ng) % hbbl)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHbbl)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# ifdef SSSFLX
!
!  Write out sea surface salinity correction salt flux
!
      IF (Qout(idSSSf,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idSSSf), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     FORCES(ng) % sssflx)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSSSf)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif

# if defined FORWARD_WRITE && defined LMD_NONLOCAL
!
!  Write out KPP nonlocal transport.
!
      DO i=1,NAT
        IF (Qout(idGhat(i),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*w3dvar
          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idGhat(i)),                    &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 0, N(ng), scale,       &
#  ifdef MASKING
     &                       GRID(ng) % rmask_full,                     &
#  endif
     &                       MIXING(ng) % ghats(:,:,:,i))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idGhat(i))), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
# endif
!
!  Write out vertical viscosity coefficient.
!
      IF (Qout(idVvis,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVvis), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
# endif
     &                     MIXING(ng) % Akv,                            &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVvis)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out vertical diffusion coefficient for potential temperature.
!
      IF (Qout(idTdif,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idTdif), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
# endif
     &                     MIXING(ng) % Akt(:,:,:,itemp),               &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTdif)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# ifdef SALINITY
!
!  Write out vertical diffusion coefficient for salinity.
!
      IF (Qout(idSdif,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idSdif), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     MIXING(ng) % Akt(:,:,:,isalt),               &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSdif)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# if defined GLS_MIXING || defined MY25_MIXING
!
!  Write out turbulent kinetic energy.
!
      IF (Qout(idMtke,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idMtke), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     MIXING(ng) % tke(:,:,:,NOUT),                &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idMtke)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
#  ifdef FORWARD_WRITE
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVmKK), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     MIXING(ng) % Akk)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVmKK)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
#  endif
      END IF
!
!  Write out turbulent length scale field.
!
      IF (Qout(idMtls,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idMtls), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     MIXING(ng) % gls(:,:,:,NOUT),                &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idMtls)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
#  ifdef FORWARD_WRITE
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVmLS), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     MIXING(ng) % Lscale)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVmLS)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
#  endif
#  if defined FORWARD_WRITE && defined GLS_MIXING
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVmKP), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#   endif
     &                     MIXING(ng) % Akp)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVmKP)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
#  endif
      END IF
# endif
# ifdef ICE_MODEL
!
!  Write out ice 2D momentum component (m/s) in the XI-direction.
!
      IF (Qout(idUice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idUice),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask_full,                       &
#  endif
     &                     ICE(ng) % ui(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUice)),                    &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice 2D momentum component (m/s) in the ETA-direction.
!
      IF (Qout(idVice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idVice),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask_full,                       &
#  endif
     &                     ICE(ng) % vi(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVice)),                    &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D Eastward and Northward ice momentum components (m/s) at
!  RHO-points.
!
      IF (Qout(idUiceE,ng).and.Qout(idViceN,ng)) THEN
        IF (.not.allocated(Ur2d)) THEN
          allocate (Ur2d(LBi:UBi,LBj:UBj))
            Ur2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
        IF (.not.allocated(Vr2d)) THEN
          allocate (Vr2d(LBi:UBi,LBj:UBj))
            Vr2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
        CALL uv_rotate2d (ng, tile, .FALSE., .TRUE.,                    &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    GRID(ng) % CosAngler,                         &
     &                    GRID(ng) % SinAngler,                         &
#ifdef MASKING
     &                    GRID(ng) % rmask_full,                        &
#endif
     &                    ICE(ng) % ui(:,:,IUOUT),                      &
     &                    ICE(ng) % vi(:,:,IUOUT),                      &
     &                    Ur2d, Vr2d)

        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUiceE),&
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#endif
     &                     Ur2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUiceE)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF

        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idViceN),&
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#endif
     &                     Vr2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idViceN)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
        deallocate (Ur2d)
        deallocate (Vr2d)
      END IF
!
!  Write out ice concentration
!
      IF (Qout(idAice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idAice),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % ai(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idAice)),                    &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice average thickness
!
      IF (Qout(idHice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idHice),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % hi(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHice)),                    &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out snow average thickness
!
      IF (Qout(idHsno,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idHsno),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % hsn(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHsno)),                    &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#ifdef MELT_PONDS
!
!  Write out surface water fraction (on ice)
!
      IF (Qout(idApond,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idApond),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % apond(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idApond)),                   &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface water thickness (on ice)
!
      IF (Qout(idHpond,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idHpond),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % hpond(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHpond)),                   &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
!
!  Write out ice age.
!
      IF (Qout(idAgeice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idAgeice),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % ageice(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idAgeice)),                  &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-ocean mass flux
!
      IF (Qout(idIomflx,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idIomflx),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % io_mflux)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIomflx)),                  &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice/snow surface temperature
!
      IF (Qout(idTice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idTice),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % tis)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTice)),                    &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice interior temperature
!
      IF (Qout(idTimid,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idTimid),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % ti(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTimid)),                   &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 11
!
      IF (Qout(idSig11,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idSig11),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICE(ng) % sig11(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSig11)),                   &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 12
!
      IF (Qout(idSig12,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idSig12),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICE(ng) % sig12(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSig12)),                   &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 22
!
      IF (Qout(idSig22,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idSig22),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % sig22(:,:,IUOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSig22)),                   &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-ocean friction velocity
!
      IF (Qout(idTauiw,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idTauiw),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % utau_iw)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTauiw)),                   &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-ocean momentum transfer coefficient
!
      IF (Qout(idChuiw,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idChuiw),                        &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     ICE(ng) % chu_iw)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idChuiw)),                   &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out temperature of molecular sublayer under ice
!
      IF (Qout(idT0mk,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idT0mk),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICE(ng) % t0mk)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idT0mk)),                    &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salinity of molecular sublayer under ice
!
      IF (Qout(idS0mk,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idS0mk),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICE(ng) % s0mk)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idS0mk)),                    &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice freeze Wfr
!
      IF (Qout(idWfr,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idWfr),                          &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICE(ng) % wfr)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWfr)),                     &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice melt/freeze wai
!
      IF (Qout(idWai,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idWai),                          &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICE(ng) % wai)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWai)),                     &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice melt/freeze Wao
!
      IF (Qout(idWao,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idWao),                          &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICE(ng) % wao)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWao)),                     &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice melt/freeze wio
!
      IF (Qout(idWio,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idWio),                          &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICE(ng) % wio)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWio)),                     &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice melt/freeze wro
!
      IF (Qout(idWro,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idWro),                          &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICE(ng) % wro)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWro)),                     &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice divergence rate
!
      IF (Qout(idWdiv,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idWdiv),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     ICE(ng) % wdiv)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWdiv)),                    &
     &                        QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# if defined RUNOFF
!
!  Write out runoff.
!
      IF (Qout(idRunoff,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                      &
     &                     QCK(ng)%Vid(idRunoff),                       &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#  endif
     &                     FORCES(ng) % Runoff)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idRunoff)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# if defined BULK_FLUXES || defined ECOSIM || defined ATM_PRESS || \
     defined ATM2OCN_FLUXES || defined CCSM_FLUXES2D
!
!  Write out surface air pressure.
!
      IF (Qout(idPair,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idPair), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % Pair)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idPair)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# if defined BULK_FLUXES || defined AIR_OCEAN || defined ECOSIM
!
!  Write out surface winds.
!
      IF (Qout(idUair,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUair), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % Uwind)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUair)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

      IF (Qout(idVair,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVair), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % Vwind)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVair)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D Eastward and Northward surface winds (m/s) at
!  RHO-points.
!
      IF (Qout(idUairE,ng).and.Qout(idVairN,ng)) THEN
        IF (.not.allocated(Ur2d)) THEN
          allocate (Ur2d(LBi:UBi,LBj:UBj))
            Ur2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
        IF (.not.allocated(Vr2d)) THEN
          allocate (Vr2d(LBi:UBi,LBj:UBj))
            Vr2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
        CALL uv_rotate2d (ng, tile, .FALSE., .TRUE.,                    &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    GRID(ng) % CosAngler,                         &
     &                    GRID(ng) % SinAngler,                         &
#ifdef MASKING
     &                    GRID(ng) % rmask_full,                        &
#endif
     &                    FORCES(ng) % Uwind,                           &
     &                    FORCES(ng) % Vwind,                           &
     &                    Ur2d, Vr2d)

        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUairE),&
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#endif
     &                     Ur2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUairE)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF

        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVairN),&
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#ifdef MASKING
     &                     GRID(ng) % rmask_full,                       &
#endif
     &                     Vr2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVairN)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
        deallocate (Ur2d)
        deallocate (Vr2d)
      END IF

# endif
!
!  Write out surface active traces fluxes.
!
      DO itrc=1,NAT
        IF (Qout(idTsur(itrc),ng)) THEN
          IF (itrc.eq.itemp) THEN
# ifdef SO_SEMI
            scale=1.0_r8
# else
            scale=rho0*Cp                   ! Celsius m/s to W/m2
# endif
          ELSE IF (itrc.eq.isalt) THEN
            scale=1.0_r8
          END IF
          gtype=gfactor*r2dvar
          status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idTsur(itrc)),                 &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
# ifdef MASKING
     &                       GRID(ng) % rmask,                          &
# endif
     &                       FORCES(ng) % stflx(:,:,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTsur(itrc))),            &
     &                          QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
# if defined BULK_FLUXES || defined AIR_OCEAN
!
!  Write out latent heat flux.
!
      IF (Qout(idLhea,ng)) THEN
        scale=rho0*Cp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idLhea), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % lhflx)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLhea)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out sensible heat flux.
!
      IF (Qout(idShea,ng)) THEN
        scale=rho0*Cp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idShea), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % shflx)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idShea)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out longwave radiation flux.
!
      IF (Qout(idLrad,ng)) THEN
        scale=rho0*Cp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idLrad), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % lrflx)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLrad)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  ifdef EMINUSP
!
!  Write out E-P (m/s).
!
      IF (Qout(idEmPf,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idEmPf), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     FORCES(ng) % EminusP)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idEmPf)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out evaporation rate (kg/m2/s).
!
      IF (Qout(idevap,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idevap), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     FORCES(ng) % evap)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idevap)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out precipitation rate (kg/m2/s).
!
      IF (Qout(idrain,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idrain), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     FORCES(ng) % rain)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idrain)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
# endif
# ifdef SHORTWAVE
!
!  Write out shortwave radiation flux.
!
      IF (Qout(idSrad,ng)) THEN
        scale=rho0*Cp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idSrad), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     FORCES(ng) % srflx)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSrad)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
#endif
!
#if defined ESTUARYBGC && defined SPECTRAL_LIGHT
!  Write out Photosynthetically Available Radiation (PAR)
!
      IF (Qout(idPARo,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idPARo), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % PARout)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idPARo)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

!  Write out Spectral Photosynthetically Available Radiation (PARs)
!
      IF (Qout(idPARs,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*s3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idPARs), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, NBAND, scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % PARs)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idPARs)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!  Write out Spectral Attenuation Kd
!
      IF (Qout(idSpKd,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*s3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idSpKd), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, NBAND, scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % SpKd)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSpKd)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
!
#if defined ESTUARYBGC && defined SAV_BIOMASS
!
!  Write out Dissolved Inorganic Nitrogen in water column
!
      IF (Qout(iddinw,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(iddinw), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % DINwcr)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,iddinw)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out Dissolved Inorganic Nitrogen in sediment column
!
      IF (Qout(iddins,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(iddins), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % DINsed)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,iddins)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out Dissolved Oxygen in water column
!
      IF (Qout(iddowc,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(iddowc), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % DOwcr)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,iddowc)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out Dissolved Inorganic Nitrogen in water column due to SAV
!
      IF (Qout(idwsvl,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idwsvl), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % DINwcr_sav)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idwsvl)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out above ground biomass.
!
      IF (Qout(idsagb,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idsagb), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % AGB)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idsagb)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out below ground biomass.
!
      IF (Qout(idsbgb,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idsbgb), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % BGB)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idsbgb)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
! PP.
!
      IF (Qout(idsvpp,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idsvpp), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % PP)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idsvpp)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
! AGM.
!
      IF (Qout(idsvam,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idsvam), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % AGM)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idsvam)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
! .
!
      IF (Qout(idsgar,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idsgar), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % AGAR)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idsgar)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
! .
!
      IF (Qout(idsvbr,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idsvbr), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % AGBR)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idsvbr)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
! .
!
      IF (Qout(idsvrs,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idsvrs), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % SEARS)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idsvrs)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
! .
!
      IF (Qout(idsvbg,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idsvbg), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % AGBG)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idsvbg)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
! .
!
      IF (Qout(idsvag,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idsvag), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % BGAG)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idsvag)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
      IF (Qout(idsbgr,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idsbgr), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % BGR)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idsbgr)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
! .
!
      IF (Qout(idsbgm,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idsbgm), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     OCEAN(ng) % BGM)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idsbgm)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
!
!  Write out surface U-momentum stress.
!
      IF (Qout(idUsms,ng)) THEN
#ifdef SO_SEMI
        scale=1.0_r8
#else
        scale=rho0                          ! m2/s2 to Pa
#endif
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUsms), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#ifdef MASKING
     &                     GRID(ng) % umask,                            &
#endif
     &                     FORCES(ng) % sustr)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUsms)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface V-momentum stress.
!
      IF (Qout(idVsms,ng)) THEN
#ifdef SO_SEMI
        scale=1.0_r8
#else
        scale=rho0
#endif
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVsms), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#endif
     &                     FORCES(ng) % svstr)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVsms)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom U-momentum stress.
!
      IF (Qout(idUbms,ng)) THEN
        scale=-rho0
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUbms), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#ifdef MASKING
     &                     GRID(ng) % umask,                            &
#endif
     &                     FORCES(ng) % bustr)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbms)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom V-momentum stress.
!
      IF (Qout(idVbms,ng)) THEN
        scale=-rho0
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVbms), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#endif
     &                     FORCES(ng) % bvstr)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbms)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#ifdef SOLVE3D
# ifdef BBL_MODEL
!
!  Write out current-induced, bottom U-stress at RHO-points.
!
      IF (Qout(idUbrs,ng)) THEN
        scale=-rho0
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUbrs), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     BBL(ng) % bustrc)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbrs)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out current-induced, bottom V-stress at RHO-points.
!
      IF (Qout(idVbrs,ng)) THEN
        scale=-rho0
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVbrs), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     BBL(ng) % bvstrc)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbrs)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out wind-induced, bottom U-stress at RHO-points.
!
      IF (Qout(idUbws,ng)) THEN
        scale=rho0
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUbws), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     BBL(ng) % bustrw)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbws)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out wind-induced, bottom V-stress at RHO-points.
!
      IF (Qout(idVbws,ng)) THEN
        scale=rho0
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVbws), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     BBL(ng) % bvstrw)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbws)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out maximum wind and current, bottom U-stress at RHO-points.
!
      IF (Qout(idUbcs,ng)) THEN
        scale=rho0
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUbcs), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     BBL(ng) % bustrcwmax)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbcs)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out maximum wind and current, bottom V-stress at RHO-points.
!
      IF (Qout(idVbcs,ng)) THEN
        scale=rho0
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVbcs), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     BBL(ng) % bvstrcwmax)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbcs)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out maximum wave and current bottom stress magnitude.
!
      IF (Qout(idUVwc,ng)) THEN
        allocate (wrk2(LBi:UBi,LBj:UBj))
        wrk2(LBi:UBi,LBj:UBj)=0.0_r8
        scale=rho0
        gtype=gfactor*r2dvar
        wrk2=sqrt(BBL(ng)%bustrcwmax*BBL(ng)%bustrcwmax+                &
     &            BBL(ng)%bvstrcwmax*BBL(ng)%bvstrcwmax+1.0E-10_r8)
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUVwc), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     wrk2(LBi:UBi,LBj:UBj))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUVwc)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
        deallocate (wrk2)
      END IF
!
!  Write out wind-induced, bed wave orbital U-velocity at RHO-points.
!
      IF (Qout(idUbot,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUbot), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     BBL(ng) % Ubot)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbot)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out wind-induced, bed wave orbital V-velocity at RHO-points
!
      IF (Qout(idVbot,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVbot), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     BBL(ng) % Vbot)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbot)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom U-velocity above bed at RHO-points.
!
      IF (Qout(idUbur,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUbur), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     BBL(ng) % Ur)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &               __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbur)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom V-velocity above bed at RHO-points.
!
      IF (Qout(idVbvr,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVbvr), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     BBL(ng) % Vr)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbvr)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# ifdef SEDIMENT
#  ifdef BEDLOAD
!
!  Write out bed load transport in U-direction.
!
      DO i=1,NST
        IF (Qout(idUbld(i),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*u2dvar
          status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idUbld(i)),                    &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#   ifdef MASKING
     &                       GRID(ng) % umask,                          &
#   endif
     &                       SEDBED(ng) % bedldu(:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idUbld(i))), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
!  Write out bed load transport in V-direction.
!
        IF (Qout(idVbld(i),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*v2dvar
          status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idVbld(i)),                    &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#   ifdef MASKING
     &                       GRID(ng) % vmask,                          &
#   endif
     &                       SEDBED(ng) % bedldv(:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idVbld(i))), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
#  endif
!
!  Write out sediment fraction of each size class in each bed layer.
!
      DO i=1,NST
        IF (Qout(idfrac(i),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*b3dvar
          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idfrac(i)),                    &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, Nbed, scale,        &
#  ifdef MASKING
     &                       GRID(ng) % rmask,                          &
#  endif
     &                       SEDBED(ng) % bed_frac(:,:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idfrac(i))), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out sediment mass of each size class in each bed layer.
!
      DO i=1,NST
        IF (Qout(idBmas(i),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*b3dvar
          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idBmas(i)),                    &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, Nbed, scale,        &
#  ifdef MASKING
     &                       GRID(ng) % rmask,                          &
#  endif
     &                       SEDBED(ng) % bed_mass(:,:,:,NOUT,i))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idBmas(i))), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out sediment properties in each bed layer.
!
      DO i=1,MBEDP
        IF (Qout(idSbed(i),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*b3dvar
          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idSbed(i)),                    &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, Nbed, scale,        &
#  ifdef MASKING
     &                       GRID(ng) % rmask,                          &
#  endif
     &                       SEDBED(ng) % bed(:,:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idSbed(i))), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
# endif
# if defined SEDIMENT || defined BBL_MODEL
!
!  Write out exposed sediment layer properties.
!
      DO i=1,MBOTP
        IF (Qout(idBott(i),ng)) THEN
          IF (i.eq.itauc) THEN
            scale=rho0
          ELSE
            scale=1.0_r8
          END IF
          gtype=gfactor*r2dvar
          status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idBott(i)),                    &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#  ifdef MASKING
     &                       GRID(ng) % rmask,                          &
#  endif
     &                       SEDBED(ng) % bottom(:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idBott(i))), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
# endif
#endif
#ifdef INWAVE_MODEL
!
!  Write out AC wave energy density.
!
      IF (Qout(idACen,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idACen), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, ND, scale,            &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     WAVEP(ng) % AC(:,:,:,NOUT))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idACen)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out energy celerity xi-direction.
!
      IF (Qout(idACcx,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idACcx), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, ND, scale,            &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % umask_full,                       &
#  else
     &                     GRID(ng) % umask,                            &
#  endif
# endif
     &                     WAVEP(ng) % cx)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idACcx)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

!
!  Write out energy celerity eta-direction.
!
      IF (Qout(idACcy,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idACcy), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, ND, scale,            &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % vmask_full,                       &
#  else
     &                     GRID(ng) % vmask,                            &
#  endif
# endif
     &                     WAVEP(ng) % cy)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idACcy)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out energy celerity xi-direction.
!
      IF (Qout(idACct,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idACct), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, ND, scale,            &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     WAVEP(ng) % ct)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idACct)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out energy peak period.
!
      IF (Qout(idACtp,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idACtp), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, ND, scale,            &
# ifdef MASKING
#  ifdef WET_DRY
     &                     GRID(ng) % rmask_full,                       &
#  else
     &                     GRID(ng) % rmask,                            &
#  endif
# endif
     &                     WAVEP(ng) % Ta)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idACtp)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#ifdef VEGETATION
#include "vegetation_wrt_his.h"
#endif

#ifdef WEC_MELLOR
!
!  Write out 2D radiation stress, Sxx-component.
!
      IF (Qout(idW2xx,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idW2xx), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     MIXING(ng) % Sxx_bar)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW2xx)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress, Sxy-component.
!
      IF (Qout(idW2xy,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idW2xy), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     MIXING(ng) % Sxy_bar)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW2xy)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress, Syy-component.
!
      IF (Qout(idW2yy,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idW2yy), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     MIXING(ng) % Syy_bar)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW2yy)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# ifdef SOLVE3D
!
!  Write out 3D radiation stress, Sxx-horizontal component.
!
      IF (Qout(idW3xx,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idW3xx), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     MIXING(ng) % Sxx)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3xx)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D radiation stress, Sxy-horizontal component.
!
      IF (Qout(idW3xy,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idW3xy), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     MIXING(ng) % Sxy)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3xy)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D radiation stress, Syy-horizontal component.
!
      IF (Qout(idW3yy,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idW3yy), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     MIXING(ng) % Syy)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3yy)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D radiation stress, Szx-vertical component.
!
      IF (Qout(idW3zx,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idW3zx), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     MIXING(ng) % Szx)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3zx)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D radiation stress, Szy-vertical component.
!
      IF (Qout(idW3zy,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idW3zy), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     MIXING(ng) % Szy)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3zy)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
#endif
#ifdef WEC
!
!  Write out total 2D WEC u-stress.
!
      IF (Qout(idU2rs,ng)) THEN
        scale=rho0
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idU2rs), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % umask,                            &
# endif
     &                     MIXING(ng) % rustr2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU2rs)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out total 2D WEC v-stress.
!
      IF (Qout(idV2rs,ng)) THEN
        scale=rho0
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idV2rs), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % vmask,                               &
# endif
     &                     MIXING(ng) % rvstr2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV2rs)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D U-momentum Stokes drift velocity.
!
      IF (Qout(idU2Sd,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idU2sd), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % umask,                               &
# endif
     &                     OCEAN(ng) % ubar_stokes)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU2Sd)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D V-momentum Stokes drift velocity.
!
      IF (Qout(idV2Sd,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idV2sd), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % vmask,                               &
# endif
     &                     OCEAN(ng) % vbar_stokes)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV2Sd)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# ifdef SOLVE3D
!
!  Write out 3D total WEC u-stress.
!
      IF (Qout(idU3rs,ng)) THEN
        scale=rho0
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idU3rs), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % umask,                            &
#  endif
     &                     MIXING(ng) % rustr3d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU3rs)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D total V-radiation stress.
!
      IF (Qout(idV3rs,ng)) THEN
        scale=rho0
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idV3rs), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#  endif
     &                     MIXING(ng) % rvstr3d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV3rs)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D U-momentum Stokes drift velocity.
!
      IF (Qout(idU3Sd,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idU3Sd), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % umask,                            &
#  endif
     &                     OCEAN(ng) % u_stokes)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU3Sd)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D V-momentum stokes velocity.
!
      IF (Qout(idV3Sd,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idV3Sd), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#  endif
     &                     OCEAN(ng) % v_stokes)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV3Sd)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D Omega-momentum stokes velocity.
!
      IF (Qout(idW3Sd,ng)) THEN
        IF (.not.allocated(Wr3d)) THEN
          allocate (Wr3d(LBi:UBi,LBj:UBj,0:N(ng)))
          Wr3d(LBi:UBi,LBj:UBj,0:N(ng))=0.0_r8
        END IF
        scale=1.0_r8
        gtype=gfactor*w3dvar
        CALL scale_omega (ng, tile, LBi, UBi, LBj, UBj, 0, N(ng),       &
     &                    GRID(ng) % pm,                                &
     &                    GRID(ng) % pn,                                &
     &                    OCEAN(ng) % W_stokes,                         &
     &                    Wr3d)
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idW3Sd), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
# ifdef MASKING
                         GRID(ng) % rmask,                            &
# endif
     &                     Wr3d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3Sd)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
        deallocate (Wr3d)
      END IF
!
!  Write out 3D W-momentum stokes velocity.
!
      IF (Qout(idW3St,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idW3St), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     OCEAN(ng) % wstvel)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3St)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
#endif
#ifdef WAVES_HEIGHT
!
!  Write out wind-induced wave height.
!
      IF (Qout(idWamp,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWamp), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % Hwave)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWamp)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#ifdef WAVES_LENGTH
!
!  Write out wind-induced wave length.
!
      IF (Qout(idWlen,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWlen), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % Lwave)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWlen)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#ifdef WAVES_LENGTHP
!
!  Write out wind-induced peak wave length.
!
      IF (Qout(idWlep,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWlep), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % Lwavep)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWlen)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#ifdef WAVES_DIR
!
!  Write out wind-induced wave direction.
!
      IF (Qout(idWdir,ng)) THEN
        scale=rad2deg
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWdir), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % Dwave)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWdir)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#ifdef WAVES_TOP_PERIOD
!
!  Write out wind-induced surface wave period.
!
      IF (Qout(idWptp,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWptp), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % Pwave_top)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWptp)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#ifdef WAVES_BOT_PERIOD
!
!  Write out wind-induced bottom wave period.
!
      IF (Qout(idWpbt,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWpbt), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % Pwave_bot)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWpbt)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#if defined BBL_MODEL || defined WAVES_OCEAN
!
!  Write out wind-induced wave bottom orbital velocity.
!
      IF (Qout(idWorb,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWorb), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % Uwave_rms)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWorb)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#if defined WAVES_OCEAN || (defined WEC_VF && defined BOTTOM_STREAMING)
!
!  Write out wave dissipation due to bottom friction.
!
      IF (Qout(idWdif,ng)) THEN
        scale=rho0                          ! W m /kg to W/m2
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWdif), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                               &
# endif
     &                     FORCES(ng) % Dissip_fric)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWdif)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#if defined WAVES_OCEAN || defined TKE_WAVEDISS || \
    defined WDISS_THORGUZA || defined WDISS_CHURTHOR || \
    defined WDISS_INWAVE
!
!  Write out wave dissipation due to breaking.
!
      IF (Qout(idWdib,ng)) THEN
        scale=rho0                          ! W m /kg to W/m2
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWdib), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % Dissip_break)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWdib)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out wave dissipation due to white capping.
!
      IF (Qout(idWdiw,ng)) THEN
        scale=rho0                          ! W m /kg to W/m2
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWdiw), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % Dissip_wcap)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWdiw)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#if defined ROLLER_SVENDSEN
!
!  Write out percent wave breaking.
!
      IF (Qout(idWbrk,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWbrk), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % Wave_break)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWbrk)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#if defined WEC_ROLLER
!
!  Write out roller dissipation.
!
      IF (Qout(idWdis,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWdis), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % Dissip_roller)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWdis)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#if defined WEC_ROLLER
!
!  Write out roller wave action density.
!
      IF (Qout(idWrol,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWrol), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % rollA)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWrol)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#if defined WAVES_DSPR
!
!  Write out waves directional spreading.
!
      IF (Qout(idWvds,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWvds), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % Wave_ds)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWvds)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out wave spectrum peakedness.
!
      IF (Qout(idWvqp,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWvqp), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     FORCES(ng) % Wave_qp)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWvqp)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#if defined WEC_VF
!
!  Write out WEC quasi-static sea level adjustment.
!
      IF (Qout(idWztw,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWztw), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     OCEAN(ng) % zetaw)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWztw)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out WEC quasi-static pressure.
!
      IF (Qout(idWqsp,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWqsp), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     OCEAN(ng) % qsp)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWqsp)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out WEC Bernoulli head.
!
      IF (Qout(idWbeh,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWbeh), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     OCEAN(ng) % bh)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWbeh)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif
#ifdef SOLVE3D
# ifdef UV_KIRBY
!
!  Write out coupling current velocity (x component).
!
      IF (Qout(idUwav,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUwav), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     OCEAN(ng) % uwave)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUwav)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out coupling current velocity (y component).
!
      IF (Qout(idVwav,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVwav), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     OCEAN(ng) % vwave)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVwav)), QCK(ng)%Rindex

          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
#endif
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
#if defined SGD_ON
!
!  Write out time-dependent SGD point source
!
      IF (Qout(idSgds,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idSgds), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     GRID(ng) % sgd_src,                          &
     &                     SetFillVal = .FALSE.)
        IF (status.ne.nf90_noerr) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSgds)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#endif

#if defined REEF_ECOSYS
!
!  Write out time-dependent Biological 2D fields.
!
      DO i=1,NHbio2d
        ifield=iHbio2(i)
        IF (Qout(ifield,ng)) THEN
          scale=1.0_r8
          gtype=gfactor*r2dvar
          status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                    &
     &                     QCK(ng)%Vid(ifield),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     OCEAN(ng) % HisBio2d(:,:,i))
          IF (status.ne.nf90_noerr) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,ifield)), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out time-dependent Biological 3D fields.
!
      DO i=1,NHbio3d
        ifield=iHbio3(i)
        IF (Qout(ifield,ng)) THEN
          scale=1.0_r8
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid,                    &
     &                     QCK(ng)%Vid(ifield),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     OCEAN(ng) % HisBio3d(:,:,:,i))
          IF (status.ne.nf90_noerr) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,ifield)), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO

!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# ifdef SEDIMENT_ECOSYS
!
!  Write out time-dependent Biological sediment 3D fields.
!
      DO i=1,NHbiosed3d
        ifield=iHbiosed3(i)
        IF (Qout(ifield,ng)) THEN
          scale=1.0_r8
          gtype=gfactor*d3dvar
          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid,                    &
     &                     QCK(ng)%Vid(ifield),                         &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, Nsed(ng), scale,      &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     OCEAN(ng) % HisBiosed3d(:,:,:,i))
          IF (status.ne.nf90_noerr) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,ifield)), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add

!
!-----------------------------------------------------------------------
!  Synchronize quicksave NetCDF file to disk to allow other processes
!  to access data immediately after it is written.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, iNLM, QCK(ng)%name, QCK(ng)%ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

#ifdef SOLVE3D
# ifdef NESTING
      IF (Master) WRITE (stdout,20) KOUT, NOUT, QCK(ng)%Rindex, ng
# else
      IF (Master) WRITE (stdout,20) KOUT, NOUT, QCK(ng)%Rindex
#ifdef OFFLINE_FLOATS
      IF (Master) print *, '  WRT_HIS model time (days), t=', tdays
#endif
# endif
#else
# ifdef NESTING
      IF (Master) WRITE (stdout,20) KOUT, QCK(ng)%Rindex, ng
# else
      IF (Master) WRITE (stdout,20) KOUT, QCK(ng)%Rindex
# endif
#endif
!
  10  FORMAT (/,' WRT_QUICK - error while writing variable: ',a,/,13x,  &
     &        'into quicksave NetCDF file for time record: ',i4)
#ifdef SOLVE3D
  20  FORMAT (6x,'WRT_QUICK   - wrote quicksave', t39,                  &
# ifdef NESTING
     &        'fields (Index=',i1,',',i1,') in record = ',i7.7,t92,i2.2)
# else
     &        'fields (Index=',i1,',',i1,') in record = ',i7.7)
# endif
#else
  20  FORMAT (6x,'WRT_QUICK   - wrote quicksave', t39,                  &
# ifdef NESTING
     &        'fields (Index=',i1,')   in record = ',i7.7,t92,i2.2)
# else
     &        'fields (Index=',i1,')   in record = ',i7.7)
# endif
#endif
      RETURN
      END SUBROUTINE wrt_quick
