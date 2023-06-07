#include "cppdefs.h"

      MODULE vegetation_drag_mod

#if defined VEGETATION && defined VEG_DRAG
!
!svn $Id: vegetation_drag.F 429 2015-05-26 10:10:26Z arango $
!=======================================================================
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license           Hernan G. Arango   !
!    See License_ROMS.txt                   Alexander F. Shchepetkin   !
!==================================================== John C. Warner ===
!==================================================== Neil K. Ganju  ===
!==================================================== Alexis Beudin  ===
!==================================================Tarandeep S. Kalra===
!                                                                      !
!  This routine computes the vegetation (posture-dependent) drag       !
!  for rhs3d.F                                                         !
!                                                                      !
!  References:                                                         !
!                                                                      !
!  Luhar M., and H. M. Nepf (2011), Flow-induced reconfiguration of    !
!  buoyant and flexible aquatic vegetation, Limnology and Oceanography,!
!   56(6): 2003-2017.                                                  !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: vegetation_drag_cal
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      PRIVATE :: mangr_get_farea
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add

      CONTAINS
!
!***********************************************************************
      SUBROUTINE vegetation_drag_cal (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_stepping
      USE mod_grid
      USE mod_ocean
      USE mod_vegarr
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 16)
# endif
      CALL vegetation_drag_tile  (ng, tile,                             &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        nrhs(ng),                                 &
     &                        GRID(ng) % Hz,                            &
     &                        OCEAN(ng) % u,                            &
     &                        OCEAN(ng) % v,                            &
     &                        VEG(ng) % plant,                          &
# ifdef VEG_FLEX
     &                        VEG(ng) % bend,                           &
# endif
     &                        VEG(ng) % ru_loc_veg,                     &
     &                        VEG(ng) % rv_loc_veg,                     &
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
     &                        VEG(ng) % ru_loc_veg_root,                &
     &                        VEG(ng) % rv_loc_veg_root,                &
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
     &                        VEG(ng) % ru_veg,                         &
     &                        VEG(ng) % rv_veg,                         &
     &                        VEG(ng) % step2d_uveg,                    &
     &                        VEG(ng) % step2d_vveg,                    &
     &                        VEG(ng) % Lveg)

# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 16)
# endif
      RETURN
      END SUBROUTINE vegetation_drag_cal
!
!***********************************************************************
      SUBROUTINE vegetation_drag_tile  (ng, tile,                       &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              IminS, ImaxS, JminS, JmaxS,         &
     &                              nrhs,                               &
     &                              Hz,                                 &
     &                              u, v,                               &
     &                              plant,                              &
# ifdef VEG_FLEX
     &                              bend,                               &
# endif
     &                              ru_loc_veg, rv_loc_veg,             &
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
     &                              ru_loc_veg_root, rv_loc_veg_root,   &
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
     &                              ru_veg, rv_veg,                     &
     &                              step2d_uveg, step2d_vveg,           &
     &                              Lveg)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
      USE mod_vegetation
      USE mod_vegarr
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      USE mod_mangr_root
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nrhs
!
# ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: u(LBi:,LBj:,:,:)
      real(r8), intent(in) :: v(LBi:,LBj:,:,:)
      real(r8), intent(in) :: plant(LBi:,LBj:,:,:)
#  ifdef VEG_FLEX
      real(r8), intent(inout) :: bend(LBi:,LBj:,:)
#  endif
      real(r8), intent(inout) :: ru_loc_veg(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: rv_loc_veg(LBi:,LBj:,:,:)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      real(r8), intent(inout) :: ru_loc_veg_root(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: rv_loc_veg_root(LBi:,LBj:,:,:)
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
      real(r8), intent(inout) :: ru_veg(LBi:,LBj:,:)
      real(r8), intent(inout) :: rv_veg(LBi:,LBj:,:)
      real(r8), intent(inout) :: step2d_uveg(LBi:,LBj:)
      real(r8), intent(inout) :: step2d_vveg(LBi:,LBj:)
      real(r8), intent(inout) :: Lveg(LBi:,LBj:,:)
# else
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: u(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(in) :: v(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(in) :: plant(LBi:UBi,LBj:UBj,NVEG,NVEGP)
#  ifdef VEG_FLEX
      real(r8), intent(inout) :: bend(LBi:UBi,LBj:UBj,N(ng),NVEG)
#  endif
      real(r8), intent(inout) ::                                        &
    &               ru_loc_veg(LBi:UBi,LBj:UBj,N(ng),NVEG),             &
    &               rv_loc_veg(LBi:UBi,LBj:UBj,N(ng),NVEG)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      real(r8), intent(inout) ::                                        &
    &               ru_loc_veg_root(LBi:UBi,LBj:UBj,N(ng),NVEG),        &
    &               rv_loc_veg_root(LBi:UBi,LBj:UBj,N(ng),NVEG)
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
      real(r8), intent(inout) :: ru_veg(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(inout) :: rv_veg(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(inout) :: step2d_uveg(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: step2d_vveg(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: Lveg(LBi:UBi,LBj:UBj,N(ng))
# endif
!
!  Local variable declarations.
!
      integer :: i, j, k, iveg
!
      real(r8), parameter :: one_third  = 1.0_r8/3.0_r8
      real(r8), parameter :: one_twelfth  = 1.0_r8/12.0_r8
      real(r8), parameter :: Inival  = 0.0_r8
      real(r8) :: cff, cff1, cff2, cff3, cff4, Hz_inverse
      real(r8) :: sma, buoy, Umag, Ca, cflex
      real(r8) :: Lveg_loc, plant_height_eff
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,0:N(ng)) :: dab
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: wrk
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: wrk_root           ! For mangrove roots
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,1:N(ng)) :: farea_out  ! Root projected area in a layer (m2 root/m2 ground)
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
!
# include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Resistance imposed on the flow by vegetation.
!-----------------------------------------------------------------------
!
      dab=Inival
      ru_veg=Inival
      rv_veg=Inival
      Lveg=Inival
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      farea_out=Inival
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
!
# ifdef WET_DRY
!
!  Set limiting factor for drag force. The drag force is adjusted
!  to not change the direction of momentum.  It only should slow down
!  to zero. The value of 0.75 is arbitrary limitation assigment
!  (same as for bottom stress).
!
      cff=0.75_r8/dt(ng)
# endif

      VEG_LOOP: DO iveg=1,NVEG
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
!
# ifdef FISH_FARM
!
! Initialization of depth from the water surface (m)
!
        dab = Inival
!
! Loop for vertical layers
!
        K_LOOP: DO k=N(ng),1,-1
!
# else
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
        K_LOOP: DO k=1,N(ng)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
          DO j=JstrV-1,Jend
            DO i=IstrU-1,Iend

# ifdef VEG_FLEX
!
! Flexible vegetation
!
! Second moment of area
!
              sma=(plant(i,j,iveg,pdiam)*                               &
     &             plant(i,j,iveg,pthck)**3.0_r8)*(one_twelfth)
!
! Buoyancy parameter
!
              buoy=(rhow-veg_massdens(iveg,ng))*g*plant(i,j,iveg,pdiam)*&
     &              plant(i,j,iveg,pthck)*                              &
     &              plant(i,j,iveg,phght)**3.0_r8/(E_veg(iveg,ng)*sma)
!
! Current speed at rho points
!
              cff2=0.5_r8*(u(i,j,k,nrhs)+u(i+1,j,k,nrhs))
              cff3=0.5_r8*(v(i,j,k,nrhs)+v(i,j+1,k,nrhs))
              Umag=SQRT(cff2*cff2+cff3*cff3)
!
! Cauchy number
!
              Ca=0.5_r8*rhow*Cd_veg(iveg,ng)*plant(i,j,iveg,pdiam)*     &
     &                   Umag**2.0_r8*plant(i,j,iveg,phght)**3.0_r8/    &
     &                       (E_veg(iveg,ng)*sma)
!
              cflex=1.0_r8-((1.0_r8-0.9_r8*Ca**(-one_third))/           &
     &             (1.0_r8+(Ca**(-1.5_r8)*(8.0_r8+buoy**(1.5_r8)))))
!
! To avoid NaN value when Ca is zero
!
              cflex=MIN(cflex,1.0_r8)
!
! Effective blade length
!
              plant_height_eff=cflex*plant(i,j,iveg,phght)
!
! Blade bending angle
!
              bend(i,j,iveg)=ACOS(cflex**one_third)*rad2deg
# else
!
! For stiff vegetation
!
              plant_height_eff=plant(i,j,iveg,phght)
# endif

!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
!
# ifdef FISH_FARM
!
! depth from the water surface (m)
!
              if (k == N(ng)) then
                 dab(i,j,k) = Hz(i,j,k)
              else
                 dab(i,j,k) = dab(i,j,k+1) + Hz(i,j,k)
              end if
!
# else
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
!
! Select the grid cell (full or part) within the canopy layer
!
              dab(i,j,k)=dab(i,j,k-1)+Hz(i,j,k)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
              Hz_inverse=1.0_r8/Hz(i,j,k)
              cff1=MIN((dab(i,j,k)-plant_height_eff)*Hz_inverse,1.0_r8)
              Lveg_loc=MIN(1.0_r8-cff1,1.0_r8)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
!
! If this is Rhizophora, compute the vertically integrated root
! projected area in a layer
!
              IF (SPE_MANGR(iveg,ng)==1 .and.                           &
     &            dab(i,j,k-1)<mangr_layer_thck*                        &
     &            dble(mangr_n_layer)) THEN
                CALL mangr_get_farea ( &
                                                     ! *** Input ***
                plant(i,j,iveg,pdiam)           , &  ! DBH given in a grid-cell (m)
                plant(i,j,iveg,pdens)           , &  ! Tree density (tree/m2)
                dab(i,j,k-1)                    , &  ! Height of lower boundary of the layer (m)
                dab(i,j,k)                      , &  ! Height of upper boundary of the layer (m)
                                                     !
                                                     ! *** Output ***
                farea_out(i,j,k)                  &  ! Vertically integrated root projected area in a layer (m2 root/m2 ground)
                )
              ELSE
                farea_out(i,j,k)=0.0_r8
              END IF
!
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
!
! Prepare drag term (at rho points)
!
              wrk(i,j)=0.5_r8*cd_veg(iveg,ng)*plant(i,j,iveg,pdiam)*    &
     &                 plant(i,j,iveg,pdens)*Hz(i,j,k)*Lveg_loc
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
# ifdef BAK_EXPERI_STEMFAC
!
! For Bakhawan drag experiment case, account for presence of multiple stems
!
              wrk(i,j)=0.5_r8*cd_veg(iveg,ng)*plant(i,j,iveg,pdiam)*    &
     &                 plant(i,j,iveg,pdens)*Hz(i,j,k)*Lveg_loc*        &
     &                 multistem_fac
# endif
!
! Drag term by the mangrove roots
              wrk_root(i,j) = 0.5_r8*cd_veg(iveg,ng)*farea_out(i,j,k)
              ! wrk(i,j)=wrk(i,j)+0.5_r8*cd_veg(iveg,ng)*farea_out(i,j,k)
!
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
!
! Store Lveg_loc for all vegetation types
!
              Lveg(i,j,k)=Lveg_loc+Lveg(i,j,k)
            END DO
          END DO
!
! Compute friction force (at cell faces)
!
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              cff1=0.25_r8*(v(i  ,j  ,k,nrhs)+                          &
     &                      v(i  ,j+1,k,nrhs)+                          &
     &                      v(i-1,j  ,k,nrhs)+                          &
     &                      v(i-1,j+1,k,nrhs))
              cff2=SQRT(u(i,j,k,nrhs)*u(i,j,k,nrhs)+cff1*cff1)
              cff3=u(i,j,k,nrhs)*cff2
              ru_loc_veg(i,j,k,iveg)=0.5_r8*(wrk(i-1,j)+wrk(i,j))*cff3
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
!
! For mangrove roots
!
              ru_loc_veg_root(i,j,k,iveg)=0.5_r8*(wrk_root(i-1,j)+      &
     &                                            wrk_root(i,j))*cff3
!
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
!
!  Add the ru_iveg from this veg type to another veg type
!  which can be there at the same point (i,j,k)
!  Alexis's comment: not confident in what is happening when
!                     multiple vegetation types are concomitant
!
              ru_veg(i,j,k)=ru_loc_veg(i,j,k,iveg)+ru_veg(i,j,k)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
!
! Add contribution of mangrove roots
!
              ru_veg(i,j,k)=ru_loc_veg_root(i,j,k,iveg)+ru_veg(i,j,k)
!
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add

# ifdef WET_DRY
              cff4=cff*0.5_r8*(Hz(i-1,j,k)+Hz(i,j,k))
              ru_veg(i,j,k)=SIGN(1.0_r8, ru_veg(i,j,k))*                &
     &               MIN(ABS(ru_veg(i,j,k)),                            &
     &                   ABS(u(i,j,k,nrhs))*cff4)
# endif

            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              cff1=0.25_r8*(u(i  ,j  ,k,nrhs)+                          &
     &                      u(i+1,j  ,k,nrhs)+                          &
     &                      u(i  ,j-1,k,nrhs)+                          &
     &                      u(i+1,j-1,k,nrhs))
              cff2=SQRT(cff1*cff1+v(i,j,k,nrhs)*v(i,j,k,nrhs))
              cff3=v(i,j,k,nrhs)*cff2
              rv_loc_veg(i,j,k,iveg)=0.5_r8*(wrk(i,j-1)+wrk(i,j))*cff3
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
!
! For mangrove roots
!
              rv_loc_veg_root(i,j,k,iveg)=0.5_r8*(wrk_root(i-1,j)+      &
     &                                            wrk_root(i,j))*cff3
!
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
!
!   Add the rv_iveg from this veg type to another veg type
!   which can be there at the same point (i,j,k)
!
              rv_veg(i,j,k)=rv_loc_veg(i,j,k,iveg)+rv_veg(i,j,k)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
!
! Add contribution of mangrove roots
!
              rv_veg(i,j,k)=rv_loc_veg_root(i,j,k,iveg)+rv_veg(i,j,k)
!
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add

# ifdef WET_DRY
              cff4=cff*0.5_r8*(Hz(i,j-1,k)+Hz(i,j,k))
              rv_veg(i,j,k)=SIGN(1.0_r8, rv_veg(i,j,k))*                &
     &               MIN(ABS(rv_veg(i,j,k)),                            &
     &                   ABS(v(i,j,k,nrhs))*cff4)
# endif

            END DO
          END DO
        END DO K_LOOP
      END DO VEG_LOOP
!
!-----------------------------------------------------------------------
!  Add in resistance imposed on the flow by the vegetation (3D->2D).
!  Changes feedback in Nonlinear/step2d_LF_AM3.F
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff=0.5_r8*(Hz(i-1,j,1)+Hz(i,j,1))
          cff2=cff*ru_veg(i,j,1)
          DO k=2,N(ng)
            cff=0.5_r8*(Hz(i-1,j,k)+Hz(i,j,k))
            cff2=cff2+cff*ru_veg(i,j,k)
          END DO
          step2d_uveg(i,j)=cff2
        END DO
      END DO
!
      DO i=Istr,Iend
        DO j=JstrV,Jend
          cff=0.5_r8*(Hz(i,j-1,1)+Hz(i,j,1))
          cff2=cff*rv_veg(i,j,1)
          DO k=2,N(ng)
            cff=0.5_r8*(Hz(i,j-1,k)+Hz(i,j,k))
            cff2=cff2+cff*rv_veg(i,j,k)
          END DO
          step2d_vveg(i,j)=cff2
        END DO
      END DO
!
      RETURN
      END SUBROUTINE vegetation_drag_tile
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add

      !-----------------------------------------------------------------------
      SUBROUTINE mangr_get_farea ( &
                                           ! *** Input ***
        diameter                      , &  ! DBH given in a grid-cell (m)
        tree_dens                     , &  ! Tree density (tree/m2)
        h_lower                       , &  ! Height of lower boundary of the layer (m)
        h_upper                       , &  ! Height of upper boundary of the layer (m)
                                           !
                                           ! *** Output ***
        mangr_farea_out_layer           &  ! Vertically integrated root projected area in a layer (m2 root/m2 ground)
        )
        !
        ! !DESCRIPTION:
        ! Calculate the vertically integrated root projected area in a layer
        !
        ! !USES:
        USE mod_mangr_root, only : mangr_DBH_class, mangr_d_DBH,        &
     &                             mangr_layer_thck, mangr_n_layer,     &
     &                             mangr_azi_effect,                    &
     &                             mangr_root_farea
        USE mod_kinds
        !
        ! !ARGUMENTS:
        implicit none
        real(r8), intent(in)    :: diameter, tree_dens, h_lower, h_upper
        real(r8), intent(inout) :: mangr_farea_out_layer
        !
        ! !LOCAL VARIABLES:
        real(r8), dimension(0:mangr_n_layer) :: mangr_dab  ! Depth above bed for mangrove root layers
        real(r8) :: mangr_Hz_inverse, mangr_cff1, mangr_cff2
        real(r8) :: mangr_Lveg_loc1, mangr_Lveg_loc2, mangr_layer_frac
        integer  :: k, my_dbh_class
        !---------------------------------------------------------------------

        ! Zero out

        mangr_dab(:) = 0.0_r8
        mangr_farea_out_layer = 0.0_r8

        ! Constant variable

        mangr_Hz_inverse = 1.0_r8/mangr_layer_thck
        my_dbh_class = max(nint(diameter/mangr_d_DBH),1)
        my_dbh_class = min(my_dbh_class, mangr_DBH_class)

        ! Compute the root projected area in the layer

        IF (tree_dens>1.e-06_r8) THEN
          DO k=1,mangr_n_layer
            mangr_dab(k)=mangr_dab(k-1)+mangr_layer_thck
            mangr_cff1=MIN((mangr_dab(k)-h_upper)*mangr_Hz_inverse,1.0_r8)
            mangr_Lveg_loc1=MIN(1.0_r8-mangr_cff1,1.0_r8)
            mangr_cff2=MIN((mangr_dab(k)-h_lower)*mangr_Hz_inverse,1.0_r8)
            mangr_Lveg_loc2=MIN(1.0_r8-mangr_cff2,1.0_r8)
            mangr_layer_frac=MAX(mangr_Lveg_loc1-mangr_Lveg_loc2,0.0_r8)
            mangr_farea_out_layer=mangr_farea_out_layer+                &
     &                            mangr_root_farea(my_dbh_class,k)*     &
     &                            tree_dens*mangr_azi_effect*           &
     &                            mangr_layer_frac
          END DO
        END IF

      RETURN
      END SUBROUTINE mangr_get_farea

!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
#endif
      END MODULE vegetation_drag_mod
