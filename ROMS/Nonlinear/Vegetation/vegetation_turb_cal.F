#include "cppdefs.h"

      MODULE vegetation_turb_mod

#if defined NONLINEAR && defined VEGETATION && defined VEG_TURB
!
!svn $Id: vegetation_turb_cal.F 429 2015-06-10 12:30:26Z arango $
!=======================================================================
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license           Hernan G. Arango   !
!    See License_ROMS.txt                                              !
!==================================================== John C. Warner ===
!==================================================== Neil K. Ganju  ===
!==================================================== Alexis Beudin  ===
!==================================================Tarandeep S. Kalra===
!                                                                      !
!  This routine computes the turbulent kinetic energy and length scale !
!  modifications due to vegetation for gls_corstep.F                   !
!                                                                      !
!  References:                                                         !
!                                                                      !
!   Uittenbogaard R. (2003): Modelling turbulence in vegetated aquatic !
!   flows. International workshop on RIParian FORest vegetated         !
!   channels: hydraulic, morphological and ecological aspects,         !
!   20-22 February 2003, Trento, Italy.                                !
!                                                                      !
!   Warner J.C., C.R. Sherwood, H.G. Arango, and R.P. Signell (2005):  !
!   Performance of four turbulence closure models implemented using a  !
!   generic length scale method, Ocean Modelling 8: 81-113.            !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: vegetation_turb_cal
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      PRIVATE :: mangr_get_dens
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add

      CONTAINS
!
!***********************************************************************
      SUBROUTINE vegetation_turb_cal (ng, tile)
!***********************************************************************
!
      USE mod_stepping
      USE mod_grid
      USE mod_ocean
      USE mod_mixing
      USE mod_vegarr
      USE vegetation_drag_mod
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
      CALL vegetation_turb_tile  ( ng, tile,                            &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        nstp(ng), nnew(ng),                       &
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
     &                        GRID(ng) % Hz,                            &
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
     &                        OCEAN(ng) % u,                            &
     &                        OCEAN(ng) % v,                            &
     &                        VEG(ng) % ru_loc_veg,                     &
     &                        VEG(ng) % rv_loc_veg,                     &
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
     &                        VEG(ng) % ru_loc_veg_root,                &
     &                        VEG(ng) % rv_loc_veg_root,                &
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
     &                        VEG(ng) % plant,                          &
# ifdef VEG_FLEX
     &                        VEG(ng) % bend,                           &
# endif
     &                        MIXING(ng) % gls,                         &
     &                        MIXING(ng) % tke,                         &
     &                        VEG(ng) % gls_veg,                        &
     &                        VEG(ng) % tke_veg )
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 16)
# endif
      RETURN
      END SUBROUTINE vegetation_turb_cal
!
!***********************************************************************
      SUBROUTINE vegetation_turb_tile ( ng, tile,                       &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              IminS, ImaxS, JminS, JmaxS,         &
     &                              nstp, nnew,                         &
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
     &                              Hz,                                 &
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
     &                              u, v,                               &
     &                              ru_loc_veg, rv_loc_veg,             &
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
     &                              ru_loc_veg_root, rv_loc_veg_root,   &
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
     &                              plant,                              &
# ifdef VEG_FLEX
     &                              bend,                               &
# endif
     &                              gls, tke,                           &
     &                              gls_veg, tke_veg )
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
      USE mod_vegetation
      USE mod_vegarr
      USE vegetation_drag_mod
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      USE mod_mangr_root
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
!
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp, nnew
!
# ifdef ASSUMED_SHAPE
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
      real(r8), intent(in) :: u(LBi:,LBj:,:,:)
      real(r8), intent(in) :: v(LBi:,LBj:,:,:)
      real(r8), intent(in) :: ru_loc_veg(LBi:,LBj:,:,:)
      real(r8), intent(in) :: rv_loc_veg(LBi:,LBj:,:,:)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      real(r8), intent(in) :: ru_loc_veg_root(LBi:,LBj:,:,:)
      real(r8), intent(in) :: rv_loc_veg_root(LBi:,LBj:,:,:)
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
      real(r8), intent(in) :: plant(LBi:,LBj:,:,:)
# ifdef VEG_FLEX
      real(r8), intent(in) :: bend(LBi:,LBj:,:)
# endif
      real(r8), intent(in) :: gls(LBi:,LBj:,0:,:)
      real(r8), intent(in) :: tke(LBi:,LBj:,0:,:)
      real(r8), intent(inout) :: gls_veg(LBi:,LBj:,0:)
      real(r8), intent(inout) :: tke_veg(LBi:,LBj:,0:)
# else
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,N(ng))
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
      real(r8), intent(in) :: u(LBi:UBi,LBj:UBj,N(ng),nstp)
      real(r8), intent(in) :: v(LBi:UBi,LBj:UBj,N(ng),nstp)
      real(r8), intent(in) :: ru_loc_veg(LBi:UBi,LBj:UBj,N(ng),NVEG)
      real(r8), intent(in) :: rv_loc_veg(LBi:UBi,LBj:UBj,N(ng),NVEG)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      real(r8), intent(in) :: ru_loc_veg_root(LBi:UBi,LBj:UBj,N(ng),NVEG)
      real(r8), intent(in) :: rv_loc_veg_root(LBi:UBi,LBj:UBj,N(ng),NVEG)
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
      real(r8), intent(in) :: plant(LBi:UBi,LBj:UBj,NVEG,NVEGP)
# ifdef VEG_FLEX
      real(r8), intent(in) :: bend(LBi:UBi,LBj:UBj,NVEG)
# endif
      real(r8), intent(in) :: gls(LBi:UBi,LBj:UBj,0:N(ng),nnew)
      real(r8), intent(in) :: tke(LBi:UBi,LBj:UBj,0:N(ng),nnew)
      real(r8), intent(inout) :: gls_veg(LBi:UBi,LBj:UBj,0:N(ng))
      real(r8), intent(inout) :: tke_veg(LBi:UBi,LBj:UBj,0:N(ng))
# endif
!
!  Local variable declarations.
!
      integer :: i, j, k, iveg
!
      real(r8), parameter :: one_half=1.0_r8/2.0_r8
      real(r8), parameter :: one_third=1.0_r8/3.0_r8
      real(r8), parameter :: Inival=0.0_r8
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Rm
!      real(r8), parameter :: cl_veg=1.0_r8, ck=0.09_r8
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Rm
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
!
!  Parameter setting for the coefficient "gamma"
!
!      real(r8), parameter :: cl_veg=1.0_r8, ck=31.62278_r8      ! gamma = 0.1
!      real(r8), parameter :: cl_veg=1.0_r8, ck=11.18034_r8      ! gamma = 0.2
!      real(r8), parameter :: cl_veg=1.0_r8, ck=6.085806_r8      ! gamma = 0.3
!      real(r8), parameter :: cl_veg=1.0_r8, ck=3.952847_r8      ! gamma = 0.4
!      real(r8), parameter :: cl_veg=1.0_r8, ck=2.828427_r8      ! gamma = 0.5
!      real(r8), parameter :: cl_veg=1.0_r8, ck=2.151657_r8      ! gamma = 0.6
!      real(r8), parameter :: cl_veg=1.0_r8, ck=1.707469_r8      ! gamma = 0.7
      real(r8), parameter :: cl_veg=1.0_r8, ck=1.397542_r8      ! gamma = 0.8
!      real(r8), parameter :: cl_veg=1.0_r8, ck=1.171214_r8      ! gamma = 0.9
!      real(r8), parameter :: cl_veg=1.0_r8, ck=1.0_r8           ! gamma = 1.0
!      real(r8), parameter :: cl_veg=1.0_r8, ck=0.866784_r8      ! gamma = 1.1
!      real(r8), parameter :: cl_veg=1.0_r8, ck=0.760726_r8      ! gamma = 1.2
!      real(r8), parameter :: cl_veg=1.0_r8, ck=0.67466_r8       ! gamma = 1.3
!      real(r8), parameter :: cl_veg=1.0_r8, ck=0.603682_r8      ! gamma = 1.4
!      real(r8), parameter :: cl_veg=1.0_r8, ck=0.544331_r8      ! gamma = 1.5
!      real(r8), parameter :: cl_veg=1.0_r8, ck=0.494106_r8      ! gamma = 1.6
!      real(r8), parameter :: cl_veg=1.0_r8, ck=0.451156_r8      ! gamma = 1.7
!      real(r8), parameter :: cl_veg=1.0_r8, ck=0.414087_r8      ! gamma = 1.8
!      real(r8), parameter :: cl_veg=1.0_r8, ck=0.381830_r8      ! gamma = 1.9
!      real(r8), parameter :: cl_veg=1.0_r8, ck=0.353553_r8      ! gamma = 2.0
!      real(r8), parameter :: cl_veg=1.0_r8, ck=0.09_r8          ! Default (gamma = 4.98)
!
!      real(r8), parameter :: ck_root=1.397542_r8      ! gamma = 0.8
!
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
      real(r8), parameter :: max_L=10.0e10_r8
      real(r8), parameter :: eps=1.0e-12_r8
      real(r8) :: wrku1, wrku2, wrku3, wrku4, wrku
      real(r8) :: wrkv1, wrkv2, wrkv3, wrkv4, wrkv
      real(r8) :: wrk, cff1, cff2, cff3, dissip, inverse_dissip
      real(r8) :: solid, L, eqvegT
      real(r8) :: taufree, tauveg, taueff
      real(r8) :: tke_loc_veg, gls_loc_veg
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: vegu
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: vegv
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      real(r8) :: L_root            ! For mangrove roots
      real(r8) :: tauveg_root       ! For mangrove roots
      real(r8) :: taueff_root       ! For mangrove roots
      real(r8) :: tke_loc_veg_root  ! For mangrove roots
      real(r8) :: gls_loc_veg_root  ! For mangrove roots

      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,0:N(ng)) :: dab
!      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: dens_out
      real(r8) :: HRmax  ! Maximum root height
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add

# include "set_bounds.h"
!
      DO k=1,N(ng)-1
        DO j=Jstr,Jend
          DO i=Istr,Iend
            tke_veg(i,j,k)=Inival
            gls_veg(i,j,k)=Inival
          END DO
        END DO
      END DO

      cff1=3.0_r8+gls_p(ng)/gls_n(ng)
      cff2=1.5_r8+gls_m(ng)/gls_n(ng)
      cff3=-1.0_r8/gls_n(ng)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
      dab=Inival
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add

      VEG_LOOP: DO iveg=1,NVEG
        DO k=1,N(ng)-1
          DO j=Jstr,Jend
            DO i=Istr,Iend
!
!-----------------------------------------------------------------------
! Additional turbulence generated by the vegetation =
! work spent by the fluid against the plants (in m3/s3)
!-----------------------------------------------------------------------
!
              wrku1=ru_loc_veg(i,j,k,iveg)*u(i,j,k,nstp)
              wrku2=ru_loc_veg(i,j,k+1,iveg)*u(i,j,k+1,nstp)
              wrku3=ru_loc_veg(i+1,j,k,iveg)*u(i+1,j,k,nstp)
              wrku4=ru_loc_veg(i+1,j,k+1,iveg)*u(i+1,j,k+1,nstp)
              wrku=0.25_r8*(wrku1+wrku2+wrku3+wrku4)
              wrkv1=rv_loc_veg(i,j,k,iveg)*v(i,j,k,nstp)
              wrkv2=rv_loc_veg(i,j,k+1,iveg)*v(i,j,k+1,nstp)
              wrkv3=rv_loc_veg(i,j+1,k,iveg)*v(i,j+1,k,nstp)
              wrkv4=rv_loc_veg(i,j+1,k+1,iveg)*v(i,j+1,k+1,nstp)
              wrkv=0.25_r8*(wrkv1+wrkv2+wrkv3+wrkv4)
              tke_loc_veg=sqrt(wrku*wrku+wrkv*wrkv)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
!
! For mangroves
!
              wrku1=ru_loc_veg_root(i,j,k,iveg)*u(i,j,k,nstp)
              wrku2=ru_loc_veg_root(i,j,k+1,iveg)*u(i,j,k+1,nstp)
              wrku3=ru_loc_veg_root(i+1,j,k,iveg)*u(i+1,j,k,nstp)
              wrku4=ru_loc_veg_root(i+1,j,k+1,iveg)*u(i+1,j,k+1,nstp)
              wrku=0.25_r8*(wrku1+wrku2+wrku3+wrku4)
              wrkv1=rv_loc_veg_root(i,j,k,iveg)*v(i,j,k,nstp)
              wrkv2=rv_loc_veg_root(i,j,k+1,iveg)*v(i,j,k+1,nstp)
              wrkv3=rv_loc_veg_root(i,j+1,k,iveg)*v(i,j+1,k,nstp)
              wrkv4=rv_loc_veg_root(i,j+1,k+1,iveg)*v(i,j+1,k+1,nstp)
              wrkv=0.25_r8*(wrkv1+wrkv2+wrkv3+wrkv4)
              tke_loc_veg_root=sqrt(wrku*wrku+wrkv*wrkv)
!
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
!
!-----------------------------------------------------------------------
! Dissipation due to vegetation
!-----------------------------------------------------------------------
! Dissipation in GLS (Eq. 12 in Warner et al., 2005)
!
              wrk=MAX(tke(i,j,k,nstp),gls_Kmin(ng))
              dissip=(gls_cmu0(ng)**cff1)*(wrk**cff2)*                  &
     &                 (gls(i,j,k,nstp)**cff3)
              inverse_dissip=1.0_r8/MAX(dissip,eps)
!
! Dissipation time-scale for free turbulence
!
              taufree=wrk*inverse_dissip
!
!# ifdef VEG_FLEX
!
! Equivalent thickness: horizontal projection of the bending plant
!
!              eqvegT=plant(i,j,iveg,pthck)+sin(bend(i,j,iveg))*         &
!     &                                       plant(i,j,iveg,phght)
!# else
              eqvegT=plant(i,j,iveg,pthck)
!# endif
!
!
! Solidity:cross-sectional area of a plant the number of plants per m2
!
!
              solid=plant(i,j,iveg,pdiam)*eqvegT*plant(i,j,iveg,pdens)
!
! Eddies typical size constrained by distance in between the plants
!
              L=cl_veg*((1.0_r8-MIN(solid,1.0_r8))/                     &
     &                 plant(i,j,iveg,pdens))**one_half
              L=MIN(L,max_L)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
! !!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Removed
! !
! ! If this is Rhizophora, compute L with the root properties
! ! instead of the stem
! !
!               dab(i,j,k)=dab(i,j,k-1)+Hz(i,j,k)
! !
!               IF (SPE_MANGR(iveg,ng)==1 .and.                           &
!      &            dab(i,j,k-1)<mangr_layer_thck*                        &
!      &            dble(mangr_n_layer)) THEN
!                 IF (plant(i,j,iveg,pdens)>0.01d0) THEN
! !
! ! Compute average distance in between the roots
! !
! !                  CALL mangr_get_dens ( &
! !                                                       ! *** Input ***
! !                  plant(i,j,iveg,pdiam)           , &  ! DBH given in a grid-cell (m)
! !                  plant(i,j,iveg,pdens)           , &  ! Tree density (tree/m2)
! !                  dab(i,j,k-1)                    , &  ! Height of lower boundary of the layer (m)
! !                  dab(i,j,k)                      , &  ! Height of upper boundary of the layer (m)
! !                                                       !
! !                                                       ! *** Output ***
! !                  dens_out(i,j)                     &  ! Vertically averaged root density in a layer (root/m2 ground)
! !                  )
! !                  solid=FAI_ROOT*FAI_ROOT*dens_out(i,j)
! !                  L=cl_veg*((1.0_r8-MIN(solid,1.0_r8))/                 &
! !     &                     dens_out(i,j))**one_half
! !
! ! Eddy length scale may be set to either of stem or root diameter
! ! depending on maximum root height.
! !
!                   HRmax = plant(i,j,iveg,pdiam)*HR_A_ROOT+HR_B_ROOT
!                   IF (dab(i,j,k-1)>HRmax) THEN
!                     L = plant(i,j,iveg,pdiam)
!                   ELSEIF (dab(i,j,k-1)>1.5_r8) THEN
!                     L = plant(i,j,iveg,pdiam)
!                   ELSE
!                     L = FAI_ROOT*1.5_r8
!                     ! L = plant(i,j,iveg,pdiam)
!                   END IF
!
!                   L=MIN(L,max_L)
!                 END IF
!               END IF
! !
! !!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Removed
!
! Modified lengh-scale: for Rhizophora, give stem diameter to L,
!                       and root diameter to L_root.
!
              L = cl_veg*plant(i,j,iveg,pdiam)
              IF (SPE_MANGR(iveg,ng)==1) THEN
                L_root = cl_veg*FAI_ROOT
              ELSE
                L_root = cl_veg*plant(i,j,iveg,pdiam)  ! Dummy input
              END IF
!
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
!
! Dissipation time-scale of eddies in between the plants
!
              tauveg=(L**2.0_r8/(ck**2.0_r8*tke_loc_veg))**one_third
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
!
! Limit maximum of dissipation time-scale to avoid infinity-value
!
              tauveg=MIN(tauveg,10.0e8_r8)
!
! Dissipation time-scale of eddies generated by mangrove roots
!
              tauveg_root=(L_root**2.0_r8/(ck**2.0_r8*                  &
              !tauveg_root=(L_root**2.0_r8/(ck_root**2.0_r8*             &
     &                     tke_loc_veg_root))**one_third
              tauveg_root=MIN(tauveg_root,10.0e8_r8)
!
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Rm
!!
!! Effective dissipation time-scale
!!
!              taueff=MIN(taufree,tauveg)
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Rm
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
!
! Choice of effective dissipation time-scale
!
              taueff=tauveg                         ! Modified time-scale
              taueff_root=tauveg_root               ! Modified time-scale
              ! taueff=taufree                        ! Time-scale of Lopez and Garcia (2001)
              ! taueff_root=taufree                   ! Time-scale of Lopez and Garcia (2001)
              ! taueff=MIN(taufree,tauveg)            ! Default, Time-scale of Beudin et al. (2017)
              ! taueff_root=MIN(taufree,tauveg_root)  ! Default, Time-scale of Beudin et al. (2017)
!
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
              gls_loc_veg=gls_c2(ng)*tke_loc_veg/taueff
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
!
! Dissipation rate of turbulence generated by mangrove roots
!
              gls_loc_veg_root=gls_c2(ng)*tke_loc_veg_root/taueff_root
!
! Zero-out in case this is not Rhizophora
!
              IF (SPE_MANGR(iveg,ng)/=1) THEN
                tke_loc_veg_root = 0.0_r8
                gls_loc_veg_root = 0.0_r8
              END IF
!
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
!
!-----------------------------------------------------------------------
! Add the tke and gls changes from all vegetation types
!-----------------------------------------------------------------------
!
              tke_veg(i,j,k)=tke_loc_veg + tke_veg(i,j,k)
              gls_veg(i,j,k)=gls_loc_veg + gls_veg(i,j,k)
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
!
! Add contribution by mangrove roots
!
              tke_veg(i,j,k)=tke_loc_veg_root + tke_veg(i,j,k)
              gls_veg(i,j,k)=gls_loc_veg_root + gls_veg(i,j,k)
!
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add

            END DO
          END DO
        END DO
      END DO VEG_LOOP
!
      RETURN
      END SUBROUTINE vegetation_turb_tile
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add

      !-----------------------------------------------------------------------
      SUBROUTINE mangr_get_dens ( &
                                           ! *** Input ***
        diameter                      , &  ! DBH given in a grid-cell (m)
        tree_dens                     , &  ! Tree density (tree/m2)
        h_lower                       , &  ! Height of lower boundary of the layer (m)
        h_upper                       , &  ! Height of upper boundary of the layer (m)
                                           !
                                           ! *** Output ***
        mangr_dens_out_layer            &  ! Vertically averaged root density in a layer (root/m2 ground)
        )
        !
        ! !DESCRIPTION:
        ! Calculate the vertically integrated root projected area in a layer
        !
        ! !USES:
        USE mod_mangr_root, only : mangr_DBH_class, mangr_d_DBH,        &
     &                             mangr_layer_thck, mangr_n_layer,     &
     &                             mangr_root_dens
        USE mod_kinds
        !
        ! !ARGUMENTS:
        implicit none
        real(r8), intent(in)  :: diameter, tree_dens, h_lower, h_upper
        real(r8), intent(out) :: mangr_dens_out_layer
        !
        ! !LOCAL VARIABLES:
        real(r8), dimension(0:mangr_n_layer) :: mangr_dab  ! Depth above bed for mangrove root layers
        real(r8) :: mangr_Hz_inverse, mangr_cff1, mangr_cff2
        real(r8) :: mangr_Lveg_loc1, mangr_Lveg_loc2, mangr_layer_frac
        real(r8) :: mangr_Lveg_loc
        integer  :: k, my_dbh_class
        !---------------------------------------------------------------------

        ! Zero out

        mangr_dab(:) = 0.0_r8
        mangr_dens_out_layer = 0.0_r8

        ! Constant variable

        mangr_Hz_inverse = 1.0_r8/mangr_layer_thck
        my_dbh_class = max(nint(diameter/mangr_d_DBH),1)
        my_dbh_class = min(my_dbh_class, mangr_DBH_class)

        ! Compute the root density in the layer

        IF (tree_dens>1.e-06_r8) THEN
          mangr_Lveg_loc=0.0_r8
          DO k=1,mangr_n_layer
            mangr_dab(k)=mangr_dab(k-1)+mangr_layer_thck
            mangr_cff1=MIN((mangr_dab(k)-h_upper)*mangr_Hz_inverse,1.0_r8)
            mangr_Lveg_loc1=MIN(1.0_r8-mangr_cff1,1.0_r8)
            mangr_cff2=MIN((mangr_dab(k)-h_lower)*mangr_Hz_inverse,1.0_r8)
            mangr_Lveg_loc2=MIN(1.0_r8-mangr_cff2,1.0_r8)
            mangr_layer_frac=MAX(mangr_Lveg_loc1-mangr_Lveg_loc2,0.0_r8)
            mangr_dens_out_layer=mangr_dens_out_layer+                &
     &                            mangr_root_dens(my_dbh_class,k)*    &
     &                            tree_dens*                          &
     &                            mangr_layer_frac
            mangr_Lveg_loc=mangr_Lveg_loc+mangr_layer_frac
          END DO
          IF (mangr_dens_out_layer>1.e-06_r8) THEN
            mangr_dens_out_layer=mangr_dens_out_layer/mangr_Lveg_loc
          END IF
        END IF

      RETURN
      END SUBROUTINE mangr_get_dens

!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
#endif
      END MODULE vegetation_turb_mod
