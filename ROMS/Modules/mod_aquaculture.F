
!!!=== Copyright (c) 2024-2025 Takashi NAKAMURA  =====

#include "cppdefs.h"

!!!*************** AQUACULTURE *****************************************

MODULE mod_aquaculture
#ifdef AQUACULTURE
  implicit none

  integer, parameter :: Naq = 4    !! Number of aquaculture type
                                    !  01 Oyster        
                                    !  02 Wakame Seaweed
                                    !  03 Sea squirt  
                                    !  04 Scallop
                                    !  05 Salmon (not used) 
                                    !
                                    !      01      02      03      04      05
  real(8), parameter :: diam_aqua(5) = [ 0.15d0, 0.15d0,  0.1d0,  0.1d0,  0.1d0 ] ! Diameter (m)
  real(8), parameter :: thck_aqua(5) = [ 0.15d0, 0.05d0,  0.1d0,  0.1d0,  0.1d0 ] ! Thickness (m)
  real(8), parameter :: hght_aqua(5) = [ 9.0d0 ,  3.0d0,  5.0d0,  5.0d0,  1.0d0 ] ! Height (m)

  real(8), parameter :: L_oyster_cluster       =  0.4d0  ! Interval of oyster clusters on a rope (m) (20 clusters per 9 m rope; Sakamaki et al., 2022)
  real(8), parameter :: num_oyster_per_cluster = 23.1d0  ! Number of oysters per cluster (23.1+-13.3 indiv./cluster) (Sakamaki et al., 2022 SM)

CONTAINS
!======================================================================================
! Density of oyster (indiv. m-3) for Sizugawa Bay aquaculture
!======================================================================================
  SUBROUTINE dens_oyster_Shizugawa(dens_aqua, N, dz, dens_oys)
    ! input parameters
    real(8), intent(in ) :: dens_aqua  ! Density of oyster aquaculture (rope m-2)
    integer, intent(in ) :: N          ! Vertical index; total number of vertical layers
    real(8), intent(in ) :: dz(N)      ! Thicknesses (m) of vertical RHO-points.
    real(8), intent(out) :: dens_oys(N)  ! Density of oyster aquaculture (indiv. m-3)

    real(8) :: z_w(0:N)   ! Depth of vertical W-points (m, positive downward)
    real(8) :: dens_oyster_per_rope   ! Density of oyster per rope (indiv. m-3 rope-1)
    real(8) :: rDepth2Rope            ! Ratio of rope length in the cell to vertiacl cell length (m-rope m-1-vertiacl cell length)
    integer :: k          ! Vertical index; k=0 at bottom, k=N at surface

    dens_oyster_per_rope = num_oyster_per_cluster / L_oyster_cluster  ! Density of oyster per rope (indiv. m-1 rope-1)

    z_w(N) = 0.0d0
    DO k=N,1,-1
      z_w(k-1) = z_w(k) + dz(k)   

      if (z_w(k-1) < hght_aqua(1) ) then ! above aquaculture depth
        rDepth2Rope = 1.0d0
      else
        if (z_w(k) < hght_aqua(1) ) then ! within aquaculture depth
          rDepth2Rope = (hght_aqua(1) - z_w(k)) / dz(k)
        else                             ! below aquaculture depth
          rDepth2Rope = 0.0d0
        end if
      end if

      dens_oys(k) = dens_aqua * dens_oyster_per_rope * rDepth2Rope  ! Density of oyster (indiv. m-3)
    END DO
    
  END SUBROUTINE dens_oyster_Shizugawa

#endif
END MODULE mod_aquaculture

