!!!=== Copyright (c) 2025 Takashi NAKAMURA  =====

#include "cppdefs.h"

!!!**** MODULE OF DYNAMIC ENRTGY BUDGET (DEB) MODEL **************************

MODULE mod_deb_model

  implicit none

CONTAINS

!======================================================================================
!  Conversion from V to L (cm3 to cm)
!   V = (delta_M * L)^3. 
!======================================================================================
  real(8) function V2L(V, delta_M)
    real(8), intent(in) :: V        ! Body Volume (cm3)
    real(8), intent(in) :: delta_M  ! Shape parameter (no dim.)

    V2L = V**(1.0d0/3.0d0) / delta_M  ! Structural length (cm)

  end function V2L

!======================================================================================
!  Temperature correction factor (no dimension)
!    Arrhenius equation
!======================================================================================
  real(8) function tempcorr1(T, Tref, T_A)
    real(8), intent(in) :: T     ! Temperature (K)
    real(8), intent(in) :: Tref  ! Refernce temperature (K)
    real(8), intent(in) :: T_A   ! Arrhenius temperature (K)

    tempcorr1 = exp(T_A*(1.0d0/Tref - 1.0d0/T))

  end function tempcorr1

!======================================================================================
!  Temperature correction factor (no dimension)
!    Arrhenius equation x enzyme denaturation at high and low temperatures
!======================================================================================
  real(8) function tempcorr2(T, Tref, T_A, T_H, T_AH, T_L, T_AL)
    real(8), intent(in) :: T     ! Temperature (K)
    real(8), intent(in) :: Tref  ! Refernce temperature (K)
    real(8), intent(in) :: T_A   ! Arrhenius temperature (K)
    real(8), intent(in) :: T_H   ! Upper boundary tolerance range (K)
    real(8), intent(in) :: T_AH  ! Arrhenius temp. for upper boundary (K)
    real(8), intent(in) :: T_L   ! Lower boundary tolerance range (K)
    real(8), intent(in) :: T_AL  ! Arrhenius temp. for lower boundary (K)

    tempcorr2 = exp(T_A*(1.0d0/Tref - 1.0d0/T))                  &
                * (1.0d0 + exp(T_AH*(1.0d0/T_H  - 1.0d0/Tref))   &
                         + exp(T_AL*(1.0d0/Tref - 1.0d0/T_L )) ) &
                / (1.0d0 + exp(T_AH*(1.0d0/T_H  - 1.0d0/T   ))   &
                         + exp(T_AL*(1.0d0/T    - 1.0d0/T_L )) )

  end function tempcorr2

!====================================================================================== 
!  Clearance rate (L s-1) (Saraiva et al., 2012)
!======================================================================================
  subroutine clearance_rate(              &
    ! input parameters
      N, X, V, kT, C_Rmax, J_XiFmax, c2e  &
    ! output parameters
    , C_R, J_Rc)

    ! input parameters
    integer, intent(in ) :: N           ! Number of food types
    real(8), intent(in ) :: X(N)        ! Food concentration (umol L-1)
    real(8), intent(in ) :: V           ! Body Volume (cm3)
    real(8), intent(in ) :: kT          ! Temperature correction factor (no dimension)
    real(8), intent(in ) :: C_Rmax      ! Maximum surface area specific clearance rate (L s-1 cm-2)
    real(8), intent(in ) :: J_XiFmax(N) ! Maximum surface area specific filtration rate (umol s−1 cm−2)
    real(8), intent(in ) :: c2e         ! enrtgy cost for water exchange (J L-1)
    ! output parameters
    real(8), intent(out) :: C_R         ! Clearance rate (L s-1)
    real(8), intent(out) :: J_Rc        ! Required energy for water exchange (J s-1)

    real(8) :: cff
    integer :: i

    cff = 0.0d0
    do i=1,N
      cff = cff + X(i)*C_Rmax/J_XiFmax(i)
    end do
    C_R = kT * C_Rmax * V**(2.0d0/3.0d0) / (1.0d0 + cff) ! Clearance rate (L s-1)

    J_Rc = C_R * c2e  ! Required energy for water exchange (J s-1)

  end subroutine clearance_rate

!======================================================================================
!  Ingestion rate (umol s−1) (Saraiva et al., 2012)
!======================================================================================
  subroutine ingestion(             &
    ! input parameters 
      N, X, C_R, rho_XiI, J_XiImax  &
    ! output parameters
    , J_XiF, J_XiI, J_PiF)
  
    ! input parameters
    integer, intent(in ) :: N           ! Number of food types
    real(8), intent(in ) :: X(N)        ! Food concentration (umol L-1)
    real(8), intent(in ) :: C_R         ! Clearance rate (L s-1)
    real(8), intent(in ) :: rho_XiI(N)  ! Algal binding probability (no dim.)
    real(8), intent(in ) :: J_XiImax(N) ! Algal maximum ingestion rate (umol s−1)
    ! output parameters
    real(8), intent(out) :: J_XiF(N)    ! Filteration rate (umol s−1)
    real(8), intent(out) :: J_XiI(N)    ! Ingestion rate (umol s−1)
    real(8), intent(out) :: J_PiF(N)    ! Pseudofaeces production rate (umol s−1)

    real(8) :: cff
    integer :: i

    cff = 0.0d0
    do i=1,N
      J_XiF(i) = C_R*X(i)  ! Filtration rate (umol s−1)
      cff = cff + rho_XiI(i)*J_XiF(i) / J_XiImax(i)
    end do

    do i=1,N
      J_XiI(i) = rho_XiI(i)*J_XiF(i) / (1.0d0 + cff) ! Ingestion rate (umol s−1)
      J_PiF(i) = J_XiF(i) - J_XiI(i)        ! Pseudofaeces production rate (umol s−1)
    end do

  end subroutine ingestion

!======================================================================================
!  Assimilation rate (umolC s−1, umolN s−1, umolP s−1)
!======================================================================================
!  subroutine assimilation( &
!    ! input parameters 
!      N, J_XiI, AE         &
!    ! output parameters 
!    , Jc_EA, Jn_EA, Jp_EA  &
!    , Jc_RA, Jn_RA, Jp_RA)
!
!    ! input parameters
!    integer, intent(in ) :: N           ! Number of food types
!    real(8), intent(in ) :: J_XiI(N)    ! Ingestion rate (umol s−1)
!    real(8), intent(in ) :: AE(N)       ! assimilation efficiency (no dim.)
!    ! output parameters
!    real(8), intent(out) :: Jc_EA, Jn_EA, Jp_EA   ! Assimilation rate (umolC s−1, umolN s−1, umolP s−1)
!    real(8), intent(out) :: Jc_RA, Jn_RA, Jp_RA   ! Overhead related to assimilation (umolC s−1, umolN s−1, umolP s−1)
!
!
!  end subroutine assimilation

!======================================================================================
! Mobilization flux (umolC s-1) (Saraiva et al., 2012)
!======================================================================================
  subroutine mobilization_flux(             &
    ! input parameters
      V, M_E, J_ES, cond, kappa, E_G, mu_E  &
    ! output parameters
    , J_EC)

    ! input parameters
    real(8), intent(in ) :: V       ! Body Volume (cm3)
    real(8), intent(in ) :: M_E     ! Reserve biomass (mmolC)
    real(8), intent(in ) :: J_ES    ! Somatic maintenance rate (umolC s-1)
    real(8), intent(in ) :: cond    ! Energy conductance (cm s-1)
    real(8), intent(in ) :: kappa   ! Allocation fraction to growth and somatic maintenance
    real(8), intent(in ) :: E_G     ! Specific cost for structure, [E_G] (J cm-3)  
    real(8), intent(in ) :: mu_E    ! Bivalve reserve chemical potential, mu_E  (J mol−1)
    ! output parameters
    real(8), intent(out) :: J_EC  ! Mobilization flux (umolC s-1)

    real(8) :: E_m   !Reserve density (umolC cm-3)
    real(8) :: k_m   !Volume-specific Somatic maintenance rate (umol s-1 cm-3)
                     ! k_m = [E_G]/mu_E = umolC cm-3

    E_m = M_E*1.0d3 / V   ! Reserve density (umolC cm-3)
    k_m = E_G/mu_E*1.0d6  ! Volume-specific Somatic maintenance rate (umol s-1 cm-3)

    J_EC = E_m/(k_m+kappa*E_m)*(k_m*cond*V**(2.0d0/3.0d0) + J_ES)  ! Mobilization flux (umolC s-1)
!    J_EC = E_m/(k_m+kappa*E_m)*(k_m*cond*V**(2.0d0/3.0d0)) + J_ES  ! Mobilization flux (umolC s-1)

  end subroutine mobilization_flux

!======================================================================================
! Somatic maintenance rate (umolC s-1) (Saraiva et al., 2012)
!======================================================================================
  subroutine somatic_maintenance( &
    ! input parameters
      V, p_M, mu_E                &
    ! output parameters
    , J_ES)

    ! input parameters
    real(8), intent(in ) :: V     ! Body Volume (cm3)
    real(8), intent(in ) :: p_M   ! volume specific somatic maintenance rate, [p_M] (J s-1 cm-3)
    real(8), intent(in ) :: mu_E  ! Bivalve reserve chemical potential, mu_E  (J mol−1) 
    ! output parameters
    real(8), intent(out) :: J_ES  ! Somatic maintenance rate (umolC s-1)

    J_ES = p_M/mu_E*1.0d6 * V  ! Somatic maintenance rate (umolC s-1)

  end subroutine somatic_maintenance

!======================================================================================
! Maturity maintenance rate (umolC s-1) (Saraiva et al., 2012)
!======================================================================================
  subroutine maturity_maintenance( &
    ! input parameters
      M_H, k_J                     &
    ! output parameters
    , J_EJ)

    ! input parameters
    real(8), intent(in ) :: M_H   ! Bivalve reproduction buffer (mmolC)
    real(8), intent(in ) :: k_J   ! Volume-specific maturity maintenance rate coefficient (s-1)
    ! output parameters
    real(8), intent(out) :: J_EJ  ! Maturity maintenance rate (umolC s-1)

    J_EJ = k_J * M_H*1.0d3 ! Maturity maintenance rate (umolC s-1)

  end subroutine maturity_maintenance

!======================================================================================
! Growth (umolC s-1) (Saraiva et al., 2012)
!======================================================================================
  subroutine growth(    &
    ! input parameters
      J_EC, J_ES, kappa &
    ! output parameters
    , J_EG)

    ! input parameters
    real(8), intent(out) :: J_EC  ! Mobilization flux (umolC s-1)
    real(8), intent(in ) :: J_ES  ! Somatic maintenance rate (umolC s-1)
    real(8), intent(in ) :: kappa ! Allocation fraction to growth and somatic maintenance
    ! output parameters
    real(8), intent(out) :: J_EG  ! Growth rate (umolC s-1)

    J_EG = kappa*J_EC - J_ES  ! Growth rate (umolC s-1)

  end subroutine growth

!======================================================================================
! Maturity and reproduction (umolC s-1) (Saraiva et al., 2012)
!======================================================================================
  subroutine maturity_reproduction(                   &
    ! input parameters
      M_H, J_EC, J_EJ, kappa, Mp_H                    &
    , M_R, kappa_R, Rspawn, GSR, GSRspawn, T, Tspawn  &
    , date                                            &
    ! output parameters
    , J_ER, JM_ER, JR_ER, Jspawn_ER)

    ! input parameters
    real(8), intent(in ) :: M_H   ! Bivalve maturity investment (mmolC)
    real(8), intent(in ) :: J_EC  ! Mobilization flux (umolC s-1)
    real(8), intent(in ) :: J_EJ  ! Maturity maintenance rate (umolC s-1)
    real(8), intent(in ) :: kappa ! Allocation fraction to growth and somatic maintenance
    real(8), intent(in ) :: Mp_H  ! Cumulative maturity at puberty (mmolC)
                                  ! E_Hp (J)  Maturity at puberty
                                  ! mu_E (J molC−1),  Energy content of reserves
                                  ! Mp_H = E_Hp/mu_E = umolC
    real(8), intent(in ) :: M_R      ! Bivalve reproduction buffer (mmolC)
    real(8), intent(in ) :: kappa_R  ! (no dim.) Reproduction efficiency
    real(8), intent(in ) :: Rspawn   ! (days) Spawning period
    real(8), intent(in ) :: GSR      ! (molC molC−1) Gonado-somatic ratio
    real(8), intent(in ) :: GSRspawn ! (molC molC−1) Gonado-somatic ratio to spawn
    real(8), intent(in ) :: T        ! (oC) Temperature
    real(8), intent(in ) :: Tspawn   ! (oC) Minimum temperature for spawning
    real(8), intent(in ) :: date     ! Date (days since x or elapsed days)
    ! output parameters
    real(8), intent(out) :: J_ER  ! Flux allocated to reproduction/maturity (umolC s-1)
    real(8), intent(out) :: JM_ER ! Flux to maturity (umolC s-1)
    real(8), intent(out) :: JR_ER ! Flux to reproduction buffer (umolC s-1)
    real(8), intent(out) :: Jspawn_ER ! Spawning (umolC s-1)

    real(8), save :: date_end_spawning = 0.0d0  ! Date at the end of spawning (days since x or elapsed days)
    real(8), save :: Jspawn_ER0 = 0.0d0  ! Date at the end of spawning (days since x or elapsed days)

    J_ER = (1.0d0 - kappa)*J_EC - J_EJ ! Flux allocated to reproduction/maturity (umolC s-1)

    if (M_H < Mp_H) then
      JM_ER = J_ER  ! Flux to maturity (umolC s-1)
      JR_ER = 0.0d0 ! Flux to reproduction buffer (umolC s-1)
    else
      JM_ER = 0.0d0 ! Flux to maturity (umolC s-1)
      JR_ER = J_ER  ! Flux to reproduction buffer (umolC s-1)
    end if

    if(GSR>=GSRspawn .and. T>=Tspawn .and. date_end_spawning < date) then
      date_end_spawning = date + Rspawn
      Jspawn_ER0 = kappa_R * M_R*1.0d3 / Rspawn / 86400.0d0  ! Spawning (umolC s-1)
    end if

    if(date_end_spawning >= date) then
      Jspawn_ER = Jspawn_ER0
    else
      Jspawn_ER = 0.0d0
    end if

  end subroutine maturity_reproduction


END MODULE mod_deb_model
