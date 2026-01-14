
!!!=== Copyright (c) 2025 Takashi NAKAMURA  =====

#include "cppdefs.h"

!!!**** MODULE FOR REEF_ECOSYS PARAMETER SETTINGS ************************************

MODULE mod_reef_ecosys_param
  implicit none

! ==== GEOCHEMICAL PARAMETER SETTINGS ===========================================================
  
! ------ Carbon Isotope parameters -----
#if defined CARBON_ISOTOPE || defined CARBON_TRACE
  integer, parameter :: N_Csp = 2   ! Number of Carbon species/isotopes
#elif defined CLUMPED_ISOTOPE
  integer, parameter :: N_Csp = 3   ! Number of Carbon species/isotopes
#else
  integer, parameter :: N_Csp = 1   ! Number of Carbon species/isotopes
#endif
  integer, parameter ::  iCt  =  1  ! Total carbon (12C+13C)
  integer, parameter ::  iC13 =  2  ! 13C
  integer, parameter ::  iD47 =  3  ! Clumpt isotope (47D)

! ------ Nitrogen Isotope parameters -----
#if defined NITROGEN_ISOTOPE || defined NITROGEN_TRACE
  integer, parameter :: N_Nsp = 2   ! Number of Nitrogen species/isotopes
#else
  integer, parameter :: N_Nsp = 1   ! Number of Nitrogen species/isotopes
#endif
  integer, parameter ::  iNt  =  1  ! Total nitrogen (14N+15N)
  integer, parameter ::  iN15 =  2  ! 15N

! ------ Phosphorous Isotope parameters -----
#if defined PHOSPHOROUS_TRACE
  integer, parameter :: N_Psp = 2   ! Number of Phosphorous species
#else
  integer, parameter :: N_Psp = 1   ! Number of Phosphorous species
#endif
  integer, parameter ::  iPt  =  1  ! Total phosphrous
  integer, parameter ::  iP1  =  2  ! phosphrous tracer1

! ------ Sulfur Isotope parameters -----
#if defined SULFUR_ISOTOPE || defined SULFUR_TRACE
  integer, parameter :: N_Ssp = 2   ! Number of Sulfur species/isotopes
#else
  integer, parameter :: N_Ssp = 1   ! Number of Sulfur species/isotopes
#endif
  integer, parameter ::  iSt  =  1  ! Total Sulfur (32S+34S)
  integer, parameter ::  iS34 =  2  ! 34S


! ==== FOODWEB MODEL PARAMETER SETTINGS ============================================================

  integer, parameter :: Ndom   = 2    ! Number of Dissolved Organic Matter types
  integer, parameter ::  iLDOM =  1   !  1: Labile DOM
  integer, parameter ::  iRDOM =  2   !  2: Refractory DOM

  integer, parameter :: Npom   = 3    ! Number of Particulate Organic Matter types
  integer, parameter ::  iLPOM =  1   !  1: Labile POM
  integer, parameter ::  iRPOM =  2   !  2: Refractory POM
  integer, parameter ::  iCPOM =  3   !  3: Coarse POM (leaf litter, etc.)

  integer, parameter :: Nphy   = 4    ! Number of Phytoplankton types
  integer, parameter ::  iDnfl =  1   !  1: Dinoflagellate
  integer, parameter ::  iDtm  =  2   !  2: Diatom
  integer, parameter ::  iCcl  =  3   !  3: Coccolithophorids
  integer, parameter ::  iCya  =  4   !  4: Cyanobacteria
   
  integer, parameter :: Nzoo   = 1    ! Number of Zooplankton types
  integer, parameter ::  iGzoo =  1   !  1: General Zooplankton

  integer, parameter :: Npim   = 2    ! Number of Particulate Inorganic Matter types
  integer, parameter ::  iLive =  1   !  1: Living Coccolith shell (CaCO3)
  integer, parameter ::  iDead =  2   !  2: Dead Coccolith shell (CaCO3)

!---- POM sinking parameters           LPOM    RPOM    CPOM    
  real(8), parameter :: rho_POM(3) = [ 1.1d0,  1.1d0,  0.99d0 ]  ! ***** POM density [g/cm3] 
  real(8), parameter :: D_POM(3)   = [ 1.0d-1, 1.0d-1, 10.0d0 ]  ! ***** POM diameter [mm] 

!---- PIM sinking parameters           iLive   iDead
  real(8), parameter :: rho_PIM(2) = [ 1.4d0 , 1.4d0  ]  ! ***** PIM density [g/cm3] 
  real(8), parameter :: D_PIM(2)   = [ 1.0d-2, 1.0d-2 ]  ! ***** PIM diameter [mm] 

!---- Stoichiometric parameters for phytoplankton and zooplankton  --------------------
! Redfield Ratio (1934) C:N:P -> C:N=6.6  C:P=106    N:P=16
! Martiny et al  (2014) C:N:P -> C:N=7.4  C:P=163    N:P=22
! GLODAP Data           C:N:P -> C:N=9.2  C:P=131.9  N:P=14.4
!                                        Dnfl        Dtm         Ccl         Cya
  real(8), parameter :: rNCp(4)  = [ 16d0/106d0, 16d0/106d0, 16d0/106d0, 16d0/106d0 ] ! (no dim.) Phy N:C ratio
  real(8), parameter :: rPCp(4)  = [  1d0/106d0,  1d0/106d0,  1d0/106d0,  1d0/106d0 ] ! (no dim.) Phy P:C ratio
  real(8), parameter :: rCaCp(4) = [      0.0d0,      0.0d0,      0.5d0,      0.0d0 ]  ! ***** (no dim.) CaCO3:OrgC ratio !!! Need Ref!!!!!!!!!!
!                                        Gzoo
  real(8), parameter :: rNCz(1)  = [ 16d0/106d0 ]    ! (no dim.) Zoo  N:C ratio
  real(8), parameter :: rPCz(1)  = [  1d0/106d0 ]    ! (no dim.) Zoo  P:C ratio
!  real(8), parameter :: rNCz(1)  = [ 16d0/300d0 ]    ! (no dim.) Zoo  N:C ratio  !! For mass balance test
!  real(8), parameter :: rPCz(1)  = [  1d0/300d0 ]    ! (no dim.) Zoo  P:C ratio  !! For mass balance test


! ==== SEDECOSYS MODEL PARAMETER SETTINGS ===========================================================

  integer, parameter :: Nsed = 21

  real(8) :: MnO2_sed  !! Manganese oxide (MnO2) concentration in sediments [nmol/g(DW)]
  real(8) :: FeOOH_sed !! Iron hydroxide (FeOOH) concentration in sediments [nmol/g(DW)]
  real(8) :: FeS2_sed  !! Iron sulfide (FeS2) concentration in sediments [nmol/g(DW)]
  real(8) :: OrgC_sed  !! Organic carbon in sediments [nmol/g(DW)]

  real(8) :: rCaCO3_sed  !! Fraction of Calcium carbonate (CaCO3) in sediments [% DW]

! ==== CORAL POLYP MODEL PARAMETER SETTINGS ===========================================================

  integer, parameter :: Ncl = 2    !! Number of coral types
  ! integer, parameter :: Nzx = 1    !! Number of zooxanthellae clade types


! ==== SEAGRASS MODEL PARAMETER SETTINGS ===========================================================

  integer, parameter :: Nsg = 1     ! Number of seagrass species

  
! ==== SEAGRASS MODEL PARAMETER SETTINGS ===========================================================

  integer, parameter :: Nag = 1    !! Number of algal groups



END MODULE mod_reef_ecosys_param

