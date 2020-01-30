
!!!=== Copyright (c) 2012-2020 Takashi NAKAMURA  =====
!!!      modified by Shinya AMANO, Faisal AMRI

#include "cppdefs.h"


!!!**** MODULE OF FOOD WEB *******************************************

MODULE mod_foodweb
  implicit none

  integer, parameter :: Ndom = 2     !! Number of Dissolved Organic Matter types
  integer, parameter :: iLbl = 1     !!  1: Labile DOM
  integer, parameter :: iRfr = 2     !!  2: Refractory DOM

  integer, parameter :: Npom = 1     !! Number of Particulate Organic Matter types
  integer, parameter :: iDtr = 1     !!  1: Detritus

  integer, parameter :: Nphy  = 3    !! Number of Phytoplankton types
  integer, parameter :: iDnfl = 1    !!  1: dinoflagellate
  integer, parameter :: iDtm  = 2    !!  2: diatom
  integer, parameter :: iCcl  = 3    !!  3: coccolithophorids
   
  integer, parameter :: Nzoo = 1     !! Number of Zooplankton types
  integer, parameter :: iGzoo = 1    !!  1: general Zooplankton

  integer, parameter :: Npim   = 1   !! Number of Particulate Inorganic Matter types
  integer, parameter :: iCaCO3 = 1   !!  1: coccolith (CaCO3)

!---- POM sinking parameters
  real(8), parameter :: rho_POM(Npom) = (/ 1.1d0 /)   ! ***** POM density [g/cm3] 
  real(8), parameter :: D_POM(Npom)   = (/ 0.1d0 /)   ! ***** POM diameter [mm] 

!---- PIM sinking parameters
  real(8), parameter :: rho_PIM(Npim) = (/ 1.4d0 /)   ! ***** PIM density [g/cm3] 
  real(8), parameter :: D_PIM(Npom)   = (/ 0.1d0 /)   ! ***** PIM diameter [mm] 

CONTAINS

!!! **********************************************************************
!!!  Main program of foodweb model (Modified from Yamamoto et al. under review)
!!! **********************************************************************

  SUBROUTINE foodweb &
!   input parameters
    ( ng, n, i, j    &   ! ng: nested grid number; n: coral compartment; i,j: position
    , dt             &   ! Time step (sec)
    , PFD            &   ! Photon flux density (umol m-2 s-1)
    , rho_sw         &   ! Density of seawater (g cm-3)
    , Tmp            &   ! Temperature (oC)
    , Sal            &   ! Salinity (PSU)
    , DIC            &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
    , TA             &   ! Total alkalinity (TA: umol kg-1)
    , DOx            &   ! Dissolved oxygen (umol L-1)
    , DOC            &   ! DOC(Ndom): Dissolved organic carbon (DOC: umol L-1)
    , POC            &   ! POC(Npom): Particulate organic carbon (POC: umol L-1)
    , PHY            &   ! PHY(Nphy): phytoplankton (umol C L-1)
    , ZOO            &   ! ZOO(Nzoo): zooplankton (umol C L-1)
    , PIC            &   ! PIC(Npim): Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
#if defined CARBON_ISOTOPE
    , DI13C          &   ! 13C of DIC (umol kg-1)
    , DO13C          &   ! 13C of Labile Dissolved organic carbon (DOC: umol L-1)
    , PO13C          &   ! 13C of Particulate organic carbon (POC: umol L-1)
    , PHY13C         &   ! 13C of phytoplankton1 (umol C L-1), dinoflagellate
    , ZOO13C         &   ! 13C of zooplankton (umol C L-1)
    , PI13C          &   ! 13C of Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
#endif
#if defined NUTRIENTS         
    , NO3            &   ! NO3 (umol L-1)
    , NO2            &   ! NO2 (umol L-1)
    , NH4            &   ! NH4 (umol L-1)
    , PO4            &   ! PO4 (umol L-1)
    , DON            &   ! Dissolved organic nitrogen (DON: umol L-1)
    , PON            &   ! Particulate organic nitrogen (PON: umol L-1)
    , DOP            &   ! Dissolved organic phosporius (DOP: umol L-1)
    , POP            &   ! Particulate organic phosporius (POP: umol L-1)
# if defined NITROGEN_ISOTOPE
    , NO3_15N        &   ! 15N of NO3 (umol L-1)
    , NO2_15N        &   ! 15N of NO2 (umol L-1)
    , NH4_15N        &   ! 15N of NH4 (umol L-1)
    , DO15N          &   ! 15N of Labile Dissolved organic nitrogen (DON: umol L-1)
    , PO15N          &   ! 15N of Particulate organic nitrogen (PON: umol L-1)
    , PHY15N         &   ! 15N of phytoplankton1 (umol C L-1), dinoflagellate
    , ZOO_15N        &   ! 15N of zooplankton (umol C L-1)
# endif
#endif
#if defined COT_STARFISH         
    , COTe           &   ! COT starfish egg (umol L-1)
    , COTl           &   ! COT starfish larvae (umol L-1)
#endif
!   output parameters
    , dDIC_dt        &   ! dDIC/dt  (umol kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    , dTA_dt         &   ! dTA/dt   (umol kg-1 s-1) 
    , dDOx_dt        &   ! dDOx/dt  (umol L-1 s-1) 
    , dDOC_dt        &   ! dDOC/dt  (umol L-1 s-1) 
    , dPOC_dt        &   ! dPOC/dt  (umol L-1 s-1) 
    , dPHY_dt        &   ! dPHY1/dt  (umol L-1 s-1)  
    , dZOO_dt        &   ! dZOO/dt  (umol L-1 s-1)  
    , dPIC_dt        &   ! dPIC/dt  (umol L-1 s-1)  
#if defined CARBON_ISOTOPE
    , dDI13C_dt      &   ! dDI13C/dt (umol kg-1 s-1)
    , dDO13C_dt      &   ! dDO13C/dt  (umol L-1 s-1) 
    , dPO13C_dt      &   ! dPO13C/dt  (umol L-1 s-1) 
    , dPHY13C_dt     &   ! dPHY13C/dt  (umol L-1 s-1)  
    , dZOO13C_dt     &   ! dZOO13C/dt  (umol L-1 s-1)  
    , dPI13C_dt      &   ! dPI13C/dt  (umol L-1 s-1)  
#endif
#if defined NUTRIENTS
    , dNO3_dt        &   ! dNO3/dt (umol L-1 s-1)
    , dNO2_dt        &   ! dNO2/dt (umol L-1 s-1)
    , dNH4_dt        &   ! dNH4/dt (umol L-1 s-1)
    , dPO4_dt        &   ! dPO4/dt (umol L-1 s-1)
    , dDON_dt        &   ! dDON/dt (umol L-1 s-1)
    , dPON_dt        &   ! dPON/dt (umol L-1 s-1)
    , dDOP_dt        &   ! dDOP/dt (umol L-1 s-1)
    , dPOP_dt        &   ! dPOP/dt (umol L-1 s-1)
# if defined NITROGEN_ISOTOPE
    , dNO3_15N_dt    &   ! dNO3_15N/dt (umol L-1 s-1)
    , dNO2_15N_dt    &   ! dNO2_15N/dt (umol L-1 s-1)
    , dNH4_15N_dt    &   ! dNH4_15N/dt (umol L-1 s-1)
    , dDO15N_dt      &   ! dDO15N/dt  (umol L-1 s-1) 
    , dPO15N_dt      &   ! dPO15N/dt  (umol L-1 s-1) 
    , dPHY15N_dt     &   ! dPHY15N/dt  (umol L-1 s-1)  
    , dZOO_15N_dt    &   ! dZOO_15N/dt  (umol L-1 s-1)  
# endif
#endif                                
#if defined COT_STARFISH         
    , dCOTe_dt       &   ! dCOTe/dt (umol L-1 s-1)
    , dCOTl_dt       &   ! dCOTl/dt (umol L-1 s-1)
#endif
    )

!-----------------------------------------------------------------------
!
    implicit none

!        input parameters
    integer, intent(in) :: ng, n, i, j    ! ng: nested grid number; n: coral compartment; i,j: position
    real(8), intent(in) :: dt             ! Time step (sec)
    real(8), intent(in) :: PFD            ! Photon flux density (umol m-2 s-1)
    real(8), intent(in) :: rho_sw         ! Density of seawater (g cm-3)
    real(8), intent(in) :: Tmp            ! Temperature (oC)
    real(8), intent(in) :: Sal            ! Salinity (PSU)
    real(8), intent(in) :: DIC            ! Total dissolved inorganic carbon (DIC: umol kg-1)
    real(8), intent(in) :: TA             ! Total alkalinity (TA: umol kg-1)
    real(8), intent(in) :: DOx            ! Dissolved oxygen (umol L-1)
    real(8), intent(in) :: DOC(Ndom)      ! Dissolved organic carbon (DOC: umol L-1)
    real(8), intent(in) :: POC(Npom)      ! Particulate organic carbon (POC: umol L-1)
    real(8), intent(in) :: PHY(Nphy)      ! phytoplankton1 (umol C L-1), dinoflagellate
    real(8), intent(in) :: ZOO(Nzoo)      ! zooplankton (umol C L-1)
    real(8), intent(in) :: PIC(Npim)      ! Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
#if defined CARBON_ISOTOPE
    real(8), intent(in) :: DI13C          ! 13C of DIC (umol kg-1)
    real(8), intent(in) :: DO13C(Ndom)    ! 13C of Labile Dissolved organic carbon (DOC: umol L-1)
    real(8), intent(in) :: PO13C(Npom)    ! 13C of Particulate organic carbon (POC: umol L-1)
    real(8), intent(in) :: PHY13C(Nphy)   ! 13C of phytoplankton1 (umol C L-1), dinoflagellate
    real(8), intent(in) :: ZOO13C(Nzoo)   ! 13C of zooplankton (umol C L-1)
    real(8), intent(in) :: PI13C(Npim)    ! 13C of Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
#endif
#if defined NUTRIENTS         
    real(8), intent(in) :: NO3            ! NO3 (umol L-1)
    real(8), intent(in) :: NO2            ! NO2 (umol L-1)
    real(8), intent(in) :: NH4            ! NH4 (umol L-1)
    real(8), intent(in) :: PO4            ! PO4 (umol L-1)
    real(8), intent(in) :: DON(Ndom)           ! Labile Dissolved organic nitrogen (DON: umol L-1)
    real(8), intent(in) :: PON(Npom)            ! Particulate organic nitrogen (PON: umol L-1)
    real(8), intent(in) :: DOP(Ndom)           ! Labile Dissolved organic phosporius (DOP: umol L-1)
    real(8), intent(in) :: POP(Npom)            ! Particulate organic phosporius (POP: umol L-1)
# if defined NITROGEN_ISOTOPE
    real(8), intent(in) :: NO3_15N        ! 15N of NO3 (umol L-1)
    real(8), intent(in) :: NO2_15N        ! 15N of NO2 (umol L-1)
    real(8), intent(in) :: NH4_15N        ! 15N of NH4 (umol L-1)
    real(8), intent(in) :: DO15N(Ndom)         ! 15N of Labile Dissolved organic nitrogen (DON: umol L-1)
    real(8), intent(in) :: PO15N(Npom)          ! 15N of Particulate organic nitrogen (PON: umol L-1)
    real(8), intent(in) :: PHY15N(Nphy)       ! 15N of phytoplankton1 (umol C L-1), dinoflagellate
    real(8), intent(in) :: ZOO15N(Nzoo)        ! 15N of zooplankton (umol C L-1)
# endif
#endif
#if defined COT_STARFISH         
    real(8), intent(in) :: COTe           ! COT starfish egg (umol L-1)
    real(8), intent(in) :: COTl           ! COT starfish larvae (umol L-1)
#endif
!   output parameters
    real(8), intent(out) :: dDIC_dt       ! dDIC/dt  (umol kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    real(8), intent(out) :: dTA_dt        ! dTA/dt   (umol kg-1 s-1) 
    real(8), intent(out) :: dDOx_dt       ! dDOx/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dDOC_dt(Ndom)      ! dDOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPOC_dt(Npom)       ! dPOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPHY_dt(Nphy)      ! dPHY1/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dZOO_dt(Nzoo)       ! dZOO/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dPIC_dt(Npim)       ! dPIC/dt  (umol L-1 s-1)  
#if defined CARBON_ISOTOPE
    real(8), intent(out) :: dDI13C_dt     ! dDI13C/dt (umol kg-1 s-1)
    real(8), intent(out) :: dDO13C_dt(Ndom)     ! dDO13C/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPO13C_dt(Npom)     ! dPO13C/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPHY13C_dt(Nphy)    ! dPHY13C/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dZOO13C_dt(Nzoo)   ! dZOO13C/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dPI13C_dt(Npim)     ! dPI13C/dt  (umol L-1 s-1)  
#endif
#if defined NUTRIENTS
    real(8), intent(out) :: dNO3_dt       ! dNO3/dt (umol L-1 s-1)
    real(8), intent(out) :: dNO2_dt       ! dNO2/dt (umol L-1 s-1)
    real(8), intent(out) :: dNH4_dt       ! dNH4/dt (umol L-1 s-1)
    real(8), intent(out) :: dPO4_dt       ! dPO4/dt (umol L-1 s-1)
    real(8), intent(out) :: dDON_dt(Ndom)      ! dDON/dt (umol L-1 s-1)
    real(8), intent(out) :: dPON_dt(Npom)       ! dPON/dt (umol L-1 s-1)
    real(8), intent(out) :: dDOP_dt(Ndom)      ! dDOP/dt (umol L-1 s-1)
    real(8), intent(out) :: dPOP_dt(Npom)       ! dPOP/dt (umol L-1 s-1)
# if defined NITROGEN_ISOTOPE
    real(8), intent(out) :: dNO3_15N_dt   ! dNO3_15N/dt (umol L-1 s-1)
    real(8), intent(out) :: dNO2_15N_dt   ! dNO2_15N/dt (umol L-1 s-1)
    real(8), intent(out) :: dNH4_15N_dt   ! dNH4_15N/dt (umol L-1 s-1)
    real(8), intent(out) :: dDO15N_dt(Ndom)    ! dDO15N/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPO15N_dt(Npom)     ! dPO15N/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPHY15N_dt(Nphy)  ! dPHY15N/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dZOO15N_dt(Nzoo)   ! dZOO15N/dt  (umol L-1 s-1)  
# endif
#endif                                
#if defined COT_STARFISH
    real(8), intent(out) :: dCOTe_dt      ! dCOTe/dt (umol L-1 s-1)
    real(8), intent(out) :: dCOTl_dt      ! dCOTl/dt (umol L-1 s-1)
#endif

!!!------------Set parameters  ----------------------------------

!------- Phytoplankton parameters ------------------------
!  Maximum photosynthetic rate at 0 oC (0.4d0 d-1; Kishi et al., 2007)0.3
    real(8), parameter :: k_Pp(Nphy) = (/ 0.4d0/86400.0d0, 0.8d0/86400.0d0, 0.4d0/86400.0d0  /)  ! (s-1)
!  Temperature coefficient for PHY photosynthesis (0.063; Kawamiya et al., 1995)          
    real(8), parameter :: b_Pp(Nphy) = (/ 0.0693d0, 0.0693d0, 0.0693d0 /)  ! (degC-1) 
!  Optimum light intensity (48.83d0 J m2 s-1; Kawamiya et al., 1995)     
    real(8), parameter :: Ip(Nphy)   = (/ 48.83d0*1.82d0, 48.83d0*1.82d0, 48.83d0*1.82d0 /) ! (umol m-2 s-1) 
!  Respiration rate at 0 oC (Kawamiya et al., 1995)
    real(8), parameter :: k_Rp(Nphy) = (/ 0.03d0/86400.0d0, 0.03d0/86400.0d0, 0.03d0/86400.0d0 /)    ! (s-1)
!  Temperature coefficient for PHY respiration rate (0.03d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Rp(Nphy) = (/ 0.0519d0, 0.0519d0, 0.0519d0 /) ! (degC-1)
!  Mortality rate at 0 oC (0.00562d0 umol-1 d-1; 0.0585L/umolN/day? 0.0088; Kishi et al., 2001)0.0066     
    real(8), parameter :: k_Mp(Nphy) = (/ 0.00562d0/86400.0d0, 0.00562d0/86400.0d0, 0.00562d0/86400.0d0 /) ! (L umol-1 s-1)
!  Temperature coefficient for PHY mortality (Kawamiya et al., 1995)
    real(8), parameter :: b_Mp(Nphy) = (/ 0.069d0, 0.069d0, 0.069d0 /) ! (degC-1)   
!  PHY ratio of extracellular excretion to production 0.135(Kawamiya et al., 1995)    
    real(8), parameter :: k_Ep(Nphy) = (/ 0.135d0, 0.135d0, 0.135d0 /) ! (no dim.)      
#if defined NUTRIENTS
! PHY1 Nutrient Assimilation Parameter         
    real(8), parameter :: Kp_NH4(Nphy) = (/ 0.2d0,  0.2d0,  0.2d0  /) ! (s-1)1.5         
    real(8), parameter :: Kp_NO3(Nphy) = (/ 0.1d0,  0.1d0,  0.1d0  /) ! (s-1)0.1         
    real(8), parameter :: Kp_PO4(Nphy) = (/ 0.01d0, 0.01d0, 0.01d0 /)  ! (s-1)0.01 
    ! (1.5L umolN-1; Kishi et al., 2007)  0.01  0.226  0.5
    real(8), parameter :: psi(Nphy) =  (/ 0.01d0, 0.01d0, 0.01d0 /)    !(L umolC-1)   
#endif
!------- Zooplankton parameters ------------------------
    real(8), parameter :: k_Gp2z(Nphy,Nzoo) = 0.30d0/86400.0d0 ! (s-1)          Maximum grazing rate of PHY1 by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)0.16
    real(8), parameter :: b_Gp2z(Nphy,Nzoo) = 0.0693d0         ! (degC-1)       Temperature coefficient for ZOO grazing (0.063d0degC-1;Kawamiya et al., 1995)
    real(8), parameter :: e_Gp2z(Nphy,Nzoo) = 0.10d0           ! (no dim.)      Assimilation efficiency of ZOO (0.7:Kawamiya et al., 1995)

    real(8), parameter :: k_Rz(Nzoo) = 0.005d0/86400.0d0     ! (s-1)          ZOO respiration rate at 0  oC  																					!!!(Tuning)
    real(8), parameter :: b_Rz(Nzoo) = 0.0693d0              ! (degC-1)       Temperature coefficient for ZOO respiration rate (Kawamiya et al., 1995)
    real(8), parameter :: k_Mz(Nzoo) = 0.0176d0/86400.0d0    ! (L umol-1 s-1)   ZOO mortality rate at 0 oC 3.0d0/86400.0d0(0.0088d0 umol-1 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Mz(Nzoo) = 0.0693d0              ! (degC-1)       Temperature coefficient for ZOO mortality (Kawamiya et al., 1995)
    real(8), parameter :: lam(Nzoo) = 0.211d0                 ! ((umol C L-1)-1)       zooplankton Ivlev constant for PHY1 (1.4L/umolN.0.211; Kishi et al., 2007)
    real(8), parameter :: t_Gp2z(Nphy,Nzoo) = 0.265d0          ! (umol C L-1)       PHY1 threshold value for grazing by ZOO (0.04umolN/L0.265; Kishi et al., 2007)
!------- Microbial loop parameters  -------
    real(8), parameter :: k_Gdoc2z(Ndom,Nzoo) = 0.0d0          ! (s-1)          Maximum grazing rate of DOC by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Gdoc2z(Ndom,Nzoo) = 0.0d0          ! (degC-1)       Temperature coefficient of DOC grazing by ZOO (Kawamiya et al., 1995)
    real(8), parameter :: k_Gpoc2z(Ndom,Nzoo) = 0.0d0          ! (s-1)          Maximum grazing rate of POC by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Gpoc2z(Ndom,Nzoo) = 0.0d0          ! (degC-1)       Temperature coefficient of DOC grazing by ZOO (Kawamiya et al., 1995)
    real(8), parameter :: e_Gdoc2z(Ndom,Nzoo) = 0.010d0  
    real(8), parameter :: e_Gpoc2z(Npom,Nzoo) = 0.010d0           ! (no dim.)      Assimilation efficiency of ZOO (0.7:Kawamiya et al., 1995)
!------- Decomposition parameters --------------------
    real(8), parameter :: k_Ddoc(Ndom) = 0.3d0/86400.0d0    ! (s-1)          Decomposition rate of DOC at 0 oC (0.3d0 d-1; Kishi et al., 2001)
    real(8), parameter :: b_Ddoc(Ndom) = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of DOC (Kishi et al., 2001)
    real(8), parameter :: k_Dpoc(Npom) = 0.3d0/86400.0d0    ! (s-1)          Decomposition rate of POC at 0 oC (0.3d0 d-1; Kishi et al., 2001)
    real(8), parameter :: b_Dpoc(Npom) = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of POC (Kishi et al., 2001)
    real(8), parameter :: k_Ddon(Ndom) = 0.02d0/86400.0d0   ! (s-1)          Decomposition rate of DON at 0 oC (0.02d0 d-1; Kishi et al., 2007)
    real(8), parameter :: b_Ddon(Ndom) = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of DON (Kishi et al., 2007)
    real(8), parameter :: k_Dpon(Npom) = 0.1d0/86400.0d0    ! (s-1)          Decomposition rate of PON at 0 oC (0.1d0 d-1; Kishi et al., 2007)
    real(8), parameter :: b_Dpon(Npom) = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of POC (Kishi et al., 2007)
    real(8), parameter :: k_Ddop(Ndom) = 0.02d0/86400.0d0   ! (s-1)          Decomposition rate of DOP at 0 oC 
    real(8), parameter :: b_Ddop(Ndom) = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of DOP 
    real(8), parameter :: k_Dpop(Npom) = 0.1d0/86400.0d0    ! (s-1)          Decomposition rate of POP at 0 oC 
    real(8), parameter :: b_Dpop(Npom) = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of POP 
!------- Physical parameters --------------------
#if defined NUTRIENTS
    real(8), parameter :: k_Nit = 0.03/86400.0d0   	 ! (s-1)          Nitrification rate at 0 oC (0.03d0 d-1; Kishi et al., 2007)
    real(8), parameter :: b_Nit = 0.0693d0           ! (degC-1)       Temperature coefficient for nitrification (Kishi et al., 2007)
#endif
!------- Physical parameters --------------------
#if defined NUTRIENTS   

! Redfield Ratio (1934) C:N:P -> C:N=6.6  C:P=106    N:P=16
! Martiny et al  (2014) C:N:P -> C:N=7.4  C:P=163    N:P=22
! GLODAP Data           C:N:P -> C:N=9.2  C:P=131.9  N:P=14.4

    real(8), parameter :: rCNp(Nphy) = (/ 9.2d0,   9.2d0,   9.2d0   /)    ! (no dim.) PHY1 C:N ratio
    real(8), parameter :: rCPp(Nphy) = (/ 131.9d0, 131.9d0, 131.9d0 /)    ! (no dim.) PHY1 C:P ratio
    real(8), parameter :: rCNz(Nzoo) = (/ 9.2d0   /)    ! (no dim.) ZOO  C:N ratio
    real(8), parameter :: rCPz(Nzoo) = (/ 131.9d0 /)    ! (no dim.) ZOO  C:P ratio
    real(8), parameter :: rNH4(Nphy) = 0.3d0            ! (no dim.)  PHY NH4:Nitrogen ratio 
#endif                                
    real(8), parameter :: rCCp(Nphy) = (/ 0.0d0, 0.0d0, 0.5d0 /)  ! ***** (no dim.) CaCO3:OrgC ratio

!------- Local variables --------------------
    real(8) :: Pp(Nphy), Rp(Nphy), Mp(Nphy), Ep(Nphy), Ap(Nphy)
    real(8) :: Gp2z(Nphy,Nzoo), Rz(Nzoo), Mz(Nzoo)
    real(8) :: Gdoc2z(Ndom,Nzoo), Gpoc2z(Npom,Nzoo)
    real(8) :: Ddoc(Ndom), Dpoc(Npom), Dpoc2doc(Npom,Ndom)
#if defined NUTRIENTS
    real(8) :: Gdon2z(Ndom,Nzoo), Gpon2z(Npom,Nzoo)
    real(8) :: Gdop2z(Ndom,Nzoo), Gpop2z(Npom,Nzoo)
    real(8) :: Ddon(Ndom), Dpon(Npom), Dpon2don(Npom,Ndom)
    real(8) :: Ddop(Ndom), Dpop(Npom), Dpop2dop(Npom,Ndom)
    real(8) :: Vp_NH4(Nphy), Vp_NO3(Nphy)
    real(8) :: Vp_PO4(Nphy)
    real(8) :: Nit
#endif
    
    integer :: k,m,p

    real(8) :: Gp2z_sum
    real(8) :: Ap2z_sum, Adoc2z_sum, Apoc2z_sum
    real(8) :: Gdoc2z_sum, Gpoc2z_sum


!===== Initiallize tracer parameters =================================
    dDIC_dt = 0.0d0
    dTA_dt  = 0.0d0
    dDOx_dt = 0.0d0
    dPOC_dt(:) = 0.0d0
    dDOC_dt(:) = 0.0d0
    dPHY_dt(:) = 0.0d0
    dZOO_dt(:) = 0.0d0
    dPIC_dt(:) = 0.0d0
#if defined CARBON_ISOTOPE
    dDI13C_dt = 0.0d0
    dDO13C_dt(:)  = 0.0d0
    dPO13C_dt(:)  = 0.0d0
    dPHY13C_dt(:) = 0.0d0
    dZOO13C_dt(:) = 0.0d0
    dPI13C_dt(:)  = 0.0d0
#endif
#if defined NUTRIENTS
    dNO3_dt = 0.0d0
    dNO2_dt = 0.0d0
    dNH4_dt = 0.0d0
    dPO4_dt = 0.0d0
    dDON_dt(:) = 0.0d0
    dPON_dt(:) = 0.0d0
    dDOP_dt(:) = 0.0d0
    dPOP_dt(:) = 0.0d0
# if defined NITROGEN_ISOTOPE
    dNO3_15N_dt = 0.0d0
    dNO2_15N_dt = 0.0d0
    dNH4_15N_dt = 0.0d0
    dDO15N_dt(:)  = 0.0d0
    dPO15N_dt(:)  = 0.0d0
    dPHY15N_dt(:) = 0.0d0
    dZOO15N_dt(:) = 0.0d0
# endif
#endif                                
#if defined COT_STARFISH
    dCOTe_dt = 0.0d0
    dCOTl_dt = 0.0d0
#endif

!===== Phytoplankton reaction =================================
    DO k=1, Nphy
!----- Gross photosynthetic rate (umolC L-1 s-1) -----------------
!      Pphy = k_Pphy * exp(b_Pphy*Tmp) * PFD/Iphy*exp(1.0d0-PFD/Iphy) * PHY
      Pp(k) = k_Pp(k) * exp(b_Pp(k)*Tmp) * tanh(PFD/Ip(k)) * PHY(k)
      IF(DIC <= 0.d0) THEN !-----For Error handling
        Pp(k) = 0.d0
      ENDIF

!----- Assimilation rate (umolC L-1 s-1) -----------------
#if defined NUTRIENTS         
      IF(k == iDnfl) THEN
!        Vphy1_NH4 = NH4/(NH4+Kphy1_NH4)
!        Vphy1_NO3 = NO3/(NO3+Kphy1_NO3) * exp(-psi1 * NH4)
        Vp_NH4(k) = NH4/(NH4+Kp_NH4(k)) * rNH4(k)
        Vp_NO3(k) = NO3/(NO3+Kp_NO3(k)) * exp(-psi(k) * NH4) *(1.0d0 - rNH4(k))
        
        Vp_PO4(k) = PO4/(PO4+Kp_PO4(k))
      ELSE IF(k == iDtm) THEN   
!        Vphy2_NH4 = NH4 * NH4/(NH4 * NH4+Kphy2_NH4 * Kphy2_NH4) * rNH4												!
!        Vphy2_NO3 = NO3 * NO3/(NO3 * NO3+Kphy2_NO3 * Kphy2_NO3) * exp(-psi2 * NH4)	*(1.0d0 - rNH4)			!
        Vp_NH4(k) = NH4 * NH4/(NH4 * NH4+Kp_NH4(k) * Kp_NH4(k))													!
        Vp_NO3(k) = NO3 * NO3/(NO3 * NO3+Kp_NO3(k) * Kp_NO3(k)) * exp(-psi(k) * NH4)				!
        
        Vp_PO4(k) = PO4 * PO4/(PO4 * PO4+Kp_PO4(k) * Kp_PO4(k))
      ELSE
        Vp_NH4(k) = NH4/(NH4+Kp_NH4(k)) * rNH4(k)
        Vp_NO3(k) = NO3/(NO3+Kp_NO3(k)) * exp(-psi(k) * NH4) *(1.0d0 - rNH4(k))      
        Vp_PO4(k) = PO4/(PO4+Kp_PO4(k))        
      END IF
      
      Ap(k) = MIN( Vp_NH4(k)+Vp_NO3(k), Vp_PO4(k) ) ! Assimilation rate
!      Aphy1 = Vphy1_NH4+Vphy1_NO3										! Assimilation rate
!      Aphy2 = Vphy2_NH4+Vphy2_NO3 									! Assimilation rate
  
      Ap(k) = Ap(k) * Pp(k)

#else
      Ap(k) = (1.0d0-k_Ep(k)) * Pp(k)

#endif

!----- Excretion rate (umolC L-1 s-1) -----------------
#if defined NUTRIENTS
      Ep(k) = Pp(k) - Ap(k)
#else
      Ep(k) = k_Ep(k) * Pp(k)
#endif

!----- Respiration rate (umolC L-1 s-1) -----------------
      Rp(k) = k_Rp(k) * exp(b_Rp(k)*Tmp) * PHY(k)
      IF(DOx <= 0.d0) THEN !-----For Error handling
        Rp(k) = 0.d0
      ENDIF

!----- Mortality (umolC L-1 s-1) -----------------
      Mp(k) = k_Mp(k) * exp(b_Mp(k)*Tmp) * PHY(k)*PHY(k)
!      Mphy = k_Mphy * exp(b_Mphy*Tmp) * PHY

!===== Mass barance =======

      dPHY_dt(k) = dPHY_dt(k) + Ap(k) - Rp(k) - Mp(k)
      dDIC_dt = dDIC_dt - Pp(k) + Rp(k)
      dDOx_dt = dDOx_dt + Pp(k) - Rp(k)  !!!!!!**************
      dDOC_dt(iLbl) = dDOC_dt(iLbl) + Ep(k)  !!!!!!**************
      dPOC_dt(iDtr) = dPOC_dt(iDtr) + Mp(k)  !!!!!!**************
      dPIC_dt(iCaCO3) = dPIC_dt(iCaCO3) + Mp(k)*rCCp(k)  !!!!!!**************
      dTA_dt = dTA_dt - Ap(k)*rCCp(k)*2.0d0
#if defined NUTRIENTS
      dNO3_dt = dNO3_dt + Ap(k)*Vp_NO3(k)*(Vp_NH4(k)+Vp_NO3(k))/rCNp(k)
      dNO2_dt = dNO2_dt + 0.0d0  !!!!!!**************
      dNH4_dt = dNH4_dt + Ap(k)*Vp_NH4(k)*(Vp_NH4(k)+Vp_NO3(k))/rCNp(k) + Rp(k)/rCNp(k)
      dPO4_dt = dPO4_dt + Ap(k)/rCPp(k) + Rp(k)/rCPp(k)
      dDON_dt(iLbl) = dDON_dt(iLbl) + Ep(k)/rCNp(k)  !!!!!!**************
      dPON_dt(iDtr) = dPON_dt(iDtr) + Mp(k)/rCNp(k)  !!!!!!**************
      dDOP_dt(iLbl) = dDOP_dt(iLbl) + Ep(k)/rCNp(k)  !!!!!!**************
      dPOP_dt(iDtr) = dPOP_dt(iDtr) + Mp(k)/rCNp(k)  !!!!!!**************
#endif
    END DO

!===== Zooplankton reaction ======================================
    DO k=1, Nzoo
!----- Grazing rate of PHY by ZOO (umolC L-1 s-1) -----------------
      DO m=1, Nphy
        Gp2z(m,k) = k_Gp2z(m,k) * exp(b_Gp2z(m,k)*Tmp)    &
          * (1-exp(lam(k)*(t_Gp2z(m,k) - PHY(m)))) * ZOO(k)			!!!!!!!!!!!!!!!!!!!
        IF(Gp2z(m,k) <= 0.d0) THEN !-----For Error handling
        	Gp2z(m,k) = 0
        ENDIF
!   ===== Mass barance =======

        dPHY_dt(m) = dPHY_dt(m) - Gp2z(m,k)
        dZOO_dt(k) = dZOO_dt(k) + Gp2z(m,k)*e_Gp2z(m,k)
        dPOC_dt(iDtr) = dPOC_dt(iDtr) + Gp2z(m,k)*(1.0d0-e_Gp2z(m,k))
#if defined NUTRIENTS
        dPON_dt(iDtr) = dPON_dt(iDtr) + Gp2z(m,k)*(1.0d0-e_Gp2z(m,k))/rCNz(k)
        dPOP_dt(iDtr) = dPOP_dt(iDtr) + Gp2z(m,k)*(1.0d0-e_Gp2z(m,k))/rCPz(k)
#endif
      END DO

!!!------- Microbial loop (inplicitly assumed) ---------------

!----- Grazing rate of DOC(iLbl) by ZOO (umolC L-1 s-1) -----------------
      DO m=1, Ndom
        Gdoc2z(m,k) = k_Gdoc2z(m,k) * exp(b_Gdoc2z(m,k)*Tmp) * DOC(m) * ZOO(k)

!   ===== Mass barance =======
        dZOO_dt(k) = dZOO_dt(k) + Gdoc2z(m,k)*e_Gdoc2z(m,k)
        dDOC_dt(m) = dDOC_dt(m) + Gdoc2z(m,k)*(1.0d0-e_Gdoc2z(m,k))
#if defined NUTRIENTS
        dDON_dt(m) = dDON_dt(m) + Gdoc2z(m,k)*(1.0d0-e_Gdoc2z(m,k))/rCNz(k)
        dDOP_dt(m) = dDOP_dt(m) + Gdoc2z(m,k)*(1.0d0-e_Gdoc2z(m,k))/rCPz(k)
#endif
      END DO

!----- Grazing rate of POC by ZOO (umolC L-1 s-1) -----------------
      DO m=1, Npom
        Gpoc2z(m,k) = k_Gdoc2z(m,k) * exp(b_Gpoc2z(m,k)*Tmp) * POC(m) * ZOO(k)

!   ===== Mass barance =======
        dZOO_dt(k) = dZOO_dt(k) + Gpoc2z(m,k)*e_Gpoc2z(m,k)
        dPOC_dt(m) = dPOC_dt(m) + Gpoc2z(m,k)*(1.0d0-e_Gpoc2z(m,k))
#if defined NUTRIENTS
        dPON_dt(m) = dPON_dt(m) + Gpoc2z(m,k)*(1.0d0-e_Gpoc2z(m,k))/rCNz(k)
        dPOP_dt(m) = dPOP_dt(m) + Gpoc2z(m,k)*(1.0d0-e_Gpoc2z(m,k))/rCPz(k)
#endif
      END DO
  
!----- Respiration rate (umolC L-1 s-1) -----------------
      Rz(k) = k_Rz(k) * exp(b_Rz(k)*Tmp) * ZOO(k)
      IF(DOx <= 0.d0) THEN !-----For Error handling
        Rz(k) = 0.d0
      ENDIF
  
!----- Mortality (umolC L-1 s-1) -----------------
      Mz(k) = k_Mz(k) * exp(b_Mz(k)*Tmp) * ZOO(k)*ZOO(k)
!      Mzoo = k_Mzoo * exp(b_Mzoo*Tmp) * ZOO
!      Mzoo = k_Mzoo * exp(b_Mzoo*Tmp) * (Gp2z(iDnfl,iGzoo) + Gphy22zoo)

!===== Mass barance =======

      dZOO_dt(k) = dZOO_dt(k) - Rz(k) - Mz(k)
      dDIC_dt = dDIC_dt + Rz(k)
      dDOx_dt = dDOx_dt - Rz(k)  !!!!!!**************
!      dDOC_dt(iLbl) = dDOC_dt(iLbl)  !!!!!!**************
      dPOC_dt(iDtr) = dPOC_dt(iDtr) + Mz(k)  !!!!!!**************
!      dPIC_dt(iCaCO3) = dPIC_dt(iCaCO3) + Mp(k)*rCCp(k)  !!!!!!**************
!      dTA_dt = dTA_dt(k) 
#if defined NUTRIENTS
      dNH4_dt = dNH4_dt + Rz(k)/rCNp(k)
      dPO4_dt = dPO4_dt + Rz(k)/rCPp(k)
#endif
    END DO

!===== Decomposition ==================================================

!----- Decomposition rate of DOM (umol L-1 s-1) -----------------
!    Ddoc = k_Ddoc * exp(b_Ddoc*Tmp) * DOC
    DO k=1, Ndom
      Ddoc(k) = k_Ddoc(k) * exp(b_Ddoc(k)*Tmp) * DOC(k)**2.0d0/(60.0d0**2.0d0+DOC(k)**2.0d0)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Ddoc(k) = 0.0d0
      ENDIF

      !===== Mass barance =======
      dDOC_dt(k) = dDOC_dt(k) - Ddoc(k)
      dDIC_dt = dDIC_dt + Ddoc(k)
      dDOx_dt = dDOx_dt - Ddoc(k)  !!!!!!**************

#if defined NUTRIENTS
!      Ddon = k_Ddon * exp(b_Ddon*Tmp) * DON
      Ddon(k) = k_Ddon(k) * exp(b_Ddon(k)*Tmp) * DON(k)**2.0d0/(60.0d0**2.0d0+DON(k)**2.0d0)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Ddon(k) = 0.0d0
      ENDIF
!      Ddop = k_Ddop * exp(b_Ddop*Tmp) * DOP
      Ddop(k) = k_Ddop(k) * exp(b_Ddop(k)*Tmp) * DOP(k)**2.0d0/(60.0d0**2.0d0+DOP(k)**2.0d0)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Ddop(k) = 0.0d0
      ENDIF

      !===== Mass barance =======

      dDON_dt(k) = dDON_dt(k) - Ddon(k)
      dDOP_dt(k) = dDOP_dt(k) - Ddop(k)
      dNH4_dt = dNH4_dt + Ddon(k)
      dPO4_dt = dPO4_dt + Ddop(k)

#endif
    END DO

!----- Decomposition rate of POM (umol L-1 s-1) -----------------
    DO k=1, Npom
      Dpoc = k_Dpoc * exp(b_Dpoc*Tmp) * POC(iDtr)**2.0d0/(60.0d0**2.0d0+POC(iDtr)**2.0d0)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Dpoc = 0.0d0
      ENDIF

      !===== Mass barance =======

      dPOC_dt(k) = dPOC_dt(k) - Dpoc(k)
      dDIC_dt = dDIC_dt + Dpoc(k)
      dDOx_dt = dDOx_dt - Dpoc(k)  !!!!!!**************

#if defined NUTRIENTS
      Dpon = k_Dpon * exp(b_Dpon*Tmp) * PON(iDtr)**2.0d0/(60.0d0**2.0d0+PON(iDtr)**2.0d0)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Dpon = 0.0d0
      ENDIF
      Dpop = k_Dpop * exp(b_Dpop*Tmp) * POP(iDtr)**2.0d0/(60.0d0**2.0d0+POP(iDtr)**2.0d0)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Dpop = 0.0d0
      ENDIF

      !===== Mass barance =======

      dPON_dt(k) = dPON_dt(k) - Dpon(k)
      dPOP_dt(k) = dPOP_dt(k) - Dpop(k)
      dNH4_dt = dNH4_dt + Dpon(k)
      dPO4_dt = dPO4_dt + Dpop(k)

#endif
    END DO
!----- Decomposition rate from POM to DOM (umol L-1 s-1) -----------------
    DO k=1, Ndom
      DO m=1, Npom
        Dpoc2doc(m,k) = 0.0d0 !!!

        !===== Mass barance =======

        dDOC_dt(k) = dDOC_dt(k) + Dpoc2doc(m,k)
        dPOC_dt(k) = dPOC_dt(k) - Dpoc2doc(m,k)
         
#if defined NUTRIENTS
        Dpon2don(m,k) = 0.0d0 !!!
        Dpop2dop(m,k) = 0.0d0 !!!

        !===== Mass barance =======

        dDON_dt(k) = dDON_dt(k) + Dpon2don(m,k)
        dPON_dt(k) = dPON_dt(k) - Dpon2don(m,k)
        dDOP_dt(k) = dDOP_dt(k) + Dpop2dop(m,k)
        dPOP_dt(k) = dPOP_dt(k) - Dpop2dop(m,k)

#endif
      END DO
    END DO

!===== Nitrification ====================================================
#if defined NUTRIENTS
    Nit = k_Nit * exp(b_Nit*Tmp) * NH4 ** 2.0d0

    !===== Mass barance =======

    dNO3_dt = dNO3_dt + Nit
    dNH4_dt = dNH4_dt - Nit

#endif

!===== Unit adjustment ===========================================
    dDIC_dt = dDIC_dt/rho_sw !(umol L-1 s-1) -> (umol kg-1 s-1)
    dTA_dt  = dTA_dt/rho_sw !(umol L-1 s-1) -> (umol kg-1 s-1)

      
    RETURN

  END SUBROUTINE foodweb

END MODULE mod_foodweb


