
!!!=== Copyright (c) 2012-2024 Takashi NAKAMURA  =====
!!!      modified by Shinya AMANO, Faisal AMRI

#include "cppdefs.h"


!!!**** MODULE OF FOOD WEB *******************************************

MODULE mod_foodweb
#if defined FOODWEB

  implicit none

  integer, parameter :: Ndom = 2     ! Number of Dissolved Organic Matter types
  integer, parameter :: iLbl = 1     !  1: Labile DOM
  integer, parameter :: iRfr = 2     !  2: Refractory DOM

  integer, parameter :: Npom  = 2    ! Number of Particulate Organic Matter types
  integer, parameter :: iDtr  = 1    !  1: Detritus
  integer, parameter :: iCPOM = 2    !  2: Coarse POM (leaf litter, etc.)

  integer, parameter :: Nphy  = 3    ! Number of Phytoplankton types
  integer, parameter :: iDnfl = 1    !  1: Dinoflagellate
  integer, parameter :: iDtm  = 2    !  2: Diatom
  integer, parameter :: iCcl  = 3    !  3: Coccolithophorids
   
  integer, parameter :: Nzoo = 1     ! Number of Zooplankton types
  integer, parameter :: iGzoo = 1    !  1: General Zooplankton

  integer, parameter :: Npim   = 1   ! Number of Particulate Inorganic Matter types
  integer, parameter :: iCaCO3 = 1   !  1: Coccolith (CaCO3)

!---- POM sinking parameters
  real(8), parameter :: rho_POM(Npom) = [ 1.1d0,  0.99d0 ]  ! ***** POM density [g/cm3] 
  real(8), parameter :: D_POM(Npom)   = [ 1.0d-1, 10.0d0 ]  ! ***** POM diameter [mm] 

!---- PIM sinking parameters
  real(8), parameter :: rho_PIM(Npim) = [ 1.4d0  ]  ! ***** PIM density [g/cm3] 
  real(8), parameter :: D_PIM(Npim)   = [ 1.0d-2 ]  ! ***** PIM diameter [mm] 

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
#if defined FOODWEB_TESTMODE
    , DeadPHY        &   ! DeadPHY(Nphy): Dead phytoplankton (umol C L-1)
#endif
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
!    , NO2            &   ! NO2 (umol L-1)
    , NH4            &   ! NH4 (umol L-1)
    , PO4            &   ! PO4 (umol L-1)
    , DON            &   ! Dissolved organic nitrogen (DON: umol L-1)
    , PON            &   ! Particulate organic nitrogen (PON: umol L-1)
    , DOP            &   ! Dissolved organic phosporius (DOP: umol L-1)
    , POP            &   ! Particulate organic phosporius (POP: umol L-1)
# if defined NITROGEN_ISOTOPE
    , NO3_15N        &   ! 15N of NO3 (umol L-1)
!    , NO2_15N        &   ! 15N of NO2 (umol L-1)
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
#if defined FOODWEB_TESTMODE
    , dDeadPHY_dt    &   ! dDeadPHY/dt  (umol L-1 s-1)
#endif
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
!    , dNO2_dt        &   ! dNO2/dt (umol L-1 s-1)
    , dNH4_dt        &   ! dNH4/dt (umol L-1 s-1)
    , dPO4_dt        &   ! dPO4/dt (umol L-1 s-1)
    , dDON_dt        &   ! dDON/dt (umol L-1 s-1)
    , dPON_dt        &   ! dPON/dt (umol L-1 s-1)
    , dDOP_dt        &   ! dDOP/dt (umol L-1 s-1)
    , dPOP_dt        &   ! dPOP/dt (umol L-1 s-1)
# if defined NITROGEN_ISOTOPE
    , dNO3_15N_dt    &   ! dNO3_15N/dt (umol L-1 s-1)
!    , dNO2_15N_dt    &   ! dNO2_15N/dt (umol L-1 s-1)
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
    , pH             &   ! pH
    , Wcal           &   ! Calcite saturation state
    , Warg           &   ! Aragonite saturation state
    )

!-----------------------------------------------------------------------
!
    USE mod_geochem

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
#if defined FOODWEB_TESTMODE
    real(8), intent(in) :: DeadPHY(Nphy)  ! DeadPHY(Nphy): Dead phytoplankton (umol C L-1)
#endif
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
!    real(8), intent(in) :: NO2           ! NO2 (umol L-1)
    real(8), intent(in) :: NH4            ! NH4 (umol L-1)
    real(8), intent(in) :: PO4            ! PO4 (umol L-1)
    real(8), intent(in) :: DON(Ndom)      ! Labile Dissolved organic nitrogen (DON: umol L-1)
    real(8), intent(in) :: PON(Npom)      ! Particulate organic nitrogen (PON: umol L-1)
    real(8), intent(in) :: DOP(Ndom)      ! Labile Dissolved organic phosporius (DOP: umol L-1)
    real(8), intent(in) :: POP(Npom)      ! Particulate organic phosporius (POP: umol L-1)
# if defined NITROGEN_ISOTOPE
    real(8), intent(in) :: NO3_15N        ! 15N of NO3 (umol L-1)
!    real(8), intent(in) :: NO2_15N       ! 15N of NO2 (umol L-1)
    real(8), intent(in) :: NH4_15N        ! 15N of NH4 (umol L-1)
    real(8), intent(in) :: DO15N(Ndom)    ! 15N of Labile Dissolved organic nitrogen (DON: umol L-1)
    real(8), intent(in) :: PO15N(Npom)    ! 15N of Particulate organic nitrogen (PON: umol L-1)
    real(8), intent(in) :: PHY15N(Nphy)   ! 15N of phytoplankton1 (umol C L-1), dinoflagellate
    real(8), intent(in) :: ZOO15N(Nzoo)   ! 15N of zooplankton (umol C L-1)
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
    real(8), intent(out) :: dDOC_dt(Ndom) ! dDOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPOC_dt(Npom) ! dPOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPHY_dt(Nphy) ! dPHY1/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dZOO_dt(Nzoo) ! dZOO/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dPIC_dt(Npim) ! dPIC/dt  (umol L-1 s-1) 
#if defined FOODWEB_TESTMODE
    real(8), intent(out) :: dDeadPHY_dt(Nphy)  ! dDeadPHY/dt  (umol L-1 s-1)
#endif
#if defined CARBON_ISOTOPE
    real(8), intent(out) :: dDI13C_dt        ! dDI13C/dt (umol kg-1 s-1)
    real(8), intent(out) :: dDO13C_dt(Ndom)  ! dDO13C/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPO13C_dt(Npom)  ! dPO13C/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPHY13C_dt(Nphy) ! dPHY13C/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dZOO13C_dt(Nzoo) ! dZOO13C/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dPI13C_dt(Npim)  ! dPI13C/dt  (umol L-1 s-1)  
#endif
#if defined NUTRIENTS
    real(8), intent(out) :: dNO3_dt       ! dNO3/dt (umol L-1 s-1)
!    real(8), intent(out) :: dNO2_dt      ! dNO2/dt (umol L-1 s-1)
    real(8), intent(out) :: dNH4_dt       ! dNH4/dt (umol L-1 s-1)
    real(8), intent(out) :: dPO4_dt       ! dPO4/dt (umol L-1 s-1)
    real(8), intent(out) :: dDON_dt(Ndom) ! dDON/dt (umol L-1 s-1)
    real(8), intent(out) :: dPON_dt(Npom) ! dPON/dt (umol L-1 s-1)
    real(8), intent(out) :: dDOP_dt(Ndom) ! dDOP/dt (umol L-1 s-1)
    real(8), intent(out) :: dPOP_dt(Npom) ! dPOP/dt (umol L-1 s-1)
# if defined NITROGEN_ISOTOPE
    real(8), intent(out) :: dNO3_15N_dt      ! dNO3_15N/dt (umol L-1 s-1)
!    real(8), intent(out) :: dNO2_15N_dt      ! dNO2_15N/dt (umol L-1 s-1)
    real(8), intent(out) :: dNH4_15N_dt      ! dNH4_15N/dt (umol L-1 s-1)
    real(8), intent(out) :: dDO15N_dt(Ndom)  ! dDO15N/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPO15N_dt(Npom)  ! dPO15N/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPHY15N_dt(Nphy) ! dPHY15N/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dZOO15N_dt(Nzoo) ! dZOO15N/dt  (umol L-1 s-1)  
# endif
#endif                                
#if defined COT_STARFISH
    real(8), intent(out) :: dCOTe_dt      ! dCOTe/dt (umol L-1 s-1)
    real(8), intent(out) :: dCOTl_dt      ! dCOTl/dt (umol L-1 s-1)
#endif
    real(8), intent(out) :: pH             ! pH
    real(8), intent(out) :: Wcal           ! Calcite saturation state
    real(8), intent(out) :: Warg           ! Aragonite saturation state

!!!------------Set parameters  ----------------------------------

!------- Phytoplankton parameters ------------------------
!  Maximum photosynthetic rate at 0 oC ( s-1) (0.4d0 d-1; Kishi et al., 2007)0.3
    real(8), parameter :: k_Pp(Nphy) = [ 3.5d-6,   1.0d-5,   3.5d-6   ]  ! (s-1)
!  Temperature coefficient for PHY photosynthesis (0.063; Kawamiya et al., 1995)          
    real(8), parameter :: b_Pp(Nphy) = [ 0.0693d0, 0.0693d0, 0.0693d0 ]  ! (degC-1) 
!  Optimum light intensity (48.83d0 J m2 s-1; Kawamiya et al., 1995)     
    real(8), parameter :: Ip(Nphy)   = [ 90.0d0,   90.0d0,   90.0d0   ] ! (umol m-2 s-1) 
!  Respiration rate at 0 oC (Kawamiya et al., 1995)
    real(8), parameter :: k_Rp(Nphy) = [ 3.5d-7,   3.5d-7,   3.5d-7   ]    ! (s-1)
!  Temperature coefficient for PHY respiration rate (0.03d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Rp(Nphy) = [ 0.0519d0, 0.0519d0, 0.0519d0 ] ! (degC-1)
!  Mortality rate at 0 oC (0.00562d0 umol-1 d-1; 0.0585L/umolN/day? 0.0088; Kishi et al., 2001)0.0066     
    real(8), parameter :: k_Mp(Nphy) = [ 6.5d-7,   3.3d-7,   6.5d-7   ] ! (L umol-1 s-1)
!  Temperature coefficient for PHY mortality (Kawamiya et al., 1995)
    real(8), parameter :: b_Mp(Nphy) = [ 0.069d0,  0.069d0,  0.069d0  ] ! (degC-1)   
!  PHY ratio of extracellular excretion to production 0.135(Kawamiya et al., 1995)    
    real(8), parameter :: k_Ep(Nphy) = [ 0.135d0,  0.135d0,  0.135d0  ] ! (no dim.) 
    real(8), parameter :: rLDOMphy = 0.99d0  ! (Labile DOM)/(Total DOM) of phytoplankton excreted DOM (no dim.) 
#if defined NUTRIENTS
!  PHY Nutrient Assimilation Parameter         
    real(8), parameter :: Kp_NH4(Nphy) = [ 1.5d0,  0.5d0,  1.5d0  ] ! (umol L-1)1.5         
    real(8), parameter :: Kp_NO3(Nphy) = [ 0.1d0,  1.0d0,  0.1d0  ] ! (umol L-1)0.1         
    real(8), parameter :: Kp_PO4(Nphy) = [ 0.01d0, 0.03d0, 0.01d0 ] ! (umol L-1)0.01 
    ! (1.5L umolN-1; Kishi et al., 2007)  0.01  0.226  0.5
    real(8), parameter :: psi(Nphy) =    [ 0.5d0,  0.5d0,  0.5d0  ] !(L umolC-1)   
#endif
!------- Zooplankton parameters ------------------------
    real(8), parameter :: k_Gp2z(Nphy,Nzoo) = reshape( [ 3.0d-6,    2.0d-6,    3.0d-7    ], [Nphy,Nzoo] ) ! (s-1)          Maximum grazing rate of PHY1 by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)0.16
    real(8), parameter :: b_Gp2z(Nphy,Nzoo) = reshape( [ 0.0693d0,  0.0693d0,  0.0693d0  ], [Nphy,Nzoo] ) ! (degC-1)       Temperature coefficient for ZOO grazing (0.063d0degC-1;Kawamiya et al., 1995)
    real(8), parameter :: e_Gp2z(Nphy,Nzoo) = reshape( [ 0.250d0,   0.250d0,   0.250d0   ], [Nphy,Nzoo] ) ! (no dim.)      Assimilation efficiency of ZOO (0.7:Kawamiya et al., 1995)

    real(8), parameter :: k_Rz(Nzoo) = [ 6.0d-8    ]  ! (s-1)            ZOO respiration rate at 0  oC  																					!!!(Tuning)
    real(8), parameter :: b_Rz(Nzoo) = [ 0.0693d0  ]  ! (degC-1)         Temperature coefficient for ZOO respiration rate (Kawamiya et al., 1995)
    real(8), parameter :: k_Mz(Nzoo) = [ 1.0d-8    ]  ! (L umol-1 s-1)   ZOO mortality rate at 0 oC 3.0d0/86400.0d0(0.0088d0 umol-1 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Mz(Nzoo) = [ 0.0693d0  ]  ! (degC-1)         Temperature coefficient for ZOO mortality (Kawamiya et al., 1995)
    real(8), parameter :: lam(Nzoo)  = [ 0.211d0   ]  ! ((umol C L-1)-1) zooplankton Ivlev constant for PHY1 (1.4L/umolN.0.211; Kishi et al., 2007)
    real(8), parameter :: t_Gp2z(Nphy,Nzoo) = reshape( [ 0.265d0,   0.265d0,   0.265d0   ], [Nphy,Nzoo] ) ! (umol C L-1)       PHY1 threshold value for grazing by ZOO (0.04umolN/L0.265; Kishi et al., 2007)
!------- Microbial loop parameters  -------
    real(8), parameter :: k_Gdoc2z(Ndom,Nzoo) = reshape( [ 1.0d-8,    0.0d0    ], [Ndom,Nzoo] ) ! (s-1)          Maximum grazing rate of DOC by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Gdoc2z(Ndom,Nzoo) = reshape( [ 0.0693d0,  0.0693d0 ], [Ndom,Nzoo] ) ! (degC-1)       Temperature coefficient of DOC grazing by ZOO (Kawamiya et al., 1995)

    real(8), parameter :: k_Gpoc2z(Npom,Nzoo) = reshape( [ 1.0d-7,    1.0d-7   ], [Ndom,Nzoo] ) ! (s-1)          Maximum grazing rate of POC by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Gpoc2z(Npom,Nzoo) = reshape( [ 0.0693d0,  0.0693d0 ], [Ndom,Nzoo] ) ! (degC-1)       Temperature coefficient of DOC grazing by ZOO (Kawamiya et al., 1995)

    real(8), parameter :: e_Gdoc2z(Ndom,Nzoo) = reshape( [ 0.010d0,   0.010d0  ], [Ndom,Nzoo] )   
    real(8), parameter :: e_Gpoc2z(Npom,Nzoo) = reshape( [ 0.10d0,    0.10d0   ], [Ndom,Nzoo] ) ! (no dim.)      Assimilation efficiency of ZOO (0.7:Kawamiya et al., 1995)
!------- Decomposition parameters --------------------
    real(8), parameter :: k_Ddoc(Ndom) = [ 3.0d-7,   1.0d-11   ]    ! (s-1)    Decomposition rate of DOC at 0 oC (0.3d0 d-1; Kishi et al., 2001)
    real(8), parameter :: k_Ddon(Ndom) = [ 2.0d-7,   1.0d-11   ]    ! (s-1)    Decomposition rate of DON at 0 oC (0.02d0 d-1; Kishi et al., 2007)
    real(8), parameter :: k_Ddop(Ndom) = [ 2.0d-7,   1.0d-11   ]    ! (s-1)    Decomposition rate of DOP at 0 oC 
  
    real(8), parameter :: k_Dpoc(Npom) = [ 3.0d-7,   1.0d-10   ]    ! (s-1)    Decomposition rate of POC at 0 oC (0.3d0 d-1; Kishi et al., 2001)
    real(8), parameter :: k_Dpon(Npom) = [ 1.0d-7,   1.0d-10   ]    ! (s-1)    Decomposition rate of PON at 0 oC (0.1d0 d-1; Kishi et al., 2007)
    real(8), parameter :: k_Dpop(Npom) = [ 1.0d-7,   1.0d-10   ]    ! (s-1)    Decomposition rate of POP at 0 oC 

    real(8), parameter :: b_Ddoc(Ndom) = [ 0.0693d0, 0.0693d0 ]    ! (degC-1) Temperature coefficient for decomposition of DOC (Kishi et al., 2001)
    real(8), parameter :: b_Ddon(Ndom) = [ 0.0693d0, 0.0693d0 ]    ! (degC-1) Temperature coefficient for decomposition of DON (Kishi et al., 2007)
    real(8), parameter :: b_Ddop(Ndom) = [ 0.0693d0, 0.0693d0 ]    ! (degC-1) Temperature coefficient for decomposition of DOP 

    real(8), parameter :: b_Dpoc(Npom) = [ 0.0693d0, 0.0693d0 ]    ! (degC-1) Temperature coefficient for decomposition of POC (Kishi et al., 2001)
    real(8), parameter :: b_Dpon(Npom) = [ 0.0693d0, 0.0693d0 ]    ! (degC-1) Temperature coefficient for decomposition of POC (Kishi et al., 2007)
    real(8), parameter :: b_Dpop(Npom) = [ 0.0693d0, 0.0693d0 ]    ! (degC-1) Temperature coefficient for decomposition of POP 
!------- Decomposition parameters (POM -> DOM) --------------------
!                                                           Detr->LDOM, CPOM->LDOM   
!                                                           Detr->RDOM, CPOM->RDOM   
    real(8), parameter :: k_Dp2doc(Npom,Ndom) = reshape( [  1.0d-6,     1.0d-9        &
                                                          , 1.0d-9,     1.0d-11   ], [Npom,Ndom] )    ! (s-1)          Decomposition rate of POC at 0 oC (0.3d0 d-1; Kishi et al., 2001)
    real(8), parameter :: k_Dp2don(Npom,Ndom) = reshape( [  1.0d-6,     1.0d-9        &
                                                          , 1.0d-9,     1.0d-11   ], [Npom,Ndom] )    ! (s-1)          Decomposition rate of PON at 0 oC (0.02d0 d-1; Kishi et al., 2007)
    real(8), parameter :: k_Dp2dop(Npom,Ndom) = reshape( [  1.0d-6,     1.0d-9        &
                                                          , 1.0d-9,     1.0d-11   ], [Npom,Ndom] )    ! (s-1)          Decomposition rate of POP at 0 oC 
 
    real(8), parameter :: b_Dp2doc(Npom,Ndom) = reshape( [  0.0693d0,   0.0693d0     &
                                                          , 0.0693d0,   0.0693d0  ], [Npom,Ndom] )    ! (degC-1)       Temperature coefficient for decomposition of POC (Kishi et al., 2001)
    real(8), parameter :: b_Dp2don(Npom,Ndom) = reshape( [  0.0693d0,   0.0693d0     &
                                                          , 0.0693d0,   0.0693d0  ], [Npom,Ndom] )    ! (degC-1)       Temperature coefficient for decomposition of PON (Kishi et al., 2007)
    real(8), parameter :: b_Dp2dop(Npom,Ndom) = reshape( [  0.0693d0,   0.0693d0     &
                                                          , 0.0693d0,   0.0693d0  ], [Npom,Ndom] )    ! (degC-1)       Temperature coefficient for decomposition of POP 
!    real(8), parameter :: rLDOMpom = 0.95d0  ! (Labile DOM)/(Total DOM) of DOM decomposed from POM (no dim.) 
!------- Decomposition parameters (CPOM -> Detritus) --------------------
    real(8), parameter :: k_Dcpoc2dtr = 1.0d-9    ! (s-1)     Decomposition rate of CPOM to Detritus at 0 oC
    real(8), parameter :: b_Dcpoc2dtr = 0.0693d0  ! (degC-1)  Temperature coefficient for decomposition of CPOM to Detritus
    real(8), parameter :: k_Dcpon2dtr = 1.0d-9    ! (s-1)     Decomposition rate of CPOM to Detritus at 0 oC
    real(8), parameter :: b_Dcpon2dtr = 0.0693d0  ! (degC-1)  Temperature coefficient for decomposition of CPOM to Detritus
    real(8), parameter :: k_Dcpop2dtr = 1.0d-9    ! (s-1)     Decomposition rate of CPOM to Detritus at 0 oC
    real(8), parameter :: b_Dcpop2dtr = 0.0693d0  ! (degC-1)  Temperature coefficient for decomposition of CPOM to Detritus
!------- Physical parameters --------------------
#if defined NUTRIENTS
    real(8), parameter :: k_Nit = 3.0d-7     ! (s-1)      Nitrification rate at 0 oC (0.03d0 d-1; Kishi et al., 2007)
    real(8), parameter :: b_Nit = 0.0693d0   ! (degC-1)   Temperature coefficient for nitrification (Kishi et al., 2007)
#endif
!------- Physical parameters --------------------
#if defined NUTRIENTS   

! Redfield Ratio (1934) C:N:P -> C:N=6.6  C:P=106    N:P=16
! Martiny et al  (2014) C:N:P -> C:N=7.4  C:P=163    N:P=22
! GLODAP Data           C:N:P -> C:N=9.2  C:P=131.9  N:P=14.4

    real(8), parameter :: rCNp(Nphy) = [ 9.2d0,   9.2d0,   9.2d0   ]    ! (no dim.) PHY1 C:N ratio
    real(8), parameter :: rCPp(Nphy) = [ 131.9d0, 131.9d0, 131.9d0 ]    ! (no dim.) PHY1 C:P ratio
    real(8), parameter :: rCNz(Nzoo) = [ 9.2d0   ]    ! (no dim.) ZOO  C:N ratio
    real(8), parameter :: rCPz(Nzoo) = [ 131.9d0 ]    ! (no dim.) ZOO  C:P ratio
    real(8), parameter :: rNH4(Nphy) = 0.3d0            ! (no dim.)  PHY NH4:Nitrogen ratio 
#endif                                
    real(8), parameter :: rCCp(Nphy) = [ 0.0d0, 0.0d0, 0.5d0 ]  ! ***** (no dim.) CaCO3:OrgC ratio !!! Need Ref!!!!!!!!!!
#if defined CARBON_ISOTOPE
!----- for carbon isotope ------------------------
# if defined CARBON_TRACE
    real(8), parameter :: a_phot = 1.0d0
    real(8), parameter :: a_calc = 1.0d0
    real(8), parameter :: a_grz  = 1.0d0
# else
    real(8), parameter :: a_phot = -20.0d-3 + 1.0d0 !!!!! To Be Confirmed
    real(8), parameter :: a_calc =   2.7d-3 + 1.0d0 !!!!! To Be Confirmed
    real(8), parameter :: a_grz  =   1.0d-3 + 1.0d0 !!!!! To Be Confirmed
# endif
    real(8), parameter :: sn =   1.0d-20 ! A small number for avoiding a number divided by 0
#endif

!------- Local variables --------------------
    real(8) :: Pp(Nphy), Rp(Nphy), Mp(Nphy), Ep(Nphy), Ap(Nphy)
    real(8) :: Gp2z(Nphy,Nzoo), Rz(Nzoo), Mz(Nzoo)
    real(8) :: Gdoc2z(Ndom,Nzoo), Gpoc2z(Npom,Nzoo)
    real(8) :: Ddoc(Ndom), Dpoc(Npom), Dp2doc(Npom,Ndom)
    real(8) :: Dcpoc2dtr
    real(8) :: CO3, Dissol
#if defined CARBON_ISOTOPE
    real(8) :: rDI13C, rPO13C(Npom), rDO13C(Ndom)
    real(8) :: rPHY13C(Nphy), rZOO13C(Nzoo), rPI13C(Npim)
#endif
#if defined NUTRIENTS
    real(8) :: Ddon(Ndom), Dpon(Npom), Dp2don(Npom,Ndom)
    real(8) :: Ddop(Ndom), Dpop(Npom), Dp2dop(Npom,Ndom)
    real(8) :: Vp_NH4(Nphy), Vp_NO3(Nphy)
    real(8) :: Vp_PO4(Nphy)
    real(8) :: Dcpon2dtr, Dcpop2dtr
    real(8) :: Nit
#endif
    
    integer :: k,m
    real(8) :: cf1, cf2, cf3

!===== Initiallize tracer parameters =================================
    dDIC_dt = 0.0d0
    dTA_dt  = 0.0d0
    dDOx_dt = 0.0d0
    dPOC_dt(:) = 0.0d0
    dDOC_dt(:) = 0.0d0
    dPHY_dt(:) = 0.0d0
    dZOO_dt(:) = 0.0d0
    dPIC_dt(:) = 0.0d0
#if defined FOODWEB_TESTMODE
    dDeadPHY_dt(:) = 0.0d0
#endif
#if defined CARBON_ISOTOPE
    dDI13C_dt = 0.0d0
    dDO13C_dt(:)  = 0.0d0
    dPO13C_dt(:)  = 0.0d0
    dPHY13C_dt(:) = 0.0d0
    dZOO13C_dt(:) = 0.0d0
    dPI13C_dt(:)  = 0.0d0
    
    cf1 = MAX(DIC,0.0d0)
    cf2= MAX(DI13C,0.0d0)
    if(cf1<sn .or. cf2<sn ) then
      rDI13C = 0.0d0
    else
!      rDI13C = cf2/MAX(cf1-cf2,sn)
      rDI13C = MIN( cf2/MAX(cf1,sn), 1.0d0 )
    endif
    DO k=1, Ndom
      cf1= MAX(DOC(k),0.0d0)
      cf2= MAX(DO13C(k),0.0d0)
      if(cf1<sn .or. cf2<sn ) then
        rDO13C(k) = 0.0d0
      else
!        rDO13C(k) = cf2/MAX(cf1-cf2,sn)
        rDO13C(k) = MIN( cf2/MAX(cf1,sn), 1.0d0 )
      endif
    END DO
    DO k=1, Npom
      cf1= MAX(POC(k),0.0d0)
      cf2= MAX(PO13C(k),0.0d0)
      if(cf1<sn .or. cf2<sn ) then
        rPO13C(k) = 0.0d0
      else
!        rPO13C(k) = cf2/MAX(cf1-cf2,sn)
        rPO13C(k) = MIN( cf2/MAX(cf1,sn), 1.0d0 )
      endif
    END DO
    DO k=1, Nphy
      cf1= MAX(PHY(k),0.0d0)
      cf2= MAX(PHY13C(k),0.0d0)
      if(cf1<sn .or. cf2<sn ) then
        rPHY13C(k) = 0.0d0
      else
!        rPHY13C(k) = cf2/MAX(cf1-cf2,sn)
        rPHY13C(k) = MIN( cf2/MAX(cf1,sn), 1.0d0 )
      endif
    END DO
    DO k=1, Nzoo
      cf1= MAX(ZOO(k),0.0d0)
      cf2= MAX(ZOO13C(k),0.0d0)
      if(cf1<sn .or. cf2<sn ) then
        rZOO13C(k) = 0.0d0
      else
!        rZOO13C(k) = cf2/MAX(cf1-cf2,sn)
        rZOO13C(k) = MIN( cf2/MAX(cf1,sn), 1.0d0 )
      endif
    END DO
    DO k=1, Npim
      cf1= MAX(PIC(k),0.0d0)
      cf2= MAX(PI13C(k),0.0d0)
      if(cf1<sn .or. cf2<sn ) then
        rPI13C(k) = 0.0d0
      else
!        rPI13C(k) = cf2/MAX(cf1-cf2,sn)
        rPI13C(k) = MIN( cf2/MAX(cf1,sn), 1.0d0 )
      endif
    END DO
#endif
#if defined NUTRIENTS
    dNO3_dt = 0.0d0
!    dNO2_dt = 0.0d0
    dNH4_dt = 0.0d0
    dPO4_dt = 0.0d0
    dDON_dt(:) = 0.0d0
    dPON_dt(:) = 0.0d0
    dDOP_dt(:) = 0.0d0
    dPOP_dt(:) = 0.0d0
# if defined NITROGEN_ISOTOPE
    dNO3_15N_dt = 0.0d0
!    dNO2_15N_dt = 0.0d0
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
      Pp(k) = k_Pp(k) * exp(b_Pp(k)*Tmp) * tanh(PFD/Ip(k)) * MAX(PHY(k), 0.0d0)
      IF(DIC <= 0.0d0) THEN !-----For Error handling
        Pp(k) = 0.0d0
      ENDIF
      Pp(k) = MAX( Pp(k), 0.0d0 )  !!! Error handling

!----- Assimilation rate (umolC L-1 s-1) -----------------

#if defined NUTRIENTS
        cf1 = MAX(NH4, 0.0d0)  !!! Error handling
        cf2 = MAX(NO3, 0.0d0)  !!! Error handling
        cf3 = MAX(PO4, 0.0d0)  !!! Error handling

      IF(k == iDnfl) THEN
        Vp_NH4(k) = cf1/(cf1+Kp_NH4(k)) * rNH4(k)
        Vp_NO3(k) = cf2/(cf2+Kp_NO3(k)) * exp(-psi(k) * cf1) * (1.0d0 - rNH4(k))   
        Vp_PO4(k) = cf3/(cf3+Kp_PO4(k))
      ELSE IF(k == iDtm) THEN   
        Vp_NH4(k) = cf1*cf1/(cf1*cf1+Kp_NH4(k)*Kp_NH4(k)) * rNH4(k)
        Vp_NO3(k) = cf2*cf2/(cf2*cf2+Kp_NO3(k)*Kp_NO3(k)) * exp(-psi(k) * cf1) * (1.0d0 - rNH4(k))    
        Vp_PO4(k) = cf3*cf3/(cf3*cf3+Kp_PO4(k)*Kp_PO4(k))
      ELSE
        Vp_NH4(k) = cf1/(cf1+Kp_NH4(k)) * rNH4(k)
        Vp_NO3(k) = cf2/(cf2+Kp_NO3(k)) * exp(-psi(k) * cf1) * (1.0d0 - rNH4(k))     
        Vp_PO4(k) = cf3/(cf3+Kp_PO4(k))
      END IF
      Vp_NH4(k) = MAX( Vp_NH4(k), 0.0d0 )  !!! Error handling
      Vp_NO3(k) = MAX( Vp_NO3(k), 0.0d0 )  !!! Error handling
      Vp_PO4(k) = MAX( Vp_PO4(k), 0.0d0 )  !!! Error handling
      
      Ap(k) = MIN( Vp_NH4(k)+Vp_NO3(k), Vp_PO4(k) ) ! Assimilation rate

      Ap(k) = MIN( Ap(k), 1.0d0 )  !!! Error handling

      Ap(k) = Ap(k) * Pp(k)

#else
      Ap(k) = (1.0d0-k_Ep(k)) * Pp(k)

#endif
      Ap(k) = MIN( Ap(k), 0.1d0*DIC/dt ) !!! yt_edit Error handling  assimilation should not exceed 10% DIC concentration in a timestep
#if defined NUTRIENTS
      Ap(k) = MIN( Ap(k), 0.1d0*(NO3+NH4)*rCNp(k)/dt ) !!! yt_edit Error handling  assimilation should not exceed 10% N concentration in a timestep
      Ap(k) = MIN( Ap(k), 0.1d0*PO4*rCPp(k)/dt ) !!! yt_edit Error handling  assimilation should not exceed 10% PO4 concentration in a timestep
#endif

!     === Mass barance ===

      dPHY_dt(k) = dPHY_dt(k) + Ap(k)
      dTA_dt = dTA_dt - Ap(k)*rCCp(k)*2.0d0  ! calcification
      dDIC_dt = dDIC_dt - Ap(k) - Ap(k)*rCCp(k)      ! calcification
      dDOx_dt = dDOx_dt + Ap(k)  !!!!!!**************
#if defined CARBON_ISOTOPE
      dPHY13C_dt(k) = dPHY13C_dt(k) + Ap(k)*a_phot*rDI13C
      dDI13C_dt = dDI13C_dt - Ap(k)*a_phot*rDI13C - Ap(k)*rCCp(k)*a_calc*rDI13C
#endif
#if defined NUTRIENTS
      if(Vp_NO3(k)+Vp_NH4(k) /= 0.0d0) then ! yt_edit prevent div by 0
        dNO3_dt = dNO3_dt -         Ap(k)*Vp_NO3(k)/(Vp_NH4(k)+Vp_NO3(k))/rCNp(k)
        dDOx_dt = dDOx_dt + 2.0d0 * Ap(k)*Vp_NO3(k)/(Vp_NH4(k)+Vp_NO3(k))/rCNp(k)  !!!*********************
        dTA_dt  = dTA_dt  + 1.0d0 * Ap(k)*Vp_NO3(k)/(Vp_NH4(k)+Vp_NO3(k))/rCNp(k)  ! increase of alkalinity by 1 mole when nitrate or nitrite is the N source
        dNH4_dt = dNH4_dt -         Ap(k)*Vp_NH4(k)/(Vp_NH4(k)+Vp_NO3(k))/rCNp(k)
        dTA_dt  = dTA_dt  - 1.0d0 * Ap(k)*Vp_NH4(k)/(Vp_NH4(k)+Vp_NO3(k))/rCNp(k)  ! decrease of alkalinity by 1 mole when ammonia is used
      endif
      dPO4_dt   = dPO4_dt -         Ap(k)/rCPp(k)
      dTA_dt    = dTA_dt  + 2.0d0 * Ap(k)/rCPp(k)  ! increases alkalinity by 2 mole per mole HPO42-
#endif

!----- Excretion rate (umolC L-1 s-1) -----------------

#if defined NUTRIENTS
      Ep(k) = (Pp(k) - Ap(k))*k_Ep(k) 
#else
      Ep(k) = k_Ep(k) * Pp(k)
#endif
      Ep(k) = MIN( Ep(k), 0.1d0*DIC/dt ) !!! yt_edit Error handling  excretion should not exceed 10% DIC concentration in a timestep


!     === Mass barance ===

      dDIC_dt = dDIC_dt - Ep(k)
      dDOx_dt = dDOx_dt + Ep(k)  !!!!!!**************
#if defined CARBON_ISOTOPE
      dDI13C_dt = dDI13C_dt - Ep(k)*a_phot*rDI13C
#endif
      dDOC_dt(iLbl) = dDOC_dt(iLbl) + Ep(k)*rLDOMphy  !!!!!!**************
      dDOC_dt(iRfr) = dDOC_dt(iRfr) + Ep(k)*(1.0d0-rLDOMphy)  !!!!!!**************
#if defined CARBON_ISOTOPE
!      dDO13C_dt(iLbl) = dDO13C_dt(iLbl) + Ep(k)*rPHY13C(k)
      dDO13C_dt(iLbl) = dDO13C_dt(iLbl) + Ep(k)*rLDOMphy*a_phot*rDI13C
      dDO13C_dt(iRfr) = dDO13C_dt(iRfr) + Ep(k)*(1.0d0-rLDOMphy)*a_phot*rDI13C
#endif
#if defined NUTRIENTS
!      dDON_dt(iLbl) = dDON_dt(iLbl) + Ep(k)*rLDOMphy/rCNp(k)  !!!!!!**************
!      dDOP_dt(iLbl) = dDOP_dt(iLbl) + Ep(k)*rLDOMphy/rCPp(k)  !!!!!!**************
!      dDON_dt(iRfr) = dDON_dt(iRfr) + Ep(k)*(1.0d0-rLDOMphy)/rCNp(k)  !!!!!!**************
!      dDOP_dt(iRfr) = dDOP_dt(iRfr) + Ep(k)*(1.0d0-rLDOMphy)/rCPp(k)  !!!!!!**************
#endif

!----- Respiration rate (umolC L-1 s-1) -----------------

      Rp(k) = k_Rp(k) * exp(b_Rp(k)*Tmp) * MAX(PHY(k), 0.0d0)
      ! IF(DOx <= 0.0d0) THEN !-----For Error handling
      !   Rp(k) = 0.0d0
      ! ENDIF
      Rp(k) = MAX( Rp(k), 0.0d0 )  !!! Error handling
      Rp(k) = MIN( Rp(k), 0.1d0*PHY(k)/dt ) !!! yt_edit Error handling  respiration should not exceed 10% PHY concentration in a timestep
      Rp(k) = MIN( Rp(k), 0.1d0*DOx/dt )    !!! yt_edit Error handling  respiration should not consume over 10% DO concentration in a time step

!     === Mass barance ===

      dPHY_dt(k) = dPHY_dt(k) - Rp(k)
      dDIC_dt = dDIC_dt + Rp(k)
      dDOx_dt = dDOx_dt - Rp(k)
#if defined CARBON_ISOTOPE
      dPHY13C_dt(k) = dPHY13C_dt(k) - Rp(k)*rPHY13C(k)
      dDI13C_dt = dDI13C_dt + Rp(k)*rPHY13C(k)
#endif
#if defined NUTRIENTS
      dNH4_dt = dNH4_dt + Rp(k)/rCNp(k)
      dPO4_dt = dPO4_dt + Rp(k)/rCPp(k)
      dTA_dt = dTA_dt + 1.0d0*Rp(k)/rCNp(k) - 2.0d0*Rp(k)/rCPp(k)
#endif

!----- Mortality (umolC L-1 s-1) -----------------

      Mp(k) = k_Mp(k) * exp(b_Mp(k)*Tmp) * MAX(PHY(k),0.0d0)*MAX(PHY(k),0.0d0)
!      Mphy = k_Mphy * exp(b_Mphy*Tmp) * PHY
      Mp(k) = MAX( Mp(k), 0.0d0 )  !!! Error handling
      Mp(k) = MIN( Mp(k), 0.1d0*PHY(k)/dt ) !!! yt_edit Error handling  mortality should not exceed 10% PHY concentration in a timestep

!     === Mass barance ===

      dPHY_dt(k) = dPHY_dt(k) - Mp(k)
      dPOC_dt(iDtr) = dPOC_dt(iDtr) + Mp(k)
      dPIC_dt(iCaCO3) = dPIC_dt(iCaCO3) + Mp(k)*rCCp(k)
#if defined FOODWEB_TESTMODE
      dDeadPHY_dt(k) = dDeadPHY_dt(k) + Mp(k)
#endif
#if defined CARBON_ISOTOPE
      dPHY13C_dt(k) = dPHY13C_dt(k) - Mp(k)*rPHY13C(k)
      dPO13C_dt(iDtr) = dPO13C_dt(iDtr) + Mp(k)*rPHY13C(k)
      dPI13C_dt = dPI13C_dt + Mp(k)*rCCp(k)*a_calc/a_phot*rPHY13C(k)  !!!!!!********** Need check
#endif
#if defined NUTRIENTS
      dPON_dt(iDtr) = dPON_dt(iDtr) + Mp(k)/rCNp(k)  !!!!!!**************
      dPOP_dt(iDtr) = dPOP_dt(iDtr) + Mp(k)/rCPp(k)  !!!!!!**************
#endif
    END DO

!===== Zooplankton reaction ======================================

    DO k=1, Nzoo

!----- Grazing rate of PHY by ZOO (umolC L-1 s-1) -----------------

      DO m=1, Nphy
        Gp2z(m,k) = k_Gp2z(m,k) * exp(b_Gp2z(m,k)*Tmp)    &
          * (1-exp(lam(k)*(t_Gp2z(m,k) - PHY(m)))) * ZOO(k)  !!!!!!!!!!!!!!!!!!!
        IF(Gp2z(m,k) <= 0.0d0) THEN !-----For Error handling
          Gp2z(m,k) = 0.0d0
        ENDIF
        Gp2z(m,k) = MAX( Gp2z(m,k), 0.0d0 )  !!! Error handling
        Gp2z(m,k) = MIN( Gp2z(m,k), 0.1d0*PHY(m)/dt ) !!! yt_edit Error handling  grazing should not exceed 10% PHY concentration in a timestep

!     === Mass barance ===

        dPHY_dt(m) = dPHY_dt(m) - Gp2z(m,k)
        dZOO_dt(k) = dZOO_dt(k) + Gp2z(m,k)*e_Gp2z(m,k)
        dPOC_dt(iDtr) = dPOC_dt(iDtr) + Gp2z(m,k)*(1.0d0-e_Gp2z(m,k))
#if defined CARBON_ISOTOPE
        dPHY13C_dt(m) = dPHY13C_dt(m) - Gp2z(m,k)*rPHY13C(m)
        dZOO13C_dt(k) = dZOO13C_dt(k) + Gp2z(m,k)*e_Gp2z(m,k)*a_grz*rPHY13C(m)
        dPO13C_dt(iDtr) = dPO13C_dt(iDtr) + Gp2z(m,k)*(1.0d0-e_Gp2z(m,k)*a_grz)*rPHY13C(m)  !!!!****Need check
#endif
#if defined NUTRIENTS
        dPON_dt(iDtr) = dPON_dt(iDtr) + Gp2z(m,k)*(1.0d0-e_Gp2z(m,k))/rCNz(k)
        dPOP_dt(iDtr) = dPOP_dt(iDtr) + Gp2z(m,k)*(1.0d0-e_Gp2z(m,k))/rCPz(k)
#endif
      END DO

!!!------- Microbial loop (inplicitly assumed) ---------------

!----- Grazing rate of DOC(iLbl) by ZOO (umolC L-1 s-1) -----------------

      DO m=1, Ndom
        cf1 = DOC(m)
#if defined NUTRIENTS
        cf1 =  MIN( MIN(DOC(m),DON(m)*rCNz(k)) , DOP(m)*rCPz(k) )
#endif
        Gdoc2z(m,k) = k_Gdoc2z(m,k) * exp(b_Gdoc2z(m,k)*Tmp) * cf1 * ZOO(k) ! yt_edit consider making this michaelis-menten dependent on ZOO to not exceed exceed 100% uptake
        Gdoc2z(m,k) = MAX( Gdoc2z(m,k), 0.0d0 )  !!! Error handling
        Gdoc2z(m,k) = MIN( Gdoc2z(m,k), 0.1d0*cf1/dt ) !!! yt_edit Error handling  grazing should not exceed 10% total of limiting factor concentration in a timestep

!     === Mass barance ===
        dZOO_dt(k) = dZOO_dt(k) + Gdoc2z(m,k)*e_Gdoc2z(m,k)
        dDOC_dt(m) = dDOC_dt(m) - Gdoc2z(m,k)
        dDOC_dt(iLbl) = dDOC_dt(iLbl) + Gdoc2z(m,k)*(1.0d0-e_Gdoc2z(m,k))
#if defined CARBON_ISOTOPE
        dZOO13C_dt(k) = dZOO13C_dt(k) + Gdoc2z(m,k)*e_Gdoc2z(m,k)*a_grz*rDO13C(m)
        dDO13C_dt(m) = dDO13C_dt(m) - Gdoc2z(m,k)*rDO13C(m)
        dDO13C_dt(iLbl) = dDO13C_dt(iLbl) + Gdoc2z(m,k)*(1.0d0-e_Gdoc2z(m,k)*a_grz)*rDO13C(m)
#endif
#if defined NUTRIENTS
        dDON_dt(m) = dDON_dt(m) - Gdoc2z(m,k)/rCNz(k)
        dDON_dt(iLbl) = dDON_dt(iLbl) + Gdoc2z(m,k)*(1.0d0-e_Gdoc2z(m,k))/rCNz(k)
        dDOP_dt(m) = dDOP_dt(m) - Gdoc2z(m,k)/rCPz(k)
        dDOP_dt(iLbl) = dDOP_dt(iLbl) + Gdoc2z(m,k)*(1.0d0-e_Gdoc2z(m,k))/rCPz(k)
#endif
      END DO

!----- Grazing rate of POC by ZOO (umolC L-1 s-1) -----------------

      DO m=1, Npom
        cf1 = POC(m)
#if defined NUTRIENTS
        cf1 =  MIN( MIN(POC(m),PON(m)*rCNz(k)) , POP(m)*rCPz(k) )
#endif
        Gpoc2z(m,k) = k_Gpoc2z(m,k) * exp(b_Gpoc2z(m,k)*Tmp) * cf1 * ZOO(k)
        Gpoc2z(m,k) = MAX( Gpoc2z(m,k), 0.0d0 )  !!! Error handling
        Gpoc2z(m,k) = MIN( Gpoc2z(m,k), 0.1d0*cf1/dt ) !!! yt_edit Error handling  grazing should not exceed 10% total of limiting factor concentration in a timestep

!     === Mass barance ===
        dZOO_dt(k) = dZOO_dt(k) + Gpoc2z(m,k)*e_Gpoc2z(m,k)
        dPOC_dt(m) = dPOC_dt(m) - Gpoc2z(m,k)
        dPOC_dt(iDtr) = dPOC_dt(iDtr) + Gpoc2z(m,k)*(1.0d0-e_Gpoc2z(m,k))
#if defined CARBON_ISOTOPE
        dZOO13C_dt(k) = dZOO13C_dt(k) + Gpoc2z(m,k)*e_Gpoc2z(m,k)*a_grz*rPO13C(m)
        dPO13C_dt(m) = dPO13C_dt(m) - Gpoc2z(m,k)*rPO13C(m)  !!!!****Need check
        dPO13C_dt(iDtr) = dPO13C_dt(iDtr) + Gpoc2z(m,k)*(1.0d0-e_Gpoc2z(m,k)*a_grz)*rPO13C(m)  !!!!****Need check
#endif
#if defined NUTRIENTS
        dPON_dt(m) = dPON_dt(m) - Gpoc2z(m,k)/rCNz(k)
        dPON_dt(iDtr) = dPON_dt(iDtr) + Gpoc2z(m,k)*(1.0d0-e_Gpoc2z(m,k))/rCNz(k)
        dPOP_dt(m) = dPOP_dt(m) - Gpoc2z(m,k)/rCPz(k)
        dPOP_dt(iDtr) = dPOP_dt(iDtr) + Gpoc2z(m,k)*(1.0d0-e_Gpoc2z(m,k))/rCPz(k)
#endif
      END DO
  
!----- Respiration rate (umolC L-1 s-1) -----------------

      Rz(k) = k_Rz(k) * exp(b_Rz(k)*Tmp) * ZOO(k)
      ! IF(DOx <= 0.0d0) THEN !-----For Error handling
      !   Rz(k) = 0.0d0
      ! ENDIF
      Rz(k) = MAX( Rz(k), 0.0d0 )  !!! Error handling
      Rz(k) = MIN( Rz(k), 0.1d0*ZOO(k)/dt )  !!! yt_edit Error handling  respiration should not exceed 10% ZOO in a timestep
      Rz(k) = MIN( Rz(k), 0.1d0*DOx/dt )     !!! yt_edit Error handling  respiration should not consume over 10% DO concentration in a time step

!     === Mass barance ===

      dZOO_dt(k) = dZOO_dt(k) - Rz(k)
      dDIC_dt = dDIC_dt + Rz(k)
      dDOx_dt = dDOx_dt - Rz(k)
#if defined CARBON_ISOTOPE
      dZOO13C_dt(k) = dZOO13C_dt(k) - Rz(k)*rZOO13C(k)
      dDI13C_dt = dDI13C_dt + Rz(k)*rZOO13C(k)
#endif
#if defined NUTRIENTS
      dNH4_dt = dNH4_dt + Rz(k)/rCNz(k)
      dPO4_dt = dPO4_dt + Rz(k)/rCPz(k)
      dTA_dt = dTA_dt + 1.0d0*Rz(k)/rCNz(k) - 2.0d0*Rz(k)/rCPz(k)
#endif
  
!----- Mortality (umolC L-1 s-1) -----------------

      Mz(k) = k_Mz(k) * exp(b_Mz(k)*Tmp) * ZOO(k)*ZOO(k)
      Mz(k) = MAX( Mz(k), 0.0d0 )  !!! Error handling
      Mz(k) = MIN( Mz(k), 0.1d0*ZOO(k)/dt )  !!! yt_edit Error handling mortality should not exceed 10% ZOO in a timestep

!     === Mass barance ===

      dZOO_dt(k) = dZOO_dt(k) - Mz(k)
      dPOC_dt(iDtr) = dPOC_dt(iDtr) + Mz(k)
#if defined CARBON_ISOTOPE
      dZOO13C_dt(k) = dZOO13C_dt(k) - Mz(k)*rZOO13C(k)
      dPO13C_dt(iDtr) = dPO13C_dt(iDtr) + Mz(k)*rZOO13C(k)
#endif
#if defined NUTRIENTS
      dPON_dt(iDtr) = dPON_dt(iDtr) + Mz(k)/rCNz(k)
      dPOP_dt(iDtr) = dPOP_dt(iDtr) + Mz(k)/rCPz(k)
#endif
    END DO

!===== Decomposition ==================================================

!----- Decomposition rate of DOM (umol L-1 s-1) -----------------

    DO k=1, Ndom
      Ddoc(k) = k_Ddoc(k) * exp(b_Ddoc(k)*Tmp) * DOC(k)
      ! IF(DOx <= 0.0d0) THEN !-----For Error handling
      !   Ddoc(k) = 0.0d0
      ! ENDIF
      Ddoc(k) = MAX( Ddoc(k), 0.0d0 )  !!! Error handling
      Ddoc(k) = MIN( Ddoc(k), 0.1d0*DOC(k)/dt )  !!! yt_edit Error handling decomposition should not exceed 10% DOC concentration in a timestep
      Ddoc(k) = MIN( Ddoc(k), 0.1d0*DOx/dt )     !!! yt_edit Error handling decomposition should not consume over 10% DO concentration in a time step


!     === Mass barance ===

      dDOC_dt(k) = dDOC_dt(k) - Ddoc(k)
      dDIC_dt = dDIC_dt + Ddoc(k)
      dDOx_dt = dDOx_dt - Ddoc(k)
#if defined CARBON_ISOTOPE
      dDO13C_dt(k) = dDO13C_dt(k) - Ddoc(k)*rDO13C(k)
      dDI13C_dt = dDI13C_dt + Ddoc(k)*rDO13C(k)
#endif
#if defined NUTRIENTS
      Ddon(k) = k_Ddon(k) * exp(b_Ddon(k)*Tmp) * DON(k)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Ddon(k) = 0.0d0
      ENDIF
      Ddon(k) = MAX( Ddon(k), 0.0d0 )  !!! Error handling
      Ddon(k) = MIN( Ddon(k), 0.1d0*DON(k)/dt )  !!! yt_edit Error handling decomposition should not exceed 10% DON concentration in a timestep

      Ddop(k) = k_Ddop(k) * exp(b_Ddop(k)*Tmp) * DOP(k)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Ddop(k) = 0.0d0
      ENDIF
      Ddop(k) = MAX( Ddop(k), 0.0d0 )  !!! Error handling
      Ddop(k) = MIN( Ddop(k), 0.1d0*DOP(k)/dt )  !!! yt_edit Error handling decomposition should not exceed 10% DOP concentration in a timestep

!     === Mass barance ===

      dDON_dt(k) = dDON_dt(k) - Ddon(k)
      dDOP_dt(k) = dDOP_dt(k) - Ddop(k)
      dNH4_dt = dNH4_dt + Ddon(k)
      dPO4_dt = dPO4_dt + Ddop(k)
      dTA_dt = dTA_dt + 1.0d0*Ddon(k) - 2.0d0*Ddop(k)
#endif
    END DO

!----- Decomposition rate of POM (umol L-1 s-1) -----------------

    DO k=1, Npom
      Dpoc(k) = k_Dpoc(k) * exp(b_Dpoc(k)*Tmp) * POC(k)
      ! IF(DOx <= 0.0d0) THEN !-----For Error handling
      !   Dpoc(k) = 0.0d0
      ! ENDIF
      Dpoc(k) = MAX( Dpoc(k), 0.0d0 )  !!! Error handling
      Dpoc(k) = MIN( Dpoc(k), 0.1d0*POC(k)/dt )  !!! yt_edit Error handling decomposition should not exceed 10% POC concentration in a timestep
      Dpoc(k) = MIN( Dpoc(k), 0.1d0*DOx/dt )     !!! yt_edit Error handling decomposition should not consume over 10% DO concentration in a time step

!     === Mass barance ===

      dPOC_dt(k) = dPOC_dt(k) - Dpoc(k)
      dDIC_dt = dDIC_dt + Dpoc(k)
      dDOx_dt = dDOx_dt - Dpoc(k)
#if defined CARBON_ISOTOPE
      dPO13C_dt(k) = dPO13C_dt(k) - Dpoc(k)*rPO13C(k)
      dDI13C_dt = dDI13C_dt + Dpoc(k)*rPO13C(k)
#endif     

#if defined NUTRIENTS
      Dpon(k) = k_Dpon(k) * exp(b_Dpon(k)*Tmp) * PON(k)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Dpon(k) = 0.0d0
      ENDIF
      Dpon(k) = MAX( Dpon(k), 0.0d0 )  !!! Error handling
      Dpon(k) = MIN( Dpon(k), 0.1d0*PON(k)/dt )  !!! yt_edit Error handling decomposition should not exceed 10% PON concentration in a timestep

      Dpop(k) = k_Dpop(k) * exp(b_Dpop(k)*Tmp) * POP(k)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Dpop(k) = 0.0d0
      ENDIF
      Dpop(k) = MAX( Dpop(k), 0.0d0 )  !!! Error handling
      Dpop(k) = MIN( Dpop(k), 0.1d0*POP(k)/dt )  !!! yt_edit Error handling decomposition should not exceed 10% POP concentration in a timestep

!     === Mass barance ===

      dPON_dt(k) = dPON_dt(k) - Dpon(k)
      dPOP_dt(k) = dPOP_dt(k) - Dpop(k)
      dNH4_dt = dNH4_dt + Dpon(k)
      dPO4_dt = dPO4_dt + Dpop(k)
      dTA_dt = dTA_dt + 1.0d0*Dpon(k) - 2.0d0*Dpop(k)
#endif
    END DO

!----- Decomposition rate from POM to DOM (umol L-1 s-1) -----------------

    DO k=1, Ndom
      DO m=1, Npom
        Dp2doc(m,k) = k_Dp2doc(m,k) * exp(b_Dp2doc(m,k)*Tmp) * MAX(POC(m),0.0d0)
        IF(DOx <= 0.0d0) THEN !-----For Error handling
          Dp2doc(m,k) = 0.0d0
        ENDIF
        Dp2doc(m,k) = MAX( Dp2doc(m,k), 0.0d0 )  !!! Error handling
        Dp2doc(m,k) = MIN( Dp2doc(m,k), 0.1d0*POC(m)/dt )  !!! yt_edit Error handling decomposition should not exceed 10% POC concentration in a timestep

!     === Mass barance ===

        dDOC_dt(k) = dDOC_dt(k) + Dp2doc(m,k)
        dPOC_dt(m) = dPOC_dt(m) - Dp2doc(m,k)
#if defined CARBON_ISOTOPE
        dDO13C_dt(k) = dDO13C_dt(k) + Dp2doc(m,k)*rPO13C(m)
        dPO13C_dt(m) = dPO13C_dt(m) - Dp2doc(m,k)*rPO13C(m)
#endif     
#if defined NUTRIENTS
        Dp2don(m,k) = k_Dp2don(m,k) * exp(b_Dp2don(m,k)*Tmp) * MAX(PON(m),0.0d0)
        IF(DOx <= 0.0d0) THEN !-----For Error handling
          Dp2don(m,k) = 0.0d0
        ENDIF
        Dp2don(m,k) = MAX( Dp2don(m,k), 0.0d0 )  !!! Error handling
        Dp2don(m,k) = MIN( Dp2don(m,k), 0.1d0*PON(m)/dt )  !!! yt_edit Error handling decomposition should not exceed 10% PON concentration in a timestep

        Dp2dop(m,k) = k_Dp2dop(m,k) * exp(b_Dp2dop(m,k)*Tmp) * MAX(POP(m),0.0d0)
        IF(DOx <= 0.0d0) THEN !-----For Error handling
          Dp2dop(m,k) = 0.0d0
        ENDIF
        Dp2dop(m,k) = MAX( Dp2dop(m,k), 0.0d0 )  !!! Error handling
        Dp2dop(m,k) = MIN( Dp2dop(m,k), 0.1d0*POP(m)/dt )  !!! yt_edit Error handling decomposition should not exceed 10% POP concentration in a timestep

!     === Mass barance ===

        dDON_dt(k) = dDON_dt(k) + Dp2don(m,k)
        dPON_dt(m) = dPON_dt(m) - Dp2don(m,k)
        dDOP_dt(k) = dDOP_dt(k) + Dp2dop(m,k)
        dPOP_dt(m) = dPOP_dt(m) - Dp2dop(m,k)

#endif
      END DO
    END DO

!----- Decomposition rate from CPOM to Detritus (umol L-1 s-1) -----------------

    Dcpoc2dtr = k_Dcpoc2dtr * exp(b_Dcpoc2dtr*Tmp) * POC(iCPOM)
    Dcpoc2dtr = MAX( Dcpoc2dtr, 0.0d0 )  !!! Error handling
    Dcpoc2dtr = MIN( Dcpoc2dtr, 0.1d0*POC(iCPOM)/dt )  !!! yt_edit Error handling decomposition should not exceed 10% POC(iCPOM) concentration in a timestep

  !     === Mass barance ===
  
    dPOC_dt(iCPOM) = dPOC_dt(iCPOM) - Dcpoc2dtr
    dPOC_dt(iDtr ) = dPOC_dt(iDtr ) + Dcpoc2dtr
#if defined CARBON_ISOTOPE
    dPO13C_dt(iCPOM) = dPO13C_dt(iCPOM) - Dcpoc2dtr*rPO13C(iCPOM)
    dPO13C_dt(iDtr ) = dPO13C_dt(iDtr ) + Dcpoc2dtr*rPO13C(iCPOM)
#endif               
#if defined NUTRIENTS
    Dcpon2dtr = k_Dcpon2dtr * exp(b_Dcpon2dtr*Tmp) * PON(iCPOM)
    Dcpon2dtr = MAX( Dcpon2dtr, 0.0d0 )  !!! Error handling
    Dcpon2dtr = MIN( Dcpon2dtr, 0.1d0*PON(iCPOM)/dt )  !!! yt_edit Error handling decomposition should not exceed 10% PON(iCPOM) concentration in a timestep

    Dcpop2dtr = k_Dcpop2dtr * exp(b_Dcpop2dtr*Tmp) * POP(iCPOM)
    Dcpop2dtr = MAX( Dcpop2dtr, 0.0d0 )  !!! Error handling
    Dcpop2dtr = MIN( Dcpop2dtr, 0.1d0*POP(iCPOM)/dt )  !!! yt_edit Error handling decomposition should not exceed 10% POP(iCPOM) concentration in a timestep
  
  !     === Mass barance ===
  
    ! dPON_dt(iCPOM) = dPON_dt(iCPOM) - Dcpon2dtr
    ! dPOP_dt(iDtr ) = dPOP_dt(iDtr ) + Dcpop2dtr
    dPON_dt(iCPOM) = dPON_dt(iCPOM) - Dcpon2dtr ! yt_edit above is missing equations???
    dPON_dt(iDtr ) = dPON_dt(iDtr ) + Dcpon2dtr ! yt_edit above is missing equations???
    dPOP_dt(iCPOM) = dPOP_dt(iCPOM) - Dcpop2dtr ! yt_edit above is missing equations???
    dPOP_dt(iDtr ) = dPOP_dt(iDtr ) + Dcpop2dtr ! yt_edit above is missing equations???
    
  
#endif

  
!===== Nitrification ====================================================

#if defined NUTRIENTS
    Nit = k_Nit * exp(b_Nit*Tmp) * MAX( NH4, 0.0d0 )**2
    IF(DOx <= 0.0d0) THEN !-----For Error handling
        Nit = 0.0d0
    ENDIF
    Nit = MAX( Nit, 0.0d0 )  !!! Error handling
    Nit = MIN( Nit, 0.1d0*NH4/dt )  !!! yt_edit Error handling nitrification should not exceed 10% NH4 concentration in a timestep
    Nit = MIN( Nit, 0.1d0*DOx/dt/2.0d0 )  !!! yt_edit Error handling nitrification should not exceed 10% DOx concentration in a timestep


!     === Mass barance ===

    dNO3_dt = dNO3_dt + Nit
    dNH4_dt = dNH4_dt - Nit
    dDOx_dt = dDOx_dt - Nit*2.0d0
    dTA_dt = dTA_dt - Nit*2.0d0
#endif

!===== Carbonate dissolution ==============================================

!!!!!! Need pressure correction (not impremented) !!!!!!
    pH     = pH_fromATCT( TA, DIC, Tmp+273.15d0, Sal )   ! pH
    CO3   = cCO3_fromCTpH( DIC, pH, Tmp+273.15d0, Sal )  ! CO32- concentration
    Wcal   = Wcal_fromcCO3( CO3, Tmp+273.15d0, Sal )  ! Calcite saturation state
    Warg   = Warg_fromcCO3( CO3, Tmp+273.15d0, Sal )  ! Calcite saturation state
    IF ( Wcal < 1.0d0 ) THEN
      Dissol = 1.3d7*(1.d0-Wcal)**2.8d0 * PIC(iCaCO3)  !! [umol L-1 s-1] Low-Mg Calcite dissolution rate (Walter and Morse, 1985)
    ELSE
      Dissol = 0.0d0
    END IF
    Dissol = MAX( Dissol, 0.0d0 )  !!! Error handling
    Dissol = MIN( Dissol, 0.1d0*PIC(iCaCO3)/dt )  !!! yt_edit Error handling  carbonate dissolution should not exceed 10% PIC(iCaCO3) in a timestep


!     === Mass barance ===

    dPIC_dt(iCaCO3) = dPIC_dt(iCaCO3) - Dissol
    dTA_dt = dTA_dt + Dissol*2.0d0
    dDIC_dt = dDIC_dt + Dissol
#if defined CARBON_ISOTOPE
    dDI13C_dt = dDI13C_dt + Dissol*rPI13C(iCaCO3)
#endif               

!===== Unit adjustment ===========================================

    dDIC_dt = dDIC_dt/rho_sw !(umol L-1 s-1) -> (umol kg-1 s-1)
    dTA_dt  = dTA_dt/rho_sw !(umol L-1 s-1) -> (umol kg-1 s-1)

      
    RETURN

  END SUBROUTINE foodweb
#endif
END MODULE mod_foodweb


