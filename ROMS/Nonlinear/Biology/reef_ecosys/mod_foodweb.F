
!!!=== Copyright (c) 2012-2020 Takashi NAKAMURA  =====
!!!      modified by Shinya AMANO, Faisal AMRI

#include "cppdefs.h"


!!!**** MODULE OF FOOD WEB *******************************************

MODULE mod_foodweb
  implicit none
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
    , DOC            &   ! Dissolved organic carbon (DOC: umol L-1)
    , POC            &   ! Particulate organic carbon (DOC: umol L-1)
    , PHY1           &   ! phytoplankton (umol C L-1)dinoflagellate
    , PHY2           &   ! phytoplankton (umol C L-1)diatom
    , ZOO            &   ! zooplankton (umol C L-1)
#if defined CARBON_ISOTOPE
    , DI13C          &   !13C of DIC (umol kg-1)
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
#endif
#if defined COT_STARFISH         
    , COTe           &   ! COT starfish egg (umol L-1)
    , COTl           &   ! COT starfish larvae (umol L-1)
#endif
!   output parameters
    , dDIC_dt        &   ! dDIC/dt  (umol kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    , dTA_dt         &   ! dTA/dt   (umol kg-1 s-1) 
    , dDOx_dt        &  ! dDOx/dt  (umol L-1 s-1) 
    , dDOC_dt        &   ! dDOC/dt  (umol L-1 s-1) 
    , dPOC_dt        &   ! dPOC/dt  (umol L-1 s-1) 
    , dPHY1_dt       &   ! dPHY/dt  (umol L-1 s-1)  
    , dPHY2_dt       &   ! dPHY/dt  (umol L-1 s-1)  
    , dZOO_dt        &   ! dZOO/dt  (umol L-1 s-1)  
#if defined CARBON_ISOTOPE
    , dDI13C_dt      &   ! dDI13C/dt (umol kg-1 s-1)
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
    real(8), intent(in) :: DOC            ! Dissolved organic carbon (DOC: umol L-1)
    real(8), intent(in) :: POC            ! Particulate organic carbon (DOC: umol L-1)
    real(8), intent(in) :: PHY1            ! phytoplankton (umol C L-1)dinoflagellata
    real(8), intent(in) :: PHY2            ! phytoplankton (umol C L-1)diatom
    real(8), intent(in) :: ZOO            ! zooplankton (umol C L-1)
#if defined CARBON_ISOTOPE
    real(8), intent(in) :: DI13C          !13C of DIC (umol kg-1)
#endif
#if defined NUTRIENTS         
    real(8), intent(in) :: NO3            ! NO3 (umol L-1)
    real(8), intent(in) :: NO2            ! NO2 (umol L-1)
    real(8), intent(in) :: NH4            ! NH4 (umol L-1)
    real(8), intent(in) :: PO4            ! PO4 (umol L-1)
    real(8), intent(in) :: DON            ! Dissolved organic nitrogen (DON: umol L-1)
    real(8), intent(in) :: PON            ! Particulate organic nitrogen (PON: umol L-1)
    real(8), intent(in) :: DOP            ! Dissolved organic phosporius (DOP: umol L-1)
    real(8), intent(in) :: POP            ! Particulate organic phosporius (POP: umol L-1)
#endif
#if defined COT_STARFISH         
    real(8), intent(in) :: COTe           ! COT starfish egg (umol L-1)
    real(8), intent(in) :: COTl           ! COT starfish larvae (umol L-1)
#endif
!          output parameters
    real(8), intent(out) :: dDIC_dt       ! dDIC/dt  (umol kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    real(8), intent(out) :: dTA_dt        ! dTA/dt   (umol kg-1 s-1) 
    real(8), intent(out) :: dDOx_dt       ! dDO/dt   (umol L-1 s-1) 
    real(8), intent(out) :: dDOC_dt       ! dDOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPOC_dt       ! dPOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPHY1_dt       ! dPHY/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dPHY2_dt       ! dPHY/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dZOO_dt       ! dZOO/dt  (umol L-1 s-1)  
#if defined CARBON_ISOTOPE
    real(8), intent(out) :: dDI13C_dt     ! dDI13C/dt (umol kg-1 s-1)
#endif
#if defined NUTRIENTS
    real(8), intent(out) :: dNO3_dt       ! dNO3/dt (umol L-1 s-1)
    real(8), intent(out) :: dNO2_dt       ! dNO2/dt (umol L-1 s-1)
    real(8), intent(out) :: dNH4_dt       ! dNH4/dt (umol L-1 s-1)
    real(8), intent(out) :: dPO4_dt       ! dPO4/dt (umol L-1 s-1)
    real(8), intent(out) :: dDON_dt       ! dDON/dt (umol L-1 s-1)
    real(8), intent(out) :: dPON_dt       ! dPON/dt (umol L-1 s-1)
    real(8), intent(out) :: dDOP_dt       ! dDOP/dt (umol L-1 s-1)
    real(8), intent(out) :: dPOP_dt       ! dPOP/dt (umol L-1 s-1)
#endif
#if defined COT_STARFISH
    real(8), intent(out) :: dCOTe_dt      ! dCOTe/dt (umol L-1 s-1)
    real(8), intent(out) :: dCOTl_dt      ! dCOTl/dt (umol L-1 s-1)
#endif

!!!------------Set parameters  ----------------------------------

!------- Phytoplankton parameters ------------------------
! PHY1 Parameter
    real(8), parameter :: k_Pphy1 =  0.4d0/86400.0d0     ! (s-1)          PHY maximum photosynthesis rate at 0 oC (0.4d0 d-1; Kishi et al., 2007)0.3
    real(8), parameter :: b_Pphy1 =  0.0693d0            ! (degC-1)       Temperature coefficient for PHY photosynthesis (0.063; Kawamiya et al., 1995)
    real(8), parameter :: Iphy1   = 48.83d0*1.82d0       ! (umol m-2 s-1) PHY optimum light intensity (48.83d0 J m2 s-1; Kawamiya et al., 1995)
    real(8), parameter :: k_Rphy1 =  0.03d0/86400.0d0    ! (s-1)          PHY respiration rate at 0 oC (Kawamiya et al., 1995)
    real(8), parameter :: b_Rphy1 =  0.0519d0            ! (degC-1)       Temperature coefficient for PHY respiration rate (0.03d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: k_Mphy1 =  0.00562d0/86400.0d0 ! (L umol-1 s-1) PHY mortality rate at 0 oC (0.00562d0 umol-1 d-1; 0.0585L/umolN/day?0.0088; Kishi et al., 2001)0.0066
    real(8), parameter :: b_Mphy1 =  0.069d0             ! (degC-1)       Temperature coefficient for PHY mortality (Kawamiya et al., 1995)
    real(8), parameter :: k_Ephy1 =  0.135d0             ! (no dim.)      PHY ratio of extracellular excretion to production 0.135(Kawamiya et al., 1995)

! PHY2 Parameter
    real(8), parameter :: k_Pphy2 =  0.8d0/86400.0d0     ! (s-1)          PHY maximum photosynthesis rate at 0 oC (0.8d0 d-1; Kishi et al., 2007)0.6
    real(8), parameter :: b_Pphy2 =  0.0693d0            ! (degC-1)       Temperature coefficient for PHY photosynthesis (0.063; Kawamiya et al., 1995)
    real(8), parameter :: Iphy2   = 48.83d0*1.82d0       ! (umol m-2 s-1) PHY optimum light intensity (48.83d0 J m2 s-1; Kawamiya et al., 1995)
    real(8), parameter :: k_Rphy2 =  0.03d0/86400.0d0    ! (s-1)          PHY respiration rate at 0 oC (Kawamiya et al., 1995)
    real(8), parameter :: b_Rphy2 =  0.0519d0            ! (degC-1)       Temperature coefficient for PHY respiration rate (0.03d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: k_Mphy2 =  0.00562d0/86400.0d0 ! (L umol-1 s-1) PHY mortality rate at 0 oC (0.00281d0 umol-1 d-1; 0.029L/umolN/day?0.0063; Kishi et al., 2001)0.003
    real(8), parameter :: b_Mphy2 =  0.069d0             ! (degC-1)       Temperature coefficient for PHY mortality (Kawamiya et al., 1995)
    real(8), parameter :: k_Ephy2 =  0.135d0             ! (no dim.)      PHY ratio of extracellular excretion to production (Kawamiya et al., 1995)
#if defined NUTRIENTS
! PHY1 Nutrient Assimilation Parameter         
    real(8), parameter :: Kphy1_NH4 = 0.2d0   ! (s-1)1.5         
    real(8), parameter :: Kphy1_NO3 = 0.1d0   ! (s-1)0.1         
    real(8), parameter :: Kphy1_PO4 = 0.01d0  ! (s-1)0.01 

! PHY2 Nutrient Assimilation Parameter
    real(8), parameter :: Kphy2_NH4 = 0.2d0   ! (s-1)     4.5d0   (Kawamiya et al., 2000) 
    real(8), parameter :: Kphy2_NO3 = 0.1d0   ! (s-1)     0.3d0  (Kawamiya et al., 2000)  
    real(8), parameter :: Kphy2_PO4 = 0.01d0  ! (s-1)  

    real(8), parameter :: psi1 =  0.01d0    !(L umolC-1)   (1.5L umolN-1; Kishi et al., 2007)  0.01  0.226  0.5
    real(8), parameter :: psi2 =  0.01d0    !(L umolC-1)   (1.5L umolN-1; Kishi et al., 2007)  0.01  0.226  0.5

#endif
!------- Zooplankton parameters ------------------------
    real(8), parameter :: k_Gphy12zoo = 0.30d0/86400.0d0 ! (s-1)          Maximum grazing rate of PHY1 by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)0.16
    real(8), parameter :: b_Gphy12zoo = 0.0693d0         ! (degC-1)       Temperature coefficient for ZOO grazing (0.063d0degC-1;Kawamiya et al., 1995)
    real(8), parameter :: e_Gphy12zoo = 0.70d0           ! (no dim.)      Assimilation efficiency of ZOO (0.7:Kawamiya et al., 1995)
    real(8), parameter :: k_Gphy22zoo = 0.30d0/86400.0d0 ! (s-1)          Maximum grazing rate of PHY2 by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)0.12
    real(8), parameter :: b_Gphy22zoo = 0.0693d0         ! (degC-1)       Temperature coefficient for ZOO grazing (Kawamiya et al., 1995)
    real(8), parameter :: e_Gphy22zoo = 0.70d0           ! (no dim.)      Assimilation efficiency of ZOO (0.7:Kawamiya et al., 1995)
    real(8), parameter :: k_Rzoo = 0.005d0/86400.0d0     ! (s-1)          ZOO respiration rate at 0  oC  																					!!!(Tuning)
    real(8), parameter :: b_Rzoo = 0.0693d0              ! (degC-1)       Temperature coefficient for ZOO respiration rate (Kawamiya et al., 1995)
    real(8), parameter :: k_Mzoo = 0.0176d0/86400.0d0    ! (L umol-1 s-1)   ZOO mortality rate at 0 oC 3.0d0/86400.0d0(0.0088d0 umol-1 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Mzoo = 0.0693d0              ! (degC-1)       Temperature coefficient for ZOO mortality (Kawamiya et al., 1995)
    real(8), parameter :: lam1 = 0.211d0                 ! ((umol C L-1)-1)       zooplankton Ivlev constant for PHY1 (1.4L/umolN.0.211; Kishi et al., 2007)
    real(8), parameter :: lam2 = 0.211d0           	     ! ((umol C L-1)-1)       zooplankton Ivlev constant for PHY2 (1.4L/umolN; Kishi et al., 2007)
    real(8), parameter :: t_Gphy12zoo = 0.265d0          ! (umol C L-1)       PHY1 threshold value for grazing by ZOO (0.04umolN/L0.265; Kishi et al., 2007)
    real(8), parameter :: t_Gphy22zoo = 0.265d0          ! (umol C L-1)       PHY2 threshold value for grazing by ZOO (0.04umolN/L; Kishi et al., 2007)
!------- Microbial loop parameters  -------
    real(8), parameter :: k_Gdoc2zoo = 0.0d0          ! (s-1)          Maximum grazing rate of DOC by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Gdoc2zoo = 0.0d0          ! (degC-1)       Temperature coefficient of DOC grazing by ZOO (Kawamiya et al., 1995)
    real(8), parameter :: k_Gpoc2zoo = 0.0d0          ! (s-1)          Maximum grazing rate of POC by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Gpoc2zoo = 0.0d0          ! (degC-1)       Temperature coefficient of DOC grazing by ZOO (Kawamiya et al., 1995)
!------- Decomposition parameters --------------------
    real(8), parameter :: k_Ddoc = 0.3d0/86400.0d0    ! (s-1)          Decomposition rate of DOC at 0 oC (0.3d0 d-1; Kishi et al., 2001)
    real(8), parameter :: b_Ddoc = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of DOC (Kishi et al., 2001)
    real(8), parameter :: k_Dpoc = 0.3d0/86400.0d0    ! (s-1)          Decomposition rate of POC at 0 oC (0.3d0 d-1; Kishi et al., 2001)
    real(8), parameter :: b_Dpoc = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of POC (Kishi et al., 2001)
    real(8), parameter :: k_Ddon = 0.02d0/86400.0d0   ! (s-1)          Decomposition rate of DON at 0 oC (0.02d0 d-1; Kishi et al., 2007)
    real(8), parameter :: b_Ddon = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of DON (Kishi et al., 2007)
    real(8), parameter :: k_Dpon = 0.1d0/86400.0d0    ! (s-1)          Decomposition rate of PON at 0 oC (0.1d0 d-1; Kishi et al., 2007)
    real(8), parameter :: b_Dpon = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of POC (Kishi et al., 2007)
    real(8), parameter :: k_Ddop = 0.02d0/86400.0d0   ! (s-1)          Decomposition rate of DOP at 0 oC 
    real(8), parameter :: b_Ddop = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of DOP 
    real(8), parameter :: k_Dpop = 0.1d0/86400.0d0    ! (s-1)          Decomposition rate of POP at 0 oC 
    real(8), parameter :: b_Dpop = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of POP 
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

    real(8), parameter :: rCNphy1 = 9.2d0      ! (no dim.) PHY1 C:N ratio
    real(8), parameter :: rCPphy1 = 131.9d0    ! (no dim.) PHY1 C:P ratio
    real(8), parameter :: rCNphy2 = 9.2d0      ! (no dim.) PHY2 C:N ratio
    real(8), parameter :: rCPphy2 = 131.9d0    ! (no dim.) PHY2 C:P ratio
    real(8), parameter :: rCNzoo = 9.2d0       ! (no dim.) ZOO  C:N ratio
    real(8), parameter :: rCPzoo = 131.9d0     ! (no dim.) ZOO  C:P ratio
    real(8), parameter :: rNH4 = 0.3d0         ! (no dim.)  PHY NH4:Nitrogen ratio 
#endif                                

!------- Local variables --------------------
    real(8) :: Pphy1, Rphy1, Mphy1, Ephy1, Aphy1
    real(8) :: Pphy2, Rphy2, Mphy2, Ephy2, Aphy2
    real(8) :: Gphy12zoo,Gphy22zoo, Rzoo, Mzoo
    real(8) :: Gdoc2zoo, Gpoc2zoo
    real(8) :: Ddoc, Dpoc, Dpoc2doc
#if defined NUTRIENTS
    real(8) :: Gdon2zoo, Gpon2zoo
    real(8) :: Gdop2zoo, Gpop2zoo
    real(8) :: Ddon, Dpon, Dpon2don
    real(8) :: Ddop, Dpop, Dpop2dop
    real(8) :: Vphy1_NH4, Vphy1_NO3
    real(8) :: Vphy1_PO4
    real(8) :: Vphy2_NH4, Vphy2_NO3
    real(8) :: Vphy2_PO4
    real(8) :: Nit
#endif

!!!------- Phytoplankton reaction ------------------------

!----- Gross photosynthetic rate (umolC L-1 s-1) -----------------
!    Pphy = k_Pphy * exp(b_Pphy*Tmp) * PFD/Iphy*exp(1.0d0-PFD/Iphy) * PHY
    Pphy1 = k_Pphy1 * exp(b_Pphy1*Tmp) * tanh(PFD/Iphy1) * PHY1
    Pphy2 = k_Pphy2 * exp(b_Pphy2*Tmp) * tanh(PFD/Iphy2) * PHY2
    IF(DIC <= 0.d0) THEN !-----For Error handling
      Pphy1 = 0.d0
      Pphy2 = 0.d0
    ENDIF

!----- Assimilation rate (umolC L-1 s-1) -----------------

#if defined NUTRIENTS         
!    Vphy1_NH4 = NH4/(NH4+Kphy1_NH4)
!    Vphy1_NO3 = NO3/(NO3+Kphy1_NO3) * exp(-psi1 * NH4)
    Vphy1_NH4 = NH4/(NH4+Kphy1_NH4) * rNH4
    Vphy1_NO3 = NO3/(NO3+Kphy1_NO3) * exp(-psi1 * NH4) *(1.0d0 - rNH4)
    
    Vphy1_PO4 = PO4/(PO4+Kphy1_PO4)
    
!    Vphy2_NH4 = NH4 * NH4/(NH4 * NH4+Kphy2_NH4 * Kphy2_NH4) * rNH4												!
!    Vphy2_NO3 = NO3 * NO3/(NO3 * NO3+Kphy2_NO3 * Kphy2_NO3) * exp(-psi2 * NH4)	*(1.0d0 - rNH4)			!
    Vphy2_NH4 = NH4 * NH4/(NH4 * NH4+Kphy2_NH4 * Kphy2_NH4)													!
    Vphy2_NO3 = NO3 * NO3/(NO3 * NO3+Kphy2_NO3 * Kphy2_NO3) * exp(-psi2 * NH4)				!
    
    Vphy2_PO4 = PO4 * PO4/(PO4 * PO4+Kphy2_PO4 * Kphy2_PO4)													!
    
    Aphy1 = MIN( Vphy1_NH4+Vphy1_NO3, Vphy1_PO4 ) ! Assimilation rate
    Aphy2 = MIN( Vphy2_NH4+Vphy2_NO3, Vphy2_PO4 ) ! Assimilation rate
!    Aphy1 = Vphy1_NH4+Vphy1_NO3										! Assimilation rate
!    Aphy2 = Vphy2_NH4+Vphy2_NO3 									! Assimilation rate

    Aphy1 = Aphy1 * Pphy1
    Aphy2 = Aphy2 * Pphy2
#else
    Aphy1 = (1.0d0-k_Ephy1) * Pphy1
    Aphy2 = (1.0d0-k_Ephy2) * Pphy2
#endif

!----- Excretion rate (umolC L-1 s-1) -----------------
#if defined NUTRIENTS
    Ephy1 = Pphy1 - Aphy1
    Ephy2 = Pphy2 - Aphy2
#else
    Ephy1 = k_Ephy1 * Pphy1
    Ephy2 = k_Ephy2 * Pphy2
#endif

!----- Respiration rate (umolC L-1 s-1) -----------------
    Rphy1 = k_Rphy1 * exp(b_Rphy1*Tmp) * PHY1
    Rphy2 = k_Rphy2 * exp(b_Rphy2*Tmp) * PHY2
    IF(DOx <= 0.d0) THEN !-----For Error handling
      Rphy1 = 0.d0
      Rphy2 = 0.d0
    ENDIF

!----- Mortality (umolC L-1 s-1) -----------------
    Mphy1 = k_Mphy1 * exp(b_Mphy1*Tmp) * PHY1*PHY1
    Mphy2 = k_Mphy2 * exp(b_Mphy2*Tmp) * PHY2*PHY2
!    Mphy = k_Mphy * exp(b_Mphy*Tmp) * PHY

!!!------- Zooplankton reaction ------------------------

!----- Grazing rate of PHY by ZOO (umolC L-1 s-1) -----------------
    Gphy12zoo = k_Gphy12zoo * exp(b_Gphy12zoo*Tmp) * (1-exp(lam1*(t_Gphy12zoo - PHY1))) * ZOO			!!!!!!!!!!!!!!!!!!!
    IF(Gphy12zoo <= 0.d0) THEN !-----For Error handling
    	Gphy12zoo = 0
    ENDIF
    Gphy22zoo = k_Gphy22zoo * exp(b_Gphy22zoo*Tmp) * (1-exp(lam2*(t_Gphy22zoo - PHY2))) * ZOO			!!!!!!!!!!!!!!!!!!!!!
    IF(Gphy22zoo <= 0.d0) THEN !-----For Error handling
    	Gphy22zoo = 0
    ENDIF

!----- Respiration rate (umolC L-1 s-1) -----------------
    Rzoo = k_Rzoo * exp(b_Rzoo*Tmp) * ZOO
    IF(DOx <= 0.d0) THEN !-----For Error handling
      Rzoo = 0.d0
    ENDIF

!----- Mortality (umolC L-1 s-1) -----------------
    Mzoo = k_Mzoo * exp(b_Mzoo*Tmp) * ZOO*ZOO
!    Mzoo = k_Mzoo * exp(b_Mzoo*Tmp) * ZOO
!    Mzoo = k_Mzoo * exp(b_Mzoo*Tmp) * (Gphy12zoo + Gphy22zoo)

!!!------- Microbial loop (inplicitly assumed) ---------------

!----- Grazing rate of DOC by ZOO (umolC L-1 s-1) -----------------
    Gdoc2zoo = k_Gpoc2zoo * exp(b_Gdoc2zoo*Tmp) * DOC * ZOO

!----- Grazing rate of POC by ZOO (umolC L-1 s-1) -----------------
    Gpoc2zoo = k_Gdoc2zoo * exp(b_Gpoc2zoo*Tmp) * POC * ZOO

!!!------- Decomposition ------------------------

!----- Decomposition rate of DOM (umol L-1 s-1) -----------------
!    Ddoc = k_Ddoc * exp(b_Ddoc*Tmp) * DOC
    Ddoc = k_Ddoc * exp(b_Ddoc*Tmp) * DOC**2.0d0/(60.0d0**2.0d0+DOC**2.0d0)
    IF(DOx <= 0.0d0) THEN !-----For Error handling
      Ddoc = 0.0d0
    ENDIF
#if defined NUTRIENTS
!    Ddon = k_Ddon * exp(b_Ddon*Tmp) * DON
    Ddon = k_Ddon * exp(b_Ddon*Tmp) * DON**2.0d0/(60.0d0**2.0d0+DON**2.0d0)
    IF(DOx <= 0.0d0) THEN !-----For Error handling
      Ddon = 0.0d0
    ENDIF
!    Ddop = k_Ddop * exp(b_Ddop*Tmp) * DOP
    Ddop = k_Ddop * exp(b_Ddop*Tmp) * DOP**2.0d0/(60.0d0**2.0d0+DOP**2.0d0)
    IF(DOx <= 0.0d0) THEN !-----For Error handling
      Ddon = 0.0d0
    ENDIF
#endif

!----- Decomposition rate of POM (umol L-1 s-1) -----------------
    Dpoc = k_Dpoc * exp(b_Dpoc*Tmp) * POC**2.0d0/(60.0d0**2.0d0+POC**2.0d0)
    IF(DOx <= 0.0d0) THEN !-----For Error handling
      Dpoc = 0.0d0
    ENDIF
#if defined NUTRIENTS
    Dpon = k_Dpon * exp(b_Dpon*Tmp) * PON**2.0d0/(60.0d0**2.0d0+PON**2.0d0)
    IF(DOx <= 0.0d0) THEN !-----For Error handling
      Dpon = 0.0d0
    ENDIF
    Dpop = k_Dpop * exp(b_Dpop*Tmp) * POP**2.0d0/(60.0d0**2.0d0+POP**2.0d0)
    IF(DOx <= 0.0d0) THEN !-----For Error handling
      Dpop = 0.0d0
    ENDIF
#endif
!----- Decomposition rate from POM to DOM (umol L-1 s-1) -----------------
    Dpoc2doc = 0.0d0 !!!
#if defined NUTRIENTS
    Dpon2don = 0.0d0 !!!
    Dpop2dop = 0.0d0 !!!
#endif

!!!------- Nitrification ------------------------
#if defined NUTRIENTS
    Nit = k_Nit * exp(b_Nit*Tmp) * NH4 ** 2.0d0
#endif

!!!------- Mass barance equations ------------------------

    dPHY1_dt = Pphy1 - Rphy1 - Mphy1 - Ephy1 - Gphy12zoo
    dPHY2_dt = Pphy2 - Rphy2 - Mphy2 - Ephy2 - Gphy22zoo
    dZOO_dt = Gphy12zoo*e_Gphy12zoo + Gphy22zoo*e_Gphy22zoo + Gdoc2zoo + Gpoc2zoo - Rzoo -Mzoo
#if defined COT_STARFISH
    dCOTe_dt = 0.0d0
    dCOTl_dt = 0.0d0
#endif

    dDIC_dt = -Pphy1 + Rphy1 - Pphy2 + Rphy2 + Rzoo + Ddoc + Dpoc
    dDIC_dt = dDIC_dt/rho_sw !(umol L-1 s-1) -> (umol kg-1 s-1)
    dTA_dt  = 0.0d0
    dTA_dt  = dTA_dt/rho_sw !(umol L-1 s-1) -> (umol kg-1 s-1)
    dDOC_dt = Ephy1 + Ephy2 - Gdoc2zoo - Ddoc + Dpoc2doc
    dPOC_dt = Mphy1 + Gphy12zoo*(1.0d0-e_Gphy12zoo) + Mphy2 + Gphy22zoo*(1.0d0-e_Gphy22zoo) + Mzoo - Gpoc2zoo - Dpoc - Dpoc2doc

#if defined NUTRIENTS         
    dNO3_dt = -Vphy1_NO3/(Vphy1_NO3 + Vphy1_NH4)*Aphy1/rCNphy1 - Vphy2_NO3/(Vphy2_NO3 + Vphy2_NH4)*Aphy2/rCNphy2 + Nit
    dNO2_dt = 0.0d0
    dNH4_dt = -Vphy1_NH4/(Vphy1_NO3 + Vphy1_NH4)*Aphy1/rCNphy1       &
             - Vphy2_NH4/(Vphy2_NO3 + Vphy2_NH4)*Aphy2/rCNphy2       &
             + Rphy1/rCNphy1 + Rphy2/rCNphy2 + Rzoo/rCNzoo           &
             + Ddon + Dpon - Nit
    dPO4_dt = (-Aphy1 + Rphy1)/rCPphy1 + (-Aphy2 + Rphy2)/rCPphy2 + Rzoo/rCPzoo + Ddop + Dpop

    dDON_dt = - Gdoc2zoo/rCNzoo - Ddon + Dpon2don
    dPON_dt = (Mphy1 + Gphy12zoo*(1.0d0-e_Gphy12zoo))/rCNphy1        &
             +(Mphy2 + Gphy22zoo*(1.0d0-e_Gphy22zoo))/rCNphy2        &
             +(Mzoo - Gpoc2zoo)/rCNzoo                               &
             - Dpon -Dpon2don

    dDOP_dt = - Gdoc2zoo/rCPzoo - Ddop + Dpop2dop
    dPOP_dt = (Mphy1 + Gphy12zoo*(1.0d0-e_Gphy12zoo))/rCPphy1        &
             +(Mphy2 + Gphy22zoo*(1.0d0-e_Gphy22zoo))/rCPphy2        &
             +(Mzoo - Gpoc2zoo)/rCPzoo                               &
             - Dpop -Dpop2dop
    dDOx_dt  = Pphy1 - Rphy1 + Pphy2 - Rphy2 - Rzoo - Ddoc - Dpoc - 1.86d0 * Nit
#else
    dDOx_dt  = Pphy1 - Rphy1 + Pphy2 - Rphy2 - Rzoo - Ddoc - Dpoc
#endif
      
    RETURN

  END SUBROUTINE foodweb

END MODULE mod_foodweb


