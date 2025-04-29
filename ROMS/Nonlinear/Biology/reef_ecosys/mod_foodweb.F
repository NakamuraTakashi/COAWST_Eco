
!!!=== Copyright (c) 2012-2025 Takashi NAKAMURA  =====
!!!      modified by Shinya AMANO, Faisal AMRI, Jorge SUAREZ

#include "cppdefs.h"

!!!**** MODULE OF FOOD WEB *******************************************

MODULE mod_foodweb
#if defined FOODWEB

  implicit none

CONTAINS

!!! **********************************************************************
!!!  Main program of foodweb model (Modified from Yamamoto et al. under review)
!!! **********************************************************************

  SUBROUTINE foodweb &
!   input parameters
    ( ng, n, i, j    &   ! ng: nested grid number; n: coral compartment; i,j: position
    , date           &   ! Date (days since x or elapsed days)
    , dt             &   ! Time step (sec)
    , PFD            &   ! Photon flux density (umol m-2 s-1)
    , rho_sw         &   ! Density of seawater (g cm-3)
    , Tmp            &   ! Temperature (oC)
    , Sal            &   ! Salinity (PSU)
    , DOx            &   ! DOx             : Dissolved oxygen (umol O2 L-1)
    , TA             &   ! TA              : Total alkalinity (umol kg-1)
    , DIC            &   ! DIC(N_Csp)      : Total dissolved inorganic carbon (umol C kg-1)
    , NO3            &   ! NO3(N_Nsp)      : (umol N L-1)
    , NH4            &   ! NH4(N_Nsp)      : (umol N L-1)
    , PO4            &   ! PO4(N_Psp)      : (umol P L-1)
    , DOC            &   ! DOC(N_Csp,Ndom) : Dissolved organic carbon       (umol C L-1)
    , POC            &   ! POC(N_Csp,Npom) : Particulate organic carbon     (umol C L-1)
    , DON            &   ! DON(N_Nsp,Ndom) : Dissolved organic nitrogen     (umol N L-1)
    , PON            &   ! PON(N_Nsp,Npom) : Particulate organic nitrogen   (umol N L-1)
    , DOP            &   ! DOP(N_Psp,Ndom) : Dissolved organic phosporius   (umol P L-1)
    , POP            &   ! POP(N_Psp,Npom) : Particulate organic phosporius (umol P L-1)
    , PhyC           &   ! PhyC(N_Csp,Nphy): phytoplankton C biomass (umol C L-1)
    , ZooC           &   ! ZooC(N_Csp,Nzoo): zooplankton C biomass   (umol C L-1)
    , PhyN           &   ! PhyN(N_Nsp,Nphy): phytoplankton N biomass (umol N L-1)
    , ZooN           &   ! ZooN(N_Nsp,Nzoo): zooplankton N biomass   (umol N L-1)
    , PhyP           &   ! PhyP(N_Psp,Nphy): phytoplankton P biomass (umol P L-1)
    , ZooP           &   ! ZooP(N_Psp,Nzoo): zooplankton P biomass   (umol P L-1)
    , PIC            &   ! PIC (N_Csp,Npim): Particulate inorganic carbon (PIC: umolC L-1), coccolith (CaCO3)
# if defined BLUE_TIDE         
    , H2S            &   ! H2S(N_Ssp): (umol S L-1)
# endif
# if defined COT_STARFISH         
    , COTe           &   ! COT starfish egg (umol L-1)
    , COTl           &   ! COT starfish larvae (umol L-1)
# endif
!   output parameters
    , dDOx_dt        &   ! dDOx_dt             : dDOx/dt  (umol O2 L-1 s-1) 
    , dTA_dt         &   ! dTA_dt              : dTA/dt   (umol kg-1 s-1) 
    , dDIC_dt        &   ! dDIC_dt(N_Csp)      : dDIC/dt  (umol C kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    , dNO3_dt        &   ! dNO3_dt(N_Nsp)      : dNO3/dt  (umol N L-1 s-1)
    , dNH4_dt        &   ! dNH4_dt(N_Nsp)      : dNH4/dt  (umol N L-1 s-1)
    , dPO4_dt        &   ! dPO4_dt(N_Psp)      : dPO4/dt  (umol P L-1 s-1)
    , dDOC_dt        &   ! dDOC_dt(N_Csp,Ndom) : dDOC/dt  (umol C L-1 s-1) 
    , dPOC_dt        &   ! dPOC_dt(N_Csp,Npom) : dPOC/dt  (umol C L-1 s-1) 
    , dDON_dt        &   ! dDON_dt(N_Nsp,Ndom) : dDON/dt  (umol N L-1 s-1)
    , dPON_dt        &   ! dPON_dt(N_Nsp,Npom) : dPON/dt  (umol N L-1 s-1)
    , dDOP_dt        &   ! dDOP_dt(N_Psp,Ndom) : dDOP/dt  (umol P L-1 s-1)
    , dPOP_dt        &   ! dPOP_dt(N_Psp,Npom) : dPOP/dt  (umol P L-1 s-1)
    , dPhyC_dt       &   ! dPhyC_dt(N_Csp,Nphy): dPhyC/dt (umol C L-1 s-1)  
    , dZooC_dt       &   ! dZooC_dt(N_Csp,Nzoo): dZooC/dt (umol C L-1 s-1)  
    , dPhyN_dt       &   ! dPhyN_dt(N_Nsp,Nphy): dPhyN/dt (umol N L-1 s-1)  
    , dZooN_dt       &   ! dZooN_dt(N_Nsp,Nzoo): dZooN/dt (umol N L-1 s-1)  
    , dPhyP_dt       &   ! dPhyP_dt(N_Psp,Nphy): dPhyP/dt (umol P L-1 s-1)  
    , dZooP_dt       &   ! dZooP_dt(N_Psp,Nzoo): dZooP/dt (umol P L-1 s-1)  
    , dPIC_dt        &   ! dPIC_dt (N_Csp,Npim): dPIC/dt  (umol C L-1 s-1)
#  if defined BLUE_TIDE         
    , dH2S_dt        &   ! dH2S_dt(N_Ssp)      : dH2S/dt  (umol S L-1 s-1)
#  endif
# if defined COT_STARFISH         
    , dCOTe_dt       &   ! dCOTe/dt (umol L-1 s-1)
    , dCOTl_dt       &   ! dCOTl/dt (umol L-1 s-1)
# endif
    , pH             &   ! pH
    , Wcal           &   ! Calcite saturation state
    , Warg           &   ! Aragonite saturation state
    )

!-----------------------------------------------------------------------
!
    USE mod_reef_ecosys_param
    USE mod_geochem
    USE mod_decomposition

    implicit none

!        input parameters
    integer, intent(in) :: ng, n, i, j          ! ng: nested grid number; n: coral compartment; i,j: position
    real(8), intent(in) :: date                 ! Date (days since x or elapsed days)
    real(8), intent(in) :: dt                   ! Time step (sec)
    real(8), intent(in) :: PFD                  ! Photon flux density (umol m-2 s-1)
    real(8), intent(in) :: rho_sw               ! Density of seawater (g cm-3)
    real(8), intent(in) :: Tmp                  ! Temperature (oC)
    real(8), intent(in) :: Sal                  ! Salinity (PSU)
    real(8), intent(in) :: DOx                  ! Dissolved oxygen (umol L-1)
    real(8), intent(in) :: TA                   ! Total alkalinity (TA: umol kg-1)
    real(8), intent(in) :: DIC(N_Csp)           ! Total dissolved inorganic carbon (DIC: umol kg-1)
    real(8), intent(in) :: NO3(N_Nsp)           ! NO3 (umol L-1)
    real(8), intent(in) :: NH4(N_Nsp)           ! NH4 (umol L-1)
    real(8), intent(in) :: PO4(N_Psp)           ! PO4 (umol L-1)
    real(8), intent(in) :: DOC(N_Csp,Ndom)      ! Dissolved organic carbon (DOC: umol L-1)
    real(8), intent(in) :: POC(N_Csp,Npom)      ! Particulate organic carbon (POC: umol L-1)
    real(8), intent(in) :: DON(N_Nsp,Ndom)      ! Labile Dissolved organic nitrogen (DON: umol L-1)
    real(8), intent(in) :: PON(N_Nsp,Npom)      ! Particulate organic nitrogen (PON: umol L-1)
    real(8), intent(in) :: DOP(N_Psp,Ndom)      ! Labile Dissolved organic phosporius (DOP: umol L-1)
    real(8), intent(in) :: POP(N_Psp,Npom)      ! Particulate organic phosporius (POP: umol L-1)
    real(8), intent(in) :: PhyC(N_Csp,Nphy)     ! phytoplankton C biomass (umol C L-1), dinoflagellate
    real(8), intent(in) :: ZooC(N_Csp,Nzoo)     ! zooplankton C biomass (umol C L-1)
    real(8), intent(in) :: PhyN(N_Nsp,Nphy)     ! phytoplankton N biomass (umol N L-1), dinoflagellate
    real(8), intent(in) :: ZooN(N_Nsp,Nzoo)     ! zooplankton N biomass (umol N L-1)
    real(8), intent(in) :: PhyP(N_Psp,Nphy)     ! phytoplankton P biomass (umol P L-1), dinoflagellate
    real(8), intent(in) :: ZooP(N_Psp,Nzoo)     ! zooplankton P biomass (umol P L-1)
    real(8), intent(in) :: PIC (N_Csp,Npim)     ! Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
# if defined BLUE_TIDE         
    real(8), intent(in) :: H2S(N_Ssp)           ! H2S(N_Ssp): (umol S L-1)
# endif
# if defined COT_STARFISH         
    real(8), intent(in) :: COTe           ! COT starfish egg (umol L-1)
    real(8), intent(in) :: COTl           ! COT starfish larvae (umol L-1)
# endif
!   output parameters
    real(8), intent(out) :: dDOx_dt              ! dDOx/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dTA_dt               ! dTA/dt   (umol kg-1 s-1) 
    real(8), intent(out) :: dDIC_dt(N_Csp)       ! dDIC/dt  (umol kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    real(8), intent(out) :: dNO3_dt(N_Nsp)       ! dNO3/dt (umol L-1 s-1)
    real(8), intent(out) :: dNH4_dt(N_Nsp)       ! dNH4/dt (umol L-1 s-1)
    real(8), intent(out) :: dPO4_dt(N_Psp)       ! dPO4/dt (umol L-1 s-1)
    real(8), intent(out) :: dDOC_dt(N_Csp,Ndom)  ! dDOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPOC_dt(N_Csp,Npom)  ! dPOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dDON_dt(N_Nsp,Ndom)  ! dDON/dt (umol L-1 s-1)
    real(8), intent(out) :: dPON_dt(N_Nsp,Npom)  ! dPON/dt (umol L-1 s-1)
    real(8), intent(out) :: dDOP_dt(N_Psp,Ndom)  ! dDOP/dt (umol L-1 s-1)
    real(8), intent(out) :: dPOP_dt(N_Psp,Npom)  ! dPOP/dt (umol L-1 s-1)
    real(8), intent(out) :: dPhyC_dt(N_Csp,Nphy) ! dPhyC/dt  (umolC L-1 s-1)  
    real(8), intent(out) :: dZooC_dt(N_Csp,Nzoo) ! dZooC/dt  (umolC L-1 s-1)  
    real(8), intent(out) :: dPhyN_dt(N_Nsp,Nphy) ! dPhyN/dt  (umolN L-1 s-1)  
    real(8), intent(out) :: dZooN_dt(N_Nsp,Nzoo) ! dZooN/dt  (umolN L-1 s-1)  
    real(8), intent(out) :: dPhyP_dt(N_Psp,Nphy) ! dPhyP/dt  (umolP L-1 s-1)  
    real(8), intent(out) :: dZooP_dt(N_Psp,Nzoo) ! dZooP/dt  (umolP L-1 s-1)  
    real(8), intent(out) :: dPIC_dt (N_Csp,Npim) ! dPIC/dt  (umol L-1 s-1) 
# if defined BLUE_TIDE         
    real(8), intent(out) :: dH2Sdt(N_Ssp)        ! dH2S/dt  (umol S L-1 s-1)
# endif
# if defined COT_STARFISH
    real(8), intent(out) :: dCOTe_dt      ! dCOTe/dt (umol L-1 s-1)
    real(8), intent(out) :: dCOTl_dt      ! dCOTl/dt (umol L-1 s-1)
# endif
    real(8), intent(out) :: pH             ! pH
    real(8), intent(out) :: Wcal           ! Calcite saturation state
    real(8), intent(out) :: Warg           ! Aragonite saturation state

!!!------------Set parameters  ----------------------------------

!------- Phytoplankton parameters ------------------------
!
!  Phytoplankton photosynthesis Parameter         
!                                       Dnfl      Dtm        Ccl      Cya
!  Maximum photosynthetic rate ( s-1) (0.4d0 d-1; Kishi et al., 2007)0.3     (!!!Tuning parameters!!!) 
    real(8), parameter :: Pp_max(4)= [ 5.0d-5,   1.0d-4,  1.0d-5,   3.5d-5 ]  ! (s-1)   JS_edit check reference values for all parameters
!  Parameters for temperature dependency on photosynthetic rate
!    fp_temp = A_Pp*exp(-Ea_Pp/Rg/TK)/(1.0d0+exp((TK*dS_Pp-dH_Pp)/Rg/TK))
    real(8), parameter :: A_Pp (4) = [3.0d11,   3.0d11,   3.0d11,   3.0d11 ]     ! !!! Tuning
    real(8), parameter :: Ea_Pp(4) = [6.5d4 ,   6.5d4 ,   6.5d4 ,   6.5d4  ]     ! !!! Tuning
    real(8), parameter :: dS_Pp(4) = [1.02d3,   1.02d3,   1.02d3,   1.02d3 ]     ! !!! Tuning
    real(8), parameter :: dH_Pp(4) = [3.07d5,   3.07d5,   3.07d5,   3.07d5 ]     ! !!! Tuning
!  Temperature coefficient for PhyC photosynthesis (0.063; Kawamiya et al., 1995)          
!    real(8), parameter :: b_Pp(4) = [ 0.0693d0, 0.0693d0, 0.0693d0, 0.0693d0 ]  ! (degC-1) 
!  Optimum light intensity (48.83d0 J m2 s-1; Kawamiya et al., 1995)     
    real(8), parameter :: Ip(4)    = [ 90.0d0,   90.0d0,   90.0d0,   90.0d0   ] ! (umol m-2 s-1) JS_edit check reference values

!  Phytoplankton Nutrient Assimilation Parameter         
!                                           Dnfl       Dtm      Ccl     Cya
!   Maximum N uptake rate normalized by maximum PO4 updatke rate.
!    nVmax_NH4 = Vmax_NH4/Vmax_PO4;  nVmax_NO3 = Vmax_NO3/Vmax_PO4
    real(8), parameter :: nVmax_NH4(4) = [ 5.0d0, 20.0d0, 10.0d0,   5.0d0 ]     ! (no dim.) 
    real(8), parameter :: nVmax_NO3(4) = [ 5.0d0, 15.0d0, 15.0d0, 10.0d0 ]      ! (no dim.)
!   Half-saturation constants.           
    real(8), parameter :: Kp_NH4(4)    = [ 0.1d0,    0.5d0 ,  0.5d0, 0.1d0 ]      ! (µmol L-1)  
    real(8), parameter :: Kp_NO3(4)    = [ 1.0d0,    0.1d0 ,   0.1d0,  1.0d0  ]      ! (µmol L-1)
    real(8), parameter :: Kp_PO4(4)    = [ 5.0d-2,    5.0d-2,  1.0d-2, 1.0d-2 ]      ! (µmol L-1)  (!!!Tuning parameters!!!)
!   Coefficients for Hill equation.  
!    V = K**n_Hill/(K**n_Hill + C**n_Hill), if n_Hill=1 -> Michaelis-Menten equation    
    real(8), parameter :: nNH4_Hill(4)   = [ 2.0d0,    1.0d0,    1.0d0,  1.0d0 ]      ! (µmol L-1)  (!!!Tuning parameters!!!)
    real(8), parameter :: nNO3_Hill(4)   = [ 1.0d0,    2.0d0,    1.0d0,  1.0d0 ]      ! (µmol L-1)  (!!!Tuning parameters!!!)
    real(8), parameter :: nPO4_Hill(4)   = [ 1.0d0,    1.0d0,    1.0d0,  1.0d0 ]      ! (µmol L-1)  (!!!Tuning parameters!!!)

!  Respiration rate at 0 oC
    real(8), parameter :: k_Rp(4) = [ 3.0d-7,   5.0d-7,   1.0d-7,   1.0d-7   ]    ! (s-1)    (!!!Tuning parameters!!!)
!  Temperature coefficient for PhyC respiration rate (0.03d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Rp(4) = [ 0.0519d0, 0.0519d0, 0.0519d0, 0.0519d0 ] ! (degC-1)

!  Mortality rate at 0 oC (0.00562d0 umol-1 d-1; 0.0585L/umolN/day? 0.0088; Kishi et al., 2001)0.0066     
    real(8), parameter :: k_Mp(4) = [ 1.0d-7,   5.0d-7,   6.5d-8,   6.5d-8   ] ! (L umol-1 s-1)
!  Temperature coefficient for PhyC mortality (Kawamiya et al., 1995)
    real(8), parameter :: b_Mp(4) = [ 0.069d0,  0.069d0,  0.069d0,  0.069d0  ] ! (degC-1)   
    real(8), parameter :: rLPOMphy = 0.42d0  ! (Labile POM)/(Total POM) of phytoplankton tissue (no dim.) 
!  PhyC ratio of extracellular excretion to production 0.135(Kawamiya et al., 1995)    
    real(8), parameter :: k_Ep(4) = [ 1.0d-8,  2.0d-8,  1.0d-8,  1.0d-8  ] ! (no dim.) 
    real(8), parameter :: rLDOMphy = 0.42d0  ! (Labile DOM)/(Total DOM) of phytoplankton excreted DOM (no dim.) 
!------- Zooplankton parameters ------------------------
!                                          from     Dnfl       Dtm        Ccl         Cya
    real(8), parameter :: k_Gp2z(4,1) = reshape( [ 1.0d-6,    1.0d-6,    1.0d-8 ,    1.0d-8    ], [4,1] ) ! to Gzoo! (s-1)          Maximum grazing rate of PhyC1 by ZooC at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)0.16
    real(8), parameter :: b_Gp2z(4,1) = reshape( [ 0.0693d0,  0.0693d0,  0.0693d0,   0.0693d0  ], [4,1] ) ! to Gzoo! (degC-1)       Temperature coefficient for ZooC grazing (0.063d0degC-1;Kawamiya et al., 1995)
    real(8), parameter :: e_Gp2z(4,1) = reshape( [ 0.15d0,    0.15d0,    0.15d0,     0.15d0    ], [4,1] ) ! to Gzoo! (no dim.)      Assimilation efficiency of ZooC (0.7:Kawamiya et al., 1995)
    real(8), parameter :: Kz_phy(4,1) = reshape( [ 1.0d0,     10.0d0,     0.1d0,      0.1d0     ], [4,1] ) ! (umol C L-1)       PhyC1 threshold value for grazing by ZooC (0.04umolN/L0.265; Kishi et al., 2007)

    real(8), parameter :: k_Rz(1) = [ 6.0d-8    ]  ! (s-1)            zooplankton respiration rate at 0  oC  																					!!!(Tuning) Check
    real(8), parameter :: b_Rz(1) = [ 0.0693d0  ]  ! (degC-1)         Temperature coefficient for ZooC respiration rate (Kawamiya et al., 1995)
    real(8), parameter :: k_Mz(1) = [ 5.0d-8    ]  ! (L umol-1 s-1)   zooplankton mortality rate at 0 oC 3.0d0/86400.0d0(0.0088d0 umol-1 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Mz(1) = [ 0.0693d0  ]  ! (degC-1)         Temperature coefficient for ZooC mortality (Kawamiya et al., 1995)
    real(8), parameter :: rLPOMzoo = 0.42d0  ! (Labile POM)/(Total POM) of zooplankton tissue (no dim.) 

!------- Microbial loop parameters  -------
!                                            from     LDOM      RDOM
    real(8), parameter :: k_Gdoc2z(2,1) = reshape( [ 1.0d-8,    0.0d0    ], [2,1] ) ! to Gzoo ! (s-1)          Maximum grazing rate of DOC by ZooC at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)
!                                            from     LDOM      RDOM
    real(8), parameter :: b_Gdoc2z(2,1) = reshape( [ 0.0693d0,  0.0693d0 ], [2,1] ) ! to Gzoo ! (degC-1)       Temperature coefficient of DOC grazing by ZooC (Kawamiya et al., 1995)

!                                            from     LPOM      RPOM      CPOM
    real(8), parameter :: k_Gpoc2z(3,1) = reshape( [ 1.0d-7,   1.0d-7,    1.0d-9   ], [3,1] ) ! to Gzoo ! (s-1)          Maximum grazing rate of POC by ZooC at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)
!                                            from     LPOM      RPOM      CPOM
    real(8), parameter :: b_Gpoc2z(3,1) = reshape( [ 0.0693d0, 0.0693d0,  0.0693d0 ], [3,1] ) ! to Gzoo ! (degC-1)       Temperature coefficient of DOC grazing by ZooC (Kawamiya et al., 1995)
!                                            from     LDOM      RDOM
    real(8), parameter :: e_Gdoc2z(2,1) = reshape( [ 0.010d0,   0.010d0  ], [2,1] )        ! to Gzoo
!                                            from     LPOM      RPOM      CPOM
    real(8), parameter :: e_Gpoc2z(3,1) = reshape( [ 0.010d0,   0.010d0,  0.010d0   ], [3,1] ) ! to Gzoo ! (no dim.)      Assimilation efficiency of ZooC (0.7:Kawamiya et al., 1995)

!------- Decomposition parameters (CPOM -> Detritus) --------------------
    real(8), parameter :: k_Dcpoc2dtr = 1.0d-8    ! (s-1)     Decomposition rate of CPOM to Detritus at 0 oC
    real(8), parameter :: b_Dcpoc2dtr = 0.0693d0  ! (degC-1)  Temperature coefficient for decomposition of CPOM to Detritus
    real(8), parameter :: k_Dcpon2dtr = 1.0d-8    ! (s-1)     Decomposition rate of CPOM to Detritus at 0 oC
    real(8), parameter :: b_Dcpon2dtr = 0.0693d0  ! (degC-1)  Temperature coefficient for decomposition of CPOM to Detritus
    real(8), parameter :: k_Dcpop2dtr = 1.0d-8    ! (s-1)     Decomposition rate of CPOM to Detritus at 0 oC
    real(8), parameter :: b_Dcpop2dtr = 0.0693d0  ! (degC-1)  Temperature coefficient for decomposition of CPOM to Detritus

!------- Nitrogen fixation parameters  --------------------  !!!! To be updated !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real(8), parameter :: k_Nfx = 1.0d-9    ! (s-1)     Cyanobacteria N biomass specific Nitrogen fixation rate at 0 oC
    real(8), parameter :: b_Nfx = 0.0693d0  ! (degC-1)  Temperature coefficient for Nitrogen fixation

!------- Local variables --------------------
    real(8) :: Pp, Rp, Mp, Ep, Ap
    real(8) :: Ap_DIC, Ap_NH4, Ap_NO3, Ap_PO4
    real(8) :: Gp2z, Gp2z_p, dCp2z, dNp2z, dPp2z, Rz, Mz
    real(8) :: Gdoc2z, Gpoc2z
    real(8) :: Ddoc, Dpoc, Dp2doc
    real(8) :: Dcpoc2dtr
    real(8) :: CO3, Dissol

  !  real(8) :: Ddon(Ndom), Dpon(Npom), Dp2don(Npom,Ndom)
  !  real(8) :: Ddop(Ndom), Dpop(Npom), Dp2dop(Npom,Ndom)
    real(8) :: fp_temp, TK
    real(8) :: nVp_NH4, nVp_NO3
    real(8) :: nVp_DIN
    real(8) :: nVp_PO4
    real(8) :: Dcpon2dtr, Dcpop2dtr
    real(8) :: Nfx

    real(8) :: rDIC(N_Csp)      
    real(8) :: rNO3(N_Nsp)      
    real(8) :: rNH4(N_Nsp)      
    real(8) :: rPO4(N_Psp)      
    real(8) :: rDOC(N_Csp,Ndom) 
    real(8) :: rPOC(N_Csp,Npom) 
    real(8) :: rDON(N_Nsp,Ndom) 
    real(8) :: rPON(N_Nsp,Npom) 
    real(8) :: rDOP(N_Psp,Ndom) 
    real(8) :: rPOP(N_Psp,Npom) 
    real(8) :: rPhyC(N_Csp,Nphy)
    real(8) :: rZooC(N_Csp,Nzoo)
    real(8) :: rPhyN(N_Nsp,Nphy)
    real(8) :: rZooN(N_Nsp,Nzoo)
    real(8) :: rPhyP(N_Psp,Nphy)
    real(8) :: rZooP(N_Psp,Nzoo)
    real(8) :: rPIC (N_Csp,Npim) 
# if defined BLUE_TIDE         
    real(8) :: rH2S(N_Ssp)
# endif

    real(8) :: rN2  (N_Nsp)      
    
    integer :: k,m,isp
    real(8) :: cf1, cf2, cf3
    real(8) :: cNO3, cNH4, cPO4, cPhyC, cZooC
    real(8) :: vC(N_Csp), vN(N_Nsp), vP(N_Psp)

! Tne following variables are not used now.
    real(8) :: Mn2                  ! Mn2+ (umol L-1) 
    real(8) :: MnO2                 ! MnO2 (nmol g-1 s-1) *solid
    real(8) :: Fe2                  ! Fe2+ (umol L-1) 
    real(8) :: FeS(N_Ssp)           ! FeS (nmol g(DW)-1 s-1)) *solid
    real(8) :: FeS2(N_Ssp)          ! FeS2 (nmol g(DW)-1 s-1)) *solid
    real(8) :: FeOOH                ! FeOOH (nmol g(DW)-1 s-1)) *solid
    real(8) :: FeOOH_PO4(N_Psp)     ! FeOOH=PO4 (nmol g(DW)-1 s-1)) *solid
# ifndef BLUE_TIDE  
    real(8) :: H2S(N_Ssp)           ! H2S (umol L-1) 
# endif
    real(8) :: S0(N_Ssp)            ! S (nmol g(DW)-1 s-1)) *solid
    real(8) :: SO4(N_Ssp)           ! SO42- (umol L-1) 
    real(8) :: dMn2_dt              ! d[Mn2+]/dt (umol L-1 s-1)
    real(8) :: dMnO2_dt             ! d[MnO2]/dt (nmol g(DW)-1 s-1) *solid
    real(8) :: dFe2_dt              ! d[Fe2+]/dt (umol L-1 s-1)
    real(8) :: dFeS_dt(N_Ssp)       ! d[FeS]/dt (nmol g(DW)-1 s-1) *solid
    real(8) :: dFeS2_dt(N_Ssp)      ! d[FeS2]/dt (nmol g(DW)-1 s-1) *solid
    real(8) :: dFeOOH_dt            ! d[FeOOH]/dt (nmol g(DW)-1 s-1) *solid
    real(8) :: dFeOOH_PO4_dt(N_Psp) ! d[FeOOH=O4]/dt (nmol g(DW)-1 s-1) *solid
# ifndef BLUE_TIDE  
    real(8) :: dH2S_dt(N_Ssp)       ! d[H2S]/dt (umol L-1 s-1)
# endif
    real(8) :: dS0_dt(N_Ssp)        ! d[S]/dt (nmol g(DW)-1 s-1) *solid
    real(8) :: dSO4_dt(N_Ssp)       ! d[SO42-]/dt (umol L-1 s-1)

# if defined FOODWEB_TESTMODE
!  Output
    real(8), save :: dsec(4) = 0.d0 !sec
# endif

!===== Initiallize tracer parameters =================================
    TK = Tmp + 273.15d0 !(K)

    dDOx_dt = 0.0d0
    dTA_dt  = 0.0d0
    dDIC_dt(:) = 0.0d0
    dNO3_dt(:) = 0.0d0
    dNH4_dt(:) = 0.0d0
    dPO4_dt(:) = 0.0d0
    dPOC_dt(:,:) = 0.0d0
    dDOC_dt(:,:) = 0.0d0
    dDON_dt(:,:) = 0.0d0
    dPON_dt(:,:) = 0.0d0
    dDOP_dt(:,:) = 0.0d0
    dPOP_dt(:,:) = 0.0d0
    dPhyC_dt(:,:) = 0.0d0
    dZooC_dt(:,:) = 0.0d0
    dPhyN_dt(:,:) = 0.0d0
    dZooN_dt(:,:) = 0.0d0
    dPhyP_dt(:,:) = 0.0d0
    dZooP_dt(:,:) = 0.0d0
    dPIC_dt(:,:) = 0.0d0
# if defined COT_STARFISH
    dCOTe_dt = 0.0d0
    dCOTl_dt = 0.0d0
# endif
    dMn2_dt           = 0.0d0
    dMnO2_dt          = 0.0d0
    dFe2_dt           = 0.0d0
    dFeS_dt(:)        = 0.0d0
    dFeS2_dt(:)       = 0.0d0
    dFeOOH_dt         = 0.0d0
    dFeOOH_PO4_dt(:)  = 0.0d0
    dH2S_dt(:)        = 0.0d0
    dS0_dt(:)         = 0.0d0
    dSO4_dt(:)        = 0.0d0

!===== Calculate istope ratio =================================
    DO isp=1,N_Csp
      rDIC(isp) = Iso_ratio( DIC(isp), DIC(iCt) )
    ENDDO
    DO isp=1,N_Nsp
      rNO3(isp) = Iso_ratio( NO3(isp), NO3(iNt) )
      rNH4(isp) = Iso_ratio( NH4(isp), NH4(iNt) )
    ENDDO
    DO isp=1,N_Psp
      rPO4(isp) = Iso_ratio( PO4(isp), PO4(iPt) )
    ENDDO
    DO m = 1,Ndom
      DO isp=1,N_Csp
        rDOC(isp,m) = Iso_ratio( DOC(isp,m), DOC(iCt,m) )  
      ENDDO
      DO isp=1,N_Nsp
        rDON(isp,m) = Iso_ratio( DON(isp,m), DON(iNt,m) )   
      ENDDO
      DO isp=1,N_Psp
        rDOP(isp,m) = Iso_ratio( DOP(isp,m), DOP(iPt,m) )
      ENDDO
    ENDDO   
    DO m = 1,Npom
      DO isp=1,N_Csp
        rPOC(isp,m) = Iso_ratio( POC(isp,m), POC(iCt,m) ) 
      ENDDO
      DO isp=1,N_Nsp
        rPON(isp,m) = Iso_ratio( PON(isp,m), PON(iNt,m) )   
      ENDDO
      DO isp=1,N_Psp
        rPOP(isp,m) = Iso_ratio( POP(isp,m), POP(iPt,m) )
      ENDDO
    ENDDO   
    DO m = 1,Nphy
      DO isp=1,N_Csp
        rPhyC(isp,m) = Iso_ratio( PhyC(isp,m), PhyC(iCt,m) ) 
      ENDDO
      DO isp=1,N_Nsp
        rPhyN(isp,m) = Iso_ratio( PhyN(isp,m), PhyN(iNt,m) ) 
      ENDDO
      DO isp=1,N_Psp
        rPhyP(isp,m) = Iso_ratio( PhyP(isp,m), PhyP(iPt,m) ) 
      ENDDO
    ENDDO   
    DO m = 1,Nzoo
      DO isp=1,N_Csp
        rZooC(isp,m) = Iso_ratio( ZooC(isp,m), ZooC(iCt,m) ) 
      ENDDO
      DO isp=1,N_Nsp
        rZooN(isp,m) = Iso_ratio( ZooN(isp,m), ZooN(iNt,m) ) 
      ENDDO
      DO isp=1,N_Psp
        rZooP(isp,m) = Iso_ratio( ZooP(isp,m), ZooP(iPt,m) ) 
      ENDDO
    ENDDO   
    DO m = 1,Npim
      DO isp=1,N_Csp
        rPIC(isp,m) = Iso_ratio( PIC(isp,m), PIC(iCt,m) ) 
      ENDDO
    ENDDO   
# if defined NITROGEN_ISOTOPE
    rN2(iN15)= 0.003663d0
    rN2(iNt) = 1.0d0
# else
    rN2(:)   = 0.0d0
    rN2(iNt) = 1.0d0
# endif
!===== Phytoplankton reaction =================================
    DO k=1, Nphy
      cPhyC = MAX(PhyC(iCt,k),0.0d0)
!----- Gross photosynthetic rate (umolC L-1 s-1) -----------------

     !--- Temperature dependency on photosynthetic rate ----
      fp_temp = A_Pp(k)*exp(-Ea_Pp(k)/Rg/TK)/(1.0d0+exp((TK*dS_Pp(k)-dH_Pp(k))/Rg/TK))

!      Pp(k) = Pp_max(k) * exp(b_Pp(k)*Tmp) * tanh(PFD/Ip(k)) * MAX(PhyC(iCt,k), 0.0d0)
      Pp = Pp_max(k) * fp_temp * tanh(PFD/Ip(k)) * cPhyC
      IF(DIC(iCt) <= 0.0d0) THEN !-----For Error handling
        Pp = 0.0d0
      ENDIF
      Pp = MAX( Pp, 0.0d0 )  !!! Error handling

!----- Assimilation rate (umolC L-1 s-1) ----------------- 

      cNH4 = MAX( NH4(iNt), 0.0d0)  !!! Error handling
      cNO3 = MAX( NO3(iNt), 0.0d0)  !!! Error handling
      cPO4 = MAX( PO4(iPt), 0.0d0)  !!! Error handling
      
      ! --- Hill equation (nC_Hill=1 -> Michaelis-Menten equation) -----
      nVp_NH4 =  nVmax_NH4(k)*cNH4**nNH4_Hill(k)/ &
                              ( cNH4**nNH4_Hill(k) + Kp_NH4(k)**nNH4_Hill(k) )
      nVp_NO3 =  nVmax_NO3(k)*cNO3**nNO3_Hill(k)/ &
                              ( cNO3**nNO3_Hill(k) + Kp_NO3(k)**nNO3_Hill(k) )
      nVp_PO4 =               cPO4**nPO4_Hill(k)/ &
                              ( cPO4**nPO4_Hill(k) + Kp_PO4(k)**nPO4_Hill(k) )

      ! --- Liebig's low of minimmum -----
      !   Carbon based assimilation rate
      Ap = MIN( (nVp_NH4+nVp_NO3)/rNCp(k)*rPCp(k), nVp_PO4 )

      Ap = Ap * Pp

      if(nVp_NH4 + nVp_NO3 > 0.0d0) then
        Ap_NO3 = Ap*rNCp(k)* nVp_NO3/( nVp_NH4 + nVp_NO3 )
        Ap_NH4 = Ap*rNCp(k)* nVp_NH4/( nVp_NH4 + nVp_NO3 )
      else
        Ap_NO3 = 0.0d0
        Ap_NH4 = 0.0d0
      endif
      Ap_PO4 = Ap*rPCp(k)

# if defined FOODWEB_TESTMODE
!------------------------------------------------------------------------
! Print section (for Phy(k))
      IF( date*86400.0d0 >= dsec(k) ) THEN
        if(dsec(k) == 0.0d0) then
          dsec(k)=date*86400.0d0+ECOSYS_OUTPUT_INTERVAL*60.0d0
          ! Write lavels at the first step
          write(69+k,'(*(g0:,", "))') 'Time', 'k', 'PFD', 'Tmp', 'fp_temp'      &
          , 'Pp', '(nVp_NH4+nVp_NO3)/rNCp(k)*rPCp(k)', 'nVp_PO4'  &
          , 'Ap', 'Ap_NO3', 'Ap_NH4', 'Ap_PO4'        
       else
          dsec(k)=dsec(k)+ECOSYS_OUTPUT_INTERVAL*60.0d0
        endif
  
        ! Write data
        write(69+k,'(*(g0:,", "))') date, k, PFD, Tmp, fp_temp      &
           , Pp, (nVp_NH4+nVp_NO3)/rNCp(k)*rPCp(k), nVp_PO4       &
           , Ap, Ap_NO3, Ap_NH4, Ap_PO4        
      END IF
!-----------------------------------------------------------------------
# endif
  
!     === Mass balance ===

      dDIC_dt(:)    = dDIC_dt(:)    - Ap     *aC_phot(1:N_Csp)*rDIC(:)  ! Primary production    
      dPhyC_dt(:,k) = dPhyC_dt(:,k) + Ap     *aC_phot(1:N_Csp)*rDIC(:)  ! Primary production
      dDOx_dt       = dDOx_dt       + Ap

      dNO3_dt(:)    = dNO3_dt(:)    - Ap_NO3 *aNO3_phot(1:N_Nsp)*rNO3(:)
      dNH4_dt(:)    = dNH4_dt(:)    - Ap_NH4 *aNH4_phot(1:N_Nsp)*rNH4(:)
      dPhyN_dt(:,k) = dPhyN_dt(:,k) + Ap_NO3 *aNO3_phot(1:N_Nsp)*rNO3(:) &
                                    + Ap_NH4 *aNH4_phot(1:N_Nsp)*rNH4(:)
      dDOx_dt       = dDOx_dt       + Ap_NO3*2.0d0  ! Assimilating ammonium is assumed: NO3- + H2O + 2H+ --> NH4+ + 2O2 
      dTA_dt        = dTA_dt        + Ap_NO3*1.0d0  ! increase of alkalinity by 1 mole when nitrate or nitrite is the N source
      dTA_dt        = dTA_dt        - Ap_NH4*1.0d0  ! decrease of alkalinity by 1 mole when ammonia is used

      dPO4_dt(:)    = dPO4_dt(:)    - Ap_PO4 *rPO4(:)
      dPhyP_dt(:,k) = dPhyP_dt(:,k) + Ap_PO4 *rPO4(:)
      dTA_dt        = dTA_dt        + Ap_PO4*2.0d0  ! increases alkalinity by 2 mole per mole HPO42-

      dDIC_dt(:)       = dDIC_dt(:)       - Ap*rCaCp(k) *aC_calc(1:N_Csp)*rDIC(:)  ! calcification
      dPIC_dt(:,iLive) = dPIC_dt(:,iLive) + Ap*rCaCp(k) *aC_calc(1:N_Csp)*rDIC(:)  ! calcification
      dTA_dt           = dTA_dt           - Ap*rCaCp(k)*2.0d0                     ! calcification

!----- Excretion rate (umolC L-1 s-1) -----------------

!      Ep(k) = (Pp(k) - Ap(k))*k_Ep(k) 
!      Ep(k) = Pp(k) - Ap(k)
      Ep = k_Ep(k)*cPhyC  !!!!**************

!     === Mass balance ===

!      dDOx_dt = dDOx_dt + Ep(k)  !!!!!!**************
!      dDIC_dt(:) = dDIC_dt(:) - Ep(k) *aC_phot(1:N_Csp)*rDIC(:)
!      dDOC_dt(:,iLDOM) = dDOC_dt(:,iLDOM) + Ep(k)*rLDOMphy *aC_phot(1:N_Csp)*rDIC(:)  !!!!!!**************
!      dDOC_dt(:,iRDOM) = dDOC_dt(:,iRDOM) + Ep(k)*(1.0d0-rLDOMphy) *aC_phot(1:N_Csp)*rDIC(:)  !!!!!!**************
      dPhyC_dt(:,k)    = dPhyC_dt(:,k)    - Ep                          *rPhyC(:,k)
      dDOC_dt(:,iLDOM) = dDOC_dt(:,iLDOM) + Ep        * rLDOMphy        *rPhyC(:,k)
      dDOC_dt(:,iRDOM) = dDOC_dt(:,iRDOM) + Ep        *(1.0d0-rLDOMphy) *rPhyC(:,k)

      dPhyN_dt(:,k)    = dPhyN_dt(:,k)    - Ep*rNCp(k)                  *rPhyN(:,k)
      dDON_dt(:,iLDOM) = dDON_dt(:,iLDOM) + Ep*rNCp(k)* rLDOMphy        *rPhyN(:,k)
      dDON_dt(:,iRDOM) = dDON_dt(:,iRDOM) + Ep*rNCp(k)*(1.0d0-rLDOMphy) *rPhyN(:,k)

      dPhyP_dt(:,k)    = dPhyP_dt(:,k)    - Ep*rPCp(k)                  *rPhyP(:,k)
      dDOP_dt(:,iLDOM) = dDOP_dt(:,iLDOM) + Ep*rPCp(k)* rLDOMphy        *rPhyP(:,k)
      dDOP_dt(:,iRDOM) = dDOP_dt(:,iRDOM) + Ep*rPCp(k)*(1.0d0-rLDOMphy) *rPhyP(:,k)

      dPIC_dt(:,iLive) = dPIC_dt(:,iLive) - Ep*rCaCp(k) *rPIC(:,iLive) !!! Dissolution (not realistic, but for keeping OrgC/CaCO3(living) ratio) 
      dDIC_dt(:)       = dDIC_dt(:)       + Ep*rCaCp(k) *rPIC(:,iLive)
      dTA_dt           = dTA_dt           + Ep*rCaCp(k)*2.0d0

!----- Respiration rate (umolC L-1 s-1) -----------------

      Rp = k_Rp(k) * exp(b_Rp(k)*Tmp) * cPhyC

!     === Mass balance ===

      dDOx_dt          = dDOx_dt          - Rp
      dPhyC_dt(:,k)    = dPhyC_dt(:,k)    - Rp         *rPhyC(:,k)
      dDIC_dt(:)       = dDIC_dt(:)       + Rp         *rPhyC(:,k)

      dPhyN_dt(:,k)    = dPhyN_dt(:,k)    - Rp*rNCp(k) *rPhyN(:,k)
      dNH4_dt(:)       = dNH4_dt(:)       + Rp*rNCp(k) *rPhyN(:,k)
      dTA_dt           = dTA_dt           + Rp*rNCp(k)*1.0d0

      dPhyP_dt(:,k)    = dPhyP_dt(:,k)    - Rp*rPCp(k) *rPhyP(:,k)
      dPO4_dt(:)       = dPO4_dt(:)       + Rp*rPCp(k) *rPhyP(:,k)
      dTA_dt           = dTA_dt           - Rp*rPCp(k)*2.0d0

      dPIC_dt(:,iLive) = dPIC_dt(:,iLive) - Rp*rCaCp(k) *rPIC(:,iLive)  !!! Dissolution (not realistic, but for keeping OrgC/CaCO3(living) ratio) 
      dDIC_dt(:)       = dDIC_dt(:)       + Rp*rCaCp(k) *rPIC(:,iLive)  !!! Dissolution (not realistic, but for keeping OrgC/CaCO3(living) ratio)
      dTA_dt           = dTA_dt           + Rp*rCaCp(k)*2.0d0

!----- Mortality (umolC L-1 s-1) -----------------

      Mp = k_Mp(k) * exp(b_Mp(k)*Tmp) * cPhyC

!     === Mass balance ===

      dPhyC_dt(:,k)    = dPhyC_dt(:,k)    - Mp                        *rPhyC(:,k)
      dPOC_dt(:,iLPOM) = dPOC_dt(:,iLPOM) + Mp* rLPOMphy              *rPhyC(:,k)
      dPOC_dt(:,iRPOM) = dPOC_dt(:,iRPOM) + Mp*(1d0-rLPOMphy)         *rPhyC(:,k)

      dPhyN_dt(:,k)    = dPhyN_dt(:,k)    - Mp               *rNCp(k) *rPhyN(:,k)
      dPON_dt(:,iLPOM) = dPON_dt(:,iLPOM) + Mp*rLPOMphy      *rNCp(k) *rPhyN(:,k)
      dPON_dt(:,iRPOM) = dPON_dt(:,iRPOM) + Mp*(1d0-rLPOMphy)*rNCp(k) *rPhyN(:,k)

      dPhyP_dt(:,k)    = dPhyP_dt(:,k)    - Mp               *rPCp(k) *rPhyP(:,k)
      dPOP_dt(:,iLPOM) = dPOP_dt(:,iLPOM) + Mp* rLPOMphy     *rPCp(k) *rPhyP(:,k)
      dPOP_dt(:,iRPOM) = dPOP_dt(:,iRPOM) + Mp*(1d0-rLPOMphy)*rPCp(k) *rPhyP(:,k)

      dPIC_dt(:,iLive) = dPIC_dt(:,iLive) - Mp*rCaCp(k)               *rPIC(:,iLive) 
      dPIC_dt(:,iDead) = dPIC_dt(:,iDead) + Mp* rCaCp(k)              *rPIC(:,iLive)

    END DO

!===== Zooplankton reaction ======================================

    DO k=1, Nzoo
      cZooC = MAX(ZooC(iCt,k),0.0d0) !!! Error handling
!----- Grazing rate of PhyC by ZooC (umolC L-1 s-1) ----------------- JS_edit Difficult part: Mixotrophy from Dinoflagellates

      DO m=1, Nphy
        cPhyC = MAX(PhyC(iCt,m),0.0d0) !!! Error handling

        Gp2z_p = k_Gp2z(m,k) * exp(b_Gp2z(m,k)*Tmp)    &
          * (cPhyC/(Kz_phy(m,k) + cPhyC)) * cZooC

        ! This routine corrects the mismatch of C:N:P stoichiometry between Phy and Zoo
        !   when mass is transferred from Phy to Zoo.         
        Gp2z = MIN( Gp2z_p, Gp2z_p/rNCz(k), Gp2z_p/rPCz(k) )
        dCp2z = Gp2z_p         - Gp2z
        dNp2z = Gp2z_p*rNCp(m) - Gp2z*rNCz(k)
        dPp2z = Gp2z_p*rPCp(m) - Gp2z*rPCz(k)


!     === Mass balance ===

        dPhyC_dt(:,m)    = dPhyC_dt(:,m)    - Gp2z_p                                   *rPhyC(:,m)
        dZooC_dt(:,k)    = dZooC_dt(:,k)    + Gp2z* e_Gp2z(m,k)*aC_grz(1:N_Csp)        *rPhyC(:,m)
        dPOC_dt(:,iLPOM) = dPOC_dt(:,iLPOM) +(Gp2z*(1.0d0-e_Gp2z(m,k)*aC_grz(1:N_Csp))             &
                                              + dCp2z)                                 *rPhyC(:,m)
        
        dPhyN_dt(:,m)    = dPhyN_dt(:,m)    - Gp2z_p*rNCp(m)                                   *rPhyN(:,m)
        dZooN_dt(:,k)    = dZooN_dt(:,k)    + Gp2z*rNCz(k)* e_Gp2z(m,k)*aN_grz(1:N_Nsp)        *rPhyN(:,m)
        dPON_dt(:,iLPOM) = dPON_dt(:,iLPOM) +(Gp2z*rNCz(k)*(1.0d0-e_Gp2z(m,k)*aN_grz(1:N_Nsp))             &
                                              + dNp2z)                                         *rPhyN(:,m)

        dPhyP_dt(:,m)    = dPhyP_dt(:,m)    - Gp2z_p*rPCp(m)                   *rPhyP(:,m)
        dZooP_dt(:,k)    = dZooP_dt(:,k)    + Gp2z*rPCz(k)* e_Gp2z(m,k)        *rPhyP(:,m)
        dPOP_dt(:,iLPOM) = dPOP_dt(:,iLPOM) +(Gp2z*rPCz(k)*(1.0d0-e_Gp2z(m,k))             &
                                              + dPp2z)                         *rPhyP(:,m)

        dPIC_dt(:,iLive) = dPIC_dt(:,iLive) - Gp2z_p*rCaCp(m) *rPIC(:,iLive)
        dPIC_dt(:,iDead) = dPIC_dt(:,iDead) + Gp2z_p*rCaCp(m) *rPIC(:,iLive)

      END DO

!!!------- Microbial loop (implicitly assumed) ---------------

!----- Grazing rate of DOC(iLDOM) by ZooC (umolC L-1 s-1) -----------------

      DO m=1, Ndom

        cf1 =  MIN( DOC(iCt,m), DON(iNt,m)/rNCz(k) , DOP(iPt,m)/rPCz(k) )
        cf1 =  MAX( cf1,0.0d0 )  !!! Error handling
        Gdoc2z = k_Gdoc2z(m,k) * exp(b_Gdoc2z(m,k)*Tmp) * cf1 * cZooC ! yt_edit consider making this michaelis-menten dependent on ZooC to not exceed exceed 100% uptake
        Gdoc2z = MAX( Gdoc2z, 0.0d0 )  !!! Error handling

!     === Mass balance ===

        dDOC_dt(:,m)     = dDOC_dt(:,m)     - Gdoc2z                                       *rDOC(:,m)
        dZooC_dt(:,k)    = dZooC_dt(:,k)    + Gdoc2z*e_Gdoc2z(m,k)*aC_grz(1:N_Csp)         *rDOC(:,m)
        dDOC_dt(:,iLDOM) = dDOC_dt(:,iLDOM) + Gdoc2z*(1.0d0-e_Gdoc2z(m,k)*aC_grz(1:N_Csp)) *rDOC(:,m)

        dDON_dt(:,m)     = dDON_dt(:,m)     - Gdoc2z*rNCz(k)                                       *rDON(:,m)
        dZooN_dt(:,k)    = dZooN_dt(:,k)    + Gdoc2z*rNCz(k)*e_Gdoc2z(m,k)*aN_grz(1:N_Nsp)         *rDON(:,m)
        dDON_dt(:,iLDOM) = dDON_dt(:,iLDOM) + Gdoc2z*rNCz(k)*(1.0d0-e_Gdoc2z(m,k)*aN_grz(1:N_Nsp)) *rDON(:,m)
        
        dDOP_dt(:,m)     = dDOP_dt(:,m)     - Gdoc2z*rPCz(k)                       *rDOP(:,m)
        dZooP_dt(:,k)    = dZooP_dt(:,k)    + Gdoc2z*rPCz(k)*e_Gdoc2z(m,k)         *rDOP(:,m)
        dDOP_dt(:,iLDOM) = dDOP_dt(:,iLDOM) + Gdoc2z*rPCz(k)*(1.0d0-e_Gdoc2z(m,k)) *rDOP(:,m)
      END DO

!----- Grazing rate of POC by ZooC (umolC L-1 s-1) -----------------

      DO m=1, Npom

        cf1 = MIN( POC(iCt,m), PON(iNt,m)/rNCz(k), POP(iPt,m)/rPCz(k) )
        cf1 = MAX( cf1, 0.0d0 )  !!! Error handling
        Gpoc2z = k_Gpoc2z(m,k) * exp(b_Gpoc2z(m,k)*Tmp) * cf1 * cZooC
        Gpoc2z = MAX( Gpoc2z, 0.0d0 )  !!! Error handling

!     === Mass balance ===

        dPOC_dt(:,m)     = dPOC_dt(:,m)     - Gpoc2z                                       *rPOC(:,m)
        dZooC_dt(:,k)    = dZooC_dt(:,k)    + Gpoc2z* e_Gpoc2z(m,k)*aC_grz(1:N_Csp)        *rPOC(:,m)
        dPOC_dt(:,iLPOM) = dPOC_dt(:,iLPOM) + Gpoc2z*(1.0d0-e_Gpoc2z(m,k)*aC_grz(1:N_Csp)) *rPOC(:,m)

        dPON_dt(:,m)     = dPON_dt(:,m)     - Gpoc2z*rNCz(k)                                       *rPON(:,m)
        dZooN_dt(:,k)    = dZooN_dt(:,k)    + Gpoc2z*rNCz(k)* e_Gpoc2z(m,k)*aN_grz(1:N_Nsp)        *rPON(:,m)
        dPON_dt(:,iLPOM) = dPON_dt(:,iLPOM) + Gpoc2z*rNCz(k)*(1.0d0-e_Gpoc2z(m,k)*aN_grz(1:N_Nsp)) *rPON(:,m)

        dPOP_dt(:,m)     = dPOP_dt(:,m)     - Gpoc2z*rPCz(k)                       *rPOP(:,m)
        dZooP_dt(:,k)    = dZooP_dt(:,k)    + Gpoc2z*rPCz(k)* e_Gpoc2z(m,k)        *rPOP(:,m)
        dPOP_dt(:,iLPOM) = dPOP_dt(:,iLPOM) + Gpoc2z*rPCz(k)*(1.0d0-e_Gpoc2z(m,k)) *rPOP(:,m)
      END DO
  
!----- Respiration rate (umolC L-1 s-1) -----------------

      Rz = k_Rz(k) * exp(b_Rz(k)*Tmp) * cZooC

!     === Mass balance ===

      dDOx_dt       = dDOx_dt       - Rz
      dZooC_dt(:,k) = dZooC_dt(:,k) - Rz         *rZooC(:,k)
      dDIC_dt (:)   = dDIC_dt (:)   + Rz         *rZooC(:,k)

      dZooN_dt(:,k) = dZooN_dt(:,k) - Rz*rNCz(k) *rZooN(:,k)
      dNH4_dt (:)   = dNH4_dt (:)   + Rz*rNCz(k) *rZooN(:,k)
      dTA_dt        = dTA_dt        + Rz*rNCz(k)*1.0d0

      dZooP_dt(:,k) = dZooP_dt(:,k) - Rz*rPCz(k) *rZooP(:,k)
      dPO4_dt (:)   = dPO4_dt (:)   + Rz*rPCz(k) *rZooP(:,k)
      dTA_dt        = dTA_dt        - Rz*rPCz(k)*2.0d0
  
!----- Mortality (umolC L-1 s-1) -----------------

      Mz = k_Mz(k) * exp(b_Mz(k)*Tmp) * cZooC

!     === Mass balance ===

      dZooC_dt(:,k)     = dZooC_dt(:,k)     - Mz                        *rZooC(:,k)
      dPOC_dt (:,iLPOM) = dPOC_dt (:,iLPOM) + Mz        * rLPOMzoo      *rZooC(:,k)
      dPOC_dt (:,iRPOM) = dPOC_dt (:,iRPOM) + Mz        *(1d0-rLPOMzoo) *rZooC(:,k)

      dZooN_dt(:,k)     = dZooN_dt(:,k)     - Mz*rNCz(k)                *rZooN(:,k)
      dPON_dt (:,iLPOM) = dPON_dt (:,iLPOM) + Mz*rNCz(k)* rLPOMzoo      *rZooN(:,k)
      dPON_dt (:,iRPOM) = dPON_dt (:,iRPOM) + Mz*rNCz(k)*(1d0-rLPOMzoo) *rZooN(:,k)

      dZooP_dt(:,k)     = dZooP_dt(:,k)     - Mz*rPCz(k)                *rZooP(:,k)
      dPOP_dt (:,iLPOM) = dPOP_dt (:,iLPOM) + Mz*rPCz(k)* rLPOMzoo      *rZooP(:,k)
      dPOP_dt (:,iRPOM) = dPOP_dt (:,iRPOM) + Mz*rPCz(k)*(1d0-rLPOMzoo) *rZooP(:,k)
    END DO


!===== Decomposition ==================================================

!----- Decomposition rate DOM and POM (umol L-1 s-1) -----------------
    Mn2          = 0.0d0   ! Mn2+ (umol L-1) 
    MnO2         = 0.0d0   ! MnO2 (nmol g-1 s-1) *solid
    Fe2          = 0.0d0   ! Fe2+ (umol L-1) 
    FeS(:)       = 0.0d0   ! FeS (nmol g(DW)-1 s-1)) *solid
    FeS2(:)      = 0.0d0   ! FeS2 (nmol g(DW)-1 s-1)) *solid
    FeOOH        = 0.0d0   ! FeOOH (nmol g(DW)-1 s-1)) *solid
    FeOOH_PO4(:) = 0.0d0   ! FeOOH=PO4 (nmol g(DW)-1 s-1)) *solid
# ifndef BLUE_TIDE  
    H2S(:)       = 0.0d0   ! H2S (umol L-1) 
# endif
    S0(:)        = 0.0d0   ! S (nmol g(DW)-1 s-1)) *solid
    SO4(iSt)     = 0.02824d0*Sal/35.0d0*rho_sw *1.0d6   ! SO42- (umol L-1) 
!   SO4(iS34) = SO4(iSt)

    CALL decomposition  &
    ! input parameters
    ( 1                 &   ! 0: Sediment column, 1: Water column
    , Tmp               &   ! Temperature (oC)
    , Sal               &   ! Salinity (PSU)
    , 0.0d0             &   ! Dry soil density (g cm-3)
    , 1.0d0             &   ! Soil porosity (= Vwater/(Vwater+Vsed) (non-dimensional: 0.0-1.0)
    , DOx               &   ! Dissolved oxygen (umol L-1)
    , TA                &   ! Total alkalinity (TA: umol kg-1)
    , DIC(:)            &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
    , NO3(:)            &   ! NO3 (umol L-1)
    , NH4(:)            &   ! NH4 (umol L-1)
    , PO4(:)            &   ! PO4 (umol L-1)
    , DOC(:,:)          &   ! DOC(Ndom): Dissolved organic carbon (DOC: umol L-1)
    , POC(:,:)          &   ! POC(Npom): Particulate organic carbon (POC: umol L-1 or nmol g(DW)-1 *solid)
    , DON(:,:)          &   ! Dissolved organic nitrogen (DON: umol L-1)
    , PON(:,:)          &   ! Particulate organic nitrogen (PON: umol L-1 or nmol g(DW)-1 *solid)
    , DOP(:,:)          &   ! Dissolved organic phosporius (DOP: umol L-1)
    , POP(:,:)          &   ! Particulate organic phosporius (POP: umol L-1 or nmol g(DW)-1 *solid)
    , PIC(:,iDead)      &   ! PIC (N_Csp,Npim): Particulate inorganic carbon (PIC: umolC L-1), coccolith (CaCO3)
    , Mn2               &   ! Mn2+ (umol L-1) 
    , MnO2              &   ! MnO2 (nmol g-1) *solid
    , Fe2               &   ! Fe2+ (umol L-1) 
    , FeS(:)            &   ! FeS (nmol g(DW)-1) *solid
    , FeS2(:)           &   ! FeS2 (nmol g(DW)-1) *solid
    , FeOOH             &   ! FeOOH (nmol g(DW)-1) *solid
    , FeOOH_PO4(:)      &   ! FeOOH=PO4 (nmol g(DW)-1) *solid
    , H2S(:)            &   ! H2S (umol L-1) 
    , S0(:)             &   ! S (nmol g(DW)-1) *solid
    , SO4(:)            &   ! SO42- (umol L-1) 
   ! input and output parameters
    , dDOx_dt           &   ! d[O2]/dt  (umol L-1 s-1) 
    , dTA_dt            &   ! dTA/dt  (umol L-1 s-1) 
    , dDIC_dt(:)        &   ! dDIC/dt (umol L-1 s-1) 
    , dNO3_dt(:)        &   ! d[NO3]/dt (umol L-1 s-1)
    , dNH4_dt(:)        &   ! d[NH4]/dt (umol L-1 s-1)
    , dPO4_dt(:)        &   ! d[PO4]/dt (umol L-1 s-1)
    , dDOC_dt(:,:)      &   ! dDOC/dt (umol L-1 s-1) 
    , dPOC_dt(:,:)      &   ! dPOC/dt (umol L-1 s-1) or (nmol g(DW)-1) *solid
    , dDON_dt(:,:)      &   ! dDON/dt (umol L-1 s-1)
    , dPON_dt(:,:)      &   ! dPON/dt (umol L-1 s-1) or (nmol g(DW)-1) *solid
    , dDOP_dt(:,:)      &   ! dDOP/dt (umol L-1 s-1)
    , dPOP_dt(:,:)      &   ! dPOP/dt (umol L-1 s-1) or (nmol g(DW)-1) *solid
    , dPIC_dt(:,iDead)  &   ! d CaCO3/dt (umol L-1 s-1) or (nmol g(DW)-1) *solid
    , dMn2_dt           &   ! d[Mn2+]/dt (umol L-1 s-1)
    , dMnO2_dt          &   ! d[MnO2]/dt (nmol g(DW)-1 s-1) *solid
    , dFe2_dt           &   ! d[Fe2+]/dt (umol L-1 s-1)
    , dFeS_dt(:)        &   ! d[FeS]/dt (nmol g(DW)-1 s-1) *solid
    , dFeS2_dt(:)       &   ! d[FeS2]/dt (nmol g(DW)-1 s-1) *solid
    , dFeOOH_dt         &   ! d[FeOOH]/dt (nmol g(DW)-1 s-1) *solid
    , dFeOOH_PO4_dt(:)  &   ! d[FeOOH=O4]/dt (nmol g(DW)-1 s-1) *solid
    , dH2S_dt(:)        &   ! d[H2S]/dt (umol L-1 s-1)
    , dS0_dt(:)         &   ! d[S]/dt (nmol g(DW)-1 s-1) *solid
    , dSO4_dt(:)        &   ! d[SO42-]/dt (umol L-1 s-1)
    ! output parameters
    , pH                &   ! pH (total scale)
    , Wcal              &   ! Calcite saturation state
    , Warg              &   ! Aragonite saturation state
    )


!===== Carbonate dissolution ==============================================

!!!!!! Need pressure correction (not impremented) !!!!!!
!    pH     = pH_fromATCT( TA, DIC(iCt),TK, Sal )   ! pH
!    CO3   = cCO3_fromCTpH( DIC(iCt), pH, TK, Sal )  ! CO32- concentration
!    Wcal   = Wcal_fromcCO3( CO3, TK, Sal )  ! Calcite saturation state
!    Warg   = Warg_fromcCO3( CO3, TK, Sal )  ! Calcite saturation state
!    IF ( Wcal < 1.0d0 ) THEN !!! CHECK: Keir 1979 !!!!!!
!      Dissol = 1.3d7*(1.d0-Wcal)**2.8d0 * PIC(iCt,iDead)  !! [umol L-1 s-1] Low-Mg Calcite dissolution rate (Walter and Morse, 1985)
!    ELSE
!      Dissol = 0.0d0
!    END IF
!    Dissol = MAX( Dissol, 0.0d0 )  !!! Error handling
!
!!     === Mass balance ===
!
!    dPIC_dt(:,iDead) = dPIC_dt(:,iDead) - Dissol*rPIC(:,iDead)
!    dDIC_dt(:)       = dDIC_dt(:)       + Dissol*rPIC(:,iDead)
!    dTA_dt = dTA_dt + Dissol*2.0d0

!#ifdef DEBUG_TEST  /*!!! To chcek mass balance !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

!===== Nitrogen fixation ==============================================

    Nfx = k_Nfx * exp(b_Nfx*Tmp) * MAX( PhyN(iNt,iCya), 0.0d0 )  !!!! To be updated !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

    !     === Mass balance ===

    dNH4_dt(:) = dNH4_dt(:) + Nfx *aN_N2fix(1:N_Nsp) *rN2(:)
    dTA_dt     = dTA_dt     + Nfx
  

!===== Unit adjustment ===========================================

    dDIC_dt(:) = dDIC_dt(:)/rho_sw !(umol L-1 s-1) -> (umol kg-1 s-1)
    dTA_dt     = dTA_dt/rho_sw     !(umol L-1 s-1) -> (umol kg-1 s-1)

!#endif  /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
      
    RETURN

  END SUBROUTINE foodweb
#endif
END MODULE mod_foodweb


