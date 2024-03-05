
!!!=== Copyright (c) 2012-2024 Takashi NAKAMURA  =====

#include "cppdefs.h"


!!!**** MODULE OF ECOSYSTEM MODEL ************************************

MODULE mod_reef_ecosys
contains
!!! **********************************************************************
!!!  Set initial conditions for reef ecosystem model
!!! **********************************************************************
  SUBROUTINE initialize_reef_ecosys         &
    (ng, Ngrids, LBi, UBi, LBj, UBj         &
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
#ifdef SEDIMENT_ECOSYS
    , Nsed                                  &   ! Number of vertical biological sediment layers
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add
    )
#ifdef CORAL_POLYP
    USE mod_coral
#endif
#ifdef SEAGRASS
    USE mod_seagrass
#endif
#ifdef MACROALGAE
    USE mod_macroalgae
#endif
#ifdef SEDIMENT_ECOSYS
    USE mod_sedecosys
#endif
      
    implicit none

    integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
#ifdef SEDIMENT_ECOSYS
    integer, intent(in) :: Nsed                            ! Number of vertical biological sediment layers
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

#ifdef CORAL_POLYP
    CALL initialize_coral(ng, Ngrids, LBi, UBi, LBj, UBj)
#endif
#ifdef SEAGRASS
    CALL initialize_seagrass(ng, Ngrids, LBi, UBi, LBj, UBj)
#endif
#ifdef MACROALGAE
    CALL initialize_macroalgae(ng, Ngrids, LBi, UBi, LBj, UBj)
#endif
#ifdef SEDIMENT_ECOSYS
    CALL initialize_sedecosys(ng, Ngrids, LBi, UBi, LBj, UBj, Nsed) ! yuta_edits_for_masa
#endif
      
    RETURN
  END SUBROUTINE initialize_reef_ecosys


!!! **********************************************************************
!!!  Main program of reef ecosystem model
!!! **********************************************************************

  SUBROUTINE reef_ecosys         &
!        input parameters
    ( ng, i, j       &   ! ng: nested grid number; i,j: position
    , N              &   ! Number of vertical grid (following ROMS vertical grid)
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
#ifdef SEDIMENT_ECOSYS
    , Nsed           &   ! Number of vertical biological sediment layers
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add
    , isplitc        &   ! Internal loop counts of coral polyp model
    , isplitsed      &   ! Internal loop counts of sediment ecosystem model
    , dt             &   ! Time step (sec)
    , dz             &   ! dz(N): vertical grid size (m)
    , PFDsurf        &   ! Sea surface photon flux density (umol m-2 s-1)
    , tau            &   ! bottom shear stress (N m-2)
    , pCO2air        &   ! Air CO2 pertial pressure (uatm)
    , U10            &   ! wind speed (m s-1)
#ifdef CORAL_POLYP
    , p_coral        &   ! Coral coverage (0-1)
#endif
#ifdef SEAGRASS
    , p_sgrass       &   ! seagrass coverage (0-1)
#endif
#ifdef MACROALGAE
    , p_algae        &   ! algal coverage (0-1)
#endif
#ifdef SEDIMENT_ECOSYS
    , p_sand         &   ! sediment coverage (0-1)
#endif
    , Tmp_in            &   ! Tmp(N): Temperature (oC)
    , Sal_in            &   ! Sal(N): Salinity (PSU)
    , DIC_in            &   ! DIC(N): Total dissolved inorganic carbon (DIC: umol kg-1)
    , TA_in             &   ! TA (N): Total alkalinity (TA: umol kg-1)
    , DOx_in            &   ! DOx(N): Dissolved oxygen (umol L-1)
#if defined ORGANIC_MATTER
    , DOC_in            &   ! DOC(N,Ndom): Labile Dissolved organic carbon (DOC: umol L-1)
    , POC_in            &   ! POC (N,:): Particulate organic carbon (POC: umol L-1)
    , PHY_in            &   ! PHY(N,:): phytoplankton1 (umol C L-1), dinoflagellate
    , ZOO_in            &   ! ZOO (N,:): zooplankton (umol C L-1)
    , PIC_in            &   ! PIC (N,:): Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
# if defined FOODWEB_TESTMODE
    , DeadPHY_in        &   ! DeadPHY(Nphy): Dead phytoplankton (umol C L-1)
# endif
#endif
#if defined CARBON_ISOTOPE
    , DI13C_in          &   ! DI13C (N): 13C of DIC (umol kg-1)
# if defined ORGANIC_MATTER
    , DO13C_in          &   ! DO13C (N): 13C of Labile Dissolved organic carbon (DOC: umol L-1)
    , PO13C_in          &   ! PO13C (N): 13C of Particulate organic carbon (POC: umol L-1)
    , PHY13C_in         &   ! PHY13C(N): 13C of phytoplankton1 (umol C L-1), dinoflagellate
    , ZOO13C_in         &   ! ZOO13C(N): 13C of zooplankton (umol C L-1)
    , PI13C_in          &   ! PI13C (N): 13C of Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
# endif
#endif
#if defined NUTRIENTS            
    , NO3_in            &   ! NO3(N): NO3 (umol L-1)
    , NH4_in            &   ! NH4(N): NH4 (umol L-1)
    , PO4_in            &   ! PO4(N): PO4 (umol L-1)
# if defined ORGANIC_MATTER
    , DON_in            &   ! DON(N): Labile Dissolved organic nitrogen (DON: umol L-1)
    , PON_in            &   ! PON(N): Particulate organic nitrogen (PON: umol L-1)
    , DOP_in            &   ! DOP(N): Labile Dissolved organic phosporius (DOP: umol L-1)
    , POP_in            &   ! POP(N): Particulate organic phosporius (POP: umol L-1)
# endif
# if defined NITROGEN_ISOTOPE
    , NO3_15N_in        &   ! NO3_15N(N): 15N of NO3 (umol L-1)
    , NH4_15N_in        &   ! NH4_15N(N): 15N of NH4 (umol L-1)
#  if defined ORGANIC_MATTER
    , DO15N_in          &   ! LDO15N  (N): 15N of Labile Dissolved organic nitrogen (DON: umol L-1)
    , PO15N_in          &   ! PO15N   (N): 15N of Particulate organic nitrogen (PON: umol L-1)
    , PHY15N_in         &   ! PHY15N(N): 15N of phytoplankton1 (umol C L-1), dinoflagellate
    , ZOO15N_in         &   ! ZOO15N (N): 15N of zooplankton (umol C L-1)
#  endif
# endif
#endif
#if defined COT_STARFISH         
    , COTe_in           &   ! COTe(N): COT starfish egg (umol L-1)
    , COTl_in           &   ! COTl(N): COT starfish larvae (umol L-1)
#endif
!   output parameters
    , dDIC_dt        &   ! dDIC_dt(N): dDIC/dt (umol kg-1 s-1)
    , dTA_dt         &   ! dTA_dt (N): dTA/dt (umol kg-1 s-1)
    , dDOx_dt        &   ! dDOx_dt(N): dDO/dt (umol L-1 s-1)
#if defined ORGANIC_MATTER
    , dDOC_dt        &   ! dDOC_dt(N): dDOC/dt  (umol L-1 s-1) 
    , dPOC_dt        &   ! dPOC_dt (N): dPOC/dt  (umol L-1 s-1) 
    , dPHY_dt        &   ! dPHY1_dt(N): dPHY1/dt  (umol L-1 s-1)  
    , dZOO_dt        &   ! dZOO_dt (N): dZOO/dt  (umol L-1 s-1)  
    , dPIC_dt        &   ! dPIC_dt (N): dPIC/dt  (umol L-1 s-1)
# if defined FOODWEB_TESTMODE
    , dDeadPHY_dt    &   ! dDeadPHY_dt(N): dDeadPHY/dt  (umol L-1 s-1)
# endif
#endif
#if defined CARBON_ISOTOPE
    , dDI13C_dt      &   ! dDI13C_dt   (N): dDI13C/dt (umol kg-1 s-1)
# if defined ORGANIC_MATTER
    , dDO13C_dt      &   ! dLDO13C_dt  (N): dLDO13C/dt  (umol L-1 s-1) 
    , dPO13C_dt      &   ! dPO13C_dt   (N): dPO13C/dt  (umol L-1 s-1) 
    , dPHY13C_dt     &   ! dPHY1_13C_dt(N): dPHY1_13C/dt  (umol L-1 s-1)  
    , dZOO13C_dt     &   ! dZOO_13C_dt (N): dZOO_13C/dt  (umol L-1 s-1)  
    , dPI13C_dt      &   ! dPI13C_dt   (N): dPI13C/dt  (umol L-1 s-1)  
# endif
#endif
#if defined NUTRIENTS          
    , dNO3_dt        &   ! dNO3_dt(N): dNO3/dt (umol L-1 s-1)
!    , dNO2_dt        &   ! dNO2_dt(N): dNO2/dt (umol L-1 s-1)
    , dNH4_dt        &   ! dNH4_dt(N): dNH4/dt (umol L-1 s-1)
    , dPO4_dt        &   ! dPO4_dt(N): dPO4/dt (umol L-1 s-1)
# if defined ORGANIC_MATTER
    , dDON_dt        &   ! dDON_d (N): dDON/dt (umol L-1 s-1)
    , dPON_dt        &   ! dPON_dt (N): dPON/dt (umol L-1 s-1)
    , dDOP_dt        &   ! dDOP_dt(N): dDOP/dt (umol L-1 s-1)
    , dPOP_dt        &   ! dPOP_dt (N): dPOP/dt (umol L-1 s-1)
# endif
# if defined NITROGEN_ISOTOPE
    , dNO3_15N_dt    &   ! dNO3_15N_dt(N): dNO3_15N/dt (umol L-1 s-1)
!    , dNO2_15N_dt    &   ! dNO2_15N_dt(N): dNO2_15N/dt (umol L-1 s-1)
    , dNH4_15N_dt    &   ! dNH4_15N_dt(N): dNH4_15N/dt (umol L-1 s-1)
#  if defined ORGANIC_MATTER
    , dDO15N_dt      &   ! dLDO15N_dt  (N): dLDO15N/dt  (umol L-1 s-1) 
    , dPO15N_dt      &   ! dPO15N_dt   (N): dPO15N/dt  (umol L-1 s-1) 
    , dPHY15N_dt     &   ! dPHY1_15N_dt(N): dPHY1_15N/dt  (umol L-1 s-1)  
    , dZOO15N_dt     &   ! dZOO_15N_dt (N): dZOO_15N/dt  (umol L-1 s-1)  
#  endif
# endif
#endif
#if defined COT_STARFISH         
    , dCOTe_dt       &   ! dCOTe/dt(N): (umol L-1 s-1)
    , dCOTl_dt       &   ! dCOTl/dt(N): (umol L-1 s-1)
#endif
    , sspH           &   ! sea surface pH
    , ssfCO2         &   ! sea surface fCO2 (uatm)
    , ssWarg         &   ! sea surface aragonite saturation state
    , ssCO2flux      &   ! sea surface CO2 flux (mmol m-2 s-1)
    , ssO2flux       &   ! sea surface O2 flux (mmol m-2 s-1)
    , PFDbott        &   ! Bottom photon flux density (umol m-2 s-1)
    )                    
!
!-----------------------------------------------------------------------
!                                                                       
!                     rho point    Face                                 
!                       (i,j)                                           
!                    _____|______  _N    Surface                        
!                   /     |      /|                                     
!      z     Layer /___________ / |                                     
!                  |           |  |_N-1                                 
!     dz(N) {   N  |           | /|                                     
!                  |___________|/ : :                                   
!                  |           |  :      Water column                   
!               :  :           :  |_2   _________________               
!               :  :           : /|                    |                
!               :  |___________|/ |                    |------------     
!                  |           |  |_1                 _|           |    
!     dz(2) {   2  |           | /|                    | z_w(2)    | z_r(2)         
!                  |___________|/ |                    |           |    
!                  |           |  |_0  _ Bottom  ______|___________|     
!     dz(1) {   1  |           | /                                      
!                  |___________|/                                       
!                                                                       
!                                                                       
!      A vertical section of the ecosys grid showing water column.      
!-----------------------------------------------------------------------

    USE mod_geochem
#ifdef CORAL_POLYP
    USE mod_coral
#endif
#ifdef SEAGRASS
    USE mod_seagrass
#endif
#ifdef MACROALGAE
    USE mod_macroalgae
#endif
#ifdef SEDIMENT_ECOSYS
    USE mod_sedecosys
#endif
#ifdef FOODWEB
    USE mod_foodweb
#endif
      
    implicit none

! input parameters
    integer, intent(in) :: ng,i,j     
    integer, intent(in) :: N
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
#ifdef SEDIMENT_ECOSYS
    integer, intent(in) :: Nsed
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

    integer, intent(in) :: isplitc     
    integer, intent(in) :: isplitsed     
    real(8), intent(in) :: dt         
    real(8), intent(in) :: dz(N)      
    real(8), intent(in) :: PFDsurf    
    real(8), intent(in) :: tau        
    real(8), intent(in) :: pCO2air    
    real(8), intent(in) :: U10        
#ifdef CORAL_POLYP
    real(8), intent(in) :: p_coral(Ncl)
#endif
#ifdef SEAGRASS
    real(8), intent(in) :: p_sgrass   
#endif
#ifdef MACROALGAE
    real(8), intent(in) :: p_algae    
#endif
#ifdef SEDIMENT_ECOSYS
    real(8), intent(in) :: p_sand      
#endif

! raw tracer inputs
    real(8), intent(in) :: Tmp_in(N)        
    real(8), intent(in) :: Sal_in(N)        
    real(8), intent(in) :: DIC_in(N)        
    real(8), intent(in) :: TA_in (N)        
    real(8), intent(in) :: DOx_in(N)        
#if defined ORGANIC_MATTER
    real(8), intent(in) :: DOC_in(N,Ndom)           ! Labile Dissolved organic carbon (DOC: umol L-1)
    real(8), intent(in) :: POC_in(N,Npom)           ! Particulate organic carbon (POC: umol L-1)
    real(8), intent(in) :: PHY_in(N,Nphy)           ! phytoplankton1 (umol C L-1), dinoflagellate
    real(8), intent(in) :: ZOO_in(N,Nzoo)           ! zooplankton (umol C L-1)
    real(8), intent(in) :: PIC_in(N,Npim)           ! Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
# if defined FOODWEB_TESTMODE
    real(8), intent(in) :: DeadPHY_in(N,Nphy)  ! DeadPHY(Nphy): Dead phytoplankton (umol C L-1)
# endif
#endif
#if defined CARBON_ISOTOPE
    real(8), intent(in) :: DI13C_in(N)          ! 13C of DIC (umol kg-1)
# if defined ORGANIC_MATTER
    real(8), intent(in) :: DO13C_in (N,Ndom)         ! 13C of Labile Dissolved organic carbon (DOC: umol L-1)
    real(8), intent(in) :: PO13C_in (N,Npom)          ! 13C of Particulate organic carbon (POC: umol L-1)
    real(8), intent(in) :: PHY13C_in(N,Nphy)       ! 13C of phytoplankton1 (umol C L-1), dinoflagellate
    real(8), intent(in) :: ZOO13C_in(N,Nzoo)        ! 13C of zooplankton (umol C L-1)
    real(8), intent(in) :: PI13C_in (N,Npim)          ! 13C of Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
# endif
#endif
#if defined NUTRIENTS            
    real(8), intent(in) :: NO3_in(N)         
    real(8), intent(in) :: NH4_in(N)        
    real(8), intent(in) :: PO4_in(N)        
# if defined ORGANIC_MATTER
    real(8), intent(in) :: DON_in(N,Ndom)           ! Labile Dissolved organic nitrogen (DON: umol L-1)
    real(8), intent(in) :: PON_in(N,Npom)            ! Particulate organic nitrogen (PON: umol L-1)
    real(8), intent(in) :: DOP_in(N,Ndom)           ! Labile Dissolved organic phosporius (DOP: umol L-1)
    real(8), intent(in) :: POP_in(N,Npom)            ! Particulate organic phosporius (POP: umol L-1)
# endif
# if defined NITROGEN_ISOTOPE
    real(8), intent(in) :: NO3_15N_in(N)        ! 15N of NO3 (umol L-1)
    real(8), intent(in) :: NH4_15N_in(N)        ! 15N of NH4 (umol L-1)
#  if defined ORGANIC_MATTER
    real(8), intent(in) :: DO15N_in(N,Ndom)         ! 15N of Labile Dissolved organic nitrogen (DON: umol L-1)
    real(8), intent(in) :: PO15N_in(N,Npom)          ! 15N of Particulate organic nitrogen (PON: umol L-1)
    real(8), intent(in) :: PHY15N_in(N,Nphy)       ! 15N of phytoplankton1 (umol C L-1), dinoflagellate
    real(8), intent(in) :: ZOO15N_in(N,Nzoo)        ! 15N of zooplankton (umol C L-1)
#  endif
# endif
#endif
#if defined COT_STARFISH         
    real(8), intent(in) :: COTe_in(N)         ! COT starfish egg (umol L-1)
    real(8), intent(in) :: COTl_in(N)         ! COT starfish larvae (umol L-1)
#endif

! input with negatives corrected to zero
    real(8) :: Tmp(N)        
    real(8) :: Sal(N)        
    real(8) :: DIC(N)        
    real(8) :: TA (N)        
    real(8) :: DOx(N)        
#if defined ORGANIC_MATTER
    real(8) :: DOC(N,Ndom)           ! Labile Dissolved organic carbon (DOC: umol L-1)
    real(8) :: POC(N,Npom)            ! Particulate organic carbon (POC: umol L-1)
    real(8) :: PHY(N,Nphy)           ! phytoplankton1 (umol C L-1), dinoflagellate
    real(8) :: ZOO(N,Nzoo)            ! zooplankton (umol C L-1)
    real(8) PIC(N,Npim)            ! Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
# if defined FOODWEB_TESTMODE
    real(8) :: DeadPHY(N,Nphy)  ! DeadPHY(Nphy): Dead phytoplankton (umol C L-1)
# endif
#endif
#if defined CARBON_ISOTOPE
    real(8) :: DI13C(N)          ! 13C of DIC (umol kg-1)
# if defined ORGANIC_MATTER
    real(8) :: DO13C(N,Ndom)         ! 13C of Labile Dissolved organic carbon (DOC: umol L-1)
    real(8) :: PO13C(N,Npom)          ! 13C of Particulate organic carbon (POC: umol L-1)
    real(8) :: PHY13C(N,Nphy)       ! 13C of phytoplankton1 (umol C L-1), dinoflagellate
    real(8) :: ZOO13C(N,Nzoo)        ! 13C of zooplankton (umol C L-1)
    real(8) :: PI13C(N,Npim)          ! 13C of Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
# endif
#endif
#if defined NUTRIENTS            
    real(8) :: NO3(N)         
    real(8) :: NH4(N)        
    real(8) :: PO4(N)        
# if defined ORGANIC_MATTER
    real(8) :: DON(N,Ndom)           ! Labile Dissolved organic nitrogen (DON: umol L-1)
    real(8) :: PON(N,Npom)            ! Particulate organic nitrogen (PON: umol L-1)
    real(8) :: DOP(N,Ndom)           ! Labile Dissolved organic phosporius (DOP: umol L-1)
    real(8) :: POP(N,Npom)            ! Particulate organic phosporius (POP: umol L-1)
# endif
# if defined NITROGEN_ISOTOPE
    real(8) :: NO3_15N(N)        ! 15N of NO3 (umol L-1)
    real(8) :: NH4_15N(N)        ! 15N of NH4 (umol L-1)
#  if defined ORGANIC_MATTER
    real(8) :: DO15N(N,Ndom)         ! 15N of Labile Dissolved organic nitrogen (DON: umol L-1)
    real(8) :: PO15N(N,Npom)          ! 15N of Particulate organic nitrogen (PON: umol L-1)
    real(8) :: PHY15N(N,Nphy)       ! 15N of phytoplankton1 (umol C L-1), dinoflagellate
    real(8) :: ZOO15N(N,Nzoo)        ! 15N of zooplankton (umol C L-1)
#  endif
# endif
#endif
#if defined COT_STARFISH         
    real(8) :: COTe(N)         ! COT starfish egg (umol L-1)
    real(8) :: COTl(N)         ! COT starfish larvae (umol L-1)
#endif

! output parameters
    real(8), intent(out) :: dDIC_dt(N)     
    real(8), intent(out) :: dTA_dt(N)     
    real(8), intent(out) :: dDOx_dt(N)     
#if defined ORGANIC_MATTER
    real(8), intent(out) :: dDOC_dt(N,Ndom)      ! dDOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPOC_dt(N,Npom)       ! dPOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPHY_dt(N,Nphy)      ! dPHY1/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dZOO_dt(N,Nzoo)       ! dZOO/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dPIC_dt(N,Npim)       ! dPIC/dt  (umol L-1 s-1) 
# if defined FOODWEB_TESTMODE
    real(8), intent(out) :: dDeadPHY_dt(N,Nphy)  ! dDeadPHY/dt  (umol L-1 s-1)
# endif
#endif
#if defined CARBON_ISOTOPE
    real(8), intent(out) :: dDI13C_dt(N)     ! dDI13C/dt (umol kg-1 s-1)
# if defined ORGANIC_MATTER
    real(8), intent(out) :: dDO13C_dt(N,Ndom)    ! dLDO13C/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPO13C_dt(N,Npom)     ! dPO13C/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPHY13C_dt(N,Nphy)  ! dPHY1_13C/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dZOO13C_dt(N,Nzoo)   ! dZOO_13C/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dPI13C_dt(N,Npim)     ! dPI13C/dt  (umol L-1 s-1)  
# endif
#endif
#if defined NUTRIENTS           
    real(8), intent(out) :: dNO3_dt(N)     
!    real(8), intent(out) :: dNO2_dt(N)     
    real(8), intent(out) :: dNH4_dt(N)     
    real(8), intent(out) :: dPO4_dt(N)     
# if defined ORGANIC_MATTER
    real(8), intent(out) :: dDON_dt(N,Ndom)      ! dDON/dt (umol L-1 s-1)
    real(8), intent(out) :: dPON_dt(N,Npom)       ! dPON/dt (umol L-1 s-1)
    real(8), intent(out) :: dDOP_dt(N,Ndom)      ! dDOP/dt (umol L-1 s-1)
    real(8), intent(out) :: dPOP_dt(N,Npom)       ! dPOP/dt (umol L-1 s-1)
# endif
# if defined NITROGEN_ISOTOPE
    real(8), intent(out) :: dNO3_15N_dt(N)   ! dNO3_15N/dt (umol L-1 s-1)
!    real(8), intent(out) :: dNO2_15N_dt(N)   ! dNO2_15N/dt (umol L-1 s-1)
    real(8), intent(out) :: dNH4_15N_dt(N)   ! dNH4_15N/dt (umol L-1 s-1)
#  if defined ORGANIC_MATTER
    real(8), intent(out) :: dDO15N_dt(N,Ndom)    ! dLDO15N/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPO15N_dt(N,Npom)     ! dPO15N/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPHY15N_dt(N,Nphy)  ! dPHY1_15N/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dZOO15N_dt(N,Nzoo)   ! dZOO_15N/dt  (umol L-1 s-1)  
#  endif
# endif
#endif
#if defined COT_STARFISH         
    real(8), intent(out) :: dCOTe_dt(N)    ! dCOTe/dt (umol L-1 s-1)
    real(8), intent(out) :: dCOTl_dt(N)    ! dCOTl/dt (umol L-1 s-1)
#endif
    real(8), intent(out) :: sspH      
    real(8), intent(out) :: ssfCO2    
    real(8), intent(out) :: ssWarg    
    real(8), intent(out) :: ssCO2flux 
    real(8), intent(out) :: ssO2flux  
    real(8), intent(out) :: PFDbott   

    real(8), parameter :: AttSW  = 0.12d0     ! Light attenuation due to seawater [1/m], {0.04d0}.
    real(8), parameter :: AttChl = 0.02486d0  ! Light attenuation by chlorophyll [1/(mg_Chl m2)], {0.02486d0}.
#ifdef CORAL_POLYP
    real(8), parameter :: P2R(Ncl) = (/ 20.0d0, 2.5d0 /)  !!! Conversion factor from polyp scale to reef scale
!    real(8), parameter :: P2R(Ncl) = (/ 5.0d0, 3.0d0 /)  !!! Conversion factor from polyp scale to reef scale
                                    !*~9.0d0 convert projected area to coral surface area for branching Acropora (Naumann et al. 2009)
#endif
    
    real(8) :: dtc     ! Internal time step for coral polyp model (sec)
    real(8) :: dtsed   ! Internal time step for sediment ecosystem model (sec)
  
    integer :: k, m, itrc          
    integer :: yt_iter ! yt_debug
    integer :: icl, itic
    real(8) :: TmpK
    real(8) :: rho_sw(N)
    real(8) :: DOsatu    
    real(8) :: cCO2aq, cHCO3, cCO3, R13C
    real(8) :: PFD, Att, AttFac, ExpAtt, Itop
    real(8) :: cff, cff1, cff2, cff3, cff4, cff5

    ! real(8) :: Flux_Tmp, Flux_Sal
    real(8) :: Flux_DIC, Flux_TA,  Flux_DO, Flux_CO2sed, Flux_N2  ! yuta_edits_for_masa
#if defined ORGANIC_MATTER
    real(8) :: Flux_DOC(Ndom), Flux_POC(Npom)
    real(8) :: Flux_POCf, Flux_POCs   ! yuta_edits_for_masa
    real(8) :: Flux_PHY(Nphy), Flux_ZOO(Nzoo)  
    real(8) :: Flux_PIC(Npim)  
#endif
#if defined CARBON_ISOTOPE
    real(8) :: Flux_DI13C
# if defined ORGANIC_MATTER
    real(8) :: Flux_DO13C(Ndom), Flux_PO13C(Npom)
    real(8) :: Flux_PHY13C(Nphy), Flux_ZOO13C(Nzoo)  
    real(8) :: Flux_PI13C(Npim) 
# endif
#endif
#if defined NUTRIENTS           
    real(8) :: Flux_NO3, Flux_NH4, Flux_PO4  !, Flux_NO2
# if defined ORGANIC_MATTER
    real(8) :: Flux_DON(Ndom), Flux_PON(Npom)    
    real(8) :: Flux_DOP(Ndom), Flux_POP(Npom)    
    real(8) :: Flux_PONf, Flux_PONs   ! yuta_edits_for_masa
    real(8) :: Flux_POPf, Flux_POPs   ! yuta_edits_for_masa
# endif
# if defined NITROGEN_ISOTOPE
    real(8) :: Flux_NO3_15N, Flux_NH4_15N  !, Flux_NO2_15N
#  if defined ORGANIC_MATTER
    real(8) :: Flux_DO15N(Ndom), Flux_PO15N(Npom)
    real(8) :: Flux_PHY15N(Nphy), Flux_ZOO15N(Nzoo)  
#  endif
# endif
#endif
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
#if defined SULFATE
    real(8) :: Flux_Mn2, Flux_MnO2 
    real(8) :: Flux_Fe2, Flux_FeS, Flux_FeS2, Flux_FeOOH, Flux_FeOOH_PO4
    real(8) :: Flux_H2S, Flux_S0, Flux_SO4 ! ,Flux_HS, Flux_S2
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add
    real(8) :: dDIC(N), dTA(N),  dDOx(N)
#if defined ORGANIC_MATTER
    real(8) :: dDOC(N,Ndom), dPOC(N,Npom)
    real(8) :: dPHY(N,Nphy), dZOO(N,Nzoo)  
    real(8) :: dPIC(N,Npim)
# if defined FOODWEB_TESTMODE
    real(8) :: dDeadPHY(N,Nphy)
# endif 
#endif
#if defined CARBON_ISOTOPE
    real(8) :: dDI13C(N)
# if defined ORGANIC_MATTER
    real(8) :: dDO13C(N,Ndom), dPO13C(N,Npom)
    real(8) :: dPHY13C(N,Nphy), dZOO13C(N,Nzoo)  
    real(8) :: dPI13C(N,Npim) 
# endif
#endif
#if defined NUTRIENTS           
    real(8) :: dNO3(N), dNH4(N), dPO4(N)
# if defined ORGANIC_MATTER
    real(8) :: dDON(N,Ndom), dPON(N,Npom)
    real(8) :: dDOP(N,Ndom), dPOP(N,Npom)
# endif
# if defined NITROGEN_ISOTOPE
    real(8) :: dNO3_15N(N), dNH4_15N(N)
#  if defined ORGANIC_MATTER
    real(8) :: dDO15N(N,Ndom), dPO15N(N,Npom)
    real(8) :: dPHY15N(N,Nphy), dZOO15N(N,Nzoo)  
#  endif
# endif
#endif
#if defined COT_STARFISH         
    real(8) :: dCOTe(N), dCOTl(N)
#endif

#if defined ORGANIC_MATTER
    real(8) :: w_PM
    real(8) :: z_r(N)      
    real(8) :: z_w(0:N)  
    real(8) :: Fdep_POC(Npom)
    real(8) :: Fdep_PIC(Npim)
# if defined CARBON_ISOTOPE
    real(8) :: Fdep_PO13C(Npom)
    real(8) :: Fdep_PI13C(Npim)
# endif
# if defined NUTRIENTS
    real(8) :: Fdep_PON(Npom)
    real(8) :: Fdep_POP(Npom)
#  if defined NITROGEN_ISOTOPE
    real(8) :: Fdep_PO15N(Npom)
#  endif
# endif
#endif
#if defined ECOSYS_TESTMODE
!  Output
    real(8), parameter :: OUTPUT_INTERVAL = 5.0d0     ! Output interval (min)
    real(8), save :: time = 0.0d0 !sec
    real(8), save :: dsec = 0.0d0 !sec
#endif



!-----------------------------------------------------------------------
! Rewrite inputs to replace negative values with zeros
!-----------------------------------------------------------------------

    where ( Tmp_in > 0.0d0 )
      Tmp = Tmp_in
    else where
      Tmp = 0.0d0
    end where
    where ( Sal_in > 0.0d0 )
      Sal = Sal_in
    else where
      Sal = 0.0d0
    end where
    where ( DIC_in > 0.0d0 )
      DIC = DIC_in
    else where
      DIC = 0.0d0
    end where
    where ( TA_in > 0.0d0 )
      TA = TA_in
    else where
      TA = 0.0d0
    end where
    where ( DOx_in > 0.0d0 )
      DOx = DOx_in
    else where
      DOx = 0.0d0
    end where
#if defined ORGANIC_MATTER
    where ( DOC_in > 0.0d0 )
      DOC = DOC_in
    else where
      DOC = 0.0d0
    end where
    where ( POC_in > 0.0d0 )
      POC = POC_in
    else where
      POC = 0.0d0
    end where
    where ( PHY_in > 0.0d0 )
      PHY = PHY_in
    else where
      PHY = 0.0d0
    end where
    where ( ZOO_in > 0.0d0 )
      ZOO = ZOO_in
    else where
      ZOO = 0.0d0
    end where
    where ( PIC_in > 0.0d0 )
      PIC = PIC_in
    else where
      PIC = 0.0d0
    end where
# if defined FOODWEB_TESTMODE
    where ( DeadPHY_in > 0.0d0 )
      DeadPHY = DeadPHY_in
    else where
      DeadPHY = 0.0d0
    end where
# endif
#endif
#if defined CARBON_ISOTOPE
    where ( DI13C_in > 0.0d0 )
      DI13C = DI13C_in
    else where
      DI13C = 0.0d0
    end where
# if defined ORGANIC_MATTER
    where ( DO13C_in > 0.0d0 )
      DO13C = DO13C_in
    else where
      DO13C = 0.0d0
    end where
    where ( PO13C_in > 0.0d0 )
      PO13C = PO13C_in
    else where
      PO13C = 0.0d0
    end where
    where ( PHY13C_in > 0.0d0 )
      PHY13C = PHY13C_in
    else where
      PHY13C = 0.0d0
    end where
    where ( ZOO13C_in > 0.0d0 )
      ZOO13C = ZOO13C_in
    else where
      ZOO13C = 0.0d0
    end where
    where ( PI13C_in > 0.0d0 )
      PI13C = PI13C_in
    else where
      PI13C = 0.0d0
    end where
# endif
#endif
#if defined NUTRIENTS            
    where ( NO3_in > 0.0d0 )
      NO3 = NO3_in
    else where
      NO3 = 0.0d0
    end where
    where ( NH4_in > 0.0d0 )
      NH4 = NH4_in
    else where
      NH4 = 0.0d0
    end where
    where ( PO4_in > 0.0d0 )
      PO4 = PO4_in
    else where
      PO4 = 0.0d0
    end where
# if defined ORGANIC_MATTER
    where ( DON_in > 0.0d0 )
      DON = DON_in
    else where
      DON = 0.0d0
    end where
    where ( PON_in > 0.0d0 )
      PON = PON_in
    else where
      PON = 0.0d0
    end where
    where ( DOP_in > 0.0d0 )
      DOP = DOP_in
    else where
      DOP = 0.0d0
    end where
    where ( POP_in > 0.0d0 )
      POP = POP_in
    else where
      POP = 0.0d0
    end where
# endif
# if defined NITROGEN_ISOTOPE
    where ( NO3_15N_in > 0.0d0 )
      NO3_15N = NO3_15N_in
    else where
      NO3_15N = 0.0d0
    end where
    where ( NH4_15N_in > 0.0d0 )
      NH4_15N = NH4_15N_in
    else where
      NH4_15N = 0.0d0
    end where
#  if defined ORGANIC_MATTER
    where ( DO15N_in > 0.0d0 )
      DO15N = DO15N_in
    else where
      DO15N = 0.0d0
    end where
    where ( PO15N_in > 0.0d0 )
      PO15N = PO15N_in
    else where
      PO15N = 0.0d0
    end where
    where ( PHY15N_in > 0.0d0 )
      PHY15N = PHY15N_in
    else where
      PHY15N = 0.0d0
    end where
    where ( ZOO15N_in > 0.0d0 )
      ZOO15N = ZOO15N_in
    else where
      ZOO15N = 0.0d0
    end where
#  endif
# endif
#endif
#if defined COT_STARFISH         
    where ( COTe_in > 0.0d0 )
      COTe = COTe_in
    else where
      COTe = 0.0d0
    end where
    where ( COTl_in > 0.0d0 )
      COTl = COTl_in
    else where
      COTl = 0.0d0
    end where
#endif

!-----------------------------------------------------------------------
! Initialize all time difference values (d_XXX)
!-----------------------------------------------------------------------
      
    dtc = dt/isplitc                  ! Internal time step for coral polyp model (sec)
    dtsed = dt/isplitsed              ! Internal time step for sediment ecosystem model (sec)
    
    DO k=1,N

      rho_sw(k) = densSW( Tmp(k),Sal(k) )  ! Density of seawater [g/cm3]

      dDIC_dt(k) = 0.0d0
      dTA_dt(k)  = 0.0d0
      dDOx_dt(k) = 0.0d0
#if defined ORGANIC_MATTER
      dDOC_dt(k,:) = 0.0d0
      dPOC_dt(k,:) = 0.0d0
      dPHY_dt(k,:) = 0.0d0
      dZOO_dt(k,:) = 0.0d0
      dPIC_dt(k,:) = 0.0d0
#endif
#if defined CARBON_ISOTOPE
      dDI13C_dt(k) = 0.0d0
# if defined ORGANIC_MATTER
      dDO13C_dt(k,:) = 0.0d0
      dPO13C_dt(k,:) = 0.0d0
      dPHY13C_dt(k,:) = 0.0d0
      dZOO13C_dt(k,:) = 0.0d0
      dPI13C_dt(k,:) = 0.0d0
# endif
#endif
#if defined NUTRIENTS            
      dNO3_dt(k) = 0.0d0
!      dNO2_dt(k) = 0.0d0
      dNH4_dt(k) = 0.0d0
      dPO4_dt(k) = 0.0d0
# if defined ORGANIC_MATTER
      dDON_dt(k,:) = 0.0d0
      dPON_dt(k,:) = 0.0d0
      dDOP_dt(k,:) = 0.0d0
      dPOP_dt(k,:) = 0.0d0
# endif
# if defined NITROGEN_ISOTOPE
      dNO3_15N_dt(k) = 0.0d0
!      dNO2_15N_dt(k) = 0.0d0
      dNH4_15N_dt(k) = 0.0d0
#  if defined ORGANIC_MATTER
      dDO15N_dt(k,:) = 0.0d0
      dPO15N_dt(k,:) = 0.0d0
      dPHY15N_dt(k,:) = 0.0d0
      dZOO15N_dt(k,:) = 0.0d0
#  endif
# endif
#endif
    END DO
    sspH      = 8.0d0   ! sea surface pH
    ssfCO2    = 380.0d0   ! sea surface fCO2 (uatm)
    ssWarg    = 4.0d0   ! sea surface aragonite saturation state
    ssCO2flux = 0.0d0   ! sea surface CO2 flux (mmol m-2 s-1)
    ssO2flux  = 0.0d0   ! sea surface O2 flux (mmol m-2 s-1)
    PFDbott   = 0.0d0   ! Bottom photon flux density (umol m-2 s-1)

! !!! yt_debug >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!     DO k=1,N
!       ! if (isnan() .or. abs() > huge()) then
!       if (isnan(DIC(k)) .or. abs(DIC(k)) > 1.0d15 .or. DIC(k) < 0.0d0) then
!         write(*,*) 'yt_debug: mod_reef_ecosys.F DIC(k)    input      i =', i, '   j =', j, '   k =', k
!         write(*,*) 'yt_debug:     DIC(k) =', DIC(k)
!       endif
!       ! if (isnan() .or. abs() > huge()) then
!       if (isnan(TA(k)) .or. abs(TA(k)) > 1.0d15 .or. TA(k) < 0.0d0) then
!         write(*,*) 'yt_debug: mod_reef_ecosys.F TA(k)    input      i =', i, '   j =', j, '   k =', k
!         write(*,*) 'yt_debug:     TA(k) =', TA(k)
!       endif
!       ! if (isnan() .or. abs() > huge()) then
!       if (isnan(DOx(k)) .or. abs(DOx(k)) > 1.0d15 .or. DOx(k) < 0.0d0) then
!         write(*,*) 'yt_debug: mod_reef_ecosys.F DOx(k)    input      i =', i, '   j =', j, '   k =', k
!         write(*,*) 'yt_debug:     DOx(k) =', DOx(k)
!       endif
! #if defined ORGANIC_MATTER
!       DO yt_iter = 1, Ndom
!         ! if (isnan() .or. abs() > huge()) then
!         if (isnan(DOC(k,yt_iter)) .or. abs(DOC(k,yt_iter)) > 1.0d15 .or. DOC(k,yt_iter) < 0.0d0) then
!           write(*,*) 'yt_debug: mod_reef_ecosys.F DOC(k,Ndom)    input      i =', i, '   j =', j, '   k =', k, '   Ndom =', yt_iter
!           write(*,*) 'yt_debug:     DOC(k,Ndom) =', DOC(k,yt_iter)
!         endif
!       ENDDO
!       DO yt_iter = 1, Npom
!         ! if (isnan() .or. abs() > huge()) then
!         if (isnan(POC(k,yt_iter)) .or. abs(POC(k,yt_iter)) > 1.0d15 .or. POC(k,yt_iter) < 0.0d0) then
!           write(*,*) 'yt_debug: mod_reef_ecosys.F POC(k,Npom)    input      i =', i, '   j =', j, '   k =', k, '   Npom =', yt_iter
!           write(*,*) 'yt_debug:     POC(k,Npom) =', POC(k,yt_iter)
!         endif
!       ENDDO
!       DO yt_iter = 1, Nphy
!         ! if (isnan() .or. abs() > huge()) then
!         if (isnan(PHY(k,yt_iter)) .or. abs(PHY(k,yt_iter)) > 1.0d15 .or. PHY(k,yt_iter) < 0.0d0) then
!           write(*,*) 'yt_debug: mod_reef_ecosys.F PHY(k,Nphy)    input      i =', i, '   j =', j, '   k =', k, '   Nphy =', yt_iter
!           write(*,*) 'yt_debug:     PHY(k,Nphy) =', PHY(k,yt_iter)
!         endif
!       ENDDO
!       DO yt_iter = 1, Nzoo
!         ! if (isnan() .or. abs() > huge()) then
!         if (isnan(ZOO(k,yt_iter)) .or. abs(ZOO(k,yt_iter)) > 1.0d15 .or. ZOO(k,yt_iter) < 0.0d0) then
!           write(*,*) 'yt_debug: mod_reef_ecosys.F ZOO(k,Nzoo)    input      i =', i, '   j =', j, '   k =', k, '   Nzoo =', yt_iter
!           write(*,*) 'yt_debug:     ZOO(k,Nzoo) =', ZOO(k,yt_iter)
!         endif
!       ENDDO
!       DO yt_iter = 1, Npim
!         ! if (isnan() .or. abs() > huge()) then
!         if (isnan(PIC(k,yt_iter)) .or. abs(PIC(k,yt_iter)) > 1.0d15 .or. PIC(k,yt_iter) < 0.0d0) then
!           write(*,*) 'yt_debug: mod_reef_ecosys.F PIC(k,Npim)    input      i =', i, '   j =', j, '   k =', k, '   Npim =', yt_iter
!           write(*,*) 'yt_debug:     PIC(k,Npim) =', PIC(k,yt_iter)
!         endif
!       ENDDO
! #endif
! #if defined NUTRIENTS           
!       ! if (isnan() .or. abs() > huge()) then
!       if (isnan(NO3(k)) .or. abs(NO3(k)) > 1.0d15 .or. NO3(k) < 0.0d0) then
!         write(*,*) 'yt_debug: mod_reef_ecosys.F NO3(k)    input      i =', i, '   j =', j, '   k =', k
!         write(*,*) 'yt_debug:     NO3(k) =', NO3(k)
!       endif
!       ! if (isnan() .or. abs() > huge()) then
!       if (isnan(NH4(k)) .or. abs(NH4(k)) > 1.0d15 .or. NH4(k) < 0.0d0) then
!         write(*,*) 'yt_debug: mod_reef_ecosys.F NH4(k)    input      i =', i, '   j =', j, '   k =', k
!         write(*,*) 'yt_debug:     NH4(k) =', NH4(k)
!       endif
!       ! if (isnan() .or. abs() > huge()) then
!       if (isnan(PO4(k)) .or. abs(PO4(k)) > 1.0d15 .or. PO4(k) < 0.0d0) then
!         write(*,*) 'yt_debug: mod_reef_ecosys.F PO4(k)    input      i =', i, '   j =', j, '   k =', k
!         write(*,*) 'yt_debug:     PO4(k) =', PO4(k)
!       endif
! # if defined ORGANIC_MATTER
!       DO yt_iter = 1, Ndom
!         ! if (isnan() .or. abs() > huge()) then
!         if (isnan(DON(k,yt_iter)) .or. abs(DON(k,yt_iter)) > 1.0d15 .or. DON(k,yt_iter) < 0.0d0) then
!           write(*,*) 'yt_debug: mod_reef_ecosys.F DON(k,Ndom)    input      i =', i, '   j =', j, '   k =', k, '   Ndom =', yt_iter
!           write(*,*) 'yt_debug:     DON(k,Ndom) =', DON(k,yt_iter)
!         endif
!         ! if (isnan() .or. abs() > huge()) then
!         if (isnan(DOP(k,yt_iter)) .or. abs(DOP(k,yt_iter)) > 1.0d15 .or. DOP(k,yt_iter) < 0.0d0) then
!           write(*,*) 'yt_debug: mod_reef_ecosys.F DOP(k,Ndom)    input      i =', i, '   j =', j, '   k =', k, '   Ndom =', yt_iter
!           write(*,*) 'yt_debug:     DOP(k,Ndom) =', DOP(k,yt_iter)
!         endif
!       ENDDO
!       DO yt_iter = 1, Npom
!         ! if (isnan() .or. abs() > huge()) then
!         if (isnan(PON(k,yt_iter)) .or. abs(PON(k,yt_iter)) > 1.0d15 .or. PON(k,yt_iter) < 0.0d0) then
!           write(*,*) 'yt_debug: mod_reef_ecosys.F PON(k,Npom)    input      i =', i, '   j =', j, '   k =', k, '   Npom =', yt_iter
!           write(*,*) 'yt_debug:     PON(k,Npom) =', PON(k,yt_iter)
!         endif
!         ! if (isnan() .or. abs() > huge()) then
!         if (isnan(POP(k,yt_iter)) .or. abs(POP(k,yt_iter)) > 1.0d15 .or. POP(k,yt_iter) < 0.0d0) then
!           write(*,*) 'yt_debug: mod_reef_ecosys.F POP(k,Npom)    input      i =', i, '   j =', j, '   k =', k, '   Npom =', yt_iter
!           write(*,*) 'yt_debug:     POP(k,Npom) =', POP(k,yt_iter)
!         endif
!       ENDDO
! # endif
! #endif
!     ENDDO
!     ! if (isnan() .or. abs() > huge()) then
!     if (isnan(sspH) .or. abs(sspH) > 1.0d15) then
!       write(*,*) 'yt_debug: mod_reef_ecosys.F sspH    input      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     sspH =', sspH
!     endif
!     ! if (isnan() .or. abs() > huge()) then
!     if (isnan(ssfCO2) .or. abs(ssfCO2) > 1.0d15) then
!       write(*,*) 'yt_debug: mod_reef_ecosys.F ssfCO2    input      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     ssfCO2 =', ssfCO2
!     endif
!     ! if (isnan() .or. abs() > huge()) then
!     if (isnan(ssWarg) .or. abs(ssWarg) > 1.0d15) then
!       write(*,*) 'yt_debug: mod_reef_ecosys.F ssWarg    input      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     ssWarg =', ssWarg
!     endif
!     ! if (isnan() .or. abs() > huge()) then
!     if (isnan(ssCO2flux) .or. abs(ssCO2flux) > 1.0d15) then
!       write(*,*) 'yt_debug: mod_reef_ecosys.F ssCO2flux    input      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     ssCO2flux =', ssCO2flux
!     endif
!     ! if (isnan() .or. abs() > huge()) then
!     if (isnan(ssO2flux) .or. abs(ssO2flux) > 1.0d15) then
!       write(*,*) 'yt_debug: mod_reef_ecosys.F ssO2flux    input      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     ssO2flux =', ssO2flux
!     endif
!     ! if (isnan() .or. abs() > huge()) then
!     if (isnan(PFDbott) .or. abs(PFDbott) > 1.0d15) then
!       write(*,*) 'yt_debug: mod_reef_ecosys.F PFDbott    input      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     PFDbott =', PFDbott
!     endif
! !!! yt_debug <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


!!!---------------------------------------------------------------------
!!!  Sea surface interaction.
!!!---------------------------------------------------------------------

!-----------------------------------------------------------------------
!  Compute surface O2 gas exchange.
!-----------------------------------------------------------------------

    TmpK = Tmp(N)+273.15d0

!----------- O2 gas exchange rate (mmol m-2 s-1). -------------------
      
    DOsatu=O2satu(TmpK,Sal(N))
    ssO2flux = Flux_O2(DOx(N), DOsatu, U10, TmpK, Sal(N) )  ! sea to air is positive

!-----------------------------------------------------------------------
!  Compute surface CO2 gas exchange.
!-----------------------------------------------------------------------

!----------- CO2 system in ambient seawater -------------------

    sspH = pH_fromATCT( TA(N), DIC(N),TmpK, Sal(N) )
    cCO2aq = cCO2aq_fromCTpH( DIC(N), sspH, TmpK, Sal(N) )
!    cHCO3 = cHCO3_fromCTpH( DIC(N), sspH, TmpK, Sal(N) )
    cCO3 = cCO3_fromCTpH( DIC(N), sspH, TmpK, Sal(N) )
    ssfCO2 = fCO2_fromcCO2aq( cCO2aq, TmpK, Sal(N) )  !! for output
    ssWarg = Warg_fromcCO3( cCO3, TmpK, Sal(N) )

!----------- CO2 gas exchange rate (mmol m-2 s-1). -------------------
    ssCO2flux = Flux_CO2(ssfCO2, pCO2air, U10, TmpK, Sal(N) )  ! sea to air is positive

    IF(ssCO2flux*0.0d0 /= 0.0d0) THEN  !!!---------Error Handling: Check NAN
!      write(50,*) ssCO2flux, TA(N), DIC(N),TmpK, Sal(N), sspH, cCO3, ssWarg
      ssCO2flux = 0.0d0
    END IF
    
#if defined AIR_SEA_GAS_EXCHANGE
    dDOx_dt(N) = dDOx_dt(N) - ssO2flux  /dz(N)  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    dDIC_dt(N) = dDIC_dt(N) - ssCO2flux  /dz(N)   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# if defined CARBON_ISOTOPE
#  if defined CARBON_TRACE
    R13C=0.d0
#  else
    R13C=R13C_fromd13C(-10.d0)
#  endif
    dDI13C_dt(N) = dDI13C_dt(N)- ssCO2flux*R13C /dz(N) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# endif
#endif


!!!---------------------------------------------------------------------
!!!  Water columun interactions.
!!!---------------------------------------------------------------------

!-----------------------------------------------------------------------
!  Light-limited computations.
!-----------------------------------------------------------------------
!
!  Compute attenuation coefficient based on the concentration of
!  chlorophyll-a within each grid box.  Then, attenuate surface
!  photosynthetically available radiation (PARsur) down inot the
!  water column.  Thus, PAR at certain depth depends on the whole
!  distribution of chlorophyll-a above.
!  To compute rate of maximum primary productivity (t_PPmax), one needs
!  PAR somewhat in the middle of the gridbox, so that attenuation "Att"
!  corresponds to half of the grid box height, while PAR is multiplied
!  by it twice: once to get it in the middle of grid-box and once the
!  compute on the lower grid-box interface.
!
    PFD=PFDsurf

    DO k=N,1,-1
    
!!
!!  Compute average light attenuation for each grid cell. To include
!!  other attenuation contributions like suspended sediment or CDOM
!!  modify AttFac.
!        AttFac=0.0d0
!
!        Att=(AttSW+                                         &
!     &       AttChl*Bio(i,k,iChlo)+                         &
!     &       AttFac)*dz(k)
!        ExpAtt=EXP(-Att)
!        Itop=PFD
!        PFD=Itop*(1.0d0-ExpAtt)/Att    ! average at cell center
!!
!!  Compute Chlorophyll-a phytoplankton ratio, [mg Chla / (mg C)].
!!
!        cff=PhyCN(ng)*12.0d0
!        Chl2C=MIN(Bio(i,k,iChlo)/(Bio(i,k,iPhyt)*cff+eps),      &
!     &            Chl2C_m(ng))
!
      Att= AttSW*dz(k) 
      ExpAtt=EXP(-Att)
      Itop=PFD
      PFD=Itop*(1.0d0-ExpAtt)/Att    ! average at cell center


#if defined FOODWEB
!-----------------------------------------------------------------------
! Food web model.
!-----------------------------------------------------------------------

      CALL foodweb       &
!       input parameters
        ( ng, 1, i, j    &   ! ng: nested grid number; n: coral compartment; i,j: position
        , dt             &   ! Time step (sec)
        , PFD            &   ! Photon flux density (umol m-2 s-1)
        , rho_sw(k)      &   ! Density of seawater (g cm-3)
        , Tmp(k)         &   ! Temperature (oC)
        , Sal(k)         &   ! Salinity (PSU)
        , DIC(k)         &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
        , TA (k)         &   ! Total alkalinity (TA: umol kg-1)
        , DOx(k)         &   ! Dissolved oxygen (umol L-1)
        , DOC(k,:)       &   ! Labile Dissolved organic carbon (DOC: umol L-1)
        , POC(k,:)       &   ! Particulate organic carbon (POC: umol L-1)
        , PHY(k,:)       &   ! phytoplankton1 (umol C L-1), dinoflagellate
        , ZOO(k,:)       &   ! zooplankton (umol C L-1)
        , PIC(k,:)       &   ! Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
# if defined FOODWEB_TESTMODE
        , DeadPHY(k,:)   &   ! DeadPHY(Nphy): Dead phytoplankton (umol C L-1)
# endif
# if defined CARBON_ISOTOPE
        , DI13C(k)       &   ! 13C of DIC (umol kg-1)
        , DO13C(k,:)     &   ! 13C of Labile Dissolved organic carbon (DOC: umol L-1)
        , PO13C(k,:)     &   ! 13C of Particulate organic carbon (POC: umol L-1)
        , PHY13C(k,:)    &   ! 13C of phytoplankton1 (umol C L-1), dinoflagellate
        , ZOO13C(k,:)    &   ! 13C of zooplankton (umol C L-1)
        , PI13C(k,:)     &   ! 13C of Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
# endif
# if defined NUTRIENTS         
        , NO3(k)         &   ! NO3 (umol L-1)
!        , NO2(k)         &   ! NO2 (umol L-1)
        , NH4(k)         &   ! NH4 (umol L-1)
        , PO4(k)         &   ! PO4 (umol L-1)
        , DON(k,:)       &   ! Labile Dissolved organic nitrogen (DON: umol L-1)
        , PON(k,:)       &   ! Particulate organic nitrogen (PON: umol L-1)
        , DOP(k,:)       &   ! Labile Dissolved organic phosporius (DOP: umol L-1)
        , POP(k,:)       &   ! Particulate organic phosporius (POP: umol L-1)
#  if defined NITROGEN_ISOTOPE
        , NO3_15N(k)     &   ! 15N of NO3 (umol L-1)
!        , NO2_15N(k)     &   ! 15N of NO2 (umol L-1)
        , NH4_15N(k)     &   ! 15N of NH4 (umol L-1)
        , DO15N(k,:)     &   ! 15N of Labile Dissolved organic nitrogen (DON: umol L-1)
        , PO15N(k,:)     &   ! 15N of Particulate organic nitrogen (PON: umol L-1)
        , PHY15N(k,:)    &   ! 15N of phytoplankton1 (umol C L-1), dinoflagellate
        , ZOO15N(k,:)    &   ! 15N of zooplankton (umol C L-1)
#  endif
# endif
# if defined COT_STARFISH      
        , COTe(k)        &   ! COT starfish egg (umol L-1)
        , COTl(k)        &   ! COT starfish larvae (umol L-1)
# endif
    !   output parameters
        , dDIC(k)        &   ! dDIC/dt  (umol kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
        , dTA(k)         &   ! dTA/dt   (umol kg-1 s-1) 
        , dDOx(k)        &   ! dDOx/dt  (umol L-1 s-1) 
        , dDOC(k,:)      &   ! dDOC/dt  (umol L-1 s-1) 
        , dPOC(k,:)      &   ! dPOC/dt  (umol L-1 s-1) 
        , dPHY(k,:)      &   ! dPHY1/dt  (umol L-1 s-1)  
        , dZOO(k,:)      &   ! dZOO/dt  (umol L-1 s-1)  
        , dPIC(k,:)      &   ! dPIC/dt  (umol L-1 s-1)  
# if defined FOODWEB_TESTMODE
        , dDeadPHY(k,:)  &   ! dDeadPHY/dt  (umol L-1 s-1)
# endif
# if defined CARBON_ISOTOPE
        , dDI13C(k)      &   ! dDI13C/dt (umol kg-1 s-1)
        , dDO13C(k,:)    &   ! dLDO13C/dt  (umol L-1 s-1) 
        , dPO13C(k,:)    &   ! dPO13C/dt  (umol L-1 s-1) 
        , dPHY13C(k,:)   &   ! dPHY1_13C/dt  (umol L-1 s-1)  
        , dZOO13C(k,:)   &   ! dZOO_13C/dt  (umol L-1 s-1)  
        , dPI13C(k,:)    &   ! dPI13C/dt  (umol L-1 s-1)  
# endif
# if defined NUTRIENTS
        , dNO3(k)        &   ! dNO3/dt (umol L-1 s-1)
        , dNH4(k)        &   ! dNH4/dt (umol L-1 s-1)
        , dPO4(k)        &   ! dPO4/dt (umol L-1 s-1)
        , dDON(k,:)      &   ! dDON/dt (umol L-1 s-1)
        , dPON(k,:)      &   ! dPON/dt (umol L-1 s-1)
        , dDOP(k,:)      &   ! dDOP/dt (umol L-1 s-1)
        , dPOP(k,:)      &   ! dPOP/dt (umol L-1 s-1)
#  if defined NITROGEN_ISOTOPE
        , dNO3_15N(k)    &   ! dNO3_15N/dt (umol L-1 s-1)
        , dNH4_15N(k)    &   ! dNH4_15N/dt (umol L-1 s-1)
        , dDO15N(k,:)    &   ! dLDO15N/dt  (umol L-1 s-1) 
        , dPO15N(k,:)    &   ! dPO15N/dt  (umol L-1 s-1) 
        , dPHY15N(k,:)   &   ! dPHY1_15N/dt  (umol L-1 s-1)  
        , dZOO15N(k,:)   &   ! dZOO_15N/dt  (umol L-1 s-1)  
#  endif
# endif                                
# if defined COT_STARFISH         
        , dCOTe(k)       &   ! dCOTe/dt (umol L-1 s-1)
        , dCOTl(k)       &   ! dCOTl/dt (umol L-1 s-1)
# endif
        )
    
!----- Mass balance equation -----------------------------------------------------------
      dDIC_dt(k) = dDIC_dt(k) + dDIC(k)
      dTA_dt (k) = dTA_dt (k) + dTA(k)
      dDOx_dt(k) = dDOx_dt(k) + dDOx(k)
      dDOC_dt(k,:) = dDOC_dt(k,:) + dDOC(k,:)
      dPOC_dt(k,:) = dPOC_dt(k,:) + dPOC(k,:)
      dPHY_dt(k,:) = dPHY_dt(k,:) + dPHY(k,:)
      dZOO_dt(k,:) = dZOO_dt(k,:) + dZOO(k,:)
      dPIC_dt(k,:) = dPIC_dt(k,:) + dPIC(k,:)
      ! yt_edit >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! if dPIC_dt would remove over 90% of PIC in one time step, limit to 90%
      IF (0.9*PIC(k,1)+dPIC_dt(k,1)*dt*dz(k) < 0.0d0) THEN
        dPIC_dt(k,1) = -0.9*PIC(k,1)/dt/dz(k)
      ENDIF
      ! yt_edit <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# if defined FOODWEB_TESTMODE
      dDeadPHY_dt(k,:) = dDeadPHY_dt(k,:) + dDeadPHY(k,:)
# endif
# if defined CARBON_ISOTOPE
      dDI13C_dt(k) = dDI13C_dt(k) + dDI13C(k)
      dDO13C_dt(k,:) = dDO13C_dt(k,:) + dDO13C(k,:)
      dPO13C_dt(k,:) = dPO13C_dt(k,:) + dPO13C(k,:)
      dPHY13C_dt(k,:) = dPHY13C_dt(k,:) + dPHY13C(k,:)
      dZOO13C_dt(k,:) = dZOO13C_dt(k,:) + dZOO13C(k,:)
      dPI13C_dt(k,:) = dPI13C_dt(k,:) + dPI13C(k,:)
# endif
# if defined NUTRIENTS
      dNO3_dt(k) = dNO3_dt(k) + dNO3(k)
      dNH4_dt(k) = dNH4_dt(k) + dNH4(k)
      dPO4_dt(k) = dPO4_dt(k) + dPO4(k)
      dDON_dt(k,:) = dDON_dt(k,:) + dDON(k,:)
      dPON_dt(k,:) = dPON_dt(k,:) + dPON(k,:)
      dDOP_dt(k,:) = dDOP_dt(k,:) + dDOP(k,:)
      dPOP_dt(k,:) = dPOP_dt(k,:) + dPOP(k,:)
#  if defined NITROGEN_ISOTOPE
      dNO3_15N_dt(k) = dNO3_15N_dt(k) + dNO3_15N(k)
      dNH4_15N_dt(k) = dNH4_15N_dt(k) + dNH4_15N(k)
      dDO15N_dt(k,:) = dDO15N_dt(k,:) + dDO15N(k,:)
      dPO15N_dt(k,:) = dPO15N_dt(k,:) + dPO15N(k,:)
      dPHY15N_dt(k,:) = dPHY15N_dt(k,:) + dPHY15N(k,:)
      dZOO15N_dt(k,:) = dZOO15N_dt(k,:) + dZOO15N(k,:)
#  endif
# endif
# if defined COT_STARFISH
      dCOTe_dt(k) = dCOTe_dt(k) + dCOTe(k)
      dCOTl_dt(k) = dCOTl_dt(k) + dCOTl(k)
# endif
#endif
                             
!!! yt_debug >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(dDIC_dt(k)) .or. abs(dDIC_dt(k)) > 1.0d15 .or. DIC(k)+dDIC_dt(k)*dt*dz(k) < -100.0d0) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F dDIC_dt(k)    foodweb      i =', i, '   j =', j, '   k =', k
        write(*,*) 'yt_debug:     dDIC_dt(k) =', dDIC_dt(k)
        write(*,*) 'yt_debug:     DIC(k) =', DIC(k)
        write(*,*) 'yt_debug:     DIC(k)+dDIC_dt(k)*dt*dz(k) =', DIC(k)+dDIC_dt(k)*dt*dz(k)
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(dTA_dt(k)) .or. abs(dTA_dt(k)) > 1.0d15 .or. TA(k)+dTA_dt(k)*dt*dz(k) < -100.0d0) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F dTA_dt(k)    foodweb      i =', i, '   j =', j, '   k =', k
        write(*,*) 'yt_debug:     dTA_dt(k) =', dTA_dt(k)
        write(*,*) 'yt_debug:     TA(k) =', TA(k)
        write(*,*) 'yt_debug:     TA(k)+dTA_dt(k)*dt*dz(k) =', TA(k)+dTA_dt(k)*dt*dz(k)
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(dDOx_dt(k)) .or. abs(dDOx_dt(k)) > 1.0d15 .or. DOx(k)+dDOx_dt(k)*dt*dz(k) < -100.0d0) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F dDOx_dt(k)    foodweb      i =', i, '   j =', j, '   k =', k
        write(*,*) 'yt_debug:     dDOx_dt(k) =', dDOx_dt(k)
        write(*,*) 'yt_debug:     DOx(k) =', DOx(k)
        write(*,*) 'yt_debug:     DOx(k)+dDOx_dt(k)*dt*dz(k) =', DOx(k)+dDOx_dt(k)*dt*dz(k)
      endif
#if defined ORGANIC_MATTER
      DO yt_iter = 1, Ndom
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dDOC_dt(k,yt_iter)) .or. abs(dDOC_dt(k,yt_iter)) > 1.0d15 .or. DOC(k,yt_iter)+dDOC_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dDOC_dt(k,Ndom)    foodweb      i =', i, '   j =', j, '   k =', k, '   Ndom =', yt_iter
          write(*,*) 'yt_debug:     dDOC_dt(k,Ndom) =', dDOC_dt(k,yt_iter)
          write(*,*) 'yt_debug:     DOC(k,Ndom) =', DOC(k,yt_iter)
          write(*,*) 'yt_debug:     DOC(k,Ndom)+dDOC_dt(k,Ndom)*dt*dz(k) =', DOC(k,yt_iter)+dDOC_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
      DO yt_iter = 1, Npom
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dPOC_dt(k,yt_iter)) .or. abs(dPOC_dt(k,yt_iter)) > 1.0d15 .or. POC(k,yt_iter)+dPOC_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dPOC_dt(k,Npom)    foodweb      i =', i, '   j =', j, '   k =', k, '   Npom =', yt_iter
          write(*,*) 'yt_debug:     dPOC_dt(k,Npom) =', dPOC_dt(k,yt_iter)
          write(*,*) 'yt_debug:     POC(k,Npom) =', POC(k,yt_iter)
          write(*,*) 'yt_debug:     POC(k,Npom)+dPOC_dt(k,Npom)*dt*dz(k) =', POC(k,yt_iter)+dPOC_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
      DO yt_iter = 1, Nphy
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dPHY_dt(k,yt_iter)) .or. abs(dPHY_dt(k,yt_iter)) > 1.0d15 .or. PHY(k,yt_iter)+dPHY_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dPHY_dt(k,Nphy)    foodweb      i =', i, '   j =', j, '   k =', k, '   Nphy =', yt_iter
          write(*,*) 'yt_debug:     dPHY_dt(k,Nphy) =', dPHY_dt(k,yt_iter)
          write(*,*) 'yt_debug:     PHY(k,Nphy) =', PHY(k,yt_iter)
          write(*,*) 'yt_debug:     PHY(k,Nphy)+dPHY_dt(k,Nphy)*dt*dz(k) =', PHY(k,yt_iter)+dPHY_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
      DO yt_iter = 1, Nzoo
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dZOO_dt(k,yt_iter)) .or. abs(dZOO_dt(k,yt_iter)) > 1.0d15 .or. ZOO(k,yt_iter)+dZOO_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dZOO_dt(k,Nzoo)    foodweb      i =', i, '   j =', j, '   k =', k, '   Nzoo =', yt_iter
          write(*,*) 'yt_debug:     dZOO_dt(k,Nzoo) =', dZOO_dt(k,yt_iter)
          write(*,*) 'yt_debug:     ZOO(k,Nzoo) =', ZOO(k,yt_iter)
          write(*,*) 'yt_debug:     ZOO(k,Nzoo)+dZOO_dt(k,Nzoo)*dt*dz(k) =', ZOO(k,yt_iter)+dZOO_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
      DO yt_iter = 1, Npim
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dPIC_dt(k,yt_iter)) .or. abs(dPIC_dt(k,yt_iter)) > 1.0d15 .or. PIC(k,yt_iter)+dPIC_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dPIC_dt(k,Npim)    foodweb      i =', i, '   j =', j, '   k =', k, '   Npim =', yt_iter
          write(*,*) 'yt_debug:     dPIC_dt(k,Npim) =', dPIC_dt(k,yt_iter)
          write(*,*) 'yt_debug:     PIC(k,Npim) =', PIC(k,yt_iter)
          write(*,*) 'yt_debug:     PIC(k,Npim)+dPIC_dt(k,Npim)*dt*dz(k) =', PIC(k,yt_iter)+dPIC_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
#endif
#if defined NUTRIENTS           
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(dNO3_dt(k)) .or. abs(dNO3_dt(k)) > 1.0d15 .or. NO3(k)+dNO3_dt(k)*dt*dz(k) < -100.0d0) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F dNO3_dt(k)    foodweb      i =', i, '   j =', j, '   k =', k
        write(*,*) 'yt_debug:     dNO3_dt(k) =', dNO3_dt(k)
        write(*,*) 'yt_debug:     NO3(k) =', NO3(k)
        write(*,*) 'yt_debug:     NO3(k)+dNO3_dt(k)*dt*dz(k) =', NO3(k)+dNO3_dt(k)*dt*dz(k)
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(dNH4_dt(k)) .or. abs(dNH4_dt(k)) > 1.0d15 .or. NH4(k)+dNH4_dt(k)*dt*dz(k) < -100.0d0) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F dNH4_dt(k)    foodweb      i =', i, '   j =', j, '   k =', k
        write(*,*) 'yt_debug:     dNH4_dt(k) =', dNH4_dt(k)
        write(*,*) 'yt_debug:     NH4(k) =', NH4(k)
        write(*,*) 'yt_debug:     NH4(k)+dNH4_dt(k)*dt*dz(k) =', NH4(k)+dNH4_dt(k)*dt*dz(k)
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(dPO4_dt(k)) .or. abs(dPO4_dt(k)) > 1.0d15 .or. PO4(k)+dPO4_dt(k)*dt*dz(k) < -100.0d0) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F dPO4_dt(k)    foodweb      i =', i, '   j =', j, '   k =', k
        write(*,*) 'yt_debug:     dPO4_dt(k) =', dPO4_dt(k)
        write(*,*) 'yt_debug:     PO4(k) =', PO4(k)
        write(*,*) 'yt_debug:     PO4(k)+dPO4_dt(k)*dt*dz(k) =', PO4(k)+dPO4_dt(k)*dt*dz(k)
      endif
# if defined ORGANIC_MATTER
      DO yt_iter = 1, Ndom
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dDON_dt(k,yt_iter)) .or. abs(dDON_dt(k,yt_iter)) > 1.0d15 .or. DON(k,yt_iter)+dDON_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dDON_dt(k,Ndom)    foodweb      i =', i, '   j =', j, '   k =', k, '   Ndom =', yt_iter
          write(*,*) 'yt_debug:     dDON_dt(k,Ndom) =', dDON_dt(k,yt_iter)
          write(*,*) 'yt_debug:     DON(k,Ndom) =', DON(k,yt_iter)
          write(*,*) 'yt_debug:     DON(k,Ndom)+dDON_dt(k,Ndom)*dt*dz(k) =', DON(k,yt_iter)+dDON_dt(k,yt_iter)*dt*dz(k)
        endif
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dDOP_dt(k,yt_iter)) .or. abs(dDOP_dt(k,yt_iter)) > 1.0d15 .or. DOP(k,yt_iter)+dDOP_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dDOP_dt(k,Ndom)    foodweb      i =', i, '   j =', j, '   k =', k, '   Ndom =', yt_iter
          write(*,*) 'yt_debug:     dDOP_dt(k,Ndom) =', dDOP_dt(k,yt_iter)
          write(*,*) 'yt_debug:     DOP(k,Ndom) =', DOP(k,yt_iter)
          write(*,*) 'yt_debug:     DOP(k,Ndom)+dDOP_dt(k,Ndom)*dt*dz(k) =', DOP(k,yt_iter)+dDOP_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
      DO yt_iter = 1, Npom
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dPON_dt(k,yt_iter)) .or. abs(dPON_dt(k,yt_iter)) > 1.0d15 .or. PON(k,yt_iter)+dPON_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dPON_dt(k,Npom)    foodweb      i =', i, '   j =', j, '   k =', k, '   Npom =', yt_iter
          write(*,*) 'yt_debug:     dPON_dt(k,Npom) =', dPON_dt(k,yt_iter)
          write(*,*) 'yt_debug:     PON(k,Npom) =', PON(k,yt_iter)
          write(*,*) 'yt_debug:     PON(k,Npom)+dPON_dt(k,Npom)*dt*dz(k) =', PON(k,yt_iter)+dPON_dt(k,yt_iter)*dt*dz(k)
        endif
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dPOP_dt(k,yt_iter)) .or. abs(dPOP_dt(k,yt_iter)) > 1.0d15 .or. POP(k,yt_iter)+dPOP_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dPOP_dt(k,Npom)    foodweb      i =', i, '   j =', j, '   k =', k, '   Npom =', yt_iter
          write(*,*) 'yt_debug:     dPOP_dt(k,Npom) =', dPOP_dt(k,yt_iter)
          write(*,*) 'yt_debug:     POP(k,Npom) =', POP(k,yt_iter)
          write(*,*) 'yt_debug:     POP(k,Npom)+dPOP_dt(k,Npom)*dt*dz(k) =', POP(k,yt_iter)+dPOP_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
# endif
#endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(sspH) .or. abs(sspH) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F sspH    foodweb      i =', i, '   j =', j
        write(*,*) 'yt_debug:     sspH =', sspH
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(ssfCO2) .or. abs(ssfCO2) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F ssfCO2    foodweb      i =', i, '   j =', j
        write(*,*) 'yt_debug:     ssfCO2 =', ssfCO2
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(ssWarg) .or. abs(ssWarg) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F ssWarg    foodweb      i =', i, '   j =', j
        write(*,*) 'yt_debug:     ssWarg =', ssWarg
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(ssCO2flux) .or. abs(ssCO2flux) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F ssCO2flux    foodweb      i =', i, '   j =', j
        write(*,*) 'yt_debug:     ssCO2flux =', ssCO2flux
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(ssO2flux) .or. abs(ssO2flux) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F ssO2flux    foodweb      i =', i, '   j =', j
        write(*,*) 'yt_debug:     ssO2flux =', ssO2flux
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(PFDbott) .or. abs(PFDbott) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F PFDbott    foodweb      i =', i, '   j =', j
        write(*,*) 'yt_debug:     PFDbott =', PFDbott
      endif
!!! yt_debug <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


!  Light attenuation at the bottom of the grid cell. It is the starting
!  PFD value for the next (deeper) vertical grid cell.
!
      PFD=Itop*ExpAtt

    END DO

    PFDbott=PFD  !!! Photon flux density at sea bottom

#if defined ORGANIC_MATTER && !defined ECOSYS_TESTMODE
!-----------------------------------------------------------------------
! Vertical advection (sinking/flaoting) of POM and PIM
!-----------------------------------------------------------------------
    z_r(1) = 0.5d0*dz(1)
    DO k=2,N
      z_r(k) = z_r(k-1) + 0.5d0*dz(k-1) + 0.5d0*dz(k)  
    END DO
    z_w(0) = 0.0d0
    DO k=1,N
      z_w(k) = z_w(k-1) + dz(k)   
    END DO
  
    DO itrc=1, Npom
      IF (itrc==iCPOM) then
        w_PM = 0.001d0
      ELSE
        w_PM = -(rho_POM(itrc)/1.024d0 - 1.0d0)*grav*D_POM(itrc)*D_POM(itrc)*1.0d-6/18.0d0/vis_sw
      END IF
      !!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MY:Add
#ifdef UPWELL_TRACER
      ! Give zero settling velocity
      w_PM = 0.0d0
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<MY:Add
      CALL bio_vadv2(N, dt, dz, z_r, z_w, w_PM, POC(:,itrc), dPOC(:,itrc), Fdep_POC(itrc))
# if defined CARBON_ISOTOPE
      CALL bio_vadv2(N, dt, dz, z_r, z_w, w_PM, PO13C(:,itrc), dPO13C(:,itrc), Fdep_PO13C(itrc))
# endif
# if defined NUTRIENTS
      CALL bio_vadv2(N, dt, dz, z_r, z_w, w_PM, PON(:,itrc), dPON(:,itrc), Fdep_PON(itrc))
      CALL bio_vadv2(N, dt, dz, z_r, z_w, w_PM, POP(:,itrc), dPOP(:,itrc), Fdep_POP(itrc))
#  if defined NITROGEN_ISOTOPE
      CALL bio_vadv2(N, dt, dz, z_r, z_w, w_PM, PO15N(:,itrc), dPO15N(:,itrc), Fdep_PO15N(itrc))
#  endif
# endif
    END DO

    DO itrc=1, Npim
      w_PM = -(rho_PIM(itrc)/1.024d0 - 1.0d0)*grav*D_PIM(itrc)*D_PIM(itrc)*1.0d-6/18.0d0/vis_sw
      CALL bio_vadv2(N, dt, dz, z_r, z_w, w_PM, PIC(:,itrc), dPIC(:,itrc), Fdep_PIC(itrc))
# if defined CARBON_ISOTOPE
      CALL bio_vadv2(N, dt, dz, z_r, z_w, w_PM, PI13C(:,itrc), dPI13C(:,itrc), Fdep_PI13C(itrc))
# endif
    END DO

!----- Mass balance equation -----------------------------------------------------------
    dPOC_dt(:,:) = dPOC_dt(:,:) + dPOC(:,:)
    dPIC_dt(:,:) = dPIC_dt(:,:) + dPIC(:,:)
# if defined CARBON_ISOTOPE
    dPO13C_dt(:,:) = dPO13C_dt(:,:) + dPO13C(:,:)
    dPI13C_dt(:,:) = dPI13C_dt(:,:) + dPI13C(:,:)
# endif
# if defined NUTRIENTS
    dPON_dt(:,:) = dPON_dt(:,:) + dPON(:,:)
    dPOP_dt(:,:) = dPOP_dt(:,:) + dPOP(:,:)
#  if defined NITROGEN_ISOTOPE
    dPO15N_dt(:,:) = dPO15N_dt(:,:) + dPO15N(:,:)
#  endif
# endif

#endif

!!! yt_debug >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    DO k=1,N
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(dDIC_dt(k)) .or. abs(dDIC_dt(k)) > 1.0d15 .or. DIC(k)+dDIC_dt(k)*dt*dz(k) < -100.0d0) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F dDIC_dt(k)    advection      i =', i, '   j =', j, '   k =', k
        write(*,*) 'yt_debug:     dDIC_dt(k) =', dDIC_dt(k)
        write(*,*) 'yt_debug:     DIC(k) =', DIC(k)
        write(*,*) 'yt_debug:     DIC(k)+dDIC_dt(k)*dt*dz(k) =', DIC(k)+dDIC_dt(k)*dt*dz(k)
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(dTA_dt(k)) .or. abs(dTA_dt(k)) > 1.0d15 .or. TA(k)+dTA_dt(k)*dt*dz(k) < -100.0d0) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F dTA_dt(k)    advection      i =', i, '   j =', j, '   k =', k
        write(*,*) 'yt_debug:     dTA_dt(k) =', dTA_dt(k)
        write(*,*) 'yt_debug:     TA(k) =', TA(k)
        write(*,*) 'yt_debug:     TA(k)+dTA_dt(k)*dt*dz(k) =', TA(k)+dTA_dt(k)*dt*dz(k)
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(dDOx_dt(k)) .or. abs(dDOx_dt(k)) > 1.0d15 .or. DOx(k)+dDOx_dt(k)*dt*dz(k) < -100.0d0) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F dDOx_dt(k)    advection      i =', i, '   j =', j, '   k =', k
        write(*,*) 'yt_debug:     dDOx_dt(k) =', dDOx_dt(k)
        write(*,*) 'yt_debug:     DOx(k) =', DOx(k)
        write(*,*) 'yt_debug:     DOx(k)+dDOx_dt(k)*dt*dz(k) =', DOx(k)+dDOx_dt(k)*dt*dz(k)
      endif
#if defined ORGANIC_MATTER
      DO yt_iter = 1, Ndom
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dDOC_dt(k,yt_iter)) .or. abs(dDOC_dt(k,yt_iter)) > 1.0d15 .or. DOC(k,yt_iter)+dDOC_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dDOC_dt(k,Ndom)    advection      i =', i, '   j =', j, '   k =', k, '   Ndom =', yt_iter
          write(*,*) 'yt_debug:     dDOC_dt(k,Ndom) =', dDOC_dt(k,yt_iter)
          write(*,*) 'yt_debug:     DOC(k,Ndom) =', DOC(k,yt_iter)
          write(*,*) 'yt_debug:     DOC(k,Ndom)+dDOC_dt(k,Ndom)*dt*dz(k) =', DOC(k,yt_iter)+dDOC_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
      DO yt_iter = 1, Npom
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dPOC_dt(k,yt_iter)) .or. abs(dPOC_dt(k,yt_iter)) > 1.0d15 .or. POC(k,yt_iter)+dPOC_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dPOC_dt(k,Npom)    advection      i =', i, '   j =', j, '   k =', k, '   Npom =', yt_iter
          write(*,*) 'yt_debug:     dPOC_dt(k,Npom) =', dPOC_dt(k,yt_iter)
          write(*,*) 'yt_debug:     POC(k,Npom) =', POC(k,yt_iter)
          write(*,*) 'yt_debug:     POC(k,Npom)+dPOC_dt(k,Npom)*dt*dz(k) =', POC(k,yt_iter)+dPOC_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
      DO yt_iter = 1, Nphy
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dPHY_dt(k,yt_iter)) .or. abs(dPHY_dt(k,yt_iter)) > 1.0d15 .or. PHY(k,yt_iter)+dPHY_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dPHY_dt(k,Nphy)    advection      i =', i, '   j =', j, '   k =', k, '   Nphy =', yt_iter
          write(*,*) 'yt_debug:     dPHY_dt(k,Nphy) =', dPHY_dt(k,yt_iter)
          write(*,*) 'yt_debug:     PHY(k,Nphy) =', PHY(k,yt_iter)
          write(*,*) 'yt_debug:     PHY(k,Nphy)+dPHY_dt(k,Nphy)*dt*dz(k) =', PHY(k,yt_iter)+dPHY_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
      DO yt_iter = 1, Nzoo
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dZOO_dt(k,yt_iter)) .or. abs(dZOO_dt(k,yt_iter)) > 1.0d15 .or. ZOO(k,yt_iter)+dZOO_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dZOO_dt(k,Nzoo)    advection      i =', i, '   j =', j, '   k =', k, '   Nzoo =', yt_iter
          write(*,*) 'yt_debug:     dZOO_dt(k,Nzoo) =', dZOO_dt(k,yt_iter)
          write(*,*) 'yt_debug:     ZOO(k,Nzoo) =', ZOO(k,yt_iter)
          write(*,*) 'yt_debug:     ZOO(k,Nzoo)+dZOO_dt(k,Nzoo)*dt*dz(k) =', ZOO(k,yt_iter)+dZOO_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
      DO yt_iter = 1, Npim
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dPIC_dt(k,yt_iter)) .or. abs(dPIC_dt(k,yt_iter)) > 1.0d15 .or. PIC(k,yt_iter)+dPIC_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dPIC_dt(k,Npim)    advection      i =', i, '   j =', j, '   k =', k, '   Npim =', yt_iter
          write(*,*) 'yt_debug:     dPIC_dt(k,Npim) =', dPIC_dt(k,yt_iter)
          write(*,*) 'yt_debug:     PIC(k,Npim) =', PIC(k,yt_iter)
          write(*,*) 'yt_debug:     PIC(k,Npim)+dPIC_dt(k,Npim)*dt*dz(k) =', PIC(k,yt_iter)+dPIC_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
#endif
#if defined NUTRIENTS           
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(dNO3_dt(k)) .or. abs(dNO3_dt(k)) > 1.0d15 .or. NO3(k)+dNO3_dt(k)*dt*dz(k) < -100.0d0) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F dNO3_dt(k)    advection      i =', i, '   j =', j, '   k =', k
        write(*,*) 'yt_debug:     dNO3_dt(k) =', dNO3_dt(k)
        write(*,*) 'yt_debug:     NO3(k) =', NO3(k)
        write(*,*) 'yt_debug:     NO3(k)+dNO3_dt(k)*dt*dz(k) =', NO3(k)+dNO3_dt(k)*dt*dz(k)
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(dNH4_dt(k)) .or. abs(dNH4_dt(k)) > 1.0d15 .or. NH4(k)+dNH4_dt(k)*dt*dz(k) < -100.0d0) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F dNH4_dt(k)    advection      i =', i, '   j =', j, '   k =', k
        write(*,*) 'yt_debug:     dNH4_dt(k) =', dNH4_dt(k)
        write(*,*) 'yt_debug:     NH4(k) =', NH4(k)
        write(*,*) 'yt_debug:     NH4(k)+dNH4_dt(k)*dt*dz(k) =', NH4(k)+dNH4_dt(k)*dt*dz(k)
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(dPO4_dt(k)) .or. abs(dPO4_dt(k)) > 1.0d15 .or. PO4(k)+dPO4_dt(k)*dt*dz(k) < -100.0d0) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F dPO4_dt(k)    advection      i =', i, '   j =', j, '   k =', k
        write(*,*) 'yt_debug:     dPO4_dt(k) =', dPO4_dt(k)
        write(*,*) 'yt_debug:     PO4(k) =', PO4(k)
        write(*,*) 'yt_debug:     PO4(k)+dPO4_dt(k)*dt*dz(k) =', PO4(k)+dPO4_dt(k)*dt*dz(k)
      endif
# if defined ORGANIC_MATTER
      DO yt_iter = 1, Ndom
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dDON_dt(k,yt_iter)) .or. abs(dDON_dt(k,yt_iter)) > 1.0d15 .or. DON(k,yt_iter)+dDON_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dDON_dt(k,Ndom)    advection      i =', i, '   j =', j, '   k =', k, '   Ndom =', yt_iter
          write(*,*) 'yt_debug:     dDON_dt(k,Ndom) =', dDON_dt(k,yt_iter)
          write(*,*) 'yt_debug:     DON(k,Ndom) =', DON(k,yt_iter)
          write(*,*) 'yt_debug:     DON(k,Ndom)+dDON_dt(k,Ndom)*dt*dz(k) =', DON(k,yt_iter)+dDON_dt(k,yt_iter)*dt*dz(k)
        endif
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dDOP_dt(k,yt_iter)) .or. abs(dDOP_dt(k,yt_iter)) > 1.0d15 .or. DOP(k,yt_iter)+dDOP_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dDOP_dt(k,Ndom)    advection      i =', i, '   j =', j, '   k =', k, '   Ndom =', yt_iter
          write(*,*) 'yt_debug:     dDOP_dt(k,Ndom) =', dDOP_dt(k,yt_iter)
          write(*,*) 'yt_debug:     DOP(k,Ndom) =', DOP(k,yt_iter)
          write(*,*) 'yt_debug:     DOP(k,Ndom)+dDOP_dt(k,Ndom)*dt*dz(k) =', DOP(k,yt_iter)+dDOP_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
      DO yt_iter = 1, Npom
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dPON_dt(k,yt_iter)) .or. abs(dPON_dt(k,yt_iter)) > 1.0d15 .or. PON(k,yt_iter)+dPON_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dPON_dt(k,Npom)    advection      i =', i, '   j =', j, '   k =', k, '   Npom =', yt_iter
          write(*,*) 'yt_debug:     dPON_dt(k,Npom) =', dPON_dt(k,yt_iter)
          write(*,*) 'yt_debug:     PON(k,Npom) =', PON(k,yt_iter)
          write(*,*) 'yt_debug:     PON(k,Npom)+dPON_dt(k,Npom)*dt*dz(k) =', PON(k,yt_iter)+dPON_dt(k,yt_iter)*dt*dz(k)
        endif
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dPOP_dt(k,yt_iter)) .or. abs(dPOP_dt(k,yt_iter)) > 1.0d15 .or. POP(k,yt_iter)+dPOP_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dPOP_dt(k,Npom)    advection      i =', i, '   j =', j, '   k =', k, '   Npom =', yt_iter
          write(*,*) 'yt_debug:     dPOP_dt(k,Npom) =', dPOP_dt(k,yt_iter)
          write(*,*) 'yt_debug:     POP(k,Npom) =', POP(k,yt_iter)
          write(*,*) 'yt_debug:     POP(k,Npom)+dPOP_dt(k,Npom)*dt*dz(k) =', POP(k,yt_iter)+dPOP_dt(k,yt_iter)*dt*dz(k)
        endif
      ENDDO
# endif
#endif
    ENDDO
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(sspH) .or. abs(sspH) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F sspH    advection      i =', i, '   j =', j
        write(*,*) 'yt_debug:     sspH =', sspH
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(ssfCO2) .or. abs(ssfCO2) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F ssfCO2    advection      i =', i, '   j =', j
        write(*,*) 'yt_debug:     ssfCO2 =', ssfCO2
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(ssWarg) .or. abs(ssWarg) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F ssWarg    advection      i =', i, '   j =', j
        write(*,*) 'yt_debug:     ssWarg =', ssWarg
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(ssCO2flux) .or. abs(ssCO2flux) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F ssCO2flux    advection      i =', i, '   j =', j
        write(*,*) 'yt_debug:     ssCO2flux =', ssCO2flux
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(ssO2flux) .or. abs(ssO2flux) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F ssO2flux    advection      i =', i, '   j =', j
        write(*,*) 'yt_debug:     ssO2flux =', ssO2flux
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(PFDbott) .or. abs(PFDbott) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F PFDbott    advection      i =', i, '   j =', j
        write(*,*) 'yt_debug:     PFDbott =', PFDbott
      endif
!!! yt_debug <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



!!!---------------------------------------------------------------------
!!!  Benthic interactions.
!!!---------------------------------------------------------------------
    Flux_DIC = 0.0d0
    Flux_TA  = 0.0d0
    Flux_DO  = 0.0d0
#if defined ORGANIC_MATTER
    Flux_DOC(:) = 0.0d0
    Flux_POC(:) = 0.0d0
    Flux_POCf = 0.0d0  ! yuta_edits_for_masa
    Flux_POCs = 0.0d0  ! yuta_edits_for_masa
    Flux_PHY(:) = 0.0d0
    Flux_ZOO(:) = 0.0d0
    Flux_PIC(:) = 0.0d0
#endif
#if defined CARBON_ISOTOPE
    Flux_DI13C    = 0.0d0
# if defined ORGANIC_MATTER
    Flux_DO13C(:) = 0.0d0
    Flux_PO13C(:) = 0.0d0
    Flux_PHY13C(:) = 0.0d0
    Flux_ZOO13C(:) = 0.0d0
    Flux_PI13C(:) = 0.0d0
# endif
#endif
#if defined NUTRIENTS            
    Flux_NO3 = 0.0d0
!    Flux_NO2 = 0.0d0
    Flux_NH4 = 0.0d0
    Flux_PO4 = 0.0d0
# if defined ORGANIC_MATTER
    Flux_DON(:) = 0.0d0
    Flux_PON(:) = 0.0d0
    Flux_DOP(:) = 0.0d0
    Flux_POP(:) = 0.0d0
    Flux_PONf = 0.0d0  ! yuta_edits_for_masa
    Flux_PONs = 0.0d0  ! yuta_edits_for_masa
    Flux_POPf = 0.0d0  ! yuta_edits_for_masa
    Flux_POPs = 0.0d0  ! yuta_edits_for_masa
# endif
# if defined NITROGEN_ISOTOPE
    Flux_NO3_15N = 0.0d0
!    Flux_NO2_15N = 0.0d0
    Flux_NH4_15N = 0.0d0
#  if defined ORGANIC_MATTER
    Flux_DO15N(:) = 0.0d0
    Flux_PO15N(:) = 0.0d0
    Flux_PHY15N(:) = 0.0d0
    Flux_ZOO15N(:) = 0.0d0
#  endif
# endif
#endif

#if defined CORAL_POLYP
!-----------------------------------------------------------------------
! Compute coral polyp model.
!-----------------------------------------------------------------------
!
    DO icl=1,Ncl
!    DO icl=1,1  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Tentative!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF(p_coral(icl) .gt. 0.0d0) THEN          

        DO m=1,isplitc   !!! Loop for coral polyp model: dtc <= 0.05 sec

          CALL coral_polyp          &
!           input parameters
            ( ng, icl, i, j   &   ! ng: nested grid number; n: coral compartment; i,j: position
            , dtc             &   ! Time step (sec)
            , PFDbott         &   ! Photon flux density (umol m-2 s-1)
            , rho_sw(1)       &   ! Density of seawater (g cm-3)
            , Tmp(1)          &   ! Temperature (oC)
            , Sal(1)          &   ! Salinity (PSU)
            , DIC(1)          &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
            , TA (1)          &   ! Total alkalinity (TA: umol kg-1)
            , DOx(1)          &   ! Dissolved oxygen (umol L-1)
# if defined CORAL_INGESTION
!            , PHY(1,1)          &   ! phytoplankton (umol C L-1)
            , ZOO(1,iGzoo)          &   ! zooplankton (umol C L-1)
# endif
# if defined CORAL_CARBON_ISOTOPE
            , DI13C(1)        &   !13C of DIC (umol kg-1)
# endif
# if defined CORAL_NUTRIENTS
            , NO3(1)          &   ! NO3 (umol L-1)
!            , NO2(1)          &   ! NO2 (umol L-1)
            , NH4(1)          &   ! NH4 (umol L-1)
            , PO4(1)          &   ! PO4 (umol L-1)
# endif
            , tau             &   ! bottom shear stress (N m-2)
            , 0.0d0           &   ! sedimentation rate (??)
!           output parameters
            , Flux_DIC       &   ! DIC uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
            , Flux_TA        &   ! TA  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
            , Flux_DO        &   ! DO  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
# if defined ORGANIC_MATTER
            , Flux_DOC(iLbl)       &   ! DOC uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
            , Flux_POC(iDtr)       &   ! POC uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
# endif
# if defined CORAL_INGESTION
!            , Flux_PHY(iDnfl)       &   ! Phytoplankton ingestion rate (nmol cm-2 s-1)  * direction of water column to coral is positive
            , Flux_ZOO(iGzoo)       &   ! Zooplankton ingestion rate (nmol cm-2 s-1)  * direction of water column to coral is positive
# endif
# if defined CORAL_CARBON_ISOTOPE
            , Flux_DI13C     &   ! DI13C uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
# endif
# if defined CORAL_NUTRIENTS
            , Flux_NO3       &   ! NO3 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
!            , Flux_NO2       &   ! NO2 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
            , Flux_NH4       &   ! NH4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
            , Flux_PO4       &   ! PO4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
#  if defined ORGANIC_MATTER
            , Flux_DON(iLbl)       &   ! DON uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
            , Flux_PON(iDtr)       &   ! PON uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
            , Flux_DOP(iLbl)       &   ! DOP uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
            , Flux_POP(iDtr)       &   ! POP uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
#  endif
# endif
            )

        END DO

        ! 1 nmol cm-2 s-1 = 0.01 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
        ! cff: convert [nmol cm-2 s-1] to [umol L-1 s-1]

        cff=0.01d0 /dz(1) * p_coral(icl)*P2R(icl)
         
        dDIC_dt(1) = dDIC_dt(1) - Flux_DIC * cff/rho_sw(1)
        dTA_dt (1) = dTA_dt (1) - Flux_TA  * cff/rho_sw(1)
        dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
# if defined ORGANIC_MATTER
        dDOC_dt(1,iLbl) = dDOC_dt(1,iLbl) - Flux_DOC(iLbl) * cff
        dPOC_dt(1,iDtr) = dPOC_dt(1,iDtr) - Flux_POC(iDtr) * cff
# endif
# if defined CORAL_INGESTION
        dPHY_dt(1,iDnfl) = dPHY_dt(1,iDnfl) - Flux_PHY(iDnfl) * cff
        dZOO_dt(1,iGzoo) = dZOO_dt(1,iGzoo) - Flux_ZOO(iGzoo) * cff
# endif
# if defined CORAL_CARBON_ISOTOPE
        dDI13C_dt(1) = dDI13C_dt(1) - Flux_DI13C * cff/rho_sw(1)
# endif
# if defined CORAL_NUTRIENTS
        dNO3_dt(1) = dNO3_dt(1) - Flux_NO3 * cff
!        dNO2_dt(1) = dNO2_dt(1) - Flux_NO2 * cff
        dNH4_dt(1) = dNH4_dt(1) - Flux_NH4 * cff
        dPO4_dt(1) = dPO4_dt(1) - Flux_PO4 * cff
#  if defined ORGANIC_MATTER
        dDON_dt(1,iLbl) = dDON_dt(1,iLbl) - Flux_DON(iLbl) * cff
        dPON_dt(1,iDtr) = dPON_dt(1,iDtr) - Flux_PON(iDtr) * cff
        dDOP_dt(1,iLbl) = dDOP_dt(1,iLbl) - Flux_DOP(iLbl) * cff
        dPOP_dt(1,iDtr) = dPOP_dt(1,iDtr) - Flux_POP(iDtr) * cff
#  endif
# endif
      END IF
    END DO
#endif

      
#if defined SEAGRASS
!-----------------------------------------------------------------------
! Compute seagrass model.
!-----------------------------------------------------------------------
!
    IF(p_sgrass .gt. 0.0d0) THEN

      CALL seagrass              &
!       input parameters
        ( ng, 1, i, j    &   ! ng: nested grid number; n: seagrass compartment; i,j: position
        , PFDbott        &   ! Photon flux density (umol m-2 s-1)
        , rho_sw(1)      &   ! Density of seawater (g cm-3)
        , DIC(1)         &   ! DIC (umol kg-1)
        , DOx(1)         &   ! DO  (umol L-1)
# if defined NUTRIENTS         
        , NH4(1)         &   ! NH4 concentration (umol L-1)
# endif
# if defined CARBON_ISOTOPE
        , DI13C(1)       &   ! 13C of DIC (umol kg-1)
# endif
!       output parameters
        , Flux_DIC      &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_DO       &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# if defined NUTRIENTS         
        , Flux_NO3      &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_NH4      &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_PO4      &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
# if defined CARBON_ISOTOPE
        , Flux_DI13C    &   ! DI13C uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
        )

      ! 1 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
      ! cff: convert [mmol m-2 s-1] to [umol L-1 s-1]

      cff = 1.0d0/dz(1) * p_sgrass

      dDIC_dt(1) = dDIC_dt(1) - Flux_DIC * cff/rho_sw(1)
      dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
# if defined NUTRIENTS
      dNO3_dt(1) = dNO3_dt(1) - Flux_NO3 * cff
      dNH4_dt(1) = dNH4_dt(1) - Flux_NH4 * cff
      dPO4_dt(1) = dPO4_dt(1) - Flux_PO4 * cff
# endif
# if defined CARBON_ISOTOPE
      dDI13C_dt(1) = dDI13C_dt(1) - Flux_DI13C * cff/rho_sw(1)
# endif

    END IF
#endif

#ifdef MACROALGAE
!-----------------------------------------------------------------------
! Compute algae model.
!-----------------------------------------------------------------------
!
    IF(p_algae .gt. 0.0d0) THEN
      CALL macroalgae            &
!       input parameters
        ( ng, 1, i, j    &   ! ng: nested grid number; n: seagrass compartment; i,j: position
        , PFDbott        &   ! Photon flux density (umol m-2 s-1)
        , rho_sw(1)      &   ! Density of seawater (g cm-3)
        , DIC(1)         &   ! DIC (umol kg-1)
        , DOx(1)         &   ! DO  (umol L-1)
# if defined NUTRIENTS         
        , NH4(1)         &   ! NH4 concentration (umol L-1)
# endif
# if defined CARBON_ISOTOPE
        , DI13C(1)       &   ! 13C of DIC (umol kg-1)
# endif
!       output parameters
        , Flux_DIC       &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_DO        &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# if defined NUTRIENTS         
        , Flux_NO3       &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_NH4       &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_PO4       &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
# if defined CARBON_ISOTOPE
        , Flux_DI13C     &   ! DI13C uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
        )

      ! 1 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
      ! cff: convert [mmol m-2 s-1] to [umol L-1 s-1]

      cff = 1.0d0/dz(1) * p_algae

      dDIC_dt(1) = dDIC_dt(1) - Flux_DIC * cff/rho_sw(1)
      dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
# if defined NUTRIENTS
      dNO3_dt(1) = dNO3_dt(1) - Flux_NO3 * cff
      dNH4_dt(1) = dNH4_dt(1) - Flux_NH4 * cff
      dPO4_dt(1) = dPO4_dt(1) - Flux_PO4 * cff
# endif
# if defined CARBON_ISOTOPE
      dDI13C_dt(1) = dDI13C_dt(1) - Flux_DI13C * cff/rho_sw(1)
# endif

    END IF            
#endif



#ifdef SEDIMENT_ECOSYS
!-----------------------------------------------------------------------
! Compute sediment ecosystem model.
!-----------------------------------------------------------------------
!
    IF(p_sand .gt. 0.0d0) THEN
# ifdef SEDIMENT_EMPIRICAL
!!!  Empirical sediment model
      CALL sedecosys     &
!       input parameters
        ( ng, i, j       &   ! ng: nested grid number; i,j: position
        , PFDbott        &   ! Photon flux density (umol m-2 s-1)
        , rho_sw(1)      &   ! Density of seawater (g cm-3)
        , DIC(1)         &   ! DIC (umol kg-1)
        , TA (1)         &   ! TA (umol kg-1)
        , DOx(1)         &   ! DO (umol L-1)
#  if defined NUTRIENTS         
        , NH4(1)         &   ! NH4 concentration (umol L-1)
#  endif
#  if defined CARBON_ISOTOPE
        , DI13C(1)       &   ! 13C of DIC (umol kg-1)
#  endif
!       output parameters
        , Flux_DIC       &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_TA        &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_DO        &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
#  if defined NUTRIENTS         
        , Flux_NO3       &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_NH4       &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_PO4       &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
#  endif
#  if defined CARBON_ISOTOPE
        , Flux_DI13C     &   ! DI13C uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
#  endif
        )
     
      ! 1 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
      ! cff: convaert [mmol cm-2 s-1] to [umol L-1 s-1]

      cff=1.0d0 /dz(1) * p_sand

      dDIC_dt(1) = dDIC_dt(1) - Flux_DIC * cff/rho_sw(1)
      dTA_dt (1) = dTA_dt (1) - Flux_TA  * cff/rho_sw(1)
      dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
#  if defined CARBON_ISOTOPE
      dDI13C_dt(1) = dDI13C_dt(1) - Flux_DI13C * cff/rho_sw(1)
#  endif
#  if defined NUTRIENTS
      dNO3_dt(1) = dNO3_dt(1) - Flux_NO3 * cff
!      dNO2_dt(1) = dNO2_dt(1) - Flux_NO2 * cff
      dNH4_dt(1) = dNH4_dt(1) - Flux_NH4 * cff
      dPO4_dt(1) = dPO4_dt(1) - Flux_PO4 * cff
#  endif
# else
!!!  Sediment ecosystem model

! yt_edit to do put output data in his2.nc file
! yt_edit don't run under reef rock (confirm coverage type interactions)
! yt_edit porosity from coverage??

      DO m=1,isplitsed   !!! Loop for sediment model: dtc <= 0.05 sec

        CALL sedecosys      &
!         input parameters
          ( ng, i, j        &   ! ng: nested grid number; i,j: position
          , Nsed            &   ! Number of vertical biological sediment layers
          , dt              &   ! Time step (sec)
          , Tmp(1)          &   ! Tmp: Temperature (oC)
          , Sal(1)          &   ! Sal: Salinity (PSU)
          , TA(1)*rho_sw(1) &   ! TA: Total alkalinity (umol L-1)
          , DOx(1)          &   ! DOx: Dissolved oxygen (umol L-1)
          , DIC(1)*rho_sw(1)&   ! CO2: Dissolved CO2    (µmol L-1)   ! DIC(umol kg-1) * rho(kg L-1) = (umol L-1)
          , 0.0d0           &   ! N2: Dissolved Nitrogen(µmol L-1)
#  if defined ORGANIC_MATTER
          , POC(1,1)         &   ! POC: Particulate organic carbon (DOC: umol L-1)
#  endif
#  if defined NUTRIENTS            
          , NO3(1)         &   ! NO3: NO3 (umol L-1)
          , NH4(1)         &   ! NH4: NH4 (umol L-1)
          , PO4(1)         &   ! PO4: PO4 (umol L-1)
#   if defined ORGANIC_MATTER
          , PON(1,1)         &   ! PON: Particulate organic nitrogen (PON: umol L-1)
          , POP(1,1)         &   ! POP: Particulate organic phosporius (POP: umol L-1)
#   endif
#  endif
#  if defined SULFATE
          , 0.0d0            &  ! Mn2: Manganese ion    (µmol L-1)
          , 0.0d0            &  ! Fe2: iron(II)         (µmol L-1)
          , 0.0d0            &  ! H2S: hydrogen sulfide (µmol L-1)
          , 27300d0          &  ! SO4: sulfate          (µmol L-1)
          , 10d0  / 86400d0  &  ! MnO2 deposition flux  (nmol cm-2 s-1)
          , 100d0 / 86400d0  &  ! FeOOH depositoin flux (nmol cm-2 s-1)    
#  endif
#  if defined ORGANIC_MATTER
          , -Fdep_POC(1)      &   ! POC deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive ! yt_edit POC deposition flux   ! double check advection scheme
#   if defined NUTRIENTS
          , -Fdep_PON(1)  &   ! PON deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , -Fdep_POP(1)  &   ! POP deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#   endif
#  endif
!         output parameters
        ! , Flux_Tmp       &   ! Temperature flux (K cm-2 s-1)  * direction of water column to sediment is positive
        ! , Flux_Sal       &   ! Salinity  flux (cm-2 s-1)  * direction of water column to sediment is positive
          , Flux_TA        &   ! Total Alkalinity flux (??? cm-2 s-1)  * direction of water column to sediment is positive
          , Flux_DO        &   ! DO  flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , Flux_CO2sed    &   ! CO2 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , Flux_N2        &   ! N2 flux
#  if defined ORGANIC_MATTER
          , Flux_POCf       &   ! POC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
          , Flux_POCs       &   ! POC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
#  endif
#  if defined NUTRIENTS
          , Flux_NO3       &   ! NO3 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , Flux_NH4       &   ! NH4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , Flux_PO4       &   ! PO4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#   if defined ORGANIC_MATTER
          , Flux_PONf       &   ! PONf flux (nmol cm-2 s-1) * direction of water column to sediment is positive
          , Flux_PONs       &   ! PONs flux (nmol cm-2 s-1) * direction of water column to sediment is positive
          , Flux_POPf       &   ! POPf flux (nmol cm-2 s-1) * direction of water column to sediment is positive
          , Flux_POPs       &   ! POPs flux (nmol cm-2 s-1) * direction of water column to sediment is positive
#   endif
#  endif
#  if defined SULFATE
          , Flux_Mn2       &   ! Mn2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
          , Flux_MnO2      &   ! MnO2 flux  (nmol cm-2 s-1) * direction of water column to sediment is positive
          , Flux_Fe2       &   ! Fe2 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
        ! , Flux_FeS       &   ! FeS flux    (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
        ! , Flux_FeS2      &   ! FeS2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
          , Flux_FeOOH     &   ! FeOOH flux  (nmol cm-2 s-1) * direction of water column to sediment is positive
        ! , Flux_FeOOH_PO4 &   ! FeOOH=PO43 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
          , Flux_H2S       &   ! H2S flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
        ! , Flux_S0        &   ! S0 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
          , Flux_SO4       &   ! SO4 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
#  endif
        )
      END DO
      
    ! 1 nmol cm-2 s-1 = 0.01 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    ! x[nmol/cm2 s] (umol/1000nmol) (10000cm2/m2) (1/dz)[1/m] (m3/1000L)= x/dz 0.01(umol L-1 s-1)
    ! cff: convaert [nmol cm-2 s-1] to [umol L-1 s-1]
      cff=0.01d0 /dz(1) * p_sand
    ! (umol L-1 s-1) / rho(kg L-1) = (1/rho) (umol kg-1 s-1)

      ! dTA_dt (1) = dTA_dt (1) - Flux_TA  * cff ! yt_edit what are the units? do we need to divide by rho?
      dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
      dDIC_dt(1) = dDIC_dt(1) - Flux_CO2sed * cff/rho_sw(1)

#  if defined ORGANIC_MATTER
      dPOC_dt(1,1) = dPOC_dt(1,1) - (Flux_POCf + Flux_POCs) * cff
#  endif

#  if defined NUTRIENTS
      dNO3_dt(1) = dNO3_dt(1) - Flux_NO3 * cff
      dNH4_dt(1) = dNH4_dt(1) - Flux_NH4 * cff
      dPO4_dt(1) = dPO4_dt(1) - Flux_PO4 * cff
#   if defined ORGANIC_MATTER
      dPON_dt(1,1) = dPON_dt(1,1) - (Flux_PONf + Flux_PONs) * cff
      dPOP_dt(1,1) = dPOP_dt(1,1) - (Flux_POPf + Flux_POPs) * cff
#   endif
#  endif
!#  if defined SULFATE
!      Flux_Mn2         ! Mn2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_MnO2        ! MnO2 flux  (nmol cm-2 s-1) * direction of water column to sediment is positive

!      Flux_Fe2         ! Fe2 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_FeS         ! FeS flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_FeS2        ! FeS2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_FeOOH       ! FeOOH flux  (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_FeOOH_PO4   ! FeOOH=PO43 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive

!      Flux_H2S         ! H2S flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_S0          ! S0 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_SO4         ! SO4 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
!#  endif

!!! yt_debug >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      DO k=1,N
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dDIC_dt(k)) .or. abs(dDIC_dt(k)) > 1.0d15 .or. DIC(k)+dDIC_dt(k)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dDIC_dt(k)    sediment      i =', i, '   j =', j, '   k =', k
          write(*,*) 'yt_debug:     dDIC_dt(k) =', dDIC_dt(k)
          write(*,*) 'yt_debug:     DIC(k) =', DIC(k)
          write(*,*) 'yt_debug:     DIC(k)+dDIC_dt(k)*dt*dz(k) =', DIC(k)+dDIC_dt(k)*dt*dz(k)
        endif
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dTA_dt(k)) .or. abs(dTA_dt(k)) > 1.0d15 .or. TA(k)+dTA_dt(k)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dTA_dt(k)    sediment      i =', i, '   j =', j, '   k =', k
          write(*,*) 'yt_debug:     dTA_dt(k) =', dTA_dt(k)
          write(*,*) 'yt_debug:     TA(k) =', TA(k)
          write(*,*) 'yt_debug:     TA(k)+dTA_dt(k)*dt*dz(k) =', TA(k)+dTA_dt(k)*dt*dz(k)
        endif
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dDOx_dt(k)) .or. abs(dDOx_dt(k)) > 1.0d15 .or. DOx(k)+dDOx_dt(k)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dDOx_dt(k)    sediment      i =', i, '   j =', j, '   k =', k
          write(*,*) 'yt_debug:     dDOx_dt(k) =', dDOx_dt(k)
          write(*,*) 'yt_debug:     DOx(k) =', DOx(k)
          write(*,*) 'yt_debug:     DOx(k)+dDOx_dt(k)*dt*dz(k) =', DOx(k)+dDOx_dt(k)*dt*dz(k)
        endif
#  if defined ORGANIC_MATTER
        DO yt_iter = 1, Ndom
          ! if (isnan() .or. abs() > huge()) then
          if (isnan(dDOC_dt(k,yt_iter)) .or. abs(dDOC_dt(k,yt_iter)) > 1.0d15 .or. DOC(k,yt_iter)+dDOC_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
            write(*,*) 'yt_debug: mod_reef_ecosys.F dDOC_dt(k,Ndom)    sediment      i =', i, '   j =', j, '   k =', k, '   Ndom =', yt_iter
            write(*,*) 'yt_debug:     dDOC_dt(k,Ndom) =', dDOC_dt(k,yt_iter)
            write(*,*) 'yt_debug:     DOC(k,Ndom) =', DOC(k,yt_iter)
            write(*,*) 'yt_debug:     DOC(k,Ndom)+dDOC_dt(k,Ndom)*dt*dz(k) =', DOC(k,yt_iter)+dDOC_dt(k,yt_iter)*dt*dz(k)
          endif
        ENDDO
        DO yt_iter = 1, Npom
          ! if (isnan() .or. abs() > huge()) then
          if (isnan(dPOC_dt(k,yt_iter)) .or. abs(dPOC_dt(k,yt_iter)) > 1.0d15 .or. POC(k,yt_iter)+dPOC_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
            write(*,*) 'yt_debug: mod_reef_ecosys.F dPOC_dt(k,Npom)    sediment      i =', i, '   j =', j, '   k =', k, '   Npom =', yt_iter
            write(*,*) 'yt_debug:     dPOC_dt(k,Npom) =', dPOC_dt(k,yt_iter)
            write(*,*) 'yt_debug:     POC(k,Npom) =', POC(k,yt_iter)
            write(*,*) 'yt_debug:     POC(k,Npom)+dPOC_dt(k,Npom)*dt*dz(k) =', POC(k,yt_iter)+dPOC_dt(k,yt_iter)*dt*dz(k)
          endif
        ENDDO
        DO yt_iter = 1, Nphy
          ! if (isnan() .or. abs() > huge()) then
          if (isnan(dPHY_dt(k,yt_iter)) .or. abs(dPHY_dt(k,yt_iter)) > 1.0d15 .or. PHY(k,yt_iter)+dPHY_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
            write(*,*) 'yt_debug: mod_reef_ecosys.F dPHY_dt(k,Nphy)    sediment      i =', i, '   j =', j, '   k =', k, '   Nphy =', yt_iter
            write(*,*) 'yt_debug:     dPHY_dt(k,Nphy) =', dPHY_dt(k,yt_iter)
            write(*,*) 'yt_debug:     PHY(k,Nphy) =', PHY(k,yt_iter)
            write(*,*) 'yt_debug:     PHY(k,Nphy)+dPHY_dt(k,Nphy)*dt*dz(k) =', PHY(k,yt_iter)+dPHY_dt(k,yt_iter)*dt*dz(k)
          endif
        ENDDO
        DO yt_iter = 1, Nzoo
          ! if (isnan() .or. abs() > huge()) then
          if (isnan(dZOO_dt(k,yt_iter)) .or. abs(dZOO_dt(k,yt_iter)) > 1.0d15 .or. ZOO(k,yt_iter)+dZOO_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
            write(*,*) 'yt_debug: mod_reef_ecosys.F dZOO_dt(k,Nzoo)    sediment      i =', i, '   j =', j, '   k =', k, '   Nzoo =', yt_iter
            write(*,*) 'yt_debug:     dZOO_dt(k,Nzoo) =', dZOO_dt(k,yt_iter)
            write(*,*) 'yt_debug:     ZOO(k,Nzoo) =', ZOO(k,yt_iter)
            write(*,*) 'yt_debug:     ZOO(k,Nzoo)+dZOO_dt(k,Nzoo)*dt*dz(k) =', ZOO(k,yt_iter)+dZOO_dt(k,yt_iter)*dt*dz(k)
          endif
        ENDDO
        DO yt_iter = 1, Npim
          ! if (isnan() .or. abs() > huge()) then
          if (isnan(dPIC_dt(k,yt_iter)) .or. abs(dPIC_dt(k,yt_iter)) > 1.0d15 .or. PIC(k,yt_iter)+dPIC_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
            write(*,*) 'yt_debug: mod_reef_ecosys.F dPIC_dt(k,Npim)    sediment      i =', i, '   j =', j, '   k =', k, '   Npim =', yt_iter
            write(*,*) 'yt_debug:     dPIC_dt(k,Npim) =', dPIC_dt(k,yt_iter)
            write(*,*) 'yt_debug:     PIC(k,Npim) =', PIC(k,yt_iter)
            write(*,*) 'yt_debug:     PIC(k,Npim)+dPIC_dt(k,Npim)*dt*dz(k) =', PIC(k,yt_iter)+dPIC_dt(k,yt_iter)*dt*dz(k)
          endif
        ENDDO
#  endif
#  if defined NUTRIENTS           
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dNO3_dt(k)) .or. abs(dNO3_dt(k)) > 1.0d15 .or. NO3(k)+dNO3_dt(k)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dNO3_dt(k)    sediment      i =', i, '   j =', j, '   k =', k
          write(*,*) 'yt_debug:     dNO3_dt(k) =', dNO3_dt(k)
          write(*,*) 'yt_debug:     NO3(k) =', NO3(k)
          write(*,*) 'yt_debug:     NO3(k)+dNO3_dt(k)*dt*dz(k) =', NO3(k)+dNO3_dt(k)*dt*dz(k)
        endif
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dNH4_dt(k)) .or. abs(dNH4_dt(k)) > 1.0d15 .or. NH4(k)+dNH4_dt(k)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dNH4_dt(k)    sediment      i =', i, '   j =', j, '   k =', k
          write(*,*) 'yt_debug:     dNH4_dt(k) =', dNH4_dt(k)
          write(*,*) 'yt_debug:     NH4(k) =', NH4(k)
          write(*,*) 'yt_debug:     NH4(k)+dNH4_dt(k)*dt*dz(k) =', NH4(k)+dNH4_dt(k)*dt*dz(k)
        endif
        ! if (isnan() .or. abs() > huge()) then
        if (isnan(dPO4_dt(k)) .or. abs(dPO4_dt(k)) > 1.0d15 .or. PO4(k)+dPO4_dt(k)*dt*dz(k) < -100.0d0) then
          write(*,*) 'yt_debug: mod_reef_ecosys.F dPO4_dt(k)    sediment      i =', i, '   j =', j, '   k =', k
          write(*,*) 'yt_debug:     dPO4_dt(k) =', dPO4_dt(k)
          write(*,*) 'yt_debug:     PO4(k) =', PO4(k)
          write(*,*) 'yt_debug:     PO4(k)+dPO4_dt(k)*dt*dz(k) =', PO4(k)+dPO4_dt(k)*dt*dz(k)
        endif
#  if defined ORGANIC_MATTER
        DO yt_iter = 1, Ndom
          ! if (isnan() .or. abs() > huge()) then
          if (isnan(dDON_dt(k,yt_iter)) .or. abs(dDON_dt(k,yt_iter)) > 1.0d15 .or. DON(k,yt_iter)+dDON_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
            write(*,*) 'yt_debug: mod_reef_ecosys.F dDON_dt(k,Ndom)    sediment      i =', i, '   j =', j, '   k =', k, '   Ndom =', yt_iter
            write(*,*) 'yt_debug:     dDON_dt(k,Ndom) =', dDON_dt(k,yt_iter)
            write(*,*) 'yt_debug:     DON(k,Ndom) =', DON(k,yt_iter)
            write(*,*) 'yt_debug:     DON(k,Ndom)+dDON_dt(k,Ndom)*dt*dz(k) =', DON(k,yt_iter)+dDON_dt(k,yt_iter)*dt*dz(k)
          endif
          ! if (isnan() .or. abs() > huge()) then
          if (isnan(dDOP_dt(k,yt_iter)) .or. abs(dDOP_dt(k,yt_iter)) > 1.0d15 .or. DOP(k,yt_iter)+dDOP_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
            write(*,*) 'yt_debug: mod_reef_ecosys.F dDOP_dt(k,Ndom)    sediment      i =', i, '   j =', j, '   k =', k, '   Ndom =', yt_iter
            write(*,*) 'yt_debug:     dDOP_dt(k,Ndom) =', dDOP_dt(k,yt_iter)
            write(*,*) 'yt_debug:     DOP(k,Ndom) =', DOP(k,yt_iter)
            write(*,*) 'yt_debug:     DOP(k,Ndom)+dDOP_dt(k,Ndom)*dt*dz(k) =', DOP(k,yt_iter)+dDOP_dt(k,yt_iter)*dt*dz(k)
          endif
        ENDDO
        DO yt_iter = 1, Npom
          ! if (isnan() .or. abs() > huge()) then
          if (isnan(dPON_dt(k,yt_iter)) .or. abs(dPON_dt(k,yt_iter)) > 1.0d15 .or. PON(k,yt_iter)+dPON_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
            write(*,*) 'yt_debug: mod_reef_ecosys.F dPON_dt(k,Npom)    sediment      i =', i, '   j =', j, '   k =', k, '   Npom =', yt_iter
            write(*,*) 'yt_debug:     dPON_dt(k,Npom) =', dPON_dt(k,yt_iter)
            write(*,*) 'yt_debug:     PON(k,Npom) =', PON(k,yt_iter)
            write(*,*) 'yt_debug:     PON(k,Npom)+dPON_dt(k,Npom)*dt*dz(k) =', PON(k,yt_iter)+dPON_dt(k,yt_iter)*dt*dz(k)
          endif
          ! if (isnan() .or. abs() > huge()) then
          if (isnan(dPOP_dt(k,yt_iter)) .or. abs(dPOP_dt(k,yt_iter)) > 1.0d15 .or. POP(k,yt_iter)+dPOP_dt(k,yt_iter)*dt*dz(k) < -100.0d0) then
            write(*,*) 'yt_debug: mod_reef_ecosys.F dPOP_dt(k,Npom)    sediment      i =', i, '   j =', j, '   k =', k, '   Npom =', yt_iter
            write(*,*) 'yt_debug:     dPOP_dt(k,Npom) =', dPOP_dt(k,yt_iter)
            write(*,*) 'yt_debug:     POP(k,Npom) =', POP(k,yt_iter)
            write(*,*) 'yt_debug:     POP(k,Npom)+dPOP_dt(k,Npom)*dt*dz(k) =', POP(k,yt_iter)+dPOP_dt(k,yt_iter)*dt*dz(k)
          endif
        ENDDO
#  endif
# endif
      ENDDO
      
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(sspH) .or. abs(sspH) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F sspH    sediment      i =', i, '   j =', j
        write(*,*) 'yt_debug:     sspH =', sspH
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(ssfCO2) .or. abs(ssfCO2) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F ssfCO2    sediment      i =', i, '   j =', j
        write(*,*) 'yt_debug:     ssfCO2 =', ssfCO2
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(ssWarg) .or. abs(ssWarg) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F ssWarg    sediment      i =', i, '   j =', j
        write(*,*) 'yt_debug:     ssWarg =', ssWarg
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(ssCO2flux) .or. abs(ssCO2flux) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F ssCO2flux    sediment      i =', i, '   j =', j
        write(*,*) 'yt_debug:     ssCO2flux =', ssCO2flux
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(ssO2flux) .or. abs(ssO2flux) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F ssO2flux    sediment      i =', i, '   j =', j
        write(*,*) 'yt_debug:     ssO2flux =', ssO2flux
      endif
      ! if (isnan() .or. abs() > huge()) then
      if (isnan(PFDbott) .or. abs(PFDbott) > 1.0d15) then
        write(*,*) 'yt_debug: mod_reef_ecosys.F PFDbott    sediment      i =', i, '   j =', j
        write(*,*) 'yt_debug:     PFDbott =', PFDbott
      endif
!!! yt_debug <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# endif

    END IF 
#endif

#if defined ECOSYS_TESTMODE
!------------------------------------------------------------------------
! Print section

    time = time +dt  ! sec
    
    IF(time.ge.dsec) THEN
# ifdef CORAL_POLYP 
      cff1=0.01d0*p_coral(1)*P2R(1)*3600.0d0  ! cff1: convert [nmol cm-2 s-1] to [mmol m-2 h-1]
      cff2=0.01d0*p_coral(2)*P2R(2)*3600.0d0  ! cff1: convert [nmol cm-2 s-1] to [mmol m-2 h-1]
# endif
# ifdef SEAGRASS
      cff3 = p_sgrass*3600.0d0        ! cff2: convert [mmol m-2 s-1] to [mmol m-2 h-1]
# endif
# ifdef MACROALGAE
      cff4 = p_algae *3600.0d0        ! cff3: convert [mmol m-2 s-1] to [mmol m-2 h-1]
# endif
# if defined SEDIMENT_ECOSYS
#  if defined  SEDIMENT_EMPIRIXCAL
      cff5 = p_sand  *3600.0d0        ! cff4: convert [mmol m-2 s-1] to [mmol m-2 h-1]
#  else
      cff5 = 0.01d0*p_sand*3600.0d0   ! cff4: convert [nmol cm-2 s-1] to [mmol m-2 h-1]
#  endif
# endif

      write(40,'(*(g0,", "))') time/86400.0d0,PFDbott             &
# ifdef CORAL_POLYP 
        , CORAL(1)%Pg(1,1,1)*cff1, CORAL(1)%R (1,1,1)*cff1        &
        , (CORAL(1)%Pg(1,1,1)-CORAL(1)%R (1,1,1))*cff1            &
        , CORAL(1)%G (1,1,1)*cff1                                 &
        , CORAL(1)%Pg(2,1,1)*cff2, CORAL(1)%R (2,1,1)*cff2        &
        , (CORAL(1)%Pg(2,1,1)-CORAL(1)%R (2,1,1))*cff2            &
        , CORAL(1)%G (2,1,1)*cff2                                 &
# endif
# ifdef SEAGRASS
        , SGRASS(1)%Pg(1,1,1)*cff3, SGRASS(1)%R (1,1,1)*cff3      &
        , (SGRASS(1)%Pg(1,1,1)-SGRASS(1)%R (1,1,1))*cff3          &
# endif
# ifdef MACROALGAE
        , ALGAE(1)%Pg(1,1,1)*cff4, ALGAE(1)%R (1,1,1)*cff4        &
        , (ALGAE(1)%Pg(1,1,1)-ALGAE(1)%R (1,1,1))*cff4            &
# endif
# ifdef SEDIMENT_ECOSYS
        , SEDECO(1)%Pg(1,1)*cff5, SEDECO(1)%R (1,1)*cff5          &
        , (SEDECO(1)%Pg(1,1)-SEDECO(1)%R (1,1))*cff5              &
        , SEDECO(1)%G (1,1)*cff5                                  &
# endif
        , dDIC_dt(1),dTA_dt(1), dDOx_dt(1)                        &
# if defined ORGANIC_MATTER
        , dDOC_dt(1,1), dPOC_dt(1,1)                              &
# endif
# if defined CARBON_ISOTOPE
        , dDI13C_dt(1)                                            &
# endif
# if defined NUTRIENTS
        , dNO3_dt(1), dNH4_dt(1)                                  &
        , dPO4_dt(1)                                              &
#  if defined ORGANIC_MATTER
        , dDON_dt(1,1), dPON_dt(1,1)                              &
        , dDOP_dt(1,1), dPOP_dt(1,1)                              &
#  endif
# endif
        , sspH, ssfCO2, ssWarg, U10, ssCO2flux, ssO2flux
        
    dsec=dsec+OUTPUT_INTERVAL*60.

    END IF
!-----------------------------------------------------------------------
#endif

    RETURN
  END SUBROUTINE reef_ecosys

!-----------------------------------------------------------------------
!!! **********************************************************************
!!!  Settling calculation
!!! **********************************************************************

  SUBROUTINE bio_vadv(N, dt, dz, z_r, z_w, w, trc, dtrc_dt, Fb_trc)
      
    implicit none
    integer, intent(in ) :: N              ! Number of vertical layer
    real(8), intent(in ) :: dt             ! (s) Time step
    real(8), intent(in ) :: dz(N)          ! (m) vertical layer tickness
    real(8), intent(in ) :: z_r(N)         ! (m) distance between bottom and RHO point
    real(8), intent(in ) :: z_w(0:N)       ! (m) distance between bottom and W point 
    real(8), intent(in ) :: w              ! (m s-1) vertical advection verocity (upper-ward is positive)
    real(8), intent(in ) :: trc(N)         ! (umol L-1) Concentration of tracer   
    real(8), intent(out) :: dtrc_dt(N)     ! (umol L-1 s-1) Time derivative of concentration
    real(8), intent(out) :: Fb_trc         ! (mmol m-2 s-1) Bottom flux from sediment to water column (upper-ward is positive)    

    real(8) :: tnew(N)    
    real(8) :: z_b
    integer :: k, ks
    real(8) :: dtrc_s, trct, tnewt   

!
!   Semi-Lagrangian advective scheme
!
    DO k=1,N
      z_b = z_r(k)-w*dt
      IF (z_b < z_r(1) ) THEN
        tnew(k) = z_b/z_r(1) * trc(1)
        tnew(k) = MAX(tnew(k),0.0d0)
      ELSE IF (z_b > z_r(N)) THEN
        tnew(k) = trc(N) + (z_b-z_r(N))/(z_w(N)-z_r(N))*(0.0d0-trc(N))
        tnew(k) = MAX(tnew(k),0.0d0)
      END IF

      DO ks=2,N
        IF (z_b < z_r(ks)) THEN
          tnew(k) = trc(ks-1) + (z_b-z_r(ks-1))/(z_r(ks)-z_r(ks-1))*(trc(ks)-trc(ks-1))
          EXIT
        END IF
      END DO

    END DO
    
    trct = 0.0d0
    tnewt = 0.0d0
    DO k=1,N
      trct = trct + trc(k)*dz(k)
      tnewt = tnewt + tnew(k)*dz(k)
    END DO
!    dtrc_s = 0.0d0

    IF (w>=0.0d0) THEN
!     If the matter has upper-ward vertical velocity (positive buoyancy),
!     the matter is accumulated at surface layer
      z_b = z_w(N)-w*dt
      DO k=N,1,-1
        IF (z_b > z_w(k-1)) THEN
          dtrc_s = dtrc_s + trc(k)*(z_w(k) - z_b)
          EXIT
        END IF
        dtrc_s = dtrc_s + trc(k)*dz(k)
      END DO
      tnew(N) = tnew(N) + dtrc_s/dz(N)

      tnew(N) = tnew(N) + (trct - tnewt)/dz(N)
    ELSE
!     If the matter has down-ward vertical velocity (sinking velocity),
!     the matter deposits on the seafloor.
      z_b = z_w(0)-w*dt
      DO k=1,N
        IF (z_b < z_w(k)) THEN
          dtrc_s = dtrc_s + trc(k)*(z_b - z_w(k-1))
          EXIT
        END IF
        dtrc_s = dtrc_s + trc(k)*dz(k)
      END DO
      tnew(1) = tnew(1) + dtrc_s/dz(1)
      Fb_trc = dtrc_s/dt

      tnew(1) = tnew(1) + (trct - tnewt)/dz(1)
!      Fb_trc = (trct - tnewt)/dt
    END IF

    DO k=1,N
      dtrc_dt(k) = (tnew(k)-trc(k))/dt
    END DO
     

    RETURN
  END SUBROUTINE bio_vadv


  SUBROUTINE bio_vadv2(N, dt, dz, z_r, z_w, w, trc, dtrc_dt, Fb_trc)
      
    implicit none
    integer, intent(in ) :: N              ! Number of vertical layer
    real(8), intent(in ) :: dt             ! (s) Time step
    real(8), intent(in ) :: dz(N)          ! (m) vertical layer tickness
    real(8), intent(in ) :: z_r(N)         ! (m) distance between bottom and RHO point
    real(8), intent(in ) :: z_w(0:N)       ! (m) distance between bottom and W point 
    real(8), intent(in ) :: w              ! (m s-1) vertical advection velocity (upward is positive)
    real(8), intent(in ) :: trc(N)         ! (umol L-1) Concentration of tracer   
    real(8), intent(out) :: dtrc_dt(N)     ! (umol L-1 s-1) Time derivative of concentration
    real(8), intent(out) :: Fb_trc         ! (mmol m-2 s-1) Bottom flux from sediment to water column (upward is positive)    

    real(8) :: dz_r(N-1)    
    real(8) :: F(0:N),v(0:N-1)
    integer :: k, ks
    integer :: kup
    real(8) :: dtrc_s, trct, tnewt, Courant

!
!   Upstream advective scheme
!
    DO k=1,N-1
      dz_r(k) = z_r(k+1) - z_r(k)
    END DO

    DO k=1,N-1
      Courant = abs( w*dt/dz_r(k) )
      if(Courant > 1 ) then
        v(k) = sign( dz_r(k)/dt, w )
      else
        v(k) = w
      endif
    END DO
    v(0) = v(1)

    if(w>0) then
      kup = 0
      F(0) = 0.0d0
    else
      kup = 1
      F(0) = trc(1)*v(0)
    endif
    DO k=1,N-1
      F(k) = v(k) * trc(K+kup)
    END DO
    F(N) = 0.0d0

    DO k=1,N
      dtrc_dt(k) = (F(k-1)-F(k))/dz(k)
    END DO    
    Fb_trc = F(0)

    RETURN
  END SUBROUTINE bio_vadv2

END MODULE mod_reef_ecosys

