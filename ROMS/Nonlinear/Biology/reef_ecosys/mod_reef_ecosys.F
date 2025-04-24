
!!!=== Copyright (c) 2012-2025 Takashi NAKAMURA  =====

#include "cppdefs.h"


!!!**** MODULE OF ECOSYSTEM MODEL ************************************

MODULE mod_reef_ecosys
#if defined REEF_ECOSYS

contains

!!! **********************************************************************
!!!  Allocate module variables of reef ecosystem model
!!! **********************************************************************
SUBROUTINE allocate_reef_ecosys(ng, Ngrids, LBi, UBi, LBj, UBj)
  USE mod_reef_ecosys_param
# ifdef CORAL_POLYP
  USE mod_coral
# endif
# ifdef SEAGRASS
  USE mod_seagrass
# endif
# ifdef MACROALGAE
  USE mod_macroalgae
# endif
# ifdef SEDIMENT_ECOSYS
  USE mod_sedecosys
# endif
    
  implicit none

  integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj

# ifdef CORAL_POLYP
  CALL allocate_coral(ng, Ngrids, LBi, UBi, LBj, UBj)
  write(*,*) 'yt_debug: mod_reef_ecosys.F allocate_reef_ecosys() finished coral aloc'
# endif
# ifdef SEAGRASS
  CALL allocate_seagrass(ng, Ngrids, LBi, UBi, LBj, UBj)
  write(*,*) 'yt_debug: mod_reef_ecosys.F allocate_reef_ecosys() finished seagrass aloc'
# endif
# ifdef MACROALGAE
  CALL allocate_macroalgae(ng, Ngrids, LBi, UBi, LBj, UBj)
  write(*,*) 'yt_debug: mod_reef_ecosys.F allocate_reef_ecosys() finished macroalgae aloc'
# endif
# ifdef SEDIMENT_ECOSYS
  CALL allocate_sedecosys(ng, Ngrids, LBi, UBi, LBj, UBj)
  write(*,*) 'yt_debug: mod_reef_ecosys.F allocate_reef_ecosys() finished sediment aloc'
# endif
    
  RETURN
END SUBROUTINE allocate_reef_ecosys


!!! **********************************************************************
!!!  Set initial conditions for reef ecosystem model
!!! **********************************************************************
  SUBROUTINE initialize_reef_ecosys(ng, LBi, UBi, LBj, UBj &
      , start_of_new_run    &   ! TRUE = initialize coral, seagass, macroalgae, sediment from start; FALSE = continue from previous run
# ifdef SEAGRASS
      , XI_size, ETA_size   &   ! grid size XI-direction (meters); grid size ETA-direction (meters)
      , p_sgrass            &   ! seagrass coverage (habitat area in grid / grid area)
# endif
    )

    USE mod_reef_ecosys_param
# ifdef CORAL_POLYP
    USE mod_coral
# endif
# ifdef SEAGRASS
    USE mod_seagrass
# endif
# ifdef MACROALGAE
    USE mod_macroalgae
# endif
# ifdef SEDIMENT_ECOSYS
    USE mod_sedecosys
# endif

    integer, intent(in) :: ng, LBi, UBi, LBj, UBj
    logical, intent(in) :: start_of_new_run
# ifdef SEAGRASS
    real(8), intent(in) :: XI_size (    LBi:UBi,LBj:UBj)   ! grid size XI-direction (meters)
    real(8), intent(in) :: ETA_size(    LBi:UBi,LBj:UBj)   ! grid size ETA-direction (meters)
    real(8), intent(in) :: p_sgrass(Nsg,LBi:UBi,LBj:UBj)   ! seagrass coverage (habitat area in grid / grid area)
# endif

    write(*,*) 'yt_debug: mod_reef_ecosys.F initialize_reef_ecosys() start_of_new_run =', start_of_new_run

# ifdef CORAL_POLYP
    CALL initialize_coral(ng, LBi, UBi, LBj, UBj, start_of_new_run)
    write(*,*) 'yt_debug: mod_reef_ecosys.F initialize_reef_ecosys() finished coral init'
# endif
# ifdef SEDIMENT_ECOSYS
    CALL initialize_sedecosys(ng, LBi, UBi, LBj, UBj, start_of_new_run)
    write(*,*) 'yt_debug: mod_reef_ecosys.F initialize_reef_ecosys() finished sediment init'
# endif
# ifdef SEAGRASS
    CALL initialize_seagrass( ng, LBi, UBi, LBj, UBj    &
                            , start_of_new_run          & ! TRUE = initialize coral, seagass, macroalgae, sediment from start; FALSE = continue from previous run
                            , XI_size, ETA_size         & ! grid size XI-direction (meters); grid size ETA-direction (meters)
                            , p_sgrass                  & ! seagrass coverage (habitat area/grid area)
                            )
    write(*,*) 'yt_debug: mod_reef_ecosys.F initialize_reef_ecosys() finished seagrass init'
# endif
# ifdef MACROALGAE
    CALL initialize_macroalgae(ng, LBi, UBi, LBj, UBj)
    write(*,*) 'yt_debug: mod_reef_ecosys.F initialize_reef_ecosys() finished macroalgae init'
# endif

    RETURN
  END SUBROUTINE initialize_reef_ecosys


!!! **********************************************************************
!!!  Main program of reef ecosystem model
!!! **********************************************************************

  SUBROUTINE reef_ecosys         &
!        input parameters
    ( ng, i, j       &   ! ng: nested grid number; i,j: position
    , N              &   ! Number of vertical grid (following ROMS vertical grid)
    , isplitc        &   ! Internal loop counts of coral polyp model
    , isplitsed      &   ! Internal loop counts of sediment ecosystem model
    , date           &   ! Date (days since x or elapsed days) yt_edit best if in phase with clock to maintain consistency on restart
    , dt             &   ! Time step (sec)
    , dz             &   ! dz(N): vertical grid size (m)
    , PFDsurf        &   ! Sea surface photon flux density (umol m-2 s-1)
    , tau            &   ! bottom shear stress (N m-2)
    , pCO2air        &   ! Air CO2 pertial pressure (uatm)
    , U10            &   ! wind speed (m s-1)
# ifdef CORAL_POLYP
    , p_coral        &   ! Coral coverage (0-1)
# endif
# ifdef SEAGRASS
    , p_sgrass       &   ! seagrass coverage (0-1)
# endif
# ifdef MACROALGAE
    , p_algae        &   ! algal coverage (0-1)
# endif
# ifdef SEDIMENT_ECOSYS
    , p_sand         &   ! sediment coverage (0-1)
# endif
    , Tmp            &   ! Tmp(N): Temperature (oC)
    , Sal            &   ! Sal(N): Salinity (PSU)
    , DOx            &   ! DOx(N): Dissolved oxygen (umol O2 L-1)
    , TA             &   ! TA(N) : Total alkalinity (umol kg-1)
    , DIC            &   ! DIC(N_Csp,N): Total dissolved inorganic carbon (umol C kg-1)
    , NO3            &   ! NO3(N_Nsp,N): (umol N L-1)
    , NH4            &   ! NH4(N_Nsp,N): (umol N L-1)
    , PO4            &   ! PO4(N_Psp,N): (umol P L-1)
    , DOC            &   ! DOC(N_Csp,Ndom,N): Dissolved organic carbon       (umol C L-1)
    , POC            &   ! POC(N_Csp,Npom,N): Particulate organic carbon     (umol C L-1)
    , DON            &   ! DON(N_Nsp,Ndom,N): Dissolved organic nitrogen     (umol N L-1)
    , PON            &   ! PON(N_Nsp,Npom,N): Particulate organic nitrogen   (umol N L-1)
    , DOP            &   ! DOP(N_Psp,Ndom,N): Dissolved organic phosporius   (umol P L-1)
    , POP            &   ! POP(N_Psp,Npom,N): Particulate organic phosporius (umol P L-1)
    , PhyC           &   ! PhyC(N_Csp,Nphy,N): phytoplankton C biomass (umol C L-1)
    , ZooC           &   ! ZooC(N_Csp,Nzoo,N): zooplankton C biomass   (umol C L-1)
    , PhyN           &   ! PhyN(N_Nsp,Nphy,N): phytoplankton N biomass (umol N L-1)
    , ZooN           &   ! ZooN(N_Nsp,Nzoo,N): zooplankton N biomass   (umol N L-1)
    , PhyP           &   ! PhyP(N_Psp,Nphy,N): phytoplankton P biomass (umol P L-1)
    , ZooP           &   ! ZooP(N_Psp,Nzoo,N): zooplankton P biomass   (umol P L-1)
    , PIC            &   ! PIC (N_Csp,Npim,N): Particulate inorganic carbon (PIC: umolC L-1), coccolith (CaCO3)
# if defined BLUE_TIDE         
    , H2S            &   ! H2S (N_Ssp,N)     : (umol S L-1)
# endif
# if defined COT_STARFISH         
    , COTe           &   ! COTe(N): COT starfish egg (umol L-1)
    , COTl           &   ! COTl(N): COT starfish larvae (umol L-1)
# endif
# if defined SEDECO_SGD && defined SGD_ON
    , sgd_flux       &   ! sumbarine groundwater discharge rate (cm s-1)  This flux is assumed to be the same throughout the sediment ecosystem layers (no compression/expansion) So volume flux doesn't change but concentrations of tracers do
    , sgd_Tmp        &   ! SGD concentration coming in through bottom: Tmp: Temperature (oC)
    , sgd_Sal        &   ! SGD concentration coming in through bottom: Sal: Salinity (PSU)
    , sgd_TA         &   ! SGD concentration coming in through bottom: TA : Total alkalinity (TA: umol kg-1)
    , sgd_DOx        &   ! SGD concentration coming in through bottom: DOx: Dissolved oxygen (umol L-1)
    , sgd_DIC        &   ! SGD concentration coming in through bottom: DIC: Total dissolved inorganic carbon (DIC: umol kg-1)
    , sgd_NO3        &   ! SGD concentration coming in through bottom: NO3: NO3 (umol L-1)
    , sgd_NH4        &   ! SGD concentration coming in through bottom: NH4: NH4 (umol L-1)
    , sgd_PO4        &   ! SGD concentration coming in through bottom: PO4: PO4 (umol L-1)
    , sgd_DOC        &   ! SGD concentration coming in through bottom: DOC: Total dissolved organic carbon (DOC: umol L-1)
    , sgd_DON        &   ! SGD concentration coming in through bottom: DOC: Total dissolved organic nitrogen (DON: umol L-1)
    , sgd_DOP        &   ! SGD concentration coming in through bottom: DOC: Total dissolved organic phosphrous (DOP: umol L-1)
# endif
# if defined SEDECO_BURIAL
    , Fdep_sed       &   ! Sedimentation rate (g cm-2 s-1) (Positive: sedimentation; Negative: erosion)
# endif
!   output parameters
    , dTemp_dt       &   ! dTemp_dt(N)           : Temperature (oC s-1)
    , dSalt_dt       &   ! dSalt_dt(N)           : Salinity (PSU s-1)
    , dDOx_dt        &   ! dDOx_dt(N)            : dDOx/dt  (umol O2 L-1 s-1) 
    , dTA_dt         &   ! dTA_dt(N)             : dTA/dt   (umol kg-1 s-1) 
    , dDIC_dt        &   ! dDIC_dt(N_Csp,N)      : dDIC/dt  (umol C kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    , dNO3_dt        &   ! dNO3_dt(N_Nsp,N)      : dNO3/dt  (umol N L-1 s-1)
    , dNH4_dt        &   ! dNH4_dt(N_Nsp,N)      : dNH4/dt  (umol N L-1 s-1)
    , dPO4_dt        &   ! dPO4_dt(N_Psp,N)      : dPO4/dt  (umol P L-1 s-1)
    , dDOC_dt        &   ! dDOC_dt(N_Csp,Ndom,N) : dDOC/dt  (umol C L-1 s-1) 
    , dPOC_dt        &   ! dPOC_dt(N_Csp,Npom,N) : dPOC/dt  (umol C L-1 s-1) 
    , dDON_dt        &   ! dDON_dt(N_Nsp,Ndom,N) : dDON/dt  (umol N L-1 s-1)
    , dPON_dt        &   ! dPON_dt(N_Nsp,Npom,N) : dPON/dt  (umol N L-1 s-1)
    , dDOP_dt        &   ! dDOP_dt(N_Psp,Ndom,N) : dDOP/dt  (umol P L-1 s-1)
    , dPOP_dt        &   ! dPOP_dt(N_Psp,Npom,N) : dPOP/dt  (umol P L-1 s-1)
    , dPhyC_dt       &   ! dPhyC_dt(N_Csp,Nphy,N): dPhyC/dt (umol C L-1 s-1)  
    , dZooC_dt       &   ! dZooC_dt(N_Csp,Nzoo,N): dZooC/dt (umol C L-1 s-1)  
    , dPhyN_dt       &   ! dPhyN_dt(N_Nsp,Nphy,N): dPhyN/dt (umol N L-1 s-1)  
    , dZooN_dt       &   ! dZooN_dt(N_Nsp,Nzoo,N): dZooN/dt (umol N L-1 s-1)  
    , dPhyP_dt       &   ! dPhyP_dt(N_Psp,Nphy,N): dPhyP/dt (umol P L-1 s-1)  
    , dZooP_dt       &   ! dZooP_dt(N_Psp,Nzoo,N): dZooP/dt (umol P L-1 s-1)  
    , dPIC_dt        &   ! dPIC_dt (N_Csp,Npim,N): dPIC/dt  (umol C L-1 s-1)
# if defined BLUE_TIDE         
    , dH2S_dt        &   ! dH2S_dt (N_Ssp,N)     : dH2S/dt  (umol S L-1 s-1)
# endif
# if defined COT_STARFISH         
    , dCOTe_dt       &   ! dCOTe/dt(N): (umol L-1 s-1)
    , dCOTl_dt       &   ! dCOTl/dt(N): (umol L-1 s-1)
# endif
    , pH             &   ! pH
    , Warg           &   ! aragonite saturation state
    , Wcal           &   ! calcite saturation state
    , ssfCO2         &   ! sea surface fCO2 (uatm)
    , ssCO2flux      &   ! sea surface CO2 flux (mmol m-2 s-1)
    , ssO2flux       &   ! sea surface O2 flux (mmol m-2 s-1)
    , PFDbott        &   ! Bottom photon flux density (umol m-2 s-1)
!!! mons light model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>KM:Add
# if defined LIGHT_MODEL
    , PFDk           &   ! Column PFD (umol m-2 s-1)
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<KM:Add
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

    USE mod_reef_ecosys_param
    USE mod_geochem
# ifdef CORAL_POLYP
    USE mod_coral
# endif
# ifdef SEAGRASS
    USE mod_seagrass
# endif
# ifdef MACROALGAE
    USE mod_macroalgae
# endif
# ifdef SEDIMENT_ECOSYS
    USE mod_sedecosys
# endif
# ifdef FOODWEB
    USE mod_foodweb
# endif
      
    implicit none

! input parameters
    integer, intent(in) :: ng,i,j     
    integer, intent(in) :: N
    integer, intent(in) :: isplitc     
    integer, intent(in) :: isplitsed     
    real(8), intent(in) :: date
    real(8), intent(in) :: dt         
    real(8), intent(in) :: dz(N)      
    real(8), intent(in) :: PFDsurf    
    real(8), intent(in) :: tau        
    real(8), intent(in) :: pCO2air    
    real(8), intent(in) :: U10        

# ifdef CORAL_POLYP
    real(8), intent(in) :: p_coral(Ncl)
# endif
# ifdef SEAGRASS
    real(8), intent(inout) :: p_sgrass(Nsg)   ! yt_edit fix me! make coverage writeable
# endif
# ifdef MACROALGAE
    real(8), intent(in) :: p_algae(Nag)    
# endif
# ifdef SEDIMENT_ECOSYS
    real(8), intent(in) :: p_sand      
# endif

! raw tracer inputs
    real(8), intent(in) :: Tmp(N)               ! Temperature (oC)
    real(8), intent(in) :: Sal(N)               ! Salinity (PSU)
    real(8), intent(in) :: DOx(N)               ! Dissolved oxygen (umol L-1)
    real(8), intent(in) :: TA (N)               ! Total alkalinity (TA: umol kg-1)
    real(8), intent(in) :: DIC(N_Csp,N)         ! Total dissolved inorganic carbon (DIC: umol kg-1)
    real(8), intent(in) :: NO3(N_Nsp,N)         ! NO3 (umol L-1)
    real(8), intent(in) :: NH4(N_Nsp,N)         ! NH4 (umol L-1)
    real(8), intent(in) :: PO4(N_Psp,N)         ! PO4 (umol L-1)
    real(8), intent(in) :: DOC(N_Csp,Ndom,N)    ! Dissolved organic carbon (DOC: umol L-1)
    real(8), intent(in) :: POC(N_Csp,Npom,N)    ! Particulate organic carbon (POC: umol L-1)
    real(8), intent(in) :: DON(N_Nsp,Ndom,N)    ! Labile Dissolved organic nitrogen (DON: umol L-1)
    real(8), intent(in) :: PON(N_Nsp,Npom,N)    ! Particulate organic nitrogen (PON: umol L-1)
    real(8), intent(in) :: DOP(N_Psp,Ndom,N)    ! Labile Dissolved organic phosporius (DOP: umol L-1)
    real(8), intent(in) :: POP(N_Psp,Npom,N)    ! Particulate organic phosporius (POP: umol L-1)
    real(8), intent(in) :: PhyC(N_Csp,Nphy,N)   ! phytoplankton C biomass (umol C L-1), dinoflagellate
    real(8), intent(in) :: ZooC(N_Csp,Nzoo,N)   ! zooplankton C biomass (umol C L-1)
    real(8), intent(in) :: PhyN(N_Nsp,Nphy,N)   ! phytoplankton N biomass (umol N L-1), dinoflagellate
    real(8), intent(in) :: ZooN(N_Nsp,Nzoo,N)   ! zooplankton N biomass (umol N L-1)
    real(8), intent(in) :: PhyP(N_Psp,Nphy,N)   ! phytoplankton P biomass (umol P L-1), dinoflagellate
    real(8), intent(in) :: ZooP(N_Psp,Nzoo,N)   ! zooplankton P biomass (umol P L-1)
    real(8), intent(in) :: PIC (N_Csp,Npim,N)   ! Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
# if defined BLUE_TIDE         
    real(8), intent(in) :: H2S (N_Ssp,N)        ! H2S (umol S L-1)
# endif
# if defined COT_STARFISH         
    real(8), intent(in) :: COTe(N)         ! COT starfish egg (umol L-1)
    real(8), intent(in) :: COTl(N)         ! COT starfish larvae (umol L-1)
# endif
# if defined SEDECO_SGD && defined SGD_ON
    real(8), intent(in) :: sgd_flux        ! sumbarine groundwater discharge rate of grid (cm s-1)  
    real(8), intent(in) :: sgd_Tmp         ! SGD concentration coming in through bottom: Tmp: Temperature (oC)
    real(8), intent(in) :: sgd_Sal         ! SGD concentration coming in through bottom: Sal: Salinity (PSU)
    real(8), intent(in) :: sgd_DIC(N_Csp)  ! SGD concentration coming in through bottom: DIC: Total dissolved inorganic carbon (DIC: umol kg-1)
    real(8), intent(in) :: sgd_TA          ! SGD concentration coming in through bottom: TA : Total alkalinity (TA: umol kg-1)
    real(8), intent(in) :: sgd_DOx         ! SGD concentration coming in through bottom: DOx: Dissolved oxygen (umol L-1)
    real(8), intent(in) :: sgd_NO3(N_Nsp)  ! SGD concentration coming in through bottom: NO3: NO3 (umol L-1)
    real(8), intent(in) :: sgd_NH4(N_Nsp)  ! SGD concentration coming in through bottom: NH4: NH4 (umol L-1)
    real(8), intent(in) :: sgd_PO4(N_Psp)  ! SGD concentration coming in through bottom: PO4: PO4 (umol L-1)
    real(8), intent(in) :: sgd_DOC(N_Csp,Ndom)  ! SGD concentration coming in through bottom: DOC: Total dissolved organic carbon (DOC: umol L-1)
    real(8), intent(in) :: sgd_DON(N_Nsp,Ndom)  ! SGD concentration coming in through bottom: DOC: Total dissolved organic nitrogen (DON: umol L-1)
    real(8), intent(in) :: sgd_DOP(N_Psp,Ndom)  ! SGD concentration coming in through bottom: DOC: Total dissolved organic phosphrous (DOP: umol L-1)
# endif
# if defined SEDECO_BURIAL
    real(8), intent(in) :: Fdep_sed        ! Sedimentation rate (cm s-1) (Positive: sedimentation; Negative: erosion)
# endif

! output parameters
    real(8), intent(out) :: dTemp_dt(N)            ! Tmp(N): Temperature (oC)
    real(8), intent(out) :: dSalt_dt(N)            ! Sal(N): Salinity (PSU)
    real(8), intent(out) :: dDOx_dt(N)             ! dDOx/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dTA_dt (N)             ! dTA/dt   (umol kg-1 s-1) 
    real(8), intent(out) :: dDIC_dt(N_Csp,N)       ! dDIC/dt  (umol kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    real(8), intent(out) :: dNO3_dt(N_Nsp,N)       ! dNO3/dt (umol L-1 s-1)
    real(8), intent(out) :: dNH4_dt(N_Nsp,N)       ! dNH4/dt (umol L-1 s-1)
    real(8), intent(out) :: dPO4_dt(N_Psp,N)       ! dPO4/dt (umol L-1 s-1)
    real(8), intent(out) :: dDOC_dt(N_Csp,Ndom,N)  ! dDOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPOC_dt(N_Csp,Npom,N)  ! dPOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dDON_dt(N_Nsp,Ndom,N)  ! dDON/dt (umol L-1 s-1)
    real(8), intent(out) :: dPON_dt(N_Nsp,Npom,N)  ! dPON/dt (umol L-1 s-1)
    real(8), intent(out) :: dDOP_dt(N_Psp,Ndom,N)  ! dDOP/dt (umol L-1 s-1)
    real(8), intent(out) :: dPOP_dt(N_Psp,Npom,N)  ! dPOP/dt (umol L-1 s-1)
    real(8), intent(out) :: dPhyC_dt(N_Csp,Nphy,N) ! dPhyC/dt  (umolC L-1 s-1)  
    real(8), intent(out) :: dZooC_dt(N_Csp,Nzoo,N) ! dZooC/dt  (umolC L-1 s-1)  
    real(8), intent(out) :: dPhyN_dt(N_Nsp,Nphy,N) ! dPhyN/dt  (umolN L-1 s-1)  
    real(8), intent(out) :: dZooN_dt(N_Nsp,Nzoo,N) ! dZooN/dt  (umolN L-1 s-1)  
    real(8), intent(out) :: dPhyP_dt(N_Psp,Nphy,N) ! dPhyP/dt  (umolP L-1 s-1)  
    real(8), intent(out) :: dZooP_dt(N_Psp,Nzoo,N) ! dZooP/dt  (umolP L-1 s-1)  
    real(8), intent(out) :: dPIC_dt (N_Csp,Npim,N) ! dPIC/dt  (umol L-1 s-1) 
# if defined BLUE_TIDE         
    real(8), intent(out) :: dH2S_dt (N_Ssp,N)      ! dH2S/dt  (umol S L-1 s-1)
# endif
# if defined COT_STARFISH         
    real(8), intent(out) :: dCOTe_dt(N)    ! dCOTe/dt (umol L-1 s-1)
    real(8), intent(out) :: dCOTl_dt(N)    ! dCOTl/dt (umol L-1 s-1)
# endif
    real(8), intent(out) :: pH(N)
    real(8), intent(out) :: Warg(N)
    real(8), intent(out) :: Wcal(N)
    real(8), intent(out) :: ssfCO2    
    real(8), intent(out) :: ssCO2flux 
    real(8), intent(out) :: ssO2flux  
    real(8), intent(out) :: PFDbott   
!!! mons light model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>KM:Add
# if defined LIGHT_MODEL
    real(8), intent(out) :: PFDk(N)  
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<KM:Add
    real(8), parameter :: AttSW  = 0.12d0     ! Light attenuation due to seawater [1/m], {0.04d0}.
    real(8), parameter :: AttChl = 0.02486d0  ! Light attenuation by chlorophyll [1/(mg_Chl m2)], {0.02486d0}.
# ifdef CORAL_POLYP
    real(8), parameter :: P2R(Ncl) = (/ 20.0d0, 2.5d0 /)  !!! Conversion factor from polyp scale to reef scale
!    real(8), parameter :: P2R(Ncl) = (/ 5.0d0, 3.0d0 /)  !!! Conversion factor from polyp scale to reef scale
                                    !*~9.0d0 convert projected area to coral surface area for branching Acropora (Naumann et al. 2009)
# endif
    
    real(8) :: dtc     ! Internal time step for coral polyp model (sec)
    real(8) :: datei   ! Date of internal calculation (day)
    real(8) :: dtsed   ! Internal time step for sediment ecosystem model (sec)
  
    integer :: k, m, itrc,isp
    integer :: yt_iter ! yt_debug
    integer :: icl, itic
    real(8) :: TmpK
    real(8) :: rho_sw(N)
    real(8) :: DOsatu    
    real(8) :: cCO2aq, cHCO3, cCO3
    real(8) :: rCO2air(N_Csp), rDIC(N_Csp)
    real(8) :: PFD, Att, AttFac, ExpAtt, Itop
    real(8) :: cff, cff1, cff2, cff3, cff4, cff5
!!! mons light model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>KM:Add
    real(8) :: sza, tPOC, TSS
    real(8) :: Aw490, Aop440, Aop490, Aip440, Aip490, Ag440, Ag490, Bbw490, Bbp490, Bbp560, At490, Bbt490, Kd490, Kdpar
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<KM:Add

    real(8) :: Flux_Tmp, Flux_Sal
    real(8) :: Flux_DIC(N_Csp), Flux_TA,  Flux_DO
    real(8) :: Flux_DOC(N_Csp,Ndom), Flux_POC(N_Csp,Npom)
    real(8) :: Flux_PhyC(N_Csp,Nphy), Flux_ZooC(N_Csp,Nzoo)  
    real(8) :: Flux_PhyN(N_Nsp,Nphy), Flux_ZooN(N_Nsp,Nzoo)  
    real(8) :: Flux_PhyP(N_Psp,Nphy), Flux_ZooP(N_Psp,Nzoo)  
    real(8) :: Flux_PIC(N_Csp,Npim)  
    real(8) :: Flux_NO3(N_Nsp), Flux_NH4(N_Nsp), Flux_PO4(N_Psp)  !, Flux_NO2
    real(8) :: Flux_DON(N_Nsp,Ndom), Flux_PON(N_Nsp,Npom)    
    real(8) :: Flux_DOP(N_Psp,Ndom), Flux_POP(N_Psp,Npom)    
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
    real(8) :: Mn2, Fe2, H2S(N_Ssp,N), SO4(N_Ssp)
    real(8) :: Fdep_MnO2, Fdep_FeOOH 
    real(8) :: Fdep_FeOOH_PO4(N_Psp)
    real(8) :: Fdep_FeS(N_Ssp), Fdep_FeS2(N_Ssp), Fdep_S0(N_Ssp)
    real(8) :: Flux_Mn2, Flux_MnO2 
    real(8) :: Flux_Fe2, Flux_FeS(N_Ssp), Flux_FeS2(N_Ssp)
    real(8) :: Flux_FeOOH, Flux_FeOOH_PO4(N_Psp)
    real(8) :: Flux_H2S(N_Ssp), Flux_S0(N_Ssp), Flux_SO4(N_Ssp) ! ,Flux_HS, Flux_S2
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add
    real(8) :: dDIC(N_Csp,N), dTA(N),  dDOx(N)
    real(8) :: dDOC(N_Csp,Ndom,N), dPOC(N_Csp,Npom,N)
    real(8) :: dPhyC(N_Csp,Nphy,N), dZooC(N_Csp,Nzoo,N)  
    real(8) :: dPhyN(N_Nsp,Nphy,N), dZooN(N_Nsp,Nzoo,N)  
    real(8) :: dPhyP(N_Psp,Nphy,N), dZooP(N_Psp,Nzoo,N)  
    real(8) :: dPIC(N_Csp,Npim,N)
    real(8) :: dNO3(N_Nsp,N), dNH4(N_Nsp,N), dPO4(N_Psp,N)
    real(8) :: dDON(N_Nsp,Ndom,N), dPON(N_Nsp,Npom,N)
    real(8) :: dDOP(N_Psp,Ndom,N), dPOP(N_Psp,Npom,N)
# if defined BLUE_TIDE         
    real(8) :: dH2S(N_Ssp,N)
# endif
# if defined COT_STARFISH         
    real(8) :: dCOTe(N), dCOTl(N)
# endif

    real(8) :: w_PM
    real(8) :: z_r(N)      
    real(8) :: z_w(0:N)  
    real(8) :: Fdep_POC(N_Csp,Npom)
    real(8) :: Fdep_PIC(N_Csp,Npim)
    real(8) :: Fdep_PON(N_Nsp,Npom)
    real(8) :: Fdep_POP(N_Nsp,Npom)
# if defined ECOSYS_TESTMODE
!  Output
!    real(8), parameter :: ECOSYS_OUTPUT_INTERVAL = 1.0d0     ! Output interval (min)
    real(8), save :: dsec = 0.0d0 !sec
# endif

!-----------------------------------------------------------------------
! Initialize all time difference values (d_XXX)
!-----------------------------------------------------------------------
      
    dtc = dt/dble(isplitc)                  ! Internal time step for coral polyp model (sec)
    dtsed = dt/dble(isplitsed)              ! Internal time step for sediment ecosystem model (sec)
    
    DO k=1,N

      rho_sw(k) = densSW( Tmp(k),Sal(k) )  ! Density of seawater [g/cm3]

      dTemp_dt(k) = 0.0d0         ! Tmp(N): Temperature (oC)
      dSalt_dt(k) = 0.0d0         ! Sal(N): Salinity (PSU)
      dDOx_dt(k) = 0.0d0
      dTA_dt(k)  = 0.0d0
      dDIC_dt(:,k) = 0.0d0
      dNO3_dt(:,k) = 0.0d0
      dNH4_dt(:,k) = 0.0d0
      dPO4_dt(:,k) = 0.0d0
      dDOC_dt(:,:,k) = 0.0d0
      dPOC_dt(:,:,k) = 0.0d0
      dDON_dt(:,:,k) = 0.0d0
      dPON_dt(:,:,k) = 0.0d0
      dDOP_dt(:,:,k) = 0.0d0
      dPOP_dt(:,:,k) = 0.0d0
      dPhyC_dt(:,:,k) = 0.0d0
      dZooC_dt(:,:,k) = 0.0d0
      dPhyN_dt(:,:,k) = 0.0d0
      dZooN_dt(:,:,k) = 0.0d0
      dPhyP_dt(:,:,k) = 0.0d0
      dZooP_dt(:,:,k) = 0.0d0
      dPIC_dt(:,:,k) = 0.0d0
# if defined BLUE_TIDE         
      dH2S_dt(:,k)  = 0.0d0
# endif
      pH(k)   = 8.0d0   ! sea surface pH
      Warg(k) = 4.0d0   ! aragonite saturation state
      Wcal(k) = 4.0d0   ! calcite saturation state
    END DO
    ssfCO2    = 380.0d0 ! sea surface fCO2 (uatm)
    ssCO2flux = 0.0d0   ! sea surface CO2 flux (mmol m-2 s-1)
    ssO2flux  = 0.0d0   ! sea surface O2 flux (mmol m-2 s-1)
    PFDbott   = 0.0d0   ! Bottom photon flux density (umol m-2 s-1)

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

    pH(N) = pH_fromATCT( TA(N), DIC(iCt,N),TmpK, Sal(N) )
    cCO2aq = cCO2aq_fromCTpH( DIC(iCt,N), pH(N), TmpK, Sal(N) )
!    cHCO3 = cHCO3_fromCTpH( DIC(N), pH(N), TmpK, Sal(N) )
!    cCO3 = cCO3_fromCTpH( DIC(N), pH(N), TmpK, Sal(N) )
    ssfCO2 = fCO2_fromcCO2aq( cCO2aq, TmpK, Sal(N) )  !! for output
!    Warg(N) = Warg_fromcCO3( cCO3, TmpK, Sal(N) )

!----------- CO2 gas exchange rate (mmol m-2 s-1). -------------------
    ssCO2flux = Flux_CO2(ssfCO2, pCO2air, U10, TmpK, Sal(N) )  ! sea to air is positive

    IF(ssCO2flux*0.0d0 /= 0.0d0) THEN  !!!---------Error Handling: Check NAN
!      write(50,*) ssCO2flux, TA(N), DIC(N),TmpK, Sal(N), pH(N)
      ssCO2flux = 0.0d0
    END IF
    
# if defined AIR_SEA_GAS_EXCHANGE
    dDOx_dt(N) = dDOx_dt(N) - ssO2flux  /dz(N)

!    dDIC_dt(iCt,N) = dDIC_dt(iCt,N) - ssCO2flux  /dz(N)
#  if defined CARBON_TRACE
    rCO2air(:) = 0.0d0
    rDIC(:)    = 0.0d0
#  endif
    rCO2air(iCt) = 1.0d0
    rDIC(iCt)    = 1.0d0
#  if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
    rCO2air(iC13) = R_from_delta(-10.d0,R13C_VPDB)
    rDIC(iC13)    = Iso_ratio(DIC(iC13,N),DIC(iCt,N))
#  endif
#  if defined CLUMPED_ISOTOPE
    rCO2air(iD47) = R_from_delta(?????,???????) !!!!!!!!!!!!!!!!!!!!!!!!!!!!! Update needed !!!!!!!!!!!!!!!
    rDIC(iD47)    = Iso_ratio(DIC(iD47,N),DIC(iCt,N))
#  endif
    dDIC_dt(:,N) = dDIC_dt(:,N)- ( (ssCO2flux+ABS(ssCO2flux))*rDIC(:)                  &
                                  +(ssCO2flux-ABS(ssCO2flux))*rCO2air(:) )*0.5d0/dz(N)

# endif

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
    
# if defined LIGHT_MODEL
      PFDk(k) = PFD
# endif

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

!      Att= AttSW*dz(k)
!      ExpAtt=EXP(-Att)
!      Itop=PFD
!      PFD=Itop*(1.0d0-ExpAtt)/Att    ! average at cell center

# if defined FOODWEB
!-----------------------------------------------------------------------
! Food web model.
!-----------------------------------------------------------------------

      CALL foodweb     &
    !   input parameters
        ( ng, n, i, j  &   ! ng: nested grid number; n: coral compartment; i,j: position
        , date         &   ! Date (days since x or elapsed days)
        , dt           &   ! Time step (sec)
        , PFD          &   ! Photon flux density (umol m-2 s-1)
        , rho_sw(k)    &   ! Density of seawater (g cm-3)
        , Tmp(k)       &   ! Temperature (oC)
        , Sal(k)       &   ! Salinity (PSU)
        , DOx(k)       &   ! DOx: Dissolved oxygen (umol O2 L-1)
        , TA (k)       &   ! TA: Total alkalinity (umol kg-1)
        , DIC(:,k)     &   ! DIC(N_Csp): Total dissolved inorganic carbon (umol C kg-1)
        , NO3(:,k)     &   ! NO3(N_Nsp): (umol N L-1)
        , NH4(:,k)     &   ! NH4(N_Nsp): (umol N L-1)
        , PO4(:,k)     &   ! PO4(N_Psp): (umol P L-1)
        , DOC(:,:,k)   &   ! DOC(N_Csp,Ndom): Dissolved organic carbon       (umol C L-1)
        , POC(:,:,k)   &   ! POC(N_Csp,Npom): Particulate organic carbon     (umol C L-1)
        , DON(:,:,k)   &   ! DON(N_Nsp,Ndom): Dissolved organic nitrogen     (umol N L-1)
        , PON(:,:,k)   &   ! PON(N_Nsp,Npom): Particulate organic nitrogen   (umol N L-1)
        , DOP(:,:,k)   &   ! DOP(N_Psp,Ndom): Dissolved organic phosporius   (umol P L-1)
        , POP(:,:,k)   &   ! POP(N_Psp,Npom): Particulate organic phosporius (umol P L-1)
        , PhyC(:,:,k)  &   ! PhyC(N_Csp,Nphy): phytoplankton C biomass (umol C L-1)
        , ZooC(:,:,k)  &   ! ZooC(N_Csp,Nzoo): zooplankton C biomass   (umol C L-1)
        , PhyN(:,:,k)  &   ! PhyN(N_Nsp,Nphy): phytoplankton N biomass (umol N L-1)
        , ZooN(:,:,k)  &   ! ZooN(N_Nsp,Nzoo): zooplankton N biomass   (umol N L-1)
        , PhyP(:,:,k)  &   ! PhyP(N_Psp,Nphy): phytoplankton P biomass (umol P L-1)
        , ZooP(:,:,k)  &   ! ZooP(N_Psp,Nzoo): zooplankton P biomass   (umol P L-1)
        , PIC (:,:,k)  &   ! PIC (N_Csp,Npim): Particulate inorganic carbon (PIC: umolC L-1), coccolith (CaCO3)
#  if defined BLUE_TIDE         
        , H2S(:,k)     &   ! H2S(N_Ssp): (umol S L-1)
#  endif
#  if defined COT_STARFISH         
        , COTe(k)      &   ! COT starfish egg (umol L-1)
        , COTl(k)      &   ! COT starfish larvae (umol L-1)
#  endif
    !   output parameters
        , dDOx(k)      &   ! dDOx_dt             : dDOx/dt  (umol O2 L-1 s-1) 
        , dTA (k)      &   ! dTA_dt              : dTA/dt   (umol kg-1 s-1) 
        , dDIC(:,k)    &   ! dDIC_dt(N_Csp)      : dDIC/dt  (umol C kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
        , dNO3(:,k)    &   ! dNO3_dt(N_Nsp)      : dNO3/dt  (umol N L-1 s-1)
        , dNH4(:,k)    &   ! dNH4_dt(N_Nsp)      : dNH4/dt  (umol N L-1 s-1)
        , dPO4(:,k)    &   ! dPO4_dt(N_Psp)      : dPO4/dt  (umol P L-1 s-1)
        , dDOC(:,:,k)  &   ! dDOC_dt(N_Csp,Ndom) : dDOC/dt  (umol C L-1 s-1) 
        , dPOC(:,:,k)  &   ! dPOC_dt(N_Csp,Npom) : dPOC/dt  (umol C L-1 s-1) 
        , dDON(:,:,k)  &   ! dDON_dt(N_Nsp,Ndom) : dDON/dt  (umol N L-1 s-1)
        , dPON(:,:,k)  &   ! dPON_dt(N_Nsp,Npom) : dPON/dt  (umol N L-1 s-1)
        , dDOP(:,:,k)  &   ! dDOP_dt(N_Psp,Ndom) : dDOP/dt  (umol P L-1 s-1)
        , dPOP(:,:,k)  &   ! dPOP_dt(N_Psp,Npom) : dPOP/dt  (umol P L-1 s-1)
        , dPhyC(:,:,k) &   ! dPhyC_dt(N_Csp,Nphy): dPhyC/dt (umol C L-1 s-1)  
        , dZooC(:,:,k) &   ! dZooC_dt(N_Csp,Nzoo): dZooC/dt (umol C L-1 s-1)  
        , dPhyN(:,:,k) &   ! dPhyN_dt(N_Nsp,Nphy): dPhyN/dt (umol N L-1 s-1)  
        , dZooN(:,:,k) &   ! dZooN_dt(N_Nsp,Nzoo): dZooN/dt (umol N L-1 s-1)  
        , dPhyP(:,:,k) &   ! dPhyP_dt(N_Psp,Nphy): dPhyP/dt (umol P L-1 s-1)  
        , dZooP(:,:,k) &   ! dZooP_dt(N_Psp,Nzoo): dZooP/dt (umol P L-1 s-1)  
        , dPIC (:,:,k) &   ! dPIC_dt (N_Csp,Npim): dPIC/dt  (umol C L-1 s-1)
#  if defined BLUE_TIDE         
        , dH2S(:,k)    &   ! dH2S_dt(N_Ssp)      : dH2S/dt  (umol S L-1 s-1)
#  endif
#  if defined COT_STARFISH         
        , dCOTe(k)     &   ! dCOTe/dt (umol L-1 s-1)
        , dCOTl(k)     &   ! dCOTl/dt (umol L-1 s-1)
#  endif
        , pH  (k)      &   ! pH
        , Wcal(k)      &   ! Calcite saturation state
        , Warg(k)      &   ! Aragonite saturation state
        )

       
!----- Mass balance equation -----------------------------------------------------------
      dDOx_dt(k) = dDOx_dt(k) + dDOx(k)
      dTA_dt (k) = dTA_dt (k) + dTA(k)
      dDIC_dt(:,k) = dDIC_dt(:,k) + dDIC(:,k)
      dNO3_dt(:,k) = dNO3_dt(:,k) + dNO3(:,k)
      dNH4_dt(:,k) = dNH4_dt(:,k) + dNH4(:,k)
      dPO4_dt(:,k) = dPO4_dt(:,k) + dPO4(:,k)
      dDOC_dt(:,:,k) = dDOC_dt(:,:,k) + dDOC(:,:,k)
      dPOC_dt(:,:,k) = dPOC_dt(:,:,k) + dPOC(:,:,k)
      dDON_dt(:,:,k) = dDON_dt(:,:,k) + dDON(:,:,k)
      dPON_dt(:,:,k) = dPON_dt(:,:,k) + dPON(:,:,k)
      dDOP_dt(:,:,k) = dDOP_dt(:,:,k) + dDOP(:,:,k)
      dPOP_dt(:,:,k) = dPOP_dt(:,:,k) + dPOP(:,:,k)
      dPhyC_dt(:,:,k) = dPhyC_dt(:,:,k) + dPhyC(:,:,k)
      dZooC_dt(:,:,k) = dZooC_dt(:,:,k) + dZooC(:,:,k)
      dPhyN_dt(:,:,k) = dPhyN_dt(:,:,k) + dPhyN(:,:,k)
      dZooN_dt(:,:,k) = dZooN_dt(:,:,k) + dZooN(:,:,k)
      dPhyP_dt(:,:,k) = dPhyP_dt(:,:,k) + dPhyP(:,:,k)
      dZooP_dt(:,:,k) = dZooP_dt(:,:,k) + dZooP(:,:,k)
      dPIC_dt(:,:,k) = dPIC_dt(:,:,k) + dPIC(:,:,k)
#  if defined BLUE_TIDE         
      dH2S_dt(:,k) = dH2S_dt(:,k) + dH2S(:,k)
#  endif
#  if defined COT_STARFISH
      dCOTe_dt(k) = dCOTe_dt(k) + dCOTe(k)
      dCOTl_dt(k) = dCOTl_dt(k) + dCOTl(k)
#  endif
# endif
                             
!  Light attenuation at the bottom of the grid cell. It is the starting
!  PFD value for the next (deeper) vertical grid cell.
!
!      PFD=Itop*ExpAtt

!!! mons light model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>KM:Add
# if defined LIGHT_MODEL

! constants that should be computed
      sza = 1.5d1 !typical PH SZA                   ! 
      tPOC = POC(k,1) + POC(k,2)

! actual calculations
      Aw490 = 1.5d-2
      Aop440 = 1.48776909221838d-2*MAX(tPOC,0.0d0)**5.75341428428829d-1
      Aop490 = MAX((5.83174d-1+7.66d-12*LOG(MAX(Aop440,1.0d-30)))*Aop440,0.0d0)
      Aip440 = 4.05699607236094d-3*MAX(tPOC,0.0d0)**9.99999435689510d-1
      Aip490 = MAX(Aip440*EXP(-5.6303965d-1),0.0d0)
      Ag440 = -4.27996832826524d-2*MAX(Sal(k),0.0d0)+1.61458559175428d0 
      Ag490 = MAX(Ag440*EXP(-4.2204005d-1),0.0d0) 
      Bbw490 = 1.622d-3
      Bbp560 = 4.28633036350060d-3*MAX(tPOC,0.0d0)**9.99862455045082d-1
      Bbp490 = MAX(Bbp560*1.138105699d0,0.0d0)
      At490 = Aw490+Aip490+Aop490+Ag490
      Bbt490 = Bbw490+Bbp490
      Kd490 = (1.0d0+5.0d-3*sza)*At490+4.18d0*(1.0d0-5.2d-1*EXP(-1.08d1*At490))*Bbt490

      IF (Kd490.GT.1.15d-1) THEN
          Kdpar = 8.1d-1*Kd490**8.256d-1
      ELSE
          Kdpar = 4.6051d0*Kd490/(6.07d0*Kd490+3.2d0)
      END IF

      Att= Kdpar*dz(k)            
      ExpAtt=EXP(-Att)
      Itop=PFD
      PFD = Itop*ExpAtt
# else
!original script
      Att= AttSW*dz(k)
      ExpAtt=EXP(-Att)
      Itop=PFD
      PFD = Itop*ExpAtt    
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<KM:Add

    END DO

    PFDbott=PFD  !!! Photon flux density at sea bottom

# if defined ECOSYS_TESTMODE && defined INPUT_BOTTOM_PFD
    PFDbott = PFDsurf !!! Photon flux density at sea bottom
# endif

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
      DO isp=1, N_Csp
        CALL bio_vadv2(N, dt, dz, z_r, z_w, w_PM, POC(isp,itrc,:), dPOC(isp,itrc,:), Fdep_POC(isp,itrc))
      ENDDO
      DO isp=1, N_Nsp
        CALL bio_vadv2(N, dt, dz, z_r, z_w, w_PM, PON(isp,itrc,:), dPON(isp,itrc,:), Fdep_PON(isp,itrc))
      ENDDO
      DO isp=1, N_Psp
        CALL bio_vadv2(N, dt, dz, z_r, z_w, w_PM, POP(isp,itrc,:), dPOP(isp,itrc,:), Fdep_POP(isp,itrc))
      ENDDO
    END DO

!    DO itrc=2, Npim
!      w_PM = -(rho_PIM(itrc)/1.024d0 - 1.0d0)*grav*D_PIM(itrc)*D_PIM(itrc)*1.0d-6/18.0d0/vis_sw
!      DO isp=1, N_Csp
!        CALL bio_vadv2(N, dt, dz, z_r, z_w, w_PM, PIC(isp,itrc,:), dPIC(isp,itrc,:), Fdep_PIC(isp,itrc))
!      ENDDO
!    END DO
    w_PM = -(rho_PIM(iDead)/1.024d0 - 1.0d0)*grav*D_PIM(iDead)*D_PIM(iDead)*1.0d-6/18.0d0/vis_sw
    DO isp=1, N_Csp
      CALL bio_vadv2(N, dt, dz, z_r, z_w, w_PM, PIC(isp,iDead,:), dPIC(isp,iDead,:), Fdep_PIC(isp,iDead))
    ENDDO

!----- Mass balance equation -----------------------------------------------------------
    dPOC_dt(:,:,:) = dPOC_dt(:,:,:) + dPOC(:,:,:)
    dPON_dt(:,:,:) = dPON_dt(:,:,:) + dPON(:,:,:)
    dPOP_dt(:,:,:) = dPOP_dt(:,:,:) + dPOP(:,:,:)
    dPIC_dt(:,iDead,:) = dPIC_dt(:,iDead,:) + dPIC(:,iDead,:)

!!!---------------------------------------------------------------------
!!!  Benthic interactions.
!!!---------------------------------------------------------------------
    Flux_Tmp = 0.0d0
    Flux_Sal = 0.0d0
    Flux_DO  = 0.0d0
    Flux_TA  = 0.0d0
    Flux_DIC(:) = 0.0d0
    Flux_NO3(:) = 0.0d0
    Flux_NH4(:) = 0.0d0
    Flux_PO4(:) = 0.0d0
    Flux_DOC(:,:) = 0.0d0
    Flux_POC(:,:) = 0.0d0
    Flux_DON(:,:) = 0.0d0
    Flux_PON(:,:) = 0.0d0
    Flux_DOP(:,:) = 0.0d0
    Flux_POP(:,:) = 0.0d0
    Flux_PhyC(:,:) = 0.0d0
    Flux_ZooC(:,:) = 0.0d0
    Flux_PhyN(:,:) = 0.0d0
    Flux_ZooN(:,:) = 0.0d0
    Flux_PhyP(:,:) = 0.0d0
    Flux_ZooP(:,:) = 0.0d0
    Flux_PIC(:,:) = 0.0d0
# if defined BLUE_TIDE         
    Flux_H2S(:) = 0.0d0
# endif

# if defined CORAL_POLYP
!-----------------------------------------------------------------------
! Compute coral polyp model.
!-----------------------------------------------------------------------
!
    DO icl=1,Ncl
!    DO icl=1,1  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Tentative!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF(p_coral(icl) .gt. 0.0d0) THEN          

        DO m=1,isplitc   !!! Loop for coral polyp model: dtc <= 0.05 sec

          datei = date + dtc*(m-1)/86400.0d0
          CALL coral_polyp    &
!           input parameters
            ( ng, icl, i, j   &   ! ng: nested grid number; n: coral compartment; i,j: position
            , datei           &   ! Date (days since x or elapsed days)
            , dtc             &   ! Time step (sec)
            , PFDbott         &   ! Photon flux density (umol m-2 s-1)
            , rho_sw(1)       &   ! Density of seawater (g cm-3)
            , Tmp(1)          &   ! Temperature (oC)
            , Sal(1)          &   ! Salinity (PSU)
            , DOx(1)          &   ! Dissolved oxygen (umol L-1)
            , TA (1)          &   ! Total alkalinity (TA: umol kg-1)
            , DIC(:,1)        &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
#  if defined CORAL_NUTRIENTS
            , NO3(:,1)        &   ! NO3 (umol L-1)
            , NH4(:,1)        &   ! NH4 (umol L-1)
            , PO4(:,1)        &   ! PO4 (umol L-1)
#  endif
#  if defined CORAL_INGESTION
            , ZooC(:,:,1)     &   ! zooplankton C biomass (umol C L-1)
            , ZooN(:,:,1)     &   ! zooplankton N biomass (umol N L-1)
            , ZooP(:,:,1)     &   ! zooplankton P biomass (umol P L-1)
#  endif
            , tau             &   ! bottom shear stress (N m-2)
            , 0.0d0           &   ! sedimentation rate (??)
!           output parameters
            , Flux_DO         &   ! DO  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
            , Flux_TA         &   ! TA  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
            , Flux_DIC(:)     &   ! DIC uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
#  if defined CORAL_NUTRIENTS
            , Flux_NO3(:)     &   ! NO3 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
            , Flux_NH4(:)     &   ! NH4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
            , Flux_PO4(:)     &   ! PO4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
#  endif
            , Flux_DOC(:,:)   &   ! DOC uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
            , Flux_POC(:,:)   &   ! POC uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
#  if defined CORAL_NUTRIENTS
            , Flux_DON(:,:)   &   ! DON uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
            , Flux_PON(:,:)   &   ! PON uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
            , Flux_DOP(:,:)   &   ! DOP uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
            , Flux_POP(:,:)   &   ! POP uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
#  endif
#  if defined CORAL_INGESTION
            , Flux_ZooC(:,:)  &   ! Zooplankton ingestion rate (nmol C cm-2 s-1)  * direction of water column to coral is positive
            , Flux_ZooN(:,:)  &   ! Zooplankton ingestion rate (nmol N cm-2 s-1)  * direction of water column to coral is positive
            , Flux_ZooP(:,:)  &   ! Zooplankton ingestion rate (nmol P cm-2 s-1)  * direction of water column to coral is positive
#  endif
            )

        END DO

        ! 1 nmol cm-2 s-1 = 0.01 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
        ! cff: convert [nmol cm-2 s-1] to [umol L-1 s-1]

        cff=0.01d0 /dz(1) * p_coral(icl)*P2R(icl)
         
        dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
        dTA_dt (1) = dTA_dt (1) - Flux_TA  * cff/rho_sw(1)
        dDIC_dt(:,1) = dDIC_dt(:,1) - Flux_DIC(:) * cff/rho_sw(1)
#  if defined CORAL_NUTRIENTS
        dNO3_dt(:,1) = dNO3_dt(:,1) - Flux_NO3(:) * cff
        dNH4_dt(:,1) = dNH4_dt(:,1) - Flux_NH4(:) * cff
        dPO4_dt(:,1) = dPO4_dt(:,1) - Flux_PO4(:) * cff
#  endif
        dDOC_dt(:,:,1) = dDOC_dt(:,:,1) - Flux_DOC(:,:) * cff
        dPOC_dt(:,:,1) = dPOC_dt(:,:,1) - Flux_POC(:,:) * cff
#  if defined CORAL_NUTRIENTS
        dDON_dt(:,:,1) = dDON_dt(:,:,1) - Flux_DON(:,:) * cff
        dPON_dt(:,:,1) = dPON_dt(:,:,1) - Flux_PON(:,:) * cff
        dDOP_dt(:,:,1) = dDOP_dt(:,:,1) - Flux_DOP(:,:) * cff
        dPOP_dt(:,:,1) = dPOP_dt(:,:,1) - Flux_POP(:,:) * cff
#  endif
#  if defined CORAL_INGESTION
        dZooC_dt(:,:,1) = dZooC_dt(:,:,1) - Flux_ZooC(:,:) * cff
        dZooN_dt(:,:,1) = dZooN_dt(:,:,1) - Flux_ZooN(:,:) * cff
        dZooP_dt(:,:,1) = dZooP_dt(:,:,1) - Flux_ZooP(:,:) * cff
#  endif
      END IF
    END DO
# endif

      
# if defined SEAGRASS
!-----------------------------------------------------------------------
! Compute seagrass model.
!-----------------------------------------------------------------------
!

!!! yuta_seagrass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
    ! write(*,*) 'yt_debug: PFD = ', PFD, 'PFDbott = ', PFDbott
    DO icl=1,Nsg    
      IF(p_sgrass(icl) .gt. 0.0d0) THEN 

        CALL seagrass           &
!       input parameters
          ( ng                  &   ! ng: nested grid number; 
          , icl                 &   ! icl: seagrass species id
          , i, j                &   ! i,j: position
          , dz(1)               &   ! vertical layer thickness (m)
          , date                &   ! Date (days since x or elapsed days) yt_edit best if in phase with clock to maintain consistency on restart
          , dt                  &   ! Time step (sec)
          , p_sgrass(:)         &   ! seagrass coverage (every species)
#  if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
          , p_sand              &   ! Sediment coverage
#  endif
          , PFD                 &   ! Photon flux density (umol m-2 s-1)
          , rho_sw(1)           &   ! Density of seawater (g cm-3)
          , DIC(:,1)*rho_sw(1)  &   ! DIC (umol l-1)            ! [umol/L] = [umol/kg] [g/cm3] (kg/1000g)(1000cm3/L)
          , DOx(1)              &   ! DO  (umol L-1)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE         
          , NO3(:,1)            &   ! NO3 concentration (umol L-1)
          , NH4(:,1)            &   ! NH4 concentration (umol L-1)
          , PO4(:,1)            &   ! PO4 concentration (umol L-1)
#  endif
!       output parameters 
          , Flux_DIC(:)         &   ! DIC uptake rate (mmol m-2 s-1)  * direction of ocean to seagrass is positive
          , Flux_DO             &   ! DO  uptake rate (mmol m-2 s-1)  * direction of ocean to seagrass is positive
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE         
          , Flux_NO3(:)         &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of ocean to seagrass is positive
          , Flux_NH4(:)         &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of ocean to seagrass is positive
          , Flux_PO4(:)         &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of ocean to seagrass is positive
#  endif
          , Flux_DOC(:,iLDOM)   &   ! DOC uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
#  if defined SEAGRASS_LEAF_POM
          , Flux_POC(:,iCPOM)   &   ! POC uptake rate (mmol m-2 s-1)  * direction of ocean to seagrass is positive
#   if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
          , Flux_PON(:,iCPOM)   &   ! PON uptake rate (mmol m-2 s-1)  * direction of ocean to seagrass is positive
          , Flux_POP(:,iCPOM)   &   ! POP uptake rate (mmol m-2 s-1)  * direction of ocean to seagrass is positive
#   endif
#  endif
          )
        !   write(*,*) "yt_debug: finished seagrass call"


        ! dDIC_dt         = Flux_CHEM      / height * coverage_area_fraction / density  * unit_conversion_factor
        ! [umol kg-1 s-1] = [mmol m-2 s-1] / [m]    * []                     / [g cm-3] * [ (1000 umol/mmol) (0.000001 m3/cm3) (1000 g/kg) = 1]

        ! dCHEM_dt        = Flux_CHEM      / height * coverage_area_fraction * unit_conversion_factor
        ! [umol L-1 s-1]  = [mmol/m2 s]    / [m]    * []                     * [ (1000 umol/mmol) (m3/1000 L) = 1]

        ! cff = unit_conversion_factor / height * coverage_area_fraction
        cff = 1.0d0/dz(1) * p_sgrass(icl)
        dDIC_dt(:,1) = dDIC_dt(:,1) - Flux_DIC(:) * cff/rho_sw(1)
        dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
        dNO3_dt(:,1) = dNO3_dt(:,1) - Flux_NO3(:) * cff
        dNH4_dt(:,1) = dNH4_dt(:,1) - Flux_NH4(:) * cff
        dPO4_dt(:,1) = dPO4_dt(:,1) - Flux_PO4(:) * cff
#  if defined SEAGRASS_LEAF_POM
        dPOC_dt(:,iCPOM,1) = dPOC_dt(:,iCPOM,1) - Flux_POC(:,iCPOM) * cff
#   if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
        dPON_dt(:,iCPOM,1) = dPON_dt(:,iCPOM,1) - Flux_PON(:,iCPOM) * cff
        dPOP_dt(:,iCPOM,1) = dPOP_dt(:,iCPOM,1) - Flux_POP(:,iCPOM) * cff
#   endif
#  endif

      ELSE
        CALL no_seagrass              &
  !       input parameters
          ( ng             &   ! ng: nested grid number; 
          , icl            &   ! icl: seagrass species id
          , i, j           &   ! i,j: position
          , dz(1)          &   ! vertical layer thickness (m)
          , date           &   ! Date (days since x or elapsed days) yt_edit best if in phase with clock to maintain consistency on restart
          , dt             &   ! Time step (sec)
          )
      END IF
    END DO
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add



# ifdef MACROALGAE
!-----------------------------------------------------------------------
! Compute algae model.
!-----------------------------------------------------------------------
!
    DO icl=1,Nag    
      IF(p_algae(icl) .gt. 0.0d0) THEN
        CALL macroalgae    &
!         input parameters
          ( ng, icl, i, j  &   ! ng: nested grid number; n: macroalgae compartment; i,j: position
          , date           &   ! Date (days since x or elapsed days)
          , dt             &   ! Time step (sec)
          , PFDbott        &   ! Photon flux density (umol m-2 s-1)
          , rho_sw(1)      &   ! Density of seawater (g cm-3)
          , DOx(1)         &   ! DO  (umol L-1)
          , DIC(:,1)       &   ! DIC (umol kg-1)
          , NO3(:,1)       &   ! NO3 concentration (umol L-1)
          , NH4(:,1)       &   ! NH4 concentration (umol L-1)
          , PO4(:,1)       &   ! NH4 concentration (umol L-1)
!       output parameters
          , Flux_DO        &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
          , Flux_DIC(:)    &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
          , Flux_NO3(:)    &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
          , Flux_NH4(:)    &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
          , Flux_PO4(:)    &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
          , Flux_DOC(:,:)  &   ! DOC uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
          , Flux_POC(:,:)  &   ! POC uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
          , Flux_DON(:,:)  &   ! DON uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
          , Flux_PON(:,:)  &   ! PON uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
          , Flux_DOP(:,:)  &   ! DOP uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
          , Flux_POP(:,:)  &   ! POP uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
          )

      ! 1 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
      ! cff: convert [mmol m-2 s-1] to [umol L-1 s-1]

        cff = 1.0d0/dz(1) * p_algae(icl)
        
        dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
        dTA_dt (1) = dTA_dt (1) - Flux_TA  * cff/rho_sw(1)
        dDIC_dt(:,1) = dDIC_dt(:,1) - Flux_DIC(:) * cff/rho_sw(1)
        dNO3_dt(:,1) = dNO3_dt(:,1) - Flux_NO3(:) * cff
        dNH4_dt(:,1) = dNH4_dt(:,1) - Flux_NH4(:) * cff
        dPO4_dt(:,1) = dPO4_dt(:,1) - Flux_PO4(:) * cff
        dDOC_dt(:,:,1) = dDOC_dt(:,:,1) - Flux_DOC(:,:) * cff
        dPOC_dt(:,:,1) = dPOC_dt(:,:,1) - Flux_POC(:,:) * cff
        dDON_dt(:,:,1) = dDON_dt(:,:,1) - Flux_DON(:,:) * cff
        dPON_dt(:,:,1) = dPON_dt(:,:,1) - Flux_PON(:,:) * cff
        dDOP_dt(:,:,1) = dDOP_dt(:,:,1) - Flux_DOP(:,:) * cff
        dPOP_dt(:,:,1) = dPOP_dt(:,:,1) - Flux_POP(:,:) * cff
      END IF
    END DO
# endif



# ifdef SEDIMENT_ECOSYS
!-----------------------------------------------------------------------
! Compute sediment ecosystem model.
!-----------------------------------------------------------------------
!
    IF(p_sand .gt. 0.0d0) THEN
#  ifdef SEDIMENT_EMPIRICAL
!!!  Empirical sediment model
      CALL sedecosys     &
!       input parameters
        ( ng, i, j       &   ! ng: nested grid number; i,j: position
        , PFDbott        &   ! Photon flux density (umol m-2 s-1)
        , rho_sw(1)      &   ! Density of seawater (g cm-3)
        , DIC(:,1)       &   ! DIC (umol kg-1)
        , TA (1)         &   ! TA (umol kg-1)
        , DOx(1)         &   ! DO (umol L-1)
        , NH4(1)         &   ! NH4 concentration (umol L-1)
!       output parameters
        , Flux_DIC(:)       &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_TA        &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_DO        &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_NO3(:)       &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_NH4(:)       &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        , Flux_PO4(:)       &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
        )
     
      ! 1 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
      ! cff: convaert [mmol cm-2 s-1] to [umol L-1 s-1]

      cff=1.0d0 /dz(1) * p_sand

      dDIC_dt(:,1) = dDIC_dt(:,1) - Flux_DIC(:) * cff/rho_sw(1)
      dTA_dt (1) = dTA_dt (1) - Flux_TA  * cff/rho_sw(1)
      dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
      dNO3_dt(:,1) = dNO3_dt(:,1) - Flux_NO3(:) * cff
      dNH4_dt(:,1) = dNH4_dt(:,1) - Flux_NH4(:) * cff
      dPO4_dt(:,1) = dPO4_dt(:,1) - Flux_PO4(:) * cff

#  else


!!!  Sediment ecosystem model

! yt_edit to do put output data in his2.nc file
! yt_edit don't run under reef rock (confirm coverage type interactions) (don't forget to fix seagrass -> sediment coupling when you implement this: umol/L volume shouuld be; gridarea * sed_dz -> p_sediment * sed_dz)
! yt_edit porosity from coverage??

      Mn2 = 0.0d0  
      Fe2 = 0.0d0   
#   ifndef BLUE_TIDE
      H2S(:,:) = 0.0d0
#   endif
      SO4(:) = 0.0d0 ! (umol L-1)
      SO4(iSt) = 0.02824d0*Sal(1)/35.0d0*rho_sw(1) *1.0d6 ! (umol L-1)
#   if defined SULFUR_ISOTOPE
      SO4(iS34) = Ci_from_Ct_delta(SO4(iSt), 21.0d0, R34S_VCDT)
#   endif

      Fdep_MnO2  = 10d0  / 86400d0 !!!! Check needed
      Fdep_FeOOH = 100d0 / 86400d0 !!!! Check needed
      Fdep_FeOOH_PO4(:) = 0.0d0 !!!! Check needed
      Fdep_FeS(:) = 0.0d0 !!!! Check needed
      Fdep_FeS2(:)= 0.0d0 !!!! Check needed
      Fdep_S0(:)  = 0.0d0 !!!! Check needed
#   if defined SEDECO_BURIAL
      Fdep_MnO2  = Fdep_sed * MnO2_sed !!!! Check needed
      Fdep_FeOOH = Fdep_sed * FeOOH_sed !!!! Check needed
      Fdep_FeOOH_PO4(iPt) = Fdep_sed * 0.0d0 !!!! Check needed
      Fdep_FeS (iSt) = Fdep_sed * 0.0d0 !!!! Check needed
      Fdep_FeS2(iSt) = Fdep_sed * FeS2_sed !!!! Check needed
      Fdep_S0  (iSt) = Fdep_sed * 0.0d0 !!!! Check needed
#    if defined SULFUR_ISOTOPE
      Fdep_FeS (iS34) = Ci_from_Ct_delta(Fdep_FeS (iSt), 0.0d0, R34S_VCDT) !!!! Check needed
      Fdep_FeS2(iS34) = Ci_from_Ct_delta(Fdep_FeS2(iSt), 0.0d0, R34S_VCDT) !!!! Check needed
      Fdep_S0  (iS34) = Ci_from_Ct_delta(Fdep_S0  (iSt), 0.0d0, R34S_VCDT) !!!! Check needed
#    endif
#   endif

      DO m=1,isplitsed   !!! Loop for sediment model: dtc <= 0.05 sec

        datei = date + dtsed*(m-1)/86400.0d0
        CALL sedecosys       &
!         input parameters
          ( ng, i, j         &   ! ng: nested grid number; i,j: position
          , datei            &   ! Date (days since x or elapsed days)
          , dtsed            &   ! Time step (sec)
          , Tmp(1)           &   ! Tmp: Temperature (oC)
          , Sal(1)           &   ! Sal: Salinity (PSU)
          , DOx(1)           &   ! DOx: Dissolved oxygen (umol L-1)
          , TA(1)            &   ! TA: Total alkalinity (umol kg-1)
          , DIC(:,1)         &   ! DIC: Dissolved inorganic carbon (umol kg-1)
          , NO3(:,1)         &   ! NO3: NO3 (umol L-1)
          , NH4(:,1)         &   ! NH4: NH4 (umol L-1)
          , PO4(:,1)         &   ! PO4: PO4 (umol L-1)
          , DOC(:,:,1)       &   ! DOC: Dissolved organic carbon (µmol L-1)
          , POC(:,:,1)       &   ! POC: Particulate organic carbon (DOC: umol L-1)
          , DON(:,:,1)       &   ! DOC: Dissolved organic nitrogen (µmol L-1)
          , PON(:,:,1)       &   ! PON: Particulate organic nitrogen (PON: umol L-1)
          , DOP(:,:,1)       &   ! DOC: Dissolved organic phosporius (µmol L-1)
          , POP(:,:,1)       &   ! POP: Particulate organic phosporius (POP: umol L-1)
          , Mn2              &  ! Mn2: Manganese ion    (µmol L-1)
          , Fe2              &  ! Fe2: iron(II)         (µmol L-1)
          , H2S(:,1)         &  ! H2S: hydrogen sulfide (µmol L-1)
          , SO4(:)           &  ! SO4: sulfate          (µmol L-1)
#   if defined SEDECO_BURIAL
          , Fdep_sed         &  ! Sedimentation rate (g cm-2 s-1) (Positive: sedimentation; Negative: erosion)
#   endif
          , -Fdep_POC(:,:)*100.0d0  &   ! POC deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive ! yt_edit POC deposition flux   ! double check advection scheme
          , -Fdep_PON(:,:)*100.0d0  &   ! PON deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , -Fdep_POP(:,:)*100.0d0  &   ! POP deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , -Fdep_PIC(:,iDead)*100.0d0  &   ! PIC deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , Fdep_MnO2         &  ! MnO2 deposition flux (nmol cm-2 s-1)
          , Fdep_FeOOH        &  ! FeOOH depositoin flux (nmol cm-2 s-1)
          , Fdep_FeOOH_PO4(:) &  ! FeOOH_PO4 depositoin flux (nmol cm-2 s-1)
          , Fdep_FeS(:)       &  ! FeS deposition flux (nmol cm-2 s-1)
          , Fdep_FeS2(:)      &  ! FeS2 depositoin flux (nmol cm-2 s-1)
          , Fdep_S0(:)        &  ! S0 depositoin flux (nmol cm-2 s-1)
#   if defined SEDECO_SGD && defined SGD_ON
          , sgd_flux/p_sand   &   ! SGD rate (cm s-1) * SGD only occures sand area. 
          , sgd_Tmp           &   ! SGD concentration coming in through bottom: Tmp: Temperature (oC)
          , sgd_Sal           &   ! SGD concentration coming in through bottom: Sal: Salinity (PSU)
          , sgd_DOx           &   ! SGD concentration coming in through bottom: DOx: Dissolved oxygen (umol L-1)
          , sgd_TA            &   ! SGD concentration coming in through bottom: TA : Total alkalinity (TA: umol kg-1)
          , sgd_DIC(:)        &   ! SGD concentration coming in through bottom: DIC: Total dissolved inorganic carbon (DIC: umol kg-1)
          , sgd_NO3(:)        &   ! SGD concentration coming in through bottom: NO3: NO3 (umol L-1)
          , sgd_NH4(:)        &   ! SGD concentration coming in through bottom: NH4: NH4 (umol L-1)
          , sgd_PO4(:)        &   ! SGD concentration coming in through bottom: PO4: PO4 (umol L-1)
          , sgd_DOC(:,:)      &   ! SGD concentration coming in through bottom: DOC: Total dissolved organic carbon (DOC: umol L-1)
          , sgd_DON(:,:)      &   ! SGD concentration coming in through bottom: DOC: Total dissolved organic nitrogen (DON: umol L-1)
          , sgd_DOP(:,:)      &   ! SGD concentration coming in through bottom: DOC: Total dissolved organic phosphrous (DOP: umol L-1)
#   endif
!         output parameters
          , Flux_Tmp          &   ! Temperature flux (oC cm s-1)  * direction of water column to sediment is positive
          , Flux_Sal          &   ! Salinity flux (PSU cm s-1)  * direction of water column to sediment is positive
          , Flux_DO           &   ! DO  flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , Flux_TA           &   ! Total Alkalinity flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , Flux_DIC(:)       &   ! DIC flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , Flux_NO3(:)       &   ! NO3 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , Flux_NH4(:)       &   ! NH4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , Flux_PO4(:)       &   ! PO4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
          , Flux_DOC(:,:)     &   ! DOC(f,s) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
          , Flux_POC(:,:)     &   ! POC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
          , Flux_DON(:,:)     &   ! DON(f,s) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
          , Flux_PON(:,:)     &   ! PON flux (nmol cm-2 s-1) * direction of water column to sediment is positive
          , Flux_DOP(:,:)     &   ! DOP(f,s) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
          , Flux_POP(:,:)     &   ! POP flux (nmol cm-2 s-1) * direction of water column to sediment is positive
          , Flux_PIC(:,iDead) &   ! PIC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
#   if defined BLUE_TIDE         
!          , Flux_Mn2       &   ! Mn2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
!          , Flux_MnO2      &   ! MnO2 flux  (nmol cm-2 s-1) * direction of water column to sediment is positive
!          , Flux_Fe2       &   ! Fe2 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
!          , Flux_FeS       &   ! FeS flux    (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
!          , Flux_FeS2      &   ! FeS2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
!          , Flux_FeOOH     &   ! FeOOH flux  (nmol cm-2 s-1) * direction of water column to sediment is positive
!          , Flux_FeOOH_PO4 &   ! FeOOH=PO43 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
          , Flux_H2S(:)       &   ! H2S flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
!          , Flux_S0        &   ! S0 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
!          , Flux_SO4       &   ! SO4 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
#   endif
        )

      END DO
      ! 1 oC cm s-1 = 0.01 oC m s-1, or  1 PSU cm s-1 = 0.01 PSU m s-1
      ! cff: convert [oC cm s-1] to [oC s-1], or convert [PSU cm s-1] to [PSU s-1]

      ! 1 nmol cm-2 s-1 = 0.01 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
      ! cff: convert [nmol cm-2 s-1] to [umol L-1 s-1]

      cff=0.01d0 /dz(1) * p_sand
      dTemp_dt(1) = dTemp_dt(1) - Flux_Tmp  * cff
      dSalt_dt(1) = dSalt_dt(1) - Flux_Sal  * cff
      dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
      dTA_dt (1) = dTA_dt (1) - Flux_TA  * cff/rho_sw(1)
      dDIC_dt(:,1) = dDIC_dt(:,1) - Flux_DIC(:) * cff/rho_sw(1)
      dNO3_dt(:,1) = dNO3_dt(:,1) - Flux_NO3(:) * cff
      dNH4_dt(:,1) = dNH4_dt(:,1) - Flux_NH4(:) * cff
      dPO4_dt(:,1) = dPO4_dt(:,1) - Flux_PO4(:) * cff
      dDOC_dt(:,:,1) = dDOC_dt(:,:,1) - Flux_DOC(:,:) * cff
      dPOC_dt(:,:,1) = dPOC_dt(:,:,1) - Flux_POC(:,:) * cff
      dDON_dt(:,:,1) = dDON_dt(:,:,1) - Flux_DON(:,:) * cff
      dPON_dt(:,:,1) = dPON_dt(:,:,1) - Flux_PON(:,:) * cff
      dDOP_dt(:,:,1) = dDOP_dt(:,:,1) - Flux_DOP(:,:) * cff
      dPOP_dt(:,:,1) = dPOP_dt(:,:,1) - Flux_POP(:,:) * cff
      dPIC_dt(:,iDead,1) = dPIC_dt(:,iDead,1) - Flux_PIC(:,iDead) * cff
#   if defined BLUE_TIDE         
!      Flux_Mn2         ! Mn2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_MnO2        ! MnO2 flux  (nmol cm-2 s-1) * direction of water column to sediment is positive

!      Flux_Fe2         ! Fe2 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_FeS         ! FeS flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_FeS2        ! FeS2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_FeOOH       ! FeOOH flux  (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_FeOOH_PO4   ! FeOOH=PO43 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive

      dH2S_dt(:,1) = dH2S_dt(:,1) - Flux_H2S(:) * cff         ! H2S flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_S0          ! S0 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
!      Flux_SO4         ! SO4 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
#   endif

#  endif

    END IF 
# endif

# if defined ECOSYS_TESTMODE
!------------------------------------------------------------------------
! Print section
    
    IF( date*86400.0d0 >= dsec ) THEN
      if(dsec == 0.0d0) then
        dsec=date*86400.0d0+ECOSYS_OUTPUT_INTERVAL*60.0d0
      else
        dsec=dsec+ECOSYS_OUTPUT_INTERVAL*60.0d0
      endif
      
#  ifdef CORAL_POLYP 
      cff1=0.01d0*p_coral(1)*P2R(1)*3600.0d0  ! cff1: convert [nmol cm-2 s-1] to [mmol m-2 h-1]
      cff2=0.01d0*p_coral(2)*P2R(2)*3600.0d0  ! cff1: convert [nmol cm-2 s-1] to [mmol m-2 h-1]
#  endif
#  ifdef SEAGRASS
      cff3 = p_sgrass(1)*3600.0d0        ! cff2: convert [mmol m-2 s-1] to [mmol m-2 h-1]
#  endif
#  ifdef MACROALGAE
      cff4 = p_algae(1) *3600.0d0        ! cff3: convert [mmol m-2 s-1] to [mmol m-2 h-1]
#  endif
#  if defined SEDIMENT_ECOSYS
#   if defined  SEDIMENT_EMPIRIXCAL
      cff5 = p_sand  *3600.0d0        ! cff4: convert [mmol m-2 s-1] to [mmol m-2 h-1]
#   else
      cff5 = 0.01d0*p_sand*3600.0d0   ! cff4: convert [nmol cm-2 s-1] to [mmol m-2 h-1]
#   endif
#  endif

      write(40,'(*(g0:,", "))') date, PFDbott                     &
#  ifdef CORAL_POLYP 
        , CORAL(1)%Pg(1,1,1)*cff1, CORAL(1)%R (1,1,1)*cff1        &
        , (CORAL(1)%Pg(1,1,1)-CORAL(1)%R (1,1,1))*cff1            &
        , CORAL(1)%G (1,1,1)*cff1                                 &
        , CORAL(1)%Pg(2,1,1)*cff2, CORAL(1)%R (2,1,1)*cff2        &
        , (CORAL(1)%Pg(2,1,1)-CORAL(1)%R (2,1,1))*cff2            &
        , CORAL(1)%G (2,1,1)*cff2                                 &
#  endif
#  ifdef SEAGRASS
!        , SGRASS(1)%Pg(1,1,1)*cff3, SGRASS(1)%R (1,1,1)*cff3      &
!        , (SGRASS(1)%Pg(1,1,1)-SGRASS(1)%R (1,1,1))*cff3          &
#  endif
#  ifdef MACROALGAE
        , ALGAE(1)%Pg(1,1,1)*cff4, ALGAE(1)%R (1,1,1)*cff4        &
        , (ALGAE(1)%Pg(1,1,1)-ALGAE(1)%R (1,1,1))*cff4            &
#  endif
#  ifdef SEDIMENT_ECOSYS
!        , SEDECO(1)%Pg(1,1)*cff5, SEDECO(1)%R (1,1)*cff5          &
!        , (SEDECO(1)%Pg(1,1)-SEDECO(1)%R (1,1))*cff5              &
!        , SEDECO(1)%G (1,1)*cff5                                  &
#  endif
        , dDIC_dt(iCt,1),dTA_dt(1), dDOx_dt(1)                    &
        , dDOC_dt(iCt,1,1), dPOC_dt(iCt,1,1)                      &
        , dNO3_dt(iNt,1), dNH4_dt(iNt,1)                          &
        , dPO4_dt(iPt,1)                                          &
        , dDON_dt(iNt,1,1), dPON_dt(iNt,1,1)                      &
        , dDOP_dt(iPt,1,1), dPOP_dt(iPt,1,1)                      &
        , pH(N), ssfCO2, Warg(N), Wcal(N), U10, ssCO2flux, ssO2flux
        
    END IF
!-----------------------------------------------------------------------
# endif

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

    real(8) :: dz_r   
    real(8) :: F(0:N),v, vmin
    integer :: k, ks
    integer :: kup
    real(8) :: dtrc_s, trct, tnewt, Courant

!
!   Upstream advective scheme
!
 ! set velocity (v) to keep Courant number less than 0.4
    v = abs(w)
    DO k=1,N-1
      dz_r = z_r(k+1) - z_r(k)
      v = min( dz_r/dt*0.4d0, v )
    END DO
    v = sign( v, w )

    if(w>0) then
      kup = 0
      Fb_trc = 0.0d0
    else
      kup = 1
      Fb_trc = v * trc(1)
    endif
    
    F(0) = 0.0d0
    DO k=1,N-1
      F(k) = v * trc(K+kup)
    END DO
    F(N) = 0.0d0

    DO k=1,N
      dtrc_dt(k) = (F(k-1)-F(k))/dz(k)
    END DO

    RETURN
  END SUBROUTINE bio_vadv2
#endif
END MODULE mod_reef_ecosys

