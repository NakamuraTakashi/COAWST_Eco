
!!!=== ver 2017/03/10   Copyright (c) 2012-2017 Takashi NAKAMURA  =====

#include "cppdefs.h"


!!!**** MODULE OF ECOSYSTEM MODEL ************************************

  MODULE mod_reef_ecosys

  contains

!!! **********************************************************************
!!!  Set initial conditions for reef ecosystem model
!!! **********************************************************************

    SUBROUTINE initialize_reef_ecosys(ng, Ngrids, LBi, UBi, LBj, UBj)
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
      CALL initialize_sedecosys(ng, Ngrids, LBi, UBi, LBj, UBj)
#endif
      
      RETURN
    END SUBROUTINE initialize_reef_ecosys


!!! **********************************************************************
!!!  Main program of reef ecosystem model
!!! **********************************************************************

    SUBROUTINE reef_ecosys        &
!          input parameters
     &            (ng, i, j       &   ! ng: nested grid number; i,j: position
     &            ,N              &   ! Number of vertical grid (following ROMS vertical grid)
     &            ,isplitc        &   ! Internal loop counts of coral polyp model
     &            ,isplitsed      &   ! Internal loop counts of sediment ecosystem model
     &            ,dt             &   ! Time step (sec)
     &            ,dz             &   ! dz(N): vertical grid size (m)
     &            ,PFDsurf        &   ! Sea surface photon flux density (umol m-2 s-1)
     &            ,tau            &   ! bottom shear stress (N m-2)
     &            ,pCO2air        &   ! Air CO2 pertial pressure (uatm)
     &            ,U10            &   ! wind speed (m s-1)
#ifdef CORAL_POLYP
     &            ,p_coral        &   ! Coral coverage (0-1)
#endif
#ifdef SEAGRASS
     &            ,p_sgrass       &   ! seagrass coverage (0-1)
#endif
#ifdef MACROALGAE
     &            ,p_algae        &   ! algal coverage (0-1)
#endif
#ifdef SEDIMENT_ECOSYS
     &            ,p_sand          &   ! sediment coverage (0-1)
#endif

     &            ,Tmp            &   ! Tmp(N): Temperature (oC)
     &            ,Sal            &   ! Sal(N): Salinity (PSU)
     &            ,DIC            &   ! DIC(N): Total dissolved inorganic carbon (DIC: umol kg-1)
     &            ,TA             &   ! TA (N): Total alkalinity (TA: umol kg-1)
     &            ,DOx            &   ! DOx(N): Dissolved oxygen (umol L-1)
#if defined ORGANIC_MATTER
     &            ,DOC            &   ! DOC(N): Dissolved organic carbon (DOC: umol L-1)
     &            ,POC            &   ! POC(N): Particulate organic carbon (DOC: umol L-1)
     &            ,PHY1            &   ! PHY(N): phytoplankton (umol C L-1)
     &            ,PHY2            &   ! PHY(N): phytoplankton (umol C L-1)
     &            ,ZOO            &   ! ZOO(N): zooplankton (umol C L-1)
#endif
#if defined CARBON_ISOTOPE
     &            ,DI13C          &   ! DI13C(N): 13C of DIC (umol kg-1)
#endif
#if defined NUTRIENTS            
     &            ,NO3            &   ! NO3(N): NO3 (umol L-1)
     &            ,NO2            &   ! NO2(N): NO2 (umol L-1)
     &            ,NH4            &   ! NH4(N): NH4 (umol L-1)
     &            ,PO4            &   ! PO4(N): PO4 (umol L-1)
# if defined ORGANIC_MATTER
     &            ,DON            &   ! DON(N): Dissolved organic nitrogen (DON: umol L-1)
     &            ,PON            &   ! PON(N): Particulate organic nitrogen (PON: umol L-1)
     &            ,DOP            &   ! DOP(N): Dissolved organic phosporius (DOP: umol L-1)
     &            ,POP            &   ! POP(N): Particulate organic phosporius (POP: umol L-1)
# endif
#endif
#if defined COT_STARFISH         
     &            ,COTe           &   ! COTe(N): COT starfish egg (umol L-1)
     &            ,COTl           &   ! COTl(N): COT starfish larvae (umol L-1)
#endif
!          output parameters
     &            ,dDIC_dt        &   ! dDIC_dt(N): dDIC/dt (umol kg-1 s-1)
     &            ,dTA_dt         &   ! dTA_dt (N): dTA/dt (umol kg-1 s-1)
     &            ,dDOx_dt        &   ! dDOx_dt(N): dDO/dt (umol L-1 s-1)
#if defined ORGANIC_MATTER
     &            ,dDOC_dt        &   ! dDOC_dt(N): dDOC/dt (umol L-1 s-1)
     &            ,dPOC_dt        &   ! dPOC_dt(N): dPOC/dt (umol L-1 s-1)
     &            ,dPHY1_dt        &   ! dPHY_dt(N): dPHY/dt (umol C L-1 s-1)
     &            ,dPHY2_dt        &   ! dPHY_dt(N): dPHY/dt (umol C L-1 s-1)
     &            ,dZOO_dt        &   ! dZOO_dt(N): dZOO/dt (umol C L-1 s-1)
#endif
#if defined CARBON_ISOTOPE
     &            ,dDI13C_dt      &   ! dDI13C_dt(N): dDI13C/dt (umol kg-1 s-1)
#endif
#if defined NUTRIENTS          
     &            ,dNO3_dt        &   ! dNO3_dt(N): dNO3/dt (umol L-1 s-1)
     &            ,dNO2_dt        &   ! dNO2_dt(N): dNO2/dt (umol L-1 s-1)
     &            ,dNH4_dt        &   ! dNH4_dt(N): dNH4/dt (umol L-1 s-1)
     &            ,dPO4_dt        &   ! dPO4_dt(N): dPO4/dt (umol L-1 s-1)
# if defined ORGANIC_MATTER
     &            ,dDON_dt        &   ! dDON_dt(N): dDON/dt (umol L-1 s-1)
     &            ,dPON_dt        &   ! dPON_dt(N): dPON/dt (umol L-1 s-1)
     &            ,dDOP_dt        &   ! dDOP_dt(N): dDOP/dt (umol L-1 s-1)
     &            ,dPOP_dt        &   ! dPOP_dt(N): dPOP/dt (umol L-1 s-1)
# endif
#endif
#if defined COT_STARFISH         
     &            ,dCOTe_dt       &   ! dCOTe/dt(N): (umol L-1 s-1)
     &            ,dCOTl_dt       &   ! dCOTl/dt(N): (umol L-1 s-1)
#endif
     &            ,sspH           &   ! sea surface pH
     &            ,ssfCO2         &   ! sea surface fCO2 (uatm)
     &            ,ssWarg         &   ! sea surface aragonite saturation state
     &            ,ssCO2flux      &   ! sea surface CO2 flux (mmol m-2 s-1)
     &            ,ssO2flux       &   ! sea surface O2 flux (mmol m-2 s-1)
     &            ,PFDbott        &   ! Bottom photon flux density (umol m-2 s-1)
     &             )                    
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
!               :  :           :  |_2                                   
!               :  :           : /|                                     
!               :  |___________|/ |                                     
!                  |           |  |_1                                   
!     dz(2) {   2  |           | /|                                     
!                  |___________|/ |                                     
!                  |           |  |_0    Bottom                         
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

      real(8), intent(in) :: Tmp(N)        
      real(8), intent(in) :: Sal(N)        
      real(8), intent(in) :: DIC(N)        
      real(8), intent(in) :: TA (N)        
      real(8), intent(in) :: DOx(N)        
#if defined ORGANIC_MATTER
      real(8), intent(in) :: DOC(N)        
      real(8), intent(in) :: POC(N)        
      real(8), intent(in) :: PHY1(N)        
      real(8), intent(in) :: PHY2(N)        
      real(8), intent(in) :: ZOO(N)        
#endif
#if defined CARBON_ISOTOPE
      real(8), intent(in) :: DI13C(N)      
#endif
#if defined NUTRIENTS            
      real(8), intent(in) :: NO3(N)        
      real(8), intent(in) :: NO2(N)        
      real(8), intent(in) :: NH4(N)        
      real(8), intent(in) :: PO4(N)        
#if defined ORGANIC_MATTER
      real(8), intent(in) :: DON(N)        
      real(8), intent(in) :: PON(N)        
      real(8), intent(in) :: DOP(N)        
      real(8), intent(in) :: POP(N)        
# endif
#endif
#if defined COT_STARFISH         
      real(8), intent(in) :: COTe(N)         ! COT starfish egg (umol L-1)
      real(8), intent(in) :: COTl(N)         ! COT starfish larvae (umol L-1)
#endif
! output parameters
      real(8), intent(out) :: dDIC_dt(N)     
      real(8), intent(out) :: dTA_dt(N)     
      real(8), intent(out) :: dDOx_dt(N)     
#if defined ORGANIC_MATTER
      real(8), intent(out) :: dDOC_dt(N)     
      real(8), intent(out) :: dPOC_dt(N)     
      real(8), intent(out) :: dPHY1_dt(N)     
      real(8), intent(out) :: dPHY2_dt(N)     
      real(8), intent(out) :: dZOO_dt(N)     
#endif
#if defined CARBON_ISOTOPE
      real(8), intent(out) :: dDI13C_dt(N)   
#endif
#if defined NUTRIENTS           
      real(8), intent(out) :: dNO3_dt(N)     
      real(8), intent(out) :: dNO2_dt(N)     
      real(8), intent(out) :: dNH4_dt(N)     
      real(8), intent(out) :: dPO4_dt(N)     
# if defined ORGANIC_MATTER
      real(8), intent(out) :: dDON_dt(N)     
      real(8), intent(out) :: dPON_dt(N)     
      real(8), intent(out) :: dDOP_dt(N)     
      real(8), intent(out) :: dPOP_dt(N)     
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
!      real(8), parameter :: P2R(Ncl) = (/ 5.0d0, 3.0d0 /)  !!! Conversion factor from polyp scale to reef scale
                                      !*~9.0d0 convert projected area to coral surface area for branching Acropora (Naumann et al. 2009)
#endif
!---- POM deposition flux parameters
      real(8), parameter :: rho_POM = 1.1d0     ! POM density [g/cm3] !!!‚Ä‚«‚Æ‚¤
      real(8), parameter :: D_POM   = 0.1d0     ! POM diameter [mm]   !!!‚Ä‚«‚Æ‚¤
      real(8), parameter :: grav    = 9.80665d0 ! Gravitational acceleration [m/s2]
      real(8), parameter :: viscW   = 8.6d-7    ! Seawater kinematic molecular viscosity (@ 29 oC) [m2/s]
      
      real(8) :: dtc     ! Internal time step for coral polyp model (sec)
      real(8) :: dtsed   ! Internal time step for sediment ecosystem model (sec)

!      real(8) :: sed_Pg  
!      real(8) :: sed_R   
!      real(8) :: sed_Gn  

      integer :: k, m          
      integer :: icl
      real(8) :: TmpK
      real(8) :: rho_sw
      real(8) :: DOsatu    
      real(8) :: cCO2aq, cHCO3, cCO3, R13C
      real(8) :: PFD, Att, AttFac, ExpAtt, Itop
      real(8) :: cff, cff1, cff2, cff3, cff4, cff5

      real(8) :: Flux_Tmp, Flux_Sal
      real(8) :: Flux_DIC, Flux_TA,  Flux_DO
#if defined ORGANIC_MATTER
      real(8) :: Flux_DOC, Flux_POC
      real(8) :: Flux_PHY1, Flux_PHY2, Flux_ZOO  
#endif
#if defined CARBON_ISOTOPE
      real(8) :: Flux_DI13C
#endif
#if defined NUTRIENTS           
      real(8) :: Flux_NO3, Flux_NO2, Flux_NH4, Flux_PO4  
# if defined ORGANIC_MATTER
      real(8) :: Flux_DON, Flux_PON    
      real(8) :: Flux_DOP, Flux_POP    
# endif
#endif

      real(8) :: dDIC, dTA,  dDOx
#if defined ORGANIC_MATTER
      real(8) :: dDOC, dPOC
      real(8) :: dPHY1, dPHY2, dZOO  
#endif
#if defined CARBON_ISOTOPE
      real(8) :: dDI13C
#endif
#if defined NUTRIENTS           
      real(8) :: dNO3, dNO2, dNH4, dPO4
# if defined ORGANIC_MATTER
      real(8) :: dDON, dPON
      real(8) :: dDOP, dPOP
# endif
#endif
#if defined COT_STARFISH         
      real(8) :: dCOTe, dCOTl
#endif

#if defined ORGANIC_MATTER
      real(8) :: w_POM
      real(8) :: Fw_POC(0:N)
      real(8) :: Fw_PHY1(0:N), Fw_PHY2(0:N), Fw_ZOO(0:N)
      real(8) :: Fdep_POC
      real(8) :: Fdep_PHY1, Fdep_PHY2, Fdep_ZOO
# if defined NUTRIENTS
      real(8) :: Fw_PON(0:N)
      real(8) :: Fw_POP(0:N)
      real(8) :: Fdep_PON
      real(8) :: Fdep_POP
# endif
#endif
#if defined COT_STARFISH
      real(8) :: Fw_COTe(0:N), Fw_COTl(0:N)
      real(8) :: Fdep_COTe, Fdep_COTl
#endif
#if defined ECOSYS_TESTMODE
!  Output
      real(8), parameter :: OUTPUT_INTERVAL = 5.0d0     ! Output interval (min)
      real(8), save :: time = 0.0d0 !sec
      real(8), save :: dsec = 0.0d0 !sec
#endif

!-----------------------------------------------------------------------
! Initialize all time difference values (d_XXX)
!-----------------------------------------------------------------------
      
      dtc = dt/isplitc                  ! Internal time step for coral polyp model (sec)
      dtsed = dt/isplitsed              ! Internal time step for sediment ecosystem model (sec)
      
      DO k=1,N
        dDIC_dt(k) = 0.0d0
        dTA_dt(k)  = 0.0d0
        dDOx_dt(k) = 0.0d0
#if defined ORGANIC_MATTER
        dDOC_dt(k) = 0.0d0
        dPOC_dt(k) = 0.0d0
        dPHY1_dt(k) = 0.0d0
        dPHY2_dt(k) = 0.0d0
        dZOO_dt(k) = 0.0d0
#endif
#if defined CARBON_ISOTOPE
        dDI13C_dt(k) = 0.0d0
#endif
#if defined NUTRIENTS            
        dNO3_dt(k) = 0.0d0
        dNO2_dt(k) = 0.0d0
        dNH4_dt(k) = 0.0d0
        dPO4_dt(k) = 0.0d0
# if defined ORGANIC_MATTER
        dDON_dt(k) = 0.0d0
        dPON_dt(k) = 0.0d0
        dDOP_dt(k) = 0.0d0
        dPOP_dt(k) = 0.0d0
# endif
#endif
      END DO
      sspH      = 8.0d0   ! sea surface pH
      ssfCO2    = 380.0d0   ! sea surface fCO2 (uatm)
      ssWarg    = 4.0d0   ! sea surface aragonite saturation state
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

      sspH = pH_fromATCT( TA(N), DIC(N),TmpK, Sal(N) )
      cCO2aq = cCO2aq_fromCTpH( DIC(N), sspH, TmpK, Sal(N) )
!      cHCO3 = cHCO3_fromCTpH( DIC(N), sspH, TmpK, Sal(N) )
      cCO3 = cCO3_fromCTpH( DIC(N), sspH, TmpK, Sal(N) )
      ssfCO2 = fCO2_fromcCO2aq( cCO2aq, TmpK, Sal(N) )  !! for output
      ssWarg = Warg_fromcCO3( cCO3, TmpK, Sal(N) )

!----------- CO2 gas exchange rate (mmol m-2 s-1). -------------------
      ssCO2flux = Flux_CO2(ssfCO2, pCO2air, U10, TmpK, Sal(N) )  ! sea to air is positive 

#if defined AIR_SEA_GAS_EXCHANGE
      dDOx_dt(N) = dDOx_dt(N) - ssO2flux  /dz(N)  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      dDIC_dt(N) = dDIC_dt(N) - ssCO2flux  /dz(N)   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# if defined CARBON_ISOTOPE
      R13C=R13C_fromd13C(-10.d0)
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
      
        rho_sw = densSW( Tmp(k),Sal(k) )  ! Density of seawater [g/cm3]

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

        CALL foodweb              &
!          input parameters
     &            (ng, 1, i, j    &   ! ng: nested grid number; n: coral compartment; i,j: position
     &            ,dt             &   ! Time step (sec)
     &            ,PFD            &   ! Photon flux density (umol m-2 s-1)
     &            ,rho_sw         &   ! Density of seawater (g cm-3)
     &            ,Tmp(k)         &   ! Temperature (oC)
     &            ,Sal(k)         &   ! Salinity (PSU)
     &            ,DIC(k)         &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
     &            ,TA (k)         &   ! Total alkalinity (TA: umol kg-1)
     &            ,DOx(k)         &   ! Dissolved oxygen (umol L-1)
     &            ,DOC(k)         &   ! Dissolved organic carbon (DOC: umol L-1)
     &            ,POC(k)         &   ! Particulate organic carbon (DOC: umol L-1)
     &            ,PHY1(k)         &   ! phytoplankton (umol C L-1)
     &            ,PHY2(k)         &   ! phytoplankton (umol C L-1)
     &            ,ZOO(k)         &   ! zooplankton (umol C L-1)
# if defined CARBON_ISOTOPE
     &            ,DI13C(k)       &   !13C of DIC (umol kg-1)
# endif
# if defined NUTRIENTS         
     &            ,NO3(k)         &   ! NO3 (umol L-1)
     &            ,NO2(k)         &   ! NO2 (umol L-1)
     &            ,NH4(k)         &   ! NH4 (umol L-1)
     &            ,PO4(k)         &   ! PO4 (umol L-1)
     &            ,DON(k)         &   ! Dissolved organic nitrogen (DON: umol L-1)
     &            ,PON(k)         &   ! Particulate organic nitrogen (PON: umol L-1)
     &            ,DOP(k)         &   ! Dissolved organic phosporius (DOP: umol L-1)
     &            ,POP(k)         &   ! Particulate organic phosporius (POP: umol L-1)
# endif
#if defined COT_STARFISH
     &            ,COTe(k)        &   ! COT starfish egg (umol L-1)
     &            ,COTl(k)        &   ! COT starfish larvae (umol L-1)
#endif
!          output parameters
     &            ,dDIC           &   ! dDIC/dt  (umol kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
     &            ,dTA            &   ! dTA/dt   (umol kg-1 s-1) 
     &            ,dDOx           &  ! dDOx/dt  (umol L-1 s-1) 
     &            ,dDOC           &   ! dDOC/dt  (umol L-1 s-1) 
     &            ,dPOC           &   ! dPOC/dt  (umol L-1 s-1) 
     &            ,dPHY1           &   ! dPHY/dt  (umol L-1 s-1)  
     &            ,dPHY2           &   ! dPHY/dt  (umol L-1 s-1)  
     &            ,dZOO           &   ! dZOO/dt  (umol L-1 s-1)  
# if defined CARBON_ISOTOPE
     &            ,dDI13C         &   ! dDI13C/dt (umol kg-1 s-1)
# endif
# if defined NUTRIENTS
     &            ,dNO3           &   ! dNO3/dt (umol L-1 s-1)
     &            ,dNO2           &   ! dNO2/dt (umol L-1 s-1)
     &            ,dNH4           &   ! dNH4/dt (umol L-1 s-1)
     &            ,dPO4           &   ! dPO4/dt (umol L-1 s-1)
     &            ,dDON           &   ! dDON/dt (umol L-1 s-1)
     &            ,dPON           &   ! dPON/dt (umol L-1 s-1)
     &            ,dDOP           &   ! dDOP/dt (umol L-1 s-1)
     &            ,dPOP           &   ! dPOP/dt (umol L-1 s-1)
# endif                                
#if defined COT_STARFISH
     &            ,dCOTe          &   ! dCOTe/dt (umol L-1 s-1)
     &            ,dCOTl          &   ! dCOTl/dt (umol L-1 s-1)
#endif
     &             )

!------------ Vertical fluxes calculation ------------------------------
!                                                                       
!                      Fw_XXX(k)                                        
!                   :_____A______ :_k                                   
!                   /     |      /|                                     
!      z     Layer /___________ / |                                     
!                  |      |    |  |    Water column                     
!                  |  |        |  |                                     
!     dz(k) {   k  |  |__ A __ |_ |_k-1                                 
!                  | /    |    | /:                                     
!                  |___________|/                                       
!                  :      |    :                                        
!                      Fw_XXX(k-1)                                        
!                                                                       
!-----------------------------------------------------------------------

!---- Surfact fluxes = 0 (mmol m-2 s-1) ----
        IF (k==N) THEN
          Fw_POC(N)=0.0d0
          Fw_PHY1(N)=0.0d0
          Fw_PHY2(N)=0.0d0
          Fw_ZOO(N)=0.0d0
# if defined NUTRIENTS
          Fw_PON(N)=0.0d0
          Fw_POP(N)=0.0d0
# endif
# if defined COT_STARFISH
          Fw_COTe(N)=0.0d0
          Fw_COTl(N)=0.0d0
# endif
        END IF
!
!---- Water column flux (mmol m-2 s-1) ----
        w_POM = (rho_POM/rho_sw - 1.0d0)*grav*D_POM*D_POM*1.0d-6/18.0d0/viscW
        
        Fw_POC(k-1)=-w_POM*POC(k)  ! umol L-1 m s-1 = mmol m-2 s-1
        Fw_PHY1(k-1)=0.0d0
        Fw_PHY2(k-1)=0.0d0
        Fw_ZOO(k-1)=0.0d0
# if defined NUTRIENTS
        Fw_PON(k-1)=-w_POM*PON(k)
        Fw_POP(k-1)=-w_POM*POP(k)
# endif
# if defined COT_STARFISH
        Fw_COTe(k-1)=0.0d0
        Fw_COTl(k-1)=0.0d0
# endif
!
!---- Bottom flux (mmol m-2 s-1) ----
        IF (k==1) THEN

          Fw_PHY1(k-1)=0.0d0
          Fw_PHY2(k-1)=0.0d0
          Fw_ZOO(k-1)=0.0d0
# if defined COT_STARFISH
          Fw_COTe(k-1)=0.0d0
          Fw_COTl(k-1)=0.0d0
# endif

          Fdep_POC = -Fw_POC(0)
          Fdep_PHY1 = -Fw_PHY1(0)
          Fdep_PHY2 = -Fw_PHY2(0)
          Fdep_ZOO = -Fw_ZOO(0)
# if defined NUTRIENTS
          Fdep_PON = -Fw_PON(0)
          Fdep_POP = -Fw_POP(0)
# endif
# if defined COT_STARFISH
          Fdep_COTe = -Fw_COTe(0)
          Fdep_COTl = -Fw_COTl(0)
# endif
        END IF

!----- Mass balance equation -----------------------------------------------------------

        dDIC_dt(k) = dDIC_dt(k) + dDIC
        dTA_dt (k) = dTA_dt (k) + dTA
        dDOx_dt(k) = dDOx_dt(k) + dDOx
        dDOC_dt(k) = dDOC_dt(k) + dDOC
        dPOC_dt(k) = dPOC_dt(k) + dPOC + (Fw_POC(k-1)-Fw_POC(k))/dz(k)
        dPHY1_dt(k) = dPHY1_dt(k) + dPHY1 + (Fw_PHY1(k-1)-Fw_PHY1(k))/dz(k)
        dPHY2_dt(k) = dPHY2_dt(k) + dPHY2 + (Fw_PHY2(k-1)-Fw_PHY2(k))/dz(k)
        dZOO_dt(k) = dZOO_dt(k) + dZOO + (Fw_ZOO(k-1)-Fw_ZOO(k))/dz(k)
# if defined CARBON_ISOTOPE
        dDI13C_dt(k) = dDI13C_dt(k) + dDI13C
# endif
# if defined NUTRIENTS
        dNO3_dt(k) = dNO3_dt(k) + dNO3
        dNO2_dt(k) = dNO2_dt(k) + dNO2
        dNH4_dt(k) = dNH4_dt(k) + dNH4
        dPO4_dt(k) = dPO4_dt(k) + dPO4
        dDON_dt(k) = dDON_dt(k) + dDON
        dPON_dt(k) = dPON_dt(k) + dPON + (Fw_PON(k-1)-Fw_PON(k))/dz(k)
        dDOP_dt(k) = dDOP_dt(k) + dDOP
        dPOP_dt(k) = dPOP_dt(k) + dPOP + (Fw_POP(k-1)-Fw_POP(k))/dz(k)
# endif
# if defined COT_STARFISH
        dCOTe_dt(k) = dCOTe_dt(k) + dCOTe+ (Fw_COTe(k-1)-Fw_COTe(k))/dz(k)
        dCOTl_dt(k) = dCOTl_dt(k) + dCOTl+ (Fw_COTl(k-1)-Fw_COTl(k))/dz(k)
# endif

#endif                                

!  Light attenuation at the bottom of the grid cell. It is the starting
!  PFD value for the next (deeper) vertical grid cell.
!
        PFD=Itop*ExpAtt

      END DO

        PFDbott=PFD

!!!---------------------------------------------------------------------
!!!  Benthic interactions.
!!!---------------------------------------------------------------------

#if defined CORAL_POLYP
!-----------------------------------------------------------------------
! Compute coral polyp model.
!-----------------------------------------------------------------------
!
      DO icl=1,Ncl

        IF(p_coral(icl) .gt. 0.0d0) THEN          

          DO m=1,isplitc   !!! Loop for coral polyp model: dtc <= 0.05 sec

            CALL coral_polyp         &
!            input parameters
     &              (ng, icl, i, j     &   ! ng: nested grid number; n: coral compartment; i,j: position
     &              ,dtc             &   ! Time step (sec)
     &              ,PFDbott         &   ! Photon flux density (umol m-2 s-1)
     &              ,rho_sw          &   ! Density of seawater (g cm-3)
     &              ,Tmp(1)          &   ! Temperature (oC)
     &              ,Sal(1)          &   ! Salinity (PSU)
     &              ,DIC(1)          &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
     &              ,TA (1)          &   ! Total alkalinity (TA: umol kg-1)
     &              ,DOx(1)          &   ! Dissolved oxygen (umol L-1)
# if defined CORAL_INGESTION
     &              ,PHY(1)          &   ! phytoplankton (umol C L-1)
     &              ,ZOO(1)          &   ! zooplankton (umol C L-1)
# endif
# if defined CORAL_CARBON_ISOTOPE
     &              ,DI13C(1)        &   !13C of DIC (umol kg-1)
# endif
# if defined CORAL_NUTRIENTS
     &              ,NO3(1)          &   ! NO3 (umol L-1)
     &              ,NO2(1)          &   ! NO2 (umol L-1)
     &              ,NH4(1)          &   ! NH4 (umol L-1)
     &              ,PO4(1)          &   ! PO4 (umol L-1)
# endif
     &              ,tau             &   ! bottom shear stress (N m-2)
     &              ,0.0d0           &   ! sedimentation rate (??)
!            output parameters
     &              ,Flux_DIC       &   ! DIC uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &              ,Flux_TA        &   ! TA  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &              ,Flux_DO        &   ! DO  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
# if defined ORGANIC_MATTER
     &              ,Flux_DOC       &   ! DOC uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
     &              ,Flux_POC       &   ! POC uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
# endif
# if defined CORAL_INGESTION
     &              ,Flux_PHY       &   ! Phytoplankton ingestion rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &              ,Flux_ZOO       &   ! Zooplankton ingestion rate (nmol cm-2 s-1)  * direction of water column to coral is positive
# endif
# if defined CORAL_CARBON_ISOTOPE
     &              ,Flux_DI13C     &   ! DI13C uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
# endif
# if defined CORAL_NUTRIENTS
     &              ,Flux_NO3       &   ! NO3 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &              ,Flux_NO2       &   ! NO2 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &              ,Flux_NH4       &   ! NH4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
     &              ,Flux_PO4       &   ! PO4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
#  if defined ORGANIC_MATTER
     &              ,Flux_DON       &   ! DON uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
     &              ,Flux_PON       &   ! PON uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
     &              ,Flux_DOP       &   ! DOP uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
     &              ,Flux_POP       &   ! POP uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
#  endif
# endif
     &                 )

          END DO

        ! 1 nmol cm-2 s-1 = 0.01 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
        ! cff: convert [nmol cm-2 s-1] to [umol L-1 s-1]

          cff=0.01d0 /dz(1) * p_coral(icl)*P2R(icl)
           
          dDIC_dt(1) = dDIC_dt(1) - Flux_DIC * cff/rho_sw
          dTA_dt (1) = dTA_dt (1) - Flux_TA  * cff/rho_sw
          dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
# if defined ORGANIC_MATTER
          dDOC_dt(1) = dDOC_dt(1) - Flux_DOC * cff
          dPOC_dt(1) = dPOC_dt(1) - Flux_POC * cff
# endif
# if defined CORAL_INGESTION
          dPHY_dt(1) = dPHY_dt(1) - Flux_PHY * cff
          dZOO_dt(1) = dZOO_dt(1) - Flux_ZOO * cff
# endif
# if defined CORAL_CARBON_ISOTOPE
          dDI13C_dt(1) = dDI13C_dt(1) - Flux_DI13C * cff/rho_sw
# endif
# if defined CORAL_NUTRIENTS
          dNO3_dt(1) = dNO3_dt(1) - Flux_NO3 * cff
          dNO2_dt(1) = dNO2_dt(1) - Flux_NO2 * cff
          dNH4_dt(1) = dNH4_dt(1) - Flux_NH4 * cff
          dPO4_dt(1) = dPO4_dt(1) - Flux_PO4 * cff
#  if defined ORGANIC_MATTER
          dDON_dt(1) = dDON_dt(1) - Flux_DON * cff
          dPON_dt(1) = dPON_dt(1) - Flux_PON * cff
          dDOP_dt(1) = dDOP_dt(1) - Flux_DOP * cff
          dPOP_dt(1) = dPOP_dt(1) - Flux_POP * cff
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

        CALL seagrass             &
!          input parameters
     &            (ng, 1, i, j     &   ! ng: nested grid number; n: seagrass compartment; i,j: position
     &            ,PFDbott         &   ! Photon flux density (umol m-2 s-1)
     &            ,rho_sw          &   ! Density of seawater (g cm-3)
     &            ,DIC(1)         &   ! DIC (umol kg-1)
     &            ,DOx(1)         &   ! DO  (umol L-1)
# if defined NUTRIENTS         
     &            ,NH4(1)         &   ! NH4 concentration (umol L-1)
# endif
# if defined CARBON_ISOTOPE
     &            ,DI13C(1)       &   ! 13C of DIC (umol kg-1)
# endif
!          output parameters
     &            ,Flux_DIC      &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,Flux_DO       &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# if defined NUTRIENTS         
     &            ,Flux_NO3      &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,Flux_NH4      &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,Flux_PO4      &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
# if defined CARBON_ISOTOPE
     &            ,Flux_DI13C    &   ! DI13C uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
     &             )

        ! 1 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
        ! cff: convert [mmol m-2 s-1] to [umol L-1 s-1]

        cff = 1.0d0/dz(1) * p_sgrass

        dDIC_dt(1) = dDIC_dt(1) - Flux_DIC * cff/rho_sw
        dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
# if defined NUTRIENTS
        dNO3_dt(1) = dNO3_dt(1) - Flux_NO3 * cff
        dNH4_dt(1) = dNH4_dt(1) - Flux_NH4 * cff
        dPO4_dt(1) = dPO4_dt(1) - Flux_PO4 * cff
# endif
# if defined CARBON_ISOTOPE
        dDI13C_dt(1) = dDI13C_dt(1) - Flux_DI13C * cff/rho_sw
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
!          input parameters
     &            (ng, 1, i, j     &   ! ng: nested grid number; n: seagrass compartment; i,j: position
     &            ,PFDbott         &   ! Photon flux density (umol m-2 s-1)
     &            ,rho_sw          &   ! Density of seawater (g cm-3)
     &            ,DIC(1)         &   ! DIC (umol kg-1)
     &            ,DOx(1)         &   ! DO  (umol L-1)
# if defined NUTRIENTS         
     &            ,NH4(1)         &   ! NH4 concentration (umol L-1)
# endif
# if defined CARBON_ISOTOPE
     &            ,DI13C(1)       &   ! 13C of DIC (umol kg-1)
# endif
!          output parameters
     &            ,Flux_DIC      &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,Flux_DO       &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# if defined NUTRIENTS         
     &            ,Flux_NO3      &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,Flux_NH4      &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,Flux_PO4      &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
# if defined CARBON_ISOTOPE
     &            ,Flux_DI13C    &   ! DI13C uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
     &             )

        ! 1 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
        ! cff: convert [mmol m-2 s-1] to [umol L-1 s-1]

        cff = 1.0d0/dz(1) * p_algae

        dDIC_dt(1) = dDIC_dt(1) - Flux_DIC * cff/rho_sw
        dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
# if defined NUTRIENTS
        dNO3_dt(1) = dNO3_dt(1) - Flux_NO3 * cff
        dNH4_dt(1) = dNH4_dt(1) - Flux_NH4 * cff
        dPO4_dt(1) = dPO4_dt(1) - Flux_PO4 * cff
# endif
# if defined CARBON_ISOTOPE
        dDI13C_dt(1) = dDI13C_dt(1) - Flux_DI13C * cff/rho_sw
# endif

      END IF            
#endif

#ifdef SEDIMENT_ECOSYS
!-----------------------------------------------------------------------
! Compute sediment ecosystem model.
!-----------------------------------------------------------------------
!
      IF(p_sand .gt. 0.0d0) THEN
#ifdef SEDIMENT_EMPIRICAL
!!!  Empirical sediment model
          CALL sedecosys           &
!          input parameters
     &            (ng, i, j       &   ! ng: nested grid number; i,j: position
     &            ,PFDbott        &   ! Photon flux density (umol m-2 s-1)
     &            ,rho_sw         &   ! Density of seawater (g cm-3)
     &            ,DIC(1)         &   ! DIC (umol kg-1)
     &            ,TA (1)         &   ! TA (umol kg-1)
     &            ,DOx(1)         &   ! DO (umol L-1)
# if defined NUTRIENTS         
     &            ,NH4(1)         &   ! NH4 concentration (umol L-1)
# endif
# if defined CARBON_ISOTOPE
     &            ,DI13C(1)       &   ! 13C of DIC (umol kg-1)
# endif
!          output parameters
     &            ,Flux_DIC       &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,Flux_TA        &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,Flux_DO        &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# if defined NUTRIENTS         
     &            ,Flux_NO3       &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,Flux_NO2       &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
     &            ,Flux_PO4       &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
# if defined CARBON_ISOTOPE
     &            ,Flux_DI13C     &   ! DI13C uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
     &             )
     
      ! 1 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
      ! cff: convaert [mmol cm-2 s-1] to [umol L-1 s-1]

        cff=1.0d0 /dz(1) * p_sand
#else
!!!  Sediment ecosystem model

        DO m=1,isplitsed   !!! Loop for coral polyp model: dtc <= 0.05 sec

          CALL sedecosys          &
!          input parameters
     &            (ng, i, j       &   ! ng: nested grid number; i,j: position
     &            ,dtsed          &   ! Time step (sec)
     &            ,PFDbott        &   ! Photon flux density (umol m-2 s-1)
     &            ,Tmp(1)         &   ! Tmp: Temperature (oC)
     &            ,Sal(1)         &   ! Sal: Salinity (PSU)
     &            ,DIC(1)         &   ! DIC: Total dissolved inorganic carbon (DIC: umol kg-1)
     &            ,TA (1)         &   ! TA : Total alkalinity (TA: umol kg-1)
     &            ,DOx(1)         &   ! DOx: Dissolved oxygen (umol L-1)
#  if defined ORGANIC_MATTER
     &            ,DOC(1)         &   ! DOC: Dissolved organic carbon (DOC: umol L-1)
     &            ,POC(1)         &   ! POC: Particulate organic carbon (DOC: umol L-1)
#  endif
#  if defined CARBON_ISOTOPE
     &            ,DI13C(1)       &   ! DI13C 13C of DIC (umol kg-1)
#  endif
#  if defined NUTRIENTS            
     &            ,NO3(1)         &   ! NO3: NO3 (umol L-1)
     &            ,NO2(1)         &   ! NO2: NO2 (umol L-1)
     &            ,NH4(1)         &   ! NH4: NH4 (umol L-1)
     &            ,PO4(1)         &   ! PO4: PO4 (umol L-1)
#   if defined ORGANIC_MATTER
     &            ,DON(1)         &   ! DON: Dissolved organic nitrogen (DON: umol L-1)
     &            ,PON(1)         &   ! PON: Particulate organic nitrogen (PON: umol L-1)
     &            ,DOP(1)         &   ! DOP: Dissolved organic phosporius (DOP: umol L-1)
     &            ,POP(1)         &   ! POP: Particulate organic phosporius (POP: umol L-1)
#   endif
#  endif
#  if defined ORGANIC_MATTER
     &            ,Fdep_POC*1.0d2       &   ! POC deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#   if defined NUTRIENTS
     &            ,Fdep_PON*1.0d2       &   ! PON deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
     &            ,Fdep_POP*1.0d2       &   ! POP deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#   endif
#  endif
     &            ,tau            &   ! bottom shear stress (N m-2)
     &            ,0.0d0          &   ! sedimentation rate (??)
!          output parameters
     &            ,Flux_Tmp       &   ! Temperature flux (K cm-2 s-1)  * direction of water column to sediment is positive
     &            ,Flux_Sal       &   ! Salinity  flux (cm-2 s-1)  * direction of water column to sediment is positive
     &            ,Flux_DIC       &   ! DIC flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
     &            ,Flux_TA        &   ! TA  flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
     &            ,Flux_DO        &   ! DO  flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#  if defined ORGANIC_MATTER
     &            ,Flux_DOC       &   ! DOC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
     &            ,Flux_POC       &   ! POC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
#  endif
#  if defined CARBON_ISOTOPE
     &            ,Flux_DI13C     &   ! DI13C flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#  endif
#  if defined NUTRIENTS
     &            ,Flux_NO3       &   ! NO3 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
     &            ,Flux_NO2       &   ! NO2 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
     &            ,Flux_NH4       &   ! NH4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
     &            ,Flux_PO4       &   ! PO4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#   if defined ORGANIC_MATTER
     &            ,Flux_DON       &   ! DON flux (nmol cm-2 s-1) * direction of water column to sediment is positive
     &            ,Flux_PON       &   ! PON flux (nmol cm-2 s-1) * direction of water column to sediment is positive
     &            ,Flux_DOP       &   ! DOP flux (nmol cm-2 s-1) * direction of water column to sediment is positive
     &            ,Flux_POP       &   ! POP flux (nmol cm-2 s-1) * direction of water column to sediment is positive
#   endif
#  endif
     &             )
        END DO
      ! 1 nmol cm-2 s-1 = 0.01 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
      ! cff: convaert [nmol cm-2 s-1] to [umol L-1 s-1]

        cff=0.01d0 /dz(1) * p_sand

# endif


        dDIC_dt(1) = dDIC_dt(1) - Flux_DIC * cff/rho_sw
        dTA_dt (1) = dTA_dt (1) - Flux_TA  * cff/rho_sw
        dDOx_dt(1) = dDOx_dt(1) - Flux_DO  * cff
# if defined ORGANIC_MATTER
        dDOC_dt(1) = dDOC_dt(1) - Flux_DOC * cff
        dPOC_dt(1) = dPOC_dt(1) - Flux_POC * cff
# endif
# if defined CARBON_ISOTOPE
        dDI13C_dt(1) = dDI13C_dt(1) - Flux_DI13C * cff/rho_sw
# endif
# if defined NUTRIENTS
        dNO3_dt(1) = dNO3_dt(1) - Flux_NO3 * cff
        dNO2_dt(1) = dNO2_dt(1) - Flux_NO2 * cff
        dNH4_dt(1) = dNH4_dt(1) - Flux_NH4 * cff
        dPO4_dt(1) = dPO4_dt(1) - Flux_PO4 * cff
#  if defined ORGANIC_MATTER
        dDON_dt(1) = dDON_dt(1) - Flux_DON * cff
        dPON_dt(1) = dPON_dt(1) - Flux_PON * cff
        dDOP_dt(1) = dDOP_dt(1) - Flux_DOP * cff
        dPOP_dt(1) = dPOP_dt(1) - Flux_POP * cff
#  endif
# endif

      END IF            
#endif


#if defined ECOSYS_TESTMODE
!------------------------------------------------------------------------
! Print section

      time = time +dt  ! sec
      
      IF(time.ge.dsec) THEN
        cff1=0.01d0*p_coral(1)*P2R(1)*3600.0d0  ! cff1: convert [nmol cm-2 s-1] to [mmol m-2 h-1]
        cff2=0.01d0*p_coral(2)*P2R(2)*3600.0d0  ! cff1: convert [nmol cm-2 s-1] to [mmol m-2 h-1]
        cff3 = p_sgrass*3600.0d0        ! cff2: convert [mmol m-2 s-1] to [mmol m-2 h-1]
        cff4 = p_algae *3600.0d0        ! cff3: convert [mmol m-2 s-1] to [mmol m-2 h-1]
# if defined  SEDIMENT_EMPIRIXCAL
        cff5 = p_sand  *3600.0d0        ! cff4: convert [mmol m-2 s-1] to [mmol m-2 h-1]
# else
        cff5 = 0.01d0*p_sand*3600.0d0   ! cff4: convert [nmol cm-2 s-1] to [mmol m-2 h-1]
# endif

        write(40,*) time/86400.0d0,',',PFDbott,','                    &
# ifdef CORAL_POLYP
     &    ,CORAL(1)%Pg(1,1,1)*cff1,',', CORAL(1)%R (1,1,1)*cff1,','   &
     &    ,(CORAL(1)%Pg(1,1,1)-CORAL(1)%R (1,1,1))*cff1,','           &
     &    ,CORAL(1)%G (1,1,1)*cff1,','                                &
     &    ,CORAL(1)%Pg(2,1,1)*cff2,',', CORAL(1)%R (2,1,1)*cff2,','   &
     &    ,(CORAL(1)%Pg(2,1,1)-CORAL(1)%R (2,1,1))*cff2,','          &
     &    ,CORAL(1)%G (2,1,1)*cff2,','                                &
# endif
# ifdef SEAGRASS
     &    ,SGRASS(1)%Pg(1,1,1)*cff3,',', SGRASS(1)%R (1,1,1)*cff3,',' &
     &    ,(SGRASS(1)%Pg(1,1,1)-SGRASS(1)%R (1,1,1))*cff3,','         &
# endif
# ifdef MACROALGAE
     &    ,ALGAE(1)%Pg(1,1,1)*cff4,',', ALGAE(1)%R (1,1,1)*cff4,','   &
     &    ,(ALGAE(1)%Pg(1,1,1)-ALGAE(1)%R (1,1,1))*cff4,','           &
# endif
# ifdef SEDIMENT_ECOSYS
     &    ,SEDECO(1)%Pg(1,1)*cff5,',', SEDECO(1)%R (1,1)*cff5,','     &
     &    ,(SEDECO(1)%Pg(1,1)-SEDECO(1)%R (1,1))*cff5,','             &
     &    ,SEDECO(1)%G (1,1)*cff5,','                                 &
# endif
     &    ,dDIC_dt(1),',',dTA_dt(1),',',dDOx_dt(1),','                &
# if defined ORGANIC_MATTER
     &    ,dDOC_dt(1),',',dPOC_dt(1),','                              &
# endif
# if defined CARBON_ISOTOPE
     &    ,dDI13C_dt(1),','                                           &
# endif
# if defined NUTRIENTS
     &    ,dNO3_dt(1),',',dNO2_dt(1),',',dNH4_dt(1),','               &
     &    ,dPO4_dt(1),','                                             &
#  if defined ORGANIC_MATTER
     &    ,dDON_dt(1),',',dPON_dt(1),','                              &
     &    ,dDOP_dt(1),',',dPOP_dt(1),','                              &
#  endif
# endif
     &    ,sspH,',', ssfCO2,',', ssWarg,','                           &
     &    ,U10,',',ssCO2flux,',', ssO2flux
        
      dsec=dsec+OUTPUT_INTERVAL*60.

      END IF
!-----------------------------------------------------------------------
#endif

      RETURN
    END SUBROUTINE reef_ecosys

!-----------------------------------------------------------------------
  END MODULE mod_reef_ecosys

