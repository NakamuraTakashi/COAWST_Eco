
!!!=== Copyright (c) 2012-2025 Takashi NAKAMURA  =====

#include "cppdefs.h"


!!! ==================================================================================================
!!! **************************************************************************************************
!!! **                                                                                              **
!!! **                                       SEAGRASS MODEL                                         **
!!! **                                                                                              **
!!! ** -------------------------------------------------------------------------------------------- **
!!! **                                                                                              **
!!! **    This is the updated seagrass module devloped by Yuta A. Takagi (2023-2025)                **
!!! **    Use CPP flag SEAGRASS in your header file to enable                                       **
!!! **                                                                                              **
!!! **************************************************************************************************
!!! ==================================================================================================

MODULE mod_seagrass
#if defined SEAGRASS
!!! ==================================================================================================
!!! **************************************************************************************************
!!!                          CREATE SEAGRASS MODEL PRESERVED VARIABLES
!!! **************************************************************************************************
!!! ==================================================================================================

  implicit none

# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
  real(8), allocatable :: SeagrassRootProf(:, :, :) ! (ng, species, k) ! Profile of seagrass root biomass: fraction in each biological sediment layer 
# endif

  TYPE T_SGRASS ! create a class for all seagrass values (for a given nested grid)
    !                                                       (r c s i j l)                               the columns correspond to indexes for: r = isotope species; c = chemical species; s = species_id_number; i = i-position; j = j-position; l = sediment layer
! >>> Housekeeping Vars <<< !
    real(8), pointer :: Day                                     (  :,:)   ! [day]                       Last growth interval
! >>> Saved Constants <<< !
    ! model params
    real(8), pointer :: GridArea                                (  :,:)   ! [m2.grid]                   Total ground area in grid cell
    ! biophysical params
    real(8), pointer :: C_N_P_ratios                          (:,:    )   !                             C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]    
    real(8), pointer :: SgNperC                                 (:    )   ! [mol.N mol-1.C]             Species specific seagrass Nitrogen to Carbon ratio
    real(8), pointer :: SgPperC                                 (:    )   ! [mol.P mol-1.C]             Species specific seagrass Phosphorus to Carbon ratio
    real(8), pointer :: SgMperC                                 (:    )   ! [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    real(8), pointer :: f_above                                 (:    )   ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction
    real(8), pointer :: rho_lf                                  (:    )   ! [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
    real(8), pointer :: k_proj                                  (:    )   ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    real(8), pointer :: root_mass_profile                       (:,    :) ! []                          Mass ratio of roots in each layer (total must sum to 1.0)
# endif
    ! photoynthesis: light-limit
    real(8), pointer :: P_max                                   (:    )   ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate  
    real(8), pointer :: I_k                                     (:    )   ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve
    ! photoynthesis: DIC-limit
    real(8), pointer :: Lf_DIC_uptake_max                       (:    )   ! [umol.DIC g-1.lf.drywt s-1] Maximum uptake rate of DIC in the leaves
    real(8), pointer :: k_lf_DIC                                (:    )   ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer :: Rt_DIC_flux_max                         (:    )   ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    real(8), pointer :: k_rt_DIC                                (:    )   ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
# endif
    ! photoynthesis: nutrient-limit
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    real(8), pointer :: Lf_NO3_uptake_max                       (:    )   ! [umol.NO3 g-1.lf.drywt s-1] Maximum uptake rate of NO3 in the leaves
    real(8), pointer :: k_lf_NO3                                (:    )   ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth
    real(8), pointer :: smin_lf_NO3                             (:    )   ! [umol.NO3 L-1]              Minimum consentration of NO3 in water column for uptake
    real(8), pointer :: Lf_NO3_ratio_slope                      (:    )   ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    real(8), pointer :: Lf_NH4_uptake_max                       (:    )   ! [umol.NH4 g-1.lf.drywt s-1] Maximum uptake rate of NH4 in the leaves
    real(8), pointer :: k_lf_NH4                                (:    )   ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth
    real(8), pointer :: smin_lf_NH4                             (:    )   ! [umol.NH4 L-1]              Minimum consentration of NH4 in water column for uptake
    real(8), pointer :: Lf_NH4_ratio_slope                      (:    )   ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    real(8), pointer :: Lf_PO4_uptake_max                       (:    )   ! [umol.PO4 g-1.lf.drywt s-1] Maximum uptake rate of PO4 in the leaves
    real(8), pointer :: k_lf_PO4                                (:    )   ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth
    real(8), pointer :: smin_lf_PO4                             (:    )   ! [umol.PO4 L-1]              Minimum consentration of PO4 in water column for uptake
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer :: Rt_NO3_flux_max                         (:    )   ! [umol.NO3 g-1.rt.drywt s-1] Maximum uptake rate of NO3 in the roots 
    real(8), pointer :: k_rt_NO3                                (:    )   ! [umol.NO3 L-1]              Half saturation consentration of NO3 in soil pore water for growth
    real(8), pointer :: smin_rt_NO3                             (:    )   ! [umol.NO3 L-1]              Minimum consentration of NO3 in soil pore water for uptake
    real(8), pointer :: Rt_NO3_ratio_slope                      (:    )   ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    real(8), pointer :: Rt_NH4_flux_max                         (:    )   ! [umol.NH4 g-1.rt.drywt s-1] Maximum uptake rate of NH4 in the roots 
    real(8), pointer :: k_rt_NH4                                (:    )   ! [umol.NH4 L-1]              Half saturation consentration of NH4 in soil pore water for growth
    real(8), pointer :: smin_rt_NH4                             (:    )   ! [umol.NH4 L-1]              Minimum consentration of NH4 in soil pore water for uptake
    real(8), pointer :: Rt_NH4_ratio_slope                      (:    )   ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    real(8), pointer :: Rt_PO4_flux_max                         (:    )   ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    real(8), pointer :: k_rt_PO4                                (:    )   ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
    real(8), pointer :: smin_rt_PO4                             (:    )   ! [umol.PO4 L-1]              Minimum consentration of PO4 in soil pore water for uptake
# endif
    ! respiration: DO-limit
    real(8), pointer :: Lf_DO_uptake_max                        (:    )   ! [umol.DO g-1.lf.drywt s-1]  Maximum uptake rate of DO in the leaves
    real(8), pointer :: k_lf_DO                                 (:    )   ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer :: Rt_DO_flux_max                          (:    )   ! [umol.DO g-1.rt.drywt s-1]  Maximum uptake rate of DO in the roots 
    real(8), pointer :: k_rt_DO                                 (:    )   ! [umol.DO L-1]               Half saturation consentration of DO in soil pore water for growth
# endif
    ! respiration: required
    real(8), pointer :: R_basal                                 (:    )   ! [umol.C g-1.leaf s-1]       Maximum respiration rate
    real(8), pointer :: Turnover_rate                           (:    )   ! [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day
    ! dieoff when respiration is insufficient
    real(8), pointer :: max_anoxic_dieoff_rate                  (:    )   ! [mol.C mol-1.C day-1]       maximum dieoff due to anoxia in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
    ! stockpile maximum cap
    real(8), pointer :: DICstockCap                             (:    )   ! Factor to multiply carbon biomsass to get maximum DIC stock (stock limit = DICstockCap*SgCBmF) 
    real(8), pointer :: CH2OstockCap                            (:    )   ! Factor to multiply carbon biomsass to get maximum sugar stock (stock limit = CH2OstockCap*SgCBmF) 
    real(8), pointer :: DOstockCap                              (:    )   ! Factor to multiply carbon biomsass to get maximum DO stock (stock limit = DOstockCap*SgCBmF) 
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer :: NO3stockCap                             (:    )   ! Factor to multiply nitrogen biomsass to get maximum NO3 stock (stock limit = NO3stockCap*SgNBmF) 
    real(8), pointer :: NH4stockCap                             (:    )   ! Factor to multiply nitrogen biomsass to get maximum NH4 stock (stock limit = NH4stockCap*SgNBmF) 
    real(8), pointer :: PO4stockCap                             (:    )   ! Factor to multiply phosphorus biomsass to get maximum PO4 stock (stock limit = PO4stockCap*SgPBmF) 
# endif
    ! Update at end of growth interval
    ! allocate growth
    ! update bed-density/habitat area
    real(8), pointer :: trans_rate                              (:    )   ! [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
    real(8), pointer :: opt_sg_bed_density                      (:    )   ! [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)

! >>> Saved Values <<< !
    ! Update at end of main interval
    real(8), pointer :: SgCBmF                              (:,  :,:,:)   ! [umol.C m-2.sg.hab]         iSgSgCBm      Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    real(8), pointer :: DICstock                            (:,  :,:,:)   ! [umol.DIC m-2.sg.hab]       iSgDICstock   Seagrass internal DIC stock per unit ground area (in seagrass habitat area)
    real(8), pointer :: CH2Ostock                           (:,  :,:,:)   ! [umol.CH2O m-2.sg.hab]      iSgCH2Ostock  Seagrass internal sugar stock per unit ground area (in seagrass habitat area)
    real(8), pointer :: DOstock                             (    :,:,:)   ! [umol.DO m-2.sg.hab]        iSgDOstock    Seagrass internal DO stock per unit ground area (in seagrass habitat area)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer :: SgNBmF                              (:,  :,:,:)   ! [umol.N m-2.sg.hab]         iSgSgNBm      Seagrass leaf+root nitrogen-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    real(8), pointer :: SgPBmF                              (:,  :,:,:)   ! [umol.P m-2.sg.hab]         iSgSgPBm      Seagrass leaf+root phosphorus-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    real(8), pointer :: NO3stock                            (:,  :,:,:)   ! [umol.NO3 m-2.sg.hab]       iSgNO3stock   Seagrass internal NO3 stock per unit ground area (in seagrass habitat area)
    real(8), pointer :: NH4stock                            (:,  :,:,:)   ! [umol.NH4 m-2.sg.hab]       iSgNH4stock   Seagrass internal NH4 stock per unit ground area (in seagrass habitat area)
    real(8), pointer :: PO4stock                            (:,  :,:,:)   ! [umol.PO4 m-2.sg.hab]       iSgPO4stock   Seagrass internal PO4 stock per unit ground area (in seagrass habitat area)
#  endif
    real(8), pointer :: TotSgCBmF                               (:,:,:)   ! [mol.C.tot]                 iSgTotSgCBm   Total seagrass carbon-biomass in grid. updates on fine time interval
    ! Update at end of growth interval
    ! update total carbon-biomass
    real(8), pointer :: TotSgCBm                                (:,:,:)   ! [mol.C.tot]                 iSgTotSgCBmS  Total seagrass carbon-biomass in grid
    ! allocate growth
    real(8), pointer :: TotLfCBm                                (:,:,:)   ! [mol.C.lf]                  iSgTotLfCBm   Total seagrass above ground carbon-biomass in grid
    real(8), pointer :: TotRtCBm                                (:,:,:)   ! [mol.C.rt]                  iSgTotRtCBm   Total seagrass below ground carbon-biomass in grid
    ! update bed-density/habitat area
    real(8), pointer :: HabArea                                 (:,:,:)   ! [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
    ! leaf-root biomass translocation
    ! update carbon-biomass per area
    real(8), pointer :: LfCBm                                   (:,:,:)   ! [umol.lf.C m-2.sg.hab]      iSgLfCBm      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
    real(8), pointer :: RtCBm                                   (:,:,:)   ! [umol.rt.C m-2.sg.hab]      iSgRtCBm      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
    ! update dry-weight biomass
    real(8), pointer :: TotSgDryBm                              (:,:,:)   ! [g.drywt]                   Total seagrass dry weight biomass in grid cell
    real(8), pointer :: TotLfDryBm                              (:,:,:)   ! [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
    real(8), pointer :: TotRtDryBm                              (:,:,:)   ! [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
    real(8), pointer :: SgDryBm                                 (:,:,:)   ! [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
    real(8), pointer :: LfDryBm                                 (:,:,:)   ! [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
    real(8), pointer :: RtDryBm                                 (:,:,:)   ! [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! update leaf area index
    real(8), pointer :: LAI                                     (:,:,:)   ! [m2.lf m-2.sg.hab]          iSgLAI          Leaf area index: one-sided green leaf area per unit ground surface area 

! >>> Saved Values Only for Output  <<< !
    real(8), pointer :: TotLA                                   (:,:,:)   ! [m2.lf]                     iSgTotLA        Total one-sided green leaf area in grid
    real(8), pointer :: GridELAP                                (:,:,:)   ! [m2.lf.proj m-2.grid]       iSgGridELAP     Effective leaf area projection on ground (whole grid)
    real(8), pointer :: GridPhot                                (:,:,:)   ! [umol.C m-2.grid s-1]       iSgGridPhot     Seagrass gross photosynthesis rate per unit ground area (whole grid)
    real(8), pointer :: GridGrow                                (:,:,:)   ! [umol.C m-2.grid s-1]       iSgGridGrow     Seagrass growth rate per unit ground area (whole grid)
    integer, pointer :: PhotLim                                 (:,:,:)   ! []                          iSgPhotLim      Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon stock
    integer, pointer :: GrowLim                                 (:,:,:)   ! []                          iSgGrowLim      Seagrass growth limiting factor: 1 = Sugar stock; 3 = Nitrogen stock; 4 = Phosphorus stock
    real(8), pointer :: GridResp                                (:,:,:)   ! [umol.C m-2.grid s-1]       iSgGridResp     Seagrass respiration rate per unit ground area (whole grid)
    real(8), pointer :: GridNetPhot                             (:,:,:)   ! [umol.C m-2.grid s-1]       iSgGridNetPhot  Seagrass net photosynthesis rate per unit ground area (whole grid)
    real(8), pointer :: GridDieoff                              (:,:,:)   ! [umol.C m-2.grid s-1]       iSgGridDieoff   Seagrass dieoff rate carbon biomass per unit ground area (whole grid)

# if defined NET_ZERO_MASS_START
! >>> Calculate initial seagrass mass balance and destroy (erase) equivalent amount of mass output until net zero is reached, before switching to normal flux <<< !
    real(8), pointer :: InitialC                                (:,:,:)   ! [mmol.C]                    Initial carbon remaining to destroy until net zero
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer :: InitialN                                (:,:,:)   ! [mmol.N]                    Initial nitrogen remaining to destroy until net zero
    real(8), pointer :: InitialP                                (:,:,:)   ! [mmol.P]                    Initial phosphorus remaining to destroy until net zero
#  endif
# endif


  END TYPE T_SGRASS

  TYPE (T_SGRASS), allocatable :: SGRASS(:) ! create an array of T_SGRASS for all nested grids
  
  CONTAINS
  

!!! ==================================================================================================
!!! **************************************************************************************************
!!!                             INITIALIZATION OF SEAGRASS MODEL
!!! **************************************************************************************************
!!! ==================================================================================================
  
! ====================================================================================================
! Seagrass initialization subroutine in/out interface
! ====================================================================================================
  SUBROUTINE allocate_seagrass(ng, Ngrids, LBi, UBi, LBj, UBj)

! ====================================================================================================
! Module dependencies
! ====================================================================================================
    USE mod_reef_ecosys_param

! ====================================================================================================
! Variable declarations
! ====================================================================================================
    
    implicit none
! input parameters
    integer, intent(in) :: ng, Ngrids                          !                         ng is the nested grid number
    integer, intent(in) :: LBi, UBi, LBj, UBj                  !                         lower and upper bounds i & j
  
! ====================================================================================================
! Variable memory allocaation
! ====================================================================================================
! if initializaing the first nested grid, allocate the SGRASS array
    IF (ng.eq.1) THEN
      allocate ( SGRASS(Ngrids) ) ! array of T_SGRASS which holds all the values for each nested grid
    ENDIF
! >>> Housekeeping Vars <<< !
    allocate( SGRASS(ng)%Day                                            (LBi:UBi,LBj:UBj)  ) ! [day]                       Last growth interval
! >>> Saved Constants <<< !
    ! model params
    allocate( SGRASS(ng)%GridArea                                       (LBi:UBi,LBj:UBj)  ) ! [m2.grid]                   Total ground area in grid cell
    ! biophysical params
    allocate( SGRASS(ng)%C_N_P_ratios                            (3, Nsg)                  ) !                             C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]
    allocate( SGRASS(ng)%SgNperC                                    (Nsg)                  ) ! [mol.N mol-1.C]             Species specific seagrass Nitrogen to Carbon ratio
    allocate( SGRASS(ng)%SgPperC                                    (Nsg)                  ) ! [mol.P mol-1.C]             Species specific seagrass Phosphorus to Carbon ratio
    allocate( SGRASS(ng)%SgMperC                                    (Nsg)                  ) ! [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    allocate( SGRASS(ng)%f_above                                    (Nsg)                  ) ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction
    allocate( SGRASS(ng)%rho_lf                                     (Nsg)                  ) ! [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
    allocate( SGRASS(ng)%k_proj                                     (Nsg)                  ) ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient  
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    allocate( SGRASS(ng)%root_mass_profile                          (Nsg, Nsed)            ) ! []                          Mass ratio of roots in each layer (total must sum to 1.0)
# endif    
    ! photoynthesis: light-limit
    allocate( SGRASS(ng)%P_max                                      (Nsg)                  ) ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate  
    allocate( SGRASS(ng)%I_k                                        (Nsg)                  ) ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve
    ! photoynthesis: DIC-limit
    allocate( SGRASS(ng)%Lf_DIC_uptake_max                          (Nsg)                  ) ! [umol.DIC g-1.lf.drywt s-1] Maximum uptake rate of DIC in the leaves
    allocate( SGRASS(ng)%k_lf_DIC                                   (Nsg)                  ) ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    allocate( SGRASS(ng)%Rt_DIC_flux_max                            (Nsg)                  ) ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    allocate( SGRASS(ng)%k_rt_DIC                                   (Nsg)                  ) ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
# endif
    ! photoynthesis: nutrient-limit
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    allocate( SGRASS(ng)%Lf_NO3_uptake_max                          (Nsg)                  ) ! [umol.NO3 g-1.lf.drywt s-1] Maximum uptake rate of NO3 in the leaves
    allocate( SGRASS(ng)%k_lf_NO3                                   (Nsg)                  ) ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth
    allocate( SGRASS(ng)%smin_lf_NO3                                (Nsg)                  ) ! [umol.NO3 L-1]              Minimum consentration of NO3 in water column for uptake
    allocate( SGRASS(ng)%Lf_NO3_ratio_slope                         (Nsg)                  ) ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    allocate( SGRASS(ng)%Lf_NH4_uptake_max                          (Nsg)                  ) ! [umol.NH4 g-1.lf.drywt s-1] Maximum uptake rate of NH4 in the leaves
    allocate( SGRASS(ng)%k_lf_NH4                                   (Nsg)                  ) ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth
    allocate( SGRASS(ng)%smin_lf_NH4                                (Nsg)                  ) ! [umol.NH4 L-1]              Minimum consentration of NH4 in water column for uptake
    allocate( SGRASS(ng)%Lf_NH4_ratio_slope                         (Nsg)                  ) ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    allocate( SGRASS(ng)%Lf_PO4_uptake_max                          (Nsg)                  ) ! [umol.PO4 g-1.lf.drywt s-1] Maximum uptake rate of PO4 in the leaves
    allocate( SGRASS(ng)%k_lf_PO4                                   (Nsg)                  ) ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth
    allocate( SGRASS(ng)%smin_lf_PO4                                (Nsg)                  ) ! [umol.PO4 L-1]              Minimum consentration of PO4 in water column for uptake
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    allocate( SGRASS(ng)%Rt_NO3_flux_max                            (Nsg)                  ) ! [umol.NO3 g-1.rt.drywt s-1] Maximum uptake rate of NO3 in the roots 
    allocate( SGRASS(ng)%k_rt_NO3                                   (Nsg)                  ) ! [umol.NO3 L-1]              Half saturation consentration of NO3 in soil pore water for growth
    allocate( SGRASS(ng)%smin_rt_NO3                                (Nsg)                  ) ! [umol.NO3 L-1]              Minimum consentration of NO3 in soil pore water for uptake
    allocate( SGRASS(ng)%Rt_NO3_ratio_slope                         (Nsg)                  ) ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    allocate( SGRASS(ng)%Rt_NH4_flux_max                            (Nsg)                  ) ! [umol.NH4 g-1.rt.drywt s-1] Maximum uptake rate of NH4 in the roots 
    allocate( SGRASS(ng)%k_rt_NH4                                   (Nsg)                  ) ! [umol.NH4 L-1]              Half saturation consentration of NH4 in soil pore water for growth
    allocate( SGRASS(ng)%smin_rt_NH4                                (Nsg)                  ) ! [umol.NH4 L-1]              Minimum consentration of NH4 in soil pore water for uptake
    allocate( SGRASS(ng)%Rt_NH4_ratio_slope                         (Nsg)                  ) ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    allocate( SGRASS(ng)%Rt_PO4_flux_max                            (Nsg)                  ) ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    allocate( SGRASS(ng)%k_rt_PO4                                   (Nsg)                  ) ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
    allocate( SGRASS(ng)%smin_rt_PO4                                (Nsg)                  ) ! [umol.PO4 L-1]              Minimum consentration of PO4 in soil pore water for uptake
# endif
    ! respiration: DO-limit
    allocate( SGRASS(ng)%Lf_DO_uptake_max                           (Nsg)                  ) ! [umol.DO g-1.lf.drywt s-1]  Maximum uptake rate of DO in the leaves
    allocate( SGRASS(ng)%k_lf_DO                                    (Nsg)                  ) ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    allocate( SGRASS(ng)%Rt_DO_flux_max                             (Nsg)                  ) ! [umol.DO g-1.rt.drywt s-1]  Maximum uptake rate of DO in the roots 
    allocate( SGRASS(ng)%k_rt_DO                                    (Nsg)                  ) ! [umol.DO L-1]               Half saturation consentration of DO in soil pore water for growth
# endif
    ! respiration: required
    allocate( SGRASS(ng)%R_basal                                    (Nsg)                  ) ! [umol.C g-1.lf.drywt s-1]   Maximum respiration rate
    allocate( SGRASS(ng)%Turnover_rate                              (Nsg)                  ) ! [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day
    ! dieoff when respiration is insufficient
    allocate( SGRASS(ng)%max_anoxic_dieoff_rate                     (Nsg)                  ) ! [mol.C mol-1.C day-1]       maximum dieoff due to anoxia in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
    ! stockpile maximum cap
    allocate( SGRASS(ng)%DICstockCap                                (Nsg)                  ) ! Factor to multiply carbon biomsass to get maximum DIC stock (stock limit = DICstockCap*SgCBmF) 
    allocate( SGRASS(ng)%CH2OstockCap                               (Nsg)                  ) ! Factor to multiply carbon biomsass to get maximum sugar stock (stock limit = CH2OstockCap*SgCBmF) 
    allocate( SGRASS(ng)%DOstockCap                                 (Nsg)                  ) ! Factor to multiply carbon biomsass to get maximum DO stock (stock limit = DOstockCap*SgCBmF) 
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    allocate( SGRASS(ng)%NO3stockCap                                (Nsg)                  ) ! Factor to multiply nitrogen biomsass to get maximum NO3 stock (stock limit = NO3stockCap*SgNBmF) 
    allocate( SGRASS(ng)%NH4stockCap                                (Nsg)                  ) ! Factor to multiply nitrogen biomsass to get maximum NH4 stock (stock limit = NH4stockCap*SgNBmF) 
    allocate( SGRASS(ng)%PO4stockCap                                (Nsg)                  ) ! Factor to multiply phosphorus biomsass to get maximum PO4 stock (stock limit = PO4stockCap*SgPBmF) 
# endif
    ! Growth interval
    allocate( SGRASS(ng)%trans_rate                                 (Nsg)                  ) ! [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
    allocate( SGRASS(ng)%opt_sg_bed_density                         (Nsg)                  ) ! [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)
! >>> Saved Values <<< !
    ! Update at end of main interval
    allocate( SGRASS(ng)%SgCBmF                             (N_Csp,  Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    allocate( SGRASS(ng)%DICstock                           (N_Csp,  Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.DIC m-2.sg.hab]       Seagrass internal DIC stock per unit ground area (in seagrass habitat area)
    allocate( SGRASS(ng)%CH2Ostock                          (N_Csp,  Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.CH2O m-2.sg.hab]      Seagrass internal sugar stock per unit ground area (in seagrass habitat area)
    allocate( SGRASS(ng)%DOstock                            (        Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.DO m-2.sg.hab]        Seagrass internal DO stock per unit ground area (in seagrass habitat area)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    allocate( SGRASS(ng)%SgNBmF                             (N_Nsp,  Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.C m-2.sg.hab]         Seagrass leaf+root nitrogen-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    allocate( SGRASS(ng)%SgPBmF                             (N_Psp,  Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.C m-2.sg.hab]         Seagrass leaf+root phosphorus-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    allocate( SGRASS(ng)%NO3stock                           (N_Nsp,  Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.NO3 m-2.sg.hab]       Seagrass internal NO3 stock per unit ground area (in seagrass habitat area)
    allocate( SGRASS(ng)%NH4stock                           (N_Nsp,  Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.NH4 m-2.sg.hab]       Seagrass internal NH4 stock per unit ground area (in seagrass habitat area)
    allocate( SGRASS(ng)%PO4stock                           (N_Psp,  Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.PO4 m-2.sg.hab]       Seagrass internal PO4 stock per unit ground area (in seagrass habitat area)
#  endif
    allocate( SGRASS(ng)%TotSgCBmF                                  (Nsg,LBi:UBi,LBj:UBj)  ) ! [mol.C.tot]                 Total seagrass carbon-biomass in grid. updates on fine time interval
    ! Update at end of growth interval
    ! update total carbon-biomass
    allocate( SGRASS(ng)%TotSgCBm                                   (Nsg,LBi:UBi,LBj:UBj)  ) ! [mol.C.tot]                 Total seagrass carbon-biomass in grid
    ! allocate growth
    allocate( SGRASS(ng)%TotLfCBm                                   (Nsg,LBi:UBi,LBj:UBj)  ) ! [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid
    allocate( SGRASS(ng)%TotRtCBm                                   (Nsg,LBi:UBi,LBj:UBj)  ) ! [mol.C.rt]                  Total seagrass root carbon-biomass in grid
    ! update bed-density/habitat area
    allocate( SGRASS(ng)%HabArea                                    (Nsg,LBi:UBi,LBj:UBj)  ) ! [m2.sg.hab]                 Seagrass habitat ground area in grid cell such that seagrass coverage (p_sgrass) times the grid area equals the habitat area
    ! leaf-root biomass translocation
    ! update carbon-biomass per area
    allocate( SGRASS(ng)%LfCBm                                      (Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
    allocate( SGRASS(ng)%RtCBm                                      (Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
    ! update dry-weight biomass
    allocate( SGRASS(ng)%TotSgDryBm                                 (Nsg,LBi:UBi,LBj:UBj)  ) ! [g.drywt]                   Total seagrass dry weight biomass in grid cell
    allocate( SGRASS(ng)%TotLfDryBm                                 (Nsg,LBi:UBi,LBj:UBj)  ) ! [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
    allocate( SGRASS(ng)%TotRtDryBm                                 (Nsg,LBi:UBi,LBj:UBj)  ) ! [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
    allocate( SGRASS(ng)%SgDryBm                                    (Nsg,LBi:UBi,LBj:UBj)  ) ! [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
    allocate( SGRASS(ng)%LfDryBm                                    (Nsg,LBi:UBi,LBj:UBj)  ) ! [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
    allocate( SGRASS(ng)%RtDryBm                                    (Nsg,LBi:UBi,LBj:UBj)  ) ! [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! update leaf area index
    allocate( SGRASS(ng)%LAI                                        (Nsg,LBi:UBi,LBj:UBj)  ) ! [m2.lf m-2.sg.hab]          Leaf area index: one-sided green leaf area per unit ground surface area
! >>> Saved Values Only for Output  <<< !
    allocate( SGRASS(ng)%TotLA                                      (Nsg,LBi:UBi,LBj:UBj)  ) ! [m2.lf]                     Total one-sided green leaf area in grid
    allocate( SGRASS(ng)%GridELAP                                   (Nsg,LBi:UBi,LBj:UBj)  ) ! [m2.lf.proj m-2.grid]       Effective leaf area projection on ground (whole grid)
    allocate( SGRASS(ng)%GridPhot                                   (Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.C m-2.grid s-1]       Seagrass gross photosynthesis rate per unit ground area (whole grid)
    allocate( SGRASS(ng)%GridGrow                                   (Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.C m-2.grid s-1]       Seagrass growth rate per unit ground area (whole grid)
    allocate( SGRASS(ng)%PhotLim                                    (Nsg,LBi:UBi,LBj:UBj)  ) ! []                          Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon stock
    allocate( SGRASS(ng)%GrowLim                                    (Nsg,LBi:UBi,LBj:UBj)  ) ! []                          Seagrass growth limiting factor: 1 = Sugar stock; 2 = Nitrogen stock; 3 = Phosphorus stock
    allocate( SGRASS(ng)%GridResp                                   (Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.C m-2.grid s-1]       Seagrass respiration rate per unit ground area (whole grid)
    allocate( SGRASS(ng)%GridNetPhot                                (Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.C m-2.grid s-1]       Seagrass net photosynthesis rate per unit ground area (whole grid)
    allocate( SGRASS(ng)%GridDieoff                                 (Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.C m-2.grid s-1]       Seagrass dieoff rate carbon biomass per unit ground area (whole grid)
# if defined NET_ZERO_MASS_START
! >>> Calculate initial seagrass mass balance and destroy (erase) equivalent amount of mass output until net zero is reached, before switching to normal flux <<< !
    allocate( SGRASS(ng)%InitialC                                   (Nsg,LBi:UBi,LBj:UBj)  ) ! [mmol.C]                    Initial carbon remaining to destroy until net zero
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    allocate( SGRASS(ng)%InitialN                                   (Nsg,LBi:UBi,LBj:UBj)  ) ! [mmol.N]                    Initial nitrogen remaining to destroy until net zero
    allocate( SGRASS(ng)%InitialP                                   (Nsg,LBi:UBi,LBj:UBj)  ) ! [mmol.P]                    Initial phosphorus remaining to destroy until net zero
#  endif
# endif

    ! write(*,*) 'yt_debug: mod_seagrass.F initialize_seagrass() after array allocations'

    RETURN

  END SUBROUTINE allocate_seagrass


!!! ==================================================================================================
!!! **************************************************************************************************
!!!                             INITIALIZATION OF SEAGRASS MODEL
!!! **************************************************************************************************
!!! ==================================================================================================
  
! ====================================================================================================
! Seagrass initialization subroutine in/out interface
! ====================================================================================================
  SUBROUTINE initialize_seagrass(ng, LBi, UBi, LBj, UBj, start_of_new_run, XI_size, ETA_size, p_sgrass)

! ====================================================================================================
! Module dependencies
! ====================================================================================================
    USE mod_reef_ecosys_param
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    USE mod_sedecosys,    ONLY : SEDECO
# endif

! ====================================================================================================
! Variable declarations
! ====================================================================================================
    implicit none
! input parameters
    integer, intent(in) :: ng                             ! ng is the nested grid number
    integer, intent(in) :: LBi, UBi, LBj, UBj             ! lower and upper bounds i & j
    logical, intent(in) :: start_of_new_run               ! TRUE = initialize coral, seagass, macroalgae, sediment from start; FALSE = continue from previous run
    real(8), intent(in) :: XI_size (    LBi:UBi,LBj:UBj)  ! grid size XI-direction (meters)
    real(8), intent(in) :: ETA_size(    LBi:UBi,LBj:UBj)  ! grid size ETA-direction (meters)
    real(8), intent(in) :: p_sgrass(Nsg,LBi:UBi,LBj:UBj)  ! seagrass coverage (habitat area/grid area)

! indexing variables
    integer species,i,j
    integer k                                             ! Vertical layer counter

! ====================================================================================================
! Variable initializations
! ====================================================================================================
! >>> Saved Constants <<< !
! Set constant values  yt_edit make this user definable!!!
    ! biophysical params
    ! SGRASS(ng)%C_N_P_ratios                         = reshape([ 545d0, 35d0, 1d0 ],[3, Nsg])     ! C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]     ! Martins and Bandeira (2001) measured C:N:P = 545:35:1 for T hemprichii
    ! SGRASS(ng)%C_N_P_ratios                         = reshape([ 474d0, 24d0, 1d0 ],[3, Nsg])     ! C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]     ! Duerte (1990) C:N:P = 474:24:1 for average across many species
    ! SGRASS(ng)%C_N_P_ratios                         = reshape([ 550d0, 30d0, 1d0 ],[3, Nsg])     ! C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]     ! Atkinson and Smith (1983) C:N:P = 550:30:1 for average across many species
    SGRASS(ng)%C_N_P_ratios                         = reshape([ 275d0, 35d0, 1d0 ],[3, Nsg])     ! C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]     ! same as Martins and Bandeira (2001) except carbon halved to account for variability in non-tissue sugar stock
    ! SGRASS(ng)%f_above                              = [ 0.21d0   ]                                    ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction                              ! 0.21 Mallombasi et al. (2020)
    SGRASS(ng)%f_above                              = [ 0.35d0   ]                                    ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction                              ! 46.8 / 133.3 = 0.35 Watanabe chamber (2012)
    ! SGRASS(ng)%f_above                              = [ 0.23d0   ]                                    ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction                              ! 93 / (93 + 316) = 0.23 Takagi incubation (2025) T. hemprichii from coral debris area
    ! SGRASS(ng)%f_above                              = [ 0.55d0   ]                                    ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction                              ! 137 / (137 + 113) = 0.55 Takagi incubation (2025) T. hemprichii from sandy area
    ! SGRASS(ng)%f_above                              = [ 0.49d0   ]                                    ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction                              ! 38 / (38 + 40) = 0.49 Takagi incubation (2025) H. uninervis from sandy area
    ! SGRASS(ng)%f_above                            = [ 1.0d0    ]                                    ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction                              ! yt_debug testing
    SGRASS(ng)%rho_lf                               = [ 0.069d0  ]                                    ! [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density                                                                  ! 0.069 kg/m2 Calculated from Zulfikar et al. (2020) 
    SGRASS(ng)%k_proj                               = [ 0.8d0    ]                                    ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient                                                                  ! 0.8 is a guess
    ! SGRASS(ng)%k_proj                             = [ 1.0d0    ]                                    ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient                                                                
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    ! SeagrassRootProf(ng, species, k)                  Profile of seagrass root biomass: fraction in each biological sediment layer 
    ! SGRASS(ng)%root_mass_profile(1,:)               = &!SeagrassRootProf(ng, :, :)                      ! Mass ratio of roots in each layer (total must sum to 1.0)
    !  [0.0125d0, 0.0125d0, 0.0125d0, 0.0125d0, 0.0125d0, 0.0125d0, 0.0125d0, 0.0125d0, 0.0125d0, 0.0125d0, &
    !   0.0625d0, 0.0625d0, 0.125d0 , 0.125d0 , 0.125d0 , 0.125d0 , 0.125d0 , 0.125d0 , 0.0d0   , 0.0d0   , 0.0d0  ]   ! Nsed = 21; even vertical distribution (straight roots) until 8 cm depth
    ! write(*,*) 'yt_debug: mod_seagrass.F initialize_seagrass() after root_mass_profile assigment'
    ! write(*,*) 'yt_debug: mod_seagrass.F initialize_seagrass() root_mass_profile = ', SGRASS(ng)%root_mass_profile


    ! Calculate root mass profile based on empirical equation below and sediment layers from sedecosys

    ! Thalassia hemprichii (seagrass group 1)
    ! y = a*(z-atan(k*(x-b).^2))
    ! where y is total root width (proportional to mass) and x is depth (cm)
    ! a = 0.5785
    ! k = 0.08588
    ! z = 1.422
    ! b = 2.224
    ! x_min = 0
    ! x_max = (tan(z)/k)^0.5+b = 11.0375

    ! dzr(:)           layer thickness (cm)
    ! zr(:)            sediment depth  (cm)

    do k=1,Nsed
      if(SEDECO(ng)%zr(k) > 11.0375d0) then
        SGRASS(ng)%root_mass_profile(1, k) = 0d0;
      else
        SGRASS(ng)%root_mass_profile(1, k) = 0.5785d0*(1.422d0-ATAN(0.08588d0*(SEDECO(ng)%zr(k)-2.224d0)**2d0)) * SEDECO(ng)%dzr(k);
      endif
    enddo
    SGRASS(ng)%root_mass_profile(1, :) = SGRASS(ng)%root_mass_profile(1, :) / sum(SGRASS(ng)%root_mass_profile(1, :)); ! Mass ratio of roots in each layer (total must sum to 1.0)
    SGRASS(ng)%root_mass_profile(1, 1) = 1.0d0 - sum(SGRASS(ng)%root_mass_profile(1, 2:Nsed));                         ! make up for numerical inaccuracy to force to sum to 1.0
    write(*,*) 'yt_debug: mod_seagrass.F initialize_seagrass() after root_mass_profile assigment'
    
# endif

    ! photoynthesis: light-limit
    ! SGRASS(ng)%P_max                              = [ 3.45d0   ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! 50 µmol/kg/s Calculated from net photosynthesis data from Bjo ̈rk et al. (1997) or 3.45 µmol.C m-2.leaf s-1 (see master's thesis section 6.3.2c)
    ! SGRASS(ng)%P_max                                = [ 73.8d0   ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! tuning: when R_basal = 2.5265d0 (half-life in complete darkness is ~7.0 days), biomass reaches equilibrium at 2 kg.drywt/m2.hab under max-photosynthesis.
    ! SGRASS(ng)%P_max                              = [ 34.5d0   ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! yt_debug testing
    ! SGRASS(ng)%P_max                                = [ 0.55d0   ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! yuta 2025 incubation experiment T. hemprichii leaf net photosynthesis rate 123 [umol h-1 g-1]; leaf area to dryweight 0.616 [cm2 mg-1];   123 (umol/hg) / 0.616 (cm2/mg) * (10000cm2/m2) (h/3600s) (g/1000mg) = 0.5547
    ! SGRASS(ng)%P_max                                = [ 0.65d0   ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! yuta 2025 incubation experiment H. uninervis leaf net photosynthesis rate 198.44 [umol h-1 g-1]; leaf area to dryweight 0.851 [cm2 mg-1];   198.44 (umol/hg) / 0.851 (cm2/mg) * (10000cm2/m2) (h/3600s) (g/1000mg) = 0.6477
    ! SGRASS(ng)%P_max                                = [ 0.74d0   ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! yuta 2025 incubation experiment T. hemprichii leaf gross photosynthesis rate 163.76 [umol h-1 g-1]; leaf area to dryweight 0.616 [cm2 mg-1];   163.76 (umol/hg) / 0.616 (cm2/mg) * (10000cm2/m2) (h/3600s) (g/1000mg) = 0.7385
    ! SGRASS(ng)%P_max                                = [ 0.86d0   ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! yuta 2025 incubation experiment H. uninervis leaf gross photosynthesis rate 263.90 [umol h-1 g-1]; leaf area to dryweight 0.851 [cm2 mg-1];   263.90 (umol/hg) / 0.851 (cm2/mg) * (10000cm2/m2) (h/3600s) (g/1000mg) = 0.8614
    ! SGRASS(ng)%I_k                                  = [ 180d0    ]                                    ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve                    ! Calculated from rETR data from Liu et al. (2005), rETR to ETR conversion using Aquation (2020), ETR to carbon assimilation rate using Miyazawa and Yahata (2006), and a leaf bend angle of 10◦ from James et al. (2021) (refer to master's thesi Section 6.3.2b)
    ! SGRASS(ng)%P_max                                = [ 150.0d0  ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! tuning to fit Watanabe 2012 chamber
    ! SGRASS(ng)%P_max                                = [ 64.0d0  ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! tuning to fit Watanabe 2012 chamber
    SGRASS(ng)%P_max                                = [ 60.0d0  ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! tuning to fit Watanabe 2012 chamber
    ! SGRASS(ng)%I_k                                  = [ 1100.0d0  ]                                    ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve                    ! tuning to fit Watanabe 2012 chamber
    SGRASS(ng)%I_k                                  = [ 850.0d0  ]                                    ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve                    ! tuning to fit Watanabe 2012 chamber

    ! photoynthesis: DIC-limit
    ! SGRASS(ng)%Lf_DIC_uptake_max                    = [ 73.8d0   ]                                    ! [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves                                                                      ! tuning set equal to P_max
    ! SGRASS(ng)%Lf_DIC_uptake_max                    = [ 1.0696d0 ]                                    ! [umol.DIC g-1.lf.drywt s-1] Maximum uptake rate of DIC in the leaves                                                                      ! tuning set equal to P_max [73.8 umol/m2 s] / [0.069 kg/m2] [kg/1000g] = 1.0696 [ umol/g s]
    ! SGRASS(ng)%Lf_DIC_uptake_max                    = [ 5.0d0    ]                                    ! [umol.DIC g-1.lf.drywt s-1] Maximum uptake rate of DIC in the leaves                                                                      ! 
    SGRASS(ng)%Lf_DIC_uptake_max                    = [ 1.0d0    ]                                    ! [umol.DIC g-1.lf.drywt s-1] Maximum uptake rate of DIC in the leaves                                                                      ! tuning set equal to P_max [64 umol/m2 s] / [0.069 kg/m2] [kg/1000g] = 0.9275 [ umol/g s]
    SGRASS(ng)%k_lf_DIC                             = [ 4500d0   ]                                    ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth                                             ! 4500 µmol L-1 from Bjo ̈rk et al. (1997) (master's thesis section 6.3.2b)
    ! SGRASS(ng)%k_lf_DIC                             = [ 3000.0d0 ]                                    ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth                                               ! yt_edit Tuning
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! SGRASS(ng)%Rt_DIC_flux_max                      = [ 73.8d0/0.069d0 ]                              ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots                                                                       ! yt_edit Tuning
    SGRASS(ng)%Rt_DIC_flux_max                      = [ 0.0d0 ]                                                                                              ! yt_edit Tuning
    SGRASS(ng)%k_rt_DIC                             = [ 3000.0d0*10d0 ]                               ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth                                            ! yt_edit Tuning
# endif
    ! photoynthesis: nutrient-limit
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    ! SGRASS(ng)%Lf_NO3_uptake_max                    = [ 200.0d0/35.0d0  ]                             ! [umol.NO3 m-2.lf s-1]       Maximum uptake rate of NO3 in the leaves                                                                      ! yt_edit tuning  set equal to max photosynthesis
    ! SGRASS(ng)%Lf_NO3_uptake_max                    = [ 73.8d0/35d0 ]                                 ! [umol.NO3 m-2.lf s-1]       Maximum uptake rate of NO3 in the leaves                                                                      ! yt_edit tuning  set equal to max photosynthesis
    ! SGRASS(ng)%k_lf_NO3                             = [ 0.52d0   ]                                    ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth                                               ! Loosely calculate from Ow et al. (2016) (master's thesis Section 6.3.5b)
    ! SGRASS(ng)%smin_lf_NO3                          = [ 0.0d0    ]                                    ! [umol.NO3 L-1]              Minimum consentration of NO3 in water column for uptake                                               
    ! SGRASS(ng)%Lf_NH4_uptake_max                    = [ 73.8d0/35d0 ]                                 ! [umol.NH4 m-2.lf s-1]       Maximum uptake rate of NH4 in the leaves                                                                      ! yt_edit tuning  set equal to max photosynthesis
    ! SGRASS(ng)%k_lf_NH4                             = [ 36.3d0   ]                                    ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth                                               ! Stapel et al. (1996) (master's thesis Section 6.3.5b)
    ! SGRASS(ng)%smin_lf_NH4                          = [ 0.0d0    ]                                    ! [umol.NH4 L-1]              Minimum consentration of NH4 in water column for uptake                                               
    ! SGRASS(ng)%Lf_PO4_uptake_max                    = [ 73.8d0/545d0 ]                                ! [umol.PO4 m-2.lf s-1]       Maximum uptake rate of PO4 in the leaves                                                                      ! yt_edit tuning  set equal to max photosynthesis
    ! SGRASS(ng)%k_lf_PO4                             = [ 11.2d0   ]                                    ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth                                               ! Stapel et al. (1996) (master's thesis Section 6.3.5b)
    ! SGRASS(ng)%smin_lf_PO4                          = [ 0.0d0    ]                                    ! [umol.PO4 L-1]              Minimum consentration of PO4 in water column for uptake                                               

    SGRASS(ng)%Lf_NO3_uptake_max                    = [ 14.6d0/3600d0 ]                               ! [umol.NO3 g-1.lf.drywt s-1] Maximum uptake rate of NO3 in the leaves                                                                      ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. A)
    SGRASS(ng)%k_lf_NO3                             = [ 2.39d0   ]                                    ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth                                               ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. A)
    ! SGRASS(ng)%Lf_NO3_uptake_max                    = [ 77.7d0/3600d0 ]                               ! [umol.NO3 g-1.lf.drywt s-1] Maximum uptake rate of NO3 in the leaves                                                                      ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. C)
    ! SGRASS(ng)%k_lf_NO3                             = [ 24.2d0   ]                                    ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth                                               ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. C)
    ! SGRASS(ng)%smin_lf_NO3                          = [ 0.58d0   ]                                    ! [umol.NO3 L-1]              Minimum consentration of NO3 in water column for uptake                                                       ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. A)
    SGRASS(ng)%smin_lf_NO3                          = [ 0.0d0   ]                                    ! [umol.NO3 L-1]              Minimum consentration of NO3 in water column for uptake                                                       ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. A)
    SGRASS(ng)%Lf_NO3_ratio_slope                   = [ 1.85d0   ]                                    ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten                                             ! Yuta 2025 seagrass experiment - Thalassia hemprichii
    SGRASS(ng)%Lf_NH4_uptake_max                    = [ 13.3d0/3600d0 ]                               ! [umol.NH4 g-1.lf.drywt s-1] Maximum uptake rate of NH4 in the leaves                                                                      ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. A)
    SGRASS(ng)%k_lf_NH4                             = [ 2.17d0   ]                                    ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth                                               ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. A)
    ! SGRASS(ng)%Lf_NH4_uptake_max                    = [ 282d0/3600d0  ]                               ! [umol.NH4 g-1.lf.drywt s-1] Maximum uptake rate of NH4 in the leaves                                                                      ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. C)
    ! SGRASS(ng)%k_lf_NH4                             = [ 52.1d0   ]                                    ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth                                               ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. C)
    SGRASS(ng)%smin_lf_NH4                          = [ 0.39d0   ]                                    ! [umol.NH4 L-1]              Minimum consentration of NH4 in water column for uptake                                                       ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. A)
    SGRASS(ng)%Lf_NH4_ratio_slope                   = [ 1.49d0   ]                                    ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten                                             ! Yuta 2025 seagrass experiment - Thalassia hemprichii
    SGRASS(ng)%Lf_PO4_uptake_max                    = [ 1.50d0/3600d0 ]                               ! [umol.PO4 g-1.lf.drywt s-1] Maximum uptake rate of PO4 in the leaves                                                                      ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. A)
    SGRASS(ng)%k_lf_PO4                             = [ 0.36d0   ]                                    ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth                                               ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. A)
    ! SGRASS(ng)%Lf_PO4_uptake_max                    = [ 31.7d0/3600d0 ]                               ! [umol.PO4 g-1.lf.drywt s-1] Maximum uptake rate of PO4 in the leaves                                                                      ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. C)
    ! SGRASS(ng)%k_lf_PO4                             = [ 13.1d0   ]                                    ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth                                               ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. C)
    SGRASS(ng)%smin_lf_PO4                          = [ 0.02d0   ]                                    ! [umol.PO4 L-1]              Minimum consentration of PO4 in water column for uptake                                                       ! Yuta 2025 seagrass experiment - Thalassia hemprichii (ser. A)

    ! SGRASS(ng)%Lf_NO3_uptake_max                    = [ 73.5d0/3600d0 ]                               ! [umol.NO3 g-1.lf.drywt s-1] Maximum uptake rate of NO3 in the leaves                                                                      ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. A)
    ! SGRASS(ng)%k_lf_NO3                             = [ 7.07d0   ]                                    ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth                                               ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. A)
    ! ! SGRASS(ng)%Lf_NO3_uptake_max                    = [ 92.8d0/3600d0 ]                               ! [umol.NO3 g-1.lf.drywt s-1] Maximum uptake rate of NO3 in the leaves                                                                      ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. C)
    ! ! SGRASS(ng)%k_lf_NO3                             = [ 12.7d0   ]                                    ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth                                               ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. C)
    ! SGRASS(ng)%smin_lf_NO3                          = [ 0.78d0   ]                                    ! [umol.NO3 L-1]              Minimum consentration of NO3 in water column for uptake                                                       ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. A)
    ! SGRASS(ng)%Lf_NO3_ratio_slope                   = [ 1.63d0   ]                                    ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten                                             ! Yuta 2025 seagrass experiment - Halodule uninervis
    ! SGRASS(ng)%Lf_NH4_uptake_max                    = [ 46.5d0/3600d0 ]                               ! [umol.NH4 g-1.lf.drywt s-1] Maximum uptake rate of NH4 in the leaves                                                                      ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. A)
    ! SGRASS(ng)%k_lf_NH4                             = [ 3.53d0   ]                                    ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth                                               ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. A)
    ! ! SGRASS(ng)%Lf_NH4_uptake_max                    = [ 505d0/3600d0  ]                               ! [umol.NH4 g-1.lf.drywt s-1] Maximum uptake rate of NH4 in the leaves                                                                      ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. C)
    ! ! SGRASS(ng)%k_lf_NH4                             = [ 45.2d0   ]                                    ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth                                               ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. C)
    ! SGRASS(ng)%smin_lf_NH4                          = [ 0.46d0   ]                                    ! [umol.NH4 L-1]              Minimum consentration of NH4 in water column for uptake                                                       ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. A)
    ! SGRASS(ng)%Lf_NH4_ratio_slope                   = [ 2.25d0   ]                                    ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten                                             ! Yuta 2025 seagrass experiment - Halodule uninervis
    ! SGRASS(ng)%Lf_PO4_uptake_max                    = [ 7.42d0/3600d0 ]                               ! [umol.PO4 g-1.lf.drywt s-1] Maximum uptake rate of PO4 in the leaves                                                                      ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. A)
    ! SGRASS(ng)%k_lf_PO4                             = [ 0.92d0   ]                                    ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth                                               ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. A)
    ! ! SGRASS(ng)%Lf_PO4_uptake_max                    = [ 39.9d0/3600d0 ]                               ! [umol.PO4 g-1.lf.drywt s-1] Maximum uptake rate of PO4 in the leaves                                                                      ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. C)
    ! ! SGRASS(ng)%k_lf_PO4                             = [ 7.47d0   ]                                    ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth                                               ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. C)
    ! SGRASS(ng)%smin_lf_PO4                          = [ 0.02d0   ]                                    ! [umol.PO4 L-1]              Minimum consentration of PO4 in water column for uptake                                                       ! Yuta 2025 seagrass experiment - Halodule uninervis (ser. A)

# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! SGRASS(ng)%Rt_NO3_flux_max                      = [ 73.8d0/35d0/0.069d0 ]                         ! [umol.NO3 g-1.rt.drywt s-1] Maximum uptake rate of NO3 in the roots                                                                       ! yt_edit tuning, same as leaves
    ! SGRASS(ng)%k_rt_NO3                             = [ 0.52d0*10d0 ]                                 ! [umol.NO3 L-1]              Half saturation consentration of NO3 in soil pore water for growth                                            ! yt_edit tuning, ten times higher than leaves
    ! SGRASS(ng)%smin_rt_NO3                          = [ 0.0d0    ]                                    ! [umol.NO3 L-1]              Minimum consentration of NO3 in soil pore water for uptake                                            
    ! SGRASS(ng)%Rt_NH4_flux_max                      = [ 73.8d0/35d0/0.069d0 ]                         ! [umol.NH4 g-1.rt.drywt s-1] Maximum uptake rate of NH4 in the roots                                                                       ! yt_edit tuning, same as leaves
    ! SGRASS(ng)%k_rt_NH4                             = [ 36.3d0*10d0 ]                                 ! [umol.NH4 L-1]              Half saturation consentration of NH4 in soil pore water for growth                                            ! yt_edit tuning, ten times higher than leaves
    ! SGRASS(ng)%smin_rt_NH4                          = [ 0.0d0    ]                                    ! [umol.NH4 L-1]              Minimum consentration of NH4 in soil pore water for uptake                                            
    ! SGRASS(ng)%Rt_PO4_flux_max                      = [ 73.8/545d0/0.069d0  ]                         ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots                                                                       ! yt_edit tuning, same as leaves
    ! SGRASS(ng)%k_rt_PO4                             = [ 11.2d0*10d0 ]                                 ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth                                            ! yt_edit tuning, ten times higher than leaves
    ! SGRASS(ng)%smin_rt_PO4                          = [ 0.0d0    ]                                    ! [umol.PO4 L-1]              Minimum consentration of PO4 in soil pore water for uptake                                            

    SGRASS(ng)%Rt_NO3_flux_max                      = [ 6581.0d0/3600d0 ]                               ! [umol.NO3 g-1.rt.drywt s-1] Maximum uptake rate of NO3 in the roots                                                                     ! Yuta 2025 seagrass experiment - Thalassia hemprichii
    SGRASS(ng)%k_rt_NO3                             = [ 34417d0  ]                                    ! [umol.NO3 L-1]              Half saturation consentration of NO3 in soil pore water for growth                                            ! Yuta 2025 seagrass experiment - Thalassia hemprichii
    SGRASS(ng)%smin_rt_NO3                          = [ 0.0d0    ]                                    ! [umol.NO3 L-1]              Minimum consentration of NO3 in soil pore water for uptake                                                    ! Yuta 2025 seagrass experiment - Thalassia hemprichii
    SGRASS(ng)%Rt_NO3_ratio_slope                   = [ 1.91d0   ]                                    ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten                                             ! Yuta 2025 seagrass experiment - Thalassia hemprichii
    SGRASS(ng)%Rt_NH4_flux_max                      = [ 43.3d0/3600d0 ]                               ! [umol.NH4 g-1.rt.drywt s-1] Maximum uptake rate of NH4 in the roots                                                                       ! Yuta 2025 seagrass experiment - Thalassia hemprichii
    SGRASS(ng)%k_rt_NH4                             = [ 21.5d0 ]                                      ! [umol.NH4 L-1]              Half saturation consentration of NH4 in soil pore water for growth                                            ! Yuta 2025 seagrass experiment - Thalassia hemprichii
    SGRASS(ng)%smin_rt_NH4                          = [ 0.0d0    ]                                    ! [umol.NH4 L-1]              Minimum consentration of NH4 in soil pore water for uptake                                                    ! Yuta 2025 seagrass experiment - Thalassia hemprichii
    SGRASS(ng)%Rt_NH4_ratio_slope                   = [ 2.27d0   ]                                    ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten                                             ! Yuta 2025 seagrass experiment - Thalassia hemprichii
    SGRASS(ng)%Rt_PO4_flux_max                      = [ 3.32d0/3600d0  ]                              ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots                                                                       ! Yuta 2025 seagrass experiment - Thalassia hemprichii
    SGRASS(ng)%k_rt_PO4                             = [ 1.45d0   ]                                    ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth                                            ! Yuta 2025 seagrass experiment - Thalassia hemprichii
    SGRASS(ng)%smin_rt_PO4                          = [ 0.0d0    ]                                    ! [umol.PO4 L-1]              Minimum consentration of PO4 in soil pore water for uptake                                                    ! Yuta 2025 seagrass experiment - Thalassia hemprichii

    ! SGRASS(ng)%Rt_NO3_flux_max                      = [ 0.0d0/3600d0  ]                               ! [umol.NO3 g-1.rt.drywt s-1] Maximum uptake rate of NO3 in the roots                                                                       ! Yuta 2025 seagrass experiment - Halodule uninervis
    ! SGRASS(ng)%k_rt_NO3                             = [ 0.0d0    ]                                    ! [umol.NO3 L-1]              Half saturation consentration of NO3 in soil pore water for growth                                            ! Yuta 2025 seagrass experiment - Halodule uninervis
    ! SGRASS(ng)%smin_rt_NO3                          = [ 0.0d0    ]                                    ! [umol.NO3 L-1]              Minimum consentration of NO3 in soil pore water for uptake                                                    ! Yuta 2025 seagrass experiment - Halodule uninervis
    ! SGRASS(ng)%Rt_NO3_ratio_slope                   = [ 1.63d0   ]                                    ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten                                             ! Yuta 2025 seagrass experiment - Halodule uninervis
    ! SGRASS(ng)%Rt_NH4_flux_max                      = [ 49.7d0/3600d0 ]                               ! [umol.NH4 g-1.rt.drywt s-1] Maximum uptake rate of NH4 in the roots                                                                       ! Yuta 2025 seagrass experiment - Halodule uninervis
    ! SGRASS(ng)%k_rt_NH4                             = [ 12.3d0 ]                                      ! [umol.NH4 L-1]              Half saturation consentration of NH4 in soil pore water for growth                                            ! Yuta 2025 seagrass experiment - Halodule uninervis
    ! SGRASS(ng)%smin_rt_NH4                          = [ 0.0d0    ]                                    ! [umol.NH4 L-1]              Minimum consentration of NH4 in soil pore water for uptake                                                    ! Yuta 2025 seagrass experiment - Halodule uninervis
    ! SGRASS(ng)%Rt_NH4_ratio_slope                   = [ 2.25d0   ]                                    ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten                                             ! Yuta 2025 seagrass experiment - Halodule uninervis
    ! SGRASS(ng)%Rt_PO4_flux_max                      = [ 3.92d0/3600d0  ]                              ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots                                                                       ! Yuta 2025 seagrass experiment - Halodule uninervis
    ! SGRASS(ng)%k_rt_PO4                             = [ 0.54d0   ]                                    ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth                                            ! Yuta 2025 seagrass experiment - Halodule uninervis
    ! SGRASS(ng)%smin_rt_PO4                          = [ 0.0d0    ]                                    ! [umol.PO4 L-1]              Minimum consentration of PO4 in soil pore water for uptake                                                    ! Yuta 2025 seagrass experiment - Halodule uninervis

# endif
    ! respiration: DO-limit
    ! SGRASS(ng)%Lf_DO_uptake_max                     = [ 2.075d0  ]                                  ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves                                                                       ! 2.075 umol.C/m2.leaf/s calculated from Rasmusson et al. (2020) (refer to master's thesis 6.3.3a)
    ! SGRASS(ng)%Lf_DO_uptake_max                     = [ 40d0     ]                                    ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves                                                                       ! tuning
    SGRASS(ng)%Lf_DO_uptake_max                     = [ 0.5797d0 ]                                    ! [umol.DO g-1.lf.drywt s-1] Maximum uptake rate of DO in the leaves                                                                       ! tuning  [40 umol/m2 s] / [0.069 kg/m2] [kg/1000g] = 0.5797 [ umol/g s]
    SGRASS(ng)%k_lf_DO                              = [ 43d0     ]                                    ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration                                           ! 43 umol.O2/L Loosely calculated from Rasmusson et al. (2020) (Section 6.3.3b)
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! SGRASS(ng)%Rt_DO_flux_max                       = [ 40d0/0.069d0 ]                                ! [umol.DO g-1.rt.drywt s-1]  Maximum uptake rate of DO in the roots                                                                        ! yt_edit tuning, same as leaves
    ! SGRASS(ng)%k_rt_DO                              = [ 43d0*10d0 ]                                   ! [umol.DO L-1]               Half saturation consentration of DO in soil pore water for growth                                             ! yt_edit tuning, same as leaves
    SGRASS(ng)%Rt_DO_flux_max                       = [ 0.5797d0 ]                                    ! [umol.DO g-1.rt.drywt s-1]  Maximum uptake rate of DO in the roots                                                                        ! yt_edit tuning, same as leaves
    SGRASS(ng)%k_rt_DO                              = [ 43d0     ]                                    ! [umol.DO L-1]               Half saturation consentration of DO in soil pore water for growth                                             ! yt_edit tuning, same as leaves
# endif
    ! respiration: required
    ! SGRASS(ng)%R_basal                              = [ 2.5265d0 ]                                    ! [umol.C m-2.leaf s-1]       Maximum respiration rate                                                                                      ! tuning: half-life in complete darkness is ~7.0 days.
    ! SGRASS(ng)%R_basal                              = [ 0.0366d0 ]                                    ! [umol.C g-1.leaf s-1]       Maximum respiration rate                                                                                      ! tuning: half-life in complete darkness is ~7.0 days. [2.5265 umol/m2 s] / [0.069 kg/m2] [kg/1000g] = 0.0366 [ umol/g s]
    ! SGRASS(ng)%R_basal                              = [ 0.0111d0 ]                                    ! [umol.C g-1.leaf s-1]       Maximum respiration rate                                                                                      ! 40 umol O2 h-1 g-1 yuta 2025 nutrient experiment T. hemprichii leaf dark respiration   40 (umol/hg) * h/3600s = 0.0111 [umol.C g-1.leaf s-1]
    ! SGRASS(ng)%R_basal                              = [ 0.0180d0 ]                                    ! [umol.C g-1.leaf s-1]       Maximum respiration rate                                                                                      ! 65 umol O2 h-1 g-1 yuta 2025 nutrient experiment H. uninervis leaf dark respiration   65 (umol/hg) * h/3600s = 0.0180 [umol.C g-1.leaf s-1]
    ! SGRASS(ng)%R_basal                              = [ 0.0178d0 ]                                    ! [umol.C g-1.leaf s-1]       Maximum respiration rate                                                                                      ! 64 umol O2 h-1 g-1 yuta 2025 nutrient experiment T. hemprichii sand root respiration   64 (umol/hg) * h/3600s = 0.0178 [umol.C g-1.leaf s-1]
    ! SGRASS(ng)%R_basal                              = [ 0.0045d0 ]                                    ! [umol.C g-1.leaf s-1]       Maximum respiration rate                                                                                      ! 16.14 umol O2 h-1 g-1 yuta 2025 nutrient experiment T. hemprichii coral root respiration   16.14 (umol/hg) * h/3600s = 0.0045 [umol.C g-1.leaf s-1]
    ! SGRASS(ng)%R_basal                              = [ 0.0121d0 ]                                    ! [umol.C g-1.leaf s-1]       Maximum respiration rate                                                                                      ! 43 umol O2 h-1 g-1 yuta 2025 nutrient experiment H. uninervis root respiration   43 (umol/hg) * h/3600s = 0.0121 [umol.C g-1.leaf s-1]
    ! SGRASS(ng)%R_basal                              = [ 0.05d0   ]                                    ! [umol.C g-1.leaf s-1]       Maximum respiration rate                                                                                      ! tuning to match Watanabe 2012 chamber
    SGRASS(ng)%R_basal                              = [ 0.04d0   ]                                    ! [umol.C g-1.leaf s-1]       Maximum respiration rate                                                                                      ! tuning to match Watanabe 2012 chamber
    ! SGRASS(ng)%Turnover_rate                        = [0.01d0    ]                                    ! [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day
    ! SGRASS(ng)%Turnover_rate                        = [0.00002d0     ]                                    ! [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day                                                  ! tuning
    SGRASS(ng)%Turnover_rate                        = [0.006d0     ]                                    ! [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day                                                  ! tuning
    ! dieoff when respiration is insufficient
    SGRASS(ng)%max_anoxic_dieoff_rate               = [ 0.5d0    ]                                    ! [mol.C mol-1.C day-1]       maximum dieoff due to anoxia in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates ! tuning
    ! stockpile maximum cap
    SGRASS(ng)%DICstockCap                          = [0.01d0]                                        ! Factor to multiply carbon biomsass to get maximum DIC stock (stock limit = DICstockCap*SgCBmF) 
    SGRASS(ng)%CH2OstockCap                         = [1.0d0]                                         ! Factor to multiply carbon biomsass to get maximum sugar stock (stock limit = CH2OstockCap*SgCBmF) 
    SGRASS(ng)%DOstockCap                           = [0.01d0]                                        ! Factor to multiply carbon biomsass to get maximum DO stock (stock limit = DOstockCap*SgCBmF) 
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
#  if defined SEAGRASS_IMMEDIATE_NO3_TO_NH4_CONVERSION
    SGRASS(ng)%NO3stockCap                          = [0.0d0]                                         ! Factor to multiply nitrogen biomsass to get maximum NO3 stock (stock limit = NO3stockCap*SgNBmF) 
# else
    SGRASS(ng)%NO3stockCap                          = [1.0d0]                                         ! Factor to multiply nitrogen biomsass to get maximum NO3 stock (stock limit = NO3stockCap*SgNBmF) 
# endif
    SGRASS(ng)%NH4stockCap                          = [1.0d0]                                         ! Factor to multiply nitrogen biomsass to get maximum NH4 stock (stock limit = NH4stockCap*SgNBmF) 
    SGRASS(ng)%PO4stockCap                          = [1.0d0]                                         ! Factor to multiply phosphorus biomsass to get maximum PO4 stock (stock limit = PO4stockCap*SgPBmF) 
# endif
    ! Growth interval
    SGRASS(ng)%trans_rate                           = [ 0.05d0   ]                                    ! [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day                                         ! yt_edit tuning: translocation 10% per day
    SGRASS(ng)%opt_sg_bed_density                   = [ 500.0d0  ]                                    ! [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)                                         ! yt_edit tuning
    ! write(*,*) 'yt_debug: mod_seagrass.F initialize_seagrass() after constant assigments'

! Set grid-wise initial conditions
    do i=LBi,UBi
      do j=LBj,UBj
        ! grid areas
        SGRASS(ng)%GridArea(i,j) = XI_size(i,j) * ETA_size(i,j)
        if (isnan(SGRASS(ng)%GridArea(i,j)) .or. abs(SGRASS(ng)%GridArea(i,j)) > huge(SGRASS(ng)%GridArea(i,j))) then
          SGRASS(ng)%GridArea(i,j) = 0.0d0
        endif

        SGRASS(ng)%Day(i,j) = -1  ! start at negative so first day triggers calculation
      enddo
    enddo
    ! write(*,*) 'yt_debug: mod_seagrass.F initialize_seagrass() after grid_area assigment'

! Set species specific initial conditions
    do species=1,Nsg
      ! SgNperC                     [mol.N mol-1.C]             Species specific seagrass Nitrogen to Carbon ratio
      ! C_N_P_ratios(chem,sp)                                   C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]    
      SGRASS(ng)%SgNperC(species) = SGRASS(ng)%C_N_P_ratios(2, species) / SGRASS(ng)%C_N_P_ratios(1, species)

      ! SgPperC                     [mol.P mol-1.C]             Species specific seagrass Phosphorus to Carbon ratio
      ! C_N_P_ratios(chem,sp)                                   C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]    
      SGRASS(ng)%SgPperC(species) = SGRASS(ng)%C_N_P_ratios(3, species) / SGRASS(ng)%C_N_P_ratios(1, species)

      ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
      ! C_N_P_ratios(chem,sp)                                   C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]    
      ! (CH2O)x(NH3)y(H3PO4)z / x
      SGRASS(ng)%SgMperC(species) = (                                                                                             &
        ! H = (1.0078) (x*2 + y*3 +z*3)            
        1.0078d0*(2*SGRASS(ng)%C_N_P_ratios(1,species)+3*SGRASS(ng)%C_N_P_ratios(2,species)+3*SGRASS(ng)%C_N_P_ratios(3,species))+&
        ! C = (12.011) (x)
        12.011d0*(  SGRASS(ng)%C_N_P_ratios(1,species)                                                                          )+&
        ! N = (14.007) (y)
        14.007d0*(                                       SGRASS(ng)%C_N_P_ratios(2,species)                                     )+&
        ! O = (15.999) (x + z*4)
        15.999d0*(  SGRASS(ng)%C_N_P_ratios(1,species)                                     +4*SGRASS(ng)%C_N_P_ratios(3,species))+&
        ! P = (30.974) (z)
        30.974d0*(                                                                            SGRASS(ng)%C_N_P_ratios(3,species)) &
        ! (sum of HCNOP) / x
        ) / SGRASS(ng)%C_N_P_ratios(1,species)
    enddo
    ! write(*,*) 'yt_debug: mod_seagrass.F initialize_seagrass() after CNP ratio assigments'


    ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
    ! GridArea                    [m2.grid]                   Total ground area in grid cell
    ! p_sgrass                    [m2.sg.hab m-2.grid](0~1)   Seagrass coverage
    do species=1,Nsg
      do i=LBi,UBi
        do j=LBj,UBj
          ! [m2.sg.hab]                   = [m2.grid]                [m2.sg.hab m-2.grid]
          SGRASS(ng)%HabArea(species,i,j) = SGRASS(ng)%GridArea(i,j) * p_sgrass(species,i,j)
        enddo
      enddo
    enddo


! analytical start
    if( start_of_new_run ) then
      do species=1,Nsg
        do i=LBi,UBi
          do j=LBj,UBj

            ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
            ! GridArea                    [m2.grid]                   Total ground area in grid cell
            ! p_sgrass                    [m2.sg.hab m-2.grid](0~1)   Seagrass coverage
            ! LAI                         [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area 
            ! ELAP                        [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
            ! k_proj                      [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient
            ! rho_lf                      [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
            ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
            ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
            ! SgNBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root nitrogen-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
            ! SgPBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root phosphorus-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
            ! DICstock                    [umol.DIC m-2.sg.hab]       Seagrass internal DIC stock per unit ground area (in seagrass habitat area)
            ! CH2Ostock                   [umol.CH2O m-2.sg.hab]      Seagrass internal sugar stock per unit ground area (in seagrass habitat area)
            ! DOstock                     [umol.DO m-2.sg.hab]        Seagrass internal DO stock per unit ground area (in seagrass habitat area)
            ! NO3stock                    [umol.NO3 m-2.sg.hab]       Seagrass internal NO3 stock per unit ground area (in seagrass habitat area)
            ! NH4stock                    [umol.NH4 m-2.sg.hab]       Seagrass internal NH4 stock per unit ground area (in seagrass habitat area)
            ! PO4stock                    [umol.PO4 m-2.sg.hab]       Seagrass internal PO4 stock per unit ground area (in seagrass habitat area)
            ! TotSgCBmF                   [mol.C.tot]                 Total seagrass carbon-biomass in grid cell. updates on fine time interval
            ! LfCBm                       [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
            ! RtCBm                       [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
            ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
            ! TotLfCBm                    [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
            ! TotRtCBm                    [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
            ! f_above                     [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction  
            ! TotSgDryBm                  [g.drywt]                   Total seagrass dry weight biomass in grid cell
            ! TotLfDryBm                  [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
            ! TotRtDryBm                  [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
            ! SgDryBm                     [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
            ! LfDryBm                     [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
            ! RtDryBm                     [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
            
            ! old >>>>>>>>>>>>>>>>>>>
            ! ! SGRASS(ng)%LAI(species,i,j) = (-1*log(1.0-0.85)/SGRASS(ng)%k_proj(species))
            ! ! SGRASS(ng)%LAI(species,i,j) = (-1*log(1.0d0-0.30433574d0)/SGRASS(ng)%k_proj(species))
            ! SGRASS(ng)%LAI(species,i,j) = 0.001d0

            ! ! [umol.C/m2.hab]              
            ! SGRASS(ng)%LfCBm(species,i,j)  &
            ! ! = [m2.lf/m2.hab]              [kg.drywt/m2.lf]             [mol.C/g.drywt]                 (1000.g.drywt/kg.drywt) (1000000umol/mol)
            !   = SGRASS(ng)%LAI(species,i,j) * SGRASS(ng)%rho_lf(species) / SGRASS(ng)%SgMperC(species) * 1000000000d0

            ! SGRASS(ng)%SgCBmF(  :,species,i,j) = 0.0d0
            ! ! [umol.C.tot/m2.hab]              = [umol.C.lf/m2.hab]             [mol.C.tot/mol.C.lf]
            ! SGRASS(ng)%SgCBmF(iCt,species,i,j) = SGRASS(ng)%LfCBm(species,i,j) / SGRASS(ng)%f_above(species)
            ! <<<<<<<<<<<<<<<<<<<

            ! new based on chamber 2012 >>>>>>>>>>>>>>>>>>>
            ! [g.drywt/m2.hab]              = g.drywt / m2.chamber
            SGRASS(ng)%LfDryBm(species,i,j) = 46.8d0  / 0.45d0 ! from Watanabe chamber experiments 2012

            ! [g.drywt/m2.hab]              = g.drywt / m2.chamber
            SGRASS(ng)%RtDryBm(species,i,j) = 86.5d0  / 0.45d0 ! from Watanabe chamber experiments 2012

            ! SGRASS(ng)%LfDryBm(species,i,j) = 0.0d0  ! no seagrass test
            ! SGRASS(ng)%RtDryBm(species,i,j) = 0.0d0  ! no seagrass test

            ! [g.drywt/m2.hab]
            SGRASS(ng)%SgDryBm(species,i,j) = SGRASS(ng)%LfDryBm(species,i,j) + SGRASS(ng)%RtDryBm(species,i,j)

            ! [umol.C/m2.hab]                 = [g.drywt/m2.hab]                / [g.drywt/mol.C]             * 1000000umol.C/mol.C
            SGRASS(ng)%LfCBm(species,i,j) = SGRASS(ng)%LfDryBm(species,i,j) / SGRASS(ng)%SgMperC(species) * 1000000.0d0

            ! [umol.C/m2.hab]                 = [g.drywt/m2.hab]                / [g.drywt/mol.C]             * 1000000umol.C/mol.C
            SGRASS(ng)%RtCBm(species,i,j) = SGRASS(ng)%RtDryBm(species,i,j) / SGRASS(ng)%SgMperC(species) * 1000000.0d0

            ! [umol.C/m2.hab]                  = [g.drywt/m2.hab]                / [g.drywt/mol.C]             * 1000000umol.C/mol.C
            SGRASS(ng)%SgCBmF(iCt,species,i,j) = SGRASS(ng)%SgDryBm(species,i,j) / SGRASS(ng)%SgMperC(species) * 1000000.0d0

            ! [m2.lf/m2.hab]            = [g.drywt/m2.hab]                / [kg.drywt/m2.lf]           * (kg.drywt/1000g.drywt)
            SGRASS(ng)%LAI(species,i,j) = SGRASS(ng)%LfDryBm(species,i,j) / SGRASS(ng)%rho_lf(species) * 0.001d0
            ! <<<<<<<<<<<<<<<<<<<

        
# if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
            SGRASS(ng)%SgCBmF(iC13,species,i,j) = Ci_from_Ct_delta( SGRASS(ng)%SgCBmF(iCt,species,i,j), -20.0d0, R13C_VPDB )
#  if defined CLUMPED_ISOTOPE
            SGRASS(ng)%SgCBmF(iD47,species,i,j) = Ci_from_Ct_delta( SGRASS(ng)%SgCBmF(iCt,species,i,j), -20.0d0, R13C_VPDB ) !!???????
#  endif
# endif
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
            SGRASS(ng)%SgNBmF(  :,species,i,j) = 0.0d0
            ! [umol.N.tot/m2.hab]              
            SGRASS(ng)%SgNBmF(iNt,species,i,j) = SGRASS(ng)%SgCBmF(iCt,species,i,j) * SGRASS(ng)%SgNperC(species)
#  if defined NITROGEN_ISOTOPE
            SGRASS(ng)%SgNBmF(iN15,species,i,j) = Ci_from_Ct_delta( SGRASS(ng)%SgNBmF(iNt,species,i,j), -20.0d0, R13C_VPDB )
#  endif
            SGRASS(ng)%SgPBmF(  :,species,i,j) = 0.0d0
            ! [umol.P.tot/m2.hab]              
            SGRASS(ng)%SgPBmF(iPt,species,i,j) = SGRASS(ng)%SgCBmF(iCt,species,i,j) * SGRASS(ng)%SgPperC(species)
# endif
            ! ! [umol.C.rt/m2.hab]          = [umol.C.tot/m2.hab]                [mol.C.rt/mol.C.tot]
            ! SGRASS(ng)%RtCBm(species,i,j) = SGRASS(ng)%SgCBmF(iCt,species,i,j) * (1.0d0 - SGRASS(ng)%f_above(species))

            ! [mol.C]                        = [umol.C/m2.hab]                    [m2.hab]                          (mol/1000000umol)
            SGRASS(ng)%TotSgCBm(species,i,j) = SGRASS(ng)%SgCBmF(iCt,species,i,j) * SGRASS(ng)%HabArea(species,i,j) * 0.000001d0

            ! [mol.C]                        = [umol.C/m2.hab]               [m2.hab]                          (mol/1000000umol)
            SGRASS(ng)%TotLfCBm(species,i,j) = SGRASS(ng)%LfCBm(species,i,j) * SGRASS(ng)%HabArea(species,i,j) * 0.000001d0

            ! [mol.C]                        = [umol.C/m2.hab]               [m2.hab]                          (mol/1000000umol)
            SGRASS(ng)%TotRtCBm(species,i,j) = SGRASS(ng)%RtCBm(species,i,j) * SGRASS(ng)%HabArea(species,i,j) * 0.000001d0
            
            ! [g.drywt]                        = [mol.C]                          [g.drywt/mol.C]
            SGRASS(ng)%TotSgDryBm(species,i,j) = SGRASS(ng)%TotSgCBm(species,i,j) * SGRASS(ng)%SgMperC(species)

            ! [g.drywt]                        = [mol.C]                          [g.drywt/mol.C]
            SGRASS(ng)%TotLfDryBm(species,i,j) = SGRASS(ng)%TotLfCBm(species,i,j) * SGRASS(ng)%SgMperC(species)

            ! [g.drywt]                        = [mol.C]                          [g.drywt/mol.C]
            SGRASS(ng)%TotRtDryBm(species,i,j) = SGRASS(ng)%TotRtCBm(species,i,j) * SGRASS(ng)%SgMperC(species)

            ! ! [g.drywt/m2.hab]              = [umol.C/m2.hab]                    [g.drywt/mol.C]               (mol.C/1000000umol.C)
            ! SGRASS(ng)%SgDryBm(species,i,j) = SGRASS(ng)%SgCBmF(iCt,species,i,j) * SGRASS(ng)%SgMperC(species) * 0.000001d0

            ! ! [g.drywt/m2.hab]              = [umol.C/m2.hab]               [g.drywt/mol.C]               (mol.C/1000000umol.C)
            ! SGRASS(ng)%LfDryBm(species,i,j) = SGRASS(ng)%LfCBm(species,i,j) * SGRASS(ng)%SgMperC(species) * 0.000001d0

            ! ! [g.drywt/m2.hab]              = [umol.C/m2.hab]               [g.drywt/mol.C]               (mol.C/1000000umol.C)
            ! SGRASS(ng)%RtDryBm(species,i,j) = SGRASS(ng)%RtCBm(species,i,j) * SGRASS(ng)%SgMperC(species) * 0.000001d0

            SGRASS(ng)%TotSgCBmF(species,i,j) = SGRASS(ng)%TotSgCBm(species,i,j)

# if defined NET_ZERO_MASS_START
            ! Calculate initial seagrass mass balance 
            ! InitialC                    [mmol.C]                    Initial carbon remaining to destroy until net zero
            ! InitialN                    [mmol.N]                    Initial nitrogen remaining to destroy until net zero
            ! InitialP                    [mmol.P]                    Initial phosphorus remaining to destroy until net zero
            ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
            ! SgNperC                     [mol.N mol-1.C]             Species specific seagrass Nitrogen to Carbon ratio
            ! SgPperC                     [mol.P mol-1.C]             Species specific seagrass Phosphorus to Carbon ratio
            ! [mmol.X]                       = [mol.C]                          [mol.X/mol.C]                 [1000 mmol/mol]
            SGRASS(ng)%InitialC(species,i,j) = SGRASS(ng)%TotSgCBm(species,i,j)                               * 1000.0d0 ! yt_edit to_do need to add this to save/restart parameters?
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
            SGRASS(ng)%InitialN(species,i,j) = SGRASS(ng)%TotSgCBm(species,i,j) * SGRASS(ng)%SgNperC(species) * 1000.0d0
            SGRASS(ng)%InitialP(species,i,j) = SGRASS(ng)%TotSgCBm(species,i,j) * SGRASS(ng)%SgPperC(species) * 1000.0d0
#  endif
# endif

            SGRASS(ng)%DICstock(:,species,i,j) = 0.0d0
            SGRASS(ng)%CH2Ostock(:,species,i,j) = 0.0d0
            SGRASS(ng)%DOstock(species,i,j) = 0.0d0
# if defined FULL_STOCK_START
            SGRASS(ng)%DICstock(:,species,i,j) = SGRASS(ng)%DICstockCap(species)* SGRASS(ng)%SgCBmF(:,species,i,j)
            ! SGRASS(ng)%CH2Ostock(:,species,i,j) = SGRASS(ng)%CH2OstockCap(species)* SGRASS(ng)%SgCBmF(:,species,i,j)
            SGRASS(ng)%DOstock(species,i,j) = SGRASS(ng)%DOstockCap(species)* SGRASS(ng)%SgCBmF(iCt,species,i,j)
# elif defined CHAMBER_EQUILIBRIUM_STOCK_START
            ! SGRASS(ng)%DICstock(:,species,i,j)  &
            !         = SGRASS(ng)%DICstockCap(species)* SGRASS(ng)%SgCBmF(:,species,i,j)  * 0.998913020616880d0
            ! SGRASS(ng)%CH2Ostock(:,species,i,j) &
            !         = SGRASS(ng)%CH2OstockCap(species)* SGRASS(ng)%SgCBmF(:,species,i,j) * 1.076256593956242d-05
            ! SGRASS(ng)%DOstock(species,i,j)     &
            !         = SGRASS(ng)%DOstockCap(species)* SGRASS(ng)%SgCBmF(iCt,species,i,j) * 1.0d0
            ! SGRASS(ng)%DICstock(:,species,i,j)  &
            !         = SGRASS(ng)%DICstockCap(species)* SGRASS(ng)%SgCBmF(:,species,i,j)  * 0.998947817125829d0
            ! SGRASS(ng)%CH2Ostock(:,species,i,j) &
            !         = SGRASS(ng)%CH2OstockCap(species)* SGRASS(ng)%SgCBmF(:,species,i,j) * 1.041802767639336d-05
            SGRASS(ng)%DICstock(:,species,i,j)  &
                    = SGRASS(ng)%DICstockCap(species)* SGRASS(ng)%SgCBmF(:,species,i,j)  * 1.0d0
            SGRASS(ng)%CH2Ostock(:,species,i,j) &
                    = SGRASS(ng)%CH2OstockCap(species)* SGRASS(ng)%SgCBmF(:,species,i,j) * 0.0d0
            SGRASS(ng)%DOstock(species,i,j)     &
                    = SGRASS(ng)%DOstockCap(species)* SGRASS(ng)%SgCBmF(iCt,species,i,j) * 1.0d0
# endif
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
            SGRASS(ng)%NO3stock(:,species,i,j) = 0.0d0
            SGRASS(ng)%NH4stock(:,species,i,j) = 0.0d0
            SGRASS(ng)%PO4stock(:,species,i,j) = 0.0d0
#  if defined FULL_STOCK_START
            SGRASS(ng)%NO3stock(:,species,i,j) = SGRASS(ng)%NO3stockCap(species)* SGRASS(ng)%SgNBmF(:,species,i,j)
            SGRASS(ng)%NH4stock(:,species,i,j) = SGRASS(ng)%NH4stockCap(species)* SGRASS(ng)%SgNBmF(:,species,i,j)
            SGRASS(ng)%PO4stock(:,species,i,j) = SGRASS(ng)%PO4stockCap(species)* SGRASS(ng)%SgPBmF(:,species,i,j)
#  elif defined CHAMBER_EQUILIBRIUM_STOCK_START
            ! SGRASS(ng)%NO3stock(:,species,i,j) &
            !         = SGRASS(ng)%NO3stockCap(species)* SGRASS(ng)%SgNBmF(:,species,i,j) * 3.666207853999131d-06
            ! SGRASS(ng)%NH4stock(:,species,i,j) &
            !         = SGRASS(ng)%NH4stockCap(species)* SGRASS(ng)%SgNBmF(:,species,i,j) * 0.285079904658520d0
            ! SGRASS(ng)%PO4stock(:,species,i,j) &
            !         = SGRASS(ng)%PO4stockCap(species)* SGRASS(ng)%SgPBmF(:,species,i,j) * 0.996486838205624d0
            SGRASS(ng)%NO3stock(:,species,i,j) &
                    = SGRASS(ng)%NO3stockCap(species)* SGRASS(ng)%SgNBmF(:,species,i,j) * 0.0d0
            ! SGRASS(ng)%NH4stock(:,species,i,j) &
            !         = SGRASS(ng)%NH4stockCap(species)* SGRASS(ng)%SgNBmF(:,species,i,j) * 0.256607049362339d0
            ! SGRASS(ng)%PO4stock(:,species,i,j) &
            !         = SGRASS(ng)%PO4stockCap(species)* SGRASS(ng)%SgPBmF(:,species,i,j) * 0.315942197801809d0
            SGRASS(ng)%NH4stock(:,species,i,j) &
                    = SGRASS(ng)%NH4stockCap(species)* SGRASS(ng)%SgNBmF(:,species,i,j) * 0.5d0
            SGRASS(ng)%PO4stock(:,species,i,j) &
                    = SGRASS(ng)%PO4stockCap(species)* SGRASS(ng)%SgPBmF(:,species,i,j) * 0.5d0
#  endif
# endif

          enddo
        enddo
      enddo

      ! >>> Saved Values Only for Output  <<< !
      SGRASS(ng)%TotLA                    = 0d0 ! [m2.lf]                     Total one-sided green leaf area in grid
      SGRASS(ng)%GridELAP                 = 0d0 ! [m2.lf.proj m-2.grid]       Effective leaf area projection on ground (whole grid)
      SGRASS(ng)%GridPhot                 = 0d0 ! [umol.C m-2.grid s-1]       Seagrass gross photosynthesis rate per unit ground area (whole grid)
      SGRASS(ng)%GridGrow                 = 0d0 ! [umol.C m-2.grid s-1]       Seagrass growth rate per unit ground area (whole grid)
      SGRASS(ng)%PhotLim                  = 0   ! []                          Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon stock
      SGRASS(ng)%GrowLim                  = 0   ! []                          Seagrass growth limiting factor: 1 = Sugar stock; 2 = Nitrogen stock; 3 = Phosphorus stock
      SGRASS(ng)%GridResp                 = 0d0 ! [umol.C m-2.grid s-1]       Seagrass respiration rate per unit ground area (whole grid)
      SGRASS(ng)%GridNetPhot              = 0d0 ! [umol.C m-2.grid s-1]       Seagrass net photosynthesis rate per unit ground area (whole grid)
      SGRASS(ng)%GridDieoff               = 0d0 ! [umol.C m-2.grid s-1]       Seagrass dieoff rate carbon biomass per unit ground area (whole grid)


      ! write(*,*) 'yt_debug: mod_seagrass.F initialize_seagrass() after analytical start mass assigments'

    endif

    ! write(*,*) 'yt_debug: mod_seagrass.F initialize_seagrass() root_mass_profile'
    ! do k=1,Nsed
    !   write(*,*) 'yt_debug: k=', k, 'root_mass_profile =', SGRASS(ng)%root_mass_profile(1, k)
    ! enddo
    ! write(*,*) 'yt_debug: k=sum of all  root_mass_profile =', sum(SGRASS(ng)%root_mass_profile(1, :))

    RETURN
    
  END SUBROUTINE initialize_seagrass



!!! ==================================================================================================
!!! **************************************************************************************************
!!!                                MAIN LOOP OF SEAGRASS MODEL
!!! **************************************************************************************************
!!! ==================================================================================================

! ====================================================================================================
! Seagrass main subroutine in/out interface
! ====================================================================================================
  SUBROUTINE seagrass             &
!   input parameters
    ( ng                          &   !                           nested grid number; 
    , species                     &   !                           seagrass species id
    , i, j                        &   !                           i,j: position
    , dz                          &   ! [m]                       vertical layer thickness (bottom water column layer)
    , date                        &   ! Date (days since x or elapsed days) yt_edit best if in phase with clock to maintain consistency on restart
    , dt                          &   ! [s]                       time step
    , p_sgrass                    &   ! [m2.sg.hab m-2.grid]      (0~1)  Seagrass coverage (every species)
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    , p_sand                      &   ! [m2.sedmnt m-2.grid]      (0~1)  Sediment coverage
# endif
    , PFD                         &   ! [umol m-2 s-1]            Photon flux density 
    , rho_sw                      &   ! [g cm-3]                  Density of seawater 
    , ocean_DIC_conc              &   ! [umol.DIC kg-1]           DIC concentration of ocean water (input only, do not use in calculations)
    , ocean_DO__conc              &   ! [umol.DO  L-1]            DO  concentration of ocean water (input only, do not use in calculations)
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    , ocean_NO3_conc              &   ! [umol.NO3 L-1]            NO3 concentration of ocean water (input only, do not use in calculations)
    , ocean_NH4_conc              &   ! [umol.NH4 L-1]            NH4 concentration of ocean water (input only, do not use in calculations)
    , ocean_PO4_conc              &   ! [umol.PO4 L-1]            PO4 concentration of ocean water (input only, do not use in calculations)
# endif
!   output parameters
    , DIC_uptake_rate_from_ocean  &   ! [mmol.DIC m-2.sg.hab s-1] DIC uptake rate  * direction of ocean to seagrass is positive
    , DO__uptake_rate_from_ocean  &   ! [mmol.DO  m-2.sg.hab s-1] DO  uptake rate  * direction of ocean to seagrass is positive
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    , NO3_uptake_rate_from_ocean  &   ! [mmol.NO3 m-2.sg.hab s-1] NO3 uptake rate  * direction of ocean to seagrass is positive
    , NH4_uptake_rate_from_ocean  &   ! [mmol.NH4 m-2.sg.hab s-1] NH4 uptake rate  * direction of ocean to seagrass is positive
    , PO4_uptake_rate_from_ocean  &   ! [mmol.PO4 m-2.sg.hab s-1] PO4 uptake rate  * direction of ocean to seagrass is positive
# endif
    , DOC_uptake_rate_from_ocean  &   ! [mmol.C m-2.sg.hab s-1]   Dissolved organic carbon (sugar) uptake rate     * direction of ocean to seagrass is positive
# if defined SEAGRASS_LEAF_POM
    , POC_uptake_rate_from_ocean  &   ! [mmol.C m-2.sg.hab s-1]   Particulate organic carbon uptake rate     * direction of ocean to seagrass is positive
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    , PON_uptake_rate_from_ocean  &   ! [mmol.N m-2.sg.hab s-1]   Particulate organic nitrogen uptake rate   * direction of ocean to seagrass is positive
    , POP_uptake_rate_from_ocean  &   ! [mmol.P m-2.sg.hab s-1]   Particulate organic phosphorus uptake rate * direction of ocean to seagrass is positive
#  endif
# endif
    )

! ====================================================================================================
! Module dependencies
! ====================================================================================================
    USE mod_reef_ecosys_param
    USE mod_geochem
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    USE mod_sedecosys,    ONLY : SEDECO
# endif

! ====================================================================================================
! Variable declarations
! ====================================================================================================
    implicit none

! >>> Inputs <<< !
    integer, intent(in)   :: ng                         !                             nested grid number
    integer, intent(in)   :: species                    !                             seagrass species id
    integer, intent(in)   :: i, j                       !                             i,j: position
    real(8), intent(in)   :: dz                         ! [m]                         vertical layer thickness (bottom water column layer)
    real(8), intent(in)   :: date                       ! [days]                      Date (days since x or elapsed days) yt_edit best if in phase with clock to maintain consistency on restart
    real(8), intent(in)   :: dt                         ! [s]                         Time step interval duration
    real(8), intent(inout):: p_sgrass(Nsg)              ! [m2.sg.hab m-2.grid]        (0~1)  Seagrass coverage (every species)
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    real(8), intent(in)   :: p_sand                     ! [m2.sedmnt m-2.grid]      (0~1)  Sediment coverage
# endif
    real(8), intent(in)   :: PFD                        ! [umol m-2 s-1]              Photon flux density 
    real(8), intent(in)   :: rho_sw                     ! [g cm-3]                    Density of seawater 
    real(8)               :: ocean_DIC_conc(N_Csp)      ! [umol.DIC L-1]              DIC concentration of ocean water
    real(8)               :: ocean_DO__conc             ! [umol.DO  L-1]              DO  concentration of ocean water
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    real(8)               :: ocean_NO3_conc(N_Nsp)      ! [umol.NO3 L-1]              NO3 concentration of ocean water
    real(8)               :: ocean_NH4_conc(N_Nsp)      ! [umol.NH4 L-1]              NH4 concentration of ocean water
    real(8)               :: ocean_PO4_conc(N_Psp)      ! [umol.PO4 L-1]              PO4 concentration of ocean water
# endif

! >>> Outputs <<< !
    real(8), intent(out)  :: DIC_uptake_rate_from_ocean(N_Csp) ! [mmol.DIC m-2.sg.hab s-1]   DIC uptake rate  * direction of ocean to seagrass is positive
    real(8), intent(out)  :: DO__uptake_rate_from_ocean        ! [mmol.DO  m-2.sg.hab s-1]   DO  uptake rate  * direction of ocean to seagrass is positive
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    real(8), intent(out)  :: NO3_uptake_rate_from_ocean(N_Nsp) ! [mmol.NO3 m-2.sg.hab s-1]   NO3 uptake rate  * direction of ocean to seagrass is positive
    real(8), intent(out)  :: NH4_uptake_rate_from_ocean(N_Nsp) ! [mmol.NH4 m-2.sg.hab s-1]   NH4 uptake rate  * direction of ocean to seagrass is positive
    real(8), intent(out)  :: PO4_uptake_rate_from_ocean(N_Psp) ! [mmol.PO4 m-2.sg.hab s-1]   PO4 uptake rate  * direction of ocean to seagrass is positive
# endif
    real(8), intent(out)  :: DOC_uptake_rate_from_ocean(N_Csp) ! [mmol.C m-2.sg.hab s-1]     Dissolved organic carbon (sugar) uptake rate     * direction of ocean to seagrass is positive
# if defined SEAGRASS_LEAF_POM
    real(8), intent(out)  :: POC_uptake_rate_from_ocean(N_Csp) ! [mmol.C m-2.sg.hab s-1]     Particulate organic carbon uptake rate     * direction of ocean to seagrass is positive
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), intent(out)  :: PON_uptake_rate_from_ocean(N_Nsp) ! [mmol.N m-2.sg.hab s-1]     Particulate organic nitrogen uptake rate   * direction of ocean to seagrass is positive
    real(8), intent(out)  :: POP_uptake_rate_from_ocean(N_Psp) ! [mmol.P m-2.sg.hab s-1]     Particulate organic phosphorus uptake rate * direction of ocean to seagrass is positive
#  endif
# endif

! >>> Housekeeping Vars <<< !
    real(8), pointer      :: day                        ! [day]                       Last growth interval
    logical               :: is_growth_interval         !                             Boolean flag for if the current interval is a growth interval
    integer               :: k                          !                             Vertical layer counter

! >>> Saved Constants <<< !
    ! model params
    real(8), pointer      :: GridArea                   ! [m2.grid]                   Area of grid cell
    ! biophysical params
    real(8), pointer      :: SgNperC                    ! [mol.N mol-1.C]             Species specific seagrass Nitrogen to Carbon ratio
    real(8), pointer      :: SgPperC                    ! [mol.P mol-1.C]             Species specific seagrass Phosphorus to Carbon ratio
    real(8), pointer      :: SgMperC                    ! [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    real(8), pointer      :: f_above                    ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction  
    real(8), pointer      :: rho_lf                     ! [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
    real(8), pointer      :: k_proj                     ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    real(8), pointer      :: root_mass_profile(:)       ! []                          Mass ratio of roots in each layer (total must sum to 1.0)
# endif
    ! photosynthesis: light-limit
    real(8), pointer      :: P_max                      ! [umol.C m-2.leaf s-1]       Maximum photoynthesis rate
    real(8), pointer      :: I_k                        ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve
    ! photosynthesis: DIC-limit
    real(8), pointer      :: Lf_DIC_uptake_max          ! [umol.DIC g-1.lf.drywt s-1] Maximum uptake rate of DIC in the leaves
    real(8), pointer      :: k_lf_DIC                   ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer      :: Rt_DIC_flux_max            ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    real(8), pointer      :: k_rt_DIC                   ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
# endif
    ! photosynthesis: nutrient-limit
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    real(8), pointer      :: Lf_NO3_uptake_max          ! [umol.NO3 g-1.lf.drywt s-1] Maximum uptake rate of NO3 in the leaves
    real(8), pointer      :: k_lf_NO3                   ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth
    real(8), pointer      :: smin_lf_NO3                ! [umol.NO3 L-1]              Minimum consentration of NO3 in water column for uptake
    real(8), pointer      :: Lf_NO3_ratio_slope         ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    real(8), pointer      :: Lf_NH4_uptake_max          ! [umol.NH4 g-1.lf.drywt s-1] Maximum uptake rate of NH4 in the leaves
    real(8), pointer      :: k_lf_NH4                   ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth
    real(8), pointer      :: smin_lf_NH4                ! [umol.NH4 L-1]              Minimum consentration of NH4 in water column for uptake
    real(8), pointer      :: Lf_NH4_ratio_slope         ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    real(8), pointer      :: Lf_PO4_uptake_max          ! [umol.PO4 g-1.lf.drywt s-1] Maximum uptake rate of PO4 in the leaves
    real(8), pointer      :: k_lf_PO4                   ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth
    real(8), pointer      :: smin_lf_PO4                ! [umol.PO4 L-1]              Minimum consentration of PO4 in water column for uptake
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer      :: Rt_NO3_flux_max            ! [umol.NO3 g-1.rt.drywt s-1] Maximum uptake rate of NO3 in the roots 
    real(8), pointer      :: k_rt_NO3                   ! [umol.NO3 L-1]              Half saturation consentration of NO3 in soil pore water for growth
    real(8), pointer      :: smin_rt_NO3                ! [umol.NO3 L-1]              Minimum consentration of NO3 in soil pore water for uptake
    real(8), pointer      :: Rt_NO3_ratio_slope         ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    real(8), pointer      :: Rt_NH4_flux_max            ! [umol.NH4 g-1.rt.drywt s-1] Maximum uptake rate of NH4 in the roots 
    real(8), pointer      :: k_rt_NH4                   ! [umol.NH4 L-1]              Half saturation consentration of NH4 in soil pore water for growth
    real(8), pointer      :: smin_rt_NH4                ! [umol.NH4 L-1]              Minimum consentration of NH4 in soil pore water for uptake
    real(8), pointer      :: Rt_NH4_ratio_slope         ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    real(8), pointer      :: Rt_PO4_flux_max            ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    real(8), pointer      :: k_rt_PO4                   ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
    real(8), pointer      :: smin_rt_PO4                ! [umol.PO4 L-1]              Minimum consentration of PO4 in soil pore water for uptake
# endif
    ! respiration: DO-limit
    real(8), pointer      :: Lf_DO_uptake_max           ! [umol.DO g-1.lf.drywt s-1]  Maximum uptake rate of DO in the leaves
    real(8), pointer      :: k_lf_DO                    ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer      :: Rt_DO_flux_max             ! [umol.DO g-1.rt.drywt s-1]  Maximum uptake rate of DO in the roots 
    real(8), pointer      :: k_rt_DO                    ! [umol.DO L-1]               Half saturation consentration of DO in soil pore water for growth
# endif
    ! respiration: required
    real(8), pointer      :: R_basal                    ! [umol.C g-1.leaf s-1]       Maximum respiration rate
    real(8), pointer      :: Turnover_rate              ! [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day
    ! dieoff when respiration is insufficient
    real(8), pointer      :: max_anoxic_dieoff_rate     ! [mol.C mol-1.C day-1]       maximum dieoff due to anoxia in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
    ! stockpile maximum cap
    real(8), pointer      :: DICstockCap                ! []                          Factor to multiply carbon biomsass to get maximum DIC stock (stock limit = DICstockCap*SgCBmF) 
    real(8), pointer      :: CH2OstockCap               ! []                          Factor to multiply carbon biomsass to get maximum sugar stock (stock limit = CH2OstockCap*SgCBmF) 
    real(8), pointer      :: DOstockCap                 ! []                          Factor to multiply carbon biomsass to get maximum DO stock (stock limit = DOstockCap*SgCBmF) 
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer      :: NO3stockCap                ! []                          Factor to multiply nitrogen biomsass to get maximum NO3 stock (stock limit = NO3stockCap*SgNBmF) 
    real(8), pointer      :: NH4stockCap                ! []                          Factor to multiply nitrogen biomsass to get maximum NH4 stock (stock limit = NH4stockCap*SgNBmF) 
    real(8), pointer      :: PO4stockCap                ! []                          Factor to multiply phosphorus biomsass to get maximum PO4 stock (stock limit = PO4stockCap*SgPBmF) 
# endif
    ! Update at end of growth interval
    ! allocate growth
    ! update bed-density/habitat area
    real(8), pointer      :: trans_rate                 ! [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
    real(8), pointer      :: opt_sg_bed_density         ! [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)

! >>> Values Read From Sediment Module <<< !
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    ! real(8)               :: Nsed                     !                             Number of sediment layers
    real(8)               :: sed_layer_thickness(Nsed)  ! [mm]                        Thickness of each sediment layer
# endif
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8)               :: sed_DIC(N_Csp,Nsed)        ! [umol.DIC L-1]              DIC consentration of sediment (in each sediment layer)
    real(8)               :: sed_DO (Nsed)              ! [umol.DO L-1]               DO  consentration of sediment (in each sediment layer)
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8)               :: sed_NO3(N_Nsp,Nsed)        ! [umol.NO3 L-1]              NO3 consentration of sediment (in each sediment layer)
    real(8)               :: sed_NH4(N_Nsp,Nsed)        ! [umol.NH4 L-1]              NH4 consentration of sediment (in each sediment layer)
    real(8)               :: sed_PO4(N_Psp,Nsed)        ! [umol.PO4 L-1]              PO4 consentration of sediment (in each sediment layer)
# endif

! >>> Saved Values <<< !
    ! Update at end of main interval
    real(8), pointer      :: SgCBmF(:)                  ! [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    real(8), pointer      :: DICstock(:)                ! [umol.DIC m-2.sg.hab]       Seagrass internal DIC stock per unit ground area (in seagrass habitat area)
    real(8), pointer      :: CH2Ostock(:)               ! [umol.CH2O m-2.sg.hab]      Seagrass internal sugar stock per unit ground area (in seagrass habitat area)
    real(8), pointer      :: DOstock                    ! [umol.DO m-2.sg.hab]        Seagrass internal DO stock per unit ground area (in seagrass habitat area)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer      :: SgNBmF(:)                  ! [umol.N m-2.sg.hab]         Seagrass leaf+root nitrogen-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    real(8), pointer      :: SgPBmF(:)                  ! [umol.P m-2.sg.hab]         Seagrass leaf+root phosphorus-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    real(8), pointer      :: NO3stock(:)                ! [umol.NO3 m-2.sg.hab]       Seagrass internal NO3 stock per unit ground area (in seagrass habitat area)
    real(8), pointer      :: NH4stock(:)                ! [umol.NH4 m-2.sg.hab]       Seagrass internal NH4 stock per unit ground area (in seagrass habitat area)
    real(8), pointer      :: PO4stock(:)                ! [umol.PO4 m-2.sg.hab]       Seagrass internal PO4 stock per unit ground area (in seagrass habitat area)
#  endif
    real(8), pointer      :: TotSgCBmF                  ! [mol.C.tot]                 Total seagrass carbon-biomass in grid cell. updates on fine time interval
    ! Update at end of growth interval
    ! update total carbon-biomass
    real(8), pointer      :: TotSgCBm                   ! [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
    ! allocate growth
    real(8), pointer      :: TotLfCBm                   ! [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
    real(8), pointer      :: TotRtCBm                   ! [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
    ! update bed-density/habitat area
    real(8), pointer      :: HabArea                    ! [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
    ! leaf-root biomass translocation
    ! update carbon-biomass per area
    real(8), pointer      :: LfCBm                      ! [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
    real(8), pointer      :: RtCBm                      ! [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
    ! update dry-weight biomass
    real(8), pointer      :: TotSgDryBm                 ! [g.drywt]                   Total seagrass dry weight biomass in grid cell
    real(8), pointer      :: TotLfDryBm                 ! [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
    real(8), pointer      :: TotRtDryBm                 ! [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
    real(8), pointer      :: SgDryBm                    ! [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
    real(8), pointer      :: LfDryBm                    ! [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
    real(8), pointer      :: RtDryBm                    ! [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! update leaf area index
    real(8), pointer      :: LAI                        ! [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area
! >>> Saved Values Only for Output  <<< !
    real(8), pointer      :: TotLA                      ! [m2.lf]                     Total one-sided green leaf area in grid
    real(8), pointer      :: GridELAP                   ! [m2.lf.proj m-2.grid]       Effective leaf area projection on ground (whole grid)
    real(8), pointer      :: GridPhot                   ! [umol.C m-2.grid s-1]       Seagrass gross photosynthesis rate per unit ground area (whole grid)
    real(8), pointer      :: GridGrow                   ! [umol.C m-2.grid s-1]       Seagrass growth rate per unit ground area (whole grid)
    integer, pointer      :: PhotLim                    ! []                          Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon stock
    integer, pointer      :: GrowLim                    ! []                          Seagrass growth limiting factor: 1 = Sugar stock; 2 = Nitrogen stock; 3 = Phosphorus stock
    real(8), pointer      :: GridResp                   ! [umol.C m-2.grid s-1]       Seagrass respiration rate per unit ground area (whole grid)
    real(8), pointer      :: GridNetPhot                ! [umol.C m-2.grid s-1]       Seagrass net photosynthesis rate per unit ground area (whole grid)
    real(8), pointer      :: GridDieoff                 ! [umol.C m-2.grid s-1]       Seagrass dieoff rate carbon biomass per unit ground area (whole grid)
# if defined NET_ZERO_MASS_START
! >>> Calculate initial seagrass mass balance and destroy (erase) equivalent amount of mass output until net zero is reached, before switching to normal flux <<< !
    real(8), pointer      :: InitialC                   ! [mmol.C]                    Initial carbon remaining to destroy until net zero
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer      :: InitialN                   ! [mmol.N]                    Initial nitrogen remaining to destroy until net zero
    real(8), pointer      :: InitialP                   ! [mmol.P]                    Initial phosphorus remaining to destroy until net zero
#  endif
# endif
! >>> Delta Values <<< !      amount to add to stored variables at end of the calculation time step to update values: positive adds, negative subtracts (erased each time step)
    real(8)               :: delta_SgCBmF(N_Csp)        ! [umol.C m-2.sg.hab s-1]     Rate of change in SgCBmF
    real(8)               :: delta_DICstock(N_Csp)      ! [umol.DIC m-2.sg.hab s-1]   Rate of change in DICstock
    real(8)               :: delta_CH2Ostock(N_Csp)     ! [umol.CH2O m-2.sg.hab]      Rate of change in CH2Ostock
    real(8)               :: delta_DOstock              ! [umol.DO m-2.sg.hab s-1]    Rate of change in DOstock
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8)               :: delta_SgNBmF(N_Nsp)        ! [umol.N m-2.sg.hab s-1]     Rate of change in SgNBmF
    real(8)               :: delta_SgPBmF(N_Psp)        ! [umol.P m-2.sg.hab s-1]     Rate of change in SgPBmF
    real(8)               :: delta_NO3stock(N_Nsp)      ! [umol.NO3 m-2.sg.hab s-1]   Rate of change in NO3stock
    real(8)               :: delta_NH4stock(N_Nsp)      ! [umol.NH4 m-2.sg.hab s-1]   Rate of change in NH4stock
    real(8)               :: delta_PO4stock(N_Psp)      ! [umol.PO4 m-2.sg.hab s-1]   Rate of change in PO4stock
#  endif
    ! values to write to sediment module
!# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
!    real(8), pointer :: delta_sed_DIC(:,:)              ! [umol.DIC L-1 s-1]          Rate of change in DIC due to flux from roots to sediment (positive means seagrass to sediment)
!    real(8), pointer :: delta_sed_DO   (:)              ! [umol.DO L-1 s-1]           Rate of change in DO due to flux from roots to sediment (positive means seagrass to sediment)
!# endif
!# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
!    real(8), pointer :: delta_sed_NO3(:,:)              ! [umol.NO3 L-1 s-1]          Rate of change in NO3 due to flux from roots to sediment (positive means seagrass to sediment)
!    real(8), pointer :: delta_sed_NH4(:,:)              ! [umol.NH4 L-1 s-1]          Rate of change in NH4 due to flux from roots to sediment (positive means seagrass to sediment)
!    real(8), pointer :: delta_sed_PO4(:,:)              ! [umol.PO4 L-1 s-1]          Rate of change in PO4 due to flux from roots to sediment (positive means seagrass to sediment)
!# endif
!# if defined SEAGRASS_ROOT_POM
!    real(8), pointer :: delta_sed_POC(:,:)              ! [umol.C L-1 s-1]            Rate of change in POC due to root death in sediment (positive means seagrass to sediment)
!#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
!    real(8), pointer :: delta_sed_PON(:,:)              ! [umol.N L-1 s-1]            Rate of change in PON due to root death in sediment (positive means seagrass to sediment)
!    real(8), pointer :: delta_sed_POP(:,:)              ! [umol.P L-1 s-1]            Rate of change in POP due to root death in sediment (positive means seagrass to sediment)
!#  endif
!# endif

! >>> Intermediate Calculation Variables <<< !      (erased each time step)
    ! Widely used temporary variables
    real(8)               :: f_abv_cur                  ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current above ground biomass to total biomass fraction, calculated on the last growth interval
    real(8)               :: f_blw_cur                  ! [g.rt.drywt g-1.tot.drywt]  also [mol.C.rt mol-1.C.tot] current below ground biomass to total biomass fraction, calculated on the last growth interval
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    real(8)               :: sgrass_per_sedmnt          ! [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    real(8)               :: root_comp_dist_factor(Nsed)! [g.rt.comp g-1.tot m2.sg.hab m-2.sedmnt mm-1]    root compartemtent distribution factor equal to f_blw_cur * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 

# endif
    ! Photosynthesis
    real(8)               :: Phot_rate                  ! [umol.C m-2.sg.hab s-1]     Gross photosynthesis rate
    ! Growth
    real(8)               :: Growth_rate                ! [umol.C m-2.sg.hab s-1]     Growth rate
    ! photosynthesis: light-limit
    real(8)               :: Phot_limit_light           ! [umol.C m-2.sg.hab s-1]     Photosynthesis rate possible as limited by light
    real(8)               :: ELAP                       ! [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
    ! photosynthesis: DIC-limit
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8)               :: DIC_uptake_possible(0:Nsed)! [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
# else
    real(8)               :: DIC_uptake_possible(0:0)   ! 
# endif
    real(8)               :: sumCUpPos                  ! [umol.DIC m-2.sg.hab s-1]   sum of all potential DIC uptake (used for scaling/normalizing uptake proportions)
    real(8)               :: Phot_limit_DIC             ! [umol.C m-2.sg.hab s-1]     Photosynthesis rate possible as limited by DIC uptake
    real(8)               :: Growth_limit_CH2O          ! [umol.C m-2.sg.hab s-1]     Growth rate possible as limited by CH2O stock
    ! photosynthesis: nutrient-limit
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE && defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8)               :: NO3_uptake_possible(0:Nsed)! [umol.NO3 m-2.sg.hab s-1]   NO3 uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    real(8)               :: NH4_uptake_possible(0:Nsed)! [umol.NH4 m-2.sg.hab s-1]   NH4 uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    real(8)               :: PO4_uptake_possible(0:Nsed)! [umol.PO4 m-2.sg.hab s-1]   PO4 uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
# elif defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    real(8)               :: NO3_uptake_possible(0:0)   ! 
    real(8)               :: NH4_uptake_possible(0:0)   ! 
    real(8)               :: PO4_uptake_possible(0:0)   ! 
# elif defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8)               :: NO3_uptake_possible(1:Nsed)! 
    real(8)               :: NH4_uptake_possible(1:Nsed)! 
    real(8)               :: PO4_uptake_possible(1:Nsed)! 
# endif
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8)               :: sumNUpPos                  ! [umol.N m-2.sg.hab s-1]     sum of all potential nitrogen uptake (used for scaling/normalizing uptake proportions)
    real(8)               :: sumNO3UpPos                ! [umol.NO3 m-2.sg.hab s-1]   sum of all potential nitrate uptake (used for scaling/normalizing uptake proportions)
    real(8)               :: sumNH4UpPos                ! [umol.NH4 m-2.sg.hab s-1]   sum of all potential ammonium uptake (used for scaling/normalizing uptake proportions)
    real(8)               :: sumPUpPos                  ! [umol.P m-2.sg.hab s-1]     sum of all potential phosphorus uptake (used for scaling/normalizing uptake proportions)
    real(8)               :: Growth_limit_NO3           ! [umol.C m-2.sg.hab s-1]     Growth rate possible as limited by NO3 stock
    real(8)               :: Growth_limit_NH4           ! [umol.C m-2.sg.hab s-1]     Growth rate possible as limited by NH4 stock
    real(8)               :: Growth_limit_PO4           ! [umol.C m-2.sg.hab s-1]     Growth rate possible as limited by PO4 stock
    real(8)               :: DO_released_from_NO3       ! [umol.O m-2.sg.hab s-1]     Total DO produced from consumption of NO3 in the growth reaction: for every mol of NO3 uptake, release 2 mol of O2
# endif
    ! Respiration/Un-growth
    real(8)               :: Resp_rate                  ! [umol.C m-2.sg.hab s-1]     Gross respiration/un-growth rate
    ! respiration: DO-limit
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8)               :: DO_uptake_possible(0:Nsed) ! [umol.DO m-2.sg.hab s-1]    DO uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
# else
    real(8)               :: DO_uptake_possible(0:0)    !
# endif
    real(8)               :: sumOUpPos                  ! [umol.DO m-2.sg.hab s-1]    sum of all potential DO uptake (used for scaling/normalizing uptake proportions)
    real(8)               :: Resp_limit_DO              ! [umol.C m-2.sg.hab s-1]     Respiration/un-growth rate possible as limited by DO uptake
    real(8)               :: CH2Orequired               ! [umol.CH2O m-2.sg.hab]      sugar required in respiration
    real(8)               :: SgCBmF_toMaintain          ! [umol.C m-2.sg.hab]         seagrass biomass to maintain by respiration
    real(8)               :: Ungrowth_rate              ! [umol.C m-2.sg.hab s-1]     rate at which biomass is metabolized for respiration when DO stock is insufficient
    ! respiration: dieoff
    real(8)               :: anoxic_dieoff              ! [umol.C m-2.sg.hab]         Seagrass dieoff due to anoxia this time step
    real(8)               :: dieoff_rate                ! [umol.C m-2.sg.hab s-1]     Seagrass dieoff rate
    ! Update at end of growth interval
    ! update total carbon-biomass
    real(8)               :: oldTotSgCBm                ! [mol.C.tot]                 Old total seagrass carbon-biomass in grid cell
    real(8)               :: oldTotLfCBm                ! [mol.C.lf]                  Old total seagrass leaf carbon-biomass in grid cell
    real(8)               :: oldTotRtCBm                ! [mol.C.rt]                  Old total seagrass root carbon-biomass in grid cell
    ! allocate growth
    real(8)               :: mass_to_allocate           ! [mol.C]                     mass to allocate in growth or death to either leaves or roots
    ! update bed-density/habitat area
    real(8)               :: desiredHabArea             ! [m2.hab]                    The desired habitat area given the biomass and optimal density
    real(8)               :: translocatableArea         ! [m2.hab]                    Area that can be translocated (shrink or expand) as limited by the translocation rate
    ! leaf-root biomass translocation
    real(8)               :: translocatable_mass        ! [mol.C]                     Biomass that can be translocated between leaf and root as limited by the translocation rate

    real(8)               :: ocnDIC                     ! [umol/L]                    Ocean DIC concentration
    real(8)               :: ocnDO                      ! [umol/L]                    Ocean DO concentration
    real(8)               :: ocnNO3                     ! [umol/L]                    Ocean NO3 concentration
    real(8)               :: ocnNH4                     ! [umol/L]                    Ocean NH4 concentration
    real(8)               :: ocnN                       ! [umol/L]                    NO3 + NH4 concentration of ocean water
    real(8)               :: ocnPO4                     ! [umol/L]                    Ocean PO4 concentration
    real(8)               :: rDIC_ocn(N_Csp)            ! [no dim.]                   Isotope ratio or tracer mass ratio
    real(8)               :: rNO3_ocn(N_Nsp)            ! [no dim.]                   Isotope ratio or tracer mass ratio
    real(8)               :: rNH4_ocn(N_Nsp)            ! [no dim.]                   Isotope ratio or tracer mass ratio
    real(8)               :: rPO4_ocn(N_Psp)            ! [no dim.]                   Isotope ratio or tracer mass ratio
    real(8)               :: rSgCBm  (N_Csp)            ! [no dim.]                   Isotope ratio or tracer mass ratio
    real(8)               :: rSgNBm  (N_Nsp)            ! [no dim.]                   Isotope ratio or tracer mass ratio
    real(8)               :: rSgPBm  (N_Psp)            ! [no dim.]                   Isotope ratio or tracer mass ratio
    real(8)               :: rDICstock(N_Csp)           ! [no dim.]                   Isotope ratio or tracer mass ratio
    real(8)               :: rCH2Ostock(N_Csp)          ! [no dim.]                   Isotope ratio or tracer mass ratio
    real(8)               :: rNO3stock(N_Nsp)           ! [no dim.]                   Isotope ratio or tracer mass ratio
    real(8)               :: rNH4stock(N_Nsp)           ! [no dim.]                   Isotope ratio or tracer mass ratio
    real(8)               :: rPO4stock(N_Psp)           ! [no dim.]                   Isotope ratio or tracer mass ratio
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8)               :: sedDIC                     ! [umol/L]                    Sediment DIC concentration
    real(8)               :: rDIC_sed(N_Csp,Nsed)       ! [no dim.]                   Isotope ratio or tracer mass ratio
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8)               :: sedNO3                     ! [umol/L]                    Sediment NO3 concentration
    real(8)               :: sedNH4                     ! [umol/L]                    Sediment NH4 concentration
    real(8)               :: sedN                       ! [umol/L]                    NO3 + NH4 concentration of sediment water
    real(8)               :: sedPO4                     ! [umol/L]                    Sediment PO4 concentration
    real(8)               :: rNO3_sed(N_Nsp,Nsed)       ! [no dim.]                   Isotope ratio or tracer mass ratio
    real(8)               :: rNH4_sed(N_Nsp,Nsed)       ! [no dim.]                   Isotope ratio or tracer mass ratio
    real(8)               :: rPO4_sed(N_Psp,Nsed)       ! [no dim.]                   Isotope ratio or tracer mass ratio
# endif
    integer               :: isp                        ! []                          index for a specific isotope or tracer
    real(8)               :: rNC, rPC                   ! []                          N/C ratio, P/C ratio

!!  Output ------------------------------------------------------------------
# if defined SEAGRASS_TESTMODE
    real(8), save :: dsec = 0.d0 !sec
# endif

    ! write(*,*) 'yt_debug: start of mod_seagrass.F seagrass()   i =', i, '   j =', j


! ====================================================================================================
! Variable initializations
! ====================================================================================================


! >>> Outputs <<< !
    ! clear old values from output variables
    DIC_uptake_rate_from_ocean(:) = 0.0d0                                      ! [mmol.DIC m-2.sg.hab s-1]   DIC uptake rate  * direction of ocean to seagrass is positive
    DOC_uptake_rate_from_ocean(:) = 0.0d0                                      ! [mmol.CH2O m-2.sg.hab s-1]  Dissolved organic carbon (sugar) uptake rate     * direction of ocean to seagrass is positive
    DO__uptake_rate_from_ocean    = 0.0d0                                      ! [mmol.DO  m-2.sg.hab s-1]   DO  uptake rate  * direction of ocean to seagrass is positive
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE        
    NO3_uptake_rate_from_ocean(:) = 0.0d0                                      ! [mmol.NO3 m-2.sg.hab s-1]   NO3 uptake rate  * direction of ocean to seagrass is positive
    NH4_uptake_rate_from_ocean(:) = 0.0d0                                      ! [mmol.NH4 m-2.sg.hab s-1]   NH4 uptake rate  * direction of ocean to seagrass is positive
    PO4_uptake_rate_from_ocean(:) = 0.0d0                                      ! [mmol.PO4 m-2.sg.hab s-1]   PO4 uptake rate  * direction of ocean to seagrass is positive
# endif
# if defined SEAGRASS_LEAF_POM
    POC_uptake_rate_from_ocean(:) = 0.0d0                                      ! [mmol.C m-2.sg.hab s-1]     Particulate organic carbon uptake rate     * direction of ocean to seagrass is positive
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    PON_uptake_rate_from_ocean(:) = 0.0d0                                      ! [mmol.N m-2.sg.hab s-1]     Particulate organic nitrogen uptake rate   * direction of ocean to seagrass is positive
    POP_uptake_rate_from_ocean(:) = 0.0d0                                      ! [mmol.P m-2.sg.hab s-1]     Particulate organic phosphorus uptake rate * direction of ocean to seagrass is positive
#  endif
# endif


! >>> Housekeeping Vars <<< !
    ! update at start of every loop before everything else
    Day                 =>  SGRASS(ng)%Day                            (i,j) ! [day]                       Last growth interval
    is_growth_interval  =   .false.                                         !                             Boolean flag for if the current interval is a growth interval

! >>> Saved Constants <<< !
    ! model params
    GridArea            =>  SGRASS(ng)%GridArea                       (i,j) ! [m2]                        Area of grid cells
    ! biophysical params
    SgNperC             =>  SGRASS(ng)%SgNperC                (species)     ! [mol.N mol-1.C]             Species specific seagrass Nitrogen to Carbon ratio
    SgPperC             =>  SGRASS(ng)%SgPperC                (species)     ! [mol.P mol-1.C]             Species specific seagrass Phosphorus to Carbon ratio
    SgMperC             =>  SGRASS(ng)%SgMperC                (species)     ! [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    f_above             =>  SGRASS(ng)%f_above                (species)     ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction  
    rho_lf              =>  SGRASS(ng)%rho_lf                 (species)     ! [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
    k_proj              =>  SGRASS(ng)%k_proj                 (species)     ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    root_mass_profile   =>  SGRASS(ng)%root_mass_profile      (species, :)  ! []                          Mass ratio of roots in each layer (total must sum to 1.0)
# endif
    ! photoynthesis: light-limit
    P_max               =>  SGRASS(ng)%P_max                  (species)     ! [umol.C m-2.leaf s-1]       Maximum photoynthesis rate
    I_k                 =>  SGRASS(ng)%I_k                    (species)     ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve
    ! photoynthesis: DIC-limit
    Lf_DIC_uptake_max   =>  SGRASS(ng)%Lf_DIC_uptake_max      (species)     ! [umol.DIC g-1.lf.drywt s-1] Maximum uptake rate of DIC in the leaves
    k_lf_DIC            =>  SGRASS(ng)%k_lf_DIC               (species)     ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    Rt_DIC_flux_max     =>  SGRASS(ng)%Rt_DIC_flux_max        (species)     ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    k_rt_DIC            =>  SGRASS(ng)%k_rt_DIC               (species)     ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
# endif
    ! photoynthesis: nutrient-limit
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    Lf_NO3_uptake_max   =>  SGRASS(ng)%Lf_NO3_uptake_max      (species)     ! [umol.NO3 g-1.lf.drywt s-1] Maximum uptake rate of NO3 in the leaves
    k_lf_NO3            =>  SGRASS(ng)%k_lf_NO3               (species)     ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth
    smin_lf_NO3         =>  SGRASS(ng)%smin_lf_NO3            (species)     ! [umol.NO3 L-1]              Minimum consentration of NO3 in water column for uptake
    Lf_NO3_ratio_slope  =>  SGRASS(ng)%Lf_NO3_ratio_slope     (species)     ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    Lf_NH4_uptake_max   =>  SGRASS(ng)%Lf_NH4_uptake_max      (species)     ! [umol.NH4 g-1.lf.drywt s-1] Maximum uptake rate of NH4 in the leaves
    k_lf_NH4            =>  SGRASS(ng)%k_lf_NH4               (species)     ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth
    smin_lf_NH4         =>  SGRASS(ng)%smin_lf_NH4            (species)     ! [umol.NH4 L-1]              Minimum consentration of NH4 in water column for uptake
    Lf_NH4_ratio_slope  =>  SGRASS(ng)%Lf_NH4_ratio_slope     (species)     ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    Lf_PO4_uptake_max   =>  SGRASS(ng)%Lf_PO4_uptake_max      (species)     ! [umol.PO4 g-1.lf.drywt s-1] Maximum uptake rate of PO4 in the leaves
    k_lf_PO4            =>  SGRASS(ng)%k_lf_PO4               (species)     ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth
    smin_lf_PO4         =>  SGRASS(ng)%smin_lf_PO4            (species)     ! [umol.PO4 L-1]              Minimum consentration of PO4 in water column for uptake
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    Rt_NO3_flux_max     =>  SGRASS(ng)%Rt_NO3_flux_max        (species)     ! [umol.NO3 g-1.rt.drywt s-1] Maximum uptake rate of NO3 in the roots 
    k_rt_NO3            =>  SGRASS(ng)%k_rt_NO3               (species)     ! [umol.NO3 L-1]              Half saturation consentration of NO3 in soil pore water for growth
    smin_rt_NO3         =>  SGRASS(ng)%smin_rt_NO3            (species)     ! [umol.NO3 L-1]              Minimum consentration of NO3 in soil pore water for uptake
    Rt_NO3_ratio_slope  =>  SGRASS(ng)%Rt_NO3_ratio_slope     (species)     ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    Rt_NH4_flux_max     =>  SGRASS(ng)%Rt_NH4_flux_max        (species)     ! [umol.NH4 g-1.rt.drywt s-1] Maximum uptake rate of NH4 in the roots 
    k_rt_NH4            =>  SGRASS(ng)%k_rt_NH4               (species)     ! [umol.NH4 L-1]              Half saturation consentration of NH4 in soil pore water for growth
    smin_rt_NH4         =>  SGRASS(ng)%smin_rt_NH4            (species)     ! [umol.NH4 L-1]              Minimum consentration of NH4 in soil pore water for uptake
    Rt_NH4_ratio_slope  =>  SGRASS(ng)%Rt_NH4_ratio_slope     (species)     ! [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    Rt_PO4_flux_max     =>  SGRASS(ng)%Rt_PO4_flux_max        (species)     ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    k_rt_PO4            =>  SGRASS(ng)%k_rt_PO4               (species)     ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
    smin_rt_PO4         =>  SGRASS(ng)%smin_rt_PO4            (species)     ! [umol.PO4 L-1]              Minimum consentration of PO4 in soil pore water for uptake
# endif
    ! respiration: DO-limit
    Lf_DO_uptake_max    =>  SGRASS(ng)%Lf_DO_uptake_max       (species)     ! [umol.DO g-1.lf.drywt s-1]  Maximum uptake rate of DO in the leaves
    k_lf_DO             =>  SGRASS(ng)%k_lf_DO                (species)     ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    Rt_DO_flux_max      =>  SGRASS(ng)%Rt_DO_flux_max         (species)     ! [umol.DO g-1.rt.drywt s-1]  Maximum uptake rate of DO in the roots 
    k_rt_DO             =>  SGRASS(ng)%k_rt_DO                (species)     ! [umol.DO L-1]               Half saturation consentration of DO in soil pore water for growth
# endif
    ! respiration: required
    R_basal             =>  SGRASS(ng)%R_basal                (species)     ! [umol.C g-1.leaf s-1]       Maximum respiration rate
    Turnover_rate       =>  SGRASS(ng)%Turnover_rate          (species)     ! [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day
    ! dieoff when respiration is insufficient
    max_anoxic_dieoff_rate =>  SGRASS(ng)%max_anoxic_dieoff_rate(species)   ! [mol.C mol-1.C day-1]       maximum dieoff due to anoxia in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
    ! stockpile maximum cap
    DICstockCap         =>  SGRASS(ng)%DICstockCap            (species)     ! []                          Factor to multiply carbon biomsass to get maximum DIC stock (stock limit = DICstockCap*SgCBmF) 
    CH2OstockCap        =>  SGRASS(ng)%CH2OstockCap           (species)     ! []                          Factor to multiply carbon biomsass to get maximum sugar stock (stock limit = CH2OstockCap*SgCBmF) 
    DOstockCap          =>  SGRASS(ng)%DOstockCap             (species)     ! []                          Factor to multiply carbon biomsass to get maximum DO stock (stock limit = DOstockCap*SgCBmF) 
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    NO3stockCap         =>  SGRASS(ng)%NO3stockCap            (species)     ! []                          Factor to multiply nitrogen biomsass to get maximum NO3 stock (stock limit = NO3stockCap*SgNBmF) 
    NH4stockCap         =>  SGRASS(ng)%NH4stockCap            (species)     ! []                          Factor to multiply nitrogen biomsass to get maximum NH4 stock (stock limit = NH4stockCap*SgNBmF) 
    PO4stockCap         =>  SGRASS(ng)%PO4stockCap            (species)     ! []                          Factor to multiply phosphorus biomsass to get maximum PO4 stock (stock limit = PO4stockCap*SgPBmF) 
# endif
    ! Update at end of growth interval
    ! allocate growth
    ! update bed-density/habitat area
    trans_rate          =>  SGRASS(ng)%trans_rate             (species)     ! [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
    opt_sg_bed_density  =>  SGRASS(ng)%opt_sg_bed_density     (species)     ! [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)

! >>> Values Read From Sediment Module <<< !
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    ! Nsed                                                    Number of sediment layers
    sed_layer_thickness(:) = SEDECO(ng)%dzr(:)/10d0                         ! [mm]                        Thickness of each sediment layer
# endif
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    sed_DIC(:,:)   =   SEDECO(ng)%DIC(:,1:Nsed,i,j)                                ! [umol.DIC L-1]              DIC consentration of sediment (in each sediment layer)
    where (sed_DIC .le. 0.0d0) sed_DIC = 0.0d0                              !                             error-correct negative values to become zero
    sed_DO (:)     =   SEDECO(ng)%O2 (  1:Nsed,i,j)                                ! [umol.DO L-1]               DO  consentration of sediment (in each sediment layer)
    where (sed_DO .le. 0.0d0) sed_DO = 0.0d0                                !                             error-correct negative values to become zero
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    sed_NO3(:,:)   =   SEDECO(ng)%NO3(:,1:Nsed,i,j)                                ! [umol.NO3 L-1]              NO3 consentration of sediment (in each sediment layer)
    where (sed_NO3 .le. 0.0d0) sed_NO3 = 0.0d0                              !                             error-correct negative values to become zero
    sed_NH4(:,:)   =   SEDECO(ng)%NH4(:,1:Nsed,i,j)                                ! [umol.NH4 L-1]              NH4 consentration of sediment (in each sediment layer)
    where (sed_NH4 .le. 0.0d0) sed_NH4 = 0.0d0                              !                             error-correct negative values to become zero
    sed_PO4(:,:)   =   SEDECO(ng)%PO4(:,1:Nsed,i,j)                                ! [umol.PO4 L-1]              PO4 consentration of sediment (in each sediment layer)
    where (sed_PO4 .le. 0.0d0) sed_PO4 = 0.0d0                              !                             error-correct negative values to become zero
# endif

! >>> Saved Values <<< !
    ! Update at end of main interval
    SgCBmF(1:N_Csp)     =>  SGRASS(ng)%SgCBmF               (:,species,i,j) ! [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    DICstock(1:N_Csp)   =>  SGRASS(ng)%DICstock             (:,species,i,j) ! [umol.DIC m-2.sg.hab]       Seagrass internal DIC stock per unit ground area (in seagrass habitat area)
    CH2Ostock(1:N_Csp)  =>  SGRASS(ng)%CH2Ostock            (:,species,i,j) ! [umol.CH2O m-2.sg.hab]      Seagrass internal sugar stock per unit ground area (in seagrass habitat area)
    DOstock             =>  SGRASS(ng)%DOstock                (species,i,j) ! [umol.DO m-2.sg.hab]        Seagrass internal DO stock per unit ground area (in seagrass habitat area)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    SgNBmF(1:N_Nsp)     =>  SGRASS(ng)%SgNBmF               (:,species,i,j) ! [umol.N m-2.sg.hab]         Seagrass leaf+root nitrogen-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    SgPBmF(1:N_Psp)     =>  SGRASS(ng)%SgPBmF               (:,species,i,j) ! [umol.P m-2.sg.hab]         Seagrass leaf+root phosphorus-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    NO3stock(1:N_Nsp)   =>  SGRASS(ng)%NO3stock             (:,species,i,j) ! [umol.NO3 m-2.sg.hab]       Seagrass internal NO3 stock per unit ground area (in seagrass habitat area)
    NH4stock(1:N_Nsp)   =>  SGRASS(ng)%NH4stock             (:,species,i,j) ! [umol.NH4 m-2.sg.hab]       Seagrass internal NH4 stock per unit ground area (in seagrass habitat area)
    PO4stock(1:N_Psp)   =>  SGRASS(ng)%PO4stock             (:,species,i,j) ! [umol.PO4 m-2.sg.hab]       Seagrass internal PO4 stock per unit ground area (in seagrass habitat area)
#  endif
    TotSgCBmF           =>  SGRASS(ng)%TotSgCBmF              (species,i,j) ! [mol.C.tot]                 Total seagrass carbon-biomass in grid cell. updates on fine time interval
    ! Update at end of growth interval
    ! update total carbon-biomass
    TotSgCBm            =>  SGRASS(ng)%TotSgCBm               (species,i,j) ! [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
    ! allocate growth
    TotLfCBm            =>  SGRASS(ng)%TotLfCBm               (species,i,j) ! [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
    TotRtCBm            =>  SGRASS(ng)%TotRtCBm               (species,i,j) ! [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
    ! update bed-density/habitat area
    HabArea             =>  SGRASS(ng)%HabArea                (species,i,j) ! [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
    ! leaf-root biomass translocation
    ! update carbon-biomass per area
    LfCBm               =>  SGRASS(ng)%LfCBm                  (species,i,j) ! [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
    RtCBm               =>  SGRASS(ng)%RtCBm                  (species,i,j) ! [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
    ! update dry-weight biomass
    TotSgDryBm          =>  SGRASS(ng)%TotSgDryBm             (species,i,j) ! [g.drywt]                   Total seagrass dry weight biomass in grid cell
    TotLfDryBm          =>  SGRASS(ng)%TotLfDryBm             (species,i,j) ! [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
    TotRtDryBm          =>  SGRASS(ng)%TotRtDryBm             (species,i,j) ! [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
    SgDryBm             =>  SGRASS(ng)%SgDryBm                (species,i,j) ! [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
    LfDryBm             =>  SGRASS(ng)%LfDryBm                (species,i,j) ! [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
    RtDryBm             =>  SGRASS(ng)%RtDryBm                (species,i,j) ! [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! update leaf area index
    LAI                 =>  SGRASS(ng)%LAI                    (species,i,j) ! []                          Leaf area index: one-sided green leaf area per unit ground surface area
! >>> Saved Values Only for Output  <<< !
    TotLA               =>  SGRASS(ng)%TotLA                  (species,i,j) ! [m2.lf]                     Total one-sided green leaf area in grid
    GridELAP            =>  SGRASS(ng)%GridELAP               (species,i,j) ! [m2.lf.proj m-2.grid]       Effective leaf area projection on ground (whole grid)
    GridPhot            =>  SGRASS(ng)%GridPhot               (species,i,j) ! [umol.C m-2.grid s-1]       Seagrass gross photosynthesis rate per unit ground area (whole grid)
    GridGrow            =>  SGRASS(ng)%GridGrow               (species,i,j) ! [umol.C m-2.grid s-1]       Seagrass growth rate per unit ground area (whole grid)
    PhotLim             =>  SGRASS(ng)%PhotLim                (species,i,j) ! []                          Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon stock
    GrowLim             =>  SGRASS(ng)%GrowLim                (species,i,j) ! []                          Seagrass growth limiting factor: 1 = Sugar stock; 2 = Nitrogen stock; 3 = Phosphorus stock
    GridResp            =>  SGRASS(ng)%GridResp               (species,i,j) ! [umol.C m-2.grid s-1]       Seagrass respiration rate per unit ground area (whole grid)
    GridNetPhot         =>  SGRASS(ng)%GridNetPhot            (species,i,j) ! [umol.C m-2.grid s-1]       Seagrass net photosynthesis rate per unit ground area (whole grid)
    GridDieoff          =>  SGRASS(ng)%GridDieoff             (species,i,j) ! [umol.C m-2.grid s-1]       Seagrass dieoff rate carbon biomass per unit ground area (whole grid)
# if defined NET_ZERO_MASS_START
! >>> Calculate initial seagrass mass balance and destroy (erase) equivalent amount of mass output until net zero is reached, before switching to normal flux <<< !
    InitialC            =>  SGRASS(ng)%InitialC               (species,i,j) ! [mmol.C]                    Initial carbon remaining to destroy until net zero
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    InitialN            =>  SGRASS(ng)%InitialN               (species,i,j) ! [mmol.N]                    Initial nitrogen remaining to destroy until net zero
    InitialP            =>  SGRASS(ng)%InitialP               (species,i,j) ! [mmol.P]                    Initial phosphorus remaining to destroy until net zero
#  endif
# endif

! >>> Delta Values <<< !      amount to add to stored variables at end of the calculation time step to update values: positive adds, negative subtracts (erased each time step)
    ! set delta variables to zero
    delta_SgCBmF(1:N_Csp)     =   0.0d0                                     ! [umol.C m-2.sg.hab s-1]     Rate of change in SgCBmF
    delta_DICstock(1:N_Csp)   =   0.0d0                                     ! [umol.DIC m-2.sg.hab s-1]   Rate of change in DICstock
    delta_CH2Ostock(1:N_Csp)  =   0.0d0                                     ! [umol.CH2O m-2.sg.hab]      Rate of change in CH2Ostock
    delta_DOstock             =   0.0d0                                     ! [umol.DO m-2.sg.hab s-1]    Rate of change in DOstock
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    delta_SgNBmF(1:N_Nsp)     =   0.0d0                                     ! [umol.N m-2.sg.hab s-1]     Rate of change in SgNBmF
    delta_SgPBmF(1:N_Psp)     =   0.0d0                                     ! [umol.P m-2.sg.hab s-1]     Rate of change in SgPBmF
    delta_NO3stock(1:N_Nsp)   =   0.0d0                                     ! [umol.NO3 m-2.sg.hab s-1]   Rate of change in NO3stock
    delta_NH4stock(1:N_Nsp)   =   0.0d0                                     ! [umol.NH4 m-2.sg.hab s-1]   Rate of change in NH4stock
    delta_PO4stock(1:N_Psp)   =   0.0d0                                     ! [umol.PO4 m-2.sg.hab s-1]   Rate of change in PO4stock
#  endif
    ! values to write to sediment module
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    SEDECO(ng)%sg_dDIC_dt(:,:,species,i,j) = 0.0d0                          ! [umol.DIC L-1 s-1]          Rate of change in DIC due to flux from roots to sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dDOC_dt(:,:,species,i,j) = 0.0d0                          ! [umol.DIC L-1 s-1]          Rate of change in DOC (sugar) due to flux from roots to sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dO2_dt (  :,species,i,j) = 0.0d0                          ! [umol.DO L-1 s-1]           Rate of change in DO due to flux from roots to sediment (positive means seagrass to sediment)
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    SEDECO(ng)%sg_dNO3_dt(:,:,species,i,j) = 0.0d0                          ! [umol.NO3 L-1 s-1]          Rate of change in NO3 due to flux from roots to sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dNH4_dt(:,:,species,i,j) = 0.0d0                          ! [umol.NH4 L-1 s-1]          Rate of change in NH4 due to flux from roots to sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dPO4_dt(:,:,species,i,j) = 0.0d0                          ! [umol.PO4 L-1 s-1]          Rate of change in PO4 due to flux from roots to sediment (positive means seagrass to sediment)
# endif
# if defined SEAGRASS_ROOT_POM
    SEDECO(ng)%sg_dPOC_dt(:,:,species,i,j) = 0.0d0                          ! [umol.C L-1 s-1]            Rate of change in POC due to root death in sediment (positive means seagrass to sediment)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    SEDECO(ng)%sg_dPON_dt(:,:,species,i,j) = 0.0d0                          ! [umol.N L-1 s-1]            Rate of change in PON due to root death in sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dPOP_dt(:,:,species,i,j) = 0.0d0                          ! [umol.P L-1 s-1]            Rate of change in POP due to root death in sediment (positive means seagrass to sediment)
#  endif
# endif

! ! yt_debug >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! if (SgCBmF /= SgCBmF) then
!   write(*,*) 'SEAGRASS error: SgCBmF is NAN at start'
!   error stop
! else 
!   write(*,*) 'SEAGRASS okay : SgCBmF =', SgCBmF
! endif
! ! yt_debug <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


! ====================================================================================================
! ----------------------------------------------------------------------------------------------------
!                                  HOUSE KEEPING AT START OF LOOP
! ----------------------------------------------------------------------------------------------------
! ====================================================================================================
    
    if (date .gt. aint(Day*24.0d0)/24.0d0 ) then
      is_growth_interval = .true.
      Day = (aint(Day*24.0d0)+1.0d0)/24.0d0  ! One hour interval
      trans_rate = trans_rate / 24.0d0
      ! if(i == 50 .and. j == 50) then
      !   write(*,*) "yt_debug:    Day = ", Day, "   date = ", date
      ! endif
    endif

! ====================================================================================================
! ----------------------------------------------------------------------------------------------------
!                                    MAIN INTERVAL CALCULATIONS
! ----------------------------------------------------------------------------------------------------
! ====================================================================================================

    ! write(*,*) 'yt_debug: i = ', i, 
    ! write(*,*) 'Yo'


! Widely used temporary variables --------------------------------------------------------------------

    ! f_abv_cur                   [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current above ground biomass to total biomass fraction, calculated on the last growth interval
    ! f_blw_cur                   [g.rt.drywt g-1.tot.drywt]  also [mol.C.rt mol-1.C.tot] current below ground biomass to total biomass fraction, calculated on the last growth interval
    if(TotSgCBm>0) then   !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation
      f_abv_cur = TotLfCBm/TotSgCBm
      f_blw_cur = TotRtCBm/TotSgCBm
    else                  !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation
      f_abv_cur = 0.0d0   !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation
      f_blw_cur = 0.0d0   !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation
    endif                 !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation

# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    ! p_sgrass(Nsg)               [m2.sg.hab m-2.grid]        (0~1)  Seagrass coverage (every species)
    ! p_sand                      [m2.sedmnt m-2.grid]        (0~1)  Sediment coverage
    ! [m2.hab/m2.sedmnt] = [m2.hab/m2.grid] [m2.grid/m2.sedmnt]
    if(p_sand>0.0d0) then
      sgrass_per_sedmnt = min( p_sgrass(species)/p_sand, 1.0d0 )
    else
      if(p_sgrass(species)>0.0d0) then
        sgrass_per_sedmnt = 1.0d0
      else
        sgrass_per_sedmnt = 0.0d0
      endif
    endif
    ! root_comp_dist_factor(Nsed) [g.rt.comp g-1.tot m2.sg.hab m-2.sedmnt mm-1]    root compartemtent distribution factor equal to f_blw_cur * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 
    ! f_blw_cur                   [g.rt.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current below ground biomass to total biomass fraction, calculated on the last growth interval
    ! root_mass_profile(sed)      []                          Mass ratio of roots in each layer (total must sum to 1.0)
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
    ! [g.rt.comp/g.tot][m2.hab/m2.sedmnt mm] = [g.rt/g.tot] [g.rt.comp/g.rt]       [m2.hab/m2.sedmnt]  [1/mm]                  
    root_comp_dist_factor(:)                 = f_blw_cur    * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 
# endif

    ! Isotope ratio or tracer mass ratio 
    !  rCHEM = rCHEM(isp)/rCHEM(iCHEMt) (no dim.)
    !                isp:    index for a specific isotope or tracer
    !                iCHEMt: index for a total CHEM concentration
    !  Iso_ratio(Ci,Ct) is a function that calculates the above ratio
    !  while avoiding zero division, which is included in mod_geochem.F

    DO isp=1,N_Csp
      rDIC_ocn(isp)  = Iso_ratio( ocean_DIC_conc(isp), ocean_DIC_conc(iCt) )
      rSgCBm  (isp)  = Iso_ratio( SgCBmF        (isp), SgCBmF        (iCt) )
      rDICstock(isp) = Iso_ratio( DICstock      (isp), DICstock      (iCt) )
      rCH2Ostock(isp)= Iso_ratio( CH2Ostock     (isp), CH2Ostock     (iCt) )
    ENDDO
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    DO isp=1,N_Nsp
      rSgNBm  (isp) = Iso_ratio( SgNBmF        (isp), SgNBmF        (iNt) )
      rNO3stock(isp)= Iso_ratio( NO3stock      (isp), NO3stock      (iNt) )
      rNH4stock(isp)= Iso_ratio( NH4stock      (isp), NH4stock      (iNt) )
    ENDDO
    DO isp=1,N_Psp
      rSgPBm  (isp) = Iso_ratio( SgPBmF        (isp), SgPBmF        (iPt) )
      rPO4stock(isp)= Iso_ratio( PO4stock      (isp), PO4stock      (iPt) )
    ENDDO
# endif
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    DO isp=1,N_Nsp
      rNO3_ocn(isp) = Iso_ratio( ocean_NO3_conc(isp), ocean_NO3_conc(iNt) )
      rNH4_ocn(isp) = Iso_ratio( ocean_NH4_conc(isp), ocean_NH4_conc(iNt) )
    ENDDO
    DO isp=1,N_Psp
      rPO4_ocn(isp) = Iso_ratio( ocean_PO4_conc(isp), ocean_PO4_conc(iPt) )
    ENDDO
# endif
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    DO k=1,Nsed
      DO isp=1,N_Csp
        rDIC_sed(isp,k) = Iso_ratio( sed_DIC(isp,k), sed_DIC(iCt,k) )
      ENDDO
    ENDDO
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    DO k=1,Nsed
      DO isp=1,N_Nsp
        rNO3_sed(isp,k) = Iso_ratio( sed_NO3(isp,k), sed_NO3(iCt,k) )
        rNH4_sed(isp,k) = Iso_ratio( sed_NH4(isp,k), sed_NH4(iNt,k) )
      ENDDO
      DO isp=1,N_Psp
        rPO4_sed(isp,k) = Iso_ratio( sed_PO4(isp,k), sed_PO4(iPt,k) )
      ENDDO
    ENDDO
# endif

! ====================================================================================================
!
! Uptake of chemicals from ocean and sediment
!
! ====================================================================================================

! DIC uptake rate -----------------------------------------------------------------------

    ! DIC_uptake_possible(:)    [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Lf_DIC_uptake_max         [umol.DIC g-1.lf.drywt s-1] Maximum uptake rate of DIC in the leaves
    ! LfDryBm                   [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
    ! ocean_DIC_conc            [umol.DIC L-1]              DIC concentration of ocean water 
    ! k_lf_DIC                  [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
    ocnDIC = max(ocean_DIC_conc(iCt),0.0d0)
    ! [umol.DIC/m2.hab s]  = [umol.DIC/g.lf s] [g.lf/m2.hab] ( Michaelis-Menten term                      )
    DIC_uptake_possible(0) = Lf_DIC_uptake_max * LfDryBm     * ocnDIC / (k_lf_DIC + ocnDIC)
    ! [umol.DIC/m2.hab s]  = min( [umol.DIC/m2.hab s]   ,      [umol.DIC/L][m/s] (1000L/m3)
    DIC_uptake_possible(0) = min( DIC_uptake_possible(0), 0.5d0*ocnDIC     *dz/dt*1000.0d0 ) ! first order interpolation at low concentration limit to prevent overdraw

# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! DIC_uptake_possible(:)      [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Rt_DIC_flux_max             [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    ! root_mass_profile(:)        []                          Mass ratio of roots in each layer (total must sum to 1.0)
    ! RtDryBm                     [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! sed_DIC(Nsed)               [umol.DIC L-1]              DIC consentration of sediment (in each sediment layer)
    ! k_rt_DIC                    [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
    do k=1,Nsed
      sedDIC = max(sed_DIC(iCt,k),0.0d0) !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation
      ! [umol.DIC/m2.hab s]  = [umol.DIC/g.rt s] []                     [g.rt/m2.hab] []
      DIC_uptake_possible(k) = Rt_DIC_flux_max   * root_mass_profile(k) * RtDryBm     &
        * ( sedDIC / (k_rt_DIC + sedDIC ) )
      ! [umol.DIC/m2.hab s]       = min( [umol.DIC/m2.hab s]        ,      [umol.DO/L] [mm                   /s](1000L/m3)(m/1000mm)
      DIC_uptake_possible(k) = min( DIC_uptake_possible(k), 0.5d0*sedDIC*sed_layer_thickness(k)/dt) ! first order interpolation at low concentration limit to prevent overdraw
    end do
# endif

    ! DIC_uptake_possible(:)      [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! sumCUpPos                   [umol.DIC m-2.sg.hab s-1]   sum of all potential DIC uptake (used for scaling/normalizing uptake proportions)
    ! DICstockCap                 []                          Factor to multiply carbon biomsass to get maximum DIC stock (stock limit = DICstockCap*SgCBmF) 
    sumCUpPos = sum(DIC_uptake_possible(:))

# if defined SEAGRASS_DEBUG_MODE
    ! if DIC uptake in a single time step exceeds the potential stockpile size, we need to increase the stock cap
    if (sumCUpPos*dt .gt. DICstockCap*SgCBmF(iCt)) then
      write(*,*) "yt_debug:     DICstockCap too low.     DIC_uptake_possible = ", sumCUpPos, "dt", dt, &
              "DICstockCap*SgCBmF = ", DICstockCap*SgCBmF(iCt)
      error stop
    endif
# endif

    ! limit uptake so that DIC stock is limited to stockpile cap
    if (DICstock(iCt) .ge. DICstockCap*SgCBmF(iCt)) then
      DIC_uptake_possible(:) = 0d0
      sumCUpPos = 0d0
    elseif (DICstock(iCt) + sumCUpPos*dt .gt. DICstockCap*SgCBmF(iCt)) then
      DIC_uptake_possible(:) = DIC_uptake_possible(:) * (DICstockCap*SgCBmF(iCt)-DICstock(iCt))/(sumCUpPos*dt)
      sumCUpPos = (DICstockCap*SgCBmF(iCt)-DICstock(iCt))/dt
    endif

! DO uptake rate --------------------------------------------------------------------------

    ! DO_uptake_possible(:)       [umol.DO m-2.sg.hab s-1]    DO uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Lf_DO_uptake_max            [umol.DO g-1.lf.drywt s-1]  Maximum uptake rate of DO in the leaves
    ! LfDryBm                     [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
    ! ocean_DO__conc              [umol.DO L-1]               DIC concentration of ocean water 
    ! k_lf_DO                     [umol.DO L-1]               Half saturation consentration of DO in water column for respiration

    ocnDO = max(ocean_DO__conc,0.0d0)

    ! [umol.DO/m2.hab s]  = [umol.DO/g.lf s] [g.lf/m2.hab] ( Michaelis-Menten term                     )
    DO_uptake_possible(0) = Lf_DO_uptake_max * LfDryBm     * ocnDO / (k_lf_DO + ocnDO)
    ! [umol.DO/m2.hab s]  = min( [umol.DO/m2.hab s]   ,      [umol.DO/L] [m/s](1000L/m3)
    DO_uptake_possible(0) = min( DO_uptake_possible(0), 0.5d0*ocnDO     *dz/dt*1000.0d0 ) ! first order interpolation at low concentration limit to prevent overdraw

# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! DO_uptake_possible(:)       [umol.DO m-2.sg.hab s-1]    DO uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Rt_DO_flux_max              [umol.DO g-1.rt.drywt s-1]  Maximum uptake rate of DO in the roots 
    ! RtDryBm                     [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! sed_DO(Nsed)                [umol.DO L-1]               DO consentration of sediment (in each sediment layer)
    ! k_rt_DO                     [umol.DO L-1]               Half saturation consentration of DO in soil pore water for growth

    ! [umol.DO/m2.hab s]       = [umol.DIC/g.rt s] []                     [g.rt/m2.hab] []
    DO_uptake_possible(1:Nsed) = Rt_DO_flux_max    * root_mass_profile(:) * RtDryBm     &
      * ( sed_DO(:) / (k_rt_DO + sed_DO(:)) )
    ! [umol.DO/m2.hab s]       = min( [umol.DO/m2.hab s]        ,      [umol.DO/L][mm                   /s](1000L/m3)(m/1000mm)
    DO_uptake_possible(1:Nsed) = min( DO_uptake_possible(1:Nsed), 0.5d0*sed_DO(:)*sed_layer_thickness(:)/dt) ! first order interpolation at low concentration limit to prevent overdraw
# endif

    ! DO_uptake_possible(:)       [umol.DO m-2.sg.hab s-1]    DO uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! sumOUpPos                   [umol.DO m-2.sg.hab s-1]    sum of all potential DO uptake (used for scaling/normalizing uptake proportions)
    ! DOstockCap                  []                          Factor to multiply carbon biomsass to get maximum DO stock (stock limit = DOstockCap*SgCBmF) 
    sumOUpPos = sum(DO_uptake_possible(:))

# if defined SEAGRASS_DEBUG_MODE
    ! if DO uptake in a single time step exceeds the potential stockpile size, we need to increase the stock cap
    if (sumOUpPos*dt .gt. DOstockCap*SgCBmF(iCt)) then
      write(*,*) "yt_debug:     DOstockCap too low.     DO_uptake_possible = ", sumOUpPos, "dt", dt, &
              "DOstockCap*SgCBmF = ", DOstockCap*SgCBmF(iCt)
      error stop
    endif
# endif

    ! limit uptake so that DO stock is limited to 100 percent of biomass C
    if (DOstock .ge. DOstockCap*SgCBmF(iCt)) then
      DO_uptake_possible(:) = 0d0
      sumOUpPos = 0d0
    elseif (DOstock + sumOUpPos*dt .gt. DOstockCap*SgCBmF(iCt)) then
      ! new amount added/dt = old amount added/dt     amount that can be added        / old amount added
      DO_uptake_possible(:) = DO_uptake_possible(:) * (DOstockCap*SgCBmF(iCt)-DOstock)/(sumOUpPos*dt)
      sumOUpPos = (DOstockCap*SgCBmF(iCt)-DOstock) / dt
    endif

# if defined SEAGRASS_DEBUG_MODE
    if (DO_uptake_possible(0) .lt. 0.0d0) then
      write(*,*) 'yt_debug: negative value in DO_uptake_possible(0)'
      write(*,*) 'yt_debug: DO_uptake_possible(0) = ', DO_uptake_possible(0), 'SgCBmF(iCt) = ', SgCBmF(iCt), 'DOstock = ', DOstock &
              , 'ocean_DO__conc = ', ocean_DO__conc, 'ocnDO = ', ocnDO
      error stop
    endif
    if (sum(DO_uptake_possible) .lt. 0.0d0) then
      write(*,*) 'yt_debug: negative value in sum(DO_uptake_possible)'
      write(*,*) 'yt_debug: sum(DO_uptake_possible) = ', sum(DO_uptake_possible)
      error stop
    endif
# endif

! Nutrient uptake rate --------------------------------------------------------------------------

# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    ! CHM_uptake_possible(:)    [umol.CHM m-2.sg.hab s-1]   Chemical (CHM) uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Lf_CHM_uptake_max         [umol.CHM g-1.lf.drywt s-1] Maximum uptake rate of CHM in the leaves
    ! LfDryBm                   [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
    ! ocean_CHM_conc            [umol.CHM L-1]              CHM concentration of ocean water 
    ! k_lf_CHM                  [umol.CHM L-1]              Half saturation consentration of CHM in water column for growth
    ! smin_lf_CHM               [umol.NO3 L-1]              Minimum consentration of CHM in water column for uptake
    ! Lf_CHM_ratio_slope        [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    ! ocnN                      [umol.N L-1]                NO3 + NH4 concentration of ocean water
    ocnNO3 = max( ocean_NO3_conc(iNt), 0.0d0 ) !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation
    ocnNH4 = max( ocean_NH4_conc(iNt), 0.0d0 ) !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation
    ocnN = ocnNO3 + ocnNH4
    ocnPO4 = max( ocean_PO4_conc(iPt), 0.0d0 ) !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation
    ! [umol.CHM/m2.hab s]  = [umol.CHM/g.lf s] (                                              ) [g.lf/m2.hab] ( Michaelis-Menten term                      )
    NO3_uptake_possible(0) = Lf_NO3_uptake_max * (Lf_NO3_ratio_slope*(ocnNO3/ocnN-0.5d0)+1.0d0) * LfDryBm     &
      * (ocnNO3-smin_lf_NO3) / (k_lf_NO3 + ocnNO3-smin_lf_NO3)
    NH4_uptake_possible(0) = Lf_NH4_uptake_max * (Lf_NH4_ratio_slope*(ocnNH4/ocnN-0.5d0)+1.0d0) * LfDryBm     &
      * (ocnNH4-smin_lf_NH4) / (k_lf_NH4 + ocnNH4-smin_lf_NH4)
    PO4_uptake_possible(0) = Lf_PO4_uptake_max                                                  * LfDryBm     &
      * (ocnPO4-smin_lf_PO4) / (k_lf_PO4 + ocnPO4-smin_lf_PO4)
    NO3_uptake_possible(0) = max( NO3_uptake_possible(0), 0.0d0) ! prevet negative uptake rates
    NH4_uptake_possible(0) = max( NH4_uptake_possible(0), 0.0d0) ! prevet negative uptake rates
    PO4_uptake_possible(0) = max( PO4_uptake_possible(0), 0.0d0) ! prevet negative uptake rates
    ! [umol.CHM/m2.hab s]  = min( [umol.CHM/m2.hab s]   ,      [umol.CHM/L][m/s] (1000L/m3)
    NO3_uptake_possible(0) = min( NO3_uptake_possible(0), 0.5d0*ocnNO3     *dz/dt*1000.0d0 ) ! first order interpolation at low concentration limit to prevent overdraw
    NH4_uptake_possible(0) = min( NH4_uptake_possible(0), 0.5d0*ocnNH4     *dz/dt*1000.0d0 ) ! first order interpolation at low concentration limit to prevent overdraw
    PO4_uptake_possible(0) = min( PO4_uptake_possible(0), 0.5d0*ocnPO4     *dz/dt*1000.0d0 ) ! first order interpolation at low concentration limit to prevent overdraw
# endif

# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! CHM_uptake_possible(:)      [umol.CHM m-2.sg.hab s-1]   Chemical (CHM) uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Rt_CHM_flux_max             [umol.CHM g-1.rt.drywt s-1] Maximum uptake rate of CHM in the roots 
    ! RtDryBm                     [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! sed_CHM(Nsed)               [umol.CHM L-1]              CHM consentration of sediment (in each sediment layer)
    ! k_rt_CHM                    [umol.CHM L-1]              Half saturation consentration of CHM in soil pore water for growth
    ! smin_rt_CHM                 [umol.CHM L-1]              Minimum consentration of CHM in soil pore water for uptake
    ! Rt_CHM_ratio_slope          [ ]                         Slope of nitrogen species ratio term to multiply michaelis-menten
    ! sedN                        [umol/L]                    NO3 + NH4 concentration of sediment water
    do k=1,Nsed !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation
      sedNO3 = max(sed_NO3(iNt,k),0.0d0) !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation
      sedNH4 = max(sed_NH4(iNt,k),0.0d0) !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation
      sedN = sedNO3 + sedNH4
      sedPO4 = max(sed_PO4(iPt,k),0.0d0) !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation
      ! [umol.CHM/m2.hab s]       = [umol.CHM/g.rt s] []                     (                                                      ) [g.rt/m2.hab] []
      NO3_uptake_possible(k) = Rt_NO3_flux_max   * root_mass_profile(k) &
        * (Rt_NO3_ratio_slope*(sedNO3/sedN-0.5d0)+1.0d0) * RtDryBm      &
        * ( (sedNO3-smin_rt_NO3) / (k_rt_NO3 + sedNO3-smin_rt_NO3) )
      NH4_uptake_possible(k) = Rt_NH4_flux_max   * root_mass_profile(k) &
        * (Rt_NH4_ratio_slope*(sedNH4/sedN-0.5d0)+1.0d0) * RtDryBm      &
        * ( (sedNH4-smin_rt_NH4) / (k_rt_NH4 + sedNH4-smin_rt_NH4) )
      PO4_uptake_possible(k) = Rt_PO4_flux_max   * root_mass_profile(k) * RtDryBm   &
        * ( (sedPO4-smin_rt_PO4) / (k_rt_PO4 + sedPO4-smin_rt_PO4) )
      NO3_uptake_possible(k) = max( NO3_uptake_possible(k), 0.0d0) ! prevet negative uptake rates
      NH4_uptake_possible(k) = max( NH4_uptake_possible(k), 0.0d0) ! prevet negative uptake rates
      PO4_uptake_possible(k) = max( PO4_uptake_possible(k), 0.0d0) ! prevet negative uptake rates
      ! [umol.CHM/m2.hab s]  = min( [umol.CHM/m2.hab s]   ,      [umol.CHM/L][mm                    /s] (1000L/m3)(m/1000mm)
      NO3_uptake_possible(k) = min( NO3_uptake_possible(k), 0.5d0*sedNO3     *sed_layer_thickness(k)/dt) ! first order interpolation at low concentration limit to prevent overdraw
      NH4_uptake_possible(k) = min( NH4_uptake_possible(k), 0.5d0*sedNH4     *sed_layer_thickness(k)/dt) ! first order interpolation at low concentration limit to prevent overdraw
      PO4_uptake_possible(k) = min( PO4_uptake_possible(k), 0.5d0*sedPO4     *sed_layer_thickness(k)/dt) ! first order interpolation at low concentration limit to prevent overdraw
      ! write(*,*) 'yt_debug: j = ', j, ( sed_PO4(1) )
    end do !!! <<<<<<<<<<<<<<<<< TN: added for safe calculation
# endif

# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! sumNUpPos                   [umol.N m-2.sg.hab s-1]     sum of all potential nitrogen uptake (used for scaling/normalizing uptake proportions)
    ! sumPUpPos                   [umol.P m-2.sg.hab s-1]     sum of all potential phosphorus uptake (used for scaling/normalizing uptake proportions)
    ! CHEM_uptake_possible(:)     [umol.CHEM m-2.sg.hab s-1]  CHEM uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    sumNO3UpPos = sum(NO3_uptake_possible(:))
    sumNH4UpPos = sum(NH4_uptake_possible(:))
#  if defined SEAGRASS_IMMEDIATE_NO3_TO_NH4_CONVERSION
    sumNUpPos = sumNO3UpPos + sumNH4UpPos
#  endif
    sumPUpPos = sum(PO4_uptake_possible(:))

# if defined SEAGRASS_DEBUG_MODE
    ! if chem uptake in a single time step exceeds the potential stockpile size, we need to increase the stock cap
#  if defined SEAGRASS_IMMEDIATE_NO3_TO_NH4_CONVERSION
#  else
    if (sumNO3UpPos*dt .gt. NO3stockCap*SgNBmF(iNt)) then
      write(*,*) "yt_debug:     NO3stockCap too low.     NO3_uptake_possible = ", sumNO3UpPos, "dt", dt, &
              "NO3stockCap*SgNBmF = ", NO3stockCap*SgNBmF(iNt)
      error stop
    endif
#  endif
    if (sumNH4UpPos*dt .gt. NH4stockCap*SgNBmF(iNt)) then
      write(*,*) "yt_debug:     NH4stockCap too low.     NH4_uptake_possible = ", sumNH4UpPos, "dt", dt, &
              "NH4stockCap*SgNBmF = ", NH4stockCap*SgNBmF(iNt)
      error stop
    endif
    if (sumPUpPos*dt .gt. PO4stockCap*SgPBmF(iPt)) then
      write(*,*) "yt_debug:     PO4stockCap too low.     PO4_uptake_possible = ", sumPUpPos, "dt", dt, &
              "PO4stockCap*SgPBmF = ", PO4stockCap*SgPBmF(iPt)
      error stop
    endif
# endif

# if defined SEAGRASS_IMMEDIATE_NO3_TO_NH4_CONVERSION

    ! NH4stockCap                 []                          Factor to multiply nitrogen biomsass to get maximum NH4 stock (stock limit = NH4stockCap*SgNBmF) 
    ! limit uptake so that NH4 stock is limited to 100 percent of biomass N
    if (NH4stock(iNt) .ge. NH4stockCap*SgNBmF(iNt)) then
      NH4_uptake_possible(:) = 0d0
      NO3_uptake_possible(:) = 0d0
      sumNH4UpPos = 0d0
      sumNO3UpPos = 0d0
      sumNUpPos   = 0d0
    elseif (NH4stock(iNt) + sumNUpPos*dt .gt. NH4stockCap*SgNBmF(iNt)) then
      NH4_uptake_possible(:) = NH4_uptake_possible(:) * (NH4stockCap*SgNBmF(iNt)-NH4stock(iNt))/(sumNH4UpPos*dt) &
              * (sumNH4UpPos / sumNUpPos)
      NO3_uptake_possible(:) = NO3_uptake_possible(:) * (NH4stockCap*SgNBmF(iNt)-NH4stock(iNt))/(sumNH4UpPos*dt) &
              * (sumNO3UpPos / sumNUpPos)
      sumNH4UpPos = (NH4stockCap*SgNBmF(iNt)-NH4stock(iNt))/dt * (sumNH4UpPos / sumNUpPos)
      sumNO3UpPos = (NH4stockCap*SgNBmF(iNt)-NH4stock(iNt))/dt * (sumNO3UpPos / sumNUpPos)
    endif

# else

    ! NO3stockCap                 []                          Factor to multiply nitrogen biomsass to get maximum NO3 stock (stock limit = NO3stockCap*SgNBmF) 
    ! limit uptake so that NO3 stock is limited to 100 percent of biomass N
    if (NO3stock(iNt) .ge. NO3stockCap*SgNBmF(iNt)) then
      NO3_uptake_possible(:) = 0d0
      sumNO3UpPos = 0d0
    elseif (NO3stock(iNt) + sumNO3UpPos*dt .gt. NO3stockCap*SgNBmF(iNt)) then
      NO3_uptake_possible(:) = NO3_uptake_possible(:) * (NO3stockCap*SgNBmF(iNt)-NO3stock(iNt))/(sumNO3UpPos*dt)
      sumNO3UpPos = (NO3stockCap*SgNBmF(iNt)-NO3stock(iNt))/dt
    endif

    ! NH4stockCap                 []                          Factor to multiply nitrogen biomsass to get maximum NH4 stock (stock limit = NH4stockCap*SgNBmF) 
    ! limit uptake so that NH4 stock is limited to 100 percent of biomass N
    if (NH4stock(iNt) .ge. NH4stockCap*SgNBmF(iNt)) then
      NH4_uptake_possible(:) = 0d0
      sumNH4UpPos = 0d0
    elseif (NH4stock(iNt) + sumNH4UpPos*dt .gt. NH4stockCap*SgNBmF(iNt)) then
      NH4_uptake_possible(:) = NH4_uptake_possible(:) * (NH4stockCap*SgNBmF(iNt)-NH4stock(iNt))/(sumNH4UpPos*dt)
      sumNH4UpPos = (NH4stockCap*SgNBmF(iNt)-NH4stock(iNt))/dt
    endif

# endif

#   if defined SEAGRASS_DEBUG_MODE
    DO k=1,Nsed
      if (NO3_uptake_possible(k) .lt. 0.0d0) then
        write(*,*) 'yt_debug: negative value in NO3_uptake_possible(k)'
        write(*,*) 'yt_debug: NO3_uptake_possible(k) = ', NO3_uptake_possible(k)
        error stop
      endif
    ENDDO
#   endif

    ! PO4stockCap                 []                          Factor to multiply phosphorus biomsass to get maximum PO4 stock (stock limit = PO4stockCap*SgPBmF) 
    ! limit uptake so that PO4 stock is limited to 100 percent of biomass P
    if (PO4stock(iPt) .ge. PO4stockCap*SgPBmF(iPt)) then
      PO4_uptake_possible(:) = 0d0
      sumPUpPos = 0d0
    elseif (PO4stock(iPt) + sumPUpPos*dt .gt. PO4stockCap*SgPBmF(iPt)) then
      PO4_uptake_possible(:) = PO4_uptake_possible(:) * (PO4stockCap*SgPBmF(iPt)-PO4stock(iPt))/(sumPUpPos*dt)
      sumPUpPos = (PO4stockCap*SgPBmF(iPt)-PO4stock(iPt))/dt
    endif

    sumNUpPos = sumNO3UpPos + sumNH4UpPos

# endif

! Uptake to stock mass balance --------------------------------------------------------------------------

  ! CHEM_uptake_rate_from_ocean [mmol.CHEM m-2.sg.hab s-1]  direction of ocean to seagrass is positive
  ! delta_CHEMstock(N_Xsp)      [umol.CHEM m-2.sg.hab s-1]  Rate of change in CHEMstock
  ! CHEM_uptake_possible(:)     [umol.CHEM m-2.sg.hab s-1]  CHEM uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
  ! aC_phot()                   []                          Carbon isotope fractionation from mod_geochem
  ! rDIC_sed(N_Csp,Nsed)        [no dim.]                   Isotope ratio or tracer mass ratio

  ! SEDECO(ng)%sg_dCHEM_dt()    [umol.CHEM L-1 s-1]         Rate of change in CHEM due to flux from roots to sediment (positive means seagrass to sediment)
  ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
  ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
  ! aC_phot()                   []                          Carbon isotope fractionation from mod_geochem
  ! rDIC_sed(N_Csp,Nsed)        [no dim.]                   Isotope ratio or tracer mass ratio

if (sumCUpPos .gt. 0d0) then
  ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          + [umol.CHEM/m2.hab s]   (mmol/1000umol) (isotope ratio)
  DIC_uptake_rate_from_ocean(:) = DIC_uptake_rate_from_ocean(:) + DIC_uptake_possible(0) * 0.001d0       * rDIC_ocn(:)
  ! [umol.CHEM/m2.hab s] = [umol.CHEM/m2.hab s] + [umol.CHEM/m2.hab s]   (isotope ratio)
  delta_DICstock(:)      = delta_DICstock(:)    + DIC_uptake_possible(0) * rDIC_ocn(:)
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
  do k=1,Nsed
    ! [umol.CHEM/L s]                      = [umol.CHEM/L s]                        - [umol.CHEM/m2.hab s]   [m2.hab/m2.sedmnt]  [1/mm] (1000mm/m)(m3/1000L) (isotope ratio)
    SEDECO(ng)%sg_dDIC_dt(:,k,species,i,j) = SEDECO(ng)%sg_dDIC_dt(:,k,species,i,j) - DIC_uptake_possible(k) &
            * sgrass_per_sedmnt / sed_layer_thickness(k)    * rDIC_sed(:,k)
    ! [umol.CHEM/m2.hab s] = [umol.CHEM/m2.hab s] + [umol.CHEM/m2.hab s]   (isotope ratio)
    delta_DICstock(:)      = delta_DICstock(:)    + DIC_uptake_possible(k) * rDIC_sed(:,k)
  end do
# endif
endif

if (sumOUpPos .gt. 0d0) then
  ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          + [umol.CHEM/m2.hab s]   (mmol/1000umol) (isotope ratio)
  DO__uptake_rate_from_ocean    = DO__uptake_rate_from_ocean    + DO_uptake_possible(0)  * 0.001d0
  ! [umol.CHEM/m2.hab s] = [umol.CHEM/m2.hab s] + [umol.CHEM/m2.hab s]   (isotope ratio)
  delta_DOstock          = delta_DOstock        + DO_uptake_possible(0)
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
  do k=1,Nsed
    ! [umol.CHEM/L s]                      = [umol.CHEM/L s]                        - [umol.CHEM/m2.hab s]   [m2.hab/m2.sedmnt]  [1/mm] (1000mm/m)(m3/1000L) (isotope ratio)
    SEDECO(ng)%sg_dO2_dt(k,species,i,j)    = SEDECO(ng)%sg_dO2_dt(k,species,i,j)    - DO_uptake_possible(k)  &
            * sgrass_per_sedmnt / sed_layer_thickness(k)
    ! [umol.CHEM/m2.hab s] = [umol.CHEM/m2.hab s] + [umol.CHEM/m2.hab s]   (isotope ratio)
    delta_DOstock          = delta_DOstock        + DO_uptake_possible(k)
  end do
# endif
endif

# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
if (sumNO3UpPos .gt. 0d0) then
  ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          + [umol.CHEM/m2.hab s]   (mmol/1000umol) (isotope ratio)
  NO3_uptake_rate_from_ocean(:) = NO3_uptake_rate_from_ocean(:) + NO3_uptake_possible(0) * 0.001d0       &
          * aNO3_phot(1:N_Nsp)*rNO3_ocn(:)
#  if defined SEAGRASS_IMMEDIATE_NO3_TO_NH4_CONVERSION
  ! [umol.CHEM/m2.hab s] = [umol.CHEM/m2.hab s] +     [umol.CHEM/m2.hab s]   (isotope ratio)
  delta_NH4stock(:)      = delta_NH4stock(:)    +     NO3_uptake_possible(0) * aNO3_phot(1:N_Nsp)*rNO3_ocn(:)
  delta_DOstock          = delta_DOstock        + 2 * NO3_uptake_possible(0) ! for every mol of NO3 converted to NH4, 2 mol of DO is produced   NO3- + H+ + 2*H2O -> NH4+ + 2*O2 + OH-
#  else
  ! [umol.CHEM/m2.hab s] = [umol.CHEM/m2.hab s] + [umol.CHEM/m2.hab s]   (isotope ratio)
  delta_NO3stock(:)      = delta_NO3stock(:)    + NO3_uptake_possible(0) * aNO3_phot(1:N_Nsp)*rNO3_ocn(:)
#  endif
endif
if (sumNH4UpPos .gt. 0d0) then
  ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          + [umol.CHEM/m2.hab s]   (mmol/1000umol) (isotope ratio)
  NH4_uptake_rate_from_ocean(:) = NH4_uptake_rate_from_ocean(:) + NH4_uptake_possible(0) * 0.001d0       &
          * aNH4_phot(1:N_Nsp)*rNH4_ocn(:)
  ! [umol.CHEM/m2.hab s] = [umol.CHEM/m2.hab s] + [umol.CHEM/m2.hab s]   (isotope ratio)
  delta_NH4stock(:)      = delta_NH4stock(:)    + NH4_uptake_possible(0) * aNH4_phot(1:N_Nsp)*rNH4_ocn(:)
endif
if (sumPUpPos .gt. 0d0) then
  ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          + [umol.CHEM/m2.hab s]   (mmol/1000umol) (isotope ratio)
  PO4_uptake_rate_from_ocean(:) = PO4_uptake_rate_from_ocean(:) + PO4_uptake_possible(0) * 0.001d0       * rPO4_ocn(:)
  ! [umol.CHEM/m2.hab s] = [umol.CHEM/m2.hab s] + [umol.CHEM/m2.hab s]   (isotope ratio)
  delta_PO4stock(:)      = delta_PO4stock(:)    + PO4_uptake_possible(0) * rPO4_ocn(:)
endif
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
if (sumNO3UpPos .gt. 0d0) then
  do k=1,Nsed
    ! [umol.CHEM/L s]                      = [umol.CHEM/L s]                        - [umol.CHEM/m2.hab s]   [m2.hab/m2.sedmnt]  [1/mm] (1000mm/m)(m3/1000L) (isotope ratio)
    SEDECO(ng)%sg_dNO3_dt(:,k,species,i,j) = SEDECO(ng)%sg_dNO3_dt(:,k,species,i,j) - NO3_uptake_possible(k) &
            * sgrass_per_sedmnt / sed_layer_thickness(k)    * aNO3_phot(1:N_Nsp)*rNO3_sed(:,k)
#  if defined SEAGRASS_IMMEDIATE_NO3_TO_NH4_CONVERSION
    ! [umol.CHEM/m2.hab s] = [umol.CHEM/m2.hab s] +    [umol.CHEM/m2.hab s]   (isotope ratio)
    delta_NH4stock(:)      = delta_NH4stock(:)    +     NO3_uptake_possible(k) * aNO3_phot(1:N_Nsp)*rNO3_sed(:,k)
    delta_DOstock          = delta_DOstock        + 2 * NO3_uptake_possible(k) ! for every mol of NO3 converted to NH4, 2 mol of DO is produced   NO3- + H+ + 2*H2O -> NH4+ + 2*O2 + OH-
#  else
    ! [umol.CHEM/m2.hab s] = [umol.CHEM/m2.hab s] + [umol.CHEM/m2.hab s]   (isotope ratio)
    delta_NO3stock(:)      = delta_NO3stock(:)    + NO3_uptake_possible(k) * aNO3_phot(1:N_Nsp)*rNO3_sed(:,k)
#  endif
  end do
endif
if (sumNH4UpPos .gt. 0d0) then
  do k=1,Nsed
    ! [umol.CHEM/L s]                      = [umol.CHEM/L s]                        - [umol.CHEM/m2.hab s]   [m2.hab/m2.sedmnt]  [1/mm] (1000mm/m)(m3/1000L) (isotope ratio)
    SEDECO(ng)%sg_dNH4_dt(:,k,species,i,j) = SEDECO(ng)%sg_dNH4_dt(:,k,species,i,j) - NH4_uptake_possible(k) &
            * sgrass_per_sedmnt / sed_layer_thickness(k)    * aNH4_phot(1:N_Nsp)*rNH4_sed(:,k)
    ! [umol.CHEM/m2.hab s] = [umol.CHEM/m2.hab s] + [umol.CHEM/m2.hab s]   (isotope ratio)
    delta_NH4stock(:)      = delta_NH4stock(:)    + NH4_uptake_possible(k) * aNH4_phot(1:N_Nsp)*rNH4_sed(:,k)
  end do
endif
if (sumPUpPos .gt. 0d0) then
  do k=1,Nsed
    ! [umol.CHEM/L s]                      = [umol.CHEM/L s]                        - [umol.CHEM/m2.hab s]   [m2.hab/m2.sedmnt]  [1/mm] (1000mm/m)(m3/1000L) (isotope ratio)
    SEDECO(ng)%sg_dPO4_dt(:,k,species,i,j) = SEDECO(ng)%sg_dPO4_dt(:,k,species,i,j) - PO4_uptake_possible(k) &
            * sgrass_per_sedmnt / sed_layer_thickness(k)    * rPO4_sed(:,k)
    ! [umol.CHEM/m2.hab s] = [umol.CHEM/m2.hab s] + [umol.CHEM/m2.hab s]   (isotope ratio)
    delta_PO4stock(:)      = delta_PO4stock(:)    + PO4_uptake_possible(k) * rPO4_sed(:,k)
  end do
endif
# endif

! ====================================================================================================
!
! Photosynthesis
!
!   CO2    +   H2O  -> CH2O  +   O2      ! for every mol of CO2  consumed in photosynthesis: 1 mol of O2 is produced
!
! ====================================================================================================


! Effective leaf area projection ---------------------------------------------------------------------

    ! ELAP                      [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
    ! LAI                       [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area 
    ! k_proj                    [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient
ELAP = 1.0d0-exp(-1d0*k_proj*LAI)

! Photosynthesis rate light limit ---------------------------------------------------------------------

    ! Phot_limit_light          [umol.C m-2.sg.hab s-1]     Photosynthesis rate possible as limited by light
    ! P_max                     [umol.C m-2.leaf s-1]       Maximum photoynthesis rate
    ! ELAP                      [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
    ! PFD                       [umol.photon m-2 s-1]       Photon flux density
    ! I_k                       [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve

    ! [umol.C/m2.hab s] = [umol.C/m2.lf s] [m2.lf/m2.hab] [             ]
    Phot_limit_light  = P_max            * ELAP         * tanh(PFD/I_k)

# if defined SEAGRASS_DEBUG_MODE
    if (Phot_limit_light .lt. 0.0d0) then
      write(*,*) 'yt_debug: negative value in Phot_limit_light'
      write(*,*) 'yt_debug: Phot_limit_light = ', Phot_limit_light, 'ELAP = ', ELAP, 'PFD = ', PFD, 'I_k = ', I_k
      error stop
    endif
# endif

! DIC limit ---------------------------------------------------------------------

    ! Phot_limit_DIC            [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by DIC uptake

    ! [umol.C/m2.hab s] = [mol.C/mol.CHM] [umol.CHM/m2.hab] [1/s]
    Phot_limit_DIC   =                    DICstock(iCt)  / dt

# if defined SEAGRASS_DEBUG_MODE
    if (Phot_limit_DIC .lt. 0.0d0) then
      write(*,*) 'yt_debug: negative value in Phot_limit_DIC'
      write(*,*) 'yt_debug: Phot_limit_DIC = ', Phot_limit_DIC, 'DICstock(iCt) = ', DICstock(iCt)
      error stop
    endif
# endif

! Gross Photosythesis rate ---------------------------------------------------------------------

    ! Phot_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis rate
    ! Phot_limit_light          [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by light
    ! Phot_limit_DIC            [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by DIC uptake

    Phot_rate = min(Phot_limit_light, Phot_limit_DIC)

! Photosynthesis mass balance (move between stocks) ---------------------------------------------------------------------------------------
    
    ! remove from CO2 stock, add to O2 and CH2O stock
    ! delta_DICstock              [umol.DIC m-2.sg.hab s-1]   Rate of change in DICstock
    ! Phot_rate                   [umol.C m-2.sg.hab s-1]     Gross photosynthesis rate

    ! [umol.C/m2.hab s] = [umol.C/m2.hab s]  - [umol.C/m2.hab s] (fractionation)  (isotope ratios)
    delta_DICstock(:)   = delta_DICstock(:)  - Phot_rate         * aC_phot(1:N_Csp) * rDICstock(:)
    delta_DOstock       = delta_DOstock      + Phot_rate ! for every mol of CO2  consumed in growth: 1 mol of O2 is produced
    delta_CH2Ostock(:)  = delta_CH2Ostock(:) + Phot_rate         * aC_phot(1:N_Csp) * rDICstock(:)

! ====================================================================================================
!
! Growth
!
!   if x:y:z is the C:N:P ratio
!   (x/x)*CH2O + (y/x)*(NO3+NH4) + (z/y)*PO4 -> (1/x)*(CH2O)x(NH3)y(H3PO4)z + (some)*O2 +/- (some)*H+
!
!   CH2O            -> CH2O              ! for every mol of CO2  consumed in growth: 1 mol of O2 is produced
!   NO3-   +   H3O+ -> NH3   + 2*O2      ! for every mol of NO3- consumed in growth: 2 mol of O2 is produced
!   NH4+   +   H2O  -> NH3   +   H3O+                          
!   PO4^3- + 3*H3O+ -> H3PO4 + 3*H2O     
!
! ====================================================================================================

! Nutrient limit ---------------------------------------------------------------------

    ! Growth_limit_CH2O           [umol.C m-2.sg.hab s-1]     Growth rate possible as limited by CH2O stock
    ! Growth_limit_NO3            [umol.C m-2.sg.hab s-1]     Growth rate possible as limited by NO3 stock
    ! Growth_limit_NH4            [umol.C m-2.sg.hab s-1]     Growth rate possible as limited by NH4 stock
    ! Growth_limit_PO4            [umol.C m-2.sg.hab s-1]     Growth rate possible as limited by PO4 stock

    ! [umol.C/m2.hab s] = [mol.C/mol.CHM] [umol.CHM/m2.hab] [1/s]
    Growth_limit_CH2O   =                   CH2Ostock(iCt)  / dt
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    Growth_limit_NO3    = 1d0 / SgNperC   * NO3stock(iNt)   / dt
    Growth_limit_NH4    = 1d0 / SgNperC   * NH4stock(iNt)   / dt
    Growth_limit_PO4    = 1d0 / SgPperC   * PO4stock(iPt)   / dt
# endif

! Gross Photosythesis/Growth rate ---------------------------------------------------------------------

# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    Growth_rate = min(Growth_limit_CH2O, (Growth_limit_NO3+Growth_limit_NH4), Growth_limit_PO4)
# else
    Growth_rate = Growth_limit_CH2O
# endif

# if defined SEAGRASS_DEBUG_MODE
    if (Growth_rate .lt. -1d-5) then ! error handling
      write(*,*) "yt_debug: negative value error in growth. Time =", &
        date, "Position =", i, j, "Growth_rate = ", Growth_rate
      write(*,*) "yt_debug:     Growth_rate        = ", Growth_rate
      write(*,*) "yt_debug:     Growth_limit_CH2O   = ", Growth_limit_CH2O
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
      write(*,*) "yt_debug:     Growth_limit_NO3   = ", Growth_limit_NO3
      write(*,*) "yt_debug:     Growth_limit_NH4   = ", Growth_limit_NH4
      write(*,*) "yt_debug:     Growth_limit_PO4   = ", Growth_limit_PO4
# endif
      ! error stop
    endif
# endif
      Growth_rate = max(Growth_rate, 0.0d0)

! Growth mass balance (remove from stock, add to biomass) ---------------------------------------------------------------------------------------
    
# if defined SEAGRASS_DEBUG_MODE
    if (NO3stock(iNt) .lt. 0.0d0) then
      write(*,*) 'yt_debug: negative value in NO3stock(iNt)'
      write(*,*) 'yt_debug: NO3stock(iNt) = ', NO3stock(iNt)
      error stop
    endif
# endif

    ! remove from stock
    ! delta_DICstock              [umol.DIC m-2.sg.hab s-1]   Rate of change in DICstock
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate

    ! [umol.C/m2.hab s] = [umol.C/m2.hab s] - [umol.C/m2.hab s] (stoicheometry) (isotope ratios) (N-species proportion)
    delta_CH2Ostock(:)   = delta_CH2Ostock(:) - Growth_rate            * rCH2Ostock(:)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    if ((NO3stock(iNt)+NH4stock(iNt)) .gt. 0d0) then
      delta_NO3stock(:)   = delta_NO3stock(:) - Growth_rate  * SgNperC * rNO3stock(:)*(NO3stock(iNt)/(NO3stock(iNt)+NH4stock(iNt))) ! portion from nitrate proportional to stock
      delta_DOstock       = delta_DOstock     + 2*Growth_rate* SgNperC *              (NO3stock(iNt)/(NO3stock(iNt)+NH4stock(iNt))) ! for every mol of NO3- consumed in growth: 2 mol of O2 is produced   NO3- + H+ + 2*H2O -> NH4+ + 2*O2 + OH-
      delta_NH4stock(:)   = delta_NH4stock(:) - Growth_rate  * SgNperC * rNH4stock(:)*(NH4stock(iNt)/(NO3stock(iNt)+NH4stock(iNt))) ! portion from ammonium proportional to stock
    endif
    delta_PO4stock(:)   = delta_PO4stock(:) - Growth_rate    * SgPperC * rPO4stock(:)
#  endif

    ! growth produces biomass (add to biomass)
    ! delta_SgCBmF                [umol.C m-2.sg.hab s-1]     Rate of change in SgCBmF
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate

    ! [umol.C/m2.hab s] = [umol.C/m2.hab s] + [umol.C/m2.hab s] (stoicheometry) (isotope ratios) (N-species proportion)
    delta_SgCBmF(:)       = delta_SgCBmF(:)   + Growth_rate           * rCH2Ostock(:)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    if ((NO3stock(iNt)+NH4stock(iNt)) .gt. 0d0) then
      delta_SgNBmF(:)     = delta_SgNBmF(:)   + Growth_rate * SgNperC * rNO3stock(:)*(NO3stock(iNt)/(NO3stock(iNt)+NH4stock(iNt))) ! portion from nitrate proportional to stock
      delta_SgNBmF(:)     = delta_SgNBmF(:)   + Growth_rate * SgNperC * rNH4stock(:)*(NH4stock(iNt)/(NO3stock(iNt)+NH4stock(iNt))) ! portion from ammonium proportional to stock
    endif
    delta_SgPBmF(:)       = delta_SgPBmF(:)   + Growth_rate * SgPperC * rPO4stock(:)
#  endif

! ====================================================================================================
!
! Respiration/Un-Growth
!
!   if x:y:z is the C:N:P ratio
!   (1/x)*(CH2O)x(NH3)y(H3PO4)z + (some)*O2 +/- (some)*H+ -> (x/x)*CO2 + (y/x)*(NO3+NH4) + (z/y)*PO4
!
!   CH2O  +   O2   -> CO2    +   H2O     ! for every mol of respiration: 1 mol of O2 is consumed
!   NH3   +   H3O+ -> NH4+   +   H2O                          
!   H3PO4 + 3*H2O  -> PO4^3- + 3*H3O+   
!
! ====================================================================================================

! Respiration possible as limited by DO -------------------------------------------------------------------------------

    ! Resp_limit_DO               [umol.C m-2.sg.hab s-1]     Respiration/un-growth rate possible as limited by DO uptake

    ! [umol.C/m2.hab s] = [mol.C/mol.CHM] [umol.CHM/m2.hab] [1/s]
    Resp_limit_DO       =                 (DOstock+delta_DOstock*dt) / dt

! Respiration/ungrowth -------------------------------------------------------------------------------

    ! Resp_rate                   [umol.C m-2.sg.hab s-1]     Gross respiration/un-growth rate
    ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    ! R_basal                     [umol.C g-1.leaf s-1]       Maximum respiration rate
    ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    ! dt                          [s]                         time step duration
    ! CH2Ostock                   [umol.CH2O m-2.sg.hab]      Seagrass internal sugar stock per unit ground area (in seagrass habitat area)
    ! CH2Orequired                [umol.CH2O m-2.sg.hab]      sugar required in respiration
    ! SgCBmF_toMaintain           [umol.C m-2.sg.hab]         seagrass biomass to maintain by respiration
    ! Ungrowth_rate               [umol.C m-2.sg.hab s-1]     rate at which biomass is metabolized for respiration when DO stock is insufficient
    
    ! [umol.C/m2.hab s] = [umol.C/g s] [g/mol.C] [umol.C/m2.hab]       (mol/1000000umol)
    ! Resp_limit_DO     = R_basal      * SgMperC * SgCBmF_maintainable * 0.000001

    ! [umol.C/m2.hab]     = [umol.C/m2.hab s] [g s/umol.C] [mol.C/g] (1000000umol/mol)
    ! SgCBmF_maintainable = Resp_limit_DO     / R_basal    / SgMperC * 1000000d0

    SgCBmF_toMaintain = min( SgCBmF(iCt), (Resp_limit_DO/R_basal/SgMperC*1000000d0) )

    anoxic_dieoff = SgCBmF(iCt) - SgCBmF_toMaintain

# if defined SEAGRASS_DEBUG_MODE
    if (SgCBmF_toMaintain .lt. -1d-5) then ! error handling
      write(*,*) "yt_debug: negative value error in respiration rate. Time =", &
              date, "Position =", i, j
      write(*,*) "yt_debug:     SgCBmF_toMaintain = ", SgCBmF_toMaintain
      write(*,*) "yt_debug:     SgCBmF            = ", SgCBmF(iCt)
      write(*,*) "yt_debug:     Resp_limit_DO     = ", Resp_limit_DO
      ! error stop
    endif
# endif
    SgCBmF_toMaintain = max(SgCBmF_toMaintain, 0d0)

    ! [umol.C/m2.hab] = [umol.C/g s] [g/mol.C] [umol.C/m2.hab]     [s]  (mol/1000000umol)
    CH2Orequired      = R_basal      * SgMperC * SgCBmF_toMaintain * dt * 0.000001

    ! [umol.C/m2.hab s] = [umol.C/m2.hab] [1/s]
    Resp_rate           = CH2Orequired    / dt

    if (CH2Ostock(iCt)+delta_CH2Ostock(iCt)*dt .ge. CH2Orequired) then
      ! respired_mass = 0d0
      delta_CH2Ostock(:) = delta_CH2Ostock(:) - Resp_rate * rCH2Ostock(:)
      delta_DICstock(:)  = delta_DICstock(:)  + Resp_rate * rCH2Ostock(:)
      delta_DOstock      = delta_DOstock      - Resp_rate

# if defined SEAGRASS_DEBUG_MODE
      if (CH2Ostock(iCt)+delta_CH2Ostock(iCt)*dt .lt. -1d-5) then
        write(*,*) "yt_debug:     CH2Ostock = ", CH2Ostock(iCt), &
                "delta_CH2Ostock = ", delta_CH2Ostock(iCt), "CH2Orequired", CH2Orequired
        ! error stop
      endif
# endif

# if defined SEAGRASS_DEBUG_MODE
      if (DOstock + delta_DOstock*dt .lt. 0.0d0) then
        write(*,*) 'yt_debug: negative value in (DOstock + delta_DOstock*dt) after respiration calculation'
        write(*,*) 'yt_debug: (DOstock + delta_DOstock*dt) = ', (DOstock + delta_DOstock*dt), 'Resp_rate = ', Resp_rate
        error stop
      endif
# endif
      
    else ! if CH2Oavailable < CH2Orequired
      delta_DICstock(:)  = delta_DICstock(:)  + (CH2Ostock(:)+delta_CH2Ostock(:)*dt) / dt
      delta_DOstock      = delta_DOstock      - (CH2Ostock(iCt)+delta_CH2Ostock(iCt)*dt) / dt
      delta_CH2Ostock(:) = delta_CH2Ostock(:) - (CH2Ostock(:)+delta_CH2Ostock(:)*dt) / dt

# if defined SEAGRASS_DEBUG_MODE
      if (CH2Ostock(iCt)+delta_CH2Ostock(iCt)*dt .lt. -1d-5) then
        write(*,*) "yt_debug:     CH2Ostock = ", CH2Ostock(iCt), &
                "delta_CH2Ostock = ", delta_CH2Ostock(iCt)
        ! error stop
      endif
# endif

# if defined SEAGRASS_DEBUG_MODE
      if (DOstock + delta_DOstock*dt .lt. 0.0d0) then
        write(*,*) 'yt_debug: negative value in (DOstock + delta_DOstock*dt) after respiration calculation'
        write(*,*) 'yt_debug: (DOstock + delta_DOstock*dt) = ', (DOstock + delta_DOstock*dt), 'Resp_rate = ', Resp_rate
        error stop
      endif
# endif

      ! massUnsupported  = SgCBmF(iCt) - CH2Ostock(iCt)
    
      ! []         [umol.C g-1 s-1] [g mol-1.C] [s] (mol/1000000umol)
      ! respFrac = R_basal          *SgMperC    *dt *0.000001

      ! original_mass = massUnsupported

      ! original_mass = respired_mass + remaining_mass
      ! respired_mass = respFrac * remaining_mass

      ! original_mass = (respFrac * remaining_mass) + remaining_mass
      ! original_mass = remaining_mass * (respFrac + 1)
      ! remaining_mass = (respFrac + 1)/original_mass

      ! original_mass = respired_mass + respired_mass/respFrac
      ! original_mass = respired_mass * (1 + 1/respFrac)
      ! respired_mass = original_mass / (1 + 1/respFrac)
      ! respired_mass = original_mass / ((respFrac + 1)/respFrac)
      ! respired_mass = original_mass * respFrac / (respFrac + 1)

      ! ungrowth_rate = respired_mass / dt

      Ungrowth_rate = (SgCBmF(iCt) - CH2Ostock(iCt)) * (R_basal*SgMperC*dt*0.000001)/(R_basal*SgMperC*dt*0.000001 + 1) / dt

      delta_SgCBmF(:)    = delta_SgCBmF(:)    - Ungrowth_rate           * rSgCBm(:)
      delta_DICstock(:)  = delta_DICstock(:)  + Ungrowth_rate           * rSgCBm(:)
      delta_DOstock      = delta_DOstock      - Ungrowth_rate
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
      delta_SgNBmF(:)    = delta_SgNBmF(:)    - Ungrowth_rate * SgNperC * rSgNBm(:)
      delta_NH4stock(:)  = delta_NH4stock(:)  + Ungrowth_rate * SgNperC * rSgNBm(:)
      delta_SgPBmF(:)    = delta_SgPBmF(:)    - Ungrowth_rate * SgPperC * rSgPBm(:)
      delta_PO4stock(:)  = delta_PO4stock(:)  + Ungrowth_rate * SgPperC * rSgPBm(:)
#  endif

# if defined SEAGRASS_DEBUG_MODE
      if (DOstock + delta_DOstock*dt .lt. 0.0d0) then
        write(*,*) 'yt_debug: negative value in (DOstock + delta_DOstock*dt) after ungrowth calculation'
        write(*,*) 'yt_debug: (DOstock + delta_DOstock*dt) = ', (DOstock + delta_DOstock*dt), 'Resp_rate = ', Resp_rate
      endif
# endif

      if (DOstock + delta_DOstock*dt .le. 0.0d0) then
        ! if overdraw on DO due to respired mass, get the difference from the ocean
        ! [mmol.CHEM/m2.hab s]     = [mmol.CHEM/m2.hab s]          - [-umol.DIC/m2.hab]          [1/s] (mmol/1000umol)
        DO__uptake_rate_from_ocean = DO__uptake_rate_from_ocean    - (DOstock + delta_DOstock*dt) / dt * 0.001d0
        delta_DOstock = -1d0 * DOstock / dt
      endif

    endif

! Dieoff from base turnover rate rate -------------------------------------------------------------------------

    ! dieoff_rate                 [umol.C m-2.sg.hab s-1]     Seagrass dieoff rate
    ! Turnover_rate               [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day
    ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    ! [umol.C.dead/m2.hab s] = [umol.C.dead/umol.C.tot day] [umol.C.tot/m2.hab] [day/86400s]
    dieoff_rate              = Turnover_rate                * SgCBmF(iCt)        / 86400.0d0

! Dieoff when respiration is insufficient ------------------------------------------------------------

    if(anoxic_dieoff .gt. 0.0d0) then

      ! anoxic_dieoff             [umol.C m-2.sg.hab]         Seagrass dieoff due to anoxia this time step
      ! SgCBmF                    [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval

      ! ! limit dieoff rate (especially for when DO intake is zero)
      ! ! max_anoxic_dieoff_rate      [mol.C mol-1.C day-1]       maximum dieoff due to anoxia in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
      ! ! [mol.C mol-1.C]     = [mol.C mol-1.C day-1] [s]  (day/86400s)
      ! ! max_dieoff_per_step = max_anoxic_dieoff_rate       * dt / 86400
      ! if (anoxic_dieoff/SgCBmF(iCt) .gt. max_anoxic_dieoff_rate*dt/86400d0) then
      !   anoxic_dieoff = SgCBmF(iCt) * max_anoxic_dieoff_rate*dt/86400d0
      ! endif

      ! dieoff_rate                 [umol.C m-2.sg.hab s-1]     Seagrass dieoff rate
      dieoff_rate = dieoff_rate + anoxic_dieoff /dt

    endif

! Dieoff mass balance (remove from biomass, add to POM) ---------------------------------------------------------------------------------------

# if defined SEAGRASS_LEAF_POM || defined SEAGRASS_ROOT_POM
    ! Dieoff removes biomass (remove from biomass)
    ! delta_SgCBmF                [umol.C m-2.sg.hab s-1]     Rate of change in SgCBmF
    ! f_abv_cur                   [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current above ground biomass to total biomass fraction, calculated on the last growth interval
    ! SgXperC                     [mol.X mol-1.C]             Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! dieoff_rate                 [umol.C m-2.sg.hab s-1]     Seagrass dieoff rate
    ! [mmol.X/m2.hab s]           = [umol.X/m2.hab s]             - [X/C]   [umol.C/m2.hab s] (isotope ratio)
    delta_SgCBmF(:)               = delta_SgCBmF(:)               -           dieoff_rate     * rSgCBm(:)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    delta_SgNBmF(:)               = delta_SgNBmF(:)               - SgNperC * dieoff_rate     * rSgNBm(:)
    delta_SgPBmF(:)               = delta_SgPBmF(:)               - SgPperC * dieoff_rate     * rSgPBm(:)
#  endif
# endif

# if defined SEAGRASS_LEAF_POM
    ! Dieoff adds leaf portion to ocean POM
    ! POX_uptake_rate_from_ocean  [mmol.X m-2.sg.hab s-1]     Particulate organic (carbon, nitrogen, or phosphorus) uptake rate     * direction of ocean to seagrass is positive
    ! f_abv_cur                   [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current above ground biomass to total biomass fraction, calculated on the last growth interval
    ! SgXperC                     [mol.X mol-1.C]             Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! dieoff_rate                 [umol.C m-2.sg.hab s-1]     Seagrass dieoff rate
    ! [mmol.X/m2.hab s]           = [mmol.X/m2.hab s]             - [X/C]     []        [umol.C/m2.hab s] (mmol/1000umol) (isotope ratio)
    POC_uptake_rate_from_ocean(:) = POC_uptake_rate_from_ocean(:) -           f_abv_cur * dieoff_rate * 0.001d0 * rSgCBm(:)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    PON_uptake_rate_from_ocean(:) = PON_uptake_rate_from_ocean(:) - SgNperC * f_abv_cur * dieoff_rate * 0.001d0 * rSgNBm(:)
    POP_uptake_rate_from_ocean(:) = POP_uptake_rate_from_ocean(:) - SgPperC * f_abv_cur * dieoff_rate * 0.001d0 * rSgPBm(:)
#  endif
# endif

# if defined SEAGRASS_ROOT_POM
    ! Dieoff adds root portion to sediment POM
    ! SEDECO(ng)%sg_dPOX_dt       [umol.X L-1 s-1]            Rate of change in POX due to root death in sediment (positive means seagrass to sediment)
    ! SgXperC                     [mol.X mol-1.C]             Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! f_blw_cur                   [g.rt.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current below ground biomass to total biomass fraction, calculated on the last growth interval
    ! root_mass_profile(sed)      []                          Mass ratio of roots in each layer (total must sum to 1.0)
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
    ! root_comp_dist_factor(Nsed) [g.rt.comp g-1.tot m2.sg.hab m-2.sedmnt mm-1]    root compartemtent distribution factor equal to f_blw_cur * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 
    ! dieoff_rate                 [umol.C m-2.sg.hab s-1]     Seagrass dieoff rate
    ! [umol.X/L s]                           = [umol.X/L s]                           + [X/C]   [umol.C/m2.hab s] {[][g.rt.comp/g.tot][m2.hab/m2.sedmnt][1/mm](m2.sedmnt*mm/L)} (isotope ratio)
    do k=1,Nsed
      SEDECO(ng)%sg_dPOC_dt(:,k,species,i,j) = SEDECO(ng)%sg_dPOC_dt(:,k,species,i,j) &
              +           dieoff_rate * root_comp_dist_factor(k) * rSgCBm(:)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
      SEDECO(ng)%sg_dPON_dt(:,k,species,i,j) = SEDECO(ng)%sg_dPON_dt(:,k,species,i,j) &
              + SgNperC * dieoff_rate * root_comp_dist_factor(k) * rSgNBm(:)
      SEDECO(ng)%sg_dPOP_dt(:,k,species,i,j) = SEDECO(ng)%sg_dPOP_dt(:,k,species,i,j) &
              + SgPperC * dieoff_rate * root_comp_dist_factor(k) * rSgPBm(:)
#  endif
    enddo
# endif

! update saved output values -------------------------------------------------------------------------

    ! GridPhot                    [umol.C m-2.grid s-1]       Seagrass gross photosynthesis rate per unit ground area (whole grid)
    ! Phot_rate                   [umol.C m-2.sg.hab s-1]     Gross photosynthesis rate
    ! p_sgrass(Nsg)               [m2.sg.hab m-2.grid]        (0~1)  Seagrass coverage (every species)
    ! [umol.C/m2.grid s] = [umol.C/m2.hab s] [m2.hab/m2.grid]
    GridPhot             = Phot_rate         * p_sgrass(species)

    ! GridResp                    [umol.C m-2.grid s-1]       Seagrass respiration rate per unit ground area (whole grid)
    ! Resp_rate                   [umol.C m-2.sg.hab s-1]     Gross respirationrate
    ! p_sgrass(Nsg)               [m2.sg.hab m-2.grid]        (0~1)  Seagrass coverage (every species)
    ! [umol.C/m2.grid s] = [umol.C/m2.hab s] [m2.hab/m2.grid]
    GridResp             = Resp_rate         * p_sgrass(species)

    ! GridNetPhot                 [umol.C m-2.grid s-1]       Seagrass net photosynthesis rate per unit ground area (whole grid)
    ! GridPhot                    [umol.C m-2.grid s-1]       Seagrass gross photosynthesis rate per unit ground area (whole grid)
    ! GridResp                    [umol.C m-2.grid s-1]       Seagrass respiration rate per unit ground area (whole grid)
    ! [umol.C/m2.grid s] = [umol.C/m2.grid s] - [umol.C/m2.grid s]
    GridNetPhot          = GridPhot           - GridResp

    ! GridGrow                    [umol.C m-2.grid s-1]       Seagrass gross photosynthesis rate per unit ground area (whole grid)
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Growth rate
    ! p_sgrass(Nsg)               [m2.sg.hab m-2.grid]        (0~1)  Seagrass coverage (every species)
    ! [umol.C/m2.grid s] = [umol.C/m2.hab s] [m2.hab/m2.grid]
    GridGrow             = Growth_rate       * p_sgrass(species)

    ! dieoff_rate                 [umol.C m-2.sg.hab s-1]     Seagrass dieoff rate
    ! GridDieoff                  [umol.C m-2.grid s-1]       Seagrass dieoff rate carbon biomass per unit ground area (whole grid)
    ! p_sgrass(Nsg)               [m2.sg.hab m-2.grid]        (0~1)  Seagrass coverage (every species)
    ! [umol.C/m2.grid s] = [umol.C/m2.hab s] [m2.hab/m2.grid]
    GridDieoff           = dieoff_rate       * p_sgrass(species)

    ! PhotLim                     []                          Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon stock
    ! GrowLim                     []                          Seagrass growth limiting factor: 1 = Sugar stock; 2 = Nitrogen stock; 3 = Phosphorus stock

    ! Growth_limit_light          [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by light
    ! Growth_limit_DIC            [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by DIC uptake
    ! Growth_limit_NO3            [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by NO3 uptake
    ! Growth_limit_NH4            [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by NH4 uptake
    ! Growth_limit_PO4            [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by PO4 uptake
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! PhotLim = minloc((/Growth_limit_light, Growth_limit_DIC, (Growth_limit_NO3+Growth_limit_NH4) &
    !         , Growth_limit_PO4/), DIM=1)
    PhotLim = minloc((/Phot_limit_light, Phot_limit_DIC/), DIM=1)
    GrowLim = minloc((/Growth_limit_CH2O, (Growth_limit_NO3+Growth_limit_NH4), Growth_limit_PO4/), DIM=1)
# else
    PhotLim = minloc((/Phot_limit_light, Phot_limit_DIC/), DIM=1)
    GrowLim = minloc((/Growth_limit_CH2O/), DIM=1)
# endif



! ====================================================================================================
! ----------------------------------------------------------------------------------------------------
!                                UPDATE AT END OF MAIN INTERVAL
! ----------------------------------------------------------------------------------------------------
! ====================================================================================================

! Mass balance of seagrass biomass -------------------------------------------------------------------------

    ! if seagrass dry weight biomass would become less than 1.0 ug per m2, then instead set to zero

    ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
    ! delta_SgCBmF                [umol.C m-2.sg.hab s-1]     Rate of change in SgCBmF
    ! dt                          [s]                         Time step interval duration
    ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    if (SgCBmF(iCt) + delta_SgCBmF(iCt) * dt .lt. 1.0d0/SgMperC) then
      ! Move seagrass biomass to POM
# if defined SEAGRASS_LEAF_POM
      ! Add leaf portion to ocean POM
      ! POX_uptake_rate_from_ocean  [mmol.X m-2.sg.hab s-1]     Particulate organic (carbon, nitrogen, or phosphorus) uptake rate     * direction of ocean to seagrass is positive
      ! f_abv_cur                   [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current above ground biomass to total biomass fraction, calculated on the last growth interval
      ! SgXperC                     [mol.X mol-1.C]             Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
      ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
      ! [mmol.X/m2.hab s]           = [mmol.X/m2.hab s]             - []        [umol.C/m2.hab] [1/s] (mmol/1000umol)
      POC_uptake_rate_from_ocean(:) = POC_uptake_rate_from_ocean(:) - f_abv_cur * SgCBmF(:)     / dt  * 0.001d0
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
      PON_uptake_rate_from_ocean(:) = PON_uptake_rate_from_ocean(:) - f_abv_cur * SgNBmF(:)     / dt  * 0.001d0
      POP_uptake_rate_from_ocean(:) = POP_uptake_rate_from_ocean(:) - f_abv_cur * SgPBmF(:)     / dt  * 0.001d0
#  endif
# endif
# if defined SEAGRASS_ROOT_POM
      ! Add root portion to sediment POM
      ! SEDECO(ng)%sg_dPOX_dt       [umol.X L-1 s-1]            Rate of change in POX due to root death in sediment (positive means seagrass to sediment)
      ! SgXperC                     [mol.X mol-1.C]             Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
      ! f_blw_cur                   [g.rt.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current below ground biomass to total biomass fraction, calculated on the last growth interval
      ! root_mass_profile(sed)      []                          Mass ratio of roots in each layer (total must sum to 1.0)
      ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
      ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
      ! root_comp_dist_factor(Nsed) [g.rt.comp g-1.tot m2.sg.hab m-2.sedmnt mm-1]    root compartemtent distribution factor equal to f_blw_cur * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 
      ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
      ! [umol.X/L s]                           = [umol.X/L s]                           + [umol.C/m2.hab] [1/s] {[][g.rt.comp/g.tot][m2.hab/m2.sedmnt][1/mm](m2.sedmnt*mm/L)} 
      do k=1,Nsed
        SEDECO(ng)%sg_dPOC_dt(:,k,species,i,j) = SEDECO(ng)%sg_dPOC_dt(:,k,species,i,j) + SgCBmF(:) / dt * root_comp_dist_factor(k)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
        SEDECO(ng)%sg_dPON_dt(:,k,species,i,j) = SEDECO(ng)%sg_dPON_dt(:,k,species,i,j) + SgNBmF(:) / dt * root_comp_dist_factor(k)
        SEDECO(ng)%sg_dPOP_dt(:,k,species,i,j) = SEDECO(ng)%sg_dPOP_dt(:,k,species,i,j) + SgPBmF(:) / dt * root_comp_dist_factor(k)
#  endif
      enddo
# endif
      ! Set the seagrass biomass to zero
      SgCBmF(:) = 0d0
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
      SgNBmF(:) = 0d0
      SgPBmF(:) = 0d0
# endif

    ! else if seagrass dry weight biomass would remain greater than 1.0 ug per m2, add the appropriate biomass

    else
      SgCBmF(:) = SgCBmF(:) + delta_SgCBmF(:) * dt
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
      SgNBmF(:) = SgNBmF(:) + delta_SgNBmF(:) * dt
      SgPBmF(:) = SgPBmF(:) + delta_SgPBmF(:) * dt
# endif
    endif

    ! ! yt_debug
    ! if (SgCBmF(iCt)*0.0d0 /= 0.0d0) then
    !   write(*,*) 'SEAGRASS error 2: SgCBmF is NAN', SgCBmF(iCt), delta_SgCBmF(iCt)
    !   error stop
    ! endif
    ! ! yt_debug
    ! if (SgNBmF(iNt)*0.0d0 /= 0.0d0) then
    !   write(*,*) 'SEAGRASS error 2: SgNBmF is NAN', SgNBmF(iNt), delta_SgNBmF(iNt)
    !   error stop
    ! endif
    ! ! yt_debug
    ! if (SgPBmF(iPt)*0.0d0 /= 0.0d0) then
    !   write(*,*) 'SEAGRASS error 2: SgPBmF is NAN', SgPBmF(iPt), delta_SgPBmF(iPt)
    !   error stop
    ! endif

    ! [mol.C] = [umol.C/m2.hab] [m2.hab]  (mol/1000000umol)
    TotSgCBmF = SgCBmF(iCt)    * HabArea * 0.000001d0

! First pass mass balance of stocks -------------------------------------------------------------------------

    ! DICstock                    [umol.DIC m-2.sg.hab]       Seagrass internal DIC stock per unit ground area (in seagrass habitat area)
    ! delta_DICstock              [umol.DIC m-2.sg.hab s-1]   Rate of change in DICstock
    ! [umol.DIC/m2.hab] = [umol.DIC/m2.hab] + [umol.DIC/m2.hab s] [s]
    DICstock(:)         = DICstock(:)       + delta_DICstock(:)   * dt
    CH2Ostock(:)        = CH2Ostock(:)      + delta_CH2Ostock(:)  * dt
    DOstock             = DOstock           + delta_DOstock       * dt
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    NO3stock(:)         = NO3stock(:)       + delta_NO3stock(:)   * dt
    NH4stock(:)         = NH4stock(:)       + delta_NH4stock(:)   * dt
    PO4stock(:)         = PO4stock(:)       + delta_PO4stock(:)   * dt
# endif

# if defined SEAGRASS_DEBUG_MODE
    ! error check for negative stock value
    if (DICstock(iCt) .lt. -1d-5) then
      write(*,*) "yt_debug: negative value error in DICstock. Time =", &
              date, "Position =", i, j
      write(*,*) "yt_debug:     DICstock = ", DICstock(iCt)
      ! error stop
    endif
    if (CH2Ostock(iCt) .lt. -1d-5) then
      write(*,*) "yt_debug: negative value error in CH2Ostock. Time =", &
              date, "Position =", i, j
      write(*,*) "yt_debug:     CH2Ostock = ", CH2Ostock(iCt), "delta_CH2Ostock = ", delta_CH2Ostock(iCt)
      ! error stop
    endif
    if (DOstock .lt. -1d-5) then
      write(*,*) "yt_debug: negative value error in DOstock. Time =", &
              date, "Position =", i, j
      write(*,*) "yt_debug:     DOstock = ", DOstock
      ! error stop
    endif
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    if (NO3stock(iNt) .lt. -1d-5) then
      write(*,*) "yt_debug: negative value error in NO3stock. Time =", &
              date, "Position =", i, j
      write(*,*) "yt_debug:     NO3stock = ", NO3stock(iNt)
      ! error stop
    endif
    if (NH4stock(iNt) .lt. -1d-5) then
      write(*,*) "yt_debug: negative value error in NH4stock. Time =", &
              date, "Position =", i, j
      write(*,*) "yt_debug:     NH4stock = ", NH4stock(iNt)
      ! error stop
    endif
    if (PO4stock(iPt) .lt. -1d-5) then
      write(*,*) "yt_debug: negative value error in PO4stock. Time =", &
              date, "Position =", i, j
      write(*,*) "yt_debug:     PO4stock = ", PO4stock(iPt)
      ! error stop
    endif
# endif
# endif

    if (DICstock(iCt) .le. 0.0d0) then
      DICstock(:) = 0.0d0
    endif
    if (CH2Ostock(iCt) .le. 0.0d0) then
      CH2Ostock(:) = 0.0d0
    endif
    if (DOstock .le. 0.0d0) then
      DOstock = 0.0d0
    endif
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    if (NO3stock(iNt) .le. 0.0d0) then
      NO3stock(:) = 0.0d0
    endif
    if (NH4stock(iNt) .le. 0.0d0) then
      NH4stock(:) = 0.0d0
    endif
    if (PO4stock(iPt) .le. 0.0d0) then
      PO4stock(:) = 0.0d0
    endif
# endif

! If stocks exceed limit, leak to environment -------------------------------------------------------------------------

    ! stock is limited to stock cap so leak any excess
    ! DICstock                    [umol.DIC m-2.sg.hab]       Seagrass internal DIC stock per unit ground area (in seagrass habitat area)
    ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
    ! delta_DICstock              [umol.DIC m-2.sg.hab s-1]   Rate of change in DICstock
    ! CHEM_uptake_rate_from_ocean [mmol.CHEM m-2.sg.hab s-1]  direction of ocean to seagrass is positive
    ! SEDECO(ng)%sg_dDIC_dt()     [umol.DIC L-1 s-1]          Rate of change in DIC due to flux from roots to sediment (positive means seagrass to sediment)
    ! f_abv_cur                   [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current above ground biomass to total biomass fraction, calculated on the last growth interval
    ! root_comp_dist_factor(Nsed) [g.rt.comp g-1.tot m2.sg.hab m-2.sedmnt mm-1]    root compartemtent distribution factor equal to f_blw_cur * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 
    ! DICstockCap                 []                          Factor to multiply carbon biomsass to get maximum DIC stock (stock limit = DICstockCap*SgCBmF) 

if (DICstock(iCt) .gt. DICstockCap*SgCBmF(iCt)) then
    ! Excess DIC = (DIC stock - seagrass biomass) = (DICstock(:)-SgCBmF(iCt)*rDICstock(:))
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! Distribute this to ocean and sediment
    ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          - [umol.DIC/m2.hab]                      [1/s] []        (mmol/1000umol)
    DIC_uptake_rate_from_ocean(:) = DIC_uptake_rate_from_ocean(:) &
            - (DICstock(:)-DICstockCap*SgCBmF(iCt)*rDICstock(:)) / dt  * f_abv_cur * 0.001d0
    do k=1,Nsed
      ! [umol.CHEM/L s]                      = [umol.CHEM/L s]                        + [umol.DIC/m2.hab]                      [1/s] {[][g.rt.comp/g.tot][m2.hab/m2.sedmnt][1/mm](m2.sedmnt*mm/L)}
      SEDECO(ng)%sg_dDIC_dt(:,k,species,i,j) = SEDECO(ng)%sg_dDIC_dt(:,k,species,i,j) &
              + (DICstock(:)-DICstockCap*SgCBmF(iCt)*rDICstock(:)) / dt  * root_comp_dist_factor(k)
    end do
# else
    ! Distribute this to ocean
    ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          - [umol.DIC/m2.hab]                      [1/s] []        (mmol/1000umol)
    DIC_uptake_rate_from_ocean(:) = DIC_uptake_rate_from_ocean(:) &
            - (DICstock(:)-DICstockCap*SgCBmF(iCt)*rDICstock(:)) / dt  * 0.001d0
# endif
    ! Set stock equal to seagrass biomass
    ! [umol.DIC/m2.hab] = [umol.C/m2.hab] (isotope ratio)
    DICstock(:)         = DICstockCap*SgCBmF(iCt)     * rDICstock(:)
endif

  ! CH2OstockCap                []                          Factor to multiply carbon biomsass to get maximum sugar stock (stock limit = CH2OstockCap*SgCBmF) 
if (CH2Ostock(iCt) .gt. CH2OstockCap*SgCBmF(iCt)) then
  ! Excess DOC = (CH2O stock - seagrass biomass) = (CH2Ostock(:)-SgCBmF(iCt)*rCH2Ostock(:))
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
  ! Distribute this to ocean and sediment
  ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          - [umol.CH2O/m2.hab]                      [1/s] []        (mmol/1000umol)
  DOC_uptake_rate_from_ocean(:) = DOC_uptake_rate_from_ocean(:) &
          - (CH2Ostock(:)-CH2OstockCap*SgCBmF(iCt)*rCH2Ostock(:)) / dt  * f_abv_cur * 0.001d0
  do k=1,Nsed
    ! [umol.CHEM/L s]                      = [umol.CHEM/L s]                        + [umol.DIC/m2.hab]                      [1/s] {[][g.rt.comp/g.tot][m2.hab/m2.sedmnt][1/mm](m2.sedmnt*mm/L)}
    SEDECO(ng)%sg_dDOC_dt(:,k,species,i,j) = SEDECO(ng)%sg_dDOC_dt(:,k,species,i,j) &
            + (CH2Ostock(:)-CH2OstockCap*SgCBmF(iCt)*rCH2Ostock(:)) / dt  * root_comp_dist_factor(k)
  end do
# else
  ! Distribute this to ocean
  ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          - [umol.CH2O/m2.hab]                      [1/s] []        (mmol/1000umol)
  DOC_uptake_rate_from_ocean(:) = DOC_uptake_rate_from_ocean(:) &
          - (CH2Ostock(:)-CH2OstockCap*SgCBmF(iCt)*rCH2Ostock(:)) / dt * 0.001d0
# endif
  ! Set stock equal to seagrass biomass
  ! [umol.DIC/m2.hab] = [umol.C/m2.hab] (isotope ratio)
  CH2Ostock(:)         = CH2OstockCap*SgCBmF(iCt)     * rCH2Ostock(:)
endif

  ! DOstockCap                  []                          Factor to multiply carbon biomsass to get maximum DO stock (stock limit = DOstockCap*SgCBmF) 
if (DOstock .gt. DOstockCap*SgCBmF(iCt)) then
  ! Excess DO = (DO stock - seagrass C-biomass) = (DOstock-SgCBmF(iCt))
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
  ! Distribute this to ocean and sediment
  ! [mmol.CHEM/m2.hab s]     = [mmol.CHEM/m2.hab s]          - [umol.DIC/m2.hab]     [1/s] []        (mmol/1000umol)
  DO__uptake_rate_from_ocean = DO__uptake_rate_from_ocean    - (DOstock-DOstockCap*SgCBmF(iCt)) / dt  * f_abv_cur * 0.001d0
  do k=1,Nsed
    ! [umol.CHEM/L s]                   = [umol.CHEM/L s]                     + [umol.DIC/m2.hab]     [1/s] {[][g.rt.comp/g.tot][m2.hab/m2.sedmnt][1/mm](m2.sedmnt*mm/L)}
    SEDECO(ng)%sg_dO2_dt(k,species,i,j) = SEDECO(ng)%sg_dO2_dt(k,species,i,j) &
            + (DOstock-DOstockCap*SgCBmF(iCt)) / dt * root_comp_dist_factor(k)
  end do
# else
  ! Distribute this to ocean
  ! [mmol.CHEM/m2.hab s]     = [mmol.CHEM/m2.hab s]          - [umol.DIC/m2.hab]     [1/s] []        (mmol/1000umol)
  DO__uptake_rate_from_ocean = DO__uptake_rate_from_ocean    - (DOstock-DOstockCap*SgCBmF(iCt)) / dt * 0.001d0
# endif
  ! Set stock equal to seagrass biomass
  ! [umol.DIC/m2.hab] = [umol.C/m2.hab] (isotope ratio)
  DOstock             = DOstockCap*SgCBmF(iCt)
endif

  ! NO3stockCap                 []                          Factor to multiply nitrogen biomsass to get maximum NO3 stock (stock limit = NO3stockCap*SgNBmF) 
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
if (NO3stock(iNt) .gt. NO3stockCap*SgNBmF(iNt)) then
  ! Excess NO3 = (NO3 stock - seagrass biomass) = (NO3stock(:)-SgNBmF(iNt)*rNO3stock(:))
# endif
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE && defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
  ! Distribute this to ocean and sediment
  ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          - [umol.NO3/m2.hab]                      [1/s] []        (mmol/1000umol)
  NO3_uptake_rate_from_ocean(:) = NO3_uptake_rate_from_ocean(:) &
          - (NO3stock(:)-NO3stockCap*SgNBmF(iNt)*rNO3stock(:)) / dt  * f_abv_cur * 0.001d0
  do k=1,Nsed
    ! [umol.CHEM/L s]                      = [umol.CHEM/L s]                        + [umol.NO3/m2.hab]                      [1/s] {[][g.rt.comp/g.tot][m2.hab/m2.sedmnt][1/mm](m2.sedmnt*mm/L)}
    SEDECO(ng)%sg_dNO3_dt(:,k,species,i,j) = SEDECO(ng)%sg_dNO3_dt(:,k,species,i,j) &
            + (NO3stock(:)-NO3stockCap*SgNBmF(iNt)*rNO3stock(:)) / dt  * root_comp_dist_factor(k)
  end do
# elif defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
  ! Distribute this to ocean
  ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          - [umol.NO3/m2.hab]                      [1/s] []        (mmol/1000umol)
  NO3_uptake_rate_from_ocean(:) = NO3_uptake_rate_from_ocean(:) &
          - (NO3stock(:)-NO3stockCap*SgNBmF(iNt)*rNO3stock(:)) / dt * 0.001d0
# elif defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
  ! Distribute this to sediment
  do k=1,Nsed
    ! [umol.CHEM/L s]                      = [umol.CHEM/L s]                        + [umol.NO3/m2.hab]                      [1/s] {[][g.rt.comp/g.tot][m2.hab/m2.sedmnt][1/mm](m2.sedmnt*mm/L)}
    SEDECO(ng)%sg_dNO3_dt(:,k,species,i,j) = SEDECO(ng)%sg_dNO3_dt(:,k,species,i,j) &
            + (NO3stock(:)-NO3stockCap*SgNBmF(iNt)*rNO3stock(:)) / dt  * root_comp_dist_factor(k) / f_blw_cur
# endif
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
  ! Set stock equal to seagrass biomass
  ! [umol.NO3/m2.hab] = [umol.N/m2.hab] (isotope ratio)
  NO3stock(:)         = NO3stockCap*SgNBmF(iNt)     * rNO3stock(:)
endif
# endif

  ! NH4stockCap                 []                          Factor to multiply nitrogen biomsass to get maximum NH4 stock (stock limit = NH4stockCap*SgNBmF) 
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
if (NH4stock(iNt) .gt. NH4stockCap*SgNBmF(iNt)) then
  ! Excess NH4 = (NH4 stock - seagrass biomass) = (NH4stock(:)-SgNBmF(iNt)*rNH4stock(:))
# endif
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE && defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
  ! Distribute this to ocean and sediment
  ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          - [umol.NH4/m2.hab]                      [1/s] []        (mmol/1000umol)
  NH4_uptake_rate_from_ocean(:) = NH4_uptake_rate_from_ocean(:) &
          - (NH4stock(:)-NH4stockCap*SgNBmF(iNt)*rNH4stock(:)) / dt  * f_abv_cur * 0.001d0
  do k=1,Nsed
    ! [umol.CHEM/L s]                      = [umol.CHEM/L s]                        + [umol.NH4/m2.hab]                      [1/s] {[][g.rt.comp/g.tot][m2.hab/m2.sedmnt][1/mm](m2.sedmnt*mm/L)}
    SEDECO(ng)%sg_dNH4_dt(:,k,species,i,j) = SEDECO(ng)%sg_dNH4_dt(:,k,species,i,j) &
            + (NH4stock(:)-NH4stockCap*SgNBmF(iNt)*rNH4stock(:)) / dt  * root_comp_dist_factor(k)
  end do
# elif defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
  ! Distribute this to ocean
  ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          - [umol.NH4/m2.hab]                      [1/s] []        (mmol/1000umol)
  NH4_uptake_rate_from_ocean(:) = NH4_uptake_rate_from_ocean(:) &
          - (NH4stock(:)-NH4stockCap*SgNBmF(iNt)*rNH4stock(:)) / dt * 0.001d0
# elif defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
  ! Distribute this to sediment
  do k=1,Nsed
    ! [umol.CHEM/L s]                      = [umol.CHEM/L s]                        + [umol.NH4/m2.hab]                      [1/s] {[][g.rt.comp/g.tot][m2.hab/m2.sedmnt][1/mm](m2.sedmnt*mm/L)}
    SEDECO(ng)%sg_dNH4_dt(:,k,species,i,j) = SEDECO(ng)%sg_dNH4_dt(:,k,species,i,j) &
            + (NH4stock(:)-NH4stockCap*SgNBmF(iNt)*rNH4stock(:)) / dt  * root_comp_dist_factor(k) / f_blw_cur
  end do
# endif
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
  ! Set stock equal to seagrass biomass
  ! [umol.NH4/m2.hab] = [umol.N/m2.hab] (isotope ratio)
  NH4stock(:)         = NH4stockCap*SgNBmF(iNt)     * rNH4stock(:)
endif
# endif

  ! PO4stockCap                 []                          Factor to multiply phosphorus biomsass to get maximum PO4 stock (stock limit = PO4stockCap*SgPBmF) 
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
if (PO4stock(iPt) .gt. PO4stockCap*SgPBmF(iPt)) then
  ! Excess PO4 = (PO4 stock - seagrass biomass) = (PO4stock(:)-SgPBmF(iPt)*rPO4stock(:))
# endif
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE && defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
  ! Distribute this to ocean and sediment
  ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          - [umol.PO4/m2.hab]                      [1/s] []        (mmol/1000umol)
  PO4_uptake_rate_from_ocean(:) = PO4_uptake_rate_from_ocean(:) &
          - (PO4stock(:)-PO4stockCap*SgPBmF(iPt)*rPO4stock(:)) / dt  * f_abv_cur * 0.001d0
  do k=1,Nsed
    ! [umol.CHEM/L s]                      = [umol.CHEM/L s]                        + [umol.PO4/m2.hab]                      [1/s] {[][g.rt.comp/g.tot][m2.hab/m2.sedmnt][1/mm](m2.sedmnt*mm/L)}
    SEDECO(ng)%sg_dPO4_dt(:,k,species,i,j) = SEDECO(ng)%sg_dPO4_dt(:,k,species,i,j) &
            + (PO4stock(:)-PO4stockCap*SgPBmF(iPt)*rPO4stock(:)) / dt  * root_comp_dist_factor(k)
  end do
# elif defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
  ! Distribute this to ocean
  ! [mmol.CHEM/m2.hab s]        = [mmol.CHEM/m2.hab s]          - [umol.PO4/m2.hab]                      [1/s] []        (mmol/1000umol)
  PO4_uptake_rate_from_ocean(:) = PO4_uptake_rate_from_ocean(:) &
          - (PO4stock(:)-PO4stockCap*SgPBmF(iPt)*rPO4stock(:)) / dt * 0.001d0
# elif defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
  ! Distribute this to sediment
  do k=1,Nsed
    ! [umol.CHEM/L s]                      = [umol.CHEM/L s]                        + [umol.PO4/m2.hab]                      [1/s] {[][g.rt.comp/g.tot][m2.hab/m2.sedmnt][1/mm](m2.sedmnt*mm/L)}
    SEDECO(ng)%sg_dPO4_dt(:,k,species,i,j) = SEDECO(ng)%sg_dPO4_dt(:,k,species,i,j) &
            + (PO4stock(:)-PO4stockCap*SgPBmF(iPt)*rPO4stock(:)) / dt  * root_comp_dist_factor(k) / f_blw_cur
# endif
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
  ! Set stock equal to seagrass biomass
  ! [umol.PO4/m2.hab] = [umol.P/m2.hab] (isotope ratio)
  PO4stock(:)         = PO4stockCap*SgPBmF(iPt)     * rPO4stock(:)
endif
# endif

! If net zero mass start option is used, Destroy mass released to environment equivalent to initial biomass amount -------------------------------------------------------------------------

# if defined NET_ZERO_MASS_START
    ! Destroy (erase) initial mass output until net zero is reached, before switching to normal flux
    ! May not properly account for isotope ratios.

    ! InitialC                    [mmol.C]                    Initial carbon remaining to destroy until net zero
    ! CHEM_uptake_rate_from_ocean [mmol.CHEM m-2.sg.hab s-1]  direction of ocean to seagrass is positive
    ! delta_sed_CHEM(Nsed)        [umol.CHEM L-1 s-1]         Rate of change in CHEM due to flux from roots to sediment (positive means seagrass to sediment)
    ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage

    if (InitialC > 0.0d0) then
      if (DIC_uptake_rate_from_ocean(iCt) < 0) then
        ! [mmol.C] = [mmol.C] + [mmol.C/m2.hab s]               [m2.hab]  [s]
        InitialC   = InitialC + DIC_uptake_rate_from_ocean(iCt) * HabArea * dt
        DIC_uptake_rate_from_ocean(:) = 0.0d0
      endif
      if (DOC_uptake_rate_from_ocean(iCt) < 0) then
        ! [mmol.C] = [mmol.C] + [mmol.C/m2.hab s]               [m2.hab]  [s]
        InitialC   = InitialC + DOC_uptake_rate_from_ocean(iCt) * HabArea * dt
        DOC_uptake_rate_from_ocean(:) = 0.0d0
      endif
#  if defined SEAGRASS_LEAF_POM
      if (POC_uptake_rate_from_ocean(iCt) < 0) then
        ! [mmol.C] = [mmol.C] + [mmol.C/m2.hab s]          [m2.hab]  [s]
        InitialC   = InitialC + POC_uptake_rate_from_ocean(iCt) * HabArea * dt
        POC_uptake_rate_from_ocean(:) = 0.0d0
      endif
#  endif
#  if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
      do k=1,Nsed
        if (SEDECO(ng)%sg_dDIC_dt(iCt,k,species,i,j) > 0 ) then
          ! [mmol.C] = [mmol.C] - [umol.C/L.sed s] [mm.sed] [m2.sed/m2.hab]     [m2.hab]  [s]  [L.sed/m2.sed mm.sed] (mmol/1000umol)
          InitialC   = InitialC - SEDECO(ng)%sg_dDIC_dt(iCt,k,species,i,j) &
                                  * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt * 0.001d0
          SEDECO(ng)%sg_dDIC_dt(:,k,species,i,j) = 0.0d0
        endif
      enddo
      do k=1,Nsed
        if (SEDECO(ng)%sg_dDOC_dt(iCt,k,species,i,j) > 0 ) then
          ! [mmol.C] = [mmol.C] - [umol.C/L.sed s] [mm.sed] [m2.sed/m2.hab]     [m2.hab]  [s]  [L.sed/m2.sed mm.sed] (mmol/1000umol)
          InitialC   = InitialC - SEDECO(ng)%sg_dDOC_dt(iCt,k,species,i,j) &
                                  * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt * 0.001d0
          SEDECO(ng)%sg_dDOC_dt(:,k,species,i,j) = 0.0d0
        endif
      enddo
#  endif
#  if defined SEAGRASS_ROOT_POM
      do k=1,Nsed
        if (SEDECO(ng)%sg_dPOC_dt(iCt,k,species,i,j) > 0 ) then
          ! [mmol.C] = [mmol.C] - [umol.C/L.sed s] [mm.sed]                 [m2.sed/m2.hab]     [m2.hab]  [s]  [L.sed/m2.sed mm.sed] (mmol/1000umol)
          InitialC   = InitialC - SEDECO(ng)%sg_dPOC_dt(iCt,k,species,i,j) &
                                  * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt * 0.001d0
          SEDECO(ng)%sg_dPOC_dt(:,k,species,i,j) = 0.0d0
        endif
      enddo
#  endif
    endif

    if (InitialN > 0.0d0) then
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE         
      if (NO3_uptake_rate_from_ocean(iNt) < 0) then
        ! [mmol.N] = [mmol.N] + [mmol.N/m2.hab s]          [m2.hab]  [s]
        InitialN   = InitialN + NO3_uptake_rate_from_ocean(iNt) * HabArea * dt
        NO3_uptake_rate_from_ocean(:) = 0.0d0
      endif
      if (NH4_uptake_rate_from_ocean(iNt) < 0) then
        ! [mmol.N] = [mmol.N] + [mmol.N/m2.hab s]          [m2.hab]  [s]
        InitialN   = InitialN + NH4_uptake_rate_from_ocean(iNt) * HabArea * dt
        NH4_uptake_rate_from_ocean(:) = 0.0d0
      endif
#  endif
#  if defined SEAGRASS_LEAF_POM && (defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE)
      if (PON_uptake_rate_from_ocean(iNt) < 0) then
        ! [mmol.N] = [mmol.N] + [mmol.N/m2.hab s]          [m2.hab]  [s]
        InitialN   = InitialN + PON_uptake_rate_from_ocean(iNt) * HabArea * dt
        PON_uptake_rate_from_ocean(:) = 0.0d0
      endif
#  endif
#  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
      do k=1,Nsed
        if (SEDECO(ng)%sg_dNO3_dt(iNt,k,species,i,j) > 0 ) then
          ! [mmol.N] = [mmol.N] - [umol.N/L.sed s] [mm.sed]                 [m2.sed/m2.hab]     [m2.hab]  [s]  [L.sed/m2.sed mm.sed] (mmol/1000umol)
          InitialN   = InitialN - SEDECO(ng)%sg_dNO3_dt(iNt,k,species,i,j) &
                                  * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt * 0.001d0
          SEDECO(ng)%sg_dNO3_dt(:,k,species,i,j) = 0.0d0
        endif
        if (SEDECO(ng)%sg_dNH4_dt(iNt,k,species,i,j) > 0 ) then
          ! [mmol.N] = [mmol.N] - [umol.N/L.sed s] [mm.sed]                 [m2.sed/m2.hab]     [m2.hab]  [s]  [L.sed/m2.sed mm.sed] (mmol/1000umol)
          InitialN   = InitialN - SEDECO(ng)%sg_dNH4_dt(iNt,k,species,i,j) &
                                  * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt * 0.001d0
          SEDECO(ng)%sg_dNH4_dt(:,k,species,i,j) = 0.0d0
        endif
      enddo
#  endif
#  if defined SEAGRASS_ROOT_POM && (defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE)
      do k=1,Nsed
        if (SEDECO(ng)%sg_dPON_dt(iNt,k,species,i,j) > 0 ) then
          ! [mmol.N] = [mmol.N] - [umol.N/L.sed s] [mm.sed]                 [m2.sed/m2.hab]     [m2.hab]  [s]  [L.sed/m2.sed mm.sed] (mmol/1000umol)
          InitialN   = InitialN - SEDECO(ng)%sg_dPON_dt(iNt,k,species,i,j) &
                                  * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt * 0.001d0
          SEDECO(ng)%sg_dPON_dt(:,k,species,i,j) = 0.0d0
        endif
      enddo
#  endif
    endif

    if (InitialP > 0.0d0) then
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE         
      if (PO4_uptake_rate_from_ocean(iPt) < 0) then
        ! [mmol.P] = [mmol.P] + [mmol.P/m2.hab s]          [m2.hab]  [s]
        InitialP   = InitialP + PO4_uptake_rate_from_ocean(iPt) * HabArea * dt
        PO4_uptake_rate_from_ocean(:) = 0.0d0
      endif
#  endif
#  if defined SEAGRASS_LEAF_POM && (defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE)
      if (POP_uptake_rate_from_ocean(iPt) < 0) then
        ! [mmol.P] = [mmol.P] + [mmol.P/m2.hab s]          [m2.hab]  [s]
        InitialP   = InitialP + POP_uptake_rate_from_ocean(iPt) * HabArea * dt
        POP_uptake_rate_from_ocean(:) = 0.0d0
      endif
#  endif
#  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
      do k=1,Nsed
        if (SEDECO(ng)%sg_dPO4_dt(iPt,k,species,i,j) > 0 ) then
          ! [mmol.P] = [mmol.P] - [umol.P/L.sed s] [mm.sed]                 [m2.sed/m2.hab]     [m2.hab]  [s]  [L.sed/m2.sed mm.sed] (mmol/1000umol)
          InitialP   = InitialP - SEDECO(ng)%sg_dPO4_dt(iPt,k,species,i,j) &
                                  * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt * 0.001d0
          SEDECO(ng)%sg_dPO4_dt(:,k,species,i,j) = 0.0d0
        endif
      enddo
#  endif
#  if defined SEAGRASS_ROOT_POM && (defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE)
      do k=1,Nsed
        if (SEDECO(ng)%sg_dPOP_dt(iPt,k,species,i,j) > 0 ) then
          ! [mmol.P] = [mmol.P] - [umol.P/L.sed s] [mm.sed]                 [m2.sed/m2.hab]     [m2.hab]  [s]  [L.sed/m2.sed mm.sed] (mmol/1000umol)
          InitialP   = InitialP - SEDECO(ng)%sg_dPOP_dt(iPt,k,species,i,j) &
                                  * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt * 0.001d0
          SEDECO(ng)%sg_dPOP_dt(:,k,species,i,j) = 0.0d0
        endif
      enddo
#  endif
    endif
  
# endif



! ====================================================================================================
! ----------------------------------------------------------------------------------------------------
!                               UPDATE AT END OF GROWTH INTERVAL
! ----------------------------------------------------------------------------------------------------
! ====================================================================================================
    
    if (is_growth_interval) then 

! update total carbon-biomass ------------------------------------------------------------------------

      !   write(*,*) 'yt_debug: TotSgCBm = ', TotSgCBm, ' TotLfCBm = ', TotLfCBm,  'TotRtCBm = ', TotRtCBm

      ! oldTotSgCBm                 [mol.C.tot]                 Old total seagrass carbon-biomass in grid cell
      ! oldTotLfCBm                 [mol.C.lf]                  Old total seagrass leaf carbon-biomass in grid cell
      ! oldTotRtCBm                 [mol.C.rt]                  Old total seagrass root carbon-biomass in grid cell
      oldTotSgCBm = TotSgCBm
      oldTotLfCBm = TotLfCBm
      oldTotRtCBm = TotRtCBm

      ! TotSgCBm                    [mol.C]                     Total seagrass carbon-biomass in grid cell
      ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
      ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)

      ! [mol.C] = [umol.C/m2.hab] [m2.hab]  (mol/1000000umol)
      TotSgCBm  = SgCBmF(iCt)     * HabArea * 0.000001d0

      if(TotSgCBm>0) then
! allocate growth ------------------------------------------------------------------------------------

      ! TotLfCBm                    [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
      ! TotRtCBm                    [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
      ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
      ! oldTotSgCBm                 [mol.C.tot]                 Old total seagrass carbon-biomass in grid cell
      ! oldTotLfCBm                 [mol.C.lf]                  Old total seagrass leaf carbon-biomass in grid cell
      ! oldTotRtCBm                 [mol.C.rt]                  Old total seagrass root carbon-biomass in grid cell
      ! f_above                     [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction  
      ! mass_to_allocate            [mol.C]                     mass to allocate in growth or death to either leaves or roots
        mass_to_allocate = TotSgCBm - oldTotSgCBm
        if (mass_to_allocate .ge. 0d0) then                                 ! if growth is positive 
          if ((oldTotLfCBm + mass_to_allocate)/TotSgCBm .lt. f_above) then  !   if adding all growth to the leaves and f_above is still below ideal, 
            TotLfCBm = oldTotLfCBm + mass_to_allocate                       !     add all growth to leaves
          elseif ((oldTotLfCBm)/TotSgCBm .gt. f_above) then                 !   elif adding all growth to the roots and f_above is still above ideal, 
            TotRtCBm = oldTotRtCBm + mass_to_allocate                       !     add all growth to roots
          else                                                              !   else
            TotLfCBm = f_above * TotSgCBm                                   !     add growth to leaves and roots to achieve ideal ratio 
            TotRtCBm = TotSgCBm - TotLfCBm
          endif
        else                                                                ! if growth is negative (death)
          if ((oldTotLfCBm + mass_to_allocate)/TotSgCBm .gt. f_above) then  !   if subtracting all death from the leaves and f_above is still above ideal, 
            TotLfCBm = oldTotLfCBm + mass_to_allocate                       !     subtract all death from leaves
          elseif ((oldTotLfCBm)/TotSgCBm .lt. f_above) then                 !   elif subtracting all death from the roots and f_above is still below ideal, 
            TotRtCBm = oldTotRtCBm + mass_to_allocate                       !     subtract all death from roots
          else                                                              !   else
            TotLfCBm = f_above * TotSgCBm                                   !     subtract death from leaves and roots to achieve ideal ratio 
            TotRtCBm = TotSgCBm - TotLfCBm
          endif
        endif

      

      ! ! Allocate biomass (grow or cull) proportional to current proportions (leaf or root)
      ! ! f_above_actual = oldTotLfCBm/oldTotSgCBm
      ! ! f_below_actual = oldTotRtCBm/oldTotSgCBm
      ! ! newTotLfCBm = f_above_actual * newTotSgCBm
      ! ! newTotRtCBm = f_below_actual * newTotSgCBm

      ! TotLfCBm = oldTotLfCBm/oldTotSgCBm * TotSgCBm
      ! TotRtCBm = oldTotRtCBm/oldTotSgCBm * TotSgCBm

# if defined DYNAMIC_COVERAGE
! update bed-density/habitat area --------------------------------------------------------------------

      ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
      ! desiredHabArea              [m2.hab]                    The desired habitat area given the biomass and optimal density
      ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
      ! opt_sg_bed_density          [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)
      ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
      ! translocatableArea          [m2.hab]                    Area that can be translocated (shrink or expand) as limited by the translocation rate
      ! trans_rate                  [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
      ! GridArea                    [m2.grid]                   Area of grid cell

      ! [mol.C/m2.hab]       = [g.drywt/m2.hab]   [mol.C/g.drywt]
      ! opt_sg_bed_C_density = opt_sg_bed_density / SgMperC

      ! TotSgCBm / desiredHabArea = opt_sg_bed_C_density
      ! desiredHabArea = TotSgCBm / opt_sg_bed_C_density

        desiredHabArea = TotSgCBm / (opt_sg_bed_density/SgMperC)
  
        if ( (TotSgCBm / HabArea) .lt. (opt_sg_bed_density/SgMperC) ) then  ! if density is too low, then shrink hab area
          ! [m2]               = [mol.C]             [m2/mol.C]
          ! translocatableArea = translocatable_mass     / current_density
          translocatableArea   = (TotSgCBm * trans_rate) / (TotSgCBm / HabArea)
          if (HabArea - translocatableArea .lt. desiredHabArea) then
            HabArea = desiredHabArea
          else
            HabArea = HabArea - translocatableArea
          endif
          if (HabArea .lt. 0d0) then 
            HabArea = 0d0
          endif
        else                                                        ! if density is too high, then expand hab area
        ! [m2]               = [mol.C]             [m2/mol.C]
        ! translocatableArea = translocatable_mass     / optimal_density
          translocatableArea   = (TotSgCBm * trans_rate) / (opt_sg_bed_density / SgMperC)
          if (HabArea + translocatableArea .gt. desiredHabArea) then
            HabArea = desiredHabArea
          else
            HabArea = HabArea + translocatableArea
          endif
          if (HabArea .gt. GridArea) then ! yt_edit need to fix this to interact with other habitat types
            HabArea = GridArea
          endif
        endif

      ! update seagrass fraction 
      ! p_sgrass(Nsg)               [m2.sg.hab m-2.grid]        (0~1)  Seagrass coverage (every species)
        p_sgrass(species) = HabArea / GridArea ! yt_edit need to fix this to interact with other habitat types
# endif


! leaf-root biomass translocation --------------------------------------------------------------------

      ! Translocate biomass
      ! yt_edit fix this to include lateral spreading
      
      ! translocatable_mass         [mol.C]                     Biomass that can be translocated between leaf and root as limited by the translocation rate
      ! trans_rate                  [mol.C/mol.C day-1]         Maximum translocation rate as fraction of mass translocatable per day
      ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
      ! TotLfCBm                    [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
      ! TotRtCBm                    [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
      ! f_above                     [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction  

        translocatable_mass = TotSgCBm * trans_rate
  
        ! write(*,*) 'yt_debug: f_actual =', TotLfCBm / TotSgCBm, '   f_above =', f_above
  
        if (TotLfCBm / TotSgCBm .lt. f_above) then                              ! translocate from root to leaf if leaf ratio is below optimum
          if ( (TotLfCBm + translocatable_mass) / TotSgCBm .lt. f_above) then   ! if leaf needs more than translocatable amount
            ! write(*,*) 'yt_debug: LEAF needs MORE than translocatable amount'
            TotLfCBm = TotLfCBm + translocatable_mass
          else                                                                  ! if leaf needs less than translocatable amount
            ! write(*,*) 'yt_debug: LEAF needs LESS than translocatable amount'
            TotLfCBm = TotSgCBm * f_above
          endif
          TotRtCBm = TotSgCBm - TotLfCBm
        else                                                                    ! translocate from leef to root if leaf ratio is above optimum
          if ( (TotLfCBm - translocatable_mass) / TotSgCBm .gt. f_above) then   ! if root needs more than translocatable amount
            ! write(*,*) 'yt_debug: ROOT needs MORE than translocatable amount'
            TotRtCBm = TotRtCBm + translocatable_mass
          else                                                                  ! if root needs less than translocatable amount
            ! write(*,*) 'yt_debug: ROOT needs LESS than translocatable amount'
            TotRtCBm = TotSgCBm * (1.0d0 - f_above)
          endif
          TotLfCBm = TotSgCBm - TotRtCBm
        endif
  
      ! write(*,*) 'yt_debug: f_actual =', TotLfCBm / TotSgCBm, '   f_above =', f_above
      ! write(*,*) '  '

      else !=== if TotSgCBm <=0 ========================================================
        
        SgCBmF(:) = 0.0d0
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
        SgNBmF(:) = 0.0d0
        SgPBmF(:) = 0.0d0
# endif
        TotSgCBm  = 0.0d0
        TotLfCBm  = 0.0d0
        TotRtCBm  = 0.0d0
# if defined DYNAMIC_COVERAGE
        p_sgrass(species) = 0.0d0
        HabArea           = 0.0d0
# endif
      endif

! update carbon-biomass per area ---------------------------------------------------------------------

      ! adjust SgCBmF according to new HabArea
      ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
      ! LfCBm                       [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
      ! RtCBm                       [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
      ! TotSgCBm                    [mol.C]                     Total seagrass carbon-biomass in grid cell
      ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
      ! [umol.C/m2.hab] = [mol.C]  / [m2.hab] (1000000umol/mol)

!      rNC = SgNBmF(iNt)/SgCBmF(iCt) ! N/C ratio
!      rPC = SgPBmF(iPt)/SgCBmF(iCt) ! P/C ratio
!      SgCBmF(:)         = TotSgCBm / HabArea  * 1000000d0 *rSgCBm(:)
!      SgNBmF(:)         = TotSgCBm*rNC / HabArea  * 1000000d0 *rSgNBm(:)
!      SgPBmF(:)         = TotSgCBm*rPC / HabArea  * 1000000d0 *rSgPBm(:)
      LfCBm             = TotLfCBm / HabArea  * 1000000d0
      RtCBm             = TotRtCBm / HabArea  * 1000000d0


! update dry-weight biomass --------------------------------------------------------------------------

      ! TotSgDryBm                  [g.drywt]                   Total seagrass dry weight biomass in grid cell
      ! TotLfDryBm                  [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
      ! TotRtDryBm                  [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
      ! TotSgCBm                    [mol.C]                     Total seagrass carbon-biomass in grid cell
      ! TotLfCBm                    [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
      ! TotRtCBm                    [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
      ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
      TotSgDryBm = TotSgCBm * SgMperC
      TotLfDryBm = TotLfCBm * SgMperC
      TotRtDryBm = TotRtCBm * SgMperC

      ! SgDryBm                     [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
      ! LfDryBm                     [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
      ! RtDryBm                     [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
      ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
      ! LfCBm                       [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
      ! RtCBm                       [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
      ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
      ! [g/m2.hab] = [umol.C/m2.hab] [g/mol.C] (mol/1000000umol)
      SgDryBm      = SgCBmF(iCt)     * SgMperC * 0.000001d0
      LfDryBm      = LfCBm           * SgMperC * 0.000001d0
      RtDryBm      = RtCBm           * SgMperC * 0.000001d0


! update leaf area index -----------------------------------------------------------------------------

      ! LAI                         []                          Leaf area index: one-sided green leaf area per unit ground surface area
      ! TotLfDryBm                  [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
      ! rho_lf                      [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
      ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
      ! [m2.lf/m2.hab] = [g.lf]     [m2.lf/kg.lf] [1/m2.hab] (kg.lf/1000g.lf)
      LAI              = TotLfDryBm / rho_lf      / HabArea  * 0.001d0

! update saved output values -------------------------------------------------------------------------

      ! TotLA                       [m2.lf]                     Total one-sided green leaf area in grid
      ! LAI                         []                          Leaf area index: one-sided green leaf area per unit ground surface area
      ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
      ! [m2.lf] = [m2.lf/m2.hab] [m2.hab]
      TotLA     = LAI            * HabArea

      ! GridELAP                    [m2.lf.proj m-2.grid]       Effective leaf area projection on ground (whole grid)
      ! ELAP                        [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
      ! p_sgrass(Nsg)               [m2.sg.hab m-2.grid]        (0~1)  Seagrass coverage (every species)

      ! [m2.lf.proj/m2.grid] = [m2.lf.proj/m2.hab] [m2.hab/m2.grid]
      GridELAP               = ELAP                * p_sgrass(species)

    endif

! ====================================================================================================
! End
! ====================================================================================================

! ********************************************************************* !
!   Print section
! ********************************************************************* !
# if defined SEAGRASS_TESTMODE
    IF( date*86400.0d0 >= dsec ) THEN
        if(dsec == 0.0d0) then
          dsec=date*86400.0d0+SEAGRASS_OUTPUT_INTERVAL*60.0d0
          ! Write lavels at the first step
          write(60,'(*(g0:,", "))') 'Time', 'PFD' &
# if defined CLUMPED_ISOTOPE 
           ,'SgCBmF','SgCBmF_13C','SgCBmF_47D'    &
# elif defined CARBON_ISOTOPE  || defined CARBON_TRACE
           ,'SgCBmF','SgCBmF_13C'  &
# else
           ,'SgCBmF'               &
# endif
# if defined NITROGEN_ISOTOPE  || defined NITROGEN_TRACE
           ,'SgNBmF','SgNBmF_15N'  &
# else
           ,'SgNBmF'               &
# endif
# if defined PHOSPHOROUS_TRACE
           ,'SgPBmF','SgPBmF_P1'   &
# else
           ,'SgPBmF'               &
# endif
           ,'TotSgCBm' ,'TotLfCBm', 'TotRtCBm' &
           ,'p_sgrass'

        else
          dsec=dsec+SEAGRASS_OUTPUT_INTERVAL*60.0d0
        endif
        
        ! Write data
        write(60,'(*(g0:,", "))') date, PFD &
           ,SgCBmF(:),SgNBmF(:),SgPBmF(:) &
           ,TotSgCBm ,TotLfCBm, TotRtCBm  &
           ,p_sgrass(species)
!        write(*,*) 'Seagrass Time (day): ', date 
    endif
# endif

!     !!! yt_debug >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!     if (isnan(SGRASS(ng)%SgCBmF(species,i,j)) .or. SGRASS(ng)%SgCBmF(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%SgCBmF(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     SgCBmF =', SGRASS(ng)%SgCBmF(species,i,j)
!       error stop
!     endif
!     if (isnan(SGRASS(ng)%LfCBm(species,i,j)) .or. SGRASS(ng)%LfCBm(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%LfCBm(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     LfCBm =', SGRASS(ng)%LfCBm(species,i,j)
!       error stop
!     endif
!     if (isnan(SGRASS(ng)%RtCBm(species,i,j)) .or. SGRASS(ng)%RtCBm(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%RtCBm(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     RtCBm =', SGRASS(ng)%RtCBm(species,i,j)
!       error stop
!     endif
!     if (isnan(SGRASS(ng)%TotSgCBm(species,i,j)) .or. SGRASS(ng)%TotSgCBm(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%TotSgCBm(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     TotSgCBm =', SGRASS(ng)%TotSgCBm(species,i,j)
!       error stop
!     endif
!     if (isnan(SGRASS(ng)%TotLfCBm(species,i,j)) .or. SGRASS(ng)%TotLfCBm(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%TotLfCBm(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     TotLfCBm =', SGRASS(ng)%TotLfCBm(species,i,j)
!       error stop
!     endif
!     if (isnan(SGRASS(ng)%TotRtCBm(species,i,j)) .or. SGRASS(ng)%TotRtCBm(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%TotRtCBm(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     TotRtCBm =', SGRASS(ng)%TotRtCBm(species,i,j)
!       error stop
!     endif
!     if (isnan(SGRASS(ng)%HabArea(species,i,j)) .or. SGRASS(ng)%HabArea(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%HabArea(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     HabArea =', SGRASS(ng)%HabArea(species,i,j)
!       error stop
!     endif
!     if (isnan(SGRASS(ng)%TotSgDryBm(species,i,j)) .or. SGRASS(ng)%TotSgDryBm(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%TotSgDryBm(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     TotSgDryBm =', SGRASS(ng)%TotSgDryBm(species,i,j)
!       error stop
!     endif
!     if (isnan(SGRASS(ng)%TotLfDryBm(species,i,j)) .or. SGRASS(ng)%TotLfDryBm(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%TotLfDryBm(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     TotLfDryBm =', SGRASS(ng)%TotLfDryBm(species,i,j)
!       error stop
!     endif
!     if (isnan(SGRASS(ng)%TotRtDryBm(species,i,j)) .or. SGRASS(ng)%TotRtDryBm(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%TotRtDryBm(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     TotRtDryBm =', SGRASS(ng)%TotRtDryBm(species,i,j)
!       error stop
!     endif
!     if (isnan(SGRASS(ng)%SgDryBm(species,i,j)) .or. SGRASS(ng)%SgDryBm(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%SgDryBm(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     SgDryBm =', SGRASS(ng)%SgDryBm(species,i,j)
!       error stop
!     endif
!     if (isnan(SGRASS(ng)%LfDryBm(species,i,j)) .or. SGRASS(ng)%LfDryBm(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%LfDryBm(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     LfDryBm =', SGRASS(ng)%LfDryBm(species,i,j)
!       error stop
!     endif
!     if (isnan(SGRASS(ng)%RtDryBm(species,i,j)) .or. SGRASS(ng)%RtDryBm(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%RtDryBm(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     RtDryBm =', SGRASS(ng)%RtDryBm(species,i,j)
!       error stop
!     endif
!     if (isnan(SGRASS(ng)%LAI(species,i,j)) .or. SGRASS(ng)%LAI(species,i,j) > 1.0d30 &
!       .or. SGRASS(ng)%LAI(species,i,j) < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     LAI =', SGRASS(ng)%LAI(species,i,j)
!       error stop
!     endif

!     if (isnan(DIC_uptake_rate_from_ocean) .or. DIC_uptake_rate_from_ocean > 1.0d30 &
!       .or. ocean_DIC_conc - DIC_uptake_rate_from_ocean*dt/dz < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     DIC_uptake_rate_from_ocean =', DIC_uptake_rate_from_ocean
!       write(*,*) 'yt_debug:     ocean_DIC_conc =', ocean_DIC_conc
!       write(*,*) 'yt_debug:     ocean_DIC_conc - DIC_uptake_rate_from_ocean*dt/dz =' &
!         , ocean_DIC_conc - DIC_uptake_rate_from_ocean*dt/dz
!       error stop
!     endif
!     if (isnan(DO__uptake_rate_from_ocean) .or. DO__uptake_rate_from_ocean > 1.0d30 &
!       .or. ocean_DO__conc - DO__uptake_rate_from_ocean*dt/dz < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     DO__uptake_rate_from_ocean =', DO__uptake_rate_from_ocean
!       write(*,*) 'yt_debug:     ocean_DO__conc =', ocean_DO__conc
!       write(*,*) 'yt_debug:     ocean_DO__conc - DO__uptake_rate_from_ocean*dt/dz =' &
!         , ocean_DO__conc - DO__uptake_rate_from_ocean*dt/dz ! [umol/L] - [mmol  m-2 s-1][s/m](m3/1000L)(1000umol/mmol)
!       write(*,*) 'yt_debug:     dt =', dt
!       write(*,*) 'yt_debug:     dz =', dz
!       write(*,*) 'yt_debug:     DO_uptake_possible(0) =', DO_uptake_possible(0)
!       write(*,*) 'yt_debug:     0.5*ocean_DO__conc*dz/dt*1000', 0.5d0*ocean_DO__conc*dz/dt*1000.0d0
!       write(*,*) 'yt_debug:     Resp_rate*0.001 =', Resp_rate*0.001d0
!       write(*,*) 'yt_debug:     Resp_pos*0.001 =', Resp_pos*0.001d0
!       write(*,*) 'yt_debug:     Resp_req*0.001 =', Resp_req*0.001d0
!       write(*,*) 'yt_debug:     -Growth_rate*0.001', -1d0*Growth_rate*0.001d0
! #  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
!       write(*,*) 'yt_debug:     2 * sum(delta_sed_NO3(:))', 2d0 * sum(delta_sed_NO3(:))
! #  endif
!       error stop
!     endif
! # if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
!     if (isnan(NO3_uptake_rate_from_ocean) .or. NO3_uptake_rate_from_ocean > 1.0d30 &
!       .or. ocean_NO3_conc - NO3_uptake_rate_from_ocean*dt/dz < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     NO3_uptake_rate_from_ocean =', NO3_uptake_rate_from_ocean
!       write(*,*) 'yt_debug:     ocean_NO3_conc =', ocean_NO3_conc
!       write(*,*) 'yt_debug:     ocean_NO3_conc - NO3_uptake_rate_from_ocean*dt/dz =' &
!         , ocean_NO3_conc - NO3_uptake_rate_from_ocean*dt/dz
!       error stop
!     endif
!     if (isnan(NH4_uptake_rate_from_ocean) .or. NH4_uptake_rate_from_ocean > 1.0d30 &
!       .or. ocean_NH4_conc - NH4_uptake_rate_from_ocean*dt/dz < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     NH4_uptake_rate_from_ocean =', NH4_uptake_rate_from_ocean
!       write(*,*) 'yt_debug:     ocean_NH4_conc =', ocean_NH4_conc
!       write(*,*) 'yt_debug:     ocean_NH4_conc - NH4_uptake_rate_from_ocean*dt/dz =' &
!         , ocean_NH4_conc - NH4_uptake_rate_from_ocean*dt/dz
!       error stop
!     endif
!     if (isnan(PO4_uptake_rate_from_ocean) .or. PO4_uptake_rate_from_ocean > 1.0d30 &
!       .or. ocean_PO4_conc - PO4_uptake_rate_from_ocean*dt/dz < 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     PO4_uptake_rate_from_ocean =', PO4_uptake_rate_from_ocean
!       write(*,*) 'yt_debug:     ocean_PO4_conc =', ocean_PO4_conc
!       write(*,*) 'yt_debug:     ocean_PO4_conc - PO4_uptake_rate_from_ocean*dt/dz =' &
!         , ocean_PO4_conc - PO4_uptake_rate_from_ocean*dt/dz
!       error stop
!     endif
! # endif
! # if defined SEAGRASS_LEAF_POM
!     if (isnan(POC_uptake_rate_from_ocean) .or. abs(POC_uptake_rate_from_ocean) > 1.0d30 &
!       .or. POC_uptake_rate_from_ocean > 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     POC_uptake_rate_from_ocean =', POC_uptake_rate_from_ocean
!       error stop
!     endif
! #  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
!     if (isnan(PON_uptake_rate_from_ocean) .or. abs(PON_uptake_rate_from_ocean) > 1.0d30 &
!       .or. PON_uptake_rate_from_ocean > 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     PON_uptake_rate_from_ocean =', PON_uptake_rate_from_ocean
!       error stop
!     endif
!     if (isnan(POP_uptake_rate_from_ocean) .or. abs(POP_uptake_rate_from_ocean) > 1.0d30 &
!       .or. POP_uptake_rate_from_ocean > 0.0d0) then
!       write(*,*) 'yt_debug: mod_seagrass.F      i =', i, '   j =', j
!       write(*,*) 'yt_debug:     POP_uptake_rate_from_ocean =', POP_uptake_rate_from_ocean
!       error stop
!     endif
! #  endif
! # endif


!     !!! yt_debug <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    RETURN
    
  END SUBROUTINE seagrass

  

!!! ==================================================================================================
!!! **************************************************************************************************
!!!                   MAIN LOOP OF SEAGRASS MODEL IF THERE IS NO SEAGRASS COVERAGE
!!! **************************************************************************************************
!!! ==================================================================================================
  SUBROUTINE no_seagrass          &
!   input parameters
    ( ng                          &   !                           nested grid number; 
    , species                     &   !                           seagrass species id
    , i, j                        &   !                           i,j: position
    , dz                          &   ! [m]                       vertical layer thickness (bottom water column layer)
    , date                        &   ! Date (days since x or elapsed days) yt_edit best if in phase with clock to maintain consistency on restart
    , dt                          &   ! [s]                       time step
    )
! ====================================================================================================
! Module dependencies
! ====================================================================================================
    USE mod_reef_ecosys_param
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    USE mod_sedecosys,    ONLY : SEDECO
# endif

! ====================================================================================================
! Variable declarations
! ====================================================================================================
    implicit none

! >>> Inputs <<< !
    integer, intent(in)   :: ng                         !                             nested grid number
    integer, intent(in)   :: species                    !                             seagrass species id
    integer, intent(in)   :: i, j                       !                             i,j: position
    real(8), intent(in)   :: dz                         ! [m]                         vertical layer thickness (bottom water column layer)
    real(8), intent(in)   :: date                       ! [days]                      Date (days since x or elapsed days) yt_edit best if in phase with clock to maintain consistency on restart
    real(8), intent(in)   :: dt                         ! [s]                         Time step interval duration

! >>> Housekeeping Vars <<< !
    real(8), pointer      :: day                        !                             Simulation whole day (integer round down)
    logical               :: is_growth_interval         !                             Boolean flag for if the current interval is a growth interval

! >>> Saved Constants <<< !
    ! model params
    real(8), pointer      :: GridArea                   ! [m2.grid]                   Area of grid cell

! ====================================================================================================
! Variable initializations
! ====================================================================================================

! >>> Housekeeping Vars <<< !
    ! update at start of every loop before everything else
    Day                 =>  SGRASS(ng)%Day                            (i,j) !                             Simulation whole day (integer round down)
    is_growth_interval  =   .false.                                         !                             Boolean flag for if the current interval is a growth interval

! >>> Saved Constants <<< !
    ! model params
    GridArea            =>  SGRASS(ng)%GridArea                       (i,j) ! [m2]                        Area of grid cells

! >>> Delta Values <<< !      amount to add to stored variables at end of the calculation time step to update values: positive adds, negative subtracts (erased each time step)
    ! values to write to sediment module
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    SEDECO(ng)%sg_dDIC_dt(:,:,species,i,j) = 0.0d0                  ! [umol.DIC L-1 s-1]          Rate of change in DIC due to flux from roots to sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dDOC_dt(:,:,species,i,j) = 0.0d0                  ! [umol.DIC L-1 s-1]          Rate of change in DIC due to flux from roots to sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dO2_dt (:,species,i,j) = 0.0d0                    ! [umol.DO L-1 s-1]           Rate of change in DO due to flux from roots to sediment (positive means seagrass to sediment)
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    SEDECO(ng)%sg_dNO3_dt(:,:,species,i,j) = 0.0d0                  ! [umol.NO3 L-1 s-1]          Rate of change in NO3 due to flux from roots to sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dNH4_dt(:,:,species,i,j) = 0.0d0                  ! [umol.NH4 L-1 s-1]          Rate of change in NH4 due to flux from roots to sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dPO4_dt(:,:,species,i,j) = 0.0d0                  ! [umol.PO4 L-1 s-1]          Rate of change in PO4 due to flux from roots to sediment (positive means seagrass to sediment)
# endif
# if defined SEAGRASS_ROOT_POM
    SEDECO(ng)%sg_dPOC_dt(:,:,species,i,j) = 0.0d0                  ! [umol.C L-1 s-1]            Rate of change in POC due to root death in sediment (positive means seagrass to sediment)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    SEDECO(ng)%sg_dPON_dt(:,:,species,i,j) = 0.0d0                  ! [umol.N L-1 s-1]            Rate of change in PON due to root death in sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dPOP_dt(:,:,species,i,j) = 0.0d0                  ! [umol.P L-1 s-1]            Rate of change in POP due to root death in sediment (positive means seagrass to sediment)
#  endif
# endif

! ====================================================================================================
! ----------------------------------------------------------------------------------------------------
!                                  HOUSE KEEPING AT START OF LOOP
! ----------------------------------------------------------------------------------------------------
! ====================================================================================================

    if (date .gt. aint(Day*24.0d0)/24.0d0 ) then
      is_growth_interval = .true.
      Day = (aint(Day*24.0d0)+1.0d0)/24.0d0  !! TN: One hour interval???????????????????????????????????????????????????
      ! if(i == 50 .and. j == 50) then
      !   write(*,*) "yt_debug:    Day = ", Day, "   date = ", date
      ! endif
    endif

! ====================================================================================================
! ----------------------------------------------------------------------------------------------------
!                                UPDATE AT END OF MAIN INTERVAL
! ----------------------------------------------------------------------------------------------------
! ====================================================================================================
    
    ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    SGRASS(ng)%SgCBmF(:,species,i,j) = 0.0d0
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! SgNBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root nitrogen-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    SGRASS(ng)%SgNBmF(:,species,i,j) = 0.0d0
    ! SgPBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root phosphorus-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    SGRASS(ng)%SgPBmF(:,species,i,j) = 0.0d0
# endif
    ! TotSgCBmF                   [mol.C]                     Total seagrass carbon-biomass in grid cell. updates on fine time interval
    SGRASS(ng)%TotSgCBmF(species,i,j) = 0.0d0

    ! GridPhot                    [umol.C m-2.grid s-1]       Seagrass gross photosynthesis rate per unit ground area (whole grid)
    SGRASS(ng)%GridPhot(species,i,j) = 0.0d0
    ! GridGrow                    [umol.C m-2.grid s-1]       Seagrass growth rate per unit ground area (whole grid)
    SGRASS(ng)%GridGrow(species,i,j) = 0.0d0
    ! PhotLim                     []                          Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon uptake; 3 = Nitrogen uptake; 4 = Phosphorus uptake
    SGRASS(ng)%PhotLim(species,i,j) = 0
    ! PhotLim                     []                          Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon uptake; 3 = Nitrogen uptake; 4 = Phosphorus uptake
    SGRASS(ng)%GrowLim(species,i,j) = 0
    ! GridResp                    [umol.C m-2.grid s-1]       Seagrass respiration rate per unit ground area (whole grid)
    SGRASS(ng)%GridResp(species,i,j) = 0.0d0
    ! GridNetPhot                 [umol.C m-2.grid s-1]       Seagrass net photosynthesis rate per unit ground area (whole grid)
    SGRASS(ng)%GridNetPhot(species,i,j) = 0.0d0
    ! GridDieoff                  [umol.C m-2.grid s-1]       Seagrass dieoff rate carbon biomass per unit ground area (whole grid)
    SGRASS(ng)%GridDieoff(species,i,j) = 0.0d0


! ====================================================================================================
! ----------------------------------------------------------------------------------------------------
!                               UPDATE AT END OF GROWTH INTERVAL
! ----------------------------------------------------------------------------------------------------
! ====================================================================================================
    
    if (is_growth_interval) then  

! update total carbon-biomass ------------------------------------------------------------------------

      ! TotSgCBm                    [mol.C]                     Total seagrass carbon-biomass in grid cell
      SGRASS(ng)%TotSgCBm(species,i,j) = 0.0d0


! allocate growth ------------------------------------------------------------------------------------

      ! TotLfCBm                    [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
      ! TotRtCBm                    [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
      SGRASS(ng)%TotLfCBm(species,i,j) = 0.0d0
      SGRASS(ng)%TotRtCBm(species,i,j) = 0.0d0

      ! TotLA                       [m2.lf]                     Total one-sided green leaf area in grid
      SGRASS(ng)%TotLA(species,i,j) = 0.0d0 
      ! GridELAP                    [m2.lf.proj m-2.grid]       Effective leaf area projection on ground (whole grid)
      SGRASS(ng)%GridELAP(species,i,j) = 0.0d0


# if defined DYNAMIC_COVERAGE
! update bed-density/habitat area --------------------------------------------------------------------
      ! yt_edit to do implement lateral transfer???
# endif

! update carbon-biomass per area ---------------------------------------------------------------------
      ! LfCBm                       [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
      ! RtCBm                       [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
      SGRASS(ng)%LfCBm(species,i,j) = 0.0d0
      SGRASS(ng)%RtCBm(species,i,j) = 0.0d0

! update dry-weight biomass --------------------------------------------------------------------------

      ! TotSgDryBm                  [g.drywt]                   Total seagrass dry weight biomass in grid cell
      ! TotLfDryBm                  [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
      ! TotRtDryBm                  [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
      SGRASS(ng)%TotSgDryBm(species,i,j) = 0.0d0
      SGRASS(ng)%TotLfDryBm(species,i,j) = 0.0d0
      SGRASS(ng)%TotRtDryBm(species,i,j) = 0.0d0

      ! SgDryBm                     [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
      ! LfDryBm                     [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
      ! RtDryBm                     [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
      SGRASS(ng)%SgDryBm(species,i,j) = 0.0d0
      SGRASS(ng)%LfDryBm(species,i,j) = 0.0d0
      SGRASS(ng)%RtDryBm(species,i,j) = 0.0d0


! update leaf area index -----------------------------------------------------------------------------

      ! LAI                         []                          Leaf area index: one-sided green leaf area per unit ground surface area
      SGRASS(ng)%LAI(species,i,j) = 0.0d0

    endif

! ====================================================================================================
! End
! ====================================================================================================

    RETURN

  END SUBROUTINE no_seagrass



  ! SUBROUTINE get_seagrass_data(ng, species, i, j, TotSgDryBm, TotSgCBm,TotLfCBm,TotRtCBm)
  !   implicit none
  !   integer, intent(in)    :: ng, species, i, j !                             nested grid number ID, species ID number, i & j positions
  !   real(8), intent(out)   :: TotSgDryBm        ! [g.drywt]                   Total seagrass dry weight biomass in grid cell
  !   real(8), intent(out)   :: TotSgCBm          ! [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
  !   real(8), intent(out)   :: TotLfCBm          ! [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
  !   real(8), intent(out)   :: TotRtCBm          ! [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell

  !   write(*,*) "yt_debug: yo"

  !   TotSgDryBm  = SGRASS(ng)%TotSgDryBm                     (species,i,j)  ! [g.drywt]                   Total seagrass dry weight biomass in grid cell
  !   TotSgCBm    = SGRASS(ng)%TotSgCBm                       (species,i,j)  ! [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
  !   TotLfCBm    = SGRASS(ng)%TotLfCBm                       (species,i,j)  ! [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
  !   TotRtCBm    = SGRASS(ng)%TotRtCBm                       (species,i,j)  ! [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
    
  !   RETURN
  ! END SUBROUTINE get_seagrass_data
#endif
END MODULE mod_seagrass  
