
!!!=== Copyright (c) 2012-2024 Takashi NAKAMURA  =====

#include "cppdefs.h"


!!! ==================================================================================================
!!! **************************************************************************************************
!!! **                                                                                              **
!!! **                                       SEAGRASS MODEL                                         **
!!! **                                                                                              **
!!! ** -------------------------------------------------------------------------------------------- **
!!! **                                                                                              **
!!! **    This is the updated seagrass module devloped by Yuta A. Takagi (2023)                     **
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

  integer, parameter :: Nsg = 1     ! Number of seagrass species
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
  real(8), allocatable :: SeagrassRootProf(:, :, :) ! (ng, species, k) ! Profile of seagrass root biomass: fraction in each biological sediment layer 
# endif

  TYPE T_SGRASS ! create a class for all seagrass values (for a given nested grid)
    !                                                         (c s i j l)                               the columns correspond to indexes for: c = chemical species; s = species_id_number; i = i-position; j = j-position; l = sediment layer
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
    real(8), pointer :: Lf_DIC_uptake_max                       (:    )   ! [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves
    real(8), pointer :: k_lf_DIC                                (:    )   ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer :: Rt_DIC_flux_max                         (:    )   ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    real(8), pointer :: k_rt_DIC                                (:    )   ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
# endif
    ! photoynthesis: nutrient-limit
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    real(8), pointer :: Lf_NO3_uptake_max                       (:    )   ! [umol.NO3 m-2.lf s-1]       Maximum uptake rate of NO3 in the leaves
    real(8), pointer :: k_lf_NO3                                (:    )   ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth
    real(8), pointer :: Lf_NH4_uptake_max                       (:    )   ! [umol.NH4 m-2.lf s-1]       Maximum uptake rate of NH4 in the leaves
    real(8), pointer :: k_lf_NH4                                (:    )   ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth
    real(8), pointer :: Lf_PO4_uptake_max                       (:    )   ! [umol.PO4 m-2.lf s-1]       Maximum uptake rate of PO4 in the leaves
    real(8), pointer :: k_lf_PO4                                (:    )   ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer :: Rt_NO3_flux_max                         (:    )   ! [umol.NO3 g-1.rt.drywt s-1] Maximum uptake rate of NO3 in the roots 
    real(8), pointer :: k_rt_NO3                                (:    )   ! [umol.NO3 L-1]              Half saturation consentration of NO3 in soil pore water for growth
    real(8), pointer :: Rt_NH4_flux_max                         (:    )   ! [umol.NH4 g-1.rt.drywt s-1] Maximum uptake rate of NH4 in the roots 
    real(8), pointer :: k_rt_NH4                                (:    )   ! [umol.NH4 L-1]              Half saturation consentration of NH4 in soil pore water for growth
    real(8), pointer :: Rt_PO4_flux_max                         (:    )   ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    real(8), pointer :: k_rt_PO4                                (:    )   ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
# endif
    ! respiration: DO-limit
    real(8), pointer :: Lf_DO_uptake_max                        (:    )   ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves
    real(8), pointer :: k_lf_DO                                 (:    )   ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer :: Rt_DO_flux_max                          (:    )   ! [umol.DO g-1.rt.drywt s-1]  Maximum uptake rate of DO in the roots 
    real(8), pointer :: k_rt_DO                                 (:    )   ! [umol.DO L-1]               Half saturation consentration of DO in soil pore water for growth
# endif
    ! respiration: required
    real(8), pointer :: R_basal                                 (:    )   ! [umol.C m-2.leaf s-1]       Maximum respiration rate
    real(8), pointer :: Turnover_rate                           (:    )   ! [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day
    ! dieoff when respiration is insufficient
    real(8), pointer :: max_anoxic_dieoff_rate                  (:    )   ! [mol.C mol-1.C day-1]       maximum dieoff due to anoxia in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
    ! Update at end of growth interval
    ! allocate growth
    ! update bed-density/habitat area
    real(8), pointer :: trans_rate                              (:    )   ! [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
    real(8), pointer :: opt_sg_bed_density                      (:    )   ! [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)

! >>> Saved Values <<< !
    ! Update at end of main interval
    real(8), pointer :: SgCBmF                                  (:,:,:)   ! [umol.C m-2.sg.hab]         iSgSgCBm      Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
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
    integer, pointer :: PhotLim                                 (:,:,:)   ! []                          iSgPhotLim      Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon uptake; 3 = Nitrogen uptake; 4 = Phosphorus uptake
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
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    USE mod_sedecosys, ONLY : Nsed
# endif

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
    allocate( SGRASS(ng)%root_mass_profile                          (Nsg, Nsed(ng))        ) ! []                          Mass ratio of roots in each layer (total must sum to 1.0)
# endif    
    ! photoynthesis: light-limit
    allocate( SGRASS(ng)%P_max                                      (Nsg)                  ) ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate  
    allocate( SGRASS(ng)%I_k                                        (Nsg)                  ) ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve
    ! photoynthesis: DIC-limit
    allocate( SGRASS(ng)%Lf_DIC_uptake_max                          (Nsg)                  ) ! [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves
    allocate( SGRASS(ng)%k_lf_DIC                                   (Nsg)                  ) ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    allocate( SGRASS(ng)%Rt_DIC_flux_max                            (Nsg)                  ) ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    allocate( SGRASS(ng)%k_rt_DIC                                   (Nsg)                  ) ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
# endif
    ! photoynthesis: nutrient-limit
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    allocate( SGRASS(ng)%Lf_NO3_uptake_max                          (Nsg)                  ) ! [umol.NO3 m-2.lf s-1]       Maximum uptake rate of NO3 in the leaves
    allocate( SGRASS(ng)%k_lf_NO3                                   (Nsg)                  ) ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth
    allocate( SGRASS(ng)%Lf_NH4_uptake_max                          (Nsg)                  ) ! [umol.NH4 m-2.lf s-1]       Maximum uptake rate of NH4 in the leaves
    allocate( SGRASS(ng)%k_lf_NH4                                   (Nsg)                  ) ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth
    allocate( SGRASS(ng)%Lf_PO4_uptake_max                          (Nsg)                  ) ! [umol.PO4 m-2.lf s-1]       Maximum uptake rate of PO4 in the leaves
    allocate( SGRASS(ng)%k_lf_PO4                                   (Nsg)                  ) ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    allocate( SGRASS(ng)%Rt_NO3_flux_max                            (Nsg)                  ) ! [umol.NO3 g-1.rt.drywt s-1] Maximum uptake rate of NO3 in the roots 
    allocate( SGRASS(ng)%k_rt_NO3                                   (Nsg)                  ) ! [umol.NO3 L-1]              Half saturation consentration of NO3 in soil pore water for growth
    allocate( SGRASS(ng)%Rt_NH4_flux_max                            (Nsg)                  ) ! [umol.NH4 g-1.rt.drywt s-1] Maximum uptake rate of NH4 in the roots 
    allocate( SGRASS(ng)%k_rt_NH4                                   (Nsg)                  ) ! [umol.NH4 L-1]              Half saturation consentration of NH4 in soil pore water for growth
    allocate( SGRASS(ng)%Rt_PO4_flux_max                            (Nsg)                  ) ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    allocate( SGRASS(ng)%k_rt_PO4                                   (Nsg)                  ) ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
# endif
    ! respiration: DO-limit
    allocate( SGRASS(ng)%Lf_DO_uptake_max                           (Nsg)                  ) ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves
    allocate( SGRASS(ng)%k_lf_DO                                    (Nsg)                  ) ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    allocate( SGRASS(ng)%Rt_DO_flux_max                             (Nsg)                  ) ! [umol.DO g-1.rt.drywt s-1]  Maximum uptake rate of DO in the roots 
    allocate( SGRASS(ng)%k_rt_DO                                    (Nsg)                  ) ! [umol.DO L-1]               Half saturation consentration of DO in soil pore water for growth
# endif
    ! respiration: required
    allocate( SGRASS(ng)%R_basal                                    (Nsg)                  ) ! [umol.C m-2.leaf s-1]       Maximum respiration rate
    allocate( SGRASS(ng)%Turnover_rate                              (Nsg)                  ) ! [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day
    ! dieoff when respiration is insufficient
    allocate( SGRASS(ng)%max_anoxic_dieoff_rate                     (Nsg)                  ) ! [mol.C mol-1.C day-1]       maximum dieoff due to anoxia in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
    ! Growth interval
    allocate( SGRASS(ng)%trans_rate                                 (Nsg)                  ) ! [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
    allocate( SGRASS(ng)%opt_sg_bed_density                         (Nsg)                  ) ! [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)
! >>> Saved Values <<< !
    ! Update at end of main interval
    allocate( SGRASS(ng)%SgCBmF                                     (Nsg,LBi:UBi,LBj:UBj)  ) ! [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
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
    allocate( SGRASS(ng)%PhotLim                                    (Nsg,LBi:UBi,LBj:UBj)  ) ! []                          Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon uptake; 3 = Nitrogen uptake; 4 = Phosphorus uptake
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
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    USE mod_sedecosys, ONLY : Nsed
# endif
    USE mod_geochem

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

! ====================================================================================================
! Variable initializations
! ====================================================================================================
! >>> Saved Constants <<< !
! Set constant values  yt_edit make this user definable!!!
    ! biophysical params
    SGRASS(ng)%C_N_P_ratios                         = reshape([ 545d0, 35d0, 1d0 ],[3, Nsg])     ! C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]     ! Martins and Bandeira (2001) measured C:N:P = 545:35:1 for T hemprichii
    SGRASS(ng)%f_above                              = [ 0.21d0   ]                                    ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction                              ! 0.21 Mallombasi et al. (2020)
    ! SGRASS(ng)%f_above                            = [ 1.0d0    ]                                    ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction                              ! yt_debug testing
    SGRASS(ng)%rho_lf                               = [ 0.069d0  ]                                    ! [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density                                                                  ! 0.069 kg/m2 Calculated from Zulfikar et al. (2020) 
    SGRASS(ng)%k_proj                               = [ 0.8d0    ]                                    ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient                                                                  ! 0.8 is a guess
    ! SGRASS(ng)%k_proj                             = [ 1.0d0    ]                                    ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient                                                                
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    ! SeagrassRootProf(ng, species, k)                  Profile of seagrass root biomass: fraction in each biological sediment layer 
    SGRASS(ng)%root_mass_profile                    = SeagrassRootProf(ng, :, :)                      ! Mass ratio of roots in each layer (total must sum to 1.0)

    ! write(*,*) 'yt_debug: mod_seagrass.F initialize_seagrass() after root_mass_profile assigment'
    ! write(*,*) 'yt_debug: mod_seagrass.F initialize_seagrass() root_mass_profile = ', SGRASS(ng)%root_mass_profile
# endif

    ! photoynthesis: light-limit
    ! SGRASS(ng)%P_max                              = [ 3.45d0   ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! 50 µmol/kg/s Calculated from net photosynthesis data from Bjo ̈rk et al. (1997) or 3.45 µmol.C m-2.leaf s-1 (see master's thesis section 6.3.2c)
    SGRASS(ng)%P_max                                = [ 73.8d0   ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! tuning: when R_basal = 2.5265d0 (half-life in complete darkness is ~7.0 days), biomass reaches equilibrium at 2 kg.drywt/m2.hab under max-photosynthesis.
    ! SGRASS(ng)%P_max                              = [ 34.5d0   ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! yt_debug testing
    SGRASS(ng)%I_k                                  = [ 180d0    ]                                    ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve                    ! Calculated from rETR data from Liu et al. (2005), rETR to ETR conversion using Aquation (2020), ETR to carbon assimilation rate using Miyazawa and Yahata (2006), and a leaf bend angle of 10◦ from James et al. (2021) (refer to master's thesi Section 6.3.2b)

    ! photoynthesis: DIC-limit
    SGRASS(ng)%Lf_DIC_uptake_max                    = [ 73.8d0   ]                                    ! [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves                                                                      ! tuning set equal to P_max
    ! SGRASS(ng)%k_lf_DIC                             = [ 4500d0   ]                                    ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth                                             ! 4500 µmol L-1 from Bjo ̈rk et al. (1997) (master's thesis section 6.3.2b)
    SGRASS(ng)%k_lf_DIC                             = [ 3000.0d0 ]                                    ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth                                               ! yt_edit Tuning
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    SGRASS(ng)%Rt_DIC_flux_max                      = [ 73.8d0/0.069d0 ]                              ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots                                                                       ! yt_edit Tuning
    SGRASS(ng)%k_rt_DIC                             = [ 3000.0d0*10d0 ]                               ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth                                            ! yt_edit Tuning
# endif

    ! photoynthesis: nutrient-limit
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    ! SGRASS(ng)%Lf_NO3_uptake_max                    = [ 200.0d0/35.0d0  ]                            ! [umol.NO3 m-2.lf s-1]       Maximum uptake rate of NO3 in the leaves                                                                      ! yt_edit tuning  set equal to max photosynthesis
    SGRASS(ng)%Lf_NO3_uptake_max                    = [ 73.8d0/35d0 ]                                 ! [umol.NO3 m-2.lf s-1]       Maximum uptake rate of NO3 in the leaves                                                                      ! yt_edit tuning  set equal to max photosynthesis
    SGRASS(ng)%k_lf_NO3                             = [ 0.52d0   ]                                    ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth                                               ! Loosely calculate from Ow et al. (2016) (master's thesis Section 6.3.5b)
    SGRASS(ng)%Lf_NH4_uptake_max                    = [ 73.8d0/35d0 ]                                 ! [umol.NH4 m-2.lf s-1]       Maximum uptake rate of NH4 in the leaves                                                                      ! yt_edit tuning  set equal to max photosynthesis
    SGRASS(ng)%k_lf_NH4                             = [ 36.3d0   ]                                    ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth                                               ! Stapel et al. (1996) (master's thesis Section 6.3.5b)
    SGRASS(ng)%Lf_PO4_uptake_max                    = [ 73.8d0/545d0 ]                                ! [umol.PO4 m-2.lf s-1]       Maximum uptake rate of PO4 in the leaves                                                                      ! yt_edit tuning  set equal to max photosynthesis
    SGRASS(ng)%k_lf_PO4                             = [ 11.2d0   ]                                    ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth                                               ! Stapel et al. (1996) (master's thesis Section 6.3.5b)
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    SGRASS(ng)%Rt_NO3_flux_max                      = [ 73.8d0/35d0/0.069d0 ]                         ! [umol.NO3 g-1.rt.drywt s-1] Maximum uptake rate of NO3 in the roots                                                                       ! yt_edit tuning, same as leaves
    SGRASS(ng)%k_rt_NO3                             = [ 0.52d0*10d0 ]                                 ! [umol.NO3 L-1]              Half saturation consentration of NO3 in soil pore water for growth                                            ! yt_edit tuning, ten times higher than leaves
    SGRASS(ng)%Rt_NH4_flux_max                      = [ 73.8d0/35d0/0.069d0 ]                         ! [umol.NH4 g-1.rt.drywt s-1] Maximum uptake rate of NH4 in the roots                                                                       ! yt_edit tuning, same as leaves
    SGRASS(ng)%k_rt_NH4                             = [ 36.3d0*10d0 ]                                 ! [umol.NH4 L-1]              Half saturation consentration of NH4 in soil pore water for growth                                            ! yt_edit tuning, ten times higher than leaves
    SGRASS(ng)%Rt_PO4_flux_max                      = [ 73.8/545d0/0.069d0  ]                         ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots                                                                       ! yt_edit tuning, same as leaves
    SGRASS(ng)%k_rt_PO4                             = [ 11.2d0*10d0 ]                                 ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth                                            ! yt_edit tuning, ten times higher than leaves
# endif
    
    ! respiration: DO-limit
    ! SGRASS(ng)%Lf_DO_uptake_max                     = [ 2.075d0  ]                                  ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves                                                                       ! 2.075 umol.C/m2.leaf/s calculated from Rasmusson et al. (2020) (refer to master's thesis 6.3.3a)
    SGRASS(ng)%Lf_DO_uptake_max                     = [ 40d0     ]                                    ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves                                                                       ! tuning
    SGRASS(ng)%k_lf_DO                              = [ 43d0     ]                                    ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration                                           ! 43 umol.O2/L Loosely calculated from Rasmusson et al. (2020) (Section 6.3.3b)
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    SGRASS(ng)%Rt_DO_flux_max                       = [ 40d0/0.069d0 ]                                ! [umol.DO g-1.rt.drywt s-1]  Maximum uptake rate of DO in the roots                                                                        ! yt_edit tuning, same as leaves
    SGRASS(ng)%k_rt_DO                              = [ 43d0*10d0 ]                                   ! [umol.DO L-1]               Half saturation consentration of DO in soil pore water for growth                                             ! yt_edit tuning, same as leaves
# endif
    ! respiration: required
    SGRASS(ng)%R_basal                              = [ 2.5265d0 ]                                    ! [umol.C m-2.leaf s-1]       Maximum respiration rate                                                                                      ! tuning: half-life in complete darkness is ~7.0 days.
    SGRASS(ng)%Turnover_rate                        = [0.01      ]                                    ! [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day
    ! dieoff when respiration is insufficient
    SGRASS(ng)%max_anoxic_dieoff_rate               = [ 0.5d0    ]                                    ! [mol.C mol-1.C day-1]       maximum dieoff due to anoxia in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates ! tuning

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
            ! when initializaing from start in a new run when only coverage is known, calculate biomass values assuming an ELAP of 90% in the coverage area and an ideal ratio of above to belowground biomass

            ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
            ! GridArea                    [m2.grid]                   Total ground area in grid cell
            ! p_sgrass                    [m2.sg.hab m-2.grid](0~1)   Seagrass coverage
            ! LAI                         [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area 
            ! ELAP                        [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
            ! k_proj                      [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient
            ! rho_lf                      [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
            ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
            ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
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
            
            ! SGRASS(ng)%LAI(species,i,j) = (-1*log(1.0-0.85)/SGRASS(ng)%k_proj(species))
            ! SGRASS(ng)%LAI(species,i,j) = (-1*log(1.0d0-0.30433574d0)/SGRASS(ng)%k_proj(species))
            SGRASS(ng)%LAI(species,i,j) = 0.001d0

            ! [umol.C/m2.hab]              
            SGRASS(ng)%LfCBm(species,i,j)  &
            ! = [m2.lf/m2.hab]              [kg.drywt/m2.lf]             [mol.C/g.drywt]                 (1000.g.drywt/kg.drywt) (1000000umol/mol)
              = SGRASS(ng)%LAI(species,i,j) * SGRASS(ng)%rho_lf(species) / SGRASS(ng)%SgMperC(species) * 1000000000d0

            ! [umol.C.tot/m2.hab]         = [umol.C.lf/m2.hab]             [mol.C.tot/mol.C.lf]
            SGRASS(ng)%SgCBmF(species,i,j) = SGRASS(ng)%LfCBm(species,i,j) / SGRASS(ng)%f_above(species)

            ! [umol.C.rt/m2.hab]          = [umol.C.tot/m2.hab]            [mol.C.rt/mol.C.tot]
            SGRASS(ng)%RtCBm(species,i,j) = SGRASS(ng)%SgCBmF(species,i,j) * (1.0d0 - SGRASS(ng)%f_above(species))

            ! [mol.C]                        = [umol.C/m2.hab]                [m2.hab]                          (mol/1000000umol)
            SGRASS(ng)%TotSgCBm(species,i,j) = SGRASS(ng)%SgCBmF(species,i,j) * SGRASS(ng)%HabArea(species,i,j) * 0.000001d0

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

            ! [g.drywt/m2.hab]              = [umol.C/m2.hab]                [g.drywt/mol.C]               (mol.C/1000000umol.C)
            SGRASS(ng)%SgDryBm(species,i,j) = SGRASS(ng)%SgCBmF(species,i,j) * SGRASS(ng)%SgMperC(species) * 0.000001d0

            ! [g.drywt/m2.hab]              = [umol.C/m2.hab]               [g.drywt/mol.C]               (mol.C/1000000umol.C)
            SGRASS(ng)%LfDryBm(species,i,j) = SGRASS(ng)%LfCBm(species,i,j) * SGRASS(ng)%SgMperC(species) * 0.000001d0

            ! [g.drywt/m2.hab]              = [umol.C/m2.hab]               [g.drywt/mol.C]               (mol.C/1000000umol.C)
            SGRASS(ng)%RtDryBm(species,i,j) = SGRASS(ng)%RtCBm(species,i,j) * SGRASS(ng)%SgMperC(species) * 0.000001d0

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

          enddo
        enddo
      enddo

      ! >>> Saved Values Only for Output  <<< !
      SGRASS(ng)%TotLA                    = 0d0 ! [m2.lf]                     Total one-sided green leaf area in grid
      SGRASS(ng)%GridELAP                 = 0d0 ! [m2.lf.proj m-2.grid]       Effective leaf area projection on ground (whole grid)
      SGRASS(ng)%GridPhot                 = 0d0 ! [umol.C m-2.grid s-1]       Seagrass gross photosynthesis rate per unit ground area (whole grid)
      SGRASS(ng)%PhotLim                  = 0   ! []                          Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon uptake; 3 = Nitrogen uptake; 4 = Phosphorus uptake
      SGRASS(ng)%GridResp                 = 0d0 ! [umol.C m-2.grid s-1]       Seagrass respiration rate per unit ground area (whole grid)
      SGRASS(ng)%GridNetPhot              = 0d0 ! [umol.C m-2.grid s-1]       Seagrass net photosynthesis rate per unit ground area (whole grid)
      SGRASS(ng)%GridDieoff               = 0d0 ! [umol.C m-2.grid s-1]       Seagrass dieoff rate carbon biomass per unit ground area (whole grid)


      ! write(*,*) 'yt_debug: mod_seagrass.F initialize_seagrass() after analytical start mass assigments'
    endif

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
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    , Nsed                        &   !                           Number of sediment layers
# endif
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
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    integer, intent(in)   :: Nsed                       !                             Number of sediment layers
# endif
    real(8), intent(in)   :: dz                         ! [m]                         vertical layer thickness (bottom water column layer)
    real(8), intent(in)   :: date                       ! [days]                      Date (days since x or elapsed days) yt_edit best if in phase with clock to maintain consistency on restart
    real(8), intent(in)   :: dt                         ! [s]                         Time step interval duration
    real(8), intent(inout):: p_sgrass(Nsg)              ! [m2.sg.hab m-2.grid]        (0~1)  Seagrass coverage (every species)
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    real(8), intent(in)   :: p_sand                     ! [m2.sedmnt m-2.grid]      (0~1)  Sediment coverage
# endif
    real(8), intent(in)   :: PFD                        ! [umol m-2 s-1]              Photon flux density 
    real(8), intent(in)   :: rho_sw                     ! [g cm-3]                    Density of seawater 
    real(8)               :: ocean_DIC_conc             ! [umol.DIC L-1]              DIC concentration of ocean water
    real(8)               :: ocean_DO__conc             ! [umol.DO  L-1]              DO  concentration of ocean water
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    real(8)               :: ocean_NO3_conc             ! [umol.NO3 L-1]              NO3 concentration of ocean water
    real(8)               :: ocean_NH4_conc             ! [umol.NH4 L-1]              NH4 concentration of ocean water
    real(8)               :: ocean_PO4_conc             ! [umol.PO4 L-1]              PO4 concentration of ocean water
# endif

! >>> Outputs <<< !
    real(8), intent(out)  :: DIC_uptake_rate_from_ocean ! [mmol.DIC m-2.sg.hab s-1]   DIC uptake rate  * direction of ocean to seagrass is positive
    real(8), intent(out)  :: DO__uptake_rate_from_ocean ! [mmol.DO  m-2.sg.hab s-1]   DO  uptake rate  * direction of ocean to seagrass is positive
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    real(8), intent(out)  :: NO3_uptake_rate_from_ocean ! [mmol.NO3 m-2.sg.hab s-1]   NO3 uptake rate  * direction of ocean to seagrass is positive
    real(8), intent(out)  :: NH4_uptake_rate_from_ocean ! [mmol.NH4 m-2.sg.hab s-1]   NH4 uptake rate  * direction of ocean to seagrass is positive
    real(8), intent(out)  :: PO4_uptake_rate_from_ocean ! [mmol.PO4 m-2.sg.hab s-1]   PO4 uptake rate  * direction of ocean to seagrass is positive
# endif
# if defined SEAGRASS_LEAF_POM
    real(8), intent(out)  :: POC_uptake_rate_from_ocean ! [mmol.C m-2.sg.hab s-1]     Particulate organic carbon uptake rate     * direction of ocean to seagrass is positive
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), intent(out)  :: PON_uptake_rate_from_ocean ! [mmol.N m-2.sg.hab s-1]     Particulate organic nitrogen uptake rate   * direction of ocean to seagrass is positive
    real(8), intent(out)  :: POP_uptake_rate_from_ocean ! [mmol.P m-2.sg.hab s-1]     Particulate organic phosphorus uptake rate * direction of ocean to seagrass is positive
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
    real(8), pointer      :: Lf_DIC_uptake_max          ! [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves
    real(8), pointer      :: k_lf_DIC                   ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer      :: Rt_DIC_flux_max            ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    real(8), pointer      :: k_rt_DIC                   ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
# endif
    ! photosynthesis: nutrient-limit
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    real(8), pointer      :: Lf_NO3_uptake_max          ! [umol.NO3 m-2.lf s-1]       Maximum uptake rate of NO3 in the leaves
    real(8), pointer      :: k_lf_NO3                   ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth
    real(8), pointer      :: Lf_NH4_uptake_max          ! [umol.NH4 m-2.lf s-1]       Maximum uptake rate of NH4 in the leaves
    real(8), pointer      :: k_lf_NH4                   ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth
    real(8), pointer      :: Lf_PO4_uptake_max          ! [umol.PO4 m-2.lf s-1]       Maximum uptake rate of PO4 in the leaves
    real(8), pointer      :: k_lf_PO4                   ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer      :: Rt_NO3_flux_max            ! [umol.NO3 g-1.rt.drywt s-1] Maximum uptake rate of NO3 in the roots 
    real(8), pointer      :: k_rt_NO3                   ! [umol.NO3 L-1]              Half saturation consentration of NO3 in soil pore water for growth
    real(8), pointer      :: Rt_NH4_flux_max            ! [umol.NH4 g-1.rt.drywt s-1] Maximum uptake rate of NH4 in the roots 
    real(8), pointer      :: k_rt_NH4                   ! [umol.NH4 L-1]              Half saturation consentration of NH4 in soil pore water for growth
    real(8), pointer      :: Rt_PO4_flux_max            ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    real(8), pointer      :: k_rt_PO4                   ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
# endif
    ! respiration: DO-limit
    real(8), pointer      :: Lf_DO_uptake_max           ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves
    real(8), pointer      :: k_lf_DO                    ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer      :: Rt_DO_flux_max             ! [umol.DO g-1.rt.drywt s-1]  Maximum uptake rate of DO in the roots 
    real(8), pointer      :: k_rt_DO                    ! [umol.DO L-1]               Half saturation consentration of DO in soil pore water for growth
# endif
    ! respiration: required
    real(8), pointer      :: R_basal                    ! [umol.C m-2.leaf s-1]       Maximum respiration rate
    real(8), pointer      :: Turnover_rate              ! [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day
    ! dieoff when respiration is insufficient
    real(8), pointer      :: max_anoxic_dieoff_rate     ! [mol.C mol-1.C day-1]       maximum dieoff due to anoxia in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
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
    real(8)               :: sed_DIC(Nsed)              ! [umol.DIC L-1]              DIC consentration of sediment (in each sediment layer)
    real(8)               :: sed_DO (Nsed)              ! [umol.DO L-1]               DO  consentration of sediment (in each sediment layer)
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8)               :: sed_NO3(Nsed)              ! [umol.NO3 L-1]              NO3 consentration of sediment (in each sediment layer)
    real(8)               :: sed_NH4(Nsed)              ! [umol.NH4 L-1]              NH4 consentration of sediment (in each sediment layer)
    real(8)               :: sed_PO4(Nsed)              ! [umol.PO4 L-1]              PO4 consentration of sediment (in each sediment layer)
# endif

! >>> Saved Values <<< !
    ! Update at end of main interval
    real(8), pointer      :: SgCBmF                     ! [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
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
    integer, pointer      :: PhotLim                    ! []                          Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon uptake; 3 = Nitrogen uptake; 4 = Phosphorus uptake
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
    real(8)               :: delta_SgCBmF               ! [umol.C m-2.sg.hab s-1]     Rate of change in SgCBmF
    ! values to write to sediment module
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer :: delta_sed_DIC(:)                ! [umol.DIC L-1 s-1]          Rate of change in DIC due to flux from roots to sediment (positive means seagrass to sediment)
    real(8), pointer :: delta_sed_DO (:)                ! [umol.DO L-1 s-1]           Rate of change in DO due to flux from roots to sediment (positive means seagrass to sediment)
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer :: delta_sed_NO3(:)                ! [umol.NO3 L-1 s-1]          Rate of change in NO3 due to flux from roots to sediment (positive means seagrass to sediment)
    real(8), pointer :: delta_sed_NH4(:)                ! [umol.NH4 L-1 s-1]          Rate of change in NH4 due to flux from roots to sediment (positive means seagrass to sediment)
    real(8), pointer :: delta_sed_PO4(:)                ! [umol.PO4 L-1 s-1]          Rate of change in PO4 due to flux from roots to sediment (positive means seagrass to sediment)
# endif
# if defined SEAGRASS_ROOT_POM
    real(8), pointer :: delta_sed_POC(:)                ! [umol.C L-1 s-1]            Rate of change in POC due to root death in sediment (positive means seagrass to sediment)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer :: delta_sed_PON(:)                ! [umol.N L-1 s-1]            Rate of change in PON due to root death in sediment (positive means seagrass to sediment)
    real(8), pointer :: delta_sed_POP(:)                ! [umol.P L-1 s-1]            Rate of change in POP due to root death in sediment (positive means seagrass to sediment)
#  endif
# endif

! >>> Intermediate Calculation Variables <<< !      (erased each time step)
    ! Widely used temporary variables
    real(8)               :: f_abv_cur                  ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current above ground biomass to total biomass fraction, calculated on the last growth interval
    real(8)               :: f_blw_cur                  ! [g.rt.drywt g-1.tot.drywt]  also [mol.C.rt mol-1.C.tot] current below ground biomass to total biomass fraction, calculated on the last growth interval
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    real(8)               :: sgrass_per_sedmnt          ! [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    real(8)               :: root_comp_dist_factor(Nsed)! [g.rt.comp g-1.tot m2.sg.hab m-2.sedmnt mm-1]    root compartemtent distribution factor equal to f_blw_cur * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 

# endif
    ! Photosynthesis/Growth
    real(8)               :: Growth_rate                ! [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! photosynthesis: light-limit
    real(8)               :: Growth_limit_light         ! [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by light
    real(8)               :: ELAP                       ! [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
    ! photosynthesis: DIC-limit
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8)               :: DIC_uptake_possible(0:Nsed)! [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
# else
    real(8)               :: DIC_uptake_possible(0:0)   ! 
# endif
    real(8)               :: sumCUpPos                  ! [umol.DIC m-2.sg.hab s-1]   sum of all potential DIC uptake (used for scaling/normalizing uptake proportions)
    real(8)               :: Growth_limit_DIC           ! [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by DIC uptake
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
    real(8)               :: sumPUpPos                  ! [umol.P m-2.sg.hab s-1]     sum of all potential phosphorus uptake (used for scaling/normalizing uptake proportions)
    real(8)               :: Growth_limit_NO3           ! [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by NO3 uptake
    real(8)               :: Growth_limit_NH4           ! [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by NH4 uptake
    real(8)               :: Growth_limit_PO4           ! [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by PO4 uptake
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
    ! respiration: possible
    real(8)               :: Resp_pos                   ! [umol.C m-2.sg.hab s-1]     Respiration rate possible in this time step
    ! respiration: required
    real(8)               :: Resp_req                   ! [umol.C m-2.sg.hab s-1]     Respiration rate required for maintenance in this time step
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


    ! write(*,*) 'yt_debug: start of mod_seagrass.F seagrass()   i =', i, '   j =', j


! ====================================================================================================
! Variable initializations
! ====================================================================================================


! >>> Outputs <<< !
    ! clear old values from output variables
    DIC_uptake_rate_from_ocean = 0.0d0                                      ! [mmol.DIC m-2.sg.hab s-1]   DIC uptake rate  * direction of ocean to seagrass is positive
    DO__uptake_rate_from_ocean = 0.0d0                                      ! [mmol.DO  m-2.sg.hab s-1]   DO  uptake rate  * direction of ocean to seagrass is positive
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE        
    NO3_uptake_rate_from_ocean = 0.0d0                                      ! [mmol.NO3 m-2.sg.hab s-1]   NO3 uptake rate  * direction of ocean to seagrass is positive
    NH4_uptake_rate_from_ocean = 0.0d0                                      ! [mmol.NH4 m-2.sg.hab s-1]   NH4 uptake rate  * direction of ocean to seagrass is positive
    PO4_uptake_rate_from_ocean = 0.0d0                                      ! [mmol.PO4 m-2.sg.hab s-1]   PO4 uptake rate  * direction of ocean to seagrass is positive
# endif
# if defined SEAGRASS_LEAF_POM
    POC_uptake_rate_from_ocean = 0.0d0                                      ! [mmol.C m-2.sg.hab s-1]     Particulate organic carbon uptake rate     * direction of ocean to seagrass is positive
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    PON_uptake_rate_from_ocean = 0.0d0                                      ! [mmol.N m-2.sg.hab s-1]     Particulate organic nitrogen uptake rate   * direction of ocean to seagrass is positive
    POP_uptake_rate_from_ocean = 0.0d0                                      ! [mmol.P m-2.sg.hab s-1]     Particulate organic phosphorus uptake rate * direction of ocean to seagrass is positive
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
    Lf_DIC_uptake_max   =>  SGRASS(ng)%Lf_DIC_uptake_max      (species)     ! [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves
    k_lf_DIC            =>  SGRASS(ng)%k_lf_DIC               (species)     ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    Rt_DIC_flux_max     =>  SGRASS(ng)%Rt_DIC_flux_max        (species)     ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    k_rt_DIC            =>  SGRASS(ng)%k_rt_DIC               (species)     ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
# endif
    ! photoynthesis: nutrient-limit
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    Lf_NO3_uptake_max   =>  SGRASS(ng)%Lf_NO3_uptake_max      (species)     ! [umol.NO3 m-2.lf s-1]       Maximum uptake rate of NO3 in the leaves
    k_lf_NO3            =>  SGRASS(ng)%k_lf_NO3               (species)     ! [umol.NO3 L-1]              Half saturation consentration of NO3 in water column for growth
    Lf_NH4_uptake_max   =>  SGRASS(ng)%Lf_NH4_uptake_max      (species)     ! [umol.NH4 m-2.lf s-1]       Maximum uptake rate of NH4 in the leaves
    k_lf_NH4            =>  SGRASS(ng)%k_lf_NH4               (species)     ! [umol.NH4 L-1]              Half saturation consentration of NH4 in water column for growth
    Lf_PO4_uptake_max   =>  SGRASS(ng)%Lf_PO4_uptake_max      (species)     ! [umol.PO4 m-2.lf s-1]       Maximum uptake rate of PO4 in the leaves
    k_lf_PO4            =>  SGRASS(ng)%k_lf_PO4               (species)     ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    Rt_NO3_flux_max     =>  SGRASS(ng)%Rt_NO3_flux_max        (species)     ! [umol.NO3 g-1.rt.drywt s-1] Maximum uptake rate of NO3 in the roots 
    k_rt_NO3            =>  SGRASS(ng)%k_rt_NO3               (species)     ! [umol.NO3 L-1]              Half saturation consentration of NO3 in soil pore water for growth
    Rt_NH4_flux_max     =>  SGRASS(ng)%Rt_NH4_flux_max        (species)     ! [umol.NH4 g-1.rt.drywt s-1] Maximum uptake rate of NH4 in the roots 
    k_rt_NH4            =>  SGRASS(ng)%k_rt_NH4               (species)     ! [umol.NH4 L-1]              Half saturation consentration of NH4 in soil pore water for growth
    Rt_PO4_flux_max     =>  SGRASS(ng)%Rt_PO4_flux_max        (species)     ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    k_rt_PO4            =>  SGRASS(ng)%k_rt_PO4               (species)     ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
# endif
    ! respiration: DO-limit
    Lf_DO_uptake_max    =>  SGRASS(ng)%Lf_DO_uptake_max       (species)     ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves
    k_lf_DO             =>  SGRASS(ng)%k_lf_DO                (species)     ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    Rt_DO_flux_max      =>  SGRASS(ng)%Rt_DO_flux_max         (species)     ! [umol.DO g-1.rt.drywt s-1]  Maximum uptake rate of DO in the roots 
    k_rt_DO             =>  SGRASS(ng)%k_rt_DO                (species)     ! [umol.DO L-1]               Half saturation consentration of DO in soil pore water for growth
# endif
    ! respiration: required
    R_basal             =>  SGRASS(ng)%R_basal                (species)     ! [umol.C m-2.leaf s-1]       Maximum respiration rate
    Turnover_rate       =>  SGRASS(ng)%Turnover_rate          (species)     ! [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day
    ! dieoff when respiration is insufficient
    max_anoxic_dieoff_rate =>  SGRASS(ng)%max_anoxic_dieoff_rate(species)   ! [mol.C mol-1.C day-1]       maximum dieoff due to anoxia in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
    ! Update at end of growth interval
    ! allocate growth
    ! update bed-density/habitat area
    trans_rate          =>  SGRASS(ng)%trans_rate             (species)     ! [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
    opt_sg_bed_density  =>  SGRASS(ng)%opt_sg_bed_density     (species)     ! [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)

! >>> Values Read From Sediment Module <<< !
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    ! Nsed                                                    Number of sediment layers
    sed_layer_thickness(:) = SEDECO(ng)%dz(:)/10d0                          ! [mm]                        Thickness of each sediment layer
# endif
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    sed_DIC(:)     =   SEDECO(ng)%DIC(i,j,:)                                ! [umol.DIC L-1]              DIC consentration of sediment (in each sediment layer)
    where (sed_DIC .le. 0.0d0) sed_DIC = 0.0d0                              !                             error-correct negative values to become zero
    sed_DO (:)     =   SEDECO(ng)%O2 (i,j,:)                                ! [umol.DO L-1]               DO  consentration of sediment (in each sediment layer)
    where (sed_DO .le. 0.0d0) sed_DO = 0.0d0                                !                             error-correct negative values to become zero
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    sed_NO3(:)     =   SEDECO(ng)%NO3(i,j,:)                                ! [umol.NO3 L-1]              NO3 consentration of sediment (in each sediment layer)
    where (sed_NO3 .le. 0.0d0) sed_NO3 = 0.0d0                              !                             error-correct negative values to become zero
    sed_NH4(:)     =   SEDECO(ng)%NH4(i,j,:)                                ! [umol.NH4 L-1]              NH4 consentration of sediment (in each sediment layer)
    where (sed_NH4 .le. 0.0d0) sed_NH4 = 0.0d0                              !                             error-correct negative values to become zero
    sed_PO4(:)     =   SEDECO(ng)%PO4(i,j,:)                                ! [umol.PO4 L-1]              PO4 consentration of sediment (in each sediment layer)
    where (sed_PO4 .le. 0.0d0) sed_PO4 = 0.0d0                              !                             error-correct negative values to become zero
# endif

! >>> Saved Values <<< !
    ! Update at end of main interval
    SgCBmF              =>  SGRASS(ng)%SgCBmF                 (species,i,j) ! [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
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
    PhotLim             =>  SGRASS(ng)%PhotLim                (species,i,j) ! []                          Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon uptake; 3 = Nitrogen uptake; 4 = Phosphorus uptake
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
    delta_SgCBmF        =   0.0d0                                           ! [umol.C m-2.sg.hab s-1]     Rate of change in SgCBmF
    ! values to write to sediment module
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    delta_sed_DIC       =>  SEDECO(ng)%sg_dDIC_dt(species,i,j,:)            ! [umol.DIC L-1 s-1]          Rate of change in DIC due to flux from roots to sediment (positive means seagrass to sediment)
    delta_sed_DIC       =   0.0d0                                           ! 
    delta_sed_DO        =>  SEDECO(ng)%sg_dO2_dt (species,i,j,:)            ! [umol.DO L-1 s-1]           Rate of change in DO due to flux from roots to sediment (positive means seagrass to sediment)
    delta_sed_DO        =   0.0d0                                           !
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    delta_sed_NO3       =>  SEDECO(ng)%sg_dNO3_dt(species,i,j,:)            ! [umol.NO3 L-1 s-1]          Rate of change in NO3 due to flux from roots to sediment (positive means seagrass to sediment)
    delta_sed_NO3       =   0.0d0                                           !
    delta_sed_NH4       =>  SEDECO(ng)%sg_dNH4_dt(species,i,j,:)            ! [umol.NH4 L-1 s-1]          Rate of change in NH4 due to flux from roots to sediment (positive means seagrass to sediment)
    delta_sed_NH4       =   0.0d0                                           ! 
    delta_sed_PO4       =>  SEDECO(ng)%sg_dPO4_dt(species,i,j,:)            ! [umol.PO4 L-1 s-1]          Rate of change in PO4 due to flux from roots to sediment (positive means seagrass to sediment)
    delta_sed_PO4       =   0.0d0                                           ! 
# endif
# if defined SEAGRASS_ROOT_POM
    delta_sed_POC       =>  SEDECO(ng)%sg_dPOC_dt(species,i,j,:)            ! [umol.C L-1 s-1]            Rate of change in POC due to root death in sediment (positive means seagrass to sediment)
    delta_sed_POC       =   0.0d0
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    delta_sed_PON       =>  SEDECO(ng)%sg_dPON_dt(species,i,j,:)            ! [umol.N L-1 s-1]            Rate of change in PON due to root death in sediment (positive means seagrass to sediment)
    delta_sed_PON       =   0.0d0
    delta_sed_POP       =>  SEDECO(ng)%sg_dPOP_dt(species,i,j,:)            ! [umol.P L-1 s-1]            Rate of change in POP due to root death in sediment (positive means seagrass to sediment)
    delta_sed_POP       =   0.0d0
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
    
    if (date .gt. real(int(Day*24), 8)/24) then
      is_growth_interval = .true.
      Day = real(int(date*24+1), 8)/24d0
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

! Widely used temporary variables --------------------------------------------------------------------

    ! f_abv_cur                   [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current above ground biomass to total biomass fraction, calculated on the last growth interval
    ! f_blw_cur                   [g.rt.drywt g-1.tot.drywt]  also [mol.C.rt mol-1.C.tot] current below ground biomass to total biomass fraction, calculated on the last growth interval
    f_abv_cur = TotLfCBm/TotSgCBm
    f_blw_cur = TotRtCBm/TotSgCBm

# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    ! p_sgrass(Nsg)               [m2.sg.hab m-2.grid]        (0~1)  Seagrass coverage (every species)
    ! p_sand                      [m2.sedmnt m-2.grid]        (0~1)  Sediment coverage
    ! [m2.hab/m2.sedmnt] = [m2.hab/m2.grid] [m2.grid/m2.sedmnt]
    sgrass_per_sedmnt = p_sgrass(species)   / p_sand
    ! root_comp_dist_factor(Nsed) [g.rt.comp g-1.tot m2.sg.hab m-2.sedmnt mm-1]    root compartemtent distribution factor equal to f_blw_cur * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 
    ! f_blw_cur                   [g.rt.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current below ground biomass to total biomass fraction, calculated on the last growth interval
    ! root_mass_profile(sed)      []                          Mass ratio of roots in each layer (total must sum to 1.0)
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
    ! [g.rt.comp/g.tot][m2.hab/m2.sedmnt mm] = [g.rt/g.tot] [g.rt.comp/g.rt]       [m2.hab/m2.sedmnt]  [1/mm]                  
    root_comp_dist_factor(:)                 = f_blw_cur    * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 
# endif

! ====================================================================================================
!
! Photosynthesis/Growth
!
!   if x:y:z is the C:N:P ratio
!   (x/x)*CO2 + (y/x)*(NO3+NH4) + (z/y)*PO4 -> (1/x)*(CH2O)x(NH3)y(H3PO4)z + (some)*O2 +/- (some)*H+
!
!   CO2    +   H2O  -> CH2O  +   O2      ! for every mol of CO2  consumed in growth: 1 mol of O2 is produced
!   NO3-   +   H3O+ -> NH3   + 2*O2      ! for every mol of NO3- consumed in growth: 2 mol of O2 is produced
!   NH4+   +   H2O  -> NH3   +   H3O+                          
!   PO4^3- + 3*H3O+ -> H3PO4 + 3*H2O     
!
! ====================================================================================================

! Effective leaf area projection ---------------------------------------------------------------------

    ! ELAP                      [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
    ! LAI                       [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area 
    ! k_proj                    [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient
    ELAP = 1.0d0-exp(-1d0*k_proj*LAI)

! Photosynthesis rate light limit ---------------------------------------------------------------------

    ! Growth_limit_light        [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by light
    ! P_max                     [umol.C m-2.leaf s-1]       Maximum photoynthesis rate
    ! ELAP                      [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
    ! PFD                       [umol.photon m-2 s-1]       Photon flux density
    ! I_k                       [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve

    ! [umol.C/m2.hab s] = [umol.C/m2.lf s] [m2.lf/m2.hab] [             ]
    Growth_limit_light  = P_max            * ELAP         * tanh(PFD/I_k)

! Photosynthesis rate DIC limit -----------------------------------------------------------------------

    ! DIC_uptake_possible(:)    [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Lf_DIC_uptake_max         [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves
    ! LAI                       [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area 
    ! ocean_DIC_conc            [umol.DIC L-1]              DIC concentration of ocean water 
    ! k_lf_DIC                  [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth

    ! [umol.DIC/m2.hab s]  = [umol.DIC/m2.lf s] [m2.lf/m2.hab] ( Michaelis-Menten term                      )
    DIC_uptake_possible(0) = Lf_DIC_uptake_max  * LAI          * ocean_DIC_conc / (k_lf_DIC + ocean_DIC_conc)
    ! [umol.DIC/m2.hab s]  = min( [umol.DIC/m2.hab s]   ,       [umol.DIC/L]   [m/s](1000L/m3)
    DIC_uptake_possible(0) = min( DIC_uptake_possible(0), 0.5d0*ocean_DIC_conc*dz/dt*1000.0d0 ) ! first order interpolation at low concentration limit to prevent overdraw

# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! DIC_uptake_possible(:)      [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Rt_DIC_flux_max             [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    ! root_mass_profile(:)        []                          Mass ratio of roots in each layer (total must sum to 1.0)
    ! RtDryBm                     [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! sed_DIC(Nsed)               [umol.DIC L-1]              DIC consentration of sediment (in each sediment layer)
    ! k_rt_DIC                    [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
    ! [umol.DIC/m2.hab s]       = [umol.DIC/g.rt s] []                     [g.rt/m2.hab] []
    DIC_uptake_possible(1:Nsed) = Rt_DIC_flux_max   * root_mass_profile(:) * RtDryBm     &
      * ( sed_DIC(:) / (k_rt_DIC + sed_DIC(:)) )
    ! [umol.DIC/m2.hab s]       = min( [umol.DIC/m2.hab s]        ,      [umol.DO/L] [mm                   /s](1000L/m3)(m/1000mm)
    DIC_uptake_possible(1:Nsed) = min( DIC_uptake_possible(1:Nsed), 0.5d0*sed_DIC(:)*sed_layer_thickness(:)/dt) ! first order interpolation at low concentration limit to prevent overdraw
# endif

    ! DIC_uptake_possible(:)      [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! sumCUpPos                   [umol.DIC m-2.sg.hab s-1]   sum of all potential DIC uptake (used for scaling/normalizing uptake proportions)
    sumCUpPos = sum(DIC_uptake_possible(:))

    ! Growth_limit_DIC            [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by DIC uptake
    ! [umol.C/m2.hab s] = [mol.C/mol.DIC] [umol.DIC/m2.hab s]
    Growth_limit_DIC    =                 sumCUpPos


! Growth rate nutrient limit --------------------------------------------------------------------------

# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    ! CHM_uptake_possible(:)    [umol.CHM m-2.sg.hab s-1]   Chemical (CHM) uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Lf_CHM_uptake_max         [umol.CHM m-2.lf s-1]       Maximum uptake rate of CHM in the leaves
    ! LAI                       [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area 
    ! ocean_CHM_conc            [umol.CHM L-1]              CHM concentration of ocean water 
    ! k_lf_CHM                  [umol.CHM L-1]              Half saturation consentration of CHM in water column for growth

    ! [umol.CHM/m2.hab s]  = [umol.CHM/m2.lf s] [m2.lf/m2.hab] ( Michaelis-Menten term                      )
    NO3_uptake_possible(0) = Lf_NO3_uptake_max * LAI           * ocean_NO3_conc / (k_lf_NO3 + ocean_NO3_conc)
    NH4_uptake_possible(0) = Lf_NH4_uptake_max * LAI           * ocean_NH4_conc / (k_lf_NH4 + ocean_NH4_conc)
    PO4_uptake_possible(0) = Lf_PO4_uptake_max * LAI           * ocean_PO4_conc / (k_lf_PO4 + ocean_PO4_conc)
    ! [umol.CHM/m2.hab s]  = min( [umol.CHM/m2.hab s]   ,      [umol.CHM/L]    [m/s](1000L/m3)
    NO3_uptake_possible(0) = min( NO3_uptake_possible(0), 0.5d0*ocean_NO3_conc*dz/dt*1000.0d0 ) ! first order interpolation at low concentration limit to prevent overdraw
    NH4_uptake_possible(0) = min( NH4_uptake_possible(0), 0.5d0*ocean_NH4_conc*dz/dt*1000.0d0 ) ! first order interpolation at low concentration limit to prevent overdraw
    PO4_uptake_possible(0) = min( PO4_uptake_possible(0), 0.5d0*ocean_PO4_conc*dz/dt*1000.0d0 ) ! first order interpolation at low concentration limit to prevent overdraw
# endif

# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! CHM_uptake_possible(:)      [umol.CHM m-2.sg.hab s-1]   Chemical (CHM) uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Rt_CHM_flux_max             [umol.CHM g-1.rt.drywt s-1] Maximum uptake rate of CHM in the roots 
    ! RtDryBm                     [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! sed_CHM(Nsed)               [umol.CHM L-1]              CHM consentration of sediment (in each sediment layer)
    ! k_rt_CHM                    [umol.CHM L-1]              Half saturation consentration of CHM in soil pore water for growth
    ! [umol.CHM/m2.hab s]       = [umol.CHM/g.rt s] []                     [g.rt/m2.hab] []
    NO3_uptake_possible(1:Nsed) = Rt_NO3_flux_max   * root_mass_profile(:) * RtDryBm     &
      * ( sed_NO3(:) / (k_rt_NO3 + sed_NO3(:)) )
    NH4_uptake_possible(1:Nsed) = Rt_NH4_flux_max   * root_mass_profile(:) * RtDryBm     &
      * ( sed_NH4(:) / (k_rt_NH4 + sed_NH4(:)) )
    PO4_uptake_possible(1:Nsed) = Rt_PO4_flux_max   * root_mass_profile(:) * RtDryBm     &
      * ( sed_PO4(:) / (k_rt_PO4 + sed_PO4(:)) )
    ! [umol.CHM/m2.hab s]       = min( [umol.CHM/m2.hab s]        ,      [umol.CHM/L] [mm                   /s](1000L/m3)(m/1000mm)
    NO3_uptake_possible(1:Nsed) = min( NO3_uptake_possible(1:Nsed), 0.5d0*sed_NO3(:)*sed_layer_thickness(:)/dt) ! first order interpolation at low concentration limit to prevent overdraw
    NH4_uptake_possible(1:Nsed) = min( NH4_uptake_possible(1:Nsed), 0.5d0*sed_NH4(:)*sed_layer_thickness(:)/dt) ! first order interpolation at low concentration limit to prevent overdraw
    PO4_uptake_possible(1:Nsed) = min( PO4_uptake_possible(1:Nsed), 0.5d0*sed_PO4(:)*sed_layer_thickness(:)/dt) ! first order interpolation at low concentration limit to prevent overdraw
    ! write(*,*) 'yt_debug: j = ', j, ( sed_PO4(1) )
# endif

# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! sumNUpPos                   [umol.N m-2.sg.hab s-1]     sum of all potential nitrogen uptake (used for scaling/normalizing uptake proportions)
    ! sumPUpPos                   [umol.P m-2.sg.hab s-1]     sum of all potential phosphorus uptake (used for scaling/normalizing uptake proportions)
    ! CHEM_uptake_possible(:)     [umol.CHEM m-2.sg.hab s-1]  CHEM uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    sumNUpPos = sum(NO3_uptake_possible(:))+sum(NH4_uptake_possible(:))
    sumPUpPos = sum(PO4_uptake_possible(:))

    ! Growth_limit_CHM          [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by chemical (CHM) uptake
    ! SgXperC                   [mol.CHM mol-1.C]           Species specific seagrass Phosphorus or Nitrogen to Carbon ratio
    ! CHM_uptake_possible(:)    [umol.CHM m-2.sg.hab s-1]   CHM uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers

    ! [umol.C/m2.hab s] = [mol.C/mol.CHM] [umol.CHM/m2.hab s]
    Growth_limit_NO3    = 1d0 / SgNperC   * sum(NO3_uptake_possible)
    Growth_limit_NH4    = 1d0 / SgNperC   * sum(NH4_uptake_possible)
    Growth_limit_PO4    = 1d0 / SgPperC   * sumPUpPos
# endif

! Gross Photosythesis/Growth rate ---------------------------------------------------------------------

    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! Growth_limit_light          [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by light
    ! Growth_limit_DIC            [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by DIC uptake
    ! Growth_limit_NO3            [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by NO3 uptake
    ! Growth_limit_NH4            [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by NH4 uptake
    ! Growth_limit_PO4            [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by PO4 uptake
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    Growth_rate = min(Growth_limit_light, Growth_limit_DIC, (Growth_limit_NO3+Growth_limit_NH4), Growth_limit_PO4)
# else
    Growth_rate = min(Growth_limit_light, Growth_limit_DIC)
# endif

    ! if (Growth_rate .lt. 0d0) then ! error handling
    !   write(*,*) "yt_debug: negative value error in photosynthesis/growth. Time =", &
    !     date, "Position =", i, j, "Growth_rate = ", Growth_rate
    !   write(*,*) "yt_debug:     Growth_rate        = ", Growth_rate
    !   write(*,*) "yt_debug:     Growth_limit_light = ", Growth_limit_light
    !   write(*,*) "yt_debug:     Growth_limit_DIC   = ", Growth_limit_DIC
    !   write(*,*) "yt_debug:     Growth_limit_NO3   = ", Growth_limit_NO3
    !   write(*,*) "yt_debug:     Growth_limit_NH4   = ", Growth_limit_NH4
    !   write(*,*) "yt_debug:     Growth_limit_PO4   = ", Growth_limit_PO4
    !   Growth_rate = 0.0d0
    ! endif

! Mass balance ---------------------------------------------------------------------------------------
    
    ! photosynthesis produces biomass
    ! delta_SgCBmF                [umol.C m-2.sg.hab s-1]     Rate of change in SgCBmF
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! dt                          [s]                         Time step interval duration
    delta_SgCBmF = delta_SgCBmF + Growth_rate

# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! CHEM_uptake_rate_from_ocean [mmol.CHEM m-2.sg.hab s-1]  direction of ocean to seagrass is positive
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! DIC_uptake_possible(:)      [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    if (sumCUpPos .gt. 0d0) then
      ! [mmol.CHEM/m2.hab s]     = [mmol.CHEM/m2.hab s]       + [umol.C/m2.hab s]                                (mmol/1000umol)
      DIC_uptake_rate_from_ocean = DIC_uptake_rate_from_ocean + Growth_rate * (DIC_uptake_possible(0)/sumCUpPos) * 0.001d0
      DO__uptake_rate_from_ocean = DO__uptake_rate_from_ocean - Growth_rate * (f_abv_cur                       ) * 0.001d0
    endif
    ! delta_sed_CHEM(Nsed)        [umol.CHEM L-1 s-1]         Rate of change in CHEM due to flux from roots to sediment (positive means seagrass to sediment)
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    ! root_mass_profile(sed)      []                          Mass ratio of roots in each layer (total must sum to 1.0)
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
    ! root_comp_dist_factor(Nsed) [g.rt.comp g-1.tot m2.sg.hab m-2.sedmnt mm-1]    root compartemtent distribution factor equal to f_blw_cur * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 
    if (sumCUpPos .gt. 0d0) then
      ! [umol.CHEM/L s] = [umol.CHEM/L s]  - [umol.C/m2.hab s] [m2.hab/m2.sedmnt] [] [1/mm] (1000mm/m)(m3/1000L)
      delta_sed_DIC(:)  = delta_sed_DIC(:) - Growth_rate      * sgrass_per_sedmnt &
                            * (DIC_uptake_possible(1:Nsed)/sumCUpPos) / sed_layer_thickness(:)
      ! [umol.CHEM/L s] = [umol.CHEM/L s]  - [umol.C/m2.hab s] [g.rt.comp/g.tot][m2.hab/m2.sedmnt mm] (1000mm/m)(m3/1000L)
      delta_sed_DO(:)   = delta_sed_DO(:)  + Growth_rate      * root_comp_dist_factor(:)
    endif
# else
    ! CHEM_uptake_rate_from_ocean [mmol.CHEM m-2.sg.hab s-1]  direction of ocean to seagrass is positive
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! [mmol.CHEM m-2.sg.hab s-1] = [mmol.CHEM m-2.sg.hab s-1] + [umol.C m-2.sg.hab s-1] (mmol/1000umol)
    DIC_uptake_rate_from_ocean   = DIC_uptake_rate_from_ocean + Growth_rate             * 0.001d0
    DO__uptake_rate_from_ocean   = DO__uptake_rate_from_ocean - Growth_rate             * 0.001d0
# endif

# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE && defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! CHEM_uptake_rate_from_ocean [mmol.CHEM m-2.sg.hab s-1]  CHEM uptake rate  * direction of ocean to seagrass is positive
    ! SgXperC                     [mol.CHEM mol-1.C]          Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! CHEM_uptake_possible(:)     [umol.CHEM m-2.sg.hab s-1]  CHEM uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! sumXUpPos                   [umol.N m-2.sg.hab s-1]     sum of all potential nitrogen or phosphate uptake (used for scaling/normalizing uptake proportions)
    ! [mmol.CHEM/m2.hab s]     = [mmol.CHEM/m2.hab s] + [mol.CHEM/mol.C] [umol.C/m2.hab s] [] (mmol/1000umol)
    if (sumNUpPos .gt. 0d0) then
      NO3_uptake_rate_from_ocean = NO3_uptake_rate_from_ocean + SgNperC * Growth_rate * (NO3_uptake_possible(0)/sumNUpPos) * 0.001d0
      NH4_uptake_rate_from_ocean = NH4_uptake_rate_from_ocean + SgNperC * Growth_rate * (NH4_uptake_possible(0)/sumNUpPos) * 0.001d0
    endif
    if (sumPUpPos .gt. 0d0) then
      PO4_uptake_rate_from_ocean = PO4_uptake_rate_from_ocean + SgPperC * Growth_rate * (PO4_uptake_possible(0)/sumPUpPos) * 0.001d0
    endif
    ! delta_sed_CHM(Nsed)         [umol.CHM L-1 s-1]          Rate of change in CHM due to flux from roots to sediment (positive means seagrass to sediment)
    ! SgXperC                     [mol.CHEM mol-1.C]          Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    ! CHM_uptake_possible(:)      [umol.CHM m-2.sg.hab s-1]   CHM uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! sumXUpPos                   [umol.N m-2.sg.hab s-1]     sum of all potential nitrogen or phosphate uptake (used for scaling/normalizing uptake proportions)
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
    ! [umol.CHEM/L s] = [umol.CHEM/L s] - [mol.CHEM/mol.C] [umol.C/m2.hab s] [m2.hab/m2.sedmnt] [] / [mm] (m2*mm/L)
    if (sumNUpPos .gt. 0d0) then
      !     (-)                           -               (+)
      delta_sed_NO3(:) = delta_sed_NO3(:) - SgNperC * Growth_rate * sgrass_per_sedmnt &
                          * (NO3_uptake_possible(1:Nsed)/sumNUpPos) / sed_layer_thickness(:)
      delta_sed_NH4(:) = delta_sed_NH4(:) - SgNperC * Growth_rate * sgrass_per_sedmnt &
                          * (NH4_uptake_possible(1:Nsed)/sumNUpPos) / sed_layer_thickness(:)
    endif
    if (sumPUpPos .gt. 0d0) then
      delta_sed_PO4(:) = delta_sed_PO4(:) - SgPperC * Growth_rate * sgrass_per_sedmnt &
                          * (PO4_uptake_possible(1:Nsed)/sumPUpPos) / sed_layer_thickness(:)
    endif
# elif defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    ! CHEM_uptake_rate_from_ocean [mmol.CHEM m-2.sg.hab s-1]  CHEM uptake rate  * direction of ocean to seagrass is positive
    ! SgXperC                     [mol.CHEM mol-1.C]          Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! sumXUpPos                   [umol.N m-2.sg.hab s-1]     sum of all potential nitrogen or phosphate uptake (used for scaling/normalizing uptake proportions)
    ! [mmol.CHEM/m2.hab s]     = [mmol.CHEM/m2.hab s]       + [mol.CHEM/mol.C] [umol.C/m2.hab s] (mmol/1000umol)
    if (sumNUpPos .gt. 0d0) then
      NO3_uptake_rate_from_ocean = NO3_uptake_rate_from_ocean + SgNperC * Growth_rate * 0.001d0 * NO3_uptake_possible(0)/sumNUpPos
      NH4_uptake_rate_from_ocean = NH4_uptake_rate_from_ocean + SgNperC * Growth_rate * 0.001d0 * NH4_uptake_possible(0)/sumNUpPos
    endif
    PO4_uptake_rate_from_ocean   = PO4_uptake_rate_from_ocean + SgPperC * Growth_rate * 0.001d0
# elif defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! delta_sed_CHM(Nsed)         [umol.CHM L-1 s-1]          Rate of change in CHM due to flux from roots to sediment (positive means seagrass to sediment)
    ! SgXperC                     [mol.CHEM mol-1.C]          Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    ! CHM_uptake_possible(:)      [umol.CHM m-2.sg.hab s-1]   CHM uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! sumXUpPos                   [umol.N m-2.sg.hab s-1]     sum of all potential nitrogen or phosphate uptake (used for scaling/normalizing uptake proportions)
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
    ! [umol.CHEM/L s] = [umol.CHEM/L s] - [mol.CHEM/mol.C] [umol.C/m2.hab s] [m2.hab/m2.sedmnt] [] / [mm] (m2*mm/L)
    if (sumNUpPos .gt. 0d0) then
      delta_sed_NO3(:) = delta_sed_NO3(:) - SgNperC * Growth_rate * sgrass_per_sedmnt &
                          * (NO3_uptake_possible(1:Nsed)/sumNUpPos) / sed_layer_thickness(:)
      delta_sed_NH4(:) = delta_sed_NH4(:) - SgNperC * Growth_rate * sgrass_per_sedmnt &
                          * (NH4_uptake_possible(1:Nsed)/sumNUpPos) / sed_layer_thickness(:)
    endif
    if (sumPUpPos .gt. 0d0) then
      delta_sed_PO4(:) = delta_sed_PO4(:) - SgPperC * Growth_rate * sgrass_per_sedmnt &
                          * (PO4_uptake_possible(1:Nsed)/sumPUpPos) / sed_layer_thickness(:)
    endif
# endif

# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! DO_released_from_NO3        [umol.O m-2.sg.hab s-1]     Total O2 produced from consumption of NO3 in the growth reaction: for every mol of NO3 uptake, release 2 mol of O2
    ! SgNperC                     [mol.N mol-1.C]             Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! [umol.O/m2.hab s] = [mol.O/mol.C] [umol.C/m2.hab s] []
    if (sumNUpPos .gt. 0d0) then
      DO_released_from_NO3 = 2d0 * SgNperC * Growth_rate * (sum(NO3_uptake_possible(:))/sumNUpPos)
    else
      DO_released_from_NO3 = 0.0d0
    endif
#  if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! DO__uptake_rate_from_ocean  [mmol.O m-2.sg.hab s-1]     direction of ocean to seagrass is positive
    ! [mmol.O/m2.hab s]        = [mmol.O/m2.hab s]          - [umol.O/m2.hab s]    []            (mmol/1000umol)
    DO__uptake_rate_from_ocean = DO__uptake_rate_from_ocean - DO_released_from_NO3 * (f_abv_cur) * 0.001d0
    ! delta_sed_DO(Nsed)          [umol.O L-1 s-1]            Rate of change in DO due to flux from roots to sediment (positive means seagrass to sediment)
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    ! root_comp_dist_factor(Nsed) [g.rt.comp g-1.tot m2.sg.hab m-2.sedmnt mm-1]    root compartemtent distribution factor equal to f_blw_cur * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 
    ! [umol.O/L s]  = [umol.O/L s]    + [umol.O/m2.hab s]    [g.rt.comp/g.tot][m2.hab/m2.sedmnt mm] (1000mm/m)(m3/1000L)
    delta_sed_DO(:) = delta_sed_DO(:) + DO_released_from_NO3 * root_comp_dist_factor(:)
#  else
    ! DO__uptake_rate_from_ocean  [mmol.O m-2.sg.hab s-1]     direction of ocean to seagrass is positive
    ! [mmol.O/m2.hab s]        = [mmol.O/m2.hab s]          - [umol.O/m2.hab s]    (mmol/1000umol)
    DO__uptake_rate_from_ocean = DO__uptake_rate_from_ocean - DO_released_from_NO3 * 0.001d0
#  endif
# endif

! update saved output values -------------------------------------------------------------------------

    ! GridPhot                    [umol.C m-2.grid s-1]       Seagrass gross photosynthesis rate per unit ground area (whole grid)
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! p_sgrass(Nsg)               [m2.sg.hab m-2.grid]        (0~1)  Seagrass coverage (every species)
    ! [umol.C/m2.grid s] = [umol.C/m2.hab s] [m2.hab/m2.grid]
    GridPhot             = Growth_rate       * p_sgrass(species)

    ! PhotLim                     []                          Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon uptake; 3 = Nitrogen uptake; 4 = Phosphorus uptake
# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    PhotLim = minloc((/Growth_limit_light, Growth_limit_DIC, (Growth_limit_NO3+Growth_limit_NH4) &
      , Growth_limit_PO4/), DIM=1)
# else
    PhotLim = minloc((/Growth_limit_light, Growth_limit_DIC/), DIM=1)
# endif

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

! Respiration rate DO limit --------------------------------------------------------------------------

    ! DO_uptake_possible(:)       [umol.DO m-2.sg.hab s-1]    DO uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Lf_DO_uptake_max            [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves
    ! LAI                         [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area 
    ! ocean_DO__conc              [umol.DO L-1]               DIC concentration of ocean water 
    ! k_lf_DO                     [umol.DO L-1]               Half saturation consentration of DO in water column for respiration

    ! [umol.DO/m2.hab s]  = [umol.DO/m2.lf s] [m2.lf/m2.hab] ( Michaelis-Menten term                     )
    DO_uptake_possible(0) = Lf_DO_uptake_max  * LAI          * ocean_DO__conc / (k_lf_DO + ocean_DO__conc)
    ! [umol.DO/m2.hab s]  = min( [umol.DO/m2.hab s]   ,      [umol.DO/L]     [m/s](1000L/m3)
    DO_uptake_possible(0) = min( DO_uptake_possible(0), 0.5d0*ocean_DO__conc*dz/dt*1000.0d0 ) ! first order interpolation at low concentration limit to prevent overdraw

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
    sumOUpPos = sum(DO_uptake_possible(:))

    ! Resp_limit_DO               [umol.C m-2.sg.hab s-1]     Respiration/un-growth rate possible as limited by DO uptake
    ! [umol.C/m2.hab s] = [mol.C/mol.DO] [umol.DO/m2.hab s]
    Resp_limit_DO       =                sumOUpPos

    ! yt_edit implement anaerobic respiraation???


! Respiration possible -------------------------------------------------------------------------------

    ! Resp_pos                    [umol.C m-2.sg.hab s-1]     Respiration rate possible in this time step
    ! Resp_limit_DO               [umol.C m-2.sg.hab s-1]     Respiration/un-growth rate possible as limited by DO uptake

    Resp_pos = Resp_limit_DO

! Respiration required -------------------------------------------------------------------------------

    ! Resp_req                    [umol.C m-2.sg.hab s-1]     Respiration rate required for maintenance in this time step
    ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    ! R_basal                     [umol.C m-2.leaf s-1]       Maximum respiration rate
    ! rho_lf                      [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
    ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    ! dt                          [s]                         time step duration

    ! respFrac = R_basal/rho_lf*SgMperC*dt*0.000000001        The fraction of mass required for respiration this time step! [umol.C m-2.leaf s-1] [m2.leaf kg-1.drywt ] [g.drywt mol-1.C] [s] (kg/1000g)(mol/1000000umol) = 0.000000001 []
    ! mass_respired = SgCBmF(respFrac)/(1+respFrac)/dt        With a 1:1 ratio of carbon-biomass to respiration we get:
    
    Resp_req = SgCBmF * (R_basal/rho_lf*SgMperC*dt*0.000000001)/(1+R_basal/rho_lf*SgMperC*dt*0.000000001) / dt

! Respiration/Un-growth rate -------------------------------------------------------------------------

    ! Resp_rate                   [umol.C m-2.sg.hab s-1]     Gross respiration/un-growth rate
    ! Resp_pos                    [umol.C m-2.sg.hab s-1]     Respiration rate possible in this time step
    ! Resp_req                    [umol.C m-2.sg.hab s-1]     Respiration rate required for maintenance in this time step
 
    Resp_rate = min(Resp_pos, Resp_req)

    if (Resp_rate .lt. 0d0) then ! error handling
      ! write(*,*) 'yt_debug: negative value error in respiration. Time =', &
      !   date, 'Position =', i, j
      ! write(*,*) 'yt_debug:     Resp_rate =', Resp_rate
      ! write(*,*) 'yt_debug:     Resp_pos  =', Resp_pos
      ! write(*,*) 'yt_debug:     Resp_req  =', Resp_req
      ! write(*,*) 'yt_debug:     DO_uptake_possible =', DO_uptake_possible
      Resp_rate = 0.0d0
    endif

    ! write(*,*) 'yt_debug: Growth_rate = ', Growth_rate, '   Resp_rate = ', &
    !   Resp_rate, 'Resp_req = ', Resp_req, 'Resp_pos = ', Resp_pos

! Dieoff from base turnover rate rate -------------------------------------------------------------------------

    ! dieoff_rate                 [umol.C m-2.sg.hab s-1]     Seagrass dieoff rate
    ! Turnover_rate               [umol.C umol-1.C day-1]     Base mass turnover rate (dieoff) in fraction of mass per day
    ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
    ! [umol.C.dead/m2.hab s] = [umol.C.dead/umol.C.tot day] [umol.C.tot/m2.hab] [day/86400s]
    dieoff_rate              = Turnover_rate                * SgCBmF             / 86400.0d0


! Dieoff when respiration is insufficient ------------------------------------------------------------

    ! Resp_req                    [umol.C m-2.sg.hab s-1]     Respiration rate required for maintenance in this time step
    ! Resp_pos                    [umol.C m-2.sg.hab s-1]     Respiration rate possible in this time step
    if (Resp_req .gt. Resp_pos) then
      ! anoxic_dieoff             [umol.C m-2.sg.hab]         Seagrass dieoff due to anoxia this time step
      ! Resp_pos                  [umol.C m-2.sg.hab s-1]     Respiration rate possible in this time step
      ! HabArea                   [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
      ! R_basal                   [umol.C m-2.leaf s-1]       Maximum respiration rate
      ! rho_lf                    [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
      ! SgMperC                   [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
      ! SgCBmF                    [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval

      ! respiration possible           = maintenance repiration rate                                                          * biomass maintainable
      ! [umol.resp / s               ] = [umol.resp / umol.C s                                                              ] [umol.C                      ]
      ! [umol.resp/m2.hab s] [m2.hab ] = [umol.resp/m2.lf s] [m2.lf/kg.drywt] [g.drywt/mol.C] (kg/1000g)(mol.C/1000000umol.C) [umol.C/m2.hab     ] [m2.hab ]
      ! Resp_pos             * HabArea = R_basal             / rho_lf         * SgMperC       * 0.000000001d0                 * SgCBm_maintainable * HabArea

      ! solve for SgCBm_maintainable:
      ! SgCBm_maintainable = Resp_pos / (R_basal             / rho_lf         * SgMperC       * 0.000000001d0)

      ! anoxic_dieoff = SgCBmF - SgCBm_maintainable

      anoxic_dieoff = SgCBmF - (Resp_pos / (R_basal             / rho_lf         * SgMperC       * 0.000000001d0))

      ! limit dieoff rate (especially for when DO intake is zero)
      ! max_anoxic_dieoff_rate      [mol.C mol-1.C day-1]       maximum dieoff due to anoxia in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
      ! [mol.C mol-1.C]     = [mol.C mol-1.C day-1] [s]  (day/86400s)
      ! max_dieoff_per_step = max_anoxic_dieoff_rate       * dt / 86400
      if (anoxic_dieoff/SgCBmF .gt. max_anoxic_dieoff_rate*dt/86400d0) then
        anoxic_dieoff = SgCBmF * max_anoxic_dieoff_rate*dt/86400d0
      endif

      ! dieoff_rate                 [umol.C m-2.sg.hab s-1]     Seagrass dieoff rate
      dieoff_rate = dieoff_rate + anoxic_dieoff /dt

    endif

! Mass balance ---------------------------------------------------------------------------------------

    ! respiration consumes CH2O in biomass
    ! delta_SgCBmF                [umol.C m-2.sg.hab s-1]     Rate of change in SgCBmF
    ! Resp_rate                   [umol.C m-2.sg.hab s-1]     Gross respiration/un-growth rate
    delta_SgCBmF = delta_SgCBmF - Resp_rate

# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! CHEM_uptake_rate from_ocean [mmol m-2 s-1]              direction of ocean to seagrass is positive
    ! Resp                        [mmol.C m-2.grid s-1]       Gross respiration rate
    ! DO_uptake_possible(:)       [umol.DO m-2.sg.hab s-1]    DO uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! sumOUpPos                   [umol.DO m-2.sg.hab s-1]    sum of all potential DO uptake (used for scaling/normalizing uptake proportions)
    ! TotLfCBm                    [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
    ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
    ! [mmol.CHEM/m2.hab s]     = [mmol.CHEM/m2.hab s]      + [umol.C/m2.hab s]                                               (mmol/1000umol)
    if (sumOUpPos .gt. 0d0) then
      DO__uptake_rate_from_ocean = DO__uptake_rate_from_ocean + Resp_rate * (DO_uptake_possible(0)/sumOUpPos) * 0.001d0
    endif
    DIC_uptake_rate_from_ocean = DIC_uptake_rate_from_ocean - Resp_rate * (f_abv_cur                      ) * 0.001d0
    ! delta_sed_CHEM(Nsed)        [umol.CHEM L-1 s-1]         Rate of change in CHEM due to flux from roots to sediment (positive means seagrass to sediment)
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    ! root_mass_profile(sed)      []                          Mass ratio of roots in each layer (total must sum to 1.0)
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
    ! root_comp_dist_factor(Nsed) [g.rt.comp g-1.tot m2.sg.hab m-2.sedmnt mm-1]    root compartemtent distribution factor equal to f_blw_cur * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 
    ! sumOUpPos                   [umol.DO m-2.sg.hab s-1]    sum of all potential DO uptake (used for scaling/normalizing uptake proportions)
    ! TotRtCBm                    [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
    ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
    ! [umol.CHEM/L s] = [umol.CHEM/L s] - [umol.C/m2.hab s] [m2.hab/m2.sedmnt] [1/mm] (1000mm/m)(m3/1000L)
    if (sumOUpPos .gt. 0d0) then
      delta_sed_DO(:) = delta_sed_DO(:) - Resp_rate * sgrass_per_sedmnt &
                          * (DO_uptake_possible(1:Nsed)/sumOUpPos) / sed_layer_thickness(:)
    endif
    ! [umol.CHEM/L s] = [umol.CHEM/L s]  - [umol.C/m2.hab s] [g.rt.comp/g.tot][m2.hab/m2.sedmnt mm] (1000mm/m)(m3/1000L)
    delta_sed_DIC(:)  = delta_sed_DIC(:) + Resp_rate         * root_comp_dist_factor(:)
# else
    ! CHEM_uptake_rate from_ocean [mmol m-2 s-1]              direction of ocean to seagrass is positive
    ! Resp                        [mmol.C m-2.grid s-1]       Gross respiration rate
    ! [mmol.CHEM/m2.hab s]     = [mmol.CHEM/m2.hab s]       + [umol.C/m2.hab s] (mmol/1000umol)
    DO__uptake_rate_from_ocean = DO__uptake_rate_from_ocean + Resp_rate         * 0.001d0
    DIC_uptake_rate_from_ocean = DIC_uptake_rate_from_ocean - Resp_rate         * 0.001d0
# endif

# if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE && defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! CHEM_uptake_rate_from_ocean [mmol.CHEM m-2.sg.hab s-1]  CHEM uptake rate  * direction of ocean to seagrass is positive
    ! SgXperC                     [mol.CHEM mol-1.C]          Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! Resp_rate                   [umol.C m-2.sg.hab s-1]     Gross respiration/un-growth rate
    ! TotLfCBm                    [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
    ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
    ! [mmol.CHEM/m2.hab s]     = [mmol.CHEM/m2.hab s] - [mol.CHEM/mol.C] [umol.C/m2.hab s] [] (mmol/1000umol)
    NH4_uptake_rate_from_ocean = NH4_uptake_rate_from_ocean - SgNperC * Resp_rate * (f_abv_cur) * 0.001d0 ! yt_edit consider if any fraction should go to nitrate waste?
    PO4_uptake_rate_from_ocean = PO4_uptake_rate_from_ocean - SgPperC * Resp_rate * (f_abv_cur) * 0.001d0
    ! delta_sed_CHM(Nsed)         [umol.CHM L-1 s-1]          Rate of change in CHM due to flux from roots to sediment (positive means seagrass to sediment)
    ! SgXperC                     [mol.CHEM mol-1.C]          Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! Resp_rate                   [umol.C m-2.sg.hab s-1]     Gross respiration/un-growth rate
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
    ! root_comp_dist_factor(Nsed) [g.rt.comp g-1.tot m2.sg.hab m-2.sedmnt mm-1]    root compartemtent distribution factor equal to f_blw_cur * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 
    ! TotRtCBm                    [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
    ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
    ! [umol.CHEM/L s] = [umol.CHEM/L s] - [mol.CHEM/mol.C] [umol.C/m2.hab s] [g.rt.comp/g.tot][m2.hab/m2.sedmnt mm] (m2.sedmnt*mm/L)
    delta_sed_NH4(:) = delta_sed_NH4(:) + SgNperC * Resp_rate * root_comp_dist_factor(:)
    delta_sed_PO4(:) = delta_sed_PO4(:) + SgPperC * Resp_rate * root_comp_dist_factor(:)
# elif defined SEAGRASS_LEAF_NUTRIENT_UPTAKE
    ! CHEM_uptake_rate_from_ocean [mmol.CHEM m-2.sg.hab s-1]  CHEM uptake rate  * direction of ocean to seagrass is positive
    ! SgXperC                     [mol.CHEM mol-1.C]          Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! Resp_rate                   [umol.C m-2.sg.hab s-1]     Gross respiration/un-growth rate
    ! [mmol.CHEM/m2.hab s]     = [mmol.CHEM/m2.hab s]       + [mol.CHEM/mol.C] [umol.C/m2.hab s] (mmol/1000umol)
    NH4_uptake_rate_from_ocean = NH4_uptake_rate_from_ocean - SgNperC          * Resp_rate       * 0.001d0
    PO4_uptake_rate_from_ocean = PO4_uptake_rate_from_ocean - SgPperC          * Resp_rate       * 0.001d0
# elif defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! delta_sed_CHM(Nsed)         [umol.CHM L-1 s-1]          Rate of change in CHM due to flux from roots to sediment (positive means seagrass to sediment)
    ! SgXperC                     [mol.CHEM mol-1.C]          Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! Resp_rate                   [umol.C m-2.sg.hab s-1]     Gross respiration/un-growth rate
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
    ! root_comp_dist_factor(Nsed) [g.rt.comp g-1.tot m2.sg.hab m-2.sedmnt mm-1]    root compartemtent distribution factor equal to f_blw_cur * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 
    ! TotRtCBm                    [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
    ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
    ! [umol.CHEM/L s] = [umol.CHEM/L s] - [mol.CHEM/mol.C] [umol.C/m2.hab s] [g.rt.comp/g.tot][m2.hab/m2.sedmnt mm] (m2.sedmnt*mm/L)
    delta_sed_NH4(:) = delta_sed_NH4(:) + SgNperC * Resp_rate * root_comp_dist_factor(:)
    delta_sed_PO4(:) = delta_sed_PO4(:) + SgPperC * Resp_rate * root_comp_dist_factor(:)
# else
    ! nothing
# endif


    ! delta_SgCBmF                [umol.C m-2.sg.hab s-1]     Rate of change in SgCBmF
    ! dieoff_rate                 [umol.C m-2.sg.hab s-1]     Seagrass dieoff rate
    delta_SgCBmF = delta_SgCBmF - dieoff_rate

# if defined SEAGRASS_LEAF_POM
    ! POX_uptake_rate_from_ocean  [mmol.X m-2.sg.hab s-1]     Particulate organic (carbon, nitrogen, or phosphorus) uptake rate     * direction of ocean to seagrass is positive
    ! f_abv_cur                   [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current above ground biomass to total biomass fraction, calculated on the last growth interval
    ! SgXperC                     [mol.X mol-1.C]             Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! dieoff_rate                 [umol.C m-2.sg.hab s-1]     Seagrass dieoff rate
    ! [mmol.X/m2.hab s]        = [mmol.X/m2.hab s]          - [X/C]   [  ]        [umol.C/m2.hab s] (mmol/1000umol)
    POC_uptake_rate_from_ocean = POC_uptake_rate_from_ocean -           f_abv_cur * dieoff_rate     * 0.001d0
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    PON_uptake_rate_from_ocean = PON_uptake_rate_from_ocean - SgNperC * f_abv_cur * dieoff_rate     * 0.001d0
    POP_uptake_rate_from_ocean = POP_uptake_rate_from_ocean - SgPperC * f_abv_cur * dieoff_rate     * 0.001d0
#  endif
# endif

# if defined SEAGRASS_ROOT_POM
    ! delta_sed_POX(Nsed)         [umol.C L-1 s-1]            Rate of change in POX due to root death in sediment (positive means seagrass to sediment)
    ! SgXperC                     [mol.X mol-1.C]             Species specific seagrass Nitrogen or Phosphorus to Carbon ratio
    ! f_blw_cur                   [g.rt.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] current below ground biomass to total biomass fraction, calculated on the last growth interval
    ! root_mass_profile(sed)      []                          Mass ratio of roots in each layer (total must sum to 1.0)
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
    ! root_comp_dist_factor(Nsed) [g.rt.comp g-1.tot m2.sg.hab m-2.sedmnt mm-1]    root compartemtent distribution factor equal to f_blw_cur * root_mass_profile(:) * sgrass_per_sedmnt / sed_layer_thickness(:) 
    ! dieoff_rate                 [umol.C m-2.sg.hab s-1]     Seagrass dieoff rate
    ! [umol.X/L s]   = [umol.X/L s]     + [X/C]   [umol.C/m2.hab s] [g.rt.comp/g.tot][m2.hab/m2.sedmnt mm] (m2.sedmnt*mm/L)
    delta_sed_POC(:) = delta_sed_POC(:) +           dieoff_rate     * root_comp_dist_factor(:)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    delta_sed_PON(:) = delta_sed_PON(:) + SgNperC * dieoff_rate     * root_comp_dist_factor(:)
    delta_sed_POP(:) = delta_sed_POP(:) + SgPperC * dieoff_rate     * root_comp_dist_factor(:)
#  endif
# endif

! update saved output values -------------------------------------------------------------------------

    ! GridResp                    [umol.C m-2.grid s-1]       Seagrass respiration rate per unit ground area (whole grid)
    ! Resp_rate                   [umol.C m-2.sg.hab s-1]     Gross respiration/un-growth rate
    ! p_sgrass(Nsg)               [m2.sg.hab m-2.grid]        (0~1)  Seagrass coverage (every species)
    ! [umol.C/m2.grid s] = [umol.C/m2.hab s] [m2.hab/m2.grid]
    GridResp             = Resp_rate       * p_sgrass(species)

    ! GridNetPhot                 [umol.C m-2.grid s-1]       Seagrass net photosynthesis rate per unit ground area (whole grid)
    ! GridPhot                    [umol.C m-2.grid s-1]       Seagrass gross photosynthesis rate per unit ground area (whole grid)
    ! GridResp                    [umol.C m-2.grid s-1]       Seagrass respiration rate per unit ground area (whole grid)
    ! [umol.C/m2.grid s] = [umol.C/m2.grid s] - [umol.C/m2.grid s]
    GridNetPhot          = GridPhot           - GridResp

    ! dieoff_rate                 [umol.C m-2.sg.hab s-1]     Seagrass dieoff rate
    ! GridDieoff                  [umol.C m-2.grid s-1]       Seagrass dieoff rate carbon biomass per unit ground area (whole grid)
    ! p_sgrass(Nsg)               [m2.sg.hab m-2.grid]        (0~1)  Seagrass coverage (every species)
    ! [umol.C/m2.grid s] = [umol.C/m2.hab s] [m2.hab/m2.grid]
    GridDieoff           = dieoff_rate       * p_sgrass(species)


! ====================================================================================================
! ----------------------------------------------------------------------------------------------------
!                                UPDATE AT END OF MAIN INTERVAL
! ----------------------------------------------------------------------------------------------------
! ====================================================================================================

    ! mass balance of seagrass growth/dieoff

    ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
    ! delta_SgCBmF                 [umol.C m-2.sg.hab s-1]     Rate of change in SgCBmF
    ! dt                          [s]                         Time step interval duration
    ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    if (SgCBmF + delta_SgCBmF * dt .lt. 1.0d0/SgMperC) then  ! if seagrass dry weight biomass would become less than 1.0 ug per m2, then instead set to zero
      delta_SgCBmF = -1d0 * SgCBmF / dt
      SgCBmF = 0d0
    else
      SgCBmF = SgCBmF + delta_SgCBmF * dt
    endif

    if (SgCBmF /= SgCBmF) then
      write(*,*) 'SEAGRASS error: SgCBmF is NAN'
      error stop
    endif

    ! [mol.C] = [umol.C/m2.hab] [m2.hab]  (mol/1000000umol)
    TotSgCBmF = SgCBmF          * HabArea * 0.000001d0



# if defined NET_ZERO_MASS_START
    ! Destroy (erase) initial mass output until net zero is reached, before switching to normal flux <<< !

    ! InitialC                    [mmol.C]                    Initial carbon remaining to destroy until net zero
    ! CHEM_uptake_rate_from_ocean [mmol.CHEM m-2.sg.hab s-1]  direction of ocean to seagrass is positive
    ! delta_sed_CHEM(Nsed)        [umol.CHEM L-1 s-1]         Rate of change in CHEM due to flux from roots to sediment (positive means seagrass to sediment)
    ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
    ! sgrass_per_sedmnt           [m2.sg.hab m-2.sedmnt]      (0~1)  Seagrass coverage per sediment coverage

    if (InitialC > 0.0d0) then
      if (DIC_uptake_rate_from_ocean < 0) then
        ! [mmol.C] = [mmol.C] + [mmol.C/m2.hab s]          [m2.hab]  [s]
        InitialC   = InitialC + DIC_uptake_rate_from_ocean * HabArea * dt
        DIC_uptake_rate_from_ocean = 0.0d0
      endif
#  if defined SEAGRASS_LEAF_POM
      if (POC_uptake_rate_from_ocean < 0) then
        ! [mmol.C] = [mmol.C] + [mmol.C/m2.hab s]          [m2.hab]  [s]
        InitialC   = InitialC + POC_uptake_rate_from_ocean * HabArea * dt
        POC_uptake_rate_from_ocean = 0.0d0
      endif
#  endif
#  if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
      do k=1,Nsed
        if (delta_sed_DIC(k) > 0 ) then
          ! [mmol.C] = [mmol.C] - [umol.C/L.sed s] [mm.sed]                 [m2.sed/m2.hab]     [m2.hab]  [s]  [1000 L.sed/m3.sed] [m.sed / 1000 mm.sed] 
          InitialC   = InitialC - delta_sed_DIC(k) * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt        
          delta_sed_DIC(k) = 0.0d0
        endif
      enddo
#  endif
#  if defined SEAGRASS_ROOT_POM
      do k=1,Nsed
        if (delta_sed_POC(k) > 0 ) then
          ! [mmol.C] = [mmol.C] - [umol.C/L.sed s] [mm.sed]                 [m2.sed/m2.hab]     [m2.hab]  [s]  [1000 L.sed/m3.sed] [m.sed / 1000 mm.sed] 
          InitialC   = InitialC - delta_sed_POC(k) * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt        
          delta_sed_POC(k) = 0.0d0
        endif
      enddo
#  endif
    endif

    if (InitialN > 0.0d0) then
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE         
      if (NO3_uptake_rate_from_ocean < 0) then
        ! [mmol.N] = [mmol.N] + [mmol.N/m2.hab s]          [m2.hab]  [s]
        InitialN   = InitialN + NO3_uptake_rate_from_ocean * HabArea * dt
        NO3_uptake_rate_from_ocean = 0.0d0
      endif
      if (NH4_uptake_rate_from_ocean < 0) then
        ! [mmol.N] = [mmol.N] + [mmol.N/m2.hab s]          [m2.hab]  [s]
        InitialN   = InitialN + NH4_uptake_rate_from_ocean * HabArea * dt
        NH4_uptake_rate_from_ocean = 0.0d0
      endif
#  endif
#  if defined SEAGRASS_LEAF_POM && (defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE)
      if (PON_uptake_rate_from_ocean < 0) then
        ! [mmol.N] = [mmol.N] + [mmol.N/m2.hab s]          [m2.hab]  [s]
        InitialN   = InitialN + PON_uptake_rate_from_ocean * HabArea * dt
        PON_uptake_rate_from_ocean = 0.0d0
      endif
#  endif
#  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
      do k=1,Nsed
        if (delta_sed_NO3(k) > 0 ) then
          ! [mmol.N] = [mmol.N] - [umol.N/L.sed s] [mm.sed]                 [m2.sed/m2.hab]     [m2.hab]  [s]  [1000 L.sed/m3.sed] [m.sed / 1000 mm.sed] 
          InitialN   = InitialN - delta_sed_NO3(k) * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt        
          delta_sed_NO3(k) = 0.0d0
        endif
        if (delta_sed_NH4(k) > 0 ) then
          ! [mmol.N] = [mmol.N] - [umol.N/L.sed s] [mm.sed]                 [m2.sed/m2.hab]     [m2.hab]  [s]  [1000 L.sed/m3.sed] [m.sed / 1000 mm.sed] 
          InitialN   = InitialN - delta_sed_NH4(k) * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt        
          delta_sed_NH4(k) = 0.0d0
        endif
      enddo
#  endif
#  if defined SEAGRASS_ROOT_POM && (defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE)
      do k=1,Nsed
        if (delta_sed_PON(k) > 0 ) then
          ! [mmol.N] = [mmol.N] - [umol.N/L.sed s] [mm.sed]                 [m2.sed/m2.hab]     [m2.hab]  [s]  [1000 L.sed/m3.sed] [m.sed / 1000 mm.sed] 
          InitialN   = InitialN - delta_sed_PON(k) * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt        
          delta_sed_PON(k) = 0.0d0
        endif
      enddo
#  endif
    endif

    if (InitialP > 0.0d0) then
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE         
      if (PO4_uptake_rate_from_ocean < 0) then
        ! [mmol.P] = [mmol.P] + [mmol.P/m2.hab s]          [m2.hab]  [s]
        InitialP   = InitialP + PO4_uptake_rate_from_ocean * HabArea * dt
        PO4_uptake_rate_from_ocean = 0.0d0
      endif
#  endif
#  if defined SEAGRASS_LEAF_POM && (defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE)
      if (POP_uptake_rate_from_ocean < 0) then
        ! [mmol.P] = [mmol.P] + [mmol.P/m2.hab s]          [m2.hab]  [s]
        InitialP   = InitialP + POP_uptake_rate_from_ocean * HabArea * dt
        POP_uptake_rate_from_ocean = 0.0d0
      endif
#  endif
#  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
      do k=1,Nsed
        if (delta_sed_PO4(k) > 0 ) then
          ! [mmol.P] = [mmol.P] - [umol.P/L.sed s] [mm.sed]                 [m2.sed/m2.hab]     [m2.hab]  [s]  [1000 L.sed/m3.sed] [m.sed / 1000 mm.sed] 
          InitialP   = InitialP - delta_sed_PO4(k) * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt        
          delta_sed_PO4(k) = 0.0d0
        endif
      enddo
#  endif
#  if defined SEAGRASS_ROOT_POM && (defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE)
      do k=1,Nsed
        if (delta_sed_POP(k) > 0 ) then
          ! [mmol.P] = [mmol.P] - [umol.P/L.sed s] [mm.sed]                 [m2.sed/m2.hab]     [m2.hab]  [s]  [1000 L.sed/m3.sed] [m.sed / 1000 mm.sed] 
          InitialP   = InitialP - delta_sed_POP(k) * sed_layer_thickness(k) / sgrass_per_sedmnt * HabArea * dt        
          delta_sed_POP(k) = 0.0d0
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
      TotSgCBm  = SgCBmF          * HabArea * 0.000001d0


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


! update carbon-biomass per area ---------------------------------------------------------------------

      ! adjust SgCBmF according to new HabArea
      ! SgCBmF                      [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) updates on fine time interval
      ! LfCBm                       [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
      ! RtCBm                       [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
      ! TotSgCBm                    [mol.C]                     Total seagrass carbon-biomass in grid cell
      ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
      ! [umol.C/m2.hab] = [mol.C]  / [m2.hab] (1000000umol/mol)
      SgCBmF            = TotSgCBm / HabArea  * 1000000d0
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
      SgDryBm      = SgCBmF          * SgMperC * 0.000001d0
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
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    , Nsed                        &   !                           Number of sediment layers
# endif
    , dz                          &   ! [m]                       vertical layer thickness (bottom water column layer)
    , date                        &   ! Date (days since x or elapsed days) yt_edit best if in phase with clock to maintain consistency on restart
    , dt                          &   ! [s]                       time step
    )
! ====================================================================================================
! Module dependencies
! ====================================================================================================
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
# if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM
    integer, intent(in)   :: Nsed                       !                             Number of sediment layers
# endif
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
    SEDECO(ng)%sg_dDIC_dt(species,i,j,:) = 0.0d0                  ! [umol.DIC L-1 s-1]          Rate of change in DIC due to flux from roots to sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dO2_dt (species,i,j,:) = 0.0d0                  ! [umol.DO L-1 s-1]           Rate of change in DO due to flux from roots to sediment (positive means seagrass to sediment)
# endif
# if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    SEDECO(ng)%sg_dNO3_dt(species,i,j,:) = 0.0d0                  ! [umol.NO3 L-1 s-1]          Rate of change in NO3 due to flux from roots to sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dNH4_dt(species,i,j,:) = 0.0d0                  ! [umol.NH4 L-1 s-1]          Rate of change in NH4 due to flux from roots to sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dPO4_dt(species,i,j,:) = 0.0d0                  ! [umol.PO4 L-1 s-1]          Rate of change in PO4 due to flux from roots to sediment (positive means seagrass to sediment)
# endif
# if defined SEAGRASS_ROOT_POM
    SEDECO(ng)%sg_dPOC_dt(species,i,j,:) = 0.0d0                  ! [umol.C L-1 s-1]            Rate of change in POC due to root death in sediment (positive means seagrass to sediment)
#  if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    SEDECO(ng)%sg_dPON_dt(species,i,j,:) = 0.0d0                  ! [umol.N L-1 s-1]            Rate of change in PON due to root death in sediment (positive means seagrass to sediment)
    SEDECO(ng)%sg_dPOP_dt(species,i,j,:) = 0.0d0                  ! [umol.P L-1 s-1]            Rate of change in POP due to root death in sediment (positive means seagrass to sediment)
#  endif
# endif

! ====================================================================================================
! ----------------------------------------------------------------------------------------------------
!                                  HOUSE KEEPING AT START OF LOOP
! ----------------------------------------------------------------------------------------------------
! ====================================================================================================

    if (date .gt. real(int(Day*24), 8)/24) then
      is_growth_interval = .true.
      Day = real(int(date*24+1), 8)/24d0
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
    SGRASS(ng)%SgCBmF(species,i,j) = 0.0d0
    ! TotSgCBmF                   [mol.C]                     Total seagrass carbon-biomass in grid cell. updates on fine time interval
    SGRASS(ng)%TotSgCBmF(species,i,j) = 0.0d0

    ! GridPhot                    [umol.C m-2.grid s-1]       Seagrass gross photosynthesis rate per unit ground area (whole grid)
    SGRASS(ng)%GridPhot(species,i,j) = 0.0d0
    ! PhotLim                     []                          Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon uptake; 3 = Nitrogen uptake; 4 = Phosphorus uptake
    SGRASS(ng)%PhotLim(species,i,j) = 0
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
