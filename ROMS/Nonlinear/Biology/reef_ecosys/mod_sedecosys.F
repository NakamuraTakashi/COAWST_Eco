!!!=== Copyright (c) 2013-2024 Takashi NAKAMURA  =====!!!
!!!         - Tokyo Institute of Technology -         !!!
!!!                                                   !!!
!!!          Sediment Biogeochemical Module           !!!
!!!            Developed by Masa Muto 2023-           !!!
!!!===================================================!!!
 
#include "cppdefs.h"

!!!**** MODULE OF SEDIMENT ECOSYSTEM MODEL ************************************
#if defined SEDIMENT_EMPIRICAL
!!! **********************************************************************
!!!  Empirical sediment model
!!! **********************************************************************
MODULE mod_sedecosys
#if defined SEDIMENT_ECOSYS
  implicit none
  TYPE T_SEDECO
    real(8), pointer :: Pg(:,:) 
    real(8), pointer :: R (:,:) 
    real(8), pointer :: G (:,:) 
    real(8), pointer :: QC(:,:)
# if defined CARBON_ISOTOPE
!  13C isotope
    real(8), pointer :: Q13C(:,:)
# endif
# if defined NUTRIENTS         
    real(8), pointer :: QN(:,:)
    real(8), pointer :: QP(:,:)
# endif
  END TYPE T_SEDECO
  TYPE (T_SEDECO), allocatable :: SEDECO(:)
CONTAINS

!!! **********************************************************************
!!!  set initial conditions for empirical sediment model
!!! **********************************************************************

  subroutine initialize_sedecosys(ng, Ngrids, LBi, UBi, LBj, UBj)
    USE mod_geochem
    
    implicit none
! input parameters
    integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj
    real(8)  R13C
    integer i,j,n
    IF (ng.eq.1) allocate ( SEDECO(Ngrids) )
    allocate( SEDECO(ng)%Pg(LBi:UBi,LBj:UBj)     )
    allocate( SEDECO(ng)%R (LBi:UBi,LBj:UBj)     )
    allocate( SEDECO(ng)%G (LBi:UBi,LBj:UBj)     )
    allocate( SEDECO(ng)%QC(LBi:UBi,LBj:UBj)     )
# if defined CARBON_ISOTOPE
    allocate( SEDECO(ng)%Q13C(LBi:UBi,LBj:UBj)   )
# endif
# if defined NUTRIENTS         
    allocate( SEDECO(ng)%QN(LBi:UBi,LBj:UBj)     )
    allocate( SEDECO(ng)%QP(LBi:UBi,LBj:UBj)     )
# endif

!------------------------------------------
!  Set initial conditions
    do j=LBj,UBj
      do i=LBi,UBi
!      sediment internal conditions
        SEDECO(ng)%Pg(i,j) = 0.0d0
        SEDECO(ng)%R (i,j) = 0.0d0
        SEDECO(ng)%G (i,j) = 0.0d0
        SEDECO(ng)%QC(i,j) = 15.0d0  !!!tentative
# if defined CARBON_ISOTOPE
        R13C = R13C_fromd13C(-18.0d0)
!        c13CH2O (n,i,j)=R13C/(1.+R13C)*CH2O(n,i,j)
        SEDECO(ng)%Q13C(i,j) = R13C * SEDECO(ng)%QC(i,j)
# endif
# if defined NUTRIENTS         
        SEDECO(ng)%QN(i,j) = 1.5d0  !!!tentative
        SEDECO(ng)%QP(i,j) = 0.1d0  !!!tentative
# endif
      enddo
    enddo
    
    RETURN
    
  END SUBROUTINE initialize_sedecosys

!!! **********************************************************************
!!!  Main program of empirical sediment model
!!! **********************************************************************

  SUBROUTINE sedecosys &
!   input parameters
    ( ng, i, j       &   ! ng: nested grid number; i,j: position
    , PFD            &   ! Photon flux density (umol m-2 s-1)
    , rho_sw         &   ! Density of seawater (g cm-3)
    , DICamb         &   ! DIC (umol kg-1)
    , TAamb          &   ! TA (umol kg-1)
    , DOamb          &   ! DO (umol L-1)
# if defined NUTRIENTS         
    , NH4amb         &   ! NH4 concentration (umol L-1)
# endif
# if defined CARBON_ISOTOPE
    , DI13Camb       &   ! 13C of DIC (umol kg-1)
# endif
!   output parameters
    , DICuptake      &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
    , TAuptake       &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
    , DOuptake       &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# if defined NUTRIENTS         
    , NO3uptake      &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
    , NH4uptake      &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
    , PO4uptake      &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
# if defined CARBON_ISOTOPE
    , DI13Cuptake    &   ! DI13C uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
# endif
    )

!-----------------------------------------------------------------------
    USE mod_geochem
    
    implicit none

! input parameters
    integer, intent(in) :: ng, i, j    ! ng: nested grid number; i,j: position
    real(8), intent(in) :: PFD
    real(8), intent(in) :: rho_sw   
    real(8), intent(in) :: DICamb
    real(8), intent(in) :: TAamb
    real(8), intent(in) :: DOamb
# if defined NUTRIENTS         
    real(8), intent(in) :: NH4amb
# endif
# if defined CARBON_ISOTOPE
    real(8), intent(in) :: DI13Camb
# endif
! output parameters
    real(8), intent(out) :: DICuptake
    real(8), intent(out) :: TAuptake
    real(8), intent(out) :: DOuptake
# if defined NUTRIENTS         
    real(8), intent(out) :: NO3uptake
    real(8), intent(out) :: NH4uptake
    real(8), intent(out) :: PO4uptake
# endif
# if defined CARBON_ISOTOPE
    real(8), intent(out) :: DI13Cuptake
# endif

! --- C:N:P ratio of seagrass ---
    real(8), parameter :: nc=27./599.d0 !M.J.Atkinson and SV Smith(1983)
    real(8), parameter :: pc=1./599.d0
! --- Photosynthesis and Calcification Parameters ---
!    real(8), parameter :: pmax =  5.02d0 ! Nakamura & Nakamori 2009
!    real(8), parameter :: pIk  = 1040.5d0
!    real(8), parameter :: p0   =  2.46d0
!    real(8), parameter :: gmax =  4.28d0 ! Nakamura & Nakamori 2009
!    real(8), parameter :: gIk  = 3507.87d0
!    real(8), parameter :: g0   =  0.61d0
    real(8), parameter :: p1 = 3.188d-3 ! Nakamura & Nakamori 2009
    real(8), parameter :: p0 = 2.369d0  !  Model skill = 0.968
    real(8), parameter :: g1 = 1.193d-3 ! Nakamura & Nakamori 2009
    real(8), parameter :: g0 = 0.6267d0 !  Model skill = 0.981
# if defined NUTRIENTS         
    real(8) npref
    real(8) ldocn,ldocd
# endif
# if defined CARBON_ISOTOPE
    real(8), parameter :: a_phot  = -20.0d-3+1.d0  ! -5 to -10 per mill (Dr. Miyajima, pers. comn.)
    real(8), parameter :: a_resp  =  0.0d-3+1.d0  !
    real(8), parameter :: a_calc =   2.7d-3 + 1.0d0 
    real(8), parameter :: a_diss =   0.0d-3 + 1.0d0 
    real(8) R13C_DIC, R13C_QC, R13C_CaCO3
# endif

! --- Organic and Inorganic Production Rate -----------------

!    SEDECO(ng)%Pg(i,j)= pmax*tanh(PFD/pIk)/3600.d0   !Light response curve [mmolC/m2/s]
!    SEDECO(ng)%R (i,j)= p0/3600.d0   !Constant [mmolC/m2/s]
!    SEDECO(ng)%G (i,j)= (gmax*tanh(PFD/gIk)-g0)/3600.d0   !Light response curve [mmolC/m2/s]
    SEDECO(ng)%Pg(i,j)= p1*PFD/3600.d0   !Light response curve [mmolC/m2/s]
    SEDECO(ng)%R (i,j)= p0/3600.d0   !Constant [mmolC/m2/s]
    SEDECO(ng)%G (i,j)= (g1*PFD-g0)/3600.d0   !Light response curve [mmolC/m2/s]
    
    IF(DICamb<=0.d0) THEN !-----For Error handling
      SEDECO(ng)%Pg(i,j) = 0.d0
    ENDIF
    IF(TAamb<=0.d0) THEN !-----For Error handling
      SEDECO(ng)%G (i,j) = 0.d0
    ENDIF
    IF(DOamb<=0.d0) THEN !-----For Error handling
      SEDECO(ng)%R (i,j) = 0.d0
    ENDIF
    
    DICuptake= SEDECO(ng)%Pg(i,j)+SEDECO(ng)%G (i,j)-SEDECO(ng)%R (i,j)
    TAuptake = 2.0d0*SEDECO(ng)%G (i,j)
    DOuptake = SEDECO(ng)%R (i,j)-SEDECO(ng)%Pg(i,j)

!!! ----- Isotope calculation (Not implemented yet!!!) ----------
# if defined CARBON_ISOTOPE
    R13C_DIC  = DI13Camb/DICamb
    R13C_QC = SEDECO(ng)%Q13C(i,j) / SEDECO(ng)%QC(i,j)
    R13C_CaCO3 = R13C_fromd13C(-3.0d0) 
    
    IF(DI13Camb<=0.d0) THEN !-----For Error handling
      R13C_DIC =0.d0
    ENDIF

    DI13Cuptake=  SEDECO(ng)%Pg(i,j)*R13C_DIC*a_phot                   &
                + max(SEDECO(ng)%G (i,j),0.0d0)*R13C_DIC*a_calc        &
                + min(SEDECO(ng)%G (i,j),0.0d0)*R13C_CaCO3*a_diss      &
                - SEDECO(ng)%R (i,j)*R13C_QC*a_resp
# endif
      
! --- Nutrient fluxes between water column and coral -----
# if defined NUTRIENTS         
!!! Under develping
    NO3uptake = 0.0d0
    NH4uptake = 0.0d0
    PO4uptake = 0.0d0
# endif
    RETURN
    
  END SUBROUTINE sedecosys
#endif
END MODULE mod_sedecosys


#else         



    
!!! **********************************************************************
!!!  Sediment model
!!! **********************************************************************



MODULE mod_sedecosys
#if defined SEDIMENT_ECOSYS

  implicit none

! Variables Declaration
  integer, allocatable :: Nsed(:)
  real(8), allocatable :: SedEcoLayerDepths(:, :)   ! (ng, k)          ! depth (cm) from surface at bottom of each biological sediment layer

!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

  TYPE T_SEDECO
! general parameters
    real(8), pointer :: poro(:,:,:)   !! Porosity (cm_w3 cm_s-3)
    ! real(8), pointer :: rhos_d(:,:,:) !! dry soil density (g　cm_s-3)
    ! real(8), pointer :: rhos_w(:,:,:) !! wet soil density (g cm_w-3)
    real(8), pointer :: dz(:)         !! layer thickness (cm)
    real(8), pointer :: z(:)          !! sediment depth  (cm)

    real(8), pointer :: Tmp(:,:,:)    !! Temperature    (ºC)
    real(8), pointer :: Sal(:,:,:)    !! Salinity       (PSU)
    real(8), pointer :: TA (:,:,:)    !! Total Alkanity (umol kg-1)

    real(8), pointer :: O2 (:,:,:)    !! Dissolved Oxygen    (µmol l-1)
    real(8), pointer :: DIC(:,:,:)    !! Carbon dyoxide      (µmol l-1)
    real(8), pointer :: N2 (:,:,:)    !! Nitrogen            (µmol l-1)

! Organic matters
# if defined ORGANIC_MATTER
    real(8), pointer :: DOCf(:,:,:)    !! Dissolved organic carbon   (fast :Labile)       (µmol l-1)
    real(8), pointer :: DOCs(:,:,:)    !! Dissolved organic carbon   (slow :Refractory)   (µmol l-1)
    real(8), pointer :: POCf(:,:,:)    !! Particulate organic carbon (fast :Labile)       (nmol g-1)
    real(8), pointer :: POCs(:,:,:)    !! Particulate organic carbon (slow :Refractory)   (nmol g-1)
    real(8), pointer :: POCn(:,:,:)    !! Particulate organic carbon (non-degradable)     (nmol g-1)
# endif

!  Nutrients dynamics
# if defined NUTRIENTS
    real(8), pointer :: NO3(:,:,:)     !! Nitrate     (µmol l-1)
    real(8), pointer :: NH4(:,:,:)     !! Ammonium    (µmol l-1)
    real(8), pointer :: PO4(:,:,:)     !! Phosphate   (µmol l-1)
#  if defined ORGANIC_MATTER
    real(8), pointer :: DONf(:,:,:)     !! Dissolved organic nitrogen   (fast :Labile)       (µmol l-1)
    real(8), pointer :: DONs(:,:,:)     !! Dissolved organic nitrogen   (slow :Refractory)   (µmol l-1)
    real(8), pointer :: PONf(:,:,:)     !! Particulate organic nitrogen (fast :Labile)       (nmol g-1)
    real(8), pointer :: PONs(:,:,:)     !! Particulate organic nitrogen (slow :Refractory)   (nmol g-1)
    real(8), pointer :: PONn(:,:,:)     !! Particulate organic nitrogen (non-degradable)     (nmol g-1)
    real(8), pointer :: DOPf(:,:,:)     !! Dissolved organic phosphorus   (fast :Labile)       (µmol l-1)
    real(8), pointer :: DOPs(:,:,:)     !! Dissolved organic phosphorus   (slow :Refractory)   (µmol l-1)
    real(8), pointer :: POPf(:,:,:)     !! Particulate organic phosphorus (fast :Labile)       (nmol g-1)
    real(8), pointer :: POPs(:,:,:)     !! Particulate organic phosphorus (slow :Refractory)   (nmol g-1)
    real(8), pointer :: POPn(:,:,:)     !! Particulate organic phosphorus (non-degradable)     (nmol g-1)
#  endif
# endif

! Sulfur dynamics
# if defined SULFATE
    real(8), pointer :: Mn2(:,:,:)    !! Manganese ion        (µmol l-1)
    real(8), pointer :: MnO2(:,:,:)   !! Manganese dioxide    (nmol g-1)

    real(8), pointer :: Fe2(:,:,:)       !! iron(II)          (µmol l-1)
    real(8), pointer :: FeS(:,:,:)       !! iron sulfide      (nmol g-1)
    real(8), pointer :: FeS2(:,:,:)      !! pyrite            (nmol g-1)
    real(8), pointer :: FeOOH(:,:,:)     !! iron hydroxide    (nmol g-1)
    real(8), pointer :: FeOOH_PO4(:,:,:) !! iron-bound phosphate (FeOOH=PO43-)   (nmol g-1)

    real(8), pointer :: H2S(:,:,:)       !! hydrogen sulfide         (µmol l-1)
    real(8), pointer :: SO4(:,:,:)      !! sulfate   (µmol l-1)
    real(8), pointer :: S0(:,:,:)       !! sulfur    (nmol g-1)
# endif

!!! yuta_seagrass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# if defined SEAGRASS
#  if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer :: sg_dDIC_dt(:,:,:,:) ! (umol L-1 s-1) change in DIC due to flux from seagrass (positive means seagrass to sediment)
    real(8), pointer :: sg_dO2_dt (:,:,:,:) ! (umol L-1 s-1) change in DO due to flux from seagrass (positive means seagrass to sediment)
#  endif
#  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer :: sg_dNO3_dt(:,:,:,:) ! (umol L-1 s-1) change in NO3 due to flux from seagrass (positive means seagrass to sediment)
    real(8), pointer :: sg_dNH4_dt(:,:,:,:) ! (umol L-1 s-1) change in NH4 due to flux from seagrass (positive means seagrass to sediment)
    real(8), pointer :: sg_dPO4_dt(:,:,:,:) ! (umol L-1 s-1) change in PO4 due to flux from seagrass (positive means seagrass to sediment)
#  endif
#  if defined SEAGRASS_ROOT_POM
    real(8), pointer :: sg_dPOC_dt(:,:,:,:) ! (umol L-1 s-1) change in POC due to root death in sediment (positive means seagrass to sediment)
    real(8), pointer :: sg_dPON_dt(:,:,:,:) ! (umol L-1 s-1) change in PON due to root death in sediment (positive means seagrass to sediment)
    real(8), pointer :: sg_dPOP_dt(:,:,:,:) ! (umol L-1 s-1) change in POP due to root death in sediment (positive means seagrass to sediment)
#  endif
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

  END TYPE T_SEDECO
  TYPE (T_SEDECO), allocatable :: SEDECO(:)

CONTAINS


!!! **********************************************************************
!!!  Initial conditions for sediment ecosystem model
!!! **********************************************************************

  SUBROUTINE allocate_sedecosys(ng, Ngrids, LBi, UBi, LBj, UBj  &
# if defined SEAGRASS && (defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || \
     defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM)
                                , Nsg                           &
# endif
    )

    USE mod_geochem ! another module in a same folder (function list)
    ! USE mod_readinput
    
    implicit none

    ! input parameters
    integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj
# if defined SEAGRASS && (defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || \
     defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM)
    integer, intent(in) :: Nsg
# endif

    ! allocate sizes of each parameters for (LBi:UBi, LBj:UBj)
    IF (ng.eq.1) allocate( SEDECO(Ngrids) )
    !! General
    allocate( SEDECO(ng)%poro(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%dz  (Nsed(ng)) )
    allocate( SEDECO(ng)%z   (Nsed(ng)) )
    allocate( SEDECO(ng)%Tmp (LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%Sal (LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%TA  (LBi:UBi,LBj:UBj,Nsed(ng)) )
    
    allocate( SEDECO(ng)%O2  (LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%DIC (LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%N2  (LBi:UBi,LBj:UBj,Nsed(ng)) )
    !! Organic Matter(Carbon)
# if defined ORGANIC_MATTER
    allocate( SEDECO(ng)%DOCf(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%DOCs(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%POCf(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%POCs(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%POCn(LBi:UBi,LBj:UBj,Nsed(ng)) )
# endif
    !! Nutrient dynamics
# if defined NUTRIENTS
    allocate( SEDECO(ng)%NO3(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%NH4(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%PO4(LBi:UBi,LBj:UBj,Nsed(ng)) )
#  if defined ORGANIC_MATTER
    allocate( SEDECO(ng)%DONf(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%DONs(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%PONf(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%PONs(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%PONn(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%DOPf(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%DOPs(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%POPf(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%POPs(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%POPn(LBi:UBi,LBj:UBj,Nsed(ng)) )
#  endif
# endif     
    !! Sulfur dynamics
# if defined SULFATE
    allocate( SEDECO(ng)%Mn2(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%MnO2(LBi:UBi,LBj:UBj,Nsed(ng)) )

    allocate( SEDECO(ng)%Fe2(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%FeS(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%FeS2(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%FeOOH(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%FeOOH_PO4(LBi:UBi,LBj:UBj,Nsed(ng)) )

    allocate( SEDECO(ng)%H2S(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%SO4(LBi:UBi,LBj:UBj,Nsed(ng)) )
    allocate( SEDECO(ng)%S0(LBi:UBi,LBj:UBj,Nsed(ng)) )
# endif

!!! yuta_seagrass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# if defined SEAGRASS
#  if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    allocate( SEDECO(ng)%sg_dDIC_dt(Nsg,LBi:UBi,LBj:UBj,Nsed(ng)) ) ! (µmol L-1 s-1) change in DIC due to flux from seagrass (positive means seagrass to sediment)
    allocate( SEDECO(ng)%sg_dO2_dt (Nsg,LBi:UBi,LBj:UBj,Nsed(ng)) ) ! (nmol g-1 s-1) change in DO due to flux from seagrass (positive means seagrass to sediment)
#  endif
#  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    allocate( SEDECO(ng)%sg_dNO3_dt(Nsg,LBi:UBi,LBj:UBj,Nsed(ng)) ) ! (µmol L-1 s-1) change in NO3 due to flux from seagrass (positive means seagrass to sediment)
    allocate( SEDECO(ng)%sg_dNH4_dt(Nsg,LBi:UBi,LBj:UBj,Nsed(ng)) ) ! (µmol L-1 s-1) change in NH4 due to flux from seagrass (positive means seagrass to sediment)
    allocate( SEDECO(ng)%sg_dPO4_dt(Nsg,LBi:UBi,LBj:UBj,Nsed(ng)) ) ! (µmol L-1 s-1) change in PO4 due to flux from seagrass (positive means seagrass to sediment)
#  endif    
#  if defined SEAGRASS_ROOT_POM
    allocate( SEDECO(ng)%sg_dPOC_dt(Nsg,LBi:UBi,LBj:UBj,Nsed(ng)) ) ! (umol L-1 s-1) change in POC due to root death in sediment (positive means seagrass to sediment)
#   if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    allocate( SEDECO(ng)%sg_dPON_dt(Nsg,LBi:UBi,LBj:UBj,Nsed(ng)) ) ! (umol L-1 s-1) change in PON due to root death in sediment (positive means seagrass to sediment)
    allocate( SEDECO(ng)%sg_dPOP_dt(Nsg,LBi:UBi,LBj:UBj,Nsed(ng)) ) ! (umol L-1 s-1) change in POP due to root death in sediment (positive means seagrass to sediment)
#   endif  
#  endif  
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

    RETURN
  END SUBROUTINE allocate_sedecosys


!!! **********************************************************************
!!!  Initial conditions for sediment ecosystem model
!!! **********************************************************************

  SUBROUTINE initialize_sedecosys(ng, LBi, UBi, LBj, UBj, start_of_new_run)
    USE mod_geochem ! another module in a same folder (function list)
    ! USE mod_readinput
    
    implicit none

    ! input parameters
    integer, intent(in) :: ng, LBi, UBi, LBj, UBj
    logical, intent(in) :: start_of_new_run               ! TRUE = initialize coral, seagass, macroalgae, sediment from start; FALSE = continue from previous run

    integer i,j,k ! symbols for loop calculations
    !! depth : Diffusive Boundary Layer (DBL)
    real(8), parameter :: dbl = 0.03d0 ! DBL:difussive boundary layer (cm)
    !! porosity   ( poro = a + b * exp(c * z)   (fossing et al 2004) )
    !! Kasih 2009
    ! real(8), parameter :: coefa_poro = 0.88d0
    ! real(8), parameter :: coefa_poro = 0.90d0
    ! real(8), parameter :: coefb_poro = 0.04d0
    ! real(8), parameter :: coefc_poro = -0.4d0
    !! Fossing 2004
    real(8), parameter :: coefa_poro = 0.763d0
    real(8), parameter :: coefb_poro = 0.086d0
    real(8), parameter :: coefc_poro = -0.216d0

    real(8), parameter :: rCPph = 106.0d0/1.0d0    ! (no dim.) algal C:P ratio (Redfield ratio)
    real(8), parameter :: rCNph = 11.0d0   ! (no dim.) algal C:N ratio (Redfield ratio)        from analysis

! - - - - - - - - - - - - - - - - - - - - - - - -
    !  Set initial conditions

    !!! layer thickness and depth
    ! SEDECO(ng)%dz(1) = 0.03d0      ! thickness of surface layer (cm)
    ! SEDECO(ng)%z(1)  = 0.03d0      ! depth at bottom surface (cm)

    ! do k = 2,41                                    ! dz is constantly 0.03 until z=1.2cm
    !     SEDECO(ng)%dz(k) = SEDECO(ng)%dz(1);
    !     SEDECO(ng)%z(k)  = SEDECO(ng)%z(k-1) + SEDECO(ng)%dz(k)
    ! enddo
    ! do k = 42,Nsed(ng)                               ! dz exponentially increases until z=20cm (Nsed=100)
    !     SEDECO(ng)%dz(k) = SEDECO(ng)%dz(2) * exp(0.06265 * (k -41))   ! 0.06265 is a tuned constant
    !     SEDECO(ng)%z(k)  = SEDECO(ng)%z(k-1) + SEDECO(ng)%dz(k)
    ! enddo

    do k = 1, Nsed(ng)
      SEDECO(ng)%z(k) = SedEcoLayerDepths(ng, k)
    enddo
    SEDECO(ng)%dz(1) = SedEcoLayerDepths(ng, 1)
    do k = 2, Nsed(ng)
      SEDECO(ng)%dz(k) = SedEcoLayerDepths(ng, k) - SedEcoLayerDepths(ng, k-1)
    enddo

    !!! porosity
    ! SEDECO(ng)%poro(:,:,1) = 1.0d0
    do k = 1,Nsed(ng)
        ! SEDECO(ng)%poro(:,:,k) = coefa_poro + coefb_poro * exp(coefc_poro * SEDECO(ng)%z(k))
        !! --- Shizugawa --- !!
        ! SEDECO(ng)%poro(:,:,k) = 0.6007d0 - 0.006d0 * log(SEDECO(ng)%z(k))
        !! --- Shiraho --- !!
        SEDECO(ng)%poro(:,:,k) = 0.6007d0 - 0.006d0 * log(SEDECO(ng)%z(k))
        !! --- Fukido --- !!
        ! SEDECO(ng)%poro(:,:,k) = 0.6007d0 - 0.006d0 * log(SEDECO(ng)%z(k))
    enddo

    if ( start_of_new_run ) then
    !!! ------------------- For determinating initial concentration ----------------------- !!!
        ! General
        SEDECO(ng)%Tmp(:,:,:)  = 20.0d0     
        SEDECO(ng)%Sal(:,:,:)  = 0.0d0
        SEDECO(ng)%TA(:,:,:)  = 2232.0d0 ! TA (umol kg-1)

        SEDECO(ng)%O2 (:,:,:)  = 0.0d0        !DO  (µmol L-1)
        SEDECO(ng)%DIC(:,:,:)  = 2000.0d0        !DIC  (µmol L-1)
        SEDECO(ng)%N2 (:,:,:)  = 0.0d0        !N2  (µmol L-1)
        ! OM
#  if defined ORGANIC_MATTER
        SEDECO(ng)%DOCf(:,:,:)  = 0.0d0       !DOCf  (µmol L-1) 
        SEDECO(ng)%DOCs(:,:,:)  = 2000.0d0       !DOCs  (µmol L-1) 
        SEDECO(ng)%POCf(:,:,:)  = 0.0d0       !POCf  (nmol g-1) 
        ! ! SEDECO(ng)%POCf(:,:,:)  = 1.0d6      !POCf  (nmol g-1)          2.0d6
        ! ! SEDECO(ng)%POCf(:,:,:)  = 3.0d6      !POCf  (nmol g-1)
        ! ! SEDECO(ng)%POCs(:,:,:)  = 1.1d6      !DOCf  (µmol L-1) 
        ! ! SEDECO(ng)%POCs(:,:,:)  = 1.0d6      !POCs  (nmol g-1)          1.0d6
        ! ! SEDECO(ng)%POCs(:,:,:)  = 8.0d5      !POCs  (nmol g-1)
        ! SEDECO(ng)%POCs(:,:,:)  = 8.5d5       !POCs  (nmol g-1)
        ! ! SEDECO(ng)%POCn(:,:,:)  = 1.0d6       !DOCf  (µmol L-1)
        ! SEDECO(ng)%POCn(:,:,:)  = 8.5d5      !DOCf  (µmol L-1)
        ! ! SEDECO(ng)%POCn(:,:,:)  = 4.0d5      !DOCf  (µmol L-1)
        SEDECO(ng)%POCs(:,:,:)  = 1000.0d0      !POCs  (nmol g-1)
        SEDECO(ng)%POCn(:,:,:)  = 1000.0d0      !POCn  (nmol g-1)
#  endif
        ! Nutrient dynamics
#  if defined NUTRIENTS
        SEDECO(ng)%NO3(:,:,:) = 5.0d0        !NO3  (µmol L-1) 
        SEDECO(ng)%NH4(:,:,:) = 50.0d0        !NH4  (µmol L-1) 
        SEDECO(ng)%PO4(:,:,:) = 10.0d0        !PO4  (µmol L-1) 
#  if defined ORGANIC_MATTER                 
        SEDECO(ng)%DONf(:,:,:) = SEDECO(ng)%DOCf(:,:,:) / rCNph   !DON  (µmol L-1) 
        SEDECO(ng)%DONs(:,:,:) = SEDECO(ng)%DOCs(:,:,:) / rCNph   !DON  (µmol L-1) 
        SEDECO(ng)%PONf(:,:,:) = SEDECO(ng)%POCf(:,:,:) / rCNph   !PON  (µmol L-1) 
        SEDECO(ng)%PONs(:,:,:) = SEDECO(ng)%POCs(:,:,:) / rCNph   !PON  (µmol L-1) 
        SEDECO(ng)%PONn(:,:,:) = SEDECO(ng)%POCn(:,:,:) / rCNph   !PON  (µmol L-1) 
        SEDECO(ng)%DOPf(:,:,:) = SEDECO(ng)%DOCf(:,:,:) / rCPph   !DOP  (µmol L-1) 
        SEDECO(ng)%DOPs(:,:,:) = SEDECO(ng)%DOCs(:,:,:) / rCPph   !DOP  (µmol L-1) 
        SEDECO(ng)%POPf(:,:,:) = SEDECO(ng)%POCf(:,:,:) / rCPph   !POP  (µmol L-1) 
        SEDECO(ng)%POPs(:,:,:) = SEDECO(ng)%POCs(:,:,:) / rCPph   !POP  (µmol L-1) 
        SEDECO(ng)%POPn(:,:,:) = SEDECO(ng)%POCn(:,:,:) / rCPph   !POP  (µmol L-1) 
#  endif
#  endif
        ! Sulfur dynamics
#  if defined SULFATE
        SEDECO(ng)%Mn2(:,:,:) = 60.0d0    !Mn2  (µmol L-1) 
        SEDECO(ng)%MnO2(:,:,1:10) = 1.0d4   !MnO2  (nmol g-1) 
        SEDECO(ng)%MnO2(:,:,11:Nsed(ng)) = 0.0d0   !MnO2  (nmol g-1) 

        SEDECO(ng)%Fe2(:,:,:) = 0.0d0          !Fe2  (µmol L-1) 
        SEDECO(ng)%FeS(:,:,:) = 0.0d0          !FeS  (nmol g-1) 
        SEDECO(ng)%FeS2(:,:,:) = 1.0d4         !FeS2  (nmol g-1) 
        SEDECO(ng)%FeOOH(:,:,1:10) = 1.0d5        !FeOOH  (nmol g-1)
        SEDECO(ng)%FeOOH(:,:,11:Nsed(ng)) = 0.0d0        !FeOOH  (nmol g-1)
        SEDECO(ng)%FeOOH_PO4(:,:,:) = 0.0d0    !FeOOH=PO4  (nmol g-1) 

        SEDECO(ng)%H2S(:,:,:) = 100.0d0     !H2S  (µmol L-1)
        SEDECO(ng)%SO4(:,:,:) = 28.0d3     !SO4  (µmol L-1)
        SEDECO(ng)%S0(:,:,:)  = 0.0d0     !S0  (nmol g-1)
#  endif

!     !!! -------------- initial concentration from sed initial concentration ----------------- !!!
!         ! ! General
!         open(199, file='input/sedinitial_DO.txt', status='old')
!         read(199,*) SEDECO(ng)%O2(:,:,:)
!         close(199)

!         open(200, file='input/sedinitial_DOCf.txt', status='old')
!         read(200,*) SEDECO(ng)%DOCf(:,:,:)
!         close(200)

!         open(201, file='input/sedinitial_DOCs.txt', status='old')
!         read(201,*) SEDECO(ng)%DOCs(:,:,:)
!         close(201)

!         open(202, file='input/sedinitial_POCf.txt', status='old')
!         read(202,*) SEDECO(ng)%POCf(:,:,:)
!         close(202)

!         open(203, file='input/sedinitial_POCs.txt', status='old')
!         read(203,*) SEDECO(ng)%POCs(:,:,:)
!         close(203)

!         open(204, file='input/sedinitial_POCn.txt', status='old')
!         read(204,*) SEDECO(ng)%POCn(:,:,:)
!         close(204)

!         open(205, file='input/sedinitial_NO3.txt', status='old')
!         read(205,*) SEDECO(ng)%NO3(:,:,:)
!         close(205)

!         open(206, file='input/sedinitial_NH4.txt', status='old')
!         read(206,*) SEDECO(ng)%NH4(:,:,:)
!         close(206)

!         open(207, file='input/sedinitial_PO4.txt', status='old')
!         read(207,*) SEDECO(ng)%PO4(:,:,:)
!         close(207)
        
!         open(208, file='input/sedinitial_Mn2.txt', status='old')
!         read(208,*) SEDECO(ng)%Mn2(:,:,:)
!         close(208)

!         open(209, file='input/sedinitial_MnO2.txt', status='old')
!         read(209,*) SEDECO(ng)%MnO2(:,:,:)
!         close(209)

!         open(210, file='input/sedinitial_Fe2.txt', status='old')
!         read(210,*) SEDECO(ng)%Fe2(:,:,:)
!         close(210)

!         open(211, file='input/sedinitial_FeOOH.txt', status='old')
!         read(211,*) SEDECO(ng)%FeOOH(:,:,:)
!         close(211)

!         open(212, file='input/sedinitial_FeS.txt', status='old')
!         read(212,*) SEDECO(ng)%FeS(:,:,:)
!         close(212)

!         open(213, file='input/sedinitial_FeS2.txt', status='old')
!         read(213,*) SEDECO(ng)%FeS2(:,:,:)
!         close(213)

!         open(214, file='input/sedinitial_FeOOH_PO4.txt', status='old')
!         read(214,*) SEDECO(ng)%FeOOH_PO4(:,:,:)
!         close(214)

!         open(215, file='input/sedinitial_H2S.txt', status='old')
!         read(215,*) SEDECO(ng)%H2S(:,:,:)
!         close(215)

!         open(216, file='input/sedinitial_S0.txt', status='old')
!         read(216,*) SEDECO(ng)%S0(:,:,:)
!         close(216)

!         open(217, file='input/sedinitial_SO4.txt', status='old')
!         read(217,*) SEDECO(ng)%SO4(:,:,:)
!         close(217)

    endif

    !! for debug
    ! write(*,*) SEDECO(ng)%SO4(1,1,:)


    RETURN

  END SUBROUTINE initialize_sedecosys
 ! ****************************************************************** !     



!!! **********************************************************************
!!!  Main program of sediment ecosystem model
!!! **********************************************************************
! amb:ambient = bottom grid of water column = Just above seafloor
! amb will be set as value(1) in mod_reef_ecosys.F

  SUBROUTINE sedecosys           &
!   input parameters
    ( ng, i, j       &   ! ng: nested grid number; i,j: position
    , Nsed           &   ! Number of biological sediment layers ! yuta_edits_for_masa
    , dt             &   ! Time step (sec)
    , Tamb           &   ! Tmp: Temperature (oC)
    , Samb           &   ! Sal: Salinity (PSU)
    , TAamb          &   ! TA: Total Alkalinity (kg L-1)

    , O2amb          &   ! O2: Dissolved oxygen (umol L-1)
    , DICamb         &   ! DIC: carbon dioxide  (µmol L-1)
    , N2amb          &   ! N2: Nitrogen         (µmol L-1)
# if defined ORGANIC_MATTER
    , POCamb         &   ! POC: Particulate organic carbon (nmol g-1)
# endif
# if defined NUTRIENTS
    , NO3amb         &   ! NO3: NO3 (umol L-1)
    , NH4amb         &   ! NH4: NH4 (umol L-1)
    , PO4amb         &   ! PO4: PO4 (umol L-1)
#  if defined ORGANIC_MATTER
    , PONamb         &   ! PON: Particulate organic nitrogen (PON: umol L-1)
    , POPamb         &   ! POP: Particulate organic phosporius (POP: umol L-1)
#  endif
# endif
# if defined SULFATE
    , Mn2amb         &  ! Mn2 (µmol L-1)
    , Fe2amb         &  ! Fe2 (µmol L-1)
    , H2Samb         &  ! H2S (µmol L-1)
    , SO4amb         &  ! SO4 (µmol L-1)
    , Fdep_MnO2      &  ! MnO2 deposition flux (nmol cm-2 s-1)
    , Fdep_FeOOH     &  ! FeOOH depositoin flux (nmol cm-2 s-1)
# endif
# if defined ORGANIC_MATTER
    , Fdep_POC       &   ! POC deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#  if defined NUTRIENTS
    , Fdep_PON       &   ! PON deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Fdep_POP       &   ! POP deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#  endif
# endif
# if defined SEDECO_ADVECTION
    , sgd_flux          &   ! sumbarine groundwater discharge rate (cm s-1)  This flux is assumed to be the same throughout the sediment ecosystem layers (no compression/expansion) So volume flux doesn't change but concentrations of tracers do
    , sgd_Tmp_in        &   ! SGD concentration coming in through bottom: Tmp: Temperature (oC)
    , sgd_Sal_in        &   ! SGD concentration coming in through bottom: Sal: Salinity (PSU)
    , sgd_DIC_in        &   ! SGD concentration coming in through bottom: DIC: Total dissolved inorganic carbon (umol L-1)
    , sgd_TA_in         &   ! SGD concentration coming in through bottom: TA : Total alkalinity (umol L-1)
    , sgd_DOx_in        &   ! SGD concentration coming in through bottom: DOx: Dissolved oxygen (umol L-1)
#  if defined CARBON_ISOTOPE
    , sgd_DI13C_in      &   ! SGD concentration coming in through bottom: DI13C : 13C of DIC (umol kg-1)
#  endif
#  if defined NUTRIENTS            
    , sgd_NO3_in        &   ! SGD concentration coming in through bottom: NO3: NO3 (umol L-1)
    , sgd_NH4_in        &   ! SGD concentration coming in through bottom: NH4: NH4 (umol L-1)
    , sgd_PO4_in        &   ! SGD concentration coming in through bottom: PO4: PO4 (umol L-1)
#   if defined NITROGEN_ISOTOPE
    , sgd_NO3_15N_in    &   ! SGD concentration coming in through bottom: NO3_15N: 15N of NO3 (umol L-1)
    , sgd_NH4_15N_in    &   ! SGD concentration coming in through bottom: NH4_15N: 15N of NH4 (umol L-1)
#   endif
#  endif
# endif
!   output parameters
    , Flux_Tmp       &   ! Heat (temperature) flux (oC cm s-1)
    , Flux_Sal       &   ! Salinity flux (cm s-1)  * direction of water column to sediment is positive
    , Flux_TA        &   ! Total Alkalinity flux (???? cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_DO        &   ! DO  flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_DIC       &   ! DIC flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_N2        &   ! N2  flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
# if defined ORGANIC_MATTER
    , Flux_POC       &   ! POC(f,s,n) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
# endif
# if defined NUTRIENTS
    , Flux_NO3       &   ! NO3 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_NH4       &   ! NH4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_PO4       &   ! PO4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#  if defined ORGANIC_MATTER
    , Flux_PON       &   ! PON(f,s,n) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_POP       &   ! POP(f,s,n) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
#  endif
# endif
# if defined SULFATE
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
# endif
    )

    USE mod_geochem
    
    implicit none

! ***********************************************************************************
!!! Declarative statement
!! These variable names are different from those in mod_reef_ecosys.F

    !! General
    integer, intent(in) :: ng, i, j, Nsed
    real(8), intent(in) :: dt
    real(8), intent(in) :: Tamb     
    real(8), intent(in) :: Samb     
    real(8), intent(in) :: TAamb     

    real(8), intent(in) :: O2amb
    real(8), intent(in) :: DICamb
    real(8), intent(in) :: N2amb
    !! Organic matters (Carbon)
# if defined ORGANIC_MATTER
    real(8), intent(in) :: POCamb
# endif
    !! Nutrient
# if defined NUTRIENTS
    real(8), intent(in) :: NO3amb   
    real(8), intent(in) :: NH4amb   
    real(8), intent(in) :: PO4amb   
#  if defined ORGANIC_MATTER
    real(8), intent(in) :: PONamb
    real(8), intent(in) :: POPamb
#  endif
# endif
    !! Sulfate
# if defined SULFATE
    real(8), intent(in) :: Mn2amb
    real(8), intent(in) :: Fe2amb
    real(8), intent(in) :: H2Samb
    real(8), intent(in) :: SO4amb 
    !! Deposition Flux (test) Kasih 2009 -------- 
    real(8), intent(in) :: Fdep_MnO2
    real(8), intent(in) :: Fdep_FeOOH
# endif

# if defined ORGANIC_MATTER
    real(8), intent(in) :: Fdep_POC   
#  if defined NUTRIENTS
    real(8), intent(in) :: Fdep_PON   
    real(8), intent(in) :: Fdep_POP   
#  endif
# endif

# if defined SEDECO_ADVECTION
    real(8), intent(in) :: sgd_flux           ! sumbarine groundwater discharge rate (cm s-1)  This flux is assumed to be the same throughout the sediment ecosystem layers (no compression/expansion) So volume flux doesn't change but concentrations of tracers do
    real(8)             :: AdvecVelo          ! advection velocity
    real(8), intent(in) :: sgd_Tmp_in         ! SGD concentration coming in through bottom: Tmp: Temperature (oC)
    real(8), intent(in) :: sgd_Sal_in         ! SGD concentration coming in through bottom: Sal: Salinity (PSU)
    real(8), intent(in) :: sgd_DIC_in         ! SGD concentration coming in through bottom: DIC: Total dissolved inorganic carbon (umol L-1)
    real(8), intent(in) :: sgd_TA_in          ! SGD concentration coming in through bottom: TA : Total alkalinity (umol L-1)
    real(8), intent(in) :: sgd_DOx_in         ! SGD concentration coming in through bottom: DOx: Dissolved oxygen (umol L-1)
#  if defined CARBON_ISOTOPE
    real(8), intent(in) :: sgd_DI13C_in       ! SGD concentration coming in through bottom: DI13C : 13C of DIC (umol L-1)
#  endif
#  if defined NUTRIENTS            
    real(8), intent(in) :: sgd_NO3_in         ! SGD concentration coming in through bottom: NO3: NO3 (umol L-1)
    real(8), intent(in) :: sgd_NH4_in         ! SGD concentration coming in through bottom: NH4: NH4 (umol L-1)
    real(8), intent(in) :: sgd_PO4_in         ! SGD concentration coming in through bottom: PO4: PO4 (umol L-1)
#   if defined NITROGEN_ISOTOPE
    real(8), intent(in) :: sgd_NO3_15N_in     ! SGD concentration coming in through bottom: NO3_15N: 15N of NO3 (umol L-1)
    real(8), intent(in) :: sgd_NH4_15N_in     ! SGD concentration coming in through bottom: NH4_15N: 15N of NH4 (umol L-1)
#   endif
#  endif
# endif

!! output parameters (Flux)
    real(8), intent(out) :: Flux_Tmp
    real(8), intent(out) :: Flux_Sal
    real(8), intent(out) :: Flux_TA

    real(8), intent(out) :: Flux_DO
    real(8), intent(out) :: Flux_DIC
    real(8), intent(out) :: Flux_N2 
# if defined ORGANIC_MATTER
    real(8), intent(out) :: Flux_POC(3)
# endif
# if defined NUTRIENTS
    real(8), intent(out) :: Flux_NO3
    real(8), intent(out) :: Flux_NH4
    real(8), intent(out) :: Flux_PO4
#  if defined ORGANIC_MATTER
    real(8), intent(out) :: Flux_PON(3)
    real(8), intent(out) :: Flux_POP(3)
#  endif
# endif
# if defined SULFATE
    real(8), intent(out) :: Flux_Mn2
    real(8), intent(out) :: Flux_MnO2

    real(8), intent(out) :: Flux_Fe2
    ! real(8), intent(out) :: Flux_FeS
    ! real(8), intent(out) :: Flux_FeS2
    real(8), intent(out) :: Flux_FeOOH
    ! real(8), intent(out) :: Flux_FeOOH_PO4

    real(8), intent(out) :: Flux_H2S
    ! real(8), intent(out) :: Flux_S0
    real(8), intent(out) :: Flux_SO4
# endif

!!! Set parameters 
    integer :: k       ! symbol for loop cal

!! -- Physical properties of sediment ------------------------------
    real(8) :: z(Nsed)         ! depth(cm)
    real(8) :: dz(Nsed)        ! layer thickness(cm)
    real(8), parameter :: dbl = 0.03d0 ! DBL:difussive boundary layer (cm) ! yt_debug make this user assignable?
    real(8) :: rho_sw(Nsed)    ! seawater density (g cm-3)
    real(8) :: rhos_d(Nsed)      ! soil density (g cm_s-3)

    
!! -- Diffusion coefficient -----------------------------------------
    !! General diffusion in pure seawater  (cm2 s-1)
    real(8) :: Dw_Tmp(Nsed),Dw_Sal(Nsed)
    real(8) :: Dw_DOM(Nsed,2)    ! 1:labile, 2:refractory
    real(8) :: Dw_O2(Nsed),Dw_DIC(Nsed),Dw_TA(Nsed),Dw_N2(Nsed)
# if defined NUTRIENTS
    real(8) :: Dw_NO3(Nsed), Dw_NH4(Nsed), Dw_PO4(Nsed)
# endif
# if defined SULFATE
    real(8) :: Dw_Mn2(Nsed), Dw_Fe2(Nsed), Dw_SO4(Nsed), Dw_H2S(Nsed)
# endif
    !! Molecular Diffusion coefficients (cm2 s-1)
    real(8) :: Ds_Tmp(Nsed),Ds_Sal(Nsed)
    real(8) :: Ds_DOM(2:Nsed,2)    ! 1:labile, 2:refractory
    real(8) :: Ds_O2(2:Nsed),Ds_DIC(2:Nsed),Ds_TA(2:Nsed),Ds_N2(2:Nsed)
# if defined NUTRIENTS
    real(8) :: Ds_NO3(2:Nsed), Ds_NH4(2:Nsed), Ds_PO4(2:Nsed)
! #  if defined ORGANIC_MATTER
!     real(8) :: Ds_DON(2:Nsed), Ds_DOP(2:Nsed)
! #  endif
# endif
# if defined SULFATE
    real(8) :: Ds_Mn2(2:Nsed), Ds_Fe2(2:Nsed), Ds_H2S(2:Nsed), Ds_SO4(2:Nsed)
# endif
    !! Biodifuusivity　(cm2 s-1)
    real(8) :: DBd(Nsed), DBp(Nsed)    !(cm2 s-1)

    !! Adsorption constant K (cm3 g-1)
    real(8),parameter :: Kad_NO3 = 5.4d0   !(cm3 g-1)
    real(8),parameter :: Kad_NH4 = 2.2d0   !(cm3 g-1)
    real(8),parameter :: Kad_PO4 = 2.0d0   !(cm3 g-1)
    real(8),parameter :: Kad_Mn2 = 13.0d0  !(cm3 g-1)
    real(8),parameter :: Kad_Fe2 = 500.0d0 !(cm3 g-1)

    !! Sedimentation rate (cm/sec)
# if defined SEDECO_BURIAL
    ! real(8),parameter :: u = 0.064d0 / (365d0*24d0*60d0*60d0) ! (cm/sec) ! Fossing 2004
    ! real(8),parameter :: u = 0.38d0 / (365d0*24d0*60d0*60d0) ! (cm/sec) ! Irie 2018
    real(8),parameter :: u = 0.5d0 / (365d0*24d0*60d0*60d0) ! (cm/sec) ! Mochida 2023
# endif
    

    !! Diffusion Flux  ----------------------------------------------
       !(Fd :: molecular diffusion flux, Fb :: biodiffusive flux)
       !! unit: (nmol cm-2 s-1)
    real(8) :: Fd_Tmp(Nsed), Fb_Tmp(2:Nsed)
    real(8) :: Fd_Sal(Nsed), Fb_sal(2:Nsed)
    real(8) :: Fd_O2 (Nsed), Fb_O2 (2:Nsed)
    real(8) :: Fd_TA (Nsed), Fb_TA (2:Nsed)
    real(8) :: Fd_DIC(Nsed), Fb_DIC(2:Nsed)
    real(8) :: Fd_N2 (Nsed), Fb_N2 (2:Nsed)
# if defined ORGANIC_MATTER
    real(8) :: Fd_DOC(2:Nsed,2),Fb_DOC(2:Nsed,2)    ! 1:labile, 2:refractory
    real(8) ::                  Fb_POC(2:Nsed,3)    ! 1:labile, 2:refractory, 3:non-degratable
# endif
# if defined NUTRIENTS
    real(8) :: Fd_NO3(Nsed), Fb_NO3(2:Nsed)
    real(8) :: Fd_NH4(Nsed), Fb_NH4(2:Nsed)
    real(8) :: Fd_PO4(Nsed), Fb_PO4(2:Nsed)
#  if defined ORGANIC_MATTER
    real(8) :: Fd_DON(2:Nsed,2), Fb_DON(2:Nsed,2)    ! 1:labile, 2:refractory
    real(8) ::                   Fb_PON(2:Nsed,3)    ! 1:labile, 2:refractory, 3:non-degratable
    real(8) :: Fd_DOP(2:Nsed,2), Fb_DOP(2:Nsed,2)    ! 1:labile, 2:refractory
    real(8) ::                   Fb_POP(2:Nsed,3)    ! 1:labile, 2:refractory, 3:non-degratable
#  endif
# endif
# if defined SULFATE
    real(8) :: Fd_Mn2(Nsed), Fb_Mn2 (2:Nsed)
    real(8) ::               Fb_MnO2(2:Nsed)
    real(8) :: Fd_Fe2(Nsed), Fb_Fe2 (2:Nsed)
    real(8) ::               Fb_FeS (2:Nsed)
    real(8) ::               Fb_FeS2(2:Nsed)
    real(8) ::               Fb_FeOOH(2:Nsed)
    real(8) ::               Fb_FeOOH_PO4(2:Nsed)
    real(8) :: Fd_H2S(Nsed), Fb_H2S (2:Nsed)
    real(8) ::               Fb_S0  (2:Nsed)
    real(8) :: Fd_SO4(Nsed), Fb_SO4 (2:Nsed)
# endif

    !! Brial Flux (Fc) ----------------------------------- 
    !! unit: (nmol cm-2 s-1)
# if defined SEDECO_BURIAL
    real(8) :: Fc_Tmp(2:Nsed+1)
    real(8) :: Fc_sal(2:Nsed+1)
    real(8) :: Fc_O2 (2:Nsed+1)
    real(8) :: Fc_TA (2:Nsed+1)
    real(8) :: Fc_DIC(2:Nsed+1)
    real(8) :: Fc_N2 (2:Nsed+1)
#  if defined ORGANIC_MATTER
    real(8) :: Fc_DOC(2:Nsed+1,2)    ! 1:labile, 2:refractory
    real(8) :: Fc_POC(2:Nsed+1,3)    ! 1:labile, 2:refractory 3:non-degratable
#  endif
#  if defined NUTRIENTS
    real(8) :: Fc_NO3(2:Nsed+1)
    real(8) :: Fc_NH4(2:Nsed+1)
    real(8) :: Fc_PO4(2:Nsed+1)
#   if defined ORGANIC_MATTER
    real(8) :: Fc_DON(2:Nsed+1,2)    ! 1:labile, 2:refractory
    real(8) :: Fc_PON(2:Nsed+1,3)    ! 1:labile, 2:refractory 3:non-degratable
    real(8) :: Fc_DOP(2:Nsed+1,2)    ! 1:labile, 2:refractory
    real(8) :: Fc_POP(2:Nsed+1,3)    ! 1:labile, 2:refractory 3:non-degratable
#   endif
#  endif
#  if defined SULFATE
    real(8) :: Fc_Mn2 (2:Nsed+1)
    real(8) :: Fc_MnO2(2:Nsed+1)
    real(8) :: Fc_Fe2 (2:Nsed+1)
    real(8) :: Fc_FeS (2:Nsed+1)
    real(8) :: Fc_FeS2(2:Nsed+1)
    real(8) :: Fc_FeOOH(2:Nsed+1)
    real(8) :: Fc_FeOOH_PO4(2:Nsed+1)
    real(8) :: Fc_H2S (2:Nsed+1)
    real(8) :: Fc_S0  (2:Nsed+1)
    real(8) :: Fc_SO4 (2:Nsed+1)
#  endif
# endif

    !! Advection Flux (Fc) ----------------------------------- 
    !! unit: (nmol cm-2 s-1)
# if defined SEDECO_ADVECTION && defined SGD_ON
    real(8) :: Fa_Tmp(2:Nsed+1)
    real(8) :: Fa_sal(2:Nsed+1)
    real(8) :: Fa_O2 (1:Nsed+1)
    real(8) :: Fa_TA (1:Nsed+1)
    real(8) :: Fa_DIC(1:Nsed+1)
    real(8) :: Fa_N2 (1:Nsed+1)
#  if defined ORGANIC_MATTER
    real(8) :: Fa_DOC(1:Nsed+1,2)    ! 1:labile, 2:refractory
    real(8) :: Fa_POC(1:Nsed+1,3)    ! 1:labile, 2:refractory 3:non-degratable
#  endif
#  if defined NUTRIENTS
    real(8) :: Fa_NO3(1:Nsed+1)
    real(8) :: Fa_NH4(1:Nsed+1)
    real(8) :: Fa_PO4(1:Nsed+1)
#   if defined ORGANIC_MATTER
    real(8) :: Fa_DON(1:Nsed+1,2)    ! 1:labile, 2:refractory
    real(8) :: Fa_PON(1:Nsed+1,3)    ! 1:labile, 2:refractory 3:non-degratable
    real(8) :: Fa_DOP(1:Nsed+1,2)    ! 1:labile, 2:refractory
    real(8) :: Fa_POP(1:Nsed+1,3)    ! 1:labile, 2:refractory 3:non-degratable
#   endif
#  endif
#  if defined SULFATE
    real(8) :: Fa_Mn2 (1:Nsed+1)
    real(8) :: Fa_MnO2(1:Nsed+1)
    real(8) :: Fa_Fe2 (1:Nsed+1)
    real(8) :: Fa_FeS (1:Nsed+1)
    real(8) :: Fa_FeS2(1:Nsed+1)
    real(8) :: Fa_FeOOH(1:Nsed+1)
    real(8) :: Fa_FeOOH_PO4(1:Nsed+1)
    real(8) :: Fa_H2S (1:Nsed+1)
    real(8) :: Fa_S0  (1:Nsed+1)
    real(8) :: Fa_SO4 (1:Nsed+1)
#  endif
# endif

    !! Total Flux (:: FT) ---------------------------------- 
    !! unit: (nmol cm-2 s-1)
    real(8) :: FT_Tmp(Nsed+1)
    real(8) :: FT_Sal(Nsed+1)
    real(8) :: FT_O2 (Nsed+1)
    real(8) :: FT_TA (Nsed+1)
    real(8) :: FT_DIC(Nsed+1)
    real(8) :: FT_N2 (Nsed+1)
# if defined ORGANIC_MATTER
    real(8) :: FT_DOC(Nsed+1,2)    ! 1:labile, 2:refractory
    real(8) :: FT_POC(Nsed+1,3)    ! 1:labile, 2:refractory 3:non-degratable
# endif
# if defined NUTRIENTS
    real(8) :: FT_NO3(Nsed+1)
    real(8) :: FT_NH4(Nsed+1)
    real(8) :: FT_PO4(Nsed+1)
#  if defined ORGANIC_MATTER
    real(8) :: FT_DON(Nsed+1,2)    ! 1:labile, 2:refractory
    real(8) :: FT_PON(Nsed+1,3)    ! 1:labile, 2:refractory 3:non-degratable
    real(8) :: FT_DOP(Nsed+1,2)    ! 1:labile, 2:refractory
    real(8) :: FT_POP(Nsed+1,3)    ! 1:labile, 2:refractory 3:non-degratable
#  endif
# endif
# if defined SULFATE
    real(8) :: FT_Mn2 (Nsed+1)
    real(8) :: FT_MnO2(Nsed+1)
    real(8) :: FT_Fe2 (Nsed+1)
    real(8) :: FT_FeS (Nsed+1)
    real(8) :: FT_FeS2(Nsed+1)
    real(8) :: FT_FeOOH(Nsed+1)
    real(8) :: FT_FeOOH_PO4(Nsed+1)
    real(8) :: FT_H2S (Nsed+1)
    real(8) :: FT_S0  (Nsed+1)
    real(8) :: FT_SO4 (Nsed+1)
# endif

!! -- Reaction parameters -----------------------------------------
!!!! ============================================================== !!!
    !! ORGANIC MATTER DECOMPOSITION
        ! POMf --> DOMf + DOMs
        ! POMs --> DOMf + DOMs
    !! PRIMARY REACTIONS  (CH2O :: Organic Matters(DOMf,s))
        ! R1:   O2 + CH2O --> CO2 + H2O
        ! R2:   4NO3- + 5CH2O + 4H+ --> N2 + 5CO2 + 7H2O
        ! R3:   2MnO2 + CH2O + 4H+ --> 2Mn2+ + CO2 + 3H2O
        ! R4:   4FeOOH + CH2O + 4H+ --> 4Fe2+ + CO2 + 7H2O
        ! R5:   SO42- + 2CH2O + 2H+ --> H2S + 2CO2 + 2H2O
    !! SECONDARY REACTIONS
        ! R6:   NH4+ + 2O2 --> NO3- + H2O + 2H+
        ! R7:   FeOOH + PO43- --> FeOOH=PO43-
        ! R8:   2Fe2+ + MnO2 + 2H2O --> 2FeOOH + Mn2+ + 2H+
        ! R9:   2Mn2+ + O2 + 2H2O --> 2MnO2 + 4H+
        ! R10a: H2S + 2FeOOH=PO43- + 4H+ --> S0 + 2Fe2+ + 4H2O + 2PO43-
        ! R10b: H2S + 2FeOOH + 4H+ --> S0 + 2Fe2+ + 4H2O
        ! R11:  4Fe2+ + O2 + 6H2O --> 4FeOOH + 8H+
        ! R12:  H2S + MnO2 + 4H+ --> S0 + Mn2+ + 2H2O
        ! R13:  H2S + Fe2+ --> FeS + 2H+
        ! R14:  FeS + S0 --> FeS2
        ! R15:  SO42- + 3H2S + 4FeS + 2H+ --> 4FeS2 + 4H2O
        ! R16:  H2S + 2O2 --> SO42- + 2H+
        ! R17:  FeS + 2O2 --> Fe2+ + SO42-
        ! R18:  2FeS2 + 7O2 + 2H2O --> 2Fe2+ + 4SO42- + 4H+
        ! R19:  4S0 + 4H2O --> 3H2S + SO42- + 2H+
!!!! ============================================================== !!!

    !! Reaction rate constants
    real(8),parameter :: k6  = 2.5d-6  ! For R6  (µM-1 s-1)           Fossing(2004)
    ! real(8),parameter :: k6  = 2.5d-7  ! For R6  (µM-1 s-1)           Berg(2003),Kasih2004
    real(8),parameter :: k7  = 5.0d-14 ! For R7  (s-1)                Kasih2009
    ! real(8),parameter :: k7  = 5.0d-11 ! For R7  (s-1)                Fossing2004
    ! real(8),parameter :: k8  = 7.5d-11 ! For R8  (µM-1 s-1)           Berg2003
    real(8),parameter :: k8  = 1.7d-9 ! For R8  (µM-1 s-1)           Kasih(2009)
    real(8),parameter :: k9  = 1.5d-5  ! For R9  (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k10 = 2.0d-7  ! For R10 (µM-1 s-1)           Kasih(2009)
    real(8),parameter :: k11 = 5.0d-4  ! For R11 (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k12 = 3.0d-9  ! For R12 (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k13 = 3.75d-5  ! For R13 (µM-1 s-1)           Kasih(2009)
    real(8),parameter :: k14 = 3.0d-10 ! For R14 (cm3 s-1 nmol-1)     Fossing(2004)
    real(8),parameter :: k15 = 7.5d-12 ! For R15 (s-1)                Kasih(2009)
    real(8),parameter :: k16 = 5.0d-5  ! For R16 (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k17 = 6.0d-7  ! For R17 (µM-1 s-1)           Fossing(2004)
    ! real(8),parameter :: k18 = 1.6d-8  ! For R18 (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k18 = 3.0d-10  ! For R18 (µM-1 s-1)           Berg2003, Kasih2009
    real(8),parameter :: k19 = 7.0d-7  ! For R19 (s-1)               Fossing(2004)
    !! non-degratable
    real(8),parameter :: k20 = 1.3d-9     !For R20 (MnO2_ded --> MnO2_undeg) (s-1)    Fossing2004
    real(8),parameter :: k21 = 9.0d-10    !For R21 (FeOOH_ded --> FeOOH_undeg) (s-1)  Fossing2004


    !! ----------- Decomposition rate constant for organic matter ------------- !!
    !! DOM
    ! real(8),parameter :: kdomf = 1.0d-3   ! For  domf (s-1)      Kasih 2009, Irie2018
    real(8),parameter :: kdomf = 1.0d-4   ! For  domf (s-1)           
    ! real(8),parameter :: kdomf = 2.0d-3   ! For  domf (s-1)
    ! real(8),parameter :: kdomf = 1.2d-1   ! For  domf (s-1)

    ! real(8),parameter :: kdoms = 5.0d-9   ! For  doms (s-1)      Kasih 2009
    ! real(8),parameter :: kdoms = 5.0d-8   ! For  doms (s-1)      Irie(2018)
    ! real(8),parameter :: kdoms = 1.0d-8
    ! real(8),parameter :: kdoms = 2.5d-9   ! For  doms (s-1)      
    ! real(8),parameter :: kdoms = 1.0d-7   ! For  doms (s-1)   
    real(8),parameter :: kdoms = 1.4d-10   ! For  doms (s-1)
    ! real(8),parameter :: kdoms = 1.4d-12   ! For  doms (s-1)            

    !! POM
    ! real(8),parameter :: kpomf = 2.5d-6   ! For  pomf (s-1)      Kasih2009, Irie2018
    real(8),parameter :: kpomf = 2.5d-7   ! For  pomf (s-1)
    ! real(8),parameter :: kpomf = 2.5d-8   ! For  pomf (s-1)          
    ! real(8),parameter :: kpomf = 2.0d-9   ! For  pomf (s-1)            
    ! real(8),parameter :: kpomf = 5.0d-10   ! For  pomf (s-1)            

    ! real(8),parameter :: kpoms = 1.2d-10   ! For  poms (s-1)     Kasih2009
    ! real(8),parameter :: kpoms = 6.0d-10   ! For  poms (s-1)     Irie(2018)
    ! real(8),parameter :: kpoms = 6.0d-11   ! For  poms (s-1)      
    ! real(8),parameter :: kpoms = 1.2d-9   ! For  poms (s-1)     
    real(8),parameter :: kpoms = 1.2d-11   ! For  poms (s-1) 
    ! real(8),parameter :: kpoms = 7.0d-12   ! For  poms (s-1)

    !! ------------------------------------------------------------------------- !!


    !! Temperature coefficient Q10
    real(8),parameter :: Q10_pri = 3.8d0   !  For primary reactions               Fossing2004
    real(8),parameter :: Q10_sec = 2.0d0   !  For secondary reactions             Fossing2004

    !! Reaction rate constants considering temperature dependency
    real(8) :: k6_Q10(Nsed),k7_Q10(Nsed),k8_Q10(Nsed),k9_Q10(Nsed),k10_Q10(Nsed)
    real(8) :: k11_Q10(Nsed),k12_Q10(Nsed),k13_Q10(Nsed),k14_Q10(Nsed),k15_Q10(Nsed)
    real(8) :: k16_Q10(Nsed),k17_Q10(Nsed),k18_Q10(Nsed),k19_Q10(Nsed),k20_Q10(Nsed),k21_Q10(Nsed) 
    real(8) :: kdomf_Q10(Nsed),kdoms_Q10(Nsed),kpomf_Q10(Nsed),kpoms_Q10(Nsed)

    !! C:N:P ratio
    ! real(8), parameter :: rCNph = 106.0d0/16.0d0   ! (no dim.) algal C:N ratio (Redfield ratio)
    real(8), parameter :: rCPph = 106.0d0/1.0d0    ! (no dim.) algal C:P ratio (Redfield ratio)
    real(8), parameter :: rCNph = 11.0d0   ! (no dim.) algal C:N ratio (Redfield ratio)        from analysis
    ! real(8), parameter :: rCPph = 80.0d0    ! (no dim.) algal C:P ratio (Redfield ratio)     Fossing2004

    !! OM ratio
    real(8), parameter :: OMFratio = 0.4d0    ! (no dim.) OMf/OM ratio       Irie 0.57 / Kasih0.4 / Fossing 0.42
    real(8), parameter :: OMSratio = 0.3d0    ! (no dim.) OMs/OM ratio   
    real(8), parameter :: OMNratio = 0.3d0    ! (no dim.) OMn/OM ratio       Irie 0.2 / Kasih0.2 / Fossing 0.08
   ! real(8), parameter :: 
 
!! Reaction Rate  -------------------------------------------------------------
    !! unit: (nmol cm-3 s-1)
    real(8) :: vdoc(Nsed,2),vdon(Nsed,2),vdop(Nsed,2)    ! 1:labile, 2:refractory
    real(8) :: vpoc(Nsed,2),vpon(Nsed,2),vpop(Nsed,2)    ! 1:labile, 2:refractory
    real(8) :: vOC(Nsed),vON(Nsed),vOP(Nsed)
    real(8) :: v1(Nsed),v2(Nsed),v3(Nsed),v4(Nsed),v5(Nsed),v6(Nsed),v7(Nsed),v8(Nsed),v9(Nsed),v10a(Nsed),v10b(Nsed)
    real(8) :: v11(Nsed),v12(Nsed),v13(Nsed),v14(Nsed),v15(Nsed),v16(Nsed),v17(Nsed),v18(Nsed),v19(Nsed),v20(Nsed),v21(Nsed)

    !! Inhibiting concentration of R19
    real(8),parameter :: H2Sstop = 10d0  ! a certain limit of [H2S] in R19 (µM) (fossing et al. 2004)

    !! Concentration limit
    real(8),parameter :: O2lim    = 20.0d0    !(µmol l-1)     Fossing2004
    real(8),parameter :: NO3lim   = 5.0d0     !(µmol l-1)     Fossing2004
    real(8),parameter :: MnO2lim  = 50000d0   !(nmol g-1)     Fossing2004
    real(8),parameter :: FeOOHlim = 100000d0  !(nmol g-1)     Fossing2004
    real(8),parameter :: SO4lim   = 5.0d0     !(µmol g-1)


!! Net Production and Consumption  -------------------------------------------
    !! unit: (nmol cm-3 s-1)
    real(8) :: R_Tmp(Nsed), R_Sal(Nsed)
    real(8) :: R_O2 (Nsed), R_DIC(Nsed), R_TA(Nsed), R_N2(Nsed)
# if defined ORGANIC_MATTER
    real(8) :: R_DOC(Nsed,2), R_POC(Nsed,2)    ! 1:labile, 2:refractory
# endif
# if defined NUTRIENTS
    real(8) :: R_NO3(Nsed),R_NH4(Nsed),R_PO4(Nsed)
#  if defined ORGANIC_MATTER
    real(8) :: R_DON(Nsed,2), R_PON(Nsed,2)    ! 1:labile, 2:refractory
    real(8) :: R_DOP(Nsed,2), R_POP(Nsed,2)    ! 1:labile, 2:refractory
#  endif
# endif
# if defined SULFATE
    real(8) :: R_Mn2(Nsed), R_MnO2(Nsed)
    real(8) :: R_Fe2(Nsed), R_FeS(Nsed), R_FeS2(Nsed), R_FeOOH(Nsed), R_FeOOH_PO4(Nsed)
    real(8) :: R_H2S(Nsed), R_S0(Nsed), R_SO4(Nsed)
# endif

!! Miscellaneous Sources  -------------------------------------------
    real(8) :: miscTmp(Nsed), miscSal(Nsed)
    !! unit: (µmol L-1 s-1 = nmol cm-3 s-1)
    !! unit: (nmol g-1 s-1)
    real(8) :: miscO2(Nsed), miscDIC(Nsed), miscTA(Nsed), miscN2(Nsed)
# if defined ORGANIC_MATTER
    real(8) :: miscDOC(Nsed,2), miscPOC(Nsed,3)    ! 1:labile, 2:refractory 3:non-degratable
# endif
# if defined NUTRIENTS
    real(8) :: miscNO3(Nsed), miscNH4(Nsed), miscPO4(Nsed)
#  if defined ORGANIC_MATTER
    real(8) :: miscDON(Nsed,2), miscPON(Nsed,3)    ! 1:labile, 2:refractory 3:non-degratable
    real(8) :: miscDOP(Nsed,2), miscPOP(Nsed,3)    ! 1:labile, 2:refractory 3:non-degratable
#  endif
# endif
# if defined SULFATE
    real(8) :: miscMn2(Nsed), miscMnO2(Nsed)
    real(8) :: miscFe2(Nsed), miscFeS(Nsed), miscFeS2(Nsed), miscFeOOH(Nsed), miscFeOOH_PO4(Nsed)
    real(8) :: miscH2S(Nsed), miscS0(Nsed), miscSO4(Nsed)
# endif


!!  Output ------------------------------------------------------------------
# if defined SEDIMENT_TESTMODE
    real(8), parameter :: OUTPUT_INTERVAL = 1440.0d0 !*365     ! Output interval (min)
    real(8), save :: time = 0.d0 !sec
    real(8), save :: dsec = 0.d0 !sec
    real(8), save :: year = 0.0d0 
    real(8) :: pHamb 
    ! real(8) :: redoxlevel(Nsed)    ! for debug / 1:O2, 2:NO3, 3:MnO2, 4:FeOOH, 5:SO4
# endif

! ============================================================================================================== !
!  STRUCTURE OF SEDIMENT COLUMN
!
!                 |   Layer     |    Layer    |       Depth       |      Concentration        |    Flux
!                 |   number    |  thickness  |                   |                           |
!
!                    --------------------------------------------------------------------------------------------
!                       k             dz                z              SEDECO(ng)%C(i,j,k)         FT_C
!                                    (cm)              (cm)           (µmol L-1),(nmol g-1)    (nmol cm-2 s-1)
!        (i,j)       --------------------------------------------------------------------------------------------
!       ___|______     DBL                               0                     Camb             ↓ FT(1)     ←diffusion in DBL  (Dw)
!     /____|____ /| 
!     |         | |
!     |   sed   | |     1           dz(1)               0.03                   C(1)              
!     |_________|/|                                                                        ---   ↓ FT(2)     ←diffusion in sediment  (Ds)
!     |         | |
!     |   sed   | |     2           dz(2)         z(2)=z(1)+dz(2)              C(2)              
!     |_________|/                                                                         ---   ↓ FT(3)     ←diffusion in sediment (Ds)   
!          ...
!      _________ /| 
!     |         | |
!     |   sed   | |    k-1         dz(k-1)       z(k-1)=z(k-2)+dz(k-1)        C(k-1)
!     |_________|/|                                                                        ---   ↓ FT(k)   
!     |         | |
!     |   sed   | |     k           dz(k)         z(k)=z(k-1)+dz(k)            C(k)
!     |_________|/|                                                                        ---   ↓ FT(k+1)   
!     |         | |
!     |   sed   | |    k+1         dz(k+1)       z(k+1)=z(k)+dz(k+1)          C(k+1)
!     |_________|/                                                                         ---   ↓ FT(k+2)   
!          ...
!      _________ /| 
!     |         | |
!     |   sed   | |   Nsed-1      dz(Nsed-1)                                  C(Nsed-1)
!     |_________|/|                                                                        ---   ↓ FT(Nsed)   (Fd, Fb == 0 (closed) / Fc ~= 0 (open))
!     |         | |
!     |   sed   | |     Nsed        dz(Nsed)     z(Nsed)=z(Nsed-1)+dz(Nsed)    C(Nsed)
!     |_________|/                                                                         ---   ↓ FT(Nsed+1) 
!
!                                                                                   [ ***  Flux of layer(k)  *** ]
!                                                                                   [     Flux IN :  FT(K)       ]
!                                                                                   [     Flux OUT:  FT(k+1)     ]
!                                                                                   [  FT: Flux Total (Fd,Fb,Fc,Fa) ]
! ============================================================================================================== !


! ***********************************************************************************
!!! executable statement

! ***************************** !
!!! physical properties of sediment layers 
! ***************************** !

    dz = SEDECO(ng)%dz(:)
    z  = SEDECO(ng)%z(:)

    !! soil density (This must be given as an input data actually)
    rhos_d(:) = 2.5d0 ! (g/cm3)   ! Note: depth varied density is not properly implemented in geochem

! ***************************** !
!!! initial Temperature and Salinity 
! ***************************** !
    !! set the same temp and sal for all of layers as water column

    SEDECO(ng)%Tmp(i,j,:) = Tamb
    SEDECO(ng)%Sal(i,j,:) = Samb

! ***************************** !


! ***************************** !
!!!  Boundary condition
! ***************************** !
!!!  Upper boundary is defined as constant concentrations in water column in main.F
!!!  Bottom boundary (nmol cm-2 s-1) --------------------------
!    Closed boundary condition 
!# if defined SEDECO_CLOSED_BOTTOM_DIFFUSION_BOUNDARY
!!! set boundary-1 to boundary and calculate below vvv in ficks law to get zero diffusion
!    SEDECO(ng)%Tmp(i,j,Nsed) = SEDECO(ng)%Tmp(i,j,Nsed-1)
!    SEDECO(ng)%Sal(i,j,Nsed) = SEDECO(ng)%Sal(i,j,Nsed-1)
!
!    SEDECO(ng)%O2(i,j,Nsed) = SEDECO(ng)%O2(i,j,Nsed-1)
!    SEDECO(ng)%DIC(i,j,Nsed) = SEDECO(ng)%DIC(i,j,Nsed-1)
!    SEDECO(ng)%N2(i,j,Nsed) = SEDECO(ng)%N2(i,j,Nsed-1)
!
!#  if defined ORGANIC_MATTER
!    SEDECO(ng)%DOCf(i,j,Nsed) = SEDECO(ng)%DOCf(i,j,Nsed-1)
!    SEDECO(ng)%DOCs(i,j,Nsed) = SEDECO(ng)%DOCs(i,j,Nsed-1)
!    SEDECO(ng)%POCf(i,j,Nsed) = SEDECO(ng)%POCf(i,j,Nsed-1)
!    SEDECO(ng)%POCs(i,j,Nsed) = SEDECO(ng)%POCs(i,j,Nsed-1)
!    SEDECO(ng)%POCn(i,j,Nsed) = SEDECO(ng)%POCn(i,j,Nsed-1)
!
!#  endif
!#  if defined NUTRIENTS
!    SEDECO(ng)%NO3(i,j,Nsed) = SEDECO(ng)%NO3(i,j,Nsed-1)
!    SEDECO(ng)%NH4(i,j,Nsed) = SEDECO(ng)%NH4(i,j,Nsed-1)
!    SEDECO(ng)%PO4(i,j,Nsed) = SEDECO(ng)%PO4(i,j,Nsed-1)
!
!#   if defined ORGANIC_MATTER
!    SEDECO(ng)%DONf(i,j,Nsed) = SEDECO(ng)%DONf(i,j,Nsed-1)
!    SEDECO(ng)%DONs(i,j,Nsed) = SEDECO(ng)%DONs(i,j,Nsed-1)
!    SEDECO(ng)%PONf(i,j,Nsed) = SEDECO(ng)%PONf(i,j,Nsed-1)
!    SEDECO(ng)%PONs(i,j,Nsed) = SEDECO(ng)%PONs(i,j,Nsed-1)
!    SEDECO(ng)%PONn(i,j,Nsed) = SEDECO(ng)%PONn(i,j,Nsed-1)
!    SEDECO(ng)%DOPf(i,j,Nsed) = SEDECO(ng)%DOPf(i,j,Nsed-1)
!    SEDECO(ng)%DOPs(i,j,Nsed) = SEDECO(ng)%DOPs(i,j,Nsed-1)
!    SEDECO(ng)%POPf(i,j,Nsed) = SEDECO(ng)%POPf(i,j,Nsed-1)
!    SEDECO(ng)%POPs(i,j,Nsed) = SEDECO(ng)%POPs(i,j,Nsed-1)
!    SEDECO(ng)%POPn(i,j,Nsed) = SEDECO(ng)%POPn(i,j,Nsed-1)
!
!#   endif
!#  endif
!#  if defined SULFATE
!    SEDECO(ng)%Mn2(i,j,Nsed)  = SEDECO(ng)%Mn2(i,j,Nsed-1)
!    SEDECO(ng)%MnO2(i,j,Nsed) = SEDECO(ng)%MnO2(i,j,Nsed-1)
!
!    SEDECO(ng)%Fe2(i,j,Nsed)       = SEDECO(ng)%Fe2(i,j,Nsed-1)
!    SEDECO(ng)%FeS(i,j,Nsed)       = SEDECO(ng)%FeS(i,j,Nsed-1)
!    SEDECO(ng)%FeS2(i,j,Nsed)      = SEDECO(ng)%FeS2(i,j,Nsed-1)
!    SEDECO(ng)%FeOOH(i,j,Nsed)     = SEDECO(ng)%FeOOH(i,j,Nsed-1)
!    SEDECO(ng)%FeOOH_PO4(i,j,Nsed) = SEDECO(ng)%FeOOH_PO4(i,j,Nsed-1)
!
!    SEDECO(ng)%H2S(i,j,Nsed) = SEDECO(ng)%H2S(i,j,Nsed-1)
!    SEDECO(ng)%S0(i,j,Nsed)  = SEDECO(ng)%S0(i,j,Nsed-1)
!    SEDECO(ng)%SO4(i,j,Nsed) = SEDECO(ng)%SO4(i,j,Nsed-1)
!
!#  endif
!# endif    


! ***************************** !
!          Diffusion
! ***************************** !

    !! ----- for sediment column ----- !!
    DO k = 1,Nsed
        Dw_Tmp(k)  = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)  !!! Need check !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        Dw_Sal(k)  = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)  !!! Need check !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        Dw_O2(k)   = (11.7d0 + 0.344d0*SEDECO(ng)%Tmp(i,j,k) + 0.00505d0*SEDECO(ng)%Tmp(i,j,k)**2d0) *(1.0d-6)
        Dw_TA (k)  = (9.39d0 + 0.267d0*SEDECO(ng)%Tmp(i,j,k) + 0.0041d0 *SEDECO(ng)%Tmp(i,j,k)**2d0) *(1.0d-6)  !!! Need check !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        Dw_DIC(k)  = (9.39d0 + 0.267d0*SEDECO(ng)%Tmp(i,j,k) + 0.0041d0 *SEDECO(ng)%Tmp(i,j,k)**2d0) *(1.0d-6) 
        Dw_N2(k)   = (9.52d0 + 0.291d0*SEDECO(ng)%Tmp(i,j,k) + 0.00448d0*SEDECO(ng)%Tmp(i,j,k)**2d0) *(1.0d-6)
# if defined ORGANIC_MATTER
        Dw_DOM(k,1) = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(i,j,k)) *(1.0d-7) ! DOMf: DOCf DONf DOPf
        Dw_DOM(k,2) = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(i,j,k)) *(1.0d-9) ! DOMs: DOCs DONs DOPs
# endif
# if defined NUTRIENTS
        Dw_NO3(k)  = (9.72d0 + 0.365d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)
        Dw_NH4(k)  = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)
        Dw_PO4(k)  = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)
# endif
# if defined SULFATE
        Dw_Mn2(k)  = (3.04d0 + 0.153d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)
        Dw_Fe2(k)  = (3.36d0 + 0.148d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)
        Dw_SO4(k)  = (4.96d0 + 0.226d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)
        Dw_H2S(k)  = (8.74d0 + 0.264d0*SEDECO(ng)%Tmp(i,j,k) + 0.004d0*SEDECO(ng)%Tmp(i,j,k)**2d0) *(1.0d-6)
# endif
    ENDDO

    !! --- Diffusion coefficient in sediment (function Dsed is defined in mod_geochem.F) --- !!
    DO k = 2,Nsed
        Ds_Tmp(k)  = Dsed(Dw_Tmp(k),SEDECO(ng)%poro(i,j,k))
        Ds_Sal(k)  = Dsed(Dw_Sal(k),SEDECO(ng)%poro(i,j,k))
        Ds_O2 (k)  = Dsed(Dw_O2 (k),SEDECO(ng)%poro(i,j,k))
        Ds_TA (k)  = Dsed(Dw_TA (k),SEDECO(ng)%poro(i,j,k))
        Ds_DIC(k)  = Dsed(Dw_DIC(k),SEDECO(ng)%poro(i,j,k))
        Ds_N2 (k)  = Dsed(Dw_N2 (k),SEDECO(ng)%poro(i,j,k))
# if defined ORGANIC_MATTER
        Ds_DOM(k,1) = Dsed(Dw_DOM(k,1),SEDECO(ng)%poro(i,j,k)) ! DOMf: DOCf DONf DOPf
        Ds_DOM(k,2) = Dsed(Dw_DOM(k,2),SEDECO(ng)%poro(i,j,k)) ! DOMs: DOCs DONs DOPs
# endif
# if defined NUTRIENTS
        Ds_NO3(k)  = Dsed(Dw_NO3(k),SEDECO(ng)%poro(i,j,k))
        Ds_NH4(k)  = Dsed(Dw_NH4(k),SEDECO(ng)%poro(i,j,k))
        Ds_PO4(k)  = Dsed(Dw_PO4(k),SEDECO(ng)%poro(i,j,k))
# endif
# if defined SULFATE
        Ds_Mn2(k)  = Dsed(Dw_Mn2(k),SEDECO(ng)%poro(i,j,k))
        Ds_Fe2(k)  = Dsed(Dw_Fe2(k),SEDECO(ng)%poro(i,j,k))
        Ds_SO4(k)  = Dsed(Dw_SO4(k),SEDECO(ng)%poro(i,j,k))
        Ds_H2S(k)  = Dsed(Dw_H2S(k),SEDECO(ng)%poro(i,j,k))
# endif
    ENDDO

!!!  (0) DBL: Water-sediment fluxes (nmol cm-2 s-1) -------------------------
    ! (0) --> (1)

    rho_sw(1) = densSW( Tamb,Samb )  ! Density of seawater [g/cm3]     ! function densSW is defined in mod_geochem
    Fd_Tmp(1) = Ficks(Dw_Tmp(1),Tamb  ,SEDECO(ng)%Tmp(i,j,1),dbl)
    Fd_Sal(1) = Ficks(Dw_Sal(1),Samb  ,SEDECO(ng)%Sal(i,j,1),dbl)
    Fd_O2 (1) = Ficks(Dw_O2 (1),O2amb ,SEDECO(ng)%O2 (i,j,1),dbl)
    Fd_TA (1) = Ficks(Dw_TA (1),DICamb,SEDECO(ng)%TA (i,j,1),dbl)
    Fd_DIC(1) = Ficks(Dw_DIC(1),DICamb,SEDECO(ng)%DIC(i,j,1),dbl)
    Fd_N2 (1) = Ficks(Dw_N2 (1),N2amb ,SEDECO(ng)%N2 (i,j,1),dbl)
# if defined NUTRIENTS
    Fd_NO3(1) = Ficks(Dw_NO3(1),NO3amb,SEDECO(ng)%NO3(i,j,1),dbl)
    Fd_NH4(1) = Ficks(Dw_NH4(1),NH4amb,SEDECO(ng)%NH4(i,j,1),dbl)
    Fd_PO4(1) = Ficks(Dw_PO4(1),PO4amb,SEDECO(ng)%PO4(i,j,1),dbl)
# endif
# if defined SULFATE
    Fd_Mn2(1) = Ficks(Dw_Mn2(1),Mn2amb,SEDECO(ng)%Mn2(i,j,1),dbl)
    Fd_Fe2(1) = Ficks(Dw_Fe2(1),Fe2amb,SEDECO(ng)%Fe2(i,j,1),dbl)
    Fd_H2S(1) = Ficks(Dw_H2S(1),H2Samb,SEDECO(ng)%H2S(i,j,1),dbl)
    Fd_SO4(1) = Ficks(Dw_SO4(1),SO4amb,SEDECO(ng)%SO4(i,j,1),dbl)
# endif


    !! (1): Molecular Diffusion Flux calculation (Only for solutes) ------------
    DO k = 2,Nsed
        ! function densSW is defined in mod_geochem
        rho_sw(k) = densSW( SEDECO(ng)%Tmp(i,j,k),SEDECO(ng)%Sal(i,j,k) )  ! Density of seawater [g/cm3]   
        Fd_Tmp(k) = Ficks_sedd(Ds_Tmp(k),SEDECO(ng)%Tmp(i,j,k-1),SEDECO(ng)%Tmp(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_Sal(k) = Ficks_sedd(Ds_Sal(k),SEDECO(ng)%Sal(i,j,k-1),SEDECO(ng)%Sal(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_O2(k)    = Ficks_sedd(Ds_O2(k),SEDECO(ng)%O2(i,j,k-1),SEDECO(ng)%O2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_TA (k)   = Ficks_sedd(Ds_TA (k),SEDECO(ng)%TA (i,j,k-1),SEDECO(ng)%TA (i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_DIC(k)   = Ficks_sedd(Ds_DIC(k),SEDECO(ng)%DIC(i,j,k-1),SEDECO(ng)%DIC(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_N2(k)    = Ficks_sedd(Ds_N2(k),SEDECO(ng)%N2(i,j,k-1),SEDECO(ng)%N2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
# if defined ORGANIC_MATTER
        Fd_DOC(k,1) = Ficks_sedd(Ds_DOM(k,1),SEDECO(ng)%DOCf(i,j,k-1),SEDECO(ng)%DOCf(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k)) ! DOCf
        Fd_DOC(k,2) = Ficks_sedd(Ds_DOM(k,2),SEDECO(ng)%DOCs(i,j,k-1),SEDECO(ng)%DOCs(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k)) ! DOCs
# endif
# if defined NUTRIENTS
        Fd_NO3(k)   = Ficks_sedd(Ds_NO3(k),SEDECO(ng)%NO3(i,j,k-1),SEDECO(ng)%NO3(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_NH4(k)   = Ficks_sedd(Ds_NH4(k),SEDECO(ng)%NH4(i,j,k-1),SEDECO(ng)%NH4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_PO4(k)   = Ficks_sedd(Ds_PO4(k),SEDECO(ng)%PO4(i,j,k-1),SEDECO(ng)%PO4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
#  if defined ORGANIC_MATTER
        Fd_DON(k,1) = Ficks_sedd(Ds_DOM(k,1),SEDECO(ng)%DONf(i,j,k-1),SEDECO(ng)%DONf(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k)) ! DONf
        Fd_DON(k,2) = Ficks_sedd(Ds_DOM(k,2),SEDECO(ng)%DONs(i,j,k-1),SEDECO(ng)%DONs(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k)) ! DONs
        Fd_DOP(k,1) = Ficks_sedd(Ds_DOM(k,1),SEDECO(ng)%DOPf(i,j,k-1),SEDECO(ng)%DOPf(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k)) ! DOPf
        Fd_DOP(k,2) = Ficks_sedd(Ds_DOM(k,2),SEDECO(ng)%DOPs(i,j,k-1),SEDECO(ng)%DOPs(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k)) ! DOPs
#  endif
# endif
# if defined SULFATE
        Fd_Mn2(k)   = Ficks_sedd(Ds_Mn2(k),SEDECO(ng)%Mn2(i,j,k-1),SEDECO(ng)%Mn2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_Fe2(k)   = Ficks_sedd(Ds_Fe2(k),SEDECO(ng)%Fe2(i,j,k-1),SEDECO(ng)%Fe2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_H2S(k)   = Ficks_sedd(Ds_H2S(k),SEDECO(ng)%H2S(i,j,k-1),SEDECO(ng)%H2S(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_SO4(k)   = Ficks_sedd(Ds_SO4(k),SEDECO(ng)%SO4(i,j,k-1),SEDECO(ng)%SO4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
# endif
     ENDDO

!!!  (2): Bio-diffusion flux in sediment column (nmol cm-2 s-1) (For solutes and solids) ------------
     DO k = 1,Nsed
        !! determine DBd and DBp depending on z
        if (z(k) <= 11.8d0 )then
            DBd(k) = 3.51d-6
        else
            DBd(k) = 3.51d-6 * exp(-0.378d0*(z(k)-11.8d0))
        endif
        DBp(k) = DBd(k)/9.3d0
     ENDDO

     DO k = 2,Nsed
        Fb_Tmp(k) = Ficks_sedd(DBd(k),SEDECO(ng)%Tmp(i,j,k-1),SEDECO(ng)%DTmp(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_Sal(k) = Ficks_sedd(DBd(k),SEDECO(ng)%Sal(i,j,k-1),SEDECO(ng)%Sal(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_O2(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%O2(i,j,k-1),SEDECO(ng)%O2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_TA (k)  = Ficks_sedd(DBd(k),SEDECO(ng)%TA (i,j,k-1),SEDECO(ng)%TA (i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_DIC(k)  = Ficks_sedd(DBd(k),SEDECO(ng)%DIC(i,j,k-1),SEDECO(ng)%DIC(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_N2(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%N2(i,j,k-1),SEDECO(ng)%N2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
# if defined ORGANIC_MATTER
        Fb_DOC(k,1) = Ficks_sedd(DBd(k),SEDECO(ng)%DOCf(i,j,k-1),SEDECO(ng)%DOCf(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_DOC(k,2) = Ficks_sedd(DBd(k),SEDECO(ng)%DOCs(i,j,k-1),SEDECO(ng)%DOCs(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_POC(k,1) = Ficks_sedp(DBp(k),SEDECO(ng)%POCf(i,j,k-1),SEDECO(ng)%POCf(i,j,k),dz(k),&
                                SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_POC(k,2) = Ficks_sedp(DBp(k),SEDECO(ng)%POCs(i,j,k-1),SEDECO(ng)%POCs(i,j,k),dz(k),&
                                SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_POC(k,3) = Ficks_sedp(DBp(k),SEDECO(ng)%POCn(i,j,k-1),SEDECO(ng)%POCn(i,j,k),dz(k),&
                                SEDECO(ng)%poro(i,j,k),rhos_d(k))
# endif
# if defined NUTRIENTS
        Fb_NO3(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%NO3(i,j,k-1),SEDECO(ng)%NO3(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_NH4(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%NH4(i,j,k-1),SEDECO(ng)%NH4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_PO4(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%PO4(i,j,k-1),SEDECO(ng)%PO4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
#  if defined ORGANIC_MATTER
        Fb_DON(k,1) = Ficks_sedd(DBd(k),SEDECO(ng)%DONf(i,j,k-1),SEDECO(ng)%DONf(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_DON(k,2) = Ficks_sedd(DBd(k),SEDECO(ng)%DONs(i,j,k-1),SEDECO(ng)%DONs(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_PON(k,1) = Ficks_sedp(DBp(k),SEDECO(ng)%PONf(i,j,k-1),SEDECO(ng)%PONf(i,j,k),dz(k),&
                                SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_PON(k,2) = Ficks_sedp(DBp(k),SEDECO(ng)%PONs(i,j,k-1),SEDECO(ng)%PONs(i,j,k),dz(k),&
                                SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_PON(k,3) = Ficks_sedp(DBp(k),SEDECO(ng)%PONn(i,j,k-1),SEDECO(ng)%PONn(i,j,k),dz(k),&
                                SEDECO(ng)%poro(i,j,k),rhos_d(k))

        Fb_DOP(k,1) = Ficks_sedd(DBd(k),SEDECO(ng)%DOPf(i,j,k-1),SEDECO(ng)%DOPf(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_DOP(k,2) = Ficks_sedd(DBd(k),SEDECO(ng)%DOPs(i,j,k-1),SEDECO(ng)%DOPs(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_POP(k,1) = Ficks_sedp(DBp(k),SEDECO(ng)%POPf(i,j,k-1),SEDECO(ng)%POPf(i,j,k),dz(k),&
                                SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_POP(k,2) = Ficks_sedp(DBp(k),SEDECO(ng)%POPs(i,j,k-1),SEDECO(ng)%POPs(i,j,k),dz(k),&
                                SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_POP(k,3) = Ficks_sedp(DBp(k),SEDECO(ng)%POPn(i,j,k-1),SEDECO(ng)%POPn(i,j,k),dz(k),&
                                SEDECO(ng)%poro(i,j,k),rhos_d(k))
#  endif
# endif
# if defined SULFATE
        Fb_Mn2(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%Mn2(i,j,k-1),SEDECO(ng)%Mn2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_MnO2(k)  = Ficks_sedp(DBp(k),SEDECO(ng)%MnO2(i,j,k-1),SEDECO(ng)%MnO2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))

        Fb_Fe2(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%Fe2(i,j,k-1),SEDECO(ng)%Fe2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_FeS(k)   = Ficks_sedp(DBp(k),SEDECO(ng)%FeS(i,j,k-1),SEDECO(ng)%FeS(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_FeS2(k)  = Ficks_sedp(DBp(k),SEDECO(ng)%FeS2(i,j,k-1),SEDECO(ng)%FeS2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_FeOOH(k) = Ficks_sedp(DBp(k),SEDECO(ng)%FeOOH(i,j,k-1),SEDECO(ng)%FeOOH(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_FeOOH_PO4(k) &
          = Ficks_sedp(DBp(k),SEDECO(ng)%FeOOH_PO4(i,j,k-1),SEDECO(ng)%FeOOH_PO4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))

        Fb_H2S(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%H2S(i,j,k-1),SEDECO(ng)%H2S(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_S0(k)    = Ficks_sedp(DBp(k),SEDECO(ng)%S0(i,j,k-1),SEDECO(ng)%S0(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_SO4(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%SO4(i,j,k-1),SEDECO(ng)%SO4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
# endif
     ENDDO

!!! (3) Adsorption (nmol cm-2 s-1) ------------------------------------------
# if defined adsorption
     DO k = 2,Nsed
#  if defined NUTRIENTS
        Fb_NO3(k)   = Fb_NO3(k) + Ficks_sedp(DBp(k) * Kad_NO3,SEDECO(ng)%NO3(i,j,k-1), &
                                            SEDECO(ng)%NO3(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_NH4(k)   = Fb_NH4(k) + Ficks_sedp(DBp(k) * Kad_NH4,SEDECO(ng)%NH4(i,j,k-1), &
                                            SEDECO(ng)%NH4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_PO4(k)   = Fb_PO4(k) + Ficks_sedp(DBp(k) * Kad_PO4,SEDECO(ng)%PO4(i,j,k-1), &
                                            SEDECO(ng)%PO4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
#  endif
#  if defined SULFATE
        Fb_Mn2(k)   = Fb_Mn2(k) + Ficks_sedp(DBp(k) * Kad_Mn2,SEDECO(ng)%Mn2(i,j,k-1), & 
                                            SEDECO(ng)%Mn2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_Fe2(k)   = Fb_Fe2(k) + Ficks_sedp(DBp(k) * Kad_Fe2,SEDECO(ng)%Fe2(i,j,k-1),& 
                                            SEDECO(ng)%Fe2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
#  endif
     ENDDO
# endif



! ***************************** !
!!!     Burial term
! ***************************** !
# if defined SEDECO_BURIAL
    !! Burial Flux calculation 
    DO k = 2,Nsed
        Fc_Tmp(k) = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%Tmp(i,j,k))
        Fc_Sal(k) = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%Sal(i,j,k))
        Fc_O2(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%O2(i,j,k))
        Fc_TA (k)  = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%TA(i,j,k))
        Fc_DIC(k)  = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%DIC(i,j,k))
        Fc_N2(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%N2(i,j,k))
#  if defined ORGANIC_MATTER
        Fc_DOC(k,1)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%DOCf(i,j,k))
        Fc_DOC(k,2)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%DOCs(i,j,k))
        Fc_POC(k,1)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%POCf(i,j,k),rhos_d(k))
        Fc_POC(k,2)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%POCs(i,j,k),rhos_d(k))
        Fc_POC(k,3)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%POCn(i,j,k),rhos_d(k))
#  endif
#  if defined NUTRIENTS
        Fc_NO3(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%NO3(i,j,k))
        Fc_NH4(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%NH4(i,j,k))
        Fc_PO4(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%PO4(i,j,k))
#   if defined ORGANIC_MATTER
        Fc_DON(k,1)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%DONf(i,j,k))
        Fc_DON(k,2)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%DONs(i,j,k))
        Fc_PON(k,1)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%PONf(i,j,k),rhos_d(k))
        Fc_PON(k,2)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%PONs(i,j,k),rhos_d(k))
        Fc_PON(k,3)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%PONn(i,j,k),rhos_d(k))
        Fc_DOP(k,1)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%DOPf(i,j,k))
        Fc_DOP(k,2)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%DOPs(i,j,k))
        Fc_POP(k,1)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%POPf(i,j,k),rhos_d(k))
        Fc_POP(k,2)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%POPs(i,j,k),rhos_d(k))
        Fc_POP(k,3)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%POPn(i,j,k),rhos_d(k))
#   endif
#  endif
#  if defined SULFATE
        Fc_Mn2(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%Mn2(i,j,k))
        Fc_MnO2(k)  = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%MnO2(i,j,k),rhos_d(k))

        Fc_Fe2(k)       = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%Fe2(i,j,k))
        Fc_FeS(k)       = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%FeS(i,j,k),rhos_d(k))
        Fc_FeS2(k)      = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%FeS2(i,j,k),rhos_d(k))
        Fc_FeOOH(k)     = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%FeOOH(i,j,k),rhos_d(k))
        Fc_FeOOH_PO4(k) = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%FeOOH_PO4(i,j,k),rhos_d(k))

        Fc_H2S(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%H2S(i,j,k))
        Fc_S0(k)    = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%S0(i,j,k),rhos_d(k))
        Fc_SO4(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%SO4(i,j,k))
#  endif
#  if defined adsorption
        Fc_NO3(k) = Fc_NO3(k) + FCp(SEDECO(ng)%poro(i,j,k),(u * Kad_NO3),SEDECO(ng)%NO3(i,j,k),rhos_d(k))
        Fc_NH4(k) = Fc_NH4(k) + FCp(SEDECO(ng)%poro(i,j,k),(u * Kad_NH4),SEDECO(ng)%NH4(i,j,k),rhos_d(k))
        Fc_PO4(k) = Fc_PO4(k) + FCp(SEDECO(ng)%poro(i,j,k),(u * Kad_PO4),SEDECO(ng)%PO4(i,j,k),rhos_d(k))
        Fc_Mn2(k) = Fc_Mn2(k) + FCp(SEDECO(ng)%poro(i,j,k),(u * Kad_Mn2),SEDECO(ng)%Mn2(i,j,k),rhos_d(k))
        Fc_Fe2(k) = Fc_Fe2(k) + FCp(SEDECO(ng)%poro(i,j,k),(u * Kad_Fe2),SEDECO(ng)%Fe2(i,j,k),rhos_d(k))
#  endif
    ENDDO
    Fc_Tmp(Nsed+1) = Fc_Tmp(Nsed)
    Fc_Sal(Nsed+1) = Fc_Sal(Nsed)
    Fc_O2(Nsed+1)   = Fc_O2(Nsed)
    Fc_TA (Nsed+1)  = Fc_TA (Nsed)
    Fc_DIC(Nsed+1)  = Fc_DIC(Nsed)
    Fc_N2(Nsed+1)   = Fc_N2(Nsed)
#  if defined ORGANIC_MATTER
    Fc_DOC(Nsed+1,:)   = Fc_DOC(Nsed,:)
    Fc_POC(Nsed+1,:)   = Fc_POC(Nsed,:)
#  endif
#  if defined NUTRIENTS
    Fc_NO3(Nsed+1)   = Fc_NO3(Nsed)
    Fc_NH4(Nsed+1)   = Fc_NH4(Nsed)
    Fc_PO4(Nsed+1)   = Fc_PO4(Nsed)
#   if defined ORGANIC_MATTER
    Fc_DON(Nsed+1,:)   = Fc_DON(Nsed,:)
    Fc_PON(Nsed+1,:)   = Fc_PON(Nsed,:)
    Fc_DOP(Nsed+1,:)   = Fc_DOP(Nsed,:)
    Fc_POP(Nsed+1,:)   = Fc_POP(Nsed,:)
#   endif
#  endif
#  if defined SULFATE
    Fc_Mn2(Nsed+1)   = Fc_Mn2(Nsed)
    Fc_MnO2(Nsed+1)  = Fc_MnO2(Nsed)
    Fc_Fe2(Nsed+1)       = Fc_Fe2(Nsed)
    Fc_FeS(Nsed+1)       = Fc_FeS(Nsed)
    Fc_FeS2(Nsed+1)      = Fc_FeS2(Nsed)
    Fc_FeOOH(Nsed+1)     = Fc_FeOOH(Nsed)
    Fc_FeOOH_PO4(Nsed+1) = Fc_FeOOH_PO4(Nsed)
    Fc_H2S(Nsed+1)   = Fc_H2S(Nsed)
    Fc_S0(Nsed+1)    = Fc_S0(Nsed)
    Fc_SO4(Nsed+1)   = Fc_SO4(Nsed)
#  endif
# endif


! ***************************** !
!!!     Advection term
! ***************************** !
# if defined SEDECO_ADVECTION
    if( sgd_flux * dt > minval( SEDECO(ng)%dz(:) / SEDECO(ng)%poro(i,j,:) ) ) then
        write(*,*) 'SEDIMENT_ECOSYS: Error: advection volume exceeds layer volume in time step.' 
        write(*,*) '                 advection flux = ', sgd_flux, 'cm s-1'
        write(*,*) '                 smallest layer thickness = ', minval(SEDECO(ng)%dz), 'cm'
        write(*,*) '                 time step (dt*SedIter) = ', dt, 's'
        write(*,*) '  Confirm advection flux, then consider increasing the layer thickness or SedIter'
        error stop
    endif

    !! Advection Flux calculation 
    AdvecVelo = -1.0d0 * sgd_flux ! [cm s-1]       advection velocity (advection downwards is positive)
    !! surface layer k = 1
    k = 1
    Fa_Tmp(k)     = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,Tamb, SEDECO(ng)%Tmp(i,j,k))
    Fa_Sal(k)     = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,Samb, SEDECO(ng)%Sal(i,j,k))
    Fa_O2(k)      = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,O2amb,SEDECO(ng)%O2 (i,j,k))
    Fa_TA (k)     = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,TAamb,SEDECO(ng)%TA (i,j,k))
    Fa_DIC(k)     = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,DICamb,SEDECO(ng)%DIC(i,j,k))
    Fa_N2(k)      = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,N2amb,SEDECO(ng)%N2 (i,j,k))
#  if defined ORGANIC_MATTER
    ! Fa_DOC(k,1)   = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,O2amb,SEDECO(ng)%DOCf(i,j,k))
    ! Fa_DOC(k,2)   = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,O2amb,SEDECO(ng)%DOCs(i,j,k))
    ! Fa_POC(k,1)   = 0.0d0
    ! Fa_POC(k,2)   = 0.0d0
    ! Fa_POC(k,3)   = 0.0d0
#  endif
#  if defined NUTRIENTS
    Fa_NO3(k)     = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,NO3amb,SEDECO(ng)%NO3(i,j,k))
    Fa_NH4(k)     = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,NH4amb,SEDECO(ng)%NH4(i,j,k))
    Fa_PO4(k)     = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,PO4amb,SEDECO(ng)%PO4(i,j,k))
#   if defined ORGANIC_MATTER
    ! Fa_DON(k,1)   = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,O2amb,SEDECO(ng)%DONf(i,j,k))
    ! Fa_DON(k,2)   = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,O2amb,SEDECO(ng)%DONs(i,j,k))
    ! Fa_PON(k,1)   = 0.0d0
    ! Fa_PON(k,2)   = 0.0d0
    ! Fa_PON(k,3)   = 0.0d0
    ! Fa_DOP(k,1)   = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,O2amb,SEDECO(ng)%DOPf(i,j,k))
    ! Fa_DOP(k,2)   = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,O2amb,SEDECO(ng)%DOPs(i,j,k))
    ! Fa_POP(k,1)   = 0.0d0
    ! Fa_POP(k,2)   = 0.0d0
    ! Fa_POP(k,3)   = 0.0d0
#   endif
#  endif
#  if defined SULFATE
    Fa_Mn2(k)     = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,Mn2amb,SEDECO(ng)%Mn2(i,j,k))
    ! Fa_MnO2(k)      = 0.0d0
    Fa_Fe2(k)     = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,Fe2amb,SEDECO(ng)%Fe2(i,j,k))
    ! Fa_FeS(k)       = 0.0d0
    ! Fa_FeS2(k)      = 0.0d0
    ! Fa_FeOOH(k)     = 0.0d0
    ! Fa_FeOOH_PO4(k) = 0.0d0
    Fa_H2S(k)     = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,H2Samb,SEDECO(ng)%H2S(i,j,k))
    ! Fa_S0(k)        = 0.0d0
    Fa_SO4(k)     = FCa(1.0d0,SEDECO(ng)%poro(i,j,k),AdvecVelo,SO4amb,SEDECO(ng)%SO4(i,j,k))
#  endif

  !! sediment layers k = 1:Nsed
  DO k = 2,Nsed
        Fa_Tmp(k) = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%Tmp(i,j,k-1),SEDECO(ng)%Tmp(i,j,k))
        Fa_Sal(k) = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%Sal(i,j,k-1),SEDECO(ng)%Sal(i,j,k))
        Fa_O2(k)  = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%O2 (i,j,k-1),SEDECO(ng)%O2 (i,j,k))
        Fa_TA (k) = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%TA (i,j,k-1),SEDECO(ng)%TA (i,j,k))
        Fa_DIC(k) = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%DIC(i,j,k-1),SEDECO(ng)%DIC(i,j,k))
        Fa_N2(k)  = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%N2 (i,j,k-1),SEDECO(ng)%N2 (i,j,k))
#  if defined ORGANIC_MATTER
        ! Fa_DOC(k,1)   = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%DOCf(i,j,k-1),SEDECO(ng)%DOCf(i,j,k))
        ! Fa_DOC(k,2)   = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%DOCs(i,j,k-1),SEDECO(ng)%DOCs(i,j,k))
        ! Fa_POC(k,1)   = 0.0d0
        ! Fa_POC(k,2)   = 0.0d0
        ! Fa_POC(k,3)   = 0.0d0
#  endif
#  if defined NUTRIENTS
        Fa_NO3(k) = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%NO3(i,j,k-1),SEDECO(ng)%NO3(i,j,k))
        Fa_NH4(k) = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%NH4(i,j,k-1),SEDECO(ng)%NH4(i,j,k))
        Fa_PO4(k) = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%PO4(i,j,k-1),SEDECO(ng)%PO4(i,j,k))
#   if defined ORGANIC_MATTER
        ! Fa_DON(k,1)   = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%DONf(i,j,k-1),SEDECO(ng)%DONf(i,j,k))
        ! Fa_DON(k,2)   = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%DONs(i,j,k-1),SEDECO(ng)%DONs(i,j,k))
        ! Fa_PON(k,1)   = 0.0d0
        ! Fa_PON(k,2)   = 0.0d0
        ! Fa_PON(k,3)   = 0.0d0
        ! Fa_DOP(k,1)   = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%DOPf(i,j,k-1),SEDECO(ng)%DOPf(i,j,k))
        ! Fa_DOP(k,2)   = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%DOPs(i,j,k-1),SEDECO(ng)%DOPs(i,j,k))
        ! Fa_POP(k,1)   = 0.0d0
        ! Fa_POP(k,2)   = 0.0d0
        ! Fa_POP(k,3)   = 0.0d0
#   endif
#  endif
#  if defined SULFATE
        Fa_Mn2(k) = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%Mn2(i,j,k-1),SEDECO(ng)%Mn2(i,j,k))
        ! Fa_MnO2(k)      = 0.0d0
        Fa_Fe2(k) = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%Fe2(i,j,k-1),SEDECO(ng)%Fe2(i,j,k))
        ! Fa_FeS(k)       = 0.0d0
        ! Fa_FeS2(k)      = 0.0d0
        ! Fa_FeOOH(k)     = 0.0d0
        ! Fa_FeOOH_PO4(k) = 0.0d0
        Fa_H2S(k) = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%H2S(i,j,k-1),SEDECO(ng)%H2S(i,j,k))
        ! Fa_S0(k)        = 0.0d0
        Fa_SO4(k) = FCa(SEDECO(ng)%poro(i,j,k-1),SEDECO(ng)%poro(i,j,k),AdvecVelo,SEDECO(ng)%SO4(i,j,k-1),SEDECO(ng)%SO4(i,j,k))
#  endif
    ENDDO

    !! bottom layer k = Nsed+1
    k = Nsed+1
    Fa_Tmp(k)     = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%Tmp(i,j,k),sgd_Tmp_in)
    Fa_Sal(k)     = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%Sal(i,j,k),sgd_Sal_in)
    Fa_O2(k)      = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%O2 (i,j,k),sgd_DOx_in)
    Fa_TA (k)     = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%TA (i,j,k),sgd_TA_in)
    Fa_DIC(k)     = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%DIC(i,j,k),sgd_DIC_in)
    Fa_N2(k)      = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%N2 (i,j,k),8.0d0) ! constant influx of 8.0 [umol.N2 L-1]
#  if defined ORGANIC_MATTER
    ! Fa_DOC(k,1)   = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%DOCf(i,j,k))
    ! Fa_DOC(k,2)   = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%DOCs(i,j,k))
    ! Fa_POC(k,1)   = 0.0d0
    ! Fa_POC(k,2)   = 0.0d0
    ! Fa_POC(k,3)   = 0.0d0
#  endif
#  if defined NUTRIENTS
    Fa_NO3(k)     = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%NO3(i,j,k),sgd_NO3_in)
    Fa_NH4(k)     = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%NH4(i,j,k),sgd_NH4_in)
    Fa_PO4(k)     = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%PO4(i,j,k),sgd_PO4_in)
#   if defined ORGANIC_MATTER
    ! Fa_DON(k,1)   = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%DONf(i,j,k))
    ! Fa_DON(k,2)   = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%DONs(i,j,k))
    ! Fa_PON(k,1)   = 0.0d0
    ! Fa_PON(k,2)   = 0.0d0
    ! Fa_PON(k,3)   = 0.0d0
    ! Fa_DOP(k,1)   = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%DOPf(i,j,k))
    ! Fa_DOP(k,2)   = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%DOPs(i,j,k))
    ! Fa_POP(k,1)   = 0.0d0
    ! Fa_POP(k,2)   = 0.0d0
    ! Fa_POP(k,3)   = 0.0d0
#   endif
#  endif
#  if defined SULFATE
    Fa_Mn2(k)       = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%Mn2(i,j,k),10.0d0) ! constant influx of 10.0 [umol.Mn2 L-1]
    ! Fa_MnO2(k)      = 0.0d0
    Fa_Fe2(k)       = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%Fe2(i,j,k),1.0d0) ! constant influx of 1.0 [umol.Fe2 L-1]
    ! Fa_FeS(k)       = 0.0d0
    ! Fa_FeS2(k)      = 0.0d0
    ! Fa_FeOOH(k)     = 0.0d0
    ! Fa_FeOOH_PO4(k) = 0.0d0
    Fa_H2S(k)       = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%H2S(i,j,k),1.0d0) ! constant influx of 1.0 [umol.H2S L-1]
    ! Fa_S0(k)        = 0.0d0
    Fa_SO4(k)       = FCa(SEDECO(ng)%poro(i,j,k),1.0d0,AdvecVelo,SEDECO(ng)%SO4(i,j,k), &
                      (2.71235d0  / 35.16504d0)   * sgd_Sal_in        * rhos_d(k)          / 96.06d0       * 1000000) ! influx constant ratio of PSU 
    !                 (2.71 g.SO4 / 35.16 g.salt) PSU(g.salt / kg.sw) * rhos_d(kg.sw/L.sw) / (g.SO4/mol.SO4) (1000000 umol/mol)   values from https://www.coastalwiki.org/wiki/Salinity#:~:text=Salinity%20classification%20(Practical%20Salinity%20Scale%20S)%20of%20water%20bodies.&text=The%20salinity%20of%20seawater%20is,)%20or%20%E2%80%B0%20(permil).
#  endif
# endif

! ***************************** !
!!!    Reaction term
! ***************************** !

    !! Reaction rate constants depending on temperature
    DO k = 1,Nsed
        k6_Q10(k)  = k6  * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k7_Q10(k)  = k7  * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Berg2003)      (s-1)
        k8_Q10(k)  = k8  * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (G.A.Anggra2009)
        k9_Q10(k)  = k9  * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k10_Q10(k) = k10 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (G.A.Anggra2009)
        k11_Q10(k) = k11 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k12_Q10(k) = k12 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k13_Q10(k) = k13 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (G.A.Anggra2009)
        k14_Q10(k) = k14 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k15_Q10(k) = k15 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (G.A.Anggra2009)(s-1)
        k16_Q10(k) = k16 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k17_Q10(k) = k17 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k18_Q10(k) = k18 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k19_Q10(k) = k19 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)   (s-1)
        k20_Q10(k) = k20 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)   (s-1)
        k21_Q10(k) = k21 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)   (s-1)
        
        kdomf_Q10(k) = kdomf * (Q10_pri**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))  ! fast  
        kdoms_Q10(k) = kdoms * (Q10_pri**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))  ! slow  
        kpomf_Q10(k) = kpomf * (Q10_pri**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))  ! fast  
        kpoms_Q10(k) = kpoms * (Q10_pri**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))  ! slow  
    ENDDO
    

    !! Reaction rate calculatoin (functions Vdd, Vsd, Vss are defined in mod_geochem.F) -- !!
        ! Vdd : reaction with solute * solute (R6, ...)
        ! Vsd : reaction with solid * solute  (R7, ...)
        ! Vss : reaction with solid * solid   (R14)
    !! ----------------------------------------------------------------------------------- !!
    DO k = 1,Nsed
        vpoc(k,1) = rhos_d(k) * (1d0 - SEDECO(ng)%poro(i,j,k)) * kpomf_Q10(k) * SEDECO(ng)%POCf(i,j,k)  ! For POC_fast                               ! For DOC_slow
        vpoc(k,2) = rhos_d(k) * (1d0 - SEDECO(ng)%poro(i,j,k)) * kpoms_Q10(k) * SEDECO(ng)%POCs(i,j,k)  ! For POC_slow
        vpon(k,1) = rhos_d(k) * (1d0 - SEDECO(ng)%poro(i,j,k)) * kpomf_Q10(k) * SEDECO(ng)%PONf(i,j,k)  ! For PON_fast                               ! For DON_slow
        vpon(k,2) = rhos_d(k) * (1d0 - SEDECO(ng)%poro(i,j,k)) * kpoms_Q10(k) * SEDECO(ng)%PONs(i,j,k)  ! For PON_slow
        vpop(k,1) = rhos_d(k) * (1d0 - SEDECO(ng)%poro(i,j,k)) * kpomf_Q10(k) * SEDECO(ng)%POPf(i,j,k)  ! For POP_fast                               ! For DOP_slow
        vpop(k,2) = rhos_d(k) * (1d0 - SEDECO(ng)%poro(i,j,k)) * kpoms_Q10(k) * SEDECO(ng)%POPs(i,j,k)  ! For POP_slow
        vdoc(k,1) = SEDECO(ng)%poro(i,j,k) * kdomf_Q10(k) * SEDECO(ng)%DOCf(i,j,k)                                ! For DOC_fast
        vdoc(k,2) = SEDECO(ng)%poro(i,j,k) * kdoms_Q10(k) * SEDECO(ng)%DOCs(i,j,k)                                ! For DOC_slow
        vdon(k,1) = SEDECO(ng)%poro(i,j,k) * kdomf_Q10(k) * SEDECO(ng)%DONf(i,j,k)                                ! For DON_fast
        vdon(k,2) = SEDECO(ng)%poro(i,j,k) * kdoms_Q10(k) * SEDECO(ng)%DONs(i,j,k)                                ! For DON_slow
        vdop(k,1) = SEDECO(ng)%poro(i,j,k) * kdomf_Q10(k) * SEDECO(ng)%DOPf(i,j,k)                                ! For DOP_fast
        vdop(k,2) = SEDECO(ng)%poro(i,j,k) * kdoms_Q10(k) * SEDECO(ng)%DOPs(i,j,k)                                ! For DOP_slow
        vOC(k)   = vdoc(k,1) + vdoc(k,2)
        vON(k)   = vdon(k,1) + vdon(k,2)
        vOP(k)   = vdop(k,1) + vdop(k,2)
        v1(k)   = 0d0
        v2(k)   = 0d0
        v3(k)   = 0d0
        v4(k)   = 0d0
        v5(k)   = 0d0
        v6(k)   = Vdd(k6_Q10(k),SEDECO(ng)%poro(i,j,k),  SEDECO(ng)%NH4(i,j,k),      SEDECO(ng)%O2(i,j,k)            )
        v7(k)   = Vsd(k7_Q10(k),SEDECO(ng)%poro(i,j,k),  SEDECO(ng)%FeOOH(i,j,k),    SEDECO(ng)%PO4(i,j,k), rhos_d(k))
        v8(k)   = Vsd(k8_Q10(k),SEDECO(ng)%poro(i,j,k),  SEDECO(ng)%MnO2(i,j,k),     SEDECO(ng)%Fe2(i,j,k), rhos_d(k))
        v9(k)   = Vdd(k9_Q10(k),SEDECO(ng)%poro(i,j,k),  SEDECO(ng)%O2(i,j,k),       SEDECO(ng)%Mn2(i,j,k)           )
        v10a(k) = Vsd(k10_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%FeOOH_PO4(i,j,k),SEDECO(ng)%H2S(i,j,k), rhos_d(k))
        v10b(k) = Vsd(k10_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%FeOOH(i,j,k),    SEDECO(ng)%H2S(i,j,k), rhos_d(k))
        v11(k)  = Vdd(k11_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%Fe2(i,j,k),      SEDECO(ng)%O2(i,j,k)            )
        v12(k)  = Vsd(k12_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%MnO2(i,j,k),     SEDECO(ng)%H2S(i,j,k), rhos_d(k))
        v13(k)  = Vdd(k13_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%Fe2(i,j,k),      SEDECO(ng)%H2S(i,j,k)           )
        v14(k)  = Vss(k14_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%FeS(i,j,k),      SEDECO(ng)%S0(i,j,k),  rhos_d(k))
        v15(k)  = Vsd(k15_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%FeS(i,j,k),      SEDECO(ng)%H2S(i,j,k), rhos_d(k)) 
        v16(k)  = Vdd(k16_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%H2S(i,j,k),      SEDECO(ng)%O2(i,j,k)            )
        v17(k)  = Vsd(k17_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%FeS(i,j,k),      SEDECO(ng)%O2(i,j,k),  rhos_d(k))
        v18(k)  = Vsd(k18_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%FeS2(i,j,k),     SEDECO(ng)%O2(i,j,k),  rhos_d(k))
        if (SEDECO(ng)%H2S(i,j,k) <= H2Sstop) then  
           v19(k)  = Vsd(k19_Q10(k),SEDECO(ng)%poro(i,j,k),SEDECO(ng)%S0(i,j,k),(1d0 - (SEDECO(ng)%H2S(i,j,k)/H2Sstop)),rhos_d(k))
        else
           v19(k)  = 0d0
        endif
        v20(k)  = rhos_d(k) * (1d0 - SEDECO(ng)%poro(i,j,k)) * k20_Q10(k) * SEDECO(ng)%MnO2(i,j,k) 
        v21(k)  = rhos_d(k) * (1d0 - SEDECO(ng)%poro(i,j,k)) * k21_Q10(k) * SEDECO(ng)%FeOOH(i,j,k) 
        

# if defined sedORP
        !! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !!
        !! redox stages change depending on ORP input
        !! create condition of it here
        !! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !!
            !! ギブスエネルギーで反応が変化するようにするべきだ
# else

        !! Redox concentration limit in organic matter degradation (Berg et al.2003)
        if (SEDECO(ng)%O2(i,j,k) > O2lim) then            !! -- Redox of CH2O       (CH2O)  (µmol L-1) -- !!   if O2 > O2lim
            v1(k) = vOC(k)
            ! redoxlevel(k) = 1d0 !!for debug
        else if(SEDECO(ng)%NO3(i,j,k) > NO3lim) then      !! -- Denitrification     (NO3)   (µmol L-1) -- !!   if O2 < O2lim   &&   NO3 > NO3lim
            v1(k) = vOC(k) * (SEDECO(ng)%O2(i,j,k) / O2lim)
            v2(k) = vOC(k) * (1d0 - SEDECO(ng)%O2(i,j,k) / O2lim)
            ! redoxlevel(k) = 2d0 !!for debug
        else if(SEDECO(ng)%MnO2(i,j,k) > MnO2lim) then    !! -- Manganese reduction (MnO2)  (nmol g-1) -- !!   if O2 < O2lim   &&   NO3 < NO3lim   &&   Mn02 > MnO2lim
            v1(k) = vOC(k) * (SEDECO(ng)%O2 (i,j,k) / O2lim )
            v2(k) = vOC(k) * (1d0 - SEDECO(ng)%O2 (i,j,k) / O2lim ) &
                           * (SEDECO(ng)%NO3(i,j,k) / NO3lim)
            v3(k) = vOC(k) * (1d0 - SEDECO(ng)%O2 (i,j,k) / O2lim ) &
                           * (1d0 - SEDECO(ng)%NO3(i,j,k) / NO3lim)
            ! redoxlevel(k) = 3d0 !!for debug
        else if(SEDECO(ng)%FeOOH(i,j,k) > FeOOHlim) then  !! -- Iron reduction      (FeOOH) (nmol g-1) -- !!   if O2 < O2lim   &&   NO3 < NO3lim   &&   Mn02 < MnO2lim   &&   FeOOH > FeOOHlim
            v1(k) = vOC(k) * (SEDECO(ng)%O2  (i,j,k) / O2lim  )
            v2(k) = vOC(k) * (1d0 - SEDECO(ng)%O2  (i,j,k) / O2lim  ) &
                           * (SEDECO(ng)%NO3 (i,j,k) / NO3lim )
            v3(k) = vOC(k) * (1d0 - SEDECO(ng)%O2  (i,j,k) / O2lim  ) &
                           * (1d0 - SEDECO(ng)%NO3 (i,j,k) / NO3lim ) &
                           * (SEDECO(ng)%MnO2(i,j,k) / MnO2lim)
            v4(k) = vOC(k) * (1d0 - SEDECO(ng)%O2  (i,j,k) / O2lim  ) &
                           * (1d0 - SEDECO(ng)%NO3 (i,j,k) / NO3lim ) &
                           * (1d0 - SEDECO(ng)%MnO2(i,j,k) / MnO2lim)
            ! redoxlevel(k) = 4d0 !!for debug
        else if(SEDECO(ng)%SO4(i,j,k) > SO4lim) then      !! -- Sulfate reduction   (SO4)   (µmol L-1) -- !!   if O2 < O2lim   &&   NO3 < NO3lim   &&   Mn02 < MnO2lim   &&   FeOOH < FeOOHlim   &&   SO4 > SO4lim
            v1(k) = vOC(k) * (SEDECO(ng)%O2   (i,j,k) / O2lim   )
            v2(k) = vOC(k) * (1d0 - SEDECO(ng)%O2   (i,j,k) / O2lim   ) &
                           * (SEDECO(ng)%NO3  (i,j,k) / NO3lim  )
            v3(k) = vOC(k) * (1d0 - SEDECO(ng)%O2   (i,j,k) / O2lim   ) &
                           * (1d0 - SEDECO(ng)%NO3  (i,j,k) / NO3lim  ) &
                           * (SEDECO(ng)%MnO2 (i,j,k) / MnO2lim )
            v4(k) = vOC(k) * (1d0 - SEDECO(ng)%O2   (i,j,k) / O2lim   ) &
                           * (1d0 - SEDECO(ng)%NO3  (i,j,k) / NO3lim  ) &
                           * (1d0 - SEDECO(ng)%MnO2 (i,j,k) / MnO2lim ) &
                           * (SEDECO(ng)%FeOOH(i,j,k) / FeOOHlim)
            v5(k) = vOC(k) * (1d0 - SEDECO(ng)%O2   (i,j,k) / O2lim   ) &
                           * (1d0 - SEDECO(ng)%NO3  (i,j,k) / NO3lim  ) &
                           * (1d0 - SEDECO(ng)%MnO2 (i,j,k) / MnO2lim ) &
                           * (1d0 - SEDECO(ng)%FeOOH(i,j,k) / FeOOHlim)
            ! redoxlevel(k) = 5d0 !!for debug
        else                                              !! limit OM decomposition if SO4 < SO4lim    -- !!   if O2 < O2lim   &&   NO3 < NO3lim   &&   Mn02 < MnO2lim   &&   FeOOH < FeOOHlim   &&   SO4 < SO4lim
            v1(k) = vOC(k) * (SEDECO(ng)%O2   (i,j,k) / O2lim   )
            v2(k) = vOC(k) * (1d0 - SEDECO(ng)%O2   (i,j,k) / O2lim   ) &
                           * (SEDECO(ng)%NO3  (i,j,k) / NO3lim  )
            v3(k) = vOC(k) * (1d0 - SEDECO(ng)%O2   (i,j,k) / O2lim   ) &
                           * (1d0 - SEDECO(ng)%NO3  (i,j,k) / NO3lim  ) &
                           * (SEDECO(ng)%MnO2 (i,j,k) / MnO2lim )
            v4(k) = vOC(k) * (1d0 - SEDECO(ng)%O2   (i,j,k) / O2lim   ) &
                           * (1d0 - SEDECO(ng)%NO3  (i,j,k) / NO3lim  ) &
                           * (1d0 - SEDECO(ng)%MnO2 (i,j,k) / MnO2lim ) &
                           * (SEDECO(ng)%FeOOH(i,j,k) / FeOOHlim)
            v5(k) = vOC(k) * (1d0 - SEDECO(ng)%O2   (i,j,k) / O2lim   ) &
                           * (1d0 - SEDECO(ng)%NO3  (i,j,k) / NO3lim  ) &
                           * (1d0 - SEDECO(ng)%MnO2 (i,j,k) / MnO2lim ) &
                           * (1d0 - SEDECO(ng)%FeOOH(i,j,k) / FeOOHlim) &
                           * (SEDECO(ng)%SO4  (i,j,k) / SO4lim  )
            ! redoxlevel(k) = 5d0 !!for debug
        endif
# endif
    ENDDO

    R_Tmp(:) = 0.0d0
    R_Sal(:) = 0.0d0

    !! Net Production and Consumption from Reactions
    R_O2 (:) = -v1(:) -2d0*v6(:) -v9(:) -v11(:) -2d0*v16(:) -2d0*v17(:) -7d0*v18(:)
    R_TA (:) = 0.0d0 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    R_DIC(:) = v1(:) + v2(:) +v3(:) +v4(:) + v5(:)
    R_N2(:)  = 0.2d0*v2(:)
# if defined ORGANIC_MATTER
    R_DOC(:,1) = -vdoc(:,1) + OMFratio*vpoc(:,1) + OMSratio*vpoc(:,2) ! DOCf ! yt_edit confirm this section with masa
    R_DOC(:,2) = -vdoc(:,2) + OMFratio*vpoc(:,1) + OMSratio*vpoc(:,2) ! DOCs
    R_POC(:,1) = -vpoc(:,1) ! POCf
    R_POC(:,2) = -vpoc(:,2) ! POCs
# endif
# if defined NUTRIENTS
    R_NO3(:) = -0.8d0*v2(:) + v6(:)
    R_NH4(:) = vON(:) -v6(:)
    R_PO4(:) = vOP(:) -v7(:) +2d0*v10a(:)
#  if defined ORGANIC_MATTER
    R_DON(:,1) = -vdon(:,1) + OMFratio*vpon(:,1) + OMSratio*vpon(:,2) ! DONf
    R_DON(:,2) = -vdon(:,2) + OMFratio*vpon(:,1) + OMSratio*vpon(:,2) ! DONs
    R_PON(:,1) = -vpon(:,1) ! PONf
    R_PON(:,2) = -vpon(:,2) ! PONs
    R_DOP(:,1) = -vdop(:,1) + OMFratio*vpop(:,1) + OMSratio*vpop(:,2) ! DOPf
    R_DOP(:,2) = -vdop(:,2) + OMFratio*vpop(:,1) + OMSratio*vpop(:,2) ! DOPs
    R_POP(:,1) = -vpop(:,1) ! POPf
    R_POP(:,2) = -vpop(:,2) ! POPs
#  endif
# endif
# if defined SULFATE
    R_Mn2(:)  = 2d0*v3(:) +v8(:) -2d0*v9(:) +v12(:)
    R_MnO2(:) = -2d0*v3(:) -v8(:) +2d0*v9(:) -v12(:) -v20(:)
    R_Fe2(:)       = 4d0*v4(:) -2d0*v8(:) +2d0*v10a(:) -4d0*v11(:) +2d0*v10b(:) -v13(:) +v17(:) +2d0*v18(:) 
    R_FeS(:)       = v13(:) -v14(:) -4d0*v15(:) -v17(:)
    R_FeS2(:)      = v14(:) +4d0*v15(:) -2d0*v18(:)
    ! !!! yt_debug >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ! do k = 1, Nsed
    !   if (isnan(R_FeS2(k)) .or. abs(R_FeS2(k)) > huge(R_FeS2(k))) then
    !     write(*,*) 'yt_debug: sedecosys R_FeS2(k)      i =', i, '   j =', j, '   k =', k
    !     write(*,*) 'yt_debug:     R_FeS2(k) =', R_FeS2(k)
    !     write(*,*) 'yt_debug:     v14(k) =', v14(k)
    !     write(*,*) 'yt_debug:     v15(k) =', v15(k)
    !     write(*,*) 'yt_debug:     v18(k) =', v18(k)
    !   endif
    ! enddo
    ! !!! yt_debug <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    R_FeOOH(:)     = -4d0*v4(:) -v7(:) +2d0*v8(:) -2d0*v10b(:) +4d0*v11(:) -v21(:)
    R_FeOOH_PO4(:) = v7(:) -2d0*v10a(:)
    R_H2S(:) = 0.5d0*v5(:) -v10a(:) -v10b(:) -v12(:) -v13(:) -3d0*v15(:) -v16(:) +3d0*v19(:)
    R_S0(:)  =  v10a(:) +v10b(:) -v14(:) +v12(:) -4d0*v19(:) 
    R_SO4(:) = -0.5d0*v5(:) -v15(:) +v16(:) +v17(:) +4d0*v18(:) +v19(:)
# endif
    

! ***************************** !
!!!   Total Flux calculation
! ***************************** !
    !! Total Flux : (nmol cm-2 s-1)
    !! DBL Flux (k = 0) (Only surface diffusion is considered. Deposition added in POM.)
    FT_Tmp(1) = Fd_Tmp(1)
    FT_Sal(1) = Fd_Sal(1)
    FT_O2(1)  = Fd_O2(1)
    FT_TA(1)  = Fd_TA(1)
    FT_DIC(1) = Fd_DIC(1)
    FT_N2(1)  = Fd_N2(1)
# if defined ORGANIC_MATTER
    FT_DOC(1,1) = 0.0d0 ! no flux with water column
    FT_DOC(1,2) = 0.0d0 ! no flux with water column
    FT_POC(1,1) = Fdep_POC * OMFratio   ! Deposition of POCf
    FT_POC(1,2) = Fdep_POC * OMSratio   ! Deposition of POCs
    FT_POC(1,3) = Fdep_POC * OMNratio   ! Deposition of POCn
# endif
# if defined NUTRIENTS
    FT_NO3(1)   = Fd_NO3(1)
    FT_NH4(1)   = Fd_NH4(1)
    FT_PO4(1)   = Fd_PO4(1)
#  if defined ORGANIC_MATTER
    FT_DON(1,1) = 0.0d0 ! no flux with water column
    FT_DON(1,2) = 0.0d0 ! no flux with water column
    FT_PON(1,1) = Fdep_PON * OMFratio   ! Deposition of PONf
    FT_PON(1,2) = Fdep_PON * OMSratio   ! Deposition of PONs
    FT_PON(1,3) = Fdep_PON * OMNratio   ! Deposition of PONn
    FT_DOP(1,1) = 0.0d0 ! no flux with water column
    FT_DOP(1,2) = 0.0d0 ! no flux with water column
    FT_POP(1,1) = Fdep_POP * OMFratio   ! Deposition of POPf
    FT_POP(1,2) = Fdep_POP * OMSratio   ! Deposition of POPs
    FT_POP(1,3) = Fdep_POP * OMNratio   ! Deposition of POPn
#  endif
# endif
# if defined SULFATE
    FT_Mn2(1)       = Fd_Mn2(1)
    FT_Fe2(1)       = Fd_Fe2(1)
    FT_FeS(1)       = 0.0d0
    FT_FeS2(1)      = 0.0d0    
    FT_FeOOH_PO4(1) = 0.0d0    
    FT_H2S(1)       = Fd_H2S(1)
    FT_S0 (1)       = 0.0d0
    FT_SO4(1)       = Fd_SO4(1)
    !! if those sedimentations were included
    FT_MnO2(1)      = Fdep_MnO2
    FT_FeOOH(1)     = Fdep_FeOOH
# endif

    !! sediment column Flux (k = 1:Nsed)
    !! -- Diffusion and biodiffusion -- !!
    DO k = 2,Nsed
        FT_Tmp(k)   = Fd_Tmp(k)   + Fb_Tmp(k)
        FT_Sal(k)   = Fd_Sal(k)   + Fb_Sal(k)
        FT_O2(k)    = Fd_O2(k)    + Fb_O2(k)
        FT_TA (k)   = Fd_TA (k)   + Fb_TA (k)
        FT_DIC(k)   = Fd_DIC(k)   + Fb_DIC(k)
        FT_N2(k)    = Fd_N2(k)    + Fb_N2(k)
# if defined ORGANIC_MATTER
        FT_DOC(k,1) = Fd_DOC(k,1) + Fb_DOC(k,1)
        FT_DOC(k,2) = Fd_DOC(k,2) + Fb_DOC(k,2)
        FT_POC(k,1) =               Fb_POC(k,1)
        FT_POC(k,2) =               Fb_POC(k,2)
        FT_POC(k,3) =               Fb_POC(k,3)
# endif
# if defined NUTRIENTS
        FT_NO3(k)   = Fd_NO3(k)   + Fb_NO3(k)
        FT_NH4(k)   = Fd_NH4(k)   + Fb_NH4(k)
        FT_PO4(k)   = Fd_PO4(k)   + Fb_PO4(k)
#  if defined ORGANIC_MATTER
        FT_DON(k,1) = Fd_DON(k,1) + Fb_DON(k,1)
        FT_DON(k,2) = Fd_DON(k,2) + Fb_DON(k,2)
        FT_PON(k,1) =               Fb_PON(k,1)
        FT_PON(k,2) =               Fb_PON(k,2)
        FT_PON(k,3) =               Fb_PON(k,3)
        FT_DOP(k,1) = Fd_DOP(k,1) + Fb_DOP(k,1)
        FT_DOP(k,2) = Fd_DOP(k,2) + Fb_DOP(k,2)
        FT_POP(k,1) =               Fb_POP(k,1)
        FT_POP(k,2) =               Fb_POP(k,2)
        FT_POP(k,3) =               Fb_POP(k,3)
#  endif
# endif
# if defined SULFATE
        FT_Mn2(k)       = Fd_Mn2(k) + Fb_Mn2(k)
        FT_MnO2(k)      =             Fb_MnO2(k)
        FT_Fe2(k)       = Fd_Fe2(k) + Fb_Fe2(k)
        FT_FeS(k)       =             Fb_FeS(k)
        FT_FeS2(k)      =             Fb_FeS2(k)
        FT_FeOOH(k)     =             Fb_FeOOH(k)
        FT_FeOOH_PO4(k) =             Fb_FeOOH_PO4(k)
        FT_H2S(k)       = Fd_H2S(k) + Fb_H2S(k)
        FT_S0(k)        =             Fb_S0(k)
        FT_SO4(k)       = Fd_SO4(k) + Fb_SO4(k)
# endif
    ENDDO

# if defined SEDECO_CLOSED_BOTTOM_DIFFUSION_BOUNDARY
    !! DBL Flux (k = Nsed+1) (Closed bottom boundary for diffusion and biodiffusion.)
    FT_O2(Nsed+1)    = 0.0d0
    FT_DIC(Nsed+1)   = 0.0d0
    FT_N2(Nsed+1)    = 0.0d0
#  if defined ORGANIC_MATTER
    FT_DOC(Nsed+1,1) = 0.0d0
    FT_DOC(Nsed+1,2) = 0.0d0
    FT_POC(Nsed+1,1) = 0.0d0
    FT_POC(Nsed+1,2) = 0.0d0
    FT_POC(Nsed+1,3) = 0.0d0
#  endif
#  if defined NUTRIENTS
    FT_NO3(Nsed+1)   = 0.0d0
    FT_NH4(Nsed+1)   = 0.0d0
    FT_PO4(Nsed+1)   = 0.0d0
#   if defined ORGANIC_MATTER
    FT_DON(Nsed+1,1) = 0.0d0
    FT_DON(Nsed+1,2) = 0.0d0
    FT_PON(Nsed+1,1) = 0.0d0
    FT_PON(Nsed+1,2) = 0.0d0
    FT_PON(Nsed+1,3) = 0.0d0
    FT_DOP(Nsed+1,1) = 0.0d0
    FT_DOP(Nsed+1,2) = 0.0d0
    FT_POP(Nsed+1,1) = 0.0d0
    FT_POP(Nsed+1,2) = 0.0d0
    FT_POP(Nsed+1,3) = 0.0d0
#   endif
#  endif
#  if defined SULFATE
    FT_Mn2(Nsed+1)       = 0.0d0
    FT_Fe2(Nsed+1)       = 0.0d0
    FT_FeS(Nsed+1)       = 0.0d0
    FT_FeS2(Nsed+1)      = 0.0d0
    FT_FeOOH_PO4(Nsed+1) = 0.0d0
    FT_H2S(Nsed+1)       = 0.0d0
    FT_S0 (Nsed+1)       = 0.0d0
    FT_SO4(Nsed+1)       = 0.0d0
    FT_MnO2(Nsed+1)      = 0.0d0
    FT_FeOOH(Nsed+1)     = 0.0d0
#  endif
# else
    write(*,*) 'SEDIMENT_ECOSYS ERROR: No bottom boundary condition specified'
    error stop
# endif


    !! -- with Burial -- !!
# if defined SEDECO_BURIAL
    DO k = 2,Nsed+1
        FT_O2(k)    = FT_O2(k)    + Fc_O2(k)
        FT_DIC(k)   = FT_DIC(k)   + Fc_DIC(k)
        FT_N2(k)    = FT_N2(k)    + Fc_N2(k)
#  if defined ORGANIC_MATTER
        FT_DOC(k,1) = FT_DOC(k,1) + Fc_DOC(k,1)
        FT_DOC(k,2) = FT_DOC(k,2) + Fc_DOC(k,2)
        FT_POC(k,1) = FT_POC(k,1) + Fc_POC(k,1)
        FT_POC(k,2) = FT_POC(k,2) + Fc_POC(k,2)
        FT_POC(k,3) = FT_POC(k,3) + Fc_POC(k,3)
#  endif
#  if defined NUTRIENTS
        FT_NO3(k)   = FT_NO3(k)   + Fc_NO3(k)
        FT_NH4(k)   = FT_NH4(k)   + Fc_NH4(k)
        FT_PO4(k)   = FT_PO4(k)   + Fc_PO4(k)
#   if defined ORGANIC_MATTER
        FT_DON(k,1) = FT_DON(k,1) + Fc_DON(k,1)
        FT_DON(k,2) = FT_DON(k,2) + Fc_DON(k,2)
        FT_PON(k,1) = FT_PON(k,1) + Fc_PON(k,1)
        FT_PON(k,2) = FT_PON(k,2) + Fc_PON(k,2)
        FT_PON(k,3) = FT_PON(k,3) + Fc_PON(k,3)        
        FT_DOP(k,1) = FT_DOP(k,1) + Fc_DOP(k,1)
        FT_DOP(k,2) = FT_DOP(k,2) + Fc_DOP(k,2)
        FT_POP(k,1) = FT_POP(k,1) + Fc_POP(k,1)
        FT_POP(k,2) = FT_POP(k,2) + Fc_POP(k,2)
        FT_POP(k,3) = FT_POP(k,3) + Fc_POP(k,3)
#   endif
#  endif
#  if defined SULFATE
        FT_Mn2(k)       = FT_Mn2(k)       + Fc_Mn2(k)
        FT_MnO2(k)      = FT_MnO2(k)      + Fc_MnO2(k)
        FT_Fe2(k)       = FT_Fe2(k)       + Fc_Fe2(k)
        FT_FeS(k)       = FT_FeS(k)       + Fc_FeS(k)
        FT_FeS2(k)      = FT_FeS2(k)      + Fc_FeS2(k) 
        FT_FeOOH(k)     = FT_FeOOH(k)     + Fc_FeOOH(k) 
        FT_FeOOH_PO4(k) = FT_FeOOH_PO4(k) + Fc_FeOOH_PO4(k) 
        FT_H2S(k)       = FT_H2S(k)       + Fc_H2S(k) 
        FT_S0(k)        = FT_S0(k)        + Fc_S0(k) 
        FT_SO4(k)       = FT_SO4(k)       + Fc_SO4(k) 
#  endif
    ENDDO
# endif


    !! -- with Advection -- !!
# if defined SEDECO_ADVECTION
    DO k = 1,Nsed+1
        FT_O2(k)    = FT_O2(k)    + Fa_O2(k)
        FT_DIC(k)   = FT_DIC(k)   + Fa_DIC(k)
        FT_N2(k)    = FT_N2(k)    + Fa_N2(k)
#  if defined ORGANIC_MATTER
        ! FT_DOC(k,1) = FT_DOC(k,1) + Fa_DOC(k,1)
        ! FT_DOC(k,2) = FT_DOC(k,2) + Fa_DOC(k,2)
        ! FT_POC(k,1) = FT_POC(k,1) + Fa_POC(k,1)
        ! FT_POC(k,2) = FT_POC(k,2) + Fa_POC(k,2)
        ! FT_POC(k,3) = FT_POC(k,3) + Fa_POC(k,3)
#  endif
#  if defined NUTRIENTS
        FT_NO3(k)   = FT_NO3(k)   + Fa_NO3(k)
        FT_NH4(k)   = FT_NH4(k)   + Fa_NH4(k)
        FT_PO4(k)   = FT_PO4(k)   + Fa_PO4(k)
#   if defined ORGANIC_MATTER
        ! FT_DON(k,1) = FT_DON(k,1) + Fa_DON(k,1)
        ! FT_DON(k,2) = FT_DON(k,2) + Fa_DON(k,2)
        ! FT_PON(k,1) = FT_PON(k,1) + Fa_PON(k,1)
        ! FT_PON(k,2) = FT_PON(k,2) + Fa_PON(k,2)
        ! FT_PON(k,3) = FT_PON(k,3) + Fa_PON(k,3)        
        ! FT_DOP(k,1) = FT_DOP(k,1) + Fa_DOP(k,1)
        ! FT_DOP(k,2) = FT_DOP(k,2) + Fa_DOP(k,2)
        ! FT_POP(k,1) = FT_POP(k,1) + Fa_POP(k,1)
        ! FT_POP(k,2) = FT_POP(k,2) + Fa_POP(k,2)
        ! FT_POP(k,3) = FT_POP(k,3) + Fa_POP(k,3)
#   endif
#  endif
#  if defined SULFATE
        FT_Mn2(k)       = FT_Mn2(k)       + Fa_Mn2(k)
        ! FT_MnO2(k)      = FT_MnO2(k)      + Fa_MnO2(k)
        FT_Fe2(k)       = FT_Fe2(k)       + Fa_Fe2(k)
        ! FT_FeS(k)       = FT_FeS(k)       + Fa_FeS(k)
        ! FT_FeS2(k)      = FT_FeS2(k)      + Fa_FeS2(k) 
        ! FT_FeOOH(k)     = FT_FeOOH(k)     + Fa_FeOOH(k) 
        ! FT_FeOOH_PO4(k) = FT_FeOOH_PO4(k) + Fa_FeOOH_PO4(k) 
        FT_H2S(k)       = FT_H2S(k)       + Fa_H2S(k) 
        ! FT_S0(k)        = FT_S0(k)        + Fa_S0(k) 
        FT_SO4(k)       = FT_SO4(k)       + Fa_SO4(k) 
#  endif
    ENDDO
# endif


! ***************************** !
!!!   Miscellaneous other sources
! ***************************** !
    miscTmp(:) = 0;
    miscSal(:) = 0;
    miscO2 (:) = 0;
    miscTA (:) = 0;
    miscDIC(:) = 0;
    miscN2 (:) = 0;
# if defined ORGANIC_MATTER
    miscDOC(:,:) = 0;    ! 1:labile, 2:refractory
    miscPOC(:,:) = 0;    ! 1:labile, 2:refractory 3:non-degratable
# endif
# if defined NUTRIENTS
    miscNO3(:) = 0;
    miscNH4(:) = 0;
    miscPO4(:) = 0;
#  if defined ORGANIC_MATTER
    miscDON(:,:) = 0;    ! 1:labile, 2:refractory
    miscPON(:,:) = 0;    ! 1:labile, 2:refractory 3:non-degratable
    miscDOP(:,:) = 0;    ! 1:labile, 2:refractory
    miscPOP(:,:) = 0;    ! 1:labile, 2:refractory 3:non-degratable
#  endif
# endif
# if defined SULFATE
    miscMn2 (:) = 0;
    miscMnO2(:) = 0;
    miscFe2 (:) = 0;
    miscFeS (:) = 0;
    miscFeS2(:) = 0;
    miscFeOOH(:) = 0;
    miscFeOOH_PO4(:) = 0;
    miscH2S (:) = 0;
    miscS0  (:) = 0;
    miscSO4 (:) = 0;
# endif

!!! yuta_seagrass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# if defined SEAGRASS
#  if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! [umol/L s] = [umol/L s]
    miscO2 (:) = miscO2 (:) + sum(SEDECO(ng)%sg_dO2_dt (:,i,j,:),DIM=1) ! (umol L-1 s-1) change in DO  due to flux from seagrass (positive means seagrass to sediment)
    miscTA (:) = miscTA (:) + 0.0d0 !sum(SEDECO(ng)%sg_dTA_dt(:,i,j,:),DIM=1) ! (umol L-1 s-1) change in TA due to flux from seagrass (positive means seagrass to sediment)
    miscDIC(:) = miscDIC(:) + sum(SEDECO(ng)%sg_dDIC_dt(:,i,j,:),DIM=1) ! (umol L-1 s-1) change in DIC due to flux from seagrass (positive means seagrass to sediment)
#  endif
#  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    miscNO3(:) = miscNO3(:) + sum(SEDECO(ng)%sg_dNO3_dt(:,i,j,:),DIM=1) ! (umol L-1 s-1) change in NO3 due to flux from seagrass (positive means seagrass to sediment)
    miscNH4(:) = miscNH4(:) + sum(SEDECO(ng)%sg_dNH4_dt(:,i,j,:),DIM=1) ! (umol L-1 s-1) change in NH4 due to flux from seagrass (positive means seagrass to sediment)
    miscPO4(:) = miscPO4(:) + sum(SEDECO(ng)%sg_dPO4_dt(:,i,j,:),DIM=1) ! (umol L-1 s-1) change in PO4 due to flux from seagrass (positive means seagrass to sediment)
#  endif    
#  if defined SEAGRASS_ROOT_POM
    ! [nmol/g s] = [nmol/g s]   +            [umol/L s]                                [cm3/g] (1000 nmol/umol) (L/1000 cm3)
    miscPOC(:,1) = miscPOC(:,1) + OMFratio * sum(SEDECO(ng)%sg_dPOC_dt(:,i,j,:),DIM=1) / rhos_d(:) ! (nmol g-1 s-1) change in POC labile due to root death in sediment (positive means seagrass to sediment)
    miscPOC(:,2) = miscPOC(:,2) + OMSratio * sum(SEDECO(ng)%sg_dPOC_dt(:,i,j,:),DIM=1) / rhos_d(:) ! (nmol g-1 s-1) change in POC refractory due to root death in sediment (positive means seagrass to sediment)
    miscPOC(:,3) = miscPOC(:,3) + OMNratio * sum(SEDECO(ng)%sg_dPOC_dt(:,i,j,:),DIM=1) / rhos_d(:) ! (nmol g-1 s-1) change in POC non-degratable due to root death in sediment (positive means seagrass to sediment)
#   if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    miscPON(:,1) = miscPON(:,1) + OMFratio * sum(SEDECO(ng)%sg_dPON_dt(:,i,j,:),DIM=1) / rhos_d(:) ! (nmol g-1 s-1) change in PON labile due to root death in sediment (positive means seagrass to sediment)
    miscPON(:,2) = miscPON(:,2) + OMSratio * sum(SEDECO(ng)%sg_dPON_dt(:,i,j,:),DIM=1) / rhos_d(:) ! (nmol g-1 s-1) change in PON refractory due to root death in sediment (positive means seagrass to sediment)
    miscPON(:,3) = miscPON(:,3) + OMNratio * sum(SEDECO(ng)%sg_dPON_dt(:,i,j,:),DIM=1) / rhos_d(:) ! (nmol g-1 s-1) change in PON non-degratable due to root death in sediment (positive means seagrass to sediment)
    miscPOP(:,1) = miscPOP(:,1) + OMFratio * sum(SEDECO(ng)%sg_dPOP_dt(:,i,j,:),DIM=1) / rhos_d(:) ! (nmol g-1 s-1) change in POP labile due to root death in sediment (positive means seagrass to sediment)
    miscPOP(:,2) = miscPOP(:,2) + OMSratio * sum(SEDECO(ng)%sg_dPOP_dt(:,i,j,:),DIM=1) / rhos_d(:) ! (nmol g-1 s-1) change in POP refractory due to root death in sediment (positive means seagrass to sediment)
    miscPOP(:,3) = miscPOP(:,3) + OMNratio * sum(SEDECO(ng)%sg_dPOP_dt(:,i,j,:),DIM=1) / rhos_d(:) ! (nmol g-1 s-1) change in POP non-degratable due to root death in sediment (positive means seagrass to sediment)
#   endif  
#  endif  
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add


! ***************************** !
!!!   Mass balance equations
! ***************************** !

    !! Mass Balance cal (Concentration at each layer in sed column)
    DO k = 1,Nsed
        ! -- Temp -- !
        SEDECO(ng)%Tmp(i,j,k)  = massbalance_sed_d(SEDECO(ng)%Tmp(i,j,k),FT_Tmp(k),FT_Tmp(k+1),R_Tmp(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscTmp(k))
         ! -- Salt -- !
        SEDECO(ng)%Sal(i,j,k)  = massbalance_sed_d(SEDECO(ng)%Sal(i,j,k),FT_Sal(k),FT_Sal(k+1),R_Sal(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscSal(k))
         ! -- O2 -- !
        SEDECO(ng)%O2(i,j,k)  = massbalance_sed_d(SEDECO(ng)%O2(i,j,k),FT_O2(k),FT_O2(k+1),R_O2(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscO2(k))
        ! -- TA -- !
        SEDECO(ng)%TA(i,j,k) = massbalance_sed_d(SEDECO(ng)%TA(i,j,k),FT_TA(k),FT_TA(k+1),R_TA(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscTA(k))
        ! -- DIC -- !
        SEDECO(ng)%DIC(i,j,k) = massbalance_sed_d(SEDECO(ng)%DIC(i,j,k),FT_DIC(k),FT_DIC(k+1),R_DIC(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscDIC(k))
        ! -- N2 -- !
        SEDECO(ng)%N2(i,j,k)  = massbalance_sed_d(SEDECO(ng)%N2(i,j,k),FT_N2(k),FT_N2(k+1),R_N2(k), & 
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscN2(k))
        
# if defined ORGANIC_MATTER
        ! -- DOC -- !
        SEDECO(ng)%DOCf(i,j,k) = massbalance_sed_d(SEDECO(ng)%DOCf(i,j,k),FT_DOC(k,1),FT_DOC(k+1,1),R_DOC(k,1), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscDOC(k,1))  ! DOCf
        SEDECO(ng)%DOCs(i,j,k) = massbalance_sed_d(SEDECO(ng)%DOCs(i,j,k),FT_DOC(k,2),FT_DOC(k+1,2),R_DOC(k,2), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscDOC(k,2))  ! DOCs
        ! -- POC -- !
        SEDECO(ng)%POCf(i,j,k) = massbalance_sed_p(SEDECO(ng)%POCf(i,j,k),FT_POC(k,1),FT_POC(k+1,1),R_POC(k,1), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscPOC(k,1))  ! POCf
        SEDECO(ng)%POCs(i,j,k) = massbalance_sed_p(SEDECO(ng)%POCs(i,j,k),FT_POC(k,2),FT_POC(k+1,2),R_POC(k,2), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscPOC(k,2))  ! POCs
        SEDECO(ng)%POCn(i,j,k) = massbalance_sed_p(SEDECO(ng)%POCn(i,j,k),FT_POC(k,3),FT_POC(k+1,3),0d0, &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscPOC(k,3))  ! POCn
# endif
# if defined NUTRIENTS
#  if defined adsorption
        ! -- NO3 -- !
        SEDECO(ng)%NO3(i,j,k) = massbalance_sed_d(SEDECO(ng)%NO3(i,j,k),FT_NO3(k),FT_NO3(k+1),R_NO3(k), &
                                            dz(k),(SEDECO(ng)%poro(i,j,k)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(i,j,k))*Kad_NO3),dt,miscNO3(k)) 
        ! -- NH4 -- !
        SEDECO(ng)%NH4(i,j,k) = massbalance_sed_d(SEDECO(ng)%NH4(i,j,k),FT_NH4(k),FT_NH4(k+1),R_NH4(k), &
                                            dz(k),(SEDECO(ng)%poro(i,j,k)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(i,j,k))*Kad_NH4),dt,miscNH4(k)) 
        ! -- PO4 -- !
        SEDECO(ng)%PO4(i,j,k) = massbalance_sed_d(SEDECO(ng)%PO4(i,j,k),FT_PO4(k),FT_PO4(k+1),R_PO4(k), &
                                            dz(k),(SEDECO(ng)%poro(i,j,k)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(i,j,k))*Kad_PO4),dt,miscPO4(k))
#  else
        ! -- NO3 -- !
        SEDECO(ng)%NO3(i,j,k) = massbalance_sed_d(SEDECO(ng)%NO3(i,j,k),FT_NO3(k),FT_NO3(k+1),R_NO3(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscNO3(k)) 
        ! -- NH4 -- !
        SEDECO(ng)%NH4(i,j,k) = massbalance_sed_d(SEDECO(ng)%NH4(i,j,k),FT_NH4(k),FT_NH4(k+1),R_NH4(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscNH4(k)) 
        !!! yt_debug >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        ! if (isnan(SEDECO(ng)%NH4(i,j,k)) .or. abs(SEDECO(ng)%NH4(i,j,k)) > huge(SEDECO(ng)%NH4(i,j,k))) then
!        if (isnan(SEDECO(ng)%NH4(i,j,k)) .or. SEDECO(ng)%NH4(i,j,k) > 1.0d8 .or. SEDECO(ng)%NH4(i,j,k) < 0.0d0) then
        if (isnan(SEDECO(ng)%NH4(i,j,k))) then
          write(*,*) 'yt_debug: sedecosys SEDECO(ng)%NH4(i,j,k)      i =', i, '   j =', j, '   k =', k
          write(*,*) 'yt_debug:     SEDECO(ng)%NH4(i,j,k) =', SEDECO(ng)%NH4(i,j,k)
          write(*,*) 'yt_debug:     FT_NH4(k) =', FT_NH4(k)
          write(*,*) 'yt_debug:     FT_NH4(k+1) =', FT_NH4(k+1)
          write(*,*) 'yt_debug:     R_NH4(k) =', R_NH4(k)
          write(*,*) 'yt_debug:     dz(k) =', dz(k)
          write(*,*) 'yt_debug:     SEDECO(ng)%poro(i,j,k) =', SEDECO(ng)%poro(i,j,k)
          write(*,*) 'yt_debug:     dt =', dt
          STOP
        endif
        !!! yt_debug <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

        ! -- PO4 -- !
        SEDECO(ng)%PO4(i,j,k) = massbalance_sed_d(SEDECO(ng)%PO4(i,j,k),FT_PO4(k),FT_PO4(k+1),R_PO4(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscPO4(k))
             
#  endif
#  if defined ORGANIC_MATTER
        ! -- DON -- !
        SEDECO(ng)%DONf(i,j,k) = massbalance_sed_d(SEDECO(ng)%DONf(i,j,k),FT_DON(k,1),FT_DON(k+1,1),R_DON(k,1), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscDON(k,1))  ! DONf
        SEDECO(ng)%DONs(i,j,k) = massbalance_sed_d(SEDECO(ng)%DONs(i,j,k),FT_DON(k,2),FT_DON(k+1,2),R_DON(k,2), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscDON(k,2))  ! DONs
        ! -- PON -- !
        SEDECO(ng)%PONf(i,j,k) = massbalance_sed_p(SEDECO(ng)%PONf(i,j,k),FT_PON(k,1),FT_PON(k+1,1),R_PON(k,1), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscPON(k,1))  ! PONf
        SEDECO(ng)%PONs(i,j,k) = massbalance_sed_p(SEDECO(ng)%PONs(i,j,k),FT_PON(k,2),FT_PON(k+1,2),R_PON(k,2), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscPON(k,2))  ! PONs
        SEDECO(ng)%PONn(i,j,k) = massbalance_sed_p(SEDECO(ng)%PONn(i,j,k),FT_PON(k,3),FT_PON(k+1,3),0d0, &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscPON(k,3))  ! PONn
        ! -- DOP -- !
        SEDECO(ng)%DOPf(i,j,k) = massbalance_sed_d(SEDECO(ng)%DOPf(i,j,k),FT_DOP(k,1),FT_DOP(k+1,1),R_DOP(k,1), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscDOP(k,1))  ! DOPf
        SEDECO(ng)%DOPs(i,j,k) = massbalance_sed_d(SEDECO(ng)%DOPs(i,j,k),FT_DOP(k,2),FT_DOP(k+1,2),R_DOP(k,2), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscDOP(k,2))  ! DOPs
        ! -- POP -- !
        SEDECO(ng)%POPf(i,j,k) = massbalance_sed_p(SEDECO(ng)%POPf(i,j,k),FT_POP(k,1),FT_POP(k+1,1),R_POP(k,1), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscPOP(k,1))  ! POPf
        SEDECO(ng)%POPs(i,j,k) = massbalance_sed_p(SEDECO(ng)%POPs(i,j,k),FT_POP(k,2),FT_POP(k+1,2),R_POP(k,2), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscPOP(k,2))  ! POPs
        SEDECO(ng)%POPn(i,j,k) = massbalance_sed_p(SEDECO(ng)%POPn(i,j,k),FT_POP(k,3),FT_POP(k+1,3),0d0, &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscPOP(k,3))  ! POPn
#  endif
# endif
# if defined SULFATE
#  if defined adsorption
        ! -- Mn2 -- !
        SEDECO(ng)%Mn2(i,j,k) = massbalance_sed_d(SEDECO(ng)%Mn2(i,j,k),FT_Mn2(k),FT_Mn2(k+1),R_Mn2(k), &
                                            dz(k),(SEDECO(ng)%poro(i,j,k)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(i,j,k))*Kad_Mn2),dt,miscMn2(k)) 
        ! -- Fe2 -- !
        SEDECO(ng)%Fe2(i,j,k) = massbalance_sed_d(SEDECO(ng)%Fe2(i,j,k),FT_Fe2(k),FT_Fe2(k+1),R_Fe2(k), &
                                            dz(k),(SEDECO(ng)%poro(i,j,k)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(i,j,k))*Kad_Fe2),dt,miscFe2(k)) 
                                            
#  else
        ! -- Mn2 -- !
        SEDECO(ng)%Mn2(i,j,k) = massbalance_sed_d(SEDECO(ng)%Mn2(i,j,k),FT_Mn2(k),FT_Mn2(k+1),R_Mn2(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscMn2(k)) 
        ! -- Fe2 -- !
        SEDECO(ng)%Fe2(i,j,k) = massbalance_sed_d(SEDECO(ng)%Fe2(i,j,k),FT_Fe2(k),FT_Fe2(k+1),R_Fe2(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscFe2(k)) 
#  endif
        ! -- MnO2 -- !
        SEDECO(ng)%MnO2(i,j,k) = massbalance_sed_p(SEDECO(ng)%MnO2(i,j,k),FT_MnO2(k),FT_MnO2(k+1),R_MnO2(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscMnO2(k)) 
        ! -- FeS -- !
        SEDECO(ng)%FeS(i,j,k) = massbalance_sed_p(SEDECO(ng)%FeS(i,j,k),FT_FeS(k),FT_FeS(k+1),R_FeS(k),  &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscFeS(k)) 
        ! -- FeS2 -- !
        SEDECO(ng)%FeS2(i,j,k) = massbalance_sed_p(SEDECO(ng)%FeS2(i,j,k),FT_FeS2(k),FT_FeS2(k+1),R_FeS2(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscFeS2(k)) 
        ! -- FeOOH -- !
        SEDECO(ng)%FeOOH(i,j,k) = massbalance_sed_p(SEDECO(ng)%FeOOH(i,j,k),FT_FeOOH(k),FT_FeOOH(k+1),R_FeOOH(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscFeOOH(k)) 
        ! -- FeOOH=PO4 -- !
        SEDECO(ng)%FeOOH_PO4(i,j,k) = massbalance_sed_p(SEDECO(ng)%FeOOH_PO4(i,j,k),FT_FeOOH_PO4(k),FT_FeOOH_PO4(k+1), & 
                                                  R_FeOOH_PO4(k),dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscFeOOH_PO4(k)) 
        ! -- H2S -- !
        SEDECO(ng)%H2S(i,j,k) = massbalance_sed_d(SEDECO(ng)%H2S(i,j,k),FT_H2S(k),FT_H2S(k+1),R_H2S(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscH2S(k)) 
        ! -- S0 -- !
        SEDECO(ng)%S0(i,j,k) = massbalance_sed_p(SEDECO(ng)%S0(i,j,k),FT_S0(k),FT_S0(k+1),R_S0(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k),miscS0(k)) 
        ! -- SO4 -- !
        SEDECO(ng)%SO4(i,j,k) = massbalance_sed_d(SEDECO(ng)%SO4(i,j,k),FT_SO4(k),FT_SO4(k+1),R_SO4(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,miscSO4(k))

# endif
    ENDDO


! ***************************** !
!!!   set output parameters
! ***************************** !
# if defined SEDECO_ADVECTION
!! Return total flux of water --> DBL
    Flux_Tmp = FT_Tmp(1)
    Flux_Sal = FT_Sal(1) 
# endif
    Flux_DO  = FT_O2(1) 
    Flux_TA  = FT_TA(1)
    Flux_DIC = FT_DIC(1)
    Flux_N2  = FT_N2(1)
# if defined ORGANIC_MATTER
    Flux_POC(:) = FT_POC(1,:)
# endif
# if defined NUTRIENTS
    Flux_NO3 = FT_NO3(1)
    Flux_NH4 = FT_NH4(1)
    Flux_PO4 = FT_PO4(1)
#  if defined ORGANIC_MATTER
    Flux_PON(:) = FT_PON(1,:)
    Flux_POP(:) = FT_POP(1,:)
#  endif
# endif
# if defined SULFATE
    Flux_Mn2  = FT_Mn2(1)
    Flux_MnO2 = FT_MnO2(1)
    Flux_Fe2       = FT_Fe2(1)
    ! Flux_FeS       = FT_FeS(1)  ! yt_edit diffusion is not modeled so flux is zero
    ! Flux_FeS2      = FT_FeS2(1)  ! yt_edit diffusion is not modeled so flux is zero
    Flux_FeOOH     = FT_FeOOH(1)
    ! Flux_FeOOH_PO4 = FT_FeOOH_PO4(1)  ! yt_edit diffusion is not modeled so flux is zero
    Flux_H2S = FT_H2S(1)
    ! Flux_S0  = FT_S0(1)  ! yt_edit diffusion is not modeled so flux is zero
    Flux_SO4 = FT_SO4(1)
# endif

! ! ***************************** !
! !!!  Print section
! ! ***************************** !
! # if defined SEDIMENT_TESTMODE

!     time = time +dt  ! sec
!     year = time/86400d0/365d0 

!     if(time.ge.dsec) then

!         write(58,*) time/86400.d0, O2amb, SEDECO(ng)%O2(i,j,:)
!         write(80,*) time/86400.d0, DICamb, SEDECO(ng)%DIC(i,j,:)
!         write(81,*) time/86400.d0, N2amb, SEDECO(ng)%N2(i,j,:)

! #  if defined ORGANIC_MATTER
!         write(65,*) time/86400.d0, DOCfamb, SEDECO(ng)%DOCf(i,j,:)
!         write(82,*) time/86400.d0, DOCsamb, SEDECO(ng)%DOCs(i,j,:)
!         write(66,*) time/86400.d0, POCfamb, SEDECO(ng)%POCf(i,j,:)
!         write(83,*) time/86400.d0, POCsamb, SEDECO(ng)%POCs(i,j,:)
!         write(100,*) time/86400.d0, 0d0, SEDECO(ng)%POCn(i,j,:)
! #  endif
! #  if defined NUTRIENTS
!         write(61,*) time/86400.d0, NH4amb, SEDECO(ng)%NH4(i,j,:)
!         write(63,*) time/86400.d0, NO3amb, SEDECO(ng)%NO3(i,j,:)
!         write(64,*) time/86400.d0, PO4amb, SEDECO(ng)%PO4(i,j,:)
!     ! #   if defined ORGANIC_MATTER
!     !       write(67,*) time/86400.d0, DONamb, SEDECO(ng)%DON(i,j,:)
!     !       write(68,*) time/86400.d0, PONamb, SEDECO(ng)%PON(i,j,:)
!     !       write(69,*) time/86400.d0, DOPamb, SEDECO(ng)%DOP(i,j,:)
!     !       write(70,*) time/86400.d0, POPamb, SEDECO(ng)%POP(i,j,:)
!     ! #   endif
! #  endif
! # if defined SULFATE
!         write(84,*) time/86400.d0, Mn2amb, SEDECO(ng)%Mn2(i,j,:)
!         write(85,*) time/86400.d0, 0d0,    SEDECO(ng)%MnO2(i,j,:)
!         write(86,*) time/86400.d0, Fe2amb, SEDECO(ng)%Fe2(i,j,:)
!         write(87,*) time/86400.d0, 0d0,    SEDECO(ng)%FeS(i,j,:)
!         write(88,*) time/86400.d0, 0d0,    SEDECO(ng)%FeS2(i,j,:)
!         write(89,*) time/86400.d0, 0d0,    SEDECO(ng)%FeOOH(i,j,:)
!         write(90,*) time/86400.d0, 0d0,    SEDECO(ng)%FeOOH_PO4(i,j,:)
!         write(91,*) time/86400.d0, H2Samb, SEDECO(ng)%H2S(i,j,:)
!         write(93,*) time/86400.d0, 0d0,    SEDECO(ng)%S0(i,j,:)
!         write(94,*) time/86400.d0, SO4amb, SEDECO(ng)%SO4(i,j,:)
! # endif

!             !! for debug !!
!         !  write(96,'(*(g0:,", "))') time/86400.d0, Tamb, Samb, O2amb   &
!         !                         ,Dw_O2(0), 0d0, Fd_O2(0), 0d0, 0d0, FT_O2(0), 0d0, 0d0
!         !  write(97,'(*(g0:,", "))') time/86400.d0, SEDECO(ng)%Tmp(i,j,1), SEDECO(ng)%Sal(i,j,1), SEDECO(ng)%O2(i,j,1)    &
!         !                         ,Dw_O2(1), Ds_O2(1), Fd_O2(1), Fb_O2(1), Fc_O2(1), FT_O2(1), v1(1), R_O2(1)

!         ! write(96,'(*(g0:,", "))') time/86400.d0, SEDECO(ng)%Tmp(i,j,1), SEDECO(ng)%Sal(i,j,1), SEDECO(ng)%DOCf(i,j,1)    &
!         !                         ,Dw_DOC(1,1), 0d0, Fd_DOC(1,1), 0d0, 0d0, FT_DOC(1,1), 0d0, 0d0
!         ! write(97,'(*(g0:,", "))') time/86400.d0, SEDECO(ng)%Tmp(i,j,2), SEDECO(ng)%Sal(i,j,2), SEDECO(ng)%DOCf(i,j,2)    &
!         !                         ,Dw_DOC(2,1), Ds_DOC(2,1), Fd_DOC(2,1), Fb_DOC(2,1), Fc_DOC(2,1), FT_DOC(2,1), vdom(2,1), R_DOC(2,1)

!         ! write(97,'(*(g0:,", "))') time/86400.d0, vOM(10), v1(10),v2(10),v3(10),v4(10),v5(10)

!         ! write(98,*) time/86400d0, redoxlevel(:) !! for debug

!         write(99,*) dz(:), z(:), SEDECO(ng)%poro(i,j,:)
!         write(101,*) time/86400.d0, v1(:)
!         write(102,*) time/86400.d0, v2(:)
!         write(103,*) time/86400.d0, v3(:)
!         write(104,*) time/86400.d0, v4(:)
!         write(105,*) time/86400.d0, v5(:)
!         write(106,*) time/86400.d0, vOM(:)
!         write(107,*) time/86400.d0, R_O2(:)   !! for debug
!         write(108,*) time/86400.d0, Flux_DO   !! for debug

!         ! endif
!         write(*,*) 'Time (day): ', time/86400d0 

!         dsec=dsec+OUTPUT_INTERVAL*60.
!         !  time = time +dt  ! sec

!     endif
! # endif

    RETURN

  END SUBROUTINE sedecosys
#endif
END MODULE mod_sedecosys

#endif
