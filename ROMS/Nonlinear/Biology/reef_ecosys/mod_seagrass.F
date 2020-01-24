
!!!=== Copyright (c) 2012-2020 Takashi NAKAMURA  =====

#include "cppdefs.h"


!!!*************** SEAGRASS ***********************************************


MODULE mod_seagrass
  implicit none
  integer, parameter :: Nsg = 1    !! Number of seagrass groups
  TYPE T_SGRASS
    real(8), pointer :: Pg(:,:,:) 
    real(8), pointer :: R (:,:,:) 
    real(8), pointer :: QC(:,:,:)
#if defined CARBON_ISOTOPE
!  13C isotope
    real(8), pointer :: Q13C(:,:,:)
#endif
#if defined NUTRIENTS         
    real(8), pointer :: QN(:,:,:)
    real(8), pointer :: QP(:,:,:)
#endif
  END TYPE T_SGRASS

  TYPE (T_SGRASS), allocatable :: SGRASS(:)

CONTAINS

!!! **********************************************************************
!!!  set initial conditions for coral polyp model (Nakamura et al. 2013)
!!! **********************************************************************

  subroutine initialize_seagrass(ng, Ngrids, LBi, UBi, LBj, UBj)

    USE mod_geochem
    
    implicit none
! input parameters
    integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj
    real(8)  R13C
    integer i,j,n

    IF (ng.eq.1) allocate ( SGRASS(Ngrids) )

    allocate( SGRASS(ng)%Pg(Nsg,LBi:UBi,LBj:UBj)     )
    allocate( SGRASS(ng)%R (Nsg,LBi:UBi,LBj:UBj)     )
    allocate( SGRASS(ng)%QC(Nsg,LBi:UBi,LBj:UBj)     )
#if defined CARBON_ISOTOPE
    allocate( SGRASS(ng)%Q13C(Nsg,LBi:UBi,LBj:UBj)   )
#endif
#if defined NUTRIENTS         
    allocate( SGRASS(ng)%QN(Nsg,LBi:UBi,LBj:UBj)     )
    allocate( SGRASS(ng)%QP(Nsg,LBi:UBi,LBj:UBj)     )
#endif

!------------------------------------------
!  Set initial conditions
    do j=LBj,UBj
      do i=LBi,UBi
        do n=1,Nsg
!      seagrass internal conditions
          SGRASS(ng)%Pg(n,i,j) = 0.0d0
          SGRASS(ng)%R (n,i,j) = 0.0d0
          SGRASS(ng)%QC(n,i,j) = 15.0d0  !!!�Ă��Ƃ�
#if defined CARBON_ISOTOPE
          R13C = R13C_fromd13C(-15.0d0)
!        c13CH2O (n,i,j)=R13C/(1.+R13C)*CH2O(n,i,j)
          SGRASS(ng)%Q13C(n,i,j) = R13C * SGRASS(ng)%QC(n,i,j)
#endif
#if defined NUTRIENTS         
          SGRASS(ng)%QN(n,i,j) = 1.5d0  !!!�Ă��Ƃ�
          SGRASS(ng)%QP(n,i,j) = 0.1d0  !!!�Ă��Ƃ�
#endif
        enddo
      enddo
    enddo
    
    RETURN
    
  END SUBROUTINE initialize_seagrass

!!! **********************************************************************
!!!  Main program of seagrass model (Watanabe et al. 2012)
!!! **********************************************************************

  SUBROUTINE seagrass           &
!   input parameters
    ( ng, n, i, j    &   ! ng: nested grid number; n: seagrass compartment; i,j: position
    , PFD            &   ! Photon flux density (umol m-2 s-1)
    , rho_sw         &   ! Density of seawater (g cm-3)
    , DICamb         &   ! DIC (umol kg-1)
    , DOamb          &   ! DO (umol L-1)
#if defined NUTRIENTS         
    , NH4amb         &   ! NH4 concentration (umol L-1)
#endif
#if defined CARBON_ISOTOPE
    , DI13Camb       &   ! 13C of DIC (umol kg-1)
#endif
!   output parameters
    , DICuptake      &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
    , DOuptake       &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
#if defined NUTRIENTS         
    , NO3uptake      &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
    , NH4uptake      &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
    , PO4uptake      &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
#endif
#if defined CARBON_ISOTOPE
    , DI13Cuptake    &   ! DI13C uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
#endif
    )

!-----------------------------------------------------------------------
    USE mod_geochem
    
    implicit none

! input parameters
    integer, intent(in) :: ng, n, i, j    ! ng: nested grid number; n: seagrass compartment; i,j: position
    real(8), intent(in) :: PFD
    real(8), intent(in) :: rho_sw   
    real(8), intent(in) :: DICamb
    real(8), intent(in) :: DOamb
#if defined NUTRIENTS         
    real(8), intent(in) :: NH4amb
#endif
#if defined CARBON_ISOTOPE
    real(8), intent(in) :: DI13Camb
#endif
! output parameters
    real(8), intent(out) :: DICuptake
    real(8), intent(out) :: DOuptake
#if defined NUTRIENTS         
    real(8), intent(out) :: NO3uptake
    real(8), intent(out) :: NH4uptake
    real(8), intent(out) :: PO4uptake
#endif
#if defined CARBON_ISOTOPE
    real(8), intent(out) :: DI13Cuptake
#endif

! --- C:N:P ratio of seagrass ---
    real(8), parameter :: nc=27./599.d0 !M.J.Atkinson and SV Smith(1983)
    real(8), parameter :: pc=1./599.d0
! --- Photosynthesis  Parameters ---
!    real(8), parameter :: pmax =  51.3d0 ! Watanabe et al. 2013
!    real(8), parameter :: pIk  = 589.65d0
!    real(8), parameter :: p0   =  15.05d0
    real(8), parameter :: pmax =  55.81d0  ! Nakamura & Nakamori 2009
    real(8), parameter :: pIk  = 671.8d0   !  Model skill = 0.990
    real(8), parameter :: p0   =  21.62d0  !
#if defined NUTRIENTS         
    real(8) npref
    real(8) ldocn,ldocd
#endif
#if defined CARBON_ISOTOPE
    real(8), parameter :: a_phot  = -7.d-3+1.d0  ! -5 to -10 per mill (Dr. Miyajima, pers. comn.)
    real(8), parameter :: a_resp  =  0.d-3+1.d0  !
    real(8) R13C_DIC, R13C_QC
#endif

! --- Organic and Inorganic Production Rate -----------------

    SGRASS(ng)%Pg(n,i,j)= pmax*tanh(PFD/pIk)/3600.d0   !Light response curve [mmolC/m2/s]
    SGRASS(ng)%R (n,i,j)= p0/3600.d0   !Constant [mmolC/m2/s]
    
    IF(DICamb<=0.d0) THEN !-----For Error handling
      SGRASS(ng)%Pg(n,i,j) = 0.d0
    ENDIF
    IF(DOamb<=0.d0) THEN !-----For Error handling
      SGRASS(ng)%R (n,i,j) = 0.d0
    ENDIF
    
    DICuptake= SGRASS(ng)%Pg(n,i,j)-SGRASS(ng)%R (n,i,j)
    DOuptake = SGRASS(ng)%R (n,i,j)-SGRASS(ng)%Pg(n,i,j)

!!! ----- Isotope calculation ----------------------------------------------------
#if defined CARBON_ISOTOPE
    R13C_DIC  = DI13Camb/DICamb
    R13C_QC = SGRASS(ng)%Q13C(n,i,j) / SGRASS(ng)%QC(n,i,j)
    
    IF(DI13Camb<=0.d0) THEN !-----For Error handling
      R13C_DIC =0.d0
    ENDIF

    DI13Cuptake=SGRASS(ng)%Pg(n,i,j)*R13C_DIC*a_phot        &
                - SGRASS(ng)%R (n,i,j)*R13C_QC*a_resp
#endif
      
! --- Nutrient fluxes between water column and coral -----
#if defined NUTRIENTS         
!!! Under developing
    NO3uptake = 0.0d0
    NH4uptake = 0.0d0
    PO4uptake = 0.0d0
#endif
    RETURN
    
  END SUBROUTINE seagrass

END MODULE mod_seagrass

