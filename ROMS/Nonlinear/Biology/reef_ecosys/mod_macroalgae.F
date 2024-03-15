
!!!=== Copyright (c) 2012-2020 Takashi NAKAMURA  =====

#include "cppdefs.h"


!!!*************** MACROALGAE *****************************************


MODULE mod_macroalgae
#ifdef MACROALGAE
  implicit none
  integer, parameter :: Nag = 1    !! Number of algal groups

  TYPE T_ALGAE
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
  END TYPE T_ALGAE
  TYPE (T_ALGAE), allocatable :: ALGAE(:)
CONTAINS


!!! **********************************************************************
!!!  set initial conditions for algae model
!!! **********************************************************************

  SUBROUTINE allocate_macroalgae(ng, Ngrids, LBi, UBi, LBj, UBj)
    USE mod_geochem
    
    implicit none
    integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj

    IF (ng.eq.1) allocate ( ALGAE(Ngrids) )
    allocate( ALGAE(ng)%Pg(Nag,LBi:UBi,LBj:UBj)     )
    allocate( ALGAE(ng)%R (Nag,LBi:UBi,LBj:UBj)     )
    allocate( ALGAE(ng)%QC(Nag,LBi:UBi,LBj:UBj)     )
#if defined CARBON_ISOTOPE
    allocate( ALGAE(ng)%Q13C(Nag,LBi:UBi,LBj:UBj)   )
#endif
#if defined NUTRIENTS         
    allocate( ALGAE(ng)%QN(Nag,LBi:UBi,LBj:UBj)     )
    allocate( ALGAE(ng)%QP(Nag,LBi:UBi,LBj:UBj)     )
#endif

    RETURN

  END SUBROUTINE allocate_macroalgae


!!! **********************************************************************
!!!  set initial conditions for algae model
!!! **********************************************************************

  SUBROUTINE initialize_macroalgae(ng, LBi, UBi, LBj, UBj)
    USE mod_geochem
    
    implicit none
! input parameters
    integer, intent(in) :: ng, LBi, UBi, LBj, UBj
    real(8)  R13C
    integer i,j,n

!------------------------------------------
!  Set initial conditions
    do j=LBj,UBj
      do i=LBi,UBi
        do n=1,Nag
!      seagrass internal conditions
          ALGAE(ng)%Pg(n,i,j) = 0.0d0
          ALGAE(ng)%R (n,i,j) = 0.0d0
          ALGAE(ng)%QC(n,i,j) = 15.0d0  !!!�Ă��Ƃ�
#if defined CARBON_ISOTOPE
          R13C = R13C_fromd13C(-15.0d0)
!        c13CH2O (n,i,j)=R13C/(1.+R13C)*CH2O(n,i,j)
          ALGAE(ng)%Q13C(n,i,j) = R13C * ALGAE(ng)%QC(n,i,j)
#endif
#if defined NUTRIENTS         
          ALGAE(ng)%QN(n,i,j) = 1.5d0  !!!�Ă��Ƃ�
          ALGAE(ng)%QP(n,i,j) = 0.1d0  !!!�Ă��Ƃ�
#endif
        enddo
      enddo
    enddo
    
    RETURN
      
  END SUBROUTINE initialize_macroalgae

!!! **********************************************************************
!!!  Main program of algal model
!!! **********************************************************************

  SUBROUTINE macroalgae          &
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
! --- Photosynthesis Parameters ---
!    real(8), parameter :: pmax =  51.3d0  ! Nakamura & Nakamori 2009
!    real(8), parameter :: pIk  = 589.65d0
!    real(8), parameter :: p0   =  15.05d0 !
    real(8), parameter :: p1 =  0.02166d0 ! Nakamura & Nakamori 2009
    real(8), parameter :: p0 = 13.13d0    !  Model skill = 0.991
#if defined NUTRIENTS         
    real(8) npref
    real(8) ldocn,ldocd
#endif
#if defined CARBON_ISOTOPE
    real(8), parameter :: a_phot  = -20.0d-3+1.d0  ! -5 to -10 per mill (Dr. Miyajima, pers. comn.)
    real(8), parameter :: a_resp  =   0.0d-3+1.d0  !
    real(8) R13C_DIC, R13C_QC
#endif

! --- Organic and Inorganic Production Rate -----------------

!    ALGAE(ng)%Pg(n,i,j)= pmax*tanh(PFD/pIk)/3600.d0   !Light response curve [mmolC/m2/s]
    ALGAE(ng)%Pg(n,i,j)= p1*PFD/3600.d0   !Linear regression line [mmolC/m2/s]
    ALGAE(ng)%R (n,i,j)= p0/3600.d0   ! Constant [mmolC/m2/s]
    
    IF(DICamb<=0.d0) THEN !-----For Error handling
      ALGAE(ng)%Pg(n,i,j) = 0.d0
    ENDIF
    IF(DOamb<=0.d0) THEN !-----For Error handling
      ALGAE(ng)%R (n,i,j) = 0.d0
    ENDIF
    
    DICuptake= ALGAE(ng)%Pg(n,i,j)-ALGAE(ng)%R (n,i,j)
    DOuptake = ALGAE(ng)%R (n,i,j)-ALGAE(ng)%Pg(n,i,j)

!!! ----- Isotope calculation ----------------------------------------------------
#if defined CARBON_ISOTOPE
    R13C_DIC  = DI13Camb/DICamb
    R13C_QC = ALGAE(ng)%Q13C(n,i,j) / ALGAE(ng)%QC(n,i,j)
    
    IF(DI13Camb<=0.d0) THEN !-----For Error handling
      R13C_DIC =0.d0
    ENDIF

    DI13Cuptake=ALGAE(ng)%Pg(n,i,j)*R13C_DIC*a_phot        &
               - ALGAE(ng)%R (n,i,j)*R13C_QC*a_resp
#endif
      
! --- Nutrient fluxes between water column and coral -----
#if defined NUTRIENTS         
!!! Under developing
    NO3uptake = 0.0d0
    NH4uptake = 0.0d0
    PO4uptake = 0.0d0
#endif
    RETURN
  
  END SUBROUTINE macroalgae
#endif
END MODULE mod_macroalgae

