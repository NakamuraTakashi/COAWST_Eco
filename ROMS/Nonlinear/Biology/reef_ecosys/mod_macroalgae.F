
!!!=== Copyright (c) 2012-2025 Takashi NAKAMURA  =====

#include "cppdefs.h"


!!!*************** MACROALGAE *****************************************


MODULE mod_macroalgae
#ifdef MACROALGAE
  implicit none

  TYPE T_ALGAE
    real(8), pointer :: Pg(:,:,:) 
    real(8), pointer :: R (:,:,:) 
    real(8), pointer :: QC(:,:,:,:)
    real(8), pointer :: QN(:,:,:,:)
    real(8), pointer :: QP(:,:,:,:)
  END TYPE T_ALGAE
  TYPE (T_ALGAE), allocatable :: ALGAE(:)
CONTAINS


!!! **********************************************************************
!!!  set initial conditions for algae model
!!! **********************************************************************

  SUBROUTINE allocate_macroalgae(ng, Ngrids, LBi, UBi, LBj, UBj)
    USE mod_reef_ecosys_param
    
    implicit none
    integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj

    IF (ng.eq.1) allocate ( ALGAE(Ngrids) )
    allocate( ALGAE(ng)%Pg(Nag,LBi:UBi,LBj:UBj)     )
    allocate( ALGAE(ng)%R (Nag,LBi:UBi,LBj:UBj)     )
    allocate( ALGAE(ng)%QC(N_Csp,Nag,LBi:UBi,LBj:UBj)     )
    allocate( ALGAE(ng)%QN(N_Nsp,Nag,LBi:UBi,LBj:UBj)     )
    allocate( ALGAE(ng)%QP(N_Psp,Nag,LBi:UBi,LBj:UBj)     )

    RETURN

  END SUBROUTINE allocate_macroalgae


!!! **********************************************************************
!!!  set initial conditions for algae model
!!! **********************************************************************

  SUBROUTINE initialize_macroalgae(ng, LBi, UBi, LBj, UBj)
    USE mod_reef_ecosys_param
    
    implicit none
! input parameters
    integer, intent(in) :: ng, LBi, UBi, LBj, UBj
    real(8) :: R13C
    integer :: i,j,n

!------------------------------------------
!  Set initial conditions
    do j=LBj,UBj
      do i=LBi,UBi
        do n=1,Nag
!      seagrass internal conditions
          ALGAE(ng)%Pg(n,i,j) = 0.0d0
          ALGAE(ng)%R (n,i,j) = 0.0d0
          ALGAE(ng)%QC(iCt,n,i,j) = 15.0d0
          R13C = R13C_fromd13C(-15.0d0)
!          ALGAE(ng)%QC(iC13,i,j) = R13C * ALGAE(ng)%QC(iCt,n,i,j)
          ALGAE(ng)%QN(iNt,n,i,j) = 1.5d0
          ALGAE(ng)%QP(iPt,n,i,j) = 0.1d0
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
    , date           &   ! Date (days since x or elapsed days)
    , dt             &   ! Time step (sec)
    , PFD            &   ! Photon flux density (umol m-2 s-1)
    , rho_sw         &   ! Density of seawater (g cm-3)
    , DOamb          &   ! DO (umol L-1)
    , DICamb         &   ! DIC (umol L-1)
    , NO3amb         &   ! NO3 concentration (umol L-1)
    , NH4amb         &   ! NH4 concentration (umol L-1)
    , PO4amb         &   ! NH4 concentration (umol L-1)
!   output parameters
    , Flux_DO        &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    , Flux_DIC       &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    , Flux_NO3       &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    , Flux_NH4       &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    , Flux_PO4       &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    , Flux_DOC       &   ! DOC uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    , Flux_POC       &   ! POC uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    , Flux_DON       &   ! DON uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    , Flux_PON       &   ! PON uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    , Flux_DOP       &   ! DOP uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    , Flux_POP       &   ! POP uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    )

!-----------------------------------------------------------------------
    USE mod_reef_ecosys_param
    USE mod_geochem
    
    implicit none

! input parameters
    integer, intent(in) :: ng, n, i, j    ! ng: nested grid number; n: seagrass compartment; i,j: position
    real(8), intent(in) :: date
    real(8), intent(in) :: dt
    real(8), intent(in) :: PFD
    real(8), intent(in) :: rho_sw   
    real(8), intent(in) :: DOamb
    real(8), intent(in) :: DICamb(N_Csp)
    real(8), intent(in) :: NO3amb(N_Nsp)
    real(8), intent(in) :: NH4amb(N_Nsp)
    real(8), intent(in) :: PO4amb(N_Psp)
! output parameters
    real(8), intent(out) :: Flux_DO
    real(8), intent(out) :: Flux_DIC(N_Csp)
    real(8), intent(out) :: Flux_NO3(N_Nsp)
    real(8), intent(out) :: Flux_NH4(N_Nsp)
    real(8), intent(out) :: Flux_PO4(N_Psp)
    real(8), intent(out) :: Flux_DOC(N_Csp,Ndom)  ! DOC uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    real(8), intent(out) :: Flux_POC(N_Csp,Npom)  ! POC uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    real(8), intent(out) :: Flux_DON(N_Nsp,Ndom)   ! DON uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    real(8), intent(out) :: Flux_PON(N_Nsp,Npom)   ! PON uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    real(8), intent(out) :: Flux_DOP(N_Psp,Ndom)   ! DOP uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive
    real(8), intent(out) :: Flux_POP(N_Psp,Npom)   ! POP uptake rate (mmol m-2 s-1)  * direction of water column to algae is positive

! --- C:N:P ratio of seagrass ---
    real(8), parameter :: nc=27./599.d0 !M.J.Atkinson and SV Smith(1983)
    real(8), parameter :: pc=1./599.d0
! --- Photosynthesis Parameters ---
!    real(8), parameter :: pmax =  51.3d0  ! Nakamura & Nakamori 2009
!    real(8), parameter :: pIk  = 589.65d0
!    real(8), parameter :: p0   =  15.05d0 !
    real(8), parameter :: p1 =  0.02166d0 ! Nakamura & Nakamori 2009
    real(8), parameter :: p0 = 13.13d0    !  Model skill = 0.991
    real(8) npref
    real(8) ldocn,ldocd
#if defined CARBON_ISOTOPE
    real(8), parameter :: aC_phot(2)  = [  1.d0, -20.0d-3+1.d0 ]  ! -5 to -10 per mill (Dr. Miyajima, pers. comn.)
    real(8), parameter :: aC_resp(2)  = [  1.d0,   0.0d-3+1.d0 ]!
    real(8) R13C_DIC, R13C_QC
#endif
    real(8) :: rQC(N_Csp),rDIC(N_Csp)

    integer :: isp

    Flux_DIC(:) = 0.0d0
    Flux_DO = 0.0d0
    Flux_DOC(:,:) = 0.0d0
    Flux_POC(:,:) = 0.0d0
    Flux_NO3(:) = 0.0d0
    Flux_NH4(:) = 0.0d0
    Flux_PO4(:) = 0.0d0
    Flux_DON(:,:) = 0.0d0
    Flux_DOP(:,:) = 0.0d0
    Flux_PON(:,:) = 0.0d0
    Flux_POP(:,:) = 0.0d0

! --- Organic and Inorganic Production Rate -----------------

!    ALGAE(ng)%Pg(n,i,j)= pmax*tanh(PFD/pIk)/3600.d0   !Light response curve [mmolC/m2/s]
    ALGAE(ng)%Pg(n,i,j)= p1*PFD/3600.d0   !Linear regression line [mmolC/m2/s]
    ALGAE(ng)%R (n,i,j)= p0/3600.d0   ! Constant [mmolC/m2/s]
    
    IF(DICamb(iCt)<=0.d0) THEN !-----For Error handling
      ALGAE(ng)%Pg(n,i,j) = 0.d0
    ENDIF
    IF(DOamb<=0.d0) THEN !-----For Error handling
      ALGAE(ng)%R (n,i,j) = 0.d0
    ENDIF
    
    Flux_DO = ALGAE(ng)%R (n,i,j)-ALGAE(ng)%Pg(n,i,j)
    DO isp=1,N_Csp
      rDIC  = Iso_ratio( DICamb(isp), DICamb(iCt) )
      rQC = Iso_ratio( ALGAE(ng)%QC(isp,n,i,j), ALGAE(ng)%QC(iCt,n,i,j) )
  
      Flux_DIC(isp)= ALGAE(ng)%Pg(n,i,j)*rDIC*aC_phot(isp)        &
                   - ALGAE(ng)%R (n,i,j)*rQC *aC_resp(isp)
    ENDDO
      
! --- Nutrient fluxes between water column and coral -----
!!! Under developing
!    Flux_NO3(:) = 
!    Flux_NH4(:) = 
!    Flux_PO4(:) = 

    RETURN
  
  END SUBROUTINE macroalgae
#endif
END MODULE mod_macroalgae

