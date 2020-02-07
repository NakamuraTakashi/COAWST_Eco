
!!!=== Copyright (c) 2013-2020 Takashi NAKAMURA  =====

#include "cppdefs.h"


!!!**** MODULE OF SEDIMENT ECOSYSTEM MODEL ************************************
#if defined SEDIMENT_EMPIRICAL
!!! **********************************************************************
!!!  Empirical sediment model
!!! **********************************************************************
MODULE mod_sedecosys
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
        SEDECO(ng)%QC(i,j) = 15.0d0  !!!�Ă��Ƃ�
# if defined CARBON_ISOTOPE
        R13C = R13C_fromd13C(-18.0d0)
!        c13CH2O (n,i,j)=R13C/(1.+R13C)*CH2O(n,i,j)
        SEDECO(ng)%Q13C(i,j) = R13C * SEDECO(ng)%QC(i,j)
# endif
# if defined NUTRIENTS         
        SEDECO(ng)%QN(i,j) = 1.5d0  !!!�Ă��Ƃ�
        SEDECO(ng)%QP(i,j) = 0.1d0  !!!�Ă��Ƃ�
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

!!! ----- Isotope calculation (�ΊD���̉e���܂�����ĂȂ�!!!!)----------
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

END MODULE mod_sedecosys


#else         
!!! **********************************************************************
!!!  Sediment model
!!! **********************************************************************

MODULE mod_sedecosys

  implicit none

  integer, parameter :: Nsed= 50    !! Number of sediment grids

  TYPE T_SEDECO

    real(8), pointer :: Pg(:,:) 
    real(8), pointer :: R (:,:) 
    real(8), pointer :: G (:,:) 
    real(8), pointer :: poro(:,:,:) 
    real(8), pointer :: Tmp(:,:,:) 
    real(8), pointer :: Sal(:,:,:) 
    real(8), pointer :: TA (:,:,:) 
    real(8), pointer :: DIC(:,:,:)
    real(8), pointer :: DOx(:,:,:) 
    real(8), pointer :: pH (:,:,:) 
    real(8), pointer :: Warg(:,:,:) 
# if defined ORGANIC_MATTER
    real(8), pointer :: DOC(:,:,:)
    real(8), pointer :: POC(:,:,:)
# endif
# if defined CARBON_ISOTOPE
!  13C isotope
    real(8), pointer :: DI13C(:,:,:)
    real(8), pointer :: DO13C(:,:,:)
    real(8), pointer :: PO13C(:,:,:)
# endif
# if defined NUTRIENTS
!  Nutrients dynamics
    real(8), pointer :: NO3(:,:,:)
!    real(8), pointer :: NO2(:,:,:)
    real(8), pointer :: NH4(:,:,:)
    real(8), pointer :: PO4(:,:,:)
#  if defined ORGANIC_MATTER
    real(8), pointer :: DON(:,:,:)
    real(8), pointer :: PON(:,:,:)
    real(8), pointer :: DOP(:,:,:)
    real(8), pointer :: POP(:,:,:)
#  endif
# endif
  END TYPE T_SEDECO

  TYPE (T_SEDECO), allocatable :: SEDECO(:)

CONTAINS

!!! **********************************************************************
!!!  set initial conditions for sediment ecosystem model
!!! **********************************************************************

  SUBROUTINE initialize_sedecosys(ng, Ngrids, LBi, UBi, LBj, UBj)

    USE mod_geochem
    
    implicit none
! input parameters
    integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj
    real(8)  R13C
    integer i,j,k

    IF (ng.eq.1) allocate ( SEDECO(Ngrids) )

    allocate( SEDECO(ng)%Pg(LBi:UBi,LBj:UBj)     )
    allocate( SEDECO(ng)%R (LBi:UBi,LBj:UBj)     )
    allocate( SEDECO(ng)%G (LBi:UBi,LBj:UBj)     )
    allocate( SEDECO(ng)%poro(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%Tmp(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%Sal(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%TA (LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%DIC(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%DOx(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%pH (LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%Warg(LBi:UBi,LBj:UBj,Nsed) )
# if defined ORGANIC_MATTER
    allocate( SEDECO(ng)%DOC(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%POC(LBi:UBi,LBj:UBj,Nsed) )
# endif

# if defined CARBON_ISOTOPE
    allocate( SEDECO(ng)%DI13C(LBi:UBi,LBj:UBj,Nsed) )
#  if defined ORGANIC_MATTER
    allocate( SEDECO(ng)%DO13C(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%PO13C(LBi:UBi,LBj:UBj,Nsed) )
#  endif
# endif

# if defined NUTRIENTS
    allocate( SEDECO(ng)%NO3(LBi:UBi,LBj:UBj,Nsed) )
!    allocate( SEDECO(ng)%NO2(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%NH4(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%PO4(LBi:UBi,LBj:UBj,Nsed) )
#  if defined ORGANIC_MATTER
    allocate( SEDECO(ng)%DON(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%PON(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%DOP(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%POP(LBi:UBi,LBj:UBj,Nsed) )
#  endif
# endif     

!  Set initial conditions
    do k=1,Nsed
      do j=LBj,UBj
        do i=LBi,UBi
          SEDECO(ng)%Pg(i,j)=0.d0
          SEDECO(ng)%R (i,j)=0.d0
          SEDECO(ng)%G (i,j)=0.d0

          SEDECO(ng)%poro(i,j,k) = 0.45d0    ! Morse and Mackenzie 1990
          SEDECO(ng)%Tmp(i,j,k) = 29.0d0     
          SEDECO(ng)%Sal(i,j,k) = 34.0d0
          SEDECO(ng)%TA (i,j,k) = 2232.0d0   !TA  (umol kg-1)
          SEDECO(ng)%DIC(i,j,k) = 1915.0d0   !DIC (umol kg-1) 
          SEDECO(ng)%DOx(i,j,k) = 200.0d0      !DO  (umol L-1)
          SEDECO(ng)%pH (i,j,k) = 7.8d0       !pH
          SEDECO(ng)%Warg(i,j,k) = 3.0d0      !Aragonite sturation stat
# if defined ORGANIC_MATTER
          SEDECO(ng)%DOC(i,j,k) = 120.0d0     !DOC  (umol L-1) 
          SEDECO(ng)%POC(i,j,k) = 50.0d0     !POC  (umol L-1) 
# endif

# if defined CARBON_ISOTOPE
          R13C=R13C_fromd13C(0.7d0)
          SEDECO(ng)%DI13C(i,j,k) = R13C * SEDECO(ng)%DIC(i,j,k)
#  if defined ORGANIC_MATTER
          R13C=R13C_fromd13C(-15.0d0)
          SEDECO(ng)%DO13C(i,j,k) =  R13C * SEDECO(ng)%DOC(i,j,k)
          R13C=R13C_fromd13C(-15.0d0)
          SEDECO(ng)%PO13C(i,j,k) =  R13C * SEDECO(ng)%POC(i,j,k)
# endif
# endif

# if defined NUTRIENTS
          SEDECO(ng)%NO3(i,j,k) = 0.5d0   !NO3  (umol L-1) 
!          SEDECO(ng)%NO2(i,j,k) = 0.02d0  !NO2  (umol L-1) 
          SEDECO(ng)%NH4(i,j,k) = 0.5d0   !NH4  (umol L-1) 
          SEDECO(ng)%PO4(i,j,k) = 0.03d0  !PO4  (umol L-1) 
#  if defined ORGANIC_MATTER                 
          SEDECO(ng)%DON(i,j,k) = 30.0d0   !DON  (umol L-1) 
          SEDECO(ng)%PON(i,j,k) = 6.0d0  !PON  (umol L-1) 
          SEDECO(ng)%DOP(i,j,k) = 2.0d0   !DOP  (umol L-1) 
          SEDECO(ng)%POP(i,j,k) = 0.4d0 !POP  (umol L-1) 
#  endif
# endif     
        enddo
      enddo
    enddo

    RETURN

  END SUBROUTINE initialize_sedecosys
      

!!! **********************************************************************
!!!  Main program of sediment ecosystem model
!!! **********************************************************************

  SUBROUTINE sedecosys           &
!   input parameters
    ( ng, i, j       &   ! ng: nested grid number; i,j: position
    , dt             &   ! Time step (sec)
    , PFD            &   ! Photon flux density (umol m-2 s-1)
    , Tamb           &   ! Tmp: Temperature (oC)
    , Samb           &   ! Sal: Salinity (PSU)
    , DICamb         &   ! DIC: Total dissolved inorganic carbon (DIC: umol kg-1)
    , TAamb          &   ! TA : Total alkalinity (TA: umol kg-1)
    , DOamb          &   ! DOx: Dissolved oxygen (umol L-1)
# if defined ORGANIC_MATTER
    , DOCamb         &   ! DOC: Dissolved organic carbon (DOC: umol L-1)
    , POCamb         &   ! POC: Particulate organic carbon (DOC: umol L-1)
# endif
# if defined CARBON_ISOTOPE
    , DI13Camb       &   ! DI13C: 13C of DIC (umol kg-1)
# endif
# if defined NUTRIENTS
    , NO3amb         &   ! NO3: NO3 (umol L-1)
!    , NO2amb         &   ! NO2: NO2 (umol L-1)
    , NH4amb         &   ! NH4: NH4 (umol L-1)
    , PO4amb         &   ! PO4: PO4 (umol L-1)
#  if defined ORGANIC_MATTER
    , DONamb         &   ! DON: Dissolved organic nitrogen (DON: umol L-1)
    , PONamb         &   ! PON: Particulate organic nitrogen (PON: umol L-1)
    , DOPamb         &   ! DOP: Dissolved organic phosporius (DOP: umol L-1)
    , POPamb         &   ! POP: Particulate organic phosporius (POP: umol L-1)
#  endif
# endif
# if defined ORGANIC_MATTER
    , Fdep_POC       &   ! POC deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#  if defined NUTRIENTS
    , Fdep_PON       &   ! PON deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Fdep_POP       &   ! POP deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#  endif
# endif
    , tau_amb        &   ! bottom shear stress (N m-2)
    , Fsed           &   ! sedimentation rate (??)
!   output parameters
    , Flux_Tmp       &   ! Temperature flux (K cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_Sal       &   ! Salinity  flux (cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_DIC       &   ! DIC flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_TA        &   ! TA  flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_DO        &   ! DO  flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
# if defined ORGANIC_MATTER
    , Flux_DOC       &   ! DOC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_POC       &   ! POC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
# endif
# if defined CARBON_ISOTOPE
    , Flux_DI13C     &   ! DI13C flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
# endif
# if defined NUTRIENTS
    , Flux_NO3       &   ! NO3 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
!    , Flux_NO2       &   ! NO2 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_NH4       &   ! NH4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_PO4       &   ! PO4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#  if defined ORGANIC_MATTER
    , Flux_DON       &   ! DON flux (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_PON       &   ! PON flux (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_DOP       &   ! DOP flux (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_POP       &   ! POP flux (nmol cm-2 s-1) * direction of water column to sediment is positive
#  endif
# endif
    )

!
!-----------------------------------------------------------------------
!                                                                       
!                     rho point    Face                                 
!                       (i,j)                                           
!                    _____|______  _0    Sea bottom                     
!                   /     |      /|                                     
!      z     Layer /___________ / |                                     
!                  |           |  |_1                                   
!     dz(1) {   1  |           | /|                                     
!                  |___________|/ |                                     
!                  |           |  |_2    sediment column                
!     dz(2) {   2  |           | /|                                     
!                  |___________|/ | :                                   
!                  |           |  :                                     
!               :  :           :  |_Nsed-1                              
!               :  :           : /|                                     
!               :  |___________|/ |                                     
!                  |           |  |_Nsed                                
!  dz(Nsed) { Nsed |           | /                                      
!                  |___________|/                                       
!                                                                       
!                                                                       
!   A vertical section of the sediment grid showing sediment column.    
!-----------------------------------------------------------------------
!!! Krumins et al., (2013)���Q�l�ɂ��č��̂��ǂ�����!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    USE mod_geochem
    
    implicit none

! input parameters
    integer, intent(in) :: ng, i, j
    real(8), intent(in) :: dt
    real(8), intent(in) :: PFD      
    real(8), intent(in) :: Tamb     
    real(8), intent(in) :: Samb     
    real(8), intent(in) :: DICamb   
    real(8), intent(in) :: TAamb    
    real(8), intent(in) :: DOamb    
# if defined ORGANIC_MATTER
    real(8), intent(in) :: DOCamb   
    real(8), intent(in) :: POCamb   
# endif
# if defined CARBON_ISOTOPE
    real(8), intent(in) :: DI13Camb 
# endif
# if defined NUTRIENTS
    real(8), intent(in) :: NO3amb   
!    real(8), intent(in) :: NO2amb   
    real(8), intent(in) :: NH4amb   
    real(8), intent(in) :: PO4amb   
#  if defined ORGANIC_MATTER
    real(8), intent(in) :: DONamb   
    real(8), intent(in) :: PONamb   
    real(8), intent(in) :: DOPamb   
    real(8), intent(in) :: POPamb   
#  endif
# endif
# if defined ORGANIC_MATTER
    real(8), intent(in) :: Fdep_POC   
#  if defined NUTRIENTS
    real(8), intent(in) :: Fdep_PON   
    real(8), intent(in) :: Fdep_POP   
#  endif
# endif
    real(8), intent(in) :: tau_amb  
    real(8), intent(in) :: Fsed
! output parameters
    real(8), intent(out) :: Flux_Tmp
    real(8), intent(out) :: Flux_Sal
    real(8), intent(out) :: Flux_DIC
    real(8), intent(out) :: Flux_TA 
    real(8), intent(out) :: Flux_DO 
# if defined ORGANIC_MATTER
    real(8), intent(out) :: Flux_DOC
    real(8), intent(out) :: Flux_POC
# endif
# if defined CARBON_ISOTOPE
    real(8), intent(out) :: Flux_DI13C
# endif
# if defined NUTRIENTS
    real(8), intent(out) :: Flux_NO3
!    real(8), intent(out) :: Flux_NO2
    real(8), intent(out) :: Flux_NH4
    real(8), intent(out) :: Flux_PO4
#  if defined ORGANIC_MATTER
    real(8), intent(out) :: Flux_DON
    real(8), intent(out) :: Flux_PON
    real(8), intent(out) :: Flux_DOP
    real(8), intent(out) :: Flux_POP
#  endif
# endif

!!!------------Set parameters  ----------------------------------
     real(8), parameter :: dz = 1.0d0  ! (cm) grid tickness
!---- Sediment dissolution parameters (Yamamoto et al., 2012)
!     Dissolution = a_diss*Warg + b_diss
    real(8), parameter :: a_diss = -0.0027d0
    real(8), parameter :: b_diss =  0.0081d0
!---- Photosynthetic rate parameters for Epilithic algal community (Larkum et al. 2003)
!     Pg(1)=Pgmax*(1.0d0-exp(-a_phot*PFD/Pgmax))
    real(8), parameter :: Pg_max  =15.0d-1  !(nmol cm-2 s-1) 
    real(8), parameter :: a_phot = 0.18d0   !(no dimension) (Pgmax/a_phot = 91 umol photons m-2 s-1)
    real(8), parameter :: Kp_NH4 = 0.2d0    !(umol L-1)  Sugimiti et al. 2009 for phytoplankton
    real(8), parameter :: Kp_NO3 = 2.0d0    !(umol L-1)  Sugimiti et al. 2009 for phytoplankton
    real(8), parameter :: Kp_PO4 = 0.17d0   !(umol L-1)  Sugimiti et al. 2009 for phytoplankton
    real(8), parameter :: psi    = 1.5d0    !(L umol-1)  Sugimiti et al. 2009 for phytoplankton
!---- Assimilation ratio --------------------------
    real(8), parameter :: r_assim =  0.5d0       !(no dim.)  !!!�Ă��Ƃ�
!---- Aerobic respiration --------------------------
    real(8), parameter :: Rp_max =  1.0d-3     !(nmol cm-3 s-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Krp_POC = 1.0d0     !(umol L-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Krp_DO =  10.0d0     !(umol L-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Rd_max =  5.0d-3     !(nmol cm-3 s-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Krd_DOC = 120.0d0    !(umol L-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Krd_DO =  10.0d0     !(umol L-1)  !!!�Ă��Ƃ�

    real(8), parameter :: Rdoc_max =  1.0d-3     !(nmol cm-3 s-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Krdoc_DO =  10.0d0     !(umol L-1)  !!!�Ă��Ƃ�
!---- Decomposition --------------------------
    real(8), parameter :: Dp2d_max =1.0d-1     !(nmol cm-3 s-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Kp2d_POC = 10.0d0     !(umol L-1)  !!!�Ă��Ƃ�
# if defined NUTRIENTS
!---- Nitrification --------------------------
!    NH4+ + 2 O2 -> NO3- + H2O + 2H+
    real(8), parameter :: Nit_max =  10.0d0 !!!10.0d0  !(nmol cm-3 s-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Kn_NH4 =  250.0d0   !(umol L-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Kn_DO =  5.0d0   !(umol L-1)  !!!�Ă��Ƃ�
!!    NH4+ + (3/2)O2 -> NO2- + H2O + 2H+
!    real(8), parameter :: Nit1_max =  10.0d0 !!!10.0d0  !(nmol cm-3 s-1)  !!!�Ă��Ƃ�
!    real(8), parameter :: Kn1_NH4 =  250.0d0   !(umol L-1)  !!!�Ă��Ƃ�
!    real(8), parameter :: Kn1_DO =  5.0d0   !(umol L-1)  !!!�Ă��Ƃ�
!!    NO2- + (1/2)O2 -> NO3-
!    real(8), parameter :: Nit2_max =  10.0d0 !!!10.0d0  !(nmol cm-3 s-1)  !!!�Ă��Ƃ�
!    real(8), parameter :: Kn2_NO2 =  250.0d0   !(umol L-1)  !!!�Ă��Ƃ�
!    real(8), parameter :: Kn2_DO =  5.0d0   !(umol L-1)  !!!�Ă��Ƃ�
!---- Denitrification --------------------------
!    NO3- + H+ +(5/4)CH2O -> (1/2)N2 + (5/4)CO2 + (7/4)H2O
    real(8), parameter :: DNd_max =  1.0d-2 !!!10.0d0  !(nmol cm-3 s-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Kdnd_DOC = 100.0d0   !(umol L-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Kdnd_NO3 =  250.0d0   !(umol L-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Kdnd_DO =  10.0d0   !(umol L-1)  !!!�Ă��Ƃ�
    real(8), parameter :: DNp_max =  1.0d-2 !!!10.0d0  !(nmol cm-3 s-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Kdnp_POC =  5.0d0   !(umol L-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Kdnp_NO3 = 250.0d0   !(umol L-1)  !!!�Ă��Ƃ�
    real(8), parameter :: Kdnp_DO =  10.0d0   !(umol L-1)  !!!�Ă��Ƃ�
!---- C:N:P ratio --------------------------
    real(8), parameter :: rCNph = 106.0d0/16.0d0   ! (no dim.) algal C:N ratio (Redfield ratio)
    real(8), parameter :: rCPph = 106.0d0/1.0d0    ! (no dim.) algal C:P ratio (Redfield ratio)
# endif

    integer :: k

    real(8) :: rho_sw(0:Nsed)

    real(8) :: Pg(Nsed)
    real(8) :: V_PFD
    real(8) :: Dp2doc(Nsed), Rpoc(Nsed), Rdoc(Nsed)
# if defined NUTRIENTS
    real(8) :: Dp2don(Nsed), Rpon(Nsed), Rdon(Nsed)
    real(8) :: Dp2dop(Nsed), Rpop(Nsed), Rdop(Nsed)
# endif
    real(8) :: Gn(Nsed)
    real(8) :: cCO3, TK
    real(8) :: Diff, t_dbl
# if defined NUTRIENTS
    real(8) :: NH4ass(Nsed), NO3ass(Nsed)!, NO2ass(Nsed)
    real(8) :: PO4ass(Nsed)
    real(8) :: V_NH4, V_NO3, V_PO4
    real(8) :: Nit(Nsed) !, Nit1(Nsed), Nit2(Nsed)
    real(8) :: DNd(Nsed), DNp(Nsed)
    real(8) :: DNdoc(Nsed), DNpoc(Nsed)
    real(8) :: DNdon(Nsed), DNpon(Nsed)
    real(8) :: DNdop(Nsed), DNpop(Nsed)
# endif
    real(8) :: F_Tmp(0:Nsed)
    real(8) :: F_Sal(0:Nsed)
    real(8) :: F_DIC(0:Nsed)
    real(8) :: F_TA (0:Nsed)
    real(8) :: F_DO (0:Nsed)
# if defined ORGANIC_MATTER
    real(8) :: F_DOC(0:Nsed)
    real(8) :: F_POC(0:Nsed)
# endif
# if defined CARBON_ISOTOPE
    real(8) :: F_DI13C(0:Nsed)
# endif
# if defined NUTRIENTS
    real(8) :: F_NO3(0:Nsed)
!    real(8) :: F_NO2(0:Nsed)
    real(8) :: F_NH4(0:Nsed)
    real(8) :: F_PO4(0:Nsed)
#  if defined ORGANIC_MATTER
    real(8) :: F_DON(0:Nsed)
    real(8) :: F_PON(0:Nsed)
    real(8) :: F_DOP(0:Nsed)
    real(8) :: F_POP(0:Nsed)
#  endif
# endif
    real(8) :: Tmp_bot
    real(8) :: Sal_bot
    real(8) :: DIC_bot
    real(8) :: TA_bot 
    real(8) :: DOx_bot
# if defined ORGANIC_MATTER
    real(8) :: DOC_bot
    real(8) :: POC_bot
# endif
# if defined CARBON_ISOTOPE
    real(8) :: DI13C_bot
# endif
# if defined NUTRIENTS
    real(8) :: NO3_bot
!    real(8) :: NO2_bot
    real(8) :: NH4_bot
    real(8) :: PO4_bot
#  if defined ORGANIC_MATTER
    real(8) :: DON_bot
    real(8) :: PON_bot
    real(8) :: DOP_bot
    real(8) :: POP_bot
#  endif
# endif

# if defined TESTMODE
!  Output
    real(8), parameter :: OUTPUT_INTERVAL = 5.0d0     ! Output interval (min)
    real(8), save :: time = 0.d0 !sec
    real(8), save :: dsec = 0.d0 !sec
    real(8) :: TKamb 
    real(8) :: pHamb 
    real(8) :: cCO3amb 
    real(8) :: Wargamb
# endif


!!!---- Diffusive flux calculations --------------------------

!---- Water-sediment fluxes (nmol cm-2 s-1) --------------------------

    Diff = 2.0d-5   !(cm2 s-1) Diffusion coefficient
    t_dbl = 10.0d-2  !(cm)      Diffusive boundary layer tickness
    rho_sw(0) = densSW( Tamb,Samb )  ! Density of seawater [g/cm3]

    
    F_Tmp(0) = Diff*( Tamb  - SEDECO(ng)%Tmp(i,j,1) )/t_dbl
    F_Sal(0) = Diff*( Samb  - SEDECO(ng)%Sal(i,j,1) )/t_dbl
    F_DIC(0) = Diff*( DICamb- SEDECO(ng)%DIC(i,j,1) )*rho_sw(0)/t_dbl
    F_TA(0)  = Diff*( TAamb - SEDECO(ng)%TA (i,j,1) )*rho_sw(0)/t_dbl
    F_DO(0)  = Diff*( DOamb - SEDECO(ng)%DOx(i,j,1) )/t_dbl
# if defined ORGANIC_MATTER
    F_DOC(0) = Diff*( DOCamb - SEDECO(ng)%DOC(i,j,1) )/t_dbl
    F_POC(0) = Diff*( POCamb - SEDECO(ng)%POC(i,j,1) )/t_dbl + Fdep_POC  ! diffusion + deposition flux
# endif
# if defined CARBON_ISOTOPE
    F_DI13C(0) = Diff*( DI13Camb - SEDECO(ng)%DI13C(i,j,1) )*rho_sw(0)/t_dbl
# endif
# if defined NUTRIENTS
    F_NO3(0) = Diff*( NO3amb - SEDECO(ng)%NO3(i,j,1) )/t_dbl
!    F_NO2(0) = Diff*( NO2amb - SEDECO(ng)%NO2(i,j,1) )/t_dbl
    F_NH4(0) = Diff*( NH4amb - SEDECO(ng)%NH4(i,j,1) )/t_dbl
    F_PO4(0) = Diff*( PO4amb - SEDECO(ng)%PO4(i,j,1) )/t_dbl
#  if defined ORGANIC_MATTER
    F_DON(0) = Diff*( DONamb - SEDECO(ng)%DON(i,j,1) )/t_dbl
    F_PON(0) = Diff*( PONamb - SEDECO(ng)%PON(i,j,1) )/t_dbl + Fdep_PON  ! diffusion + deposition flux
    F_DOP(0) = Diff*( DOPamb - SEDECO(ng)%DOP(i,j,1) )/t_dbl
    F_POP(0) = Diff*( POPamb - SEDECO(ng)%POP(i,j,1) )/t_dbl + Fdep_POP  ! diffusion + deposition flux
#  endif
# endif

!---- Sediment column fluxes (nmol cm-2 s-1) --------------------------

    DO k=1,Nsed-1
      Diff = 1.0d-5   !(cm2 s-1) Diffusion coefficient
      rho_sw(k) = densSW( SEDECO(ng)%Tmp(i,j,k),SEDECO(ng)%Sal(i,j,k) )  ! Density of seawater [g/cm3]

      F_Tmp(k) = Diff*( SEDECO(ng)%Tmp(i,j,k) - SEDECO(ng)%Tmp(i,j,k+1) )/dz
      F_Sal(k) = Diff*( SEDECO(ng)%Sal(i,j,k) - SEDECO(ng)%Sal(i,j,k+1) )/dz
      F_DIC(k) = Diff*( SEDECO(ng)%DIC(i,j,k) - SEDECO(ng)%DIC(i,j,k+1) )*rho_sw(k)/dz
      F_TA(k)  = Diff*( SEDECO(ng)%TA (i,j,k) - SEDECO(ng)%TA (i,j,k+1) )*rho_sw(k)/dz
      F_DO(k)  = Diff*( SEDECO(ng)%DOx(i,j,k) - SEDECO(ng)%DOx(i,j,k+1) )/dz
# if defined ORGANIC_MATTER
      F_DOC(k) = Diff*( SEDECO(ng)%DOC(i,j,k) - SEDECO(ng)%DOC(i,j,k+1) )/dz
      F_POC(k) = Diff*( SEDECO(ng)%POC(i,j,k) - SEDECO(ng)%POC(i,j,k+1) )/dz
# endif
# if defined CARBON_ISOTOPE
      F_DI13C(k) = Diff*( SEDECO(ng)%DI13C(i,j,k) - SEDECO(ng)%DI13C(i,j,k+1) )*rho_sw(k)/dz
# endif
# if defined NUTRIENTS
      F_NO3(k) = Diff*( SEDECO(ng)%NO3(i,j,k) - SEDECO(ng)%NO3(i,j,k+1) )/dz
!      F_NO2(k) = Diff*( SEDECO(ng)%NO2(i,j,k) - SEDECO(ng)%NO2(i,j,k+1) )/dz
      F_NH4(k) = Diff*( SEDECO(ng)%NH4(i,j,k) - SEDECO(ng)%NH4(i,j,k+1) )/dz
      F_PO4(k) = Diff*( SEDECO(ng)%PO4(i,j,k) - SEDECO(ng)%PO4(i,j,k+1) )/dz
#  if defined ORGANIC_MATTER
      F_DON(k) = Diff*( SEDECO(ng)%DON(i,j,k) - SEDECO(ng)%DON(i,j,k+1) )/dz
      F_PON(k) = Diff*( SEDECO(ng)%PON(i,j,k) - SEDECO(ng)%PON(i,j,k+1) )/dz
      F_DOP(k) = Diff*( SEDECO(ng)%DOP(i,j,k) - SEDECO(ng)%DOP(i,j,k+1) )/dz
      F_POP(k) = Diff*( SEDECO(ng)%POP(i,j,k) - SEDECO(ng)%POP(i,j,k+1) )/dz
#  endif
# endif
    ENDDO

!---- Bottom boundary (nmol cm-2 s-1) --------------------------

    Diff = 1.0d-5   !(cm2 s-1) Diffusion coefficient
    rho_sw(Nsed) = densSW( SEDECO(ng)%Tmp(i,j,Nsed),SEDECO(ng)%Sal(i,j,Nsed) )  ! Density of seawater [g/cm3]

!   Closed boundary condition
    Tmp_bot = SEDECO(ng)%Tmp(i,j,Nsed)
    Sal_bot = SEDECO(ng)%Sal(i,j,Nsed)
    DIC_bot = SEDECO(ng)%DIC(i,j,Nsed)
    TA_bot  = SEDECO(ng)%TA (i,j,Nsed)
    DOx_bot = SEDECO(ng)%DOx(i,j,Nsed)
# if defined ORGANIC_MATTER
    DOC_bot = SEDECO(ng)%DOC(i,j,Nsed)
    POC_bot = SEDECO(ng)%POC(i,j,Nsed)
# endif
# if defined CARBON_ISOTOPE
    DI13C_bot = SEDECO(ng)%DI13C(i,j,Nsed)
# endif
# if defined NUTRIENTS
    NO3_bot = SEDECO(ng)%NO3(i,j,Nsed)
!    NO2_bot = SEDECO(ng)%NO2(i,j,Nsed)
    NH4_bot = SEDECO(ng)%NH4(i,j,Nsed)
    PO4_bot = SEDECO(ng)%PO4(i,j,Nsed)
#  if defined ORGANIC_MATTER
    DON_bot = SEDECO(ng)%DON(i,j,Nsed)
    PON_bot = SEDECO(ng)%PON(i,j,Nsed)
    DOP_bot = SEDECO(ng)%DOP(i,j,Nsed)
    POP_bot = SEDECO(ng)%POP(i,j,Nsed)
#  endif
# endif
      
    F_Tmp(Nsed) = Diff*( SEDECO(ng)%Tmp(i,j,Nsed) - Tmp_bot )/dz
    F_Sal(Nsed) = Diff*( SEDECO(ng)%Sal(i,j,Nsed) - Sal_bot )/dz
    F_DIC(Nsed) = Diff*( SEDECO(ng)%DIC(i,j,Nsed) - DIC_bot )*rho_sw(Nsed)/dz
    F_TA(Nsed)  = Diff*( SEDECO(ng)%TA (i,j,Nsed) - TA_bot  )*rho_sw(Nsed)/dz
    F_DO(Nsed)  = Diff*( SEDECO(ng)%DOx(i,j,Nsed) - DOx_bot )/dz
# if defined ORGANIC_MATTER
    F_DOC(Nsed) = Diff*( SEDECO(ng)%DOC(i,j,Nsed) - DOC_bot )/dz
    F_POC(Nsed) = Diff*( SEDECO(ng)%POC(i,j,Nsed) - POC_bot )/dz
# endif
# if defined CARBON_ISOTOPE
    F_DI13C(Nsed) = Diff*( SEDECO(ng)%DI13C(i,j,Nsed) - DI13C_bot )*rho_sw(Nsed)/dz
# endif
# if defined NUTRIENTS
    F_NO3(Nsed) = Diff*( SEDECO(ng)%NO3(i,j,Nsed) - NO3_bot )/dz
!    F_NO2(Nsed) = Diff*( SEDECO(ng)%NO2(i,j,Nsed) - NO2_bot )/dz
    F_NH4(Nsed) = Diff*( SEDECO(ng)%NH4(i,j,Nsed) - NH4_bot )/dz
    F_PO4(Nsed) = Diff*( SEDECO(ng)%PO4(i,j,Nsed) - PO4_bot )/dz
#  if defined ORGANIC_MATTER
    F_DON(Nsed) = Diff*( SEDECO(ng)%DON(i,j,Nsed) - DON_bot )/dz
    F_PON(Nsed) = Diff*( SEDECO(ng)%PON(i,j,Nsed) - PON_bot )/dz
    F_DOP(Nsed) = Diff*( SEDECO(ng)%DOP(i,j,Nsed) - DOP_bot )/dz
    F_POP(Nsed) = Diff*( SEDECO(ng)%POP(i,j,Nsed) - POP_bot )/dz
#  endif
# endif


!!!---- Reaction term calculations --------------------------

    DO k=1,Nsed

!---- Photosynthetic rate (nmolO2 cm-3 s-1) --------------------------
      IF (k==1) THEN
        V_PFD = 1.0d0-exp(-a_phot*PFD/Pg_max)
# if defined NUTRIENTS
        V_NH4 = SEDECO(ng)%NH4(i,j,k)/(Kp_NH4+SEDECO(ng)%NH4(i,j,k))
        V_NO3 = SEDECO(ng)%NO3(i,j,k)/(Kp_NO3+SEDECO(ng)%NO3(i,j,k))  &
                 *exp( - psi * SEDECO(ng)%NO3(i,j,k) )   
        V_PO4 = SEDECO(ng)%PO4(i,j,k)/(Kp_PO4+SEDECO(ng)%PO4(i,j,k))

        Pg(k) = Pg_max/dz * V_PFD * min( V_NH4+V_NO3, V_PO4 )

# else
        Pg(k) = Pg_max/dz * V_PFD
# endif
      ELSE
        Pg(k) = 0.0d0
      ENDIF 

# if defined NUTRIENTS
!      Nutrients assimilation rates
      NH4ass(k) = Pg(k)/rCNph * V_NH4/(V_NH4+V_NO3)
      NO3ass(k) = Pg(k)/rCNph * V_NO3/(V_NH4+V_NO3)
!      NO2ass(k) = 0.0d0!!!
      PO4ass(k) = Pg(k)/rCPph
# endif

!--- Aerobic respiration rates ------------------------------------------
# if defined ORGANIC_MATTER
!     Respiration rate of POC (nmolC cm-3 s-1)
      Rpoc(k) = Rp_max                                             &
         *SEDECO(ng)%POC(i,j,k)**4.0d0                             &
          /(Krp_POC**4.0d0+SEDECO(ng)%POC(i,j,k)**4.0d0)           &
         *SEDECO(ng)%DOx(i,j,k)/(Krp_DO+SEDECO(ng)%DOx(i,j,k))
    
!     Respiration rate of DOC (nmolC cm-3 s-1)
      Rdoc(k) = Rd_max                                             &
         *SEDECO(ng)%DOC(i,j,k)**4.0d0                             &
          /(Krd_DOC**4.0d0+SEDECO(ng)%DOC(i,j,k)**4.0d0)           &
         *SEDECO(ng)%DOx(i,j,k)/(Krd_DO+SEDECO(ng)%DOx(i,j,k))
#  if defined NUTRIENTS
!     Respiration rate of PON (nmolN cm-3 s-1)
      Rpon(k) = Rpoc(k)                                             &
         *SEDECO(ng)%PON(i,j,k)/SEDECO(ng)%POC(i,j,k)
!     Respiration rate of DON (nmolN cm-3 s-1)
      Rdon(k) = Rdoc(k)                                             &
         *SEDECO(ng)%DON(i,j,k)/SEDECO(ng)%DOC(i,j,k)
!     Respiration rate of POP (nmolP cm-3 s-1)
      Rpop(k) = Rpoc(k)                                             &
         *SEDECO(ng)%POP(i,j,k)/SEDECO(ng)%POC(i,j,k)
!     Respiration rate of DOP (nmolP cm-3 s-1)
      Rdop(k) = Rdoc(k)                                             &
         *SEDECO(ng)%DOP(i,j,k)/SEDECO(ng)%DOC(i,j,k)
#  endif
# else
!     Respiration rate of POC (nmolC cm-3 s-1)
      Rpoc(k) = 0.0d0     
!     Respiration rate of DOC (nmolC cm-3 s-1)
      Rdoc(k) = Rdoc_max                                             &
         *SEDECO(ng)%DOx(i,j,k)/(Krdoc_DO+SEDECO(ng)%DOx(i,j,k))
#  if defined NUTRIENTS
!     Respiration rate of PON (nmolN cm-3 s-1)
      Rpon(k) = 0.0d0
!     Respiration rate of DON (nmolN cm-3 s-1)
      Rdon(k) = Rdoc(k)/rCNph

!     Respiration rate of POP (nmolP cm-3 s-1)
      Rpop(k) = 0.0d0
!     Respiration rate of DOP (nmolP cm-3 s-1)
      Rdop(k) = Rdoc(k)/rCPph
#  endif
# endif

!--- Decomposition rates ------------------------------------------
# if defined ORGANIC_MATTER
!     Decomposition rate from POC to DOC (nmolC cm-3 s-1)
     Dp2doc(k) = Dp2d_max                                             &
          *SEDECO(ng)%POC(i,j,k)**4.0d0                                 &
           /(Kp2d_POC**4.0d0+SEDECO(ng)%POC(i,j,k)**4.0d0)
#  if defined NUTRIENTS
!     Decomposition rate from PON to DON (nmolN cm-3 s-1)
      Dp2don(k) = Dp2doc(k)                                             &
         *SEDECO(ng)%PON(i,j,k)/SEDECO(ng)%POC(i,j,k)
!     Decomposition rate from POP to DOP (nmolP cm-3 s-1)
      Dp2dop(k) = Dp2doc(k)                                             &
         *SEDECO(ng)%POP(i,j,k)/SEDECO(ng)%POC(i,j,k)
#  endif
# else
!     Decomposition rate from POC to DOC (nmolC cm-3 s-1)
      Dp2doc(k) = 0.0d0
#  if defined NUTRIENTS
!     Decomposition rate from PON to DON (nmolN cm-3 s-1)
      Dp2don(k) = 0.0d0
!     Decomposition rate from POP to DOP (nmolP cm-3 s-1)
      Dp2dop(k) = 0.0d0
#  endif
# endif

!---- Dissolution rate (nmol cm-3 s-1) --------------------------
!    CO2 sysytem parameter calculation
      TK = SEDECO(ng)%Tmp(i,j,k) + 273.15d0
      
      SEDECO(ng)%pH(i,j,k) =                                         &
       pH_fromATCT( SEDECO(ng)%TA(i,j,k), SEDECO(ng)%DIC(i,j,k)      &
                   ,TK, SEDECO(ng)%Sal(i,j,k) )
      
      cCO3 = cCO3_fromCTpH(SEDECO(ng)%DIC(i,j,k),SEDECO(ng)%pH(i,j,k) &
                          ,TK, SEDECO(ng)%Sal(i,j,k) )
!    Aragonite saturation state
      SEDECO(ng)%Warg(i,j,k) = Warg_fromcCO3(cCO3,TK, SEDECO(ng)%Sal(i,j,k))
!    Dissolution rate
      Gn(k) = - max( a_diss*SEDECO(ng)%Warg(i,j,k) + b_diss, 0.0d0)       ! (percent h-1)
      Gn(k) = Gn(k)* 75.0d0 * SEDECO(ng)%poro(i,j,k)   ! (nmol cm-3 s-1)
      ! (nmol cm-3 s-1)  = 1 (percent h-1) * 2.7(g cm-3)*porosity/100(g mol-1)* 1/60/60 (h s-1) /100 (/percent) *1.0d9
      !                  = 1 (percent h-1) * 75 * porosity


# if defined NUTRIENTS
!---- Nitrification rate (nmolN cm-3 s-1) --------------------------
!    NH4+ + 2 O2 -> NO3- + H2O + 2H+
      Nit(k) = Nit_max                                            &
         *SEDECO(ng)%DOx(i,j,k)/(Kn_DO+SEDECO(ng)%DOx(i,j,k))     &
         *SEDECO(ng)%NH4(i,j,k)/(Kn_NH4+SEDECO(ng)%NH4(i,j,k))

!    NH4+ + (3/2)O2 -> NO2- + H2O + 2H+
!      Nit1(k) = Nit1_max                                           &
!         *SEDECO(ng)%DOx(i,j,k)/(Kn1_DO+SEDECO(ng)%DOx(i,j,k))     &
!         *SEDECO(ng)%NH4(i,j,k)/(Kn1_NH4+SEDECO(ng)%NH4(i,j,k))
!
!    NO2- + (1/2)O2 -> NO3-
!      Nit2(k) = Nit2_max                                           &
!         *SEDECO(ng)%DOx(i,j,k)/(Kn2_DO+SEDECO(ng)%DOx(i,j,k))     &
!         *SEDECO(ng)%NO2(i,j,k)/(Kn2_NO2+SEDECO(ng)%NO2(i,j,k))

!---- Denitrification rate (nmolN cm-3 s-1) --------------------------
#  if defined ORGANIC_MATTER
!    NO3- + H+ +(5/4)CH2O -> (1/2)N2 + (5/4)CO2 + (7/4)H2O
      DNd(k) = DNd_max                                             &
         *Kdnd_DO/(Kdnd_DO+SEDECO(ng)%DOx(i,j,k))                  &
         *SEDECO(ng)%DOC(i,j,k)**4.0d0                             &
          /(Kdnd_DOC**4.0d0+SEDECO(ng)%DOC(i,j,k)**4.0d0)          &
         *SEDECO(ng)%NO3(i,j,k)/(Kdnd_NO3+SEDECO(ng)%NO3(i,j,k))

      DNp(k) = DNp_max                                             &
         *Kdnp_DO/(Kdnp_DO+SEDECO(ng)%DOx(i,j,k))                  &
         *SEDECO(ng)%POC(i,j,k)**4.0d0                             &
          /(Kdnp_POC**4.0d0+SEDECO(ng)%POC(i,j,k)**4.0d0)          &
         *SEDECO(ng)%NO3(i,j,k)/(Kdnp_NO3+SEDECO(ng)%NO3(i,j,k))

!    Denitrification rate of DOC (nmolC cm-3 s-1)
      DNdoc(k) = DNd(k) * 1.25d0
!    Denitrification rate of POC (nmolC cm-3 s-1) N:C = 1:1.25
      DNpoc(k) = DNp(k) * 1.25d0

!    Denitrification rate of DON (nmolN cm-3 s-1)
      DNdon(k) = DNdoc(k)                                             &
         *SEDECO(ng)%DON(i,j,k)/SEDECO(ng)%DOC(i,j,k)
!    Denitrification rate of PON (nmolN cm-3 s-1)
      DNpon(k) = DNpoc(k)                                             &
         *SEDECO(ng)%PON(i,j,k)/SEDECO(ng)%POC(i,j,k)
!    Denitrification rate of DOP (nmolP cm-3 s-1)
      DNdop(k) = DNdoc(k)                                             &
         *SEDECO(ng)%DOP(i,j,k)/SEDECO(ng)%DOC(i,j,k)
!    Denitrification rate of POP (nmolP cm-3 s-1)
      DNpop(k) = DNpoc(k)                                             &
         *SEDECO(ng)%POP(i,j,k)/SEDECO(ng)%POC(i,j,k)
#  else
!    NO3- + H+ +(5/4)CH2O -> (1/2)N2 + (5/4)CO2 + (7/4)H2O
      DNd(k) = DNd_max                                              &
         *SEDECO(ng)%NO3(i,j,k)/(Kdn_NO3+SEDECO(ng)%NO3(i,j,k))
      DNp(k) = 0.0d0

!    Denitrification rate of DOC (nmolC cm-3 s-1)
      DNdoc(k) = DNd(k) * 1.25d0
!    Denitrification rate of POC (nmolC cm-3 s-1) N:C = 1:1.25
      DNpoc(k) = DNp(k) * 1.25d0

!    Denitrification rate of DON (nmolN cm-3 s-1)
      DNdon(k) = DNdoc(k)                                             &
         *SEDECO(ng)%DON(i,j,k)/SEDECO(ng)%DOC(i,j,k)
!    Denitrification rate of PON (nmolN cm-3 s-1)
      DNpon(k) = DNpoc(k)                                             &
         *SEDECO(ng)%PON(i,j,k)/SEDECO(ng)%POC(i,j,k)
!    Denitrification rate of DOP (nmolP cm-3 s-1)
      DNdop(k) = DNdoc(k)                                             &
         *SEDECO(ng)%DOP(i,j,k)/SEDECO(ng)%DOC(i,j,k)
!    Denitrification rate of POP (nmolP cm-3 s-1)
      DNpop(k) = DNpoc(k)                                             &
         *SEDECO(ng)%POP(i,j,k)/SEDECO(ng)%POC(i,j,k)
#  endif

# endif
    ENDDO


!!!---- Mass balance equations --------------------------

    DO k=1,Nsed
!    DIC (umol kg-1)
      SEDECO(ng)%DIC(i,j,k)=SEDECO(ng)%DIC(i,j,k)                     &
            +(-Pg(k)                                                  &
              -Gn(k)                                                  &
              +Rdoc(k)+Rpoc(k)                                        &
# if defined NUTRIENTS
              +DNdoc(k)+DNpoc(k)                                      &
# endif
              +( F_DIC(k-1)-F_DIC(k) )/dz                             &
             )/SEDECO(ng)%poro(i,j,k)/rho_sw(k) *dt    !nmol cm-3 s-1 = 1./rho_sw umol kg-1 s-1
      SEDECO(ng)%DIC(i,j,k)=max(SEDECO(ng)%DIC(i,j,k),0.0d0) !Error handring
      SEDECO(ng)%DIC(i,j,k)=min(SEDECO(ng)%DIC(i,j,k),1.0d5) !Error handring

!    TA (umol kg-1)
      SEDECO(ng)%TA(i,j,k)=SEDECO(ng)%TA(i,j,k)                       &
            +(-Gn(k)*2.0d0                                            &
# if defined NUTRIENTS
!           Ref. Wolf-Gladrow et al. (2007)
              +Rdon(k)+Rpon(k)                                        &
              -Rdop(k)-Rpop(k)                                        &
              -NH4ass(k)                                              &
!              +NO2ass(k)                                              &
              +NO3ass(k)                                              &
              +PO4ass(k)                                              &
              -Nit(k)*2.0d0                                           &
              +DNd(k)                                                 &
              +DNp(k)                                                 &
              +DNdon(k)+DNpon(k)                                      &
              -DNdop(k)-DNpop(k)                                      &
# endif
              +( F_TA(k-1)-F_TA(k) )/dz                               &
             )/SEDECO(ng)%poro(i,j,k)/rho_sw(k) *dt    !nmol cm-3 s-1 = 1./rho_sw umol kg-1 s-1
      SEDECO(ng)%TA(i,j,k)=max(SEDECO(ng)%TA(i,j,k),0.0d0) !Error handring
      SEDECO(ng)%TA(i,j,k)=min(SEDECO(ng)%TA(i,j,k),1.0d5) !Error handring

!    DO (umol L-1)
      SEDECO(ng)%DOx(i,j,k)=SEDECO(ng)%DOx(i,j,k)                     &
            +( Pg(k)                                                  &
              -Rdoc(k)-Rpoc(k)                                        &
# if defined NUTRIENTS
              -Nit(k) * 2.0d0                                        & ! NH4+ + 2 O2 -> NO3- + H2O + 2H+
!              -Nit1(k) * 1.5d0                                        & ! NH4+ + (3/2)O2 -> NO2- +H2O + 2H+
!              -Nit2(k) * 0.5d0                                        & ! NO2- + (1/2)O2 -> NO3-
# endif
              +( F_DO(k-1)-F_DO(k) )/dz                               &
             )/SEDECO(ng)%poro(i,j,k) *dt    !nmol cm-3 s-1 = umol L-1 s-1
      SEDECO(ng)%DOx(i,j,k)=max(SEDECO(ng)%DOx(i,j,k),0.0d0) !Error handring
      SEDECO(ng)%DOx(i,j,k)=min(SEDECO(ng)%DOx(i,j,k),1.0d4) !Error handring

# if defined ORGANIC_MATTER
!    POC (umol L-1)
      SEDECO(ng)%POC(i,j,k)=SEDECO(ng)%POC(i,j,k)                     &
            +( Pg(k) * r_assim                                        &
              -Dp2doc(k)                                              &
              -Rpoc(k)                                                &
              -DNpoc(k)                                                &
              +( F_POC(k-1)-F_POC(k) )/dz                             &
             )/SEDECO(ng)%poro(i,j,k) *dt    !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
      SEDECO(ng)%POC(i,j,k)=max(SEDECO(ng)%POC(i,j,k),0.0d0) !Error handring
      SEDECO(ng)%POC(i,j,k)=min(SEDECO(ng)%POC(i,j,k),1.0d4) !Error handring

!    DOC (umol L-1)
      SEDECO(ng)%DOC(i,j,k)=SEDECO(ng)%DOC(i,j,k)                     &
            +( Pg(k)*(1.0d0 - r_assim)                                &
              +Dp2doc(k)                                              &
              -Rdoc(k)                                                &
              -DNdoc(k)                                                &
              +( F_DOC(k-1)-F_DOC(k) )/dz                             &
             )/SEDECO(ng)%poro(i,j,k) *dt    !nmol cm-3 s-1 = umol L-1 s-1
      SEDECO(ng)%DOC(i,j,k)=max(SEDECO(ng)%DOC(i,j,k),0.0d0) !Error handring
      SEDECO(ng)%DOC(i,j,k)=min(SEDECO(ng)%DOC(i,j,k),1.0d4) !Error handring
# endif

# if defined NUTRIENTS
!    NH4 (umol L-1)
      SEDECO(ng)%NH4(i,j,k)=SEDECO(ng)%NH4(i,j,k)                     &
            +(-NH4ass(k)                                              &
              +Rdon(k)+Rpon(k)                                        &
              -Nit(k)                                                 &
              +DNdon(k)+DNpon(k)                                      &
              +( F_NH4(k-1)-F_NH4(k) )/dz                             &
             )/SEDECO(ng)%poro(i,j,k) *dt    !nmol cm-3 s-1 = umol L-1 s-1
      SEDECO(ng)%NH4(i,j,k)=max(SEDECO(ng)%NH4(i,j,k),0.0d0) !Error handring
      SEDECO(ng)%NH4(i,j,k)=min(SEDECO(ng)%NH4(i,j,k),1.0d4) !Error handring

!    NO2 (umol L-1)
!      SEDECO(ng)%NO2(i,j,k)=SEDECO(ng)%NO2(i,j,k)                     &
!            +(-NO2ass(k)                                              &
!              +Nit1(k)                                                &
!              -Nit2(k)                                                &
!              +( F_NO2(k-1)-F_NO2(k) )/dz                             &
!             )/SEDECO(ng)%poro(i,j,k) *dt    !nmol cm-3 s-1 = umol L-1 s-1
!      SEDECO(ng)%NO2(i,j,k)=max(SEDECO(ng)%NO2(i,j,k),0.0d0) !Error handring
!      SEDECO(ng)%NO2(i,j,k)=min(SEDECO(ng)%NO2(i,j,k),1.0d4) !Error handring

!    NO3 (umol L-1)
      SEDECO(ng)%NO3(i,j,k)=SEDECO(ng)%NO3(i,j,k)                     &
            +(-NO3ass(k)                                              &
              +Nit(k)                                                 &
              -DNd(k)                                                 &
              -DNp(k)                                                 &
              +( F_NO3(k-1)-F_NO3(k) )/dz                             &
             )/SEDECO(ng)%poro(i,j,k) *dt    !nmol cm-3 s-1 = umol L-1 s-1
      SEDECO(ng)%NO3(i,j,k)=max(SEDECO(ng)%NO3(i,j,k),0.0d0) !Error handring
      SEDECO(ng)%NO3(i,j,k)=min(SEDECO(ng)%NO3(i,j,k),1.0d4) !Error handring

!    PO4 (umol L-1)
      SEDECO(ng)%PO4(i,j,k)=SEDECO(ng)%PO4(i,j,k)                     &
            +(-PO4ass(k)                                              &
              +Rdop(k)+Rpop(k)                                                &
              +DNdop(k)+DNpop(k)                                      &
              +( F_PO4(k-1)-F_PO4(k) )/dz                             &
             )/SEDECO(ng)%poro(i,j,k) *dt    !nmol cm-3 s-1 = umol L-1 s-1
      SEDECO(ng)%PO4(i,j,k)=max(SEDECO(ng)%PO4(i,j,k),0.0d0) !Error handring
      SEDECO(ng)%PO4(i,j,k)=min(SEDECO(ng)%PO4(i,j,k),1.0d4) !Error handring

#  if defined ORGANIC_MATTER
!    DON (umol L-1)
      SEDECO(ng)%DON(i,j,k)=SEDECO(ng)%DON(i,j,k)                     &
            +((NH4ass(k)+NO3ass(k))*(1.0d0 - r_assim)       &
              +Dp2don(k)                                              &
              -Rdon(k)                                                &
              -DNdon(k)                                                &
              +( F_DON(k-1)-F_DON(k) )/dz                             &
             )/SEDECO(ng)%poro(i,j,k) *dt    !nmol cm-3 s-1 = umol L-1 s-1
      SEDECO(ng)%DON(i,j,k)=max(SEDECO(ng)%DON(i,j,k),0.0d0) !Error handring
      SEDECO(ng)%DON(i,j,k)=min(SEDECO(ng)%DON(i,j,k),1.0d4) !Error handring

!    PON (umol L-1)
      SEDECO(ng)%PON(i,j,k)=SEDECO(ng)%PON(i,j,k)                     &
            +((NH4ass(k)+NO3ass(k)) * r_assim               &
              -Dp2don(k)                                              &
              -Rpon(k)                                                &
              -DNpon(k)                                                &
              +( F_PON(k-1)-F_PON(k) )/dz                             &
             )/SEDECO(ng)%poro(i,j,k) *dt    !nmol cm-3 s-1 = umol L-1 s-1
      SEDECO(ng)%PON(i,j,k)=max(SEDECO(ng)%PON(i,j,k),0.0d0) !Error handring
      SEDECO(ng)%PON(i,j,k)=min(SEDECO(ng)%PON(i,j,k),1.0d4) !Error handring

!    DOP (umol L-1)
      SEDECO(ng)%DOP(i,j,k)=SEDECO(ng)%DOP(i,j,k)                     &
            +( PO4ass(k)*(1.0d0 - r_assim)                            &
              +Dp2dop(k)                                              &
              -Rdop(k)                                                &
              -DNdop(k)                                                &
              +( F_DOP(k-1)-F_DOP(k) )/dz                             &
             )/SEDECO(ng)%poro(i,j,k) *dt    !nmol cm-3 s-1 = umol L-1 s-1
      SEDECO(ng)%DOP(i,j,k)=max(SEDECO(ng)%DOP(i,j,k),0.0d0) !Error handring
      SEDECO(ng)%DOP(i,j,k)=min(SEDECO(ng)%DOP(i,j,k),1.0d4) !Error handring

!    POP (umol L-1)
      SEDECO(ng)%POP(i,j,k)=SEDECO(ng)%POP(i,j,k)                     &
            +( PO4ass(k) * r_assim                                    &
              -Dp2dop(k)                                              &
              -Rpop(k)                                                &
              -DNpop(k)                                                &
              +( F_POP(k-1)-F_POP(k) )/dz                             &
             )/SEDECO(ng)%poro(i,j,k) *dt    !nmol cm-3 s-1 = umol L-1 s-1
      SEDECO(ng)%POP(i,j,k)=max(SEDECO(ng)%POP(i,j,k),0.0d0) !Error handring
      SEDECO(ng)%POP(i,j,k)=min(SEDECO(ng)%POP(i,j,k),1.0d4) !Error handring
#  endif
# endif

    ENDDO

!!!---- Set output parameters --------------------------

    SEDECO(ng)%Pg(i,j) = 0.0d0
    SEDECO(ng)%R (i,j) = 0.0d0
    SEDECO(ng)%G (i,j) = 0.0d0

    DO k=1,Nsed
      SEDECO(ng)%Pg(i,j) = SEDECO(ng)%Pg(i,j) + Pg(k)*dz      
      SEDECO(ng)%R (i,j) = SEDECO(ng)%R (i,j) + (Rdoc(k)+Rpoc(k)) *dz       
      SEDECO(ng)%G (i,j) = SEDECO(ng)%G (i,j) + Gn(k) *dz             
    ENDDO

    Flux_Tmp = F_Tmp(0)
    Flux_Sal = F_Sal(0)
    Flux_DIC = F_DIC(0)
    Flux_TA  = F_TA(0) 
    Flux_DO  = F_DO(0) 
# if defined ORGANIC_MATTER
    Flux_DOC = F_DOC(0)
    Flux_POC = F_POC(0)
# endif
# if defined CARBON_ISOTOPE
    Flux_DI13C = F_DI13C(0)
# endif
# if defined NUTRIENTS
    Flux_NO3 = F_NO3(0)
!    Flux_NO2 = F_NO2(0)
    Flux_NH4 = F_NH4(0)
    Flux_PO4 = F_PO4(0)
#  if defined ORGANIC_MATTER
    Flux_DON = F_DON(0)
    Flux_PON = F_PON(0)
    Flux_DOP = F_DOP(0)
    Flux_POP = F_POP(0)
#  endif
# endif

# if defined SEDIMENT_TESTMODE
!------------------------------------------------------------------------
! Print section
    time = time +dt  ! sec
    
    if(time.ge.dsec) then
    
      TKamb = Tamb + 273.15d0
      pHamb = pH_fromATCT( TAamb, DICamb ,TKamb, Samb)
      cCO3amb = cCO3_fromCTpH(DICamb, pHamb, TKamb, Samb )
      Wargamb = Warg_fromcCO3(cCO3amb,TKamb, Samb)
      
      write(56,*) time/86400.d0, DICamb, SEDECO(ng)%DIC(i,j,:)
      write(57,*) time/86400.d0, TAamb, SEDECO(ng)%TA(i,j,:)
      write(58,*) time/86400.d0, DOamb, SEDECO(ng)%DOx(i,j,:)
      write(59,*) time/86400.d0, pHamb, SEDECO(ng)%pH(i,j,:)
      write(60,*) time/86400.d0, Wargamb, SEDECO(ng)%Warg(i,j,:)
#  if defined ORGANIC_MATTER
      write(65,*) time/86400.d0, DOCamb, SEDECO(ng)%DOC(i,j,:)
      write(66,*) time/86400.d0, POCamb, SEDECO(ng)%POC(i,j,:)
#  endif
#  if defined NUTRIENTS
      write(61,*) time/86400.d0, NH4amb, SEDECO(ng)%NH4(i,j,:)
!      write(62,*) time/86400.d0, NO2amb, SEDECO(ng)%NO2(i,j,:)
      write(63,*) time/86400.d0, NO3amb, SEDECO(ng)%NO3(i,j,:)
      write(64,*) time/86400.d0, PO4amb, SEDECO(ng)%PO4(i,j,:)
#   if defined ORGANIC_MATTER
      write(67,*) time/86400.d0, DONamb, SEDECO(ng)%DON(i,j,:)
      write(68,*) time/86400.d0, PONamb, SEDECO(ng)%PON(i,j,:)
      write(69,*) time/86400.d0, DOPamb, SEDECO(ng)%DOP(i,j,:)
      write(70,*) time/86400.d0, POPamb, SEDECO(ng)%POP(i,j,:)
#   endif
#  endif
      write(71,*) time/86400.d0, 0.0d0, SEDECO(ng)%Pg(i,j)
      write(72,*) time/86400.d0, 0.0d0, Rdoc
      write(73,*) time/86400.d0, 0.0d0, Rpoc
      write(74,*) time/86400.d0, 0.0d0, SEDECO(ng)%G (i,j)
#  if defined NUTRIENTS
      write(75,*) time/86400.d0, 0.0d0, Nit
!      write(76,*) time/86400.d0, 0.0d0, Nit2
      write(78,*) time/86400.d0, 0.0d0, DNd
      write(79,*) time/86400.d0, 0.0d0, DNp
#  endif

      dsec=dsec+OUTPUT_INTERVAL*60.

    endif
# endif

    RETURN

  END SUBROUTINE sedecosys

END MODULE mod_sedecosys
#endif

