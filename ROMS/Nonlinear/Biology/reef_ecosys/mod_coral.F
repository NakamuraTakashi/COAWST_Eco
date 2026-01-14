
!!!=== Copyright (c) 2012-2025 Takashi NAKAMURA  =====

#include "cppdefs.h"

!!!**** MODULE OF CORAL POLYP MODEL ************************************

MODULE mod_coral
#if defined CORAL_POLYP
  implicit none

  TYPE T_CORAL

    real(8), pointer :: Pg(:,:,:) 
    real(8), pointer :: R (:,:,:) 
    real(8), pointer :: G (:,:,:) 
    real(8), pointer :: TAcal(:,:,:) 
    real(8), pointer :: TAcoe(:,:,:) 
    real(8), pointer :: DICcal(:,:,:,:)
    real(8), pointer :: DICcoe(:,:,:,:)
    real(8), pointer :: DOcoe(:,:,:) 
    real(8), pointer :: QC(:,:,:,:)
    real(8), pointer :: CO2cal(:,:,:,:)
    real(8), pointer :: CO2coe(:,:,:,:)
# if defined CORAL_NUTRIENTS
!  Nutrients dynamics
    real(8), pointer :: NO3(:,:,:,:)
    real(8), pointer :: NH4(:,:,:,:)
    real(8), pointer :: PO4(:,:,:,:)
    real(8), pointer :: QN(:,:,:,:)
    real(8), pointer :: QP(:,:,:,:)
# endif
# if defined CORAL_ZOOXANTHELLAE
    real(8), pointer :: ROS(:,:,:)
# endif
# if defined CORAL_SIZE_DYNAMICS
    real(8), pointer :: growth(:,:,:)  ! growth rate
    real(8), pointer :: mort(:,:,:)  ! mortaliny rate
!    real(8), pointer :: dsed_coral(:,:,:)
# endif
  END TYPE T_CORAL

!-----------------------------------------------------------------------
# if defined CORAL_ZOOXANTHELLAE
!  zooxanthellae dynamics

  TYPE T_ZOOX

    real(8), pointer :: Pg(:,:,:) 
    real(8), pointer :: R (:,:,:) 
    real(8), pointer :: dens(:,:,:)
    real(8), pointer :: QC(:,:,:,:)
!  Photoinihibition model
    real(8), pointer :: QAo(:,:,:)
    real(8), pointer :: QAr(:,:,:)
    real(8), pointer :: QAi(:,:,:)
    real(8), pointer :: Chl(:,:,:)
    real(8), pointer :: SOx(:,:,:)
    real(8), pointer :: NADPHn(:,:,:)
    real(8), pointer :: RuBPn(:,:,:)
#  if defined CORAL_NUTRIENTS
    real(8), pointer :: QN(:,:,:,:)
    real(8), pointer :: QP(:,:,:,:)
    real(8), pointer :: NO3(:,:,:,:)
    real(8), pointer :: NH4(:,:,:,:)
    real(8), pointer :: PO4(:,:,:,:)
#  endif
  END TYPE T_ZOOX
# endif

  TYPE (T_CORAL), allocatable :: CORAL(:)
# if defined CORAL_ZOOXANTHELLAE
  TYPE (T_ZOOX), allocatable :: ZOOX(:)
# endif


CONTAINS


!!! **********************************************************************
!!!  allocate variables for coral polyp model (Nakamura et al. 2013)
!!! **********************************************************************

  SUBROUTINE allocate_coral(ng, Ngrids, LBi, UBi, LBj, UBj)

    USE mod_reef_ecosys_param

    implicit none
! input parameters
    integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj

    IF (ng.eq.1) allocate ( CORAL(Ngrids) )

    allocate( CORAL(ng)%Pg(Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%R (Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%G (Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%TAcal(Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%TAcoe(Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%DICcal(N_Csp,Ncl,LBi:UBi,LBj:UBj) )
    allocate( CORAL(ng)%DICcoe(N_Csp,Ncl,LBi:UBi,LBj:UBj) )
    allocate( CORAL(ng)%DOcoe(Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%QC(N_Csp,Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%CO2cal(N_Csp,Ncl,LBi:UBi,LBj:UBj) )
    allocate( CORAL(ng)%CO2coe(N_Csp,Ncl,LBi:UBi,LBj:UBj) )
# if defined CORAL_NUTRIENTS
    allocate( CORAL(ng)%NO3(N_Nsp,Ncl,LBi:UBi,LBj:UBj)  )
    allocate( CORAL(ng)%NH4(N_Nsp,Ncl,LBi:UBi,LBj:UBj)  )
    allocate( CORAL(ng)%PO4(N_Psp,Ncl,LBi:UBi,LBj:UBj)  )
    allocate( CORAL(ng)%QN(N_Nsp,Ncl,LBi:UBi,LBj:UBj)   )
    allocate( CORAL(ng)%QP(N_Psp,Ncl,LBi:UBi,LBj:UBj)   )
# endif     
# if defined CORAL_ZOOXANTHELLAE
    allocate( CORAL(ng)%ROS(Ncl,LBi:UBi,LBj:UBj)  )
# endif
# if defined CORAL_SIZE_DYNAMICS
    allocate( CORAL(ng)%growth(Ncl,LBi:UBi,LBj:UBj)   )
    allocate( CORAL(ng)%mort(Ncl,LBi:UBi,LBj:UBj)   )
# endif

# if defined CORAL_ZOOXANTHELLAE
    IF (ng.eq.1) allocate ( ZOOX(Ngrids) )
    allocate( ZOOX(ng)%Pg(Ncl,LBi:UBi,LBj:UBj)     )
    allocate( ZOOX(ng)%R (Ncl,LBi:UBi,LBj:UBj)     )

    allocate( ZOOX(ng)%dens(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZOOX(ng)%QC(N_Csp,Ncl,LBi:UBi,LBj:UBj)    )
    allocate( ZOOX(ng)%QAo(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZOOX(ng)%QAr(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZOOX(ng)%QAi(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZOOX(ng)%Chl(Ncl,LBi:UBi,LBj:UBj)   )
    allocate( ZOOX(ng)%SOx(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZOOX(ng)%NADPHn(Ncl,LBi:UBi,LBj:UBj) )
    allocate( ZOOX(ng)%RuBPn(Ncl,LBi:UBi,LBj:UBj)  )
#  if defined CORAL_NUTRIENTS
    allocate( ZOOX(ng)%NO3(N_Nsp,Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZOOX(ng)%NH4(N_Nsp,Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZOOX(ng)%PO4(N_Psp,Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZOOX(ng)%QN (N_Nsp,Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZOOX(ng)%QP (N_Psp,Ncl,LBi:UBi,LBj:UBj)  )
#  endif
# endif

    RETURN

  END SUBROUTINE allocate_coral


!!! **********************************************************************
!!!  set initial conditions for coral polyp model (Nakamura et al. 2013)
!!! **********************************************************************

  SUBROUTINE initialize_coral(ng, LBi, UBi, LBj, UBj, start_of_new_run)

    USE mod_reef_ecosys_param
    USE mod_geochem
    
    implicit none
! input parameters
    integer, intent(in) :: ng, LBi, UBi, LBj, UBj
    logical, intent(in) :: start_of_new_run               ! TRUE = initialize coral, seagass, macroalgae, sediment from start; FALSE = continue from previous run
    real(8)  R13C
    real(8)  QAt
    integer i,j,n

!  Set initial conditions
    do j=LBj,UBj
      do i=LBi,UBi
        do n=1,Ncl
!      coral internal conditions
          CORAL(ng)%Pg(n,i,j)=0.d0
          CORAL(ng)%R (n,i,j)=0.d0
          CORAL(ng)%G (n,i,j)=0.d0
# if defined CORAL_SIZE_DYNAMICS
          CORAL(ng)%growth(n,i,j) = 0.0d0
          CORAL(ng)%mort(n,i,j) = 0.0d0
# endif
# if defined CORAL_ZOOXANTHELLAE
          ZOOX(ng)%Pg(n,i,j)=0.d0
          ZOOX(ng)%R (n,i,j)=0.d0
# endif
        enddo
      enddo
    enddo

! analytical start
    if( start_of_new_run ) then
      do j=LBj,UBj
        do i=LBi,UBi
          do n=1,Ncl
            CORAL(ng)%DICcal(:,n,i,j) = 0.0d0
            CORAL(ng)%DICcoe(:,n,i,j) = 0.0d0
            CORAL(ng)%CO2cal(:,n,i,j) = 0.0d0
            CORAL(ng)%CO2coe(:,n,i,j) = 0.0d0
            CORAL(ng)%QC    (:,n,i,j) = 0.0d0

!      coral internal conditions
            CORAL(ng)%TAcal (n,i,j)=2500.d0
            CORAL(ng)%TAcoe (n,i,j)=2150.d0
            CORAL(ng)%DOcoe (n,i,j)=120.d0 !(umol/L)

            CORAL(ng)%DICcal(iCt,n,i,j)=2000.d0
            CORAL(ng)%DICcoe(iCt,n,i,j)=1950.d0
!            CORAL(ng)%QC (n,i,j)=15.d0 !450 ug/cm2 = 15 umol cm-2 -> 5.e6 umol kg-1 
            CORAL(ng)%QC (iCt,n,i,j)=300.0d0 !290.d0 ! (umolC/cm2) Tanaka et al. 2018
                          ! QC0=250.0d0
            CORAL(ng)%CO2cal(iCt,n,i,j)= 9.d0
            CORAL(ng)%CO2coe(iCt,n,i,j)=33.d0

# if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
            CORAL(ng)%QC    (iC13,n,i,j)   &
                      = Ci_from_Ct_delta( CORAL(ng)%QC    (iCt,n,i,j), -17.d0, R13C_VPDB )
            CORAL(ng)%DICcoe(iC13,n,i,j)   &
                      = Ci_from_Ct_delta( CORAL(ng)%DICcoe(iCt,n,i,j),  0.0d0, R13C_VPDB )
            CORAL(ng)%DICcal(iC13,n,i,j)   &
                      = Ci_from_Ct_delta( CORAL(ng)%DICcal(iCt,n,i,j),  0.0d0, R13C_VPDB )
            CORAL(ng)%CO2cal(iC13,n,i,j)   &
                      = Ci_from_Ct_delta( CORAL(ng)%CO2cal(iCt,n,i,j),  -8.d0, R13C_VPDB )
            CORAL(ng)%CO2coe(iC13,n,i,j)   &
                      = Ci_from_Ct_delta( CORAL(ng)%CO2coe(iCt,n,i,j),  -8.d0, R13C_VPDB )
#  if defined CLUMPED_ISOTOPE
            CORAL(ng)%QC    (iD47,n,i,j) = ????
            CORAL(ng)%DICcoe(iD47,n,i,j) = ????
            CORAL(ng)%DICcal(iD47,n,i,j) = ????
            CORAL(ng)%CO2cal(iD47,n,i,j) = ????
            CORAL(ng)%CO2coe(iD47,n,i,j) = ????
#  endif
# endif
# if defined CORAL_ZOOXANTHELLAE
            CORAL(ng)%ROS(n,i,j) = 2.08 !0.0d0
# endif
# if defined CORAL_NUTRIENTS
            CORAL(ng)%NO3(  :,n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
            CORAL(ng)%NH4(  :,n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
            CORAL(ng)%QN (  :,n,i,j) = 0.0d0  ! (pmolN/cell)  calculated from C:N:P = 116:16:1

            CORAL(ng)%NO3(iNt,n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
            CORAL(ng)%NH4(iNt,n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
            CORAL(ng)%QN (iNt,n,i,j) = CORAL(ng)%QC (iCt,n,i,j) *16.0d0/106.0d0  ! (pmolN/cell)  calculated from C:N:P = 116:16:1
#  if defined NITROGEN_ISOTOPE
            CORAL(ng)%NO3(iN15,n,i,j)  &
                  = Ci_from_Ct_delta( CORAL(ng)%NO3(iNt,n,i,j), 5.d0, R15N_AIR )  !!!??????????????????????????????
            CORAL(ng)%NH4(iN15,n,i,j)  &
                  = Ci_from_Ct_delta( CORAL(ng)%NH4(iNt,n,i,j), 5.d0, R15N_AIR )  !!!??????????????????????????????
            CORAL(ng)%QN (iN15,n,i,j)  &
                  = Ci_from_Ct_delta( CORAL(ng)%QN (iNt,n,i,j), 5.d0, R15N_AIR )  !!!??????????????????????????????
#  endif
            CORAL(ng)%PO4(  :,n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
            CORAL(ng)%QP (  :,n,i,j) = 0.0d0 ! (pmolP/cell)

            CORAL(ng)%PO4(iPt,n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
            CORAL(ng)%QP (iPt,n,i,j) = CORAL(ng)%QC (iCt,n,i,j) *1.0d0/106.0d0 ! (pmolP/cell)
# endif

# if defined CORAL_ZOOXANTHELLAE
            ZOOX(ng)%Pg(n,i,j)=0.d0
            ZOOX(ng)%R (n,i,j)=0.d0
            ZOOX(ng)%dens(n,i,j) = 1.0d6 ! (cell/cm2)
!            ZOOX(ng)%QC(n,i,j)   = 120.0d0  ! (pmolC/cell) 66 pmol/cell: Tanaka et al 2018;  600-1400 pgC cell-1: Muller-Parker et al., 1994)
                            ! QC0= 50.0d0
            ZOOX(ng)%Chl(n,i,j)  = 3.5 ! 2.0d0 ! (pg/cell) Suggett et al. 2008
!            QAt = ZOOX(ng)%Chl(n,i,j)*1.2d-6 ! (pmolRCII/cell) 1.2d-6 ~1.7d-6 -> Suggett et al. 2008
            QAt = 1.2d-6 ! (pmolRCII/cell) 1.2d-6 ~1.7d-6 -> Suggett et al. 2008
            ZOOX(ng)%QAo(n,i,j) = QAt*0.6d0 ! (pmolRCII/cell) 1.2d-6 ~1.7d-6 -> Suggett et al. 2008
            ZOOX(ng)%QAr(n,i,j) = 0.0d0
            ZOOX(ng)%QAi(n,i,j) = QAt*0.4d0
            ZOOX(ng)%SOx(n,i,j) = 0.5 !0.0d0 ! Superoxide (O2-)
            ZOOX(ng)%NADPHn(n,i,j) = 0.085 !0.0d0 ! normalized NADPH (no dim.) NADPH/(NADPH + NADP+) 
            ZOOX(ng)%RuBPn(n,i,j) = 0.091 !0.0d0  ! normalized RuBP  (no dim.) RuBP/(0.5*PGA + RuBP) 

            ZOOX(ng)%QC(  :,n,i,j) = 0.0d0
            ZOOX(ng)%QC(iCt,n,i,j) = 77.0d0  ! (pmolC/cell) 66 pmol/cell: Tanaka et al 2018;  600-1400 pgC cell-1: Muller-Parker et al., 1994)
#  if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
            ZOOX(ng)%QC(iC13,n,i,j)  &
                  = Ci_from_Ct_delta( ZOOX(ng)%QC(iCt,n,i,j), -20.d0, R13C_VPDB )  !!!??????????????????????????????
#   if defined CLUMPED_ISOTOPE
            ZOOX(ng)%QC(iD47,n,i,j) = ?????  !!!??????????????????????????????
#   endif
#  endif
#  if defined CORAL_NUTRIENTS
            ZOOX(ng)%NO3(  :,n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
            ZOOX(ng)%NH4(  :,n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
            ZOOX(ng)%QN (  :,n,i,j) = 0.0d0  ! (pmolN/cell)  calculated from C:N:P = 116:16:1

            ZOOX(ng)%NO3(iNt,n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
            ZOOX(ng)%NH4(iNt,n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
            ZOOX(ng)%QN (iNt,n,i,j) = 1.1d0  ! (pmolN/cell)  calculated from C:N:P = 116:16:1
#   if defined NITROGEN_ISOTOPE
            ZOOX(ng)%NO3(iN15,n,i,j)  &
                  = Ci_from_Ct_delta( ZOOX(ng)%NO3(iNt,n,i,j), 5.d0, R15N_AIR )  !!!??????????????????????????????
            ZOOX(ng)%NH4(iN15,n,i,j)  &
                  = Ci_from_Ct_delta( ZOOX(ng)%NH4(iNt,n,i,j), 5.d0, R15N_AIR )  !!!??????????????????????????????
            ZOOX(ng)%QN (iN15,n,i,j)  &
                  = Ci_from_Ct_delta( ZOOX(ng)%QN (iNt,n,i,j), 5.d0, R15N_AIR )  !!!??????????????????????????????
#   endif
            ZOOX(ng)%PO4(  :,n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
            ZOOX(ng)%QP (  :,n,i,j) = 0.0d0 ! (pmolP/cell)

            ZOOX(ng)%PO4(iPt,n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
            ZOOX(ng)%QP (iPt,n,i,j) = 0.07d0 ! (pmolP/cell)
#  endif
# endif
          enddo
        enddo
      enddo
    endif

    RETURN

  END SUBROUTINE initialize_coral
      

!!! **********************************************************************
!!!  Main program of coral polyp model (Nakamura et al. 2013, 2018)
!!! **********************************************************************

  SUBROUTINE coral_polyp  &
!   input parameters
    ( ng, n, i, j    &   ! ng: nested grid number; n: coral compartment; i,j: position
    , date           &   ! Date (days since x or elapsed days)
    , dt             &   ! Time step (sec)
    , PFD            &   ! Photon flux density (umol m-2 s-1)
    , rho_sw         &   ! Density of seawater (g cm-3)
    , Tamb           &   ! Temperature (oC)
    , Samb           &   ! Salinity (PSU)
    , DOamb          &   ! Dissolved oxygen (umol L-1)
    , TAamb          &   ! Total alkalinity (TA: umol kg-1)
    , DICamb         &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
# if defined CORAL_NUTRIENTS         
    , NO3amb         &   ! NO3 (umol L-1)
    , NH4amb         &   ! NH4 (umol L-1)
    , PO4amb         &   ! PO4 (umol L-1)
# endif
# if defined CORAL_INGESTION
    , ZooC           &   ! zooplankton C bomass(umol C L-1)
    , ZooN           &   ! zooplankton N bomass (umol N L-1)
    , ZooP           &   ! zooplankton P bomass (umol P L-1)
# endif
    , tau_amb        &   ! bottom shear stress (N m-2)
    , Fsed           &   ! sedimentation rate (??)
!   output parameters
    , Flux_DO        &   ! DO  flux (nmol cm-2 s-1)  * direction of water column to coral is positive
    , Flux_TA        &   ! TA  flux (nmol cm-2 s-1)  * direction of water column to coral is positive
    , Flux_DIC       &   ! DIC flux (nmol cm-2 s-1)  * direction of water column to coral is positive
# if defined CORAL_NUTRIENTS
    , Flux_NO3       &   ! NO3 flux (nmol cm-2 s-1)  * direction of water column to coral is positive
    , Flux_NH4       &   ! NH4 flux (nmol cm-2 s-1)  * direction of water column to coral is positive
    , Flux_PO4       &   ! PO4 flux (nmol cm-2 s-1)  * direction of water column to coral is positive
# endif
    , Flux_DOC       &   ! DOC flux (nmol cm-2 s-1) * direction of water column to coral is positive
    , Flux_POC       &   ! POC flux (nmol cm-2 s-1) * direction of water column to coral is positive
# if defined CORAL_NUTRIENTS
    , Flux_DON       &   ! DON flux (nmol cm-2 s-1) * direction of water column to coral is positive
    , Flux_PON       &   ! PON flux (nmol cm-2 s-1) * direction of water column to coral is positive
    , Flux_DOP       &   ! DOP flux (nmol cm-2 s-1) * direction of water column to coral is positive
    , Flux_POP       &   ! POP flux (nmol cm-2 s-1) * direction of water column to coral is positive
# endif
# if defined CORAL_INGESTION
    , Flux_ZooC      &   ! Zooplankton ingestion rate (nmol C cm-2 s-1)  * direction of water column to coral is positive
    , Flux_ZooN      &   ! Zooplankton ingestion rate (nmol N cm-2 s-1)  * direction of water column to coral is positive
    , Flux_ZooP      &   ! Zooplankton ingestion rate (nmol P cm-2 s-1)  * direction of water column to coral is positive
# endif
    )

!-----------------------------------------------------------------------

    USE mod_reef_ecosys_param
    USE mod_geochem
    
    implicit none

! input parameters
    integer, intent(in) :: ng, n, i, j
    real(8), intent(in) :: date
    real(8), intent(in) :: dt
    real(8), intent(in) :: PFD      
    real(8), intent(in) :: rho_sw   
    real(8), intent(in) :: Tamb     
    real(8), intent(in) :: Samb     
    real(8), intent(in) :: DOamb    
    real(8), intent(in) :: TAamb    
    real(8), intent(in) :: DICamb(N_Csp)   
# if defined CORAL_NUTRIENTS
    real(8), intent(in) :: NO3amb(N_Nsp)
    real(8), intent(in) :: NH4amb(N_Nsp)   
    real(8), intent(in) :: PO4amb(N_Psp)
# endif
# if defined CORAL_INGESTION
    real(8), intent(in) :: ZooC(N_Csp,Nzoo)
    real(8), intent(in) :: ZooN(N_Nsp,Nzoo)
    real(8), intent(in) :: ZooP(N_Psp,Nzoo)
# endif
    real(8), intent(in) :: tau_amb  
    real(8), intent(in) :: Fsed
! output parameters
    real(8), intent(out) :: Flux_DO 
    real(8), intent(out) :: Flux_TA 
    real(8), intent(out) :: Flux_DIC(N_Csp)
# if defined CORAL_NUTRIENTS
    real(8), intent(out) :: Flux_NO3(N_Nsp)
    real(8), intent(out) :: Flux_NH4(N_Nsp)
    real(8), intent(out) :: Flux_PO4(N_Psp)
# endif
    real(8), intent(out) :: Flux_DOC(N_Csp,Ndom)
    real(8), intent(out) :: Flux_POC(N_Csp,Npom)
# if defined CORAL_NUTRIENTS
    real(8), intent(out) :: Flux_DON(N_Nsp,Ndom)
    real(8), intent(out) :: Flux_PON(N_Nsp,Npom)
    real(8), intent(out) :: Flux_DOP(N_Psp,Ndom)
    real(8), intent(out) :: Flux_POP(N_Psp,Npom)
# endif
# if defined CORAL_INGESTION
    real(8), intent(out) :: Flux_ZooC(N_Csp,Nzoo)
    real(8), intent(out) :: Flux_ZooN(N_Nsp,Nzoo)
    real(8), intent(out) :: Flux_ZooP(N_Psp,Nzoo)
# endif

!!!------------Set parameters  ----------------------------------

!----- Physical constants ------------------------
    real(8), parameter :: Rgas=8.314d0 ! Gas constant (J mol-1)

!------------Coral parameters ---------------------------------

!----- Coral basic parameters ----------------------------------------

    real(8), parameter :: hcoe=0.3d0    ! Tickness of coral tissue (cm) 3000 um Reviewer's suggestion: 1000-5000 um
    real(8), parameter :: hcal=0.0005d0 ! Tickness of coral calcifying fluid (cm) 5um Reviewer's suggestion: 5 um
    real(8), parameter :: htis=0.01d0   ! Tickness of coral tissue (cm) 100 um  !!!!!!!!!!!!!!!!!!!! Need check!!!

!----- for gross photosynthesi -------------------

!  Kuhl et al. 1995, MEPS 
!   Pg=Pgmax*tanh(I/Ik) for Acropora sp.

!    Pg=Pgmax(n)*tanh(PFDsurf/Ik(n))*(1-Br(i,j))
!          *HCO3coe/(K_HCO3+HCO3coe)
!          *hcoe

    real(8), parameter :: Pgmax(2)=  [ 0.6d0, 0.6d0 ] !0.73d0*    !0.91d0    !Maximum photosynthesis rate (nmol O2 cm-2 s-1) Kuhl et al. (1995) *Tuned
    real(8), parameter :: Ik(2)=     [ 275.d0, 275.d0 ]   !(275 uEin m-2 s-1) * refference value

!------ Goreau et al. (1996)------------------------------------------   
!                    Coral    FIZ    CZ
!          K_HCO3   408.    71.     178.   uM
!          V_HCO3     1.45   1.09    13.   nmol O2 (ug Chl a)-1 min-1
!          K_CO2aq    5.     0.67     2.7  uM
!          V_CO2aq    2.     1.15    12.5  nmol O2 (ug Chl a)-1 min-1
!---------------------------------------------------------------------
    real(8), parameter :: K_HCO3(2)= [ 408.d0, 408.d0 ] !uM
    real(8), parameter :: V_HCO3(2)= [ 1.45d0, 1.45d0 ] !nmol O2 (ug Chl a)-1 min-1
    real(8), parameter :: K_CO2(2)=[ 5.d0, 5.d0 ]     !uM
    real(8), parameter :: V_CO2(2)=[ 2.d0, 2.d0 ]     !nmol O2 (ug Chl a)-1 min-1

!   Temperature dependency parameters (Hikosaka et al. 2006) 
!      - Parameters estimated from data by Al-Horani (2005)

    real(8), parameter :: Hact(2) = [ 1.855d5, 1.855d5 ] !Activation energy (J mol-1)) *tuned
    real(8), parameter :: Hdeact(2)=[ 3.055d5, 3.055d5 ] !Deactivation energy (J mol-1) *tuned
    real(8), parameter :: Entr(2)=  [ 1028.d0, 1028.d0 ] !Entropy term (J K-1) *tuned

!----- for respiration --------------------------

!    R= k_R(n)*CORAL(ng)%QC(n,i,j)/(K_QC(n)+CORAL(ng)%QC(n,i,j))
!       *CORAL(ng)%DOcoe(n,i,j)/(K_DO(n)+CORAL(ng)%DOcoe(n,i,j))

                                             !!! * Nakamura et al. 2013 setting; ** New setting

    real(8), parameter :: Rmax(2)   = [ 0.05d0, 0.05d0 ] !!!0.53d0*  0.37d0** !Maximum respiration rate (nmol O2 cm-2 s-1) *Tuned using Kuhl et al (1995), Al-Horani et al. (2003) data
    real(8), parameter :: K_QC(2)   = [ 100.d0, 100.d0 ]     !!!5.0d0* umol cm-2!!!
    real(8), parameter :: K_DO(2)   = [ 46.d0, 46.d0 ]   ! ca. 46 (umol kg-1) Newton & Atkinson (1991)

    real(8), parameter :: Rratio(2) = [ 0.8d0, 0.8d0 ] !!!0.9d0** 0.8d0  !1.0d0* !!!

!   Temperature dependency parameters (Hikosaka et al. 2006) 
!      - Parameters estimated from data by Fujimura et al. (2008)

    real(8), parameter :: Eact(2)   = [ 5.117d4, 5.117d4 ] !Activation energy (J mol-1)) *Tuned

! ---- OC turnover ---------------------
    real(8), parameter :: C_dec(2) = [ 0.02d0, 0.02d0 ] ! (nmol cm-2 s-1) ~0.1 (nmol cm-2 s-1) Tanaka et al., 2018
    real(8), parameter :: p2r = 0.5d0    ! molC respiration/(molC protain) required respiration rate for 1 molC biosynthesis
    real(8), parameter :: Ebs = 230.0d3  ! (J molC-1) energy required for biosynthesis 

!----- for calcification ------------------------

    real(8), parameter :: eeff(2)  = [ 0.12d0, 0.12d0 ]   !!! 0.01d0** !0.3d0*!Energy efficency of calcification (<12%, 4~7%?; Lervik et al., 2012)
!    real(8), parameter :: E_m(2)=[ 2.0d4, 2.0d4 ]   !!! 1.0d4** ! 5.0d3* Energy flux for maintenance (nJ cm-2 s-1)
    real(8) :: E_m

    real(8), parameter :: k_CO2i(2) =[ 1.5d-3, 1.5d-3 ]  ! 1.5d-3* !permeability coefficient (cm s-1): Sueltemeyer and Rinast (1996): (1.49d-3 cm s-1)
    real(8), parameter :: k_TA(2)   =[ 1.0d-4, 1.0d-4 ]  !!! 3.0d-4**  5.0d-5 !3.0d-3* !conductivity of TA through the leak pass (cm s-1) *Tuned
    real(8), parameter :: k_DIC(2)  =[ 1.0d-4, 1.0d-4 ]  !!! 3.0d-4**  5.0d-5 !3.0d-3* !conductivity of DIC through the leak pass (cm s-1) *Tuned

    real(8), parameter :: c2g(2) = [ 6.6d0, 6.6d0 ]  ! (calcification rate)/(net tissue growth rate) (no dimension)!!!!!!!! Tuning parameter

# if defined CORAL_MUCUS
    !                  ForgC(Ncl,Ndom)      Coral1        Coral2     
    real(8), parameter :: ForgC(2,2)  = reshape(                          &
                                     [ 0.232d0/20.0d0, 0.073d0/2.5d0    & ! LDOM  !! Organic carbon release rate(+) or uptake rate(-) (nmol cm-2 s-1) *Tuned
                                     , 0.232d0/20.0d0, 0.073d0/2.5d0 ]  & ! RDOM  !! Organic carbon release rate(+) or uptake rate(-) (nmol cm-2 s-1) *Tuned
                                     , [2,2] ) !
    ! 200 mmol m-2 d-1 for 100% covered inner reef corals
    ! -63 mmol m-2 d-1 for 100% covered reef slope corals 
    ! For converting reef scale (mmol m-2 d-1) to polyp scale (nmol cm-2 s-1)
    !   1 mmol m-2 d-1 = 100/24/60/60/P2R(n) nmol cm-2 s-1 ~ 1.16d-3/P2R(n),
    !   Thus: 200*1.16d-3/P2R =  0.232/P2R nmol cm-2 s-1 for inner reef corals
    !!!       -63*1.16d-3/P2R = -0.073/P2R nmol cm-2 s-1 for reef slope corals  ****** To Be Confirmed
# endif
# if defined CORAL_INGESTION
    real(8), parameter :: r_assim = 0.1d0 ! Assimilation ratio with ingesting zooplankton
# endif
# if defined CORAL_ZOOXANTHELLAE
!    real(8), parameter :: K_ROS(2)   = [ 10.0d0, 10.0d0 ]     !!! umol L !!! To Be Confirmed
!    real(8), parameter :: V_detox(2) = [ 2.0d0, 2.0d0 ]   ! 
    real(8), parameter :: k_detox(2) = [ 3.4d7, 3.4d7 ]     ! Rate constant of specific catalase activity (M-1 s-1): Ogura (1955), Mueller et al., (1997)
    real(8), parameter :: CAT(2)     = [ 5.0d-11, 5.0d-11 ]   ! Catalase concentration (M): !!!!!!! Tuning
    real(8), parameter :: k_dam(2)   = [ 0.0d-5, 0.0d-5 ]   ! Reaction rate constant (s-1): !!!!!!! Tuning
    real(8), parameter :: a_dam(2)   = [ 2.0d3,  2.0d3  ]   ! Constant ((nmol cm-2 s-1) -1): !!!!!!! Tuning
    real(8), parameter :: k_Zelm(2)  = [ 7.0d-9,  7.0d-9 ]   ! Reaction rate constant (cell cm-2 s-1): !!!!!!! Tuning
!    real(8), parameter :: F_cROS(2)  = [ 3.0d-1, 3.0d-1 ]   ! ROS production rate by coral (nmol cm-2 s-1): !!!!!!! Tuning
    real(8), parameter :: a_cROS(2)  = [ 1.0d-2, 1.0d-2 ]   ! ROS production rate by coral (nmol cm-2 s-1): !!!!!!! Tuning
# endif

!----- Coral minimum quota ------------------------
!    real(8), parameter :: QC0(2) = [ 2.0d0, 2.0d0 ]   ! Minimum quota (umol cm-2)  refereed minimum value of tissue C by Tanaka et al., (2009)
    real(8), parameter :: QC0(2) = [250.0d0, 250.0d0 ] ! Minimum quota (umol cm-2)  refereed minimum value of tissue C by Tanaka et al., (2009)
# if defined CORAL_NUTRIENTS
    real(8), parameter :: QN0(2) = [ 0.20d0, 0.20d0 ]   ! Minimum quota (umol cm-2)  
    real(8), parameter :: QP0(2) = [ 0.02d0, 0.02d0 ]   ! Minimum quota (umol cm-2) 
# endif
# if defined CORAL_SIZE_DYNAMICS
    real(8), parameter :: g_max(2) = [ 1.0d-2, 1.0d-2 ] ! Maximum growth rate (nmol cm-2 s-1)  !!!!!! Tuning parameter
    real(8), parameter :: aC_m   = 2.0d-1   ! ((umol cm-2)-1)!!!!!!! Tuning parameter
    real(8), parameter :: km_dam = 1.0d-10  ! (s-1)!!!!!!!!!!!!!!!!! Tuning parameter
    real(8), parameter :: km_es(2) = [ 1.0d0, 1.0d0 ]   ! !!!!!!!!!!!!!! TBC
    real(8), parameter :: km(2) = [ 1.0d0, 1.0d0 ]   ! !!!!!!!!!!!!!! TBC
# endif

!--- Variables for summing mass fluxes (nmol cm-2 s-1) -----------------
    real(8) :: F_TAcal
    real(8) :: F_TAcoe 
    real(8) :: F_DICcal(N_Csp)
    real(8) :: F_DICcoe(N_Csp)
    real(8) :: F_DOcoe
    real(8) :: F_QC(N_Csp)
    real(8) :: F_CO2cal(N_Csp)
    real(8) :: F_CO2coe(N_Csp)
# if defined CORAL_NUTRIENTS
!  Nutrients dynamics
    real(8) :: F_NO3(N_Nsp)
    real(8) :: F_NH4(N_Nsp)
    real(8) :: F_PO4(N_Psp)
    real(8) :: F_QN (N_Nsp)
    real(8) :: F_QP (N_Psp)
# endif
# if defined CORAL_ZOOXANTHELLAE
    real(8) :: F_ROS
# endif
!--- Variables of isotope ratio (R13C = 12C/13C) -----------------
    real(8) :: rDICcal(N_Csp)
    real(8) :: rDICcoe(N_Csp)
    real(8) :: rDICamb(N_Csp)
    real(8) :: rQC(N_Csp)
    real(8) :: rCO2cal(N_Csp)
    real(8) :: rCO2coe(N_Csp)
    real(8) :: rCO2amb(N_Csp)
# if defined CORAL_INGESTION
    real(8) :: rZooC(N_Csp,Nzoo)
# endif
# if defined CORAL_NUTRIENTS
!--- Variables of isotope ratio (R15N = 15N/14N) -----------------
    real(8) :: rNO3coe(N_Nsp)
    real(8) :: rNO3amb(N_Nsp)
    real(8) :: rNH4coe(N_Nsp)
    real(8) :: rNH4amb(N_Nsp)
    real(8) :: rQN(N_Nsp)
    real(8) :: rPO4amb(N_Psp)
    real(8) :: rQP(N_Psp)
#  if defined CORAL_INGESTION
    real(8) :: rZooN(N_Nsp,Nzoo)
    real(8) :: rZooP(N_Psp,Nzoo)
#  endif
# endif

!--- Other variables -------------------------------------------
    real(8) :: Rc, Rz
!    real(8) :: Si   !Mass transfer coefficient (cm s-1)
    real(8) :: Si_DIC(N_Csp), Si_TA, Si_DO   !Mass transfer coefficient (cm s-1)
# if defined CORAL_INGESTION
    real(8) :: Si_PHY(N_Csp), Si_ZOO(N_Csp)
# endif
# if defined CORAL_NUTRIENTS
    real(8) :: Si_NO3(N_Nsp), Si_NH4(N_Nsp), Si_PO4(N_Psp)
# endif
# if defined CORAL_NONE_CO2_EQ
    real(8) :: Si_CO2(N_Csp)
# endif
# if defined CORAL_ZOOXANTHELLAE
    real(8) :: Si_ROS
# endif
    real(8) :: TKamb   !Temperature (K)
!----- for CO2 system dynamics -------------------
    real(8) :: pHcal,fCO2cal,CO2cal(N_Csp),HCO3cal(N_Csp),CO3cal(N_Csp),Wargcal
    real(8) :: pHcoe,fCO2coe,CO2coe(N_Csp),HCO3coe(N_Csp),CO3coe(N_Csp)
    real(8) :: pHamb,fCO2amb,CO2amb(N_Csp),HCO3amb(N_Csp),CO3amb(N_Csp),Wargamb  !!!!!
# if defined CORAL_NONE_CO2_EQ
    real(8) :: cHcoe,cOHcoe
    real(8) :: cHcal,cOHcal
    real(8) :: Fpp_CO2(N_Csp), Flux_CO2(N_Csp)
    real(8) :: kp1(N_Csp),km1(N_Csp),kp4(N_Csp),km4(N_Csp)
# endif
!----- for gross photosynthesis -------------------
    real(8) :: PFDsurf
!     Temperature related factor
    real(8) :: f_temp
    real(8) :: f_min
!----- for calcification ------------------------
    real(8) :: E_tot,E_ca,F_H
    real(8) :: E_rq_H_transport
    real(8) :: E_g  !!! Energy flux for growth (nJ cm-2 s-1)
    real(8) :: Fpm_CO2(N_Csp),Fpp_TA,Fpp_DIC(N_Csp)
!    real(8) :: Fpp_CO2,Fpp_HCO3,Fpp_CO3

!----- for nutrients dynamics -------------------

    real(8) :: QCcr
    real(8) :: F_zDO
# if defined CORAL_ZOOXANTHELLAE
    real(8) :: F_Csec(N_Csp)
    real(8) :: F_Cwaste(N_Csp)
    real(8) :: F_Zelm
    real(8) :: F_Zmort
    real(8) :: F_zDIC(N_Csp), F_zROS
    real(8) :: F_detox
    real(8) :: F_dam
    real(8) :: F_cROS
    real(8) :: Damage
#  if defined CORAL_NUTRIENTS
    real(8) :: F_Nsec(N_Nsp)
    real(8) :: F_Nwaste(N_Nsp)
    real(8) :: F_Psec(N_Psp)
    real(8) :: F_Pwaste(N_Psp)
    real(8) :: F_zNO3(N_Nsp), F_zNH4(N_Nsp)
    real(8) :: F_zPO4(N_Psp)
    real(8) :: F_ONOO
#  endif
# endif

# if defined CORAL_MUCUS
    real(8) :: F_Cmucus(N_Csp)
#  if defined CORAL_NUTRIENTS
    real(8) :: F_Nmucus(N_Nsp)
    real(8) :: F_Pmucus(N_Psp)
#  endif
# endif
# if defined CORAL_INGESTION
    real(8) :: F_Cingest(N_Csp)
#  if defined CORAL_NUTRIENTS
    real(8) :: F_Ningest(N_Nsp)
    real(8) :: F_Pingest(N_Psp)
#  endif
# endif
    real(8) :: F_Cgrowth(N_Csp)
# if defined CORAL_SIZE_DYNAMICS

    real(8) :: netCgrowth
    real(8) :: g2c_tmp   ! (net growth rate)/(calcification rate)
    real(8) :: mort_es
#  if defined CORAL_NUTRIENTS
    real(8) :: F_Ngrowth(N_Nsp)
    real(8) :: F_Pgrowth(N_Psp)
#  endif
# endif

# if defined CORAL_TESTMODE
!  Output
!    real(8), parameter :: CORAL_OUTPUT_INTERVAL  = 5.0d0    ! Output interval (min)
!    real(8), parameter :: CORAL_AVERAGE_INTERVAL = 1.0d0    ! Average interval (day)
    real(8), save :: dsec = 0.d0 !sec
    real(8), save :: dday = 0.d0 !day
    real(8), save :: S_PFD_dt(2)      = 0.0d0
    real(8), save :: S_Gn_dt(N_Csp,2) = 0.0d0
    real(8), save :: S_Pg_dt(2)       = 0.0d0
    real(8), save :: S_R_dt(2)        = 0.0d0
!    real(8), save :: S_QC_dt(N_Csp,2) = 0.0d0
#  if defined CARBON_ISOTOPE
    real(8) :: d13C_DICamb, d13C_CO2amb, d13C_HCO3amb, d13C_CO3amb
    real(8) :: d13C_DICcoe, d13C_CO2coe, d13C_HCO3coe, d13C_CO3coe
    real(8) :: d13C_DICcal, d13C_CO2cal, d13C_HCO3cal, d13C_CO3cal
    real(8) :: d13C_QC, d13C_G, d13C_arg
#  endif
#  if defined CORAL_BORON_ISOTOPE
    real(8) :: d11Barg
    real(8), save :: S_d11BargxGn_dt(2) = 0.0d0
#  endif
# endif
    real(8) :: k_CO2i_tmp(N_Csp), k_DIC_tmp(N_Csp), k_CO2_tmp(N_Csp)
    real(8) :: cff,cff2,cff3
    integer :: isp, m

!======================================================================
!   Preparation for calculation
!======================================================================
!   Set initial zeros for output mass fluxes (nmol cm-2 s-1)
    Flux_DIC = 0.0d0
    Flux_TA  = 0.0d0
    Flux_DO  = 0.0d0
    Flux_DOC = 0.0d0
    Flux_POC = 0.0d0
# if defined CORAL_INGESTION
!    Flux_PHY = 0.0d0
    Flux_ZooC = 0.0d0
# endif
# if defined CORAL_NUTRIENTS
    Flux_NO3 = 0.0d0
    Flux_NH4 = 0.0d0
    Flux_PO4 = 0.0d0
    Flux_DON = 0.0d0
    Flux_PON = 0.0d0
    Flux_DOP = 0.0d0
    Flux_POP = 0.0d0
# endif

!--- Set initial zeros for summing mass fluxes (nmol cm-2 s-1) -----------------
    F_DOcoe = 0.0d0
    F_TAcal = 0.0d0
    F_TAcoe = 0.0d0
    F_DICcal(:) = 0.0d0
    F_DICcoe(:) = 0.0d0
    F_CO2cal(:) = 0.0d0
    F_CO2coe(:) = 0.0d0
    F_QC(:) = 0.0d0
# if defined CORAL_NUTRIENTS
    F_NO3(:) = 0.0d0
    F_NH4(:) = 0.0d0
    F_PO4(:) = 0.0d0
    F_QN (:) = 0.0d0
    F_QP (:) = 0.0d0
# endif
# if defined CORAL_ZOOXANTHELLAE
    F_ROS = 0.0d0
# endif
!---- Temperature (K) -------------------------------------
    TKamb=Tamb+273.15d0 !(oC->K)

!---- CO2 system parameters --------------------------------

    CO2amb (:) = 0.0d0
    HCO3amb(:) = 0.0d0
    CO3amb (:) = 0.0d0
!    CO2coe (:) = 0.0d0
    HCO3coe(:) = 0.0d0
    CO3coe (:) = 0.0d0
!    CO2cal (:) = 0.0d0
    HCO3cal(:) = 0.0d0
    CO3cal (:) = 0.0d0

!----CO2*+H2O <-> H+ +HCO3- is none equilibrium
# if defined CORAL_NONE_CO2_EQ
!--------- CO2 system in calcified fulid -------------------
    pHcal         = pH_fromATCTcCO2aq(CORAL(ng)%TAcal(n,i,j),CORAL(ng)%DICcal(iCt,n,i,j),CORAL(ng)%CO2cal(n,i,j),TKamb, Samb)
    HCO3cal(iCt)  = cHCO3_noneq(CORAL(ng)%DICcal(iCt,n,i,j),CORAL(ng)%CO2cal(iCt,n,i,j),pHcal,TKamb, Samb)!! for Isotope
    CO3cal (iCt)  = cCO3_noneq(CORAL(ng)%DICcal(iCt,n,i,j),CORAL(ng)%CO2cal(iCt,n,i,j),pHcal,TKamb, Samb)
    fCO2cal       = fCO2_fromcCO2aq(CORAL(ng)%CO2cal(iCt,n,i,j),TKamb, Samb)  !! for output
    Wargcal       = Warg_fromcCO3cCa(CO3cal,10.4d0,TKamb, Samb)  !Calcifying fluid {Ca2+]=10.4 mmol kg-1   Assumed
!--------- CO2 system in coelenteron -------------------
    pHcoe         = pH_fromATCTcCO2aq(CORAL(ng)%TAcoe(n,i,j),CORAL(ng)%DICcoe(iCt,n,i,j),CORAL(ng)%CO2coe(iCt,n,i,j),TKamb, Samb)
    HCO3coe(iCt)  = cHCO3_noneq(CORAL(ng)%DICcoe(iCt,n,i,j),CORAL(ng)%CO2coe(iCt,n,i,j),pHcoe,TKamb, Samb)
    CO3coe (iCt)  = cCO3_noneq(CORAL(ng)%DICcoe(iCt,n,i,j),CORAL(ng)%CO2coe(iCt,n,i,j),pHcoe,TKamb, Samb)!! for Isotope
    fCO2coe       = fCO2_fromcCO2aq(CORAL(ng)%CO2coe(iCt,n,i,j),TKamb, Samb)  !! for output

!--------- CO2 system in ambient seawater ------------------- !! for output
    pHamb        = pH_fromATCT(TAamb,DICamb(iCt),TKamb, Samb)
    CO2amb (iCt) = cCO2aq_fromCTpH(DICamb(iCt),pHamb,TKamb, Samb)
    HCO3amb(iCt) = cHCO3_fromCTpH (DICamb(iCt),pHamb,TKamb, Samb)
    CO3amb (iCt) = cCO3_fromCTpH  (DICamb(iCt),pHamb,TKamb, Samb)
    fCO2amb      = fCO2_fromcCO2aq(CO2amb(iCt),TKamb, Samb)  !! for output
    Wargamb      = Warg_fromcCO3  (CO3amb(iCt),TKamb, Samb)
!----CO2 equilibrium
#  if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE 
!----------- CO2 system in calcified fulid -------------------
    HCO3cal(iC13) = cH13CO3_noneq2(CORAL(ng)%DICcal(iC13,n,i,j), CORAL(ng)%CO2cal(iC13,n,i,j),HCO3cal(iCt),CO3cal(iCt),TKamb)
    CO3cal (iC13) = c13CO3_noneq2 (CORAL(ng)%DICcal(iC13,n,i,j), CORAL(ng)%CO2cal(iC13,n,i,j),HCO3cal(iCt),CO3cal(iCt),TKamb)
!----------- CO2 system in coelenteron -------------------
    HCO3coe(iC13) = cH13CO3_noneq2(CORAL(ng)%DICcoe(iC13,n,i,j), CORAL(ng)%CO2coe(iC13,n,i,j),HCO3coe(iCt),CO3coe(iCt),TKamb)
    CO3coe (iC13) = c13CO3_noneq2 (CORAL(ng)%DICcoe(iC13,n,i,j), CORAL(ng)%CO2coe(iC13,n,i,j),HCO3coe(iCt),CO3coe(iCt),TKamb)
!----------- CO2 system in ambient seawater -------------------
    CO2amb (iC13) = conc13CO2(DICamb(iC13),CO2amb(iCt),HCO3amb(iCt),CO3amb(iCt),TKamb)
#   if defined CLUMPED_ISOTOPE 
!----------- CO2 system in calcified fulid -------------------
    HCO3cal(iD47) = ???
    CO3cal (iD47) = ???
!----------- CO2 system in coelenteron -------------------
    HCO3coe(iD47) = ???
    CO3coe (iD47) = ???
!----------- CO2 system in ambient seawater -------------------
    CO2amb (iD47) = ???
#   endif
#  endif

!----CO2 system equilibrium is assumed
# else
!--------- CO2 system in calcified fulid -------------------
    pHcal        = pH_fromATCT(CORAL(ng)%TAcal(n,i,j),CORAL(ng)%DICcal(iCt,n,i,j),TKamb, Samb)
    CORAL(ng)%CO2cal(iCt,n,i,j) &
                 = cCO2aq_fromCTpH(CORAL(ng)%DICcal(iCt,n,i,j),pHcal,TKamb, Samb)
    HCO3cal(iCt) = cHCO3_fromCTpH (CORAL(ng)%DICcal(iCt,n,i,j),pHcal,TKamb, Samb)!! for Isotope
    CO3cal (iCt) = cCO3_fromCTpH  (CORAL(ng)%DICcal(iCt,n,i,j),pHcal,TKamb, Samb)
    fCO2cal      = fCO2_fromcCO2aq(CORAL(ng)%CO2cal(iCt,n,i,j),TKamb, Samb)  !! for output
    Wargcal      = Warg_fromcCO3cCa(CO3cal(iCt),10.4d0,TKamb, Samb)  !Calcifying fluid {Ca2+]=10.4 mmol kg-1   Assumed

!--------- CO2 system in coelenteron -------------------
    pHcoe        = pH_fromATCT(CORAL(ng)%TAcoe(n,i,j),CORAL(ng)%DICcoe(iCt,n,i,j),TKamb, Samb)
    CORAL(ng)%CO2coe(iCt,n,i,j) &
                 = cCO2aq_fromCTpH(CORAL(ng)%DICcoe(iCt,n,i,j),pHcoe,TKamb, Samb)!! for Isotope and output
    HCO3coe(iCt) = cHCO3_fromCTpH (CORAL(ng)%DICcoe(iCt,n,i,j),pHcoe,TKamb, Samb)
    CO3coe (iCt) = cCO3_fromCTpH  (CORAL(ng)%DICcoe(iCt,n,i,j),pHcoe,TKamb, Samb)!! for Isotope
    fCO2coe      = fCO2_fromcCO2aq(CORAL(ng)%CO2coe(iCt,n,i,j),TKamb, Samb)  !! for output

#  if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE 
!!! ----- Isotope calculation ----------------------------------------------------
    CORAL(ng)%CO2cal(iC13,n,i,j) &
                  = conc13CO2aq(CORAL(ng)%DICcal(iC13,n,i,j),CORAL(ng)%CO2cal(iCt,n,i,j),HCO3cal(iCt),CO3cal(iCt),TKamb)
    HCO3cal(iC13) = concH13CO3 (CORAL(ng)%DICcal(iC13,n,i,j),CORAL(ng)%CO2cal(iCt,n,i,j),HCO3cal(iCt),CO3cal(iCt),TKamb)
    CO3cal (iC13) = conc13CO3  (CORAL(ng)%DICcal(iC13,n,i,j),CORAL(ng)%CO2cal(iCt,n,i,j),HCO3cal(iCt),CO3cal(iCt),TKamb)
!----------- CO2 system in coelenteron -------------------
    CORAL(ng)%CO2coe(iC13,n,i,j) &
                  = conc13CO2aq(CORAL(ng)%DICcoe(iC13,n,i,j),CORAL(ng)%CO2coe(iCt,n,i,j),HCO3coe(iCt),CO3coe(iCt),TKamb)
    HCO3coe(iC13) = concH13CO3 (CORAL(ng)%DICcoe(iC13,n,i,j),CORAL(ng)%CO2coe(iCt,n,i,j),HCO3coe(iCt),CO3coe(iCt),TKamb)
    CO3coe (iC13) = conc13CO3  (CORAL(ng)%DICcoe(iC13,n,i,j),CORAL(ng)%CO2coe(iCt,n,i,j),HCO3coe(iCt),CO3coe(iCt),TKamb)
#   if defined CLUMPED_ISOTOPE 
!----------- CO2 system in calcified fulid -------------------
    HCO3cal(iD47) = ???
    CO3cal (iD47) = ???
!----------- CO2 system in coelenteron -------------------
    HCO3coe(iD47) = ???
    CO3coe (iD47) = ???
!----------- CO2 system in ambient seawater -------------------
    CO2amb (iD47) = ???
#   endif
#  endif
#  if defined CORAL_TESTMODE
!--------- CO2 system in ambient seawater ------------------- !! for output
    pHamb        = pH_fromATCT(TAamb,DICamb(iCt),TKamb, Samb)
    CO2amb (iCt) = cCO2aq_fromCTpH(DICamb(iCt),pHamb,TKamb, Samb)
    HCO3amb(iCt) = cHCO3_fromCTpH (DICamb(iCt),pHamb,TKamb, Samb)
    CO3amb (iCt) = cCO3_fromCTpH  (DICamb(iCt),pHamb,TKamb, Samb)
    fCO2amb      = fCO2_fromcCO2aq(CO2amb(iCt),TKamb, Samb)  !! for output
    Wargamb      = Warg_fromcCO3  (CO3amb(iCt),TKamb, Samb)
#  endif
# endif  /*end CORAL_NONE_CO2_EQ*/

!--- Carbon isotope or tracer ratio (R13C = 13C/12C) -----------------
    DO isp=1,N_Csp
      rQC    (isp) = Iso_ratio( CORAL(ng)%QC    (isp,n,i,j), CORAL(ng)%QC    (iCt,n,i,j) )   !coral organism
      rDICcal(isp) = Iso_ratio( CORAL(ng)%DICcal(isp,n,i,j), CORAL(ng)%DICcal(iCt,n,i,j) ) !DIC in calcifying fluid
      rCO2cal(isp) = Iso_ratio( CORAL(ng)%CO2cal(isp,n,i,j), CORAL(ng)%CO2cal(iCt,n,i,j) )
      rDICcoe(isp) = Iso_ratio( CORAL(ng)%DICcoe(isp,n,i,j), CORAL(ng)%DICcoe(iCt,n,i,j) ) !DIC in coelenteron
      rCO2coe(isp) = Iso_ratio( CORAL(ng)%CO2cal(isp,n,i,j), CORAL(ng)%CO2cal(iCt,n,i,j) )
      rDICamb(isp) = Iso_ratio( DICamb(isp), DICamb(iCt) )!!!!!  DIC in ambient seawater  
      rCO2amb(isp) = Iso_ratio( CO2amb(isp), CO2amb(iCt) )
!      rHCO3cal(isp) = Iso_ratio( cH13CO3cal(isp), HCO3cal(isp) )  !!!!!!!!!!!!!????????
# if defined CORAL_INGESTION
      DO m=1,Nzoo
        rZooC(isp,m)  = Iso_ratio( ZooC(isp,m), ZooC(iCt,m) )
      END DO
# endif
    END DO
# if defined CORAL_NUTRIENTS
!--- Nitrogen isotope or tracer ratio (R15N = 15N/14N) -----------------
    DO isp=1,N_Nsp
      rQN    (isp) = Iso_ratio( CORAL(ng)%QN (isp,n,i,j), CORAL(ng)%QN (iNt,n,i,j) )   !coral organism
      rNO3coe(isp) = Iso_ratio( CORAL(ng)%NO3(isp,n,i,j), CORAL(ng)%NO3(iNt,n,i,j) ) 
      rNH4coe(isp) = Iso_ratio( CORAL(ng)%NH4(isp,n,i,j), CORAL(ng)%NH4(iNt,n,i,j) ) 
      rNO3amb(isp) = Iso_ratio( NO3amb(isp), NO3amb(iNt) ) 
      rNH4amb(isp) = Iso_ratio( NH4amb(isp), NH4amb(iNt) ) 
#  if defined CORAL_INGESTION
      DO m=1,Nzoo
        rZooN(isp,m)  = Iso_ratio( ZooN(isp,m), ZooN(iNt,m) )  !!!!!!!!!!!!!!!!!!!!!!!
      END DO
#  endif
    END DO
!--- Phosphrous tracer ratio (R15N = 15N/14N) -----------------
    DO isp=1,N_Psp
      rQP    (isp) = Iso_ratio( CORAL(ng)%QP (isp,n,i,j), CORAL(ng)%QP (iPt,n,i,j) )   !coral organism
      rPO4coe(isp) = Iso_ratio( CORAL(ng)%PO4(isp,n,i,j), CORAL(ng)%PO4(iPt,n,i,j) ) 
      rPO4amb(isp) = Iso_ratio( PO4amb(isp), PO4amb(iPt) ) 
#  if defined CORAL_INGESTION
      DO m=1,Nzoo
        rZooP(isp,m)  = Iso_ratio( ZooP(isp,m), ZooP(iPt,m) )  !!!!!!!!!!!!!!!!!!!!!!!
      END DO
#  endif
    END DO

# endif

!=========================================================================
!     Mass fluxes between ambient seawater and coelenteron (nmol cm-2 s-1)
!=========================================================================

!---- calculate mass transfer velocity (cm s-1) ---------------------

!    Si = (65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    
!    Si_DIC=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
!    Si_TA =(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
!    Si_DO =(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_DIC(:)=S_i(tau_amb,dif_HCO3,340.0d0,1.05d0)*1.0d2
    Si_TA =S_i(tau_amb,dif_HCO3,340.0d0,1.05d0)*1.0d2
    Si_DO =S_i(tau_amb,dif_O2  ,340.0d0,1.05d0)*1.0d2
# if defined CORAL_INGESTION
    Si_PHY(:)=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_ZOO(:)=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
# endif
# if defined CORAL_NUTRIENTS
    Si_NO3(:)=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_NH4(:)=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_PO4(:)=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
# endif
# if defined CORAL_NONE_CO2_EQ
!    Si_CO2=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_CO2(:)=S_i(tau_amb,dif_CO2 ,340.0d0,1.05d0)*1.0d2
# endif
# if defined CORAL_ZOOXANTHELLAE
    Si_ROS=S_i(tau_amb,dif_H2O2,340.0d0,1.05d0)*1.0d2
# endif

!------- Flux calculation (nmol cm-2 s-1) ------------------------

! DIC uptake (nmol cm-2 s-1)
    Flux_DIC(:) = Si_DIC(:)*(DICamb(:)-CORAL(ng)%DICcoe(:,n,i,j))*rho_sw
# if defined CORAL_NONE_CO2_EQ
    Flux_CO2(:) = Si_CO2*(CO2amb(:)-CORAL(ng)%CO2coe(:,n,i,j))*rho_sw
# endif
! TA uptake (nmol cm-2 s-1)
    Flux_TA  = Si_TA *(TAamb-CORAL(ng)%TAcoe(n,i,j))*rho_sw
! DO uptake (nmol cm-2 s-1)
    Flux_DO  = Si_DO *(DOamb-CORAL(ng)%DOcoe(n,i,j))

!----- Summing Fluxes (nmol cm-2 s-1)--------------------------
    F_TAcoe = F_TAcoe + Flux_TA
    F_DOcoe = F_DOcoe + Flux_DO
    F_DICcoe(:) = F_DICcoe(:) + Flux_DIC(:)
# if defined CORAL_NONE_CO2_EQ
    F_CO2coe(:) = F_CO2coe(:) + Flux_CO2(:)
# endif

# if defined CORAL_NUTRIENTS

! Coral carbon cell quoto

    c_SQC=min((Flux_NH4 +Flux_NO3 )*c_CNP(nC)/c_CNP(nN)
                 ,Flux_PO4 *c_CNP(nC)/c_CNP(nP))

    c_SQC=CORAL(ng)%QC(n,i,j)/(1000.+CORAL(ng)%QC(n,i,j))*c_SQC
    c_SQN=c_SQC*c_CNP(nN)/c_CNP(nC)
    c_SQP=c_SQC*c_CNP(nP)/c_CNP(nC)

    tempb=Flux_NH4 /(Flux_NH4 +Flux_NO3 )*c_SQN
    Flux_NO3 =Flux_NO3 /(Flux_NH4 +Flux_NO3 )*c_SQN
    Flux_NH4 =tempb
    Flux_PO4 =c_SQP
    Flux_NH4 =0.
! NO3 uptake
    Flux_NO3 =0.
! PO4 uptake
    Flux_PO4 =0.

! NH4 uptake (nmol cm-2 s-1) 
    Flux_NH4(:) = Si_NH4*NH4amb
! NO3 uptake (nmol cm-2 s-1) 
    Flux_NO3(:) = Si_NO3*NO3amb
! PO4 uptake (nmol cm-2 s-1) 
    Flux_PO4(:) = Si_PO4*PO4amb

!----- Summing Fluxes (nmol cm-2 s-1)--------------------------
    F_NH4(:) = F_NH4(:) + Flux_NH4(:)
    F_NO3(:) = F_NO3(:) + Flux_NO3(:)
    F_PO4(:) = F_PO4(:) + Flux_PO4(:)

# endif

!----- Coral mucus release rate (nmol cm-2 s-1)--------------------------
# if defined CORAL_MUCUS
!
    DO m=1,Ndom
      F_Cmucus(m) = ForgC(n,m)  !!!! constant value is assumed
      Flux_DOC(:,m) = -F_Cmucus(m) *rQC(:)
!----- Summing Fluxes (nmol cm-2 s-1)--------------------------
      F_QC(:)  = F_QC(:) - Flux_DOC(:,m)
#  if defined CORAL_NUTRIENTS
      F_Nmucus(m) = ???
      F_Pmucus(m) = ???
      Flux_DON(:,m) = -F_Nmucus(m) *rQN(:)
      Flux_DOP(:,m) = -F_Pmucus(m) *rQP(:)
!----- Summing Fluxes (nmol cm-2 s-1)--------------------------
      F_QN(:)  = F_QN(:) - Flux_DON(:,m)
      F_QP(:)  = F_QP(:) - Flux_DOP(:,m)
#  endif
    END DO
# endif

!----- Plankton ingestion (nmol cm-2 s-1)--------------------------
# if defined CORAL_INGESTION
!
    DO m=1,Nzoo
      Flux_ZooC(:,m) = Si_ZOO * ZooC(:,m) *rZooC(:,m)
      Flux_POC(:,iLPOM)  = Flux_ZooC(:,m)*(1.0d0 - r_assim) *0.9d0
      Flux_POC(:,iRPOM)  = Flux_ZooC(:,m)*(1.0d0 - r_assim) *0.1d0
!----- Summing Fluxes (nmol cm-2 s-1)--------------------------
      F_QC(:)  = F_QC(:) + Flux_ZooC(:,m) * r_assim
#  if defined CORAL_NUTRIENTS
      Flux_ZooN(:) = Si_ZOO * ZooN(:)
      Flux_PON(:,iLPOM)  = Flux_ZooN(:,m)*(1.0d0 - r_assim) *0.9d0
      Flux_PON(:,iRPOM)  = Flux_ZooN(:,m)*(1.0d0 - r_assim) *0.1d0
      Flux_ZooP(:) = Si_ZOO * ZooP(:)
      Flux_POP(:,iLPOM)  = Flux_ZooP(:,m)*(1.0d0 - r_assim) *0.9d0
      Flux_POP(:,iRPOM)  = Flux_ZooP(:,m)*(1.0d0 - r_assim) *0.1d0
!----- Summing Fluxes (nmol cm-2 s-1)--------------------------
      F_QN(:)  = F_QN(:) + Flux_ZooN(:,m) * r_assim
      F_QP(:)  = F_QP(:) + Flux_ZooP(:,m) * r_assim
#  endif
    END DO
# endif



!======================================================================
!     Gross photosynthesis rate (nmol cm-2 s-1)
!======================================================================

!-------- Coral reserve OC pool (umolC cm-2) ----------------------------
    QCcr = max( CORAL(ng)%QC(iCt,n,i,j) - QC0(n), 0.0d0)

!   Sediment dependent term
    PFDsurf=PFD!*exp(-lamb*dsed)

!======================================================================
# if defined CORAL_ZOOXANTHELLAE
!-------- ROS release rate by mitochondria in coral  (nmol cm-2 s-1) ----------------------------
!    F_ROS = F_ROS -  a_cROS(n)*Tamb + b_cROS(n)
    F_ROS = F_ROS + a_cROS(n)*CORAL(ng)%R(n,i,j)

!-------- ROS detox rate (nmol cm-2 s-1) ----------------------------
!    F_ROS = F_ROS - V_detox(n)*CORAL(ng)%ROS(n,i,j)/(K_ROS(n)+CORAL(ng)%ROS(n,i,j))
    F_ROS = F_ROS - k_detox(n) * CORAL(ng)%ROS(n,i,j) * CAT(n)

!-------- ROS oxydation rate (damage rate) (nmol cm-2 s-1) ----------------------------
!    F_ROS = F_ROS -  k_dam(n) * CORAL(ng)%ROS(n,i,j)

!  Zooxanthellae release rate by host coral (cell cm-2 s-1)
!    Damage=exp(F_dam + 3.*F_ONOO)-1.   !Damage by ROS (F_dam) and peroxynitrite (F_ONOO)
!    Damage=exp(F_dam + F_ONOO)-1.   !Damage by ROS (F_dam) and peroxynitrite (F_ONOO)
!    Damage = exp( a_dam(n) * F_dam ) - 1.0d0   !Damage by ROS (F_dam)
!    F_Zelm  = k_Zelm(n) * Damage

    F_Zelm  = k_Zelm(n) * CORAL(ng)%ROS(n,i,j)*ZOOX(ng)%dens(n,i,j)
!    F_Zelm  = 0.0d0
    
!----- Zooxanthellae internal model -----

    CALL zooxanthellae             &
!     input parameters
      ( ng, n, i, j                & ! ng: nested grid number; n: coral compartment; i,j: position
      , date                       & ! Date (days since x or elapsed days)
      , dt                         & ! Time step (sec)
      , PFDsurf                    & ! Photon flux density (umol m-2 s-1)
      , Tamb                       & ! Temperature (oC)
      , CORAL(ng)%CO2coe(:,n,i,j)  & ! CO2* concentration in the coral tissue (umol kg-1)
      , CORAL(ng)%DOcoe (n,i,j)    & ! DO concentration in the coral tissue (umol L-1)
      , QCcr                       & ! Host reserve C pool (umolC cm-2)
      , F_Zelm                     & ! Zooxanthellae release rate by host coral (cell cm-2 s-1)
      , CORAL(ng)%ROS(n,i,j)       & ! ROS concentration in the coral tissue (umol L-1)
#  if defined CORAL_NUTRIENTS
      , CORAL(ng)%NO3(:,n,i,j)    & ! NO3 concentration in the coral tissue (umol kg-1)
      , CORAL(ng)%NH4(:,n,i,j)    & ! NH4 concentration in the coral tissue (umol kg-1)
      , CORAL(ng)%PO4(:,n,i,j)    & ! PO4 concentration in the coral tissue (umol kg-1)
#  endif
!   input and output parameters
      , F_QC(:)                    & ! Organic C translocation (nmol C cm-2 s-1) *direction of Symbiont to host coral is positivell-1 s-1)
      , F_DICcoe(:)                & ! Flux of DIC (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
      , Flux_POC(:,:)              & ! Flux of POC as zooxanthellae release (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
#  if defined CORAL_NONE_CO2_EQ
      , F_CO2coe(:)                & ! Flux of DIC (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
#  endif
      , F_DOcoe                    & ! Flux of DO  (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
      , F_ROS                      & ! Flux of Reactive Oxygen Species (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
#  if defined CORAL_NUTRIENTS
      , F_QN(:)                    & ! Organic N excretion controled by host coral (pmol cell-1 s-1)
      , F_QP(:)                    & ! Organic P excretion controled by host coral (pmol cell-1 s-1)
      , F_NO3(:)                   & ! NO3 transport rate from host to Zooxanthellae (pmol cell-1 s-1)
      , F_NH4(:)                   & ! NH4 transport rate from host to Zooxanthellae (pmol cell-1 s-1)
      , F_PO4(:)                   & ! PO4 transport rate from host to Zooxanthellae (pmol cell-1 s-1)
      , Flux_PON(:,:)              & ! Flux of PON as zooxanthellae release (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
      , Flux_POP(:,:)              & ! Flux of POP as zooxanthellae release (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
  !      , F_ONOO                     & ! Flux of peroxynitrite  (pmol cell-1 s-1)
#  endif
!   output parameters
      , CORAL(ng)%Pg(n,i,j)        & ! Total gross photosynthetic rate (nmol cm-2 s-1)
      , Rz                         & ! Total respiration rate (nmol cm-2 s-1)
      )


!======================================================================
# else
!  ----- Gross photosynthetic rate (nmolC cm-2 s-1) ------------------------------------
! Temperature dependent term
    f_temp=1.d0
!    f_temp=exp(Hact(n)*(TKamb-298.)/298./Rgas/TKamb)        &
!          *(1.+exp((298.*Entr(n)-Hdeact(n))/298./Rgas))     &
!          /(1.+exp((TKamb*Entr(n)-Hdeact(n))/TKamb/Rgas))

! Gross photosynthesis rate (nmolO2 cm-2 s-1)
!    Pg=Pgmax(n)*f_temp*tanh(PFDsurf/Ik(n))                  &
!        *1./(V_HCO3(n)+V_CO2(n))                          &
!          *( V_HCO3(n)*HCO3coe/(K_HCO3(n)+HCO3coe)        &
!            +V_CO2(n)*CORAL(ng)%CO2coe(n,i,j)/(K_CO2(n)+CORAL(ng)%CO2coe(n,i,j))   &
!           )

    CORAL(ng)%Pg(n,i,j) = Pgmax(n)*f_temp*tanh(PFDsurf/Ik(n))       &
                        * HCO3coe/(K_HCO3(n)+HCO3coe)
    Rz = 0.0d0

!----- Summing Fluxes (nmol cm-2 s-1)--------------------------
    F_DOcoe = F_DOcoe + CORAL(ng)%Pg(n,i,j)
    F_QC(:)     = F_QC(:)     + CORAL(ng)%Pg(n,i,j) *rDICcoe(:)*aC_phot(:)
    F_DICcoe(:) = F_DICcoe(:) - CORAL(ng)%Pg(n,i,j) *rDICcoe(:)*aC_phot(:) 

# endif

!======================================================================
!     Respilation rate (nmol cm-2 s-1)
!======================================================================

!   Temperature dependent term
    f_temp=1.d0
!    f_temp=exp(Eact(n)*(TKamb-298.)/298./Rgas/TKamb)
    f_min = min( QCcr/(K_QC(n)+QCcr),                      &
                 CORAL(ng)%DOcoe(n,i,j)/(K_DO(n)+CORAL(ng)%DOcoe(n,i,j)) )  !  Michaelis-Menten equation

    Rc = Rmax(n) * f_temp * f_min
    CORAL(ng)%R(n,i,j) = Rc + Rz

!----- Summing Fluxes (nmol cm-2 s-1)--------------------------
    F_DOcoe = F_DOcoe - Rc
    F_QC(:)    = F_QC(:)    - Rc *rQC(:)
# if defined CORAL_NONE_CO2_EQ
    F_CO2coe(:) = F_CO2coe(:) + Rc *rQC(:) *Rratio(n)
    F_CO2cal(:) = F_CO2cal(:) + Rc *rQC(:) *(1.0d0-Rratio(n))
# else
    F_DICcoe(:) = F_DICcoe(:) + Rc *rQC(:) *Rratio(n)
    F_DICcal(:) = F_DICcal(:) + Rc *rQC(:) *(1.0d0-Rratio(n))
# endif
  

!======================================================================
!     Allocaion of energy produced by respiration (nJ cm-2 s-1)
!======================================================================

!   energy Flux (nJ cm-2 s-1) partition

!  C6H12O6 + 6 O2 + 30 ADP + 30 Pi -> 6 CO2 + 6 H2O + 30 ATP
!  ATP + H2O -> ADP + Pi  dG=-7.3kcal/mol=-30.5kJ/mol

!    E_tot=30.5d3 * 30.0d0/6.0d0 * Rc  !nJ cm-2 s-1

!  C6H12O6 + 6 O2 -> 6 CO2 + 6 H2O  dG=-2850 kJ/mol (dG=-475 kJ/molC)
    E_tot= 475.0d3 * Rc  !nJ cm-2 s-1
    E_m = C_dec(n)*Ebs

    if (E_tot > E_m ) then
# if defined CORAL_SIZE_DYNAMICS
      netCgrowth = ( CORAL(ng)%growth(n,i,j)-CORAL(ng)%mort(n,i,j) )  &
                   *CORAL(ng)%QC(iCt,n,i,j)*1.0d3    ! growth rate (nmol cm-2 s-1)=(cm2 cm-2 s-1)*(umol cm-2)*1.d3
      g2c_tmp = netCgrowth/CORAL(ng)%G(n,i,j)
      E_g = (E_tot - E_m)*g2c_tmp
      E_ca = E_tot - E_m - E_g
# else
!      E_ca = E_tot-E_m
!    Energy for 1 mol CaCO3 production:           2*E_rq_H_transport                        
!    Energy for 1 mol proton (tisuue) production: Ebs
      E_ca = (E_tot-E_m) * 2.0d0*E_rq_H_transport*c2g(n)/(2.0d0*E_rq_H_transport*c2g(n) + Ebs)
      E_g  = E_tot - E_m - E_ca

# endif
    else
      E_ca = 0.0d0
      E_g  = 0.0d0
# if defined CORAL_SIZE_DYNAMICS
      mort_es = km_es(n)*( E_m - E_tot ) ! Mortality due to energy shortage
# endif
    endif

!======================================================================
!     Mass fluxes related to growth (nmol cm-2 s-1)
!======================================================================

    F_Cgrowth = E_g/Ebs

# if defined CORAL_SIZE_DYNAMICS
!
!---------- Coral growth (nmol cm-2 s-1) ------------
!
!    Droop model (1973) + Liebig's minimum law

#  if defined CORAL_NUTRIENTS

    F_Cgrowth = g_max(n)*min( 1.0d0 - QC0(n)/CORAL(ng)%QC(iCt,n,i,j) ,    &
                          min(1.0d0 - QN0(n)/CORAL(ng)%QN(iNt,n,i,j),     &
                              1.0d0 - QP0(n)/CORAL(ng)%QP(iPt,n,i,j) ) )  &
    F_Cgrowth = max(F_Cgrowth, 0.0d0)
    
    F_Ngrowth = F_Cgrowth*CORAL(ng)%QN(n,i,j)/CORAL(ng)%QC(iCt,n,i,j)
    F_Pgrowth = F_Cgrowth*CORAL(ng)%QP(n,i,j)/CORAL(ng)%QC(iCt,n,i,j)

#  else
!    F_Cgrowth = g_max(n) * (1.0d0 - QC0(n)/CORAL(ng)%QC(n,i,j) )
    F_Cgrowth = 5.0d-5 * (CORAL(ng)%QC(iCt,n,i,j) - QC0(n))
    F_Cgrowth = max(F_Cgrowth, 0.0d0)
#  endif
!
!----- Convert internal conditions to size dynamics parameters
!
!---------- Coral growth rate (cm cm-2 s-1) ------------
!
    CORAL(ng)%growth(n,i,j) = F_Cgrowth/CORAL(ng)%QC(iCt,n,i,j)*1.0d-3  ! growth rate (cm2 cm-2 s-1)
!   (cm2 cm-2 s-1)=(nmol cm-2 s-1)/(umol cm-2)*1.d-3
!
!---------- Coral mortality (cm cm-2 s-1) ------------
!
#  if defined CORAL_NUTRIENTS

    CORAL(ng)%mort(n,i,j) = km_CNP                                   &  ! mortality (cm2 cm-2 s-1) 
                    *exp(-am_CNP*                                    &  !!!!!!!!!!!!!!!!TBC
                        min( 1.0d0 - QC0(n)/CORAL(ng)%QC(iCt,n,i,j) ,    &
                         min(1.0d0 - QN0(n)/CORAL(ng)%QN(iNt,n,i,j),     &
                             1.0d0 - QP0(n)/CORAL(ng)%QP(iPt,n,i,j) )))  & 
                   +km_dam * Damage                                  & !!!!!!!!!!!!!!!TBC
                   +mort_es !!!!!!!!!!!!!!!TBC
#  else
    CORAL(ng)%mort(n,i,j) =                                   &
          km(n) *exp( aC_m*(QC0(n)-CORAL(ng)%QC(iCt,n,i,j)) )     &
         +km_dam * Damage                                     & !!!!!!!!!!!!!!!!!TBC
         +mort_es

!    CORAL(ng)%mort(n,i,j) = min(CORAL(ng)%mort(n,i,j), 1.0d0)  !!! for Error handling

!-----------END CORAL_SIZE_DYNAMICS ------------------------------------
#  endif
# endif

!----- Summing Fluxes (nmol cm-2 s-1)--------------------------
    F_QC(:) = F_QC(:) - F_Cgrowth*rQC(:)
# if defined CORAL_NUTRIENTS
    F_QN(:) = F_QN(:) - F_Ngrowth*rQN(:)
    F_QP(:) = F_QP(:) - F_Pgrowth*rQP(:)
# endif


!======================================================================
!     H+ flux through Ca2+ ATPase  (nmol cm-2 s-1)
!     (TA flux from coelenteron to calcifying fluid (nmol cm-2 s-1) )
!======================================================================

!  Energy for 1 mol H+ transport from coe. to cal.: E_rq_H_transport                        
    E_rq_H_transport = (2.3d0*8.31d0*(Tamb+273.15d0)                &
                       *max((pHcal-pHcoe)+0.01d0, 0.01d0)) / eeff(n)

!  H+ flux (nmol cm-2 s-1)
    F_H = E_ca / E_rq_H_transport

!----- Summing Fluxes (nmol cm-2 s-1)--------------------------
    F_TAcoe = F_TAcoe - F_H
    F_TAcal = F_TAcal + F_H

!======================================================================
!     Mass fluxes between coelenteron and calcifying fluid (nmol cm-2 s-1)
!======================================================================

    k_CO2i_tmp(:) = k_CO2i(n)
    k_DIC_tmp(:)  = k_DIC(n)
# if defined CORAL_NONE_CO2_EQ
    k_CO2_tmp(:)  = k_DIC(n)/1.0007d0 !????????????????????????????????????
# endif
# if defined CARBON_ISOTOPE
    k_CO2i_tmp(iC13) = k_CO2i_tmp(iCt)/1.0007d0
    k_DIC_tmp(iC13)  = k_DIC_tmp(iCt)/1.00087d0
#  if defined CORAL_NONE_CO2_EQ
    k_CO2_tmp(iC13) = k_CO2_tmp(iCt)/1.0007d0  !????????????????????????????????????
#  endif
# endif
# if defined CLUMPED_ISOTOPE
    k_CO2i_tmp(iD47) = k_CO2i_tmp(iCt)/1.0007d0  !????????????????????????????????????
    k_DIC_tmp(iD47)  = k_DIC_tmp(iCt)/1.00087d0  !????????????????????????????????????
#  if defined CORAL_NONE_CO2_EQ
    k_CO2_tmp(iD47)  = k_CO2_tmp(iCt)/1.0007d0  !????????????????????????????????????
#  endif
# endif

!   CO2* flux (nmol cm-2 s-1) through membrane permeation
!    Fpm_CO2(isp)=k_CO2i(n)*(fCO2coe-fCO2cal)
    Fpm_CO2(:) = k_CO2i_tmp(:) *(CORAL(ng)%CO2coe(:,n,i,j)-CORAL(ng)%CO2cal(:,n,i,j))*rho_sw
    
!   DIC Leak flux (nmol cm-2 s-1) 
    Fpp_DIC(:) = k_DIC_tmp(:) *(CORAL(ng)%DICcoe(:,n,i,j)-CORAL(ng)%DICcal(:,n,i,j))*rho_sw
# if defined CORAL_NONE_CO2_EQ
    Fpp_CO2(:) = k_CO2_tmp(:) *(CORAL(ng)%CO2coe(:,n,i,j)-CORAL(ng)%CO2cal(:,n,i,j))*rho_sw
# endif
!   TA Leak flux (nmol cm-2 s-1) 
    Fpp_TA = k_TA(n) *(CORAL(ng)%TAcoe(n,i,j)-CORAL(ng)%TAcal(n,i,j))*rho_sw

!----- Summing Fluxes (nmol cm-2 s-1)--------------------------
    F_TAcoe = F_TAcoe - F_H
    F_TAcal = F_TAcal + F_H
    F_DICcoe(:) = F_DICcoe(:) - Fpm_CO2(:) - Fpp_DIC(:)
    F_DICcal(:) = F_DICcal(:) + Fpm_CO2(:) + Fpp_DIC(:)
# if defined CORAL_NONE_CO2_EQ
    F_CO2coe(:) = F_CO2coe(:) - Fpm_CO2(:) - Fpp_CO2(:)
    F_CO2cal(:) = F_CO2cal(:) + Fpm_CO2(:) + Fpp_CO2(:)
# endif

!======================================================================
!     Calcification rate (nmol cm-2 s-1)
!======================================================================
 
    if (Wargcal .ge. 1.d0) then
      CORAL(ng)%G(n,i,j)=1.1d-3*(Wargcal-1.d0)**1.63d0
    else
      CORAL(ng)%G(n,i,j)=-2.7d-2*(1.d0-Wargcal)**2.5d0
    endif

!----- Summing Fluxes (nmol cm-2 s-1)--------------------------
    F_TAcal = F_TAcal - 2.0d0*CORAL(ng)%G(n,i,j)
    F_DICcal(:) = F_DICcal(:) - CORAL(ng)%G(n,i,j) *aC_calc(1:N_Csp)*rDICcal(:)


!======================================================================
!     Time stepping (internal condition changes)
!======================================================================

!   CORAL(ng)%QC (umol cm-2)
!   Pg,R: nmol cm-2 s-1
    CORAL(ng)%QC(:,n,i,j) = CORAL(ng)%QC(:,n,i,j) + F_QC(:)*1.d-3 *dt    !nmol cm-2 =1.e-3 umol cm-2

!    DIC & TA (umol kg-1) in calcified fulid
    CORAL(ng)%DICcal(:,n,i,j) = CORAL(ng)%DICcal(:,n,i,j)         &
                               + F_DICcal(:)/hcal/rho_sw *dt    !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1

    CORAL(ng)%TAcal (n,i,j) = CORAL(ng)%TAcal (n,i,j)             &
                               + F_TAcal/hcal/rho_sw *dt    !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1

!    DIC & TA (umol kg-1) in coelectron
    CORAL(ng)%DICcoe(:,n,i,j) = CORAL(ng)%DICcoe(:,n,i,j)         &
                               + F_DICcoe(:)/hcoe/rho_sw *dt   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1

    CORAL(ng)%TAcoe (n,i,j) = CORAL(ng)%TAcoe (n,i,j)             &
                               + F_TAcoe/hcoe/rho_sw *dt   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1

!    DO (umol kg-1) in coelectron
    CORAL(ng)%DOcoe (n,i,j) = CORAL(ng)%DOcoe (n,i,j)             &
                              + F_DOcoe/hcoe *dt   !nmol cm-3 s-1 = 1. umol L-1 s-1
    CORAL(ng)%DOcoe (n,i,j)=max( CORAL(ng)%DOcoe (n,i,j),0.d0) !Error handring

# if defined CORAL_NONE_CO2_EQ

    kp1(:)=kp1_Johnson1982(TKamb, Samb)
    km1(:)=km1_Johnson1982(TKamb, Samb)
    kp4(:)=kp4_Johnson1982(TKamb, Samb)
    km4(:)=km4_Johnson1982(TKamb, Samb)
#  if defined CARBON_ISOTOPE
    kp1(iC13)=kp1(iCt)/1.013d0
    km1(iC13)=km1(iCt)/1.022d0
    kp4(iC13)=kp4(iCt)/1.011d0
    km4(iC13)=km4(iCt)/1.027d0
#  endif
#  if defined CLUMPED_ISOTOPE
    kp1(iD47)=kp1(iCt)/1.013d0  !??????????????????????????????????????????????????????
    km1(iD47)=km1(iCt)/1.022d0  !??????????????????????????????????????????????????????
    kp4(iD47)=kp4(iCt)/1.011d0  !??????????????????????????????????????????????????????
    km4(iD47)=km4(iCt)/1.027d0  !??????????????????????????????????????????????????????
#  endif

    cHcoe =10.d0**(-pHcoe)
    cOHcoe=Kw_Millero1995(TKamb, Samb)/cHcoe
    
    cHcal =10.d0**(-pHcal)
    cOHcal=Kw_Millero1995(TKamb, Samb)/cHcal

!    [CO2*] (umol kg-1) in coelectron
    CORAL(ng)%CO2coe(:,n,i,j)=CORAL(ng)%CO2coe(:,n,i,j)              &
                + F_CO2coe(:)/hcoe/rho_sw *dt                        &   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
!     kinetics of CO2 system
                +( (km1(:)*cHcoe+km4)*HCO3coe(:)                     & ! umol kg-1 s-1
                  -(kp1(:)+kp4(:)*cOHcoe)*CORAL(ng)%CO2coe(:,n,i,j)  & ! umol kg-1 s-1
                 ) *dt   ! umol kg-1

    if(CORAL(ng)%CO2coe(n,i,j)<0.0d0) then
      write(*,*) 'DEBUG1',CORAL(ng)%CO2coe(n,i,j)
      Stop
    endif

!    [CO2*] (umol kg-1) in calcified fulid
    CORAL(ng)%CO2cal(n,i,j)=CORAL(ng)%CO2cal(n,i,j)                  &
                + F_CO2cal(:)/hcal/rho_sw *dt                        &!nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
!     kinetics of CO2 system
                +( (km1(:)*cHcal+km4(:))*HCO3cal(:)                  &! umol kg-1 s-1
                  -(kp1(:)+kp4(:)*cOHcal)*CORAL(ng)%CO2cal(:,n,i,j)  &! umol kg-1 s-1
                 ) *dt   ! umol kg-1
    
    if(CORAL(ng)%CO2cal(n,i,j)<0.0d0) then
      write(*,*) 'DEBUG2',CORAL(ng)%CO2cal(n,i,j)
      Stop
    endif
# endif

# if defined CORAL_ZOOXANTHELLAE
!!!
!!! ----- Zooxanthellae interaction ----------------------------------------------------
!!!
!    ROS (umol/L) mass balance

    CORAL(ng)%ROS(n,i,j)=CORAL(ng)%ROS(n,i,j) + F_ROS/htis *dt            !umol L-1
    CORAL(ng)%ROS(n,i,j)=max(CORAL(ng)%ROS(n,i,j),0.d0) !Error handring

# endif

!======== CORAL_TESTMODE ===============================================================
# if defined CORAL_TESTMODE

!------ Calculation of d13C -------------------------
#  if defined CARBON_ISOTOPE      
    d13C_DICamb  = delta_from_Ci_Ct( DICamb (iC13), DICamb (iCt), R13C_VPDB )
    d13C_CO2amb  = delta_from_Ci_Ct( CO2amb (iC13), CO2amb (iCt), R13C_VPDB )
    d13C_HCO3amb = delta_from_Ci_Ct( HCO3amb(iC13), HCO3amb(iCt), R13C_VPDB )
    d13C_CO3amb  = delta_from_Ci_Ct( CO3amb (iC13), CO3amb (iCt), R13C_VPDB )
    d13C_DICcoe  = delta_from_Ci_Ct( CORAL(ng)%DICcoe(iC13,n,i,j), CORAL(ng)%DICcoe(iCt,n,i,j), R13C_VPDB )
    d13C_CO2coe  = delta_from_Ci_Ct( CORAL(ng)%CO2coe(iC13,n,i,j), CORAL(ng)%CO2coe(iCt,n,i,j), R13C_VPDB )
    d13C_HCO3coe = delta_from_Ci_Ct( HCO3coe(iC13), HCO3coe(iCt), R13C_VPDB )
    d13C_CO3coe  = delta_from_Ci_Ct( CO3coe (iC13), CO3coe (iCt), R13C_VPDB )
    d13C_DICcal  = delta_from_Ci_Ct( CORAL(ng)%DICcal(iC13,n,i,j), CORAL(ng)%DICcal(iCt,n,i,j), R13C_VPDB )
    d13C_CO2cal  = delta_from_Ci_Ct( CORAL(ng)%CO2cal(iC13,n,i,j), CORAL(ng)%CO2cal(iCt,n,i,j), R13C_VPDB )
    d13C_HCO3cal = delta_from_Ci_Ct( HCO3cal(iC13), HCO3cal(iCt), R13C_VPDB )
    d13C_CO3cal  = delta_from_Ci_Ct( CO3cal (iC13), CO3cal (iCt), R13C_VPDB )

    d13C_QC      = delta_from_Ci_Ct( CORAL(ng)%QC    (iC13,n,i,j), CORAL(ng)%QC    (iCt,n,i,j), R13C_VPDB )

    d13C_G       = delta_from_Ci_Ct(  CORAL(ng)%G(n,i,j)*aC_calc(iC13)*rDICcal(iC13) &
                                    , CORAL(ng)%G(n,i,j), R13C_VPDB  )

#  endif

!------------------------------------------------------------------------
! Print section

#  if defined CORAL_BORON_ISOTOPE
    d11Barg = d11B_BOH4_frompHd11BT(pHcal, 39.5d0, TKamb, Samb)
#  endif

    IF( date*86400.0d0 >= dsec ) THEN
      if(dsec == 0.0d0) then
        dsec=date*86400.0d0+CORAL_OUTPUT_INTERVAL*60.0d0
      else
        dsec=dsec+CORAL_OUTPUT_INTERVAL*60.0d0
      endif
       
      write(10+n,'(*(g0:,", "))') date, PFD                                      &
       , CORAL(ng)%Pg(n,i,j), CORAL(ng)%R (n,i,j)                                &
       , CORAL(ng)%Pg(n,i,j)-CORAL(ng)%R (n,i,j)                                 &
       , CORAL(ng)%G (n,i,j), CORAL(ng)%QC(iCt,n,i,j)                            &
       , CORAL(ng)%TAcal(n,i,j), CORAL(ng)%TAcoe(n,i,j),TAamb                    &
       , CORAL(ng)%DICcal(iCt,n,i,j), CORAL(ng)%DICcoe(iCt,n,i,j),DICamb(iCt)    &
       , CORAL(ng)%DOcoe(n,i,j), DOamb                                           &
       , pHcal, pHcoe, pHamb, Wargcal, Wargamb                                   &
       , fCO2cal, fCO2coe, fCO2amb                                               &
       , CORAL(ng)%CO2cal(iCt,n,i,j), HCO3cal(iCt),CO3cal(iCt)                   &
       , CORAL(ng)%CO2coe(iCt,n,i,j), HCO3coe(iCt),CO3coe(iCt)                   &
#  if defined CARBON_ISOTOPE
       , d13C_DICamb, d13C_CO2amb, d13C_HCO3amb, d13C_CO3amb                     &
       , d13C_DICcoe, d13C_CO2coe, d13C_HCO3coe, d13C_CO3coe                     &
       , d13C_DICcal, d13C_CO2cal, d13C_HCO3cal, d13C_CO3cal                     &
       , d13C_QC, d13C_G                     &
#  endif
#  if defined CORAL_ZOOXANTHELLAE
! TODO アウトプット未検討
       , CORAL(ng)%ROS(n,i,j), Rz, F_Csec, F_zROS, F_detox, F_dam                &
#  endif
#  if defined CORAL_BORON_ISOTOPE
       , d11Barg                                                                 &
#  endif
       , Flux_DOC(iCt,:)                                                           &
#  if defined CORAL_INGESTION
       , Flux_ZooC(iCt,:)                                                          &
#  endif
#  if defined CORAL_SIZE_DYNAMICS
       , CORAL(ng)%growth(n,i,j), CORAL(ng)%mort(n,i,j)                          &
       , Damage,F_Cgrowth                                                  &
#  endif
       , E_ca, F_Cgrowth, Si_DIC(iCt), Si_DO
    END IF

! Coral record calculation section

    S_PFD_dt(n)  = S_PFD_dt(n)+PFD*dt
    S_Gn_dt(:,n) = S_Gn_dt(:,n)+CORAL(ng)%G (n,i,j)*aC_calc(1:N_Csp)*rDICcal(:) *dt
    S_Pg_dt(n)   = S_Pg_dt(n)+CORAL(ng)%Pg(n,i,j)*dt
    S_R_dt(n)    = S_R_dt(n) +CORAL(ng)%R(n,i,j) *dt
#  if defined CARBON_ISOTOPE
    d13C_arg = delta_from_Ci_Ct( S_Gn_dt(iC13,n), S_Gn_dt(iCt,n), R13C_VPDB)
#  endif
#  if defined CORAL_BORON_ISOTOPE
    S_d11BargxGn_dt(n)=S_d11BargxGn_dt(n)+d11Barg*CORAL(ng)%G (n,i,j)*dt
#  endif

!    IF(mod(int(date*86400.0d0), int(CORAL_AVERAGE_INTERVAL*86400.0d0))==0) THEN
    IF( date >= dday ) THEN   
      if(dday == 0.0d0) then
        dday=date+CORAL_AVERAGE_INTERVAL
      else
        dday=dday+CORAL_AVERAGE_INTERVAL
      endif

      write(20+n,'(*(g0:,", "))') date    &
       , S_PFD_dt(n)*1.d-6                &   ! Photon flux density (mol m-2 d-1)
#  if defined CARBON_ISOTOPE
       , S_Gn_dt(:,n)*1.d-3               &   ! Calcification rate (umol cm-2 d-1)
       , d13C_arg                         &   ! 1 day avaraged value of d13C_QC
#  endif
#  if defined CORAL_BORON_ISOTOPE
       , S_d11BargxGn_dt(n)/S_Gn_dt(n)    & 
#  endif
       , S_Pg_dt(n)*1.d-3                 &   ! Gross photosynthesis rate (umol cm-2 d-1)
       , S_R_dt(n)*1.d-3                  &   ! Respiration rate (umol cm-2 d-1)
       , (S_Pg_dt(n)-S_R_dt(n))*1.d-3         ! Net photosynthesis rate (umol cm-2 d-1)
       
      S_PFD_dt(n)=0.d0
      S_Gn_dt(:,n)=0.d0
      S_Pg_dt(n)=0.d0
      S_R_dt(n)=0.d0
#  if defined CARBON_ISOTOPE
      S_d13CargxGn_dt(n)=0.d0
#  endif
#  if defined CORAL_BORON_ISOTOPE
      S_d11BargxGn_dt(n)=0.d0
#  endif
    END IF
# endif

    RETURN

  END SUBROUTINE coral_polyp
    
!!! **********************************************************************
!!!  Mass transfer function
!!! **********************************************************************
! Falter et al., (2016) Geophys. Res. Lett. 43: 9764-9772
! Chan et al., (2016) Frontiers in Marine Science 3:
! Shapiro et al., (2014) PNAS 111: 13391-13396

  real(8) function S_i(tau,D,ak,Eosc) ! (m s-1)
    implicit none
    real(8), intent(in) :: tau  ! Bottom shear stress (N m-2)
    real(8), intent(in) :: D    ! Molecular diffusion coefficient (m2 s-1)
    real(8), intent(in) :: ak   ! Scaling coefficient (Falter et al., 2016)
                                ! 240-280: most experimental reef communities
                                ! 300-400 (median~340): natural reef communities
    real(8), intent(in) :: Eosc ! Ratio ofmasstransfer ratesunderoscillatory 
                                ! to unidirectional flow (Falter et al., 2016);
                                ! 1.1: pure wave-driven oscillatory flow
                                ! 1.0: pure unidirectional flow
    
    real(8), parameter :: vis_sw = 0.94d-6  ! kinematic viscosity   (m2 s-1)
    real(8), parameter :: d_DBL = 1.0d-3    ! Maximum boundary layer depth (m)
                                            ! ~1 mm: due to vortical ciliary flows (Shapiro et al., 2014)
    real(8) :: Sc               ! Schmidt number
    real(8) :: S_high,S_low
    
    Sc = vis_sw/D
    S_high = ak*Eosc*tau**0.4d0*Sc**(-0.6d0)/86400.0d0  !!! day to sec
    S_low  = D/d_DBL
    
    S_i=max(S_high, S_low)
    
    return
  end function S_i


# if defined CORAL_ZOOXANTHELLAE

!!! **********************************************************************
!!!  Zooxanthellae population dynamics
!!! **********************************************************************

  SUBROUTINE zooxanthellae &
!   input parameters
    ( ng, n, i, j  & ! ng: nested grid number; n: coral compartment; i,j: position
    , date         & ! Date (days since x or elapsed days)
    , dt           & ! Time step (sec)
    , PFDsurf      & ! Photon flux density (umol m-2 s-1)
    , Tamb         & ! Temperature (oC)
    , CO2coe       & ! CO2*(aq) concentration in the coral tissue (umol kg-1)
    , DOcoe        & ! DO concentration in the coral tissue (umol L-1)
    , QCcr         & ! Host reserve C pool (umolC cm-2)
    , F_Zelm       & ! Zooxanthellae release rate by host coral (cell cm-2 s-1)
    , ROS          & ! ROS concentration in the coral tissue (umol L-1)
#  if defined CORAL_NUTRIENTS
    , NO3          & ! NO3 concentration in the coral tissue (umol kg-1)
    , NH4          & ! NH4 concentration in the coral tissue (umol kg-1)
    , PO4          & ! PO4 concentration in the coral tissue (umol kg-1)
#  endif
!   input and output parameters
    , F_QC         & ! Organic C translocation (nmol C cm-2 s-1) *direction of Symbiont to host coral is positive
    , F_DIC        & ! Flux of DIC (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
    , F_POC        & ! Flux of POC as zooxanthellae release (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
#  if defined CORAL_NONE_CO2_EQ
    , F_CO2        & ! Flux of CO2 (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
#  endif
    , F_DO         & ! Flux of DO  (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
    , F_ROS        & ! Flux of Reactive Oxygen Species (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
#  if defined CORAL_NUTRIENTS
    , F_QN         & ! Organic N excretion controled by host coral (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
    , F_QP         & ! Organic P excretion controled by host coral (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
    , F_NO3        & ! NO3 transport rate from host to Zooxanthellae (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
    , F_NH4        & ! NH4 transport rate from host to Zooxanthellae (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
    , F_PO4        & ! PO4 transport rate from host to Zooxanthellae (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
    , F_PON        & ! Flux of PON as zooxanthellae release (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
    , F_POP        & ! Flux of POP as zooxanthellae release (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
!    , F_ONOO       & ! Flux of peroxynitrite (nmol cm-2 s-1) *direction of Symbiont to host coral is positive
#  endif
!   output parameters
    , Pg           & ! Total gross photosynthetic rate (nmol cm-2 s-1)
    , Rz           & ! Total respiration rate (nmol cm-2 s-1)
    )
!-----------------------------------------------------------------------

    USE mod_reef_ecosys_param
    USE mod_geochem

    implicit none

!   input parameters
    integer, intent(in) :: ng, n, i, j
    real(8), intent(in) :: date
    real(8), intent(in) :: dt
    real(8), intent(in) :: PFDsurf
    real(8), intent(in) :: Tamb
    real(8), intent(in) :: CO2coe(N_Csp), DOcoe
    real(8), intent(in) :: QCcr
    real(8), intent(in) :: F_Zelm
    real(8), intent(in) :: ROS
#  if defined CORAL_NUTRIENTS
    real(8), intent(in) :: NO3(N_Nsp), NH4(N_Nsp)
    real(8), intent(in) :: PO4(N_Psp)
#  endif
!   input and output parameters
    real(8), intent(inout) :: F_QC(N_Csp)
    real(8), intent(inout) :: F_DIC(N_Csp)
    real(8), intent(inout) :: F_POC(N_Csp,Npom)
#  if defined CORAL_NONE_CO2_EQ
    real(8), intent(inout) :: F_CO2(N_Csp)
#  endif
    real(8), intent(inout) :: F_DO, F_ROS
#  if defined CORAL_NUTRIENTS
    real(8), intent(inout) :: F_QN(N_Nsp)
    real(8), intent(inout) :: F_QP(N_Psp)
    real(8), intent(inout) :: F_NO3(N_Nsp), F_NH4(N_Nsp)
    real(8), intent(inout) :: F_PO4(N_Psp)
    real(8), intent(inout) :: F_PON(N_Nsp,Npom)
    real(8), intent(inout) :: F_POP(N_Psp,Npom)
!    real(8), intent(inout) :: F_ONOO
#  endif
!   output parameters
    real(8), intent(out) :: Pg
    real(8), intent(out) :: Rz
!    real(8), intent(out) :: F_Zmort

!------------Set Constants  ----------------------------------
!----- Zooxanthella physica condition ---------------------------
    real(8), parameter :: V_zoox = 5.0d-10   ! Volume of zooxanthellae (cm3/cells) ~300 um3 (Fujise et al. 2018)
                             ! ~500 um3 =5.e-10 cm3 (e.g. Jone 1997; Jones and Yellowlees 1997)
!----- Respitration constants ---------------------------
    real(8), parameter :: Rzmax  = 5.0d-6  ! Maximum respiration rate (pmol cell-1 s-1)
                                           ! ~0.1 (pmol cell-1 h-1) = 2.8d-5 (pmol cell-1 s-1) (Suggett et al., 2008)
    real(8), parameter :: K_QCzr = 50.0d0   ! (pmolC cell-1) (Asssumed)
    real(8), parameter :: K_DO   = 5.d0    ! ca. 10 (umolO2 kg-1) Shick et al. (1990) *Tuned value

    real(8), parameter :: p2r   = 0.5d0    ! molC respiration/(molC protain) required respiration rate for 1 molC biosynthesis
    real(8), parameter :: C_dec = 1.0d-6   ! (pmol cell-1 s-1) Decay rate 

!----- C translocation constants ---------------------------
    real(8), parameter :: QC0 =  50.0d0    ! (pmolC/cell)  Minimal cell quota  ! 66 pmol/cell: Tanaka et al 2018;  600-1400 pgC cell-1: Muller-Parker et al., 1994)
    real(8), parameter :: QCm = 120.0d0    ! (pmolC/cell)  Maximum cell quota  ! 66 pmol/cell: Tanaka et al 2018;  600-1400 pgC cell-1: Muller-Parker et al., 1994)
    real(8), parameter :: u_z2c = 1.0d-6   ! (s-1)  Cell-quota specific carbon translocation rate from zoox to host (Assumed)
    real(8), parameter :: u_c2z = 5.0d-7   ! (pmolC cell-1 (umol cm-2)-1 s-1)  Cell-quota specific carbon translocation rate from host to zoox (Assumed)

!----- Chl dynamics constants ---------------------------
    real(8), parameter :: QCzr_max  = 20.0d0  ! Target QCr value (pmol cell-1)  (Asssumed)

#  if defined CORAL_NUTRIENTS
    real(8), parameter :: QN0 = 1.1d0  ! (pmolN/cell)  calculated from C:N:P = 116:16:1
    real(8), parameter :: QP0 = 0.07d0 ! (pmolP/cell) 
#  endif

#  if defined CORAL_NUTRIENTS
!----- Nutrients related parameters -------------------

#  endif

!----- Population dynamics parameters -------------------

    real(8), parameter :: u_max  = 5.0d-5 ! Maximum reproduction rate (pmol cell-1 s-1)       !!! Tuning
    real(8), parameter :: rg2c   = 2.0d0  ! Conversion factor from growth respiration to reproduction (no dimension)
    real(8), parameter :: kC_sec = 2.0d-6 ! Mass transfer velocity of secreted organic matter (s-1) !!! Tuning

    real(8), parameter :: km_CNP = 1.d-8 ! (cell cm-2 s-1)  !!!!!!!!!!!!! TBD
    real(8), parameter :: am_CNP = 1.d0
    real(8), parameter :: km_ROS = 1.d-11 !                  !!!!!!!!!!!! TBD
    real(8), parameter :: km_min = 1.d-14   ! (cell cm-2 s-1)  !!!!!!!!!! TBD
    real(8), parameter :: km = 1.d-2   ! (cell cm-2 s-1)  !!!!!!!!!!!!!! TBD

! ----------------------------------------------------------
    real(8) :: F_zQC(N_Csp)
    real(8) :: F_zROS, F_zCO2(N_Csp), F_zDO
    real(8) :: F_zdens
    real(8) :: F_zPOC(N_Csp,Npom)
#  if defined CORAL_NUTRIENTS
    real(8) :: F_zQN(N_Nsp)
    real(8) :: F_zQP(N_Psp)
    real(8) :: F_zNO3(N_Nsp), F_zNH4(N_Nsp)
    real(8) :: F_zPO4(N_Psp)
    real(8) :: F_zPON(N_Nsp,Npom),F_zPOP(N_Psp,Npom)
!    real(8) :: F_ONOO
    real(8) :: rNC, rPC
#  endif
    real(8) :: rCO2(N_Csp)
    real(8) :: rQC(N_Csp),rQN(N_Nsp),rQP(N_Psp)

    real(8) :: QCzr    ! Reserve OC pool (= QC - QC0 ) (pmolC/cell) 
    real(8) :: Cg_bio  ! Gross biosyntesis rate (pmolC cell-1 s-1)
    real(8) :: Cn_bio  ! Net biosyntesis rate (pmolC cell-1 s-1)
    real(8) :: Rzm     ! Maximum respiration rate at the present condition (pmolC cell-1 s-1) 
    real(8) :: Rzrq    ! Required respiration rate for biosynthesis (pmolC cell-1 s-1) 
!    real(8) :: F_zSO   ! Flux of Superoxide (O2-) from photosystem (pmol cell-1 s-1)
    real(8) :: f_min    

#  if defined CORAL_NUTRIENTS
!----- Nutrients related parameters -------------------

#  endif
!----- Population dynamics parameters -------------------
    real(8) ::  Morta
    real(8) ::  Repro, Death

#  if defined CORAL_TESTMODE
!  Output
!    real(8), parameter :: CORAL_OUTPUT_INTERVAL  = 5.0d0    ! Output interval (min)
    real(8), save :: dsec = 0.d0 !sec
#  endif
    integer :: isp
    real(8) :: cff

!===============================================================

!---- Preparation --------------------
    F_zQC (:) = 0.0d0
    F_zCO2(:) = 0.0d0
    F_zDO  = 0.0d0
    F_zROS = 0.0d0
    F_zdens = F_Zelm
#  if defined CORAL_NUTRIENTS
    F_zQN (:) = 0.0d0
    F_zQP (:) = 0.0d0
    F_zNO3(:) = 0.0d0
    F_zNH4(:) = 0.0d0
    F_zPO4(:) = 0.0d0
!    F_ONOO = 0.0d0
    rNC = ZOOX(ng)%QN(iNt,n,i,j)/ZOOX(ng)%QC(iCt,n,i,j)
    rPC = ZOOX(ng)%QP(iPt,n,i,j)/ZOOX(ng)%QC(iCt,n,i,j)
#  endif
!----- Isotope ratio calculation ---------------------------
    DO isp=1,N_Csp
      rCO2(isp) = Iso_ratio( CO2coe(isp), CO2coe(iCt) )
      rQC (isp) = Iso_ratio( ZOOX(ng)%QC(isp,n,i,j), ZOOX(ng)%QC(iCt,n,i,j) )
    END DO
#  if defined CORAL_NUTRIENTS
    DO isp=1,N_Nsp
      rQN (isp) = Iso_ratio( ZOOX(ng)%QN(isp,n,i,j), ZOOX(ng)%QN(iNt,n,i,j) )
    END DO
    DO isp=1,N_Psp
      rQP (isp) = Iso_ratio( ZOOX(ng)%QP(isp,n,i,j), ZOOX(ng)%QP(iPt,n,i,j) )
    END DO
#  endif

!----- Reserve C pool (pmol cell-1) ---------------------------
    QCzr = max( ZOOX(ng)%QC(iCt,n,i,j) - QC0, 0.0d0)

!----- Zooxanthellae metabolism  ----------------------------------

    CALL photosystem         &
!        input parameters
      ( ng, n, i, j          & ! ng: nested grid number; n: coral compartment; i,j: position
      , date                 &   ! Date (days since x or elapsed days)
      , dt                   & ! Time step (sec)
      , PFDsurf              & ! Photon flux density (umol m-2 s-1)
      , Tamb                 & ! Temperature (oC)
      , CO2coe(iCt)          & ! CO2 concentration in the coral tissue
      , DOcoe                & ! DO concentration in the coral tissue
      , ROS                  & ! ROS concentration in Zooxanthellar cell
      , QCzr                 & ! Reserve OC pool (= QC - QC0 ) (pmolC/cell)
!        input and output parameters
      , ZOOX(ng)%QAo(n,i,j)    & ! 
      , ZOOX(ng)%QAr(n,i,j)    & ! 
      , ZOOX(ng)%QAi(n,i,j)    & ! 
      , ZOOX(ng)%Chl(n,i,j)    & ! 
      , ZOOX(ng)%SOx(n,i,j)    & ! Superoxide (O2-) concentration in Zooxanthellar cell
      , ZOOX(ng)%NADPHn(n,i,j) & ! normalized NADPH (no dim.) NADPH/(NADPH + NADP+) 
      , ZOOX(ng)%RuBPn(n,i,j)  & ! normalized RuBP  (no dim.) RuBP/(0.5*PGA + RuBP) 
!        output parameters
      , ZOOX(ng)%Pg(n,i,j)   & ! Gross photosynthetic rate (pmolC cell-1 s-1)
      , F_zDO                & ! Flux of DO  (pmol cell-1 s-1)
      , F_zROS               & ! Flux of Superoxide (O2-) from photosystem (pmol cell-1 s-1)
      )
      
!----- CO2 fixation rate (pmol cell-1 s-1) ---------------------------

    F_zCO2(:) = F_zCO2(:) - ZOOX(ng)%Pg(n,i,j) *aC_phot(1:N_Csp)*rCO2(:)

!----- peroxynitrite (ONOO-) flux (nmol cm-2 s-1)------------------------------------
!
!    F_ONOO=1.e0 * ZOOX(ng)%NO2(n,i,j)*CORAL(ng)%ROS(n,i,j)   ! (pmol cell-1 s-1)!!!!!!!!!!!!!!!!!!�Ă��Ƃ�
!    F_ONOO=F_ONOO*ZOOX(ng)%dens(n,i,j)*1.d-3   ! (nmol cm-2 s-1)


!----- Respilation rate (pmol cell-1 s-1) ---------------------------
    f_min = min( QCzr/(K_QCzr+QCzr), DOcoe/(K_DO+DOcoe) )
    Rzm = Rzmax * f_min
    Rzrq = C_dec * p2r ! Gross biosyntesis rate (pmolC cell-1 s-1)

!    Cn_bio = Cg_bio - C_dec          ! Net biosyntesis rate (pmolC cell-1 s-1)
!    if (ZOOX(ng)%QC(n,i,j) >= QCm) then
    Cn_bio = (Rzm - Rzrq)/p2r
    ZOOX(ng)%R(n,i,j) = Rzm
!    else
!       Cn_bio = 0.0d0
!       ZOOX(ng)%R(n,i,j) = min(Rzm, Rzrq)
!    endif

    ! Summing fluxes
    F_zDO     = F_zDO     - ZOOX(ng)%R(n,i,j)
    F_zCO2(:) = F_zCO2(:) - ZOOX(ng)%R(n,i,j) *rQC(:)
#  if defined CORAL_NUTRIENTS
    F_zNH4(:) = F_zNH4(:) - ZOOX(ng)%R(n,i,j)*rNC *rQN(:)
    F_zPO4(:) = F_zPO4(:) - ZOOX(ng)%R(n,i,j)*rPC *rQP(:)
#  endif

!----- interaction between zooxanthellae and coral -----------------
!    F_zQC(:) = F_zQC(:) + u_z2c*QCzr *rQC(:)      & !*(1.0d0-QC0/ZOOX(ng)%QC(n,i,j))    & !Simbiont secretion
!                        - u_c2z*QCcr        ! receive from the host
    F_zQC(:) = F_zQC(:) + u_z2c*QCzr *rQC(:) !Simbiont secretion
#  if defined CORAL_NUTRIENTS
    F_zQN(:) = F_zQN(:) + u_z2c*QCzr*rNC *rQN(:)
    F_zQP(:) = F_zQP(:) + u_z2c*QCzr*rPC *rQP(:)
#  endif

!TODO 褐虫藻の増殖過程の再検討
!----- Convert cell conditions to population dynamics parameters

    Repro = max(Cn_bio, 0.0d0)/ZOOX(ng)%QC(iCt,n,i,j)  ! Cell specific growth rate (s-1)

!  ----- Growth rate (cell cm-2 s-1) -----------------
    F_zdens = F_zdens + Repro * ZOOX(ng)%dens(n,i,j)

!  ----- Cell specific mortality rate (s-1) -----------------

#  if defined CORAL_NUTRIENTS
!
#  else
!    Morta = 0.0d0!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    Morta = km * max( -Cn_bio, 0.0d0 )
    Morta = 1.0d-8
#  endif
!  ----- Mortality rate (cell cm-2 s-1) -----------------
    F_zdens = F_zdens - Morta*ZOOX(ng)%dens(n,i,j)

    F_POC(:,iLPOM) = F_POC(:,iLPOM) + Morta*ZOOX(ng)%dens(n,i,j)*ZOOX(ng)%QC(iCt,n,i,j)*1.0d-3 *rQC(:)
#  if defined CORAL_NUTRIENTS
    F_PON(:,iLPOM) = F_PON(:,iLPOM) + Morta*ZOOX(ng)%dens(n,i,j)*ZOOX(ng)%QN(iNt,n,i,j)*1.0d-3 *rQN(:)
    F_POP(:,iLPOM) = F_POP(:,iLPOM) + Morta*ZOOX(ng)%dens(n,i,j)*ZOOX(ng)%QP(iPt,n,i,j)*1.0d-3 *rQP(:)
#  endif



! ==== Set output parameters ========================================================
!----- Convert (pmol cell-1 s-1) to (nmol cm-2 s-1)------------------------------------
!  ----- Zoox. total photosynthetic rate (nmolC cm-2 s-1) ---------------------------
    Pg = ZOOX(ng)%Pg(n,i,j) * ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1) 

!  ----- Zoox. total respilation rate (nmolC cm-2 s-1) ---------------------------
    Rz = ZOOX(ng)%R (n,i,j) * ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1) 

!  ----- Mass fluxes (nmol cm-2 s-1) ---------------------------
    F_DO  = F_DO  + F_zDO  * ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
    F_ROS = F_ROS + F_zROS * ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)

    F_QC (:) = F_QC (:) + F_zQC (:) * ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
    F_DIC(:) = F_DIC(:) + F_zCO2(:) * ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
#  if defined CORAL_NONE_CO2_EQ
    F_CO2(:) = F_CO2(:) + F_zCO2(:) * ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
#  endif
   
#  if defined CORAL_NUTRIENTS

    F_QN (:) = F_QN (:) + F_zQN  * ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
    F_NO3(:) = F_NO3(:) + F_zNO3 * ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
    F_NH4(:) = F_NH4(:) + F_zNH4 * ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)

    F_QP (:) = F_QP (:) + F_zQP  * ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
    F_PO4(:) = F_PO4(:) + F_zPO4 * ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
!      F_Nwaste = F_Zmort*ZOOX(ng)%QN(n,i,j)*1.0d-3   !=(cell cm-2 s-1)*(pmol cell-1)*1.e-3=(nmol cm-2 s-1)
!      F_Pwaste = F_Zmort*ZOOX(ng)%QP(n,i,j)*1.0d-3   !=(cell cm-2 s-1)*(pmol cell-1)*1.e-3=(nmol cm-2 s-1)

#  endif

!==== Time step progressing =================================================
!-----Cell quota (pmol/cell) -----------------!!!!!
    ZOOX(ng)%QC (:,n,i,j)=ZOOX(ng)%QC (:,n,i,j) + F_zQC (:) *dt

#  if defined CORAL_NUTRIENTS
!    Nutrients (pmol/cell)
    ZOOX(ng)%NO3(:,n,i,j)=ZOOX(ng)%NO3(:,n,i,j) + F_zNO3(:) *dt
    ZOOX(ng)%NH4(:,n,i,j)=ZOOX(ng)%NH4(:,n,i,j) + F_zNH4(:) *dt
    ZOOX(ng)%PO4(:,n,i,j)=ZOOX(ng)%PO4(:,n,i,j) + F_zPO4(:) *dt

    ZOOX(ng)%QN (:,n,i,j)=ZOOX(ng)%QN (:,n,i,j) + F_zQN (:) *dt
    ZOOX(ng)%QP (:,n,i,j)=ZOOX(ng)%QP (:,n,i,j) + F_zQP (:) *dt
#  endif

!----- Zooxanthellae density (cell cm-2) -----------------
!    ZOOX(ng)%dens ~ 1.e6 cells cm-2  (e.g. Higuchi et al., 2012)

    ZOOX(ng)%dens(n,i,j)=ZOOX(ng)%dens(n,i,j) + F_zdens *dt

!==============================================================================
! Print section (for debug)

#  if defined CORAL_TESTMODE

!    IF(mod(int(date*86400.0d0*100.0d0), int(CORAL_OUTPUT_INTERVAL*60.0d0*100.0d0))==0) THEN
    IF( date*86400.0d0 >= dsec ) THEN
      if(dsec == 0.0d0) then
        dsec=date*86400.0d0+CORAL_OUTPUT_INTERVAL*60.0d0
      else
        dsec=dsec+CORAL_OUTPUT_INTERVAL*60.0d0
      endif

      write(30+n,'(*(g0:,", "))')                                        &
         date, PFDsurf                                                   &
       , ZOOX(ng)%Pg(n,i,j), ZOOX(ng)%R(n,i,j)                           &
       , ZOOX(ng)%Pg(n,i,j)-ZOOX(ng)%R(n,i,j), ZOOX(ng)%dens(n,i,j)      &
       , ZOOX(ng)%QC(iCt,n,i,j), ZOOX(ng)%Chl(n,i,j)                     &
       , Cg_bio, Cn_bio, Repro, Morta, F_zROS,F_zQC(iCt)                 &
       , F_Zelm, Repro*ZOOX(ng)%dens(n,i,j), Morta*ZOOX(ng)%dens(n,i,j)  &
#   if defined CORAL_NUTRIENTS
       , ZOOX(ng)%QN(iNt,n,i,j), ZOOX(ng)%QP(iPt,n,i,j)                  &
       , N_repro, P_repro                                                &
       , ZOOX(ng)%NO3(iNt,n,i,j), ZOOX(ng)%NH4(iNt,n,i,j), F_ONOO        &
       , NO3_trans, NH4_trans,NO3_reduc, N_assim, N_dissim               &
       , ZOOX(ng)%PO4(iPt,n,i,j), PO4_trans, P_assim                     &
#   endif
       , CORAL(ng)%ROS(n,i,j), CORAL(ng)%ROS(n,i,j)*V_zoox*1.0d3         &
       , ZOOX(ng)%dens(n,i,j)*ZOOX(ng)%Chl(n,i,j)*1.0d-6
    
    END IF
#  endif
!---------------------------------------------------------------------

    RETURN

  END SUBROUTINE zooxanthellae

!!! **********************************************************************
!!!  Photoinihibition model (modified from Gustafsson et al. 2014)
!!! **********************************************************************

  SUBROUTINE photosystem  &
!   input parameters
    ( ng, n, i, j     & ! ng: nested grid number; n: coral compartment; i,j: position
    , date            &   ! Date (days since x or elapsed days)
    , dt              & ! Time step (sec)
    , PFDsurf         & ! Photon flux density (umol m-2 s-1)
    , Tamb            & ! Temperature (oC)
    , CO2             & ! CO2 concentration in the coral tissue
    , DO              & ! DO concentration in the coral tissue
    , ROS             & ! ROS concentration in the coral tissue
    , QCzr            & ! Reserve OC pool (= QC - QC0 ) (pmolC/cell)
!   input and output parameters
    , QAo             & ! 
    , QAr             & ! 
    , QAi             & ! 
    , Chl             & ! Chlorophyll concentration (pg cell-1)
    , SOx             & ! Superoxide (O2-) concentration in Zooxanthellar cell
    , NADPHn          & ! normalized NADPH (no dim.) NADPH/(NADPH + NADP+) 
    , RuBPn           & ! normalized RuBP  (no dim.) RuBP/(0.5*PGA + RuBP) 
!!   output parameters
    , Pg              & ! Gross photosynthetic rate (pmolC cell-1 s-1)
    , F_zDO           & ! Flux of DO  (pmolO2 cell-1 s-1)
    , F_zROS          & ! Flux of ROS from photosystem (pmol cell-1 s-1)
    )
!-----------------------------------------------------------------------

    implicit none
!   input parameters
    integer, intent(in) :: ng, n, i, j
    real(8), intent(in) :: date
    real(8), intent(in) :: dt
    real(8), intent(in) :: PFDsurf
    real(8), intent(in) :: Tamb
    real(8), intent(in) :: CO2, DO
    real(8), intent(in) :: ROS
    real(8), intent(in) :: QCzr
!   input and output parameters
    real(8), intent(inout) :: QAo
    real(8), intent(inout) :: QAr
    real(8), intent(inout) :: QAi
    real(8), intent(inout) :: Chl
    real(8), intent(inout) :: SOx
    real(8), intent(inout) :: NADPHn
    real(8), intent(inout) :: RuBPn
!   output parameters
    real(8), intent(out) :: Pg
    real(8), intent(out) :: F_zDO, F_zROS

!------------Set Constants  ----------------------------------
    real(8), parameter :: PI  = 3.14159265359d0
    real(8), parameter :: Rg  = 8.31d0    ! Gass constant (J K–1 mol–1)

    real(8), parameter :: s_0 = 2.3d0*1.2d0    ! Absorption cross-section (m2 (umol photon)-1); ~390 (ANGSTROM quanta-1) Suggett et al. (2008) * 6.02x10^23 quanta = 1 mol photon
    real(8) :: s_ab    ! Absorption cross-section (m2 (umol photon)-1); ~390 (ANGSTROM quanta-1) Suggett et al. (2008) * 6.02x10^23 quanta = 1 mol photon
    real(8), parameter :: phi_q = 1.0d0     ! quantum efficiency of charge separation at PSII(e- photon-1)  Ross et al. (2008) !!!!!!!!!!!!!check
!    real(8), parameter :: Trb = 33.8d0    ! (oC) estimated from Lilley et al. (2010)
!    real(8), parameter :: arb = 0.55d0    ! (K-1) estimated from Lilley et al. (2010)
    real(8), parameter :: e2o = 0.25d0    ! Yield of O2 per electron (mol O2 (mol e-)-1) 2H2O -> 4e- +4H+ + O2 
    real(8), parameter :: o2c = 1.0d0     ! Photosynthetic quotient (mol O2 (mol CO2)-1) !!!Assumed
    real(8), parameter :: e2c = e2o*o2c   ! (mol C (mol e-)-1)

    !   ---- RubisCO activity related parameters -----------------
    real(8), parameter :: Vcmax = 0.5d-3   ! maximum carboxylation rate (pmol cell-1 s-1) !!! Tuning
    real(8), parameter :: Vomax = 0.2d0*Vcmax   ! maximum oxygenation rate (pmol cell-1 s-1) 
    real(8), parameter :: Kco2  = 20.0d0  ! Half saturation constant (uM) !!! Tuning (Ref. von Caemmerer, 2000)
    real(8), parameter :: Ko2   = 300.0d0 ! Half saturation constant (uM) !!! Tuning (Ref. von Caemmerer, 2000)
    real(8), parameter :: Krupb = 0.2d0   ! Half saturation constant (no dim) !!! Tuning (Ref. von Caemmerer, 2000)
    real(8), parameter :: kphot = 1.0d-3   ! rate constant of photosynthesis **** Unknown *********** 

!    real(8), parameter :: A  = 3.0d11     ! !!! Tuning
    real(8), parameter :: Ea = 1.4d5      ! !!! Tuning
    real(8), parameter :: dS = 1.2d3     ! 
    real(8), parameter :: dH = 3.58d5     ! 

!    real(8), parameter :: f_RO = 1.0d0    ! ROS fraction from zoox. to host. !!!Assumed

    real(8), parameter :: ko = 1.04d3     ! Relaxation rate of QAr to QAo (s-1) : tau = 960 us; Suggett et al. (2008)
    real(8), parameter :: ki = 7.0d-7    ! Rate constant of photoinhibition (s-1 (umol m-2 s-1)-1)
                                          !      Tyystjarvi & Aro (1996): 6.11d-8 (s-1 (umol m-2 s-1)-1);
                                          !      Nishiyama et al. (2004): 1.7d-7  (s-1 (umol m-2 s-1)-1)
!    real(8), parameter :: di = 1.7d-4     ! Natural decay from QAa to QAi (s-1)  !!!Assumed
    real(8), parameter :: ka = 3.0d-4     ! Repair rate of QAi to QAa (s-1) estimated from Takahashi et al. (2009) !!! TBD
!    real(8), parameter :: ka = 5.0d-11     ! Repair rate of QAi to QAa (s-1) estimated from Takahashi et al. (2009) !!! TBD
    real(8), parameter :: kros = 0.2d0   ! 
    real(8), parameter :: kros2 = 1.0d0   !

    real(8), parameter :: Vsoxmax = 1.0d-5   ! maximum superoxide detox rate (pmol cell-1 s-1)  !!! Tuning !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real(8), parameter :: Ksox = 1.0d0      ! Half saturation constant (pmo; cell-1) !!! Tuning !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    real(8), parameter :: kqap = 1.0d-11    ! rate constant for QAa production
    real(8), parameter :: QCzrm = 50.0d0    ! Threashold reserv QC value for Chl production 
    real(8), parameter :: uchl_max = 1.0d-4 ! Maximum Chl production rate (pg cell-1 s-1)  (Asssumed)
    real(8), parameter :: QAt = 1.2d-6      ! Total RCII (pmol RCII cell-1)
!---------------------------------------------------------------------
!    real(8) :: QAt       ! Total RCII (pmol RCII cell-1)
    real(8) :: QAa       ! Active RCII (pmol RCII cell-1)
    real(8) :: RubisCO   ! Active fraction of RubisCO
    real(8) :: Vc, Vo, f_temp, TK   
    real(8) :: Gamma
    real(8) :: J_ea      ! Electron absorption rate (pmol e- cell-1 s-1)
    real(8) :: J_ep      ! Rate of electrons leading to C fixation (pmol e- cell-1 s-1)
    real(8) :: J_ee      ! electrons diverted from C fixation (pmol e- cell-1 s-1)
    real(8) :: J_ep_max  ! maximum electron transport rate (pmol e- cell-1 s-1)
    real(8) :: J_ep_in   ! Input electron transport rate (pmol e- cell-1 s-1)
    real(8) :: J_RO      ! Rate of ROS formed by e- (pmol ROS cell-1 s-1)
    real(8) :: J_ROs     ! Rate of slow ROS formed by e- (pmol ROS cell-1 s-1)
    real(8) :: J_ROf     ! Rate of fast ROS formed by e- (pmol ROS cell-1 s-1)
    real(8) :: Jo2r, Jr2o, Jo2i, Jr2i, Ji2o, Ja2i, Ji2a
    real(8) :: kr       
    real(8) :: I2e       
    real(8) :: K_QAi
    real(8) :: F_QAd, F_QAp
    real(8) :: dChl_dt   ! Chl concentration (pg/cell/s)
    real(8) :: F_zSOx   ! Flux of Superoxide (O2-) from photosystem (pmol cell-1 s-1)
    real(8) :: F_NADPH  ! Flux of normalized NADPH (s-1)
    real(8) :: F_RuBP   ! Flux of normalized RuBP (s-1)
    real(8) :: phi      ! Vo/Vc
    
#  if defined CORAL_TESTMODE
!  Output
!    real(8), parameter :: CORAL_OUTPUT_INTERVAL  = 5.0d0    ! Output interval (min)
    real(8), save :: dsec = 0.d0 !sec
    real(8) :: FvFm       
    real(8) :: YII
    real(8) :: KF       
      
#  endif

!===== Photosynthetic rate calculation ==============================================
    
!---- electrons leading to C fixation (electron transfer rate (ETR)) (pmol e- cell-1 s-1) ---
    J_ep_in = ko * QAr

!---- Photosyenthetic rate (von Caemmerer, 2000; Eq.(2.34))
    TK = Tamb + 273.15 !(K)
    f_temp = exp(-(TK-298.15)*Ea/298.15/Rg/TK)* &
              (1.0d0+exp((298.15*dS-dH)/Rg/298.15))/(1.0d0+exp((TK*dS-dH)/Rg/TK))

    Vc = Vcmax * f_temp * CO2/(CO2+Kco2*(1.0d0+DO/Ko2))  &
                        * RuBPn/(RuBPn+Krupb)
!    Vo = Vomax*f_temp*DO/(DO+Ko2*(1.0d0+CO2/Kco2))
    phi = Vomax/Vcmax*(DO/Ko2)/(CO2/Kco2)
    Vo = phi*Vc

    Pg = Vc - 0.5d0*Vo
     
!---- Oxygen production rate  (pmol O2 cell-1 s-1) ---
    F_zDO = J_ep_in/4.0d0 - 0.5d0*Vo

!---- Electrons that form ROS  (pmol ROS cell-1 s-1) ---
!   NADP+ + H+ + 2 e- -> NADPH
    F_NADPH = 0.5d0*J_ep_in * (1.0d0-NADPHn) 

    F_RuBP = kphot*NADPHn*(1.0d0-RuBPn)

!   O2 + e- -> O2- 
    F_zSOx = NADPHn * J_ep_in

!   Superoxide desimutaze (SOD)     
!    2 O2- + 2 H+ -> H2O2 + O2
    F_zROS = Vsoxmax * SOx/(Ksox + SOx)

!   DO flux (pmol O2 cell-1 s-1) 
    F_zDO = F_zDO - F_zSOx + F_zROS

!===== Photoinhibition calculation ==============================================

!----- Active RCII -----------------
!    QAa = QAt - ZOOX(ng)%QAi(n,i,j)
    QAa  = QAo + QAr

!---- Photoinihibition rate of QAa to QAi  (pmol RCII cell-1 s-1) ---
    Ja2i = ki*PFDsurf * QAa
!    Ja2i = 0.0d0  ! No photidamage case

!---- Repair rate of QAi to QAo  (pmol RCII cell-1 s-1) --- !!!!!!!!!!!!!!!!! Need update
!    Ji2a = ka * QAi * exp(-kros2*ROS)  !!! ROS concentration dependent repair
!    Ji2a = ka * QAi * exp(-kros2*ROS) * (J_ep_in*1.0d4)  !!! ROS concentration dependent repair
    Ji2a = ka * QAi * exp(-kros2*ROS) * tanh(PFDsurf/50.0d0)  !!! ROS concentration dependent repair

! ----- QAa production rate (pmol RCII cell-1 s-1) -------
    F_QAp = kqap * (1.0d0-QCzr/QCzrm)

!===== Time step progressing ================================================

!-----Inhibited RCII -----------------
    QAi = QAi +( Ja2i - Ji2a )*dt

!-----Active RCII (updated) -----------------
    QAa = QAt - QAi

!---- electron absorption rate (s-1) 
!   = reduction of active QA (s-1) ---
!    s_ab = s_0*0.83d0*exp(-kros*ROS)  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    s_ab = s_0*0.83d0
!    s_ab = s_0*0.65d0
    s_ab = s_0 * 0.80d0*exp(-kros*QAi/QAt)

    kr = s_ab * Chl * phi_q * PFDsurf

!-----Reduced RCII -----------------
    QAr = kr/(kr+ko) * QAa

!-----Oxidizes RCII -----------------
    QAo = QAa - QAr

!===== Chl dynamics calculation ==============================================
!    dChl_dt = uchl_max * (1.0d0 - QCzr/QCzr_max)
!    dChl_dt = - 3.0d-6*ROS + 3.0d-5
    dChl_dt = 1.0d-6*(3.0d0-ROS)
!-----Chl concentration (pg cell-1) -----------------
    Chl = Chl + dChl_dt *dt
    Chl = max(Chl,0.0d0) !!!Error handring

!-- NADPHn = NADPH/(NADPH + NADP+) concentration (no unit, (pmol cell-1)/(pmol cell-1) ) -----------------
!   Eq. (23) in Farquhar et al. (1980)
!    NADPHn = 1.0d0 - (4.0d0+3.0d0*phi)*Vc/J_ep_in
    NADPHn = NADPHn + (F_NADPH - F_RuBP) *dt

!-- RuBPn = RuBP/(0.5*PGA + RuBP) concentration (no unit, (pmol cell-1)/(pmol cell-1) ) -----------------
!   Eq. (27) in Farquhar et al. (1980)
!    RuBPn = 1.0d0 - (2.0d0+1.5d0*phi)*Vc/NADPHn/kphot
!    RuBPn = RuBPn + ( F_RuBP - (2.0d0+1.5d0*phi)*Vc ) *dt
    RuBPn = RuBPn + ( F_RuBP - (2.0d0+1.5d0*phi)*Vc *100.0d0 ) *dt

!-----Superoxide concentration (pmol cell-1) -----------------
!   F_zSOx: O2 + e- -> O2- 
!   F_zROS: 2 O2- + 2 H+ -> H2O2 + O2
!    
!     d SOx/dt = F_zSOx - 2*F_zROS = 0 (quasi steady state)
!   O2 + e- -> O2- 
!      F_zSOx = NADPHn * J_ep_in  
!   2 O2- + 2 H+ -> H2O2 + O2 (Superoxide desimutaze (SOD)) 
!      F_zROS = Vsoxmax * SOx/(Ksox + SOx)
!
!    SOx = NADPHn*J_ep_in*Ksox/(2.0d0*Vsoxmax - NADPHn*J_ep_in)
    SOx = SOx + ( F_zSOx - 2.0d0*F_zROS ) *dt

!------------------------------------------------------------------------
! Print section (for debug)

#  if defined CORAL_TESTMODE

    IF( date*86400.0d0 >= dsec ) THEN
      if(dsec == 0.0d0) then
        dsec=date*86400.0d0+CORAL_OUTPUT_INTERVAL*60.0d0
      else
        dsec=dsec+CORAL_OUTPUT_INTERVAL*60.0d0
      endif

      KF = s_0/s_ab - 1.0d0
      FvFm = (QAa/QAt)/(KF+(QAa/QAt))
      YII  = (QAo/QAt)/(KF+(QAo/QAt))
      
      write(40+n,'(*(g0:,", "))')                                   &
         date, PFDsurf, Tamb                                        &
       , QAo, QAr, QAi                                              &
       , QAt, QAa, FvFm, YII, J_ep_in                               &
       , Ji2a, Ja2i, kr, s_ab, F_zSOx, SOx                          &
       , Pg, J_ep_in*e2c, Vc, Vo, phi, F_zDO                       &
       , Chl, CO2, DO, F_QAp, NADPHn, RuBPn, F_zROS, ROS
      
    END IF
#  endif
!-----------------------------------------------------------------------
    RETURN

  END SUBROUTINE photosystem
# endif

#endif      
END MODULE mod_coral

