
!!!=== Copyright (c) 2012-2020 Takashi NAKAMURA  =====

#include "cppdefs.h"

!!!**** MODULE OF CORAL POLYP MODEL ************************************

MODULE mod_coral

  implicit none

  integer, parameter :: Ncl= 2    !! Number of coral types
!    integer, parameter :: Nzx= 1    !! Number of zooxanthellae clade types

  TYPE T_CORAL

    real(8), pointer :: Pg(:,:,:) 
    real(8), pointer :: R (:,:,:) 
    real(8), pointer :: G (:,:,:) 
    real(8), pointer :: TAcal(:,:,:) 
    real(8), pointer :: TAcoe(:,:,:) 
    real(8), pointer :: DICcal(:,:,:)
    real(8), pointer :: DICcoe(:,:,:)
    real(8), pointer :: DOcoe(:,:,:) 
    real(8), pointer :: QC(:,:,:)
    real(8), pointer :: cCO2aqcal(:,:,:)
    real(8), pointer :: cCO2aqcoe(:,:,:)
#if defined CORAL_CARBON_ISOTOPE
!  13C isotope
    real(8), pointer :: DI13Ccal(:,:,:)
    real(8), pointer :: DI13Ccoe(:,:,:)
    real(8), pointer :: Q13C(:,:,:)
    real(8), pointer :: c13CO2aqcal(:,:,:)
    real(8), pointer :: c13CO2aqcoe(:,:,:)
#endif
#if defined CORAL_NUTRIENTS
!  Nutrients dynamics
    real(8), pointer :: NO3(:,:,:)
!    real(8), pointer :: NO2(:,:,:)
    real(8), pointer :: NH4(:,:,:)
    real(8), pointer :: PO4(:,:,:)
    real(8), pointer :: QN(:,:,:)
    real(8), pointer :: QP(:,:,:)
#endif
#if defined CORAL_ZOOXANTHELLAE
    real(8), pointer :: ROS(:,:,:)
# endif
#if defined CORAL_SIZE_DYNAMICS
    real(8), pointer :: growth(:,:,:)  ! growth rate
    real(8), pointer :: mort(:,:,:)  ! mortaliny rate
!    real(8), pointer :: dsed_coral(:,:,:)
#endif
  END TYPE T_CORAL

!-----------------------------------------------------------------------
#if defined CORAL_PHOTOINHIBITION
!  Photoinihibition model

  TYPE T_PHOT
    real(8), pointer :: QAo(:,:,:)
    real(8), pointer :: QAr(:,:,:)
    real(8), pointer :: QAi(:,:,:)
    real(8), pointer :: Rep(:,:,:)
  END TYPE T_PHOT
#endif

!-----------------------------------------------------------------------
#if defined CORAL_ZOOXANTHELLAE
!  zooxanthellae dynamics

  TYPE T_ZOOX

    real(8), pointer :: Pg(:,:,:) 
    real(8), pointer :: R (:,:,:) 
    real(8), pointer :: dens(:,:,:)
    real(8), pointer :: QC(:,:,:)
!    real(8), pointer :: ROS(:,:,:)
# if defined CORAL_CARBON_ISOTOPE
!  13C isotope
    real(8), pointer :: Q13C(:,:,:)
# endif
# if defined CORAL_NUTRIENTS
    real(8), pointer :: QN(:,:,:)
    real(8), pointer :: QP(:,:,:)
    real(8), pointer :: NO3(:,:,:)
!    real(8), pointer :: NO2(:,:,:)
    real(8), pointer :: NH4(:,:,:)
    real(8), pointer :: PO4(:,:,:)
# endif
  END TYPE T_ZOOX
#endif

  TYPE (T_CORAL), allocatable :: CORAL(:)
#if defined CORAL_ZOOXANTHELLAE
  TYPE (T_ZOOX), allocatable :: ZOOX(:)
# if defined CORAL_PHOTOINHIBITION
  TYPE (T_PHOT), allocatable :: ZPHOT(:)
# endif
#endif


CONTAINS

!!! **********************************************************************
!!!  set initial conditions for coral polyp model (Nakamura et al. 2013)
!!! **********************************************************************

  SUBROUTINE initialize_coral(ng, Ngrids, LBi, UBi, LBj, UBj)

    USE mod_geochem
    
    implicit none
! input parameters
    integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj
    real(8)  R13C
    integer i,j,n

    IF (ng.eq.1) allocate ( CORAL(Ngrids) )

    allocate( CORAL(ng)%Pg(Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%R (Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%G (Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%TAcal(Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%TAcoe(Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%DICcal(Ncl,LBi:UBi,LBj:UBj)    )
    allocate( CORAL(ng)%DICcoe(Ncl,LBi:UBi,LBj:UBj)    )
    allocate( CORAL(ng)%DOcoe(Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%QC(Ncl,LBi:UBi,LBj:UBj)        )
    allocate( CORAL(ng)%cCO2aqcal(Ncl,LBi:UBi,LBj:UBj) )
    allocate( CORAL(ng)%cCO2aqcoe(Ncl,LBi:UBi,LBj:UBj) )
#if defined CORAL_CARBON_ISOTOPE
    allocate( CORAL(ng)%DI13Ccal(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( CORAL(ng)%DI13Ccoe(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( CORAL(ng)%Q13C (Ncl,LBi:UBi,LBj:UBj)     )
    allocate( CORAL(ng)%c13CO2aqcal(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( CORAL(ng)%c13CO2aqcoe(Ncl,LBi:UBi,LBj:UBj)  )
#endif
#if defined CORAL_NUTRIENTS
    allocate( CORAL(ng)%NO3(Ncl,LBi:UBi,LBj:UBj)  )
!    allocate( CORAL(ng)%NO2(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( CORAL(ng)%NH4(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( CORAL(ng)%PO4(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( CORAL(ng)%QN(Ncl,LBi:UBi,LBj:UBj)   )
    allocate( CORAL(ng)%QP(Ncl,LBi:UBi,LBj:UBj)   )
#endif     
#if defined CORAL_ZOOXANTHELLAE
    allocate( CORAL(ng)%ROS(Ncl,LBi:UBi,LBj:UBj)  )
#endif
#if defined CORAL_SIZE_DYNAMICS
    allocate( CORAL(ng)%growth(Ncl,LBi:UBi,LBj:UBj)   )
    allocate( CORAL(ng)%mort(Ncl,LBi:UBi,LBj:UBj)   )
# endif

#if defined CORAL_ZOOXANTHELLAE
    IF (ng.eq.1) allocate ( ZOOX(Ngrids) )
    allocate( ZOOX(ng)%Pg(Ncl,LBi:UBi,LBj:UBj)     )
    allocate( ZOOX(ng)%R (Ncl,LBi:UBi,LBj:UBj)     )

    allocate( ZOOX(ng)%dens(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZOOX(ng)%QC(Ncl,LBi:UBi,LBj:UBj)    )
!    allocate( ZOOX(ng)%ROS(Ncl,LBi:UBi,LBj:UBj)   )
# if defined CORAL_CARBON_ISOTOPE
    allocate( ZOOX(ng)%Q13C(Ncl,LBi:UBi,LBj:UBj)  )
# endif
# if defined CORAL_NUTRIENTS
    allocate( ZOOX(ng)%NO3(Ncl,LBi:UBi,LBj:UBj)  )
!    allocate( ZOOX(ng)%NO2(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZOOX(ng)%NH4(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZOOX(ng)%PO4(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZOOX(ng)%QN(Ncl,LBi:UBi,LBj:UBj)   )
    allocate( ZOOX(ng)%QP(Ncl,LBi:UBi,LBj:UBj)   )
# endif
# if defined CORAL_PHOTOINHIBITION
    IF (ng.eq.1) allocate ( ZPHOT(Ngrids) )
    allocate( ZPHOT(ng)%QAo(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZPHOT(ng)%QAr(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZPHOT(ng)%QAi(Ncl,LBi:UBi,LBj:UBj)  )
    allocate( ZPHOT(ng)%Rep(Ncl,LBi:UBi,LBj:UBj)  )
# endif
#endif

!  Set initial conditions
    do j=LBj,UBj
      do i=LBi,UBi
        do n=1,Ncl

!      coral internal conditions
          CORAL(ng)%Pg(n,i,j)=0.d0
          CORAL(ng)%R (n,i,j)=0.d0
          CORAL(ng)%G (n,i,j)=0.d0

          CORAL(ng)%TAcal (n,i,j)=2500.d0
          CORAL(ng)%TAcoe (n,i,j)=2200.d0
          CORAL(ng)%DICcal (n,i,j)=2100.d0
          CORAL(ng)%DICcoe (n,i,j)=2100.d0
          CORAL(ng)%DOcoe (n,i,j)=10.d0 !(umol/L)
!          CORAL(ng)%QC (n,i,j)=15.d0 !450 ug/cm2 = 15 umol cm-2 -> 5.e6 umol kg-1 (�o�������ϔZ�x�Ƃ����j
          CORAL(ng)%QC (n,i,j)=158.d0 ! Tanaka et al. 2009

#if defined CORAL_NONE_CO2_EQ
          CORAL(ng)%cCO2aqcal(n,i,j)= 9.d0
          CORAL(ng)%cCO2aqcoe(n,i,j)=33.d0
#endif

#if defined CORAL_CARBON_ISOTOPE
# if defined CARBON_TRACE
          CORAL(ng)%Q13C (n,i,j) = 0.0d0
          CORAL(ng)%DI13Ccoe (n,i,j) = 0.0d0
          CORAL(ng)%DI13Ccal (n,i,j) = 0.0d0
#  if defined CORAL_NONE_CO2_EQ
          CORAL(ng)%c13CO2aqcal(n,i,j) = 0.0d0
          CORAL(ng)%c13CO2aqcoe(n,i,j) = 0.0d0
#  endif
# else
          R13C=R13C_fromd13C(-17.d0)
          CORAL(ng)%Q13C (n,i,j)=R13C*CORAL(ng)%QC(n,i,j)
          R13C=R13C_fromd13C(0.d0)
          CORAL(ng)%DI13Ccoe (n,i,j)=R13C*CORAL(ng)%DICcoe (n,i,j)
          R13C=R13C_fromd13C(0.d0)
          CORAL(ng)%DI13Ccal (n,i,j)=R13C*CORAL(ng)%DICcal (n,i,j)

#  if defined CORAL_NONE_CO2_EQ
          R13C=R13C_fromd13C(-8.d0)
          CORAL(ng)%c13CO2aqcal(n,i,j)=R13C*CORAL(ng)%cCO2aqcal(n,i,j)

          R13C=R13C_fromd13C(-8.d0)
          CORAL(ng)%c13CO2aqcoe(n,i,j)=R13C*CORAL(ng)%cCO2aqcoe(n,i,j)
#  endif
# endif
#endif
#if defined CORAL_ZOOXANTHELLAE
          CORAL(ng)%ROS(n,i,j) = 0.0d0
#endif
#if defined CORAL_SIZE_DYNAMICS
          CORAL(ng)%growth(n,i,j) = 0.0d0
          CORAL(ng)%mort(n,i,j) = 0.0d0
# endif


#if defined CORAL_ZOOXANTHELLAE
          ZOOX(ng)%Pg(n,i,j)=0.d0
          ZOOX(ng)%R (n,i,j)=0.d0
          ZOOX(ng)%dens(n,i,j) = 3.0d6 ! (cell/cm2)
          ZOOX(ng)%QC(n,i,j)   = 82.7d0  ! (pmolC/cell)  600-1400 pgC cell-1 (Muller-Parker et al., 1994)
!          ZOOX(ng)%ROS(n,i,j)  = 0.0d0  ! (pmolC/cell)
# if defined CORAL_CARBON_ISOTOPE
          ZOOX(ng)%Q13C(n,i,j) = R13C*ZOOX(ng)%QC(n,i,j)
# endif
# if defined CORAL_NUTRIENTS
          ZOOX(ng)%NO3(n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
!          ZOOX(ng)%NO2(n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
          ZOOX(ng)%NH4(n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
          ZOOX(ng)%PO4(n,i,j) = 0.0d0  !!!!!!!!!!!!!!!!!!!!!!!!!!
          ZOOX(ng)%QN(n,i,j)  = 1.1d0  ! (pmolN/cell)  calculated from C:N:P = 116:16:1
          ZOOX(ng)%QP(n,i,j)  = 0.07d0 ! (pmolP/cell)
# endif
# if defined CORAL_PHOTOINHIBITION
          ZPHOT(ng)%QAo(n,i,j) = 2.3d-6*0.6 ! (pmolRCII/cell) Gustafsson et al. 2014
          ZPHOT(ng)%QAr(n,i,j) = 2.3d-6*0.1 ! (pmolRCII/cell) Gustafsson et al. 2014
          ZPHOT(ng)%QAi(n,i,j) = 2.3d-6*0.3 ! (pmolRCII/cell) Gustafsson et al. 2014
          ZPHOT(ng)%Rep(n,i,j) = 0.9d0 ! Repair status 0-1 (No dimension)
# endif
#endif
        enddo
      enddo
    enddo

    RETURN

  END SUBROUTINE initialize_coral
      

!!! **********************************************************************
!!!  Main program of coral polyp model (Nakamura et al. 2013, 2018)
!!! **********************************************************************

  SUBROUTINE coral_polyp  &
!   input parameters
    ( ng, n, i, j    &   ! ng: nested grid number; n: coral compartment; i,j: position
    , dt             &   ! Time step (sec)
    , PFD            &   ! Photon flux density (umol m-2 s-1)
    , rho_sw         &   ! Density of seawater (g cm-3)
    , Tamb           &   ! Temperature (oC)
    , Samb           &   ! Salinity (PSU)
    , DICamb         &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
    , TAamb          &   ! Total alkalinity (TA: umol kg-1)
    , DOamb          &   ! Dissolved oxygen (umol L-1)
#if defined CORAL_INGESTION
    , PHYamb         &   ! phytoplankton (umol C L-1)
    , ZOOamb         &   ! zooplankton (umol C L-1)
#endif
#if defined CORAL_CARBON_ISOTOPE
    , DI13Camb       &   !13C of DIC (umol kg-1)
#endif
#if defined CORAL_NUTRIENTS         
    , NO3amb         &   ! NO3 (umol L-1)
!    , NO2amb         &   ! NO2 (umol L-1)
    , NH4amb         &   ! NH4 (umol L-1)
    , PO4amb         &   ! PO4 (umol L-1)
#endif
    , tau_amb        &   ! bottom shear stress (N m-2)
    , Fsed           &   ! sedimentation rate (??)
!   output parameters
    , DICuptake      &   ! DIC uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
    , TAuptake       &   ! TA  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
    , DOuptake       &   ! DO  uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
#if defined ORGANIC_MATTER
    , DOCuptake      &   ! DOC uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
    , POCuptake      &   ! POC uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
#endif
#if defined CORAL_INGESTION
    , PHYuptake      &   ! Phytoplankton ingestion rate (nmol cm-2 s-1)  * direction of water column to coral is positive
    , ZOOuptake      &   ! Zooplankton ingestion rate (nmol cm-2 s-1)  * direction of water column to coral is positive
#endif
#if defined CORAL_CARBON_ISOTOPE
    , DI13Cuptake    &   ! DI13C uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
#endif
#if defined CORAL_NUTRIENTS
    , NO3uptake      &   ! NO3 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
!    , NO2uptake      &   ! NO2 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
    , NH4uptake      &   ! NH4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
    , PO4uptake      &   ! PO4 uptake rate (nmol cm-2 s-1)  * direction of water column to coral is positive
# if defined ORGANIC_MATTER
    , DONuptake      &   ! DON uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
    , PONuptake      &   ! PON uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
    , DOPuptake      &   ! DOP uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
    , POPuptake      &   ! POP uptake rate (nmol cm-2 s-1) * direction of water column to coral is positive
# endif
#endif
    )

!-----------------------------------------------------------------------

    USE mod_geochem
    
    implicit none

! input parameters
    integer, intent(in) :: ng, n, i, j
    real(8), intent(in) :: dt
    real(8), intent(in) :: PFD      
    real(8), intent(in) :: rho_sw   
    real(8), intent(in) :: Tamb     
    real(8), intent(in) :: Samb     
    real(8), intent(in) :: DICamb   
    real(8), intent(in) :: TAamb    
    real(8), intent(in) :: DOamb    
#if defined CORAL_INGESTION
    real(8), intent(in) :: PHYamb   
    real(8), intent(in) :: ZOOamb   
#endif
#if defined CORAL_CARBON_ISOTOPE
    real(8), intent(in) :: DI13Camb 
#endif
#if defined CORAL_NUTRIENTS
    real(8), intent(in) :: NO3amb   
!    real(8), intent(in) :: NO2amb   
    real(8), intent(in) :: NH4amb   
    real(8), intent(in) :: PO4amb   
#endif
    real(8), intent(in) :: tau_amb  
    real(8), intent(in) :: Fsed
! output parameters
    real(8), intent(out) :: DICuptake
    real(8), intent(out) :: TAuptake 
    real(8), intent(out) :: DOuptake 
#if defined ORGANIC_MATTER
    real(8), intent(out) :: DOCuptake
    real(8), intent(out) :: POCuptake
#endif
#if defined CORAL_INGESTION
    real(8), intent(out) :: PHYuptake
    real(8), intent(out) :: ZOOuptake
#endif
#if defined CORAL_CARBON_ISOTOPE
    real(8), intent(out) :: DI13Cuptake
#endif
#if defined CORAL_NUTRIENTS
    real(8), intent(out) :: NO3uptake
!    real(8), intent(out) :: NO2uptake
    real(8), intent(out) :: NH4uptake
    real(8), intent(out) :: PO4uptake
# if defined ORGANIC_MATTER
    real(8), intent(out) :: DONuptake
    real(8), intent(out) :: PONuptake
    real(8), intent(out) :: DOPuptake
    real(8), intent(out) :: POPuptake
# endif
#endif

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
!          *cHCO3coe/(K_HCO3+cHCO3coe)
!          *hcoe

    real(8), parameter :: Pgmax(Ncl)=  (/ 0.6d0, 0.6d0 /) !0.73d0*    !0.91d0    !Maximum photosynthesis rate (nmol O2 cm-2 s-1) Kuhl et al. (1995) *Tuned
    real(8), parameter :: Ik(Ncl)=     (/ 275.d0, 275.d0 /)   !(275 uEin m-2 s-1) * refference value

!------ Goreau et al. (1996)------------------------------------------   
!                    Coral    FIZ    CZ
!          K_HCO3   408.    71.     178.   uM
!          V_HCO3     1.45   1.09    13.   nmol O2 (ug Chl a)-1 min-1
!          K_CO2aq    5.     0.67     2.7  uM
!          V_CO2aq    2.     1.15    12.5  nmol O2 (ug Chl a)-1 min-1
!---------------------------------------------------------------------
    real(8), parameter :: K_HCO3(Ncl)= (/ 408.d0, 408.d0 /) !uM
    real(8), parameter :: V_HCO3(Ncl)= (/ 1.45d0, 1.45d0 /) !nmol O2 (ug Chl a)-1 min-1
    real(8), parameter :: K_CO2aq(Ncl)=(/ 5.d0, 5.d0 /)     !uM
    real(8), parameter :: V_CO2aq(Ncl)=(/ 2.d0, 2.d0 /)     !nmol O2 (ug Chl a)-1 min-1

!   Temperature dependency parameters (Hikosaka et al. 2006) 
!      - Parameters estimated from data by Al-Horani (2005)

    real(8), parameter :: Hact(Ncl) = (/ 1.855d5, 1.855d5 /) !Activation energy (J mol-1)) *tuned
    real(8), parameter :: Hdeact(Ncl)=(/ 3.055d5, 3.055d5 /) !Deactivation energy (J mol-1) *tuned
    real(8), parameter :: Entr(Ncl)=  (/ 1028.d0, 1028.d0 /) !Entropy term (J K-1) *tuned

!----- for respiration --------------------------

!    R= k_R(n)*CORAL(ng)%QC(n,i,j)/(K_QC(n)+CORAL(ng)%QC(n,i,j))
!       *CORAL(ng)%DOcoe(n,i,j)/(K_DO(n)+CORAL(ng)%DOcoe(n,i,j))

                                             !!! * Nakamura et al. 2013 setting; ** New setting

    real(8), parameter :: Rmax(Ncl) = (/ 0.30d0, 0.30d0 /) !!!0.53d0*  0.37d0** !Maximum respiration rate (nmol O2 cm-2 s-1) *Tuned using Kuhl et al (1995), Al-Horani et al. (2003) data
!    real(8), parameter :: K_QC(Ncl) = (/ 20.d0, 20.d0 /)     !!!5.0d0* umol cm-2!!!
    real(8), parameter :: K_QC(Ncl) = (/ 150.d0, 150.d0 /)     !!!5.0d0* umol cm-2!!!
    real(8), parameter :: K_DO(Ncl) = (/ 46.d0, 46.d0 /)   ! ca. 46 (umol kg-1) Newton & Atkinson (1991)

    real(8), parameter :: ratio(Ncl) = (/ 0.8d0, 0.8d0 /) !!!0.9d0** 0.8d0  !1.0d0* !!!

!   Temperature dependency parameters (Hikosaka et al. 2006) 
!      - Parameters estimated from data by Fujimura et al. (2008)

    real(8), parameter :: Eact(Ncl) = (/ 5.117d4, 5.117d4 /) !Activation energy (J mol-1)) *Tuned

!----- for calcification ------------------------

    real(8), parameter :: eeff(Ncl)  = (/ 0.05d0, 0.05d0 /)   !!! 0.01d0** !0.3d0*!Energy efficency of calcification (<12%, 4~7%?; Lervik et al., 2012)
    real(8), parameter :: E_m(Ncl)=(/ 2.0d4, 2.0d4 /)   !!! 1.0d4** ! 5.0d3* Energy flux for maintenance (nJ cm-2 s-1)

    real(8), parameter :: k_CO2i(Ncl) =(/ 1.5d-3, 1.5d-3 /)  ! 1.5d-3* !permeability coefficient (cm s-1): Sueltemeyer and Rinast (1996): (1.49d-3 cm s-1)
    real(8), parameter :: k_TA(Ncl)   =(/ 1.0d-4, 1.0d-4 /)  !!! 3.0d-4**  5.0d-5 !3.0d-3* !conductivity of TA through the leak pass (cm s-1) *Tuned
    real(8), parameter :: k_DIC(Ncl)  =(/ 1.0d-4, 1.0d-4 /)  !!! 3.0d-4**  5.0d-5 !3.0d-3* !conductivity of DIC through the leak pass (cm s-1) *Tuned
#if defined CORAL_MUCUS
    real(8), parameter :: ForgC(Ncl)  =(/ 0.232d0/20.0d0, -0.073d0/2.5d0 /)  !!! Organic carbon release rate(+) or uptake rate(-) (nmol cm-2 s-1) *Tuned
    ! 200 mmol m-2 d-1 for 100% covered inner reef corals
    ! -63 mmol m-2 d-1 for 100% covered reef slope corals 
    ! For converting reef scale (mmol m-2 d-1) to polyp scale (nmol cm-2 s-1)
    !   1 mmol m-2 d-1 = 100/24/60/60/P2R(n) nmol cm-2 s-1 ~ 1.16d-3/P2R(n),
    !   Thus: 200*1.16d-3/P2R =  0.232/P2R nmol cm-2 s-1 for inner reef corals
    !!!       -63*1.16d-3/P2R = -0.073/P2R nmol cm-2 s-1 for reef slope corals  ****** To Be Confirmed
#endif
#if defined CORAL_ZOOXANTHELLAE
!    real(8), parameter :: K_ROS(Ncl)   = (/ 10.0d0, 10.0d0 /)     !!! umol L !!! To Be Confirmed
!    real(8), parameter :: V_detox(Ncl) = (/ 2.0d0, 2.0d0 /)   ! 
    real(8), parameter :: k_detox(Ncl) = (/ 3.0d7, 3.0d7 /)     ! Rate constant of specific catalase activity (M-1 s-1): Ogura (1955), Mueller et al., (1997)
    real(8), parameter :: CAT(Ncl)     = (/ 1.0d-10, 1.0d-10 /)   ! Catalase concentration (M): !!!!!!! Tuning
    real(8), parameter :: k_dam(Ncl)   = (/ 0.0d-5, 0.0d-5 /)   ! Reaction rate constant (s-1): !!!!!!! Tuning
    real(8), parameter :: a_dam(Ncl)   = (/ 2.0d3,  2.0d3  /)   ! Constant ((nmol cm-2 s-1) -1): !!!!!!! Tuning
    real(8), parameter :: k_Zelm(Ncl)  = (/ 0.2d0,  0.2d0 /)   ! Reaction rate constant (cell cm-2 s-1): !!!!!!! Tuning
!    real(8), parameter :: F_cROS(Ncl)  = (/ 3.0d-1, 3.0d-1 /)   ! ROS production rate by coral (nmol cm-2 s-1): !!!!!!! Tuning
    real(8), parameter :: a_cROS(Ncl)  = (/ 2.0d-2, 2.0d-2 /)   ! ROS production rate by coral (nmol cm-2 s-1): !!!!!!! Tuning
#endif
#if defined CORAL_CARBON_ISOTOPE
!----- for carbon isotope ------------------------
# if defined CARBON_TRACE
    real(8), parameter :: a_phot = 1.0d0
    real(8), parameter :: a_resp = 1.0d0
    real(8), parameter :: a_calc = 1.0d0 
    real(8), parameter :: a_co2  = 1.0d0
# else
    real(8), parameter :: a_phot = -20.0d-3 + 1.0d0 !!!!! To Be Confirmed
    real(8), parameter :: a_resp =   0.0d-3 + 1.0d0
    real(8), parameter :: a_calc =   2.7d-3 + 1.0d0 
    real(8), parameter :: a_co2  =  -7.0d-3 + 1.0d0 !!!!! To Be Confirmed. Maybe ~-8-4.4
# endif
#endif
#if ( defined CORAL_ZOOXANTHELLAE || defined CORAL_SIZE_DYNAMICS )
!----- Coral minimum quota ------------------------
!    real(8), parameter :: QC0(Ncl) = (/ 2.0d0, 2.0d0 /)   ! Minimum quota (umol cm-2)  refereed minimum value of tissue C by Tanaka et al., (2009)
    real(8), parameter :: QC0(Ncl) = (/130.0d0, 130.0d0 /) ! Minimum quota (umol cm-2)  refereed minimum value of tissue C by Tanaka et al., (2009)
# if defined CORAL_NUTRIENTS
    real(8), parameter :: QN0(Ncl) = (/ 0.20d0, 0.20d0 /)   ! Minimum quota (umol cm-2)  
    real(8), parameter :: QP0(Ncl) = (/ 0.02d0, 0.02d0 /)   ! Minimum quota (umol cm-2) 
# endif
#endif
#if defined CORAL_SIZE_DYNAMICS
    real(8), parameter :: g_max(Ncl) = (/ 1.0d-2, 1.0d-2 /) ! Maximum growth rate (nmol cm-2 s-1)  !!!!!! Tuning parameter
    real(8), parameter :: aC_m   = 2.0d-1   ! ((umol cm-2)-1)!!!!!!! Tuning parameter
    real(8), parameter :: km_dam = 1.0d-10  ! (s-1)!!!!!!!!!!!!!!!!! Tuning parameter
    real(8), parameter :: g2c(Ncl) = (/ 1.0d0, 1.0d0 /)  ! (net growth rate)/(calcification rate) (no dimension)!!!!!!!! Tuning parameter
    real(8), parameter :: km_es(Ncl) = (/ 1.0d0, 1.0d0 /)   ! !!!!!!!!!!!!!! TBC
    real(8), parameter :: km(Ncl) = (/ 1.0d0, 1.0d0 /)   ! !!!!!!!!!!!!!! TBC
#endif

!--- Other variables ----------------------------------------------
    real(8) :: Pgc, Rc
!    real(8) :: Si   !Mass transfer coefficient (cm s-1)
    real(8) :: Si_DIC, Si_TA, Si_DO   !Mass transfer coefficient (cm s-1)
#if defined ORGANIC_MATTER
    real(8) :: Si_DOC
    real(8) :: Si_POC
#endif
#if defined CORAL_INGESTION
    real(8) :: Si_PHY, Si_ZOO
#endif
# if defined CORAL_NUTRIENTS
    real(8) :: Si_NO3, Si_NH4, Si_PO4
# if defined ORGANIC_MATTER
    real(8) :: Si_DON, Si_DOP, Si_PON, Si_POP
# endif
#endif
#if defined CORAL_NONE_CO2_EQ
    real(8) :: Si_CO2
#endif
#if defined CORAL_CARBON_ISOTOPE  
    real(8) :: Si_DI13C
# if defined CORAL_NONE_CO2_EQ
    real(8) :: Si_13CO2
# endif
#endif
#if defined CORAL_ZOOXANTHELLAE
    real(8) :: Si_ROS
#endif
    real(8) :: TKamb   !Temperature (K)
!----- for CO2 system dynamics -------------------
    real(8) :: pHcal,fCO2cal,cHCO3cal,cCO3cal,Wargcal
    real(8) :: pHcoe,fCO2coe,cHCO3coe,cCO3coe
    real(8) :: pHamb,fCO2amb,cCO2aqamb,cHCO3amb,cCO3amb,Wargamb  !!!!!�s�v�F�A�E�g�v�b�g�p
#if defined CORAL_NONE_CO2_EQ
    real(8) :: cHcoe,cOHcoe
    real(8) :: cHcal,cOHcal
    real(8) :: Fpp_CO2aq, CO2aquptake
    real(8) :: kp1,km1,kp4,km4
#endif
!----- for gross photosynthesis -------------------
    real(8) :: PFDsurf
!     Temperature related factor
    real(8) :: f_temp
!----- for calcification ------------------------
    real(8) :: E_tot,E_ca,F_H,F_CO2
    real(8) :: Fpp_TA,Fpp_DIC
!    real(8) :: Fpp_CO2aq,Fpp_HCO3,Fpp_CO3

!----- for nutrients dynamics -------------------

!----- for cabon isotope calculation -------------------
#if defined CORAL_CARBON_ISOTOPE
    real(8) :: d13C_DICcoe,d13C_DICcal,d13C_QC,d13C_DICamb,d13C_arg
    real(8) :: R13Ccoe,R13Ccal,RQ13C,R13Camb,R13Carg
    real(8) :: R13CFpp,R13Cfco2,R13Cuptake
    real(8) :: F_13CO2,k_13CO2i
    real(8) :: Fpp_DI13C
    real(8) :: cH13CO3coe, c13CO3coe
    real(8) :: cH13CO3cal, c13CO3cal
    real(8) :: Fpp_13CO2aq, Fpp_H13CO3, Fpp_13CO3
    real(8) :: c13CO2aquptake
    real(8) :: R13C_HCO3cal
    real(8) :: c13CO2aqamb
#endif

#if defined CORAL_ZOOXANTHELLAE
    real(8) :: F_Csec
    real(8) :: F_Cwaste
    real(8) :: F_Zelm
    real(8) :: F_Zmort
    real(8) :: F_zROS
    real(8) :: F_detox
    real(8) :: F_dam
    real(8) :: F_cROS
    real(8) :: Damage
# if defined CORAL_NUTRIENTS
    real(8) :: F_Nsec
    real(8) :: F_Nwaste
    real(8) :: F_Psec
    real(8) :: F_Pwaste
# endif
#endif

#if defined CORAL_MUCUS
    real(8) :: F_Cmucus
# if defined CORAL_NUTRIENTS
    real(8) :: F_Nmucus
    real(8) :: F_Pmucus
# endif
#endif
#if defined CORAL_INGESTION
    real(8) :: F_Cingest
# if defined CORAL_NUTRIENTS
    real(8) :: F_Ningest
    real(8) :: F_Pingest
# endif
#endif
#if defined CORAL_SIZE_DYNAMICS
    real(8) :: E_g  !!! Energy flux for growth (nJ cm-2 s-1)
    real(8) :: F_Cgrowth
    real(8) :: netCgrowth
    real(8) :: g2c_tmp   ! (net growth rate)/(calcification rate)
    real(8) :: mort_es
# if defined CORAL_NUTRIENTS
    real(8) :: F_Ngrowth
    real(8) :: F_Pgrowth
# endif
#endif

#if defined CORAL_TESTMODE
!  Output
    real(8), parameter :: OUTPUT_INTERVAL  = 5.0d0    ! Output interval (min)
    real(8), parameter :: AVERAGE_INTERVAL = 1.0d0    ! Average interval (day)
    real(8), save :: time(Ncl)         = (/ 0.d0, 0.d0 /)  !sec
    real(8), save :: S_PFD_dt(Ncl)     = (/ 0.d0, 0.d0 /)
    real(8), save :: S_Gn_dt(Ncl)      = (/ 0.d0, 0.d0 /)
    real(8), save :: S_Pg_dt(Ncl)      = (/ 0.d0, 0.d0 /)
    real(8), save :: S_R_dt(Ncl)       = (/ 0.d0, 0.d0 /)
    real(8), save :: S_QC_dt(Ncl)      = (/ 0.d0, 0.d0 /)
    real(8), save :: dsec(Ncl)         = (/ 0.d0, 0.d0 /) !sec
    real(8), save :: dday(Ncl)         = (/ 1.d0, 1.d0 /) !day
# if defined CORAL_CARBON_ISOTOPE
    real(8), save :: S_d13CargxGn_dt(Ncl) = (/ 0.d0, 0.d0 /)
    real(8), save :: S_d13C_QC_dt(Ncl) = (/ 0.d0, 0.d0 /)
# endif
# if defined CORAL_BORON_ISOTOPE
    real(8) d11Barg
    real(8), save :: S_d11BargxGn_dt(Ncl) = (/ 0.d0, 0.d0 /)
# endif
#endif

!--------- Temperature (K)  --------------------------------

    TKamb=Tamb+273.15d0 !(oC->K)

!--------- calculate mass transfer velocity (cm s-1) -------

!    Si = (65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    
!    Si_DIC=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
!    Si_TA =(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
!    Si_DO =(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_DIC=S_i(tau_amb,dif_HCO3,340.0d0,1.05d0)*1.0d2
    Si_TA =S_i(tau_amb,dif_HCO3,340.0d0,1.05d0)*1.0d2
    Si_DO =S_i(tau_amb,dif_O2  ,340.0d0,1.05d0)*1.0d2
#if defined ORGANIC_MATTER
    Si_DOC=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_POC=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
#endif
#if defined CORAL_INGESTION
    Si_PHY=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_ZOO=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
#endif
#if defined CORAL_NUTRIENTS
    Si_NO3=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_NH4=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_PO4=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
# if defined ORGANIC_MATTER
    Si_DON=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_DOP=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_PON=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_POP=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
# endif
#endif
#if defined CORAL_NONE_CO2_EQ
!    Si_CO2=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_CO2=S_i(tau_amb,dif_CO2 ,340.0d0,1.05d0)*1.0d2
#endif
#if defined CORAL_CARBON_ISOTOPE  
!    Si_DI13C=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
    Si_DI13C=S_i(tau_amb,dif_HCO3,340.0d0,1.05d0)*1.0d2
# if defined CORAL_NONE_CO2_EQ
!    Si_13CO2=(65.7d0*tau_amb**0.4d0+4.7d0)*1.d-4
!    Si_13CO2=S_i(tau_amb,dif_CO2 ,340.0d0,1.05d0)*1.0d2
    Si_13CO2=S_i(tau_amb,dif_CO2/1.00087d0 ,340.0d0,1.05d0)*1.0d2   ! ratio of diffusion coefficent: 1.00087 by Jahne et al 1987 
# endif
#endif
#if defined CORAL_ZOOXANTHELLAE
    Si_ROS=S_i(tau_amb,dif_H2O2,340.0d0,1.05d0)*1.0d2
#endif

#if defined CORAL_NUTRIENTS

!------- Nutrients flux (nmol cm-2 s-1) ------------------------

! NH4 uptake
! M Atkinson, J Falter - Coral Reefs, 2001 
!     SiNH4 =7.5 m day-1 = 7.5*100./24./60./60. cm s-1
!     cNH4 umol kg-1 -> 1.023 nmol cm-3
!     Flux nmol cm-2 s-1

!    NH4uptake=7.5*100./86400.*cNH4*1.023
! NO3 uptake
!    NO3uptake=4.4*100./86400.*cNO3*1.023
! PO4 uptake
!    PO4uptake=7.4*100./86400.*cPO4*1.023

! NH4 uptake
    NH4uptake=Si_NH4*NH4amb
! NO3 uptake
    NO3uptake=Si_NO3*NO3amb
! PO4 uptake
    PO4uptake=Si_PO4*PO4amb

! Coral carbon cell quoto

!    c_SQC=min((NH4uptake+NO3uptake)*c_CNP(nC)/c_CNP(nN)
!                 ,PO4uptake*c_CNP(nC)/c_CNP(nP))
!
!    c_SQC=CORAL(ng)%QC(n,i,j)/(1000.+CORAL(ng)%QC(n,i,j))*c_SQC
!    c_SQN=c_SQC*c_CNP(nN)/c_CNP(nC)
!    c_SQP=c_SQC*c_CNP(nP)/c_CNP(nC)
!
!    tempb=NH4uptake/(NH4uptake+NO3uptake)*c_SQN
!    NO3uptake=NO3uptake/(NH4uptake+NO3uptake)*c_SQN
!    NH4uptake=tempb
!    PO4uptake=c_SQP

!    NH4uptake=0.
!! NO3 uptake
!    NO3uptake=0.
!! PO4 uptake
!    PO4uptake=0.
#endif

!----CO2*+H2O <-> H+ +HCO3- is none equilibrium
#if defined CORAL_NONE_CO2_EQ
!--------- CO2 system in calcified fulid -------------------
    pHcal     = pH_fromATCTcCO2aq(CORAL(ng)%TAcal(n,i,j),CORAL(ng)%DICcal(n,i,j),CORAL(ng)%cCO2aqcal(n,i,j),TKamb, Samb)
    cHCO3cal  = cHCO3_noneq(CORAL(ng)%DICcal(n,i,j),CORAL(ng)%cCO2aqcal(n,i,j),pHcal,TKamb, Samb)!! for Isotope
    cCO3cal   = cCO3_noneq(CORAL(ng)%DICcal(n,i,j),CORAL(ng)%cCO2aqcal(n,i,j),pHcal,TKamb, Samb)
    fCO2cal   = fCO2_fromcCO2aq(CORAL(ng)%cCO2aqcal(n,i,j),TKamb, Samb)  !! for output
    Wargcal   = Warg_fromcCO3cCa(cCO3cal,10.4d0,TKamb, Samb)  !Calcifying fluid {Ca2+]=10.4 mmol kg-1   Assumed
!--------- CO2 system in coelenteron -------------------
    pHcoe     = pH_fromATCTcCO2aq(CORAL(ng)%TAcoe(n,i,j),CORAL(ng)%DICcoe(n,i,j),CORAL(ng)%cCO2aqcoe(n,i,j),TKamb, Samb)
    cHCO3coe  = cHCO3_noneq(CORAL(ng)%DICcoe(n,i,j),CORAL(ng)%cCO2aqcoe(n,i,j),pHcoe,TKamb, Samb)
    cCO3coe   = cCO3_noneq(CORAL(ng)%DICcoe(n,i,j),CORAL(ng)%cCO2aqcoe(n,i,j),pHcoe,TKamb, Samb)!! for Isotope
    fCO2coe   = fCO2_fromcCO2aq(CORAL(ng)%cCO2aqcoe(n,i,j),TKamb, Samb)  !! for output

!--------- CO2 system in ambient seawater ------------------- !! for output
    pHamb   = pH_fromATCT(TAamb,DICamb,TKamb, Samb)
    cCO2aqamb = cCO2aq_fromCTpH(DICamb,pHamb,TKamb, Samb)
    cHCO3amb = cHCO3_fromCTpH(DICamb,pHamb,TKamb, Samb)
    cCO3amb = cCO3_fromCTpH(DICamb,pHamb,TKamb, Samb)
    fCO2amb   = fCO2_fromcCO2aq(cCO2aqamb,TKamb, Samb)  !! for output
    Wargamb = Warg_fromcCO3(cCO3amb,TKamb, Samb)

!----CO2 equilibrium
#else
!--------- CO2 system in calcified fulid -------------------
    pHcal     = pH_fromATCT(CORAL(ng)%TAcal(n,i,j),CORAL(ng)%DICcal(n,i,j),TKamb, Samb)
    CORAL(ng)%cCO2aqcal(n,i,j) = cCO2aq_fromCTpH(CORAL(ng)%DICcal(n,i,j),pHcal,TKamb, Samb)
    cHCO3cal  = cHCO3_fromCTpH(CORAL(ng)%DICcal(n,i,j),pHcal,TKamb, Samb)!! for Isotope
    cCO3cal   = cCO3_fromCTpH(CORAL(ng)%DICcal(n,i,j),pHcal,TKamb, Samb)
    fCO2cal   = fCO2_fromcCO2aq(CORAL(ng)%cCO2aqcal(n,i,j),TKamb, Samb)  !! for output
    Wargcal   = Warg_fromcCO3cCa(cCO3cal,10.4d0,TKamb, Samb)  !Calcifying fluid {Ca2+]=10.4 mmol kg-1   Assumed

!--------- CO2 system in coelenteron -------------------
    pHcoe     = pH_fromATCT(CORAL(ng)%TAcoe(n,i,j),CORAL(ng)%DICcoe(n,i,j),TKamb, Samb)
    CORAL(ng)%cCO2aqcoe(n,i,j) = cCO2aq_fromCTpH(CORAL(ng)%DICcoe(n,i,j),pHcoe,TKamb, Samb)!! for Isotope and output
    cHCO3coe  = cHCO3_fromCTpH(CORAL(ng)%DICcoe(n,i,j),pHcoe,TKamb, Samb)
    cCO3coe   = cCO3_fromCTpH(CORAL(ng)%DICcoe(n,i,j),pHcoe,TKamb, Samb)!! for Isotope
    fCO2coe   = fCO2_fromcCO2aq(CORAL(ng)%cCO2aqcoe(n,i,j),TKamb, Samb)  !! for output

# if defined CORAL_TESTMODE
!--------- CO2 system in ambient seawater ------------------- !! for output
    pHamb   = pH_fromATCT(TAamb,DICamb,TKamb, Samb)
    cCO2aqamb = cCO2aq_fromCTpH(DICamb,pHamb,TKamb, Samb)
    cHCO3amb = cHCO3_fromCTpH(DICamb,pHamb,TKamb, Samb)
    cCO3amb = cCO3_fromCTpH(DICamb,pHamb,TKamb, Samb)
    fCO2amb   = fCO2_fromcCO2aq(cCO2aqamb,TKamb, Samb)  !! for output
    Wargamb = Warg_fromcCO3(cCO3amb,TKamb, Samb)
# endif
#endif  /*end CORAL_NONE_CO2_EQ*/


!----- Gross photosynthesis rate (nmolO2 cm-2 s-1) -----------------

!   Sediment dependent term
    PFDsurf=PFD!*exp(-lamb*dsed)

#if defined CORAL_ZOOXANTHELLAE
!-------- ROS release rate by mitochondria in coral  (nmol cm-2 s-1) ----------------------------
!    F_cROS = a_cROS(n)*Tamb + b_cROS(n)
    F_cROS = a_cROS(n)*CORAL(ng)%R(n,i,j)
!-------- ROS detox rate (nmol cm-2 s-1) ----------------------------

!    F_detox=V_detox(n)*CORAL(ng)%ROS(n,i,j)/(K_ROS(n)+CORAL(ng)%ROS(n,i,j))
    F_detox = k_detox(n) * CORAL(ng)%ROS(n,i,j) * CAT(n)
!-------- ROS oxydation rate (damage rate) (nmol cm-2 s-1) ----------------------------
    F_dam = k_dam(n) * CORAL(ng)%ROS(n,i,j)

!  Zooxanthellae release rate by host coral (cell cm-2 s-1)
!    Damage=exp(F_dam + 3.*F_ONOO)-1.   !Damage by ROS (F_dam) and peroxynitrite (F_ONOO)
!    Damage=exp(F_dam + F_ONOO)-1.   !Damage by ROS (F_dam) and peroxynitrite (F_ONOO)
    Damage = exp( a_dam(n) * F_dam ) - 1.0d0   !Damage by ROS (F_dam)
    F_Zelm  = k_Zelm(n) * Damage

!----- Zooxanthellae internal model -----

    CALL zooxanthellae             &
!     input parameters
      ( ng, n, i, j                & ! ng: nested grid number; n: coral compartment; i,j: position
      , dt                         & ! Time step (sec)
      , PFDsurf                    & ! Photon flux density (umol m-2 s-1)
      , Tamb                       & ! Temperature (oC)
      , CORAL(ng)%cCO2aqcoe(n,i,j) & ! CO2* concentration in the coral tissue (umol kg-1)
      , cHCO3coe                   & ! HCO3- concentration in the coral tissue (umol kg-1)
      , F_Zelm                     & ! Zooxanthellae release rate by host coral (cell cm-2 s-1)
      , CORAL(ng)%ROS(n,i,j)       & ! ROS concentration in the coral tissue (umol L-1)
!     output parameters
      , F_Csec                     & ! Organic C excretion controled by host coral (pmol cell-1 s-1)
      , F_zROS                     & ! Flux of Reactive Oxygen Species  (pmol cell-1 s-1)
# if defined CORAL_NUTRIENTS
      , F_Nsec                     & ! Organic N excretion controled by host coral (pmol cell-1 s-1)
      , F_Psec                     & ! Organic P excretion controled by host coral (pmol cell-1 s-1)
      , NO3_trans                  & ! NO3 transport rate from host to Zooxanthellae (pmol cell-1 s-1)
!      , NO2_trans                  & ! NO2 transport rate from host to Zooxanthellae (pmol cell-1 s-1)
      , NH4_trans                  & ! NH4 transport rate from host to Zooxanthellae (pmol cell-1 s-1)
      , PO4_trans                  & ! PO4 transport rate from host to Zooxanthellae (pmol cell-1 s-1)
      , F_ONOO                     & ! Flux of peroxynitrite  (pmol cell-1 s-1)
# endif
      , F_Zmort                    & ! Dead zooxanthellae release rate (cells cm-2 s-1)
      )

    !----- Convert (pmol cell-1 s-1) to (nmol cm-2 s-1)------------------------------------

    ZOOX(ng)%Pg(n,i,j)=ZOOX(ng)%Pg(n,i,j)*ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
    ZOOX(ng)%R (n,i,j)=ZOOX(ng)%R (n,i,j)*ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1) 
    F_zROS = F_zROS * ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
    F_Csec = F_Csec*ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
    F_Cwaste = F_Zmort*ZOOX(ng)%QC(n,i,j)*1.0d-3  !=(cell cm-2 s-1)*(pmol cell-1)*1.e-3=(nmol cm-2 s-1) 
# if defined CORAL_NUTRIENTS
    F_Nsec = F_Nsec*ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
    F_Psec = F_Psec*ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
    NO3_trans=NO3_trans*ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
!    NO2_trans=NO2_trans*ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
    NH4_trans=NH4_trans*ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
    PO4_trans=PO4_trans*ZOOX(ng)%dens(n,i,j)*1.0d-3  !=(pmol cell-1 s-1)*(cell cm-1)*1.e-3=(nmol cm-2 s-1)
    F_Nwaste = F_Zmort*ZOOX(ng)%QN(n,i,j)*1.0d-3  !=(cell cm-2 s-1)*(pmol cell-1)*1.e-3=(nmol cm-2 s-1)
    F_Pwaste = F_Zmort*ZOOX(ng)%QP(n,i,j)*1.0d-3  !=(cell cm-2 s-1)*(pmol cell-1)*1.e-3=(nmol cm-2 s-1)
# endif
      
    Pgc = ZOOX(ng)%Pg(n,i,j)-ZOOX(ng)%R(n,i,j)

#else

! Temperature dependent term
    f_temp=1.d0
!    f_temp=exp(Hact(n)*(TKamb-298.)/298./Rgas/TKamb)        &
!          *(1.+exp((298.*Entr(n)-Hdeact(n))/298./Rgas))     &
!          /(1.+exp((TKamb*Entr(n)-Hdeact(n))/TKamb/Rgas))

! Gross photosynthesis rate (nmolO2 cm-2 s-1)
!    Pg=Pgmax(n)*f_temp*tanh(PFDsurf/Ik(n))                  &
!        *1./(V_HCO3(n)+V_CO2aq(n))                          &
!          *( V_HCO3(n)*cHCO3coe/(K_HCO3(n)+cHCO3coe)        &
!            +V_CO2aq(n)*CORAL(ng)%cCO2aqcoe(n,i,j)/(K_CO2aq(n)+CORAL(ng)%cCO2aqcoe(n,i,j))   &
!           )

    Pgc = Pgmax(n)*f_temp*tanh(PFDsurf/Ik(n))       &
                        * cHCO3coe/(K_HCO3(n)+cHCO3coe)

#endif

!----- Respilation rate (nmolO2 cm-2 s-1) ---------------------------

!   Temperature dependent term
    f_temp=1.d0
!    f_temp=exp(Eact(n)*(TKamb-298.)/298./Rgas/TKamb)

    Rc = Rmax(n)*f_temp                                            &
         *CORAL(ng)%QC(n,i,j)**6.d0/(K_QC(n)**6.d0+CORAL(ng)%QC(n,i,j)**6.d0)    & !  Hill equation
         *CORAL(ng)%DOcoe(n,i,j)/(K_DO(n)+CORAL(ng)%DOcoe(n,i,j))   !  Michaelis-Menten equation

!----- H+ flux through Ca2+ ATPase  (nmol cm-2 s-1) --------------------------

!   energy Flux (nJ cm-2 s-1) partition

!  C6H12O6 + 6 O2 + 38 ADP + 38 Pi -> 6 CO2 + 6 H2O + 38 ATP  dG=-2873.4 kJ/mol
!  ATP + H2O -> ADP + Pi  dG=-7.3kcal/mol=-30.5kJ/mol

    E_tot=30.5d3 * 38.0d0/6.0d0 * Rc  !nJ cm-2 s-1

    if (E_tot > E_m(n) ) then
#if defined CORAL_SIZE_DYNAMICS
      netCgrowth = ( CORAL(ng)%growth(n,i,j)-CORAL(ng)%mort(n,i,j) )  &
                   *CORAL(ng)%QC(n,i,j)*1.0d3    ! growth rate (nmol cm-2 s-1)=(cm2 cm-2 s-1)*(umol cm-2)*1.d3
      g2c_tmp = netCgrowth/CORAL(ng)%G(n,i,j)
      E_g = (E_tot - E_m(n))*g2c_tmp
      E_ca = E_tot - E_m(n) - E_g
#else
      E_ca = E_tot-E_m(n)
#endif
    else
      E_ca=0.0d0
#if defined CORAL_SIZE_DYNAMICS
      mort_es = km_es(n)*( E_m(n) - E_tot ) ! Mortality due to energy shortage
#endif
    endif

!   H+ flux (nmol cm-2 s-1)

    F_H= E_ca * eeff(n)/                                      &
      (2.3d0*8.31d0*(Tamb+273.15d0)                           &
                     *max((pHcal-pHcoe)+0.01d0, 0.01d0))
!             *10.**(-pHcal)*10.**8
!    F_H=max(F_H,0.) !Error handring
!    F_H=E_ca/50000.*2.

#if defined CORAL_MUCUS
!----- Coral mucus release rate (nmol cm-2 s-1)--------------------------
!
    F_Cmucus = ForgC(n)  !!!! constant value is assumed
      
# if defined ORGANIC_MATTER
#  if defined CORAL_ZOOXANTHELLAE
    DOCuptake = -F_Cmucus 
    POCuptake = -F_Cwaste
#  else
    DOCuptake = -F_Cmucus
    POCuptake = 0.0d0
#  endif

#  if defined CORAL_NUTRIENTS
    F_Nmucus = ???
    F_Pmucus = ???
#   if defined CORAL_ZOOXANTHELLAE
    DONuptake = -F_Nmucus 
    PONuptake = -F_Nwaste
    DOPuptake = -F_Pmucus 
    POPuptake = -F_Pwaste
#   else
    DONuptake = -F_Nmucus
    PONuptake = 0.0d0
    DOPuptake = -F_Pmucus
    POPuptake = 0.0d0
#   endif
#  endif
# endif
!----- END CORAL_MUCUS --------------------------
#endif

#if defined CORAL_INGESTION
!----- Plankton ingestion (nmol cm-2 s-1)--------------------------
!
!    PHYuptake=Si_PHY*cPHY*1.023d0
    PHYuptake = 0.0d0
    ZOOuptake = Si_ZOO*ZOOamb
    
    F_Cingest = PHYuptake+ZOOuptake
# if defined CORAL_NUTRIENTS
    F_Ningest = ???
    F_Pingest = ???
# endif
!----- END CORAL_INGESTION --------------------------
#endif
!---------- Other fluxes (nmol cm-2 s-1) ----------------------------

!   CO2* flux (nmol cm-2 s-1)
!    F_CO2=k_CO2i(n)*(fCO2coe-fCO2cal)
    F_CO2=k_CO2i(n)*(CORAL(ng)%cCO2aqcoe(n,i,j)-CORAL(ng)%cCO2aqcal(n,i,j))*rho_sw
      
!   Leak flux (nmol cm-2 s-1) 
    Fpp_TA =k_TA(n) *(CORAL(ng)%TAcoe(n,i,j)-CORAL(ng)%TAcal(n,i,j))*rho_sw
    Fpp_DIC=k_DIC(n)*(CORAL(ng)%DICcoe(n,i,j)-CORAL(ng)%DICcal(n,i,j))*rho_sw
    
!    Fpp_TA =k_TA(n) *(TAamb-CORAL(ng)%TAcal(n,i,j))*rho_sw
!    Fpp_DIC=k_DIC(n)*(DICamb-CORAL(ng)%DICcal(n,i,j))*rho_sw

!   DIC and TA uptake (nmol cm-2 s-1)
    DICuptake=Si_DIC*(DICamb-CORAL(ng)%DICcoe(n,i,j))*rho_sw
    TAuptake =Si_TA *(TAamb-CORAL(ng)%TAcoe(n,i,j))*rho_sw

!   DO uptake (nmol cm-2 s-1)
    DOuptake =Si_DO *(DOamb-CORAL(ng)%DOcoe(n,i,j))

#if defined CORAL_NONE_CO2_EQ
    Fpp_CO2aq=k_DIC(n)*(CORAL(ng)%cCO2aqcoe(n,i,j)-CORAL(ng)%cCO2aqcal(n,i,j))*rho_sw
    CO2aquptake=Si_CO2*(cCO2aqamb-CORAL(ng)%cCO2aqcoe(n,i,j))*rho_sw
#endif

#if defined CORAL_SIZE_DYNAMICS
!
!---------- Coral growth (nmol cm-2 s-1) ------------
!
!    Droop model (1973) + Liebig's minimum law

# if defined CORAL_NUTRIENTS

    F_Cgrowth = g_max(n)*min( 1.0d0 - QC0(n)/CORAL(ng)%QC(n,i,j) ,    &
                          min(1.0d0 - QN0(n)/CORAL(ng)%QN(n,i,j),     &
                              1.0d0 - QP0(n)/CORAL(ng)%QP(n,i,j) ) )  &
    F_Cgrowth = max(F_Cgrowth, 0.0d0)
    
    F_Ngrowth = F_Cgrowth*CORAL(ng)%QN(n,i,j)/CORAL(ng)%QC(n,i,j)
    F_Pgrowth = F_Cgrowth*CORAL(ng)%QP(n,i,j)/CORAL(ng)%QC(n,i,j)

# else
!    F_Cgrowth = g_max(n) * (1.0d0 - QC0(n)/CORAL(ng)%QC(n,i,j) )
    F_Cgrowth = 5.0d-5 * (CORAL(ng)%QC(n,i,j) - QC0(n))
    F_Cgrowth = max(F_Cgrowth, 0.0d0)

# endif
!
!----- Convert internal conditions to size dynamics parameters
!
!---------- Coral growth rate (cm cm-2 s-1) ------------
!
    CORAL(ng)%growth(n,i,j) = F_Cgrowth/CORAL(ng)%QC(n,i,j)*1.0d-3  ! growth rate (cm2 cm-2 s-1)
!   (cm2 cm-2 s-1)=(nmol cm-2 s-1)/(umol cm-2)*1.d-3
!
!---------- Coral mortality (cm cm-2 s-1) ------------
!
# if defined CORAL_NUTRIENTS

    CORAL(ng)%mort(n,i,j) = km_CNP                                   &  ! mortality (cm2 cm-2 s-1) 
                    *exp(-am_CNP*                                    &  !!!!!!!!!!!!!!!!TBC
                        min( 1.0d0 - QC0(n)/CORAL(ng)%QC(n,i,j) ,    &
                         min(1.0d0 - QN0(n)/CORAL(ng)%QN(n,i,j),     &
                             1.0d0 - QP0(n)/CORAL(ng)%QP(n,i,j) )))  & 
                   +km_dam * Damage                                  & !!!!!!!!!!!!!!!TBC
                   +mort_es !!!!!!!!!!!!!!!TBC
# else
    CORAL(ng)%mort(n,i,j) =                                   &
          km(n) *exp( aC_m*(QC0(n)-CORAL(ng)%QC(n,i,j)) )     &
         +km_dam * Damage                                     & !!!!!!!!!!!!!!!!!TBC
         +mort_es
# endif

!    CORAL(ng)%mort(n,i,j) = min(CORAL(ng)%mort(n,i,j), 1.0d0)  !!! for Error handling

!-----------END CORAL_SIZE_DYNAMICS ------------------------------------
#endif

#if defined CORAL_CARBON_ISOTOPE  
!!! ----- Isotope calculation ----------------------------------------------------

# if defined CORAL_NONE_CO2_EQ
!----------- CO2 system in calcified fulid -------------------

!    cH13CO3cal  = cH13CO3_noneq(CORAL(ng)%DI13Ccal(n,i,j), CORAL(ng)%c13CO2aqcal(n,i,j), pHcal, TKamb, Samb)
!    c13CO3cal   = c13CO3_noneq(CORAL(ng)%DI13Ccal(n,i,j), CORAL(ng)%c13CO2aqcal(n,i,j), pHcal, TKamb, Samb)
    cH13CO3cal  = cH13CO3_noneq2(CORAL(ng)%DI13Ccal(n,i,j), CORAL(ng)%c13CO2aqcal(n,i,j),cHCO3cal,cCO3cal,TKamb)
    c13CO3cal   = c13CO3_noneq2(CORAL(ng)%DI13Ccal(n,i,j), CORAL(ng)%c13CO2aqcal(n,i,j),cHCO3cal,cCO3cal,TKamb)

!----------- CO2 system in coelenteron -------------------

!    cH13CO3coe  = cH13CO3_noneq(CORAL(ng)%DI13Ccoe(n,i,j), CORAL(ng)%c13CO2aqcoe(n,i,j), pHcoe, TKamb, Samb)
!    c13CO3coe   = c13CO3_noneq(CORAL(ng)%DI13Ccoe(n,i,j), CORAL(ng)%c13CO2aqcoe(n,i,j), pHcoe, TKamb, Samb)
    cH13CO3coe  = cH13CO3_noneq2(CORAL(ng)%DI13Ccoe(n,i,j), CORAL(ng)%c13CO2aqcoe(n,i,j),cHCO3coe,cCO3coe,TKamb)
    c13CO3coe   = c13CO3_noneq2(CORAL(ng)%DI13Ccoe(n,i,j), CORAL(ng)%c13CO2aqcoe(n,i,j),cHCO3coe,cCO3coe,TKamb)
      
!----------- CO2 system in ambient seawater -------------------

    c13CO2aqamb = conc13CO2aq(DI13Camb,cCO2aqamb,cHCO3amb,cCO3amb,TKamb)

# else
!----------- CO2 system in calcified fulid -------------------

    CORAL(ng)%c13CO2aqcal(n,i,j) = conc13CO2aq(CORAL(ng)%DI13Ccal(n,i,j),CORAL(ng)%cCO2aqcal(n,i,j),cHCO3cal,cCO3cal,TKamb)
    cH13CO3cal  = concH13CO3(CORAL(ng)%DI13Ccal(n,i,j),CORAL(ng)%cCO2aqcal(n,i,j),cHCO3cal,cCO3cal,TKamb)
    c13CO3cal   = conc13CO3(CORAL(ng)%DI13Ccal(n,i,j),CORAL(ng)%cCO2aqcal(n,i,j),cHCO3cal,cCO3cal,TKamb)

!----------- CO2 system in coelenteron -------------------

    CORAL(ng)%c13CO2aqcoe(n,i,j) = conc13CO2aq(CORAL(ng)%DI13Ccoe(n,i,j),CORAL(ng)%cCO2aqcoe(n,i,j),cHCO3coe,cCO3coe,TKamb)
    cH13CO3coe  = concH13CO3(CORAL(ng)%DI13Ccoe(n,i,j),CORAL(ng)%cCO2aqcoe(n,i,j),cHCO3coe,cCO3coe,TKamb)
    c13CO3coe   = conc13CO3(CORAL(ng)%DI13Ccoe(n,i,j),CORAL(ng)%cCO2aqcoe(n,i,j),cHCO3coe,cCO3coe,TKamb)

# endif

! Carbon isotope ratio R13C=13C/12C

!    RQ13C=CORAL(ng)%Q13C(n,i,j)/(CORAL(ng)%QC(n,i,j)-CORAL(ng)%Q13C(n,i,j))   !coral organism
!    R13Ccoe=CORAL(ng)%DI13Ccoe(n,i,j)/(CORAL(ng)%DICcoe(n,i,j)-CORAL(ng)%DI13Ccoe(n,i,j))   !DIC in coelenteron
!    R13Ccal=CORAL(ng)%DI13Ccal(n,i,j)/(CORAL(ng)%DICcal(n,i,j)-CORAL(ng)%DI13Ccal(n,i,j))   !DIC in calcifying fluid
!    R13Camb=DI13Camb/(DICamb-DI13Camb)!!!!!  DIC in ambient seawater
!    
!    CORAL(ng)%c13CO2aqcoe=CORAL(ng)%cCO2aqcoe(n,i,j)*R13Ccoe*a_co2/(1.+R13Ccoe*a_co2)
!    CORAL(ng)%c13CO2aqcal(n,i,j)=CORAL(ng)%cCO2aqcal(n,i,j)*R13Ccal*a_co2/(1.+R13Ccal*a_co2)


    RQ13C=CORAL(ng)%Q13C(n,i,j)/CORAL(ng)%QC(n,i,j)   !coral organism
    R13Ccoe=CORAL(ng)%DI13Ccoe(n,i,j)/CORAL(ng)%DICcoe(n,i,j)   !DIC in coelenteron
    R13Ccal=CORAL(ng)%DI13Ccal(n,i,j)/CORAL(ng)%DICcal(n,i,j)   !DIC in calcifying fluid
    R13Camb=DI13Camb/DICamb!!!!!  DIC in ambient seawater
    
    R13C_HCO3cal=cH13CO3cal/cHCO3cal
      
! Carbon isotope fluxes
    F_13CO2=k_CO2i(n)/1.0007d0*(CORAL(ng)%c13CO2aqcoe(n,i,j)-CORAL(ng)%c13CO2aqcal(n,i,j))*rho_sw
    DI13Cuptake=Si_DI13C *(DI13Camb-CORAL(ng)%DI13Ccoe(n,i,j))*rho_sw
    Fpp_DI13C=k_DIC(n)*(CORAL(ng)%DI13Ccoe(n,i,j)-CORAL(ng)%DI13Ccal(n,i,j))*rho_sw
                    
# if defined CORAL_NONE_CO2_EQ
! Carbon isotope fluxes
    c13CO2aquptake=Si_13CO2 *(c13CO2aqamb-CORAL(ng)%c13CO2aqcoe(n,i,j))*rho_sw
!    Fpp_13CO2aq=k_DIC(n)*(CORAL(ng)%c13CO2aqcoe(n,i,j)-CORAL(ng)%c13CO2aqcal(n,i,j))*rho_sw
    Fpp_13CO2aq=k_DIC(n)/1.00087d0*(CORAL(ng)%c13CO2aqcoe(n,i,j)-CORAL(ng)%c13CO2aqcal(n,i,j))*rho_sw
                ! ratio of diffusion coefficent: 1.00087 by Jahne et al 1987 
# endif

#endif /* End isotope calculation */

!------------ internal condition change -----------------------------

!----- Photosynthetic rate (nmol cm-2 s-1) ------------------------------------
    CORAL(ng)%Pg(n,i,j) = Pgc

!----- Respiration rate (nmol cm-2 s-1) ------------------------------------
    CORAL(ng)%R(n,i,j) = Rc

!----- Calcification rate (nmol cm-2 s-1) ------------------------------------    
    if (Wargcal .ge. 1.d0) then
      CORAL(ng)%G(n,i,j)=1.1d-3*(Wargcal-1.d0)**1.63d0
    else
      CORAL(ng)%G(n,i,j)=-2.7d-2*(1.d0-Wargcal)**2.5d0
    endif

!   CORAL(ng)%QC (umol cm-2)
!   Pg,R: nmol cm-2 s-1
    CORAL(ng)%QC (n,i,j)=CORAL(ng)%QC (n,i,j)+(              &
#if defined CORAL_ZOOXANTHELLAE
             F_Csec-Rc                                       &
#else
             CORAL(ng)%Pg(n,i,j)-CORAL(ng)%R (n,i,j)         &
#endif
#if defined CORAL_MUCUS
             -F_Cmucus                                       &
#endif
#if defined CORAL_INGESTION
             +F_Cingest                                      &
#endif
#if defined CORAL_SIZE_DYNAMICS
             -F_Cgrowth                                      &
#endif
             )*1.d-3 *dt    !nmol cm-2 =1.e-3 umol cm-2
    CORAL(ng)%QC(n,i,j)=max(CORAL(ng)%QC(n,i,j),1.0d-1) !Error handring

!    DIC & TA (umol kg-1) in calcified fulid
    CORAL(ng)%DICcal (n,i,j)=CORAL(ng)%DICcal (n,i,j)              &
                +(-CORAL(ng)%G(n,i,j)                              &
                  +CORAL(ng)%R(n,i,j)*(1.-ratio(n))                & !!!!Test: add R*(1.-ratio(n))
                  +F_CO2                                           &
                  +Fpp_DIC                                         &
                 )/hcal/rho_sw *dt    !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
    CORAL(ng)%DICcal(n,i,j)=max(CORAL(ng)%DICcal(n,i,j),0.d0) !Error handring
    CORAL(ng)%DICcal(n,i,j)=min(CORAL(ng)%DICcal(n,i,j),100000.d0) !Error handring

    CORAL(ng)%TAcal (n,i,j)=CORAL(ng)%TAcal (n,i,j)               &
         +(-2.d0*CORAL(ng)%G (n,i,j)                              &
           +F_H                                                   &
           +Fpp_TA                                                &
          )/hcal/rho_sw *dt    !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
    CORAL(ng)%TAcal(n,i,j)=max(CORAL(ng)%TAcal(n,i,j),0.d0) !Error handring
    CORAL(ng)%TAcal(n,i,j)=min(CORAL(ng)%TAcal(n,i,j),100000.d0) !Error handring


!    DIC & TA (umol kg-1) in coelectron
    CORAL(ng)%DICcoe (n,i,j)=CORAL(ng)%DICcoe (n,i,j)          &
                +(-CORAL(ng)%Pg(n,i,j)                         &
                  +CORAL(ng)%R (n,i,j)*ratio(n)                & !!!!Test: add  *ratio(n)
                  +DICuptake                                   &
                  -F_CO2                                       &
                  -Fpp_DIC                                     &
                 )/hcoe/rho_sw *dt   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
    CORAL(ng)%DICcoe(n,i,j)=max(CORAL(ng)%DICcoe(n,i,j),0.d0) !Error handring
    CORAL(ng)%DICcoe(n,i,j)=min(CORAL(ng)%DICcoe(n,i,j),100000.d0) !Error handring

    CORAL(ng)%TAcoe (n,i,j)=CORAL(ng)%TAcoe (n,i,j)             &
         +(-F_H                                                 &
            +TAuptake                                           &
            -Fpp_TA                                             &
          )/hcoe/rho_sw *dt   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
    CORAL(ng)%TAcoe(n,i,j)=max(CORAL(ng)%TAcoe(n,i,j),0.d0) !Error handring
    CORAL(ng)%TAcoe(n,i,j)=min(CORAL(ng)%TAcoe(n,i,j),100000.d0) !Error handring


!    DO (umol kg-1) in coelectron
    CORAL(ng)%DOcoe (n,i,j)=CORAL(ng)%DOcoe (n,i,j)             &
                +(CORAL(ng)%Pg(n,i,j)-CORAL(ng)%R (n,i,j)       &
                  +DOuptake                                     &
                 )/hcoe *dt   !nmol cm-3 s-1 = 1. umol L-1 s-1
    CORAL(ng)%DOcoe (n,i,j)=max( CORAL(ng)%DOcoe (n,i,j),0.d0) !Error handring

#if defined CORAL_NONE_CO2_EQ

    kp1=kp1_Johnson1982(TKamb, Samb)
    km1=km1_Johnson1982(TKamb, Samb)
    kp4=kp4_Johnson1982(TKamb, Samb)
    km4=km4_Johnson1982(TKamb, Samb)

    cHcoe =10.d0**(-pHcoe)
    cOHcoe=Kw_Millero1995(TKamb, Samb)/cHcoe
    
    cHcal =10.d0**(-pHcal)
    cOHcal=Kw_Millero1995(TKamb, Samb)/cHcal

!    [CO2*] (umol kg-1) in coelectron
    CORAL(ng)%cCO2aqcoe(n,i,j)=CORAL(ng)%cCO2aqcoe (n,i,j)                &
                +( CORAL(ng)%R(n,i,j) *ratio(n)       & !!!!Test: add  *ratio(n)
                  +CO2aquptake                        &
                  -F_CO2                              &
                  -Fpp_CO2aq                          &
                 )/hcoe/rho_sw *dt                    &   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
!     kinetics of CO2 system
                +( (km1*cHcoe+km4)*cHCO3coe           & ! umol kg-1 s-1
                  -(kp1+kp4*cOHcoe)*CORAL(ng)%cCO2aqcoe(n,i,j)  & ! umol kg-1 s-1
                 ) *dt   ! umol kg-1

    if(CORAL(ng)%cCO2aqcoe(n,i,j)<0.0d0) then
      write(*,*) 'DEBUG1',CORAL(ng)%cCO2aqcoe(n,i,j)
      Stop
    endif
!    CORAL(ng)%cCO2aqcoe(n,i,j)=max(CORAL(ng)%cCO2aqcoe(n,i,j),0.d0) !Error handring
!    CORAL(ng)%cCO2aqcoe(n,i,j)=min(CORAL(ng)%cCO2aqcoe(n,i,j),1000.d0) !Error handring

!    [CO2*] (umol kg-1) in calcified fulid
    CORAL(ng)%cCO2aqcal(n,i,j)=CORAL(ng)%cCO2aqcal(n,i,j)                 &
                +(CORAL(ng)%R(n,i,j) *(1.-ratio(n))   & !!!!Test: add R*ratio(n)
                  +F_CO2                              &
                  +Fpp_CO2aq                          &
                 )/hcal/rho_sw *dt                    &!nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
!     kinetics of CO2 system
                +( (km1*cHcal+km4)*cHCO3cal           &! umol kg-1 s-1
                  -(kp1+kp4*cOHcal)*CORAL(ng)%cCO2aqcal(n,i,j)  &! umol kg-1 s-1
                 ) *dt   ! umol kg-1
    
    if(CORAL(ng)%cCO2aqcal(n,i,j)<0.0d0) then
      write(*,*) 'DEBUG2',CORAL(ng)%cCO2aqcal(n,i,j)
      Stop
    endif
#endif

#if defined CORAL_ZOOXANTHELLAE
!!!
!!! ----- Zooxanthellae interaction ----------------------------------------------------
!!!
!    ROS (umol/L) mass balance

    CORAL(ng)%ROS(n,i,j)=CORAL(ng)%ROS(n,i,j)        &
             +( F_zROS                               &
               +F_cROS                               &
               -F_detox                              &
               -F_dam                                &
!               -F_ONOO                               &
!               -Si_ROS * CORAL(ng)%ROS(n,i,j)        &
              )/htis *dt            !umol L-1
      CORAL(ng)%ROS(n,i,j)=max(CORAL(ng)%ROS(n,i,j),0.d0) !Error handring

#endif

#if defined CORAL_CARBON_ISOTOPE
!!!
!!! ----- Isotope calculation ----------------------------------------------------
!!!
!      a_calc=exp((3.-0.140*Gn/0.011098)/1000.)    ! kinetic effect calcualtion: 1 mm y-1 
                   ! -> (10cm*1.4g/cm) /(365*24*60*60s*40g/mol)* 10**6 = 0.011098 umol cm-2 s-1

    CORAL(ng)%Q13C (n,i,j)=CORAL(ng)%Q13C (n,i,j)+(                 &
!             +(Pg*R13Ccoe*a_phot/(1.+R13Ccoe*a_phot)  &
!               -R*RQ13C*a_resp/(1.+RQ13C*a_resp)  &
                CORAL(ng)%Pg(n,i,j)*R13Ccoe*a_phot                  &
               -CORAL(ng)%R (n,i,j)*RQ13C*a_resp                    &
!              )/hcoe/rho_sw *dt    !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
#if defined CORAL_MUCUS
               -F_Cmucus*RQ13C                                      &
#endif
              )*1.e-3 *dt    !nmol cm-2 =1.e-3 umol cm-2

    CORAL(ng)%DI13Ccoe (n,i,j)=CORAL(ng)%DI13Ccoe (n,i,j)        &
!                +(-Pg*R13Ccoe*a_phot/(1.+R13Ccoe*a_phot)  &
!                  +R*RQ13C*a_resp/(1.+RQ13C*a_resp)   &
                +(-CORAL(ng)%Pg(n,i,j)*R13Ccoe*a_phot            &
                  +CORAL(ng)%R (n,i,j)*RQ13C*a_resp *ratio(n)    & !!!!Test: add  *ratio(n)
                  +DI13Cuptake                                   &
                  -F_13CO2                                       &
                  -Fpp_DI13C                                     &
                 )/hcoe/rho_sw *dt   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1

    CORAL(ng)%DI13Ccal (n,i,j)=CORAL(ng)%DI13Ccal (n,i,j)          &
!                +(-Gn*R13Ccal*a_calc/(1.+R13Ccal*a_calc)   &
!                +(-Gn*R13Ccal*a_calc                       &
                +(-CORAL(ng)%G (n,i,j)*R13C_HCO3cal*a_calc         &
                  +CORAL(ng)%R (n,i,j)*RQ13C*a_resp *(1.-ratio(n)) & !!!!Test: add R*(1.-ratio(n))
                  +F_13CO2                                         &
                  +Fpp_DI13C                                       &
                 )/hcal/rho_sw *dt    !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1


# if defined CORAL_NONE_CO2_EQ

    kp1=kp1/1.013d0
    km1=km1/1.022d0
    kp4=kp4/1.011d0
    km4=km4/1.027d0

!    cHcoe =10.d0**(-pHcoe)
!    cOHcoe=Kw_Millero1995(TKamb, Samb)/cHcoe
!    
!    cHcal =10.d0**(-pHcal)
!    cOHcal=Kw_Millero1995(TKamb, Samb)/cHcal

!    [13CO2*] (umol kg-1) in coelectron
    CORAL(ng)%c13CO2aqcoe(n,i,j)=CORAL(ng)%c13CO2aqcoe (n,i,j)                &
                +( CORAL(ng)%R(n,i,j)*RQ13C*a_resp*ratio(n)        &!!!!Test: add *ratio(n)
                  +c13CO2aquptake                                  &
                  -F_13CO2                                         &
                  -Fpp_13CO2aq                                     &
                 )/hcoe/rho_sw *dt                                 &   !nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
!     kinetics of CO2 system
                +( (km1*cHcoe+km4)*cH13CO3coe                      & ! umol kg-1 s-1
                  -(kp1+kp4*cOHcoe)*CORAL(ng)%c13CO2aqcoe(n,i,j)   & ! umol kg-1 s-1
                 ) *dt   ! umol kg-1

!    CORAL(ng)%cCO2aqcoe(n,i,j)=max(CORAL(ng)%cCO2aqcoe(n,i,j),0.d0) !Error handring
!    CORAL(ng)%cCO2aqcoe(n,i,j)=min(CORAL(ng)%cCO2aqcoe(n,i,j),1000.d0) !Error handring

!    [13CO2*] (umol kg-1) in calcified fulid
    CORAL(ng)%c13CO2aqcal(n,i,j)=CORAL(ng)%c13CO2aqcal(n,i,j)                 &
                +( CORAL(ng)%R (n,i,j)*RQ13C*a_resp*(1.-ratio(n))  &!!!!Test: add R*(1.-ratio(n))
                  +F_13CO2                                         &
                  +Fpp_13CO2aq                                     &
                 )/hcal/rho_sw *dt                                 &!nmol cm-3 s-1 = 1./1.023 umol kg-1 s-1
!     kinetics of CO2 system
                +( (km1*cHcal+km4)*cH13CO3cal                      &! umol kg-1 s-1
                  -(kp1+kp4*cOHcal)*CORAL(ng)%c13CO2aqcal(n,i,j)   &! umol kg-1 s-1
                 ) *dt   ! umol kg-1
!    CORAL(ng)%cCO2aqcal(n,i,j)=max(CORAL(ng)%cCO2aqcal(n,i,j),0.d0) !Error handring
!    CORAL(ng)%cCO2aqcal(n,i,j)=min(CORAL(ng)%cCO2aqcal(n,i,j),1000.d0) !Error handring

!    write(60,*) CORAL(ng)%cCO2aqcal(n,i,j),F_CO2,Fpp_CO2aq,(km1*cHcal+km4)*cHCO3cal,(kp1+kp4*cOHcal)*CORAL(ng)%cCO2aqcal(n,i,j) !!!!!!!!!for Debug

# endif

!------ Calculation of d13C -------------------------
      
    d13C_DICamb = d13C_fromR13C( R13Camb )
    d13C_DICcoe = d13C_fromR13C( R13Ccoe )
    d13C_QC     = d13C_fromR13C( RQ13C   )
    d13C_DICcal = d13C_fromR13C( R13Ccal )

!    R13Carg=R13Ccal*a_calc
    R13Carg     = R13C_HCO3cal*a_calc
    d13C_arg    = d13C_fromR13C( R13Carg )

#endif  /* End isotope calculation */



#if defined CORAL_TESTMODE
!------------------------------------------------------------------------
! Print section

# if defined CORAL_BORON_ISOTOPE
    d11Barg = d11B_BOH4_frompHd11BT(pHcal, 39.5d0, TKamb, Samb)
# endif

    time(n) = time(n) +dt  ! sec
    
    if(time(n).ge.dsec(n)) then
      
      write(10+n,*) time(n)/86400.0d0,',',PFD,','                     &
       ,CORAL(ng)%Pg(n,i,j),',', CORAL(ng)%R (n,i,j),','              &
       ,CORAL(ng)%Pg(n,i,j)-CORAL(ng)%R (n,i,j),','                   &
       ,CORAL(ng)%G (n,i,j),',',CORAL(ng)%QC(n,i,j),','               &
       ,CORAL(ng)%TAcal(n,i,j),',',CORAL(ng)%TAcoe(n,i,j),',',TAamb,','     &
       ,CORAL(ng)%DICcal(n,i,j),',',CORAL(ng)%DICcoe(n,i,j),',',DICamb,','  &
       ,CORAL(ng)%DOcoe(n,i,j),',',DOamb,','                          &
       ,pHcal,',',pHcoe,',',pHamb,',',Wargcal,',',Wargamb,','         &
       ,fCO2cal,',',fCO2coe,',',fCO2amb,','                           &
       ,CORAL(ng)%cCO2aqcal(n,i,j),',',cHCO3cal,',',cCO3cal,','       &
       ,CORAL(ng)%cCO2aqcoe(n,i,j),',',cHCO3coe,',',cCO3coe,','       &
# if defined CORAL_CARBON_ISOTOPE
       ,d13C_DICamb,',',d13C_DICcoe,',',d13C_QC,',',d13C_DICcal,','   &
       ,d13C_arg,',',d13C_arg*CORAL(ng)%G (n,i,j),','                                  &
       ,d13C_fromR13C(CORAL(ng)%c13CO2aqcal(n,i,j)/CORAL(ng)%cCO2aqcal(n,i,j)),','   &
       ,d13C_fromR13C(cH13CO3cal/cHCO3cal),','                        &
       ,d13C_fromR13C(c13CO3cal/cCO3cal),','                          &
       ,d13C_fromR13C(CORAL(ng)%c13CO2aqcoe(n,i,j)/CORAL(ng)%cCO2aqcoe(n,i,j)),','   &
       ,d13C_fromR13C(cH13CO3coe/cHCO3coe),','                        &
       ,d13C_fromR13C(c13CO3coe/cCO3coe),','                          &
       ,CORAL(ng)%c13CO2aqcal(n,i,j),',',cH13CO3cal,',',c13CO3cal,',' &
       ,CORAL(ng)%c13CO2aqcoe(n,i,j),',',cH13CO3coe,',',c13CO3coe,',' &
# endif
# if defined CORAL_ZOOXANTHELLAE
       ,CORAL(ng)%ROS(n,i,j),',',F_zROS,',',F_detox,',',F_dam,','      &
# endif
# if defined CORAL_BORON_ISOTOPE
       ,d11Barg,','                                                   &
# endif
# if defined ORGANIC_MATTER
       ,DOCuptake,','                                                 &
# endif
# if defined CORAL_INGESTION
       ,ZOOuptake,','                                                 &
# endif
# if defined CORAL_SIZE_DYNAMICS
       ,CORAL(ng)%growth(n,i,j),',', CORAL(ng)%mort(n,i,j),','        &
       , Damage,',',F_Cgrowth,','                                     &
# endif
       ,E_ca
    
      dsec(n)=dsec(n)+OUTPUT_INTERVAL*60.

    endif

! Coral record calculation section

    S_PFD_dt(n)       =S_PFD_dt(n)+PFD*dt
    S_Gn_dt(n)        =S_Gn_dt(n)+CORAL(ng)%G (n,i,j)*dt
    S_Pg_dt(n)        =S_Pg_dt(n)+CORAL(ng)%Pg(n,i,j)*dt
    S_R_dt(n)         =S_R_dt(n) +CORAL(ng)%R(n,i,j) *dt
    S_QC_dt(n)        =S_QC_dt(n)+CORAL(ng)%QC(n,i,j)*dt
# if defined CORAL_CARBON_ISOTOPE
    S_d13CargxGn_dt(n)=S_d13CargxGn_dt(n)+d13C_arg*CORAL(ng)%G (n,i,j)*dt
    S_d13C_QC_dt(n)   =S_d13C_QC_dt(n)+d13C_QC*dt
# endif
# if defined CORAL_BORON_ISOTOPE
    S_d11BargxGn_dt(n)=S_d11BargxGn_dt(n)+d11Barg*CORAL(ng)%G (n,i,j)*dt
# endif

    if((time(n)/86400.).ge.dday(n)) then

      write(20+n,*) dday(n),','             &
       ,S_PFD_dt(n)*1.d-6,','               &   !Photon flux density (mol m-2 d-1)
# if defined CORAL_CARBON_ISOTOPE
!       ,S_d13CargxGn_dt(n),','              &
       ,S_d13CargxGn_dt(n)/S_Gn_dt(n),','   &   !d13C
       ,S_d13C_QC_dt(n)/24./60./60.,','     &   ! 1 day avaraged value of d13C_QC
# endif
# if defined CORAL_BORON_ISOTOPE
        ,S_d11BargxGn_dt(n)/S_Gn_dt(n),','   & 
# endif
       ,S_Gn_dt(n)*1.d-3,','                &   !Calcification rate (umol cm-2 d-1)
       ,S_Pg_dt(n)*1.d-3,','                &   !Gross photosynthesis rate (umol cm-2 d-1)
       ,S_R_dt(n)*1.d-3,','                 &   !Respiration rate (umol cm-2 d-1)
       ,S_QC_dt(n)/24./60./60.,','          &   ! 1 day avaraged value of QC
       ,(S_Pg_dt(n)-S_R_dt(n))*1.d-3            !Net photosynthesis rate (umol cm-2 d-1)

      dday(n)=dday(n)+AVERAGE_INTERVAL
        
      S_PFD_dt(n)=0.d0
      S_Gn_dt(n)=0.d0
      S_Pg_dt(n)=0.d0
      S_R_dt(n)=0.d0
      S_QC_dt(n)=0.d0
# if defined CORAL_CARBON_ISOTOPE
      S_d13CargxGn_dt(n)=0.d0
      S_d13C_QC_dt(n)=0.d0
# endif
# if defined CORAL_BORON_ISOTOPE
      S_d11BargxGn_dt(n)=0.d0
# endif
    endif
#endif

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


#if defined CORAL_ZOOXANTHELLAE

!!! **********************************************************************
!!!  Zooxanthellae population dynamics
!!! **********************************************************************

  SUBROUTINE zooxanthellae &
!   input parameters
    ( ng, n, i, j  & ! ng: nested grid number; n: coral compartment; i,j: position
    , dt           & ! Time step (sec)
    , PFDsurf      & ! Photon flux density (umol m-2 s-1)
    , Tamb         & ! Temperature (oC)
    , cCO2aqcoe    & ! CO2* concentration in the coral tissue (umol kg-1)
    , cHCO3coe     & ! HCO3- concentration in the coral tissue (umol kg-1)
    , F_Zelm       & ! Zooxanthellae release rate by host coral (cell cm-2 s-1)
    , ROS          & ! ROS concentration in the coral tissue (umol L-1)
!   output parameters
    , F_Csec       & ! Organic C excretion controled by host coral (pmol cell-1 s-1)
    , F_zROS       & ! Flux of Reactive Oxygen Species  (pmol cell-1 s-1)
# if defined CORAL_NUTRIENTS
    , F_Nsec       & ! Organic N excretion controled by host coral (pmol cell-1 s-1)
    , F_Psec       & ! Organic P excretion controled by host coral (pmol cell-1 s-1)
    , NO3_trans    & ! NO3 transport rate from host to Zooxanthellae (pmol cell-1 s-1)
!    , NO2_trans    & ! NO2 transport rate from host to Zooxanthellae (pmol cell-1 s-1)
    , NH4_trans    & ! NH4 transport rate from host to Zooxanthellae (pmol cell-1 s-1)
    , PO4_trans    & ! PO4 transport rate from host to Zooxanthellae (pmol cell-1 s-1)
    , F_ONOO       & ! Flux of peroxynitrite  (pmol cell-1 s-1)
# endif
    , F_Zmort      & ! Dead zooxanthellae release rate (cells cm-2 s-1)
    )
!-----------------------------------------------------------------------

    implicit none

    integer, intent(in) :: ng, n, i, j
    real(8), intent(in) :: dt
    real(8), intent(in) :: PFDsurf
    real(8), intent(in) :: Tamb
    real(8), intent(in) :: cCO2aqcoe, cHCO3coe
    real(8), intent(in) :: F_Zelm
    real(8), intent(in) :: ROS

!    real(8), intent(out) :: Pg
!    real(8), intent(out) :: Rz
    real(8), intent(out) :: F_Csec
    real(8), intent(out) :: F_zROS
# if defined CORAL_NUTRIENTS
    real(8), intent(out) :: F_Nsec
    real(8), intent(out) :: F_Psec
    real(8), intent(out) :: NO3_trans, NH4_trans  !, NO2_trans
    real(8), intent(out) :: PO4_trans
    real(8), intent(out) :: F_ONOO
# endif
    real(8), intent(out) :: F_Zmort

!------------Set Constants  ----------------------------------

!    real(8), parameter :: Pgmax(Ncl) = 2.1d-4 ! ~0.76 (pmol cell-1 h-1) =~1.0d-4 (pmol cell-1 s-1)
    real(8), parameter :: Pgmax(Ncl) = 4.0d-4 ! Maximum photosynthetic rate (pmol cell-1 s-1) !!! Tuning
!    real(8), parameter :: Ik(Ncl)    = 42.0d0    ! 210 (umol m-2 s-1) ~210 Suggett et al. (2008); 42 Goreau et al. (1996)
    real(8), parameter :: Ik(Ncl)    = 210.0d0    ! 210 (umol m-2 s-1) ~210 Suggett et al. (2008); 42 Goreau et al. (1996)

!------ Goreau et al. (1996)------------------------------------------   
!                    Coral    FIZ    CZ
!          K_HCO3   408.    71.     178.   uM
!          V_HCO3     1.45   1.09    13.   nmol O2 (ug Chl a)-1 min-1
!          K_CO2aq    5.     0.67     2.7  uM
!          V_CO2aq    2.     1.15    12.5  nmol O2 (ug Chl a)-1 min-1
!---------------------------------------------------------------------
    real(8), parameter :: K_HCO3(Ncl) = 71.d0   !uM Goreau et al. (1996)
!    real(8), parameter :: V_HCO3(Ncl) = 1.45d0  !nmol O2 (ug Chl a)-1 min-1
!    real(8), parameter :: K_CO2aq(Ncl)= 5.d0 !/5./  !uM
!    real(8), parameter :: V_CO2aq(Ncl)= 2.d0 !/2./  !nmol O2 (ug Chl a)-1 min-1

!   Temperature dependency parameters (Hikosaka et al. 2006) 
!      - Parameters estimated from data by Al-Horani (2005)

!    real(8), parameter :: Rgas = 8.314d0 ! Gas constant (J mol-1)
!    real(8), parameter :: Hact(Ncl)   = 1.855d5 !Activation energy (J mol-1)) *tuned
!    real(8), parameter :: Hdeact(Ncl) = 3.055d5 !Deactivation energy (J mol-1) *tuned
!    real(8), parameter :: Entr(Ncl)   = 1028.d0   !Entropy term (J K-1) *tuned

!----- Respitration constants ---------------------------

!    real(8), parameter :: Rzmax = 0.0d0  !!! 0.2d-4 ! Maximum respiration rate (pmol cell-1 s-1)
    real(8), parameter :: Rzmax = 3.5d-5 ! Maximum respiration rate (pmol cell-1 s-1)
                                       ! ~0.1 (pmol cell-1 h-1) = 2.8d-5 (pmol cell-1 s-1) (Suggett et al., 2008)
    real(8), parameter :: K_QC = 8.d0  ! (pmol cell-1)
    real(8), parameter :: K_DO = 5.d0  ! ca. 10 (umol kg-1) Shick et al. (1990) *Tuned value
    real(8), parameter :: Rz_m  = 2.5d-5  ! Maintenance respiration rate (pmol cell-1 s-1)

!----- Zooxanthellar physica condition ---------------------------

!    real(8), parameter :: V_zoox = 5.0d-10   ! Volume of zooxanthellae (cm3/cells) ~300 um3 (Fujise et al. 2018)
                             ! ~500 um3 =5.e-10 cm3 (e.g. Jone 1997; Jones and Yellowlees 1997)

    real(8), parameter :: QC0 = 50.0d0  ! (pmolC/cell)  100 pg cell-1 ~ 10 pmol cell-1 = (Verde & McCloskey 1998)
# if defined CORAL_NUTRIENTS
    real(8), parameter :: QN0 = 1.1d0  ! (pmolN/cell)  calculated from C:N:P = 116:16:1
    real(8), parameter :: QP0 = 0.07d0 ! (pmolP/cell) 
# endif

# if defined CORAL_NUTRIENTS
!----- Nutrients related parameters -------------------
    real(8), parameter :: Vmax_NO3tra = 1.67d-6 ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ� 
!    real(8), parameter :: Vmax_NO2tra = 1.5d-7  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�
    real(8), parameter :: Vmax_NH4tra = 1.0d-5  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�
    real(8), parameter :: Vmax_PO4tra = 3.0d-7  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�

    real(8), parameter :: K_NO3tra = 50.d0  ! (umol kg-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�
!    real(8), parameter :: K_NO2tra = 10.d0  ! (umol kg-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�
    real(8), parameter :: K_NH4tra = 300.d0 ! (umol kg-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�      
    real(8), parameter :: K_PO4tra = 300.d0 ! (umol kg-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�      

    real(8), parameter :: k_NH4rel = 1.0d-5  !(s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�
    real(8), parameter :: k_NO3rel = 1.9d-8  !(s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�
!    real(8), parameter :: k_NO2rel = 1.0d-5  !(s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�
    real(8), parameter :: k_PO4rel = 1.0d-5  !(s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�

    real(8), parameter :: Vmax_NO3red = 1.3d-6  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ� 
!    real(8), parameter :: Vmax_NO2red = 1.3d-6  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�

    real(8), parameter :: K_NO3red = 0.014d0  !(pmol cell-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�
!    real(8), parameter :: K_NO2red = 0.0015d0 !(pmol cell-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�

    real(8), parameter :: Vmax_Nass = 2.d-5  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ� 
    real(8), parameter :: K_Nass = 1.d-1  !(pmol cell-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�

    real(8), parameter :: Vmax_Pass = 5.d-7  ! (pmol cell-1 s-1) !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ� 
    real(8), parameter :: K_Pass = 1.d-2  !(pmol cell-1)  !!!!!!!!!!!!!!!!!!!!!!!!!!�Ă��Ƃ�
# endif

!----- Population dynamics parameters -------------------

    real(8), parameter :: u_max  = 5.0d-5 ! Maximum reproduction rate (pmol cell-1 s-1)       !!! Tuning
    real(8), parameter :: rg2c   = 2.0d0  ! Conversion factor from growth respiration to reproduction (no dimension)
    real(8), parameter :: kC_sec = 2.0d-6 ! Mass transfer velocity of secreted organic matter (s-1) !!! Tuning

    real(8), parameter :: km_CNP = 1.d-8 ! (cell cm-2 s-1)  �K��!!!!!!!!!!!!!!
    real(8), parameter :: am_CNP = 1.d0
    real(8), parameter :: km_ROS = 1.d-11 !                  �K��!!!!!!!!!!!!!!
    real(8), parameter :: km_min = 1.d-14   ! (cell cm-2 s-1)  �K��!!!!!!!!!!!!!!
    real(8), parameter :: km = 1.d-14   ! (cell cm-2 s-1)  �K��!!!!!!!!!!!!!!

!----- for gross photosynthesi -------------------
!    Temperature related factor
    real(8) :: f_temp

!----- Respiration parameters -------------------
    real(8) :: SQC
    real(8) :: Rz   ! Potential respiration rate
    real(8) :: Rz_g ! Growth respiration rate

# if defined CORAL_NUTRIENTS
!----- Nutrients related parameters -------------------
    real(8) :: N_dissim       ! Nitrogen dissimilation rate (pmol cell-1 s-1)
    real(8) :: N_assim        ! Nitrogen assimilation rate (pmol cell-1 s-1)
!    real(8) :: P_dissim       ! P dissimilation rate (pmol cell-1 s-1)
    real(8) :: P_assim        ! P assimilation rate (pmol cell-1 s-1)
    real(8) :: NO3_reduc      ! NO3 reductance rate (pmol cell-1 s-1)
!    real(8) :: NO2_reduc      ! NO2 reductance rate (pmol cell-1 s-1)
# endif
                                         
!----- Metabolic parameters -------------------
    real(8) ::  C_repro
# if defined CORAL_NUTRIENTS
    real(8) ::  N_repro
    real(8) ::  P_repro
# endif

!----- Population dynamics parameters -------------------
    real(8) ::  Morta
    real(8) ::  Repro, Death

# if defined CORAL_TESTMODE
!  Output
    real(8), parameter :: OUTPUT_INTERVAL  = 5.0d0    ! Output interval (min)
    real(8), save :: time(Ncl)         = (/ 0.d0, 0.d0 /) !sec
    real(8), save :: dsec(Ncl)         = (/ 0.d0, 0.d0 /) !sec
    real(8), save :: dday(Ncl)         = (/ 1.d0, 1.d0 /) !day
# endif

!----- Zooxanthellae metabolism  ----------------------------------

# if defined CORAL_PHOTOINHIBITION
    CALL photosystem         &
!        input parameters
      ( ng, n, i, j          & ! ng: nested grid number; n: coral compartment; i,j: position
      , dt                   & ! Time step (sec)
      , PFDsurf              & ! Photon flux density (umol m-2 s-1)
      , Tamb                 & ! Temperature (K)
      , cCO2aqcoe            & ! CO2 concentration in the coral tissue
      , ROS                  & ! ROS concentration in Zooxanthellar cell
!        output parameters
      , ZOOX(ng)%Pg(n,i,j)   & ! Gross photosynthetic rate (pmol cell-1 s-1)
      , F_zROS               & ! Flux of Reactive Oxygen Species (pmol cell-1 s-1)
      )
# else

!----- Gross photosynthesis rate (pmol cell-1 s-1) -----------------
!   Temperature dependent term
    f_temp=1.
!    f_temp=exp(Hact(n)*(TKamb-298.)/298./Rgas/TKamb)      &
!           *(1.+exp((298.*Entr(n)-Hdeact(n))/298./Rgas))  &
!           /(1.+exp((TKamb*Entr(n)-Hdeact(n))/TKamb/Rgas))

!   Gross photosynthesis rate (pmol cell-1 s-1)
!    ZOOX(ng)%Pg(n,i,j)=Pgmax(n)*f_temp*tanh(PFDsurf/Ik(n))           &
!                       * cHCO3coe/(K_HCO3(n)+cHCO3coe)
    ZOOX(ng)%Pg(n,i,j)=Pgmax(n)*f_temp*(1.0d0 - exp(-PFDsurf/Ik(n)))  &
                       * cHCO3coe/(K_HCO3(n)+cHCO3coe)

!----- ROS flux  (pmol cell-1 s-1) ------------------------------------

!  Estimated from Saragosti et al. (2010) data
!    F_zROS = 0.0072d0*exp(0.14d0*(TKamb-273.15))+0.0024d0*(PFDsurf) ! (fmol min-1 cell-1)
!  Estimated from combined data of Saragosti et al. (2010) and McGinty et al. (2012)
    F_zROS = 6.0d-7*exp(0.5d0*(TKamb-273.15))+0.0024d0*(PFDsurf) ! (fmol min-1 cell-1)
    F_zROS = F_zROS/60.0d0 * 1.0d-3   ! (pmol cell-1 s-1)
# endif
      
!----- peroxynitrite (ONOO-) flux (nmol cm-2 s-1)------------------------------------
!
!    F_ONOO=1.e0 * ZOOX(ng)%NO2(n,i,j)*CORAL(ng)%ROS(n,i,j)   ! (pmol cell-1 s-1)!!!!!!!!!!!!!!!!!!�Ă��Ƃ�
!    F_ONOO=F_ONOO*ZOOX(ng)%dens(n,i,j)*1.d-3   ! (nmol cm-2 s-1)

!----- Respilation rate (pmol cell-1 s-1) ---------------------------
!    Respilation rate ~0.1 (pmol cell-1 h-1) (Suggett et al., 2008)
!
    Rz = Rzmax * ZOOX(ng)%QC(n,i,j)/(K_QC+ZOOX(ng)%QC(n,i,j))         & ! Respilation rate (pmol cell-1 s-1)
               * CORAL(ng)%DOcoe(n,i,j)/(K_DO+CORAL(ng)%DOcoe(n,i,j))

!    Rz = 0.1d0/3600.0d0  ! Respilation rate (pmol cell-1 s-1)  Suggett et al. (2008) ~0.1 (pmol cell-1 h-1)
!    Rz = Rzmax ! Respilation rate (pmol cell-1 s-1)  !!! assumed constant value !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# if defined CORAL_NUTRIENTS
!----- Nutrient internal dynamics -----------------------------------
!----- Nutrients transport rate (pmol cell-1 s-1) ---------------------

!  These formulations are basically followed to Flynn and Flynn (1998)
    NH4_trans =Vmax_NH4tra*CORAL(ng)%NH4(n,i,j)/(K_NH4tra+CORAL(ng)%NH4(n,i,j)) &
              -k_NH4rel * ZOOX(ng)%NH4(n,j,i)
    NO3_trans =Vmax_NO3tra*CORAL(ng)%NO3(n,i,j)/(K_NO3tra+CORAL(ng)%NO3(n,i,j)) &
                   *(1.-ZOOX(ng)%NH4(n,i,j)/0.1)                           &
              -k_NO3rel * ZOOX(ng)%NO3(n,j,i)
!    NO2_trans =Vmax_NO2tra*CORAL(ng)%NO2(n,i,j)/(K_NO2tra+CORAL(ng)%NO2(n,i,j)) &
!                   *(1.-ZOOX(ng)%NH4(n,i,j)/0.1)                           &
!              -k_NO2rel * ZOOX(ng)%NO2(n,j,i)
    PO4_trans =Vmax_PO4tra*PO4(n,i,j)/(K_PO4tra+CORAL(ng)%PO4(n,i,j))

!----- Nutrients reductance rate (pmol cell-1 s-1) ---------------------
    NO3_reduc = Vmax_NO3red*ZOOX(ng)%NO3(n,i,j)/(K_NO3red+ZOOX(ng)%NO3(n,i,j))
!    NO2_reduc = Vmax_NO2red*ZOOX(ng)%NO2(n,i,j)/(K_NO2red+ZOOX(ng)%NO2(n,i,j))

!----- Nitrogen assimilation rate (pmol cell-1 s-1) ---------------------
    N_assim = Vmax_Nass* ZOOX(ng)%NH4(n,i,j)/(K_Nass+ZOOX(ng)%NH4(n,i,j)) 

!----- P assimilation rate (pmol cell-1 s-1) ---------------------
    P_assim = Vmax_Pass* ZOOX(ng)%PO4(n,i,j)/(K_Pass+ZOOX(ng)%PO4(n,i,j)) 

!----- Nitrogen dissimilation rate (pmol cell-1 s-1) ---------------------
    N_dissim = Rz * ZOOX(ng)%QN(n,i,j)/ZOOX(ng)%QC(n,i,j)   ! Dissimilation rate (pmol cell-1 s-1)

    F_Nsec = k_excre(n)*(                           &
                      QNzoox(n,i,j)/QN0zoox         &
                     -QNcoral(n,i,j)/QN0coral(n)    &
                     )*DENSzoox(n,i,j)
    
    F_Psec = k_excre(n)*(                           &
                      QPzoox(n,i,j)/QP0zoox         &
                     -QPcoral(n,i,j)/QP0coral(n)    &
                     )*DENSzoox(n,i,j)
# endif

!----- Reproduction rate (pmol cell-1 s-1) ----------------------------
# if defined CORAL_NUTRIENTS

    C_repro=u_max*min(    1.0d0-QP0/ZOOX(ng)%QP(n,i,j)  ,   &
                      min(1.0d0-QN0/ZOOX(ng)%QN(n,i,j),     &
                          1.0d0-QC0/ZOOX(ng)%QC(n,i,j) ) )
    C_repro = max(C_repro, 0.0d0)
    
    N_repro=C_repro*ZOOX(ng)%QN(n,i,j)/ZOOX(ng)%QC(n,i,j)
    P_repro=C_repro*ZOOX(ng)%QP(n,i,j)/ZOOX(ng)%QC(n,i,j)

# else
    C_repro = u_max*(1.0d0 - QC0/ZOOX(ng)%QC(n,i,j))            ! Droop (1973) model
!    C_repro = C_repro *((2.0d6 - ZOOX(ng)%dens(n,i,j))/2.0d6)       !!! Logistic reproduction
    C_repro = max(C_repro, 0.0d0)
    if (Rz > Rz_m) then
      C_repro = min(C_repro, (Rz-Rz_g)*rg2c)
      Rz_g = C_repro/rg2c
      ZOOX(ng)%R(n,i,j) = Rz_m + Rz_g
    else
      C_repro = 0.0d0
      Rz_g = 0.0d0
      ZOOX(ng)%R(n,i,j) = Rz
    endif
    
# endif

!----- Convert cell conditions to population dynamics parameters

    Repro = C_repro/ZOOX(ng)%QC(n,i,j)  ! Reproduction rate (s-1)

# if defined CORAL_NUTRIENTS

    Morta= km_CNP                                       & ! mortality (s-1) 
                *exp(-am_CNP*                           &             !* �����Ȃ��h�{�f�Ɉˑ��i�v����!!!�j
                    min(  1.-QP0/ZOOX(ng)%QP(n,i,j)  ,  &
                      min(1.-QN0/ZOOX(ng)%QN(n,i,j),    &
                          1.-QC0/ZOOX(ng)%QC(n,i,j) ) ) &
                 )
!              +km_ROS * CORAL(ng)%ROS(n,i,j)                                !* ROS�Z�x�ˑ��i�v����!!!�j  ���̂Ƃ����[���ɂ��Ă���
!              +km_min                                    !
# else
!    Morta = km_CNP * (1.0d0 - QC0/ZOOX(ng)%QC(n,i,j) )!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    Morta = 0.0d0!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    Morta = km * max( Rz_m-Rz, 0.0d0 )
# endif

!!!----- interaction between zooxanthellae and coral -----------------
!------ Extracellular secretion rate of organic carbon (pmol cell-1 s-1) -----

    F_Csec = kC_sec * (ZOOX(ng)%QC(n,i,j) - QC0)

!--------Time step progressing ------------------------


# if defined CORAL_NUTRIENTS

!---- Nutrients (pmol/cell) ------------------

    ZOOX(ng)%NO3(n,i,j)=ZOOX(ng)%NO3(n,i,j)  &
                  +(               &
                     NO3_trans     &
                    -NO3_reduc     &
                   )*dt
    ZOOX(ng)%NO3(n,i,j)=max(ZOOX(ng)%NO3(n,i,j),0.d0) !Error handring

!    ZOOX(ng)%NO2(n,i,j)=ZOOX(ng)%NO2(n,i,j)  &
!                  +(               &
!                     NO2_trans     &
!                    +NO3_reduc     &
!                    -NO2_reduc     &
!                   )*dt
!    ZOOX(ng)%NO2(n,i,j)=max(ZOOX(ng)%NO2(n,i,j),0.d0) !Error handring

    ZOOX(ng)%NH4(n,i,j)=ZOOX(ng)%NH4(n,i,j)  &
                  +(               &
                     NH4_trans     &
!                    +NO2_reduc     &
                    -N_assim       &
                    +N_dissim      &
                   )*dt
    ZOOX(ng)%NH4(n,i,j)=max(ZOOX(ng)%NH4(n,i,j),0.d0) !Error handring
    

    ZOOX(ng)%PO4(n,i,j)=ZOOX(ng)%PO4(n,i,j)  &
                  +(               &
                     PO4_trans     &
                    -P_assim       &
                   )*dt
    ZOOX(ng)%PO4(n,i,j)=max(ZOOX(ng)%PO4(n,i,j),0.d0) !Error handring
# endif

!-----Cell quota (pmol/cell) -----------------!!!!!�P�ʕϊ��`�F�b�N!!!!!!!!!!!!!!!!!!!!!!!!!!

    ZOOX(ng)%QC(n,i,j)=ZOOX(ng)%QC(n,i,j)                     &
                  +(                                          &
                     ZOOX(ng)%Pg(n,i,j)-ZOOX(ng)%R(n,i,j)     &
                    -F_Csec                                   &   !(nmol cm-2 s-1)/(cell cm-2)*1.e3=(pmol cell-1 s-1)
                    -C_repro                                  &
                   )*dt
    ZOOX(ng)%QC(n,i,j)=max(ZOOX(ng)%QC(n,i,j),1.0d-1) !!!Error handring

# if defined CORAL_NUTRIENTS
           
    ZOOX(ng)%QN(n,i,j)=ZOOX(ng)%QN(n,i,j)            &
                  +(                                 &
                     N_assim                         & 
                    -N_dissim                        &
!                    +NO3uptake/ZOOX(ng)%dens(n,i,j)*1.e3  &  !(nmol cm-2 s-1)/(cell cm-2)*1.e3=(pmol cell-1 s-1)
!                    +NH4uptake/ZOOX(ng)%dens(n,i,j)*1.e3  &  !(nmol cm-2 s-1)/(cell cm-2)*1.e3=(pmol cell-1 s-1)
                    -F_Nsec                          &  !(nmol cm-2 s-1)/(cell cm-2)*1.e3=(pmol cell-1 s-1)
                    -N_repro                         &
                   )*dt
    ZOOX(ng)%QN(n,i,j)=max(ZOOX(ng)%QN(n,i,j),0.d0) !Error handring     
      
    ZOOX(ng)%QP(n,i,j)=ZOOX(ng)%QP(n,i,j)                  &
                  +(                                       &
                     P_assim                               &
!                    +PO4uptake/ZOOX(ng)%dens(n,i,j)*1.e3   &  !(nmol cm-2 s-1)/(cell cm-2)*1.e3=(pmol cell-1 s-1)
                    -F_Psec                                &   !(nmol cm-2 s-1)/(cell cm-2)*1.e3=(pmol cell-1 s-1)
                    -P_repro                               &
                   )*dt
    ZOOX(ng)%QP(n,i,j)=max(ZOOX(ng)%QP(n,i,j),0.d0) !Error handring
# endif

!----- Dead zooxanthellae release rate (cell cm-2 s-1) -----------------
    F_Zmort = Morta*ZOOX(ng)%dens(n,i,j)

!----- Zooxanthellae density (cell cm-2) -----------------

!    ZOOX(ng)%dens ~ 1.e6 cells cm-2  (e.g. Higuchi et al., 2012)

    ZOOX(ng)%dens(n,i,j)=ZOOX(ng)%dens(n,i,j)    &
                 +(                              &
                    Repro * ZOOX(ng)%dens(n,i,j) & ! Reproduction
                   -F_Zmort                      & ! Death
                   -F_Zelm                       & ! Expulsion by host coral
                  )*dt
    ZOOX(ng)%dens(n,i,j)=max(ZOOX(ng)%dens(n,i,j),1.d0) !Error handring


!-----ROS concentration (pmol/cell) -----------------
!
!    ZOOX(ng)%ROS(n,i,j)=ZOOX(ng)%ROS(n,i,j)               &
!                  +(                                      &
!                     F_zROS                                &
!                    -F_zROS                                & !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                   )*dt
!    ZOOX(ng)%ROS(n,i,j)=max(ZOOX(ng)%ROS(n,i,j),1.0d-10) !!!Error handring
!
!------------------------------------------------------------------------
! Print section (for debug)

# if defined CORAL_TESTMODE

    time(n) = time(n) +dt  ! sec
    
    if(time(n).ge.dsec(n)) then
      
      write(30+n,*)                                        &
        time(n)/86400.0d0,',', PFDsurf,','                 &
       ,ZOOX(ng)%Pg(n,i,j),',',  ZOOX(ng)%R(n,i,j),','     &
       ,ZOOX(ng)%dens(n,i,j),',',ZOOX(ng)%QC(n,i,j),','    &
       ,CORAL(ng)%ROS(n,i,j),','                           &
       ,C_repro,',',Repro,',',Morta,',', F_zROS,','         &
       ,F_Csec,','                                         &
#  if defined CORAL_NUTRIENTS
       ,ZOOX(ng)%QN(n,i,j),',',ZOOX(ng)%QP(n,i,j),','      &
       ,N_repro,',',P_repro,','                            &
       ,ZOOX(ng)%NO3(n,i,j),','    &
       ,ZOOX(ng)%NH4(n,i,j),',',F_ONOO,','                 &
       ,NO3_trans,',',NH4_trans,','          &
       ,NO3_reduc,','                       &
       ,N_assim,',',N_dissim,','                           &
       ,ZOOX(ng)%PO4(n,i,j),',',PO4_trans,',',P_assim,','  &
#  endif
       ,F_Zelm,',',Repro*ZOOX(ng)%dens(n,i,j),','        &
       ,Morta*ZOOX(ng)%dens(n,i,j)

      dsec(n)=dsec(n)+OUTPUT_INTERVAL*60.0d0
    
    endif
# endif
!---------------------------------------------------------------------

    RETURN

  END SUBROUTINE zooxanthellae
#endif


#if defined CORAL_PHOTOINHIBITION

!!! **********************************************************************
!!!  Photoinihibition model (modified from Gustafsson et al. 2014)
!!! **********************************************************************

  SUBROUTINE photosystem  &
!   input parameters
    ( ng, n, i, j     & ! ng: nested grid number; n: coral compartment; i,j: position
    , dt              & ! Time step (sec)
    , PFDsurf         & ! Photon flux density (umol m-2 s-1)
    , Tamb            & ! Temperature (oC)
    , cCO2aqcoe       & ! CO2 concentration in the coral tissue
    , ROS             & ! ROS concentration in Zooxanthellar cell
!   output parameters
    , Pg              & ! Gross photosynthetic rate (pmol cell-1 s-1)
    , F_zROS          & ! Flux of Reactive Oxygen Species (pmol cell-1 s-1)
    )
!-----------------------------------------------------------------------

    implicit none

    integer, intent(in) :: ng, n, i, j
    real(8), intent(in) :: dt
    real(8), intent(in) :: PFDsurf
    real(8), intent(in) :: Tamb
    real(8), intent(in) :: cCO2aqcoe
    real(8), intent(in) :: ROS
!        output parameters
    real(8), intent(out) :: Pg
    real(8), intent(out) :: F_zROS

!------------Set Constants  ----------------------------------
    real(8), parameter :: PI  = 3.14159265359d0

    real(8), parameter :: QAt = 2.3d-6    ! Total RCII (pmol RCII cell-1) Sugget et al. (2008)
    real(8), parameter :: s_ab = 2.3d0    ! Absorption cross-section (m2 (umol photon)-1); ~390 (ANGSTROM quanta-1) Suggett et al. (2008) * 6.02x10^23 quanta = 1 mol photon
    real(8), parameter :: phi = 1.0d0     ! quantum efficiency of charge separation at PSII(e- photon-1)  Ross et al. (2008) !!!!!!!!!!!!!check
    real(8), parameter :: Trb = 33.8d0    ! (oC) estimated from Lilley et al. (2010)
    real(8), parameter :: arb = 0.55d0    ! (K-1) estimated from Lilley et al. (2010)
    real(8), parameter :: o2e = 0.25d0    ! Yield of O2 per electron (mol O2 (mol e-)-1) 2H2O -> 4e- +4H+ + O2 
    real(8), parameter :: o2c = 1.0d0     ! Photosynthetic quotient (mol O2 (mol CO2)-1) !!!Assumed
    real(8), parameter :: c2e = o2e/o2c   ! (mol C (mol e-)-1)
    real(8), parameter :: Pmax= 1.0d-4    ! Maximum photosynthetic rate (pmol cell-1 s-1) !!! Tuning
    real(8), parameter :: K_HCO3 = 71.0d0 ! Half saturation constant (uM) Goreau et al. (1996)
    real(8), parameter :: K_CO2 = 1.0d0   ! Half saturation constant (uM) !!! Tuning
    real(8), parameter :: e2ROS = 2.0d0   ! mol of e- required to make mol ROS (mol e- mol RO-1) O2 + 2e- + 2H+ -> H2O2 
    real(8), parameter :: f_RO = 1.0d0    ! ROS fraction from zoox. to host. !!!Assumed
    real(8), parameter :: ko = 1.04d3     ! Relaxation rate of QAr to QAo (s-1) : tau = 960 us; Suggett et al. (2008)
    real(8), parameter :: ki = 2.5d-7     ! Rate constant of photoinhibition (s-1 (umol m-2 s-1)-1)
                                          !      Tyystjarvi & Aro (1996): 6.11d-8 (s-1 (umol m-2 s-1)-1);
                                          !      Nishiyama et al. (2004): 1.7d-7  (s-1 (umol m-2 s-1)-1)
    real(8), parameter :: di = 1.7d-4     ! Natural decay from QAa to QAi (s-1)  !!!Assumed
    real(8), parameter :: ka = 4.0d-4     ! Repair rate of QAi to QAa (s-1) estimated from Takahashi et al. (2009)
    real(8), parameter :: Ti2a = 32.1d0   ! (oC) estimated from Takahashi et al. (2004) A. digitifera
    real(8), parameter :: ai2a = 0.806d0  ! (K-1) estimated from Takahashi et al. (2004) A. digitifera
!    real(8), parameter :: Ti2a = 35.6d0   ! (oC) estimated from Takahashi et al. (2004) Stylophora pistillata
!    real(8), parameter :: ai2a = 0.6d0    ! (K-1) estimated from Takahashi et al. (2004) Stylophora pistillata
    real(8), parameter :: ROSi2a = 30.0d0   ! (uM) estimated from Takahashi et al. (2004) A. digitifera
    real(8), parameter :: arosi2a = 1.0d0 ! (uM-1) estimated from Takahashi et al. (2004) A. digitifera
    real(8), parameter :: krep = 5.0d-6 ! (s-1) estimated from Takahashi et al. (2004) A. digitifera
    real(8), parameter :: kdam = 1.0d-6 ! (uM-1 s-1) estimated from Takahashi et al. (2004) A. digitifera

!---------------------------------------------------------------------
    real(8) :: QAa       ! Active RCII (pmol RCII cell-1)
    real(8) :: RubisCO   ! Active fraction of RubisCO
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

# if defined CORAL_TESTMODE
!  Output
    real(8), parameter :: OUTPUT_INTERVAL  = 5.0d0    ! Output interval (min)
    real(8), save :: time(Ncl)         = (/ 0.d0, 0.d0 /) !sec
    real(8), save :: dsec(Ncl)         = (/ 0.d0, 0.d0 /) !sec
    real(8), save :: dday(Ncl)         = (/ 1.d0, 1.d0 /) !day
# endif

!---------------------------------------------------------------------   
    QAa = ZPHOT(ng)%QAo(n,i,j)+ZPHOT(ng)%QAr(n,i,j)
      
!---- Active fraction of RubisCO
    RubisCO = 1.0d0/(1.0d0+exp(-arb*(Trb-Tamb)))
    RubisCO = RubisCO * cCO2aqcoe/(K_CO2+cCO2aqcoe)
      
!---- electron absorption rate (s-1) 
!   = reduction of active QA (s-1) ---
    kr = s_ab * phi * PFDsurf
      
!---- electrons leading to C fixation (electron transfer rate (ETR)) (pmol e- cell-1 s-1) ---
    J_ep_in = ko * ZPHOT(ng)%QAr(n,i,j)
    J_ep_max = Pmax*RubisCO/c2e
    J_ep = min( J_ep_in,  J_ep_max ) 
      
!---- Photosynthetic rate  (pmol C cell-1 s-1) ---
    Pg = J_ep * c2e

!---- Electrons that form ROS  (pmol ROf cell-1 s-1) ---
    J_ee = J_ep_in - J_ep
    F_zROS = f_RO*J_ee/e2ROS
    
!---- Photoinihibition rate of QAa to QAi  (pmol RCII cell-1 s-1) ---
!    Ja2i = ( ki*PFDsurf + di ) * QAa
    Ja2i = di * QAa  ! No photidamage case
    !    Ja2i = 0.0d0

!---- Repair rate of QAi to QAo  (pmol RCII cell-1 s-1) --- !!!!!!!!!!!!!!!!! Need update
!    Ji2a = ka * (ZPHOT(ng)%QAi(n,i,j)-0.3d0*QAt) /(1.0d0+exp(-ai2a*(Ti2a-Tamb)))  !!! Temperature dependent repair
!    Ji2a = ka * (ZPHOT(ng)%QAi(n,i,j)-0.3d0*QAt)   !!! No temperature dependence
!    Ji2a = ka * ZPHOT(ng)%QAi(n,i,j)/max(ROS, 1.0d0)  !!! ROS concentration dependent repair
!    Ji2a = ka * ZPHOT(ng)%QAi(n,i,j)/(1.0d0+exp(-arosi2a*(ROSi2a-ROS)))  !!! T  !!! ROS concentration dependent repair

    Ji2a = ka * ZPHOT(ng)%QAi(n,i,j) * ZPHOT(ng)%Rep(n,i,j)  !!! T  !!! ROS concentration dependent repair
!    Ji2a = ka * ZPHOT(ng)%QAi(n,i,j) * 0.9 !!! No photidamage case/No repair delay case

    !    Ji2a = ka * ZPHOT(ng)%QAi(n,i,j)  !!! ROS concentration dependent repair

!--------Time step progressing ------------------------

!-----Inhibited RCII -----------------
    ZPHOT(ng)%QAi(n,i,j)=ZPHOT(ng)%QAi(n,i,j) &
                 +(                                     &
                    Ja2i                                & ! Active to inactive
                   -Ji2a                                & ! Repair of QAi to QAo
                  )*dt
!    ZPHOT(ng)%QAi(n,i,j)=max(ZPHOT(ng)%QAi(n,i,j), 0.0d0) !Error handring

!-----Active RCII -----------------
    QAa = QAt - ZPHOT(ng)%QAi(n,i,j)

!-----Reduced RCII -----------------
    ZPHOT(ng)%QAr(n,i,j)=kr/(kr+ko)*QAa

!-----Oxidizes RCII -----------------
    ZPHOT(ng)%QAo(n,i,j)=QAa-ZPHOT(ng)%QAr(n,i,j)

!-----Status that repair from QAi to QAa  -----------------
    ZPHOT(ng)%Rep(n,i,j) = ZPHOT(ng)%Rep(n,i,j) &
        +(   krep *(1.0d0-ZPHOT(ng)%Rep(n,i,j)) &
!        +(   krep                               &
            - kdam*ROS*ZPHOT(ng)%Rep(n,i,j)      &
        )*dt
    ZPHOT(ng)%Rep(n,i,j)=min(ZPHOT(ng)%Rep(n,i,j), 1.0d0)

!------------------------------------------------------------------------
! Print section (for debug)

# if defined CORAL_TESTMODE

    time(n) = time(n) +dt  ! sec
    
    if(time(n).ge.dsec(n)) then
      
      write(40+n,*)                                             &
        time(n)/86400.0d0,',', PFDsurf,',', Tamb,','            &
       ,ZPHOT(ng)%QAo(n,i,j),',',ZPHOT(ng)%QAr(n,i,j),','       &
       ,ZPHOT(ng)%QAi(n,i,j),',',QAt,',',QAa,','                &
       ,QAa/QAt,',',ZPHOT(ng)%QAo(n,i,j)/QAt,','                &
       ,J_ea,',',J_ep_in,',',J_ep,',',J_ee,',',J_ep_max,','     &
       ,Ji2a,',',Ja2i,',',kr,',',ko,',',F_zROS,','              &
       ,Pg,',',J_ep_in*c2e,',',J_ep_max*c2e,','                 &
       ,RubisCO,',', ZPHOT(ng)%Rep(n,i,j)

      dsec(n)=dsec(n)+OUTPUT_INTERVAL*60.0d0
      
    endif
# endif
!-----------------------------------------------------------------------
    RETURN

  END SUBROUTINE photosystem
#endif
      
END MODULE mod_coral

