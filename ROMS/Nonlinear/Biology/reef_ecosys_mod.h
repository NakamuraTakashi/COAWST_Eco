!
!svn $Id: fennel_mod.h 585 2012-01-03 18:44:28Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2012 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!================================================== Takashi Nakamura ===
!                                                                      !
!  Parameters for Coral reef ecosystem model:                          !                                                                   !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
      implicit none
!
!  Set biological tracer identification indices.
!
#if defined ORGANIC_MATTER
      integer, parameter :: N_phyt = 3  ! Number of functional groups of phytoplankton
                                        !  1: dinoflagellate
                                        !  2: diatom
                                        !  3: coccolithophorids
      integer, parameter :: N_zoop = 1  ! Number of functional groups of zooplankton
                                        !  1: General Zooplankton 
      integer, parameter :: N_dom  = 2  ! Number of functional groups of Dissolved organoic matter
                                        !  1: Labile DOM
                                        !  2: Refractory DOM
      integer, parameter :: N_pom  = 2  ! Number of functional groups of Particulate organoic matter
                                        !  1: Detritus
                                        !  2: Coarse POM (leaf litter, etc.)
      integer, parameter :: N_pim  = 1  ! Number of functional groups of Particulate inorganoic matter
                                        !  1: coccolith (CaCO3)
#endif
      integer :: idCrl1                 ! Coral coverage
      integer :: idCrl2                 ! Coral2 coverage
      integer :: idSgrs                 ! Seagrass coverage
      integer :: idAlga                 ! Algal coverage
      integer :: idSand                 ! Sand coverage
      
      integer :: idd13C                 ! d13C of DIC

      integer, allocatable :: idbio(:)  ! Biological tracers
      integer :: iTIC_                  ! Total inorganic carbon
      integer :: iTAlk                  ! Total alkalinity
      integer :: iOxyg                  ! Dissolved oxygen concentration
#if defined ORGANIC_MATTER
      integer :: iDOC(N_dom)            ! Dissolved organic C-concentration
      integer :: iPOC(N_pom)            ! Particulate organic C-concentration
      integer :: iPhyt(N_phyt)          ! Phytoplankton1 density
      integer :: iZoop(N_zoop)          ! Zooplankton density
      integer :: iPIC(N_pim)            ! Particulate inorganic C-concentration
#endif
#if defined CARBON_ISOTOPE
      integer :: iT13C                  ! Corbon 13 of total inorganic carbon
# if defined ORGANIC_MATTER
      integer :: iDO13C(N_dom)          ! Dissolved organic 13C-concentration
      integer :: iPO13C(N_pom)          ! Particulate organic 13C-concentration
      integer :: iPhyt13C(N_phyt)       ! Phytoplankton1 13C-concentration
      integer :: iZoop13C(N_zoop)       ! Zooplankton 13C-concentration
      integer :: iPI13C(N_pim)          ! Particulate inorganic 13C-concentration
# endif
#endif
#if defined NUTRIENTS
      integer :: iNO3_                  ! Nitrate concentration
!      integer :: iNO2_                  ! Nitrite concentration
      integer :: iNH4_                  ! Ammonium concentration
      integer :: iPO4_                  ! Ammonium concentration
# if defined ORGANIC_MATTER
      integer :: iDON(N_dom)            ! Dissolved organic N-concentration
      integer :: iPON(N_pom)            ! Particulate organic N-concentration
      integer :: iDOP(N_dom)            ! Dissolved organic P-concentration
      integer :: iPOP(N_pom)            ! Particulate organic P-concentration
# endif
# if defined NITROGEN_ISOTOPE
      integer :: i15NO3                  ! Nitrogen isotope concentration in Nitrate 
!      integer :: i15NO2                  ! Nitrogen isotope concentration in Nitrite
      integer :: i15NH4                  ! Nitrogen isotope concentration in Ammonium
#  if defined ORGANIC_MATTER
      integer :: iDO15N(N_dom)          ! Dissolved organic 15N-concentration
      integer :: iPO15N(N_pom)          ! Particulate organic 15N-concentration
      integer :: iPhyt15N(N_phyt)       ! Phytoplankton 15N-concentration
      integer :: iZoop15N(N_zoop)       ! Zooplankton 15N-concentration
#  endif
# endif
#endif
#if defined COT_STARFISH
      integer :: iCOTe                  ! Eggs of crown-of-thorns starfish
      integer :: iCOTl                  ! Larvae of crown-of-thorns starfish
#endif
!
!  Biological 2D Histrory variable IDs.
!
      integer, allocatable :: iHbio2(:)       ! 2D biological terms
#ifdef CORAL_POLYP
      integer  :: iC1Pg                       ! coral1 gross photosynthesis rate
      integer  :: iC1_R                       ! coral1 respiration rate
      integer  :: iC1Pn                       ! coral1 net photosynthesis rate
      integer  :: iC1_G                       ! coral1 calcification rate
      integer  :: iC1OC                       ! coral1 tissue organic carbon
      integer  :: iC2Pg                       ! coral2 gross photosynthesis rate
      integer  :: iC2_R                       ! coral2 respiration rate
      integer  :: iC2Pn                       ! coral2 net photosynthesis rate
      integer  :: iC2_G                       ! coral2 calcification rate
      integer  :: iC2OC                       ! coral2 tissue organic carbon
# ifdef CORAL_CARBON_ISOTOPE
      integer  :: iC113                       ! coral1 tissue carbon isotope ratio
      integer  :: iC213                       ! coral2 tissue carbon isotope ratio
# endif
# ifdef CORAL_ZOOXANTHELLAE
      integer  :: iC1zx                       ! coral1 zooxanthellae density
      integer  :: iC2zx                       ! coral2 zooxanthellae density
# endif
# ifdef CORAL_SIZE_DYNAMICS
      integer  :: iC1mt                       ! coral1 mortality
      integer  :: iC1gr                       ! coral1 growth rate
      integer  :: iC2mt                       ! coral2 mortality
      integer  :: iC2gr                       ! coral2 growth rate
# endif
#endif
#ifdef SEAGRASS
      integer  :: iSgPg                       ! seagrass gross photosynthesis rate
      integer  :: iSg_R                       ! seagrass respiration rate
      integer  :: iSgPn                       ! seagrass net photosynthesis rate
#endif
#ifdef MACROALGAE
      integer  :: iAgPg                       ! Algal gross photosynthesis rate
      integer  :: iAg_R                       ! Algal respiration rate
      integer  :: iAgPn                       ! Algal net photosynthesis rate
#endif
#ifdef SEDIMENT_ECOSYS
      integer  :: iSdPg                       ! Sediment gross photosynthesis rate
      integer  :: iSd_R                       ! Sediment respiration rate
      integer  :: iSdPn                       ! Sediment net photosynthesis rate
      integer  :: iSd_G                       ! Sediment net calcification rate
#endif
      integer  :: ipHt_                       ! sea surface pH (total scale)
      integer  :: iWarg                       ! sea surface aragonite saturation state

      integer  :: iCOfx                       ! air-sea CO2 flux
      integer  :: ipCO2                       ! partial pressure of CO2
      integer  :: iO2fx                       ! air-sea O2 flux
      integer  :: iPARb                       ! bottom photon flux density (umol m-2 s-1)
      integer  :: iTau_                       ! bottom shear stress (N m-2)
!
!  Biological 3D Histrory variable IDs.
!
      integer, allocatable :: iHbio3(:)       ! 3D biological terms
#ifdef CARBON_ISOTOPE
      integer  :: id13C                       ! d13C of total inorganic carbon
#endif

      integer, allocatable :: iDbio2(:)       ! 2D biological terms
      integer, allocatable :: iDbio3(:)       ! 3D biological terms

!
!  Biological parameters.
!
      integer, allocatable :: CrlIter(:)
      integer, allocatable :: SedIter(:)

      real(r8), allocatable :: PARfrac(:)            ! nondimensional
      real(r8), allocatable :: pCO2air(:)            ! ppmv
      real(r8), allocatable :: TAlk0(:)              ! umol/kg
      real(r8), allocatable :: TIC_0(:)              ! umol/kg
      real(r8), allocatable :: Oxyg0(:)              ! umol/L
#if defined ORGANIC_MATTER
      real(r8), allocatable :: DOC_0(:,:)            ! umol/L
      real(r8), allocatable :: POC_0(:,:)            ! umol/L
      real(r8), allocatable :: Phyt_0(:,:)           ! umol/L
      real(r8), allocatable :: Zoop_0(:,:)           ! umol/L
      real(r8), allocatable :: PIC_0(:,:)            ! umol/L
#endif
#if defined CARBON_ISOTOPE
      real(r8), allocatable :: d13C_TIC0(:)          ! permil (VPDB)
# if defined ORGANIC_MATTER
      real(r8), allocatable :: d13C_DOC_0(:,:)       ! permil (VPDB)
      real(r8), allocatable :: d13C_POC_0(:,:)       ! permil (VPDB)
      real(r8), allocatable :: d13C_Phyt_0(:,:)      ! permil (VPDB)
      real(r8), allocatable :: d13C_Zoop_0(:,:)      ! permil (VPDB)
      real(r8), allocatable :: d13C_PIC_0(:,:)       ! permil (VPDB)
# endif
#endif
#if defined NUTRIENTS
      real(r8), allocatable :: NO3_0(:)              ! umol/L
!      real(r8), allocatable :: NO2_0(:)              ! umol/L
      real(r8), allocatable :: NH4_0(:)              ! umol/L
      real(r8), allocatable :: PO4_0(:)              ! umol/L
# if defined ORGANIC_MATTER
      real(r8), allocatable :: DON_0(:,:)            ! umolN/L
      real(r8), allocatable :: PON_0(:,:)            ! umolN/L
      real(r8), allocatable :: DOP_0(:,:)            ! umolP/L
      real(r8), allocatable :: POP_0(:,:)            ! umolP/L
# endif
# if defined NITROGEN_ISOTOPE
     real(r8), allocatable :: d15N_NO3_0(:)          ! permil
!     real(r8), allocatable :: d15N_NO2_0(:)          ! permil
     real(r8), allocatable :: d15N_NH4_0(:)          ! permil
#  if defined ORGANIC_MATTER
      real(r8), allocatable :: d15N_DOC_0(:,:)       ! permil
      real(r8), allocatable :: d15N_POC_0(:,:)       ! permil
      real(r8), allocatable :: d15N_Phyt_0(:,:)      ! permil
      real(r8), allocatable :: d15N_Zoop_0(:,:)      ! permil
#  endif
# endif
#endif
#if defined COT_STARFISH
      real(r8), allocatable :: COTe0(:)              ! umolC/L
      real(r8), allocatable :: COTl0(:)              ! umolC/L
#endif

      CONTAINS

      SUBROUTINE initialize_biology
!
!=======================================================================
!                                                                      !
!  This routine sets several variables needed by the biology model.    !
!  It allocates and assigns biological tracers indices.                !
!                                                                      !
!=======================================================================

!
!  Local variable declarations
!
      integer :: i, j, ic

!
!-----------------------------------------------------------------------
!  Initialize tracer identification indices.
!-----------------------------------------------------------------------
!
      ic=NAT+NPT+NCS+NNS

      i=1
      iTIC_=ic+i
      i=i+1
      iTAlk=ic+i
      i=i+1
      iOxyg=ic+i  !  4
#if defined ORGANIC_MATTER
      DO j=1,N_dom
        i=i+1
        iDOC(j)=ic+i
      END DO
      DO j=1,N_pom
        i=i+1
        iPOC(j)=ic+i
      END DO
      DO j=1,N_phyt
        i=i+1
        iPhyt(j)=ic+i
      END DO
      DO j=1,N_zoop
        i=i+1
        iZoop(j)=ic+i
      END DO
      DO j=1,N_pim
        i=i+1
        iPIC(j)=ic+i
      END DO
#endif
#if defined CARBON_ISOTOPE
      i=i+1
      iT13C=ic+i  ! +1
# if defined ORGANIC_MATTER
      DO j=1,N_dom
        i=i+1
        iDO13C(j)=ic+i
      END DO
      DO j=1,N_pom
        i=i+1
        iPO13C(j)=ic+i
      END DO
      DO j=1,N_phyt
        i=i+1
        iPhyt13C(j)=ic+i
      END DO
      DO j=1,N_zoop
        i=i+1
        iZoop13C(j)=ic+i
      END DO
      DO j=1,N_pim
        i=i+1
        iPI13C(j)=ic+i
      END DO
# endif
#endif
#if defined NUTRIENTS
      i=i+1
      iNO3_=ic+i
!      i=i+1
!      iNO2_=ic+i
      i=i+1
      iNH4_=ic+i
      i=i+1
      iPO4_=ic+i
# if defined ORGANIC_MATTER
      DO j=1,N_dom
        i=i+1
        iDON(j)=ic+i
      END DO
      DO j=1,N_pom
        i=i+1
        iPON(j)=ic+i
      END DO
      DO j=1,N_dom
        i=i+1
        iDOP(j)=ic+i
      END DO
      DO j=1,N_pom
        i=i+1
        iPOP(j)=ic+i
      END DO
# endif
# if defined NITROGEN_ISOTOPE
      i=i+1
      i15NO3=ic+i  ! +1
!      i=i+1
!      i15NO2=ic+i  ! +1
      i=i+1
      i15NH4=ic+i  ! +1
#  if defined ORGANIC_MATTER
      DO j=1,N_dom
        i=i+1
        iDO15N(j)=ic+i
      END DO
      DO j=1,N_pom
        i=i+1
        iPO15N(j)=ic+i
      END DO
      DO j=1,N_phyt
        i=i+1
        iPhyt15N(j)=ic+i
      END DO
      DO j=1,N_zoop
        i=i+1
        iZoop15N(j)=ic+i
      END DO
#  endif
# endif
#endif
#if defined COT_STARFISH
      i=i+1
      iCOTe=ic+i
      i=i+1
      iCOTl=ic+i
#endif

!-----------------------------------------------------------------------
!  Determine number of biological tracers.
!-----------------------------------------------------------------------

      NBT=i
!---------------------------------------------------------------------
!
!  Allocate biological tracer vector.
!
      IF (.not.allocated(idbio)) THEN
        allocate ( idbio(NBT) )
      END IF

      DO i=1,NBT
        idbio(i)=NAT+NPT+NCS+NNS+i
      END DO

!
!-----------------------------------------------------------------------
!  Set sources and sinks biology history/diagnostic parameters.
!-----------------------------------------------------------------------

!
!  Initialize 2D biology indices.
!
      ic=0     ! ic reset

      ic=ic+1
      ipHt_=ic
      ic=ic+1
      iWarg=ic

      ic=ic+1
      iCOfx=ic
      ic=ic+1
      ipCO2=ic
      ic=ic+1
      iO2fx=ic

      ic=ic+1
      iPARb=ic

      ic=ic+1
      iTau_=ic

#ifdef CORAL_POLYP
      ic=ic+1
      iC1Pg=ic
      ic=ic+1
      iC1_R=ic
      ic=ic+1
      iC1Pn=ic
      ic=ic+1
      iC1_G=ic
      ic=ic+1
      iC1OC=ic
      ic=ic+1
      iC2Pg=ic
      ic=ic+1
      iC2_R=ic
      ic=ic+1
      iC2Pn=ic
      ic=ic+1
      iC2_G=ic
      ic=ic+1
      iC2OC=ic
# ifdef CORAL_CARBON_ISOTOPE
      ic=ic+1
      iC113=ic
      ic=ic+1
      iC213=ic
# endif
# ifdef CORAL_ZOOXANTHELLAE
      ic=ic+1
      iC1zx=ic
      ic=ic+1
      iC2zx=ic
# endif
# ifdef CORAL_SIZE_DYNAMICS
      ic=ic+1
      iC1mt=ic
      ic=ic+1
      iC1gr=ic
      ic=ic+1
      iC2mt=ic
      ic=ic+1
      iC2gr=ic
# endif
#endif
#ifdef SEAGRASS
      ic=ic+1
      iSgPg=ic
      ic=ic+1
      iSg_R=ic
      ic=ic+1
      iSgPn=ic
#endif
#ifdef MACROALGAE
      ic=ic+1
      iAgPg=ic
      ic=ic+1
      iAg_R=ic
      ic=ic+1
      iAgPn=ic
#endif
#ifdef SEDIMENT_ECOSYS
      ic=ic+1
      iSdPg=ic
      ic=ic+1
      iSd_R=ic
      ic=ic+1
      iSdPn=ic
      ic=ic+1
      iSd_G=ic
#endif

!  Set number of 2D history terms.
!
      NHbio2d=ic
!
!  Allocate biological history vectors
!
      IF (.not.allocated(iHbio2)) THEN
        allocate ( iHbio2(NHbio2d) )
      END IF

!----------------------------------------------------------------------
!  Initialize 3D biology indices.
!
      ic=0     ! ic reset
#ifdef CARBON_ISOTOPE
      ic=ic+1
      id13C=ic  ! +1
#endif
!
!  Set number of 3D history terms.
!
      NHbio3d=ic


      IF (.not.allocated(iHbio3)) THEN
        allocate ( iHbio3(NHbio3d) )
      END IF

!
!-----------------------------------------------------------------------
!  Allocate various module variables.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(CrlIter)) THEN
        allocate ( CrlIter(Ngrids) )
      END IF
      IF (.not.allocated(SedIter)) THEN
        allocate ( SedIter(Ngrids) )
      END IF
      IF (.not.allocated(PARfrac)) THEN
        allocate ( PARfrac(Ngrids) )
      END IF
      IF (.not.allocated(pCO2air)) THEN
        allocate ( pCO2air(Ngrids) )
      END IF
      IF (.not.allocated(TAlk0)) THEN
        allocate ( TAlk0(Ngrids) )
      END IF
      IF (.not.allocated(TIC_0)) THEN
        allocate ( TIC_0(Ngrids) )
      END IF
      IF (.not.allocated(Oxyg0)) THEN
        allocate ( Oxyg0(Ngrids) )
      END IF
#if defined ORGANIC_MATTER
      IF (.not.allocated(DOC_0)) THEN
        allocate ( DOC_0(N_dom,Ngrids) )
      END IF
      IF (.not.allocated(POC_0)) THEN
        allocate ( POC_0(N_pom,Ngrids) )
      END IF
      IF (.not.allocated(Phyt_0)) THEN
        allocate ( Phyt_0(N_phyt,Ngrids) )
      END IF
      IF (.not.allocated(Zoop_0)) THEN
        allocate ( Zoop_0(N_zoop,Ngrids) )
      END IF
      IF (.not.allocated(PIC_0)) THEN
        allocate ( PIC_0(N_pim,Ngrids) )
      END IF
#endif
#if defined CARBON_ISOTOPE
      IF (.not.allocated(d13C_TIC0)) THEN
        allocate ( d13C_TIC0(Ngrids) )
      END IF
# if defined ORGANIC_MATTER
      IF (.not.allocated(d13C_DOC_0)) THEN
        allocate ( d13C_DOC_0(N_dom,Ngrids) )
      END IF
      IF (.not.allocated(d13C_POC_0)) THEN
        allocate ( d13C_POC_0(N_pom,Ngrids) )
      END IF
      IF (.not.allocated(d13C_Phyt_0)) THEN
        allocate ( d13C_Phyt_0(N_phyt,Ngrids) )
      END IF
      IF (.not.allocated(d13C_Zoop_0)) THEN
        allocate ( d13C_Zoop_0(N_zoop,Ngrids) )
      END IF
      IF (.not.allocated(d13C_PIC_0)) THEN
        allocate ( d13C_PIC_0(N_pim,Ngrids) )
      END IF
# endif
#endif
#if defined NUTRIENTS
      IF (.not.allocated(NO3_0)) THEN
        allocate ( NO3_0(Ngrids) )
      END IF
!      IF (.not.allocated(NO2_0)) THEN
!        allocate ( NO2_0(Ngrids) )
!      END IF
      IF (.not.allocated(NH4_0)) THEN
        allocate ( NH4_0(Ngrids) )
      END IF
      IF (.not.allocated(PO4_0)) THEN
        allocate ( PO4_0(Ngrids) )
      END IF
# if defined ORGANIC_MATTER
      IF (.not.allocated(DON_0)) THEN
        allocate ( DON_0(N_dom,Ngrids) )
      END IF
      IF (.not.allocated(PON_0)) THEN
        allocate ( PON_0(N_pom,Ngrids) )
      END IF
      IF (.not.allocated(DOP_0)) THEN
        allocate ( DOP_0(N_dom,Ngrids) )
      END IF
      IF (.not.allocated(POP_0)) THEN
        allocate ( POP_0(N_pom,Ngrids) )
      END IF
# endif
# if defined NITROGEN_ISOTOPE
      IF (.not.allocated(d15N_NO3_0)) THEN
        allocate ( d15N_NO3_0(Ngrids) )
      END IF
!      IF (.not.allocated(d15N_NO2_0)) THEN
!        allocate ( d15N_NO2_0(Ngrids) )
!      END IF
      IF (.not.allocated(d15N_NH4_0)) THEN
        allocate ( d15N_NH4_0(Ngrids) )
      END IF
#  if defined ORGANIC_MATTER
      IF (.not.allocated(d15N_DOC0)) THEN
        allocate ( d15N_DOC_0(N_dom,Ngrids) )
      END IF
      IF (.not.allocated(d15N_POC0)) THEN
        allocate ( d15N_POC_0(N_pom,Ngrids) )
      END IF
      IF (.not.allocated(d15N_Phyt_0)) THEN
        allocate ( d15N_Phyt_0(N_phyt,Ngrids) )
      END IF
      IF (.not.allocated(d15N_Zoop_0)) THEN
        allocate ( d15N_Zoop_0(N_zoop,Ngrids) )
      END IF
#  endif
# endif
#endif
#if defined COT_STARFISH
      IF (.not.allocated(COTe0)) THEN
        allocate ( COTe0(Ngrids) )
      END IF
      IF (.not.allocated(COTl0)) THEN
        allocate ( COTl0(Ngrids) )
      END IF
#endif
      RETURN
      END SUBROUTINE initialize_biology
