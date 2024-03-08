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
      USE mod_ocean
      USE mod_foodweb    !, ONLY : Ndom, Npom, Nphy, Nzoo, Npim
      USE mod_coral      !, ONLY : Ncl
      USE mod_seagrass   !, ONLY : Nsg
      USE mod_macroalgae !, ONLY : Nag
      USE mod_sedecosys
!
      implicit none
!
!  Set biological tracer identification indices.
!
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
      integer :: iDOC(Ndom)            ! Dissolved organic C-concentration
      integer :: iPOC(Npom)            ! Particulate organic C-concentration
      integer :: iPhyt(Nphy)          ! Phytoplankton1 density
      integer :: iZoop(Nzoo)          ! Zooplankton density
      integer :: iPIC(Npim)            ! Particulate inorganic C-concentration
#endif
#if defined CARBON_ISOTOPE
      integer :: iT13C                  ! Corbon 13 of total inorganic carbon
# if defined ORGANIC_MATTER
      integer :: iDO13C(Ndom)          ! Dissolved organic 13C-concentration
      integer :: iPO13C(Npom)          ! Particulate organic 13C-concentration
      integer :: iPhyt13C(Nphy)       ! Phytoplankton1 13C-concentration
      integer :: iZoop13C(Nzoo)       ! Zooplankton 13C-concentration
      integer :: iPI13C(Npim)          ! Particulate inorganic 13C-concentration
# endif
#endif
#if defined NUTRIENTS
      integer :: iNO3_                  ! Nitrate concentration
!      integer :: iNO2_                  ! Nitrite concentration
      integer :: iNH4_                  ! Ammonium concentration
      integer :: iPO4_                  ! Ammonium concentration
# if defined ORGANIC_MATTER
      integer :: iDON(Ndom)            ! Dissolved organic N-concentration
      integer :: iPON(Npom)            ! Particulate organic N-concentration
      integer :: iDOP(Ndom)            ! Dissolved organic P-concentration
      integer :: iPOP(Npom)            ! Particulate organic P-concentration
# endif
# if defined NITROGEN_ISOTOPE
      integer :: i15NO3                  ! Nitrogen isotope concentration in Nitrate 
!      integer :: i15NO2                  ! Nitrogen isotope concentration in Nitrite
      integer :: i15NH4                  ! Nitrogen isotope concentration in Ammonium
#  if defined ORGANIC_MATTER
      integer :: iDO15N(Ndom)          ! Dissolved organic 15N-concentration
      integer :: iPO15N(Npom)          ! Particulate organic 15N-concentration
      integer :: iPhyt15N(Nphy)       ! Phytoplankton 15N-concentration
      integer :: iZoop15N(Nzoo)       ! Zooplankton 15N-concentration
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
      integer  :: iClTAcal (Ncl)
      integer  :: iClTAcoe (Ncl)
      integer  :: iClDICcal(Ncl)
      integer  :: iClDICcoe(Ncl)
      integer  :: iClDOcoe (Ncl)
      integer  :: iClQC    (Ncl)
# if defined CORAL_NONE_CO2_EQ
      integer  :: iClCO2cal(Ncl)
      integer  :: iClCO2coe(Ncl)
# endif
# ifdef CORAL_CARBON_ISOTOPE
      integer  :: iClDI13Ccal(Ncl)
      integer  :: iClDI13Ccoe(Ncl)
      integer  :: iClQ13C    (Ncl)
#  if defined CORAL_NONE_CO2_EQ
      integer  :: iCl13CO2cal(Ncl)
      integer  :: iCl13CO2coe(Ncl)
#  endif
# endif
# ifdef CORAL_ZOOXANTHELLAE
      integer  :: iClROS(Ncl)
      integer  :: iZxDns(Ncl)
      integer  :: iZxQC (Ncl)
      integer  :: iZxChl(Ncl)
      integer  :: iZxQAo(Ncl)
      integer  :: iZxQAr(Ncl)
      integer  :: iZxQAi(Ncl)
      integer  :: iZxQAid(Ncl)
#  ifdef CORAL_CARBON_ISOTOPE
      integer  :: iZxQ13C(Ncl)
#  endif
# endif
# ifdef CORAL_SIZE_DYNAMICS

# endif
#endif
#ifdef SEAGRASS

#endif
#ifdef MACROALGAE

#endif
!
!  Biological 3D Histrory variable IDs.
!
      integer, allocatable :: iHbio3(:)       ! 3D biological terms
#ifdef CARBON_ISOTOPE
      integer  :: id13C  ! d13C of total inorganic carbon  !!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Remove??
#endif

#if defined DIAGNOSTICS_BIO
!
!  Biological 2D Diagnostic variable IDs.
!
      integer, allocatable :: iDbio2(:)       ! 2D biological terms

      integer  :: ipHt_                       ! sea surface pH (total scale)
      integer  :: iWarg                       ! sea surface aragonite saturation state
      integer  :: iCO2fx                      ! air-sea CO2 flux
      integer  :: ipCO2                       ! partial pressure of CO2
      integer  :: iO2fx                       ! air-sea O2 flux
      integer  :: iPARb                       ! bottom photon flux density (umol m-2 s-1)
      integer  :: iTau_                       ! bottom shear stress (N m-2)

# ifdef CORAL_POLYP
      integer  :: iClPg(Ncl)
      integer  :: iCl_R(Ncl)
      integer  :: iCl_G(Ncl)
      integer  :: iClPn(Ncl)
#  ifdef CORAL_CARBON_ISOTOPE
      integer  :: iClQCd13C(Ncl)
#  endif
#  ifdef CORAL_ZOOXANTHELLAE
      integer  :: iZxPg(Ncl)
      integer  :: iZx_R(Ncl)
      integer  :: iZxPn(Ncl)
#  endif
#  ifdef CORAL_SIZE_DYNAMICS
      integer  :: iClmt(Ncl)
      integer  :: iClgw(Ncl)
#  endif
# endif
# ifdef SEAGRASS
      integer  :: iSgPg(Nsg)                   ! seagrass gross photosynthesis rate
      integer  :: iSg_R(Nsg)                   ! seagrass respiration rate
      integer  :: iSgPn(Nsg)                   ! seagrass net photosynthesis rate
# endif
# ifdef MACROALGAE
      integer  :: iAgPg(Nag)                  ! Algal gross photosynthesis rate
      integer  :: iAg_R(Nag)                  ! Algal respiration rate
      integer  :: iAgPn(Nag)                  ! Algal net photosynthesis rate
# endif
!
!  Biological 3D Diagnostic variable IDs.
!
      integer, allocatable :: iDbio3(:)       ! 3D biological terms

# ifdef CARBON_ISOTOPE
      integer  :: id13C                       ! d13C of total inorganic carbon
# endif
#endif
!
!  Biological parameters.
!
      logical, allocatable :: LReadBioINI(:,:)

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

!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
#ifdef SEDIMENT_ECOSYS
!
!  Biological 3D Sediment Histrory variable IDs.
!
      integer, allocatable :: iHbiosed3(:)       ! 3D biological sediment terms

      integer :: iSdporo   !! Porosity (cm_w3 cm_s-3)
      integer :: iSdTmp    !! Temperature    (ºC)
      integer :: iSdSal    !! Salinity       (PSU)
      ! integer :: iSdpH     !! pH   
      integer :: iSdTA     !! Total Alkalinity
      ! integer :: iSdDIC    !! Dissolved inorganic carbon
      integer :: iSdO2     !! Dissolved Oxygen    (µmol l-1)
      integer :: iSdCO2    !! Carbon dyoxide      (µmol l-1)
      integer :: iSdN2     !! Nitrogen            (µmol l-1)
! Organic matters
# if defined ORGANIC_MATTER
      integer :: iSdDOCf    !! Dissolved organic carbon   (fast :Labile)       (µmol l-1)
      integer :: iSdDOCs    !! Dissolved organic carbon   (slow :Refractory)   (µmol l-1)
      integer :: iSdPOCf    !! Particulate organic carbon (fast :Labile)       (nmol g-1)
      integer :: iSdPOCs    !! Particulate organic carbon (slow :Refractory)   (nmol g-1)
      integer :: iSdPOCn    !! Particulate organic carbon (non-degradable)     (nmol g-1)
# endif
!  Nutrients dynamics
# if defined NUTRIENTS
      integer :: iSdNO3     !! Nitrate     (µmol l-1)
      integer :: iSdNH4     !! Ammonium    (µmol l-1)
      integer :: iSdPO4     !! Phosphate   (µmol l-1)
#  if defined ORGANIC_MATTER
      integer :: iSdDONf     !! Dissolved organic nitrogen     (fast :Labile)      (µmol l-1)
      integer :: iSdDONs     !! Dissolved organic nitrogen     (slow :Refractory)  (µmol l-1)
      integer :: iSdPONf     !! Particulate organic nitrogen   (fast :Labile)      (µmol l-1)
      integer :: iSdPONs     !! Particulate organic nitrogen   (slow :Refractory)  (µmol l-1)
      integer :: iSdPONn     !! Particulate organic nitrogen   (non-degradable)    (µmol l-1)
      integer :: iSdDOPf     !! Dissolved organic phosphorus   (fast :Labile)      (µmol l-1)
      integer :: iSdDOPs     !! Dissolved organic phosphorus   (slow :Refractory)  (µmol l-1)
      integer :: iSdPOPf     !! Particulate organic phosphorus (fast :Labile)      (µmol l-1)
      integer :: iSdPOPs     !! Particulate organic phosphorus (slow :Refractory)  (µmol l-1)
      integer :: iSdPOPn     !! Particulate organic phosphorus (non-degradable)    (µmol l-1)
#  endif
# endif
! Sulfur dynamics
# if defined SULFATE
      integer :: iSdMn2       !! Manganese ion        (µmol l-1)
      integer :: iSdMnO2      !! Manganese dioxide    (nmol g-1)
      integer :: iSdFe2       !! iron(II)          (µmol l-1)
      integer :: iSdFeS       !! iron sulfide      (nmol g-1)
      integer :: iSdFeS2      !! pyrite            (nmol g-1)
      integer :: iSdFeOOH     !! iron hydroxide    (nmol g-1)
      integer :: iSdFeOOH_PO4 !! iron-bound phosphate (FeOOH=PO43-)   (nmol g-1)
      integer :: iSdH2S       !! hydrogen sulfide         (µmol l-1)
      integer :: iSdSO4       !! sulfate   (µmol l-1)
      integer :: iSdS0        !! sulfur    (nmol g-1)
# endif
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

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
      DO j=1,Ndom
        i=i+1
        iDOC(j)=ic+i
      END DO
      DO j=1,Npom
        i=i+1
        iPOC(j)=ic+i
      END DO
      DO j=1,Nphy
        i=i+1
        iPhyt(j)=ic+i
      END DO
      DO j=1,Nzoo
        i=i+1
        iZoop(j)=ic+i
      END DO
      DO j=1,Npim
        i=i+1
        iPIC(j)=ic+i
      END DO
#endif
#if defined CARBON_ISOTOPE
      i=i+1
      iT13C=ic+i  ! +1
# if defined ORGANIC_MATTER
      DO j=1,Ndom
        i=i+1
        iDO13C(j)=ic+i
      END DO
      DO j=1,Npom
        i=i+1
        iPO13C(j)=ic+i
      END DO
      DO j=1,Nphy
        i=i+1
        iPhyt13C(j)=ic+i
      END DO
      DO j=1,Nzoo
        i=i+1
        iZoop13C(j)=ic+i
      END DO
      DO j=1,Npim
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
      DO j=1,Ndom
        i=i+1
        iDON(j)=ic+i
      END DO
      DO j=1,Npom
        i=i+1
        iPON(j)=ic+i
      END DO
      DO j=1,Ndom
        i=i+1
        iDOP(j)=ic+i
      END DO
      DO j=1,Npom
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
      DO j=1,Ndom
        i=i+1
        iDO15N(j)=ic+i
      END DO
      DO j=1,Npom
        i=i+1
        iPO15N(j)=ic+i
      END DO
      DO j=1,Nphy
        i=i+1
        iPhyt15N(j)=ic+i
      END DO
      DO j=1,Nzoo
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
!  Set sources and sinks biology history parameters.
!-----------------------------------------------------------------------

!
!  Initialize 2D biology indices.
!
      ic=0     ! ic reset

#ifdef CORAL_POLYP
      DO j=1,Ncl
        ic=ic+1
        iClTAcal(j)=ic
        ic=ic+1
        iClTAcoe(j)=ic
        ic=ic+1
        iClDICcal(j)=ic
        ic=ic+1
        iClDICcoe(j)=ic
        ic=ic+1
        iClDOcoe(j)=ic
        ic=ic+1
        iClQC(j)=ic
#if defined CORAL_NONE_CO2_EQ
        ic=ic+1
        iClCO2cal(j)=ic
        ic=ic+1
        iClCO2coe(j)=ic
# endif
# ifdef CORAL_CARBON_ISOTOPE
        ic=ic+1
        iClDI13Ccal(j)=ic
        ic=ic+1
        iClDI13Ccoe(j)=ic
        ic=ic+1
        iClQ13C(j)=ic
# if defined CORAL_NONE_CO2_EQ
        ic=ic+1
        iCl13CO2cal(j)=ic
        ic=ic+1
        iCl13CO2coe(j)=ic
#  endif
# endif
# ifdef CORAL_ZOOXANTHELLAE
        ic=ic+1
        iClROS(j)=ic
        ic=ic+1
        iZxDns(j)=ic
        ic=ic+1
        iZxQC(j)=ic
        ic=ic+1
        iZxChl(j)=ic
        ic=ic+1
        iZxQAo(j)=ic
        ic=ic+1
        iZxQAr(j)=ic
        ic=ic+1
        iZxQAi(j)=ic
        ic=ic+1
        iZxQAid(j)=ic
#  ifdef CORAL_CARBON_ISOTOPE
        ic=ic+1
        iZxQ13C(j)=ic
#  endif
# endif
      END DO
#endif
#ifdef SEAGRASS
      DO j=1,Nsg
        
      END DO
#endif
#ifdef MACROALGAE
      DO j=1,Nag
        
      END DO
#endif
!
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

!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
!----------------------------------------------------------------------
!  Initialize 3D biological sediment indices.
!
#ifdef SEDIMENT_ECOSYS  
      ic=0     ! ic reset

      ic=ic+1
      iSdporo=ic
      ic=ic+1
      iSdTmp=ic
      ic=ic+1
      iSdSal=ic
      ! ic=ic+1
      ! iSdpH=ic
      ic=ic+1
      iSdTA=ic
      ! ic=ic+1
      ! iSdDIC=ic
      ic=ic+1
      iSdO2=ic
      ic=ic+1
      iSdCO2=ic
      ic=ic+1
      iSdN2=ic
# if defined ORGANIC_MATTER
      ic=ic+1
      iSdDOCf=ic
      ic=ic+1
      iSdDOCs=ic
      ic=ic+1
      iSdPOCf=ic
      ic=ic+1
      iSdPOCs=ic
      ic=ic+1
      iSdPOCn=ic
# endif
# if defined NUTRIENTS
      ic=ic+1
      iSdNO3=ic
      ic=ic+1
      iSdNH4=ic
      ic=ic+1
      iSdPO4=ic
#  if defined ORGANIC_MATTER
      ic=ic+1
      iSdDONf=ic
      ic=ic+1
      iSdDONs=ic
      ic=ic+1
      iSdPONf=ic
      ic=ic+1
      iSdPONs=ic
      ic=ic+1
      iSdPONn=ic
      ic=ic+1
      iSdDOPf=ic
      ic=ic+1
      iSdDOPs=ic
      ic=ic+1
      iSdPOPf=ic
      ic=ic+1
      iSdPOPs=ic
      ic=ic+1
      iSdPOPn=ic
#  endif
# endif
# if defined SULFATE
      ic=ic+1
      iSdMn2=ic
      ic=ic+1
      iSdMnO2=ic
      ic=ic+1
      iSdFe2=ic
      ic=ic+1
      iSdFeS=ic
      ic=ic+1
      iSdFeS2=ic
      ic=ic+1
      iSdFeOOH=ic
      ic=ic+1
      iSdFeOOH_PO4=ic
      ic=ic+1
      iSdH2S=ic
      ic=ic+1
      iSdSO4=ic
      ic=ic+1
      iSdS0=ic
# endif
!
!  Set number of 3D biological sediment history terms.
!
      NHbiosed3d=ic
!
!  Allocate biological history vectors
!
      IF (.not.allocated(iHbiosed3)) THEN
        allocate ( iHbiosed3(NHbiosed3d) )
      END IF

#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

#if defined DIAGNOSTICS_BIO
!
!-----------------------------------------------------------------------
!  Set sources and sinks biology diagnostic parameters.
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
      iCO2fx=ic
      ic=ic+1
      ipCO2=ic
      ic=ic+1
      iO2fx=ic

      ic=ic+1
      iPARb=ic

      ic=ic+1
      iTau_=ic

# ifdef CORAL_POLYP
      DO j=1,Ncl
        ic=ic+1
        iClPg(j)=ic
        ic=ic+1
        iCl_R(j)=ic
        ic=ic+1
        iCl_G(j)=ic
        ic=ic+1
        iClPn(j)=ic
#  ifdef CORAL_CARBON_ISOTOPE
        ic=ic+1
        iClQCd13C(j)=ic
#  endif
#  ifdef CORAL_ZOOXANTHELLAE
        ic=ic+1
        iZxPg(j)=ic
        ic=ic+1
        iZx_R(j)=ic
        ic=ic+1
        iZxPn(j)=ic
#  endif
#  ifdef CORAL_SIZE_DYNAMICS
        ic=ic+1
        iClmt(j)=ic
        ic=ic+1
        iClgw(j)=ic
#  endif
      END DO
# endif
# ifdef SEAGRASS
      DO j=1,Nsg
        ic=ic+1
        iSgPg(j)=ic
        ic=ic+1
        iSg_R(j)=ic
        ic=ic+1
        iSgPn(j)=ic
      END DO
# endif
# ifdef MACROALGAE
      DO j=1,Nag
        ic=ic+1
        iAgPg(j)=ic
        ic=ic+1
        iAg_R(j)=ic
        ic=ic+1
        iAgPn(j)=ic
      END DO
# endif
!
!  Set number of 2D diagnostic terms.
!
      NDbio2d=ic
!
!  Allocate biological diagnostic vectors
!
      IF (.not.allocated(iDbio2)) THEN
        allocate ( iDbio2(NDbio2d) )
      END IF

!----------------------------------------------------------------------
!  Initialize 3D biology indices.
!
      ic=0     ! ic reset

# ifdef CARBON_ISOTOPE
      ic=ic+1
      id13C=ic  ! +1
# endif
!
!  Set number of 3D diagnostic terms.
!
      NDbio3d=ic


      IF (.not.allocated(iDbio3)) THEN
        allocate ( iDbio3(NDbio3d) )
      END IF

#endif

!
!-----------------------------------------------------------------------
!  Allocate various module variables.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(LReadBioINI)) THEN
        allocate ( LReadBioINI(2,Ngrids) )
      END IF
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
        allocate ( DOC_0(Ndom,Ngrids) )
      END IF
      IF (.not.allocated(POC_0)) THEN
        allocate ( POC_0(Npom,Ngrids) )
      END IF
      IF (.not.allocated(Phyt_0)) THEN
        allocate ( Phyt_0(Nphy,Ngrids) )
      END IF
      IF (.not.allocated(Zoop_0)) THEN
        allocate ( Zoop_0(Nzoo,Ngrids) )
      END IF
      IF (.not.allocated(PIC_0)) THEN
        allocate ( PIC_0(Npim,Ngrids) )
      END IF
#endif
#if defined CARBON_ISOTOPE
      IF (.not.allocated(d13C_TIC0)) THEN
        allocate ( d13C_TIC0(Ngrids) )
      END IF
# if defined ORGANIC_MATTER
      IF (.not.allocated(d13C_DOC_0)) THEN
        allocate ( d13C_DOC_0(Ndom,Ngrids) )
      END IF
      IF (.not.allocated(d13C_POC_0)) THEN
        allocate ( d13C_POC_0(Npom,Ngrids) )
      END IF
      IF (.not.allocated(d13C_Phyt_0)) THEN
        allocate ( d13C_Phyt_0(Nphy,Ngrids) )
      END IF
      IF (.not.allocated(d13C_Zoop_0)) THEN
        allocate ( d13C_Zoop_0(Nzoo,Ngrids) )
      END IF
      IF (.not.allocated(d13C_PIC_0)) THEN
        allocate ( d13C_PIC_0(Npim,Ngrids) )
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
        allocate ( DON_0(Ndom,Ngrids) )
      END IF
      IF (.not.allocated(PON_0)) THEN
        allocate ( PON_0(Npom,Ngrids) )
      END IF
      IF (.not.allocated(DOP_0)) THEN
        allocate ( DOP_0(Ndom,Ngrids) )
      END IF
      IF (.not.allocated(POP_0)) THEN
        allocate ( POP_0(Npom,Ngrids) )
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
        allocate ( d15N_DOC_0(Ndom,Ngrids) )
      END IF
      IF (.not.allocated(d15N_POC0)) THEN
        allocate ( d15N_POC_0(Npom,Ngrids) )
      END IF
      IF (.not.allocated(d15N_Phyt_0)) THEN
        allocate ( d15N_Phyt_0(Nphy,Ngrids) )
      END IF
      IF (.not.allocated(d15N_Zoop_0)) THEN
        allocate ( d15N_Zoop_0(Nzoo,Ngrids) )
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

!***********************************************************************

      SUBROUTINE send_roms_his2reef_ecosys(ng,LBi, UBi, LBj, UBj)
!
!=======================================================================
!                                                                      !
!  This routine sets reef_ecosys arraies to roms arralies for          !
!  his nc output.                                                      !
!  This routine is called in mod_arrays.F and only used for initial    !
!  condotion setting.                                                  !
!                                                                      !
!=======================================================================
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj
!
!  Local variable declarations.
!
      integer :: i,j,k
      integer :: isp

!-----------------------------------------------------------------------
      DO j=LBj, UBj
        DO i=LBi, UBi
#ifdef CORAL_POLYP
          DO isp=1,Ncl
            !  :  (To be updated)
            CORAL(ng)%TAcal(isp,i,j)     = OCEAN(ng)%HisBio2d(i,j, iClTAcal (isp) )
            CORAL(ng)%TAcoe(isp,i,j)     = OCEAN(ng)%HisBio2d(i,j, iClTAcoe (isp) )
            CORAL(ng)%DICcal(isp,i,j)    = OCEAN(ng)%HisBio2d(i,j, iClDICcal(isp) )
            CORAL(ng)%DICcoe(isp,i,j)    = OCEAN(ng)%HisBio2d(i,j, iClDICcoe(isp) )
            CORAL(ng)%DOcoe(isp,i,j)     = OCEAN(ng)%HisBio2d(i,j, iClDOcoe (isp) )
            CORAL(ng)%QC(isp,i,j)        = OCEAN(ng)%HisBio2d(i,j, iClQC    (isp) )
# if defined CORAL_NONE_CO2_EQ
            CORAL(ng)%cCO2aqcal(isp,i,j) = OCEAN(ng)%HisBio2d(i,j, iClCO2cal(isp) )
            CORAL(ng)%cCO2aqcoe(isp,i,j) = OCEAN(ng)%HisBio2d(i,j, iClCO2coe(isp) )
# endif
# if defined CORAL_CARBON_ISOTOPE
            CORAL(ng)%DI13Ccal(isp,i,j)    = OCEAN(ng)%HisBio2d(i,j, iClDI13Ccal(isp) )
            CORAL(ng)%DI13Ccoe(isp,i,j)    = OCEAN(ng)%HisBio2d(i,j, iClDI13Ccoe(isp) )
            CORAL(ng)%Q13C (isp,i,j)       = OCEAN(ng)%HisBio2d(i,j, iClQ13C    (isp) )
#  if defined CORAL_NONE_CO2_EQ
            CORAL(ng)%c13CO2aqcal(isp,i,j) = OCEAN(ng)%HisBio2d(i,j, iCl13CO2cal(isp) )
            CORAL(ng)%c13CO2aqcoe(isp,i,j) = OCEAN(ng)%HisBio2d(i,j, iCl13CO2coe(isp) )
#  endif
# endif
# if defined CORAL_ZOOXANTHELLAE
            CORAL(ng)%ROS(isp,i,j) = OCEAN(ng)%HisBio2d(i,j, iClROS(isp) )
# endif
# if defined CORAL_NUTRIENTS
            !  :  (To be updated)
# endif     
# if defined CORAL_SIZE_DYNAMICS
            !  :  (To be updated)
# endif

# if defined CORAL_ZOOXANTHELLAE
            ZOOX(ng)%dens(isp,i,j) = OCEAN(ng)%HisBio2d(i,j, iZxDns(isp) )
            ZOOX(ng)%QC(isp,i,j)   = OCEAN(ng)%HisBio2d(i,j, iZxQC (isp) )
            ZOOX(ng)%Chl(isp,i,j)  = OCEAN(ng)%HisBio2d(i,j, iZxChl(isp) )
            ZOOX(ng)%QAo(isp,i,j)  = OCEAN(ng)%HisBio2d(i,j, iZxQAo(isp) )
            ZOOX(ng)%QAr(isp,i,j)  = OCEAN(ng)%HisBio2d(i,j, iZxQAr(isp) )
            ZOOX(ng)%QAi(isp,i,j)  = OCEAN(ng)%HisBio2d(i,j, iZxQAi(isp) )
            ZOOX(ng)%QAid(isp,i,j) = OCEAN(ng)%HisBio2d(i,j, iZxQAid(isp) )
#  if defined CORAL_CARBON_ISOTOPE
            ZOOX(ng)%Q13C(isp,i,j) = OCEAN(ng)%HisBio2d(i,j, iZxQ13C(isp) )
#  endif
#  if defined CORAL_NUTRIENTS
            !  :  (To be updated)
#  endif
# endif
          END DO
#endif

#ifdef SEAGRASS  
          DO isp=1,Nsg
          !  :  (To be updated)
          END DO
#endif
#ifdef MACROALGAE  
          DO isp=1,Nag
          !  :  (To be updated)
          END DO
#endif
        END DO
      END DO

#ifdef SEDIMENT_ECOSYS  
      DO k=1,Nsed(ng)
        DO j=LBj, UBj
          DO i=LBi, UBi
            SEDECO(ng)%poro(i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdporo) 
            SEDECO(ng)%Tmp (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdTmp ) 
            SEDECO(ng)%Sal (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdSal ) 
            ! SEDECO(ng)%pH  (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdpH  ) 
            SEDECO(ng)%TA  (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdTA  ) 
            ! SEDECO(ng)%DIC (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdDIC ) 
            SEDECO(ng)%O2  (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdO2  ) 
            SEDECO(ng)%CO2 (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdCO2 ) 
            SEDECO(ng)%N2  (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdN2  ) 
# if defined ORGANIC_MATTER
            SEDECO(ng)%DOCf(i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdDOCf) 
            SEDECO(ng)%DOCs(i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdDOCs) 
            SEDECO(ng)%POCf(i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOCf) 
            SEDECO(ng)%POCs(i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOCs) 
            SEDECO(ng)%POCn(i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOCn) 
# endif
# if defined NUTRIENTS
            SEDECO(ng)%NO3 (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdNO3 ) 
            SEDECO(ng)%NH4 (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdNH4 ) 
            SEDECO(ng)%PO4 (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPO4 ) 
#  if defined ORGANIC_MATTER
            SEDECO(ng)%DONf (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdDONf ) 
            SEDECO(ng)%DONs (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdDONs ) 
            SEDECO(ng)%PONf (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPONf ) 
            SEDECO(ng)%PONs (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPONs ) 
            SEDECO(ng)%PONn (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPONn ) 
            SEDECO(ng)%DOPf (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdDOPf ) 
            SEDECO(ng)%DOPs (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdDOPs ) 
            SEDECO(ng)%POPf (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOPf ) 
            SEDECO(ng)%POPs (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOPs ) 
            SEDECO(ng)%POPn (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOPn ) 
#  endif
# endif
# if defined SULFATE
            SEDECO(ng)%Mn2 (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdMn2 ) 
            SEDECO(ng)%MnO2(i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdMnO2) 
            SEDECO(ng)%Fe2 (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdFe2 ) 
            SEDECO(ng)%FeS (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeS ) 
            SEDECO(ng)%FeS2(i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeS2) 
            SEDECO(ng)%FeOOH    (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeOOH    ) 
            SEDECO(ng)%FeOOH_PO4(i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeOOH_PO4) 
            SEDECO(ng)%H2S (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdH2S )
            SEDECO(ng)%SO4 (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdSO4 )
            SEDECO(ng)%S0  (i,j,k) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdS0  )
# endif
          END DO
        END DO
      END DO
#endif
      END SUBROUTINE send_roms_his2reef_ecosys

!***********************************************************************

      SUBROUTINE send_reef_ecosys2roms_his(ng,LBi, UBi, LBj, UBj)
!
!=======================================================================
!                                                                      !
!  This routine sets reef_ecosys arraies to roms arralies for          !
!  his nc output.                                                      !
!  This routine is called in mod_arrays.F and only used for initial    !
!  condotion setting.                                                  !
!                                                                      !
!=======================================================================
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj
!
!  Local variable declarations.
!
      integer :: i,j,k
      integer :: isp

!-----------------------------------------------------------------------
      DO j=LBj, UBj
        DO i=LBi, UBi
#ifdef CORAL_POLYP
          DO isp=1,Ncl
            !  :  (To be updated)
            OCEAN(ng)%HisBio2d(i,j, iClTAcal (isp) ) = CORAL(ng)%TAcal(isp,i,j) 
            OCEAN(ng)%HisBio2d(i,j, iClTAcoe (isp) ) = CORAL(ng)%TAcoe(isp,i,j) 
            OCEAN(ng)%HisBio2d(i,j, iClDICcal(isp) ) = CORAL(ng)%DICcal(isp,i,j)
            OCEAN(ng)%HisBio2d(i,j, iClDICcoe(isp) ) = CORAL(ng)%DICcoe(isp,i,j)
            OCEAN(ng)%HisBio2d(i,j, iClDOcoe (isp) ) = CORAL(ng)%DOcoe(isp,i,j) 
            OCEAN(ng)%HisBio2d(i,j, iClQC    (isp) ) = CORAL(ng)%QC(isp,i,j)    
# if defined CORAL_NONE_CO2_EQ
            OCEAN(ng)%HisBio2d(i,j, iClCO2cal(isp) ) = CORAL(ng)%cCO2aqcal(isp,i,j)
            OCEAN(ng)%HisBio2d(i,j, iClCO2coe(isp) ) = CORAL(ng)%cCO2aqcoe(isp,i,j)
# endif
# if defined CORAL_CARBON_ISOTOPE
            OCEAN(ng)%HisBio2d(i,j, iClDI13Ccal(isp) ) = CORAL(ng)%DI13Ccal(isp,i,j)
            OCEAN(ng)%HisBio2d(i,j, iClDI13Ccoe(isp) ) = CORAL(ng)%DI13Ccoe(isp,i,j)
            OCEAN(ng)%HisBio2d(i,j, iClQ13C    (isp) ) = CORAL(ng)%Q13C (isp,i,j)   
#  if defined CORAL_NONE_CO2_EQ
            OCEAN(ng)%HisBio2d(i,j, iCl13CO2cal(isp) ) = CORAL(ng)%c13CO2aqcal(isp,i,j)
            OCEAN(ng)%HisBio2d(i,j, iCl13CO2coe(isp) ) = CORAL(ng)%c13CO2aqcoe(isp,i,j)
#  endif
# endif
# if defined CORAL_ZOOXANTHELLAE
            OCEAN(ng)%HisBio2d(i,j, iClROS(isp) ) = CORAL(ng)%ROS(isp,i,j) 
# endif
# if defined CORAL_NUTRIENTS
            !  :  (To be updated)
# endif     
# if defined CORAL_SIZE_DYNAMICS
            !  :  (To be updated)
# endif

# if defined CORAL_ZOOXANTHELLAE
            OCEAN(ng)%HisBio2d(i,j, iZxDns(isp) ) = ZOOX(ng)%dens(isp,i,j)
            OCEAN(ng)%HisBio2d(i,j, iZxQC (isp) ) = ZOOX(ng)%QC(isp,i,j)  
            OCEAN(ng)%HisBio2d(i,j, iZxChl(isp) ) = ZOOX(ng)%Chl(isp,i,j) 
            OCEAN(ng)%HisBio2d(i,j, iZxQAo(isp) ) = ZOOX(ng)%QAo(isp,i,j) 
            OCEAN(ng)%HisBio2d(i,j, iZxQAr(isp) ) = ZOOX(ng)%QAr(isp,i,j) 
            OCEAN(ng)%HisBio2d(i,j, iZxQAi(isp) ) = ZOOX(ng)%QAi(isp,i,j) 
            OCEAN(ng)%HisBio2d(i,j, iZxQAid(isp) )= ZOOX(ng)%QAid(isp,i,j)
#  if defined CORAL_CARBON_ISOTOPE
            OCEAN(ng)%HisBio2d(i,j, iZxQ13C(isp) ) = ZOOX(ng)%Q13C(isp,i,j) 
#  endif
#  if defined CORAL_NUTRIENTS
            !  :  (To be updated)
#  endif
# endif
          END DO
#endif
#ifdef SEAGRASS
          DO isp=1,Nsg
          !  :  (To be updated)
          END DO
#endif
#ifdef MACROALGAE
          DO isp=1,Nag
          !  :  (To be updated)
          END DO
#endif
        END DO
      END DO

#ifdef SEDIMENT_ECOSYS  
      DO k=1,Nsed(ng)
        DO j=LBj, UBj
          DO i=LBi, UBi
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdporo) = SEDECO(ng)%poro(i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdTmp ) = SEDECO(ng)%Tmp (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdSal ) = SEDECO(ng)%Sal (i,j,k)
            ! OCEAN(ng)%HisBiosed3d(i,j,k,iSdpH  ) = SEDECO(ng)%pH  (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdTA  ) = SEDECO(ng)%TA  (i,j,k)
            ! OCEAN(ng)%HisBiosed3d(i,j,k,iSdDIC ) = SEDECO(ng)%DIC (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdO2  ) = SEDECO(ng)%O2  (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdCO2 ) = SEDECO(ng)%CO2 (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdN2  ) = SEDECO(ng)%N2  (i,j,k)
# if defined ORGANIC_MATTER
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdDOCf) = SEDECO(ng)%DOCf(i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdDOCs) = SEDECO(ng)%DOCs(i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOCf) = SEDECO(ng)%POCf(i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOCs) = SEDECO(ng)%POCs(i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOCn) = SEDECO(ng)%POCn(i,j,k)
# endif
# if defined NUTRIENTS
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdNO3 ) = SEDECO(ng)%NO3 (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdNH4 ) = SEDECO(ng)%NH4 (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdPO4 ) = SEDECO(ng)%PO4 (i,j,k)
#  if defined ORGANIC_MATTER
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdDONf ) = SEDECO(ng)%DONf (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdDONs ) = SEDECO(ng)%DONs (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdPONf ) = SEDECO(ng)%PONf (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdPONs ) = SEDECO(ng)%PONs (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdPONn ) = SEDECO(ng)%PONn (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdDOPf ) = SEDECO(ng)%DOPf (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdDOPs ) = SEDECO(ng)%DOPs (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOPf ) = SEDECO(ng)%POPf (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOPs ) = SEDECO(ng)%POPs (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOPn ) = SEDECO(ng)%POPn (i,j,k)
#  endif
# endif
# if defined SULFATE
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdMn2 ) = SEDECO(ng)%Mn2 (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdMnO2) = SEDECO(ng)%MnO2(i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdFe2 ) = SEDECO(ng)%Fe2 (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeS ) = SEDECO(ng)%FeS (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeS2) = SEDECO(ng)%FeS2(i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeOOH    ) = SEDECO(ng)%FeOOH    (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeOOH_PO4) = SEDECO(ng)%FeOOH_PO4(i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdH2S ) = SEDECO(ng)%H2S (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdSO4 ) = SEDECO(ng)%SO4 (i,j,k)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdS0  ) = SEDECO(ng)%S0  (i,j,k)
# endif
          END DO
        END DO
      END DO
#endif
      END SUBROUTINE send_reef_ecosys2roms_his

!***********************************************************************
#if defined DIAGNOSTICS_BIO

      SUBROUTINE send_reef_ecosys2roms_dia(ng,LBi, UBi, LBj, UBj)
!
!=======================================================================
!                                                                      !
!  This routine sets reef_ecosys arraies to roms arralies for          !
!  his nc output.                                                      !
!  This routine is called in mod_arrays.F and only used for initial    !
!  condotion setting.                                                  !
!                                                                      !
!=======================================================================

      USE mod_geochem
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj
!
!  Local variable declarations.
!
      integer :: i,j,k
      integer :: isp
      real(r8) :: tmp

!-----------------------------------------------------------------------
      DO j=LBj, UBj
        DO i=LBi, UBi
# ifdef CORAL_POLYP
          DO isp=1,Ncl
            !  :  (To be updated)
            OCEAN(ng)%DiaBio2d(i,j, iClPg(isp) ) = CORAL(ng)%Pg(isp,i,j) 
            OCEAN(ng)%DiaBio2d(i,j, iCl_R(isp) ) = CORAL(ng)%R (isp,i,j) 
            OCEAN(ng)%DiaBio2d(i,j, iCl_G(isp) ) = CORAL(ng)%G (isp,i,j)
            OCEAN(ng)%DiaBio2d(i,j, iClPn(isp) ) = CORAL(ng)%Pg(isp,i,j)-CORAL(ng)%R (isp,i,j)
#  if defined CORAL_CARBON_ISOTOPE
            tmp = CORAL(ng)%Q13C(isp,i,j) / CORAL(ng)%QC(isp,i,j)   !coral organism
            OCEAN(ng)%DiaBio2d(i,j, iClQCd13C(isp) ) = d13C_fromR13C(tmp)
#   if defined CORAL_NONE_CO2_EQ
            !  :  (To be updated)
#   endif
#  endif
#  if defined CORAL_ZOOXANTHELLAE
            !  :  (To be updated)
#  endif
#  if defined CORAL_NUTRIENTS
            !  :  (To be updated)
#  endif     
#  if defined CORAL_SIZE_DYNAMICS
            OCEAN(ng)%DiaBio2d(i,j, iClmt(isp) ) = CORAL(ng)%mort(isp,i,j) 
            OCEAN(ng)%DiaBio2d(i,j, iClgw(isp) ) = CORAL(ng)%growth(isp,i,j) 
#  endif

#  if defined CORAL_ZOOXANTHELLAE
            !  :  (To be updated)
#   if defined CORAL_CARBON_ISOTOPE
            !  :  (To be updated)
#   endif
#   if defined CORAL_NUTRIENTS
            !  :  (To be updated)
#   endif
#  endif
          END DO
# endif

# ifdef SEAGRASS
          DO isp=1,Nsg
            OCEAN(ng)%DiaBio2d(i,j, iSgPg(isp) ) = SGRASS(ng)%Pg(isp,i,j)
            OCEAN(ng)%DiaBio2d(i,j, iSg_R(isp) ) = SGRASS(ng)%R (isp,i,j)
            OCEAN(ng)%DiaBio2d(i,j, iSgPn(isp) ) = SGRASS(ng)%Pg(isp,i,j)-SGRASS(ng)%R (isp,i,j)
          END DO
# endif

# ifdef MACROALGAE
          DO isp=1,Nsg
            OCEAN(ng)%DiaBio2d(i,j, iAgPg(isp) ) = ALGAE(ng)%Pg(isp,i,j)
            OCEAN(ng)%DiaBio2d(i,j, iAg_R(isp) ) = ALGAE(ng)%R (isp,i,j)
            OCEAN(ng)%DiaBio2d(i,j, iAgPn(isp) ) = ALGAE(ng)%Pg(isp,i,j)-ALGAE(ng)%R (isp,i,j)
          END DO
# endif
        END DO
      END DO

# ifdef SEDIMENT_ECOSYS  
      DO k=1,Nsed(ng)
        DO j=LBj, UBj
          DO i=LBi, UBi
            !  :  (To be updated)
#  if defined ORGANIC_MATTER
            !  :  (To be updated)
#  endif
#  if defined NUTRIENTS
            !  :  (To be updated)
#   if defined ORGANIC_MATTER
            !  :  (To be updated)
#   endif
#  endif
#  if defined SULFATE
            !  :  (To be updated)
#  endif
          END DO
        END DO
      END DO
# endif

      END SUBROUTINE send_reef_ecosys2roms_dia
#endif
