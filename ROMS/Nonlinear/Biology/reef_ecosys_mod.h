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
      USE mod_reef_ecosys_param
      USE mod_sedecosys
      USE mod_coral
      USE mod_seagrass
      USE mod_macroalgae
      USE mod_bivalve
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

      integer, allocatable :: idbio(:)  ! Biological tracers
      integer :: iDO                    ! Dissolved oxygen concentration
      integer :: iTA                    ! Total alkalinity
      integer :: iDIC(N_Csp)            ! Total inorganic carbon
      integer :: iNO3(N_Nsp)            ! NO3 concentration
      integer :: iNH4(N_Nsp)            ! NH4 concentration
      integer :: iPO4(N_Psp)            ! PO4 concentration
      integer :: iDOC(N_Csp,Ndom)       ! Dissolved organic C-concentration
      integer :: iPOC(N_Csp,Npom)       ! Particulate organic C-concentration
      integer :: iDON(N_Nsp,Ndom)       ! Dissolved organic N-concentration
      integer :: iPON(N_Nsp,Npom)       ! Particulate organic N-concentration
      integer :: iDOP(N_Psp,Ndom)       ! Dissolved organic P-concentration
      integer :: iPOP(N_Psp,Npom)       ! Particulate organic P-concentration
      integer :: iPhyC(N_Csp,Nphy)      ! Phytoplankton1 density
      integer :: iZooC(N_Csp,Nzoo)      ! Zooplankton density
      integer :: iPhyN(N_Nsp,Nphy)      ! Phytoplankton1 density
      integer :: iZooN(N_Nsp,Nzoo)      ! Zooplankton density
      integer :: iPhyP(N_Psp,Nphy)      ! Phytoplankton1 density
      integer :: iZooP(N_Psp,Nzoo)      ! Zooplankton density
      integer :: iPIC(N_Csp,Npim)       ! Particulate inorganic C-concentration
#if defined BLUE_TIDE         
      integer :: iH2S(N_Ssp)            ! H2S concentration
      integer :: iS0 (N_Ssp)            ! S0 concentration
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
      integer  :: iClDOcoe (Ncl)
      integer  :: iClTAcal (Ncl)
      integer  :: iClTAcoe (Ncl)
      integer  :: iClDICcal(N_Csp,Ncl)
      integer  :: iClDICcoe(N_Csp,Ncl)
      integer  :: iClQC    (N_Csp,Ncl)
# if defined CORAL_NONE_CO2_EQ
      integer  :: iClCO2cal(N_Csp,Ncl)
      integer  :: iClCO2coe(N_Csp,Ncl)
# endif
# ifdef CORAL_ZOOXANTHELLAE
      integer  :: iClROS(Ncl)
      integer  :: iZxDns(Ncl)
      integer  :: iZxQC (N_Csp,Ncl)
      integer  :: iZxChl(Ncl)
      integer  :: iZxQAo(Ncl)
      integer  :: iZxQAr(Ncl)
      integer  :: iZxQAi(Ncl)
      integer  :: iZxQAid(Ncl)
# endif
# ifdef CORAL_SIZE_DYNAMICS

# endif
#endif
#ifdef SEAGRASS
      integer  :: iSgSgCBm(N_Csp,Nsg)     ! Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) [umol.C m-2.sg.hab]
      integer  :: iSgSgNBm(N_Nsp,Nsg)     ! Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) [umol.C m-2.sg.hab]
      integer  :: iSgSgPBm(N_Psp,Nsg)     ! Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area) [umol.C m-2.sg.hab]
      integer  :: iSgDICstock(N_Csp,Nsg)  ! Seagrass internal DIC stock per unit ground area (in seagrass habitat area) [umol.C m-2.sg.hab]
      integer  :: iSgCH2Ostock(N_Csp,Nsg) ! Seagrass internal sugar stock per unit ground area (in seagrass habitat area) [umol.C m-2.sg.hab]
      integer  :: iSgDOstock(Nsg)         ! Seagrass internal DO stock per unit ground area (in seagrass habitat area) [umol.O m-2.sg.hab]
      integer  :: iSgNO3stock(N_Nsp,Nsg)  ! Seagrass internal NO3 stock per unit ground area (in seagrass habitat area) [umol.N m-2.sg.hab]
      integer  :: iSgNH4stock(N_Nsp,Nsg)  ! Seagrass internal NH4 stock per unit ground area (in seagrass habitat area) [umol.N m-2.sg.hab]
      integer  :: iSgPO4stock(N_Psp,Nsg)  ! Seagrass internal PO4 stock per unit ground area (in seagrass habitat area) [umol.P m-2.sg.hab]
      integer  :: iSgLfCBm(Nsg)           ! Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area) [umol.lf.C m-2.sg.hab]
      integer  :: iSgRtCBm(Nsg)           ! Seagrass root carbon-biomass per unit ground area (in seagrass habitat area) [umol.rt.C m-2.sg.hab]
      integer  :: iSgTotSgCBmS(Nsg)       ! SgTotSgCBm of last growth interval (For internal use only, need to save to restart file, but use SgTotSgCBm for output)
      integer  :: iSgTotSgCBm(Nsg)        ! Total seagrass carbon-biomass in grid [mol.C.tot]
      integer  :: iSgTotLfCBm(Nsg)        ! Total seagrass above ground carbon-biomass in grid [mol.C.lf]
      integer  :: iSgTotRtCBm(Nsg)        ! Total seagrass below ground carbon-biomass in grid [mol.C.rt]
      integer  :: iSgLAI(Nsg)             ! Leaf area index: one-sided green leaf area per unit ground surface area [m2.lf m-2.sg.hab]
      integer  :: iSgTotLA(Nsg)           ! Total one-sided green leaf area in grid [m2.lf]
      integer  :: iSgGridELAP(Nsg)        ! Effective leaf area projection on ground (whole grid) [m2.lf.proj m-2.grid]
      integer  :: iSgGridPhot(Nsg)        ! Seagrass gross photosynthesis rate per unit ground area (whole grid) [umol.C m-2.grid s-1]
      integer  :: iSgGridGrow(Nsg)        ! Seagrass growth rate per unit ground area (whole grid) [umol.C m-2.grid s-1]
      integer  :: iSgPhotLim(Nsg)         ! Seagrass photosynthesis limiting factor: 1 = light; 2 = Carbon stock
      integer  :: iSgGrowLim(Nsg)         ! Seagrass growth limiting factor: 1 = Sugar stock; 2 = Nitrogen stock; 3 = Phosphorus stock
      integer  :: iSgGridResp(Nsg)        ! Seagrass respiration rate per unit ground area (whole grid) [umol.C m-2.grid s-1]
      integer  :: iSgGridNetPhot(Nsg)     ! Seagrass net photosynthesis rate per unit ground area (whole grid) [umol.C m-2.grid s-1]
      integer  :: iSgGridDieoff(Nsg)      ! Seagrass dieoff rate carbon biomass per unit ground area (whole grid) [umol.C m-2.grid s-1]    
      integer  :: iSgInitC(Nsg)           ! Initial carbon remaining to destroy until net zero  [mmol.C]
      integer  :: iSgInitN(Nsg)           ! Initial nitrogen remaining to destroy until net zero  [mmol.N]
      integer  :: iSgInitP(Nsg)           ! Initial phosphorus remaining to destroy until net zero  [mmol.P]
#endif
#ifdef MACROALGAE
      integer  :: iAgQC(N_Csp,Nag)
      integer  :: iAgQN(N_Nsp,Nag)
      integer  :: iAgQP(N_Psp,Nag)
#endif
#ifdef BIVALVE
      integer  :: iBvDens(Nbv)
      integer  :: iBvQCe (N_Csp,Nbv)
      integer  :: iBvQCv (N_Csp,Nbv)
      integer  :: iBvQCr (N_Csp,Nbv)
      integer  :: iBvQCh (N_Csp,Nbv)
      integer  :: iBvQNe (N_Nsp,Nbv)
      integer  :: iBvQNv (N_Nsp,Nbv)
      integer  :: iBvQNr (N_Nsp,Nbv)
      integer  :: iBvQNh (N_Nsp,Nbv)
      integer  :: iBvQPe (N_Psp,Nbv)
      integer  :: iBvQPv (N_Psp,Nbv)
      integer  :: iBvQPr (N_Psp,Nbv)
      integer  :: iBvQPh (N_Psp,Nbv)
      integer  :: iBvCaCO3(N_Csp,Nbv)
# endif
!
!  Biological 3D Histrory variable IDs.
!
      integer, allocatable :: iHbio3(:)       ! 3D biological terms
!#ifdef CARBON_ISOTOPE
!      integer  :: id13C  ! d13C of total inorganic carbon  !!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Remove??
!#endif

#if defined DIAGNOSTICS_BIO
!
!  Biological 2D Diagnostic variable IDs.
!
      integer, allocatable :: iDbio2(:)       ! 2D biological terms

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
#  ifdef CARBON_ISOTOPE
      integer  :: iClQCd13C(Ncl)
      integer  :: iCl_Gd13C(Ncl)
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
# endif
# ifdef MACROALGAE
      integer  :: iAgPg(Nag)                  ! Algal gross photosynthesis rate
      integer  :: iAg_R(Nag)                  ! Algal respiration rate
      integer  :: iAgPn(Nag)                  ! Algal net photosynthesis rate
#  ifdef CARBON_ISOTOPE
      integer  :: iAgQCd13C(Nag)
#  endif
#  ifdef NITROGEN_ISOTOPE
      integer  :: iAgQNd15N(Nag)
#  endif
# endif
!
!  Biological 3D Diagnostic variable IDs.
!
      integer, allocatable :: iDbio3(:)       ! 3D biological terms
      integer  :: ipHt_                       ! pH (total scale)
      integer  :: iWarg                       ! aragonite saturation state
      integer  :: iWcal                       ! calcite saturation state
      integer :: iDOCTot(N_Csp)       ! Total Dissolved organic C-concentration
      integer :: iPOCTot(N_Csp)       ! Total Particulate organic C-concentration
      integer :: iDONTot(N_Nsp)       ! Total Dissolved organic N-concentration
      integer :: iPONTot(N_Nsp)       ! Total Particulate organic N-concentration
      integer :: iDOPTot(N_Psp)       ! Total Dissolved organic P-concentration
      integer :: iPOPTot(N_Psp)       ! Total Particulate organic P-concentration
      integer :: iPhyCTot(N_Csp)      ! Total Phytoplankton1 density
      integer :: iZooCTot(N_Csp)      ! Total Zooplankton density
      integer :: iPhyNTot(N_Nsp)      ! Total Phytoplankton1 density
      integer :: iZooNTot(N_Nsp)      ! Total Zooplankton density
      integer :: iPhyPTot(N_Psp)      ! Total Phytoplankton1 density
      integer :: iZooPTot(N_Psp)      ! Total Zooplankton density
      integer :: iPICTot(N_Csp)       ! Total Particulate inorganic C-concentration

!!! mons light model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>KM:Add
# ifdef LIGHT_MODEL
      integer  :: iLight                      ! photon flux density
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<KM:Add

# ifdef BIVALVE
      integer  :: iBvR (Nbv)
      integer  :: iBvG (Nbv)
      integer  :: iBvV (Nbv)
      integer  :: iBvL (Nbv)
      integer  :: iBvWd(Nbv)
      integer  :: iBvWw(Nbv)
      integer  :: iBvWt(Nbv)
# endif


# ifdef CARBON_ISOTOPE
      integer  :: iDICd13C                    ! d13C of total inorganic carbon
      integer  :: iDOCd13C(Ndom)              ! d13C of DOC
      integer  :: iPOCd13C(Npom)              ! d13C of POC
      integer  :: iPhyd13C(Nphy)              ! d13C of phytoplankton
      integer  :: iZood13C(Nzoo)              ! d13C of zooplankton
      integer  :: iPICd13C(Npim)              ! d13C of PIC
# endif
# ifdef NITROGEN_ISOTOPE
      integer  :: iNO3d15N                    ! d13C of NO3
      integer  :: iNH4d15N                    ! d13C of NH4
      integer  :: iDONd15N(Ndom)              ! d13C of DON
      integer  :: iPONd15N(Npom)              ! d13C of PON
      integer  :: iPhyd15N(Nphy)              ! d13C of phytoplankton
      integer  :: iZood15N(Nzoo)              ! d13C of zooplankton
# endif
#endif
!
!  Biological parameters.
!
      logical, allocatable :: LReadBioINI(:,:)       ! Switch to control reading of initial conditions from initial/restart nc file; 1 = foodweb (tracer variables); 2 = coral, seagass, macroalgae, sediment, etc.

      integer, allocatable :: CrlIter(:)
      integer, allocatable :: SedIter(:)

      real(r8), allocatable :: PARfrac(:)            ! nondimensional
      real(r8), allocatable :: pCO2air(:)            ! ppmv
      real(r8), allocatable :: DO_0 (:)              ! umol/L
      real(r8), allocatable :: TA_0 (:)              ! umol/kg
      real(r8), allocatable :: DIC_0(:)              ! umolC/kg
      real(r8), allocatable :: NO3_0(:)              ! umolN/L
      real(r8), allocatable :: NH4_0(:)              ! umolN/L
      real(r8), allocatable :: PO4_0(:)              ! umolP/L
      real(r8), allocatable :: DOC_0(:,:)            ! umolC/L
      real(r8), allocatable :: POC_0(:,:)            ! umolC/L
      real(r8), allocatable :: DON_0(:,:)            ! umolN/L
      real(r8), allocatable :: PON_0(:,:)            ! umolN/L
      real(r8), allocatable :: DOP_0(:,:)            ! umolP/L
      real(r8), allocatable :: POP_0(:,:)            ! umolP/L
      real(r8), allocatable :: PhyC_0(:,:)           ! umolC/L
      real(r8), allocatable :: ZooC_0(:,:)           ! umolC/L
      real(r8), allocatable :: PIC_0(:,:)            ! umolC/L
#if defined BLUE_TIDE         
      real(r8), allocatable :: H2S_0(:)              ! umol/L
      real(r8), allocatable :: S0_0 (:)              ! umol/L
#endif
#if defined CARBON_ISOTOPE
      real(r8), allocatable :: d13C_DIC_0(:)         ! permil (VPDB)
      real(r8), allocatable :: d13C_DOC_0(:,:)       ! permil (VPDB)
      real(r8), allocatable :: d13C_POC_0(:,:)       ! permil (VPDB)
      real(r8), allocatable :: d13C_PhyC_0(:,:)      ! permil (VPDB)
      real(r8), allocatable :: d13C_ZooC_0(:,:)      ! permil (VPDB)
      real(r8), allocatable :: d13C_PIC_0(:,:)       ! permil (VPDB)
#endif
#if defined NITROGEN_ISOTOPE
      real(r8), allocatable :: d15N_NO3_0(:)         ! permil
      real(r8), allocatable :: d15N_NH4_0(:)         ! permil
      real(r8), allocatable :: d15N_DON_0(:,:)       ! permil
      real(r8), allocatable :: d15N_PON_0(:,:)       ! permil
      real(r8), allocatable :: d15N_PhyN_0(:,:)      ! permil
      real(r8), allocatable :: d15N_ZooN_0(:,:)      ! permil
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
      integer :: iSdO2     !! Dissolved Oxygen    (µmol l-1)
      integer :: iSdTA     !! Total Alkalinity
      integer :: iSdDIC(N_Csp)    !! Dissolved inorganic carbon
      integer :: iSdNO3(N_Nsp)     !! Nitrate     (µmol l-1)
      integer :: iSdNH4(N_Nsp)     !! Ammonium    (µmol l-1)
      integer :: iSdPO4(N_Psp)     !! Phosphate   (µmol l-1)
      integer :: iSdDOC(N_Csp,Ndom)    !! Dissolved organic carbon (µmol l-1)
      integer :: iSdPOC(N_Csp,Npom)    !! Particulate organic carbon (nmol g-1)
      integer :: iSdDON(N_Nsp,Ndom)    !! Dissolved organic nitrogen     (fast :Labile)      (µmol l-1)
      integer :: iSdPON(N_Nsp,Npom)    !! Particulate organic nitrogen   (fast :Labile)      (µmol l-1)
      integer :: iSdDOP(N_Psp,Ndom)    !! Dissolved organic phosphorus   (fast :Labile)      (µmol l-1)
      integer :: iSdPOP(N_Psp,Npom)    !! Particulate organic phosphorus (fast :Labile)      (µmol l-1)
      integer :: iSdPIC(N_Csp)         !! CaCO3 (nmol g-1)
      integer :: iSdMn2       !! Manganese ion        (µmol l-1)
      integer :: iSdMnO2      !! Manganese dioxide    (nmol g-1)
      integer :: iSdFe2       !! iron(II)          (µmol l-1)
      integer :: iSdFeS (N_Ssp)      !! iron sulfide      (nmol g-1)
      integer :: iSdFeS2(N_Ssp)      !! pyrite            (nmol g-1)
      integer :: iSdFeOOH     !! iron hydroxide    (nmol g-1)
      integer :: iSdFeOOH_PO4(N_Psp) !! iron-bound phosphate (FeOOH=PO43-)   (nmol g-1)
      integer :: iSdH2S(N_Ssp)       !! hydrogen sulfide         (µmol l-1)
      integer :: iSdSO4(N_Ssp)       !! sulfate   (µmol l-1)
      integer :: iSdS0 (N_Ssp)       !! sulfur    (nmol g-1)
      integer :: iSdEfDO       !! DO  efflux (µmol m-2 s-1)
      integer :: iSdEfTA       !! TA  efflux (µmol m-2 s-1)
      integer :: iSdEfDIC      !! DIC efflux (µmol m-2 s-1)
      integer :: iSdEfNO3      !! NO3 efflux (µmol m-2 s-1)
      integer :: iSdEfNH4      !! NH4 efflux (µmol m-2 s-1)
      integer :: iSdEfPO4      !! PO4 efflux (µmol m-2 s-1)
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
      integer :: isp, m

!
!-----------------------------------------------------------------------
!  Initialize tracer identification indices.
!-----------------------------------------------------------------------
!
      ic=NAT+NPT+NCS+NNS

      i=1
      iDO=ic+i
      i=i+1
      iTA=ic+i
      DO isp=1,N_Csp     
        i=i+1
        iDIC(isp)=ic+i
      END DO       
      DO isp=1,N_Nsp
        i=i+1     
        iNO3(isp)=ic+i
      END DO       
      DO isp=1,N_Nsp
        i=i+1    
        iNH4(isp)=ic+i       
      END DO       
      DO isp=1,N_Psp     
        i=i+1    
        iPO4(isp)=ic+i       
      END DO
      DO m=1,Ndom    
        DO isp=1,N_Csp     
          i=i+1    
          iDOC(isp,m)=ic+i
        END DO
      END DO 
      DO m=1,Ndom    
        DO isp=1,N_Nsp     
          i=i+1    
          iDON(isp,m)=ic+i 
        END DO
      END DO 
      DO m=1,Ndom    
        DO isp=1,N_Psp     
          i=i+1    
          iDOP(isp,m)=ic+i  
        END DO
      END DO 
      DO m=1,Npom    
        DO isp=1,N_Csp     
          i=i+1    
          iPOC(isp,m)=ic+i  
        END DO
      END DO 
      DO m=1,Npom    
        DO isp=1,N_Nsp     
          i=i+1    
          iPON(isp,m)=ic+i  
        END DO
      END DO 
      DO m=1,Npom    
        DO isp=1,N_Psp     
          i=i+1    
          iPOP(isp,m)=ic+i  
        END DO
      END DO 
      DO m=1,Nphy    
        DO isp=1,N_Csp     
          i=i+1    
          iPhyC(isp,m)=ic+i
        END DO
      END DO 
      DO m=1,Nphy    
        DO isp=1,N_Nsp     
          i=i+1    
          iPhyN(isp,m)=ic+i
        END DO
      END DO 
      DO m=1,Nphy    
        DO isp=1,N_Psp     
          i=i+1    
          iPhyP(isp,m)=ic+i
        END DO
      END DO 
      DO m=1,Nzoo    
        DO isp=1,N_Csp     
          i=i+1    
          iZooC(isp,m)=ic+i
        END DO
      END DO 
      DO m=1,Nzoo    
        DO isp=1,N_Nsp     
          i=i+1    
          iZooN(isp,m)=ic+i
        END DO
      END DO 
      DO m=1,Nzoo    
        DO isp=1,N_Psp     
          i=i+1    
          iZooP(isp,m)=ic+i
        END DO
      END DO 
      DO m=1,Npim    
        DO isp=1,N_Csp     
          i=i+1    
          iPIC(isp,m)=ic+i 
        END DO
      END DO 
#if defined BLUE_TIDE 
      DO isp=1,N_Ssp
        i=i+1    
        iH2S(isp)=ic+i       
      END DO       
      DO isp=1,N_Ssp
        i=i+1    
        iS0(isp)=ic+i       
      END DO       
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
    !  write(*,*) 'NBT = ', NBT

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
      DO m=1,Ncl
        ic=ic+1
        iClDOcoe(m)=ic
      END DO
      DO m=1,Ncl
        ic=ic+1
        iClTAcal(m)=ic
      END DO
      DO m=1,Ncl
        ic=ic+1
        iClTAcoe(m)=ic
      END DO
      DO m=1,Ncl
        DO isp=1,N_Csp     
          ic=ic+1
          iClDICcal(isp,m)=ic
        END DO
      END DO
      DO m=1,Ncl
        DO isp=1,N_Csp     
          ic=ic+1
          iClDICcoe(isp,m)=ic
        END DO
      END DO
      DO m=1,Ncl
        DO isp=1,N_Csp     
          ic=ic+1
          iClQC(isp,m)=ic
        END DO
      END DO
# if defined CORAL_NONE_CO2_EQ
      DO m=1,Ncl
        DO isp=1,N_Csp     
          ic=ic+1
          iClCO2cal(isp,m)=ic
        END DO
      END DO
      DO m=1,Ncl
        DO isp=1,N_Csp     
          ic=ic+1
          iClCO2coe(isp,m)=ic
        END DO
      END DO
# endif
# ifdef CORAL_ZOOXANTHELLAE
      DO m=1,Ncl
        ic=ic+1
        iClROS(m)=ic
      END DO
      DO m=1,Ncl
        ic=ic+1
        iZxDns(m)=ic
      END DO
      DO m=1,Ncl
        DO isp=1,N_Csp     
          ic=ic+1
          iZxQC(isp,m)=ic
        END DO
      END DO
      DO m=1,Ncl
        ic=ic+1
        iZxChl(m)=ic
      END DO
      DO m=1,Ncl
        ic=ic+1
        iZxQAo(m)=ic
      END DO
      DO m=1,Ncl
        ic=ic+1
        iZxQAr(m)=ic
      END DO
      DO m=1,Ncl
        ic=ic+1
        iZxQAi(m)=ic
      END DO
      DO m=1,Ncl
        ic=ic+1
        iZxQAid(m)=ic
      END DO
# endif
#endif
#ifdef SEAGRASS
      DO m=1,Nsg
        DO isp=1,N_Csp     
          ic=ic+1
          iSgSgCBm(isp,m)=ic
        END DO
      END DO
      DO m=1,Nsg
        DO isp=1,N_Nsp     
          ic=ic+1
          iSgSgNBm(isp,m)=ic
        END DO
      END DO
      DO m=1,Nsg
        DO isp=1,N_Psp     
          ic=ic+1
          iSgSgPBm(isp,m)=ic
        END DO
      END DO
      DO m=1,Nsg
        DO isp=1,N_Csp     
          ic=ic+1
          iSgDICstock(isp,m)=ic
        END DO
      END DO
      DO m=1,Nsg
        DO isp=1,N_Csp     
          ic=ic+1
          iSgCH2Ostock(isp,m)=ic
        END DO
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgDOstock(m)=ic
      END DO
      DO m=1,Nsg
        DO isp=1,N_Nsp     
          ic=ic+1
          iSgNO3stock(isp,m)=ic
        END DO
      END DO
      DO m=1,Nsg
        DO isp=1,N_Nsp     
          ic=ic+1
          iSgNH4stock(isp,m)=ic
        END DO
      END DO
      DO m=1,Nsg
        DO isp=1,N_Psp     
          ic=ic+1
          iSgPO4stock(isp,m)=ic
        END DO
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgLfCBm(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgRtCBm(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgTotSgCBmS(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgTotSgCBm(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgTotLfCBm(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgTotRtCBm(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgLAI(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgTotLA(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgGridELAP(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgGridPhot(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgGridGrow(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgPhotLim(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgGrowLim(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgGridResp(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgGridNetPhot(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgGridDieoff(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgInitC(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgInitN(m)=ic
      END DO
      DO m=1,Nsg
        ic=ic+1
        iSgInitP(m)=ic
      END DO
#endif
#ifdef MACROALGAE
      DO m=1,Nag
        DO isp=1,N_Csp     
          ic=ic+1
          iAgQC(isp,m)=ic
        END DO
      END DO
      DO m=1,Nag
        DO isp=1,N_Nsp     
          ic=ic+1
          iAgQN(isp,m)=ic
        END DO
      END DO
      DO m=1,Nag
        DO isp=1,N_Psp     
          ic=ic+1
          iAgQP(isp,m)=ic
        END DO
      END DO
#endif
#ifdef SEDIMENT_ECOSYS  
      ic=ic+1
      iSdEfDO=ic
      ic=ic+1
      iSdEfTA=ic
      ic=ic+1
      iSdEfDIC=ic
      ic=ic+1
      iSdEfNO3=ic
      ic=ic+1
      iSdEfNH4=ic
      ic=ic+1
      iSdEfPO4=ic
#endif
!
!  Set number of 2D history terms.
!
      NHbio2d=ic
      ! write(*,*) 'NHbio2d = ', NHbio2d
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
#ifdef BIVALVE
      DO m=1,Nbv
        ic=ic+1
        iBvDens(m)=ic
      END DO
      DO m=1,Nbv
        DO isp=1,N_Csp     
          ic=ic+1
          iBvQCe(isp,m)=ic
        END DO
      END DO
      DO m=1,Nbv
        DO isp=1,N_Csp     
          ic=ic+1
          iBvQCv(isp,m)=ic
        END DO
      END DO
      DO m=1,Nbv
        DO isp=1,N_Csp     
          ic=ic+1
          iBvQCr(isp,m)=ic
        END DO
      END DO
      DO m=1,Nbv
        DO isp=1,N_Csp     
          ic=ic+1
          iBvQCh(isp,m)=ic
        END DO
      END DO
      DO m=1,Nbv
        DO isp=1,N_Nsp     
          ic=ic+1
          iBvQNe(isp,m)=ic
        END DO
      END DO
      DO m=1,Nbv
        DO isp=1,N_Nsp     
          ic=ic+1
          iBvQNv(isp,m)=ic
        END DO
      END DO
      DO m=1,Nbv
        DO isp=1,N_Nsp     
          ic=ic+1
          iBvQNr(isp,m)=ic
        END DO
      END DO
      DO m=1,Nbv
        DO isp=1,N_Nsp     
          ic=ic+1
          iBvQNh(isp,m)=ic
        END DO
      END DO
      DO m=1,Nbv
        DO isp=1,N_Psp     
          ic=ic+1
          iBvQPe(isp,m)=ic
        END DO
      END DO
      DO m=1,Nbv
        DO isp=1,N_Psp     
          ic=ic+1
          iBvQPv(isp,m)=ic
        END DO
      END DO
      DO m=1,Nbv
        DO isp=1,N_Psp     
          ic=ic+1
          iBvQPr(isp,m)=ic
        END DO
      END DO
      DO m=1,Nbv
        DO isp=1,N_Psp     
          ic=ic+1
          iBvQPh(isp,m)=ic
        END DO
      END DO
      DO m=1,Nbv
        DO isp=1,N_Csp     
          ic=ic+1
          iBvCaCO3(isp,m)=ic
        END DO
      END DO
#endif

!  Set number of 3D history terms.
!
      NHbio3d=ic
      ! write(*,*) 'NHbio3d = ', NHbio3d

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
      ic=ic+1
      iSdO2=ic
      ic=ic+1
      iSdTA=ic
      DO isp=1,N_Csp     
        ic=ic+1
        iSdDIC(isp)=ic
      END DO
      DO isp=1,N_Nsp     
        ic=ic+1
        iSdNO3(isp)=ic
      END DO
      DO isp=1,N_Nsp     
        ic=ic+1
        iSdNH4(isp)=ic
      END DO
      DO isp=1,N_Psp     
        ic=ic+1
        iSdPO4(isp)=ic
      END DO
      DO m=1,Ndom
        DO isp=1,N_Csp     
          ic=ic+1
          iSdDOC(isp,m)=ic
        END DO
      END DO
      DO m=1,Npom
        DO isp=1,N_Csp     
          ic=ic+1
          iSdPOC(isp,m)=ic
        END DO
      END DO
      DO m=1,Ndom
        DO isp=1,N_Nsp     
          ic=ic+1
          iSdDON(isp,m)=ic
        END DO
      END DO
      DO m=1,Npom
        DO isp=1,N_Nsp     
          ic=ic+1
          iSdPON(isp,m)=ic
        END DO
      END DO
      DO m=1,Ndom
        DO isp=1,N_Psp     
          ic=ic+1
          iSdDOP(isp,m)=ic
        END DO
      END DO
      DO m=1,Npom
        DO isp=1,N_Psp     
          ic=ic+1
          iSdPOP(isp,m)=ic
        END DO
      END DO  
      DO isp=1,N_Csp     
        ic=ic+1
        iSdPIC(isp)=ic
      END DO
      ic=ic+1
      iSdMn2=ic
      ic=ic+1
      iSdMnO2=ic
      ic=ic+1
      iSdFe2=ic
      DO isp=1,N_Ssp     
        ic=ic+1
        iSdFeS(isp)=ic
      END DO
      DO isp=1,N_Ssp     
        ic=ic+1
        iSdFeS2(isp)=ic
      END DO
      ic=ic+1
      iSdFeOOH=ic
      DO isp=1,N_Psp     
        ic=ic+1
        iSdFeOOH_PO4(isp)=ic
      END DO
      DO isp=1,N_Ssp     
        ic=ic+1
        iSdH2S(isp)=ic
      END DO
      DO isp=1,N_Ssp     
        ic=ic+1
        iSdSO4(isp)=ic
      END DO
      DO isp=1,N_Ssp     
        ic=ic+1
        iSdS0(isp)=ic
      END DO  
!
!  Set number of 3D biological sediment history terms.
!
      NHbiosed3d=ic
      ! write(*,*) 'NHbiosed3d = ', NHbiosed3d
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
      DO m=1,Ncl
        ic=ic+1
        iClPg(m)=ic
      END DO
      DO m=1,Ncl     
        ic=ic+1
        iCl_R(m)=ic
      END DO
      DO m=1,Ncl     
        ic=ic+1
        iCl_G(m)=ic
      END DO
      DO m=1,Ncl     
        ic=ic+1
        iClPn(m)=ic
      END DO
#  ifdef CARBON_ISOTOPE
      DO m=1,Ncl     
        ic=ic+1
        iClQCd13C(m)=ic
      END DO
      DO m=1,Ncl     
        ic=ic+1
        iCl_Gd13C(m)=ic
      END DO
#  endif
#  ifdef CORAL_ZOOXANTHELLAE
      DO m=1,Ncl     
        ic=ic+1
        iZxPg(m)=ic
      END DO
      DO m=1,Ncl     
        ic=ic+1
        iZx_R(m)=ic
      END DO
      DO m=1,Ncl     
        ic=ic+1
        iZxPn(m)=ic
      END DO
#  endif
#  ifdef CORAL_SIZE_DYNAMICS
      DO m=1,Ncl     
        ic=ic+1
        iClmt(m)=ic
      END DO
      DO m=1,Ncl     
        ic=ic+1
        iClgw(m)=ic
      END DO
#  endif
# endif
# ifdef SEAGRASS
      DO m=1,Nsg
      END DO
# endif
# ifdef MACROALGAE
      DO m=1,Nag
        ic=ic+1
        iAgPg(m)=ic
      END DO
      DO m=1,Nag
        ic=ic+1
        iAg_R(m)=ic
      END DO
      DO m=1,Nag
        ic=ic+1
        iAgPn(m)=ic
      END DO
#  ifdef CARBON_ISOTOPE
      DO m=1,Nag     
        ic=ic+1
        iAgQCd13C(m)=ic
      END DO
#  endif
#  ifdef NITROGEN_ISOTOPE
      DO m=1,Nag     
        ic=ic+1
        iAgQNd15N(m)=ic
      END DO
#  endif
# endif
!
!  Set number of 2D diagnostic terms.
!
      NDbio2d=ic
      ! write(*,*) 'NDbio2d = ', NDbio2d
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

      ic=ic+1
      ipHt_=ic
      ic=ic+1
      iWarg=ic
      ic=ic+1
      iWcal=ic

!!!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
      DO isp=1,N_Csp     
        ic=ic+1    
        iDOCTot(isp)=ic
      END DO
      DO isp=1,N_Csp     
        ic=ic+1    
        iPOCTot(isp)=ic
      END DO
      DO isp=1,N_Nsp     
        ic=ic+1    
        iDONTot(isp)=ic
      END DO
      DO isp=1,N_Nsp     
        ic=ic+1    
        iPONTot(isp)=ic
      END DO
      DO isp=1,N_Psp     
        ic=ic+1    
        iDOPTot(isp)=ic
      END DO
      DO isp=1,N_Psp     
        ic=ic+1    
        iPOPTot(isp)=ic
      END DO
      DO isp=1,N_Csp     
        ic=ic+1    
        iPhyCTot(isp)=ic
      END DO
      DO isp=1,N_Csp     
        ic=ic+1    
        iZooCTot(isp)=ic
      END DO
      DO isp=1,N_Nsp     
        ic=ic+1    
        iPhyNTot(isp)=ic
      END DO
      DO isp=1,N_Nsp     
        ic=ic+1    
        iZooNTot(isp)=ic
      END DO
      DO isp=1,N_Psp     
        ic=ic+1    
        iPhyPTot(isp)=ic
      END DO
      DO isp=1,N_Psp     
        ic=ic+1    
        iZooPTot(isp)=ic
      END DO
      DO isp=1,N_Csp     
        ic=ic+1    
        iPICTot(isp)=ic
      END DO
!!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

!!! mons light model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>KM:Add
# ifdef LIGHT_MODEL
      ic=ic+1
      iLight=ic
# endif
!!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<KM:Add

# ifdef CARBON_ISOTOPE
!      ic=ic+1
!      iDICd13C=ic  ! +1
!      DO m=1,Ndom
!        ic=ic+1
!        iDOCd13C(m)
!      END DO
!      DO m=1,Npom
!        ic=ic+1
!        iPOCd13C(m)
!      END DO
!      DO m=1,Nphy
!        ic=ic+1
!        iPhyd13C(m)
!      END DO
!      DO m=1,Nzoo
!        ic=ic+1
!        iZood13C(m)
!      END DO
!      DO m=1,Npim
!        ic=ic+1
!        iPICd13C(m)
!      END DO
# endif
# ifdef NITROGEN_ISOTOPE
!      ic=ic+1
!      iNO3d15N=ic  ! +1
!      ic=ic+1
!      iNH4d15N=ic  ! +1
!      DO m=1,Ndom
!        ic=ic+1
!        iDONd15N(m)
!      END DO
!      DO m=1,Npom
!        ic=ic+1
!        iPONd15N(m)
!      END DO
!      DO m=1,Nphy
!        ic=ic+1
!        iPhyd15N(m)
!      END DO
!      DO m=1,Nzoo
!        ic=ic+1
!        iZood15N(m)
!      END DO
# endif
#ifdef BIVALVE
      DO m=1,Nbv
        ic=ic+1
        iBvR (m)=ic
      END DO
      DO m=1,Nbv
        ic=ic+1
        iBvG (m)=ic
      END DO
      DO m=1,Nbv
        ic=ic+1
        iBvV (m)=ic
      END DO
      DO m=1,Nbv
        ic=ic+1
        iBvL (m)=ic
      END DO
      DO m=1,Nbv
        ic=ic+1
        iBvWd(m)=ic
      END DO
      DO m=1,Nbv
        ic=ic+1
        iBvWw(m)=ic
      END DO
      DO m=1,Nbv
        ic=ic+1
        iBvWt(m)=ic
      END DO
#endif

!  Set number of 3D diagnostic terms.
!
      NDbio3d=ic
      ! write(*,*) 'NDbio3d = ', NDbio3d


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
      IF (.not.allocated(DO_0)) THEN
        allocate ( DO_0(Ngrids) )
      END IF
      IF (.not.allocated(TA_0)) THEN
        allocate ( TA_0(Ngrids) )
      END IF
      IF (.not.allocated(DIC_0)) THEN
        allocate ( DIC_0(Ngrids) )
      END IF
      IF (.not.allocated(NO3_0)) THEN
        allocate ( NO3_0(Ngrids) )
      END IF
      IF (.not.allocated(NH4_0)) THEN
        allocate ( NH4_0(Ngrids) )
      END IF
      IF (.not.allocated(PO4_0)) THEN
        allocate ( PO4_0(Ngrids) )
      END IF
      IF (.not.allocated(DOC_0)) THEN
        allocate ( DOC_0(Ndom,Ngrids) )
      END IF
      IF (.not.allocated(POC_0)) THEN
        allocate ( POC_0(Npom,Ngrids) )
      END IF
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
      IF (.not.allocated(PhyC_0)) THEN
        allocate ( PhyC_0(Nphy,Ngrids) )
      END IF
      IF (.not.allocated(ZooC_0)) THEN
        allocate ( ZooC_0(Nzoo,Ngrids) )
      END IF
      IF (.not.allocated(PIC_0)) THEN
        allocate ( PIC_0(Npim,Ngrids) )
      END IF
#if defined BLUE_TIDE 
      IF (.not.allocated(H2S_0)) THEN
        allocate ( H2S_0(Ngrids) )
      END IF
      IF (.not.allocated(S0_0)) THEN
        allocate ( S0_0(Ngrids) )
      END IF
#endif
#if defined CARBON_ISOTOPE
      IF (.not.allocated(d13C_DIC_0)) THEN
        allocate ( d13C_DIC_0(Ngrids) )
      END IF
      IF (.not.allocated(d13C_DOC_0)) THEN
        allocate ( d13C_DOC_0(Ndom,Ngrids) )
      END IF
      IF (.not.allocated(d13C_POC_0)) THEN
        allocate ( d13C_POC_0(Npom,Ngrids) )
      END IF
      IF (.not.allocated(d13C_PhyC_0)) THEN
        allocate ( d13C_PhyC_0(Nphy,Ngrids) )
      END IF
      IF (.not.allocated(d13C_ZooC_0)) THEN
        allocate ( d13C_ZooC_0(Nzoo,Ngrids) )
      END IF
      IF (.not.allocated(d13C_PIC_0)) THEN
        allocate ( d13C_PIC_0(Npim,Ngrids) )
      END IF
#endif
#if defined NITROGEN_ISOTOPE
      IF (.not.allocated(d15N_NO3_0)) THEN
        allocate ( d15N_NO3_0(Ngrids) )
      END IF
      IF (.not.allocated(d15N_NH4_0)) THEN
        allocate ( d15N_NH4_0(Ngrids) )
      END IF
      IF (.not.allocated(d15N_DON_0)) THEN
        allocate ( d15N_DON_0(Ndom,Ngrids) )
      END IF
      IF (.not.allocated(d15N_PON_0)) THEN
        allocate ( d15N_PON_0(Npom,Ngrids) )
      END IF
      IF (.not.allocated(d15N_PhyN_0)) THEN
        allocate ( d15N_PhyN_0(Nphy,Ngrids) )
      END IF
      IF (.not.allocated(d15N_ZooN_0)) THEN
        allocate ( d15N_ZooN_0(Nzoo,Ngrids) )
      END IF
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

      SUBROUTINE send_roms_his2reef_ecosys(ng,LBi, UBi, LBj, UBj, N)
!
!=======================================================================
!                                                                      !
!  This routine sets reef_ecosys arrays to roms arrays for             !
!  his nc input.                                                       !
!  This routine is called in get_state.F and only used for initial     !
!  condition setting.                                                  !
!                                                                      !
!=======================================================================
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj, N
!
!  Local variable declarations.
!
      integer :: i,j,k
      integer :: isp,m

!-----------------------------------------------------------------------
      DO j=LBj, UBj
        DO i=LBi, UBi
#ifdef CORAL_POLYP
          DO m=1,Ncl
            !  :  (To be updated)
            CORAL(ng)%DOcoe(m,i,j)    = OCEAN(ng)%HisBio2d(i,j, iClDOcoe (m) )
            CORAL(ng)%TAcal(m,i,j)    = OCEAN(ng)%HisBio2d(i,j, iClTAcal (m) )
            CORAL(ng)%TAcoe(m,i,j)    = OCEAN(ng)%HisBio2d(i,j, iClTAcoe (m) )
            CORAL(ng)%DICcal(:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iClDICcal(1,m):iClDICcal(N_Csp,m) )
            CORAL(ng)%DICcoe(:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iClDICcoe(1,m):iClDICcoe(N_Csp,m) )
            CORAL(ng)%QC    (:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iClQC    (1,m):iClQC    (N_Csp,m) )
# if defined CORAL_NONE_CO2_EQ
            CORAL(ng)%CO2cal(:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iClCO2cal(1,m):iClCO2cal(N_Csp,m) )
            CORAL(ng)%CO2coe(:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iClCO2coe(1,m):iClCO2coe(N_Csp,m) )
# endif
# if defined CORAL_ZOOXANTHELLAE
            CORAL(ng)%ROS(m,i,j) = OCEAN(ng)%HisBio2d(i,j, iClROS(m) )
# endif
# if defined CORAL_NUTRIENTS
            !  :  (To be updated)
# endif     
# if defined CORAL_SIZE_DYNAMICS
            !  :  (To be updated)
# endif

# if defined CORAL_ZOOXANTHELLAE
            ZOOX(ng)%dens(m,i,j) = OCEAN(ng)%HisBio2d(i,j, iZxDns(m) )
            ZOOX(ng)%QC(:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iZxQC (1,m):iZxQC (N_Csp,m) )
            ZOOX(ng)%Chl (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iZxChl(m) )
            ZOOX(ng)%QAo (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iZxQAo(m) )
            ZOOX(ng)%QAr (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iZxQAr(m) )
            ZOOX(ng)%QAi (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iZxQAi(m) )
!            ZOOX(ng)%QAid(m,i,j) = OCEAN(ng)%HisBio2d(i,j, iZxQAid(m) )
#  if defined CORAL_NUTRIENTS
            !  :  (To be updated)
#  endif
# endif
          END DO
#endif

#ifdef SEAGRASS  
          DO m=1,Nsg
            SGRASS(ng)%SgCBmF   (:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgSgCBm(1,m):iSgSgCBm(N_Csp,m) )
            SGRASS(ng)%SgNBmF   (:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgSgNBm(1,m):iSgSgNBm(N_Nsp,m) )
            SGRASS(ng)%SgPBmF   (:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgSgPBm(1,m):iSgSgPBm(N_Psp,m) )
            SGRASS(ng)%DICstock (:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgDICstock(1,m):iSgDICstock(N_Csp,m) )
            SGRASS(ng)%CH2Ostock(:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgCH2Ostock(1,m):iSgCH2Ostock(N_Csp,m) )
            SGRASS(ng)%DOstock    (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgDOstock    (m) )
            SGRASS(ng)%NO3stock (:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgNO3stock(1,m):iSgNO3stock(N_Nsp,m) )
            SGRASS(ng)%NH4stock (:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgNH4stock(1,m):iSgNH4stock(N_Nsp,m) )
            SGRASS(ng)%PO4stock (:,m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgPO4stock(1,m):iSgPO4stock(N_Psp,m) )
            SGRASS(ng)%LfCBm      (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgLfCBm      (m) )
            SGRASS(ng)%RtCBm      (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgRtCBm      (m) )
            SGRASS(ng)%TotSgCBm   (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgTotSgCBmS  (m) )
            SGRASS(ng)%TotSgCBmF  (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgTotSgCBm   (m) )
            SGRASS(ng)%TotLfCBm   (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgTotLfCBm   (m) )
            SGRASS(ng)%TotRtCBm   (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgTotRtCBm   (m) )
            SGRASS(ng)%LAI        (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgLAI        (m) )
            SGRASS(ng)%TotLA      (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgTotLA      (m) )
            SGRASS(ng)%GridELAP   (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgGridELAP   (m) )
            SGRASS(ng)%GridPhot   (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgGridPhot   (m) )
            SGRASS(ng)%GridGrow   (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgGridGrow   (m) )
            SGRASS(ng)%PhotLim    (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgPhotLim    (m) )
            SGRASS(ng)%GridResp   (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgGridResp   (m) )
            SGRASS(ng)%GridNetPhot(m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgGridNetPhot(m) )
            SGRASS(ng)%GridDieoff (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgGridDieoff (m) )
            SGRASS(ng)%InitialC   (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgInitC      (m) )
            SGRASS(ng)%InitialN   (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgInitN      (m) )
            SGRASS(ng)%InitialP   (m,i,j) = OCEAN(ng)%HisBio2d(i,j, iSgInitP      (m) )
          END DO
#endif
#ifdef MACROALGAE  
          DO m=1,Nag
          !  :  (To be updated)
          END DO
#endif
        END DO
      END DO
!-----------------------------------------------------------------------
#ifdef BIVALVE
      DO j=LBj, UBj
        DO i=LBi, UBi
          DO k=1,N
            DO m=1,Nbv
              BVLV(ng)%dens(  m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvDens(m) )
              BVLV(ng)%QCe (:,m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvQCe(1,m):iBvQCe(N_Csp,m) )
              BVLV(ng)%QCv (:,m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvQCv(1,m):iBvQCv(N_Csp,m) )
              BVLV(ng)%QCr (:,m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvQCr(1,m):iBvQCr(N_Csp,m) )
              BVLV(ng)%QCh (:,m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvQCh(1,m):iBvQCh(N_Csp,m) )
              BVLV(ng)%QNe (:,m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvQNe(1,m):iBvQNe(N_Nsp,m) )
              BVLV(ng)%QNv (:,m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvQNv(1,m):iBvQNv(N_Nsp,m) )
              BVLV(ng)%QNr (:,m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvQNr(1,m):iBvQNr(N_Nsp,m) )
              BVLV(ng)%QNh (:,m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvQNh(1,m):iBvQNh(N_Nsp,m) )
              BVLV(ng)%QPe (:,m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvQPe(1,m):iBvQPe(N_Psp,m) )
              BVLV(ng)%QPv (:,m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvQPv(1,m):iBvQPv(N_Psp,m) )
              BVLV(ng)%QPr (:,m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvQPr(1,m):iBvQPr(N_Psp,m) )
              BVLV(ng)%QPh (:,m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvQPh(1,m):iBvQPh(N_Psp,m) )
              BVLV(ng)%CaCO3(:,m,k,i,j) = OCEAN(ng)%HisBio3d(i,j,k, iBvCaCO3(1,m):iBvCaCO3(N_Csp,m) )
            END DO
           !  :  (To be updated)
          END DO
        END DO
      END DO
#endif

!-----------------------------------------------------------------------

#ifdef SEDIMENT_ECOSYS  
      DO j=LBj, UBj
        DO i=LBi, UBi
          DO k=1,Nsed
            SEDECO(ng)%poro(k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdporo) 
            SEDECO(ng)%Tmp (k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdTmp ) 
            SEDECO(ng)%Sal (k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdSal ) 
            SEDECO(ng)%O2  (k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdO2  ) 
            SEDECO(ng)%TA  (k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdTA  ) 
            SEDECO(ng)%DIC (:,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdDIC(1):iSdDIC(N_Csp) ) 
            SEDECO(ng)%NO3 (:,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdNO3(1):iSdNO3(N_Nsp) ) 
            SEDECO(ng)%NH4 (:,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdNH4(1):iSdNH4(N_Nsp) ) 
            SEDECO(ng)%PO4 (:,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPO4(1):iSdPO4(N_Psp) ) 
            DO m=1,Ndom
              SEDECO(ng)%DOC (:,m,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdDOC(1,m):iSdDOC(N_Csp,m) )
              SEDECO(ng)%DON (:,m,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdDON(1,m):iSdDON(N_Nsp,m) ) 
              SEDECO(ng)%DOP (:,m,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdDOP(1,m):iSdDOP(N_Psp,m) ) 
            END DO 
            DO m=1,Npom
              SEDECO(ng)%POC (:,m,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOC(1,m):iSdPOC(N_Csp,m) ) 
              SEDECO(ng)%PON (:,m,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPON(1,m):iSdPON(N_Nsp,m) ) 
              SEDECO(ng)%POP (:,m,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOP(1,m):iSdPOP(N_Psp,m) ) 
            END DO 
            SEDECO(ng)%PIC (:,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdPIC(1):iSdPIC(N_Csp) ) 
            SEDECO(ng)%Mn2 (k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdMn2 ) 
            SEDECO(ng)%MnO2(k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdMnO2) 
            SEDECO(ng)%Fe2 (k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdFe2 ) 
            SEDECO(ng)%FeS (:,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeS (1):iSdFeS (N_Ssp)) 
            SEDECO(ng)%FeS2(:,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeS2(1):iSdFeS2(N_Ssp)) 
            SEDECO(ng)%FeOOH    (k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeOOH ) 
            SEDECO(ng)%FeOOH_PO4(:,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeOOH_PO4(1):iSdFeOOH_PO4(N_Psp)) 
            SEDECO(ng)%H2S (:,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdH2S(1):iSdH2S(N_Ssp) )
            SEDECO(ng)%SO4 (:,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdSO4(1):iSdSO4(N_Ssp) )
            SEDECO(ng)%S0  (:,k,i,j) = OCEAN(ng)%HisBiosed3d(i,j,k,iSdS0 (1):iSdS0 (N_Ssp) )
          END DO
          SEDECO(ng)%Sed_Efflux_DO (i,j) = OCEAN(ng)%HisBio2d(i,j,iSdEfDO)
          SEDECO(ng)%Sed_Efflux_TA (i,j) = OCEAN(ng)%HisBio2d(i,j,iSdEfTA)
          SEDECO(ng)%Sed_Efflux_DIC(i,j) = OCEAN(ng)%HisBio2d(i,j,iSdEfDIC)
          SEDECO(ng)%Sed_Efflux_NO3(i,j) = OCEAN(ng)%HisBio2d(i,j,iSdEfNO3)
          SEDECO(ng)%Sed_Efflux_NH4(i,j) = OCEAN(ng)%HisBio2d(i,j,iSdEfNH4)
          SEDECO(ng)%Sed_Efflux_PO4(i,j) = OCEAN(ng)%HisBio2d(i,j,iSdEfPO4)
        END DO
      END DO
#endif
      END SUBROUTINE send_roms_his2reef_ecosys

!***********************************************************************

      SUBROUTINE send_reef_ecosys2roms_his(ng,LBi, UBi, LBj, UBj, N)
!
!=======================================================================
!                                                                      !
!  This routine sets reef_ecosys arrays to roms arrays for             !
!  his nc output.                                                      !
!  This routine is called in mod_arrays.F and only used for initial    !
!  condition setting.                                                  !
!                                                                      !
!=======================================================================
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj, N
!
!  Local variable declarations.
!
      integer :: i,j,k,m
      integer :: isp

!-----------------------------------------------------------------------
      DO j=LBj, UBj
        DO i=LBi, UBi
#ifdef CORAL_POLYP
          DO m=1,Ncl
            !  :  (To be updated)
            OCEAN(ng)%HisBio2d(i,j, iClDOcoe (m) ) = CORAL(ng)%DOcoe(m,i,j) 
            OCEAN(ng)%HisBio2d(i,j, iClTAcal (m) ) = CORAL(ng)%TAcal(m,i,j) 
            OCEAN(ng)%HisBio2d(i,j, iClTAcoe (m) ) = CORAL(ng)%TAcoe(m,i,j) 
            OCEAN(ng)%HisBio2d(i,j, iClDICcal(1,m):iClDICcal(N_Csp,m) ) = CORAL(ng)%DICcal(:,m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iClDICcoe(1,m):iClDICcoe(N_Csp,m) ) = CORAL(ng)%DICcoe(:,m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iClQC    (1,m):iClQC    (N_Csp,m) ) = CORAL(ng)%QC    (:,m,i,j)    
# if defined CORAL_NONE_CO2_EQ
            OCEAN(ng)%HisBio2d(i,j, iClCO2cal(1,m):iClCO2cal(N_Csp,m) ) = CORAL(ng)%CO2cal(:,m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iClCO2coe(1,m):iClCO2coe(N_Csp,m) ) = CORAL(ng)%CO2coe(:,m,i,j)
# endif
# if defined CORAL_ZOOXANTHELLAE
            OCEAN(ng)%HisBio2d(i,j, iClROS(m) ) = CORAL(ng)%ROS(m,i,j) 
# endif
# if defined CORAL_NUTRIENTS
            !  :  (To be updated)
# endif     
# if defined CORAL_SIZE_DYNAMICS
            !  :  (To be updated)
# endif

# if defined CORAL_ZOOXANTHELLAE
            OCEAN(ng)%HisBio2d(i,j, iZxDns(m) ) = ZOOX(ng)%dens(m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iZxQC (1,m):iZxQC (N_Csp,m) ) = ZOOX(ng)%QC(:,m,i,j)  
            OCEAN(ng)%HisBio2d(i,j, iZxChl(m) ) = ZOOX(ng)%Chl(m,i,j) 
            OCEAN(ng)%HisBio2d(i,j, iZxQAo(m) ) = ZOOX(ng)%QAo(m,i,j) 
            OCEAN(ng)%HisBio2d(i,j, iZxQAr(m) ) = ZOOX(ng)%QAr(m,i,j) 
            OCEAN(ng)%HisBio2d(i,j, iZxQAi(m) ) = ZOOX(ng)%QAi(m,i,j) 
!            OCEAN(ng)%HisBio2d(i,j, iZxQAid(m) )= ZOOX(ng)%QAid(m,i,j)
#  if defined CARBON_ISOTOPE
            OCEAN(ng)%HisBio2d(i,j, iZxQ13C(m) ) = ZOOX(ng)%Q13C(m,i,j) 
#  endif
#  if defined CORAL_NUTRIENTS
            !  :  (To be updated)
#  endif
# endif
          END DO
#endif
#ifdef SEAGRASS
          DO m=1,Nsg
! # ifdef SEAGRASS_DEBUG_MODE
!             if(SGRASS(ng)%TotSgCBm   (m,i,j) /= SGRASS(ng)%TotSgCBm   (m,i,j)) then ! check for nan
!               write(*,*) 'yt_debug: TotSgCBm (m,i,j) = ', SGRASS(ng)%TotSgCBm   (m,i,j), 'm = ', m, 'i = ', i, 'j = ', j
!             endif
! # endif
            OCEAN(ng)%HisBio2d(i,j, iSgSgCBm(1,m):iSgSgCBm(N_Csp,m) ) = SGRASS(ng)%SgCBmF(:,m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgSgNBm(1,m):iSgSgNBm(N_Nsp,m) ) = SGRASS(ng)%SgNBmF(:,m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgSgPBm(1,m):iSgSgPBm(N_Psp,m) ) = SGRASS(ng)%SgPBmF(:,m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgDICstock(1,m):iSgDICstock(N_Csp,m) ) = SGRASS(ng)%DICstock(:,m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgCH2Ostock(1,m):iSgCH2Ostock(N_Csp,m) ) = SGRASS(ng)%CH2Ostock(:,m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgDOstock    (m) ) = SGRASS(ng)%DOstock (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgNO3stock(1,m):iSgNO3stock(N_Nsp,m) ) = SGRASS(ng)%NO3stock(:,m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgNH4stock(1,m):iSgNH4stock(N_Nsp,m) ) = SGRASS(ng)%NH4stock(:,m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgPO4stock(1,m):iSgPO4stock(N_Psp,m) ) = SGRASS(ng)%PO4stock(:,m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgLfCBm      (m) ) = SGRASS(ng)%LfCBm      (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgRtCBm      (m) ) = SGRASS(ng)%RtCBm      (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgTotSgCBmS  (m) ) = SGRASS(ng)%TotSgCBm   (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgTotSgCBm   (m) ) = SGRASS(ng)%TotSgCBmF  (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgTotLfCBm   (m) ) = SGRASS(ng)%TotLfCBm   (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgTotRtCBm   (m) ) = SGRASS(ng)%TotRtCBm   (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgLAI        (m) ) = SGRASS(ng)%LAI        (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgTotLA      (m) ) = SGRASS(ng)%TotLA      (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgGridELAP   (m) ) = SGRASS(ng)%GridELAP   (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgGridPhot   (m) ) = SGRASS(ng)%GridPhot   (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgGridGrow   (m) ) = SGRASS(ng)%GridGrow   (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgPhotLim    (m) ) = SGRASS(ng)%PhotLim    (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgGrowLim    (m) ) = SGRASS(ng)%GrowLim    (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgGridResp   (m) ) = SGRASS(ng)%GridResp   (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgGridNetPhot(m) ) = SGRASS(ng)%GridNetPhot(m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgGridDieoff (m) ) = SGRASS(ng)%GridDieoff (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgInitC      (m) ) = SGRASS(ng)%InitialC   (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgInitN      (m) ) = SGRASS(ng)%InitialN   (m,i,j)
            OCEAN(ng)%HisBio2d(i,j, iSgInitP      (m) ) = SGRASS(ng)%InitialP   (m,i,j)
          END DO
#endif
#ifdef MACROALGAE
          DO m=1,Nag
          !  :  (To be updated)
          END DO
#endif
        END DO
      END DO
!-----------------------------------------------------------------------
#ifdef BIVALVE
      DO j=LBj, UBj
        DO i=LBi, UBi
          DO k=1,N
            DO m=1,Nbv
              OCEAN(ng)%HisBio3d(i,j,k, iBvDens(m) )                  = BVLV(ng)%dens(  m,k,i,j)
              OCEAN(ng)%HisBio3d(i,j,k, iBvQCe(1,m):iBvQCe(N_Csp,m) ) = BVLV(ng)%QCe (:,m,k,i,j)
              OCEAN(ng)%HisBio3d(i,j,k, iBvQCv(1,m):iBvQCv(N_Csp,m) ) = BVLV(ng)%QCv (:,m,k,i,j)
              OCEAN(ng)%HisBio3d(i,j,k, iBvQCr(1,m):iBvQCr(N_Csp,m) ) = BVLV(ng)%QCr (:,m,k,i,j)
              OCEAN(ng)%HisBio3d(i,j,k, iBvQCh(1,m):iBvQCh(N_Csp,m) ) = BVLV(ng)%QCh (:,m,k,i,j)
              OCEAN(ng)%HisBio3d(i,j,k, iBvQNe(1,m):iBvQNe(N_Nsp,m) ) = BVLV(ng)%QNe (:,m,k,i,j)
              OCEAN(ng)%HisBio3d(i,j,k, iBvQNv(1,m):iBvQNv(N_Nsp,m) ) = BVLV(ng)%QNv (:,m,k,i,j)
              OCEAN(ng)%HisBio3d(i,j,k, iBvQNr(1,m):iBvQNr(N_Nsp,m) ) = BVLV(ng)%QNr (:,m,k,i,j)
              OCEAN(ng)%HisBio3d(i,j,k, iBvQNh(1,m):iBvQNh(N_Nsp,m) ) = BVLV(ng)%QNh (:,m,k,i,j)
              OCEAN(ng)%HisBio3d(i,j,k, iBvQPe(1,m):iBvQPe(N_Psp,m) ) = BVLV(ng)%QPe (:,m,k,i,j)
              OCEAN(ng)%HisBio3d(i,j,k, iBvQPv(1,m):iBvQPv(N_Psp,m) ) = BVLV(ng)%QPv (:,m,k,i,j)
              OCEAN(ng)%HisBio3d(i,j,k, iBvQPr(1,m):iBvQPr(N_Psp,m) ) = BVLV(ng)%QPr (:,m,k,i,j)
              OCEAN(ng)%HisBio3d(i,j,k, iBvQPh(1,m):iBvQPh(N_Psp,m) ) = BVLV(ng)%QPh (:,m,k,i,j)
              OCEAN(ng)%HisBio3d(i,j,k, iBvCaCO3(1,m):iBvCaCO3(N_Csp,m) ) = BVLV(ng)%CaCO3(:,m,k,i,j)
            END DO
           !  :  (To be updated)
          END DO
        END DO
      END DO
#endif

!-----------------------------------------------------------------------

#ifdef SEDIMENT_ECOSYS  
      DO j=LBj, UBj
        DO i=LBi, UBi
          DO k=1,Nsed
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdporo) = SEDECO(ng)%poro(k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdTmp ) = SEDECO(ng)%Tmp (k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdSal ) = SEDECO(ng)%Sal (k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdO2  ) = SEDECO(ng)%O2  (k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdTA  ) = SEDECO(ng)%TA  (k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdDIC(1):iSdDIC(N_Csp) ) = SEDECO(ng)%DIC (:,k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdNO3(1):iSdNO3(N_Nsp) ) = SEDECO(ng)%NO3 (:,k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdNH4(1):iSdNH4(N_Nsp) ) = SEDECO(ng)%NH4 (:,k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdPO4(1):iSdPO4(N_Psp) ) = SEDECO(ng)%PO4 (:,k,i,j)
            DO m=1,Ndom
              OCEAN(ng)%HisBiosed3d(i,j,k,iSdDOC(1,m):iSdDOC(N_Csp,m) ) = SEDECO(ng)%DOC (:,m,k,i,j)
              OCEAN(ng)%HisBiosed3d(i,j,k,iSdDON(1,m):iSdDON(N_Nsp,m) ) = SEDECO(ng)%DON (:,m,k,i,j)
              OCEAN(ng)%HisBiosed3d(i,j,k,iSdDOP(1,m):iSdDOP(N_Psp,m) ) = SEDECO(ng)%DOP (:,m,k,i,j)
            END DO 
            DO m=1,Npom
              OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOC(1,m):iSdPOC(N_Csp,m) ) = SEDECO(ng)%POC (:,m,k,i,j)
              OCEAN(ng)%HisBiosed3d(i,j,k,iSdPON(1,m):iSdPON(N_Nsp,m) ) = SEDECO(ng)%PON (:,m,k,i,j)
              OCEAN(ng)%HisBiosed3d(i,j,k,iSdPOP(1,m):iSdPOP(N_Psp,m) ) = SEDECO(ng)%POP (:,m,k,i,j)
            END DO 
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdPIC(1):iSdPIC(N_Csp) ) = SEDECO(ng)%PIC (:,k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdMn2 ) = SEDECO(ng)%Mn2 (k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdMnO2) = SEDECO(ng)%MnO2(k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdFe2 ) = SEDECO(ng)%Fe2 (k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeS (1):iSdFeS (N_Ssp)) = SEDECO(ng)%FeS (:,k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeS2(1):iSdFeS2(N_Ssp)) = SEDECO(ng)%FeS2(:,k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeOOH    ) = SEDECO(ng)%FeOOH    (k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdFeOOH_PO4(1):iSdFeOOH_PO4(N_Psp)) = SEDECO(ng)%FeOOH_PO4(:,k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdH2S(1):iSdH2S(N_Ssp) ) = SEDECO(ng)%H2S (:,k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdSO4(1):iSdSO4(N_Ssp) ) = SEDECO(ng)%SO4 (:,k,i,j)
            OCEAN(ng)%HisBiosed3d(i,j,k,iSdS0 (1):iSdS0 (N_Ssp) ) = SEDECO(ng)%S0  (:,k,i,j)
          END DO
          OCEAN(ng)%HisBio2d(i,j,iSdEfDO)  = SEDECO(ng)%Sed_Efflux_DO (i,j)
          OCEAN(ng)%HisBio2d(i,j,iSdEfTA)  = SEDECO(ng)%Sed_Efflux_TA (i,j)
          OCEAN(ng)%HisBio2d(i,j,iSdEfDIC) = SEDECO(ng)%Sed_Efflux_DIC(i,j)
          OCEAN(ng)%HisBio2d(i,j,iSdEfNO3) = SEDECO(ng)%Sed_Efflux_NO3(i,j)
          OCEAN(ng)%HisBio2d(i,j,iSdEfNH4) = SEDECO(ng)%Sed_Efflux_NH4(i,j)
          OCEAN(ng)%HisBio2d(i,j,iSdEfPO4) = SEDECO(ng)%Sed_Efflux_PO4(i,j)
        END DO
      END DO
#endif
      END SUBROUTINE send_reef_ecosys2roms_his

!***********************************************************************
#if defined DIAGNOSTICS_BIO

      SUBROUTINE send_reef_ecosys2roms_dia(ng,LBi, UBi, LBj, UBj, N)
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
      USE mod_diags
      USE mod_stepping, ONLY: nnew
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj, N
!
!  Local variable declarations.
!
      integer :: i,j,k
      integer :: isp,m
      real(r8) :: tmp

!-----------------------------------------------------------------------
      DO j=LBj, UBj
        DO i=LBi, UBi
# ifdef CORAL_POLYP
          DO m=1,Ncl
            !  :  (To be updated)
            DIAGS(ng)%DiaBio2d(i,j, iClPg(m) ) = CORAL(ng)%Pg(m,i,j) 
            DIAGS(ng)%DiaBio2d(i,j, iCl_R(m) ) = CORAL(ng)%R (m,i,j) 
            DIAGS(ng)%DiaBio2d(i,j, iCl_G(m) ) = CORAL(ng)%G (m,i,j)
            DIAGS(ng)%DiaBio2d(i,j, iClPn(m) ) = CORAL(ng)%Pg(m,i,j)-CORAL(ng)%R (m,i,j)
#  if defined CARBON_ISOTOPE
            tmp = CORAL(ng)%Q13C(m,i,j) / CORAL(ng)%QC(m,i,j)   !coral organism
            DIAGS(ng)%DiaBio2d(i,j, iClQCd13C(m) ) = d13C_fromR13C(tmp)
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
            DIAGS(ng)%DiaBio2d(i,j, iClmt(m) ) = CORAL(ng)%mort(m,i,j) 
            DIAGS(ng)%DiaBio2d(i,j, iClgw(m) ) = CORAL(ng)%growth(m,i,j) 
#  endif

#  if defined CORAL_ZOOXANTHELLAE
            !  :  (To be updated)
#   if defined CARBON_ISOTOPE
            !  :  (To be updated)
#   endif
#   if defined CORAL_NUTRIENTS
            !  :  (To be updated)
#   endif
#  endif
          END DO
# endif

# ifdef SEAGRASS
          DO m=1,Nsg
          END DO
# endif

# ifdef MACROALGAE
          DO m=1,Nag
            DIAGS(ng)%DiaBio2d(i,j, iAgPg(m) ) = ALGAE(ng)%Pg(m,i,j)
            DIAGS(ng)%DiaBio2d(i,j, iAg_R(m) ) = ALGAE(ng)%R (m,i,j)
            DIAGS(ng)%DiaBio2d(i,j, iAgPn(m) ) = ALGAE(ng)%Pg(m,i,j)-ALGAE(ng)%R (m,i,j)
          END DO
# endif
        END DO
      END DO
!-----------------------------------------------------------------------
#ifdef BIVALVE
      DO j=LBj, UBj
        DO i=LBi, UBi
          DO k=1,N
            DO m=1,Nbv
              DIAGS(ng)%DiaBio3d(i,j,k, iBvR (m) ) = BVLV(ng)%R (m,k,i,j)
              DIAGS(ng)%DiaBio3d(i,j,k, iBvG (m) ) = BVLV(ng)%G (m,k,i,j)
              DIAGS(ng)%DiaBio3d(i,j,k, iBvV (m) ) = BVLV(ng)%V (m,k,i,j)
              DIAGS(ng)%DiaBio3d(i,j,k, iBvL (m) ) = BVLV(ng)%L (m,k,i,j)
              DIAGS(ng)%DiaBio3d(i,j,k, iBvWd(m) ) = BVLV(ng)%Wd(m,k,i,j)
              DIAGS(ng)%DiaBio3d(i,j,k, iBvWw(m) ) = BVLV(ng)%Ww(m,k,i,j)
              DIAGS(ng)%DiaBio3d(i,j,k, iBvWt(m) ) = BVLV(ng)%Wt(m,k,i,j)
            END DO
           !  :  (To be updated)
          END DO
        END DO
      END DO
#endif

!-----------------------------------------------------------------------

# ifdef SEDIMENT_ECOSYS  
      DO k=1,Nsed
        DO j=LBj, UBj
          DO i=LBi, UBi
            !  :  (To be updated)
          END DO
        END DO
      END DO
# endif


!!!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
      DO j=LBj, UBj
        DO i=LBi, UBi
          DO isp=1,N_Csp
            DIAGS(ng)%DiaBio3d(i,j,:,iDOCTot(isp)) = 0
            DO m=1,Ndom    
              DIAGS(ng)%DiaBio3d(i,j,:,iDOCTot(isp)) &
              = DIAGS(ng)%DiaBio3d(i,j,:,iDOCTot(isp)) + OCEAN(ng)%t(i,j,:,nnew(ng),iDOC(isp,m))
            END DO
          END DO
          DO isp=1,N_Csp
            DIAGS(ng)%DiaBio3d(i,j,:,iPOCTot(isp)) = 0
            DO m=1,Npom    
              DIAGS(ng)%DiaBio3d(i,j,:,iPOCTot(isp)) &
              = DIAGS(ng)%DiaBio3d(i,j,:,iPOCTot(isp)) + OCEAN(ng)%t(i,j,:,nnew(ng),iPOC(isp,m))
            END DO
          END DO
          DO isp=1,N_Nsp
            DIAGS(ng)%DiaBio3d(i,j,:,iDONTot(isp)) = 0
            DO m=1,Ndom    
              DIAGS(ng)%DiaBio3d(i,j,:,iDONTot(isp)) &
              = DIAGS(ng)%DiaBio3d(i,j,:,iDONTot(isp)) + OCEAN(ng)%t(i,j,:,nnew(ng),iDON(isp,m))
            END DO
          END DO
          DO isp=1,N_Nsp
            DIAGS(ng)%DiaBio3d(i,j,:,iPONTot(isp)) = 0
            DO m=1,Npom    
              DIAGS(ng)%DiaBio3d(i,j,:,iPONTot(isp)) &
              = DIAGS(ng)%DiaBio3d(i,j,:,iPONTot(isp)) + OCEAN(ng)%t(i,j,:,nnew(ng),iPON(isp,m))
            END DO
          END DO
          DO isp=1,N_Psp
            DIAGS(ng)%DiaBio3d(i,j,:,iDOPTot(isp)) = 0
            DO m=1,Ndom    
              DIAGS(ng)%DiaBio3d(i,j,:,iDOPTot(isp)) &
              = DIAGS(ng)%DiaBio3d(i,j,:,iDOPTot(isp)) + OCEAN(ng)%t(i,j,:,nnew(ng),iDOP(isp,m))
            END DO
          END DO
          DO isp=1,N_Psp
            DIAGS(ng)%DiaBio3d(i,j,:,iPOPTot(isp)) = 0
            DO m=1,Npom    
              DIAGS(ng)%DiaBio3d(i,j,:,iPOPTot(isp)) &
              = DIAGS(ng)%DiaBio3d(i,j,:,iPOPTot(isp)) + OCEAN(ng)%t(i,j,:,nnew(ng),iPOP(isp,m))
            END DO
          END DO
          DO isp=1,N_Csp
            DIAGS(ng)%DiaBio3d(i,j,:,iPhyCTot(isp)) = 0
            DO m=1,Nphy    
              DIAGS(ng)%DiaBio3d(i,j,:,iPhyCTot(isp)) &
              = DIAGS(ng)%DiaBio3d(i,j,:,iPhyCTot(isp)) + OCEAN(ng)%t(i,j,:,nnew(ng),iPhyC(isp,m))
            END DO
          END DO
          DO isp=1,N_Csp
            DIAGS(ng)%DiaBio3d(i,j,:,iZooCTot(isp)) = 0
            DO m=1,Nzoo    
              DIAGS(ng)%DiaBio3d(i,j,:,iZooCTot(isp)) &
              = DIAGS(ng)%DiaBio3d(i,j,:,iZooCTot(isp)) + OCEAN(ng)%t(i,j,:,nnew(ng),iZooC(isp,m))
            END DO
          END DO
          DO isp=1,N_Nsp
            DIAGS(ng)%DiaBio3d(i,j,:,iPhyNTot(isp)) = 0
            DO m=1,Nphy    
              DIAGS(ng)%DiaBio3d(i,j,:,iPhyNTot(isp)) &
              = DIAGS(ng)%DiaBio3d(i,j,:,iPhyNTot(isp)) + OCEAN(ng)%t(i,j,:,nnew(ng),iPhyN(isp,m))
            END DO
          END DO
          DO isp=1,N_Nsp
            DIAGS(ng)%DiaBio3d(i,j,:,iZooNTot(isp)) = 0
            DO m=1,Nzoo    
              DIAGS(ng)%DiaBio3d(i,j,:,iZooNTot(isp)) &
              = DIAGS(ng)%DiaBio3d(i,j,:,iZooNTot(isp)) + OCEAN(ng)%t(i,j,:,nnew(ng),iZooN(isp,m))
            END DO
          END DO
          DO isp=1,N_Psp
            DIAGS(ng)%DiaBio3d(i,j,:,iPhyPTot(isp)) = 0
            DO m=1,Nphy    
              DIAGS(ng)%DiaBio3d(i,j,:,iPhyPTot(isp)) &
              = DIAGS(ng)%DiaBio3d(i,j,:,iPhyPTot(isp)) + OCEAN(ng)%t(i,j,:,nnew(ng),iPhyP(isp,m))
            END DO
          END DO
          DO isp=1,N_Psp
            DIAGS(ng)%DiaBio3d(i,j,:,iZooPTot(isp)) = 0
            DO m=1,Nzoo    
              DIAGS(ng)%DiaBio3d(i,j,:,iZooPTot(isp)) &
              = DIAGS(ng)%DiaBio3d(i,j,:,iZooPTot(isp)) + OCEAN(ng)%t(i,j,:,nnew(ng),iZooP(isp,m))
            END DO
          END DO
          DO isp=1,N_Csp
            DIAGS(ng)%DiaBio3d(i,j,:,iPICTot(isp)) = 0
            DO m=1,Npim    
              DIAGS(ng)%DiaBio3d(i,j,:,iPICTot(isp)) &
              = DIAGS(ng)%DiaBio3d(i,j,:,iPICTot(isp)) + OCEAN(ng)%t(i,j,:,nnew(ng),iPIC(isp,m))
            END DO
          END DO
        END DO
      END DO
!!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add


      END SUBROUTINE send_reef_ecosys2roms_dia
#endif

!!! yuta_edits >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
#ifdef REEF_ECOSYS
      ! yt_edit this subroutine wraps the logic for getting tile bounds and ROMS inputs with the actual initialize_reef_ecosys() call
      SUBROUTINE call_initialize_reef_ecosys_wrapper(ng, tile)
      
      USE mod_param,        ONLY : N
      USE mod_grid,         ONLY : GRID
      USE mod_reef_ecosys,  ONLY : initialize_reef_ecosys
# if defined AQUACULTURE && defined BIVALVE
      USE mod_aquaculture
# endif

      implicit none
      integer, intent(in) :: ng, tile
      integer :: i, j
      integer :: LBi, UBi, LBj, UBj
      !
# include "set_bounds.h"
      !
      !  Set array initialization range.
      !
      LBi=BOUNDS(ng)%LBi(tile)
      UBi=BOUNDS(ng)%UBi(tile)
      LBj=BOUNDS(ng)%LBj(tile)
      UBj=BOUNDS(ng)%UBj(tile)

      CALL initialize_reef_ecosys(ng, N(ng)                        &
          , GRID(ng)%Hz                                            &
          , IstrR, IendR, JstrR, JendR                                     &
          , .not. LReadBioINI(2,ng)                                &   ! TRUE = initialize coral, seagass, macroalgae, sediment from start; FALSE = continue from previous run
# if defined SEAGRASS || defined SEDIMENT_ECOSYS
          , GRID(ng)%om_r(IstrR:IendR,JstrR:JendR)                 &   ! grid size XI-direction (meters)
          , GRID(ng)%on_r(IstrR:IendR,JstrR:JendR)                 &   ! grid size ETA-direction (meters)
# endif
# ifdef SEAGRASS
          , GRID(ng)%p_sgrass(Nsg,IstrR:IendR,JstrR:JendR)         &   ! seagrass coverage (habitat area in grid / grid area)
# endif
          , GRID(ng)%p_sand(IstrR:IendR,JstrR:JendR)         &   ! sand coverage (sand area in grid / grid area)
# ifdef BIVALVE
          , GRID(ng)%dens_aqua                                     &   ! Aquaculture density
# endif
          )


      CALL send_reef_ecosys2roms_his (ng, LBi, UBi, LBj, UBj, N(ng))
      write(*,*) 'yt_debug: mod_reef_ecosys.F initialize_reef_ecosys() finished send_reef_ecosys2roms_his'
    
      END SUBROUTINE call_initialize_reef_ecosys_wrapper
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add
!***********************************************************************

      SUBROUTINE set_reef_ecosys_vertical_profile(ng, z_r, t)
!
!=======================================================================
!                                                                      !
!  This routine sets vertical profiles of biological parameters used   !
!  in reef_ecosys for analytically creating initial and boundary       !
!  conditions.                                                         !
!                                                                      !
!=======================================================================
!
      USE mod_scalars
      USE mod_geochem
      USE mod_reef_ecosys_param

!  Imported variable declarations.
!
      integer,  intent(in   ) :: ng
      real(r8), intent(in   ) :: z_r
      real(r8), intent(inout) :: t(NT(ng))
!
!  Local variable declarations.
!
      integer :: m
!
! Initialize all tracer values to be zero
!
      t(iDIC (1)  :iDIC (N_Csp)     ) = 0.0_r8
      t(iNO3 (1)  :iNO3 (N_Nsp)     ) = 0.0_r8
      t(iNH4 (1)  :iNH4 (N_Nsp)     ) = 0.0_r8
      t(iPO4 (1)  :iPO4 (N_Psp)     ) = 0.0_r8
      t(iDOC (1,1):iDOC (N_Csp,Ndom)) = 0.0_r8     ! umolC L-1
      t(iPOC (1,1):iPOC (N_Csp,Npom)) = 0.0_r8     ! umolC L-1
      t(iDON (1,1):iDON (N_Nsp,Ndom)) = 0.0_r8     ! umolN L-1
      t(iPON (1,1):iPON (N_Nsp,Npom)) = 0.0_r8     ! umolN L-1
      t(iDOP (1,1):iDOP (N_Psp,Ndom)) = 0.0_r8     ! umolP L-1
      t(iPOP (1,1):iPOP (N_Psp,Npom)) = 0.0_r8     ! umolP L-1
      t(iPhyC(1,1):iPhyC(N_Csp,Nphy)) = 0.0_r8     ! umolC L-1
      t(iZooC(1,1):iZooC(N_Csp,Nzoo)) = 0.0_r8     ! umolC L-1
      t(iPhyN(1,1):iPhyN(N_Nsp,Nphy)) = 0.0_r8     ! umolN L-1
      t(iZooN(1,1):iZooN(N_Nsp,Nzoo)) = 0.0_r8     ! umolN L-1
      t(iPhyP(1,1):iPhyP(N_Psp,Nphy)) = 0.0_r8     ! umolP L-1
      t(iZooP(1,1):iZooP(N_Psp,Nzoo)) = 0.0_r8     ! umolP L-1
      t(iPIC (1,1):iPIC (N_Csp,Npim)) = 0.0_r8     ! umolC L-1
# if defined BLUE_TIDE 
      t(iH2S (1)  :iH2S (N_Ssp)     ) = 0.0_r8
      t(iS0  (1)  :iS0  (N_Ssp)     ) = 0.0_r8
# endif

    ! TA  
      t(iTA) = TA_Profile (t(iTemp), t(iSalt), z_r)
    ! DIC  
      t(iDIC(iCt)) = DIC_Profile(t(iTemp), t(iSalt), z_r)
    ! DO  
      t(iDO) = DO_Profile (t(iTemp), t(iSalt), z_r)
    ! NO3  
      t(iNO3(iNt)) = NO3_Profile(t(iTemp), t(iSalt), z_r)
    ! NH4   
      t(iNH4(iNt)) = NH4_0(ng)     ! umol L-1
    ! PO4
      t(iPO4(iPt)) = PO4_Profile(t(iTemp), t(iSalt), z_r)
    ! DOC
      DO m=1,Ndom
        t(iDOC(iCt,m)) = DOC_Profile(t(iTemp), t(iSalt), z_r, m)
      END DO
    ! POC
      DO m=1,Npom
        t(iPOC(iCt,m)) = POC_Profile(t(iTemp), t(iSalt), z_r, m)
      END DO
    ! DON
      DO m=1,Ndom
        t(iDON(iNt,m)) = t(iDOC(iCt,m))*16.0_r8/106.0_r8
      END DO
    ! PON
      DO m=1,Npom
        t(iPON(iNt,m)) = t(iPOC(iCt,m))*16.0_r8/106.0_r8
      END DO
    ! DOP
      DO m=1,Ndom
        t(iDOP(iPt,m)) = t(iDOC(iCt,m))/106.0_r8
      END DO
    ! POP
      DO m=1,Npom
        t(iPOP(iPt,m)) = t(iPOC(iCt,m))/106.0_r8
      END DO
    ! PhyC        
      DO m=1,Nphy
        t(iPhyC(iCt,m))  = PhyC_Profile(t(iTemp), t(iSalt), z_r, m)
      END DO
    ! ZooC
      DO m=1,Nzoo
        t(iZooC(iCt,m)) = ZooC_Profile(t(iTemp), t(iSalt), z_r, m)
      END DO
      ! PhyN        
      DO m=1,Nphy
        t(iPhyN(iNt,m)) = t(iPhyC(iCt,m))*rNCp(m)
      END DO
    ! ZooN
      DO m=1,Nzoo
        t(iZooN(iNt,m)) = t(iZooC(iCt,m))*rNCz(m)
      END DO
    ! PhyP      
      DO m=1,Nphy
        t(iPhyP(iPt,m)) = t(iPhyC(iCt,m))*rPCp(m)
      END DO
    ! ZooP
      DO m=1,Nzoo
        t(iZooP(iPt,m)) = t(iZooC(iCt,m))*rPCz(m)
      END DO
    ! PIC
      t(iPIC(iCt,iLive)) = t(iPhyC(iCt,iCcl))*rCaCp(iCcl) ! PIC_0(iLive,ng)     ! umolC L-1
      DO m=2,Npim
        t(iPIC(iCt,m)) = PIC_0(m,ng)     ! umolC L-1
      END DO
# if defined BLUE_TIDE 
      t(iH2S(iSt)) = H2S_0(ng)
      t(iS0 (iSt)) = S0_0 (ng)
# endif
# if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
      t(iDIC(iC13)) = Ci_from_Ct_delta(t(iDIC(iCt)),   d13C_DIC_0(ng), R13C_VPDB )                    &
      DO m=1,Ndom
        t(iDOC(iC13,m)) = Ci_from_Ct_delta(t(iDOC(iCt,m)), d13C_DOC_0(m,ng), R13C_VPDB )                    &
      END DO
      DO m=1,Npom
        t(iPOC(iC13,m)) = Ci_from_Ct_delta(t(iPOC(iCt,m)), d13C_POC_0(m,ng), R13C_VPDB )                    &
      END DO
      DO m=1,Nphy
        t(iPhyC(iC13,m)) = Ci_from_Ct_delta(t(iPhyC(iCt,m)), d13C_PhyC_0(m,ng), R13C_VPDB )                    &
      END DO
      DO m=1,Nzoo
        t(iZooC(iC13,m)) = Ci_from_Ct_delta(t(iZooC(iCt,m)), d13C_ZooC_0(m,ng), R13C_VPDB )                    &
      END DO
      DO m=1,Npim
        t(iPIC(iC13,m)) = Ci_from_Ct_delta(t(iPIC(iCt,m)), d13C_PIC_0(m,ng), R13C_VPDB )                    &
      END DO
#  if defined CLUMPED_ISOTOPE
!**************** Under developpment *************************
      t(iDIC(iD47)) = Ci_from_Ct_delta(t(iDIC(iCt)),   D47_DIC_0(ng), R47D_???? )                    &
      DO m=1,Ndom
        t(iDOC(iD47,m)) = Ci_from_Ct_delta(t(iDOC(iCt,m)), D47_DOC_0(m,ng), R47D_???? )                    &
      END DO
      DO m=1,Npom
        t(iPOC(iD47,m)) = Ci_from_Ct_delta(t(iPOC(iCt,m)), D47_POC_0(m,ng), R47D_???? )                    &
      END DO
      DO m=1,Nphy
        t(iPhyC(iD47,m)) = Ci_from_Ct_delta(t(iPhyC(iCt,m)), D47_PhyC_0(m,ng), R47D_???? )                    &
      END DO
      DO m=1,Nzoo
        t(iZooC(iD47,m)) = Ci_from_Ct_delta(t(iZooC(iCt,m)), D47_ZooC_0(m,ng), R47D_???? )                    &
      END DO
      DO m=1,Npim
        t(iPIC(iD47,m)) = Ci_from_Ct_delta(t(iPIC(iCt,m)), D47_PIC_0(m,ng), R47D_???? )                    &
      END DO
#  endif
# endif
# if defined NITROGEN_ISOTOPE
      t(iNO3(iN15)) = Ci_from_Ct_delta(t(iNO3(iNt)),   d15N_NO3_0(ng), R15N_AIR )                    &
      t(iNH4(iN15)) = Ci_from_Ct_delta(t(iNH4(iNt)),   d15N_NH4_0(ng), R15N_AIR )                    &
      DO m=1,Ndom
        t(iDON(iN15,m)) = Ci_from_Ct_delta(t(iDON(iNt,m)), d15N_DON_0(m,ng), R15N_AIR )                    &
      END DO
      DO m=1,Npom
        t(iPON(iN15,m)) = Ci_from_Ct_delta(t(iPON(iNt,m)), d15N_PON_0(m,ng), R15N_AIR )                    &
      END DO
      DO m=1,Nphy
        t(iPhyN(iN15,m)) = Ci_from_Ct_delta(t(iPhyN(iNt,m)), d15N_PhyN_0(m,ng), R15N_AIR )                    &
      END DO
      DO m=1,Nzoo
        t(iZooN(iN15,m)) = Ci_from_Ct_delta(t(iZooN(iNt,m)), d15N_ZooN_0(m,ng), R15N_AIR )                    &
      END DO
# endif
# if defined COT_STARFISH
      t(iCOTe)=COTe0(ng)     ! umolC L-1
      t(iCOTl)=COTl0(ng)     ! umolC L-1
# endif


!-----------------------------------------------------------------------
      END SUBROUTINE set_reef_ecosys_vertical_profile

!***********************************************************************

