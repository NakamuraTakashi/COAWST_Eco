      SUBROUTINE biology (ng,tile)

!!!=== Copyright (c) 2012-2020 Takashi NAKAMURA  =====
!***********************************************************************
!  References:                                                         !
!                                                                      !
!    Nakamura T, Nadaoka K, Watanabe A, Yamamoto T, Miyajima T,        !
!     Blanco AC (2018) Reef-scale modeling of coral calcification      !
!     responses to ocean acidification and sea-level rise.             !
!     Coral Reefs 37:37–53. doi: 10.1007/s00338-017-1632-3.            !
!                                                                      !
!    Nakamura T, Nadaoka K, Watanabe A (2013) A coral polyp model of   !
!     photosynthesis, respiration and calcification incorporating      !
!     a transcellular ion transport mechanism.                         !
!     Coral Reefs 32:779-794. doi: 10.1007/s00338-013-1032-2           !
!                                                                      !
!***********************************************************************
!
      USE mod_param
      USE mod_forces
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping
#ifdef BBL_MODEL
      USE mod_bbl
#endif


!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
#include "tile.h"
!
!  Set header file name.
!
#ifdef DISTRIBUTE
      IF (Lbiofile(iNLM)) THEN
#else
      IF (Lbiofile(iNLM).and.(tile.eq.0)) THEN
#endif
        Lbiofile(iNLM)=.FALSE.
        BIONAME(iNLM)=__FILE__
      END IF
!
#ifdef PROFILE
      CALL wclock_on (ng, iNLM, 15)
#endif

      CALL biology_tile (ng, tile,                                      &
     &                   LBi, UBi, LBj, UBj, N(ng), NT(ng),             &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
     &                   nstp(ng), nnew(ng),                            &
#ifdef MASKING
     &                   GRID(ng) % rmask,                              &
# if defined WET_DRY && defined DIAGNOSTICS_BIO
     &                   GRID(ng) % rmask_full,                         &
# endif
#endif
     &                   GRID(ng) % Hz,                                 &
     &                   GRID(ng) % z_r,                                &
     &                   GRID(ng) % z_w,                                &
     &                   FORCES(ng) % srflx,                            &
#ifdef BULK_FLUXES
     &                   FORCES(ng) % Uwind,                            &
     &                   FORCES(ng) % Vwind,                            &
#else
     &                   FORCES(ng) % sustr,                            &
     &                   FORCES(ng) % svstr,                            &
#endif
     &                   OCEAN(ng) % HisBio2d,                          &
     &                   OCEAN(ng) % HisBio3d,                          &
#ifdef CORAL_POLYP
     &                   GRID(ng) % p_coral,                            &
#endif
#ifdef SEAGRASS
     &                   GRID(ng) % p_seagrass,                         &
#endif
#ifdef MACROALGAE
     &                   GRID(ng) % p_algae,                            &
#endif
#ifdef SEDIMENT_ECOSYS
     &                   GRID(ng) % p_sand,                             &
#endif
#ifdef BBL_MODEL
     &                   BBL(ng) % bustrc,                              &
     &                   BBL(ng) % bvstrc,                              &
     &                   BBL(ng) % bustrw,                              &
     &                   BBL(ng) % bvstrw,                              &
     &                   BBL(ng) % bustrcwmax,                          &
     &                   BBL(ng) % bvstrcwmax,                          &
# ifdef SSW_BBL
     &                   BBL(ng) % bstrcwave,                           &
# endif
#else
     &                   FORCES(ng) % bustr,                            &
     &                   FORCES(ng) % bvstr,                            &
#endif
     &                   OCEAN(ng) % t)


#ifdef PROFILE
      CALL wclock_off (ng, iNLM, 15)
#endif

      RETURN
      END SUBROUTINE biology
!
!-----------------------------------------------------------------------
      SUBROUTINE biology_tile (ng, tile,                                &
     &                         LBi, UBi, LBj, UBj, UBk, UBt,            &
     &                         IminS, ImaxS, JminS, JmaxS,              &
     &                         nstp, nnew,                              &
#ifdef MASKING
     &                         rmask,                                   &
# if defined WET_DRY && defined DIAGNOSTICS_BIO
     &                         rmask_full,                              &
# endif
#endif
     &                         Hz, z_r, z_w, srflx,                     &
#ifdef BULK_FLUXES
     &                         Uwind, Vwind,                            &
#else
     &                         sustr, svstr,                            &
#endif
     &                         HisBio2d, HisBio3d,                      &
#ifdef CORAL_POLYP
     &                         p_coral,                                 &
#endif
#ifdef SEAGRASS
     &                         p_seagrass,                              &
#endif
#ifdef MACROALGAE
     &                         p_algae,                                 &
#endif
#ifdef SEDIMENT_ECOSYS
     &                         p_sand,                                  &
#endif
#ifdef BBL_MODEL
     &                         bustrc, bvstrc,                          &
     &                         bustrw, bvstrw,                          &
     &                         bustrcwmax, bvstrcwmax,                  &
# ifdef SSW_BBL
     &                         bstrcwave,                               &
# endif
#else
     &                         bustr, bvstr,                            &
#endif
     &                         t)
!-----------------------------------------------------------------------
!
      USE mod_param
      USE mod_biology
      USE mod_ncparam
      USE mod_scalars
      
      USE mod_reef_ecosys  !!!<<<<<<<<<<<<<<<< Reef ecosystem model
      USE mod_geochem
#ifdef CORAL_POLYP
      USE mod_coral
#endif
#ifdef MACROALGAE
      USE mod_macroalgae
#endif
#ifdef SEAGRASS
      USE mod_seagrass
#endif
#ifdef SEDIMENT_ECOSYS
      USE mod_sedecosys
#endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, UBk, UBt
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp, nnew

#ifdef ASSUMED_SHAPE
# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#  if defined WET_DRY && defined DIAGNOSTICS_BIO
      real(r8), intent(in) :: rmask_full(LBi:,LBj:)
#  endif
# endif
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in) :: srflx(LBi:,LBj:)
# ifdef BULK_FLUXES
      real(r8), intent(in) :: Uwind(LBi:,LBj:)
      real(r8), intent(in) :: Vwind(LBi:,LBj:)
# else
      real(r8), intent(in) :: sustr(LBi:,LBj:)
      real(r8), intent(in) :: svstr(LBi:,LBj:)
# endif
      real(r8), intent(inout) :: HisBio2d(LBi:,LBj:,:)
      real(r8), intent(inout) :: HisBio3d(LBi:,LBj:,:,:)
# ifdef CORAL_POLYP
      real(r8), intent(inout) :: p_coral(2,LBi:UBi,LBj:UBj)
# endif
# ifdef SEAGRASS
      real(r8), intent(inout) :: p_seagrass(LBi:UBi,LBj:UBj)
# endif
# ifdef MACROALGAE
      real(r8), intent(inout) :: p_algae(LBi:UBi,LBj:UBj)
# endif
# ifdef SEDIMENT_ECOSYS
      real(r8), intent(inout) :: p_sand(LBi:UBi,LBj:UBj)
# endif
# ifdef BBL_MODEL
      real(r8), intent(in) :: bustrc(LBi:,LBj:)
      real(r8), intent(in) :: bvstrc(LBi:,LBj:)
      real(r8), intent(in) :: bustrw(LBi:,LBj:)
      real(r8), intent(in) :: bvstrw(LBi:,LBj:)
      real(r8), intent(in) :: bustrcwmax(LBi:,LBj:)
      real(r8), intent(in) :: bvstrcwmax(LBi:,LBj:)
#   ifdef SSW_BBL
      real(r8), intent(in) :: bstrcwave(LBi:,LBj:)
#   endif
#  else
      real(r8), intent(in) :: bustr(LBi:,LBj:)
      real(r8), intent(in) :: bvstr(LBi:,LBj:)
#  endif
      real(r8), intent(inout) :: t(LBi:,LBj:,:,:,:)

#else
# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#  if defined WET_DRY && defined DIAGNOSTICS_BIO
      real(r8), intent(in) :: rmask_full(LBi:UBi,LBj:UBj)
#  endif
# endif
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:UBk)
      real(r8), intent(in) :: srflx(LBi:UBi,LBj:UBj)
# ifdef BULK_FLUXES
      real(r8), intent(in) :: Uwind(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Vwind(LBi:UBi,LBj:UBj)
# else
      real(r8), intent(in) :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: svstr(LBi:UBi,LBj:UBj)
# endif
      real(r8), intent(inout) :: HisBio2d(LBi:UBi,LBj:UBj,NHbio2d)
      real(r8), intent(inout) :: HisBio3d(LBi:UBi,LBj:UBj,UBk,NHbio3d)
# ifdef CORAL_POLYP
      real(r8), intent(inout) :: p_coral(2,LBi:UBi,LBj:UBj)
# endif
# ifdef SEAGRASS
      real(r8), intent(inout) :: p_seagrass(LBi:UBi,LBj:UBj)
# endif
# ifdef MACROALGAE
      real(r8), intent(inout) :: p_algae(LBi:UBi,LBj:UBj)
# endif
# ifdef SEDIMENT_ECOSYS
      real(r8), intent(inout) :: p_sand(LBi:UBi,LBj:UBj)
# endif
# ifdef BBL_MODEL
      real(r8), intent(in) :: bustrc(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bvstrc(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bustrw(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bvstrw(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bustrcwmax(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bvstrcwmax(LBi:UBi,LBj:UBj)
#   ifdef SSW_BBL
      real(r8), intent(in) :: bstrcwave(LBi:UBi,LBj:UBj)
#   endif
#  else
      real(r8), intent(in) :: bustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bvstr(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,UBk,3,UBt)
#endif
!
!  Local variable declarations.
!
      integer :: Iter, i, ibio, isink, itrc, ivar, j, k, ks

      real(r8) :: PFDsurf    
      real(r8) :: tau, tau_u, tau_v        
      real(r8) :: u10        
      
      real(r8) :: sspH      
      real(r8) :: ssfCO2    
      real(r8) :: ssWarg    
      real(r8) :: ssCO2flux 
      real(r8) :: ssO2flux  
      real(r8) :: PFDbott   

#ifdef CORAL_CARBON_ISOTOPE
      real(r8) :: R13CH2O
#endif
      real(r8) :: dtrc_dt(UBk,UBt)

#include "set_bounds.h"

          
!$acc kernels
!-----------------------------------------------------------------------
      DO j=Jstr,Jend

        DO i=Istr,Iend
! Set initial zero 
          dtrc_dt(:,:)=0.0_r8
!          DO k=1,N(ng)
!            DO itrc=1,NBT
!              ibio=idbio(itrc)
!              dtrc_dt(k,ibio)=0.0_r8
!            END DO
!          END DO
          sspH      = 8.0_r8         ! sea surface pH
          ssfCO2    = 380.0_r8         ! sea surface fCO2 (uatm)
          ssWarg    = 4.0_r8         ! sea surface aragonite saturation state
          ssCO2flux = 0.0_r8         ! sea surface CO2 flux (mmol m-2 s-1)
          ssO2flux  = 0.0_r8         ! sea surface O2 flux (mmol m-2 s-1)

#ifdef MASKING
          IF (rmask(i,j).eq.1.0_r8) THEN
# endif

!
!  Calculate surface Photosynthetically Available Radiation (PAR).  The
!  net shortwave radiation is scaled back to Watts/m2 and multiplied by
!  the fraction that is photosynthetically available, PARfrac.
!
            PFDsurf = PARfrac(ng)*srflx(i,j)*rho0*Cp

            u10 = SQRT( Uwind(i,j)*Uwind(i,j) + Vwind(i,j)*Vwind(i,j) )
!
!-----------------------------------------------------------------------
! Compute bottom shear stress (N m-2).
!-----------------------------------------------------------------------

#ifdef BBL_MODEL
# ifdef SSW_BBL
            tau = bstrcwave(i,j) *rho0    !! (m2 s-2) * (kg m-3) = (kg m-1 s-2) = (kg m s-2 m-2) = (N m-2)
# else
            tau_u=bustrc(i,j)+0.5_r8*bustrw(i,j)
            tau_v=bvstrc(i,j)+0.5_r8*bvstrw(i,j)
            tau = SQRT(tau_u*tau_u + tau_v*tau_v) *rho0    !! (m2 s-2) * (kg m-3) = (kg m-1 s-2) = (kg m s-2 m-2) = (N m-2)
            
!            tau = SQRT(bustrcwmax(i,j)*bustrcwmax(i,j)+          &
!     &                   bvstrcwmax(i,j)*bvstrcwmax(i,j)) *rho0    !! (m2 s-2) * (kg m-3) = (kg m-1 s-2) = (kg m s-2 m-2) = (N m-2)

# endif
#else
            tau = 0.5_r8*SQRT((bustr(i,j)+bustr(i+1,j))*         &
     &                          (bustr(i,j)+bustr(i+1,j))+         &
     &                          (bvstr(i,j)+bvstr(i,j+1))*         &
     &                          (bvstr(i,j)+bvstr(i,j+1))) *rho0
#endif

!----- Ecosystem model ----------------------------------------

            CALL reef_ecosys           &
!          input parameters
     &            (ng, i, j            &   ! ng: nested grid number; i,j: position
     &            ,N(ng)               &   ! Number of vertical grid (following ROMS vertical grid)
     &            ,CrlIter(ng)         &   ! Internal loop counts of coral polyp model
     &            ,SedIter(ng)         &   ! Internal loop counts of sediment ecosystem model
     &            ,dt(ng)              &   ! Time step (sec)
     &            ,Hz(i,j,:)           &   ! dz(N): vertical grid size (m)
     &            ,PFDsurf             &   ! Sea surface photon flux density (umol m-2 s-1)
     &            ,tau                 &   ! bottom shear stress (N m-2)
     &            ,pCO2air(ng)         &   ! Air CO2 pertial pressure (uatm)
     &            ,u10                 &   ! wind speed (m s-1)
#ifdef CORAL_POLYP
     &            ,p_coral(:,i,j)      &   ! Coral coverage (0-1)
#endif
#ifdef SEAGRASS
     &            ,p_seagrass(i,j)     &   ! seagrass coverage (0-1)
#endif
#ifdef MACROALGAE
     &            ,p_algae(i,j)        &   ! algal coverage (0-1)
#endif
#ifdef SEDIMENT_ECOSYS
     &            ,p_sand(i,j)         &   ! sediment coverage (0-1)
#endif
     &            ,t(i,j,:,nstp,iTemp)     &   ! Tmp(N): Temperature (oC)
     &            ,t(i,j,:,nstp,iSalt)     &   ! Sal(N): Salinity (PSU)
     &            ,t(i,j,:,nstp,iTIC_)     &   ! DIC(N): Total dissolved inorganic carbon (DIC: umol kg-1)
     &            ,t(i,j,:,nstp,iTAlk)     &   ! TA (N): Total alkalinity (TA: umol kg-1)
     &            ,t(i,j,:,nstp,iOxyg)     &   ! DOx(N): Dissolved oxygen (umol L-1)
#if defined ORGANIC_MATTER
     &            ,t(i,j,:,nstp,iDOC(:))   &   ! DOC(N): Dissolved organic carbon (DOC: umol L-1)
     &            ,t(i,j,:,nstp,iPOC(:))   &   ! POC(N): Particulate organic carbon (POC: umol L-1)
     &            ,t(i,j,:,nstp,iPhyt(:))  &   ! PHY(N): phytoplankton1 (umol C L-1), dinoflagellate
     &            ,t(i,j,:,nstp,iZoop(:))  &   ! ZOO(N): zooplankton (umol C L-1)
     &            ,t(i,j,:,nstp,iPIC(:))   &   ! PIC(N): Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
#endif
#if defined CARBON_ISOTOPE
     &            ,t(i,j,:,nstp,iT13C)         &   ! DI13C (N): 13C of DIC (umol kg-1)
# if defined ORGANIC_MATTER
     &            ,t(i,j,:,nstp,iDO13C(:))     &   ! DO13C (N): 13C of Labile Dissolved organic carbon (LDOC: umol L-1)
     &            ,t(i,j,:,nstp,iPO13C(:))     &   ! PO13C (N): 13C of Particulate organic carbon (POC: umol L-1)
     &            ,t(i,j,:,nstp,iPhyt13C(:))   &   ! PHY13C(N): 13C of phytoplankton1 (umol C L-1), dinoflagellate
     &            ,t(i,j,:,nstp,iZoop13C(:))   &   ! ZOO13C(N): 13C of zooplankton (umol C L-1)
     &            ,t(i,j,:,nstp,iPI13C(:))     &   ! PI13C (N): 13C of Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
# endif
#endif
#if defined NUTRIENTS         
     &            ,t(i,j,:,nstp,iNO3_)     &   ! NO3(N): NO3 (umol L-1)
!     &            ,t(i,j,:,nstp,iNO2_)     &   ! NO2(N): NO2 (umol L-1)
     &            ,t(i,j,:,nstp,iNH4_)     &   ! NH4(N): NH4 (umol L-1)
     &            ,t(i,j,:,nstp,iPO4_)     &   ! PO4(N): PO4 (umol L-1)
# if defined ORGANIC_MATTER
     &            ,t(i,j,:,nstp,iDON(:))   &   ! DON(N): Dissolved organic nitrogen (DON: umol L-1)
     &            ,t(i,j,:,nstp,iPON(:))   &   ! PON(N): Particulate organic nitrogen (PON: umol L-1)
     &            ,t(i,j,:,nstp,iDOP(:))   &   ! DOP(N): Dissolved organic phosporius (DOP: umol L-1)
     &            ,t(i,j,:,nstp,iPOP(:))   &   ! POP(N): Particulate organic phosporius (POP: umol L-1)
# endif
# if defined NITROGEN_ISOTOPE
    &            ,t(i,j,:,nstp,i15NO3)     &   ! NO3_15N(N): 15N of NO3 (umol L-1)
!    &            ,t(i,j,:,nstp,i15NO2)     &   ! NO2_15N(N): 15N of NO2 (umol L-1)
    &            ,t(i,j,:,nstp,i15NH4)     &   ! NH4_15N(N): 15N of NH4 (umol L-1)
#  if defined ORGANIC_MATTER
    &            ,t(i,j,:,nstp,iDO15N(:))     &   ! DO15N (N): 15N of Labile Dissolved organic nitrogen (LDON: umol L-1)
    &            ,t(i,j,:,nstp,iPO15N(:))     &   ! PO15N (N): 15N of Particulate organic nitrogen (PON: umol L-1)
    &            ,t(i,j,:,nstp,iPhyt15N(:))   &   ! PHY15N(N): 15N of phytoplankton1 (umol C L-1), dinoflagellate
    &            ,t(i,j,:,nstp,iZoop15N(:))   &   ! ZOO15N(N): 15N of zooplankton (umol C L-1)
#  endif
# endif
#endif
#if defined COT_STARFISH         
     &            ,t(i,j,:,nstp,iCOTe)     &   ! COTe(N): COT starfish egg (umol L-1)
     &            ,t(i,j,:,nstp,iCOTl)     &   ! COTl(N): COT starfish larvae (umol L-1)
#endif
!          output parameters
     &            ,dtrc_dt(:,iTIC_)        &   ! dDIC_dt(N): dDIC/dt (umol kg-1 s-1)
     &            ,dtrc_dt(:,iTAlk)        &   ! dTA_dt (N): dTA/dt (umol kg-1 s-1)
     &            ,dtrc_dt(:,iOxyg)        &   ! dDOx_dt(N): dDO/dt (umol L-1 s-1)
#if defined ORGANIC_MATTER     
     &            ,dtrc_dt(:,iDOC(1):iDOC(N_dom))      &   ! dDOC_dt(N): dDOC/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iPOC(1):iPOC(N_pom))      &   ! dPOC_dt(N): dPOC/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iPhyt(1):iPhyt(N_phyt))   &   ! dPHY_dt(N): dPHY/dt (umol C L-1 s-1)
     &            ,dtrc_dt(:,iZoop(1):iZoop(N_zoop))   &   ! dZOO_dt(N): dZOO/dt (umol C L-1 s-1)
     &            ,dtrc_dt(:,iPIC(1):iPIC(N_pim))      &   ! dPIC_dt(N): dPIC/dt (umol L-1 s-1)
#endif
#if defined CARBON_ISOTOPE
     &            ,dtrc_dt(:,iT13C)        &   ! dDI13C_dt (N): dDI13C/dt (umol kg-1 s-1)
# if defined ORGANIC_MATTER     
     &            ,dtrc_dt(:,iDO13C(1):iDO13C(N_dom))    &   ! dDO13C_dt (N): dDO13C/dt  (umol L-1 s-1) 
     &            ,dtrc_dt(:,iPO13C(1):iPO13C(N_pom))    &   ! dPO13C_dt (N): dPO13C/dt  (umol L-1 s-1) 
     &            ,dtrc_dt(:,iPhyt13C(1):iPhyt13C(N_phyt))  &   ! dPHY13C_dt(N): dPHY13C/dt  (umol L-1 s-1)  
     &            ,dtrc_dt(:,iZoop13C(1):iZoop13C(N_zoop))  &   ! dZOO13C_dt(N): dZOO13C/dt  (umol L-1 s-1)  
     &            ,dtrc_dt(:,iPI13C(1):iPI13C(N_pim))    &   ! dPI13C_dt (N): dPI13C/dt  (umol L-1 s-1)  
# endif
#endif     
#if defined NUTRIENTS      
     &            ,dtrc_dt(:,iNO3_)        &   ! dNO3_dt(N): dNO3/dt (umol L-1 s-1)
!     &            ,dtrc_dt(:,iNO2_)        &   ! dNO2_dt(N): dNO2/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iNH4_)        &   ! dNH4_dt(N): dNH4/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iPO4_)        &   ! dPO4_dt(N): dPO4/dt (umol L-1 s-1)
# if defined ORGANIC_MATTER     
     &            ,dtrc_dt(:,iDON(1):iDON(N_dom))      &   ! dDON_dt(N): dDON/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iPON(1):iPON(N_pom))      &   ! dPON_dt(N): dPON/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iDOP(1):iDOP(N_dom))      &   ! dDOP_dt(N): dDOP/dt (umol L-1 s-1)
     &            ,dtrc_dt(:,iPOP(1):iPOP(N_pom))      &   ! dPOP_dt(N): dPOP/dt (umol L-1 s-1)
# endif     
# if defined NITROGEN_ISOTOPE
    &            ,dtrc_dt(:,i15NO3)        &   ! dNO3_15N_dt(N): dNO3_15N/dt (umol L-1 s-1)
!    &            ,dtrc_dt(:,i15NO2)        &   ! dNO2_15N_dt(N): dNO2_15N/dt (umol L-1 s-1)
    &            ,dtrc_dt(:,i15NH4)        &   ! dNH4_15N_dt(N): dNH4_15N/dt (umol L-1 s-1)
#  if defined ORGANIC_MATTER
    &            ,dtrc_dt(:,iDO15N(1):iDO15N(N_dom))     &   ! dDO15N_dt (N): dDO15N/dt  (umol L-1 s-1) 
    &            ,dtrc_dt(:,iPO15N(1):iPO15N(N_pom))     &   ! dPO15N_dt (N): dPO15N/dt  (umol L-1 s-1) 
    &            ,dtrc_dt(:,iPhyt15N(1):iPhyt15N(N_phyt))   &   ! dPHY15N_dt(N): dPHY1_15N/dt  (umol L-1 s-1)  
    &            ,dtrc_dt(:,iZoop15N(1):iZoop15N(N_zoop))   &   ! dZOO15N_dt(N): dZOO_15N/dt  (umol L-1 s-1)  
#  endif
# endif
#endif     
#if defined COT_STARFISH              
     &            ,dtrc_dt(:,iCOTe)        &   ! dCOTe/dt(N): (umol L-1 s-1)
     &            ,dtrc_dt(:,iCOTl)        &   ! dCOTl/dt(N): (umol L-1 s-1)
#endif
     &            ,sspH           &   ! sea surface pH
     &            ,ssfCO2         &   ! sea surface fCO2 (uatm)
     &            ,ssWarg         &   ! sea surface aragonite saturation state
     &            ,ssCO2flux      &   ! sea surface CO2 flux (mmol m-2 s-1)
     &            ,ssO2flux       &   ! sea surface O2 flux (mmol m-2 s-1)
     &            ,PFDbott        &   ! Bottom photon flux density (umol m-2 s-1)
     &             )
!
#ifdef MASKING
          END IF
#endif

            HisBio2d(i,j,ipHt_) = sspH
            HisBio2d(i,j,iWarg) = ssWarg
            HisBio2d(i,j,iCOfx) = ssCO2flux
            HisBio2d(i,j,ipCO2) = ssfCO2
            HisBio2d(i,j,iO2fx) = ssO2flux
            HisBio2d(i,j,iPARb) = PFDbott
            HisBio2d(i,j,iTau_) = tau
#ifdef CORAL_POLYP
            HisBio2d(i,j,iC1Pg) = CORAL(ng)%Pg(1,i,j)
            HisBio2d(i,j,iC1_R) = CORAL(ng)%R(1,i,j)
            HisBio2d(i,j,iC1Pn) = CORAL(ng)%Pg(1,i,j)-CORAL(ng)%R(1,i,j)
            HisBio2d(i,j,iC1_G) = CORAL(ng)%G(1,i,j)
            HisBio2d(i,j,iC1OC) = CORAL(ng)%QC(1,i,j)
            HisBio2d(i,j,iC2Pg) = CORAL(ng)%Pg(2,i,j)
            HisBio2d(i,j,iC2_R) = CORAL(ng)%R(2,i,j)
            HisBio2d(i,j,iC2Pn) = CORAL(ng)%Pg(2,i,j)-CORAL(ng)%R(2,i,j)
            HisBio2d(i,j,iC2_G) = CORAL(ng)%G(2,i,j)
            HisBio2d(i,j,iC2OC) = CORAL(ng)%QC(2,i,j)
# ifdef CORAL_CARBON_ISOTOPE
            R13CH2O = CORAL(ng)%Q13C(1,i,j) / CORAL(ng)%QC(1,i,j)   !coral organism
            HisBio2d(i,j,iC113) = d13C_fromR13C(R13CH2O)
            R13CH2O = CORAL(ng)%Q13C(2,i,j) / CORAL(ng)%QC(2,i,j)   !coral organism
            HisBio2d(i,j,iC213) = d13C_fromR13C(R13CH2O)
# endif
# ifdef CORAL_ZOOXANTHELLAE
            HisBio2d(i,j,iC1zx) = ZOOX(ng)%dens(1,i,j)
            HisBio2d(i,j,iC2zx) = ZOOX(ng)%dens(2,i,j)
# endif
# ifdef CORAL_SIZE_DYNAMICS
            HisBio2d(i,j,iC1mt) = CORAL(ng)%mort(1,i,j)
            HisBio2d(i,j,iC1gr) = CORAL(ng)%growth(1,i,j)
            HisBio2d(i,j,iC2mt) = CORAL(ng)%mort(2,i,j)
            HisBio2d(i,j,iC2gr) = CORAL(ng)%growth(2,i,j)
# endif
#endif
#ifdef SEAGRASS
            HisBio2d(i,j,iSgPg) = SGRASS(ng)%Pg(1,i,j)
            HisBio2d(i,j,iSg_R) = SGRASS(ng)%R (1,i,j)
            HisBio2d(i,j,iSgPn) = SGRASS(ng)%Pg(1,i,j)-SGRASS(ng)%R (1,i,j)
#endif
#ifdef MACROALGAE
            HisBio2d(i,j,iAgPg) = ALGAE(ng)%Pg(1,i,j)
            HisBio2d(i,j,iAg_R) = ALGAE(ng)%R (1,i,j)
            HisBio2d(i,j,iAgPn) = ALGAE(ng)%Pg(1,i,j)-ALGAE(ng)%R (1,i,j)
#endif
#ifdef SEDIMENT_ECOSYS
            HisBio2d(i,j,iSdPg) = SEDECO(ng)%Pg(i,j)
            HisBio2d(i,j,iSd_R) = SEDECO(ng)%R (i,j)
            HisBio2d(i,j,iSdPn) = SEDECO(ng)%Pg(i,j)-SEDECO(ng)%R (i,j)
            HisBio2d(i,j,iSd_G) = SEDECO(ng)%G (i,j)
#endif

!-----------------------------------------------------------------------
!  Update global tracer variables: Add increment due to BGC processes
!  to tracer array in time index "nnew". Index "nnew" is solution after
!  advection and mixing and has transport units (m Tunits) hence the
!  increment is multiplied by Hz.  Notice that we need to subtract
!  original values "Bio_old" at the top of the routine to just account
!  for the concentractions affected by BGC processes. This also takes
!  into account any constraints (non-negative concentrations, carbon
!  concentration range) specified before entering BGC kernel. If "Bio"
!  were unchanged by BGC processes, the increment would be exactly
!  zero. Notice that final tracer values, t(:,:,:,nnew,:) are not
!  bounded >=0 so that we can preserve total inventory of N and
!  C even when advection causes tracer concentration to go negative.
!  (J. Wilkin and H. Arango, Apr 27, 2012)
!-----------------------------------------------------------------------

          DO k=1,N(ng)
            DO itrc=1,NBT
              ibio=idbio(itrc)
              
              IF(dtrc_dt(k,ibio)*0.0_r8 /= 0.0_r8) THEN  !!!---------Error Handling: Check NAN
!                write(50,*) i,j,k,itrc,dtrc_dt(k,ibio),ssO2flux, ssCO2flux,rmask(i,j) 
!                write(50,*) t(i,j,k,nnew,:)
!                write(50,*) t(i,j,k,nstp,:)
                dtrc_dt(k,ibio)=0.0_r8
              END IF
              
              t(i,j,k,nnew,ibio)=t(i,j,k,nnew,ibio)                    &
    &                              +dtrc_dt(k,ibio)*dt(ng)*Hz(i,j,k)
    
              t(i,j,k,nnew,ibio)=MAX(0.0_r8,t(i,j,k,nnew,ibio))!!!---------Error Handling
              
            END DO

#if defined CARBON_ISOTOPE
! Carbon isotope ratio calculation
            HisBio3d(i,j,k,id13C)=d13C_fromR13C(t(i,j,k,nnew,iT13C)/t(i,j,k,nnew,iTIC_))
#endif
          END DO

        END DO
      END DO
!-----------------------------------------------------------------------
!$acc end kernels

      RETURN
      END SUBROUTINE biology_tile
      

