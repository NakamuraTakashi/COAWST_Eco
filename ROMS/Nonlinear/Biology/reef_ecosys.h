      SUBROUTINE biology (ng,tile)

!!!=== Copyright (c) 2012-2025 Takashi NAKAMURA  =====
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
#if defined DIAGNOSTICS_BIO
      USE mod_diags
#endif
#if defined SEDECO_SGD && defined SGD_ON
      USE mod_sources
#endif
#if defined SEDIMENT && defined SUSPLOAD
      USE mod_sedbed
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
#if defined DIAGNOSTICS_BIO
     &                   DIAGS(ng) % DiaBio2d,                          &
     &                   DIAGS(ng) % DiaBio3d,                          &
#endif
#ifdef CORAL_POLYP
     &                   GRID(ng) % p_coral,                            &
#endif
#ifdef SEAGRASS
     &                   GRID(ng) % p_sgrass,                           &
#endif
#ifdef MACROALGAE
     &                   GRID(ng) % p_algae,                            &
#endif
#ifdef SEDIMENT_ECOSYS
     &                   GRID(ng) % p_sand,                             &
# if defined SEDECO_SGD && defined SGD_ON
     &                   GRID(ng) % sgd_src,                            &
     &                   GRID(ng) % pm,                                 &
     &                   GRID(ng) % pn,                                 &
     &                   SOURCES(ng) % Qsgd(1),                         &
     &                   SOURCES(ng) % Tsgd,                            &
# endif
#endif
#if defined SEDIMENT && defined SUSPLOAD
     &                   SEDBED(ng) % ero_flux,                         &
     &                   SEDBED(ng) % settling_flux,                    &
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
#endif
     &                         Hz, z_r, z_w, srflx,                     &
#ifdef BULK_FLUXES
     &                         Uwind, Vwind,                            &
#else
     &                         sustr, svstr,                            &
#endif
#if defined DIAGNOSTICS_BIO
     &                         DiaBio2d, DiaBio3d,                      &
#endif
#ifdef CORAL_POLYP
     &                         p_coral,                                 &
#endif
#ifdef SEAGRASS
     &                         p_sgrass,                                &
#endif
#ifdef MACROALGAE
     &                         p_algae,                                 &
#endif
#ifdef SEDIMENT_ECOSYS
     &                         p_sand,                                  &
# if defined SEDECO_SGD && defined SGD_ON
     &                         sgd_src,                                 &
     &                         pm,                                      &
     &                         pn,                                      &
     &                         Qsgd,                                    &
     &                         Tsgd,                                    &
# endif
#endif
#if defined SEDIMENT && defined SUSPLOAD
     &                         ero_flux,                                &
     &                         settling_flux,                           &
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
      
      USE mod_reef_ecosys_param
      USE mod_reef_ecosys  !!!<<<<<<<<<<<<<<<< Reef ecosystem model
      USE mod_geochem

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
# if defined DIAGNOSTICS_BIO
      real(r8), intent(inout) :: DiaBio2d(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaBio3d(LBi:,LBj:,:,:)
# endif
# ifdef CORAL_POLYP
      real(r8), intent(inout) :: p_coral(:,LBi:,LBj:)
# endif
# ifdef SEAGRASS
      real(r8), intent(inout) :: p_sgrass(:,LBi:,LBj:)
# endif
# ifdef MACROALGAE
      real(r8), intent(inout) :: p_algae(:,LBi:,LBj:)
# endif
# ifdef SEDIMENT_ECOSYS
      real(r8), intent(inout) :: p_sand(LBi:,LBj:)
#  if defined SEDECO_SGD && defined SGD_ON
      real(r8), intent(inout) :: sgd_src(LBi:,LBj:)
      real(r8), intent(in)    :: pm(LBi:,LBj:)
      real(r8), intent(in)    :: pn(LBi:,LBj:)
      real(r8), intent(in)    :: Qsgd
      real(r8), intent(in)    :: Tsgd(UBt)
#  endif
# endif
# if defined SEDIMENT && defined SUSPLOAD
      real(r8), intent(in)    :: ero_flux(LBi:,LBj:,:)
      real(r8), intent(in)    :: settling_flux(LBi:,LBj:,:)
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
# if defined DIAGNOSTICS_BIO
      real(r8), intent(inout) :: DiaBio2d(LBi:UBi,LBj:UBj,NHbio2d)
      real(r8), intent(inout) :: DiaBio3d(LBi:UBi,LBj:UBj,UBk,NHbio3d)
# endif
# ifdef CORAL_POLYP
      real(r8), intent(inout) :: p_coral(Ncl,LBi:UBi,LBj:UBj)
# endif
# ifdef SEAGRASS
      real(r8), intent(inout) :: p_sgrass(Nsg,LBi:UBi,LBj:UBj)
# endif
# ifdef MACROALGAE
      real(r8), intent(inout) :: p_algae(Nag,LBi:UBi,LBj:UBj)
# endif
# ifdef SEDIMENT_ECOSYS
      real(r8), intent(inout) :: p_sand(LBi:UBi,LBj:UBj)
#  if defined SEDECO_SGD && defined SGD_ON
      real(r8), intent(inout) :: sgd_src(LBi:UBi,LBj:UBj)
      real(r8), intent(in)    :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in)    :: pn(LBi:UBi,LBj:UBj)
      real(r8), intent(in)    :: Qsgd
      real(r8), intent(in)    :: Tsgd(UBt)
#  endif
# endif
# if defined SEDIMENT && defined SUSPLOAD
      real(r8), intent(in)    :: ero_flux(LBi:UBi,LBj:UBj,NST(ng))
      real(r8), intent(in)    :: settling_flux(LBi:UBi,LBj:UBj,NST(ng))
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
      integer :: Iter, i, ibio, itrc, j, k, isp, m
      integer :: icl
      real(r8) :: PFDsurf    
      real(r8) :: tau, tau_u, tau_v        
      real(r8) :: u10

      real(r8) :: Tmp(N(ng))                 ! Temperature (oC)
      real(r8) :: Sal(N(ng))                 ! Salinity (PSU)
      real(r8) :: DOx(N(ng))                 ! Dissolved oxygen (umol L-1)
      real(r8) :: TA (N(ng))                 ! Total alkalinity (TA: umol kg-1)
      real(r8) :: DIC(N_Csp,N(ng))           ! Total dissolved inorganic carbon (DIC: umol kg-1)
      real(r8) :: NO3(N_Nsp,N(ng))           ! NO3 (umol L-1)
      real(r8) :: NH4(N_Nsp,N(ng))           ! NH4 (umol L-1)
      real(r8) :: PO4(N_Nsp,N(ng))           ! PO4 (umol L-1)
      real(r8) :: DOC(N_Csp,Ndom,N(ng))      ! Dissolved organic carbon (DOC: umol L-1)
      real(r8) :: POC(N_Csp,Npom,N(ng))      ! Particulate organic carbon (POC: umol L-1)
      real(r8) :: DON(N_Nsp,Ndom,N(ng))      ! Labile Dissolved organic nitrogen (DON: umol L-1)
      real(r8) :: PON(N_Nsp,Npom,N(ng))      ! Particulate organic nitrogen (PON: umol L-1)
      real(r8) :: DOP(N_Psp,Ndom,N(ng))      ! Labile Dissolved organic phosporius (DOP: umol L-1)
      real(r8) :: POP(N_Psp,Npom,N(ng))      ! Particulate organic phosporius (POP: umol L-1)
      real(r8) :: PhyC(N_Csp,Nphy,N(ng))     ! phytoplankton C biomass (umol C L-1), dinoflagellate
      real(r8) :: ZooC(N_Csp,Nzoo,N(ng))     ! zooplankton C biomass (umol C L-1)
      real(r8) :: PhyN(N_Nsp,Nphy,N(ng))     ! phytoplankton N biomass (umol N L-1), dinoflagellate
      real(r8) :: ZooN(N_Nsp,Nzoo,N(ng))     ! zooplankton N biomass (umol N L-1)
      real(r8) :: PhyP(N_Psp,Nphy,N(ng))     ! phytoplankton P biomass (umol P L-1), dinoflagellate
      real(r8) :: ZooP(N_Psp,Nzoo,N(ng))     ! zooplankton P biomass (umol P L-1)
      real(r8) :: PIC (N_Csp,Npim,N(ng))     ! Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
#if defined BLUE_TIDE         
      real(r8) :: H2S (N_Ssp,N(ng))          ! H2S (umol S L-1)
      real(r8) :: Slf0(N_Ssp,N(ng))          ! Slf0 (umol S L-1)
#endif
#if defined COT_STARFISH         
      real(r8) :: COTe(N(ng))         ! COT starfish egg (umol L-1)
      real(r8) :: COTl(N(ng))         ! COT starfish larvae (umol L-1)
#endif
    
      real(r8) :: dTemp_dt(N(ng))            ! Tmp(N): Temperature (oC)
      real(r8) :: dSalt_dt(N(ng))            ! Sal(N): Salinity (PSU)
      real(r8) :: dDOx_dt(N(ng))             ! dDOx/dt  (umol L-1 s-1) 
      real(r8) :: dTA_dt (N(ng))             ! dTA/dt   (umol kg-1 s-1) 
      real(r8) :: dDIC_dt(N_Csp,N(ng))       ! dDIC/dt  (umol kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
      real(r8) :: dNO3_dt(N_Nsp,N(ng))       ! dNO3/dt (umol L-1 s-1)
      real(r8) :: dNH4_dt(N_Nsp,N(ng))       ! dNH4/dt (umol L-1 s-1)
      real(r8) :: dPO4_dt(N_Psp,N(ng))       ! dPO4/dt (umol L-1 s-1)
      real(r8) :: dDOC_dt(N_Csp,Ndom,N(ng))  ! dDOC/dt  (umol L-1 s-1) 
      real(r8) :: dPOC_dt(N_Csp,Npom,N(ng))  ! dPOC/dt  (umol L-1 s-1) 
      real(r8) :: dDON_dt(N_Nsp,Ndom,N(ng))  ! dDON/dt (umol L-1 s-1)
      real(r8) :: dPON_dt(N_Nsp,Npom,N(ng))  ! dPON/dt (umol L-1 s-1)
      real(r8) :: dDOP_dt(N_Psp,Ndom,N(ng))  ! dDOP/dt (umol L-1 s-1)
      real(r8) :: dPOP_dt(N_Psp,Npom,N(ng))  ! dPOP/dt (umol L-1 s-1)
      real(r8) :: dPhyC_dt(N_Csp,Nphy,N(ng)) ! dPhyC/dt  (umolC L-1 s-1)  
      real(r8) :: dZooC_dt(N_Csp,Nzoo,N(ng)) ! dZooC/dt  (umolC L-1 s-1)  
      real(r8) :: dPhyN_dt(N_Nsp,Nphy,N(ng)) ! dPhyN/dt  (umolN L-1 s-1)  
      real(r8) :: dZooN_dt(N_Nsp,Nzoo,N(ng)) ! dZooN/dt  (umolN L-1 s-1)  
      real(r8) :: dPhyP_dt(N_Psp,Nphy,N(ng)) ! dPhyP/dt  (umolP L-1 s-1)  
      real(r8) :: dZooP_dt(N_Psp,Nzoo,N(ng)) ! dZooP/dt  (umolP L-1 s-1)  
      real(r8) :: dPIC_dt (N_Csp,Npim,N(ng)) ! dPIC/dt  (umol L-1 s-1) 
#if defined BLUE_TIDE         
      real(r8) :: dH2S_dt (N_Ssp,N(ng))      ! dH2S/dt  (umol S L-1 s-1)
      real(r8) :: dS0_dt  (N_Ssp,N(ng))      ! dS0/dt   (umol S L-1 s-1)
#endif
#if defined COT_STARFISH         
      real(r8) :: dCOTe_dt(N)    ! dCOTe/dt (umol L-1 s-1)
      real(r8) :: dCOTl_dt(N)    ! dCOTl/dt (umol L-1 s-1)
#endif
#if defined SEDECO_SGD && defined SGD_ON
      real(r8) :: sgd_flux        ! sumbarine groundwater discharge rate of grid (cm s-1)  
      real(r8) :: sgd_Tmp         ! SGD concentration coming in through bottom: Tmp: Temperature (oC)
      real(r8) :: sgd_Sal         ! SGD concentration coming in through bottom: Sal: Salinity (PSU)
      real(r8) :: sgd_DIC(N_Csp)  ! SGD concentration coming in through bottom: DIC: Total dissolved inorganic carbon (DIC: umol kg-1)
      real(r8) :: sgd_TA          ! SGD concentration coming in through bottom: TA : Total alkalinity (TA: umol kg-1)
      real(r8) :: sgd_DOx         ! SGD concentration coming in through bottom: DOx: Dissolved oxygen (umol L-1)
      real(r8) :: sgd_NO3(N_Nsp)  ! SGD concentration coming in through bottom: NO3: NO3 (umol L-1)
      real(r8) :: sgd_NH4(N_Nsp)  ! SGD concentration coming in through bottom: NH4: NH4 (umol L-1)
      real(r8) :: sgd_PO4(N_Psp)  ! SGD concentration coming in through bottom: PO4: PO4 (umol L-1)
      real(r8) :: sgd_DOC(N_Csp,Ndom)  ! SGD concentration coming in through bottom: DOC: Total dissolved organic carbon (DOC: umol L-1)
      real(r8) :: sgd_DON(N_Nsp,Ndom)  ! SGD concentration coming in through bottom: DOC: Total dissolved organic nitrogen (DON: umol L-1)
      real(r8) :: sgd_DOP(N_Psp,Ndom)  ! SGD concentration coming in through bottom: DOC: Total dissolved organic phosphrous (DOP: umol L-1)
#endif
#if defined SEDECO_BURIAL
      real(r8) :: Fdep_sed        ! Sedimentation rate (cm s-1) (Positive: sedimentation; Negative: erosion)
#endif
          
      real(r8) :: pH(UBk)      
      real(r8) :: Warg(UBk)    
      real(r8) :: Wcal(UBk)    
      real(r8) :: ssfCO2    
      real(r8) :: ssCO2flux 
      real(r8) :: ssO2flux  
      real(r8) :: PFDbott   
!!! mons light model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>KM:Add
#if defined LIGHT_MODEL
      real(r8) :: PFDk(UBk)   
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<KM:Add

#ifdef CORAL_CARBON_ISOTOPE
      real(r8) :: R13CH2O
#endif
      real(r8) :: dtrc_dt(UBk,UBt)

#include "set_bounds.h"

!-----------------------------------------------------------------------
      DO j=Jstr,Jend

        DO i=Istr,Iend
! Set initial zero 
          dtrc_dt(:,:)=0.0_r8
          
#ifdef MASKING
          IF (rmask(i,j).eq.1.0_r8) THEN
# endif

!=== Import ROMS tracer arrays into REEF_ECOSYS model arrays ===
            Tmp(:) = t(i,j,:,nstp,iTemp)       
            Sal(:) = t(i,j,:,nstp,iSalt)       
            DOx(:) = t(i,j,:,nstp,iDO)       
            TA (:) = t(i,j,:,nstp,iTA)
            DO isp=1,N_Csp     
              DIC(isp,:) = t(i,j,:,nstp,iDIC(isp))
            END DO       
            DO isp=1,N_Nsp     
              NO3(isp,:) = t(i,j,:,nstp,iNO3(isp))       
            END DO       
            DO isp=1,N_Nsp     
              NH4(isp,:) = t(i,j,:,nstp,iNH4(isp))       
            END DO       
            DO isp=1,N_Psp     
              PO4(isp,:) = t(i,j,:,nstp,iPO4(isp))       
            END DO
            DO m=1,Ndom    
              DO isp=1,N_Csp     
                DOC(isp,m,:) = t(i,j,:,nstp,iDOC(isp,m))
              END DO
            END DO
            DO m=1,Ndom    
              DO isp=1,N_Nsp     
                DON(isp,m,:) = t(i,j,:,nstp,iDON(isp,m))  
              END DO
            END DO
            DO m=1,Ndom    
              DO isp=1,N_Psp     
                DOP(isp,m,:) = t(i,j,:,nstp,iDOP(isp,m))  
              END DO
            END DO 
            DO m=1,Npom    
              DO isp=1,N_Csp     
                POC(isp,m,:) = t(i,j,:,nstp,iPOC(isp,m))  
              END DO
            END DO 
            DO m=1,Npom    
              DO isp=1,N_Nsp     
                PON(isp,m,:) = t(i,j,:,nstp,iPON(isp,m))  
              END DO
            END DO 
            DO m=1,Npom    
              DO isp=1,N_Psp     
                POP(isp,m,:) = t(i,j,:,nstp,iPOP(isp,m))  
              END DO
            END DO 
            DO m=1,Nphy    
              DO isp=1,N_Csp     
                PhyC(isp,m,:) = t(i,j,:,nstp,iPhyC(isp,m))
              END DO
            END DO 
            DO m=1,Nphy    
              DO isp=1,N_Nsp     
                PhyN(isp,m,:) = t(i,j,:,nstp,iPhyN(isp,m))
              END DO
            END DO 
            DO m=1,Nphy    
              DO isp=1,N_Psp     
                PhyP(isp,m,:) = t(i,j,:,nstp,iPhyP(isp,m))
              END DO
            END DO 
            DO m=1,Nzoo    
              DO isp=1,N_Csp     
                ZooC(isp,m,:) = t(i,j,:,nstp,iZooC(isp,m))
              END DO
            END DO 
            DO m=1,Nzoo    
              DO isp=1,N_Nsp     
                ZooN(isp,m,:) = t(i,j,:,nstp,iZooN(isp,m))
              END DO
            END DO 
            DO m=1,Nzoo    
              DO isp=1,N_Psp     
                ZooP(isp,m,:) = t(i,j,:,nstp,iZooP(isp,m))
              END DO
            END DO 
            DO m=1,Npim    
              DO isp=1,N_Csp     
                 PIC (isp,m,:) = t(i,j,:,nstp,iPIC(isp,m)) 
              END DO
            END DO 
#if defined BLUE_TIDE 
            DO isp=1,N_Ssp     
              H2S(isp,:) = t(i,j,:,nstp,iH2S(isp))       
            END DO       
            DO isp=1,N_Ssp     
              Slf0(isp,:)  = t(i,j,:,nstp,iS0(isp))       
            END DO       
#endif
#if defined COT_STARFISH         
            COTe(:) = t(i,j,:,nstp,iCOTe)     &   ! COTe(N): COT starfish egg (umol L-1)
            COTl(:) = t(i,j,:,nstp,iCOTl)     &   ! COTl(N): COT starfish larvae (umol L-1)
#endif
#if defined SEDECO_SGD && defined SGD_ON
            sgd_flux  = Qsgd*pm(i,j)*pn(i,j)*sgd_src(i,j)*100.0_r8  ! m/s => 100 cm/s; sumbarine groundwater discharge rate of grid (cm s-1)
            sgd_Tmp   = Tsgd(iTemp) 
            sgd_Sal   = Tsgd(iSalt) 
            sgd_DOx   = Tsgd(iDO  )
            sgd_TA    = Tsgd(iTA  )
            sgd_DIC(:)= Tsgd( iDIC(1):iDIC(N_Csp) )     
            sgd_NO3(:)= Tsgd( iNO3(1):iNO3(N_Nsp) )  
            sgd_NH4(:)= Tsgd( iNH4(1):iNH4(N_Nsp) ) 
            sgd_PO4(:)= Tsgd( iPO4(1):iPO4(N_Nsp) )
            DO m=1,Ndom
              sgd_DOC(:,m)= Tsgd( iDOC(1,m):iDOC(N_Nsp,m) )
              sgd_DON(:,m)= Tsgd( iDON(1,m):iDON(N_Nsp,m) )
              sgd_DOP(:,m)= Tsgd( iDOP(1,m):iDOP(N_Nsp,m) )
            END DO
#endif
#if defined SEDECO_BURIAL
!  Sedimentation rate (g cm-2 s-1) (Positive: sedimentation; Negative: erosion)
            Fdep_sed = 0.0d0 !ero_flux !(kg/m2)
#endif

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
     &            ,tdays(ng)           &   ! Date (days since x or elapsed days)  yt_edit I believe tdays is roms model clock https://www.myroms.org/projects/src/ticket/724
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
     &            ,p_sgrass(:,i,j)     &   ! seagrass coverage (0-1)
#endif
#ifdef MACROALGAE
     &            ,p_algae(:,i,j)      &   ! algal coverage (0-1)
#endif
#ifdef SEDIMENT_ECOSYS
     &            ,p_sand(i,j)         &   ! sediment coverage (0-1)
#endif
     &            , Tmp                &   ! Tmp(N): Temperature (oC)
     &            , Sal                &   ! Sal(N): Salinity (PSU)
     &            , DOx                &   ! DOx(N): Dissolved oxygen (umol O2 L-1)
     &            , TA                 &   ! TA(N) : Total alkalinity (umol kg-1)
     &            , DIC                &   ! DIC(N_Csp,N): Total dissolved inorganic carbon (umol C kg-1)
     &            , NO3                &   ! NO3(N_Nsp,N): (umol N L-1)
     &            , NH4                &   ! NH4(N_Nsp,N): (umol N L-1)
     &            , PO4                &   ! PO4(N_Psp,N): (umol P L-1)
     &            , DOC                &   ! DOC(N_Csp,Ndom,N): Dissolved organic carbon       (umol C L-1)
     &            , POC                &   ! POC(N_Csp,Npom,N): Particulate organic carbon     (umol C L-1)
     &            , DON                &   ! DON(N_Nsp,Ndom,N): Dissolved organic nitrogen     (umol N L-1)
     &            , PON                &   ! PON(N_Nsp,Npom,N): Particulate organic nitrogen   (umol N L-1)
     &            , DOP                &   ! DOP(N_Psp,Ndom,N): Dissolved organic phosporius   (umol P L-1)
     &            , POP                &   ! POP(N_Psp,Npom,N): Particulate organic phosporius (umol P L-1)
     &            , PhyC               &   ! PhyC(N_Csp,Nphy,N): phytoplankton C biomass (umol C L-1)
     &            , ZooC               &   ! ZooC(N_Csp,Nzoo,N): zooplankton C biomass   (umol C L-1)
     &            , PhyN               &   ! PhyN(N_Nsp,Nphy,N): phytoplankton N biomass (umol N L-1)
     &            , ZooN               &   ! ZooN(N_Nsp,Nzoo,N): zooplankton N biomass   (umol N L-1)
     &            , PhyP               &   ! PhyP(N_Psp,Nphy,N): phytoplankton P biomass (umol P L-1)
     &            , ZooP               &   ! ZooP(N_Psp,Nzoo,N): zooplankton P biomass   (umol P L-1)
     &            , PIC                &   ! PIC (N_Csp,Npim,N): Particulate inorganic carbon (PIC: umolC L-1), coccolith (CaCO3)
#if defined BLUE_TIDE         
     &            , H2S                &   ! H2S (N_Ssp,N)     : (umol S L-1)
     &            , Slf0               &   ! Slf0 (N_Ssp,N)     : (umol S L-1)
#endif
#if defined COT_STARFISH         
     &            , COTe               &   ! COTe(N): COT starfish egg (umol L-1)
     &            , COTl               &   ! COTl(N): COT starfish larvae (umol L-1)
#endif
#if defined SEDECO_SGD && defined SGD_ON
     &            , sgd_flux           &   ! sumbarine groundwater discharge rate (cm s-1)  This flux is assumed to be the same throughout the sediment ecosystem layers (no compression/expansion) So volume flux doesn't change but concentrations of tracers do
     &            , sgd_Tmp            &   ! SGD concentration coming in through bottom: Tmp: Temperature (oC)
     &            , sgd_Sal            &   ! SGD concentration coming in through bottom: Sal: Salinity (PSU)
     &            , sgd_TA             &   ! SGD concentration coming in through bottom: TA : Total alkalinity (TA: umol kg-1)
     &            , sgd_DOx            &   ! SGD concentration coming in through bottom: DOx: Dissolved oxygen (umol L-1)
     &            , sgd_DIC            &   ! SGD concentration coming in through bottom: DIC: Total dissolved inorganic carbon (DIC: umol kg-1)
     &            , sgd_NO3            &   ! SGD concentration coming in through bottom: NO3: NO3 (umol L-1)
     &            , sgd_NH4            &   ! SGD concentration coming in through bottom: NH4: NH4 (umol L-1)
     &            , sgd_PO4            &   ! SGD concentration coming in through bottom: PO4: PO4 (umol L-1)
     &            , sgd_DOC            &   ! SGD concentration coming in through bottom: DOC: Total dissolved organic carbon (DOC: umol L-1)
     &            , sgd_DON            &   ! SGD concentration coming in through bottom: DOC: Total dissolved organic nitrogen (DON: umol L-1)
     &            , sgd_DOP            &   ! SGD concentration coming in through bottom: DOC: Total dissolved organic phosphrous (DOP: umol L-1)
#endif
#if defined SEDECO_BURIAL
     &            , Fdep_sed           &   ! Sedimentation rate (g cm-2 s-1) (Positive: sedimentation; Negative: erosion)
#endif
!   output parameters
     &            , dTemp_dt           &   ! dTemp_dt(N)           : Temperature (oC s-1)
     &            , dSalt_dt           &   ! dSalt_dt(N)           : Salinity (PSU s-1)
     &            , dDOx_dt            &   ! dDOx_dt(N)            : dDOx/dt  (umol O2 L-1 s-1) 
     &            , dTA_dt             &   ! dTA_dt(N)             : dTA/dt   (umol kg-1 s-1) 
     &            , dDIC_dt            &   ! dDIC_dt(N_Csp,N)      : dDIC/dt  (umol C kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
     &            , dNO3_dt            &   ! dNO3_dt(N_Nsp,N)      : dNO3/dt  (umol N L-1 s-1)
     &            , dNH4_dt            &   ! dNH4_dt(N_Nsp,N)      : dNH4/dt  (umol N L-1 s-1)
     &            , dPO4_dt            &   ! dPO4_dt(N_Psp,N)      : dPO4/dt  (umol P L-1 s-1)
     &            , dDOC_dt            &   ! dDOC_dt(N_Csp,Ndom,N) : dDOC/dt  (umol C L-1 s-1) 
     &            , dPOC_dt            &   ! dPOC_dt(N_Csp,Npom,N) : dPOC/dt  (umol C L-1 s-1) 
     &            , dDON_dt            &   ! dDON_dt(N_Nsp,Ndom,N) : dDON/dt  (umol N L-1 s-1)
     &            , dPON_dt            &   ! dPON_dt(N_Nsp,Npom,N) : dPON/dt  (umol N L-1 s-1)
     &            , dDOP_dt            &   ! dDOP_dt(N_Psp,Ndom,N) : dDOP/dt  (umol P L-1 s-1)
     &            , dPOP_dt            &   ! dPOP_dt(N_Psp,Npom,N) : dPOP/dt  (umol P L-1 s-1)
     &            , dPhyC_dt           &   ! dPhyC_dt(N_Csp,Nphy,N): dPhyC/dt (umol C L-1 s-1)  
     &            , dZooC_dt           &   ! dZooC_dt(N_Csp,Nzoo,N): dZooC/dt (umol C L-1 s-1)  
     &            , dPhyN_dt           &   ! dPhyN_dt(N_Nsp,Nphy,N): dPhyN/dt (umol N L-1 s-1)  
     &            , dZooN_dt           &   ! dZooN_dt(N_Nsp,Nzoo,N): dZooN/dt (umol N L-1 s-1)  
     &            , dPhyP_dt           &   ! dPhyP_dt(N_Psp,Nphy,N): dPhyP/dt (umol P L-1 s-1)  
     &            , dZooP_dt           &   ! dZooP_dt(N_Psp,Nzoo,N): dZooP/dt (umol P L-1 s-1)  
     &            , dPIC_dt            &   ! dPIC_dt (N_Csp,Npim,N): dPIC/dt  (umol C L-1 s-1)
#if defined BLUE_TIDE         
     &            , dH2S_dt            &   ! dH2S_dt (N_Ssp,N)     : dH2S/dt  (umol S L-1 s-1)
     &            , dS0_dt             &   ! dS0_dt  (N_Ssp,N)     : dS0/dt   (umol S L-1 s-1)
#endif
#if defined COT_STARFISH         
     &            , dCOTe_dt           &   ! dCOTe/dt(N): (umol L-1 s-1)
     &            , dCOTl_dt           &   ! dCOTl/dt(N): (umol L-1 s-1)
#endif
     &            , pH                     &   ! pH
     &            , Warg                   &   ! aragonite saturation state
     &            , Wcal                   &   ! calcite saturation state
     &            , ssfCO2                 &   ! sea surface fCO2 (uatm)
     &            , ssCO2flux              &   ! sea surface CO2 flux (mmol m-2 s-1)
     &            , ssO2flux               &   ! sea surface O2 flux (mmol m-2 s-1)
     &            , PFDbott                &   ! Bottom photon flux density (umol m-2 s-1)
!!! mons light model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>KM:Add
#if defined LIGHT_MODEL
     &            , PFDk                   &   ! Column photon flux density (umol m-2 s-1)
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<KM:Add
     &             )
!
!=== Export REEF_ECOSYS model arrays into ROMS tracer arrays ===
            dtrc_dt(:,iTemp) = dTemp_dt(:)        
            dtrc_dt(:,iSalt) = dSalt_dt(:)        
            dtrc_dt(:,iDO  ) = dDOx_dt(:)        
            dtrc_dt(:,iTA  ) = dTA_dt(:) 
            DO isp=1,N_Csp     
              dtrc_dt(:,iDIC(isp)) = dDIC_dt(isp,:)
            END DO       
            DO isp=1,N_Nsp     
              dtrc_dt(:,iNO3(isp)) = dNO3_dt(isp,:)        
            END DO       
            DO isp=1,N_Nsp     
              dtrc_dt(:,iNH4(isp)) = dNH4_dt(isp,:)        
            END DO       
            DO isp=1,N_Psp     
              dtrc_dt(:,iPO4(isp)) = dPO4_dt(isp,:)       
            END DO
            DO m=1,Ndom    
              DO isp=1,N_Csp     
                dtrc_dt(:,iDOC(isp,m)) = dDOC_dt(isp,m,:) 
              END DO
            END DO
            DO m=1,Ndom    
              DO isp=1,N_Nsp     
                dtrc_dt(:,iDON(isp,m)) = dDON_dt(isp,m,:)   
              END DO
            END DO
            DO m=1,Ndom    
              DO isp=1,N_Psp     
                dtrc_dt(:,iDOP(isp,m)) = dDOP_dt(isp,m,:)  
              END DO
            END DO 
            DO m=1,Npom    
              DO isp=1,N_Csp     
                dtrc_dt(:,iPOC(isp,m)) = dPOC_dt(isp,m,:)  
              END DO
            END DO 
            DO m=1,Npom    
              DO isp=1,N_Nsp     
                dtrc_dt(:,iPON(isp,m)) = dPON_dt(isp,m,:)  
              END DO
            END DO 
            DO m=1,Npom    
              DO isp=1,N_Psp     
                dtrc_dt(:,iPOP(isp,m)) = dPOP_dt(isp,m,:)   
              END DO
            END DO 
            DO m=1,Nphy    
              DO isp=1,N_Csp     
                dtrc_dt(:,iPhyC(isp,m)) = dPhyC_dt(isp,m,:) 
              END DO
            END DO 
            DO m=1,Nphy    
              DO isp=1,N_Nsp     
                dtrc_dt(:,iPhyN(isp,m)) = dPhyN_dt(isp,m,:)
              END DO
            END DO 
            DO m=1,Nphy    
              DO isp=1,N_Psp     
                dtrc_dt(:,iPhyP(isp,m)) = dPhyP_dt(isp,m,:)
              END DO
            END DO 
            DO m=1,Nzoo    
              DO isp=1,N_Csp     
                dtrc_dt(:,iZooC(isp,m)) = dZooC_dt(isp,m,:)
              END DO
            END DO 
            DO m=1,Nzoo    
              DO isp=1,N_Nsp     
                dtrc_dt(:,iZooN(isp,m)) = dZooN_dt(isp,m,:)
              END DO
            END DO 
            DO m=1,Nzoo    
              DO isp=1,N_Psp     
                dtrc_dt(:,iZooP(isp,m)) = dZooP_dt(isp,m,:) 
              END DO
            END DO 
            DO m=1,Npim    
              DO isp=1,N_Csp     
                dtrc_dt(:,iPIC(isp,m)) = dPIC_dt (isp,m,:)  
              END DO
            END DO 
# if defined BLUE_TIDE         
            DO isp=1,N_Ssp     
              dtrc_dt(:,iH2S(isp)) = dH2S_dt(isp,:)        
            END DO       
            DO isp=1,N_Ssp     
              dtrc_dt(:,iS0 (isp)) = dS0_dt (isp,:)        
            END DO       
# endif
#if defined COT_STARFISH         
            dtrc_dt(:,iCOTe) = dCOTe_d(:)      &   ! COTe(N): COT starfish egg (umol L-1)
            dtrc_dt(:,iCOTl) = dCOTl_d(:)      &   ! COTl(N): COT starfish larvae (umol L-1)
#endif

#if defined DIAGNOSTICS_BIO
            DiaBio3d(i,j,:,ipHt_) = pH(:)
            DiaBio3d(i,j,:,iWarg) = Warg(:)
            DiaBio3d(i,j,:,iWcal) = Wcal(:)
!!! mons light model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>KM:Add
# if defined LIGHT_MODEL
            DiaBio3d(i,j,:,iLight) = PFDk(:)
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<KM:Add

            DiaBio2d(i,j,iCO2fx) = ssCO2flux
            DiaBio2d(i,j,ipCO2) = ssfCO2
            DiaBio2d(i,j,iO2fx) = ssO2flux
            DiaBio2d(i,j,iPARb) = PFDbott
            DiaBio2d(i,j,iTau_) = tau
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
  
              DO itrc=1,NAT
  
                IF(dtrc_dt(k,itrc)*0.0_r8 /= 0.0_r8) THEN  !!!---------Error Handling: Check NAN
!                  write(50,*) i,j,k,itrc,dtrc_dt(k,itrc),ssO2flux, ssCO2flux,rmask(i,j) 
!                  write(50,*) t(i,j,k,nnew,:)
!                  write(50,*) t(i,j,k,nstp,:)
                  dtrc_dt(k,itrc)=0.0_r8
                END IF
                
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)                    &
    &                                +dtrc_dt(k,itrc)*dt(ng)*Hz(i,j,k)
  
              END DO
  
  
              DO itrc=1,NBT
                ibio=idbio(itrc)
  
                IF(dtrc_dt(k,ibio)*0.0_r8 /= 0.0_r8) THEN  !!!---------Error Handling: Check NAN
!                  write(50,*) i,j,k,itrc,dtrc_dt(k,ibio),ssO2flux, ssCO2flux,rmask(i,j) 
!                  write(50,*) t(i,j,k,nnew,:)
!                  write(50,*) t(i,j,k,nstp,:)
                  dtrc_dt(k,ibio)=0.0_r8
                END IF
                
                t(i,j,k,nnew,ibio)=t(i,j,k,nnew,ibio)                    &
    &                                +dtrc_dt(k,ibio)*dt(ng)*Hz(i,j,k)
      
  
                t(i,j,k,nnew,ibio)=MAX(0.0_r8,t(i,j,k,nnew,ibio))!!!---------Error Handling
  
                
              END DO
            END DO
#ifdef MASKING
          END IF
#endif
        END DO
      END DO
!-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE biology_tile
      

