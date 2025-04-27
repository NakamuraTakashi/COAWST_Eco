!!!=== Copyright (c) 2013-2025 Takashi NAKAMURA  =====!!!
!!!         - Institute of Science Tokyo -            !!!
!!!                                                   !!!
!!!          Sediment Biogeochemical Module           !!!
!!!            Developed by Masa Muto 2023-           !!!
!!!===================================================!!!

! ================================================================================================================ !
!  STRUCTURE OF SEDIMENT COLUMN
! ================================================================================================================ !
!   zw(Nsed):    Sediment depths, at horizontal RHO-points and vertical W-points (cm)
!   zr(Nsed):    Sediment depths, at horizontal RHO-points and vertical RHO-points (cm) 
!   dzw(Nsed):   Vertical W-grid spacing (cm)
!   dzr(Nsed):   Vertical RHO-grid spacing (cm)
!   C(Nsed):     Consentration in porewater or dry weight, SEDECO(ng)%C(Nsed,:,:)  (umol L-1),(nmol g-1)
!   poro(Nsed):  Porosity = Vwater/(Vwater+Vsed) (non-dimensional: 0.0-1.0)
!   Fw(Nsed):    Flux, dwonward is positive (nmol cm-2 s-1)
!-----------------------------------------------------------------------                       
!                                                                                                        
!     rho point                                       C(0) = Camb (ambient value)                          
!       (i,j)                                       -------                                      ------------------
!     ____|_______ __0 _______0cm (surface) _________________________________ ↓ Fw(1)  ____ 0cm ___   } dzw(1)=DBL    
!   /     |      /|           |                                                             |                       
!  /___________ / |--1        |         } dzr(1)      C(1)      poro(1)                   --zr(1)------------------
!  |           |  |__1 _______zw(1)__________________________________________ ↓ Fw(2) ______|         } dzw(2)      
!  |           | /|           |                                                             |             
!  |___________|/ |--2        |         } dzr(2)      C(2)      poro(2)                   --zr(2)------------------
!  |           |  |__2 _______zw(2)__________________________________________ ↓ Fw(3) ______|                  
!  |           | /|           |                                                             :                  
!  |___________|/ :                                                                                            
!  |           |  :           :                                                                 -------------------
!  :           :  |__Nsed-2_ _zw(Nsed-2)_____________________________________ ↓ Fw(Nsed-1)__:        } dzw(Nsed-1) 
!  :           : /|           |                                                             |           
!  |___________|/ |--Nsed-1   |         } dzr(Nsed-1) C(Nsed-1) poro(Nsed-1)              --zr(Nsed-1)-------------
!  |           |  |__Nsed-1___zw(Nsed-1)_____________________________________ ↓ Fw(Nsed) ___|        } dzw(Nsed)   
!  |           | /|           |                                                             |         
!  |___________|/ |--Nsed     |         } dzr(Nsed)   C(Nsed)   poro(Nsed)                --zr(Nsed)---------------
!  |           |  |__Nsed   __zw(Nsed) ______________ = bottom _____________________________|                  
!  |           | /                                      boundary                                                                 
!  |___________|/                                       value                                                 
!                                                                                           
! ================================================================================================================ !
 
#include "cppdefs.h"

!!!**** MODULE OF SEDIMENT ECOSYSTEM MODEL ************************************
!!! **********************************************************************
!!!  Sediment model
!!! **********************************************************************

MODULE mod_sedecosys
#if defined SEDIMENT_ECOSYS

  implicit none

! Variables Declaration

!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

  TYPE T_SEDECO
! general parameters
    real(8), pointer :: poro(:,:,:)     !! Porosity (cm_w3 cm_s-3)
    real(8), pointer :: rhos_d(:,:,:)   !! dry soil density (g　cm_s-3)
    real(8), pointer :: dzr(:)          !! layer thickness (cm)
    real(8), pointer :: dzw(:)          !! layer thickness (cm)
    real(8), pointer :: zr(:)           !! sediment depth  (cm)
    real(8), pointer :: zw(:)           !! sediment depth  (cm)

    real(8), pointer :: Tmp(:,:,:)      !! Temperature    (oC)
    real(8), pointer :: Sal(:,:,:)      !! Salinity       (PSU)
    real(8), pointer :: O2 (:,:,:)      !! Dissolved Oxygen    (umol l-1)
    real(8), pointer :: TA (:,:,:)      !! Total Alkanity (umol kg-1)
    real(8), pointer :: DIC(:,:,:,:)    !! Total dissolved inorganic carbon (umol kg-1)
    real(8), pointer :: NO3(:,:,:,:)    !! Nitrate     (umol l-1)
    real(8), pointer :: NH4(:,:,:,:)    !! Ammonium    (umol l-1)
    real(8), pointer :: PO4(:,:,:,:)    !! Phosphate   (umol l-1)
    real(8), pointer :: DOC(:,:,:,:,:)  !! Dissolved organic carbon   (umol l-1)
    real(8), pointer :: POC(:,:,:,:,:)  !! Particulate organic carbon (nmol g-1)
    real(8), pointer :: DON(:,:,:,:,:)  !! Dissolved organic nitrogen  (umol l-1)
    real(8), pointer :: PON(:,:,:,:,:)  !! Particulate organic nitrogen (nmol g-1)
    real(8), pointer :: DOP(:,:,:,:,:)  !! Dissolved organic phosphorus  (umol l-1)
    real(8), pointer :: POP(:,:,:,:,:)  !! Particulate organic phosphorus (nmol g-1)
    real(8), pointer :: PIC(:,:,:,:)    !! Dead Calcium carbonate (nmol g-1)
! Sulfur dynamics
    real(8), pointer :: Mn2(:,:,:)    !! Manganese ion        (umol l-1)
    real(8), pointer :: MnO2(:,:,:)   !! Manganese dioxide    (nmol g-1)
    real(8), pointer :: Fe2 (:,:,:)       !! iron(II)          (umol l-1)
    real(8), pointer :: FeS (:,:,:,:)       !! iron sulfide      (nmol g-1)
    real(8), pointer :: FeS2(:,:,:,:)      !! pyrite            (nmol g-1)
    real(8), pointer :: FeOOH(:,:,:)     !! iron hydroxide    (nmol g-1)
    real(8), pointer :: FeOOH_PO4(:,:,:,:) !! iron-bound phosphate (FeOOH=PO43-)   (nmol g-1)
    real(8), pointer :: H2S(:,:,:,:)       !! hydrogen sulfide         (umol l-1)
    real(8), pointer :: SO4(:,:,:,:)      !! sulfate   (umol l-1)
    real(8), pointer :: S0 (:,:,:,:)       !! sulfur    (nmol g-1)


!!! yuta_seagrass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# if defined SEAGRASS
#  if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer :: sg_dDIC_dt(:,:,:,:,:) ! (umol L-1 s-1) change in DIC due to flux from seagrass (positive means seagrass to sediment)
    real(8), pointer :: sg_dDOC_dt(:,:,:,:,:) ! (umol L-1 s-1) change in DOC due to flux from seagrass (positive means seagrass to sediment)
    real(8), pointer :: sg_dO2_dt (:,:,:,:) ! (umol L-1 s-1) change in DO due to flux from seagrass (positive means seagrass to sediment)
#  endif
#  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer :: sg_dNO3_dt(:,:,:,:,:) ! (umol L-1 s-1) change in NO3 due to flux from seagrass (positive means seagrass to sediment)
    real(8), pointer :: sg_dNH4_dt(:,:,:,:,:) ! (umol L-1 s-1) change in NH4 due to flux from seagrass (positive means seagrass to sediment)
    real(8), pointer :: sg_dPO4_dt(:,:,:,:,:) ! (umol L-1 s-1) change in PO4 due to flux from seagrass (positive means seagrass to sediment)
#  endif
#  if defined SEAGRASS_ROOT_POM
    real(8), pointer :: sg_dPOC_dt(:,:,:,:,:) ! (umol L-1 s-1) change in POC due to root death in sediment (positive means seagrass to sediment)
    real(8), pointer :: sg_dPON_dt(:,:,:,:,:) ! (umol L-1 s-1) change in PON due to root death in sediment (positive means seagrass to sediment)
    real(8), pointer :: sg_dPOP_dt(:,:,:,:,:) ! (umol L-1 s-1) change in POP due to root death in sediment (positive means seagrass to sediment)
#  endif
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

  END TYPE T_SEDECO
  TYPE (T_SEDECO), allocatable :: SEDECO(:)

CONTAINS


!!! **********************************************************************
!!!  Initial conditions for sediment ecosystem model
!!! **********************************************************************

  SUBROUTINE allocate_sedecosys(ng, Ngrids, LBi, UBi, LBj, UBj)

    USE mod_reef_ecosys_param
    
    implicit none

    ! input parameters
    integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj

    ! allocate sizes of each parameters for (LBi:UBi, LBj:UBj)
    IF (ng.eq.1) allocate( SEDECO(Ngrids) )
    !! General
    allocate( SEDECO(ng)%poro(0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%rhos_d(0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%dzr(Nsed) )
    allocate( SEDECO(ng)%dzw(Nsed) )
    allocate( SEDECO(ng)%zr (Nsed) )
    allocate( SEDECO(ng)%zw (Nsed) )
    allocate( SEDECO(ng)%Tmp(0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%Sal(0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%O2 (0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%TA (0:Nsed,LBi:UBi,LBj:UBj) )  
    allocate( SEDECO(ng)%DIC(N_Csp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%NO3(N_Nsp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%NH4(N_Nsp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%PO4(N_Psp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%DOC(N_Csp,Ndom,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%POC(N_Csp,Npom,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%DON(N_Nsp,Ndom,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%PON(N_Nsp,Npom,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%DOP(N_Psp,Ndom,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%POP(N_Psp,Npom,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%PIC(N_Csp,0:Nsed,LBi:UBi,LBj:UBj) )
    !! Sulfur dynamics
    allocate( SEDECO(ng)%Mn2 (0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%MnO2(0:Nsed,LBi:UBi,LBj:UBj) )

    allocate( SEDECO(ng)%Fe2 (0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%FeS (N_Ssp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%FeS2(N_Ssp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%FeOOH(0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%FeOOH_PO4(N_Psp,0:Nsed,LBi:UBi,LBj:UBj) )

    allocate( SEDECO(ng)%H2S(N_Ssp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%SO4(N_Ssp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%S0 (N_Ssp,0:Nsed,LBi:UBi,LBj:UBj) )


!!! yuta_seagrass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# if defined SEAGRASS
#  if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    allocate( SEDECO(ng)%sg_dDIC_dt(N_Csp,Nsed,Nsg,LBi:UBi,LBj:UBj) ) ! (umol L-1 s-1) change in DIC due to flux from seagrass (positive means seagrass to sediment)
    allocate( SEDECO(ng)%sg_dDOC_dt(N_Csp,Nsed,Nsg,LBi:UBi,LBj:UBj) ) ! (umol L-1 s-1) change in DOC due to flux from seagrass (positive means seagrass to sediment)
    allocate( SEDECO(ng)%sg_dO2_dt       (Nsed,Nsg,LBi:UBi,LBj:UBj) ) ! (nmol g-1 s-1) change in DO due to flux from seagrass (positive means seagrass to sediment)
#  endif
#  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    allocate( SEDECO(ng)%sg_dNO3_dt(N_Nsp,Nsed,Nsg,LBi:UBi,LBj:UBj) ) ! (umol L-1 s-1) change in NO3 due to flux from seagrass (positive means seagrass to sediment)
    allocate( SEDECO(ng)%sg_dNH4_dt(N_Nsp,Nsed,Nsg,LBi:UBi,LBj:UBj) ) ! (umol L-1 s-1) change in NH4 due to flux from seagrass (positive means seagrass to sediment)
    allocate( SEDECO(ng)%sg_dPO4_dt(N_Psp,Nsed,Nsg,LBi:UBi,LBj:UBj) ) ! (umol L-1 s-1) change in PO4 due to flux from seagrass (positive means seagrass to sediment)
#  endif    
#  if defined SEAGRASS_ROOT_POM
    allocate( SEDECO(ng)%sg_dPOC_dt(N_Csp,Nsed,Nsg,LBi:UBi,LBj:UBj) ) ! (umol L-1 s-1) change in POC due to root death in sediment (positive means seagrass to sediment)
#   if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    allocate( SEDECO(ng)%sg_dPON_dt(N_Nsp,Nsed,Nsg,LBi:UBi,LBj:UBj) ) ! (umol L-1 s-1) change in PON due to root death in sediment (positive means seagrass to sediment)
    allocate( SEDECO(ng)%sg_dPOP_dt(N_Psp,Nsed,Nsg,LBi:UBi,LBj:UBj) ) ! (umol L-1 s-1) change in POP due to root death in sediment (positive means seagrass to sediment)
#   endif  
#  endif  
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

    RETURN
  END SUBROUTINE allocate_sedecosys


!!! **********************************************************************
!!!  Initial conditions for sediment ecosystem model
!!! **********************************************************************

  SUBROUTINE initialize_sedecosys(ng, LBi, UBi, LBj, UBj, start_of_new_run)

    USE mod_reef_ecosys_param
    USE mod_geochem
    ! USE mod_readinput
    
    implicit none

    ! input parameters
    integer, intent(in) :: ng, LBi, UBi, LBj, UBj
    logical, intent(in) :: start_of_new_run               ! TRUE = initialize coral, seagass, macroalgae, sediment from start; FALSE = continue from previous run

    integer k,i,j ! symbols for loop calculations
    !! depth : Diffusive Boundary Layer (DBL)
    real(8), parameter :: dbl = 0.03d0 ! DBL:difussive boundary layer (cm)

    real(8), parameter :: dzmin = 0.1d0
    real(8), parameter :: dzmax = 2.0d0
    real(8), parameter :: k_lg = 0.3d0

    !! porosity   ( poro = a + b * exp(c * z)   (fossing et al 2004) )
    !! Kasih 2009
    ! real(8), parameter :: coefa_poro = 0.88d0
    ! real(8), parameter :: coefa_poro = 0.90d0
    ! real(8), parameter :: coefb_poro = 0.04d0
    ! real(8), parameter :: coefc_poro = -0.4d0
    !! Fossing 2004
    real(8), parameter :: coefa_poro = 0.763d0
    real(8), parameter :: coefb_poro = 0.086d0
    real(8), parameter :: coefc_poro = -0.216d0

    real(8), parameter :: rPCph =  1.0d0/106.0d0    ! (no dim.) algal C:P ratio (Redfield ratio)
    real(8), parameter :: rNCph = 16.0d0/106.0d0   ! (no dim.) algal C:N ratio (Redfield ratio)        from analysis

    real(8) :: tmp,cSO4
# if defined SEDECO_CSV_RESTART
    integer fid
# endif
! - - - - - - - - - - - - - - - - - - - - - - - -
! ********************************************************************* !
!   physical properties of sediment layers 
! ********************************************************************* !

    ! layer thickness and depth
    do k = 1,Nsed
      ! The logistic equation is applied for generating layer tickness (dzr; cm)
      SEDECO(ng)%dzr(k) = dzmin*dzmax/   &
                          (dzmin+(dzmax-dzmin)*exp(-k_lg*dble(k-1)))
    enddo
    SEDECO(ng)%dzw(1)=SEDECO(ng)%dzr(1)*0.5d0
    SEDECO(ng)%zw(1)=SEDECO(ng)%dzw(1)
    SEDECO(ng)%zr(1)=SEDECO(ng)%dzr(1)
    do k = 2,Nsed
      SEDECO(ng)%zr(k)  = SEDECO(ng)%zr(k-1) + SEDECO(ng)%dzr(k)
      SEDECO(ng)%dzw(k) = SEDECO(ng)%zr(k) - SEDECO(ng)%zr(k-1)
      SEDECO(ng)%zw(k)  = SEDECO(ng)%zw(k-1) + SEDECO(ng)%dzw(k)
    enddo
 
!    do k = 1, Nsed(ng)
!      SEDECO(ng)%z(k) = SedEcoLayerDepths(ng, k)
!    enddo
!    SEDECO(ng)%dz(1) = SedEcoLayerDepths(ng, 1)
!    do k = 2, Nsed(ng)
!      SEDECO(ng)%dz(k) = SedEcoLayerDepths(ng, k) - SedEcoLayerDepths(ng, k-1)
!    enddo

    !!! porosity
    ! SEDECO(ng)%poro(:,:,1) = 1.0d0
    do k = 0,Nsed
      !!! KEEP CONSTANT! (need update)
      !!! Still have an error for diffusion advection fluxes calculation 
      !!! if depth-varied porosity is applied.
      SEDECO(ng)%poro(k,:,:) = 0.5d0  !! constant porosity

      ! SEDECO(ng)%poro(k,:,:) = coefa_poro + coefb_poro * exp(coefc_poro * SEDECO(ng)%zr(k))
      !! --- Shizugawa --- !!
      ! SEDECO(ng)%poro(k,:,:) = 0.6007d0 - 0.006d0 * log(SEDECO(ng)%zr(k))
      !! --- Shiraho --- !!
      ! SEDECO(ng)%poro(k,:,:) = 0.6007d0 - 0.006d0 * log(SEDECO(ng)%zr(k))
      !! --- Fukido --- !!
      ! SEDECO(ng)%poro(k,:,:) = 0.6007d0 - 0.006d0 * log(SEDECO(ng)%zr(k))

      !! soil density (This must be given as an input data actually)
      SEDECO(ng)%rhos_d(k,:,:) = 2.5d0 ! (g/cm3)   ! Note: depth varied density is not properly implemented in geochem
    enddo

# if defined SEDIMENT_TESTMODE
      write(50,'(*(g0:,", "))') SEDECO(ng)%dzw(:)
      write(50,'(*(g0:,", "))') SEDECO(ng)%dzr(:)
      write(50,'(*(g0:,", "))') SEDECO(ng)%zw(:)
      write(50,'(*(g0:,", "))') SEDECO(ng)%zr(:)
      write(50,'(*(g0:,", "))') SEDECO(ng)%poro  (:,LBi,LBj)
      write(50,'(*(g0:,", "))') SEDECO(ng)%rhos_d(:,LBi,LBj)
# endif

    !  Set initial conditions

    if ( start_of_new_run ) then

# if defined SEDECO_CSV_RESTART
!     !!! -------------- initial concentration from sed initial concentration ----------------- !!!
!      ! ! General
!      fid = 199
!      open(fid, file='sedeco_rst.csv', status='old')
      open(newunit=fid, file='sedeco_rst.csv', status='old')
      read(fid,*)   ! read header
      read(fid,*)   ! read header
      DO k=0,Nsed
        read(fid,*) tmp                     &
        , SEDECO(ng)%Tmp(k,LBi,LBj)         &
        , SEDECO(ng)%Sal(k,LBi,LBj)         & 
        , SEDECO(ng)%O2 (k,LBi,LBj)         & 
        , SEDECO(ng)%TA (k,LBi,LBj)         & 
        , SEDECO(ng)%DIC(:,k,LBi,LBj)       & 
        , SEDECO(ng)%NO3(:,k,LBi,LBj)       &
        , SEDECO(ng)%NH4(:,k,LBi,LBj)       &
        , SEDECO(ng)%PO4(:,k,LBi,LBj)       &
        , SEDECO(ng)%DOC(:,:,k,LBi,LBj)     &
        , SEDECO(ng)%POC(:,:,k,LBi,LBj)     &
        , SEDECO(ng)%DON(:,:,k,LBi,LBj)     &
        , SEDECO(ng)%PON(:,:,k,LBi,LBj)     &
        , SEDECO(ng)%DOP(:,:,k,LBi,LBj)     &
        , SEDECO(ng)%POP(:,:,k,LBi,LBj)     &
        , SEDECO(ng)%PIC(:,k,LBi,LBj)       &
        , SEDECO(ng)%Mn2 (k,LBi,LBj)        &
        , SEDECO(ng)%MnO2(k,LBi,LBj)        &
        , SEDECO(ng)%Fe2 (k,LBi,LBj)        &
        , SEDECO(ng)%FeS2(:,k,LBi,LBj)      &
        , SEDECO(ng)%FeOOH(k,LBi,LBj)       &
        , SEDECO(ng)%FeOOH_PO4(:,k,LBi,LBj) &
        , SEDECO(ng)%H2S(:,k,LBi,LBj)       &
        , SEDECO(ng)%SO4(:,k,LBi,LBj)       &
        , SEDECO(ng)%S0 (:,k,LBi,LBj)       
      END DO
      close(fid)

      DO j=LBj,UBj
        DO i=LBi,UBi
          DO k=0,Nsed
            SEDECO(ng)%Tmp(k,i,j)         = SEDECO(ng)%Tmp(k,LBi,LBj)        
            SEDECO(ng)%Sal(k,i,j)         = SEDECO(ng)%Sal(k,LBi,LBj)         
            SEDECO(ng)%O2 (k,i,j)         = SEDECO(ng)%O2 (k,LBi,LBj)         
            SEDECO(ng)%TA (k,i,j)         = SEDECO(ng)%TA (k,LBi,LBj)         
            SEDECO(ng)%DIC(:,k,i,j)       = SEDECO(ng)%DIC(:,k,LBi,LBj)       
            SEDECO(ng)%NO3(:,k,i,j)       = SEDECO(ng)%NO3(:,k,LBi,LBj)      
            SEDECO(ng)%NH4(:,k,i,j)       = SEDECO(ng)%NH4(:,k,LBi,LBj)      
            SEDECO(ng)%PO4(:,k,i,j)       = SEDECO(ng)%PO4(:,k,LBi,LBj)      
            SEDECO(ng)%DOC(:,:,k,i,j)     = SEDECO(ng)%DOC(:,:,k,LBi,LBj)    
            SEDECO(ng)%POC(:,:,k,i,j)     = SEDECO(ng)%POC(:,:,k,LBi,LBj)    
            SEDECO(ng)%DON(:,:,k,i,j)     = SEDECO(ng)%DON(:,:,k,LBi,LBj)    
            SEDECO(ng)%PON(:,:,k,i,j)     = SEDECO(ng)%PON(:,:,k,LBi,LBj)    
            SEDECO(ng)%DOP(:,:,k,i,j)     = SEDECO(ng)%DOP(:,:,k,LBi,LBj)    
            SEDECO(ng)%POP(:,:,k,i,j)     = SEDECO(ng)%POP(:,:,k,LBi,LBj)    
            SEDECO(ng)%PIC(:,k,i,j)       = SEDECO(ng)%PIC(:,k,LBi,LBj)       
            SEDECO(ng)%Mn2 (k,i,j)        = SEDECO(ng)%Mn2 (k,LBi,LBj)       
            SEDECO(ng)%MnO2(k,i,j)        = SEDECO(ng)%MnO2(k,LBi,LBj)       
            SEDECO(ng)%Fe2 (k,i,j)        = SEDECO(ng)%Fe2 (k,LBi,LBj)       
            SEDECO(ng)%FeS2(:,k,i,j)      = SEDECO(ng)%FeS2(:,k,LBi,LBj)     
            SEDECO(ng)%FeOOH(k,i,j)       = SEDECO(ng)%FeOOH(k,LBi,LBj)      
            SEDECO(ng)%FeOOH_PO4(:,k,i,j) = SEDECO(ng)%FeOOH_PO4(:,k,LBi,LBj)
            SEDECO(ng)%H2S(:,k,i,j)       = SEDECO(ng)%H2S(:,k,LBi,LBj)      
            SEDECO(ng)%SO4(:,k,i,j)       = SEDECO(ng)%SO4(:,k,LBi,LBj)      
            SEDECO(ng)%S0 (:,k,i,j)       = SEDECO(ng)%S0 (:,k,LBi,LBj)      
          END DO
        END DO
      END DO
# else

    !!! ------------------- For determinating initial concentration ----------------------- !!!
      ! General
      DO j=LBj,UBj
        DO i=LBi,UBi
          DO k=0,Nsed
            SEDECO(ng)%Tmp(k,i,j)  = 25.0d0     
            SEDECO(ng)%Sal(k,i,j)  = 34.0d0
            SEDECO(ng)%O2 (k,i,j)  = 50.0d0        !DO  (umol L-1)
            SEDECO(ng)%TA(k,i,j)  = 2232.0d0 ! TA (umol kg-1)
            cSO4 = 0.02824d0*34.0d0/35.0d0 *1.0d6  ! SO4 (umol L-1)
      
            ! initialize 
            SEDECO(ng)%DIC(:,k,i,j)   = 0.0d0        !DIC  (umol L-1)
            SEDECO(ng)%NO3(:,k,i,j)   = 0.0d0     !NO3  (umol L-1) 
            SEDECO(ng)%NH4(:,k,i,j)   = 0.0d0      !NH4  (umol L-1) 
            SEDECO(ng)%PO4(:,k,i,j)   = 0.0d0      !PO4  (umol L-1) 
            SEDECO(ng)%DOC(:,:,k,i,j) = 0.0d0
            SEDECO(ng)%POC(:,:,k,i,j) = 0.0d0
            SEDECO(ng)%DON(:,:,k,i,j) = 0.0d0
            SEDECO(ng)%PON(:,:,k,i,j) = 0.0d0
            SEDECO(ng)%DOP(:,:,k,i,j) = 0.0d0
            SEDECO(ng)%POP(:,:,k,i,j) = 0.0d0
            SEDECO(ng)%PIC(:,k,i,j)   = 0.0d0        !
            SEDECO(ng)%FeOOH_PO4(:,k,i,j) = 0.0d0    !FeOOH=PO4  (nmol g-1) 
            SEDECO(ng)%FeS (:,k,i,j) = 0.0d0          !FeS  (nmol g-1) 
            SEDECO(ng)%FeS2(:,k,i,j) = 0.0d0         !FeS2  (nmol g-1) 
            SEDECO(ng)%H2S (:,k,i,j) = 0.0d0     !H2S  (umol L-1)
            SEDECO(ng)%SO4 (:,k,i,j) = 0.0d0     !SO4  (umol L-1)
            SEDECO(ng)%S0  (:,k,i,j) = 0.0d0     !S0  (nmol g-1)

            ! Set Total OM values
            SEDECO(ng)%DIC(iCt,k,i,j)        = 1915.0d0 !DIC  (umol L-1)
            SEDECO(ng)%DOC(iCt,iLDOM,k,i,j)  = 10.0d0   !LDOC (umol L-1) 
            SEDECO(ng)%DOC(iCt,iRDOM,k,i,j)  = 50.0d0   !RDOC (umol L-1) 
            SEDECO(ng)%POC(iCt,iLPOM,k,i,j)  = OrgC_sed*1.0d-6!LPOC (nmol g-1)
            SEDECO(ng)%POC(iCt,iRPOM,k,i,j)  = OrgC_sed   !RPOC (nmol g-1)
            SEDECO(ng)%POC(iCt,iCPOM,k,i,j)  = 0.0d0 !CPOC (nmol g-1)
            SEDECO(ng)%PIC(iCt,k,i,j) = SEDECO(ng)%poro(k,i,j)*rCaCO3_sed  & ! poro[no dim.]*rCaCO3[no dim.]
                                         *2.7d0/100.0d0*1.0d9              & ! *2.7[g/cm3]/ 100[g/mol]*nano(1.0d9)
                                         /SEDECO(ng)%rhos_d(k,i,j)           ! /rhos_d[g(DW)/cm3] = CaCO3 (nmol g(DW)-1)
!            SEDECO(ng)%POC(iCt,iCPOM,k,i,j)  = 1.0d0 !CPOC (nmol g-1)   ! for CARBON_TRACE test
!            SEDECO(ng)%POC(iC13,iCPOM,k,i,j)  = 1.0d0 !CPOC (nmol g-1)  ! for CARBON_TRACE test
#  if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
            SEDECO(ng)%DIC(iC13,k,i,j)        = Ci_from_Ct_delta(SEDECO(ng)%DIC(iCt,k,i,j), 2.0d0, R13C_VPDB) !DIC  (umol L-1)
            SEDECO(ng)%DOC(iC13,iLDOM,k,i,j)  = Ci_from_Ct_delta(SEDECO(ng)%DOC(iCt,iLDOM,k,i,j), -20.0d0, R13C_VPDB) !LDOC (umol L-1) 
            SEDECO(ng)%DOC(iC13,iRDOM,k,i,j)  = Ci_from_Ct_delta(SEDECO(ng)%DOC(iCt,iRDOM,k,i,j), -20.0d0, R13C_VPDB) !RDOC (umol L-1) 
            SEDECO(ng)%POC(iC13,iLPOM,k,i,j)  = Ci_from_Ct_delta(SEDECO(ng)%POC(iCt,iLPOM,k,i,j), -20.0d0, R13C_VPDB) !LPOC (nmol g-1) 
            SEDECO(ng)%POC(iC13,iRPOM,k,i,j)  = Ci_from_Ct_delta(SEDECO(ng)%POC(iCt,iRPOM,k,i,j), -20.0d0, R13C_VPDB) !RPOC (nmol g-1)
            SEDECO(ng)%POC(iC13,iCPOM,k,i,j)  = Ci_from_Ct_delta(SEDECO(ng)%POC(iCt,iCPOM,k,i,j), -20.0d0, R13C_VPDB) !RPOC (nmol g-1)
            SEDECO(ng)%PIC(iC13,k,i,j)        = Ci_from_Ct_delta(SEDECO(ng)%PIC(iCt,k,i,j), -2.0d0, R13C_VPDB) !CaCO3 (nmol g-1)
#   if defined CLUMPED_ISOTOPE
            SEDECO(ng)%DIC(iD47,k,i,j)        = ???? !DIC  (umol L-1)
            SEDECO(ng)%DOC(iD47,iLDOM,k,i,j)  = ???? !LDOC (umol L-1) 
            SEDECO(ng)%DOC(iD47,iRDOM,k,i,j)  = ???? !RDOC (umol L-1) 
            SEDECO(ng)%POC(iD47,iLPOM,k,i,j)  = ???? !LPOC (nmol g-1) 
            SEDECO(ng)%POC(iD47,iRPOM,k,i,j)  = ???? !RPOC (nmol g-1)
            SEDECO(ng)%POC(iD47,iCPOM,k,i,j)  = ???? !RPOC (nmol g-1)
            SEDECO(ng)%PIC(iD47,k,i,j)        = ???? !CaCO3 (nmol g-1)
#   endif
#  endif
            SEDECO(ng)%NO3(iNt,k,i,j)       = 0.2d0        !NO3  (umol L-1) 
            SEDECO(ng)%NH4(iNt,k,i,j)       = 0.26d0       !NH4  (umol L-1) 
            SEDECO(ng)%DON(iNt,iLDOM,k,i,j) = SEDECO(ng)%DOC(iCt,iLDOM,k,i,j) * rNCph   !DON  (umol L-1) 
            SEDECO(ng)%DON(iNt,iRDOM,k,i,j) = SEDECO(ng)%DOC(iCt,iRDOM,k,i,j) * rNCph   !DON  (umol L-1) 
            SEDECO(ng)%PON(iNt,iLPOM,k,i,j) = SEDECO(ng)%POC(iCt,iLPOM,k,i,j) * rNCph   !PON  (umol L-1) 
            SEDECO(ng)%PON(iNt,iRPOM,k,i,j) = SEDECO(ng)%POC(iCt,iRPOM,k,i,j) * rNCph   !PON  (umol L-1)
            SEDECO(ng)%PON(iNt,iCPOM,k,i,j) = SEDECO(ng)%POC(iCt,iCPOM,k,i,j) * rNCph   !PON  (umol L-1)
!            SEDECO(ng)%PON(iN15,iCPOM,k,i,j) = SEDECO(ng)%POC(iCt,iCPOM,k,i,j) * rNCph   !PON  (umol L-1) ! for NITROGEN_TRACE test
#  if defined NITROGEN_ISOTOPE
            SEDECO(ng)%NO3(iN15,k,i,j)       = Ci_from_Ct_delta(SEDECO(ng)%NO3(iNt,k,i,j)      , 2.0d0, R15N_AIR)    !NO3  (umol L-1) 
            SEDECO(ng)%NH4(iN15,k,i,j)       = Ci_from_Ct_delta(SEDECO(ng)%NH4(iNt,k,i,j)      , 2.0d0, R15N_AIR)   !NH4  (umol L-1) 
            SEDECO(ng)%DON(iN15,iLDOM,k,i,j) = Ci_from_Ct_delta(SEDECO(ng)%DON(iNt,iLDOM,k,i,j), 2.0d0, R15N_AIR) !DON  (umol L-1) 
            SEDECO(ng)%DON(iN15,iRDOM,k,i,j) = Ci_from_Ct_delta(SEDECO(ng)%DON(iNt,iRDOM,k,i,j), 2.0d0, R15N_AIR) !DON  (umol L-1) 
            SEDECO(ng)%PON(iN15,iLPOM,k,i,j) = Ci_from_Ct_delta(SEDECO(ng)%PON(iNt,iLPOM,k,i,j), 2.0d0, R15N_AIR) !PON  (umol L-1) 
            SEDECO(ng)%PON(iN15,iRPOM,k,i,j) = Ci_from_Ct_delta(SEDECO(ng)%PON(iNt,iRPOM,k,i,j), 2.0d0, R15N_AIR) !PON  (umol L-1)
            SEDECO(ng)%PON(iN15,iCPOM,k,i,j) = Ci_from_Ct_delta(SEDECO(ng)%PON(iNt,iCPOM,k,i,j), 2.0d0, R15N_AIR) !PON  (umol L-1)
#  endif

            SEDECO(ng)%PO4(iPt,k,i,j)       = 0.04d0        !PO4  (umol L-1) 
            SEDECO(ng)%DOP(iPt,iLDOM,k,i,j) = SEDECO(ng)%DOC(iCt,iLDOM,k,i,j) * rPCph   !DOP  (umol L-1) 
            SEDECO(ng)%DOP(iPt,iRDOM,k,i,j) = SEDECO(ng)%DOC(iCt,iRDOM,k,i,j) * rPCph   !DOP  (umol L-1) 
            SEDECO(ng)%POP(iPt,iLPOM,k,i,j) = SEDECO(ng)%POC(iCt,iLPOM,k,i,j) * rPCph   !POP  (umol L-1) 
            SEDECO(ng)%POP(iPt,iRPOM,k,i,j) = SEDECO(ng)%POC(iCt,iRPOM,k,i,j) * rPCph   !POP  (umol L-1)
            SEDECO(ng)%POP(iPt,iCPOM,k,i,j) = SEDECO(ng)%POC(iCt,iCPOM,k,i,j) * rPCph   !POP  (umol L-1)
!            SEDECO(ng)%POP(iP1,iCPOM,k,i,j) = SEDECO(ng)%POC(iCt,iCPOM,k,i,j) * rPCph   !POP  (umol L-1) ! for PHOSPHOROUS_TRACE test
      
            SEDECO(ng)%FeOOH_PO4(iPt,k,i,j) = 0.0d0    !FeOOH=PO4  (nmol g-1) 
      
            SEDECO(ng)%Mn2(k,i,j)   = 0.0d0       !Mn2+  (umol L-1) 
            SEDECO(ng)%MnO2(k,i,j)  = MnO2_sed    !MnO2  (nmol g-1) 
            SEDECO(ng)%Fe2 (k,i,j)  = 0.0d0       !Fe2+  (umol L-1) 
            SEDECO(ng)%FeOOH(k,i,j) = FeOOH_sed   !FeOOH  (nmol g-1)
      
            SEDECO(ng)%FeS (iSt,k,i,j) = 0.0d0     !FeS  (nmol g-1) 
            SEDECO(ng)%FeS2(iSt,k,i,j) = FeS2_sed  !FeS2  (nmol g-1) 
            SEDECO(ng)%H2S (iSt,k,i,j) = 0.0d0     !H2S  (umol L-1)
            SEDECO(ng)%SO4 (iSt,k,i,j) = cSO4  ! SO4 (umol L-1)   !SO4  (umol L-1)
!            SEDECO(ng)%SO4 (iS34,k,i,j) = cSO4  ! SO4 (umol L-1)   !SO4  (umol L-1) ! for SULFUR_TRACE test
            SEDECO(ng)%S0  (iSt,k,i,j) = 0.0d0     !S0  (nmol g-1)
#  if defined SULFUR_ISOTOPE
            SEDECO(ng)%FeS (iS34,k,i,j) = 0.0d0     !FeS  (nmol g-1) 
            SEDECO(ng)%FeS2(iS34,k,i,j) = Ci_from_Ct_delta(FeS2_sed, -20.0d0, R34S_VCDT)   !FeS2  (nmol g-1) 
            SEDECO(ng)%H2S (iS34,k,i,j) = 0.0d0     !H2S  (umol L-1)
            SEDECO(ng)%SO4 (iS34,k,i,j) = Ci_from_Ct_delta(cSO4, 21.0d0, R34S_VCDT)  !SO4  (umol L-1)
            SEDECO(ng)%S0  (iS34,k,i,j) = 0.0d0     !S0  (nmol g-1)
#  endif
          END DO
        END DO
      END DO
# endif
    endif

    RETURN

  END SUBROUTINE initialize_sedecosys
 ! ****************************************************************** !     



!!! **********************************************************************
!!!  Main program of sediment ecosystem model
!!! **********************************************************************
! amb:ambient = bottom grid of water column = Just above seafloor
! amb will be set as value(1) in mod_reef_ecosys.F

  SUBROUTINE sedecosys           &
!   input parameters
    ( ng, i, j       &  ! ng: nested grid number; i,j: position
    , date           &  ! Date (days since x or elapsed days)
    , dt             &  ! Time step (sec)
    , Tamb           &  ! Tmp: Temperature (oC)
    , Samb           &  ! Sal: Salinity (PSU)
    , O2amb          &  ! O2: Dissolved oxygen (umol L-1)
    , TAamb          &  ! TA: Total Alkalinity (umol kg-1)
    , DICamb         &  ! DIC: carbon dioxide  (umol kg-1)
    , NO3amb         &  ! NO3: NO3 (umol L-1)
    , NH4amb         &  ! NH4: NH4 (umol L-1)
    , PO4amb         &  ! PO4: PO4 (umol L-1)
    , DOCamb         &  ! DOC: Dissolved organic carbon (umol L-1)
    , POCamb         &  ! POC: Particulate organic carbon (nmol g-1)
    , DONamb         &  ! DOC: Dissolved organic nitrogen (umol L-1)
    , PONamb         &  ! PON: Particulate organic nitrogen (PON: umol L-1)
    , DOPamb         &  ! DOC: Dissolved organic phosporius (umol L-1)
    , POPamb         &  ! POP: Particulate organic phosporius (POP: umol L-1)
    , Mn2amb         &  ! Mn2 (umol L-1)
    , Fe2amb         &  ! Fe2 (umol L-1)
    , H2Samb         &  ! H2S (umol L-1)
    , SO4amb         &  ! SO4 (umol L-1)
# if defined SEDECO_BURIAL
    , Fdep_sed       &  ! Sedimentation rate (g cm-2 s-1) (Positive: sedimentation; Negative: erosion)
# endif
    , Fdep_POC       &  ! POC deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Fdep_PON       &  ! PON deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Fdep_POP       &  ! POP deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Fdep_PIC       &  ! PIC deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Fdep_MnO2      &  ! MnO2 deposition flux (nmol cm-2 s-1)
    , Fdep_FeOOH     &  ! FeOOH depositoin flux (nmol cm-2 s-1)
    , Fdep_FeOOH_PO4 &  ! FeOOH_PO4 depositoin flux (nmol cm-2 s-1)
    , Fdep_FeS       &  ! FeS deposition flux (nmol cm-2 s-1)
    , Fdep_FeS2      &  ! FeS2 depositoin flux (nmol cm-2 s-1)
    , Fdep_S0        &  ! S0 depositoin flux (nmol cm-2 s-1)
# if defined SEDECO_SGD
    , sgd_flux       &  ! sumbarine groundwater discharge rate (cm s-1)  This flux is assumed to be the same throughout the sediment ecosystem layers (no compression/expansion) So volume flux doesn't change but concentrations of tracers do
    , sgd_Tmp        &  ! SGD concentration coming in through bottom: Tmp: Temperature (oC)
    , sgd_Sal        &  ! SGD concentration coming in through bottom: Sal: Salinity (PSU)
    , sgd_DOx        &  ! SGD concentration coming in through bottom: DOx: Dissolved oxygen (umol L-1)
    , sgd_TA         &  ! SGD concentration coming in through bottom: TA : Total alkalinity (umol L-1)
    , sgd_DIC        &  ! SGD concentration coming in through bottom: DIC: Total dissolved inorganic carbon (umol L-1)
    , sgd_NO3        &  ! SGD concentration coming in through bottom: NO3: NO3 (umol L-1)
    , sgd_NH4        &  ! SGD concentration coming in through bottom: NH4: NH4 (umol L-1)
    , sgd_PO4        &  ! SGD concentration coming in through bottom: PO4: PO4 (umol L-1)
    , sgd_DOC        &  ! SGD concentration coming in through bottom: DOC: Total dissolved organic carbon (DOC: umol L-1)
    , sgd_DON        &  ! SGD concentration coming in through bottom: DOC: Total dissolved organic nitrogen (DON: umol L-1)
    , sgd_DOP        &  ! SGD concentration coming in through bottom: DOC: Total dissolved organic phosphrous (DOP: umol L-1)
# endif
!   output parameters
    , Flux_Tmp       &  ! Temperature flux (oC cm s-1)  * direction of water column to sediment is positive
    , Flux_Sal       &  ! Salinity flux (PSU cm s-1)  * direction of water column to sediment is positive
    , Flux_DO        &  ! DO  flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_TA        &  ! Total Alkalinity flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_DIC       &  ! DIC flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_NO3       &  ! NO3 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_NH4       &  ! NH4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_PO4       &  ! PO4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_DOC       &  ! DOC(f,s) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_POC       &  ! POC(f,s) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_DON       &  ! DON(f,s) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_PON       &  ! PON(f,s) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_DOP       &  ! DOP(f,s) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_POP       &  ! POP(f,s) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_PIC       &  ! CaCO3 fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
# if defined BLUE_TIDE         
!    , Flux_Mn2       &  ! Mn2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
!    , Flux_MnO2      &  ! MnO2 flux  (nmol cm-2 s-1) * direction of water column to sediment is positive
!    , Flux_Fe2       &  ! Fe2 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
!    , Flux_FeS       &  ! FeS flux    (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
!    , Flux_FeS2      &  ! FeS2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
!    , Flux_FeOOH     &  ! FeOOH flux  (nmol cm-2 s-1) * direction of water column to sediment is positive
!    , Flux_FeOOH_PO4 &  ! FeOOH=PO43 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
    , Flux_H2S       &  ! H2S flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
!    , Flux_S0        &  ! S0 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
!    , Flux_SO4       &  ! SO4 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
# endif
    )

    USE mod_reef_ecosys_param
    USE mod_geochem
    USE mod_decomposition
    
    implicit none

! ***********************************************************************************
!!! Declarative statement
!! These variable names are different from those in mod_reef_ecosys.F

    !! General
    integer, intent(in) :: ng, i, j
    real(8), intent(in) :: date
    real(8), intent(in) :: dt
    real(8), intent(in) :: Tamb     
    real(8), intent(in) :: O2amb
    real(8), intent(in) :: Samb     
    real(8), intent(in) :: TAamb     
    real(8), intent(in) :: DICamb(N_Csp)
    real(8), intent(in) :: NO3amb(N_Nsp)   
    real(8), intent(in) :: NH4amb(N_Nsp)   
    real(8), intent(in) :: PO4amb(N_Psp)   
    real(8), intent(in) :: DOCamb(N_Csp,Ndom)
    real(8), intent(in) :: POCamb(N_Csp,Npom)
    real(8), intent(in) :: DONamb(N_Nsp,Ndom)
    real(8), intent(in) :: PONamb(N_Nsp,Npom)
    real(8), intent(in) :: DOPamb(N_Psp,Ndom)
    real(8), intent(in) :: POPamb(N_Psp,Npom)
    !! Sulfate
    real(8), intent(in) :: Mn2amb
    real(8), intent(in) :: Fe2amb
    real(8), intent(in) :: H2Samb(N_Ssp)
    real(8), intent(in) :: SO4amb(N_Ssp) 
    !! Deposition Flux (test) Kasih 2009 -------- 
# if defined SEDECO_BURIAL
    real(8), intent(in) :: Fdep_sed  ! Sedimentation rate (g cm-2 s-1) (Positive: sedimentation; Negative: erosion)
# endif
    real(8), intent(in) :: Fdep_POC(N_Csp,Npom)   
    real(8), intent(in) :: Fdep_PON(N_Nsp,Npom)   
    real(8), intent(in) :: Fdep_POP(N_Psp,Npom)   
    real(8), intent(in) :: Fdep_PIC(N_Csp)   
    real(8), intent(in) :: Fdep_MnO2
    real(8), intent(in) :: Fdep_FeOOH
    real(8), intent(in) :: Fdep_FeOOH_PO4(N_Psp)   ! FeOOH_PO4 depositoin flux (nmol cm-2 s-1)
    real(8), intent(in) :: Fdep_FeS(N_Ssp)         ! FeS deposition flux (nmol cm-2 s-1)
    real(8), intent(in) :: Fdep_FeS2(N_Ssp)        ! FeS2 depositoin flux (nmol cm-2 s-1)
    real(8), intent(in) :: Fdep_S0(N_Ssp)          ! S0 depositoin flux (nmol cm-2 s-1)
 
# if defined SEDECO_SGD
    real(8), intent(in) :: sgd_flux     ! Submarine Groundwater Discharge (SDG) volume flux (cm s-1)
    real(8), intent(in) :: sgd_Tmp   ! SGD concentration coming in through bottom: Tmp: Temperature (oC)
    real(8), intent(in) :: sgd_Sal   ! SGD concentration coming in through bottom: Sal: Salinity (PSU)
    real(8), intent(in) :: sgd_DOx   ! SGD concentration coming in through bottom: DOx: Dissolved oxygen (umol L-1)
    real(8), intent(in) :: sgd_TA    ! SGD concentration coming in through bottom: TA : Total alkalinity (umol L-1)
    real(8), intent(in) :: sgd_DIC(N_Csp)   ! SGD concentration coming in through bottom: DIC: Total dissolved inorganic carbon (umol L-1)
    real(8), intent(in) :: sgd_NO3(N_Nsp)   ! SGD concentration coming in through bottom: NO3: NO3 (umol L-1)
    real(8), intent(in) :: sgd_NH4(N_Nsp)   ! SGD concentration coming in through bottom: NH4: NH4 (umol L-1)
    real(8), intent(in) :: sgd_PO4(N_Psp)   ! SGD concentration coming in through bottom: PO4: PO4 (umol L-1)
    real(8), intent(in) :: sgd_DOC(N_Csp,Ndom)  ! SGD concentration coming in through bottom: DOC: Total dissolved organic carbon (DOC: umol L-1)
    real(8), intent(in) :: sgd_DON(N_Nsp,Ndom)  ! SGD concentration coming in through bottom: DOC: Total dissolved organic nitrogen (DON: umol L-1)
    real(8), intent(in) :: sgd_DOP(N_Psp,Ndom)  ! SGD concentration coming in through bottom: DOC: Total dissolved organic phosphrous (DOP: umol L-1)
# endif

!! output parameters (Flux)
    real(8), intent(out) :: Flux_Tmp
    real(8), intent(out) :: Flux_Sal
    real(8), intent(out) :: Flux_DO
    real(8), intent(out) :: Flux_TA
    real(8), intent(out) :: Flux_DIC(N_Csp)
    real(8), intent(out) :: Flux_NO3(N_Nsp)
    real(8), intent(out) :: Flux_NH4(N_Nsp)
    real(8), intent(out) :: Flux_PO4(N_Psp)
    real(8), intent(out) :: Flux_DOC(N_Csp,Ndom)
    real(8), intent(out) :: Flux_POC(N_Csp,Npom)
    real(8), intent(out) :: Flux_DON(N_Nsp,Ndom)
    real(8), intent(out) :: Flux_PON(N_Nsp,Npom)
    real(8), intent(out) :: Flux_DOP(N_Psp,Ndom)
    real(8), intent(out) :: Flux_POP(N_Psp,Npom)
    real(8), intent(out) :: Flux_PIC(N_Csp)
# if defined BLUE_TIDE         
!    real(8), intent(out) :: Flux_Mn2
!    real(8), intent(out) :: Flux_MnO2
!    real(8), intent(out) :: Flux_Fe2
!    real(8), intent(out) :: Flux_FeS (N_Ssp)
!    real(8), intent(out) :: Flux_FeS2(N_Ssp)
!    real(8), intent(out) :: Flux_FeOOH
!    real(8), intent(out) :: Flux_FeOOH_PO4(N_Psp)
    real(8), intent(out) :: Flux_H2S(N_Ssp)
!    real(8), intent(out) :: Flux_S0(N_Ssp)
!    real(8), intent(out) :: Flux_SO4(N_Ssp)
# endif

!!! Set parameters 
    integer :: k, m, isp, isp2    ! symbol for loop cal
  
!! -- Diffusion coefficient -----------------------------------------
    real(8) :: D_Tmp(Nsed)
    real(8) :: D_Sal(Nsed)
    real(8) :: D_O2 (Nsed)
    real(8) :: D_TA (Nsed)
    real(8) :: D_DIC(N_Csp,Nsed)
    real(8) :: D_NO3(N_Nsp,Nsed)
    real(8) :: D_NH4(N_Nsp,Nsed)
    real(8) :: D_PO4(N_Psp,Nsed)
    real(8) :: D_DOM(Ndom,Nsed)
    real(8) :: D_Mn2(Nsed)
    real(8) :: D_MnO2(Nsed)
    real(8) :: D_Fe2 (Nsed)
    real(8) :: D_FeS (N_Ssp,Nsed)
    real(8) :: D_FeS2(N_Ssp,Nsed)
    real(8) :: D_FeOOH(Nsed)
    real(8) :: D_FeOOH_PO4(N_Psp,Nsed)
    real(8) :: D_H2S(N_Ssp,Nsed)
    real(8) :: D_S0 (N_Ssp,Nsed)
    real(8) :: D_SO4(N_Ssp,Nsed)
    real(8) :: D_PIC(N_Csp,Nsed)
    !! Biodifuusivity　(cm2 s-1)
    real(8) :: DBd(Nsed)    !(cm2 s-1)
    real(8) :: DBp(Nsed)    !(cm2 s-1)

    !! Adsorption constant K (cm3 g-1)
    real(8),parameter :: Kad_NO3 = 5.4d0   !(cm3 g-1)
    real(8),parameter :: Kad_NH4 = 2.2d0   !(cm3 g-1)
    real(8),parameter :: Kad_PO4 = 2.0d0   !(cm3 g-1)
    real(8),parameter :: Kad_Mn2 = 13.0d0  !(cm3 g-1)
    real(8),parameter :: Kad_Fe2 = 500.0d0 !(cm3 g-1)

    !! Sedimentation rate (cm/sec)
# if defined SEDECO_BURIAL
    ! real(8),parameter :: u = 0.064d0 / (365d0*24d0*60d0*60d0) ! (cm/sec) ! Fossing 2004
    ! real(8),parameter :: u = 0.38d0 / (365d0*24d0*60d0*60d0) ! (cm/sec) ! Irie 2018
    ! real(8),parameter :: u = 0.5d0 / (365d0*24d0*60d0*60d0) ! (cm/sec) ! Mochida 2023
# endif

    real(8) :: Fw_Tmp(Nsed)
    real(8) :: Fw_Sal(Nsed)
    real(8) :: Fw_O2 (Nsed)
    real(8) :: Fw_TA (Nsed)
    real(8) :: Fw_DIC(N_Csp,Nsed)
    real(8) :: Fw_NO3(N_Nsp,Nsed)
    real(8) :: Fw_NH4(N_Nsp,Nsed)
    real(8) :: Fw_PO4(N_Psp,Nsed)
    real(8) :: Fw_DOC(N_Csp,Ndom,Nsed)
    real(8) :: Fw_POC(N_Csp,Npom,Nsed)
    real(8) :: Fw_DON(N_Nsp,Ndom,Nsed)
    real(8) :: Fw_PON(N_Nsp,Npom,Nsed)
    real(8) :: Fw_DOP(N_Psp,Ndom,Nsed)
    real(8) :: Fw_POP(N_Psp,Npom,Nsed)
    real(8) :: Fw_PIC(N_Csp,Nsed)
    real(8) :: Fw_Mn2(Nsed)
    real(8) :: Fw_MnO2(Nsed)
    real(8) :: Fw_Fe2 (Nsed)
    real(8) :: Fw_FeS (N_Ssp,Nsed)
    real(8) :: Fw_FeS2(N_Ssp,Nsed)
    real(8) :: Fw_FeOOH(Nsed)
    real(8) :: Fw_FeOOH_PO4(N_Psp,Nsed)
    real(8) :: Fw_H2S(N_Ssp,Nsed)
    real(8) :: Fw_S0 (N_Ssp,Nsed)
    real(8) :: Fw_SO4(N_Ssp,Nsed)

    !! Advection velocity
    real(8) :: Wd(Nsed)    ! Solutes advection velocity (cm s-1)
    real(8) :: Wp(Nsed)    ! Solids advection velocity (cm s-1)

!! Net Production and Consumption  -------------------------------------------
    !! unit: (umol L-1 s-1 = nmol cm-3 s-1)
    !! unit: (nmol g-1 s-1)
    real(8) :: R_Tmp(Nsed)
    real(8) :: R_Sal(Nsed)
    real(8) :: R_O2 (Nsed)
    real(8) :: R_TA (Nsed)
    real(8) :: R_DIC(N_Csp,Nsed)
    real(8) :: R_NO3(N_Nsp,Nsed)
    real(8) :: R_NH4(N_Nsp,Nsed)
    real(8) :: R_PO4(N_Psp,Nsed)
    real(8) :: R_DOC(N_Csp,Ndom,Nsed)
    real(8) :: R_POC(N_Csp,Npom,Nsed)
    real(8) :: R_DON(N_Nsp,Ndom,Nsed)
    real(8) :: R_PON(N_Nsp,Npom,Nsed)
    real(8) :: R_DOP(N_Psp,Ndom,Nsed)
    real(8) :: R_POP(N_Psp,Npom,Nsed)
    real(8) :: R_PIC(N_Csp,Nsed)
    real(8) :: R_Mn2 (Nsed)
    real(8) :: R_MnO2(Nsed)
    real(8) :: R_Fe2 (Nsed)
    real(8) :: R_FeS (N_Ssp,Nsed)
    real(8) :: R_FeS2(N_Ssp,Nsed)
    real(8) :: R_FeOOH(Nsed)
    real(8) :: R_FeOOH_PO4(N_Psp,Nsed)
    real(8) :: R_H2S(N_Ssp,Nsed)
    real(8) :: R_S0 (N_Ssp,Nsed)
    real(8) :: R_SO4(N_Ssp,Nsed)

    real(8) :: cff, cff2, cff3, cfdd, cfsd, cfss
    real(8) :: pH, Wcal, Warg

!!  Output ------------------------------------------------------------------
# if defined SEDIMENT_TESTMODE
!    real(8), parameter :: SEDECO_OUTPUT_INTERVAL = 60.0d0 !1d0/60d0/20.0d0 !1440.0d0 !*365     ! Output interval (min)
    real(8), save :: dsec = 0.d0 !sec
    real(8) :: pHamb 
    ! real(8) :: redoxlevel(Nsed)    ! for debug / 1:O2, 2:NO3, 3:MnO2, 4:FeOOH, 5:SO4
# endif

! ***********************************************************************************
!!! executable statement


! ********************************************************************* !
!   Set surface boundary values
! ********************************************************************* !
!  Solutes (open boundary)
    SEDECO(ng)%Tmp(0,i,j)  = Tamb           ! Tmp: Temperature (oC)
    SEDECO(ng)%Sal(0,i,j)  = Samb           ! Sal: Salinity (PSU)
    SEDECO(ng)%O2 (0,i,j)  = O2amb          ! O2: Dissolved oxygen (umol L-1)
    SEDECO(ng)%TA (0,i,j)  = TAamb          ! TA: Total Alkalinity (kg L-1)
    SEDECO(ng)%DIC(:,0,i,j)= DICamb(:)    ! DIC: carbon dioxide  (umol L-1)
    SEDECO(ng)%NO3(:,0,i,j) = NO3amb(:)         ! NO3: NO3 (umol L-1)
    SEDECO(ng)%NH4(:,0,i,j) = NH4amb(:)         ! NH4: NH4 (umol L-1)
    SEDECO(ng)%PO4(:,0,i,j) = PO4amb(:)         ! PO4: PO4 (umol L-1)
    SEDECO(ng)%DOC(:,:,0,i,j) = DOCamb(:,:)   ! DOC: Dissolved organic carbon (umol L-1)
    SEDECO(ng)%DON(:,:,0,i,j) = DONamb(:,:)   ! DOC: Dissolved organic nitrogen (umol L-1)
    SEDECO(ng)%DOP(:,:,0,i,j) = DOPamb(:,:)   ! DOC: Dissolved organic phosporius (umol L-1)
    SEDECO(ng)%Mn2(0,i,j) = Mn2amb         ! Mn2 (umol L-1)
    SEDECO(ng)%Fe2 (0,i,j) = Fe2amb        ! Fe2 (umol L-1)
    SEDECO(ng)%H2S(:,0,i,j) = H2Samb(:)     ! H2S (umol L-1)
    SEDECO(ng)%SO4(:,0,i,j) = SO4amb(:)     ! SO4 (umol L-1)

!  Solids (closed boundary)
    SEDECO(ng)%POC(:,:,0,i,j) = SEDECO(ng)%POC(:,:,1,i,j)   ! POC: Particulate organic carbon (nmol g-1)
    SEDECO(ng)%PON(:,:,0,i,j) = SEDECO(ng)%PON(:,:,1,i,j)   ! PON: Particulate organic nitrogen (PON: umol L-1)
    SEDECO(ng)%POP(:,:,0,i,j) = SEDECO(ng)%POP(:,:,1,i,j)   ! POP: Particulate organic phosporius (POP: umol L-1)
    SEDECO(ng)%PIC(:,0,i,j)  = SEDECO(ng)%PIC(:,1,i,j)      ! PIC: Particulate inorganic carbon (nmol g-1)
    SEDECO(ng)%MnO2(0,i,j)   = SEDECO(ng)%MnO2(1,i,j)  
    SEDECO(ng)%FeS2(:,0,i,j) = SEDECO(ng)%FeS2(:,1,i,j)
    SEDECO(ng)%FeOOH(0,i,j)  = SEDECO(ng)%FeOOH(1,i,j) 
    SEDECO(ng)%FeOOH_PO4(:,0,i,j)  = SEDECO(ng)%FeOOH_PO4(:,1,i,j) 
    SEDECO(ng)%S0 (:,0,i,j)  = SEDECO(ng)%S0 (:,1,i,j) 
    
! ********************************************************************* !
!   Set bottom boundary values
! ********************************************************************* !
! Closed boundary condition
    SEDECO(ng)%Tmp(Nsed,i,j)  = SEDECO(ng)%Tmp(Nsed-1,i,j) ! Tmp: Temperature (oC)
    SEDECO(ng)%Sal(Nsed,i,j)  = SEDECO(ng)%Sal(Nsed-1,i,j) ! Sal: Salinity (PSU)
    SEDECO(ng)%O2 (Nsed,i,j)  = SEDECO(ng)%O2 (Nsed-1,i,j) ! O2: Dissolved oxygen (umol L-1)
    SEDECO(ng)%TA (Nsed,i,j)  = SEDECO(ng)%TA (Nsed-1,i,j) ! TA: Total Alkalinity (kg L-1)
    SEDECO(ng)%DIC(:,Nsed,i,j)= SEDECO(ng)%DIC(:,Nsed-1,i,j)    ! DIC: carbon dioxide  (umol L-1)
    SEDECO(ng)%NO3(:,Nsed,i,j) = SEDECO(ng)%NO3(:,Nsed-1,i,j)    ! NO3: NO3 (umol L-1)
    SEDECO(ng)%NH4(:,Nsed,i,j) = SEDECO(ng)%NH4(:,Nsed-1,i,j)    ! NH4: NH4 (umol L-1)
    SEDECO(ng)%PO4(:,Nsed,i,j) = SEDECO(ng)%PO4(:,Nsed-1,i,j)    ! PO4: PO4 (umol L-1)
    SEDECO(ng)%DOC(:,:,Nsed,i,j) = SEDECO(ng)%DOC(:,:,Nsed-1,i,j)   ! DOC: Dissolved organic carbon (umol L-1)
    SEDECO(ng)%POC(:,:,Nsed,i,j) = SEDECO(ng)%POC(:,:,Nsed-1,i,j)   ! POC: Particulate organic carbon (nmol g-1)
    SEDECO(ng)%DON(:,:,Nsed,i,j) = SEDECO(ng)%DON(:,:,Nsed-1,i,j)  ! DOC: Dissolved organic nitrogen (umol L-1)
    SEDECO(ng)%DOP(:,:,Nsed,i,j) = SEDECO(ng)%DOP(:,:,Nsed-1,i,j)  ! DOC: Dissolved organic phosporius (umol L-1)
    SEDECO(ng)%PON(:,:,Nsed,i,j) = SEDECO(ng)%PON(:,:,Nsed-1,i,j)  ! PON: Particulate organic nitrogen (PON: umol L-1)
    SEDECO(ng)%POP(:,:,Nsed,i,j) = SEDECO(ng)%POP(:,:,Nsed-1,i,j)  ! POP: Particulate organic phosporius (POP: umol L-1)
    SEDECO(ng)%PIC(:,Nsed,i,j)= SEDECO(ng)%PIC(:,Nsed-1,i,j)    ! DIC: carbon dioxide  (umol L-1)
    SEDECO(ng)%Mn2 (Nsed,i,j) = SEDECO(ng)%Mn2 (Nsed-1,i,j)    ! Mn2 (umol L-1)
    SEDECO(ng)%MnO2(Nsed,i,j) = SEDECO(ng)%MnO2(Nsed-1,i,j)
    SEDECO(ng)%Fe2 (Nsed,i,j) = SEDECO(ng)%Fe2 (Nsed-1,i,j)  ! Fe2 (umol L-1)
    SEDECO(ng)%FeS2(:,Nsed,i,j) = SEDECO(ng)%FeS2(:,Nsed-1,i,j)
    SEDECO(ng)%FeOOH(Nsed,i,j) = SEDECO(ng)%FeOOH(Nsed-1,i,j)
    SEDECO(ng)%FeOOH_PO4(:,Nsed,i,j) = SEDECO(ng)%FeOOH_PO4(:,Nsed-1,i,j)
    SEDECO(ng)%H2S(:,Nsed,i,j) = SEDECO(ng)%H2S(:,Nsed-1,i,j)  ! H2S (umol L-1)
    SEDECO(ng)%SO4(:,Nsed,i,j) = SEDECO(ng)%SO4(:,Nsed-1,i,j)  ! SO4 (umol L-1)
    SEDECO(ng)%S0 (:,Nsed,i,j) = SEDECO(ng)%S0 (:,Nsed-1,i,j)

! ********************************************************************* !
!          Diffusion
! ********************************************************************* !
!! ----- Diffusion coefficient in water column (cm-2 s-1) ----- !!
    DO k = 1,Nsed
      D_Tmp(k) = (11.7d0 + 0.344d0*SEDECO(ng)%Tmp(k,i,j) + 0.00505d0*SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6)
      D_Sal(k) = (11.7d0 + 0.344d0*SEDECO(ng)%Tmp(k,i,j) + 0.00505d0*SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6)
      D_O2 (k) = (11.7d0 + 0.344d0*SEDECO(ng)%Tmp(k,i,j) + 0.00505d0*SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6)
      D_DIC(:,k) = (9.39d0 + 0.267d0*SEDECO(ng)%Tmp(k,i,j) + 0.0041d0 *SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6)
# if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
      D_DIC(iC13,k) = (9.39d0 + 0.267d0*SEDECO(ng)%Tmp(k,i,j) + 0.0041d0 *SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6)  
# endif
# if defined CLUMPED_ISOTOPE
      D_DIC(iD47,k) = (9.39d0 + 0.267d0*SEDECO(ng)%Tmp(k,i,j) + 0.0041d0 *SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6)
# endif
      D_NO3(:,k)  = (9.72d0 + 0.365d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
      D_NH4(:,k)  = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
# if defined NITROGEN_ISOTOPE
      D_NO3(iN15,k)  = (9.72d0 + 0.365d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
      D_NH4(iN15,k)  = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
# endif
      D_PO4(:,k)  = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
      D_DOM(iLDOM,k) = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(k,i,j)) *(1.0d-7) ! DOMf: DOCf DONf DOPf ??????????????????
      D_DOM(iRDOM,k) = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(k,i,j)) *(1.0d-9) ! DOMs: DOCs DONs DOPs ??????????????????

      D_Mn2(k)  = (3.04d0 + 0.153d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
      D_Fe2(k)  = (3.36d0 + 0.148d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
      D_SO4(:,k)  = (4.96d0 + 0.226d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
      D_H2S(:,k)  = (8.74d0 + 0.264d0*SEDECO(ng)%Tmp(k,i,j) + 0.004d0*SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6)
# if defined SULFUR_ISOTOPE
      D_SO4(iS34,k)  = (4.96d0 + 0.226d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
      D_H2S(iS34,k)  = (8.74d0 + 0.264d0*SEDECO(ng)%Tmp(k,i,j) + 0.004d0*SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6)
# endif
    ENDDO

!! --- Bio-diffusion coefficient in sediment column (cm-2 s-1) (For solutes and solids) ------------
    DBd(1)=0.0d0
    DBp(1)=0.0d0

    DO k = 2,Nsed
      !! determine DBd and DBp depending on z
      if (SEDECO(ng)%zw(k) <= 11.8d0 )then
!        DBd(k) = 3.51d-6
        DBd(k) = 3.51d-5
      else
!        DBd(k) = 3.51d-6 * exp(-0.378d0*(SEDECO(ng)%zw(k)-11.8d0))
        DBd(k) = 3.51d-5 * exp(-0.378d0*(SEDECO(ng)%zw(k)-11.8d0))
      endif
      DBp(k) = DBd(k)/9.3d0 * SEDECO(ng)%rhos_d(k,i,j)*(1d0-SEDECO(ng)%poro(k,i,j))
      DBd(k) = DBd(k) * SEDECO(ng)%poro(k,i,j)
    ENDDO

!! --- Total diffusion coefficient in sediment column (cm-2 s-1) --- !!
    DO k = 2,Nsed
      cff = SEDECO(ng)%poro(k,i,j)/(1.0d0+3.0d0*(1.0d0-SEDECO(ng)%poro(k,i,j)))

      D_O2(k) = cff*D_O2(k) + DBd(k)
      D_DIC(:,k) = cff*D_DIC(:,k) + DBd(k)
      D_NO3(:,k)  = cff*D_NO3(:,k) + DBd(k)
      D_NH4(:,k)  = cff*D_NH4(:,k) + DBd(k)
      D_PO4(:,k)  = cff*D_PO4(:,k) + DBd(k)
      DO m = 1,Ndom
        D_DOM(m,k) = cff*D_DOM(m,k) + DBd(k) ! DOMf: DOCf DONf DOPf
      ENDDO
      D_Mn2(k)  = cff*D_Mn2(k) + DBd(k)
      D_Fe2(k)  = cff*D_Fe2(k) + DBd(k)
      D_SO4(:,k)  = cff*D_SO4(:,k) + DBd(k)
      D_H2S(:,k)  = cff*D_H2S(:,k) + DBd(k)
    ENDDO

!! ----- Diffusive boundary layer tickness ----- !!
    SEDECO(ng)%dzw(1) = 0.01d0 ! (cm) ???????????? Need update for adding dependency on flow rate ??????????????????????????

!! === Diffusion Flux calculation ======================================
! --- Solutes ----
    DIFF_LOOP: DO k = 1,Nsed
    
      Fw_Tmp(k) = Ficks(D_Tmp(k),SEDECO(ng)%Tmp(k-1,i,j),SEDECO(ng)%Tmp(k,i,j),SEDECO(ng)%dzw(k))
      Fw_Sal(k) = Ficks(D_Sal(k),SEDECO(ng)%Sal(k-1,i,j),SEDECO(ng)%Sal(k,i,j),SEDECO(ng)%dzw(k))
      Fw_O2 (k) = Ficks(D_O2 (k),SEDECO(ng)%O2 (k-1,i,j),SEDECO(ng)%O2 (k,i,j),SEDECO(ng)%dzw(k))
      Fw_TA (k) = Ficks(D_O2 (k),SEDECO(ng)%TA (k-1,i,j),SEDECO(ng)%TA (k,i,j),SEDECO(ng)%dzw(k))
      DO isp = 1,N_Csp
        Fw_DIC(isp,k) = Ficks(D_DIC(isp,k),SEDECO(ng)%DIC(isp,k-1,i,j),SEDECO(ng)%DIC(isp,k,i,j),SEDECO(ng)%dzw(k))
      ENDDO
      DO isp = 1,N_Nsp
        Fw_NO3(isp,k) = Ficks(D_NO3(isp,k),SEDECO(ng)%NO3(isp,k-1,i,j),SEDECO(ng)%NO3(isp,k,i,j),SEDECO(ng)%dzw(k))
        Fw_NH4(isp,k) = Ficks(D_NH4(isp,k),SEDECO(ng)%NH4(isp,k-1,i,j),SEDECO(ng)%NH4(isp,k,i,j),SEDECO(ng)%dzw(k))
      ENDDO
      DO isp = 1,N_Psp
        Fw_PO4(isp,k) = Ficks(D_PO4(isp,k),SEDECO(ng)%PO4(isp,k-1,i,j),SEDECO(ng)%PO4(isp,k,i,j),SEDECO(ng)%dzw(k))
      ENDDO
      DO m = 1,Ndom
        DO isp = 1,N_Csp
          Fw_DOC(isp,m,k) = Ficks(D_DOM(m,k),SEDECO(ng)%DOC(isp,m,k-1,i,j),SEDECO(ng)%DOC(isp,m,k,i,j),SEDECO(ng)%dzw(k))
        ENDDO
        DO isp = 1,N_Nsp
          Fw_DON(isp,m,k) = Ficks(D_DOM(m,k),SEDECO(ng)%DON(isp,m,k-1,i,j),SEDECO(ng)%DON(isp,m,k,i,j),SEDECO(ng)%dzw(k)) ! DONf
        ENDDO
        DO isp = 1,N_Psp
          Fw_DOP(isp,m,k) = Ficks(D_DOM(m,k),SEDECO(ng)%DOP(isp,m,k-1,i,j),SEDECO(ng)%DOP(isp,m,k,i,j),SEDECO(ng)%dzw(k)) ! DONf
        ENDDO
      ENDDO
      Fw_Mn2(k)   = Ficks(D_Mn2(k),SEDECO(ng)%Mn2(k-1,i,j),SEDECO(ng)%Mn2(k,i,j),SEDECO(ng)%dzw(k))
      Fw_Fe2(k)   = Ficks(D_Fe2(k),SEDECO(ng)%Fe2(k-1,i,j),SEDECO(ng)%Fe2(k,i,j),SEDECO(ng)%dzw(k))
      DO isp=1,N_Ssp
        Fw_H2S(isp,k) = Ficks(D_H2S(isp,k),SEDECO(ng)%H2S(isp,k-1,i,j),SEDECO(ng)%H2S(isp,k,i,j),SEDECO(ng)%dzw(k))
        Fw_SO4(isp,k) = Ficks(D_SO4(isp,k),SEDECO(ng)%SO4(isp,k-1,i,j),SEDECO(ng)%SO4(isp,k,i,j),SEDECO(ng)%dzw(k))
      ENDDO

! --- Solids -----
      DO m = 1,Npom
        DO isp = 1,N_Csp
          Fw_POC(isp,m,k) = Ficks(DBp(k),SEDECO(ng)%POC(isp,m,k-1,i,j),SEDECO(ng)%POC(isp,m,k,i,j),SEDECO(ng)%dzw(k))
        ENDDO
        DO isp = 1,N_Nsp
          Fw_PON(isp,m,k) = Ficks(DBp(k),SEDECO(ng)%PON(isp,m,k-1,i,j),SEDECO(ng)%PON(isp,m,k,i,j),SEDECO(ng)%dzw(k))
        ENDDO
        DO isp = 1,N_Psp
          Fw_POP(isp,m,k) = Ficks(DBp(k),SEDECO(ng)%POP(isp,m,k-1,i,j),SEDECO(ng)%POP(isp,m,k,i,j),SEDECO(ng)%dzw(k))
        ENDDO
      ENDDO
      DO isp = 1,N_Csp
        Fw_PIC(isp,k) = Ficks(DBp(k),SEDECO(ng)%PIC(isp,k-1,i,j),SEDECO(ng)%PIC(isp,k,i,j),SEDECO(ng)%dzw(k))
      ENDDO

      Fw_MnO2(k)  = Ficks(DBp(k),SEDECO(ng)%MnO2(k-1,i,j),SEDECO(ng)%MnO2(k,i,j),SEDECO(ng)%dzw(k))
      Fw_FeOOH(k) = Ficks(DBp(k),SEDECO(ng)%FeOOH(k-1,i,j),SEDECO(ng)%FeOOH(k,i,j),SEDECO(ng)%dzw(k))
      DO isp = 1,N_Psp
        Fw_FeOOH_PO4(isp,k) &
          = Ficks(DBp(k),SEDECO(ng)%FeOOH_PO4(isp,k-1,i,j),SEDECO(ng)%FeOOH_PO4(isp,k,i,j),SEDECO(ng)%dzw(k))
      ENDDO
      DO isp=1,N_Ssp
        Fw_FeS(isp,k)  = Ficks(DBp(k),SEDECO(ng)%FeS (isp,k-1,i,j),SEDECO(ng)%FeS (isp,k,i,j),SEDECO(ng)%dzw(k))
        Fw_FeS2(isp,k) = Ficks(DBp(k),SEDECO(ng)%FeS2(isp,k-1,i,j),SEDECO(ng)%FeS2(isp,k,i,j),SEDECO(ng)%dzw(k))
        Fw_S0(isp,k)   = Ficks(DBp(k),SEDECO(ng)%S0  (isp,k-1,i,j),SEDECO(ng)%S0  (isp,k,i,j),SEDECO(ng)%dzw(k))
      ENDDO

    ENDDO DIFF_LOOP


! ********************************************************************* !
!     BURIAL Advection term
! ********************************************************************* !
# if defined SEDECO_BURIAL || defined SEDECO_SGD
!! --- Advection velocity in sediment column (cm s-1) -----
    Wd(:)=0.0d0
    Wp(:)=0.0d0
#  if defined SEDECO_BURIAL
    !Sedimentation rate conversion [g/cm2/s -> cm/s ]
    cff = Fdep_sed /SEDECO(ng)%rhos_d(1,i,j)/(1.0d0-SEDECO(ng)%poro(1,i,j)) 

    DO k = 1,Nsed
      Wd(k) = Wd(k) + cff * SEDECO(ng)%poro(k,i,j)
      Wp(k) = Wd(k) + Fdep_sed
    ENDDO

!! === Advection Flux calculation ======================================

! --- Solutes, Flux in sediment column ----
    ADV_D_LOOP: DO k = 1,Nsed
      Fw_Tmp(k) = Fw_Tmp(k) + AdvUp(Wd(k),SEDECO(ng)%Tmp(k-1,i,j),SEDECO(ng)%Tmp(k,i,j))
      Fw_Sal(k) = Fw_Sal(k) + AdvUp(Wd(k),SEDECO(ng)%Sal(k-1,i,j),SEDECO(ng)%Sal(k,i,j))
      Fw_O2 (k) = Fw_O2 (k) + AdvUp(Wd(k),SEDECO(ng)%O2 (k-1,i,j),SEDECO(ng)%O2 (k,i,j))
      Fw_TA (k) = Fw_TA (k) + AdvUp(Wd(k),SEDECO(ng)%TA (k-1,i,j),SEDECO(ng)%TA (k,i,j))
      DO isp = 1,N_Csp
        Fw_DIC(isp,k) = Fw_DIC(isp,k) + AdvUp(Wd(k),SEDECO(ng)%DIC(isp,k-1,i,j),SEDECO(ng)%DIC(isp,k,i,j))
      ENDDO
      DO isp = 1,N_Nsp
        Fw_NO3(isp,k) = Fw_NO3(isp,k) + AdvUp(Wd(k),SEDECO(ng)%NO3(isp,k-1,i,j),SEDECO(ng)%NO3(isp,k,i,j))
        Fw_NH4(isp,k) = Fw_NH4(isp,k) + AdvUp(Wd(k),SEDECO(ng)%NH4(isp,k-1,i,j),SEDECO(ng)%NH4(isp,k,i,j))
      ENDDO
      DO isp = 1,N_Psp
        Fw_PO4(isp,k) = Fw_PO4(isp,k) + AdvUp(Wd(k),SEDECO(ng)%PO4(isp,k-1,i,j),SEDECO(ng)%PO4(isp,k,i,j))
      ENDDO
      DO m = 1,Ndom
        DO isp = 1,N_Csp
          Fw_DOC(isp,m,k) = Fw_DOC(isp,m,k) + AdvUp(Wd(k),SEDECO(ng)%DOC(isp,m,k-1,i,j),SEDECO(ng)%DOC(isp,m,k,i,j))
        ENDDO
        DO isp = 1,N_Nsp
          Fw_DON(isp,m,k) = Fw_DON(isp,m,k) + AdvUp(Wd(k),SEDECO(ng)%DON(isp,m,k-1,i,j),SEDECO(ng)%DON(isp,m,k,i,j)) ! DONf
        ENDDO
        DO isp = 1,N_Psp
          Fw_DOP(isp,m,k) = Fw_DOP(isp,m,k) + AdvUp(Wd(k),SEDECO(ng)%DOP(isp,m,k-1,i,j),SEDECO(ng)%DOP(isp,m,k,i,j)) ! DONf
        ENDDO
      ENDDO
      Fw_Mn2(k) = Fw_Mn2(k) + AdvUp(Wd(k),SEDECO(ng)%Mn2(k-1,i,j),SEDECO(ng)%Mn2(k,i,j))
      Fw_Fe2(k) = Fw_Fe2(k) + AdvUp(Wd(k),SEDECO(ng)%Fe2(k-1,i,j),SEDECO(ng)%Fe2(k,i,j))
      DO isp=1,N_Ssp
        Fw_H2S(isp,k) = Fw_H2S(isp,k) + AdvUp(Wd(k),SEDECO(ng)%H2S(isp,k-1,i,j),SEDECO(ng)%H2S(isp,k,i,j))
        Fw_SO4(isp,k) = Fw_SO4(isp,k) + AdvUp(Wd(k),SEDECO(ng)%SO4(isp,k-1,i,j),SEDECO(ng)%SO4(isp,k,i,j))
      ENDDO

    ENDDO ADV_D_LOOP

! --- Solids, Flux in sediment column  ----

    ADV_S_LOOP: DO k = 2,Nsed
      DO m = 1,Npom
        DO isp = 1,N_Csp
          Fw_POC(isp,m,k) = Fw_POC(isp,m,k) + AdvUp(Wp(k),SEDECO(ng)%POC(isp,m,k-1,i,j),SEDECO(ng)%POC(isp,m,k,i,j))
        ENDDO
        DO isp = 1,N_Nsp
          Fw_PON(isp,m,k) = Fw_PON(isp,m,k) + AdvUp(Wp(k),SEDECO(ng)%PON(isp,m,k-1,i,j),SEDECO(ng)%PON(isp,m,k,i,j))
        ENDDO
        DO isp = 1,N_Psp
          Fw_POP(isp,m,k) = Fw_POP(isp,m,k) + AdvUp(Wp(k),SEDECO(ng)%POP(isp,m,k-1,i,j),SEDECO(ng)%POP(isp,m,k,i,j))
        ENDDO
      ENDDO
      DO isp = 1,N_Csp
        Fw_PIC(isp,k) = Fw_PIC(isp,k) + AdvUp(Wp(k),SEDECO(ng)%PIC(isp,k-1,i,j),SEDECO(ng)%PIC(isp,k,i,j))
      ENDDO

      Fw_MnO2(k)  = Fw_MnO2(k) + AdvUp(Wp(k),SEDECO(ng)%MnO2(k-1,i,j),SEDECO(ng)%MnO2(k,i,j))
      Fw_FeOOH(k) = Fw_FeOOH(k)+ AdvUp(Wp(k),SEDECO(ng)%FeOOH(k-1,i,j),SEDECO(ng)%FeOOH(k,i,j))
      DO isp = 1,N_Psp
        Fw_FeOOH_PO4(isp,k) = Fw_FeOOH_PO4(isp,k)     &
                  + AdvUp(Wp(k),SEDECO(ng)%FeOOH_PO4(isp,k-1,i,j),SEDECO(ng)%FeOOH_PO4(isp,k,i,j))
      ENDDO
      DO isp=1,N_Ssp
        Fw_FeS(isp,k)  = Fw_FeS(isp,k) + AdvUp(Wp(k),SEDECO(ng)%FeS (isp,k-1,i,j),SEDECO(ng)%FeS (isp,k,i,j))
        Fw_FeS2(isp,k) = Fw_FeS2(isp,k)+ AdvUp(Wp(k),SEDECO(ng)%FeS2(isp,k-1,i,j),SEDECO(ng)%FeS2(isp,k,i,j))
        Fw_S0(isp,k)   = Fw_S0(isp,k)  + AdvUp(Wp(k),SEDECO(ng)%S0  (isp,k-1,i,j),SEDECO(ng)%S0  (isp,k,i,j))
      ENDDO

    ENDDO ADV_S_LOOP
#  endif

! ********************************************************************* !
!     SGD Advection term
! ********************************************************************* !

#  if defined SEDECO_SGD
    DO k = 1,Nsed
      Wd(k)=-sgd_flux ! * SEDECO(ng)%poro(k,i,j)
    ENDDO

! *************************************** !
!   Set bottom boundary values for SGD
! *************************************** !
    SEDECO(ng)%Tmp(Nsed,i,j)  = sgd_Tmp           ! Tmp: Temperature (oC)
    SEDECO(ng)%Sal(Nsed,i,j)  = sgd_Sal           ! Sal: Salinity (PSU)
    SEDECO(ng)%O2 (Nsed,i,j)  = sgd_DOx          ! O2: Dissolved oxygen (umol L-1)
    SEDECO(ng)%TA (Nsed,i,j)  = sgd_TA          ! TA: Total Alkalinity (kg L-1)
    SEDECO(ng)%DIC(:,Nsed,i,j)  = sgd_DIC(:)    ! DIC: carbon dioxide  (umol L-1)
    SEDECO(ng)%NO3(:,Nsed,i,j) = sgd_NO3(:)         ! NO3: NO3 (umol L-1)
    SEDECO(ng)%NH4(:,Nsed,i,j) = sgd_NH4(:)         ! NH4: NH4 (umol L-1)
    SEDECO(ng)%PO4(:,Nsed,i,j) = sgd_PO4(:)         ! PO4: PO4 (umol L-1)
    SEDECO(ng)%DOC(:,:,Nsed,i,j) = sgd_DOC(:,:)   ! DOC: Dissolved organic carbon (umol L-1)
!    SEDECO(ng)%POC(:,:,Nsed,i,j) = 0.0d0          ! POC: Particulate organic carbon (nmol g-1)
    SEDECO(ng)%DON(:,:,Nsed,i,j) = sgd_DON(:,:)   ! DON: Dissolved organic nitrogen (umol L-1)
!    SEDECO(ng)%PON(:,:,Nsed,i,j) = 0.0d0          ! PON: Particulate organic nitrogen (PON: umol L-1)
    SEDECO(ng)%DOP(:,:,Nsed,i,j) = sgd_DOP(:,:)   ! DOP: Dissolved organic phosporius (umol L-1)
!    SEDECO(ng)%POP(:,:,Nsed,i,j) = 0.0d0          ! POP: Particulate organic phosporius (POP: umol L-1)
!    SEDECO(ng)%PIC(:,Nsed,i,j)  = 0.0d0    ! PIC: Particulate CaCO3 (nmol g-1)
    SEDECO(ng)%Mn2(Nsed,i,j) = 0.0d0         ! Mn2 (umol L-1)
!    SEDECO(ng)%MnO2(Nsed,i,j) = 0.0d0
    SEDECO(ng)%Fe2 (Nsed,i,j) = 0.0d0        ! Fe2 (umol L-1)
!    SEDECO(ng)%FeS2(:,Nsed,i,j) = 0.0d0
!    SEDECO(ng)%FeOOH(Nsed,i,j) = 0.0d0
    SEDECO(ng)%H2S(:,Nsed,i,j) = 0.0d0     ! H2S (umol L-1)
    SEDECO(ng)%SO4(:,Nsed,i,j) = 0.02824d0*sgd_Sal/35.0d0 *1.0d6  ! SO4 (umol L-1)
!    SEDECO(ng)%S0 (:,Nsed,i,j) = 0.0d0

!! === Advection Flux calculation ======================================

! --- Solutes, Flux in sediment column ----
    ADV_SGD_LOOP: DO k = 1,Nsed
      Fw_Tmp(k) = Fw_Tmp(k) + AdvUp(Wd(k),SEDECO(ng)%Tmp(k-1,i,j),SEDECO(ng)%Tmp(k,i,j))
      Fw_Sal(k) = Fw_Sal(k) + AdvUp(Wd(k),SEDECO(ng)%Sal(k-1,i,j),SEDECO(ng)%Sal(k,i,j))
      Fw_O2 (k) = Fw_O2 (k) + AdvUp(Wd(k),SEDECO(ng)%O2 (k-1,i,j),SEDECO(ng)%O2 (k,i,j))
      Fw_TA (k) = Fw_TA (k) + AdvUp(Wd(k),SEDECO(ng)%TA (k-1,i,j),SEDECO(ng)%TA (k,i,j))
      DO isp = 1,N_Csp
        Fw_DIC(isp,k) = Fw_DIC(isp,k) + AdvUp(Wd(k),SEDECO(ng)%DIC(isp,k-1,i,j),SEDECO(ng)%DIC(isp,k,i,j))
      ENDDO
      DO isp = 1,N_Nsp
        Fw_NO3(isp,k) = Fw_NO3(isp,k) + AdvUp(Wd(k),SEDECO(ng)%NO3(isp,k-1,i,j),SEDECO(ng)%NO3(isp,k,i,j))
        Fw_NH4(isp,k) = Fw_NH4(isp,k) + AdvUp(Wd(k),SEDECO(ng)%NH4(isp,k-1,i,j),SEDECO(ng)%NH4(isp,k,i,j))
      ENDDO
      DO isp = 1,N_Psp
        Fw_PO4(isp,k) = Fw_PO4(isp,k) + AdvUp(Wd(k),SEDECO(ng)%PO4(isp,k-1,i,j),SEDECO(ng)%PO4(isp,k,i,j))
      ENDDO
      DO m = 1,Ndom
        DO isp = 1,N_Csp
          Fw_DOC(isp,m,k) = Fw_DOC(isp,m,k) + AdvUp(Wd(k),SEDECO(ng)%DOC(isp,m,k-1,i,j),SEDECO(ng)%DOC(isp,m,k,i,j))
        ENDDO
        DO isp = 1,N_Nsp
          Fw_DON(isp,m,k) = Fw_DON(isp,m,k) + AdvUp(Wd(k),SEDECO(ng)%DON(isp,m,k-1,i,j),SEDECO(ng)%DON(isp,m,k,i,j)) ! DONf
        ENDDO
        DO isp = 1,N_Psp
          Fw_DOP(isp,m,k) = Fw_DOP(isp,m,k) + AdvUp(Wd(k),SEDECO(ng)%DOP(isp,m,k-1,i,j),SEDECO(ng)%DOP(isp,m,k,i,j)) ! DONf
        ENDDO
      ENDDO
      Fw_Mn2(k) = Fw_Mn2(k) + AdvUp(Wd(k),SEDECO(ng)%Mn2(k-1,i,j),SEDECO(ng)%Mn2(k,i,j))
      Fw_Fe2(k) = Fw_Fe2(k) + AdvUp(Wd(k),SEDECO(ng)%Fe2(k-1,i,j),SEDECO(ng)%Fe2(k,i,j))
      DO isp=1,N_Ssp
        Fw_H2S(isp,k) = Fw_H2S(isp,k) + AdvUp(Wd(k),SEDECO(ng)%H2S(isp,k-1,i,j),SEDECO(ng)%H2S(isp,k,i,j))
        Fw_SO4(isp,k) = Fw_SO4(isp,k) + AdvUp(Wd(k),SEDECO(ng)%SO4(isp,k-1,i,j),SEDECO(ng)%SO4(isp,k,i,j))
      ENDDO

    ENDDO ADV_SGD_LOOP
#  endif
# endif

! ********************************************************************* !
!    Set settling Flux (solids)
! ********************************************************************* !
    DO m = 1,Npom
      DO isp = 1,N_Csp
        Fw_POC(isp,m,1) = Fdep_POC(isp,m)
      ENDDO
      DO isp = 1,N_Nsp
        Fw_PON(isp,m,1) = Fdep_PON(isp,m)
      ENDDO
      DO isp = 1,N_Psp
        Fw_POP(isp,m,1) = Fdep_POP(isp,m)
      ENDDO
    ENDDO
    DO isp = 1,N_Csp
      Fw_PIC(isp,1) = Fdep_PIC(isp)
    ENDDO

    Fw_MnO2(1)  = Fdep_MnO2
    Fw_FeOOH(1) = Fdep_FeOOH
    DO isp = 1,N_Psp
      Fw_FeOOH_PO4(isp,1) = Fdep_FeOOH_PO4(isp) 
    ENDDO
    DO isp=1,N_Ssp
      Fw_FeS(isp,1)  = Fdep_FeS(isp) 
      Fw_FeS2(isp,1) = Fdep_FeS2(isp)
      Fw_S0(isp,1)   = Fdep_S0(isp)
    ENDDO

! ********************************************************************* !
!    Set resuspention Flux (solids)
! ********************************************************************* !
# if defined SEDECO_BURIAL
    IF(Fdep_sed<0) THEN
      DO m = 1,Npom
        DO isp = 1,N_Csp
          Fw_POC(isp,m,1) = Fw_POC(isp,m,1)
        ENDDO
        DO isp = 1,N_Nsp
          Fw_PON(isp,m,1) = Fw_PON(isp,m,1)
        ENDDO
        DO isp = 1,N_Psp
          Fw_POP(isp,m,1) = Fw_POP(isp,m,1)
        ENDDO
      ENDDO
      DO isp = 1,N_Csp
        Fw_PIC(isp,1) = Fw_PIC(isp,1)
      ENDDO

      Fw_MnO2(1)  = Fw_MnO2(1)
      Fw_FeOOH(1) = Fw_FeOOH(1)
      DO isp = 1,N_Psp
        Fw_FeOOH_PO4(isp,1) = Fw_FeOOH_PO4(isp,1)
      ENDDO
      DO isp=1,N_Ssp
        Fw_FeS(isp,1)  = Fw_FeS(isp,1) 
        Fw_FeS2(isp,1) = Fw_FeS2(isp,1)
        Fw_S0(isp,1)   = Fw_S0(isp,1)  
      ENDDO
    ENDIF

# endif

! ********************************************************************* !
!    Reaction term
! ********************************************************************* !
! Initialize reaction term (R_*) values
    R_Tmp(:) = 0.0d0    
    R_Sal(:) = 0.0d0    
    R_O2 (:) = 0.0d0    
    R_TA (:) = 0.0d0    
    R_DIC(:,:) = 0.0d0    
    R_NO3(:,:) = 0.0d0    
    R_NH4(:,:) = 0.0d0    
    R_PO4(:,:) = 0.0d0    
    R_DOC(:,:,:) = 0.0d0
    R_POC(:,:,:) = 0.0d0
    R_DON(:,:,:) = 0.0d0
    R_PON(:,:,:) = 0.0d0
    R_DOP(:,:,:) = 0.0d0
    R_POP(:,:,:) = 0.0d0
    R_PIC(:,:) = 0.0d0    
    R_Mn2 (:) = 0.0d0    
    R_MnO2(:) = 0.0d0    
    R_Fe2 (:) = 0.0d0    
    R_FeS (:,:) = 0.0d0    
    R_FeS2(:,:) = 0.0d0    
    R_FeOOH(:) = 0.0d0    
    R_FeOOH_PO4(:,:) = 0.0d0    
    R_H2S(:,:) = 0.0d0    
    R_S0 (:,:) = 0.0d0    
    R_SO4(:,:) = 0.0d0    

    DO k = 1,Nsed-1
    
      CALL decomposition                &
        ! input parameters
        ( 0                             &   ! 0: Sediment column, 1: Water column
        , SEDECO(ng)%Tmp(k,i,j)         &   ! Temperature (oC)
        , SEDECO(ng)%Sal(k,i,j)         &   ! Salinity (PSU)
        , SEDECO(ng)%rhos_d(k,i,j)      &   ! Dry soil density (g cm-3)
        , SEDECO(ng)%poro(k,i,j)        &   ! Soil porosity (= Vwater/(Vwater+Vsed) (non-dimensional: 0.0-1.0)
        , SEDECO(ng)%O2 (k,i,j)         &   ! Dissolved oxygen (umol L-1)
        , SEDECO(ng)%TA (k,i,j)         &   ! Total alkalinity (TA: umol kg-1)
        , SEDECO(ng)%DIC(:,k,i,j)       &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
        , SEDECO(ng)%NO3(:,k,i,j)       &   ! NO3 (umol L-1)
        , SEDECO(ng)%NH4(:,k,i,j)       &   ! NH4 (umol L-1)
        , SEDECO(ng)%PO4(:,k,i,j)       &   ! PO4 (umol L-1)
        , SEDECO(ng)%DOC(:,:,k,i,j)     &   ! DOC(Ndom): Dissolved organic carbon (DOC: umol L-1)
        , SEDECO(ng)%POC(:,:,k,i,j)     &   ! POC(Npom): Particulate organic carbon (POC: umol L-1 or nmol g(DW)-1 *solid)
        , SEDECO(ng)%DON(:,:,k,i,j)     &   ! Dissolved organic nitrogen (DON: umol L-1)
        , SEDECO(ng)%PON(:,:,k,i,j)     &   ! Particulate organic nitrogen (PON: umol L-1 or nmol g(DW)-1 *solid)
        , SEDECO(ng)%DOP(:,:,k,i,j)     &   ! Dissolved organic phosporius (DOP: umol L-1)
        , SEDECO(ng)%POP(:,:,k,i,j)     &   ! Particulate organic phosporius (POP: umol L-1 or nmol g(DW)-1 *solid)
        , SEDECO(ng)%PIC(:,k,i,j)       &   ! CaCO3 (umolC L-1 or nmol g(DW)-1 *solid)
        , SEDECO(ng)%Mn2(k,i,j)         &   ! Mn2+ (umol L-1) 
        , SEDECO(ng)%MnO2(k,i,j)        &   ! MnO2 (nmol g-1 s-1) *solid
        , SEDECO(ng)%Fe2 (k,i,j)        &   ! Fe2+ (umol L-1) 
        , SEDECO(ng)%FeS (:,k,i,j)      &   ! FeS (nmol g(DW)-1 s-1)) *solid
        , SEDECO(ng)%FeS2(:,k,i,j)      &   ! FeS2 (nmol g(DW)-1 s-1)) *solid
        , SEDECO(ng)%FeOOH(k,i,j)       &   ! FeOOH (nmol g(DW)-1 s-1)) *solid
        , SEDECO(ng)%FeOOH_PO4(:,k,i,j) &   ! FeOOH=PO4 (nmol g(DW)-1 s-1)) *solid
        , SEDECO(ng)%H2S(:,k,i,j)       &   ! H2S (umol L-1) 
        , SEDECO(ng)%S0 (:,k,i,j)       &   ! S (nmol g(DW)-1 s-1)) *solid
        , SEDECO(ng)%SO4(:,k,i,j)       &   ! SO42- (umol L-1) 
       ! input and output parameters
        , R_O2 (k)          &   ! d[O2]/dt  (umol L-1 s-1) 
        , R_TA (k)          &   ! dTA/dt  (umol L-1 s-1) 
        , R_DIC(:,k)        &   ! dDIC/dt (umol L-1 s-1) 
        , R_NO3(:,k)        &   ! d[NO3]/dt (umol L-1 s-1)
        , R_NH4(:,k)        &   ! d[NH4]/dt (umol L-1 s-1)
        , R_PO4(:,k)        &   ! d[PO4]/dt (umol L-1 s-1)
        , R_DOC(:,:,k)      &   ! dDOC/dt (umol L-1 s-1) 
        , R_POC(:,:,k)      &   ! dPOC/dt (umol L-1 s-1) or (nmol g(DW)-1) *solid
        , R_DON(:,:,k)      &   ! dDON/dt (umol L-1 s-1)
        , R_PON(:,:,k)      &   ! dPON/dt (umol L-1 s-1) or (nmol g(DW)-1) *solid
        , R_DOP(:,:,k)      &   ! dDOP/dt (umol L-1 s-1)
        , R_POP(:,:,k)      &   ! dPOP/dt (umol L-1 s-1) or (nmol g(DW)-1) *solid
        , R_PIC(:,k)        &   ! d CaCO3/dt (umol L-1 s-1) or (nmol g(DW)-1) *solid
        , R_Mn2(k)          &   ! d[Mn2+]/dt (umol L-1 s-1)
        , R_MnO2(k)         &   ! d[MnO2]/dt (nmol g(DW)-1 s-1) *solid
        , R_Fe2(k)          &   ! d[Fe2+]/dt (umol L-1 s-1)
        , R_FeS(:,k)        &   ! d[FeS]/dt (nmol g(DW)-1 s-1) *solid
        , R_FeS2(:,k)       &   ! d[FeS2]/dt (nmol g(DW)-1 s-1) *solid
        , R_FeOOH(k)        &   ! d[FeOOH]/dt (nmol g(DW)-1 s-1) *solid
        , R_FeOOH_PO4(:,k)  &   ! d[FeOOH=O4]/dt (nmol g(DW)-1 s-1) *solid
        , R_H2S(:,k)        &   ! d[H2S]/dt (umol L-1 s-1)
        , R_S0(:,k)         &   ! d[S]/dt (nmol g(DW)-1 s-1) *solid
        , R_SO4(:,k)        &   ! d[SO42-]/dt (umol L-1 s-1)
       ! output parameters
        , pH                &   ! pH (total scale)
        , Wcal              &   ! Calcite saturation state
        , Warg              &   ! Aragonite saturation state
        )

    ENDDO
    
! ********************************************************************* !
!   Miscellaneous other sources
! ********************************************************************* !
!!! yuta_seagrass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# if defined SEAGRASS
    DO k = 1,Nsed-1
#  if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! [umol/L s] = [umol/L s]
      R_O2 (k) = R_O2 (k) + sum(SEDECO(ng)%sg_dO2_dt (k,:,i,j)) ! (umol L-1 s-1) change in DO  due to flux from seagrass (positive means seagrass to sediment)
      DO isp=1,N_Csp
        R_DIC(isp,k) = R_DIC(isp,k) + sum(SEDECO(ng)%sg_dDIC_dt(isp,k,:,i,j)) ! (umol L-1 s-1) change in DIC due to flux from seagrass (positive means seagrass to sediment)
        R_DOC(isp,iLDOM,k) = R_DOC(isp,iLDOM,k) + sum(SEDECO(ng)%sg_dDOC_dt(isp,k,:,i,j)) ! (umol L-1 s-1) change in DOC due to flux from seagrass (positive means seagrass to sediment)
      ENDDO
#  endif
#  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
      DO isp=1,N_Nsp
        R_NO3(isp,k) = R_NO3(isp,k) + sum(SEDECO(ng)%sg_dNO3_dt(isp,k,:,i,j)) ! (umol L-1 s-1) change in NO3 due to flux from seagrass (positive means seagrass to sediment)
        R_NH4(isp,k) = R_NH4(isp,k) + sum(SEDECO(ng)%sg_dNH4_dt(isp,k,:,i,j)) ! (umol L-1 s-1) change in NH4 due to flux from seagrass (positive means seagrass to sediment)
      ENDDO
      DO isp=1,N_Psp
        R_PO4(isp,k) = R_PO4(isp,k) + sum(SEDECO(ng)%sg_dPO4_dt(isp,k,:,i,j)) ! (umol L-1 s-1) change in PO4 due to flux from seagrass (positive means seagrass to sediment)
      ENDDO
#  endif    
#  if defined SEAGRASS_ROOT_POM
    ! [nmol/g s] = [nmol/g s]   +            [umol/L s]                                [cm3/g] (1000 nmol/umol) (L/1000 cm3)
      DO isp=1,N_Csp
        R_POC(isp,iCPOM,k) = R_POC(isp,iCPOM,k) &
                            + sum(SEDECO(ng)%sg_dPOC_dt(isp,k,:,i,j)) &
                                  / SEDECO(ng)%rhos_d(k,i,j) ! (nmol g-1 s-1) change in POC labile due to root death in sediment (positive means seagrass to sediment)
      ENDDO
#   if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
      DO isp=1,N_Nsp
        R_PON(isp,iCPOM,k) = R_PON(isp,iCPOM,k) &
                            + sum(SEDECO(ng)%sg_dPON_dt(isp,k,:,i,j)) &
                                  / SEDECO(ng)%rhos_d(k,i,j) ! (nmol g-1 s-1) change in PON labile due to root death in sediment (positive means seagrass to sediment)
      ENDDO
      DO isp=1,N_Psp
        R_POP(isp,iCPOM,k) = R_POP(isp,iCPOM,k) &
                            + sum(SEDECO(ng)%sg_dPOP_dt(isp,k,:,i,j)) &
                                  / SEDECO(ng)%rhos_d(k,i,j) ! (nmol g-1 s-1) change in POP labile due to root death in sediment (positive means seagrass to sediment)
      ENDDO
#   endif  
#  endif  
    ENDDO
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

! ********************************************************************* !
!   Mass balance equations
! ********************************************************************* !

    !! Mass Balance cal (Concentration at each layer in sed column)
    DO k = 1,Nsed-1
      cfdd = SEDECO(ng)%poro(k,i,j)
      cfsd = SEDECO(ng)%rhos_d(k,i,j) * (1.0d0 - SEDECO(ng)%poro(k,i,j))
  
      ! -- Tmp -- !
      SEDECO(ng)%Tmp(k,i,j) = massbalance( SEDECO(ng)%Tmp(k,i,j)   &
                               ,Fw_Tmp(k),Fw_Tmp(k+1),R_Tmp(k)     &
                               ,SEDECO(ng)%dzr(k),cfdd,dt)
      ! -- Sal -- !
      SEDECO(ng)%Sal(k,i,j) = massbalance( SEDECO(ng)%Sal(k,i,j)   &
                               ,Fw_Sal(k),Fw_Sal(k+1),R_Sal(k)     &
                               ,SEDECO(ng)%dzr(k),cfdd,dt)
      ! -- O2 -- !
      SEDECO(ng)%O2(k,i,j) = massbalance( SEDECO(ng)%O2(k,i,j)   &
                               ,Fw_O2(k),Fw_O2(k+1),R_O2(k)      &
                               ,SEDECO(ng)%dzr(k),cfdd,dt)
      ! -- TA -- !
      SEDECO(ng)%TA(k,i,j) = massbalance( SEDECO(ng)%TA(k,i,j)   &
                               ,Fw_TA(k),Fw_TA(k+1),R_TA(k)      &
                               ,SEDECO(ng)%dzr(k),cfdd,dt )
      ! -- DIC -- !
      DO isp=1,N_Csp
        SEDECO(ng)%DIC(isp,k,i,j) = massbalance( SEDECO(ng)%DIC(isp,k,i,j)       &
                                    ,Fw_DIC(isp,k),Fw_DIC(isp,k+1),R_DIC(isp,k)  &
                                    ,SEDECO(ng)%dzr(k),cfdd,dt )
      ENDDO
      DO isp=1,N_Nsp
        ! -- NO3 -- !
        SEDECO(ng)%NO3(isp,k,i,j) = massbalance( SEDECO(ng)%NO3(isp,k,i,j)        &
                                      ,Fw_NO3(isp,k),Fw_NO3(isp,k+1),R_NO3(isp,k) &
                                      ,SEDECO(ng)%dzr(k),cfdd,dt ) 
        ! -- NH4 -- !
        SEDECO(ng)%NH4(isp,k,i,j) = massbalance( SEDECO(ng)%NH4(isp,k,i,j)        &
                                      ,Fw_NH4(isp,k),Fw_NH4(isp,k+1),R_NH4(isp,k) &
                                      ,SEDECO(ng)%dzr(k),cfdd,dt ) 
      ENDDO
      DO isp=1,N_Nsp
        ! -- PO4 -- !
        SEDECO(ng)%PO4(isp,k,i,j) = massbalance( SEDECO(ng)%PO4(isp,k,i,j)        &
                                      ,Fw_PO4(isp,k),Fw_PO4(isp,k+1),R_PO4(isp,k) &
                                      ,SEDECO(ng)%dzr(k),cfdd,dt )
      ENDDO
      ! -- DOM -- !
      DO m = 1,Ndom
        DO isp=1,N_Csp
          SEDECO(ng)%DOC(isp,m,k,i,j) = massbalance(SEDECO(ng)%DOC(isp,m,k,i,j)        &
                                     ,Fw_DOC(isp,m,k),Fw_DOC(isp,m,k+1),R_DOC(isp,m,k) &
                                     ,SEDECO(ng)%dzr(k),cfdd,dt)     
        ENDDO
        DO isp=1,N_Nsp
          SEDECO(ng)%DON(isp,m,k,i,j) = massbalance(SEDECO(ng)%DON(isp,m,k,i,j)        &
                                     ,Fw_DON(isp,m,k),Fw_DON(isp,m,k+1),R_DON(isp,m,k) &
                                     ,SEDECO(ng)%dzr(k),cfdd,dt)     
        ENDDO
        DO isp=1,N_Psp
          SEDECO(ng)%DOP(isp,m,k,i,j) = massbalance(SEDECO(ng)%DOP(isp,m,k,i,j)        &
                                     ,Fw_DOP(isp,m,k),Fw_DOP(isp,m,k+1),R_DOP(isp,m,k) &
                                     ,SEDECO(ng)%dzr(k),cfdd,dt)     
        ENDDO
      ENDDO
      ! -- POM (solid) -- !
      DO m = 1,Npom
        DO isp=1,N_Csp
          SEDECO(ng)%POC(isp,m,k,i,j) = massbalance(SEDECO(ng)%POC(isp,m,k,i,j)        &
                                     ,Fw_POC(isp,m,k),Fw_POC(isp,m,k+1),R_POC(isp,m,k) &
                                     ,SEDECO(ng)%dzr(k),cfsd,dt)     
        ENDDO
        DO isp=1,N_Nsp
          SEDECO(ng)%PON(isp,m,k,i,j) = massbalance(SEDECO(ng)%PON(isp,m,k,i,j)        &
                                     ,Fw_PON(isp,m,k),Fw_PON(isp,m,k+1),R_PON(isp,m,k) &
                                     ,SEDECO(ng)%dzr(k),cfsd,dt)     
        ENDDO
        DO isp=1,N_Psp
          SEDECO(ng)%POP(isp,m,k,i,j) = massbalance(SEDECO(ng)%POP(isp,m,k,i,j)        &
                                     ,Fw_POP(isp,m,k),Fw_POP(isp,m,k+1),R_POP(isp,m,k) &
                                     ,SEDECO(ng)%dzr(k),cfsd,dt)     
        ENDDO
      ENDDO
      ! -- PIC (solid) -- !
      DO isp=1,N_Csp
        SEDECO(ng)%PIC(isp,k,i,j) = massbalance(SEDECO(ng)%PIC(isp,k,i,j)      &
                                   ,Fw_PIC(isp,k),Fw_PIC(isp,k+1),R_PIC(isp,k) &
                                   ,SEDECO(ng)%dzr(k),cfsd,dt)     
      ENDDO

      ! -- Mn2+ -- !
      SEDECO(ng)%Mn2(k,i,j) = massbalance( SEDECO(ng)%Mn2(k,i,j)  &
                                ,Fw_Mn2(k),Fw_Mn2(k+1),R_Mn2(k)   &
                                ,SEDECO(ng)%dzr(k),cfdd,dt ) 
      ! -- Fe2+ -- !
      SEDECO(ng)%Fe2(k,i,j) = massbalance( SEDECO(ng)%Fe2(k,i,j)  &
                                ,Fw_Fe2(k),Fw_Fe2(k+1),R_Fe2(k)   &
                                ,SEDECO(ng)%dzr(k),cfdd,dt ) 
      ! -- MnO2 (solid) -- !
      SEDECO(ng)%MnO2(k,i,j) = massbalance( SEDECO(ng)%MnO2(k,i,j)  &
                                 ,Fw_MnO2(k),Fw_MnO2(k+1),R_MnO2(k) &
                                 ,SEDECO(ng)%dzr(k),cfsd,dt ) 
      ! -- FeOOH (solid) -- !
      SEDECO(ng)%FeOOH(k,i,j) = massbalance( SEDECO(ng)%FeOOH(k,i,j)   &
                                 ,Fw_FeOOH(k),Fw_FeOOH(k+1),R_FeOOH(k) &
                                 ,SEDECO(ng)%dzr(k),cfsd,dt ) 
      DO isp=1,N_Psp
        ! -- FeOOH=PO4 (solid) -- !
        SEDECO(ng)%FeOOH_PO4(isp,k,i,j) = massbalance( SEDECO(ng)%FeOOH_PO4(isp,k,i,j) &
                         ,Fw_FeOOH_PO4(isp,k),Fw_FeOOH_PO4(isp,k+1),R_FeOOH_PO4(isp,k) &
                         ,SEDECO(ng)%dzr(k),cfsd,dt ) 
      ENDDO
      DO isp=1,N_Ssp
        ! -- FeS (solid) -- !
        SEDECO(ng)%FeS(isp,k,i,j) = massbalance( SEDECO(ng)%FeS(isp,k,i,j)      &
                                  ,Fw_FeS(isp,k),Fw_FeS(isp,k+1),R_FeS(isp,k)   &
                                  ,SEDECO(ng)%dzr(k),cfsd,dt ) 
        ! -- FeS2 (solid) -- !
        SEDECO(ng)%FeS2(isp,k,i,j) = massbalance( SEDECO(ng)%FeS2(isp,k,i,j)    &
                                 ,Fw_FeS2(isp,k),Fw_FeS2(isp,k+1),R_FeS2(isp,k) &
                                 ,SEDECO(ng)%dzr(k),cfsd,dt ) 
        ! -- H2S -- !
        SEDECO(ng)%H2S(isp,k,i,j) = massbalance( SEDECO(ng)%H2S(isp,k,i,j)      &
                                ,Fw_H2S(isp,k),Fw_H2S(isp,k+1),R_H2S(isp,k)     &
                                ,SEDECO(ng)%dzr(k),cfdd,dt ) 
         ! -- S0 (solid) -- !
        SEDECO(ng)%S0(isp,k,i,j) = massbalance( SEDECO(ng)%S0(isp,k,i,j)        &
                                ,Fw_S0(isp,k),Fw_S0(isp,k+1),R_S0(isp,k)        &
                                ,SEDECO(ng)%dzr(k),cfsd,dt ) 
        ! -- SO4 -- !
        SEDECO(ng)%SO4(isp,k,i,j) = massbalance( SEDECO(ng)%SO4(isp,k,i,j)      &
                                ,Fw_SO4(isp,k),Fw_SO4(isp,k+1),R_SO4(isp,k)     &
                                ,SEDECO(ng)%dzr(k),cfdd,dt )
      ENDDO
    ENDDO

! ********************************************************************* !
!   set output parameters
! ********************************************************************* !
    Flux_Tmp = Fw_Tmp(1)
    Flux_Sal = Fw_Sal(1) 
    Flux_DO  = Fw_O2 (1) 
    Flux_TA  = Fw_TA (1) 
    Flux_DIC(:) = Fw_DIC(:,1)
    Flux_NO3(:) = Fw_NO3(:,1)
    Flux_NH4(:) = Fw_NH4(:,1)
    Flux_PO4(:) = Fw_PO4(:,1)
    Flux_POC(:,:) = Fw_POC(:,:,1)
    Flux_PON(:,:) = Fw_PON(:,:,1)
    Flux_POP(:,:) = Fw_POP(:,:,1)
    Flux_PIC(:) = Fw_PIC(:,1)
# if defined BLUE_TIDE 
!    Flux_Mn2  = Fw_Mn2 (1)
!    Flux_MnO2 = Fw_MnO2(1)
!    Flux_Fe2  = Fw_Fe2 (1)
    ! Flux_FeS (:) = Fw_FeS (:,1)  ! yt_edit diffusion is not modeled so flux is zero
    ! Flux_FeS2(:) = Fw_FeS2(:,1)  ! yt_edit diffusion is not modeled so flux is zero
!    Flux_FeOOH = Fw_FeOOH(1)
    ! Flux_FeOOH_PO4 = Fw_FeOOH_PO4(:,1)  ! yt_edit diffusion is not modeled so flux is zero
    Flux_H2S(:) = Fw_H2S(:,1)
    ! Flux_S0(:)  = -Fw_S0(:,1)  ! yt_edit diffusion is not modeled so flux is zero
!    Flux_SO4(:) = Fw_SO4(:,1)
# endif

! ********************************************************************* !
!   Print section
! ********************************************************************* !
# if defined SEDIMENT_TESTMODE
    IF( date*86400.0d0 >= dsec ) THEN
        if(dsec == 0.0d0) then
          dsec=date*86400.0d0+SEDECO_OUTPUT_INTERVAL*60.0d0
          ! Write lavels at the first step
          write(53,'(*(g0:,", "))') 'Time', 'Flux_Tmp', 'Flux_Sal','Flux_DO','Flux_TA' &
                      ,'Flux_NH4','Flux_NO3','Flux_PO4'   &
                      ,'Flux_DIC','Flux_LPOC','Flux_RPOC'!! sedeco-calcheck0.csv
          write(54,'(*(g0:,", "))') 'Time', 'Wp' !! sedeco-calcheck1.csv
          write(55,'(*(g0:,", "))') 'Time', 'Flux_DO_mmolm-2-h'
        else
          dsec=dsec+SEDECO_OUTPUT_INTERVAL*60.0d0
        endif
        
        ! Write data
        write(53,'(*(g0:,", "))') date, Flux_Tmp, Flux_Sal,Flux_DO,Flux_TA &
                    ,Flux_NH4(iNt),Flux_NO3(iNt),Flux_PO4(iPt)   &
                    ,Flux_DIC(iCt),Flux_POC(iCt,iLPOM),Flux_POC(iCt,iRPOM)!! sedeco-calcheck0.csv
        write(54,'(*(g0:,", "))') date, Wp(:) !! sedeco-calcheck1.csv
        write(55,'(*(g0:,", "))') date, Flux_DO*0.01d0*60d0*60d0   !! for debug mmol/m2/h
!        write(*,*) 'Time (day): ', date 
    endif
# endif

    RETURN

  END SUBROUTINE sedecosys

!!!***********************************************************************
!!!   Sediment biogeochemical parameters
!!!*********************************************************************** 
!     Added by Masa Muto (2023)

!! Ficks law (diffusion)
  ! General
  real(8) function Ficks(D,C1,C2,dz)
    implicit none
    real(8), intent(in) :: D
    real(8), intent(in) :: C1
    real(8), intent(in) :: C2
    real(8), intent(in) :: dz
    Ficks =  D * (C1 - C2)/dz
    return
  end function Ficks

!! --- Upstream Advection Flux calculation ------------------------------------------ !!
  real(8) function AdvUp(u,C1,C2)
    implicit none
    real(8), intent(in) :: u
    real(8), intent(in) :: C1
    real(8), intent(in) :: C2
    ! if      u>0, AdvUp = u*C1  
    ! else if u<0, AdvUp = u*C2
    AdvUp = ((u+abs(u))*C1 + (u-abs(u))*C2)*0.5d0
    return
  end function AdvUp

!! --- Mass balance calculation in sediment column ---------------------------- !!
  real(8) function massbalance(C,FTin,FTout,R,dz,cff,dt)
    implicit none
    real(8),intent(in) :: C             ! concentration                   (umol L-1 = nmol cm-3)
    real(8),intent(in) :: FTin          ! total flux (IN)                 (nmol cm-2 s-1)
    real(8),intent(in) :: FTout         ! total flux (OUT)                (nmol cm-2 s-1)
    real(8),intent(in) :: R             ! Net prodution and consumption   (nmol cm-3 s-1)
    real(8),intent(in) :: dz            ! layer thickness                 (cm)
    real(8),intent(in) :: cff           ! porosity                        (cm_w3 cm_s-3)
    real(8),intent(in) :: dt            ! time step                       (s)

    massbalance = C + ((FTin - FTout)/dz + R)/cff * dt
!  [nmol cm_w-3] = [nmol cm_w-3] + ([nmol cm_s-2 s-1]/(cm_s) + [nmol cm_s-3 s-1])/[cm_w3 cm_s-3] * [s] 
    return
  end function massbalance

#endif
END MODULE mod_sedecosys
