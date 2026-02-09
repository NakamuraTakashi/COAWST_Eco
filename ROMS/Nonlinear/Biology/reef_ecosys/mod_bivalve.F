
!!!=== Copyright (c) 2025-2026 Takashi NAKAMURA  =====

#include "cppdefs.h"


!!!*************** BIVALVE MODEL *****************************************


MODULE mod_bivalve
#ifdef BIVALVE
  implicit none

  TYPE T_BVLV
    real(8), pointer :: dens(:,:,:,:) 
    real(8), pointer :: QCe(:,:,:,:,:)
    real(8), pointer :: QCv(:,:,:,:,:)
    real(8), pointer :: QCr(:,:,:,:,:)
    real(8), pointer :: QCh(:,:,:,:,:)
    real(8), pointer :: QNe(:,:,:,:,:)
    real(8), pointer :: QNv(:,:,:,:,:)
    real(8), pointer :: QNr(:,:,:,:,:)
    real(8), pointer :: QNh(:,:,:,:,:)
    real(8), pointer :: QPe(:,:,:,:,:)
    real(8), pointer :: QPv(:,:,:,:,:)
    real(8), pointer :: QPr(:,:,:,:,:)
    real(8), pointer :: QPh(:,:,:,:,:)
    real(8), pointer :: CaCO3(:,:,:,:,:)
    real(8), pointer :: Dead_CaCO3(:,:,:,:,:)
    real(8), pointer :: R   (:,:,:,:) 
    real(8), pointer :: G   (:,:,:,:) 
    real(8), pointer :: V   (:,:,:,:) 
    real(8), pointer :: L   (:,:,:,:) 
    real(8), pointer :: Wd  (:,:,:,:) 
    real(8), pointer :: Ww  (:,:,:,:) 
    real(8), pointer :: Wt  (:,:,:,:) 
  END TYPE T_BVLV

  TYPE (T_BVLV), allocatable :: BVLV(:)

CONTAINS


!!! **********************************************************************
!!!  set initial conditions for the bivalve model
!!! **********************************************************************

  SUBROUTINE allocate_bivalve(ng, Ngrids, Nz, LBi, UBi, LBj, UBj)
    USE mod_reef_ecosys_param
    
    implicit none
    integer, intent(in) :: ng, Ngrids, Nz, LBi, UBi, LBj, UBj

    IF (ng.eq.1) allocate ( BVLV(Ngrids) )
    allocate( BVLV(ng)%dens     (Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%QCe(N_Csp,Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%QCv(N_Csp,Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%QCr(N_Csp,Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%QCh(N_Csp,Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%QNe(N_Nsp,Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%QNv(N_Nsp,Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%QNr(N_Nsp,Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%QNh(N_Nsp,Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%QPe(N_Psp,Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%QPv(N_Psp,Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%QPr(N_Psp,Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%QPh(N_Psp,Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%CaCO3(N_Csp,Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%Dead_CaCO3(N_Csp,Nbv,Nz,LBi:UBi,LBj:UBj) )

    allocate( BVLV(ng)%R        (Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%G        (Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%V        (Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%L        (Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%Wd       (Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%Ww       (Nbv,Nz,LBi:UBi,LBj:UBj) )
    allocate( BVLV(ng)%Wt       (Nbv,Nz,LBi:UBi,LBj:UBj) )

    RETURN

  END SUBROUTINE allocate_bivalve


!!! **********************************************************************
!!!  set initial conditions for the bivalve model
!!! **********************************************************************

  SUBROUTINE initialize_bivalve(ng, Nz, dz, LBi, UBi, LBj, UBj, dens_aqua, f_dens, start_of_new_run)
    USE mod_reef_ecosys_param
    USE mod_aquaculture
    USE mod_deb_model
    
    implicit none
! input parameters
    integer, intent(in) :: ng, Nz, LBi, UBi, LBj, UBj
    real(8), intent(in) :: dz(LBi:UBi,LBj:UBj,Nz)
    real(8), intent(in) :: dens_aqua(Naq,LBi:UBi,LBj:UBj)     ! Density of oyster aquaculture (rope m-3)
    real(8), intent(in) :: f_dens                             ! Factor to adjust oyster density (default = 1.0)
    logical, intent(in) :: start_of_new_run               ! TRUE = initialize; FALSE = continue from previous run
! --- Dynamic Energy Budget (DEB) model parameters ---
    real(8), parameter :: delta_M(2) = [   0.245d0,   0.245d0 ] ! (no dim.)      Shape parameter (0.175: Van der Veer et al., 2006; 0.245: Sakamaki et al., 2025)
    real(8), parameter :: C2Wd(2) = [12.0d0/0.51d0,12.0d0/0.51d0] ! (g(dw) molC−1) [carbon mass]/[dry mass] = 0.51+-0.07 (Sakamaki et al. 2022; Sakamaki et al. 2020; Fujibayashi et al. 2021). 
    real(8), parameter :: Ww2Wd(2)   = [   0.19d0 ,    0.19d0 ] ! (g(dw) cm−3)   ! 0.19+-0.02 g(dw)/g(ww), 1g(ww)/cm3 (Sakamaki et al. 2022; Sakamaki et al. 2020; Fujibayashi et al. 2021).
    real(8) :: R13C
    integer :: i,j,k,n

!------------------------------------------
!  Set initial conditions
!   analytical start
    if( start_of_new_run ) then
    !Initialize bivalve variables
     BVLV(ng)%dens(:,:,:,:) = 0.0d0
     BVLV(ng)%R   (:,:,:,:) = 0.0d0
     BVLV(ng)%G   (:,:,:,:) = 0.0d0
     BVLV(ng)%V   (:,:,:,:) = 0.0d0
     BVLV(ng)%L   (:,:,:,:) = 0.0d0
     BVLV(ng)%Ww  (:,:,:,:) = 0.0d0
     BVLV(ng)%Wd  (:,:,:,:) = 0.0d0
     BVLV(ng)%Wt  (:,:,:,:) = 0.0d0
 
     BVLV(ng)%QCe(:,:,:,:,:) = 0.0d0
     BVLV(ng)%QCv(:,:,:,:,:) = 0.0d0
     BVLV(ng)%QCr(:,:,:,:,:) = 0.0d0
     BVLV(ng)%QCh(:,:,:,:,:) = 0.0d0
     BVLV(ng)%QNe(:,:,:,:,:) = 0.0d0
     BVLV(ng)%QNv(:,:,:,:,:) = 0.0d0
     BVLV(ng)%QNr(:,:,:,:,:) = 0.0d0
     BVLV(ng)%QNh(:,:,:,:,:) = 0.0d0
     BVLV(ng)%QPe(:,:,:,:,:) = 0.0d0
     BVLV(ng)%QPv(:,:,:,:,:) = 0.0d0
     BVLV(ng)%QPr(:,:,:,:,:) = 0.0d0
     BVLV(ng)%QPh(:,:,:,:,:) = 0.0d0
     BVLV(ng)%CaCO3(:,:,:,:,:) = 0.0d0  ! CaCO3 in living bivalve (mmolC ind.-1)
     BVLV(ng)%Dead_CaCO3(:,:,:,:,:) = 0.0d0  ! CaCO3 in dead bivalve (mmolC m-3)

     do j=LBj,UBj
       do i=LBi,UBi

         if (dens_aqua(1,i,j) > 0.0d0) then
           CALL dens_oyster_Shizugawa(dens_aqua(1,i,j), Nz, dz(i,j,:), f_dens, BVLV(ng)%dens(iOys1,:,i,j))   ! Initial bivalve density (ind. m-3)
           do k=1,Nz !!!>>>>>>>>>>>DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if(BVLV(ng)%dens(iOys1,k,i,j)*0.0d0/=0.0d0) then
              write(*,*) 'Error', i,j,k, BVLV(ng)%dens(iOys1,k,i,j)
              BVLV(ng)%dens(iOys1,k,i,j) = 0.0d0
            end if
           enddo     !!!<<<<<<<<<<<DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           CALL dens_oyster_Shizugawa(dens_aqua(1,i,j), Nz, dz(i,j,:), f_dens, BVLV(ng)%dens(iOys2,:,i,j))   ! Initial bivalve density (ind. m-3)
           do k=1,Nz !!!>>>>>>>>>>>DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if(BVLV(ng)%dens(iOys2,k,i,j)*0.0d0/=0.0d0) then
              write(*,*) 'Error', i,j,k, BVLV(ng)%dens(iOys2,k,i,j)
              BVLV(ng)%dens(iOys2,k,i,j) = 0.0d0
            end if
           enddo     !!!<<<<<<<<<<<DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         end if

         do k=1,Nz
           do n=1,Nbv
             
             ! initialize for each bivalve group
             if (n.eq.iOys1) then ! 01: Oyster
               if (BVLV(ng)%dens(n,k,i,j) > 0.0d0) then
                 !--- Shell length (cm) -----------------
                 BVLV(ng)%L(n,k,i,j)  = 0.5d0  ! cm
                 !--- Body volume (cm3) ----------------- 
                 BVLV(ng)%V(n,k,i,j)  = (delta_M(n)*BVLV(ng)%L(n,k,i,j))**3.0d0  ! mmolC -> cm3
                                                                        ! w_V   = 12.0d0/0.51d0 (g(dw) molC−1) [carbon mass]/[dry mass]=0.51d0 (Sakamaki et al. 2022; Sakamaki et al. 2020; Fujibayashi et al. 2021). 
                 BVLV(ng)%QCv(iCt,n,k,i,j) = BVLV(ng)%V(n,k,i,j)*Ww2Wd(n)/C2Wd(n)*1.0d3 *0.99d0! mmolC individual-1
                 BVLV(ng)%QCe(iCt,n,k,i,j) = BVLV(ng)%QCv(iCt,n,k,i,j) *0.01d0  ! mmolC individual-1
                 BVLV(ng)%QCr(iCt,n,k,i,j) = 0.0d0 ! mmolC individual-1
                 BVLV(ng)%QCh(iCt,n,k,i,j) = 0.0d0 ! mmolC individual-1
                 BVLV(ng)%QNv(iNt,n,k,i,j) = BVLV(ng)%QCv(iCt,n,k,i,j)*16.0d0/106.0d0  ! mmolN individual-1
                 BVLV(ng)%QNe(iNt,n,k,i,j) = BVLV(ng)%QCe(iCt,n,k,i,j)*16.0d0/106.0d0  ! mmolN individual-1
                 BVLV(ng)%QNr(iNt,n,k,i,j) = BVLV(ng)%QCr(iCt,n,k,i,j)*16.0d0/106.0d0  ! mmolN individual-1
                 BVLV(ng)%QNh(iNt,n,k,i,j) = BVLV(ng)%QCh(iCt,n,k,i,j)*16.0d0/106.0d0  ! mmolN individual-1
                 BVLV(ng)%QPv(iPt,n,k,i,j) = BVLV(ng)%QCv(iCt,n,k,i,j)* 1.0d0/106.0d0  ! mmolP individual-1
                 BVLV(ng)%QPe(iPt,n,k,i,j) = BVLV(ng)%QCe(iCt,n,k,i,j)* 1.0d0/106.0d0  ! mmolP individual-1
                 BVLV(ng)%QPr(iPt,n,k,i,j) = BVLV(ng)%QCr(iCt,n,k,i,j)* 1.0d0/106.0d0  ! mmolP individual-1
                 BVLV(ng)%QPh(iPt,n,k,i,j) = BVLV(ng)%QCh(iCt,n,k,i,j)* 1.0d0/106.0d0  ! mmolP individual-1
                 !--- Total dry weight (g) (mmolC -> g(dw)) -----------------
                 BVLV(ng)%Wd(n,k,i,j) = (  BVLV(ng)%QCe(iCt,n,k,i,j) + BVLV(ng)%QCv(iCt,n,k,i,j)    &
                                         + BVLV(ng)%QCr(iCt,n,k,i,j) + BVLV(ng)%QCh(iCt,n,k,i,j) )  &
                                         * C2Wd(n) * 1.0d-3  ! mmolC -> g(dw)  !!!!! Check the conversion factor !!!!!!
                 !--- Wet flesh (meat) weight (g) (Wd g(dw)-> Ww g(ww)) -----------------
                 BVLV(ng)%Ww(n,k,i,j) = BVLV(ng)%Wd(n,k,i,j) / Ww2Wd(n)
                 !--- Total (shell + flesh) weight (g) (L (cm) -> Wt g(ww)) (Kobayashi et al., 1997)-----------------
                 BVLV(ng)%Wt(n,k,i,j) = 0.105d0 * BVLV(ng)%L(n,k,i,j)**2.967d0 ! g(wet weight)
                 !--- CaCO3 in living bivalve (mmolC ind.-1)
                 BVLV(ng)%CaCO3(iCt,n,k,i,j) = (BVLV(ng)%Wt(n,k,i,j)-BVLV(ng)%Ww(n,k,i,j)) * 10.0d0 ! = 1000.0d0 / 100.09d0 ! g -> mmol CaCO3               BVLV(ng)%QCv(iCt,n,k,i,j) = BVLV(ng)%Wd(n,k,i,j)*0.51d0/12.0d0 * 1.0d3 *0.9d0! mmolC individual-1
                                                                        ! [carbon mass]/[dry mass] = 0.51+-0.07 (Sakamaki et al. 2022; Sakamaki et al. 2020; Fujibayashi et al. 2021). 
               end if
             else if (n.eq.iOys2) then ! 01: Oyster
               if (BVLV(ng)%dens(n,k,i,j) > 0.0d0) then
                 !--- Shell length (cm) -----------------
                 BVLV(ng)%L(n,k,i,j)  = 6.6d0  ! cm
                 !--- Body volume (cm3) ----------------- 
                 BVLV(ng)%V(n,k,i,j)  = (delta_M(n)*BVLV(ng)%L(n,k,i,j))**3.0d0  ! mmolC -> cm3
                                                                        ! w_V   = 12.0d0/0.51d0 (g(dw) molC−1) [carbon mass]/[dry mass]=0.51d0 (Sakamaki et al. 2022; Sakamaki et al. 2020; Fujibayashi et al. 2021). 
                 BVLV(ng)%QCv(iCt,n,k,i,j) = BVLV(ng)%V(n,k,i,j)*Ww2Wd(n)/C2Wd(n)*1.0d3 *0.99d0! mmolC individual-1
                 BVLV(ng)%QCe(iCt,n,k,i,j) = BVLV(ng)%QCv(iCt,n,k,i,j) *0.01d0  ! mmolC individual-1
                 BVLV(ng)%QCr(iCt,n,k,i,j) = 17.8d0 ! mmolC individual-1
                 BVLV(ng)%QCh(iCt,n,k,i,j) = 1.0d0 ! mmolC individual-1
                 BVLV(ng)%QNv(iNt,n,k,i,j) = BVLV(ng)%QCv(iCt,n,k,i,j)*16.0d0/106.0d0  ! mmolN individual-1
                 BVLV(ng)%QNe(iNt,n,k,i,j) = BVLV(ng)%QCe(iCt,n,k,i,j)*16.0d0/106.0d0  ! mmolN individual-1
                 BVLV(ng)%QNr(iNt,n,k,i,j) = BVLV(ng)%QCr(iCt,n,k,i,j)*16.0d0/106.0d0  ! mmolN individual-1
                 BVLV(ng)%QNh(iNt,n,k,i,j) = BVLV(ng)%QCh(iCt,n,k,i,j)*16.0d0/106.0d0  ! mmolN individual-1
                 BVLV(ng)%QPv(iPt,n,k,i,j) = BVLV(ng)%QCv(iCt,n,k,i,j)* 1.0d0/106.0d0  ! mmolP individual-1
                 BVLV(ng)%QPe(iPt,n,k,i,j) = BVLV(ng)%QCe(iCt,n,k,i,j)* 1.0d0/106.0d0  ! mmolP individual-1
                 BVLV(ng)%QPr(iPt,n,k,i,j) = BVLV(ng)%QCr(iCt,n,k,i,j)* 1.0d0/106.0d0  ! mmolP individual-1
                 BVLV(ng)%QPh(iPt,n,k,i,j) = BVLV(ng)%QCh(iCt,n,k,i,j)* 1.0d0/106.0d0  ! mmolP individual-1
                 !--- Total dry weight (g) (mmolC -> g(dw)) -----------------
                 BVLV(ng)%Wd(n,k,i,j) = (  BVLV(ng)%QCe(iCt,n,k,i,j) + BVLV(ng)%QCv(iCt,n,k,i,j)    &
                                         + BVLV(ng)%QCr(iCt,n,k,i,j) + BVLV(ng)%QCh(iCt,n,k,i,j) )  &
                                         * C2Wd(n) * 1.0d-3  ! mmolC -> g(dw)  !!!!! Check the conversion factor !!!!!!
                 !--- Wet flesh (meat) weight (g) (Wd g(dw)-> Ww g(ww)) -----------------
                 BVLV(ng)%Ww(n,k,i,j) = BVLV(ng)%Wd(n,k,i,j) / Ww2Wd(n)
                 !--- Total (shell + flesh) weight (g) (L (cm) -> Wt g(ww)) (Kobayashi et al., 1997)-----------------
                 BVLV(ng)%Wt(n,k,i,j) = 0.105d0 * BVLV(ng)%L(n,k,i,j)**2.967d0 ! g(wet weight)
                 !--- CaCO3 in living bivalve (mmolC ind.-1)
                 BVLV(ng)%CaCO3(iCt,n,k,i,j) = (BVLV(ng)%Wt(n,k,i,j)-BVLV(ng)%Ww(n,k,i,j)) * 10.0d0 ! = 1000.0d0 / 100.09d0 ! g -> mmol CaCO3               BVLV(ng)%QCv(iCt,n,k,i,j) = BVLV(ng)%Wd(n,k,i,j)*0.51d0/12.0d0 * 1.0d3 *0.9d0! mmolC individual-1
                                                                        ! [carbon mass]/[dry mass] = 0.51+-0.07 (Sakamaki et al. 2022; Sakamaki et al. 2020; Fujibayashi et al. 2021). 
               end if
             end if
 
           enddo
         enddo
       enddo
     enddo
    end if
    
    RETURN
      
  END SUBROUTINE initialize_bivalve

!!! **********************************************************************
!!!  Main program of the bivalve model
!!! **********************************************************************

  SUBROUTINE bivalve  &
!   input parameters
    ( ng, n, i, j, k &   ! ng: nested grid number; n: bivalve compartment; i,j,k: position
    , date           &   ! Date (days since x or elapsed days)
    , dt             &   ! Time step (sec)
    , rho_sw         &   ! Density of seawater (g cm-3)
    , Tmp            &   ! Temperature (oC)
    , Sal            &   ! Salinity (PSU)
    , DOx            &   ! DOx             : Dissolved oxygen (umol O2 L-1)
    , DIC            &   ! DIC(N_Csp)      : Total dissolved inorganic carbon (umol C L-1)
    , Wcal           &   ! Calcite saturation state
    , PhyC           &   ! Phytoplankton C bomass (umol C L-1)
    , PhyN           &   ! Phytoplankton N bomass (umol N L-1)
    , PhyP           &   ! Phytoplankton P bomass (umol P L-1)
    , PIC            &   ! Particulate inorganic carbon (PIC: umolC L-1), coccolith (CaCO3)
!   output parameters
    , dDOx_dt        &   ! dDOx_dt             : dDOx/dt  (umol O2 L-1 s-1) 
    , dTA_dt         &   ! dTA_dt              : dTA/dt   (umol L-1 s-1) 
    , dDIC_dt        &   ! dDIC_dt(N_Csp)      : dDIC/dt  (umol C L-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    , dNH4_dt        &   ! dNH4_dt(N_Nsp)      : dNH4/dt  (umol N L-1 s-1)
    , dPO4_dt        &   ! dPO4_dt(N_Psp)      : dPO4/dt  (umol P L-1 s-1)
    , dDOC_dt        &   ! dDOC_dt(N_Csp,Ndom) : dDOC/dt  (umol C L-1 s-1) 
    , dPOC_dt        &   ! dPOC_dt(N_Csp,Npom) : dPOC/dt  (umol C L-1 s-1) 
    , dDON_dt        &   ! dDON_dt(N_Nsp,Ndom) : dDON/dt  (umol N L-1 s-1)
    , dPON_dt        &   ! dPON_dt(N_Nsp,Npom) : dPON/dt  (umol N L-1 s-1)
    , dDOP_dt        &   ! dDOP_dt(N_Psp,Ndom) : dDOP/dt  (umol P L-1 s-1)
    , dPOP_dt        &   ! dPOP_dt(N_Psp,Npom) : dPOP/dt  (umol P L-1 s-1)
    , dPhyC_dt       &   ! dPhyC_dt(N_Csp,Nphy): dPhyC/dt (umol C L-1 s-1)  
    , dPhyN_dt       &   ! dPhyN_dt(N_Nsp,Nphy): dPhyN/dt (umol N L-1 s-1)  
    , dPhyP_dt       &   ! dPhyP_dt(N_Psp,Nphy): dPhyP/dt (umol P L-1 s-1)  
    , dPIC_dt        &   ! dPIC_dt (N_Csp,Npim): dPIC/dt  (umol C L-1 s-1)
    )

!-----------------------------------------------------------------------
    USE mod_reef_ecosys_param
    USE mod_geochem
    USE mod_deb_model
    
    implicit none

! input parameters
    integer, intent(in) :: ng, n, i, j, k   ! ng: nested grid number; n: bivalve compartment; i,j,k: position
    real(8), intent(in) :: date                 ! Date (days since x or elapsed days)
    real(8), intent(in) :: dt                   ! Time step (sec)
    real(8), intent(in) :: rho_sw               ! Density of seawater (g cm-3)
    real(8), intent(in) :: Tmp                  ! Temperature (oC)
    real(8), intent(in) :: Sal                  ! Salinity (PSU)
    real(8), intent(in) :: DOx                  ! Dissolved oxygen (umol L-1)
    real(8), intent(in) :: DIC(N_Csp)           ! Total dissolved inorganic carbon (DIC: umol kg-1)
    real(8), intent(in) :: Wcal                 ! Calcite saturation state
    real(8), intent(in) :: PhyC(N_Csp,Nphy)     ! phytoplankton C biomass (umol C L-1), dinoflagellate
    real(8), intent(in) :: PhyN(N_Nsp,Nphy)     ! phytoplankton N biomass (umol N L-1), dinoflagellate
    real(8), intent(in) :: PhyP(N_Psp,Nphy)     ! phytoplankton P biomass (umol P L-1), dinoflagellate
    real(8), intent(in) :: PIC (N_Csp,Npim)     ! Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
! output parameters
    real(8), intent(inout) :: dDOx_dt              ! dDOx/dt  (umol L-1 s-1) 
    real(8), intent(inout) :: dTA_dt               ! dTA/dt   (umol L-1 s-1) 
    real(8), intent(inout) :: dDIC_dt(N_Csp)       ! dDIC/dt  (umol L-1 s-1)  1 mmol m-3 = 1 umol L-1
    real(8), intent(inout) :: dNH4_dt(N_Nsp)       ! dNH4/dt (umol L-1 s-1)
    real(8), intent(inout) :: dPO4_dt(N_Psp)       ! dPO4/dt (umol L-1 s-1)
    real(8), intent(inout) :: dDOC_dt(N_Csp,Ndom)  ! dDOC/dt  (umol L-1 s-1) 
    real(8), intent(inout) :: dPOC_dt(N_Csp,Npom)  ! dPOC/dt  (umol L-1 s-1) 
    real(8), intent(inout) :: dDON_dt(N_Nsp,Ndom)  ! dDON/dt (umol L-1 s-1)
    real(8), intent(inout) :: dPON_dt(N_Nsp,Npom)  ! dPON/dt (umol L-1 s-1)
    real(8), intent(inout) :: dDOP_dt(N_Psp,Ndom)  ! dDOP/dt (umol L-1 s-1)
    real(8), intent(inout) :: dPOP_dt(N_Psp,Npom)  ! dPOP/dt (umol L-1 s-1)
    real(8), intent(inout) :: dPhyC_dt(N_Csp,Nphy) ! dPhyC/dt  (umolC L-1 s-1)  
    real(8), intent(inout) :: dPhyN_dt(N_Nsp,Nphy) ! dPhyN/dt  (umolN L-1 s-1)  
    real(8), intent(inout) :: dPhyP_dt(N_Psp,Nphy) ! dPhyP/dt  (umolP L-1 s-1)  
    real(8), intent(inout) :: dPIC_dt (N_Csp,Npim) ! dPIC/dt  (umol L-1 s-1) 

! --- C:N:P:CaCO3 ratio of bivalves ---
    real(8), parameter :: rNCb(2)  = [ 16d0/106d0, 16d0/106d0 ] ! (no dim.) Oyster N:C ratio
    real(8), parameter :: rPCb(2)  = [  1d0/106d0,   1d0/106d0 ] ! (no dim.) Oyster P:C ratio
    real(8), parameter :: rCaCb(2) = [      0.0d0,      0.0d0 ]  ! ***** (no dim.) CaCO3:OrgC ratio !!! Need Ref!!!!!!!!!!
! --- Photoplankton ingestion parameters ---
    ! Assimilation efficiency (no dim.)  (0.75: Van der Veer et al., 2006)
    !                                   from     Dnfl       Dtm        Ccl         Cya
    real(8), parameter :: AE(4,2) = reshape( [  0.75d0,    0.75d0,    0.75d0,     0.75d0  &
                                              , 0.75d0,    0.75d0,    0.75d0,     0.75d0  ], [4,2] ) ! to bivalve (no dim.)      Assimilation efficiency

! --- Dynamic Energy Budget (DEB) model parameters ---
    real(8), parameter :: delta_M(2) = [   0.245d0,   0.245d0 ] ! (no dim.)      Shape parameter (0.175: Van der Veer et al., 2006; 0.245: Sakamaki et al., 2025)
    real(8), parameter :: C2Wd(2) = [12.0d0/0.51d0,12.0d0/0.51d0] ! (g(dw) molC−1) [carbon mass]/[dry mass] = 0.51+-0.07 (Sakamaki et al. 2022; Sakamaki et al. 2020; Fujibayashi et al. 2021). 
    real(8), parameter :: Ww2Wd(2)   = [   0.19d0 ,    0.19d0 ] ! (g(dw) cm−3)   ! 0.19+-0.02 g(dw)/g(ww), 1g(ww)/cm3 (Sakamaki et al. 2022; Sakamaki et al. 2020; Fujibayashi et al. 2021).
    real(8), parameter :: Tref(2)    = [  293.15d0,  293.15d0] ! (K) Refernce temperature
    real(8), parameter :: T_A(2)     = [ 5800.0d0, 5800.0d0 ] ! (K) Arrhenius temperature (Van der Veer et al., 2006)
!    real(8), parameter :: T_H(2)     = [  305.0d0,  305.0d0] ! (K) Upper boundary tolerance range (Van der Veer et al., 2006)
    real(8), parameter :: T_Hing(2)  = [  298.0d0,  298.0d0 ] ! (K) Upper boundary tolerance range for ingestion (Bourles et al., 2009)
    real(8), parameter :: T_Hresp(2) = [  305.0d0,  305.0d0 ] ! (K) Upper boundary tolerance range for respiration (Bourles et al., 2009)
    real(8), parameter :: T_AH(2)    = [30000.0d0, 30000.0d0] ! (K) Arrhenius temp. for upper boundary (Van der Veer et al., 2006)
!    real(8), parameter :: T_L(2)     = [  281.0d0, 281.0d0 ] ! (K) Lower boundary tolerance range (Van der Veer et al., 2006)
    real(8), parameter :: T_L(2)     = [  276.0d0,  276.0d0 ] ! (K) Lower boundary tolerance range (Bourles et al., 2009)
    real(8), parameter :: T_AL(2)    = [75000.0d0, 75000.0d0] ! (K) Arrhenius temp. for lower boundary (Van der Veer et al., 2006)
    !  The clearance rate of oysters may be two times higher than that of mussels. (Check is needed!!)
!    real(8), parameter :: C_Rmax  (2) = [ 2.2d-3, 2.2d-3 ] !Maximum surface area specific clearance rate (L s-1 cm-2)
    real(8), parameter :: C_Rmax  (2) = [ 2.2d-2, 2.2d-2 ] !Maximum surface area specific clearance rate (L s-1 cm-2)
!    real(8), parameter :: C_Rmax  (2) = [ 1.5d-3, 1.5d-3 ] !Maximum surface area specific clearance rate (L s-1 cm-2)
    ! Maximum surface area specific filtration rate (umol s−1 cm−2)
    !  4.8d-4 (molC d−1 cm−2), Rosland et al. (2009)
    real(8), parameter :: J_XiFmax(4,2) = reshape( [  0.0693d0,  0.0693d0,  0.0693d0,   0.0693d0 &
                                                    , 0.0693d0,  0.0693d0,  0.0693d0,   0.0693d0 ], [4,2] ) ! to Oyster
!    real(8), parameter :: J_XiFmax(4,1) = reshape( [ 5.56d-3, 5.56d-3,  5.56d-3,  5.56d-3  ], [4,1] ) ! to Oyster
    ! Algal binding probability (no dim.)
    !  0.4 (no dim.),  Saraiva et al. (2011a)
    real(8), parameter :: rho_XiI (4,2) = reshape( [ 0.4d0, 0.4d0, 0.4d0, 0.4d0 &
                                                   , 0.4d0, 0.4d0, 0.4d0, 0.4d0 ], [4,2] ) ! to Oyster
    ! Algal maximum ingestion rate (umol s−1)
    !  1.3d4 (molC d-1), Saraiva et al. (2011a)
    real(8), parameter :: J_XiImax(4,2) = reshape( [ 0.15d0,    0.15d0,    0.15d0,     0.15d0 &
                                                   , 0.15d0,    0.15d0,    0.15d0,     0.15d0 ], [4,2] ) ! to Oyster
    !!! Need to be checked !!!
    real(8), parameter :: c2e = 0.0d0 ! enrtgy cost for water exchange (J L-1)
  ! --- Assimiration related parameters ---
    real(8), parameter :: y_EX(2) = [ 0.75d0,  0.75d0 ] ! Yield coefficient of reserves in algal structure (Saraiva et al., 2012)
  ! --- Somatic maintenance related parameters ---
    real(8), parameter :: mu_E(2) = [ 5.29d5, 5.29d5 ] ! (J molC-1), Energy content of reserves (Ren & Schiel, 2008)
    ! mu_E = 22.49 (J mg−1) = 5.29d5 (J molC-1),  Energy content of reserves (Ren & Schiel, 2008)
    !                        12.0d0/0.51d0 (g(dw) molC−1) [carbon mass]/[dry mass]=0.51d0 (Sakamaki et al. 2022; Sakamaki et al. 2020; Fujibayashi et al. 2021). 
    real(8), parameter :: p_M(2) = [ 2.14d-4, 2.14d-4 ] ! (J s-1 cm-3), volume specific somatic maintenance rate (18.5 (J d-1 cm-3): Ren & Schiel, 2008)
  ! --- Mobilization flux related parameters ---
    real(8), parameter :: E_G(2)  = [ 2.9d3, 2.9d3 ]  !  Specific cost for structure ([E_G] = 2900 (J cm-3), Ren & Schiel, 2008)
    real(8), parameter :: cond(2) = [ 3.83d-6, 3.83d-6 ]  ! (cm s-1) Energy conductance; 0.01381 (cm h-1), (Monaco & McQuaid, 2018)
    real(8), parameter :: kappa(2)= [ 0.55d0, 0.55d0 ]  ! (no dim.) Allocation fraction to growth and somatic maintenance, (Ren & Schiel, 2008)
  ! --- Maturity and reproduction related parameters --- 
    ! E_Hp (J) = 1.58d-2 (J)  Maturity at puberty (Saraiva et al., 2011)
    ! mu_E = 22.49 (J mg−1) = 5.29d5 (J molC-1),  Energy content of reserves (Ren & Schiel, 2008)
    !                        12.0d0/0.51d0 (g(dw) molC−1) [carbon mass]/[dry mass]=0.51d0 (Sakamaki et al. 2022; Sakamaki et al. 2020; Fujibayashi et al. 2021). 
    ! Mp_H = E_Hp/mu_E = 2.99d-4 molC = 2.99d-1 mmolC
    real(8), parameter :: Mp_H(2)     = [ 1.0d0, 1.0d0 ]  ! Cumulative maturity at puberty (mmolC) (2.99d-1; Saraiva et al., 2011)
    real(8), parameter :: kappa_R(2)  = [ 0.7d0, 0.7d0 ]  ! (no dim.) Reproduction efficiency (Pouvreau et al., 2006)
    real(8), parameter :: Rspawn(2)   = [ 1.0d0, 1.0d0 ]  ! (days) Spawning period (1 day? assumed in Saraiva et al. (2012))
    real(8), parameter :: GSRspawn(2) = [ 0.46d0, 0.46d0 ]  ! (molC molC−1) Gonado-somatic ratio to spawn (0.46d0; Bernard et al., 2011)
    real(8), parameter :: Tspawn(2)   = [ 19.0d0, 19.0d0 ]  ! (oC) Minimum temperature for spawning, (Bernard et al., 2011)
  ! --- Mortality (s-1) --- 
    real(8), parameter :: BM(2)   = [ 0.0d0, 0.0d0 ]  ! (s-1) Background mortality rate

! --- Variables ---
    real(8) :: kT   ! Temperature correction factor (no dimension)
    real(8) :: C_R  ! Clearance rate (m3 d−1)
    real(8) :: J_Rc ! Required energy for water exchange (J s-1)

    real(8) :: J_XiF(Nphy) ! Filtration rate (umol s−1)
    real(8) :: J_XiI(Nphy) ! Ingestion rate (umol s−1)
    real(8) :: J_PiF(Nphy) ! Pseudofaeces production rate (umol s−1)
    real(8) :: J_EC  ! Mobilization flux (umolC s-1)
    real(8) :: J_ES  ! Somatic maintenance rate (umolC s-1)
    real(8) :: J_EJ  ! Maturity maintenance rate (umolC s-1)
    real(8) :: J_EG  ! Flux allocated to growth (umolC s-1)
!    real(8) :: J_VG  ! Growth rate (umolC s-1)
    real(8) :: J_ER  ! Flux allocated to reproduction/maturity (umolC s-1)
    real(8) :: JM_ER ! Flux to maturity (umolC s-1)
    real(8) :: JR_ER ! Flux to reproduction buffer (umolC s-1)
    real(8) :: Jspawn_ER ! Spawning (umolC s-1)
    real(8) :: k_J   ! Volume-specific maturity maintenance rate (s-1)
    real(8) :: GSR   ! Gonado-somatic ratio
    real(8) :: R     ! Respiration rate (umolC s-1)
    real(8) :: G, G0 ! Calcification rate (umolC s-1)
    real(8) :: Mort  ! Mortality rate (population dens. s-1)
    real(8) :: Ltmp  ! Shell length (cm) (temporal variable)

    real(8) :: rQCe(N_Csp),rQCv(N_Csp),rQCr(N_Csp),rQCh(N_Csp)
    real(8) :: rQNe(N_Nsp),rQNv(N_Nsp),rQNr(N_Nsp),rQNh(N_Nsp)
    real(8) :: rQPe(N_Psp),rQPv(N_Psp),rQPr(N_Psp),rQPh(N_Psp)
    real(8) :: rDIC (N_Csp) 
    real(8) :: rPhyC(N_Csp,Nphy)
    real(8) :: rPhyN(N_Nsp,Nphy)
    real(8) :: rPhyP(N_Psp,Nphy)
    real(8) :: rPIC (N_Csp,Npim) 
    real(8) :: Gp2b, Gp2b_p, dCp2b, dNp2b, dPp2b
    real(8) :: cDOx
    real(8) :: cPhyC(Nphy),cPhyN(Nphy),cPhyP(Nphy)

    real(8) :: dQCe_dt(N_Csp),dQCr_dt(N_Csp),dQCh_dt(N_Csp),dQCv_dt(N_Csp)
    real(8) :: dQNe_dt(N_Nsp),dQNr_dt(N_Nsp),dQNh_dt(N_Nsp),dQNv_dt(N_Nsp)
    real(8) :: dQPe_dt(N_Psp),dQPr_dt(N_Psp),dQPh_dt(N_Psp),dQPv_dt(N_Psp)
    real(8) :: dCaCO3_dt(N_Csp)

    integer :: isp, m

# if defined BIVALVE_TESTMODE
!  Output
    real(8), save :: dsec(Nbv) = 0.d0 !sec
# endif

!======================================================================================
!  Preparation
!    dDOx_dt       = 0.0d0
!    dTA_dt        = 0.0d0
!    dDIC_dt(:)    = 0.0d0
!    dNH4_dt(:)    = 0.0d0
!    dPO4_dt(:)    = 0.0d0
!    dDOC_dt(:,:)  = 0.0d0
!    dPOC_dt(:,:)  = 0.0d0
!    dDON_dt(:,:)  = 0.0d0
!    dPON_dt(:,:)  = 0.0d0
!    dDOP_dt(:,:)  = 0.0d0
!    dPOP_dt(:,:)  = 0.0d0
!    dPhyC_dt(:,:) = 0.0d0
!    dPhyN_dt(:,:) = 0.0d0
!    dPhyP_dt(:,:) = 0.0d0
!    dPIC_dt (:,:) = 0.0d0

    dQCe_dt(:) = 0.0d0
    dQCv_dt(:) = 0.0d0
    dQCr_dt(:) = 0.0d0
    dQCh_dt(:) = 0.0d0
    dQNe_dt(:) = 0.0d0
    dQNv_dt(:) = 0.0d0
    dQNr_dt(:) = 0.0d0
    dQNh_dt(:) = 0.0d0
    dQPe_dt(:) = 0.0d0
    dQPv_dt(:) = 0.0d0
    dQPr_dt(:) = 0.0d0
    dQPh_dt(:) = 0.0d0
    dCaCO3_dt(:) = 0.0d0

!--- Carbon isotope or tracer ratio (R13C = 13C/Ct) -----------------
    DO isp=1,N_Csp
      rDIC(isp) = Iso_ratio( DIC(isp), DIC(iCt) ) 
      rQCe(isp) = Iso_ratio( BVLV(ng)%QCe(isp,n,k,i,j), BVLV(ng)%QCe(iCt,n,k,i,j) ) ! BVLV(ng)%QC
      rQCv(isp) = Iso_ratio( BVLV(ng)%QCv(isp,n,k,i,j), BVLV(ng)%QCv(iCt,n,k,i,j) ) ! BVLV(ng)%QC
      rQCr(isp) = Iso_ratio( BVLV(ng)%QCr(isp,n,k,i,j), BVLV(ng)%QCr(iCt,n,k,i,j) ) ! BVLV(ng)%QC
      rQCh(isp) = Iso_ratio( BVLV(ng)%QCh(isp,n,k,i,j), BVLV(ng)%QCh(iCt,n,k,i,j) ) ! BVLV(ng)%QC
      DO m = 1,Npim
        rPIC(isp,m) = Iso_ratio( PIC(isp,m), PIC(iCt,m) ) 
      ENDDO   
      DO m=1,Nphy
        rPhyC(isp,m) = Iso_ratio( PhyC(isp,m), PhyC(iCt,m) ) ! PhyC
      END DO
      DO m = 1,Npim
        rPIC(isp,m) = Iso_ratio( PIC(isp,m), PIC(iCt,m) ) 
      ENDDO   
    END DO
!--- Nitrogen isotope or tracer ratio (R15N = 15N/Nt) -----------------
    DO isp=1,N_Nsp
      rQNe(isp) = Iso_ratio( BVLV(ng)%QNe(isp,n,k,i,j), BVLV(ng)%QNe(iNt,n,k,i,j) ) ! BVLV(ng)%QN
      rQNv(isp) = Iso_ratio( BVLV(ng)%QNv(isp,n,k,i,j), BVLV(ng)%QNv(iNt,n,k,i,j) ) ! BVLV(ng)%QN
      rQNr(isp) = Iso_ratio( BVLV(ng)%QNr(isp,n,k,i,j), BVLV(ng)%QNr(iNt,n,k,i,j) ) ! BVLV(ng)%QN
      rQNh(isp) = Iso_ratio( BVLV(ng)%QNh(isp,n,k,i,j), BVLV(ng)%QNh(iNt,n,k,i,j) ) ! BVLV(ng)%QN
      DO m=1,Nphy
        rPhyN(isp,m) = Iso_ratio( PhyN(isp,m), PhyN(iCt,m) ) ! PhyN
      END DO
    END DO
!--- Phosphrous tracer ratio (RP = P1/Pt) -----------------
    DO isp=1,N_Psp
      rQPe(isp) = Iso_ratio( BVLV(ng)%QPe(isp,n,k,i,j), BVLV(ng)%QPe(iPt,n,k,i,j) ) ! BVLV(ng)%QP
      rQPv(isp) = Iso_ratio( BVLV(ng)%QPv(isp,n,k,i,j), BVLV(ng)%QPv(iPt,n,k,i,j) ) ! BVLV(ng)%QP
      rQPr(isp) = Iso_ratio( BVLV(ng)%QPr(isp,n,k,i,j), BVLV(ng)%QPr(iPt,n,k,i,j) ) ! BVLV(ng)%QP
      rQPh(isp) = Iso_ratio( BVLV(ng)%QPh(isp,n,k,i,j), BVLV(ng)%QPh(iPt,n,k,i,j) ) ! BVLV(ng)%QP
      DO m=1,Nphy
        rPhyP(isp,m) = Iso_ratio( PhyP(isp,m), PhyP(iPt,m) ) ! PhyP
      END DO
    END DO


    cDOx = MAX(DOx,0.0d0) !!! Error handling
    DO m=1,Nphy
      cPhyC(m) = MAX(PhyC(iCt,m),0.0d0) !!! Error handling
      cPhyN(m) = MAX(PhyN(iNt,m),0.0d0) !!! Error handling
      cPhyP(m) = MAX(PhyP(iPt,m),0.0d0) !!! Error handling
    END DO

!======================================================================================

! === Somatic maintenance rate (umolC s-1) (Saraiva et al., 2012) =======
  CALL somatic_maintenance(                 &
    ! input parameters
      BVLV(ng)%V(n,k,i,j), p_M(n), mu_E(n)  &
    ! output parameters
    , J_ES)

! === Maturity maintenance rate (umolC s-1) (Saraiva et al., 2012) ====== 
    k_J = p_M(n) / E_G(n)  ! Volume-specific maturity maintenance rate (s-1)
                           ! s-1 = (J s-1 cm-3)/(J cm-3)
    CALL maturity_maintenance(                &
    ! input parameters
      BVLV(ng)%QCh(iCt,n,k,i,j), k_J          &
    ! output parameters
    , J_EJ)

! ===  Clearance rate (L s-1 ind.-1) =========================
    ! Temperature correction factor for respiration (no dimension)
    kT = tempcorr2(Tmp+273.15d0, Tref(n), T_A(n), T_Hresp(n), T_AH(n), T_L(n), T_AL(n))
    ! Clearance rate (L s-1)
    CALL clearance_rate(                                            &
    ! input parameters
      Nphy, cPhyC, BVLV(ng)%V(n,k,i,j), kT, C_Rmax(n), J_XiFmax(1:Nphy,n), c2e &
    ! output parameters
    , C_R, J_Rc)

! === Phtoplankton ingestion Rate (umolC s−1 ind.-1) =========================

    ! Temperature correction factor for ingestion (no dimension)
    kT = tempcorr2(Tmp+273.15d0, Tref(n), T_A(n), T_Hing(n), T_AH(n), T_L(n), T_AL(n))
    ! Ingestion rate (umol s−1)
    CALL ingestion(                                            &
    ! input parameters 
      Nphy, cPhyC, C_R, rho_XiI(1:Nphy,n), J_XiImax(1:Nphy,n)  &
    ! output parameters
    , J_XiF, J_XiI, J_PiF)

! === Assimilation rate (umolC s−1 ind.-1) =========================

!     === Mass balance ===
    DO m=1, Nphy

      Gp2b_p = J_XiI(m) ! umolC s−1 ind.-1

      ! This routine corrects the mismatch of C:N:P stoichiometry between Phy and bivalve
      !   when mass is transferred from Phy to bivalve.         
      Gp2b = MIN( Gp2b_p, Gp2b_p/rNCb(n), Gp2b_p/rPCb(n) )
      dCp2b = Gp2b_p         - Gp2b
      dNp2b = Gp2b_p*rNCp(m) - Gp2b*rNCb(n)
      dPp2b = Gp2b_p*rPCp(m) - Gp2b*rPCb(n)

!     === Mass balance ===
      dPhyC_dt(:,m)    = dPhyC_dt(:,m)    -(Gp2b_p  + J_PiF(m))                  *rPhyC(:,m)
      dQCe_dt(:)       = dQCe_dt(:)       + Gp2b* AE(m,n)*aC_grz(1:N_Csp)        *rPhyC(:,m)
      dPOC_dt(:,iLPOM) = dPOC_dt(:,iLPOM) +(Gp2b*(1.0d0-AE(m,n)*aC_grz(1:N_Csp))               &
                                            + dCp2b + J_PiF(m))                  *rPhyC(:,m)
      
      dPhyN_dt(:,m)    = dPhyN_dt(:,m)    -(Gp2b_p*rNCp(m) + J_PiF(m)*rNCp(m))           *rPhyN(:,m)
      dQNe_dt(:)       = dQNe_dt(:)       + Gp2b*rNCb(n)* AE(m,n)*aN_grz(1:N_Nsp)        *rPhyN(:,m)
      dPON_dt(:,iLPOM) = dPON_dt(:,iLPOM) +(Gp2b*rNCb(n)*(1.0d0-AE(m,n)*aN_grz(1:N_Nsp))             &
                                            + dNp2b        + J_PiF(m)*rNCp(m))           *rPhyN(:,m)

      dPhyP_dt(:,m)    = dPhyP_dt(:,m)    -(Gp2b_p*rPCp(m) + J_PiF(m)*rPCp(m)) *rPhyP(:,m)
      dQPe_dt(:)       = dQPe_dt(:)       + Gp2b*rPCb(n)* AE(m,n)              *rPhyP(:,m)
      dPOP_dt(:,iLPOM) = dPOP_dt(:,iLPOM) +(Gp2b*rPCb(n)*(1.0d0-AE(m,n))                   &
                                            + dPp2b        + J_PiF(m)*rPCp(m)) *rPhyP(:,m)

      dPIC_dt(:,iLive) = dPIC_dt(:,iLive) - (Gp2b_p*rCaCp(m)+J_PiF(m)*rCaCp(m)) *rPIC(:,iLive)
      dPIC_dt(:,iDead) = dPIC_dt(:,iDead) + (Gp2b_p*rCaCp(m)+J_PiF(m)*rCaCp(m)) *rPIC(:,iLive)

    END DO

! === Mobilization flux (umolC s-1) (Saraiva et al., 2012)
    CALL mobilization_flux(                          &
    ! input parameters
      BVLV(ng)%V(n,k,i,j), BVLV(ng)%QCe(iCt,n,k,i,j) &
    , J_ES, cond(n), kappa(n), E_G(n), mu_E(n)       &
    ! output parameters
    , J_EC)

!     === Mass balance ===
    dQCe_dt(:) = dQCe_dt(:) - J_EC         *rQCe(:)
    dQNe_dt(:) = dQNe_dt(:) - J_EC*rNCb(n) *rQNe(:)
    dQPe_dt(:) = dQPe_dt(:) - J_EC*rPCb(n) *rQPe(:)

! === Growth (umolC s-1) (Saraiva et al., 2012) =========================
    CALL growth(              &
    ! input parameters
      J_EC, J_ES, kappa(n)    &
    ! output parameters
    , J_EG )

!     === Mass balance ===
    dQCv_dt(:) = dQCv_dt(:) + J_EG         *rQCe(:)
    dQNv_dt(:) = dQNv_dt(:) + J_EG*rNCb(n) *rQNe(:)
    dQPv_dt(:) = dQPv_dt(:) + J_EG*rPCb(n) *rQPe(:)

! === Maturity and reproduction (umolC s-1) (Saraiva et al., 2012) =====================
  
    GSR = BVLV(ng)%QCr(iCt,n,k,i,j) /        &
               (  BVLV(ng)%QCv(iCt,n,k,i,j)  &
                + BVLV(ng)%QCe(iCt,n,k,i,j)  &
                + BVLV(ng)%QCr(iCt,n,k,i,j) )

    CALL maturity_reproduction(                                 &
    ! input parameters
      BVLV(ng)%QCh(iCt,n,k,i,j), J_EC, J_EJ, kappa(n), Mp_H(n)  &
    , BVLV(ng)%QCr(iCt,n,k,i,j), kappa_R(n), Rspawn(n)          &
    , GSR, GSRspawn(n), Tmp, Tspawn(n)                          &
    , date                                                      &
    ! output parameters
    , J_ER, JM_ER, JR_ER, Jspawn_ER)

!     === Mass balance ===
    dQCr_dt(:) = dQCr_dt(:) + JR_ER         *rQCe(:)
    dQNr_dt(:) = dQNr_dt(:) + JR_ER*rNCb(n) *rQNe(:)
    dQPr_dt(:) = dQPr_dt(:) + JR_ER*rPCb(n) *rQPe(:)

    dQCh_dt(:) = dQCh_dt(:) + JM_ER         *rQCe(:)
    dQNh_dt(:) = dQNh_dt(:) + JM_ER*rNCb(n) *rQNe(:)
    dQPh_dt(:) = dQPh_dt(:) + JM_ER*rPCb(n) *rQPe(:)

    dQCr_dt(:) = dQCr_dt(:) - Jspawn_ER         *rQCr(:)
    dQNr_dt(:) = dQNr_dt(:) - Jspawn_ER*rNCb(n) *rQNr(:)
    dQPr_dt(:) = dQPr_dt(:) - Jspawn_ER*rPCb(n) *rQPr(:)

! === Bivalve's respiration rate (ummolC s-1 ind.-1) =========================
    R = J_ES + J_EJ + JR_ER !+ (J_EG - J_VG)
    BVLV(ng)%R(n,k,i,j) = R

!     === Mass balance ===
    dDOx_dt    = dDOx_dt    - R
    dQCe_dt(:) = dQCe_dt(:) - R         *rQCe(:)
    dDIC_dt(:) = dDIC_dt(:) + R         *rQCe(:)

    dQNe_dt(:) = dQNe_dt(:) - R*rNCb(n) *rQNe(:)
    dNH4_dt(:) = dNH4_dt(:) + R*rNCb(n) *rQNe(:)
    dTA_dt     = dTA_dt     + R*rNCb(n)*1.0d0

    dQPe_dt(:) = dQPe_dt(:) - R*rPCb(n) *rQPe(:)
    dPO4_dt(:) = dPO4_dt(:) + R*rPCb(n) *rQPe(:)
    dTA_dt     = dTA_dt     - R*rPCb(n)*2.0d0

! === Time step calculations for bivalve internal parameters =========================
   ! Convert unit: umolC s-1 ind.-1 -> 1.0d-3 mmolC s-1 ind.-1
    BVLV(ng)%QCv(:,n,k,i,j) = BVLV(ng)%QCv(:,n,k,i,j) + dQCv_dt(:)*1.0d-3 * dt
    BVLV(ng)%QCe(:,n,k,i,j) = BVLV(ng)%QCe(:,n,k,i,j) + dQCe_dt(:)*1.0d-3 * dt
    BVLV(ng)%QCr(:,n,k,i,j) = BVLV(ng)%QCr(:,n,k,i,j) + dQCr_dt(:)*1.0d-3 * dt
    BVLV(ng)%QCh(:,n,k,i,j) = BVLV(ng)%QCh(:,n,k,i,j) + dQCh_dt(:)*1.0d-3 * dt
    BVLV(ng)%QNv(:,n,k,i,j) = BVLV(ng)%QNv(:,n,k,i,j) + dQNv_dt(:)*1.0d-3 * dt
    BVLV(ng)%QNe(:,n,k,i,j) = BVLV(ng)%QNe(:,n,k,i,j) + dQNe_dt(:)*1.0d-3 * dt
    BVLV(ng)%QNr(:,n,k,i,j) = BVLV(ng)%QNr(:,n,k,i,j) + dQNr_dt(:)*1.0d-3 * dt
    BVLV(ng)%QNh(:,n,k,i,j) = BVLV(ng)%QNh(:,n,k,i,j) + dQNh_dt(:)*1.0d-3 * dt
    BVLV(ng)%QPv(:,n,k,i,j) = BVLV(ng)%QPv(:,n,k,i,j) + dQPv_dt(:)*1.0d-3 * dt
    BVLV(ng)%QPe(:,n,k,i,j) = BVLV(ng)%QPe(:,n,k,i,j) + dQPe_dt(:)*1.0d-3 * dt
    BVLV(ng)%QPr(:,n,k,i,j) = BVLV(ng)%QPr(:,n,k,i,j) + dQPr_dt(:)*1.0d-3 * dt
    BVLV(ng)%QPh(:,n,k,i,j) = BVLV(ng)%QPh(:,n,k,i,j) + dQPh_dt(:)*1.0d-3 * dt
!--- Body volume V (cm3) -----------------
!    BVLV(ng)%V(n,k,i,j)  = BVLV(ng)%V(n,k,i,j) + ( J_EG / k_m(n) ) * dt  ! umolC s-1 -> cm3
    BVLV(ng)%V(n,k,i,j)  = BVLV(ng)%QCv(iCt,n,k,i,j) * C2Wd(n)/ Ww2Wd(n) * 1.0d-3  ! mmolC -> g(dw)  ! umolC -> cm3

!--- Total dry weight (g) (mmolC -> g(dw)) -----------------
    BVLV(ng)%Wd(n,k,i,j) = (  BVLV(ng)%QCe(iCt,n,k,i,j) + BVLV(ng)%QCv(iCt,n,k,i,j)    &
                               + BVLV(ng)%QCr(iCt,n,k,i,j) + BVLV(ng)%QCh(iCt,n,k,i,j) )  &
                              * C2Wd(n) * 1.0d-3  ! mmolC -> g(dw)  !!!!! Check the conversion factor !!!!!!
!--- Wet flesh (meat) weight (g) (Wd g(dw)-> Ww g(ww)) -----------------
    BVLV(ng)%Ww(n,k,i,j) = BVLV(ng)%Wd(n,k,i,j) / Ww2Wd(n)
!--- Flesh (meat) volume (g) (Ww (g) -> V (cm3)) -----------------
!    BVLV(ng)%V(n,k,i,j)  = BVLV(ng)%Ww(n,k,i,j) / 1.0d0 ! 1.0 g/cm3
!--- Shell length (cm) ( Ww g(dw) -> L (cm); Kobayashi et al., 1997) -----------------
!    BVLV(ng)%L(n,k,i,j)  = 4.42d0 * BVLV(ng)%Ww(n,k,i,j)**0.340d0
    Ltmp = V2L( BVLV(ng)%V(n,k,i,j), delta_M(n) )
    BVLV(ng)%L(n,k,i,j)  = max(Ltmp, BVLV(ng)%L(n,k,i,j)) ! Prevent decreasing shell length
!--- Total (shell + flesh) weight (g) (L (cm) -> Wt g(ww)) (Kobayashi et al., 1997)-----------------
    BVLV(ng)%Wt(n,k,i,j) = 0.105d0 * BVLV(ng)%L(n,k,i,j)**2.967d0 ! g(wet weight)

!--- Shell CaCO3 amount (mmolC ind.-1)-----------------
    !!!! Need update !!!!
!    G0 = BVLV(ng)%CaCO3(iCt,n,k,i,j) 
!    BVLV(ng)%CaCO3(iCt,n,k,i,j) = (BVLV(ng)%Wt(n,k,i,j)-BVLV(ng)%Ww(n,k,i,j)) * 10.0d0 ! = 1000.0d0 / 100.09d0 ! g -> mmol CaCO3

! === Calcification rate (mmolC ind.-1 s-1) =========================
    G = (  (BVLV(ng)%Wt(n,k,i,j)-BVLV(ng)%Ww(n,k,i,j)) * 10.0d0       &
          - BVLV(ng)%CaCO3(iCt,n,k,i,j)                         )/dt
    G = MAX( G, 0.0d0 )
    BVLV(ng)%G(n,k,i,j) = G

!     === Mass balance ===
    dCaCO3_dt(:) = dCaCO3_dt(:) + G*aC_calc(1:N_Csp)*rDIC(:)
    dDIC_dt(:)   = dDIC_dt(:)   - G*aC_calc(1:N_Csp)*rDIC(:)  ! calcification
    dTA_dt       = dTA_dt       - G*2.0d0                     ! calcification

! === Time step calculations for bivalve shell CaCO3 =========================
    BVLV(ng)%CaCO3(:,n,k,i,j) = BVLV(ng)%CaCO3(:,n,k,i,j) + dCaCO3_dt(:) * dt

!===== Unit adjustment ===========================================
 ! [umol L-1 s-1] = [umol indiv.-1 s-1] * [indiv. m-3] * 1.0d-3 [m3 L-1]
    dDOx_dt       = dDOx_dt       * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dTA_dt        = dTA_dt        * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dDIC_dt(:)    = dDIC_dt(:)    * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dNH4_dt(:)    = dNH4_dt(:)    * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dPO4_dt(:)    = dPO4_dt(:)    * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dDOC_dt(:,:)  = dDOC_dt(:,:)  * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dPOC_dt(:,:)  = dPOC_dt(:,:)  * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dPOC_dt(:,:)  = dPOC_dt(:,:)  * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dDON_dt(:,:)  = dDON_dt(:,:)  * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dPON_dt(:,:)  = dPON_dt(:,:)  * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dDOP_dt(:,:)  = dDOP_dt(:,:)  * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dPOP_dt(:,:)  = dPOP_dt(:,:)  * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dPhyC_dt(:,:) = dPhyC_dt(:,:) * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dPhyN_dt(:,:) = dPhyN_dt(:,:) * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dPhyP_dt(:,:) = dPhyP_dt(:,:) * BVLV(ng)%dens(n,k,i,j)*1.0d-3
    dPIC_dt (:,:) = dPIC_dt (:,:) * BVLV(ng)%dens(n,k,i,j)*1.0d-3

! === Mortality rate (population dens. s-1) =========================
    Mort = BM(n) * BVLV(ng)%dens(n,k,i,j) ! Mortality rate (population dens. s-1)

! === Time step calculations for bivalve population density =========================
    BVLV(ng)%dens(n,k,i,j) = BVLV(ng)%dens(n,k,i,j) - Mort * dt
! Detritus from dead bivalves are not considered in this model version

!------------------------------------------------------------------------
! Print section (for debug)

# if defined BIVALVE_TESTMODE

    IF( date*86400.0d0 >= dsec(n) ) THEN
      if(dsec(n) == 0.0d0) then
        dsec(n)=date*86400.0d0+BIVALVE_OUTPUT_INTERVAL*60.0d0
        ! Write lavels at the first step
        write(80+n,'(*(g0:,", "))')                      &
           'date', 'k'                                   &
         , 'dens','L','V','Ww','Wd','Wt'                 &
         , 'QCe','QCv','QCr','QCh'                       &
         , 'QNe','QNv','QNr','QNh'                       &
         , 'QPe','QPv','QPr','QPh'                       &
         , 'CaCO3','Dead_CaCO3'                          &
         , 'C_R', 'J_Rc'                                 &
         , 'J_X1F', 'J_X2F', 'J_X3F', 'J_X4F'            &
         , 'J_X1I', 'J_X2I', 'J_X3I', 'J_X4I'            &
         , 'J_P1F', 'J_P2F', 'J_P3F', 'J_P4F'            &
         , 'J_EC', 'J_ES', 'J_EJ', 'J_EG'                &
         , 'J_ER', 'JM_ER', 'JR_ER', 'Jspawn_ER'         &
         , 'GSR', 'G', 'R', 'Mort', 'kT'
      else
        dsec(n)=dsec(n)+BIVALVE_OUTPUT_INTERVAL*60.0d0
      endif
      
      write(80+n,'(*(g0:,", "))')            &
         date, k                             &
       , BVLV(ng)%dens(n,k,i,j)              &
       , BVLV(ng)%L (n,k,i,j)                &
       , BVLV(ng)%V (n,k,i,j)                &
       , BVLV(ng)%Ww(n,k,i,j)                &
       , BVLV(ng)%Wd(n,k,i,j)                &
       , BVLV(ng)%Wt(n,k,i,j)                &
       , BVLV(ng)%QCe(:,n,k,i,j)             &
       , BVLV(ng)%QCv(:,n,k,i,j)             &
       , BVLV(ng)%QCr(:,n,k,i,j)             &
       , BVLV(ng)%QCh(:,n,k,i,j)             &
       , BVLV(ng)%QNe(:,n,k,i,j)             &
       , BVLV(ng)%QNv(:,n,k,i,j)             &
       , BVLV(ng)%QNr(:,n,k,i,j)             &
       , BVLV(ng)%QNh(:,n,k,i,j)             &
       , BVLV(ng)%QPe(:,n,k,i,j)             &
       , BVLV(ng)%QPv(:,n,k,i,j)             &
       , BVLV(ng)%QPr(:,n,k,i,j)             &
       , BVLV(ng)%QPh(:,n,k,i,j)             &
       , BVLV(ng)%CaCO3(:,n,k,i,j)           &
       , BVLV(ng)%Dead_CaCO3(:,n,k,i,j)      &
       , C_R, J_Rc, J_XiF, J_XiI, J_PiF      &
       , J_EC, J_ES, J_EJ, J_EG              &
       , J_ER, JM_ER, JR_ER, Jspawn_ER       &
       , GSR, G, R, Mort, kT
      
    END IF
# endif
!-----------------------------------------------------------------------

    RETURN
  
  END SUBROUTINE bivalve
#endif
END MODULE mod_bivalve

