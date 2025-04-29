
!!!=== Copyright (c) 2025 Takashi NAKAMURA & Masa MUTO  =====

#include "cppdefs.h"


!!!** DECOMPOSITION MODULE USED FOR FOODWEB & SEDIMENT_ECOSYS *****************************************


MODULE mod_decomposition
#if defined FOODWEB || defined SEDIMENT_ECOSYS
  implicit none

CONTAINS

!!! **********************************************************************
!!!  Organic matter decomposition processes
!!! **********************************************************************

  SUBROUTINE decomposition       &
    ! input parameters
    ( Flag_SW        &   ! 0: Sediment column, 1: Water column
    , Tmp            &   ! Temperature (oC)
    , Sal            &   ! Salinity (PSU)
    , rhos_d         &   ! Dry soil density (g cm-3)
    , poro           &   ! Soil porosity (= Vwater/(Vwater+Vsed) (non-dimensional: 0.0-1.0)
    , O2             &   ! Dissolved oxygen (umol L-1)
    , TA             &   ! Total alkalinity (TA: umol kg-1)
    , DIC            &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
    , NO3            &   ! NO3 (umol L-1)
    , NH4            &   ! NH4 (umol L-1)
    , PO4            &   ! PO4 (umol L-1)
    , DOC            &   ! DOC(Ndom): Dissolved organic carbon (DOC: umol L-1)
    , POC            &   ! POC(Npom): Particulate organic carbon (POC: umol L-1 or nmol g(DW)-1 *solid)
    , DON            &   ! Dissolved organic nitrogen (DON: umol L-1)
    , PON            &   ! Particulate organic nitrogen (PON: umol L-1 or nmol g(DW)-1 *solid)
    , DOP            &   ! Dissolved organic phosporius (DOP: umol L-1)
    , POP            &   ! Particulate organic phosporius (POP: umol L-1 or nmol g(DW)-1 *solid)
    , PIC            &   ! CaCO3 (umolC L-1 or nmol g(DW)-1 *solid)
    , Mn2            &   ! Mn2+ (umol L-1) 
    , MnO2           &   ! MnO2 (nmol g-1) *solid
    , Fe2            &   ! Fe2+ (umol L-1) 
    , FeS            &   ! FeS (nmol g(DW)-1) *solid
    , FeS2           &   ! FeS2 (nmol g(DW)-1) *solid
    , FeOOH          &   ! FeOOH (nmol g(DW)-1) *solid
    , FeOOH_PO4      &   ! FeOOH=PO4 (nmol g(DW)-1) *solid
    , H2S            &   ! H2S (umol L-1) 
    , S0             &   ! S (nmol g(DW)-1) *solid
    , SO4            &   ! SO42- (umol L-1) 
    ! input and output parameters
    , R_O2           &   ! d[O2]/dt  (umol L-1 s-1) 
    , R_TA           &   ! dTA/dt  (umol L-1 s-1) 
    , R_DIC          &   ! dDIC/dt (umol L-1 s-1) 
    , R_NO3          &   ! d[NO3]/dt (umol L-1 s-1)
    , R_NH4          &   ! d[NH4]/dt (umol L-1 s-1)
    , R_PO4          &   ! d[PO4]/dt (umol L-1 s-1)
    , R_DOC          &   ! dDOC/dt (umol L-1 s-1) 
    , R_POC          &   ! dPOC/dt (umol L-1 s-1) or (nmol g(DW)-1) *solid
    , R_DON          &   ! dDON/dt (umol L-1 s-1)
    , R_PON          &   ! dPON/dt (umol L-1 s-1) or (nmol g(DW)-1) *solid
    , R_DOP          &   ! dDOP/dt (umol L-1 s-1)
    , R_POP          &   ! dPOP/dt (umol L-1 s-1) or (nmol g(DW)-1) *solid
    , R_PIC          &   ! d CaCO3/dt (umol L-1 s-1) or (nmol g(DW)-1) *solid
    , R_Mn2          &   ! d[Mn2+]/dt (umol L-1 s-1)
    , R_MnO2         &   ! d[MnO2]/dt (nmol g(DW)-1 s-1) *solid
    , R_Fe2          &   ! d[Fe2+]/dt (umol L-1 s-1)
    , R_FeS          &   ! d[FeS]/dt (nmol g(DW)-1 s-1) *solid
    , R_FeS2         &   ! d[FeS2]/dt (nmol g(DW)-1 s-1) *solid
    , R_FeOOH        &   ! d[FeOOH]/dt (nmol g(DW)-1 s-1) *solid
    , R_FeOOH_PO4    &   ! d[FeOOH=PO4]/dt (nmol g(DW)-1 s-1) *solid
    , R_H2S          &   ! d[H2S]/dt (umol L-1 s-1)
    , R_S0           &   ! d[S]/dt (nmol g(DW)-1 s-1) *solid
    , R_SO4          &   ! d[SO42-]/dt (umol L-1 s-1)
    ! output parameters
    , pH             &   ! pH (total scale)
    , Wcal           &   ! Calcite saturation state
    , Warg           &   ! Aragonite saturation state
    )

!-----------------------------------------------------------------------
    USE mod_reef_ecosys_param
    USE mod_geochem
    
    implicit none

! input parameters
    integer, intent(in) :: Flag_SW
    real(8), intent(in) :: Tmp
    real(8), intent(in) :: Sal
    real(8), intent(in) :: rhos_d   
    real(8), intent(in) :: poro   
    real(8), intent(in) :: O2 
    real(8), intent(in) :: TA 
    real(8), intent(in) :: DIC(N_Csp)
    real(8), intent(in) :: NO3(N_Nsp)
    real(8), intent(in) :: NH4(N_Nsp)
    real(8), intent(in) :: PO4(N_Psp)
    real(8), intent(in) :: DOC(N_Csp,Ndom)   
    real(8), intent(in) :: POC(N_Csp,Npom) 
    real(8), intent(in) :: DON(N_Nsp,Ndom)   
    real(8), intent(in) :: PON(N_Nsp,Npom) 
    real(8), intent(in) :: DOP(N_Psp,Ndom)   
    real(8), intent(in) :: POP(N_Psp,Npom) 
    real(8), intent(in) :: PIC(N_Csp)
    real(8), intent(in) :: Mn2 
    real(8), intent(in) :: MnO2
    real(8), intent(in) :: Fe2 
    real(8), intent(in) :: FeS (N_Ssp)
    real(8), intent(in) :: FeS2(N_Ssp)
    real(8), intent(in) :: FeOOH
    real(8), intent(in) :: FeOOH_PO4(N_Psp)
    real(8), intent(in) :: H2S(N_Ssp)
    real(8), intent(in) :: S0 (N_Ssp)
    real(8), intent(in) :: SO4(N_Ssp)
! input and output parameters
!    real(8), intent(inout) :: R_Tmp
!    real(8), intent(inout) :: R_Sal
    real(8), intent(inout) :: R_O2 
    real(8), intent(inout) :: R_TA 
    real(8), intent(inout) :: R_DIC(N_Csp)
    real(8), intent(inout) :: R_NO3(N_Nsp)
    real(8), intent(inout) :: R_NH4(N_Nsp)
    real(8), intent(inout) :: R_PO4(N_Psp)
    real(8), intent(inout) :: R_DOC(N_Csp,Ndom)   
    real(8), intent(inout) :: R_POC(N_Csp,Npom) 
    real(8), intent(inout) :: R_DON(N_Nsp,Ndom)   
    real(8), intent(inout) :: R_PON(N_Nsp,Npom) 
    real(8), intent(inout) :: R_DOP(N_Psp,Ndom)   
    real(8), intent(inout) :: R_POP(N_Psp,Npom) 
    real(8), intent(inout) :: R_PIC(N_Csp)
    real(8), intent(inout) :: R_Mn2 
    real(8), intent(inout) :: R_MnO2
    real(8), intent(inout) :: R_Fe2 
    real(8), intent(inout) :: R_FeS (N_Ssp)
    real(8), intent(inout) :: R_FeS2(N_Ssp)
    real(8), intent(inout) :: R_FeOOH
    real(8), intent(inout) :: R_FeOOH_PO4(N_Psp)
    real(8), intent(inout) :: R_H2S(N_Ssp)
    real(8), intent(inout) :: R_S0 (N_Ssp)
    real(8), intent(inout) :: R_SO4(N_Ssp)
! output parameters
    real(8), intent(out) :: pH
    real(8), intent(out) :: Wcal
    real(8), intent(out) :: Warg

! =======================================================================
!! -- Reaction parameters -----------------------------------------
!!!! ============================================================== !!!
    !! ORGANIC MATTER DECOMPOSITION
        ! POMf --> DOMf + DOMs
        ! POMs --> DOMf + DOMs
    !! PRIMARY REACTIONS  (CH2O :: Organic Matters(DOMf,s))
        ! R1:   O2 + CH2O --> CO2 + H2O
        ! R2:   4/5 NO3- + CH2O + 4/5 H+ --> 4/5 N2 + CO2 + 7/5 H2O
        ! R3:   2MnO2 + CH2O + 4H+ --> 2Mn2+ + CO2 + 3H2O
        ! R4:   4FeOOH + CH2O + 4H+ --> 4Fe2+ + CO2 + 7H2O
        ! R5:   1/2 SO42- + CH2O + H+ --> 1/2 H2S + CO2 + H2O
    !! SECONDARY REACTIONS
        ! R6:   NH4+ + 2O2 --> NO3- + H2O + 2H+
        ! R7:   FeOOH + PO43- --> FeOOH=PO43-
        ! R8:   2Fe2+ + MnO2 + 2H2O --> 2FeOOH + Mn2+ + 2H+
        ! R9:   2Mn2+ + O2 + 2H2O --> 2MnO2 + 4H+
        ! R10a: H2S + 2FeOOH=PO43- + 4H+ --> S0 + 2Fe2+ + 4H2O + 2PO43-
        ! R10b: H2S + 2FeOOH + 4H+ --> S0 + 2Fe2+ + 4H2O
        ! R11:  4Fe2+ + O2 + 6H2O --> 4FeOOH + 8H+
        ! R12:  H2S + MnO2 + 2H+ --> S0 + Mn2+ + 2H2O
        ! R13:  H2S + Fe2+ --> FeS + 2H+
        ! R14:  FeS + S0 --> FeS2
        ! R15:  SO42- + 3H2S + 4FeS + 2H+ --> 4FeS2 + 4H2O
        ! R16:  H2S + 2O2 --> SO42- + 2H+
        ! R17:  FeS + 2O2 --> Fe2+ + SO42-
        ! R18:  2FeS2 + 7O2 + 2H2O --> 2Fe2+ + 4SO42- + 4H+
        ! R19:  4S0 + 4H2O --> 3H2S + SO42- + 2H+
!!!! ============================================================== !!!
    !! ----------- Decomposition rate constant for organic matter ------------- !!
    !! DOM
    ! real(8),parameter :: kdomf = 1.0d-3   ! For  domf (s-1)      Kasih (2009), Irie (2018)

    ! real(8),parameter :: kdoms = 5.0d-9   ! For  doms (s-1)      Kasih (2009)
    ! real(8),parameter :: kdoms = 5.0d-8   ! For  doms (s-1)      Irie (2018)

    !! POM
    ! real(8),parameter :: kpomf = 2.5d-6   ! For  pomf (s-1)      Kasih (2009), Irie (2018)

    ! real(8),parameter :: kpoms = 1.2d-10   ! For  poms (s-1)     Kasih (2009)
    ! real(8),parameter :: kpoms = 6.0d-10   ! For  poms (s-1)     Irie (2018)

!                                    LDOM    RDOM
!    real(8),parameter :: kdom(2) = [ 1.0d-4, 5.0d-8 ]  ! For  dom (s-1)           
    real(8),parameter :: kdom(2) = [ 1.0d-6, 1.0d-10 ]  ! For  dom (s-1)           
!                                    LPOM    RPOM
!    real(8),parameter :: kpom(2) = [ 9.6d-6, 1.2d-8 ]  ! For  pom (s-1) Fossing (2004)
    real(8),parameter :: kpom(3) = [ 1.0d-6, 1.0d-11, 2.5d-8 ]  ! For  pom (s-1) Kasih (2009)

    !! Reaction rate constants
    ! real(8),parameter :: k6  = 2.5d-6  ! For R6  (uM-1 s-1)           Fossing(2004)
!    real(8),parameter :: k6  = 2.5d-7  ! For R6  (uM-1 s-1)           Berg(2003),Kasih2004
    real(8),parameter :: k6  = 1.0d-8  ! For R6  (uM-1 s-1) 
    real(8),parameter :: k7  = 5.0d-14 ! For R7  (s-1)                Kasih2009
    ! real(8),parameter :: k7  = 5.0d-11 ! For R7  (s-1)                Fossing2004
    ! real(8),parameter :: k8  = 7.5d-11 ! For R8  (uM-1 s-1)           Berg2003
    real(8),parameter :: k8  = 1.7d-9 ! For R8  (uM-1 s-1)           Kasih(2009)
    real(8),parameter :: k9  = 1.5d-5  ! For R9  (uM-1 s-1)           Fossing(2004)
    real(8),parameter :: k10 = 2.0d-7  ! For R10 (uM-1 s-1)           Kasih(2009)
    real(8),parameter :: k11 = 5.0d-4  ! For R11 (uM-1 s-1)           Fossing(2004)
    real(8),parameter :: k12 = 3.0d-9  ! For R12 (uM-1 s-1)           Fossing(2004)
    real(8),parameter :: k13 = 3.75d-5  ! For R13 (uM-1 s-1)           Kasih(2009)
    real(8),parameter :: k14 = 3.0d-10 ! For R14 (cm3 s-1 nmol-1)     Fossing(2004)
    real(8),parameter :: k15 = 7.5d-12 ! For R15 (s-1)                Kasih(2009)
    real(8),parameter :: k16 = 5.0d-5  ! For R16 (uM-1 s-1)           Fossing(2004)
    real(8),parameter :: k17 = 6.0d-7  ! For R17 (uM-1 s-1)           Fossing(2004)
    ! real(8),parameter :: k18 = 1.6d-8  ! For R18 (uM-1 s-1)           Fossing(2004)
    real(8),parameter :: k18 = 3.0d-10  ! For R18 (uM-1 s-1)           Berg2003, Kasih2009
    real(8),parameter :: k19 = 7.0d-7  ! For R19 (s-1)               Fossing(2004)
    !! non-degratable
    real(8),parameter :: k20 = 1.3d-9     !For R20 (MnO2_ded --> MnO2_undeg) (s-1)    Fossing2004
    real(8),parameter :: k21 = 9.0d-10    !For R21 (FeOOH_ded --> FeOOH_undeg) (s-1)  Fossing2004

    !! ------------------------------------------------------------------------- !!
    !! Temperature coefficient Q10
    real(8),parameter :: Q10_pri = 3.8d0   !  For primary reactions    Fossing (2004)
    real(8),parameter :: Q10_sec = 2.0d0   !  For secondary reactions  Fossing (2004)

    !! OM ratio
    real(8), parameter :: OMFratio = 0.6d0    ! (no dim.) OMf/OM ratio       Irie 0.57 / Kasih0.4 / Fossing 0.42
    real(8), parameter :: OMSratio = 0.4d0    ! (no dim.) OMs/OM ratio   
!    real(8), parameter :: OMNratio = 0.3d0    ! (no dim.) OMn/OM ratio       Irie 0.2 / Kasih0.2 / Fossing 0.08
   ! real(8), parameter :: 
 
    !! Inhibiting concentration of R19
    real(8),parameter :: H2Sstop = 10d0  ! a certain limit of [H2S] in R19 (uM) (fossing et al. 2004)

    !! Concentration limit
    real(8),parameter :: O2lim    = 20.0d0    !(umol l-1)     Fossing2004
    real(8),parameter :: NO3lim   = 5.0d0     !(umol l-1)     Fossing2004
    real(8),parameter :: MnO2lim  = 5.0d4     !(nmol g-1)     Fossing2004
    real(8),parameter :: FeOOHlim = 1.0d5     !(nmol g-1)     Fossing2004
    real(8),parameter :: SO4lim   = 5.0d0     !(umol L-1)

    !! pH dependency on sulfate reduction (a normal distribution is assumed)
    real(8),parameter :: pHopt    = 7.3d0    ! Optimun pH value for suflate reduction  Tuning !!!!!!!!!!!
    real(8),parameter :: pHstd    = 1.0d0    ! Standard deviation of pH dependency  Tuning !!!!!!!!!!!

! ======================================================================= 
   !! Decompostion rate from CPOM to:  LPOM    RPOM
    real(8),parameter :: kcpom(2) = [ 2.0d-8, 1.0d-8 ]  ! (s-1) Tuning !!!!!!!!!!!

! ======================================================================= 
   !! CaCO3 dissolution rate (Yamamoto et al 2012)
    real(8),parameter :: a_diss = 0.0015d0/100.0d0/3600.0d0 ! (s-1)
    real(8),parameter :: b_diss = 0.0056d0/100.0d0/3600.0d0 ! (s-1)

! ======================================================================= 
!! Reaction rate constants considering temperature dependency
    real(8) :: k6_Q10,  k7_Q10,  k8_Q10,  k9_Q10,  k10_Q10
    real(8) :: k11_Q10, k12_Q10, k13_Q10, k14_Q10, k15_Q10
    real(8) :: k16_Q10, k17_Q10, k18_Q10, k19_Q10, k20_Q10, k21_Q10 
    real(8) :: kdom_Q10(Ndom), kpom_Q10(Npom), kcpom_Q10(Npom-1)
!! Reaction Rate  -------------------------------------------------------------
    !! unit: (nmol cm-3 s-1)
    real(8) :: vdiss
    real(8) :: v0
    real(8) :: v1c, v2c, v3c, v4c, v5c
    real(8) :: v1n, v2n, v3n, v4n, v5n
    real(8) :: v1p, v2p, v3p, v4p, v5p
    real(8) :: v6,   v7,  v8,  v9,v10a,v10b
    real(8) :: v11, v12, v13, v14, v15
    real(8) :: v16, v17, v18, v19, v20, v21

    real(8) :: r1,  r2, r3, r4, r5
    real(8) :: r5pH
    
    real(8) :: cff, cff2, cff3
    real(8) :: cfd, cfs, cfdd, cfsd, cfss

!    real(8) :: rDIC(N_Csp)
    real(8) :: rNO3(N_Nsp)
    real(8) :: rNH4(N_Nsp)
    real(8) :: rPO4(N_Psp)
    real(8) :: rDOC(N_Csp,Ndom)  
    real(8) :: rPOC(N_Csp,Npom)
    real(8) :: rDON(N_Nsp,Ndom)  
    real(8) :: rPON(N_Nsp,Npom)
    real(8) :: rDOP(N_Psp,Ndom)  
    real(8) :: rPOP(N_Psp,Npom)
    real(8) :: rPIC(N_Csp)
    real(8) :: rFeS (N_Ssp)
    real(8) :: rFeS2(N_Ssp)
    real(8) :: rFeOOH_PO4(N_Psp)
    real(8) :: rH2S(N_Ssp)
    real(8) :: rS0 (N_Ssp)
    real(8) :: rSO4(N_Ssp)

    real(8) :: cO2       
    real(8) :: cNO3      
    real(8) :: cNH4      
    real(8) :: cPO4      
    real(8) :: cPIC
    real(8) :: cMn2      
    real(8) :: cMnO2     
    real(8) :: cFe2      
    real(8) :: cFeS      
    real(8) :: cFeS2     
    real(8) :: cFeOOH    
    real(8) :: cFeOOH_PO4
    real(8) :: cH2S      
    real(8) :: cS0       
    real(8) :: cSO4

    real(8) :: CO3, TK
    integer :: isp, m

! =======================================================================
!!! executable statement

    !! Isotope ratio calculation -- !!
    !  DO isp=1,N_Csp
    !    rDIC(isp)
    !  ENDDO
    DO isp=1,N_Nsp
      rNO3(isp) = Iso_ratio( NO3(isp), NO3(iNt) )
      rNH4(isp) = Iso_ratio( NH4(isp), NH4(iNt) )
    ENDDO
    DO isp=1,N_Psp
      rPO4(isp) = Iso_ratio( PO4(isp), PO4(iPt) )
    ENDDO
    DO m = 1,Ndom
      DO isp=1,N_Csp
        rDOC(isp,m) = Iso_ratio( DOC(isp,m), DOC(iCt,m) )  
      ENDDO
      DO isp=1,N_Nsp
        rDON(isp,m) = Iso_ratio( DON(isp,m), DON(iNt,m) )   
      ENDDO
      DO isp=1,N_Psp
        rDOP(isp,m) = Iso_ratio( DOP(isp,m), DOP(iPt,m) )
      ENDDO
    ENDDO   
    DO m = 1,Npom
      DO isp=1,N_Csp
        rPOC(isp,m) = Iso_ratio( POC(isp,m), POC(iCt,m) ) 
      ENDDO
      DO isp=1,N_Nsp
        rPON(isp,m) = Iso_ratio( PON(isp,m), PON(iNt,m) )   
      ENDDO
      DO isp=1,N_Psp
        rPOP(isp,m) = Iso_ratio( POP(isp,m), POP(iPt,m) )
      ENDDO
    ENDDO   
    DO isp=1,N_Psp
      rFeOOH_PO4(isp) = Iso_ratio( FeOOH_PO4(isp), FeOOH_PO4(iPt) )
    ENDDO
    DO isp=1,N_Ssp
      rFeS(isp) = Iso_ratio( FeS(isp), FeS(iSt) )
      rFeS2(isp)= Iso_ratio( FeS2(isp), FeS2(iSt) )
      rH2S(isp) = Iso_ratio( H2S(isp), H2S(iSt) )
      rS0 (isp) = Iso_ratio( S0(isp), S0(iSt) )
      rSO4(isp) = Iso_ratio( SO4(isp), SO4(iSt) )
    ENDDO
    DO isp=1,N_Csp
      rPIC(isp) = Iso_ratio( PIC(isp), PIC(iCt) )
    ENDDO

  !! Correct negative concentratios for safe calculation  
    cO2        = max(O2,0.0d0)
    cNO3       = max(NO3(iNt),0.0d0)
    cNH4       = max(NH4(iNt),0.0d0)
    cPO4       = max(PO4(iPt),0.0d0)
    cMn2       = max(Mn2,0.0d0)
    cMnO2      = max(MnO2,0.0d0)
    cFe2       = max(Fe2,0.0d0)
    cFeS       = max(FeS(iSt),0.0d0)
    cFeS2      = max(FeS2(iSt),0.0d0)
    cFeOOH     = max(FeOOH,0.0d0)
    cFeOOH_PO4 = max(FeOOH_PO4(iPt),0.0d0)
    cH2S       = max(H2S(iSt),0.0d0)
    cS0        = max(S0(iSt),0.0d0)
    cSO4       = max(SO4(iSt),0.0d0)
    cPIC       = max(PIC(iCt),0.0d0)

  !! ----------------------------------------------------------------------------------- !!

    IF(Flag_SW==0) THEN
      cfd  = poro
      cfs  = rhos_d*(1.0d0 - poro)
    ELSE
      cfd  = 1.0d0
      cfs  = 1.0d0
    END IF
    cfdd = cfd * cfd
    cfsd = cfs * cfd
    cfss = cfs * cfs

!!!! ==== Carbonate dissoution =========================================== !!!

    TK   = Tmp + 273.15d0
    pH   = pH_fromATCT( TA, DIC(iCt),TK, Sal )   ! pH
    CO3  = cCO3_fromCTpH( DIC(iCt), pH, TK, Sal )  ! CO32- concentration
    Wcal = Wcal_fromcCO3( CO3, TK, Sal )  ! Calcite saturation state
    Warg = Warg_fromcCO3( CO3, TK, Sal )  ! Calcite saturation state

    ! R:  CaCO3 --> Ca2+ + CO3(2-) 
    vdiss = cfs *(-a_diss * Warg + b_diss)* cPIC ! Yamamoto et al. (2012)
    Vdiss = max(vdiss,0.0d0)

!    if (Vdiss < 0.0d0) then
!      Vdiss=-1.1d-3*(Warg-1.d0)**1.63d0  ! Calcification rate (nmol cm-2 s-1)
!    endif

    R_PIC(:) = R_PIC(:) - vdiss *rPIC(:)
    R_DIC(:) = R_DIC(:) + vdiss *rPIC(:)
    R_TA     = R_TA     + vdiss*2.0d0

!!!! ==== Organic matter remineralization =========================================== !!!

  !! Reaction rate constants depending on temperature
    cff = Q10_pri**((Tmp-10d0)/10d0)
    DO m = 1,Ndom
      kdom_Q10(m) = kdom(m) * cff
    ENDDO
    DO m = 1,Npom
      kpom_Q10(m) = kpom(m) * cff
    ENDDO
    DO m = 1,Npom-1
      kcpom_Q10(m) = kcpom(m) * cff
    ENDDO

    cff = Q10_sec**((Tmp-10d0)/10d0)
    k6_Q10  = k6  * cff     ! (Fossing2004)
    k7_Q10  = k7  * cff     ! (Berg2003)      (s-1)
    k8_Q10  = k8  * cff     ! (G.A.Anggra2009)
    k9_Q10  = k9  * cff     ! (Fossing2004)
    k10_Q10 = k10 * cff     ! (G.A.Anggra2009)
    k11_Q10 = k11 * cff     ! (Fossing2004)
    k12_Q10 = k12 * cff     ! (Fossing2004)
    k13_Q10 = k13 * cff     ! (G.A.Anggra2009)
    k14_Q10 = k14 * cff     ! (Fossing2004)
    k15_Q10 = k15 * cff     ! (G.A.Anggra2009)(s-1)
    k16_Q10 = k16 * cff     ! (Fossing2004)
    k17_Q10 = k17 * cff     ! (Fossing2004)
    k18_Q10 = k18 * cff     ! (Fossing2004)
    k19_Q10 = k19 * cff     ! (Fossing2004)   (s-1)
    k20_Q10 = k20 * cff     ! (Fossing2004)   (s-1)
    k21_Q10 = k21 * cff     ! (Fossing2004)   (s-1)

  !! pH dependence on sulfate reduction (a normal distribution is assumed)
    r5pH = exp(-0.5d0*(pH-pHopt)**2/pHstd**2) !/sqrt(2.0d0*PI*pHstd**2)


  !! ----------------------------------------------------------------------------------- !!
    ! CPOM decomposition to LPOM and RPOM 
    DO m = 1,Npom-1
      v0 = kcpom_Q10(m) *cfs* POC(iCt,iCPOM)
      R_POC(:,iCPOM) = R_POC(:,iCPOM) - v0 *rPOC(:,iCPOM)
      R_POC(:,m)     = R_POC(:,m)     + v0 *rPOC(:,iCPOM)
      
      v0 = kcpom_Q10(m) *cfs* PON(iNt,iCPOM)
      R_PON(:,iCPOM) = R_PON(:,iCPOM) - v0 *rPON(:,iCPOM)
      R_PON(:,m)     = R_PON(:,m)     + v0 *rPON(:,iCPOM)

      v0 = kcpom_Q10(m) *cfs* POP(iPt,iCPOM)
      R_POP(:,iCPOM) = R_POP(:,iCPOM) - v0 *rPOP(:,iCPOM)
      R_POP(:,m)     = R_POP(:,m)     + v0 *rPOP(:,iCPOM)
    END DO

  !! ----------------------------------------------------------------------------------- !!
  !! PRIMARY REACTIONS

     ! R1:   O2 + CH2O --> CO2 + H2O
     ! R2:   4/5 NO3- + CH2O + 4/5 H+ --> 4/5 N2 + CO2 + 7/5 H2O
     ! R3:   2MnO2 + CH2O + 4H+ --> 2Mn2+ + CO2 + 3H2O
     ! R4:   4FeOOH + CH2O + 4H+ --> 4Fe2+ + CO2 + 7H2O
     ! R5:   1/2 SO42- + CH2O + H+ --> 1/2 H2S + CO2 + H2O

# if defined SEDECO_ORP
    !! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !!
    !! redox stages change depending on ORP input
    !! create condition of it here
    !! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !!
        !! ギブスエネルギーで反応が変化するようにするべき
# else
    !! Redox concentration limit in organic matter degradation (Berg et al.2003)
    if (O2 > O2lim) then            !! -- Redox of CH2O       (CH2O)  (umol L-1) -- !!   if O2 > O2lim
      r1 = 1.0d0
      r2 = 0.0d0
      r3 = 0.0d0
      r4 = 0.0d0
      r5 = 0.0d0
    else
      r1 = cO2/O2lim 
      if(cNO3 > NO3lim) then      !! -- Denitrification     (NO3)   (umol L-1) -- !!   if O2 < O2lim   &&   NO3 > NO3lim
        r2 = 1.0d0-r1
        r3 = 0.0d0
        r4 = 0.0d0
        r5 = 0.0d0       
      else
        r2 =(1.0d0-r1)*cNO3/NO3lim      
        if(cMnO2 > MnO2lim) then    !! -- Manganese reduction (MnO2)  (nmol g-1) -- !!   if O2 < O2lim   &&   NO3 < NO3lim   &&   Mn02 > MnO2lim
          r3 = 1.0d0-r1-r2
          r4 = 0.0d0
          r5 = 0.0d0       
        else 
          r3 =(1.0d0-r1-r2)*cMnO2/MnO2lim
          if(cFeOOH > FeOOHlim) then  !! -- Iron reduction      (FeOOH) (nmol g-1) -- !!   if O2 < O2lim   &&   NO3 < NO3lim   &&   Mn02 < MnO2lim   &&   FeOOH > FeOOHlim
            r4 = 1.0d0-r1-r2-r3
            r5 = 0.0d0       
          else
            r4 =(1.0d0-r1-r2-r3)*cFeOOH/FeOOHlim
            if(cSO4 > SO4lim) then      !! -- Sulfate reduction   (SO4)   (umol L-1) -- !!   if O2 < O2lim   &&   NO3 < NO3lim   &&   Mn02 < MnO2lim   &&   FeOOH < FeOOHlim   &&   SO4 > SO4lim
              r5 = 1.0d0-r1-r2-r3-r4
            else                                              !! limit OM decomposition if SO4 < SO4lim    -- !!   if O2 < O2lim   &&   NO3 < NO3lim   &&   Mn02 < MnO2lim   &&   FeOOH < FeOOHlim   &&   SO4 < SO4lim
              r5 =(1.0d0-r1-r2-r3-r4)*cSO4/SO4lim
            endif
          endif
        endif
      endif
    endif

# endif
    ! POM remineralization
    DO m = 1,Npom
      cff = kpom_Q10(m) * POC(iCt,m) *r5pH
      cff = max(cff,0.0d0) !! Error handring
      v1c = r1*cfsd * cff
      v2c = r2*cfsd * cff
      v3c = r3*cfss * cff
      v4c = r4*cfss * cff
      v5c = r5*cfsd * cff

      cff = kpom_Q10(m) * PON(iNt,m) *r5pH
      cff = max(cff,0.0d0) !! Error handring
      v1n = r1*cfsd * cff
      v2n = r2*cfsd * cff
      v3n = r3*cfss * cff
      v4n = r4*cfss * cff
      v5n = r5*cfsd * cff

      cff = kpom_Q10(m) * POP(iPt,m) *r5pH
      cff = max(cff,0.0d0) !! Error handring
      v1p = r1*cfsd * cff
      v2p = r2*cfsd * cff
      v3p = r3*cfss * cff
      v4p = r4*cfss * cff
      v5p = r5*cfsd * cff

      R_POC(:,m) = R_POC(:,m) - (v1c +v2c +v3c +v4c +v5c) *rPOC(:,m)
      R_DIC(:)   = R_DIC(:)   + (v1c +v2c +v3c +v4c +v5c) *rPOC(:,m)
    
      R_PON(:,m) = R_PON(:,m) - (v1n +v2n +v3n +v4n +v5n) *rPON(:,m)
      R_NH4(:)   = R_NH4(:)   + (v1n +v2n +v3n +v4n +v5n) *rPON(:,m)
      R_TA       = R_TA       + (v1n +v2n +v3n +v4n +v5n)*1.0d0

      R_POP(:,m) = R_POP(:,m) - (v1p +v2p +v3p +v4p +v5p) *rPOP(:,m)
      R_PO4(:)   = R_PO4(:)   + (v1p +v2p +v3p +v4p +v5p) *rPOP(:,m)
      R_TA       = R_TA       - (v1p +v2p +v3p +v4p +v5p)*2.0d0

      ! R1:   O2 + CH2O --> CO2 + H2O
      R_O2       = R_O2       - v1c

      ! R2:   4/5 NO3- + CH2O + 4/5 H+ --> 4/5 N2 + CO2 + 7/5 H2O
      R_NO3(:)   = R_NO3(:)   - v2c*0.8d0 *aN_denitr(1:N_Nsp)*rNO3(:)
      R_TA       = R_TA       + v2c*0.8d0

      ! R3:   2MnO2 + CH2O + 4H+ --> 2Mn2+ + CO2 + 3H2O
      R_MnO2     = R_MnO2     - v3c*2.0d0
      R_Mn2      = R_Mn2      + v3c*2.0d0
      R_TA       = R_TA       + v3c*4.0d0

      ! R4:   4FeOOH + CH2O + 4H+ --> 4Fe2+ + CO2 + 7H2O
      R_FeOOH    = R_FeOOH    - v4c*4.0d0
      R_Fe2      = R_Fe2      + v4c*4.0d0
      R_TA       = R_TA       + v4c*4.0d0

      ! R5:   1/2 SO42- + CH2O + H+ --> 1/2 H2S + CO2 + H2O
      R_SO4(:)   = R_SO4(:)   - v5c*0.5d0 *aS_SO4red(1:N_Ssp)*rSO4(:)
      R_H2S(:)   = R_H2S(:)   + v5c*0.5d0 *aS_SO4red(1:N_Ssp)*rSO4(:)
      R_TA       = R_TA       + v5c

    ENDDO

    ! DOM remineralization
    DO m = 1,Ndom
      cff = kdom_Q10(m) * DOC(iCt,m) *r5pH
      cff = max(cff,0.0d0) !! Error handring
      v1c = r1*cfdd * cff
      v2c = r2*cfdd * cff
      v3c = r3*cfsd * cff
      v4c = r4*cfsd * cff
      v5c = r5*cfdd * cff

      cff = kdom_Q10(m) * DON(iNt,m) *r5pH
      cff = max(cff,0.0d0) !! Error handring
      v1n = r1*cfdd * cff
      v2n = r2*cfdd * cff
      v3n = r3*cfsd * cff
      v4n = r4*cfsd * cff
      v5n = r5*cfdd * cff

      cff = kdom_Q10(m) * DOP(iPt,m) *r5pH
      cff = max(cff,0.0d0) !! Error handring
      v1p = r1*cfdd * cff
      v2p = r2*cfdd * cff
      v3p = r3*cfsd * cff
      v4p = r4*cfsd * cff
      v5p = r5*cfdd * cff

      R_DOC(:,m) = R_DOC(:,m) - (v1c +v2c +v3c +v4c +v5c) *rDOC(:,m)
      R_DIC(:)   = R_DIC(:)   + (v1c +v2c +v3c +v4c +v5c) *rDOC(:,m)
    
      R_DON(:,m) = R_DON(:,m) - (v1n +v2n +v3n +v4n +v5n) *rDON(:,m)
      R_NH4(:)   = R_NH4(:)   + (v1n +v2n +v3n +v4n +v5n) *rDON(:,m)
      R_TA       = R_TA       + (v1n +v2n +v3n +v4n +v5n)*1.0d0

      R_DOP(:,m) = R_DOP(:,m) - (v1p +v2p +v3p +v4p +v5p) *rDOP(:,m)
      R_PO4(:)   = R_PO4(:)   + (v1p +v2p +v3p +v4p +v5p) *rDOP(:,m)
      R_TA       = R_TA       - (v1p +v2p +v3p +v4p +v5p)*2.0d0

      ! R1:   O2 + CH2O --> CO2 + H2O
      R_O2       = R_O2       - v1c

      ! R2:   4/5 NO3- + CH2O + 4/5 H+ --> 4/5 N2 + CO2 + 7/5 H2O
      R_NO3(:)   = R_NO3(:)   - v2c*0.8d0 *aN_denitr(1:N_Nsp)*rNO3(:)
      R_TA       = R_TA       + v2c*0.8d0

      ! R3:   2MnO2 + CH2O + 4H+ --> 2Mn2+ + CO2 + 3H2O
      R_MnO2     = R_MnO2     - v3c*2.0d0
      R_Mn2      = R_Mn2      + v3c*2.0d0
      R_TA       = R_TA       + v3c*4.0d0

      ! R4:   4FeOOH + CH2O + 4H+ --> 4Fe2+ + CO2 + 7H2O
      R_FeOOH    = R_FeOOH    - v4c*4.0d0
      R_Fe2      = R_Fe2      + v4c*4.0d0
      R_TA       = R_TA       + v4c*4.0d0

      ! R5:   1/2 SO42- + CH2O + H+ --> 1/2 H2S + CO2 + H2O
      R_SO4(:)   = R_SO4(:)   - v5c*0.5d0 *aS_SO4red(1:N_Ssp)*rSO4(:)
      R_H2S(:)   = R_H2S(:)   + v5c*0.5d0 *aS_SO4red(1:N_Ssp)*rSO4(:)
      R_TA       = R_TA       + v5c

    ENDDO

  !! SECONDARY REACTIONS

    ! R6:   NH4+ + 2O2 --> NO3- + H2O + 2H+
    v6 = cfdd * k6_Q10*cNH4*cO2
    R_NH4(:) = R_NH4(:) - v6        *aN_nitr(1:N_Nsp)*rNH4(:)
    R_O2     = R_O2     - v6*2.0d0
    R_NO3(:) = R_NO3(:) + v6        *aN_nitr(1:N_Nsp)*rNH4(:)
    R_TA     = R_TA     - v6*2.0d0

    ! R7:   FeOOH + PO43- --> FeOOH=PO43-
    v7 = cfsd * k7_Q10*cFeOOH*cPO4
    R_FeOOH        = R_FeOOH        - v7
    R_PO4(:)       = R_PO4(:)       - v7 *rPO4(:)
    R_FeOOH_PO4(:) = R_FeOOH_PO4(:) + v7 *rPO4(:)

    ! R8:   2Fe2+ + MnO2 + 2H2O --> 2FeOOH + Mn2+ + 2H+
    v8 = cfsd * k8_Q10*cMnO2*cFe2
    R_Fe2   = R_Fe2   - v8*2.0d0
    R_MnO2  = R_MnO2  - v8
    R_FeOOH = R_FeOOH + v8*2.0d0
    R_Mn2   = R_Mn2   + v8
    R_TA    = R_TA    - v8*2.0d0

    ! R9:   2Mn2+ + O2 + 2H2O --> 2MnO2 + 4H+
    v9 = cfdd * k9_Q10 *cO2*cMn2
    R_Mn2  = R_Mn2  - v9*2.0d0
    R_O2   = R_O2   - v9
    R_MnO2 = R_MnO2 + v9*2.0d0
    R_TA   = R_TA   - v9*4.0d0

    ! R10a: H2S + 2FeOOH=PO43- + 4H+ --> S0 + 2Fe2+ + 4H2O + 2PO43-
    v10a = cfsd * k10_Q10 *cFeOOH_PO4*cH2S
    R_H2S(:)       = R_H2S(:)       - v10a       *rH2S(:)
    R_FeOOH_PO4(:) = R_FeOOH_PO4(:) - v10a*2.0d0 *rFeOOH_PO4(:)
    R_S0(:)        = R_S0(:)        + v10a       *rH2S(:)
    R_PO4(:)       = R_PO4(:)       + v10a*2.0d0 *rFeOOH_PO4(:)
    R_TA           = R_TA           + v10a*4.0d0

    ! R10b: H2S + 2FeOOH + 4H+ --> S0 + 2Fe2+ + 4H2O
    v10b = cfsd * k10_Q10 *cFeOOH*cH2S
    R_H2S(:) = R_H2S(:) - v10b       *rH2S(:)
    R_FeOOH  = R_FeOOH  - v10b*2.0d0
    R_S0(:)  = R_S0(:)  + v10b       *rH2S(:)
    R_Fe2    = R_Fe2    + v10b*2.0d0
    R_TA     = R_TA     + v10b*4.0d0

    ! R11:  4Fe2+ + O2 + 6H2O --> 4FeOOH + 8H+
    v11  = cfdd * k11_Q10 *cFe2*cO2
    R_Fe2   = R_Fe2   - v11*4.0d0
    R_O2    = R_O2    - v11
    R_FeOOH = R_FeOOH + v11*4.0d0
    R_TA    = R_TA    - v11*8.0d0

    ! R12:  H2S + MnO2 + 2H+ --> S0 + Mn2+ + 2H2O
    v12  = cfsd * k12_Q10 *cMnO2*cH2S
    R_H2S(:) = R_H2S(:) - v12       *rH2S(:)
    R_MnO2   = R_MnO2   - v12
    R_Mn2    = R_Mn2    + v12
    R_S0(:)  = R_S0(:)  + v12       *rH2S(:)
    R_TA     = R_TA     + v12*2.0d0

    ! R13:  H2S + Fe2+ --> FeS + 2H+
    v13  = cfdd * k13_Q10 *cFe2*cH2S
    R_H2S(:) = R_H2S(:) - v13       *rH2S(:)
    R_Fe2    = R_Fe2    - v13
    R_FeS(:) = R_FeS(:) + v13       *rH2S(:)
    R_TA     = R_TA     - v13*2.0d0

    ! R14:  FeS + S0 --> FeS2
    v14  = cfss * k14_Q10 *cFeS*cS0
    R_FeS(:) = R_FeS(:)  - v14 *rFeS(:)
    R_S0(:)  = R_S0(:)   - v14 *rS0(:)
    R_FeS2(:)= R_FeS2(:) + v14 *(rFeS(:) + rS0(:))/2.0d0 !!?????????????????????????????????????Check needed!!!

    ! R15:  SO42- + 3H2S + 4FeS + 2H+ --> 4FeS2 + 4H2O
    v15  = cfsd * k15_Q10 *cFeS*cH2S
    R_SO4(:)  = R_SO4(:)  - v15       *rSO4(:)
    R_H2S(:)  = R_H2S(:)  - v15*3.0d0 *rH2S(:)
    R_FeS(:)  = R_FeS(:)  - v15*4.0d0 *rFeS(:)
    R_FeS2(:) = R_FeS2(:) + v15*4.0d0 *(rSO4(:)+rH2S(:)*3.0d0+rFeS(:)*4.0d0)/8.0d0 !!?????????????????????????????????????Check needed!!!
    R_TA      = R_TA      + v15*2.0d0

    ! R16:  H2S + 2O2 --> SO42- + 2H+
    v16  = cfdd * k16_Q10 *cH2S*cO2
    R_H2S(:) = R_H2S(:) - v16       *rH2S(:)
    R_O2     = R_O2     - v16*2.0d0 
    R_SO4(:) = R_SO4(:) + v16       *rH2S(:)
    R_TA     = R_TA     - v16*2.0d0

    ! R17:  FeS + 2O2 --> Fe2+ + SO42-
    v17  = cfsd * k17_Q10 * cFeS* cO2
    R_FeS(:) = R_FeS(:) - v17       *rFeS(:)
    R_O2     = R_O2     - v17*2.0d0
    R_Fe2    = R_Fe2    + v17
    R_SO4(:) = R_SO4(:) + v17       *rFeS(:)

    ! R18:  2FeS2 + 7O2 + 2H2O --> 2Fe2+ + 4SO42- + 4H+
    v18  = cfsd * k18_Q10 *cFeS2*cO2
    R_FeS2(:) = R_FeS2(:) - v18*2.0d0 *rFeS2(:)
    R_O2      = R_O2      - v18*7.0d0
    R_Fe2     = R_Fe2     + v18*2.0d0
    R_SO4(:)  = R_SO4(:)  + v18*4.0d0 *rFeS2(:)
    R_TA      = R_TA      - v18*4.0d0

    ! R19:  4S0 + 4H2O --> 3H2S + SO42- + 2H+
    if (H2S(iSt) <= H2Sstop) then  
      v19  = cfsd * k19_Q10 *cS0*(1d0 - (cH2S/H2Sstop))  !!??????????????????????
    else
      v19  = 0.0d0
    endif
    R_S0(:)  = R_S0(:)  - v19*3.0d0 *aS_S02H2S(1:N_Ssp)*rS0(:)  &
                        - v19*1.0d0 *aS_S02SO4(1:N_Ssp)*rS0(:)
    R_H2S(:) = R_H2S(:) + v19*3.0d0 *aS_S02H2S(1:N_Ssp)*rS0(:)
    R_SO4(:) = R_SO4(:) + v19*1.0d0 *aS_S02SO4(1:N_Ssp)*rS0(:)
    R_TA     = R_TA     - v19*2.0d0

    ! R20:  MnO2 --> MnO2_undeg
    v20  = cfsd * k20_Q10 *cMnO2
    R_MnO2 = R_MnO2 - v20 
    ! R21:  FeOOH --> FeOOH_undeg
    v21  = cfsd * k21_Q10 *cFeOOH
    R_FeOOH = R_FeOOH - v21


    RETURN
  
  END SUBROUTINE decomposition
#endif
END MODULE mod_decomposition

