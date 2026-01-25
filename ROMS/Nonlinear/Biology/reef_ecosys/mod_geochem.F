
!!!=== Copyright (c) 2012-2026 Takashi NAKAMURA  =====

#include "cppdefs.h"

module mod_geochem

  implicit none

!----- Carbon isotope fractionation ------------------------
#if defined CARBON_TRACE
  real(8), parameter :: aC_phot(3) = [ 1.0d0, 1.0d0, 1.0d0 ]
!  real(8), parameter :: aC_resp(3) = [ 1.0d0, 1.0d0, 1.0d0 ]
  real(8), parameter :: aC_calc(3) = [ 1.0d0, 1.0d0, 1.0d0 ] 
  real(8), parameter :: aC_co2 (3) = [ 1.0d0, 1.0d0, 1.0d0 ]
  real(8), parameter :: aC_grz (3) = [ 1.0d0, 1.0d0, 1.0d0 ]
#else
                                     ! iCt      iC13             iD47 (clumped isotope)              
  real(8), parameter :: aC_phot(3) = [ 1.0d0, -20.0d-3 +1.0d0, -20.0d-3 +1.0d0 ] !!!!! To Be Confirmed
!  real(8), parameter :: aC_resp(3) = [ 1.0d0,   0.0d-3 +1.0d0,   0.0d-3 +1.0d0 ]
  real(8), parameter :: aC_calc(3) = [ 1.0d0,   2.7d-3 +1.0d0,   2.7d-3 +1.0d0 ] 
  real(8), parameter :: aC_co2 (3) = [ 1.0d0,  -7.0d-3 +1.0d0,  -7.0d-3 +1.0d0 ] !!!!! To Be Confirmed. Maybe ~-8-4.4
  real(8), parameter :: aC_grz (3) = [ 1.0d0,   0.8d-3 +1.0d0,           1.0d0 ]
#endif

!----- Nitrogen isotope fractionation ------------------------
#if defined NITROGEN_TRACE
  real(8), parameter :: aNO3_phot(2) = [ 1.0d0, 1.0d0 ]
  real(8), parameter :: aNH4_phot(2) = [ 1.0d0, 1.0d0 ]
  real(8), parameter :: aN_grz   (2) = [ 1.0d0, 1.0d0 ]
  real(8), parameter :: aN_N2fix (2) = [ 1.0d0, 1.0d0 ]
  real(8), parameter :: aN_nitr  (2) = [ 1.0d0, 1.0d0 ]
  real(8), parameter :: aN_denitr(2) = [ 1.0d0, 1.0d0 ]
#else
                                        ! iNt         iN15         
  real(8), parameter :: aNO3_phot(2) = [ 1.0d0, -10.0d-3 +1.0d0 ] ! NO3 assimilation by phytoplankton !!!! To Be Confirmed
  real(8), parameter :: aNH4_phot(2) = [ 1.0d0,  -5.0d-3 +1.0d0 ] ! NH4 assimilation by phytoplankton !!!! To Be Confirmed
  real(8), parameter :: aN_grz   (2) = [ 1.0d0,   3.4d-3 +1.0d0 ] ! condensation
  real(8), parameter :: aN_N2fix (2) = [ 1.0d0,   1.0d-3 +1.0d0 ] ! nitrogen fixation (N2 -> NH4) !!!! To be confirmed
  real(8), parameter :: aN_nitr  (2) = [ 1.0d0, -35.0d-3 +1.0d0 ] ! nitrification (NH4 -> NO3) ! To be confirmed
  real(8), parameter :: aN_denitr(2) = [ 1.0d0, -20.0d-3 +1.0d0 ] ! denitrification (NO3 -> N2) ! To be confirmed
#endif

!----- Sulfur isotope fractionation ------------------------
#if defined SULFUR_TRACE
  real(8), parameter :: aS_SO4red(2) = [ 1.0d0, 1.0d0 ]
  real(8), parameter :: aS_S02SO4(2) = [ 1.0d0, 1.0d0 ]
  real(8), parameter :: aS_S02H2S(2) = [ 1.0d0, 1.0d0 ]
#else
                                         ! iSt      iS34         
  real(8), parameter :: aS_SO4red(2) = [ 1.0d0, -25.0d-3 +1.0d0 ] ! Sulfate reduction (SO42- -> H2S)             ( 25 permill; Dale et al., 2009)
  real(8), parameter :: aS_S02SO4(2) = [ 1.0d0,  20.0d-3 +1.0d0 ] ! S0 disproportionation to SO42- (S0 -> SO42-) (-20 permill; Dale et al., 2009)
  real(8), parameter :: aS_S02H2S(2) = [ 1.0d0,  -7.0d-3 +1.0d0 ] ! S0 disproportionation to H2S   (S0 -> H2S)   (  7 permill; Dale et al., 2009)
#endif

! ------ Carbon Isotope related parameters -----
  real(8), parameter :: R13C_VPDB = 0.011056d0/0.988944d0
! ------ Nitrogen Isotope related parameters -----
  real(8), parameter :: R15N_AIR = 0.003663d0/0.996337d0
! ------ Sulfur Isotope related parameters -----
  real(8), parameter :: R34S_VCDT = 0.00419719d0/0.9503957d0
! ------ Oxygen Isotope related parameters -----
  real(8), parameter :: R18O_VSMOW = 0.0020004d0/0.9976206d0
! ------ Boron Isotope related parameters -----
  real(8), parameter :: R11B_SRM951 = 80.173d0/19.827d0  !NIST SRM 951 boric acid standard

! ------ Molecular diffusion coefficients in seawater @ 25 degC, 35 psu -----
!!!*** Checke and update by following Wolf-Gladrow and Riebesell (1996) ****
  real(8), parameter :: dif_O2   = 2.2d-9    ! Oxygen                (m2 s-1)
  real(8), parameter :: dif_CO2  = 1.0d-9    ! Carbon dioxide CO2aq  (m2 s-1)
  real(8), parameter :: dif_HCO3 = 1.18d-9   ! Bicarbonate ion HCO3- (m2 s-1)
  real(8), parameter :: dif_CO3  = 0.955d-9  ! Carbonate ion CO32-   (m2 s-1)
  real(8), parameter :: dif_NH4  = 2.0d-9    ! Ammonium ion NH4+     (m2 s-1)
  real(8), parameter :: dif_NO2  = 1.9d-9    ! Nitrite ion NO2-      (m2 s-1)
  real(8), parameter :: dif_NO3  = 1.9d-9    ! Nitrate ion NO3-      (m2 s-1)
  real(8), parameter :: dif_PO4  = 0.85d-9   ! H2PO4-                (m2 s-1)
  real(8), parameter :: dif_H    = 9.31d-9   ! Proton H+             (m2 s-1)
  real(8), parameter :: dif_OH   = 5.27d-9   ! OH-                   (m2 s-1)
! ----------------------------------------------------------------------------
  real(8), parameter :: dif_H2O2 = 1.19d-9   ! H2O2                  (m2 s-1)
! ------ kinematic viscosity of seawater @ 25 degC, 35 psu -----
  real(8), parameter :: vis_sw   = 0.94d-6   ! kinematic viscosity   (m2 s-1)

  real(8), parameter :: grav    = 9.80665d0 ! Gravitational acceleration [m/s2]

  real(8), parameter :: PI  = 3.14159265359d0
  real(8), parameter :: Rg  = 8.31d0    ! Gass constant (J K–1 mol–1)

contains
!!!***********************************************************************
!!!   Physical parameters of seawater
!!!*********************************************************************** 

! Density of seawater (g/cm^3) or (kg/l)
  real(8) function densSW(temp0, S0)
  !$acc routine seq
    real(8), intent(in) :: temp0    ! Temperature (oC)
    real(8), intent(in) :: S0       ! Salinity (psu)
    real(8) :: dSMOW               ! Density of SMOW (kg/m^3)
    real(8) :: temp, S
    temp = max( temp0, 0.0d0 )  !!! Error handling
    S    = max( S0   , 0.0d0 )  !!! Error handling

    dSMOW = 999.842594d0 + 6.793952d-2*temp                         &
      - 9.09529d-3*temp**2.0d0 + 1.001685d-4*temp**3.0d0            &
      - 1.120083d-6*temp**4.0d0 + 6.536332d-9*temp**5.0d0

    densSW = dSMOW                                                  &
      + S*(0.824493d0 - 4.0899d-3*temp + 7.6438d-5*temp**2.0d0      &
           - 8.2467d-7*temp**3.0d0 + 5.3875d-9*temp**4.0d0)         &
      + S**1.5d0 * (-5.72466d-3 + 1.0227d-4*temp                    &
           - 1.6546d-6*temp**2.0d0)                                 &
      + S**2.0d0 * 4.8314d-4

    densSW = densSW*1.0d-3  !kg/m3 -> g/cm3
    return
  end function densSW

!-----------------------------------------------------------------------

!  Density of water (g/cm^3) or (kg/l)
  real(8) function densW(temp0)
  !$acc routine seq
    real(8), intent(in) :: temp0    ! Temperature (oC)
    real(8) :: temp
    temp = max( temp0, 0.0d0 )  !!! Error handling

    densW = 999.84847d0 + 6.337563d-2*temp - 8.523829d-3*temp**2.0d0 &
      + 6.943248d-5*temp**3.0d0 - 3.821216d-7*temp**4.0d0
   
    densW = densW*1.0d-3  !kg/m3 -> g/cm3
    return
  end function densW

!!!***********************************************************************
!!!   CO2system functions
!!!*********************************************************************** 

  real(8)  function pH_fromATCT(AT2, CT2, T, S ) ! pH (total scale)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: AT2 ! Total alkalinity (umol kg-1)
    real(8), intent(in) :: CT2 ! Total dissolved inorganic carbon (umol kg-1)
    real(8), intent(in) :: T   ! Temperature (K)
    real(8), intent(in) :: S   ! Salinity (psu)

    real(8) AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF
    real(8) cH ![H+](TOT) Total hydrogen ion concentration scale
    real(8) cH0, cH1
    real(8) q, q0, q1
    real(8) p, p0, p1

    AT=AT2*1.d-6
    CT=CT2*1.d-6

    BT=BT_Uppstrom1974(S)
    ST=ST_MorrisRiley1966(S)
    FT=FT_Weiss1974(S)
    K1=K1_Lueker2000(T, S)
    K2=K2_Lueker2000(T, S)
    Kw=Kw_Millero1995(T, S)
    KB=KB_Dickson1990(T, S)
    KS=KS_Dickson1990(T, S)
    KF=KF_DicksonRiley1979(T, S)

    cH0=10.d0**(-8.d0)

    !cH=Newton_Raphson(cH0,cH1,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF)
    cH=Newton_Raphson2(cH0,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF)

    pH_fromATCT = -Log10(cH)

    return
  end function pH_fromATCT

!-----------------------------------------------------------------------

  real(8)  function Ac_fromATpH(AT, pH, T, S) ! Carbonate alkalinity (umol kg-1)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: AT  ! Total alkalinity (umol kg-1)
    real(8), intent(in) :: pH  ! pH (total scale)
    real(8), intent(in) :: T   ! Temperature (K)
    real(8), intent(in) :: S   ! Salinity (psu)

    real(8)  BT,ST,FT,Kw,KB,KS,KF
    real(8)  cBOH4,cOH,cHF,cHSO4,cHfree,cH

    BT = BT_Uppstrom1974(S)
    ST = ST_MorrisRiley1966(S)
    FT = FT_Weiss1974(S)
    Kw = Kw_Millero1995(T, S)
    KB = KB_Dickson1990(T, S)
    KS = KS_Dickson1990(T, S)
    KF = KF_DicksonRiley1979(T, S)

    cH = 10.d0**(-pH)
    cBOH4 = BT/(1.+cH/KB)
    cOH = Kw/cH
    cHfree = cH/(1.+ST/KS)
    cHSO4 = ST/(1.+KS/cHfree)
    cHF = FT/(1.+KF/cH)

    Ac_fromATpH = (AT*1.d-6 -cBOH4-cOH+cHfree+cHSO4+cHF)*1.d6

    return
  end function Ac_fromATpH

!---------------------------------------------------------------------

  real(8)  function cCO2aq_fromATpH(AT, pH, T, S) ! [CO2*] (umol kg-1)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: AT  ! Total alkalinity (umol kg-1)
    real(8), intent(in) :: pH  ! pH (total scale)
    real(8), intent(in) :: T   ! Temperature (K)
    real(8), intent(in) :: S   ! Salinity (psu)
    
    real(8)  cH,K1, K2

    K1 = K1_Lueker2000(T, S)
    K2 = K2_Lueker2000(T, S)

    cH = 10.d0**(-pH)

    cCO2aq_fromATpH = Ac_fromATpH(AT,pH,T,S)*cH**2.d0/K1/(cH+2.d0*K2)

    return
  end function cCO2aq_fromATpH

!---------------------------------------------------------------------

  real(8)  function cHCO3_fromATpH(AT, pH, T, S) ! [HCO3-] (umol kg-1)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: AT  ! Total alkalinity (umol kg-1)
    real(8), intent(in) :: pH  ! pH (total scale)
    real(8), intent(in) :: T   ! Temperature (K)
    real(8), intent(in) :: S   ! Salinity (psu)
    
    real(8)  cH,K1, K2

    K1 = K1_Lueker2000(T, S)
    K2 = K2_Lueker2000(T, S)

    cH = 10.d0**(-pH)

    cHCO3_fromATpH = Ac_fromATpH(AT,pH,T,S)*cH/(cH+2.*K2)

    return
  end function cHCO3_fromATpH

!---------------------------------------------------------------------

  real(8)  function cCO3_fromATpH(AT, pH, T, S) ! [CO3^2-] (umol kg-1)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: AT  ! Total alkalinity (umol kg-1)
    real(8), intent(in) :: pH  ! pH (total scale)
    real(8), intent(in) :: T   ! Temperature (K)
    real(8), intent(in) :: S   ! Salinity (psu)

    real(8)  cH,K1, K2

    K1 = K1_Lueker2000(T, S)
    K2 = K2_Lueker2000(T, S)

    cH = 10.d0**(-pH)

    cCO3_fromATpH = Ac_fromATpH(AT,pH,T,S)*K2/(cH+2.d0*K2)

    return
  end function cCO3_fromATpH

!---------------------------------------------------------------------

  real(8)  function fCO2_fromATpH(AT, pH, T, S) ! fugacity of CO2 (fCO2) (uatm)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: AT  ! Total alkalinity (umol kg-1)
    real(8), intent(in) :: pH  ! pH (total scale)
    real(8), intent(in) :: T   ! Temperature (K)
    real(8), intent(in) :: S   ! Salinity (psu)

    real(8)  cCO2,K0

    cCO2 = cCO2aq_fromATpH(AT, pH, T, S)
    K0 = K0_Weiss1974(T, S)

    fCO2_fromATpH = cCO2/K0

    return
  end function fCO2_fromATpH

!---------------------------------------------------------------------

  real(8)  function fCO2_fromcCO2aq(cCO2aq, T, S) ! fugacity of CO2 (fCO2) (uatm)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: cCO2aq ! [CO2*] (umol kg-1)
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)

    real(8)  K0

    !cCO2 = cCO2aq_fromATpH(AT, pH, T, S)
    K0 = K0_Weiss1974(T, S)

    fCO2_fromcCO2aq = cCO2aq/K0

    return
  end function fCO2_fromcCO2aq

!---------------------------------------------------------------------

  real(8)  function AT_fromCTpH(CT, pH, T, S) ! Total alkalinity (umol kg-1)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: CT  ! Total dissolved inorganic carbon (umol kg-1)
    real(8), intent(in) :: pH  ! pH (total scale)
    real(8), intent(in) :: T   ! Temperature (K)
    real(8), intent(in) :: S   ! Salinity (psu)

    real(8)  K1, K2
    real(8)  BT,ST,FT,Kw,KB,KS,KF
    real(8)  cHCO3,cCO3
    real(8)  cBOH4,cOH,cHF,cHSO4,cHfree,cH
    

    K1 = K1_Lueker2000(T, S)
    K2 = K2_Lueker2000(T, S)

    BT = BT_Uppstrom1974(S)
    ST = ST_MorrisRiley1966(S)
    FT = FT_Weiss1974(S)
    Kw = Kw_Millero1995(T, S)
    KB = KB_Dickson1990(T, S)
    KS = KS_Dickson1990(T, S)
    KF = KF_DicksonRiley1979(T, S)

    cH = 10.d0**(-pH)
    
    cHCO3=CT*1.d-6 * K1*cH/(cH**2.+K1*cH+K1*K2)
    cCO3 =CT*1.d-6 * K1*K2/(cH**2.+K1*cH+K1*K2)
    
    cBOH4 = BT/(1.+cH/KB)
    cOH = Kw/cH
    cHfree = cH/(1.+ST/KS)
    cHSO4 = ST/(1.+KS/cHfree)
    cHF = FT/(1.+KF/cH)

    AT_fromCTpH = (cHCO3+2.*cCO3+cBOH4+cOH        &
   &               -cHfree-cHSO4-cHF              &
   &                )*1.d6  !mol kg-1 -> umol kg-1

    return
  end function AT_fromCTpH

!---------------------------------------------------------------------

  real(8)  function cCO2aq_fromCTpH(CT, pH, T, S) ! [CO2*] (umol kg-1)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: CT  ! Total dissolved inorganic carbon (umol kg-1)
    real(8), intent(in) :: pH  ! pH (total scale)
    real(8), intent(in) :: T   ! Temperature (K)
    real(8), intent(in) :: S   ! Salinity (psu)

    real(8)  K1, K2
    real(8)  cH,cHCO3,cCO3
    

    K1 = K1_Lueker2000(T, S)
    K2 = K2_Lueker2000(T, S)

    cH = 10.d0**(-pH)
    cHCO3=CT*1.d-6 * K1*cH/(cH**2.+K1*cH+K1*K2)

    cCO2aq_fromCTpH = cH*cHCO3/K1 *1.d6 !umol kg-1
    
    return
  end function cCO2aq_fromCTpH

!---------------------------------------------------------------------

  real(8)  function cHCO3_fromCTpH(CT, pH, T, S) ! [HCO3-] (umol kg-1)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: CT  ! Total dissolved inorganic carbon (umol kg-1)
    real(8), intent(in) :: pH  ! pH (total scale)
    real(8), intent(in) :: T   ! Temperature (K)
    real(8), intent(in) :: S   ! Salinity (psu)

    real(8)  K1, K2
    real(8)  cH

    K1 = K1_Lueker2000(T, S)
    K2 = K2_Lueker2000(T, S)

    cH = 10.d0**(-pH)

    cHCO3_fromCTpH = CT* K1*cH/(cH**2.+K1*cH+K1*K2)  !umol kg-1
    
    return
  end function cHCO3_fromCTpH

!---------------------------------------------------------------------

  real(8)  function cCO3_fromCTpH(CT, pH, T, S) ! [CO3^2-] (umol kg-1)
  !$acc routine seq
    implicit none

    real(8), intent(in) :: CT  ! Total dissolved inorganic carbon (umol kg-1)
    real(8), intent(in) :: pH  ! pH (total scale)
    real(8), intent(in) :: T   ! Temperature (K)
    real(8), intent(in) :: S   ! Salinity (psu)

    real(8)  K1, K2
    real(8)  cH

    K1 = K1_Lueker2000(T, S)
    K2 = K2_Lueker2000(T, S)

    cH = 10.d0**(-pH)

    cCO3_fromCTpH = CT* K1*K2/(cH**2.+K1*cH+K1*K2)  !umol kg-1
    
    return
  end function cCO3_fromCTpH

! **********************************************************************
!  Aragonite saturation state
! **********************************************************************

  real(8)  function Warg_fromcCO3(cCO3, T, S) ! Aragonite saturation state
  !$acc routine seq

    implicit none

    real(8), intent(in) :: cCO3 ! [CO3^2-] (umol kg-1)
    real(8), intent(in) :: T    ! Temperature (K)
    real(8), intent(in) :: S    ! Salinity (psu)

    real(8)  cCO3mol,Karg,Ca

    cCO3mol = cCO3*1.d-6        ! cCO3mol=[CO32-](mol kg-1)
    Ca = 0.01028d0*(S/35.d0)    ! Ca=[Ca2+] (mol kg-1)
    Karg = Karg_Mucci1983(T, S)

    Warg_fromcCO3 = cCO3mol*Ca/Karg

    return
  end function Warg_fromcCO3

!---------------------------------------------------------------------

  real(8)  function Warg_fromcCO3cCa(cCO3,cCa, T, S) ! Aragonite saturation state
  !$acc routine seq

    implicit none

    real(8), intent(in) :: cCO3 ! [CO3^2-] (umol kg-1)
    real(8), intent(in) :: cCa  ! [Ca2+] (mmol kg-1)
    real(8), intent(in) :: T    ! Temperature (K)
    real(8), intent(in) :: S    ! Salinity (psu)

    real(8)  cCO3mol,Karg,Ca

    cCO3mol = cCO3*1.d-6            ! cCO3mol=[CO32-](mol kg-1)
    Ca = cCa*1.d-3                  ! Ca=[Ca2+] (mol kg-1)
    Karg = Karg_Mucci1983(T, S)

    Warg_fromcCO3cCa = cCO3mol*Ca/Karg

    return
  end function Warg_fromcCO3cCa


! ********************************************************************
! Calcite saturation state
! ********************************************************************

  real(8)  function Wcal_fromcCO3(cCO3, T, S) ! Calcite saturation state
  !$acc routine seq

    implicit none

    real(8), intent(in) :: cCO3 ! [CO3^2-] (umol kg-1)
    real(8), intent(in) :: T    ! Temperature (K)
    real(8), intent(in) :: S    ! Salinity (psu)

    real(8)  cCO3mol,Kcal,Ca

    cCO3mol = cCO3*1.d-6           ! cCO3mol=[CO32-](mol kg-1)
    Ca = 0.01028d0*(S/35.d0)       ! Ca=[Ca2+] (mol kg-1)
    Kcal = Kcal_Mucci1983(T, S)

    Wcal_fromcCO3 = cCO3mol*Ca/Kcal

    return
  end function Wcal_fromcCO3

!---------------------------------------------------------------------

  real(8)  function Wcal_fromcCO3cCa(cCO3,cCa, T, S)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: cCO3 ! [CO3^2-] (umol kg-1)
    real(8), intent(in) :: cCa  ! [Ca2+] (mmol kg-1)
    real(8), intent(in) :: T    ! Temperature (K)
    real(8), intent(in) :: S    ! Salinity (psu)

    real(8)  cCO3mol,Kcal,Ca

    cCO3mol = cCO3*1.d-6           ! cCO3mol=[CO32-](mol kg-1)
    Ca = cCa*1.d-3                 ! Ca=[Ca2+] (mol kg-1)
    Kcal = Kcal_Mucci1983(T, S)

    Wcal_fromcCO3cCa = cCO3mol*Ca/Kcal

    return
  end function Wcal_fromcCO3cCa
  

! **********************************************************************
!  Air-sea CO2 flux (sea to air is positive)  (mmol m-2 s-1)
! **********************************************************************

  real(8)  function Flux_CO2(fCO2w, fCO2a, u10, T, S) ! (mmol m-2 s-1)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: fCO2w ! fCO2 (or pCO2) in the sea water (uatm)
    real(8), intent(in) :: fCO2a ! fCO2 (or pCO2) in the air (uatm)
    real(8), intent(in) :: u10   ! Wind speed (m s-1)
    real(8), intent(in) :: T     ! Temperature (K)
    real(8), intent(in) :: S     ! Salinity (psu)

! - CO2 Exchange Parameters (Wanninkhof 1992) ------------------------
    real(8), parameter :: Acoef = 2073.1d0      ! Schmidt
    real(8), parameter :: Bcoef = 125.62d0      ! number
    real(8), parameter :: Ccoef = 3.6276d0      ! transfer
    real(8), parameter :: Dcoef = 0.043219d0    ! coefficients
!   (Weiss,1974)
    real(8), parameter :: A1 = -60.2409d0       ! surface
    real(8), parameter :: A2 = 93.4517d0        ! CO2
    real(8), parameter :: A3 = 23.3585d0        ! solubility
    real(8), parameter :: B1 = 0.023517d0       ! coefficients
    real(8), parameter :: B2 = -0.023656d0
    real(8), parameter :: B3 = 0.0047036d0

    real(8)  Sc, kvex, sol
    real(8)  rhoref, t_oC

! - CO2 Exchange Parameters (Wanninkhof 1992) ------------------------
    
    t_oC = T-273.15d0
    t_oC = min( t_oC, 30.0d0 )  !!! Error handling
    t_oC = max( t_oC,  0.0d0 )  !!! Error handling

    rhoref =1023.0d0

    Sc=Acoef - Bcoef*t_oC + Ccoef*t_oC**2.0d0 - Dcoef*t_oC**3.0d0   !Schmidt number for seawater for temperature ranging from 0 to 30 degrees celsius
    kvex=0.31d0*u10*u10*SQRT(660.0d0/Sc) !Gas transfer velocity for O2;Wanninkhof(1992)  (cm/hr)

    sol=exp(     A1+A2*(100.d0/T)+A3*log(T/100.d0)       &  !Solubility of CO2 [mol/(kg atm)]; Weiss,1974
   &         +S*(B1+B2*(T/100.d0)+B3*(T/100.d0)**2.d0)   &
   &        )

    Flux_CO2 = kvex*sol*(fCO2w-fCO2a)*rhoref    &
   &             /100000.d0/3600.d0

    return
  end function Flux_CO2

! **********************************************************************
!  Air-sea O2 flux (sea to air is positive)  (mmol m-2 s-1)
! **********************************************************************

  real(8) function Flux_O2(DOw, DOsatu, u10, T, S) ! (mmol m-2 s-1)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: DOw    ! Dissolved oxygen (DO) in the sea water (umol L-1)
    real(8), intent(in) :: DOsatu ! Saturation concentration of DO (umol L-1)
    real(8), intent(in) :: u10    ! Wind speed (m s-1)
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)
! - O2 Exchange Parameters (Wanninkhof 1992) ------------------------
!    real(8), parameter :: Acoef = 1953.4d0      ! Schmidt
!    real(8), parameter :: Bcoef = 128.00d0      ! number
!    real(8), parameter :: Ccoef = 3.9918d0      ! transfer
!    real(8), parameter :: Dcoef = 0.050091d0    ! coefficients
! - O2 Exchange Parameters (Keeling et al. 1998) ------------------------
    real(8), parameter :: Acoef = 1638.0d0       ! Schmidt
    real(8), parameter :: Bcoef = 81.83d0        ! number
    real(8), parameter :: Ccoef = 1.483d0        ! transfer
    real(8), parameter :: Dcoef = 0.008004d0     ! coefficients

    real(8)  Sc, kvex, sol
    real(8)  rhoref, t_oC

    t_oC=T-273.15d0
    t_oC = min( t_oC, 40.0d0 )  !!! Error handling
    t_oC = max( t_oC,  0.0d0 )  !!! Error handling

    Sc=Acoef - Bcoef*t_oC + Ccoef*t_oC**2.0d0 - Dcoef*t_oC**3.0d0   !Schmidt number for seawater for temperature ranging from 0 to 30 degrees celsius
    kvex=0.31d0*u10*u10*SQRT(660.0d0/Sc) !Gas transfer velocity for O2;Wanninkhof(1992)  (cm/hr)

    Flux_O2 = kvex*(DOw-DOsatu)/100.d0/3600.d0

    return
  end function Flux_O2

! **********************************************************************
!  O2 saturation concentration  (umol L-1)
! **********************************************************************

  real(8) function O2satu(T, S0) ! O2 saturation concentration  (umol L-1)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S0     ! Salinity (psu)
    
    real(8), parameter :: OA0 = 2.00907d0       ! Oxygen
    real(8), parameter :: OA1 = 3.22014d0       ! saturation
    real(8), parameter :: OA2 = 4.05010d0       ! coefficients
    real(8), parameter :: OA3 = 4.94457d0
    real(8), parameter :: OA4 =-0.256847d0
    real(8), parameter :: OA5 = 3.88767d0
    real(8), parameter :: OB0 =-0.00624523d0
    real(8), parameter :: OB1 =-0.00737614d0
    real(8), parameter :: OB2 =-0.0103410d0
    real(8), parameter :: OB3 =-0.00817083d0
    real(8), parameter :: OC0 =-0.000000488682d0
    real(8), parameter :: rOxNO3= 8.625d0       ! 138/16
    real(8), parameter :: rOxNH4= 6.625d0       ! 106/16
    real(8) :: l2mol = 1000.0d0/22.9316d0      ! liter to mol

    real(8)  t_oC, TS, AA, S

! alculate O2 saturation concentration using Garcia and Gordon
! &O (1992) formula, (EXP(AA) is in ml/l).
!
    t_oC = T-273.15d0
    t_oC = max( t_oC,  0.0d0 )  !!! Error handling
    S    = max( S0,    0.0d0 )  !!! Error handling
    
    TS=LOG((298.15d0-t_oC)/(273.15d0+t_oC))
    AA=OA0+TS*(OA1+TS*(OA2+TS*(OA3+TS*(OA4+TS*OA5))))+          &
                S*(OB0+TS*(OB1+TS*(OB2+TS*OB3)))+               &
                OC0*S*S
!
!  Convert from ml/l to mmol/m3 = umol/L.
!
    O2satu=l2mol*EXP(AA)

    return
  end function O2satu

! **********************************************************************
!  Equilibrium constants
! **********************************************************************

! Weiss (1974) Mar. Chem. 2: 203-215
  real(8)  function K0_Weiss1974(T, S0)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S0     ! Salinity (psu)
    real(8) :: S

    S = max( S0, 0.0d0 )  !!! Error handling

    K0_Weiss1974=exp(-60.2409+93.4517*(100./T)+23.3585*Log(T/100.) &
        +S*(0.023517-0.023656*(T/100.)+0.0047036*(T/100.)**2.))
    return
  end function K0_Weiss1974
!-----------------------------------------------------------------------
! Dissociation constant of carbonic acid by Lueker et al. (2000)
! Total hydrogen ion concentration scale
  real(8)  function K1_Lueker2000(T, S0)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S0     ! Salinity (psu)
    real(8) :: S
    
    S = max( S0, 0.0d0 )  !!! Error handling

    K1_Lueker2000=10.**(-(3633.86/T-61.2172+9.6777*Log(T) &
                        -0.011555*S+0.0001152*S**2.))
    return
  end function K1_Lueker2000
!-----------------------------------------------------------------------
! Dissociation constant of carbonic acid by Lueker et al. (2000)
! Total hydrogen ion concentration scale
  real(8)  function K2_Lueker2000(T, S0)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S0     ! Salinity (psu)
    real(8) :: S
    
    S = max( S0, 0.0d0 )  !!! Error handling

    K2_Lueker2000=10.**(-(471.78/T+25.929-3.16967*Log(T) &
                        -0.01781*S+0.0001122*S**2.))
    return
  end function K2_Lueker2000
!-----------------------------------------------------------------------
! Uppstrom (1974) Deep-Sea Res. 21: 161-162
  real(8)  function BT_Uppstrom1974(S0)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: S0     ! Salinity (psu)
    real(8) :: S
    
    S = max( S0, 0.0d0 )  !!! Error handling

    BT_Uppstrom1974=(0.000232/10.811)*(S/1.80655)
    return
  end function BT_Uppstrom1974
!-----------------------------------------------------------------------
! Dickson (1990) Deep-Sea Res. 37: 755-766
  real(8)  function KB_Dickson1990(T, S0)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S0     ! Salinity (psu)
    real(8) :: S
    
    S = max( S0, 0.0d0 )  !!! Error handling

    KB_Dickson1990=exp((-8966.9-2890.53*S**0.5-77.942*S          &
                        +1.728*S**1.5-0.0996*S**2.)/T            &
                       +(148.0248+137.1942*S**0.5+1.62142*S)     &
                       +(-24.4344-25.085*S**0.5-0.2474*S)*Log(T) &
                       +0.053105*S**0.5*T)
    return
  end function KB_Dickson1990
!-----------------------------------------------------------------------
! Millero (1995) Geochim.Cosmochim Acta 59: 661-677
  real(8)  function Kw_Millero1995(T, S0)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S0     ! Salinity (psu)
    real(8) :: S
    
    S = max( S0, 0.0d0 )  !!! Error handling

    Kw_Millero1995=exp(148.9652-13847.26/T-23.6521*Log(T)       &
                        +(-5.977+118.67/T+1.0495*Log(T))*S**0.5 &
                        - 0.01615*S)
    return
  end function Kw_Millero1995
!-----------------------------------------------------------------------
! Morris & Riley (1966) Deep-Sea Res. 13: 699-705
  real(8)  function ST_MorrisRiley1966(S0)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: S0     ! Salinity (psu)
    real(8) :: S

    S = max( S0, 0.0d0 )  !!! Error handling

    ST_MorrisRiley1966=(0.14/96.062)*(S/1.80655)
    return
  end function ST_MorrisRiley1966
!-----------------------------------------------------------------------
! Dickson (1990) J. Chem. Thermodynamics 22: 113-127
!   -- free hydrogen ion scale
  real(8)  function KS_Dickson1990(T, S0)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S0     ! Salinity (psu)
    real(8) :: SI
    real(8) :: S

    S = max( S0, 0.0d0 )  !!! Error handling

    SI=19.924*S/(1000.0d0-1.005d0*S)

    KS_Dickson1990 = exp(-4276.1/T+141.328-23.093*Log(T)         &
               +(-13856/T+324.57-47.986*Log(T))*SI**0.5          &
               +(35474/T-771.54+114.723*Log(T))* SI              &
               -2698*SI**1.5/T+1776*SI**2./T+Log(1.-0.001005*S))
    return
  end function KS_Dickson1990
!-----------------------------------------------------------------------
! Weiss (1974) Mar. Chem. 2: 203-215
  real(8)  function FT_Weiss1974(S0)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: S0     ! Salinity (psu)
    real(8) :: S

    S = max( S0, 0.0d0 )  !!! Error handling
    
    FT_Weiss1974=(0.000067/18.998)*(S/1.80655)
    
    return
  end function FT_Weiss1974
!-----------------------------------------------------------------------
! Dickson & Riley (1979) Mar. Chem. 7: 89-99
!   -- change pH scale to total
  real(8)  function KF_DicksonRiley1979(T, S0)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S0     ! Salinity (psu)
    real(8)  KF,SI,KS
    real(8) :: S

    S = max( S0, 0.0d0 )  !!! Error handling

    KS = KS_Dickson1990(T, S)

    SI = 19.924*S/(1000.-1.005*S)

    KF = 1590.2/T-12.641+1.525*SI**0.5+Log(1.-0.001005*S)
    KF = KF+Log(1.+(0.14/96.062)*(S/1.80655)/KS)
    KF_DicksonRiley1979 = exp(KF)

    return
  end function KF_DicksonRiley1979
!-----------------------------------------------------------------------
! Perez & Fraga (1987) Mar. Chem. 21: 161-168
  real(8)  function KF_PerezFraga1987(T, S0)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S0     ! Salinity (psu)
    real(8) :: S

    S = max( S0, 0.0d0 )  !!! Error handling

    KF_PerezFraga1987=exp(874./T-9.68+0.111*S**0.5)
    return
  end function KF_PerezFraga1987
!-----------------------------------------------------------------------
! Mucci (1983) Amer. J. Sci. 282: 780
  real(8)  function Kcal_Mucci1983(T, S0)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S0     ! Salinity (psu)
    real(8) :: S

    S = max( S0, 0.0d0 )  !!! Error handling

    Kcal_Mucci1983=10.**(-171.9065-0.077993*T+2839.319/T        &
   &                    +71.595*Log10(T)                        &
   &                    +(-0.77712+0.0028426*T+178.34/T)*S**0.5 &
   &                    - 0.07711*S+0.0041249*S**1.5)
    return
  end function Kcal_Mucci1983
!-----------------------------------------------------------------------
! Mucci (1983) Amer. J. Sci. 282: 780
  real(8)  function Karg_Mucci1983(T, S0)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S0     ! Salinity (psu)
    real(8) :: S

    S = max( S0, 0.0d0 )  !!! Error handling

    Karg_Mucci1983=10.**(-171.945-0.077993*T+2903.293/T          &
                        +71.595*Log10(T)                         &
                        +(-0.068393+0.0017276*T+88.135/T)*S**0.5 &
                        -0.10018*S+0.0059415*S**1.5)
    return
  end function Karg_Mucci1983


! **********************************************************************
!  Rate constants
! **********************************************************************

! Johnson (1982) Limnol. Oceanogr. 27(5): 849-855
  real(8)  function kp1_Johnson1982(T, S)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)

    kp1_Johnson1982=exp(1246.98-6.19d4/T-183.0*Log(T))
    return
  end function kp1_Johnson1982
!-----------------------------------------------------------------------
  real(8)  function km1_Johnson1982(T, S)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)
    real(8)  K1,kp1
    K1 = K1_Lueker2000(T, S)
    kp1= kp1_Johnson1982(T, S)
    
    km1_Johnson1982=kp1/K1
    return
  end function km1_Johnson1982
!-----------------------------------------------------------------------
  real(8)  function kp4_Johnson1982(T, S)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)
    real(8), parameter :: R=8.314d0 ! Gas constant (J mol-1)
    
    kp4_Johnson1982=4.7d7*exp(-23.2d3/(R*T))  !23.2 (kJ mol-1)
    return
  end function kp4_Johnson1982
!-----------------------------------------------------------------------
  real(8)  function km4_Johnson1982(T, S)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)
    real(8)  K1,Kw,kp4
    
    K1 = K1_Lueker2000(T, S)
    Kw = Kw_Millero1995(T, S)
    kp4= kp4_Johnson1982(T, S)
    
    km4_Johnson1982=kp4*Kw/K1
    return
  end function km4_Johnson1982
!-----------------------------------------------------------------------

! **********************************************************************
!  pH calculation when CO2*+H2O <-> H+ +HCO3- is none equilibrium
! **********************************************************************

  real(8)  function pH_fromATCTcCO2aq(AT2, CT2, cCO2aq2, T, S ) ! pH (total scale)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: AT2     ! Total alkalinity (umol kg-1)
    real(8), intent(in) :: CT2     ! Total dissolved inorganic carbon (umol kg-1)
    real(8), intent(in) :: cCO2aq2 ! [CO2*] (umol kg-1)
    real(8), intent(in) :: T       ! Temperature (K)
    real(8), intent(in) :: S       ! Salinity (psu)

    real(8)  AT,CT,cCO2aq,BT,ST,FT,K2,Kw,KB,KS,KF
    real(8)  cH ![H+](TOT) Total hydrogen ion concentration scale
    real(8)  cH0, cH1
    real(8)  q, q0, q1
    real(8)  p, p0, p1

    AT=AT2*1.d-6
    CT=CT2*1.d-6
    cCO2aq=cCO2aq2*1.d-6

    BT=BT_Uppstrom1974(S)
    ST=ST_MorrisRiley1966(S)
    FT=FT_Weiss1974(S)
!    K1=K1_Lueker2000(T, S)
    K2=K2_Lueker2000(T, S)
    Kw=Kw_Millero1995(T, S)
    KB=KB_Dickson1990(T, S)
    KS=KS_Dickson1990(T, S)
    KF=KF_DicksonRiley1979(T, S)

    cH0=10.d0**(-8.d0)

    cH=Newton_Raphson3(cH0,AT,CT,cCO2aq,BT,ST,FT,K2,Kw,KB,KS,KF)

    pH_fromATCTcCO2aq = -Log10(cH)

    return
  end function pH_fromATCTcCO2aq


!-----------------------------------------------------------------------

  real(8)  function cHCO3_noneq(CT, cCO2aq, pH, T, S) ! [HCO3-] (umol kg-1)
  !$acc routine seq

    implicit none
    
    real(8), intent(in) :: CT      ! Total dissolved inorganic carbon (umol kg-1)
    real(8), intent(in) :: cCO2aq  ! [CO2*] (umol kg-1)
    real(8), intent(in) :: pH      ! pH (total scale)
    real(8), intent(in) :: T       ! Temperature (K)
    real(8), intent(in) :: S       ! Salinity (psu)

    real(8)  K2
    real(8)  cH

    K2 = K2_Lueker2000(T, S)

    cH = 10.d0**(-pH)

    cHCO3_noneq = (CT-cCO2aq)*cH/(cH+K2)  !umol kg-1
    
    return
  end function cHCO3_noneq

!-----------------------------------------------------------------------

  real(8)  function cCO3_noneq(CT, cCO2aq, pH, T, S) ! [CO3^2-] (umol kg-1)
  !$acc routine seq

    implicit none
    
    real(8), intent(in) :: CT      ! Total dissolved inorganic carbon (umol kg-1)
    real(8), intent(in) :: cCO2aq  ! [CO2*] (umol kg-1)
    real(8), intent(in) :: pH      ! pH (total scale)
    real(8), intent(in) :: T       ! Temperature (K)
    real(8), intent(in) :: S       ! Salinity (psu)

    real(8)  K2
    real(8)  cH

    K2 = K2_Lueker2000(T, S)

    cH = 10.d0**(-pH)

    cCO3_noneq = (CT-cCO2aq)*K2*cH/(cH**2.+K2*cH)  !umol kg-1
    
    return
  end function cCO3_noneq



! **********************************************************************
!  pH solved by Newton-Raphson method
! **********************************************************************

  real(8)  function func(cH,AT,CT,BT,ST,FT &
                           ,K1,K2,Kw,KB,KS,KF)
  !$acc routine seq

    implicit none

    real(8)  cH,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF
    real(8)  cHCO3
    real(8)  cCO3
    real(8)  cBOH4
    real(8)  cOH
    real(8)  cHF
    real(8)  cHSO4
    real(8)  cHfree

    cHCO3 = CT*K1*cH/(cH**2.+K1*cH+K1*K2)
    cCO3 = CT*K1*K2/(cH**2.+K1*cH+K1*K2)
    cBOH4 = BT/(1.+cH/KB)
    cOH = Kw/cH
    cHfree = cH/(1.+ST/KS)
    cHSO4 = ST/(1.+KS/cHfree)
    cHF = FT/(1.+KF/cH)

    func = (cHCO3+2.*cCO3+cBOH4+cOH-cHfree-cHSO4-cHF)-AT

    return
  end function func

!---------------------------------------------------------------------

  real(8)  function dfunc(cH,AT,CT,BT,ST,FT &
                         ,K1,K2,Kw,KB,KS,KF)
  !$acc routine seq

    implicit none

    real(8)  cH,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF

    dfunc = -cH*CT*K1*(2.*cH+K1)/(cH**2.+cH*K1+K1*K2)**2.    &
      -2.*CT*K1*(2.*cH+K1)*K2/(cH**2.+cH*K1+K1*K2)**2.       &
      +CT*K1/(cH**2.+cH*K1+K1*K2)                            &
      -BT/(KB*(1+cH/KB)**2.)                                 &
      -FT*KF/((1+KF/cH)**2.*cH*cH)                           &
      -Kw/cH**2.-1/(1+ST/KS)                                 &
      - KS*ST*(1+ST/KS)/(cH**2.*(1+KS*(1+ST/KS)/cH)**2.)

    return
  end function dfunc

!-----------------------------------------------------------------------

  real(8)  function Newton_Raphson(x0,x1,AT,CT,BT,ST,FT, &
   &                       K1,K2,Kw,KB,KS,KF)
  !$acc routine seq

    implicit none

    real(8)  x0,x1,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF
    real(8)  q, q0, q1
    real(8)  p, p0, p1

!    real(8)  func

    p0=x0
    p1=x1
    q0=func(p0,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF)
    q1=func(p1,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF)

    do while (abs(q1).gt.1.d-7)
      if(q1.eq.q0) then
        p1=1.d-8
        exit
      endif 
      p =p1-q1*(p1-p0)/(q1-q0)
      p0=p1
      q0=q1
      p1=p
      q1=func(p,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF)
    enddo
     
    Newton_Raphson = p1

    return
  end function Newton_Raphson

!-----------------------------------------------------------------------

  real(8)  function Newton_Raphson2(x0,AT,CT,BT,ST,FT, &
                                    K1,K2,Kw,KB,KS,KF)
  !$acc routine seq

    implicit none

    real(8)  x0,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF
    real(8)  x, y, dy
    real(8)  xini

!    real(8)  func,dfunc
    integer i

    i=0
    xini=x0

    y=func(x0,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF)
    Do while (y.lt.0.d0)! Error handring
      x0=x0*0.1
      y=func(x0,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF)
    end do
    dy=dfunc(x0,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF)

    do while (abs(y).gt.1.d-7)

      x =x0-y/dy

      Do while (x.lt.0.d0)! Error handring
        x=xini*0.1d0
        xini=x
        y=func(x,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF)
       !i=0
      end do

      y=func(x,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF)
      dy=dfunc(x,AT,CT,BT,ST,FT,K1,K2,Kw,KB,KS,KF)
      x0=x
      i=i+1
      if(i.eq.200) then
        !write(50,*) 'pH error: pH = ', -Log10(x),', AT = ', AT,', CT = ',CT
        x=10.**(-8.)
        exit
      endif
    enddo

    Newton_Raphson2 = x0

    return
  end function Newton_Raphson2

!---------------------------------------------------------------------

  real(8)  function func_noneq(cH,AT,CT,cCO2aq,BT,ST,FT &
                              ,K2,Kw,KB,KS,KF)
  !$acc routine seq

    implicit none

    real(8)  cH,AT,CT,cCO2aq,BT,ST,FT,K2,Kw,KB,KS,KF
    real(8)  cHCO3
    real(8)  cCO3
    real(8)  cBOH4
    real(8)  cOH
    real(8)  cHF
    real(8)  cHSO4
    real(8)  cHfree

    cHCO3 = (CT-cCO2aq)*cH/(cH+K2)
    cCO3 = (CT-cCO2aq)*K2/(cH+K2)
    cBOH4 = BT/(1.+cH/KB)
    cOH = Kw/cH
    cHfree = cH/(1.+ST/KS)
    cHSO4 = ST/(1.+KS/cHfree)
    cHF = FT/(1.+KF/cH)

    func_noneq = (cHCO3+2.*cCO3+cBOH4+cOH-cHfree-cHSO4-cHF)-AT

    return
  end function func_noneq

!---------------------------------------------------------------------

  real(8)  function dfunc_noneq(cH,AT,CT,cCO2aq,BT,ST,FT &
                               ,K2,Kw,KB,KS,KF)
  !$acc routine seq

    implicit none

    real(8)  cH,AT,CT,cCO2aq,BT,ST,FT,K2,Kw,KB,KS,KF

    dfunc_noneq =                                            &
       (CT-cCO2aq)*K2/(cH+K2)**2.                            &
      -2.*(CT-cCO2aq)*K2/(cH+K2)**2.                    		 &
      -BT/(KB*(1+cH/KB)**2.)                                 &
      -FT*KF/((1+KF/cH)**2.*cH*cH)                           &
      -Kw/cH**2.-1/(1+ST/KS)                                 &
      - KS*ST*(1+ST/KS)/(cH**2.*(1+KS*(1+ST/KS)/cH)**2.)

    return
  end function dfunc_noneq

!-----------------------------------------------------------------------

  real(8)  function Newton_Raphson3(x0,AT,CT,cCO2aq,BT,ST,FT, &
   &                       K2,Kw,KB,KS,KF)
  !$acc routine seq

    implicit none

    real(8)  x0,AT,CT,cCO2aq,BT,ST,FT,K2,Kw,KB,KS,KF
    real(8)  x, y, dy
    real(8)  xini

!    real(8)  func,dfunc
    integer i

    i=0
    xini=x0

    y=func_noneq(x0,AT,CT,cCO2aq,BT,ST,FT,K2,Kw,KB,KS,KF)
    Do while (y.lt.0.d0)! Error handring
      x0=x0*0.1
      y=func_noneq(x0,AT,CT,cCO2aq,BT,ST,FT,K2,Kw,KB,KS,KF)
    end do
    dy=dfunc_noneq(x0,AT,CT,cCO2aq,BT,ST,FT,K2,Kw,KB,KS,KF)

    do while (abs(y).gt.1.d-7)

      x =x0-y/dy

      Do while (x.lt.0.d0)! Error handring
        x=xini*0.1d0
        xini=x
        y=func_noneq(x,AT,CT,cCO2aq,BT,ST,FT,K2,Kw,KB,KS,KF)
       !i=0
      end do

      y=func_noneq(x,AT,CT,cCO2aq,BT,ST,FT,K2,Kw,KB,KS,KF)
      dy=dfunc_noneq(x,AT,CT,cCO2aq,BT,ST,FT,K2,Kw,KB,KS,KF)
      x0=x
      i=i+1
      if(i.eq.200) then
        !write(6,*) 'pH calculation error: pH = ', -Log10(x)
        x=10.**(-8.)
        exit
      endif
    enddo

    Newton_Raphson3 = x0

    return
  end function Newton_Raphson3


!!!***********************************************************************
!!!   Isotope functions
!!!*********************************************************************** 
! **********************************************************************
!  Safe calculation of isotope ratio.
! **********************************************************************

  real(8) function Iso_ratio(Ci,Ct) ! isotope ratio Ci/Ct
  !$acc routine seq
    implicit none
    real(8), intent(in) :: Ci  ! Isotope concentration (e.g., umol/L) 
    real(8), intent(in) :: Ct  ! Total concentration   (e.g., umol/L)

    if(Ct>0.0d0) then
      Iso_ratio = max(Ci,0.0d0)/Ct
    else
      Iso_ratio = 0.0d0
    endif
    Iso_ratio = min(Iso_ratio, 1.0d0)

    return
  end function Iso_ratio

! **********************************************************************
!  R (isotope ratio) from delta value with standard R value.
! **********************************************************************

  real(8) function R_from_delta(delta,Rstd)  ! e.g., R13C = 13C/12C; R15N = 15N/14N
  !$acc routine seq

    implicit none
    real(8), intent(in) :: delta ! Isotope ratio (delta value; per mill vs standard)
    real(8), intent(in) :: Rstd  ! Isotope ratio of standard

    R_from_delta=(delta*1.d-3 + 1.d0)*Rstd

    return
  end function R_from_delta

! **********************************************************************
!  delta value (isotope ratio vs standard) from 
!   R (isotope ratio) with standard R value.
! **********************************************************************

  real(8) function delta_from_R(Rsample,Rstd)  ! e.g., R13C = 13C/12C; R15N = 15N/14N
  !$acc routine seq

    implicit none
    real(8), intent(in) :: Rsample ! Isotope ratio of sample
    real(8), intent(in) :: Rstd    ! Isotope ratio of standard

    delta_from_R=(Rsample/Rstd-1.d0)*1.d3

    return
  end function delta_from_R

! **********************************************************************
!  delta value (isotope ratio vs standard) from 
!   isotope concentration and total concentration with standard R value.
! **********************************************************************

  real(8) function delta_from_Ci_Ct(Ci,Ct,Rstd)  ! e.g., R13C = 13C/12C; R15N = 15N/14N
  !$acc routine seq

    implicit none
    real(8), intent(in) :: Ci    ! Isotope concentration
    real(8), intent(in) :: Ct    ! Total concentration
    real(8), intent(in) :: Rstd  ! Isotope ratio of standard

    if((Ct-Ci)<=0.0d0) then
      delta_from_Ci_Ct=-9999.0d0
    else
      delta_from_Ci_Ct=(Ci/(Ct-Ci)/Rstd-1.d0)*1.d3
    endif

    return
  end function delta_from_Ci_Ct

! **********************************************************************
!  isotope concentration from delta value (isotope ratio vs standard)
!   and total concentration with standard R value.
! **********************************************************************

  real(8) function Ci_from_Ct_delta(Ct,delta,Rstd)  ! e.g., R13C = 13C/12C; R15N = 15N/14N
  !$acc routine seq

    implicit none
    real(8), intent(in) :: Ct    ! Total concentration
    real(8), intent(in) :: delta ! Isotope ratio (delta value; per mill vs standard)
    real(8), intent(in) :: Rstd  ! Isotope ratio of standard
    real(8) :: cff

    cff = (delta*1.0d-3 + 1.0d0)*Rstd

    Ci_from_Ct_delta= cff/(1+cff) * Ct

    return
  end function Ci_from_Ct_delta


! **********************************************************************
!  d13C VPDB from R13C value.
! **********************************************************************

  real(8) function d13C_fromR13C(R13C) ! Carbon isotope ratio (per mill VPDB)
  !$acc routine seq

    implicit none
    real(8), intent(in) :: R13C  ! R13C = 13C/12C

    d13C_fromR13C=(R13C/R13C_VPDB-1.d0)*1.d3

    return
  end function d13C_fromR13C


! **********************************************************************
!  R13C from R13C VPDB value.
! **********************************************************************

  real(8) function R13C_fromd13C(d13C)  ! R13C = 13C/12C
  !$acc routine seq

    implicit none
    real(8), intent(in) :: d13C ! Carbon isotope ratio (per mill VPDB)

    R13C_fromd13C=(d13C*1.d-3 + 1.d0)*R13C_VPDB

    return
  end function R13C_fromd13C

! **********************************************************************
!  R15N from R15N air value.
! **********************************************************************

  real(8) function R15N_fromd15N(d15N)  ! R15N = 15N/14N
  !$acc routine seq

    implicit none
    real(8), intent(in) :: d15N ! Nitrogen isotope ratio (per mill air)

    R15N_fromd15N=(d15N*1.d-3 + 1.d0)*R15N_AIR

    return
  end function R15N_fromd15N

! **********************************************************************
!  Carbon 13 isotope concentrations
! **********************************************************************

  real(8) function concH13CO3(DI13C,c12CO2aq,cH12CO3,c12CO3,T) ! 13C concentration in HCO3- (umol kg-1)
  !$acc routine seq

    implicit none
    real(8), intent(in) :: DI13C     ! 13C concentration in DIC  (umol kg-1)
    real(8), intent(in) :: c12CO2aq  ! 12C concentration in CO2aq  (umol kg-1)
    real(8), intent(in) :: cH12CO3   ! 12C concentration in HCO3-  (umol kg-1)
    real(8), intent(in) :: c12CO3    ! 12C concentration in CO32-  (umol kg-1)
    real(8), intent(in) :: T         ! Temperature (K)

    real(8) a_db, a_cb
  
    ! Cabon 13 isotope fractionation factors (a)
!    a_db = a_db_Mook1986(T)   ! a(CO2(aq)-HCO3-)
!    a_cb = a_cb_Mook1986(T)   ! a(CO32- -HCO3-)

    a_db = a_db_Zhang1995(T-273.15d0)   ! a(CO2(aq)-HCO3-)
    a_cb = a_cb_Zhang1995(T-273.15d0)   ! a(CO32- -HCO3-)

    concH13CO3 = DI13C/(a_db*c12CO2aq+cH12CO3+a_cb*c12CO3) *cH12CO3

    return
  end function concH13CO3

!---------------------------------------------------------------------

  real(8) function conc13CO2aq(DI13C,c12CO2aq,cH12CO3,c12CO3,T) ! 13C concentration in CO2* (umol kg-1)
  !$acc routine seq

    implicit none
    real(8), intent(in) :: DI13C     ! 13C concentration in DIC (umol kg-1)
    real(8), intent(in) :: c12CO2aq  ! 12C concentration in CO2aq (umol kg-1)
    real(8), intent(in) :: cH12CO3   ! 12C concentration in HCO3- (umol kg-1)
    real(8), intent(in) :: c12CO3    ! 12C concentration in CO32- (umol kg-1)
    real(8), intent(in) :: T         ! Temperature (K)

    real(8) a_db
    real(8) cH13CO3
    
    cH13CO3 = concH13CO3(DI13C,c12CO2aq,cH12CO3,c12CO3,T)
  
    ! Cabon 13 isotope fractionation factors (a)
!    a_db = a_db_Mook1986(T) ! a(CO2(aq)-HCO3-)
    a_db = a_db_Zhang1995(T-273.15d0)   ! a(CO2(aq)-HCO3-)
    
    conc13CO2aq = a_db *cH13CO3/cH12CO3 *c12CO2aq

    return
  end function conc13CO2aq

!-----------------------------------------------------------------------

  real(8) function conc13CO3(DI13C,c12CO2aq,cH12CO3,c12CO3,T) ! 13C concentration in CO3^2- (umol kg-1)
  !$acc routine seq

    implicit none
    real(8), intent(in) :: DI13C     ! 13C concentration in DIC (umol kg-1)
    real(8), intent(in) :: c12CO2aq  ! 12C concentration in CO2aq (umol kg-1)
    real(8), intent(in) :: cH12CO3   ! 12C concentration in HCO3- (umol kg-1)
    real(8), intent(in) :: c12CO3    ! 12C concentration in CO32- (umol kg-1)
    real(8), intent(in) :: T         ! Temperature (K)

    real(8) a_cb
    real(8) cH13CO3
    
    cH13CO3 = concH13CO3(DI13C,c12CO2aq,cH12CO3,c12CO3,T)
  
    ! Cabon 13 isotope fractionation factors (a)
!    a_cb = a_cb_Mook1986(T)   ! a(CO32- -HCO3-)
    a_cb = a_cb_Zhang1995(T-273.15d0)   ! a(CO32- -HCO3-)

    conc13CO3 = a_cb *cH13CO3/cH12CO3 *c12CO3

    return
  end function conc13CO3


! **********************************************************************
!  [H13CO3-] and [13CO32-] calculation 
!  when CO2*+H2O <-> H+ +HCO3- is none equilibrium
! **********************************************************************

  real(8)  function cH13CO3_noneq(DI13C, c13CO2aq, pH, T, S) ! 13C concentration in HCO3- (umol kg-1)
  !$acc routine seq

    implicit none
    
    real(8), intent(in) :: DI13C     ! 13C concentration in DIC (umol kg-1)
    real(8), intent(in) :: c13CO2aq  ! 13C concentration in CO2aq (umol kg-1)
    real(8), intent(in) :: pH        ! pH (total scale)
    real(8), intent(in) :: T         ! Temperature (K)
    real(8), intent(in) :: S         ! Salinity (psu)

    real(8)  K2
    real(8)  cH

    K2 = K2_Lueker2000(T, S) * a_cb_Zhang1995(T-273.15)

    cH = 10.d0**(-pH)

    cH13CO3_noneq = (DI13C-c13CO2aq)*cH/(cH+K2)  !umol kg-1
    
    return
  end function cH13CO3_noneq
    

!-----------------------------------------------------------------------

  real(8)  function c13CO3_noneq(DI13C, c13CO2aq, pH, T, S) ! 13C concentration in CO2* (umol kg-1)
  !$acc routine seq

    implicit none
    real(8), intent(in) :: DI13C     ! 13C concentration in DIC (umol kg-1)
    real(8), intent(in) :: c13CO2aq  ! 13C concentration in CO2aq (umol kg-1)
    real(8), intent(in) :: pH        ! pH (total scale)
    real(8), intent(in) :: T         ! Temperature (K)
    real(8), intent(in) :: S         ! Salinity (psu)

    real(8)  K2
    real(8)  cH

    K2 = K2_Lueker2000(T, S) * a_cb_Zhang1995(T-273.15)

    cH = 10.d0**(-pH)

    c13CO3_noneq = (DI13C-c13CO2aq)*K2*cH/(cH**2.+K2*cH)  !umol kg-1
    
    return
  end function c13CO3_noneq

!-----------------------------------------------------------------------

  real(8)  function cH13CO3_noneq2(DI13C, c13CO2aq, cH12CO3, c12CO3, T) ! 13C concentration in HCO3- (umol kg-1)
  !$acc routine seq

    implicit none
    real(8), intent(in) :: DI13C     ! 13C concentration in DIC (umol kg-1)
    real(8), intent(in) :: c13CO2aq  ! 13C concentration in CO2aq (umol kg-1)
    real(8), intent(in) :: cH12CO3   ! 12C concentration in HCO3- (umol kg-1)
    real(8), intent(in) :: c12CO3    ! 12C concentration in CO32- (umol kg-1)
    real(8), intent(in) :: T         ! Temperature (K)

    real(8)  K2
    real(8)  cH

    real(8) a_db, a_cb
  
    ! Cabon 13 isotope fractionation factors (a)
!    a_cb = a_cb_Mook1986(T)   ! a(CO32- -HCO3-)
    a_cb = a_cb_Zhang1995(T-273.15d0)   ! a(CO32- -HCO3-)

    cH13CO3_noneq2 = (DI13C-c13CO2aq)/(cH12CO3+a_cb*c12CO3) *cH12CO3  !umol kg-1
    
    return
  end function cH13CO3_noneq2

!---------------------------------------------------------------------

  real(8)  function c13CO3_noneq2(DI13C, c13CO2aq, cH12CO3, c12CO3, T) ! 13C concentration in CO3^2- (umol kg-1)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: DI13C     ! 13C concentration in DIC (umol kg-1)
    real(8), intent(in) :: c13CO2aq  ! 13C concentration in CO2aq (umol kg-1)
    real(8), intent(in) :: cH12CO3   ! 12C concentration in HCO3- (umol kg-1)
    real(8), intent(in) :: c12CO3    ! 12C concentration in CO32- (umol kg-1)
    real(8), intent(in) :: T         ! Temperature (K)

    real(8) a_cb
    real(8) cH13CO3
    
    cH13CO3 = cH13CO3_noneq2(DI13C, c13CO2aq, cH12CO3, c12CO3, T)
    
    ! Cabon 13 isotope fractionation factors (a)
!    a_cb = a_cb_Mook1986(T)   ! a(CO32- -HCO3-)
    a_cb = a_cb_Zhang1995(T-273.15d0)   ! a(CO32- -HCO3-)

    c13CO3_noneq2 = a_cb *cH13CO3/cH12CO3 *c12CO3  !umol kg-1
    
    return
  end function c13CO3_noneq2

! **********************************************************************
!  Cabon 13 isotope fractionation factors (a)
! **********************************************************************

! Mook (Mook) Netherlands J. Sea Res. 20: 211-223

  real(8) function a_gb_Mook1986(T)  ! a(CO2(g)-HCO3-)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T   ! Temperatureture (K)
    a_gb_Mook1986=(-9483./T+23.89)*1.d-3 +1.d0
    return
  end function a_gb_Mook1986

  real(8) function a_dg_Mook1986(T)  ! a(CO2(aq)-CO2(g))
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T   ! Temperatureture (K)
    a_dg_Mook1986=(-373./T+0.19)*1.d-3 +1.d0
    return
  end function a_dg_Mook1986

  real(8) function a_db_Mook1986(T)  ! a(CO2(aq)-HCO3-)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T   ! Temperatureture (K)
    a_db_Mook1986=(-9866./T+24.12)*1.d-3 +1.d0
    return
  end function a_db_Mook1986

  real(8) function a_cb_Mook1986(T)  ! a(CO32- -HCO3-)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T   ! Temperatureture (K)
    a_cb_Mook1986=(-867./T+2.52)*1.d-3 +1.d0
    return
  end function a_cb_Mook1986

!-----------------------------------------------------------------------
! Zhang et al. (1995) Geochim. Cosmochim. Acta 59: 107-114

  real(8) function a_bg_Zhang1995(t)  ! a(HCO3--CO2(g))
  !$acc routine seq
    implicit none
    real(8), intent(in) :: t   ! Temperatureture (oC)
    a_bg_Zhang1995=(-0.1141*t+10.78)*1.d-3 +1.d0
    return
  end function a_bg_Zhang1995

  real(8) function a_dg_Zhang1995(t)  ! a(CO2(aq)-CO2(g))
  !$acc routine seq
    implicit none
    real(8), intent(in) :: t   ! Temperatureture (oC)
    a_dg_Zhang1995=(0.0049*t-1.31)*1.d-3 +1.d0
    return
  end function a_dg_Zhang1995

  real(8) function a_cg_Zhang1995(t)  ! a(CO32--CO2(g))
  !$acc routine seq
    implicit none
    real(8), intent(in) :: t   ! Temperatureture (oC)
    a_cg_Zhang1995=(-0.052*t+7.22)*1.d-3 +1.d0
    return
  end function a_cg_Zhang1995

  real(8) function a_cb_Zhang1995(t)  ! a(CO32--HCO3-)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: t   ! Temperatureture (oC)
    real a_cg, a_bg
    a_cg = a_cg_Zhang1995(t)
    a_bg = a_bg_Zhang1995(t)
    a_cb_Zhang1995= a_cg/a_bg
    return
  end function a_cb_Zhang1995

  real(8) function a_db_Zhang1995(t)  ! a(CO2aq-HCO3-)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: t   ! Temperatureture (oC)
    real a_dg, a_bg
    a_dg = a_dg_Zhang1995(t)
    a_bg = a_bg_Zhang1995(t)
    a_db_Zhang1995= a_dg/a_bg
    return
  end function a_db_Zhang1995

! **********************************************************************
!  Boron isotope
! **********************************************************************

  real(8)  function cBOH4_frompH(pH, T, S) ! [B(OH)4-] (umol kg-1)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: pH      ! pH (total scale)
    real(8), intent(in) :: T       ! Temperature (K)
    real(8), intent(in) :: S       ! Salinity (psu)
    
    real(8)  BT
    real(8)  cBOH4,cH,KB

    BT = BT_Uppstrom1974(S)
    KB = KB_Dickson1990(T, S)

    cH = 10.d0**(-pH)
    cBOH4 = BT/(1.+cH/KB)

    cBOH4_frompH = cBOH4*1.d6

    return
  end function cBOH4_frompH

!----------------------------------------------------------------------

  real(8)  function d11B_BOH4_frompHd11BT(pH,d11BT, T, S) ! Boron isotope ratio of B(OH)4- (per mill)
  !$acc routine seq

    implicit none

    real(8), intent(in) :: pH      ! pH (total scale)
    real(8), intent(in) :: d11BT   ! d11B of BT (total borate) (per mill)
    real(8), intent(in) :: T       ! Temperature (K)
    real(8), intent(in) :: S       ! Salinity (psu)

    real(8) BT
    real(8) cBOH4,cH,KB
    real(8) cBOH3
    real(8) eB,aB

    BT = BT_Uppstrom1974(S)
    KB = KB_Dickson1990(T, S)

    cH = 10.d0**(-pH)
    cBOH4 = BT/(1.+cH/KB)
    cBOH3 = BT - cBOH4

    aB = aB_Kakihana1977(T)
!    aB = aB_Palmer1987(T)

    eB = (aB-1.0d0)*1.0d3

    d11B_BOH4_frompHd11BT = (d11BT*BT-eB*cBOH3)/(cBOH4+aB*cBOH3)

    return
  end function d11B_BOH4_frompHd11BT

!----------------------------------------------------------------------
!  d11B NIST SRM951 from R11B value.

  real(8) function d11B_fromR11B(R11B) ! Boron isotope ratio (per mill)
  !$acc routine seq

    implicit none
    real(8), intent(in) :: R11B  ! R11B = 11B/10B

    d11B_fromR11B = (R11B/R11B_SRM951 - 1.d0)*1.d3

    return
  end function d11B_fromR11B

!----------------------------------------------------------------------
!  R11B from R11B NIST SRM951 value.

  real(8) function R11B_fromd11B(d11B)  ! R11B = 11B/10B
  !$acc routine seq

    implicit none
    real(8), intent(in) :: d11B  ! Boron isotope ratio (per mill)

    R11B_fromd11B=(d11B*1.d-3 + 1.d0)*R11B_SRM951

    return
  end function R11B_fromd11B

!----------------------------------------------------------------------
!  Boron isotope fractionation factors (a(B(OH)3-B(OH)4^-))

! Kakihana et al. (1977)

  real(8) function aB_Kakihana1977(T)  ! a(B(OH)3-B(OH)4^-)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T   ! Temperatureture (K)
    real(8) t_oC ! Temperature (oC)
    
    aB_Kakihana1977=1.0194d0
    return
  end function aB_Kakihana1977

! Palmer et al. (1987)

  real(8) function aB_Palmer1987(T)  ! a(B(OH)3-B(OH)4^-)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T   ! Temperatureture (K)
    real(8) t_oC ! Temperature (oC)
    
    aB_Palmer1987=1.0d0/0.968d0
    return
  end function aB_Palmer1987

!!!***********************************************************************
!!!   Empirical vertical profiles of biogeochemical parameters
!!!*********************************************************************** 

!--- Dissolved Inorganic Carbon (DIC) ----------------
!     Based on GLODAPv2.2022

  real(8) function DIC_Profile(T, S, d)
  !$acc routine seq
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)

  real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 2.007d3, -1.103d1, 8.029d-1, 5.370d1,  9.696d0,  9.124d1,  8.111d0,  8.774d0 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 2.009d3,  3.488d0, 9.748d-1, 2.390d1, -1.309d1,  6.951d2, -1.732d1,  3.108d1 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 2.002d3, -1.043d1, 7.908d-1, 3.965d1, -3.804d0,  1.635d2,  6.672d0,  1.203d1 ]
#elif defined BIO_VPROFILE_PALAU
    [ 2.080d3, -1.409d1, 2.725d-1, 6.200d1, -1.019d1, -5.377d1,  2.378d0, -1.039d0 ]
#elif defined BIO_VPROFILE_JAPAN
    [ 1.997d3, -1.053d1, 6.797d-1, 6.403d1, -4.851d0,  1.955d2,  1.084d0,  1.177d1 ]
#elif defined BIO_VPROFILE_SOUTH_JAPAN
    [ 1.979d3, -8.542d0, 4.749d-1, 4.705d1, -1.605d-1, 3.206d2, -8.712d0,  1.464d1 ]
#else
    [ 1.997d3, -1.053d1, 6.797d-1, 6.403d1, -4.851d0,  1.955d2,  1.084d0,  1.177d1 ]
#endif

  real(8), parameter :: as(0:5)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 1.993d3, -7.097d0, 4.383d-1, 2.318d1, 2.035d0,  2.045d0  ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 1.993d3, -6.429d0, 1.149d0 , 3.807d1, 2.469d1, -9.990d0  ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 2.001d3, -7.191d0, 8.247d-1, 6.991d1, 3.951d1,  5.203d0  ]
#elif defined BIO_VPROFILE_PALAU
    [ 2.118d3, -3.514d1, 2.216d0 , 9.692d0, 4.023d0,  8.289d0  ]
#elif defined BIO_VPROFILE_JAPAN
    [ 2.004d3, -6.663d0, 1.223d-1, 4.656d1, 8.660d0,  1.011d0  ]
#elif defined BIO_VPROFILE_SOUTH_JAPAN
    [ 2.000d3, -6.390d0, 1.118d-1, 4.192d1, 1.002d1,  1.642d-1 ]
#else
    [ 2.004d3, -6.663d0, 1.223d-1, 4.656d1, 8.660d0,  1.011d0  ]
#endif
  
  real(8) :: Tm20, Sm35, Dp1000
  real(8) :: DIC_ProfileS

  Tm20 = T-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  DIC_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                     + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000 
  DIC_ProfileS= as(0) + as(1)*Tm20 + as(2)*Tm20*Tm20 + as(3)*Sm35 + as(4)*Sm35*Sm35  &
                      + as(5)*Tm20*Sm35
  DIC_Profile = max( DIC_Profile, DIC_ProfileS )

  end function DIC_Profile

!--- Total Alikalinity (TA) ------------------------------
!     Based on GLODAPv2.2022

  real(8) function TA_Profile(T, S, d)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: T   ! Potential temperatureture (oC)
    real(8), intent(in) :: S   ! Salinity (PSU)
    real(8), intent(in) :: d   ! Depth (m)

    real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 2.307d3, -1.918d0 ,  2.205d-1, 7.441d1, 1.403d1, -5.749d1, -2.726d0, -3.941d0  ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 2.311d3, -1.963d0 , -4.433d-2, 7.873d1, 1.528d1, -1.533d0, -1.430d1, -5.478d0  ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 2.302d3, -9.010d-1,  2.861d-1, 7.795d1, 1.246d1, -2.137d1, -5.095d0, -2.425d0  ]
#elif defined BIO_VPROFILE_PALAU
    [ 2.302d3, -7.912d-1,  2.131d-1, 9.929d1, 1.825d1,  3.087d1, -7.132d0, -6.071d-1 ]
#elif defined BIO_VPROFILE_JAPAN
    [ 2.299d3, -1.034d0 ,  2.147d-1, 9.433d1, 1.676d1,  3.711d1, -5.374d0,  8.546d-2 ]
#elif defined BIO_VPROFILE_SOUTH_JAPAN
    [ 2.293d3, -6.713d-1,  1.639d-1, 1.090d2, 2.492d1,  1.272d2, -6.411d0,  4.269d0  ]
#else
    [ 2.299d3, -1.034d0 ,  2.147d-1, 9.433d1, 1.676d1,  3.711d1, -5.374d0,  8.546d-2 ]
#endif

  real(8), parameter :: as(0:5)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 2.487d3, -3.638d1 ,  1.915d0 , 1.561d2,  3.375d1, -4.095d0 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 2.294d3,  1.095d0 , -1.049d-1, 3.103d1, -2.767d0,  2.174d0 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 2.298d3,  9.481d-1,  7.033d-2, 6.270d1,  1.369d1,  3.171d0 ]
#elif defined BIO_VPROFILE_PALAU
    [ 2.487d3, -3.638d1 ,  1.915d0 , 1.561d2,  3.375d1, -4.095d0 ]
#elif defined BIO_VPROFILE_JAPAN
    [ 2.295d3,  6.491d-1, -6.974d-2, 4.748d1,  6.403d0,  2.181d0 ]
#elif defined BIO_VPROFILE_SOUTH_JAPAN
    [ 2.293d3,  7.694d-1, -8.261d-2, 4.271d1,  5.810d0,  1.955d0 ]
#else
    [ 2.295d3,  6.491d-1, -6.974d-2, 4.748d1,  6.403d0,  2.181d0 ]
#endif
  
  real(8) :: Tm20, Sm35, Dp1000
  real(8) :: TA_ProfileS

  Tm20 = T-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  TA_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                    + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000 
  TA_ProfileS= as(0) + as(1)*Tm20 + as(2)*Tm20*Tm20 + as(3)*Sm35 + as(4)*Sm35*Sm35  &
                      + as(5)*Tm20*Sm35
  TA_Profile = max( TA_Profile, TA_ProfileS )

  end function TA_Profile

!--- Dissolved Oxygen (DO) ------------------------------
!     Based on GLODAPv2.2022

  real(8) function DO_Profile(T, S, d)
  !$acc routine seq
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)

  real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 2.072d2,  3.011d0 , -6.450d-1, -1.319d1,  2.402d0, -4.065d1, -1.214d1 , -7.883d0 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 2.102d2, -1.435d1 , -9.573d-1, -5.411d0,  2.391d1, -1.034d3,  1.264d1 , -5.162d1 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 2.325d2,  7.121d-1, -6.509d-1,  3.333d1,  2.923d1, -1.317d2, -2.071d0 , -1.001d1 ]
#elif defined BIO_VPROFILE_PALAU
    [ 1.504d2,  6.169d0 , -1.266d-1, -7.345d0, -5.712d0,  3.998d1, -8.364d0 , -1.864d0 ]
#elif defined BIO_VPROFILE_JAPAN
    [ 2.360d2,  7.591d-1, -4.747d-1,  2.125d1,  3.800d1, -2.282d2, -8.667d-1, -1.358d1 ]
#elif defined BIO_VPROFILE_SOUTH_JAPAN
    [ 2.556d2, -1.459d0 , -2.503d-1,  4.094d1,  3.543d1, -3.431d2,  1.209d1 , -1.506d1 ]
#else
    [ 2.360d2,  7.591d-1, -4.747d-1,  2.125d1,  3.800d1, -2.282d2, -8.667d-1, -1.358d1 ]
#endif

!  real(8), parameter :: as(0:5)  = &
!#if defined BIO_VPROFILE_YAEYAMA
!    [ 2.216d2, -3.472d0, 1.008d-1, -4.275d0, -7.918d-1, 2.164d-1 ]
!#elif defined BIO_VPROFILE_SHIZUGAWA
!    [ 2.331d2, -2.532d0, -2.088d-1, -3.245d0, -3.367d0, 4.001d0 ]
!#elif defined BIO_VPROFILE_TOKYOBAY
!    [ 2.226d2, -2.726d0, -1.003d-1, 4.669d0, 2.215d0, -1.760d0 ]
!#elif defined BIO_VPROFILE_PALAU
!    [ 1.530d2, 1.240d1, -8.342d-1, 5.506d0, -5.926d-1, -7.529d-1 ]
!#elif defined BIO_VPROFILE_JAPAN
!    [ 2.241d2, -3.699d0, 1.076d-1, -5.114d0, -5.175d-1, 7.316d-1 ]
!#elif defined BIO_VPROFILE_SOUTH_JAPAN
!    [ 2.215d2, -3.307d0, 1.169d-1, -8.838d0, -1.658d0, 1.209d0 ]
!#else
!    [ 2.241d2, -3.699d0, 1.076d-1, -5.114d0, -5.175d-1, 7.316d-1 ]
!#endif

  real(8) :: Tm20, Sm35, Dp1000
  real(8) :: DOsat
  
  Tm20 = T-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  DO_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                    + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000 
  DOsat = O2satu(T+273.15d0, S)          
  DO_Profile = max( DO_Profile, 0.0d0 )
!  DO_Profile = min( DO_Profile, DOsat )

  end function DO_Profile

!--- Dissolved Oxygen (DO) ------------------------------
!     TN test

  real(8) function DO_Profile2(SST, SSS, SSDIC, DIC, SSTA, TA)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: SST   ! Sea Surface Temperatureture (oC)
    real(8), intent(in) :: SSS   ! Sea Surface Salinity (psu)
    real(8), intent(in) :: SSDIC ! Sea Surface DIC (umol kg-1)
    real(8), intent(in) :: DIC   ! in situ DIC (umol kg-1)
    real(8), intent(in) :: SSTA ! Sea Surface DIC (umol kg-1)
    real(8), intent(in) :: TA   ! in situ DIC (umol kg-1)

    ! C106 H212 O106 N16 P (POM) + 127.25*O2 + 9.5*H2O
    ! --> 106*CO2 + 106*H2O +19*H+ + 16*NO3- + PO43-

    real(8) :: dTA, dDIC, CaCO3_Dissol

    dTA = TA - SSTA
    dDIC = DIC - SSDIC
    CaCO3_Dissol = 0.50d0*( dTA + dDIC*16.0d0/106.0d0 + dDIC*1.0d0/106.0d0 )

    DO_Profile2 = O2satu(SST+273.15d0, SSS)               &
          - ( dDIC - CaCO3_Dissol )*127.25d0/106.0d0*1.024d0
    DO_Profile2 = max( DO_Profile2, 50.0d0 ) !! Error handling

  end function DO_Profile2


!--- Phytoplankton_01 -------------------------------------
!     developed by Faisal AMRI

  real(8) function PHY1_Profile(d)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: d   ! elevation (m) = -depth

    IF(( d .GE. -100.0d0) .AND. ( d .LE. -150.0d0) ) THEN
      PHY1_Profile = 0.0d0
    ELSE IF(( d .LT. -100.0d0) .AND. ( d .GT. -150.0d0)) THEN
      PHY1_Profile = 0.8d0
    END IF

  end function PHY1_Profile

!--- Phytoplankton_02 -------------------------------------
!     developed by Faisal AMRI

  real(8) function PHY2_Profile(d)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: d   ! elevation (m) = -depth  
 
    IF(( d .GE. -100.0d0) .AND. ( d .LE. -150.0d0) ) THEN
      PHY2_Profile = 0.0d0
    ELSE IF(( d .LT. -100.0d0) .AND. ( d .GT. -150.0d0)) THEN
      PHY2_Profile = 0.8d0
    END IF

  end function PHY2_Profile

!--- Phytoplankton -------------------------------------
!     Based on GLODAPv2.2022

  real(8) function PhyC_Profile(T, S, d, m)
  !$acc routine seq
  implicit none
  real(8), intent(in) :: T    ! Potential temperatureture (oC)
  real(8), intent(in) :: S    ! Salinity (PSU)
  real(8), intent(in) :: d    ! Depth (m)
  integer, intent(in) :: m    ! Index of Phytoplankton functional group 

                                      !  Dnfl   Dtm    Ccl    Cya
  real(8), parameter :: r(4)        = [ 0.4d0, 0.3d0, 0.1d0, 0.2d0 ]
!  real(8), parameter :: Chl2PhyC(4) = [ 0.2d0, 0.2d0, 0.2d0, 0.2d0 ] !Conversion ratio from Chl-a to phyC (umolC/L)/(ug/L)
         
  PhyC_Profile = ( 2.7d0-PO4_Profile(T, S, d) )*106.0d0/1.0d0*0.1d0*r(4)
  PhyC_Profile = max( PhyC_Profile, 0.0d0 )

  end function PhyC_Profile

!--- Phytoplankton -------------------------------------
!     developed by Shinya AMANO, Takashi NAKAMURA

  real(8) function PHY_Profile2(iphy, SSPhyC, d)
  !$acc routine seq
    implicit none
    integer, intent(in) :: iphy   ! Phytoplankton functional group num 
    real(8), intent(in) :: SSphyC ! Sea surface phytoplankton density (umolC L-1) 
    real(8), intent(in) :: d      ! elevation (m) = -depth  
 
    SELECT CASE(iphy)
    !--- Phytoplankton_01 ---
    CASE(1)
      IF (d.gt.-155.0d0) THEN
        PHY_Profile2=(-0.00095d0*(d+50.0d0)**2 + 10.5d0)/24.0d0 * SSphyC    ! umol L-1
      ELSE IF (d.le.-155d0) THEN
        PHY_Profile2=0.0d0     ! umol L-1
      END IF
    !--- Phytoplankton_02 ---
    CASE(2)
      IF (d.gt.-155.0d0) THEN
        PHY_Profile2=(-0.00095d0*(d+50.0d0)**2 + 10.5d0)/24.0d0 * SSphyC    ! umol L-1
      ELSE IF (d.le.-155d0) THEN
        PHY_Profile2=0.0d0     ! umol L-1
      END IF
    !--- Phytoplankton_03 ---
    CASE(3)
      IF (d.gt.-155.0d0) THEN
        PHY_Profile2=(-0.00095d0*(d+50.0d0)**2 + 10.5d0)/24.0d0 * SSphyC    ! umol L-1
      ELSE IF (d.le.-155d0) THEN
        PHY_Profile2=0.0d0     ! umol L-1
      END IF
    !--- Phytoplankton_04 ---
    CASE(4)
      IF (d.gt.-155.0d0) THEN
        PHY_Profile2=(-0.00095d0*(d+50.0d0)**2 + 10.5d0)/24.0d0 * SSphyC    ! umol L-1
      ELSE IF (d.le.-155d0) THEN
        PHY_Profile2=0.0d0     ! umol L-1
      END IF
    END SELECT

  end function PHY_Profile2

!--- Zooplankton -------------------------------------
!     developed by Faisal AMRI

  real(8) function ZOO_Profile(d)
  !$acc routine seq
    implicit none
    real(8), intent(in) :: d   ! elevation (m) = -depth  
 
    IF(( d .GE. -400.0d0) .AND. ( d .LE. 10.0d0 )) THEN
      ZOO_Profile = 2.5d0+(d/200.0d0)    ! umolC L-1
    ELSE
      ZOO_Profile = 0.0d0                            ! umolC L-1
    END IF

  end function ZOO_Profile

!--- Zooplankton -------------------------------------
!     TN test

  real(8) function ZOO_Profile2(izoo, SSzooC, d)
  !$acc routine seq
    implicit none
    integer, intent(in) :: izoo ! Zooplankton functional group num 
    real(8), intent(in) :: SSzooC ! Sea surface zooplankton density (umolC L-1) 
    real(8), intent(in) :: d    ! elevation (m) = -depth

    SELECT CASE(izoo)
    !--- Zooplankton_01 ---
    CASE(1)
      ZOO_Profile2 = max( 0.25d0+(d/2000.0d0), 0.0d0 )    ! umolC L-1
    END SELECT

  end function ZOO_Profile2

!--- Zooplankton -------------------------------------
!     Based on GLODAPv2.2022

  real(8) function ZooC_Profile(T, S, d, m)
  !$acc routine seq
  implicit none
  real(8), intent(in) :: T    ! Potential temperatureture (oC)
  real(8), intent(in) :: S    ! Salinity (PSU)
  real(8), intent(in) :: d    ! Depth (m)
  integer, intent(in) :: m    ! Index of Phytoplankton functional group 

  ZooC_Profile = PhyC_Profile(T, S, d, 2) *0.1d0             
  ZooC_Profile = max( ZooC_Profile, 0.0d0 )
!  ZooC_Profile = max( ZooC_Profile, 0.2d0 )

  end function ZooC_Profile

!--- NO3 -------------------------------------
!     Based on GLODAPv2.2022

  real(8) function NO3_Profile(T, S, d)
  !$acc routine seq
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)

  real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [  2.208d0, -1.137d0 , 1.001d-1, -5.227d-1,  3.034d-1,  2.623d0,  1.286d0 , 7.374d-1 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [  4.088d0,  1.561d0 , 1.855d-1, -1.271d0 , -3.409d0 ,  1.131d2, -5.355d-1, 5.905d0  ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ -1.257d0, -9.759d-1, 9.293d-2, -9.219d0 , -5.306d0 ,  1.279d1,  7.003d-1, 1.061d0  ]
#elif defined BIO_VPROFILE_PALAU
    [  7.059d0, -1.538d0 , 6.149d-2, -2.690d0 , -9.674d-1, -1.112d0,  1.415d0 , 5.571d-1 ]
#elif defined BIO_VPROFILE_JAPAN
    [ -1.447d0, -8.447d-1, 8.370d-2, -3.811d0 , -4.265d0 ,  3.089d1,  1.750d-1, 1.793d0  ]
#elif defined BIO_VPROFILE_SOUTH_JAPAN
    [ -4.288d0, -5.084d-1, 5.955d-2, -5.185d0 , -3.196d0 ,  4.910d1, -1.338d0 , 2.204d0  ]
#else
    [ -1.447d0, -8.447d-1, 8.370d-2, -3.811d0 , -4.265d0 ,  3.089d1,  1.750d-1, 1.793d0  ]
#endif

  real(8), parameter :: as(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 9.032d-1, -3.844d-1, 3.951d-2, -8.172d-1,  5.009d-1,  2.258d-1 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 5.200d-1, -5.865d-1, 8.589d-2,  2.190d0 ,  1.815d0 , -8.454d-1 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 7.313d-1, -6.208d-1, 6.290d-2, -2.212d0 , -7.390d-1,  1.325d-1 ]
#elif defined BIO_VPROFILE_PALAU
    [ 3.293d0 , -8.412d-1, 5.174d-2, -8.735d-1, -3.583d-2,  8.459d-2 ]
#elif defined BIO_VPROFILE_JAPAN
    [ 1.179d0 , -3.838d-1, 3.009d-2,  1.546d0 ,  8.115d-1, -7.434d-2 ]
#elif defined BIO_VPROFILE_SOUTH_JAPAN
    [ 1.561d0 , -3.742d-1, 2.604d-2,  2.988d0 ,  1.464d0 , -1.642d-1 ]
#else
    [ 1.179d0 , -3.838d-1, 3.009d-2,  1.546d0 ,  8.115d-1, -7.434d-2 ]
#endif

  real(8) :: Tm20, Sm35, Dp1000
  real(8) :: NO3_ProfileS
  
  Tm20 = min(T,27.0d0)-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  NO3_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                     + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000 
  NO3_ProfileS= as(0) + as(1)*Tm20 + as(2)*Tm20*Tm20 + as(3)*Sm35 + as(4)*Sm35*Sm35  &
                      + as(5)*Tm20*Sm35
  NO3_Profile = max( NO3_Profile, NO3_ProfileS )
  NO3_Profile = max( NO3_Profile, 0.2d0 )

  end function NO3_Profile

!--- NO3 -------------------------------------
!     developed by Shinya AMANO

  real(8) function NO3_Profile2( d )
  !$acc routine seq
    implicit none
    real(8), intent(in) :: d   ! elevation (m) = -depth  
    real(8), parameter :: c_NO3_s=-0.0508d0
    real(8), parameter :: c_NO3_sc=-2.9698d0
    real(8), parameter :: c_NO3_d=0.0018d0
    real(8), parameter :: c_NO3_dc=43.891d0

    IF (d.gt.-63.9196d0) THEN
      NO3_Profile2 = 0.5d0     ! umol L-1
    ELSE IF (d.gt.-900d0) THEN
      NO3_Profile2 = c_NO3_s*d + c_NO3_sc     ! umol L-1
    ELSE IF (d.le.-900d0) THEN
      NO3_Profile2 = c_NO3_d*d + c_NO3_dc     ! umol L-1
    END IF

  end function NO3_Profile2

!--- NO3 -------------------------------------
!     TN test 

  real(8) function NO3_Profile3( SSNO3, SSDO, DO )
  !$acc routine seq
    implicit none
    real(8), intent(in) :: SSNO3 ! Sea surface NO3 (umol L-1)
    real(8), intent(in) :: SSDO  ! Sea Surface DO (umol L-1)
    real(8), intent(in) :: DO    ! in situ DO (umol L-1)

    ! C106 H212 O106 N16 P (POM) + 127.25*O2 + 9.5*H2O
    ! --> 106*CO2 + 106*H2O +19*H+ + 16*NO3- + PO43-
 
    NO3_Profile3 = SSNO3 + ( SSDO- DO )*16.0d0/127.25d0
    NO3_Profile3 = max( NO3_Profile3, SSNO3 )  !! Error handling

  end function NO3_Profile3


!--- PO4 -------------------------------------
!     Based on GLODAPv2.2022

  real(8) function PO4_Profile(T, S, d)
  !$acc routine seq
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)

  real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [  1.826d-1, -8.207d-2, 7.395d-3, -7.733d-2, -3.363d-2, 5.292d-2,  1.027d-1, 5.057d-2 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [  4.005d-1,  1.109d-1, 1.362d-2,  7.927d-2, -1.441d-1, 7.608d0 , -2.802d-2, 4.025d-1 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ -8.995d-2, -6.702d-2, 7.062d-3, -6.973d-1, -4.271d-1, 8.137d-1,  5.524d-2, 7.413d-2 ]
#elif defined BIO_VPROFILE_PALAU
    [  5.739d-1, -1.047d-1, 4.408d-3, -6.078d-2, -3.111d-2, 2.040d-1,  1.004d-1, 5.465d-2 ]
#elif defined BIO_VPROFILE_JAPAN
    [ -6.903d-2, -6.035d-2, 6.247d-3, -2.620d-1, -2.924d-1, 2.013d0 ,  2.113d-2, 1.221d-1 ]
#elif defined BIO_VPROFILE_SOUTH_JAPAN
    [ -2.791d-1, -3.745d-2, 4.330d-3, -4.121d-1, -2.369d-1, 3.209d0 , -9.455d-2, 1.436d-1 ]
#else
    [ -6.903d-2, -6.035d-2, 6.247d-3, -2.620d-1, -2.924d-1, 2.013d0 ,  2.113d-2, 1.221d-1 ]
#endif

  real(8), parameter :: as(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 1.096d-1, -3.178d-2, 2.530d-3, -4.185d-2,  5.948d-3,  6.234d-3 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 1.887d-1, -2.347d-2, 6.930d-3,  2.783d-1,  1.652d-1, -4.782d-2 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 7.185d-2, -4.182d-2, 3.981d-3, -1.950d-1, -7.760d-2,  1.183d-2 ]
#elif defined BIO_VPROFILE_PALAU
    [ 5.215d-1, -1.011d-1, 6.138d-3,  2.676d-2,  4.451d-2,  1.089d-2 ]
#elif defined BIO_VPROFILE_JAPAN
    [ 9.015d-2, -3.022d-2, 2.612d-3,  2.672d-2,  1.036d-2, -2.594d-3 ]
#elif defined BIO_VPROFILE_SOUTH_JAPAN
    [ 1.144d-1, -2.857d-2, 2.109d-3,  1.264d-1,  5.276d-2, -7.881d-3 ]
#else
    [ 9.015d-2, -3.022d-2, 2.612d-3,  2.672d-2,  1.036d-2, -2.594d-3 ]
#endif

  real(8) :: Tm20, Sm35, Dp1000
  real(8) :: PO4_ProfileS
  
  Tm20 = min(T,27.0d0)-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  PO4_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                     + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000 
  PO4_ProfileS= as(0) + as(1)*Tm20 + as(2)*Tm20*Tm20 + as(3)*Sm35 + as(4)*Sm35*Sm35  &
                      + as(5)*Tm20*Sm35
  PO4_Profile = max( PO4_Profile, PO4_ProfileS )
  PO4_Profile = max( PO4_Profile, 0.03d0 )

  end function PO4_Profile

!--- PO4 -------------------------------------
!     developed by Shinya AMANO

  real(8) function PO4_Profile2( d )
  !$acc routine seq
    implicit none
    real(8), intent(in) :: d   ! elevation (m) = -depth  
    real(8), parameter :: c_PO4_s=-0.0037d0
    real(8), parameter :: c_PO4_sc=-0.1513d0
    real(8), parameter :: c_PO4_d=0.0002d0
    real(8), parameter :: c_PO4_dc=3.3429d0

    IF (d.gt.-52.716d0) THEN
      PO4_Profile2=0.04     ! umol L-1
    ELSE IF (d.gt.-950d0) THEN
      PO4_Profile2=c_PO4_s*d + c_PO4_sc     ! umol L-1
    ELSE IF (d.le.-950d0) THEN
      PO4_Profile2=c_PO4_d*d + c_PO4_dc     ! umol L-1
    END IF

  end function PO4_Profile2

!--- PO4 -------------------------------------
!     TN test

  real(8) function PO4_Profile3( SSPO4, SSDO, DO )
  !$acc routine seq
    implicit none
    real(8), intent(in) :: SSPO4 ! Sea surface PO4 (umol L-1)
    real(8), intent(in) :: SSDO ! Sea Surface DO (umol L-1)
    real(8), intent(in) :: DO   ! in situ DO (umol L-1)

    ! C106 H212 O106 N16 P (POM) + 127.25*O2 + 9.5*H2O
    ! --> 106*CO2 + 106*H2O +19*H+ + 16*NO3- + PO43-
 
    PO4_Profile3 = SSPO4 + ( SSDO- DO )/127.25d0
    PO4_Profile3 = max(PO4_Profile3, SSPO4)  !! Error handling

  end function PO4_Profile3

!--- DOC -------------------------------------
!     Based on GLODAPv2.2022

  real(8) function DOC_Profile(T, S, d, m)
  !$acc routine seq
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)
  Integer, intent(in) :: m   ! Index of DOM category

  real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 7.508d1, 1.771d-1, -7.876d-2, -5.447d1 , -5.423d1, -9.065d1,  6.655d-1, -4.560d0  ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 6.443d1, 1.544d0 ,  3.699d-2,  5.138d0 ,  5.104d0, -2.380d1,  4.623d-1, -1.065d0  ] ! BIO_VPROFILE_JAPAN data was used
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 6.443d1, 1.544d0 ,  3.699d-2,  5.138d0 ,  5.104d0, -2.380d1,  4.623d-1, -1.065d0  ] ! BIO_VPROFILE_JAPAN data was used
#elif defined BIO_VPROFILE_PALAU
    [ 5.184d1, 1.846d0 ,  4.738d-2, -1.389d1 , -1.953d1, -3.084d1, -1.121d1 , -2.361d0  ]
#elif defined BIO_VPROFILE_JAPAN
    [ 6.443d1, 1.544d0 ,  3.699d-2,  5.138d0 ,  5.104d0, -2.380d1,  4.623d-1, -1.065d0  ]
#elif defined BIO_VPROFILE_SOUTH_JAPAN
    [ 6.268d1, 1.767d0 ,  2.947d-2,  3.806d-2,  3.781d0, -1.878d1, -2.338d-2, -9.997d-1 ]
#else
    [ 6.443d1, 1.544d0 ,  3.699d-2,  5.138d0 ,  5.104d0, -2.380d1,  4.623d-1, -1.065d0  ]
#endif
                                 ! LDOM    RDOM
!  real(8), parameter :: r(2)  = [ 0.42d0, 0.58d0 ]
  real(8), parameter :: r(2)  = [ 0.01d0, 0.99d0 ]
  real(8), parameter :: RDOC0 = 0.35d0 ! Baseline value of Reflactory DOC (umolC/L) 

  real(8) :: Tm20, Sm35, Dp1000
 
  Tm20 = T-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  DOC_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                     + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000
  if ( DOC_Profile > RDOC0 ) then
    DOC_Profile = RDOC0 + ( DOC_Profile - RDOC0 )*r(m)
  end if
  DOC_Profile = min( DOC_Profile, 200.0d0*r(m) )
  DOC_Profile = max( DOC_Profile, 20.0d0*r(m) )

  end function DOC_Profile

!--- POC -------------------------------------
!     Based on ?????????????てきとう。要修正 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real(8) function POC_Profile(T, S, d, m)
  !$acc routine seq
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)
  Integer, intent(in) :: m   ! Index of POM category

                                 ! LPOM    RPOM    CPOM
  real(8), parameter :: r(3)  = [ 0.42d0, 0.58d0, 0.0d0 ]
  real(8), parameter :: RDOC0 = 0.35d0 ! Baseline value of Reflactory DOC (umolC/L) 

  POC_Profile = PO4_Profile(T, S, d) * 106.0d0/1.0d0  ! C/P ratio = 106 for POM

  if ( POC_Profile > RDOC0 ) then
    POC_Profile = RDOC0 + ( POC_Profile - RDOC0 )*r(m)
  end if
  POC_Profile = min( POC_Profile, 200.0d0*r(m) )*0.1d0  ! 10% of DOC
  POC_Profile = max( POC_Profile, 0.0d0 )

  end function POC_Profile

!--- d13C_DIC -------------------------------------
!     Based on GLODAPv2.2022

  real(8) function d13C_DIC_Profile(T, S, d)
  !$acc routine seq
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)

  real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 5.945d-1, 8.812d-2, -8.214d-4, -2.949d-1,  1.242d-1,  3.770d-1, -2.554d-1, -2.798d-2 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 9.557d-1, 5.620d-2, -1.996d-3,  3.414d-1,  3.810d-1, -2.912d-1, -3.516d-2, -3.689d-2 ] ! BIO_VPROFILE_JAPAN data was used
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 7.723d-1, 8.465d-2,  3.162d-3, -6.280d-1, -4.075d-1,  4.817d-1,  1.297d0 ,  1.647d-1 ]
#elif defined BIO_VPROFILE_PALAU
    [ 5.945d-1, 8.812d-2, -8.214d-4, -2.949d-1,  1.242d-1,  3.770d-1, -2.554d-1, -2.798d-2 ] ! BIO_VPROFILE_YAEYAMA data was used
#elif defined BIO_VPROFILE_JAPAN
    [ 9.557d-1, 5.620d-2, -1.996d-3,  3.414d-1,  3.810d-1, -2.912d-1, -3.516d-2, -3.689d-2 ]
#elif defined BIO_VPROFILE_SOUTH_JAPAN
    [ 9.531d-1, 4.960d-2, -3.153d-3,  4.418d-1,  6.432d-1, -6.239d-1, -6.162d-2, -6.516d-2 ]
#else
    [ 9.557d-1, 5.620d-2, -1.996d-3,  3.414d-1,  3.810d-1, -2.912d-1, -3.516d-2, -3.689d-2 ]
#endif

  real(8) :: Tm20, Sm35, Dp1000

  Tm20 = T-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  d13C_DIC_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                     + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000 
  d13C_DIC_Profile = max( d13C_DIC_Profile, -10.0d0 )
  d13C_DIC_Profile = min( d13C_DIC_Profile,  10.0d0 )

  end function d13C_DIC_Profile

!!!*********************************************************************

end module mod_geochem

