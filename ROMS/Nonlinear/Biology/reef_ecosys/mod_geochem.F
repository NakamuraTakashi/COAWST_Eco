
!!!=== Copyright (c) 2012-2025 Takashi NAKAMURA  =====

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
  real(8) function densSW(temp, S)
    real(8), intent(in) :: temp    ! Temperature (oC)
    real(8), intent(in) :: S       ! Salinity (psu)
    real(8) :: dSMOW               ! Density of SMOW (kg/m^3)

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
  real(8) function densW(temp)
    real(8), intent(in) :: temp    ! Temperature (oC)

    densW = 999.84847d0 + 6.337563d-2*temp - 8.523829d-3*temp**2.0d0 &
      + 6.943248d-5*temp**3.0d0 - 3.821216d-7*temp**4.0d0
   
    densW = densW*1.0d-3  !kg/m3 -> g/cm3
    return
  end function densW

!!!***********************************************************************
!!!   CO2system functions
!!!*********************************************************************** 

  real(8)  function pH_fromATCT(AT2, CT2, T, S ) ! pH (total scale)

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
    
    t_oC=T-273.15d0
    IF(t_oC >= 30.0d0) THEN  !!! Error handling
      t_oC = 30.0d0
    ENDIF

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
    IF(t_oC >= 40.0d0) THEN  !!! Error handling
      t_oC = 40.0d0
    ENDIF

    Sc=Acoef - Bcoef*t_oC + Ccoef*t_oC**2.0d0 - Dcoef*t_oC**3.0d0   !Schmidt number for seawater for temperature ranging from 0 to 30 degrees celsius
    kvex=0.31d0*u10*u10*SQRT(660.0d0/Sc) !Gas transfer velocity for O2;Wanninkhof(1992)  (cm/hr)

    Flux_O2 = kvex*(DOw-DOsatu)/100.d0/3600.d0

    return
  end function Flux_O2

! **********************************************************************
!  O2 saturation concentration  (umol L-1)
! **********************************************************************

  real(8) function O2satu(T, S) ! O2 saturation concentration  (umol L-1)

    implicit none

    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)
    
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

    real(8)  t_oC, TS, AA

! alculate O2 saturation concentration using Garcia and Gordon
! &O (1992) formula, (EXP(AA) is in ml/l).
!
    t_oC=T-273.15d0
    
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
  real(8)  function K0_Weiss1974(T, S)
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)

    K0_Weiss1974=exp(-60.2409+93.4517*(100./T)+23.3585*Log(T/100.) &
        +S*(0.023517-0.023656*(T/100.)+0.0047036*(T/100.)**2.))
    return
  end function K0_Weiss1974
!-----------------------------------------------------------------------
! Dissociation constant of carbonic acid by Lueker et al. (2000)
! Total hydrogen ion concentration scale
  real(8)  function K1_Lueker2000(T, S)
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)

    K1_Lueker2000=10.**(-(3633.86/T-61.2172+9.6777*Log(T) &
                        -0.011555*S+0.0001152*S**2.))
    return
  end function K1_Lueker2000
!-----------------------------------------------------------------------
! Dissociation constant of carbonic acid by Lueker et al. (2000)
! Total hydrogen ion concentration scale
  real(8)  function K2_Lueker2000(T, S)
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)

    K2_Lueker2000=10.**(-(471.78/T+25.929-3.16967*Log(T) &
                        -0.01781*S+0.0001122*S**2.))
    return
  end function K2_Lueker2000
!-----------------------------------------------------------------------
! Uppstrom (1974) Deep-Sea Res. 21: 161-162
  real(8)  function BT_Uppstrom1974(S)
    implicit none
    real(8), intent(in) :: S      ! Salinity (psu)

    BT_Uppstrom1974=(0.000232/10.811)*(S/1.80655)
    return
  end function BT_Uppstrom1974
!-----------------------------------------------------------------------
! Dickson (1990) Deep-Sea Res. 37: 755-766
  real(8)  function KB_Dickson1990(T, S)
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)

    KB_Dickson1990=exp((-8966.9-2890.53*S**0.5-77.942*S          &
                        +1.728*S**1.5-0.0996*S**2.)/T            &
                       +(148.0248+137.1942*S**0.5+1.62142*S)     &
                       +(-24.4344-25.085*S**0.5-0.2474*S)*Log(T) &
                       +0.053105*S**0.5*T)
    return
  end function KB_Dickson1990
!-----------------------------------------------------------------------
! Millero (1995) Geochim.Cosmochim Acta 59: 661-677
  real(8)  function Kw_Millero1995(T, S)
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)

    Kw_Millero1995=exp(148.9652-13847.26/T-23.6521*Log(T)       &
                        +(-5.977+118.67/T+1.0495*Log(T))*S**0.5 &
                        - 0.01615*S)
    return
  end function Kw_Millero1995
!-----------------------------------------------------------------------
! Morris & Riley (1966) Deep-Sea Res. 13: 699-705
  real(8)  function ST_MorrisRiley1966(S)
    implicit none
    real(8), intent(in) :: S      ! Salinity (psu)

    ST_MorrisRiley1966=(0.14/96.062)*(S/1.80655)
    return
  end function ST_MorrisRiley1966
!-----------------------------------------------------------------------
! Dickson (1990) J. Chem. Thermodynamics 22: 113-127
!   -- free hydrogen ion scale
  real(8)  function KS_Dickson1990(T, S)
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)
    real(8)  SI

    SI=19.924*S/(1000.-1.005*S)

    KS_Dickson1990 = exp(-4276.1/T+141.328-23.093*Log(T)         &
               +(-13856/T+324.57-47.986*Log(T))*SI**0.5          &
               +(35474/T-771.54+114.723*Log(T))* SI              &
               -2698*SI**1.5/T+1776*SI**2./T+Log(1.-0.001005*S))
    return
  end function KS_Dickson1990
!-----------------------------------------------------------------------
! Weiss (1974) Mar. Chem. 2: 203-215
  real(8)  function FT_Weiss1974(S)
    implicit none
    real(8), intent(in) :: S      ! Salinity (psu)
    
    FT_Weiss1974=(0.000067/18.998)*(S/1.80655)
    
    return
  end function FT_Weiss1974
!-----------------------------------------------------------------------
! Dickson & Riley (1979) Mar. Chem. 7: 89-99
!   -- change pH scale to total
  real(8)  function KF_DicksonRiley1979(T, S)
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)
    real(8)  KF,SI,KS

    KS = KS_Dickson1990(T, S)

    SI = 19.924*S/(1000.-1.005*S)

    KF = 1590.2/T-12.641+1.525*SI**0.5+Log(1.-0.001005*S)
    KF = KF+Log(1.+(0.14/96.062)*(S/1.80655)/KS)
    KF_DicksonRiley1979 = exp(KF)

    return
  end function KF_DicksonRiley1979
!-----------------------------------------------------------------------
! Perez & Fraga (1987) Mar. Chem. 21: 161-168
  real(8)  function KF_PerezFraga1987(T, S)
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)

    KF_PerezFraga1987=exp(874./T-9.68+0.111*S**0.5)
    return
  end function KF_PerezFraga1987
!-----------------------------------------------------------------------
! Mucci (1983) Amer. J. Sci. 282: 780
  real(8)  function Kcal_Mucci1983(T, S)
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)

    Kcal_Mucci1983=10.**(-171.9065-0.077993*T+2839.319/T        &
   &                    +71.595*Log10(T)                        &
   &                    +(-0.77712+0.0028426*T+178.34/T)*S**0.5 &
   &                    - 0.07711*S+0.0041249*S**1.5)
    return
  end function Kcal_Mucci1983
!-----------------------------------------------------------------------
! Mucci (1983) Amer. J. Sci. 282: 780
  real(8)  function Karg_Mucci1983(T, S)
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)

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
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)

    kp1_Johnson1982=exp(1246.98-6.19d4/T-183.0*Log(T))
    return
  end function kp1_Johnson1982
!-----------------------------------------------------------------------
  real(8)  function km1_Johnson1982(T, S)
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
    implicit none
    real(8), intent(in) :: T      ! Temperature (K)
    real(8), intent(in) :: S      ! Salinity (psu)
    real(8), parameter :: R=8.314d0 ! Gas constant (J mol-1)
    
    kp4_Johnson1982=4.7d7*exp(-23.2d3/(R*T))  !23.2 (kJ mol-1)
    return
  end function kp4_Johnson1982
!-----------------------------------------------------------------------
  real(8)  function km4_Johnson1982(T, S)
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

    implicit none
    real(8), intent(in) :: R13C  ! R13C = 13C/12C

    d13C_fromR13C=(R13C/R13C_VPDB-1.d0)*1.d3

    return
  end function d13C_fromR13C


! **********************************************************************
!  R13C from R13C VPDB value.
! **********************************************************************

  real(8) function R13C_fromd13C(d13C)  ! R13C = 13C/12C

    implicit none
    real(8), intent(in) :: d13C ! Carbon isotope ratio (per mill VPDB)

    R13C_fromd13C=(d13C*1.d-3 + 1.d0)*R13C_VPDB

    return
  end function R13C_fromd13C

! **********************************************************************
!  R15N from R15N air value.
! **********************************************************************

  real(8) function R15N_fromd15N(d15N)  ! R15N = 15N/14N

    implicit none
    real(8), intent(in) :: d15N ! Nitrogen isotope ratio (per mill air)

    R15N_fromd15N=(d15N*1.d-3 + 1.d0)*R15N_AIR

    return
  end function R15N_fromd15N

! **********************************************************************
!  Carbon 13 isotope concentrations
! **********************************************************************

  real(8) function concH13CO3(DI13C,c12CO2aq,cH12CO3,c12CO3,T) ! 13C concentration in HCO3- (umol kg-1)

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
    implicit none
    real(8), intent(in) :: T   ! Temperatureture (K)
    a_gb_Mook1986=(-9483./T+23.89)*1.d-3 +1.d0
    return
  end function a_gb_Mook1986

  real(8) function a_dg_Mook1986(T)  ! a(CO2(aq)-CO2(g))
    implicit none
    real(8), intent(in) :: T   ! Temperatureture (K)
    a_dg_Mook1986=(-373./T+0.19)*1.d-3 +1.d0
    return
  end function a_dg_Mook1986

  real(8) function a_db_Mook1986(T)  ! a(CO2(aq)-HCO3-)
    implicit none
    real(8), intent(in) :: T   ! Temperatureture (K)
    a_db_Mook1986=(-9866./T+24.12)*1.d-3 +1.d0
    return
  end function a_db_Mook1986

  real(8) function a_cb_Mook1986(T)  ! a(CO32- -HCO3-)
    implicit none
    real(8), intent(in) :: T   ! Temperatureture (K)
    a_cb_Mook1986=(-867./T+2.52)*1.d-3 +1.d0
    return
  end function a_cb_Mook1986

!-----------------------------------------------------------------------
! Zhang et al. (1995) Geochim. Cosmochim. Acta 59: 107-114

  real(8) function a_bg_Zhang1995(t)  ! a(HCO3--CO2(g))
    implicit none
    real(8), intent(in) :: t   ! Temperatureture (oC)
    a_bg_Zhang1995=(-0.1141*t+10.78)*1.d-3 +1.d0
    return
  end function a_bg_Zhang1995

  real(8) function a_dg_Zhang1995(t)  ! a(CO2(aq)-CO2(g))
    implicit none
    real(8), intent(in) :: t   ! Temperatureture (oC)
    a_dg_Zhang1995=(0.0049*t-1.31)*1.d-3 +1.d0
    return
  end function a_dg_Zhang1995

  real(8) function a_cg_Zhang1995(t)  ! a(CO32--CO2(g))
    implicit none
    real(8), intent(in) :: t   ! Temperatureture (oC)
    a_cg_Zhang1995=(-0.052*t+7.22)*1.d-3 +1.d0
    return
  end function a_cg_Zhang1995

  real(8) function a_cb_Zhang1995(t)  ! a(CO32--HCO3-)
    implicit none
    real(8), intent(in) :: t   ! Temperatureture (oC)
    real a_cg, a_bg
    a_cg = a_cg_Zhang1995(t)
    a_bg = a_bg_Zhang1995(t)
    a_cb_Zhang1995= a_cg/a_bg
    return
  end function a_cb_Zhang1995

  real(8) function a_db_Zhang1995(t)  ! a(CO2aq-HCO3-)
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

    implicit none
    real(8), intent(in) :: R11B  ! R11B = 11B/10B

    d11B_fromR11B = (R11B/R11B_SRM951 - 1.d0)*1.d3

    return
  end function d11B_fromR11B

!----------------------------------------------------------------------
!  R11B from R11B NIST SRM951 value.

  real(8) function R11B_fromd11B(d11B)  ! R11B = 11B/10B

    implicit none
    real(8), intent(in) :: d11B  ! Boron isotope ratio (per mill)

    R11B_fromd11B=(d11B*1.d-3 + 1.d0)*R11B_SRM951

    return
  end function R11B_fromd11B

!----------------------------------------------------------------------
!  Boron isotope fractionation factors (a(B(OH)3-B(OH)4^-))

! Kakihana et al. (1977)

  real(8) function aB_Kakihana1977(T)  ! a(B(OH)3-B(OH)4^-)
    implicit none
    real(8), intent(in) :: T   ! Temperatureture (K)
    real(8) t_oC ! Temperature (oC)
    
    aB_Kakihana1977=1.0194d0
    return
  end function aB_Kakihana1977

! Palmer et al. (1987)

  real(8) function aB_Palmer1987(T)  ! a(B(OH)3-B(OH)4^-)
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
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)

  real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 2003.9,  -11.77, 0.88016,  50.077, -20.498,   45.24,  8.1948,  6.8363 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 2004.8, 0.17266,  1.2439,  18.668, -46.533,   443.0,    2.24,  25.275 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 1980.7, -6.4484,  1.2021, -22.521, -100.78,  141.42,  7.1261,  11.649 ]
#elif defined BIO_VPROFILE_PALAU
    [ 2088.6, -15.218,  0.1884,   78.14,  12.431, -36.077,-0.077502,-0.58378 ]
#elif defined BIO_VPROFILE_JAPAN
    [ 2004.7,  -11.57, 0.93413,  45.795, -46.885,  60.398,  7.9545,  7.77   ]
#else
    [ 2004.7,  -11.57, 0.93413,  45.795, -46.885,  60.398,  7.9545,  7.77   ]
#endif
  
  real(8) :: Tm20, Sm35, Dp1000

  Tm20 = T-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  DIC_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                     + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000 
  DIC_Profile = max( DIC_Profile, 0.0d0 )

  end function DIC_Profile

!--- Total Alikalinity (TA) ------------------------------
!     Based on GLODAPv2.2022

  real(8) function TA_Profile(T, S, d)
    implicit none
    real(8), intent(in) :: T   ! Potential temperatureture (oC)
    real(8), intent(in) :: S   ! Salinity (PSU)
    real(8), intent(in) :: d   ! Depth (m)

    real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
      [ 2301.1, -2.1405, 0.32678,  70.044, -29.691, -28.614,  -1.161, -1.5422 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
      [ 2315.3,  1.8002, 0.36142,  88.544,  9.9163,  48.886, -4.4386,  1.0972 ]
#elif defined BIO_VPROFILE_TOKYOBAY
      [ 2294.8, 0.16538, 0.38731,  46.311, -32.931, -4.3979,  -2.356,-0.86631 ]
#elif defined BIO_VPROFILE_PALAU
      [ 2295.9,-0.51979, 0.21917,  84.811,  7.8105,  38.435, -8.1414,-0.48131 ]
#elif defined BIO_VPROFILE_JAPAN
      [ 2297.4, -1.0691, 0.30873,  68.021, -13.129, -22.406, -3.9028, -2.2066 ]
#else
      [ 2297.4, -1.0691, 0.30873,  68.021, -13.129, -22.406, -3.9028, -2.2066 ]
#endif

  real(8) :: Tm20, Sm35, Dp1000

  Tm20 = T-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  TA_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                    + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000 
  TA_Profile = max( TA_Profile, 0.0d0 )

  end function TA_Profile

!--- Dissolved Oxygen (DO) ------------------------------
!     Based on GLODAPv2.2022

  real(8) function DO_Profile(T, S, d)
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)

  real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 218.07,    3.27, -0.79919,  -5.157,  34.434, -19.565, -13.597, -7.7349 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 218.46, -14.017,  -1.3884, -21.657,  37.863, -706.28, -4.7566, -40.368 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 247.64, -3.2328,  -1.2391,  57.067,  121.26, -168.22, -11.935,  -15.31 ]
#elif defined BIO_VPROFILE_PALAU
    [ 140.64,  7.4234, -0.14649, -7.5404,  -5.715,  82.358, -8.8514,-0.38958 ]
#elif defined BIO_VPROFILE_JAPAN
    [ 223.65,  2.5876, -0.95841,  6.3665,   79.41, -71.675, -15.363, -11.386 ]
#else
    [ 223.65,  2.5876, -0.95841,  6.3665,   79.41, -71.675, -15.363, -11.386 ]
#endif

  real(8) :: Tm20, Sm35, Dp1000
  
  Tm20 = T-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  DO_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                    + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000 
  DO_Profile = max( DO_Profile, 0.0d0 )

  end function DO_Profile

!--- Dissolved Oxygen (DO) ------------------------------
!     TN test

  real(8) function DO_Profile2(SST, SSS, SSDIC, DIC, SSTA, TA)
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
  implicit none
  real(8), intent(in) :: T    ! Potential temperatureture (oC)
  real(8), intent(in) :: S    ! Salinity (PSU)
  real(8), intent(in) :: d    ! Depth (m)
  integer, intent(in) :: m    ! Index of Phytoplankton functional group 

  real(8), parameter :: a(0:8)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 0.22899, -0.044904, 0.0023059,-0.032198,-0.0085123, 0.57808, -16.399,  42.786,   0.279 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 0.8377 , -0.092857, 0.001421 ,  0.62685,   0.20414,  -15.15,  94.699, -117.52, 0.67562 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 0.58184, -0.07835 , 0.0025208,  0.2612 ,   0.17559, -4.4179,  3.1193,  39.441, 0.55383 ]
#elif defined BIO_VPROFILE_PALAU
    [ 0.19481, -0.015131,0.00021533,  0.03188, 0.0063201, 0.87858,  -16.46,  47.271, 0.13102 ]
#elif defined BIO_VPROFILE_JAPAN
    [ 0.26379, -0.036883,0.00098696,-0.0067455,-0.010478,-0.51103, -11.061,  43.275, 0.27491 ]
#else
    [ 0.26379, -0.036883,0.00098696,-0.0067455,-0.010478,-0.51103, -11.061,  43.275, 0.27491 ]
#endif
                                      !  Dnfl   Dtm    Ccl    Cya
  real(8), parameter :: r(4)        = [ 0.4d0, 0.3d0, 0.1d0, 0.2d0 ]
  real(8), parameter :: Chl2PhyC(4) = [ 0.2d0, 0.2d0, 0.2d0, 0.2d0 ] !Conversion ratio from Chl-a to phyC (umolC/L)/(ug/L)

  real(8) :: Tm20, Sm35, Dp1000

  Tm20 = T-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  ! Chl-a concentration (ug/L)
  PhyC_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35           &
                      + a(5)*Dp1000 + a(6)*Dp1000*Dp1000  + a(7)*Dp1000*Dp1000*Dp1000     &
                      + a(7)*Tm20*Dp1000 
  ! PhyC density (umolC/L)
  PhyC_Profile = PhyC_Profile * r(m) * Chl2PhyC(m)              
  PhyC_Profile = max( PhyC_Profile, 0.0d0 )

  end function PhyC_Profile

!--- Phytoplankton -------------------------------------
!     developed by Shinya AMANO, Takashi NAKAMURA

  real(8) function PHY_Profile2(iphy, SSPhyC, d)
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
  implicit none
  real(8), intent(in) :: T    ! Potential temperatureture (oC)
  real(8), intent(in) :: S    ! Salinity (PSU)
  real(8), intent(in) :: d    ! Depth (m)
  integer, intent(in) :: m    ! Index of Phytoplankton functional group 

  real(8), parameter :: a(0:8)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 0.22899, -0.044904, 0.0023059,-0.032198,-0.0085123, 0.57808, -16.399,  42.786,   0.279 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 0.8377 , -0.092857, 0.001421 ,  0.62685,   0.20414,  -15.15,  94.699, -117.52, 0.67562 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 0.58184, -0.07835 , 0.0025208,  0.2612 ,   0.17559, -4.4179,  3.1193,  39.441, 0.55383 ]
#elif defined BIO_VPROFILE_PALAU
    [ 0.19481, -0.015131,0.00021533,  0.03188, 0.0063201, 0.87858,  -16.46,  47.271, 0.13102 ]
#elif defined BIO_VPROFILE_JAPAN
    [ 0.26379, -0.036883,0.00098696,-0.0067455,-0.010478,-0.51103, -11.061,  43.275, 0.27491 ]
#else
    [ 0.26379, -0.036883,0.00098696,-0.0067455,-0.010478,-0.51103, -11.061,  43.275, 0.27491 ]
#endif
                                      !  Dnfl   Dtm    Ccl    Cya
  real(8), parameter :: r(4)        = [ 0.4d0, 0.3d0, 0.1d0, 0.2d0 ]
  real(8), parameter :: Chl2PhyC(4) = [ 0.2d0, 0.2d0, 0.2d0, 0.2d0 ] !Conversion ratio from Chl-a to phyC (umolC/L)/(ug/L)

  real(8) :: Tm20, Sm35, Dp1000

  Tm20 = T-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  ! Chl-a concentration (ug/L)
  ZooC_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35           &
                      + a(5)*Dp1000 + a(6)*Dp1000*Dp1000  + a(7)*Dp1000*Dp1000*Dp1000     &
                      + a(7)*Tm20*Dp1000 
  ! PhyC density (umolC/L)
  ZooC_Profile = ZooC_Profile * Chl2PhyC(m) *0.1d0             
  ZooC_Profile = max( ZooC_Profile, 0.0d0 )

  end function ZooC_Profile

!--- NO3 -------------------------------------
!     Based on GLODAPv2.2022

  real(8) function NO3_Profile(T, S, d)
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)

  real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 0.57862, -0.98093, 0.11813,  0.4974,  0.6834,   5.121,  1.2865, 0.89172 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ -0.67507, 0.37257, 0.15954, -3.0303, -5.3351,  67.176, 0.58408,  3.8603 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ -2.8824, -0.52928, 0.14487, -10.635, -12.349,  16.949,  1.1114,  1.4678 ]
#elif defined BIO_VPROFILE_PALAU
    [  8.0235,  -1.6272,0.069239, -1.2926,-0.38817, -4.7416,  1.5917,  0.4794 ]
#elif defined BIO_VPROFILE_JAPAN
    [ 0.81133,  -1.0881, 0.13916,-0.46799, -7.1662,  9.4007,  1.9147,  1.3848 ]
#else
    [ 0.81133,  -1.0881, 0.13916,-0.46799, -7.1662,  9.4007,  1.9147,  1.3848 ]
#endif

  real(8) :: Tm20, Sm35, Dp1000
  
  Tm20 = min(T,27.0d0)-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  NO3_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                     + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000 
  NO3_Profile = max( NO3_Profile, 0.2d0 )

  end function NO3_Profile

!--- NO3 -------------------------------------
!     developed by Shinya AMANO

  real(8) function NO3_Profile2( d )
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
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)

  real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [ 0.081546, -0.073089, 0.0085317, 0.074849, 0.098112, 0.11621,  0.10087, 0.054868 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [ 0.010694,  0.027332,  0.011494, -0.14294, -0.29705,  4.4996, 0.044849,  0.26221 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ -0.16553, -0.037279,  0.010517, -0.66991, -0.78668,  1.0741,  0.08311,  0.10011 ]
#elif defined BIO_VPROFILE_PALAU
    [  0.66644, -0.11061 , 0.0047956,-0.043324,-0.053875,-0.13164,   0.1098, 0.044095 ]
#elif defined BIO_VPROFILE_JAPAN
    [ 0.080835, -0.074531,  0.010308, -0.08938, -0.57938, 0.66142,  0.14611,  0.10271 ]
#else
    [ 0.080835, -0.074531,  0.010308, -0.08938, -0.57938, 0.66142,  0.14611,  0.10271 ]
#endif

  real(8) :: Tm20, Sm35, Dp1000
  
  Tm20 = min(T,27.0d0)-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  PO4_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                     + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000 
  PO4_Profile = max( PO4_Profile, 0.04d0 )

  end function PO4_Profile

!--- PO4 -------------------------------------
!     developed by Shinya AMANO

  real(8) function PO4_Profile2( d )
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
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)
  Integer, intent(in) :: m   ! Index of DOM category

  real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [  62.599,   1.872,  0.048378,  4.6167,  6.7483, -8.4206,  0.37596,-0.30665 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [  56.171, 0.91687, -0.019579, -26.166,  -9.841,  -55.44, -0.24788, -3.0331 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [  63.563,  1.0345, -0.012349, -22.643, -14.282, -32.301,-0.026612, -1.7206 ]
#elif defined BIO_VPROFILE_PALAU
    [  50.837,   1.889,  0.030908, -10.907, -9.1075,  21.078,  -1.6244, 0.61922 ]
#elif defined BIO_VPROFILE_JAPAN
    [  64.282,   1.303,  0.008285,  7.5523,  12.042, -33.224, 0.036558, -1.7053 ]
#else
    [  64.282,   1.303,  0.008285,  7.5523,  12.042, -33.224, 0.036558, -1.7053 ]
#endif
                                       ! LDOM    RDOM
  real(8), parameter :: r(2)        = [ 0.42d0, 0.58d0 ]

  real(8) :: Tm20, Sm35, Dp1000
 
  Tm20 = T-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  DOC_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                     + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000 
  DOC_Profile = max( DOC_Profile, 0.0d0 )*r(m)

  end function DOC_Profile

!--- POC -------------------------------------
!     Based on ?????????????てきとう。要修正 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real(8) function POC_Profile(T, S, d, m)
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)
  Integer, intent(in) :: m   ! Index of POM category

  real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [  62.599,   1.872,  0.048378,  4.6167,  6.7483, -8.4206,  0.37596,-0.30665 ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [  56.171, 0.91687, -0.019579, -26.166,  -9.841,  -55.44, -0.24788, -3.0331 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [  63.563,  1.0345, -0.012349, -22.643, -14.282, -32.301,-0.026612, -1.7206 ]
#elif defined BIO_VPROFILE_PALAU
    [  50.837,   1.889,  0.030908, -10.907, -9.1075,  21.078,  -1.6244, 0.61922 ]
#elif defined BIO_VPROFILE_JAPAN
    [  64.282,   1.303,  0.008285,  7.5523,  12.042, -33.224, 0.036558, -1.7053 ]
#else
    [  64.282,   1.303,  0.008285,  7.5523,  12.042, -33.224, 0.036558, -1.7053 ]
#endif
                                       ! LPOM    RPOM    CPOM
  real(8), parameter :: r(3)        = [ 0.42d0, 0.58d0, 0.0d0 ]

  real(8) :: Tm20, Sm35, Dp1000
  
  Tm20 = T-20.0d0
  Sm35 = S-35.0d0
  Dp1000 = max(d,0.0d0)*0.001d0

  POC_Profile = a(0) + a(1)*Tm20 + a(2)*Tm20*Tm20 + a(3)*Sm35 + a(4)*Sm35*Sm35  &
                     + a(5)*Dp1000 + a(6)*Dp1000*Dp1000 + a(7)*Tm20*Dp1000 
  POC_Profile = max( POC_Profile, 0.0d0 )*0.1d0*r(m)  ! 10% of DOC

  end function POC_Profile

!--- d13C_DIC -------------------------------------
!     Based on GLODAPv2.2022

  real(8) function d13C_DIC_Profile(T, S, d)
  implicit none
  real(8), intent(in) :: T   ! Potential temperatureture (oC)
  real(8), intent(in) :: S   ! Salinity (PSU)
  real(8), intent(in) :: d   ! Depth (m)

  real(8), parameter :: a(0:7)  = &
#if defined BIO_VPROFILE_YAEYAMA
    [  0.96915, 0.056565, -0.0033155,  0.14265, 0.30931,-0.22066, -0.063338, -0.0452   ]
#elif defined BIO_VPROFILE_SHIZUGAWA
    [  1.3615, 0.0041151, -0.0076296,   0.8062, 0.92911, -1.0461, -0.084518, -0.096642 ]
#elif defined BIO_VPROFILE_TOKYOBAY
    [ 0.99782, 0.053674 , -0.0040557,-0.068709,  0.2929, -0.4597, -0.051394, -0.055209 ]
#elif defined BIO_VPROFILE_PALAU
    [ 0.71132, 0.074679 ,   0.002164, 0.075444,-0.0042372, 0.44914, 0.028641, 0.028598 ]
#elif defined BIO_VPROFILE_JAPAN
    [ 0.91762,  0.075141, -0.002297, -0.14969,  0.035444, 0.37965,-0.049614,-0.0097213 ]
#else
    [ 0.91762,  0.075141, -0.002297, -0.14969,  0.035444, 0.37965,-0.049614,-0.0097213 ]
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

