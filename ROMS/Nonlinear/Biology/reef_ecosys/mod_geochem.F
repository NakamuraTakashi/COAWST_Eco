
!!!=== Copyright (c) 2012-2020 Takashi NAKAMURA  =====

module mod_geochem

  implicit none
! ------ Isotope related parameter -----
  real(8), parameter :: R13C_VPDB = 0.011056d0/0.988944d0
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
!!!   Sediment biogeochemical parameters
!!!*********************************************************************** 
  !     Added by Masa Muto (2023)

  ! ----For all of module which is newer than mod_sedecosys_MM3.F ---------!
  ! ----For all of mod_sedecosys_test().F  ---------!

  !!--- Diffusion coefficient in sediment --- !!
  real(8) function Dsed(Dw,poro)  
    implicit none
    real(8), intent(in) :: Dw   ! diffusion coefficient in pure seawater
    real(8), intent(in) :: poro ! porosity
    Dsed = Dw / (1d0+3d0*(1d0-poro))
    return
  end function Dsed

  !! Fick's law (diffusion)
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
  ! Dissolved compounds
  real(8) function Ficks_sedd(D,C1,C2,dz,poro) ! for solutes
    implicit none
    real(8), intent(in) :: D
    real(8), intent(in) :: C1
    real(8), intent(in) :: C2
    real(8), intent(in) :: dz
    real(8), intent(in) :: poro
    Ficks_sedd = poro * D * (C1 - C2)/dz
    return
  end function Ficks_sedd
  ! Particle compounds
  real(8) function Ficks_sedp(D,C1,C2,dz,poro,rho_s) ! for solids
    implicit none
    real(8), intent(in) :: D
    real(8), intent(in) :: C1
    real(8), intent(in) :: C2
    real(8), intent(in) :: dz
    real(8), intent(in) :: poro
    real(8), intent(in) :: rho_s
    Ficks_sedp = rho_s * (1d0-poro) * D * (C1 - C2)/dz
    return
  end function Ficks_sedp


  !! --- Burial Flux calculation --- !!
  ! Dissolved compounds
  real(8) function FCd(poro,u,C)
    implicit none
    real(8), intent(in) :: poro ! porosity
    real(8), intent(in) :: u    ! deposition rate
    real(8), intent(in) :: C    ! compound concentration
    FCd = poro * u * C
    return
  end function FCd
! Particle compounds
  real(8) function FCp(poro,u,C,rho_s)
    implicit none
    real(8), intent(in) :: poro
    real(8), intent(in) :: u
    real(8), intent(in) :: C
    real(8), intent(in) :: rho_s ! soil density
    FCp = rho_s * (1d0-poro) * u * C
    return
  end function FCp

  !! --- Reaction rate --- !!
  ! reaction with solute * solute
  real(8) function Vdd(k,poro,Cd1,Cd2)
    implicit none
    real(8), intent(in) :: k
    real(8), intent(in) :: poro
    real(8), intent(in) :: Cd1
    real(8), intent(in) :: Cd2
    Vdd = k * poro * Cd1 * Cd2
    return
  end function Vdd
  ! reaction with solid * solute
  real(8) function Vsd(k,poro,Cs1,Cd1,rho_s)
    implicit none
    real(8), intent(in) :: k
    real(8), intent(in) :: poro
    real(8), intent(in) :: Cs1
    real(8), intent(in) :: Cd1
    real(8), intent(in) :: rho_s
    Vsd = k * rho_s * (1d0 - poro) * Cs1 * Cd1
    return
  end function Vsd
  ! reaction with solid * solid
  real(8) function Vss(k,poro,Cs1,Cs2,rho_s)
    implicit none
    real(8), intent(in) :: k
    real(8), intent(in) :: poro
    real(8), intent(in) :: Cs1
    real(8), intent(in) :: Cs2
    real(8), intent(in) :: rho_s
    Vss = k * ((rho_s * (1d0 - poro))**2) * Cs1 * Cs2
    return
  end function Vss

  !! Mass balance calculation in sediment column
  !! solutes (dissolved)
  real(8) function massbalance_sed_d(C,FTin,FTout,R,dz,poro,dt)
    implicit none
    real(8),intent(in) :: C        ! concentration                   (µmol L-1 = nmol cm-3)
    real(8),intent(in) :: FTin     ! total flux (IN)                 (nmol cm-2 s-1)
    real(8),intent(in) :: FTout    ! total flux (OUT)                (nmol cm-2 s-1)
    real(8),intent(in) :: R        ! Net prodution and consumption   (nmol cm-3 s-1)
    real(8),intent(in) :: dz       ! layer thickness                 (cm)
    real(8),intent(in) :: poro     ! porosity                        (cm_w3 cm_s-3)
    real(8),intent(in) :: dt       ! time step                       (s)
    real(8) :: Ccal

       Ccal    =      C      +  ((FTin - FTout) / dz    +        R       )/   poro        *  dt
!  [nmol cm-3]    [nmol cm-3]   ([nmol cm-2 s-1]/(cm)   + [nmol cm-3 s-1])/[cm_w3 cm_s-3] * [s]
    Ccal = max(Ccal,0.0d0)
    ! Ccal = min(Ccal,3.0d4)
    ! Ccal = min(Ccal,1.0d4)
    ! massbalance_sed_d = min(Ccal,1.0d4)
    massbalance_sed_d = Ccal

    return
  end function massbalance_sed_d

  !! solids (particulate)
  real(8) function massbalance_sed_p(C,FTin,FTout,R,dz,poro,dt,rho_s)
    implicit none
    real(8),intent(in) :: C        ! concentration                  (nmol g-1)
    real(8),intent(in) :: FTin     ! total flux  (IN)               (nmol cm-2 s-1)
    real(8),intent(in) :: FTout    ! total flux  (OUT)              (nmol cm-2 s-1)
    real(8),intent(in) :: R        ! Net prodution and consumption  (nmol cm-3 s-1)
    real(8),intent(in) :: dz       ! layer thickness                (cm)
    real(8),intent(in) :: poro     ! porosity                       (cm_w3 cm_s-3)
    real(8),intent(in) :: dt       ! time step                      (s)
    real(8),intent(in) :: rho_s    ! soil(wet) density              (g cm_w-3)
    real(8) :: Ccal

       Ccal     =      C    +    ( (FTin -FTout)  /dz  +       R        )/(rho_s      *  (1d0 - poro) ) * dt
  ! [nmol g-1]      [nmol g-1] + ([nmol cm-2 s-1]/[cm] + [nmol cm-3 s-1])/([g cm_w-3] * [cm_w3 cm_s-3]) * [s]
    Ccal = max(Ccal,0.0d0)
    ! Ccal = min(Ccal,1.0d4)
    ! Ccal = min(Ccal,1.0d7)
    ! massbalance_sed_p = min(Ccal,1.0d4)
    massbalance_sed_p = Ccal

    return
  end function massbalance_sed_p


  ! ------------For mod_sedecosys_MM2.F --------------!
  !! almost out of use
  !---O2---!
  real(8) function DiffsedO2(temp,poro) !Diffuison coefficient of O2 (cm2 s-1)
    implicit none
    real(8), intent(in) :: temp   !temperature
    real(8), intent(in) :: poro   !porosity

    DiffsedO2 = (11.7d0 + 0.334d0*temp + 0.00505d0*temp**2.0d0) / (1.0d0+3.0d0*(1.0d0- poro)) *10d-6 
    return
  end function DiffsedO2
  !---NO3---!
  real(8) function DiffsedNO3(temp,poro) !Diffusion coefficient of NO3 (cm2 s-1)
    implicit none
    real(8), intent(in) :: temp   !temperature
    real(8), intent(in) :: poro   !porosity

    DiffsedNO3 = (9.72d0 + 0.365d0*temp) / (1.0d0+3.0d0*(1.0d0- poro)) *10d-6  
    return
  end function DiffsedNO3
  !---NH4---!
  real(8) function DiffsedNH4(temp,poro) !Diffusion coefficient of NH4 (cm2 s-1)
    implicit none
    real(8), intent(in) :: temp   !temperature
    real(8), intent(in) :: poro   !porosity

    DiffsedNH4 = (9.76d0 + 0.398d0*temp) / (1.0d0+3.0d0*(1.0d0- poro)) *10d-6 
    return
  end function DiffsedNH4
  !---PO4---!
  real(8) function DiffsedPO4(temp,poro) !Diffusion coefficient of PO4 (cm2 s-1)
    implicit none
    real(8), intent(in) :: temp   !temperature
    real(8), intent(in) :: poro   !porosity

    DiffsedPO4 = (9.76d0 + 0.398d0*temp) / (1.0d0+3.0d0*(1.0d0- poro)) *10d-6 
    return
  end function DiffsedPO4
  !---Mn2+---!

  !---Fe2+---! 

  !---SO4---!

  !---H2S---!


!!!*********************************************************************
!!!***********************************************************************
!!!   Empirical vertical profiles of biogeochemical parameters
!!!*********************************************************************** 

!--- Dissolved Inorganic Carbon (DIC) ----------------
!     developed by Faisal AMRI

  real(8) function DIC_Profile(t_oC)
    implicit none
    real(8), intent(in) :: t_oC   ! Temperatureture (oC)
    
    DIC_Profile = 2312.116d0+(10.68979d0*t_oC)                &
      - (3.50371d0*(t_oC**2)) + (0.162252d0*(t_oC**3))        &
      - (0.00242d0*(t_oC**4))    

  end function DIC_Profile

!--- Total Alikalinity (TA) ------------------------------
!     developed by Faisal AMRI

  real(8) function TA_Profile(t_oC)
    implicit none
    real(8), intent(in) :: t_oC   ! Temperatureture (oC)
    
    TA_Profile = 2444.733d0-(22.2978d0*t_oC)                  &
      + (0.884354d0*(t_oC**2)) + (0.001281d0*(t_oC**3))       &
      - (0.00046d0*(t_oC**4))    

  end function TA_Profile

!--- Dissolved Oxygen (DO) ------------------------------
!     developed by Faisal AMRI

  real(8) function DO_Profile(t_oC)
    implicit none
    real(8), intent(in) :: t_oC   ! Temperatureture (oC)
    
    DO_Profile = 245.8469d0 - (52.0103d0*t_oC)                &
      + (6.461959d0*(t_oC**2)) - (0.27842d0*(t_oC**3))        &
      + (0.003842d0*(t_oC**4))   

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

!--- Phytoplankton_01 -------------------------------------
!     developed by Shinya AMANO

  real(8) function PHY1_Profile2(d)
    implicit none
    real(8), intent(in) :: d   ! elevation (m) = -depth

    IF (d.gt.-155.0d0) THEN
      PHY1_Profile2=(-0.00095d0*(d+50.0d0)**2 + 10.5d0)/24.0d0 *0.2d0    ! umol L-1
    ELSE IF (d.le.-155.0d0) THEN
      PHY1_Profile2=0.0d0     ! umol L-1
    END IF

  end function PHY1_Profile2

!--- Phytoplankton_02 -------------------------------------
!     developed by Shinya AMANO

  real(8) function PHY2_Profile2(d)
    implicit none
    real(8), intent(in) :: d   ! elevation (m) = -depth  
 
    IF (d.gt.-155.0d0) THEN
      PHY2_Profile2=(-0.00095d0*(d+50.0d0)**2 + 10.5d0)/24.0d0     ! umol L-1
    ELSE IF (d.le.-155d0) THEN
      PHY2_Profile2=0.0d0     ! umol L-1
    END IF

  end function PHY2_Profile2

!--- Phytoplankton_03 -------------------------------------
!     developed by Shinya AMANO

  real(8) function PHY3_Profile2(d)
    implicit none
    real(8), intent(in) :: d   ! elevation (m) = -depth  
 
    IF (d.gt.-155.0d0) THEN
      PHY3_Profile2=(-0.00095d0*(d+50.0d0)**2 + 10.5d0)/24.0d0 * 0.8d0    ! umol L-1
    ELSE IF (d.le.-155d0) THEN
      PHY3_Profile2=0.0d0     ! umol L-1
    END IF

  end function PHY3_Profile2


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

  real(8) function ZOO_Profile2(d)
    implicit none
    real(8), intent(in) :: d   ! elevation (m) = -depth

    ZOO_Profile2 = max( 0.25d0+(d/2000.0d0), 0.0d0 )    ! umolC L-1

  end function ZOO_Profile2

!--- NO3 -------------------------------------
!     developed by Faisal AMRI

  real(8) function NO3_Profile( DIC )
    implicit none
    real(8), intent(in) :: DIC   ! DIC concentration (umol kg-1) 
 
    NO3_Profile = ( DIC - 1968.4d0 )/9.2d0
    IF( NO3_Profile .LE. 0.0d0 )THEN
      NO3_Profile = 0.0d0
    END IF

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
!     developed by Faisal AMRI

  real(8) function PO4_Profile( NO3 )
    implicit none
    real(8), intent(in) :: NO3   ! DIC concentration (umol kg-1) 
 
    PO4_Profile = ( NO3 + 0.7764d0 )/14.36d0       
    IF ( PO4_Profile .LE. 0.0d0 )THEN
      PO4_Profile = 0.0d0
    END IF

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

!!!*********************************************************************

end module mod_geochem

