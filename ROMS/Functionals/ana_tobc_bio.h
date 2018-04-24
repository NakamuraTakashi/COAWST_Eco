      SUBROUTINE ana_tobc_bio (ng, tile, model)
!
!! svn $Id$
!!======================================================================
!! Copyright (c) 2002-2015 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets tracer-type variables open boundary conditions    !
!  using analytical expressions.                                       !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_boundary
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping
!
! Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model

#include "tile.h"
!
      CALL ana_tobc_bio_tile (ng, tile, model,                              &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    IminS, ImaxS, JminS, JmaxS,                   &
     &                    nstp(ng),                                     &
     &                    GRID(ng) % z_r,                               &
     &                    OCEAN(ng) % t)
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME(34)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_tobc_bio
!
!***********************************************************************
      SUBROUTINE ana_tobc_bio_tile (ng, tile, model,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          nstp,                                   &
     &                          z_r, t)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
      USE mod_boundary
      USE mod_ncparam
      USE mod_ocean
#ifdef SEDIMENT
      USE mod_sediment
#endif
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
#ifdef REEF_ECOSYS
      USE mod_biology
      USE mod_geochem
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp

#ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: t(LBi:,LBj:,:,:,:)
#else
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#endif
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>SA:Add
#ifdef REEF_ECOSYS
      real(r8) :: c_NO3_s, c_NO3_sc, c_NO3_d, c_NO3_dc, c_PO4_s, c_PO4_sc, c_PO4_d, c_PO4_dc
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<SA:Add
!
!  Local variable declarations.
!
      integer :: i, ised, itrc, j, k
      real(r8) :: cff

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Tracers open boundary conditions.
!-----------------------------------------------------------------------
!
#ifdef SHIRAHO_REEF
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
            BOUNDARY(ng)%t_east(j,k,itemp)=T0(ng)
            BOUNDARY(ng)%t_east(j,k,isalt)=S0(ng)
#  ifdef SEDIMENT
            BOUNDARY(ng)%t_east(j,k,idmud(1))=0.0_r8  !(kg/m3 = g/L)
#  endif
#  ifdef REEF_ECOSYS
            BOUNDARY(ng)%t_east(j,k,iTIC_)=TIC_0(ng)
            BOUNDARY(ng)%t_east(j,k,iTAlk)=TAlk0(ng)
            BOUNDARY(ng)%t_east(j,k,iOxyg)=Oxyg0(ng)
#   if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_east(j,k,iDOC_)=DOC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iPOC_)=POC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iPhy1)=Phy10(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iPhy2)=Phy20(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iZoop)=Zoop0(ng)     ! umolC L-1
#   endif
#   if defined CARBON_ISOTOPE
            BOUNDARY(ng)%t_east(j,k,iT13C)=R13C_fromd13C( d13C_TIC0(ng) )*TIC_0(ng) ! umol kg-1  !!! R13C_fromd13C included geochem module
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_east(j,k,iDO13)=R13C_fromd13C( d13C_DOC0(ng) )*DOC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_east(j,k,iPO13)=R13C_fromd13C( d13C_POC0(ng) )*POC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_east(j,k,iPh13)=R13C_fromd13C( d13C_Phy0(ng) )*Phyt0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_east(j,k,iZo13)=R13C_fromd13C( d13C_Zoo0(ng) )*Zoop0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
#    endif
#   endif
#   if defined NUTRIENTS
            BOUNDARY(ng)%t_east(j,k,iNO3_)=NO3_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_east(j,k,iNO2_)=NO2_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_east(j,k,iNH4_)=NH4_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_east(j,k,iPO4_)=PO4_0(ng)     ! umol L-1
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_east(j,k,iDON_)=DON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_east(j,k,iPON_)=PON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_east(j,k,iDOP_)=DOP_0(ng)     ! umolP L-1
            BOUNDARY(ng)%t_east(j,k,iPOP_)=POP_0(ng)     ! umolP L-1
#    endif
#   endif
#   if defined COT_STARFISH
            BOUNDARY(ng)%t_east(j,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iCOTl)=COTl0(ng)     ! umolC L-1
#   endif
#  endif

          END DO
        END DO
      END IF
      
! YAEYAMA2 case
#elif defined YAEYAMA2
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>SA:Add
      c_NO3_s=-0.0508_r8
      c_NO3_sc=-2.9698_r8
      c_NO3_d=0.0018_r8
      c_NO3_dc=43.891_r8
      c_PO4_s=-0.0037_r8
      c_PO4_sc=-0.1513_r8
      c_PO4_d=0.0002_r8
      c_PO4_dc=3.3429_r8
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<SA:Add
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
#  ifdef SEDIMENT
            BOUNDARY(ng)%t_east(j,k,idmud(1))=0.0_r8  !(kg/m3 = g/L)
#  endif
#  ifdef REEF_ECOSYS
            BOUNDARY(ng)%t_east(j,k,iTIC_)=TIC_0(ng)
            BOUNDARY(ng)%t_east(j,k,iTAlk)=TAlk0(ng)
            BOUNDARY(ng)%t_east(j,k,iOxyg)=Oxyg0(ng)
#   if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_east(j,k,iDOC_)=DOC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iPOC_)=POC_0(ng)     ! umolC L-1
#    if defined SIMPLE_BIO_BOUNDARY
            BOUNDARY(ng)%t_east(j,k,iPhy1)=Phy10(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iPhy2)=Phy20(ng)     ! umolC L-1
#    else
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>SA:Add
            IF (z_r(Iend,j,k).gt.-155_r8) THEN
              BOUNDARY(ng)%t_east(j,k,iPhy1)=(-0.00095_r8*(z_r(i,j,k)+50.0_r8)**2+10.5_r8)/24.0_r8     ! umol L-1
            ELSE IF (z_r(Iend,j,k).le.-155_r8) THEN
              BOUNDARY(ng)%t_east(j,k,iPhy1)=0.0_r8     ! umol L-1
            END IF
            IF (z_r(Iend,j,k).gt.-155_r8) THEN
              BOUNDARY(ng)%t_east(j,k,iPhy2)=(-0.00095_r8*(z_r(i,j,k)+50.0_r8)**2+10.5_r8)/24.0_r8     ! umol L-1
            ELSE IF (z_r(Iend,j,k).le.-155_r8) THEN
              BOUNDARY(ng)%t_east(j,k,iPhy2)=0.0_r8     ! umol L-1
            END IF
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<SA:Add
#    endif
            BOUNDARY(ng)%t_east(j,k,iZoop)=Zoop0(ng)     ! umolC L-1
#   endif
#   if defined CARBON_ISOTOPE
            BOUNDARY(ng)%t_east(j,k,iT13C)=R13C_fromd13C( d13C_TIC0(ng) )*TIC_0(ng) ! umol kg-1  !!! R13C_fromd13C included geochem module
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_east(j,k,iDO13)=R13C_fromd13C( d13C_DOC0(ng) )*DOC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_east(j,k,iPO13)=R13C_fromd13C( d13C_POC0(ng) )*POC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_east(j,k,iPh13)=R13C_fromd13C( d13C_Phy0(ng) )*Phyt0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_east(j,k,iZo13)=R13C_fromd13C( d13C_Zoo0(ng) )*Zoop0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
#    endif
#   endif
#   if defined NUTRIENTS
            BOUNDARY(ng)%t_east(j,k,iNO3_)=NO3_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_east(j,k,iNO2_)=NO2_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_east(j,k,iNH4_)=NH4_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_east(j,k,iPO4_)=PO4_0(ng)     ! umol L-1
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>SA:Add
            IF (z_r(Iend,j,k).gt.-63.9196_r8) THEN
              BOUNDARY(ng)%t_east(j,k,iNO3_)=NO3_0(ng)                      ! umol L-1
            ELSE IF (z_r(Iend,j,k).gt.-900_r8) THEN
              BOUNDARY(ng)%t_east(j,k,iNO3_)=c_NO3_s*z_r(Iend,j,k)+c_NO3_sc ! umol L-1
            ELSE IF (z_r(Iend,j,k).le.-900_r8) THEN
              BOUNDARY(ng)%t_east(j,k,iNO3_)=c_NO3_d*z_r(Iend,j,k)+c_NO3_dc ! umol L-1
            END IF
            IF (z_r(Iend,j,k).gt.-52.716_r8) THEN
              BOUNDARY(ng)%t_east(j,k,iPO4_)=PO4_0(ng)                      ! umol L-1
            ELSE IF (z_r(Iend,j,k).gt.-950_r8) THEN
              BOUNDARY(ng)%t_east(j,k,iPO4_)=c_PO4_s*z_r(Iend,j,k)+c_PO4_sc ! umol L-1
            ELSE IF (z_r(Iend,j,k).le.-950_r8) THEN
              BOUNDARY(ng)%t_east(j,k,iPO4_)=c_PO4_d*z_r(Iend,j,k)+c_PO4_dc ! umol L-1
            END IF
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<SA:Add
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_east(j,k,iDON_)=DON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_east(j,k,iPON_)=PON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_east(j,k,iDOP_)=DOP_0(ng)     ! umolP L-1
            BOUNDARY(ng)%t_east(j,k,iPOP_)=POP_0(ng)     ! umolP L-1
#    endif
#   endif
#   if defined COT_STARFISH
            BOUNDARY(ng)%t_east(j,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iCOTl)=COTl0(ng)     ! umolC L-1
#   endif
#  endif

          END DO
        END DO
      END IF
      IF (ANY(LBC(iwest,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Western_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
#  ifdef SEDIMENT
            BOUNDARY(ng)%t_west(j,k,idmud(1))=0.0_r8  !(kg/m3 = g/L)
#  endif
#  ifdef REEF_ECOSYS
            BOUNDARY(ng)%t_west(j,k,iTIC_)=TIC_0(ng)
            BOUNDARY(ng)%t_west(j,k,iTAlk)=TAlk0(ng)
            BOUNDARY(ng)%t_west(j,k,iOxyg)=Oxyg0(ng)
#   if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_west(j,k,iDOC_)=DOC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iPOC_)=POC_0(ng)     ! umolC L-1
#    if defined SIMPLE_BIO_BOUNDARY
            BOUNDARY(ng)%t_west(j,k,iPhy1)=Phy10(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iPhy2)=Phy20(ng)     ! umolC L-1
#    else
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>SA:Add
            IF (z_r(Istr,j,k).gt.-155_r8) THEN
              BOUNDARY(ng)%t_west(j,k,iPhy1)=(-0.00095_r8*(z_r(i,j,k)+50.0_r8)**2+10.5_r8)/24.0_r8     ! umol L-1
            ELSE IF (z_r(Istr,j,k).le.-155_r8) THEN
              BOUNDARY(ng)%t_west(j,k,iPhy1)=0.0_r8     ! umol L-1
            END IF
            IF (z_r(Istr,j,k).gt.-155_r8) THEN
              BOUNDARY(ng)%t_west(j,k,iPhy2)=(-0.00095_r8*(z_r(i,j,k)+50.0_r8)**2+10.5_r8)/24.0_r8     ! umol L-1
            ELSE IF (z_r(Istr,j,k).le.-155_r8) THEN
              BOUNDARY(ng)%t_west(j,k,iPhy2)=0.0_r8     ! umol L-1
            END IF
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<SA:Add
#    endif
            BOUNDARY(ng)%t_west(j,k,iZoop)=Zoop0(ng)     ! umolC L-1
#   endif
#   if defined CARBON_ISOTOPE
            BOUNDARY(ng)%t_west(j,k,iT13C)=R13C_fromd13C( d13C_TIC0(ng) )*TIC_0(ng) ! umol kg-1  !!! R13C_fromd13C included geochem module
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_west(j,k,iDO13)=R13C_fromd13C( d13C_DOC0(ng) )*DOC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_west(j,k,iPO13)=R13C_fromd13C( d13C_POC0(ng) )*POC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_west(j,k,iPh13)=R13C_fromd13C( d13C_Phy0(ng) )*Phyt0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_west(j,k,iZo13)=R13C_fromd13C( d13C_Zoo0(ng) )*Zoop0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
#    endif
#   endif
#   if defined NUTRIENTS
!            BOUNDARY(ng)%t_west(j,k,iNO3_)=NO3_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_west(j,k,iNO2_)=NO2_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_west(j,k,iNH4_)=NH4_0(ng)     ! umol L-1
!            BOUNDARY(ng)%t_west(j,k,iPO4_)=PO4_0(ng)     ! umol L-1
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>SA:Add
            IF (z_r(Istr,j,k).gt.-63.9196_r8) THEN
              BOUNDARY(ng)%t_west(j,k,iNO3_)=NO3_0(ng)                      ! umol L-1
            ELSE IF (z_r(Istr,j,k).gt.-900_r8) THEN
              BOUNDARY(ng)%t_west(j,k,iNO3_)=c_NO3_s*z_r(Istr,j,k)+c_NO3_sc ! umol L-1
            ELSE IF (z_r(Istr,j,k).le.-900_r8) THEN
              BOUNDARY(ng)%t_west(j,k,iNO3_)=c_NO3_d*z_r(Istr,j,k)+c_NO3_dc ! umol L-1
            END IF
            IF (z_r(Istr,j,k).gt.-52.716_r8) THEN
              BOUNDARY(ng)%t_west(j,k,iPO4_)=PO4_0(ng)                      ! umol L-1
            ELSE IF (z_r(Istr,j,k).gt.-950_r8) THEN
              BOUNDARY(ng)%t_west(j,k,iPO4_)=c_PO4_s*z_r(Istr,j,k)+c_PO4_sc ! umol L-1
            ELSE IF (z_r(Istr,j,k).le.-950_r8) THEN
              BOUNDARY(ng)%t_west(j,k,iPO4_)=c_PO4_d*z_r(Istr,j,k)+c_PO4_dc ! umol L-1
            END IF
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<SA:Add
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_west(j,k,iDON_)=DON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_west(j,k,iPON_)=PON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_west(j,k,iDOP_)=DOP_0(ng)     ! umolP L-1
            BOUNDARY(ng)%t_west(j,k,iPOP_)=POP_0(ng)     ! umolP L-1
#    endif
#   endif
#   if defined COT_STARFISH
            BOUNDARY(ng)%t_west(j,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iCOTl)=COTl0(ng)     ! umolC L-1
#   endif
#  endif
          END DO
        END DO
      END IF

      IF (ANY(LBC(isouth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Southern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
#  ifdef SEDIMENT
            BOUNDARY(ng)%t_south(i,k,idmud(1))=0.0_r8  !(kg/m3 = g/L)
#  endif
#  ifdef REEF_ECOSYS
            BOUNDARY(ng)%t_south(i,k,iTIC_)=TIC_0(ng)
            BOUNDARY(ng)%t_south(i,k,iTAlk)=TAlk0(ng)
            BOUNDARY(ng)%t_south(i,k,iOxyg)=Oxyg0(ng)
#   if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_south(i,k,iDOC_)=DOC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iPOC_)=POC_0(ng)     ! umolC L-1
#    if defined SIMPLE_BIO_BOUNDARY
            BOUNDARY(ng)%t_south(i,k,iPhy1)=Phy10(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iPhy2)=Phy20(ng)     ! umolC L-1
#    else
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>SA:Add
            IF (z_r(i,Jstr,k).gt.-155_r8) THEN
              BOUNDARY(ng)%t_south(i,k,iPhy1)=(-0.00095_r8*(z_r(i,j,k)+50.0_r8)**2+10.5_r8)/24.0_r8     ! umol L-1
            ELSE IF (z_r(i,Jstr,k).le.-155_r8) THEN
              BOUNDARY(ng)%t_south(i,k,iPhy1)=0.0_r8     ! umol L-1
            END IF
            IF (z_r(i,Jstr,k).gt.-155_r8) THEN
              BOUNDARY(ng)%t_south(i,k,iPhy2)=(-0.00095_r8*(z_r(i,j,k)+50.0_r8)**2+10.5_r8)/24.0_r8     ! umol L-1
            ELSE IF (z_r(i,Jstr,k).le.-155_r8) THEN
              BOUNDARY(ng)%t_south(i,k,iPhy2)=0.0_r8     ! umol L-1
            END IF
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<SA:Add
#    endif
            BOUNDARY(ng)%t_south(i,k,iZoop)=Zoop0(ng)     ! umolC L-1
#   endif
#   if defined CARBON_ISOTOPE
            BOUNDARY(ng)%t_south(i,k,iT13C)=R13C_fromd13C( d13C_TIC0(ng) )*TIC_0(ng) ! umol kg-1  !!! R13C_fromd13C included geochem module
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_south(i,k,iDO13)=R13C_fromd13C( d13C_DOC0(ng) )*DOC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_south(i,k,iPO13)=R13C_fromd13C( d13C_POC0(ng) )*POC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_south(i,k,iPh13)=R13C_fromd13C( d13C_Phy0(ng) )*Phyt0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_south(i,k,iZo13)=R13C_fromd13C( d13C_Zoo0(ng) )*Zoop0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
#    endif
#   endif
#   if defined NUTRIENTS
!            BOUNDARY(ng)%t_south(i,k,iNO3_)=NO3_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_south(i,k,iNO2_)=NO2_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_south(i,k,iNH4_)=NH4_0(ng)     ! umol L-1
!            BOUNDARY(ng)%t_south(i,k,iPO4_)=PO4_0(ng)     ! umol L-1
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>SA:Add
            IF (z_r(i,Jstr,k).gt.-63.9196_r8) THEN
              BOUNDARY(ng)%t_south(i,k,iNO3_)=NO3_0(ng)                      ! umol L-1
            ELSE IF (z_r(i,Jstr,k).gt.-900_r8) THEN
              BOUNDARY(ng)%t_south(i,k,iNO3_)=c_NO3_s*z_r(i,Jstr,k)+c_NO3_sc ! umol L-1
            ELSE IF (z_r(i,Jstr,k).le.-900_r8) THEN
              BOUNDARY(ng)%t_south(i,k,iNO3_)=c_NO3_d*z_r(i,Jstr,k)+c_NO3_dc ! umol L-1
            END IF
            IF (z_r(i,Jstr,k).gt.-52.716_r8) THEN
              BOUNDARY(ng)%t_south(i,k,iPO4_)=PO4_0(ng)                      ! umol L-1
            ELSE IF (z_r(i,Jstr,k).gt.-950_r8) THEN
              BOUNDARY(ng)%t_south(i,k,iPO4_)=c_PO4_s*z_r(i,Jstr,k)+c_PO4_sc ! umol L-1
            ELSE IF (z_r(i,Jstr,k).le.-950_r8) THEN
              BOUNDARY(ng)%t_south(i,k,iPO4_)=c_PO4_d*z_r(i,Jstr,k)+c_PO4_dc ! umol L-1
            END IF
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<SA:Add
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_south(i,k,iDON_)=DON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_south(i,k,iPON_)=PON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_south(i,k,iDOP_)=DOP_0(ng)     ! umolP L-1
            BOUNDARY(ng)%t_south(i,k,iPOP_)=POP_0(ng)     ! umolP L-1
#    endif
#   endif
#   if defined COT_STARFISH
            BOUNDARY(ng)%t_south(i,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iCOTl)=COTl0(ng)     ! umolC L-1
#   endif
#  endif
          END DO
        END DO
      END IF

      IF (ANY(LBC(inorth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Northern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
#  ifdef SEDIMENT
            BOUNDARY(ng)%t_north(i,k,idmud(1))=0.0_r8  !(kg/m3 = g/L)
#  endif
#  ifdef REEF_ECOSYS
            BOUNDARY(ng)%t_north(i,k,iTIC_)=TIC_0(ng)
            BOUNDARY(ng)%t_north(i,k,iTAlk)=TAlk0(ng)
            BOUNDARY(ng)%t_north(i,k,iOxyg)=Oxyg0(ng)
#   if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_north(i,k,iDOC_)=DOC_0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iPOC_)=POC_0(ng)     ! umolC L-1
#    if defined SIMPLE_BIO_BOUNDARY
            BOUNDARY(ng)%t_north(i,k,iPhy1)=Phy10(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iPhy2)=Phy20(ng)     ! umolC L-1
#    else
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>SA:Add
            IF (z_r(i,Jend,k).gt.-155_r8) THEN
              BOUNDARY(ng)%t_north(i,k,iPhy1)=(-0.00095_r8*(z_r(i,j,k)+50.0_r8)**2+10.5_r8)/24.0_r8     ! umol L-1
            ELSE IF (z_r(i,Jend,k).le.-155_r8) THEN
              BOUNDARY(ng)%t_north(i,k,iPhy1)=0.0_r8     ! umol L-1
            END IF
            IF (z_r(i,Jend,k).gt.-155_r8) THEN
              BOUNDARY(ng)%t_north(i,k,iPhy2)=(-0.00095_r8*(z_r(i,j,k)+50.0_r8)**2+10.5_r8)/24.0_r8     ! umol L-1
            ELSE IF (z_r(i,Jend,k).le.-155_r8) THEN
              BOUNDARY(ng)%t_north(i,k,iPhy2)=0.0_r8     ! umol L-1
            END IF
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<SA:Add
#    endif
            BOUNDARY(ng)%t_north(i,k,iZoop)=Zoop0(ng)     ! umolC L-1
#   endif
#   if defined CARBON_ISOTOPE
            BOUNDARY(ng)%t_north(i,k,iT13C)=R13C_fromd13C( d13C_TIC0(ng) )*TIC_0(ng) ! umol kg-1  !!! R13C_fromd13C included geochem module
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_north(i,k,iDO13)=R13C_fromd13C( d13C_DOC0(ng) )*DOC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_north(i,k,iPO13)=R13C_fromd13C( d13C_POC0(ng) )*POC_0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_north(i,k,iPh13)=R13C_fromd13C( d13C_Phy0(ng) )*Phyt0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
            BOUNDARY(ng)%t_north(i,k,iZo13)=R13C_fromd13C( d13C_Zoo0(ng) )*Zoop0(ng) ! umol L-1  !!! R13C_fromd13C included geochem module
#    endif
#   endif
#   if defined NUTRIENTS
!            BOUNDARY(ng)%t_north(i,k,iNO3_)=NO3_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_north(i,k,iNO2_)=NO2_0(ng)     ! umol L-1
            BOUNDARY(ng)%t_north(i,k,iNH4_)=NH4_0(ng)     ! umol L-1
!            BOUNDARY(ng)%t_north(i,k,iPO4_)=PO4_0(ng)     ! umol L-1
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>SA:Add
            IF (z_r(i,Jend,k).gt.-63.9196_r8) THEN
              BOUNDARY(ng)%t_north(i,k,iNO3_)=NO3_0(ng)                      ! umol L-1
            ELSE IF (z_r(i,Jend,k).gt.-900_r8) THEN
              BOUNDARY(ng)%t_north(i,k,iNO3_)=c_NO3_s*z_r(i,Jend,k)          ! umol L-1
            ELSE IF (z_r(i,Jend,k).le.-900_r8) THEN
              BOUNDARY(ng)%t_north(i,k,iNO3_)=c_NO3_d*z_r(i,Jend,k)+c_NO3_dc ! umol L-1
            END IF
            IF (z_r(i,Jend,k).gt.-52.716_r8) THEN
              BOUNDARY(ng)%t_north(i,k,iPO4_)=PO4_0(ng)                      ! umol L-1
            ELSE IF (z_r(i,Jend,k).gt.-950_r8) THEN
              BOUNDARY(ng)%t_north(i,k,iPO4_)=c_PO4_s*z_r(i,Jend,k)+c_PO4_sc ! umol L-1
            ELSE IF (z_r(i,Jend,k).le.-950_r8) THEN
              BOUNDARY(ng)%t_north(i,k,iPO4_)=c_PO4_d*z_r(i,Jend,k)+c_PO4_dc ! umol L-1
            END IF
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<SA:Add
#    if defined ORGANIC_MATTER
            BOUNDARY(ng)%t_north(i,k,iDON_)=DON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_north(i,k,iPON_)=PON_0(ng)     ! umolN L-1
            BOUNDARY(ng)%t_north(i,k,iDOP_)=DOP_0(ng)     ! umolP L-1
            BOUNDARY(ng)%t_north(i,k,iPOP_)=POP_0(ng)     ! umolP L-1
#    endif
#   endif
#   if defined COT_STARFISH
            BOUNDARY(ng)%t_north(i,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iCOTl)=COTl0(ng)     ! umolC L-1
#   endif
#  endif
          END DO
        END DO
      END IF
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add

#else
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO itrc=1,NT(ng)
          DO k=1,N(ng)
            DO j=JstrT,JendT
              BOUNDARY(ng)%t_east(j,k,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END IF

      IF (ANY(LBC(iwest,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Western_Edge(tile)) THEN
        DO itrc=1,NT(ng)
          DO k=1,N(ng)
            DO j=JstrT,JendT
              BOUNDARY(ng)%t_west(j,k,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END IF

      IF (ANY(LBC(isouth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Southern_Edge(tile)) THEN
        DO itrc=1,NT(ng)
          DO k=1,N(ng)
            DO i=IstrT,IendT
              BOUNDARY(ng)%t_south(i,k,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END IF

      IF (ANY(LBC(inorth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Northern_Edge(tile)) THEN
        DO itrc=1,NT(ng)
          DO k=1,N(ng)
            DO i=IstrT,IendT
              BOUNDARY(ng)%t_north(i,k,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END IF
#endif

      RETURN
      END SUBROUTINE ana_tobc_bio_tile
