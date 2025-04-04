      SUBROUTINE ana_tobc (ng, tile, model)
!
!! svn $Id: ana_tobc.h 830 2017-01-24 21:21:11Z arango $
!!======================================================================
!! Copyright (c) 2002-2018 The ROMS/TOMS Group                         !
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
      CALL ana_tobc_tile (ng, tile, model,                              &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    IminS, ImaxS, JminS, JmaxS,                   &
     &                    nstp(ng), nnew(ng),                           &
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
      END SUBROUTINE ana_tobc
!
!***********************************************************************
      SUBROUTINE ana_tobc_tile (ng, tile, model,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          nstp, nnew,                             &
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
#ifdef OFFLINE
      USE mod_clima
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp, nnew

#ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: t(LBi:,LBj:,:,:,:)
#else
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#endif
!
!  Local variable declarations.
!
      integer :: i, ised, itrc, j, k, m
      real(r8) :: cff

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Tracers open boundary conditions.
!-----------------------------------------------------------------------
!
#ifdef ESTUARY_TEST
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
            BOUNDARY(ng)%t_east(j,k,itemp)=T0(ng)
            BOUNDARY(ng)%t_east(j,k,isalt)=0.0_r8
# ifdef SEDIMENT
            DO ised=1,NST
              BOUNDARY(ng)%t_east(j,k,idsed(ised))=0.0_r8
            END DO
# endif
          END DO
        END DO
      END IF

      IF (ANY(LBC(iwest,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Western_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
            BOUNDARY(ng)%t_west(j,k,itemp)=T0(ng)
            BOUNDARY(ng)%t_west(j,k,isalt)=30.0_r8
# ifdef SEDIMENT
            DO ised=1,NST
              BOUNDARY(ng)%t_west(j,k,idsed(ised))=0.0_r8
            END DO
# endif
          END DO
        END DO
      END IF

#elif defined NJ_BIGHT
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
            IF (z_r(Iend+1,j,k).ge.-15.0_r8) THEN
              BOUNDARY(ng)%t_east(j,k,itemp)=2.04926425772840E+01_r8-   &
     &                                       z_r(Iend+1,j,k)*           &
     &                                       (2.64085084879392E-01_r8+  &
     &                                        z_r(Iend+1,j,k)*          &
     &                                        (2.75112532853521E-01_r8+ &
     &                                         z_r(Iend+1,j,k)*         &
     &                                        (9.20748976164887E-02_r8+ &
     &                                         z_r(Iend+1,j,k)*         &
     &                                        (1.44907572574284E-02_r8+ &
     &                                         z_r(Iend+1,j,k)*         &
     &                                        (1.07821568591208E-03_r8+ &
     &                                         z_r(Iend+1,j,k)*         &
     &                                        (3.24031805390397E-05_r8+ &
     &                                         1.26282685769027E-07_r8*
     &                                         z_r(Iend+1,j,k)))))))
              BOUNDARY(ng)%t_east(j,k,isalt)=3.06648914919313E+01_r8-   &
     &                                       z_r(Iend+1,j,k)*           &
     &                                       (1.47672526294673E-01_r8+  &
     &                                        z_r(Iend+1,j,k)*          &
     &                                        (1.12645576031340E-01_r8+ &
     &                                         z_r(Iend+1,j,k)*         &
     &                                        (3.90092328187102E-02_r8+ &
     &                                         z_r(Iend+1,j,k)*         &
     &                                        (6.93901493744710E-03_r8+ &
     &                                         z_r(Iend+1,j,k)*         &
     &                                        (6.60443669679294E-04_r8+ &
     &                                         z_r(Iend+1,j,k)*         &
     &                                        (3.19179236195422E-05_r8+ &
     &                                         6.17735263440932E-07_r8*
     &                                         z_r(Iend+1,j,k)))))))
            ELSE
              cff=TANH(1.1_r8*z_r(Iend+1,j,k)+15.9_r8)
              t_east(j,k,itemp)=14.6_r8+6.70_r8*cff
              t_east(j,k,isalt)=31.3_r8-0.55_r8*cff
            END IF
          END DO
        END DO
      END IF

      IF (ANY(LBC(isouth,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Southern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
            IF (z_r(i,Jstr-1,k).ge.-15.0_r8) THEN
              BOUNDARY(ng)%t_south(i,k,itemp)=2.04926425772840E+01_r8-  &
     &                                        z_r(i,Jstr-1,k)*          &
     &                                        (2.64085084879392E-01_r8+ &
     &                                         z_r(i,Jstr-1,k)*         &
     &                                         (2.75112532853521E-01_r8+&
     &                                          z_r(i,Jstr-1,k)*        &
     &                                         (9.20748976164887E-02_r8+&
     &                                          z_r(i,Jstr-1,k)*        &
     &                                         (1.44907572574284E-02_r8+&
     &                                          z_r(i,Jstr-1,k)*        &
     &                                         (1.07821568591208E-03_r8+&
     &                                          z_r(i,Jstr-1,k)*        &
     &                                         (3.24031805390397E-05_r8+&
     &                                          1.26282685769027E-07_r8*
     &                                          z_r(i,Jstr-1,k)))))))
              BOUNDARY(ng)%t_south(i,k,isalt)=3.06648914919313E+01_r8-  &
     &                                        z_r(i,Jstr-1,k)*          &
     &                                        (1.47672526294673E-01_r8+ &
     &                                         z_r(i,Jstr-1,k)*         &
     &                                         (1.12645576031340E-01_r8+&
     &                                          z_r(i,Jstr-1,k)*        &
     &                                         (3.90092328187102E-02_r8+&
     &                                          z_r(i,Jstr-1,k)*        &
     &                                         (6.93901493744710E-03_r8+&
     &                                          z_r(i,Jstr-1,k)*        &
     &                                         (6.60443669679294E-04_r8+&
     &                                          z_r(i,Jstr-1,k)*        &
     &                                         (3.19179236195422E-05_r8+&
     &                                          6.17735263440932E-07_r8*
     &                                          z_r(i,Jstr-1,k)))))))
            ELSE
              cff=TANH(1.1_r8*depth+15.9_r8)
              BOUNDARY(ng)%t_south(i,k,itemp)=14.6_r8+6.70_r8*cff
              BOUNDARY(ng)%t_south(i,k,isalt)=31.3_r8-0.55_r8*cff
            END IF
          END DO
        END DO
      END IF

#elif defined SED_TEST1
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
            BOUNDARY(ng)%t_east(j,k,itemp)=20.0_r8
            BOUNDARY(ng)%t_east(j,k,isalt)=0.0_r8
          END DO
        END DO
      END IF
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
#elif defined OFFLINE
! ---- Eastern boundary -----------------------------------------------
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
            BOUNDARY(ng)%t_east(j,k,itemp) = CLIMA(ng)%tclm(Iend+1,j,k,itemp)
            BOUNDARY(ng)%t_east(j,k,isalt) = CLIMA(ng)%tclm(Iend+1,j,k,isalt)
          END DO
        END DO
      END IF
! ---- Western boundary -----------------------------------------------
      IF (ANY(LBC(iwest,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Western_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
            BOUNDARY(ng)%t_west(j,k,itemp) = CLIMA(ng)%tclm(Istr-1,j,k,itemp)
            BOUNDARY(ng)%t_west(j,k,isalt) = CLIMA(ng)%tclm(Istr-1,j,k,isalt)
          END DO
        END DO
      END IF

! ---- Southern boundary -----------------------------------------------
      IF (ANY(LBC(isouth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Southern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
            BOUNDARY(ng)%t_south(i,k,itemp) = CLIMA(ng)%tclm(i,Jstr-1,k,itemp)
            BOUNDARY(ng)%t_south(i,k,isalt) = CLIMA(ng)%tclm(i,Jstr-1,k,isalt)
          END DO
        END DO
      END IF

! ---- Northern boundary -----------------------------------------------
      IF (ANY(LBC(inorth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Northern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
            BOUNDARY(ng)%t_north(i,k,itemp) = CLIMA(ng)%tclm(i,Jend+1,k,itemp)
            BOUNDARY(ng)%t_north(i,k,isalt) = CLIMA(ng)%tclm(i,Jend+1,k,isalt)
          END DO
        END DO
      END IF


#elif defined REEF_ECOSYS && !defined OFFLINE

! ---- Eastern boundary -----------------------------------------------
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
            BOUNDARY(ng)%t_east(j,k,itemp)=T0(ng)
            BOUNDARY(ng)%t_east(j,k,isalt)=S0(ng)
# if defined SEDIMENT
            DO ised=1,NST
              BOUNDARY(ng)%t_east(j,k,idsed(ised))=0.0_r8
            END DO
# endif
          ! Initialize all tracer values to be zero
            BOUNDARY(ng)%t_east(j,k,iDIC (1)  :iDIC (N_Csp)     ) = 0.0_r8
            BOUNDARY(ng)%t_east(j,k,iNO3 (1)  :iNO3 (N_Nsp)     ) = 0.0_r8
            BOUNDARY(ng)%t_east(j,k,iNH4 (1)  :iNH4 (N_Nsp)     ) = 0.0_r8
            BOUNDARY(ng)%t_east(j,k,iPO4 (1)  :iPO4 (N_Psp)     ) = 0.0_r8
            BOUNDARY(ng)%t_east(j,k,iDOC (1,1):iDOC (N_Csp,Ndom)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iPOC (1,1):iPOC (N_Csp,Npom)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iDON (1,1):iDON (N_Nsp,Ndom)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_east(j,k,iPON (1,1):iPON (N_Nsp,Npom)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_east(j,k,iDOP (1,1):iDOP (N_Psp,Ndom)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_east(j,k,iPOP (1,1):iPOP (N_Psp,Npom)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_east(j,k,iPhyC(1,1):iPhyC(N_Csp,Nphy)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iZooC(1,1):iZooC(N_Csp,Nzoo)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iPhyN(1,1):iPhyN(N_Nsp,Nphy)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_east(j,k,iZooN(1,1):iZooN(N_Nsp,Nzoo)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_east(j,k,iPhyP(1,1):iPhyP(N_Psp,Nphy)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_east(j,k,iZooP(1,1):iZooP(N_Psp,Nzoo)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_east(j,k,iPIC (1,1):iPIC (N_Csp,Npim)) = 0.0_r8     ! umolC L-1

          ! TA  
            BOUNDARY(ng)%t_east(j,k,iTA)                              &
     &       = TA_Profile ( TA_0(ng),  BOUNDARY(ng)%t_east(j,k,iTemp))
          ! DIC  
            BOUNDARY(ng)%t_east(j,k,iDIC(iCt))                        &
     &       = DIC_Profile( DIC_0(ng), BOUNDARY(ng)%t_east(j,k,iTemp))
          END DO
        END DO
        DO k=1,N(ng)
          DO j=JstrT,JendT
          ! DO  
            BOUNDARY(ng)%t_east(j,k,iDO) =                            &
     &        DO_Profile2( BOUNDARY(ng)%t_east(j,N(ng),iTemp)         &
     &                   , BOUNDARY(ng)%t_east(j,N(ng),iSalt)         &
     &                   , BOUNDARY(ng)%t_east(j,N(ng),iDIC(iCt))     &
     &                   , BOUNDARY(ng)%t_east(j,k    ,iDIC(iCt))     &
     &                   , BOUNDARY(ng)%t_east(j,N(ng),iTA)           &
     &                   , BOUNDARY(ng)%t_east(j,k    ,iTA)     )
          END DO
        END DO

        DO k=1,N(ng)
          DO j=JstrT,JendT
          ! NO3  
            BOUNDARY(ng)%t_east(j,k,iNO3(iNt)) = NO3_Profile3( NO3_0(ng) &
     &                   , BOUNDARY(ng)%t_east(j,N(ng),iDO)              &
     &                   , BOUNDARY(ng)%t_east(j,k    ,iDO) )
          ! NH4   
            BOUNDARY(ng)%t_east(j,k,iNH4(iNt))=NH4_0(ng)     ! umol L-1
          ! PO4
            BOUNDARY(ng)%t_east(j,k,iPO4(iPt)) = PO4_Profile3( PO4_0(ng) &
     &                   , BOUNDARY(ng)%t_east(j,N(ng),iDO)              &
     &                   , BOUNDARY(ng)%t_east(j,k    ,iDO)  )
          ! DOC
            DO m=1,Ndom
              BOUNDARY(ng)%t_east(j,k,iDOC(iCt,m)) = DOC_0(m,ng)     ! umolC L-1
            END DO
          ! POC
            DO m=1,Npom
              BOUNDARY(ng)%t_east(j,k,iPOC(iCt,m)) = POC_0(m,ng)     ! umolC L-1
            END DO
          ! DON
            DO m=1,Ndom
              BOUNDARY(ng)%t_east(j,k,iDON(iNt,m)) = DON_0(m,ng)     ! umolC L-1
            END DO
          ! PON
            DO m=1,Npom
              BOUNDARY(ng)%t_east(j,k,iPON(iNt,m)) = PON_0(m,ng)     ! umolC L-1
            END DO
          ! DOP
            DO m=1,Ndom
              BOUNDARY(ng)%t_east(j,k,iDOP(iPt,m)) = DOP_0(m,ng)     ! umolC L-1
            END DO
          ! POP
            DO m=1,Npom
              BOUNDARY(ng)%t_east(j,k,iPOP(iPt,m)) = POP_0(m,ng)     ! umolC L-1
            END DO
          ! PhyC        
            DO m=1,Nphy
              BOUNDARY(ng)%t_east(j,k,iPhyC(iCt,m)) &
     &          = PHY_Profile2( m, PhyC_0(m,ng), z_r(Iend+1,j,k) )
            END DO
          ! ZooC
            DO m=1,Nzoo
              BOUNDARY(ng)%t_east(j,k,iZooC(iCt,m)) &
     &          = ZOO_Profile2( m, ZooC_0(m,ng), z_r(Iend+1,j,k) )
            END DO
            ! PhyN        
            DO m=1,Nphy
              BOUNDARY(ng)%t_east(j,k,iPhyN(iNt,m)) &
     &          = BOUNDARY(ng)%t_east(j,k,iPhyC(iCt,m))*rNCp(m)
            END DO
          ! ZooN
            DO m=1,Nzoo
              BOUNDARY(ng)%t_east(j,k,iZooN(iNt,m)) &
     &          = BOUNDARY(ng)%t_east(j,k,iZooC(iCt,m))*rNCz(m)
            END DO
          ! PhyP      
            DO m=1,Nphy
              BOUNDARY(ng)%t_east(j,k,iPhyP(iPt,m)) &
     &          = BOUNDARY(ng)%t_east(j,k,iPhyC(iCt,m))*rPCp(m)
            END DO
          ! ZooP
            DO m=1,Nzoo
              BOUNDARY(ng)%t_east(j,k,iZooP(iPt,m)) &
     &          = BOUNDARY(ng)%t_east(j,k,iZooC(iCt,m))*rPCz(m)
            END DO
          ! PIC
            BOUNDARY(ng)%t_east(j,k,iPIC(iCt,iLive)) &
     &         = BOUNDARY(ng)%t_east(j,k,iPhyC(iCt,iCcl))*rCaCp(iCcl) ! PIC_0(iLive,ng)     ! umolC L-1
            DO m=2,Npim
              BOUNDARY(ng)%t_east(j,k,iPIC(iCt,m)) = PIC_0(m,ng)     ! umolC L-1
            END DO

# if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
            BOUNDARY(ng)%t_east(j,k,iDIC(iC13))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iDIC(iCt)),   d13C_DIC_0(ng), R13C_VPDB )
            DO m=1,Ndom
              BOUNDARY(ng)%t_east(j,k,iDOC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iDOC(iCt,m)), d13C_DOC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Npom
              BOUNDARY(ng)%t_east(j,k,iPOC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iPOC(iCt,m)), d13C_POC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Nphy
              BOUNDARY(ng)%t_east(j,k,iPhyC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iPhyC(iCt,m)), d13C_PhyC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Nzoo
              BOUNDARY(ng)%t_east(j,k,iZooC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iZooC(iCt,m)), d13C_ZooC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Npim
              BOUNDARY(ng)%t_east(j,k,iPIC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iPIC(iCt,m)), d13C_PIC_0(m,ng), R13C_VPDB )
            END DO
#  if defined CLUMPED_ISOTOPE
!**************** Under developpment *************************
            BOUNDARY(ng)%t_east(j,k,iDIC(iD47))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iDIC(iCt)),   D47_DIC_0(ng), R47D_???? )
            DO m=1,Ndom
              BOUNDARY(ng)%t_east(j,k,iDOC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iDOC(iCt,m)), D47_DOC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Npom
              BOUNDARY(ng)%t_east(j,k,iPOC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iPOC(iCt,m)), D47_POC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Nphy
              BOUNDARY(ng)%t_east(j,k,iPhyC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iPhyC(iCt,m)), D47_PhyC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Nzoo
              BOUNDARY(ng)%t_east(j,k,iZooC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iZooC(iCt,m)), D47_ZooC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Npim
              BOUNDARY(ng)%t_east(j,k,iPIC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iPIC(iCt,m)), D47_PIC_0(m,ng), R47D_???? )
            END DO
#  endif
# endif
# if defined NITROGEN_ISOTOPE
            BOUNDARY(ng)%t_east(j,k,iNO3(iN15))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iNO3(iNt)),   d15N_NO3_0(ng), R15N_AIR )
            BOUNDARY(ng)%t_east(j,k,iNH4(iN15))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iNH4(iNt)),   d15N_NH4_0(ng), R15N_AIR )
            DO m=1,Ndom
              BOUNDARY(ng)%t_east(j,k,iDON(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iDON(iNt,m)), d15N_DON_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Npom
              BOUNDARY(ng)%t_east(j,k,iPON(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iPON(iNt,m)), d15N_PON_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Nphy
              BOUNDARY(ng)%t_east(j,k,iPhyN(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iPhyN(iNt,m)), d15N_PhyN_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Nzoo
              BOUNDARY(ng)%t_east(j,k,iZooN(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_east(j,k,iZooN(iNt,m)), d15N_ZooN_0(m,ng), R15N_AIR )
            END DO
# endif
# if defined COT_STARFISH
            BOUNDARY(ng)%t_east(j,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_east(j,k,iCOTl)=COTl0(ng)     ! umolC L-1
# endif
          END DO
        END DO
      END IF

! ---- Western boundary -----------------------------------------------
      IF (ANY(LBC(iwest,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Western_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
            BOUNDARY(ng)%t_west(j,k,itemp)=T0(ng)
            BOUNDARY(ng)%t_west(j,k,isalt)=S0(ng)
# if defined SEDIMENT
            DO ised=1,NST
              BOUNDARY(ng)%t_west(j,k,idsed(ised))=0.0_r8
            END DO
# endif
          ! Initialize all tracer values to be zero
            BOUNDARY(ng)%t_west(j,k,iDIC (1)  :iDIC (N_Csp)     ) = 0.0_r8
            BOUNDARY(ng)%t_west(j,k,iNO3 (1)  :iNO3 (N_Nsp)     ) = 0.0_r8
            BOUNDARY(ng)%t_west(j,k,iNH4 (1)  :iNH4 (N_Nsp)     ) = 0.0_r8
            BOUNDARY(ng)%t_west(j,k,iPO4 (1)  :iPO4 (N_Psp)     ) = 0.0_r8
            BOUNDARY(ng)%t_west(j,k,iDOC (1,1):iDOC (N_Csp,Ndom)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iPOC (1,1):iPOC (N_Csp,Npom)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iDON (1,1):iDON (N_Nsp,Ndom)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_west(j,k,iPON (1,1):iPON (N_Nsp,Npom)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_west(j,k,iDOP (1,1):iDOP (N_Psp,Ndom)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_west(j,k,iPOP (1,1):iPOP (N_Psp,Npom)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_west(j,k,iPhyC(1,1):iPhyC(N_Csp,Nphy)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iZooC(1,1):iZooC(N_Csp,Nzoo)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iPhyN(1,1):iPhyN(N_Nsp,Nphy)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_west(j,k,iZooN(1,1):iZooN(N_Nsp,Nzoo)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_west(j,k,iPhyP(1,1):iPhyP(N_Psp,Nphy)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_west(j,k,iZooP(1,1):iZooP(N_Psp,Nzoo)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_west(j,k,iPIC (1,1):iPIC (N_Csp,Npim)) = 0.0_r8     ! umolC L-1

          ! TA  
            BOUNDARY(ng)%t_west(j,k,iTA)                              &
     &       = TA_Profile ( TA_0(ng),  BOUNDARY(ng)%t_west(j,k,iTemp))
          ! DIC  
            BOUNDARY(ng)%t_west(j,k,iDIC(iCt))                        &
     &       = DIC_Profile( DIC_0(ng), BOUNDARY(ng)%t_west(j,k,iTemp))
          END DO
        END DO
        DO k=1,N(ng)
          DO j=JstrT,JendT
          ! DO  
            BOUNDARY(ng)%t_west(j,k,iDO) =                            &
     &        DO_Profile2( BOUNDARY(ng)%t_west(j,N(ng),iTemp)         &
     &                   , BOUNDARY(ng)%t_west(j,N(ng),iSalt)         &
     &                   , BOUNDARY(ng)%t_west(j,N(ng),iDIC(iCt))     &
     &                   , BOUNDARY(ng)%t_west(j,k    ,iDIC(iCt))     &
     &                   , BOUNDARY(ng)%t_west(j,N(ng),iTA)           &
     &                   , BOUNDARY(ng)%t_west(j,k    ,iTA)     )
          END DO
        END DO

        DO k=1,N(ng)
          DO j=JstrT,JendT
          ! NO3  
            BOUNDARY(ng)%t_west(j,k,iNO3(iNt)) = NO3_Profile3( NO3_0(ng) &
     &                   , BOUNDARY(ng)%t_west(j,N(ng),iDO)              &
     &                   , BOUNDARY(ng)%t_west(j,k    ,iDO) )
          ! NH4   
            BOUNDARY(ng)%t_west(j,k,iNH4(iNt))=NH4_0(ng)     ! umol L-1
          ! PO4
            BOUNDARY(ng)%t_west(j,k,iPO4(iPt)) = PO4_Profile3( PO4_0(ng) &
     &                   , BOUNDARY(ng)%t_west(j,N(ng),iDO)              &
     &                   , BOUNDARY(ng)%t_west(j,k    ,iDO)  )
          ! DOC
            DO m=1,Ndom
              BOUNDARY(ng)%t_west(j,k,iDOC(iCt,m)) = DOC_0(m,ng)     ! umolC L-1
            END DO
          ! POC
            DO m=1,Npom
              BOUNDARY(ng)%t_west(j,k,iPOC(iCt,m)) = POC_0(m,ng)     ! umolC L-1
            END DO
          ! DON
            DO m=1,Ndom
              BOUNDARY(ng)%t_west(j,k,iDON(iNt,m)) = DON_0(m,ng)     ! umolC L-1
            END DO
          ! PON
            DO m=1,Npom
              BOUNDARY(ng)%t_west(j,k,iPON(iNt,m)) = PON_0(m,ng)     ! umolC L-1
            END DO
          ! DOP
            DO m=1,Ndom
              BOUNDARY(ng)%t_west(j,k,iDOP(iPt,m)) = DOP_0(m,ng)     ! umolC L-1
            END DO
          ! POP
            DO m=1,Npom
              BOUNDARY(ng)%t_west(j,k,iPOP(iPt,m)) = POP_0(m,ng)     ! umolC L-1
            END DO
          ! PhyC        
            DO m=1,Nphy
              BOUNDARY(ng)%t_west(j,k,iPhyC(iCt,m)) &
     &          = PHY_Profile2( m, PhyC_0(m,ng), z_r(Istr-1,j,k) )
            END DO
          ! ZooC
            DO m=1,Nzoo
              BOUNDARY(ng)%t_west(j,k,iZooC(iCt,m)) &
     &          = ZOO_Profile2( m, ZooC_0(m,ng), z_r(Istr-1,j,k) )
            END DO
            ! PhyN        
            DO m=1,Nphy
              BOUNDARY(ng)%t_west(j,k,iPhyN(iNt,m)) &
     &          = BOUNDARY(ng)%t_west(j,k,iPhyC(iCt,m))*rNCp(m)
            END DO
          ! ZooN
            DO m=1,Nzoo
              BOUNDARY(ng)%t_west(j,k,iZooN(iNt,m)) &
     &          = BOUNDARY(ng)%t_west(j,k,iZooC(iCt,m))*rNCz(m)
            END DO
          ! PhyP      
            DO m=1,Nphy
              BOUNDARY(ng)%t_west(j,k,iPhyP(iPt,m)) &
     &          = BOUNDARY(ng)%t_west(j,k,iPhyC(iCt,m))*rPCp(m)
            END DO
          ! ZooP
            DO m=1,Nzoo
              BOUNDARY(ng)%t_west(j,k,iZooP(iPt,m)) &
     &          = BOUNDARY(ng)%t_west(j,k,iZooC(iCt,m))*rPCz(m)
            END DO
          ! PIC
            BOUNDARY(ng)%t_west(j,k,iPIC(iCt,iLive)) &
     &         = BOUNDARY(ng)%t_west(j,k,iPhyC(iCt,iCcl))*rCaCp(iCcl) ! PIC_0(iLive,ng)     ! umolC L-1
            DO m=2,Npim
              BOUNDARY(ng)%t_west(j,k,iPIC(iCt,m)) = PIC_0(m,ng)     ! umolC L-1
            END DO

# if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
            BOUNDARY(ng)%t_west(j,k,iDIC(iC13))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iDIC(iCt)),   d13C_DIC_0(ng), R13C_VPDB )
            DO m=1,Ndom
              BOUNDARY(ng)%t_west(j,k,iDOC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iDOC(iCt,m)), d13C_DOC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Npom
              BOUNDARY(ng)%t_west(j,k,iPOC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iPOC(iCt,m)), d13C_POC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Nphy
              BOUNDARY(ng)%t_west(j,k,iPhyC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iPhyC(iCt,m)), d13C_PhyC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Nzoo
              BOUNDARY(ng)%t_west(j,k,iZooC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iZooC(iCt,m)), d13C_ZooC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Npim
              BOUNDARY(ng)%t_west(j,k,iPIC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iPIC(iCt,m)), d13C_PIC_0(m,ng), R13C_VPDB )
            END DO
#  if defined CLUMPED_ISOTOPE
!**************** Under developpment *************************
            BOUNDARY(ng)%t_west(j,k,iDIC(iD47))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iDIC(iCt)),   D47_DIC_0(ng), R47D_???? )
            DO m=1,Ndom
              BOUNDARY(ng)%t_west(j,k,iDOC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iDOC(iCt,m)), D47_DOC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Npom
              BOUNDARY(ng)%t_west(j,k,iPOC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iPOC(iCt,m)), D47_POC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Nphy
              BOUNDARY(ng)%t_west(j,k,iPhyC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iPhyC(iCt,m)), D47_PhyC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Nzoo
              BOUNDARY(ng)%t_west(j,k,iZooC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iZooC(iCt,m)), D47_ZooC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Npim
              BOUNDARY(ng)%t_west(j,k,iPIC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iPIC(iCt,m)), D47_PIC_0(m,ng), R47D_???? )
            END DO
#  endif
# endif
# if defined NITROGEN_ISOTOPE
            BOUNDARY(ng)%t_west(j,k,iNO3(iN15))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iNO3(iNt)),   d15N_NO3_0(ng), R15N_AIR )
            BOUNDARY(ng)%t_west(j,k,iNH4(iN15))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iNH4(iNt)),   d15N_NH4_0(ng), R15N_AIR )
            DO m=1,Ndom
              BOUNDARY(ng)%t_west(j,k,iDON(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iDON(iNt,m)), d15N_DON_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Npom
              BOUNDARY(ng)%t_west(j,k,iPON(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iPON(iNt,m)), d15N_PON_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Nphy
              BOUNDARY(ng)%t_west(j,k,iPhyN(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iPhyN(iNt,m)), d15N_PhyN_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Nzoo
              BOUNDARY(ng)%t_west(j,k,iZooN(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_west(j,k,iZooN(iNt,m)), d15N_ZooN_0(m,ng), R15N_AIR )
            END DO
# endif
# if defined COT_STARFISH
            BOUNDARY(ng)%t_west(j,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_west(j,k,iCOTl)=COTl0(ng)     ! umolC L-1
# endif
          END DO
        END DO
      END IF

! ---- Southern boundary -----------------------------------------------
      IF (ANY(LBC(isouth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Southern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
            BOUNDARY(ng)%t_south(i,k,itemp)=T0(ng)
            BOUNDARY(ng)%t_south(i,k,isalt)=S0(ng)
# if defined SEDIMENT
            DO ised=1,NST
              BOUNDARY(ng)%t_south(i,k,idsed(ised))=0.0_r8
            END DO
# endif
          ! Initialize all tracer values to be zero
            BOUNDARY(ng)%t_south(i,k,iDIC (1)  :iDIC (N_Csp)     ) = 0.0_r8
            BOUNDARY(ng)%t_south(i,k,iNO3 (1)  :iNO3 (N_Nsp)     ) = 0.0_r8
            BOUNDARY(ng)%t_south(i,k,iNH4 (1)  :iNH4 (N_Nsp)     ) = 0.0_r8
            BOUNDARY(ng)%t_south(i,k,iPO4 (1)  :iPO4 (N_Psp)     ) = 0.0_r8
            BOUNDARY(ng)%t_south(i,k,iDOC (1,1):iDOC (N_Csp,Ndom)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iPOC (1,1):iPOC (N_Csp,Npom)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iDON (1,1):iDON (N_Nsp,Ndom)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_south(i,k,iPON (1,1):iPON (N_Nsp,Npom)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_south(i,k,iDOP (1,1):iDOP (N_Psp,Ndom)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_south(i,k,iPOP (1,1):iPOP (N_Psp,Npom)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_south(i,k,iPhyC(1,1):iPhyC(N_Csp,Nphy)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iZooC(1,1):iZooC(N_Csp,Nzoo)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iPhyN(1,1):iPhyN(N_Nsp,Nphy)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_south(i,k,iZooN(1,1):iZooN(N_Nsp,Nzoo)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_south(i,k,iPhyP(1,1):iPhyP(N_Psp,Nphy)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_south(i,k,iZooP(1,1):iZooP(N_Psp,Nzoo)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_south(i,k,iPIC (1,1):iPIC (N_Csp,Npim)) = 0.0_r8     ! umolC L-1

          ! TA  
            BOUNDARY(ng)%t_south(i,k,iTA)                              &
     &       = TA_Profile ( TA_0(ng),  BOUNDARY(ng)%t_south(i,k,iTemp))
          ! DIC  
            BOUNDARY(ng)%t_south(i,k,iDIC(iCt))                        &
     &       = DIC_Profile( DIC_0(ng), BOUNDARY(ng)%t_south(i,k,iTemp))
          END DO
        END DO
        DO k=1,N(ng)
          DO i=IstrT,IendT
          ! DO  
            BOUNDARY(ng)%t_south(i,k,iDO) =                            &
     &        DO_Profile2( BOUNDARY(ng)%t_south(i,N(ng),iTemp)         &
     &                   , BOUNDARY(ng)%t_south(i,N(ng),iSalt)         &
     &                   , BOUNDARY(ng)%t_south(i,N(ng),iDIC(iCt))     &
     &                   , BOUNDARY(ng)%t_south(i,k    ,iDIC(iCt))     &
     &                   , BOUNDARY(ng)%t_south(i,N(ng),iTA)           &
     &                   , BOUNDARY(ng)%t_south(i,k    ,iTA)     )
          END DO
        END DO

        DO k=1,N(ng)
          DO i=IstrT,IendT
          ! NO3  
            BOUNDARY(ng)%t_south(i,k,iNO3(iNt)) = NO3_Profile3( NO3_0(ng) &
     &                   , BOUNDARY(ng)%t_south(i,N(ng),iDO)              &
     &                   , BOUNDARY(ng)%t_south(i,k    ,iDO) )
          ! NH4   
            BOUNDARY(ng)%t_south(i,k,iNH4(iNt))=NH4_0(ng)     ! umol L-1
          ! PO4
            BOUNDARY(ng)%t_south(i,k,iPO4(iPt)) = PO4_Profile3( PO4_0(ng) &
     &                   , BOUNDARY(ng)%t_south(i,N(ng),iDO)              &
     &                   , BOUNDARY(ng)%t_south(i,k    ,iDO)  )
          ! DOC
            DO m=1,Ndom
              BOUNDARY(ng)%t_south(i,k,iDOC(iCt,m)) = DOC_0(m,ng)     ! umolC L-1
            END DO
          ! POC
            DO m=1,Npom
              BOUNDARY(ng)%t_south(i,k,iPOC(iCt,m)) = POC_0(m,ng)     ! umolC L-1
            END DO
          ! DON
            DO m=1,Ndom
              BOUNDARY(ng)%t_south(i,k,iDON(iNt,m)) = DON_0(m,ng)     ! umolC L-1
            END DO
          ! PON
            DO m=1,Npom
              BOUNDARY(ng)%t_south(i,k,iPON(iNt,m)) = PON_0(m,ng)     ! umolC L-1
            END DO
          ! DOP
            DO m=1,Ndom
              BOUNDARY(ng)%t_south(i,k,iDOP(iPt,m)) = DOP_0(m,ng)     ! umolC L-1
            END DO
          ! POP
            DO m=1,Npom
              BOUNDARY(ng)%t_south(i,k,iPOP(iPt,m)) = POP_0(m,ng)     ! umolC L-1
            END DO
          ! PhyC        
            DO m=1,Nphy
              BOUNDARY(ng)%t_south(i,k,iPhyC(iCt,m)) &
     &          = PHY_Profile2( m, PhyC_0(m,ng), z_r(i,Jstr-1,k) )
            END DO
          ! ZooC
            DO m=1,Nzoo
              BOUNDARY(ng)%t_south(i,k,iZooC(iCt,m)) &
     &          = ZOO_Profile2( m, ZooC_0(m,ng), z_r(i,Jstr-1,k) )
            END DO
            ! PhyN        
            DO m=1,Nphy
              BOUNDARY(ng)%t_south(i,k,iPhyN(iNt,m)) &
     &          = BOUNDARY(ng)%t_south(i,k,iPhyC(iCt,m))*rNCp(m)
            END DO
          ! ZooN
            DO m=1,Nzoo
              BOUNDARY(ng)%t_south(i,k,iZooN(iNt,m)) &
     &          = BOUNDARY(ng)%t_south(i,k,iZooC(iCt,m))*rNCz(m)
            END DO
          ! PhyP      
            DO m=1,Nphy
              BOUNDARY(ng)%t_south(i,k,iPhyP(iPt,m)) &
     &          = BOUNDARY(ng)%t_south(i,k,iPhyC(iCt,m))*rPCp(m)
            END DO
          ! ZooP
            DO m=1,Nzoo
              BOUNDARY(ng)%t_south(i,k,iZooP(iPt,m)) &
     &          = BOUNDARY(ng)%t_south(i,k,iZooC(iCt,m))*rPCz(m)
            END DO
          ! PIC
            BOUNDARY(ng)%t_south(i,k,iPIC(iCt,iLive)) &
     &         = BOUNDARY(ng)%t_south(i,k,iPhyC(iCt,iCcl))*rCaCp(iCcl) ! PIC_0(iLive,ng)     ! umolC L-1
            DO m=2,Npim
              BOUNDARY(ng)%t_south(i,k,iPIC(iCt,m)) = PIC_0(m,ng)     ! umolC L-1
            END DO

# if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
            BOUNDARY(ng)%t_south(i,k,iDIC(iC13))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iDIC(iCt)),   d13C_DIC_0(ng), R13C_VPDB )
            DO m=1,Ndom
              BOUNDARY(ng)%t_south(i,k,iDOC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iDOC(iCt,m)), d13C_DOC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Npom
              BOUNDARY(ng)%t_south(i,k,iPOC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iPOC(iCt,m)), d13C_POC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Nphy
              BOUNDARY(ng)%t_south(i,k,iPhyC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iPhyC(iCt,m)), d13C_PhyC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Nzoo
              BOUNDARY(ng)%t_south(i,k,iZooC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iZooC(iCt,m)), d13C_ZooC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Npim
              BOUNDARY(ng)%t_south(i,k,iPIC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iPIC(iCt,m)), d13C_PIC_0(m,ng), R13C_VPDB )
            END DO
#  if defined CLUMPED_ISOTOPE
!**************** Under developpment *************************
            BOUNDARY(ng)%t_south(i,k,iDIC(iD47))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iDIC(iCt)),   D47_DIC_0(ng), R47D_???? )
            DO m=1,Ndom
              BOUNDARY(ng)%t_south(i,k,iDOC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iDOC(iCt,m)), D47_DOC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Npom
              BOUNDARY(ng)%t_south(i,k,iPOC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iPOC(iCt,m)), D47_POC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Nphy
              BOUNDARY(ng)%t_south(i,k,iPhyC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iPhyC(iCt,m)), D47_PhyC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Nzoo
              BOUNDARY(ng)%t_south(i,k,iZooC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iZooC(iCt,m)), D47_ZooC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Npim
              BOUNDARY(ng)%t_south(i,k,iPIC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iPIC(iCt,m)), D47_PIC_0(m,ng), R47D_???? )
            END DO
#  endif
# endif
# if defined NITROGEN_ISOTOPE
            BOUNDARY(ng)%t_south(i,k,iNO3(iN15))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iNO3(iNt)),   d15N_NO3_0(ng), R15N_AIR )
            BOUNDARY(ng)%t_south(i,k,iNH4(iN15))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iNH4(iNt)),   d15N_NH4_0(ng), R15N_AIR )
            DO m=1,Ndom
              BOUNDARY(ng)%t_south(i,k,iDON(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iDON(iNt,m)), d15N_DON_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Npom
              BOUNDARY(ng)%t_south(i,k,iPON(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iPON(iNt,m)), d15N_PON_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Nphy
              BOUNDARY(ng)%t_south(i,k,iPhyN(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iPhyN(iNt,m)), d15N_PhyN_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Nzoo
              BOUNDARY(ng)%t_south(i,k,iZooN(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_south(i,k,iZooN(iNt,m)), d15N_ZooN_0(m,ng), R15N_AIR )
            END DO
# endif
# if defined COT_STARFISH
            BOUNDARY(ng)%t_south(i,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_south(i,k,iCOTl)=COTl0(ng)     ! umolC L-1
# endif
          END DO
        END DO
      END IF

! ---- Northern boundary -----------------------------------------------
      IF (ANY(LBC(inorth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Northern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
            BOUNDARY(ng)%t_north(i,k,itemp)=T0(ng)
            BOUNDARY(ng)%t_north(i,k,isalt)=S0(ng)
# if defined SEDIMENT
            DO ised=1,NST
              BOUNDARY(ng)%t_north(i,k,idsed(ised))=0.0_r8
            END DO
# endif
          ! Initialize all tracer values to be zero
            BOUNDARY(ng)%t_north(i,k,iDIC (1)  :iDIC (N_Csp)     ) = 0.0_r8
            BOUNDARY(ng)%t_north(i,k,iNO3 (1)  :iNO3 (N_Nsp)     ) = 0.0_r8
            BOUNDARY(ng)%t_north(i,k,iNH4 (1)  :iNH4 (N_Nsp)     ) = 0.0_r8
            BOUNDARY(ng)%t_north(i,k,iPO4 (1)  :iPO4 (N_Psp)     ) = 0.0_r8
            BOUNDARY(ng)%t_north(i,k,iDOC (1,1):iDOC (N_Csp,Ndom)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iPOC (1,1):iPOC (N_Csp,Npom)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iDON (1,1):iDON (N_Nsp,Ndom)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_north(i,k,iPON (1,1):iPON (N_Nsp,Npom)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_north(i,k,iDOP (1,1):iDOP (N_Psp,Ndom)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_north(i,k,iPOP (1,1):iPOP (N_Psp,Npom)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_north(i,k,iPhyC(1,1):iPhyC(N_Csp,Nphy)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iZooC(1,1):iZooC(N_Csp,Nzoo)) = 0.0_r8     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iPhyN(1,1):iPhyN(N_Nsp,Nphy)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_north(i,k,iZooN(1,1):iZooN(N_Nsp,Nzoo)) = 0.0_r8     ! umolN L-1
            BOUNDARY(ng)%t_north(i,k,iPhyP(1,1):iPhyP(N_Psp,Nphy)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_north(i,k,iZooP(1,1):iZooP(N_Psp,Nzoo)) = 0.0_r8     ! umolP L-1
            BOUNDARY(ng)%t_north(i,k,iPIC (1,1):iPIC (N_Csp,Npim)) = 0.0_r8     ! umolC L-1

          ! TA  
            BOUNDARY(ng)%t_north(i,k,iTA)                              &
     &       = TA_Profile ( TA_0(ng),  BOUNDARY(ng)%t_north(i,k,iTemp))
          ! DIC  
            BOUNDARY(ng)%t_north(i,k,iDIC(iCt))                        &
     &       = DIC_Profile( DIC_0(ng), BOUNDARY(ng)%t_north(i,k,iTemp))
          END DO
        END DO
        DO k=1,N(ng)
          DO i=IstrT,IendT
          ! DO  
            BOUNDARY(ng)%t_north(i,k,iDO) =                            &
     &        DO_Profile2( BOUNDARY(ng)%t_north(i,N(ng),iTemp)         &
     &                   , BOUNDARY(ng)%t_north(i,N(ng),iSalt)         &
     &                   , BOUNDARY(ng)%t_north(i,N(ng),iDIC(iCt))     &
     &                   , BOUNDARY(ng)%t_north(i,k    ,iDIC(iCt))     &
     &                   , BOUNDARY(ng)%t_north(i,N(ng),iTA)           &
     &                   , BOUNDARY(ng)%t_north(i,k    ,iTA)     )
          END DO
        END DO

        DO k=1,N(ng)
          DO i=IstrT,IendT
          ! NO3  
            BOUNDARY(ng)%t_north(i,k,iNO3(iNt)) = NO3_Profile3( NO3_0(ng) &
     &                   , BOUNDARY(ng)%t_north(i,N(ng),iDO)              &
     &                   , BOUNDARY(ng)%t_north(i,k    ,iDO) )
          ! NH4   
            BOUNDARY(ng)%t_north(i,k,iNH4(iNt))=NH4_0(ng)     ! umol L-1
          ! PO4
            BOUNDARY(ng)%t_north(i,k,iPO4(iPt)) = PO4_Profile3( PO4_0(ng) &
     &                   , BOUNDARY(ng)%t_north(i,N(ng),iDO)              &
     &                   , BOUNDARY(ng)%t_north(i,k    ,iDO)  )
          ! DOC
            DO m=1,Ndom
              BOUNDARY(ng)%t_north(i,k,iDOC(iCt,m)) = DOC_0(m,ng)     ! umolC L-1
            END DO
          ! POC
            DO m=1,Npom
              BOUNDARY(ng)%t_north(i,k,iPOC(iCt,m)) = POC_0(m,ng)     ! umolC L-1
            END DO
          ! DON
            DO m=1,Ndom
              BOUNDARY(ng)%t_north(i,k,iDON(iNt,m)) = DON_0(m,ng)     ! umolC L-1
            END DO
          ! PON
            DO m=1,Npom
              BOUNDARY(ng)%t_north(i,k,iPON(iNt,m)) = PON_0(m,ng)     ! umolC L-1
            END DO
          ! DOP
            DO m=1,Ndom
              BOUNDARY(ng)%t_north(i,k,iDOP(iPt,m)) = DOP_0(m,ng)     ! umolC L-1
            END DO
          ! POP
            DO m=1,Npom
              BOUNDARY(ng)%t_north(i,k,iPOP(iPt,m)) = POP_0(m,ng)     ! umolC L-1
            END DO
          ! PhyC        
            DO m=1,Nphy
              BOUNDARY(ng)%t_north(i,k,iPhyC(iCt,m)) &
     &          = PHY_Profile2( m, PhyC_0(m,ng), z_r(i,Jend+1,k) )
            END DO
          ! ZooC
            DO m=1,Nzoo
              BOUNDARY(ng)%t_north(i,k,iZooC(iCt,m)) &
     &          = ZOO_Profile2( m, ZooC_0(m,ng), z_r(i,Jend+1,k) )
            END DO
          ! PhyN        
            DO m=1,Nphy
              BOUNDARY(ng)%t_north(i,k,iPhyN(iNt,m)) &
     &          = BOUNDARY(ng)%t_north(i,k,iPhyC(iCt,m))*rNCp(m)
            END DO
          ! ZooN
            DO m=1,Nzoo
              BOUNDARY(ng)%t_north(i,k,iZooN(iNt,m)) &
     &          = BOUNDARY(ng)%t_north(i,k,iZooC(iCt,m))*rNCz(m)
            END DO
          ! PhyP      
            DO m=1,Nphy
              BOUNDARY(ng)%t_north(i,k,iPhyP(iPt,m)) &
     &          = BOUNDARY(ng)%t_north(i,k,iPhyC(iCt,m))*rPCp(m)
            END DO
          ! ZooP
            DO m=1,Nzoo
              BOUNDARY(ng)%t_north(i,k,iZooP(iPt,m)) &
     &          = BOUNDARY(ng)%t_north(i,k,iZooC(iCt,m))*rPCz(m)
            END DO
          ! PIC
            BOUNDARY(ng)%t_north(i,k,iPIC(iCt,iLive)) &
     &         = BOUNDARY(ng)%t_north(i,k,iPhyC(iCt,iCcl))*rCaCp(iCcl) ! PIC_0(iLive,ng)     ! umolC L-1
            DO m=2,Npim
              BOUNDARY(ng)%t_north(i,k,iPIC(iCt,m)) = PIC_0(m,ng)     ! umolC L-1
            END DO

# if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
            BOUNDARY(ng)%t_north(i,k,iDIC(iC13))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iDIC(iCt)),   d13C_DIC_0(ng), R13C_VPDB )
            DO m=1,Ndom
              BOUNDARY(ng)%t_north(i,k,iDOC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iDOC(iCt,m)), d13C_DOC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Npom
              BOUNDARY(ng)%t_north(i,k,iPOC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iPOC(iCt,m)), d13C_POC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Nphy
              BOUNDARY(ng)%t_north(i,k,iPhyC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iPhyC(iCt,m)), d13C_PhyC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Nzoo
              BOUNDARY(ng)%t_north(i,k,iZooC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iZooC(iCt,m)), d13C_ZooC_0(m,ng), R13C_VPDB )
            END DO
            DO m=1,Npim
              BOUNDARY(ng)%t_north(i,k,iPIC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iPIC(iCt,m)), d13C_PIC_0(m,ng), R13C_VPDB )
            END DO
#  if defined CLUMPED_ISOTOPE
!**************** Under developpment *************************
            BOUNDARY(ng)%t_north(i,k,iDIC(iD47))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iDIC(iCt)),   D47_DIC_0(ng), R47D_???? )
            DO m=1,Ndom
              BOUNDARY(ng)%t_north(i,k,iDOC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iDOC(iCt,m)), D47_DOC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Npom
              BOUNDARY(ng)%t_north(i,k,iPOC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iPOC(iCt,m)), D47_POC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Nphy
              BOUNDARY(ng)%t_north(i,k,iPhyC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iPhyC(iCt,m)), D47_PhyC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Nzoo
              BOUNDARY(ng)%t_north(i,k,iZooC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iZooC(iCt,m)), D47_ZooC_0(m,ng), R47D_???? )
            END DO
            DO m=1,Npim
              BOUNDARY(ng)%t_north(i,k,iPIC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iPIC(iCt,m)), D47_PIC_0(m,ng), R47D_???? )
            END DO
#  endif
# endif
# if defined NITROGEN_ISOTOPE
            BOUNDARY(ng)%t_north(i,k,iNO3(iN15))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iNO3(iNt)),   d15N_NO3_0(ng), R15N_AIR )
            BOUNDARY(ng)%t_north(i,k,iNH4(iN15))                                              &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iNH4(iNt)),   d15N_NH4_0(ng), R15N_AIR )
            DO m=1,Ndom
              BOUNDARY(ng)%t_north(i,k,iDON(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iDON(iNt,m)), d15N_DON_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Npom
              BOUNDARY(ng)%t_north(i,k,iPON(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iPON(iNt,m)), d15N_PON_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Nphy
              BOUNDARY(ng)%t_north(i,k,iPhyN(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iPhyN(iNt,m)), d15N_PhyN_0(m,ng), R15N_AIR )
            END DO
            DO m=1,Nzoo
              BOUNDARY(ng)%t_north(i,k,iZooN(iN15,m))                                          &
     &        = Ci_from_Ct_delta(BOUNDARY(ng)%t_north(i,k,iZooN(iNt,m)), d15N_ZooN_0(m,ng), R15N_AIR )
            END DO
# endif
# if defined COT_STARFISH
            BOUNDARY(ng)%t_north(i,k,iCOTe)=COTe0(ng)     ! umolC L-1
            BOUNDARY(ng)%t_north(i,k,iCOTl)=COTl0(ng)     ! umolC L-1
# endif
          END DO
        END DO
      END IF
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add

#else
      IF (ANY(LBC(ieast,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
            BOUNDARY(ng)%t_east(j,k,itemp)=T0(ng)
            BOUNDARY(ng)%t_east(j,k,isalt)=S0(ng)
          END DO
        END DO
        DO itrc=3,NT(ng)
          DO k=1,N(ng)
            DO j=JstrT,JendT
              BOUNDARY(ng)%t_east(j,k,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END IF

      IF (ANY(LBC(iwest,isTvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Western_Edge(tile)) THEN
        DO k=1,N(ng)
          DO j=JstrT,JendT
            BOUNDARY(ng)%t_west(j,k,itemp)=T0(ng)
            BOUNDARY(ng)%t_west(j,k,isalt)=S0(ng)
          END DO
        END DO
        DO itrc=3,NT(ng)
          DO k=1,N(ng)
            DO j=JstrT,JendT
              BOUNDARY(ng)%t_west(j,k,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END IF

      IF (ANY(LBC(isouth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Southern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
            BOUNDARY(ng)%t_south(i,k,itemp)=T0(ng)
            BOUNDARY(ng)%t_south(i,k,isalt)=S0(ng)
          END DO
        END DO
        DO itrc=3,NT(ng)
          DO k=1,N(ng)
            DO i=IstrT,IendT
              BOUNDARY(ng)%t_south(i,k,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END IF

      IF (ANY(LBC(inorth,isTvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Northern_Edge(tile)) THEN
        DO k=1,N(ng)
          DO i=IstrT,IendT
            BOUNDARY(ng)%t_north(i,k,itemp)=T0(ng)
            BOUNDARY(ng)%t_north(i,k,isalt)=S0(ng)
          END DO
        END DO
        DO itrc=3,NT(ng)
          DO k=1,N(ng)
            DO i=IstrT,IendT
              BOUNDARY(ng)%t_north(i,k,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END IF
#endif

      RETURN
      END SUBROUTINE ana_tobc_tile
