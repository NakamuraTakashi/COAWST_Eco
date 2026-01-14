      SUBROUTINE ana_vegetation (ng, tile, model)
!                                                                      ! 
!! svn $Id: ana_vegetation.h 429 2015-18-05 17:00:25 Z arango $        !
!!=====================================================================!
!! Copyright (c) 2002-2016 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!================================================== John C. Warner ====!
!==================================================== Neil K. Ganju  ==!
!==================================================== Alexis Beudin  ==!
!==================================================Tarandeep S. Kalra==!
!                                                                      !
!  Vegetation Model Kernel Variables:                                  !
!  NVEG          Number of vegetation types                            !
!  NVEGP         Varying vegetation properties                         !
!  plant         Vegetation variable properties:                       !
!                   plant(:,:,:,pdiam) => diameter                     !
!                   plant(:,:,:,phght) => height                       !
!                   plant(:,:,:,pdens) => density                      !
!                   plant(:,:,:,pthck) => thickness                    !
!                   plant(:,:,:,pupbm) => above ground biomass         !
!                   plant(:,:,:,pdwbm) => below ground biomass         !
!  marsh_mask    Initialize the mask to get wave thrust on marsh       ! 
!                                                                      !
!  This routine sets initial conditions for vegetation fields          !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_grid
      USE mod_ncparam
      USE mod_vegarr
      
!
! Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model

#include "tile.h"
!
      CALL ana_vegetation_tile (ng, tile, model                         &
     &                        , LBi, UBi, LBj, UBj                      &
     &                        , IminS, ImaxS, JminS, JmaxS              &
     &                        , GRID(ng) % h                            &
     &                        , GRID(ng) % rmask                        &
#ifdef SEAGRASS
     &                        , GRID(ng) % p_sgrass                     &
#endif
#ifdef AQUACULTURE
     &                        , GRID(ng) % dens_aqua                    &
#endif
#ifdef VEG_DRAG
     &                        , VEG(ng) % plant                         &
#endif
#ifdef MARSH_WAVE_THRUST
     &                        , VEG(ng) % marsh_mask                    &
#endif
     & )
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME(48)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_vegetation
!
!***********************************************************************
      SUBROUTINE ana_vegetation_tile (ng, tile, model                   &
     &                              , LBi, UBi, LBj, UBj                &
     &                              , IminS, ImaxS, JminS, JmaxS        &
     &                              , h, rmask                          &
#ifdef SEAGRASS
     &                              , p_sgrass                          &
#endif
#ifdef AQUACULTURE
     &                              , dens_aqua                         &
#endif
#ifdef VEG_DRAG
     &                              , plant                             &
#endif
#ifdef MARSH_WAVE_THRUST
     &                              , marsh_mask                        &
#endif
     & )
!***********************************************************************
!
      USE mod_param
      USE mod_ncparam
      USE mod_scalars
      USE mod_vegetation
      USE mod_vegarr
#ifdef SEAGRASS
      USE mod_seagrass
#endif
#ifdef AQUACULTURE
      USE mod_aquaculture, ONLY : Naq, diam_aqua, thck_aqua, hght_aqua
#endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
#ifdef VEG_DRAG
# ifdef ASSUMED_SHAPE
      real(r8), intent(inout) :: plant(LBi:,LBj:,:,:)
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#  ifdef SEAGRASS
      real(r8), intent(inout) :: p_sgrass(:,LBi:,LBj:)
#  endif
#  ifdef AQUACULTURE
      real(r8), intent(inout) :: dens_aqua(:,LBi:,LBj:)
#  endif
# else
      real(r8), intent(inout) :: plant(LBi:UBi,LBj:UBj,NVEG,NVEGP)
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#  ifdef SEAGRASS
      real(r8), intent(inout) :: p_sgrass(Nsg,LBi:UBi,LBj:UBj)
#  endif
#  ifdef AQUACULTURE
      real(r8), intent(inout) :: dens_aqua(Naq,LBi:UBi,LBj:UBj)
#  endif
# endif
#endif 
!
#ifdef MARSH_WAVE_THRUST
# ifdef ASSUMED_SHAPE
      real(r8), intent(inout) :: marsh_mask(LBi:,LBj:)
# else
      real(r8), intent(inout) :: marsh_mask(LBi:UBi,LBj:UBj)
# endif 
#endif
!
!  Local variable declarations.
!
#ifdef DISTRIBUTE
      integer :: Tstr, Tend
#endif
      integer :: i, j, k, iveg, ivegS, isg

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Set initial properties for each plant 
!  To have variable properties in array->plant(x,y,iveg,iprop)
!----------------------------------------------------------------------- 
!
#if defined SEAGRASS || defined AQUACULTURE
      ivegS=1
# if defined AQUACULTURE
#  ifdef VEG_DRAG
      DO iveg=1,Naq  !! Aquaculture drag
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            plant(i,j,iveg,pdens)=dens_aqua(iveg,i,j)  !Density
!            plant(i,j,iveg,pdiam)=diam_aqua      !Diameter
            plant(i,j,iveg,pdiam)=diam_aqua(iveg)! 0.1d0 !diam_aqua(iveg)      !Diameter
            plant(i,j,iveg,phght)=hght_aqua(iveg)!10.0d0 !hght_aqua(iveg)      !Height
            plant(i,j,iveg,pthck)=thck_aqua(iveg)! 0.1d0 !thck_aqua(iveg)      !Thickness 1cm
          END DO
        END DO
      END DO
#  endif
      ivegS=Naq+1
# endif
# if defined SEAGRASS
      isg=1
#  ifdef VEG_DRAG
      DO iveg=ivegS,NVEG  !! seagrass drag
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            plant(i,j,iveg,pdiam)=0.01_r8        !Diameter
            plant(i,j,iveg,phght)=0.16_r8         !Height
            plant(i,j,iveg,pdens)=p_sgrass(isg,i,j)*2000.0_r8       !Density
            plant(i,j,iveg,pthck)=0.0005_r8      !Thickness 1cm
          END DO
        END DO
        isg=isg+1
      END DO
#  endif
# endif
!      
#elif defined FUKIDO
# ifdef VEG_DRAG
      DO iveg=1,NVEG
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            IF(h(i,j)>0.2_r8 .or. rmask(i,j)==0.0_r8 ) THEN
              plant(i,j,iveg,pdiam)=0.0_r8         !Diameter
              plant(i,j,iveg,phght)=0.0_r8         !Height
              plant(i,j,iveg,pdens)=0.0_r8         !Density
              plant(i,j,iveg,pthck)=0.0_r8         !Thickness
            ELSE
              plant(i,j,iveg,pdiam)=0.07_r8        !Diameter
              plant(i,j,iveg,phght)=6.0_r8         !Height
              plant(i,j,iveg,pdens)=0.36_r8        !Density
              plant(i,j,iveg,pthck)=0.07_r8        !Thickness
              ! plant(i,j,iveg,pdiam)=0.03_r8        !Diameter
              ! plant(i,j,iveg,phght)=6.0_r8         !Height
              ! plant(i,j,iveg,pdens)=16.49270807_r8 !Density
              ! plant(i,j,iveg,pthck)=0.03_r8        !Thickness
            ENDIF
          END DO
        END DO
      END DO
# endif

#elif defined FUKIDO_REEF_NST
# ifdef VEG_DRAG
      if(ng==1) then ! Fukido2 grid
        DO iveg=1,NVEG
          DO j=JstrT,JendT
            DO i=IstrT,IendT
              plant(i,j,iveg,pdiam)=0.0_r8         !Diameter
              plant(i,j,iveg,phght)=0.0_r8         !Height
              plant(i,j,iveg,pdens)=0.0_r8         !Density
              plant(i,j,iveg,pthck)=0.0_r8         !Thickness
            END DO
          END DO
        END DO
        
      else if(ng==2) then ! Fukido3 grid       
        DO iveg=1,NVEG
          DO j=JstrT,JendT
            DO i=IstrT,IendT
              IF(h(i,j)>-0.1_r8 .or. rmask(i,j)==0.0_r8 ) THEN
                plant(i,j,iveg,pdiam)=0.0_r8         !Diameter
                plant(i,j,iveg,phght)=0.0_r8         !Height
                plant(i,j,iveg,pdens)=0.0_r8         !Density
                plant(i,j,iveg,pthck)=0.0_r8         !Thickness
              ELSE
                plant(i,j,iveg,pdiam)=0.05_r8        !Diameter
                plant(i,j,iveg,phght)=2.0_r8         !Height
                plant(i,j,iveg,pdens)=40.0_r8        !Density
                plant(i,j,iveg,pthck)=0.05_r8        !Thickness
              ENDIF
            END DO
          END DO
        END DO
      endif
# endif

#else

# ifdef VEG_DRAG
      DO iveg=1,NVEG
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            plant(i,j,iveg,pdiam)=0.01_r8        !Diameter
            plant(i,j,iveg,phght)=2.0_r8         !Height
            plant(i,j,iveg,pdens)=800.0_r8       !Density
            plant(i,j,iveg,pthck)=0.0005_r8      !Thickness
# ifdef VEGETATION_BIOMASS
            plant(i,j,iveg,pagbm)=0.0_r8         !Above ground Biomass
            plant(i,j,iveg,pbgbm)=0.0_r8         !Below ground Biomass
# endif  
          END DO
        END DO
      END DO
# endif
!      
# ifdef MARSH_WAVE_THRUST
      DO j=Jstr,JendT
        DO i=IstrT,IendT
          marsh_mask(i,j)=1.0_r8 
        END DO 
      END DO  
# endif
#endif            
!                                        
      RETURN

      END SUBROUTINE ana_vegetation_tile
