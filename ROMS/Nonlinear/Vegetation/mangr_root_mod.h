  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Calculate vertical distribution of the whole-tree root projected area
  ! (m2/tree) for the given stem diameter (DBH, m) based on allometric
  ! relationship for Rhizophora species.
  !
  ! Reference:
  ! Yoshikai et al. (2021) Estuarine, Coastal and Shelf Science 248: 107014
  ! Scaling relations and substrate conditions controlling the complexity of
  ! Rhizophora prop root system.
  !
  ! !USES:
  USE mod_kinds
  !
  ! !PUBLIC TYPES:
  implicit none
  !
  ! *** Input parameters ***
  !
  ! integer,  parameter :: mangr_DBH_class  = 40       ! Number of DBH class (0.01-0.40 m)
  ! real(r8), parameter :: mangr_d_DBH      = 0.01_r8  ! Interval of DBH to compute root area (m)
  integer,  parameter :: mangr_DBH_class  = 400      ! Number of DBH class (0.01-0.40 m)
  real(r8), parameter :: mangr_d_DBH      = 0.001_r8 ! Interval of DBH to compute root area (m)
  real(r8), parameter :: mangr_layer_thck = 0.05_r8  ! Layer thickness (m/layer)
#if defined MANGR_MAZA
  integer,  parameter :: mangr_n_layer    = 50       ! Number of vertical layers
  real(r8), parameter :: mangr_azi_effect = 1.00_r8  ! Reduction factor of aximuth angle for projected area
# elif defined MANGR_BAK_ACTUAL
  integer,  parameter :: mangr_n_layer    = 40       ! Number of vertical layers
  real(r8), parameter :: mangr_azi_effect = 1.00_r8  ! Reduction factor of aximuth angle for projected area
#else
  integer,  parameter :: mangr_n_layer    = 40       ! Number of vertical layers
  real(r8), parameter :: mangr_azi_effect = 0.80_r8  ! Reduction factor of aximuth angle for projected area
#endif
#if defined BAK_EXPERI_STEMFAC
# if defined MANGR_BAK_ACTUAL
  real(r8), parameter :: multistem_fac    = 2.5_r8   !  Factor for multiple stems for Bakhawan drag experiment (stems/tree)
# else
  real(r8), parameter :: multistem_fac    = 2.5_r8   !  Factor for multiple stems for Bakhawan drag experiment (stems/tree)
# endif
#endif
  !
  ! *** Calculated variables ***
  !
  real(r8), pointer :: mangr_root_farea(:,:)        ! Root projected area, m2/tree (DBH, layer)
  real(r8), pointer :: mangr_root_dens (:,:)        ! Root density, root/tree (DBH, layer)
  !
  ! *** Other variables ***
  !
  real(r8), parameter :: PI = 3.14159265359_r8
  real(r8), parameter :: deg2rad = PI/180.0_r8
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  PUBLIC  :: calc_root_farea
  !
  ! !PRIVATE MEMBER FUNCTIONS:
  PRIVATE :: mangr_root_model
#if defined MANGR_MAZA
  PRIVATE :: mangr_root_maza
#endif
#if defined MANGR_BAK_ACTUAL
  PRIVATE :: mangr_root_bak
#endif
  !-----------------------------------------------------------------------

CONTAINS

  !-----------------------------------------------------------------------
  SUBROUTINE calc_root_farea ( &
                                       ! *** Input ***
                                       !
    )
    !
    ! !DESCRIPTION:
    ! Driver of the Rhizophora root model
    !
    ! !USES:
    USE mod_vegetation, only : HR_A_ROOT, HR_B_ROOT, S_A_ROOT, S_B_ROOT
    !
    ! !ARGUMENTS:
    implicit none
    !
    ! !LOCAL VARIABLES:
    real(r8) :: DBH, HRmax, d_scale                 ! DBH, maximum root height, scaling factor
    integer  :: i                                   ! Miscellaneous use
    real(r8), dimension(mangr_n_layer) :: root_out1 ! Vertical profile of root projected area (m2/layer)
    real(r8), dimension(mangr_n_layer) :: root_out2 ! Vertical profile of number of roots (root/layer)
    !---------------------------------------------------------------------

    ! Allocate variables

    allocate(mangr_root_farea(mangr_DBH_class, mangr_n_layer))
    allocate(mangr_root_dens (mangr_DBH_class, mangr_n_layer))

    ! Run root model for each DBH class

    DO i=1,mangr_DBH_class

      ! DBH to compute (m)
      DBH = mangr_d_DBH * dble(i)
      ! HRmax (m)
      HRmax = DBH * HR_A_ROOT + HR_B_ROOT
      ! Scaling factor
      d_scale = 1.0_r8 - (10.0_r8**S_B_ROOT) * (DBH**S_A_ROOT)

      CALL mangr_root_model ( &
                                           ! *** Input ***
      HRmax                           , &  ! Maximum root height (m)
      d_scale                         , &  ! Scaling factor
                                           !
                                           ! *** Output ***
      root_out1                       , &  ! Vertical profile of root projected area (m2/layer)
      root_out2                         &  ! Vertical profile of number of roots (root/layer)
      )

#if defined MANGR_MAZA
      ! If the condition used in Maza et al. (2017) is being used,
      ! the followwing function is used to give root projected area
      ! and number of roots for all DBH class.
      CALL mangr_root_maza ( &
                                           ! *** Output ***
      root_out1                       , &  ! Vertical profile of root projected area (m2/layer)
      root_out2                         &  ! Vertical profile of number of roots (root/layer)
      )
#endif
#if defined MANGR_BAK_ACTUAL
      ! This gives the actual measured root projected area
      ! and number of roots for all DBH class.
      CALL mangr_root_bak ( &
                                           ! *** Output ***
      root_out1                       , &  ! Vertical profile of root projected area (m2/layer)
      root_out2                         &  ! Vertical profile of number of roots (root/layer)
      )
#endif

      ! Root projected area of the DBH class, m2/tree
      mangr_root_farea(i,:) = root_out1
      ! Root density of the DBH class, root/tree
      mangr_root_dens(i,:) = root_out2

    END DO

  RETURN
  END SUBROUTINE calc_root_farea

  !-----------------------------------------------------------------------
  SUBROUTINE mangr_root_model ( &
                                       ! *** Input ***
    HRmax                         , &  ! Maximum root height (m)
    d_scale                       , &  ! Scaling factor
                                       !
                                       ! *** Output ***
    root_out1                     , &  ! Vertical profile of root projected area (m2/layer)
    root_out2                       &  ! Vertical profile of number of roots (root/layer)
    )
    !
    ! !DESCRIPTION:
    ! Calculate vertical profile of root projected area
    ! for given DBH
    !
    ! !USES:
    USE mod_vegetation, only : HRMIN_ROOT, THETA_ROOT, FAI_ROOT
    !
    ! !ARGUMENTS:
    implicit none
    real(r8), intent(in)  :: HRmax, d_scale
    real(r8), intent(out) :: root_out1(:), root_out2(:)
    !
    ! !LOCAL VARIABLES:
    integer, parameter                 :: max_no = 10000  ! Maximum number of roots
    real(r8), dimension(max_no)        :: hr_mat          ! Compilation of all root height
    real(r8), dimension(mangr_n_layer) :: h_layer         ! Height of upper boundary of each vertical layer
    real(r8), dimension(mangr_n_layer) :: nroot_out       ! Vertical profile of number of roots (root/layer)
    real(r8), dimension(mangr_n_layer) :: aroot_out       ! Vertical profile of root projected area (m2/layer)
    real(r8), dimension(mangr_n_layer) :: length_out      ! Root length in vertical layers
    real(r8) :: hr                                        ! Root height
    real(r8) :: root_sl                                   ! Slope of the root
    integer  :: i, z, count                               ! Miscellaneous use
    real(r8) :: z_start, z_end, x_ini, x_end, val         ! Miscellaneous use
    !---------------------------------------------------------------------

    ! Zero out

    nroot_out(:) = 0.0_r8
    aroot_out(:) = 0.0_r8
    hr_mat(:) = 0.0_r8

    ! Height of upper boundary of each vertical layer

    DO z=1,mangr_n_layer

      h_layer(z) = dble(z)*mangr_layer_thck

    END DO

    ! --- Computation of height of roots

    ! Initial root height (m)

    hr = HRmax

    ! Loop from the highest to the lowest root

    count = 1
    hr_mat(count) = hr
    DO i=1,max_no

      IF (hr <= HRMIN_ROOT) EXIT

      ! Loop for vertical layer

      DO z=1,mangr_n_layer
        IF (hr > h_layer(z)) THEN
          nroot_out(z) = nroot_out(z) + 1.0_r8
        ELSE
          IF (z == 1) then
            nroot_out(z) = nroot_out(z) + hr/mangr_layer_thck
          ELSEIF (hr > h_layer(z-1)) then
            nroot_out(z) = nroot_out(z) + (hr - h_layer(z-1))/mangr_layer_thck
          END IF
        END IF
      END DO

      ! Next root height

      hr = hr*d_scale
      count = count + 1
      hr_mat(count) = hr

    END DO

    ! --- Computation of vertical profile of root density and projected area per layer

    root_sl = tan(THETA_ROOT*deg2rad)

    DO i=1,max_no

      ! Height of the root (m)

      hr = hr_mat(i)

      ! Calculate length of each root in each layer

      ! Zero out

      length_out(:) = 0.0_r8

      DO z=1,mangr_n_layer

        ! z-axis boundary

        z_start = h_layer(z)            ! Upper boundary
        z_end   = z_start - mangr_layer_thck  ! Lower boundary

        IF (z_end < hr) THEN  ! Root in the layer

          IF (z_start > hr) then
            z_start = hr
          END IF

          ! Root length in the layer

          x_ini = (z_start-hr)/root_sl
          x_end = (z_end-hr)/root_sl
          val = sqrt(1.0_r8+root_sl**2.0_r8)*(x_end-x_ini)

        ELSE  ! No root in the layer

          val = 0.0_r8

        END IF

        length_out(z) = length_out(z) + val

      END DO

      aroot_out = aroot_out + length_out(:)*FAI_ROOT

    END DO

    root_out1 = aroot_out
    root_out2 = nroot_out

  RETURN
  END SUBROUTINE mangr_root_model

#if defined MANGR_MAZA
  !-----------------------------------------------------------------------
  SUBROUTINE mangr_root_maza ( &
                                       ! *** Output ***
    root_out1                     , &  ! Vertical profile of root projected area (m2/layer)
    root_out2                       &  ! Vertical profile of number of roots (root/layer)
    )
    !
    ! !DESCRIPTION:
    ! Give vertical profile of root projected area of Rhizophora model
    ! used in Maza et al. (2017)
    !
    ! !USES:
    !
    ! !ARGUMENTS:
    implicit none
    real(r8), intent(out) :: root_out1(:), root_out2(:)
    !
    ! !LOCAL VARIABLES:
    real(r8), dimension(mangr_n_layer) :: nroot_out       ! Vertical profile of number of roots (root/layer)
    real(r8), dimension(mangr_n_layer) :: aroot_out       ! Vertical profile of root projected area (m2/layer)
    !---------------------------------------------------------------------

    ! Zero out

    nroot_out(:) = 0.0_r8
    aroot_out(:) = 0.0_r8

    ! Give values used in Maza et al. (2017)
    ! Note that these are for layer thickness fixed as 0.05 m.

    aroot_out(1) = 0.054316378_r8
    aroot_out(2) = 0.054316378_r8
    aroot_out(3) = 0.054579798_r8
    aroot_out(4) = 0.053969491_r8
    aroot_out(5) = 0.052562152_r8
    aroot_out(6) = 0.051136016_r8
    aroot_out(7) = 0.049611078_r8
    aroot_out(8) = 0.048072662_r8
    aroot_out(9) = 0.047011232_r8
    aroot_out(10) = 0.046351542_r8
    aroot_out(11) = 0.045590367_r8
    aroot_out(12) = 0.044336397_r8
    aroot_out(13) = 0.043018323_r8
    aroot_out(14) = 0.042045593_r8
    aroot_out(15) = 0.041341923_r8
    aroot_out(16) = 0.040528983_r8
    aroot_out(17) = 0.039225042_r8
    aroot_out(18) = 0.037862008_r8
    aroot_out(19) = 0.036549868_r8
    aroot_out(20) = 0.035274468_r8
    aroot_out(21) = 0.033920837_r8
    aroot_out(22) = 0.032241452_r8
    aroot_out(23) = 0.030529075_r8
    aroot_out(24) = 0.029110227_r8
    aroot_out(25) = 0.027887968_r8
    aroot_out(26) = 0.026732914_r8
    aroot_out(27) = 0.025966759_r8
    aroot_out(28) = 0.025262880_r8
    aroot_out(29) = 0.023988191_r8
    aroot_out(30) = 0.022185038_r8
    aroot_out(31) = 0.020499241_r8
    aroot_out(32) = 0.019439398_r8
    aroot_out(33) = 0.018470954_r8
    aroot_out(34) = 0.016910114_r8
    aroot_out(35) = 0.014843085_r8
    aroot_out(36) = 0.012749365_r8
    aroot_out(37) = 0.010524162_r8
    aroot_out(38) = 0.008279672_r8
    aroot_out(39) = 0.005577183_r8
    aroot_out(40) = 0.002512797_r8
    aroot_out(41) = 0.000156897_r8
    aroot_out(42:mangr_n_layer) = 0.0_r8

    nroot_out(1) = 24.0_r8
    nroot_out(2) = 24.0_r8
    nroot_out(3) = 24.0_r8
    nroot_out(4) = 24.0_r8
    nroot_out(5) = 24.0_r8
    nroot_out(6) = 23.0_r8
    nroot_out(7) = 23.0_r8
    nroot_out(8) = 22.0_r8
    nroot_out(9) = 22.0_r8
    nroot_out(10) = 21.0_r8
    nroot_out(11) = 20.0_r8
    nroot_out(12) = 20.0_r8
    nroot_out(13) = 19.0_r8
    nroot_out(14) = 18.0_r8
    nroot_out(15) = 18.0_r8
    nroot_out(16) = 17.0_r8
    nroot_out(17) = 16.0_r8
    nroot_out(18) = 16.0_r8
    nroot_out(19) = 15.0_r8
    nroot_out(20) = 14.0_r8
    nroot_out(21) = 14.0_r8
    nroot_out(22) = 13.0_r8
    nroot_out(23) = 12.0_r8
    nroot_out(24) = 12.0_r8
    nroot_out(25) = 11.0_r8
    nroot_out(26) = 10.0_r8
    nroot_out(27) = 10.0_r8
    nroot_out(28) = 9.0_r8
    nroot_out(29) = 8.0_r8
    nroot_out(30) = 8.0_r8
    nroot_out(31) = 7.0_r8
    nroot_out(32) = 7.0_r8
    nroot_out(33) = 6.0_r8
    nroot_out(34) = 5.0_r8
    nroot_out(35) = 5.0_r8
    nroot_out(36) = 4.0_r8
    nroot_out(37) = 3.0_r8
    nroot_out(38) = 3.0_r8
    nroot_out(39) = 2.0_r8
    nroot_out(40) = 1.0_r8
    nroot_out(41) = 1.0_r8
    nroot_out(42:mangr_n_layer) = 0.0_r8

    root_out1 = aroot_out
    root_out2 = nroot_out

  RETURN
  END SUBROUTINE mangr_root_maza
#endif
#if defined MANGR_BAK_ACTUAL
  !-----------------------------------------------------------------------
  SUBROUTINE mangr_root_bak ( &
                                       ! *** Output ***
    root_out1                     , &  ! Vertical profile of root projected area (m2/layer)
    root_out2                       &  ! Vertical profile of number of roots (root/layer)
    )
    !
    ! !DESCRIPTION:
    ! Give vertical profile of the "actual" root projected area
    ! of the Bakhawan Ecopark experiment plot
    !
    ! !USES:
    !
    ! !ARGUMENTS:
    implicit none
    real(r8), intent(out) :: root_out1(:), root_out2(:)
    !
    ! !LOCAL VARIABLES:
    real(r8), dimension(mangr_n_layer) :: nroot_out       ! Vertical profile of number of roots (root/layer)
    real(r8), dimension(mangr_n_layer) :: aroot_out       ! Vertical profile of root projected area (m2/layer)
    !---------------------------------------------------------------------

    ! Zero out

    nroot_out(:) = 0.0_r8
    aroot_out(:) = 0.0_r8

    ! Give values of actual condition
    ! Note that these are for layer thickness fixed as 0.05 m.
    ! Also, this corresponds to tree density of 0.3584 m-2.

    aroot_out(1) = 0.128757401_r8
    aroot_out(2) = 0.111103101_r8
    aroot_out(3) = 0.082579169_r8
    aroot_out(4) = 0.061466239_r8
    aroot_out(5) = 0.048152763_r8
    aroot_out(6) = 0.040259927_r8
    aroot_out(7) = 0.036104646_r8
    aroot_out(8) = 0.032257539_r8
    aroot_out(9) = 0.029615124_r8
    aroot_out(10) = 0.026433825_r8
    aroot_out(11) = 0.024197622_r8
    aroot_out(12) = 0.022901343_r8
    aroot_out(13) = 0.020587299_r8
    aroot_out(14) = 0.018805253_r8
    aroot_out(15) = 0.016870209_r8
    aroot_out(16) = 0.014270559_r8
    aroot_out(17) = 0.012824193_r8
    aroot_out(18) = 0.011455247_r8
    aroot_out(19) = 0.009908328_r8
    aroot_out(20) = 0.008476292_r8
    aroot_out(21) = 0.007612886_r8
    aroot_out(22) = 0.00547898_r8
    aroot_out(23) = 0.004251895_r8
    aroot_out(24) = 0.003606965_r8
    aroot_out(25) = 0.003473731_r8
    aroot_out(26) = 0.002321076_r8
    aroot_out(27) = 0.001361337_r8
    aroot_out(28) = 0.0012188_r8
    aroot_out(29) = 0.001168506_r8
    aroot_out(30) = 0.000840959_r8
    aroot_out(31) = 0.000583663_r8
    aroot_out(32) = 0.000603331_r8
    aroot_out(33) = 0.000502164_r8
    aroot_out(34) = 0.000215112_r8
    aroot_out(35:mangr_n_layer) = 0.0_r8

    nroot_out(1) = 62.21739129_r8
    nroot_out(2) = 54.78260871_r8
    nroot_out(3) = 42.04347826_r8
    nroot_out(4) = 32.26086956_r8
    nroot_out(5) = 25.91304348_r8
    nroot_out(6) = 22.73913044_r8
    nroot_out(7) = 20.04347826_r8
    nroot_out(8) = 18.04347826_r8
    nroot_out(9) = 16.2173913_r8
    nroot_out(10) = 14.39130435_r8
    nroot_out(11) = 12.86956522_r8
    nroot_out(12) = 11.82608696_r8
    nroot_out(13) = 10.60869565_r8
    nroot_out(14) = 9.347826088_r8
    nroot_out(15) = 8.217391303_r8
    nroot_out(16) = 7.0_r8
    nroot_out(17) = 5.956521738_r8
    nroot_out(18) = 5.173913044_r8
    nroot_out(19) = 4.565217391_r8
    nroot_out(20) = 3.826086956_r8
    nroot_out(21) = 3.130434782_r8
    nroot_out(22) = 2.260869565_r8
    nroot_out(23) = 1.826086956_r8
    nroot_out(24) = 1.391304347_r8
    nroot_out(25) = 1.043478262_r8
    nroot_out(26) = 0.826086956_r8
    nroot_out(27) = 0.565217391_r8
    nroot_out(28) = 0.434782609_r8
    nroot_out(29) = 0.347826088_r8
    nroot_out(30) = 0.304347826_r8
    nroot_out(31) = 0.173913044_r8
    nroot_out(32) = 0.130434782_r8
    nroot_out(33) = 0.043478262_r8
    nroot_out(34) = 0.0_r8
    nroot_out(35:mangr_n_layer) = 0.0_r8

    root_out1 = aroot_out
    root_out2 = nroot_out

  RETURN
  END SUBROUTINE mangr_root_bak
#endif
