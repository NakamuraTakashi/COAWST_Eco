
!!!=== Copyright (c) 2024 Takashi NAKAMURA  =====

#include "cppdefs.h"

!!!*************** AQUACULTURE *****************************************

MODULE mod_aquaculture
#ifdef AQUACULTURE
  implicit none
  integer, parameter :: Naq = 4    !! Number of aquaculture type
  real(8), parameter :: diam_aqua(5) = [  0.15d0, 0.15d0,  0.1d0,  0.1d0,  0.1d0 ] ! Diameter (m)
  real(8), parameter :: thck_aqua(5) = [  0.15d0, 0.05d0,  0.1d0,  0.1d0,  0.1d0 ] ! Thickness (m)
  real(8), parameter :: hght_aqua(5) = [ 10.0d0 ,  3.0d0,  5.0d0,  5.0d0,  1.0d0 ] ! Height (m)
#endif
END MODULE mod_aquaculture

