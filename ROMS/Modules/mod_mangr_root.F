#include "cppdefs.h"
      MODULE mod_mangr_root

/*
** svn $Id: mod_vegarr.F 429 2015-06-10 17:30:26Z arango $
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2016 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
** This module declares Rhizophora mangrove root area for each DBH    **
** class for computation of mangrove vegetation drag.                 **
**                                                                    **
** Note that the  *.h file is located in ROMS/Nonlinear/Vegetation    **
** it is included within <...> to allow the user customize it in the  **
** project directory, while keeping the distributed file intact       **
** (check build scripts for details).                                 **
**                                                                    **
************************************************************************
*/

#if defined VEGETATION
# include <mangr_root_mod.h>
#endif

      END MODULE mod_mangr_root
