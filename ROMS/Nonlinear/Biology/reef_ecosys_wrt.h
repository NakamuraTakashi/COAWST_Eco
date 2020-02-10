/*
** svn $Id: fennel_wrt.h 585 2012-01-03 18:44:28Z arango $
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2012 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
*************************************************** Takashi Nakamura ***
**                                                                    **
**  Writes Coral reef ecosystem model input parameters into           **
**  output NetCDF files. It is included in routine "wrt_info.F".      **
**                                                                    **
************************************************************************
*/
!
!  Write out Coral reef ecosystem model parameters.
!
!      CALL netcdf_put_ivar (ng, model, ncname, 'CrlIter',               &
!     &                      CrlIter(ng), (/0/), (/0/),                  &
!     &                      ncid = ncid)
!      IF (exit_flag.ne.NoError) RETURN
!
!      CALL netcdf_put_ivar (ng, model, ncname, 'SedIter',               &
!     &                      SedIter(ng), (/0/), (/0/),                  &
!     &                      ncid = ncid)
!      IF (exit_flag.ne.NoError) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'PARfrac',               &
     &                      PARfrac(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (exit_flag.ne.NoError) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'pCO2air',               &
     &                      pCO2air(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (exit_flag.ne.NoError) RETURN
      
