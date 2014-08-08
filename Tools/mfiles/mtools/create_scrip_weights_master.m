% function create_scrip_weights_master.m
%
% This m file is part of a series of routines driven by "create_scrip_weights_master.m"
%
% This routine is the Master driver m file to create sparse matrix interpolation 
% weights file using SCRIP.
%
% jcwarner 29June2014
%
% set up for Projects/Inlet_test/Refined
%

%%%%%%%  Begin USER Input Section  %%%%%%%%%%%%%

%step 1:  Enter the number of grids for each model:
Ngrids_roms=2;
Ngrids_swan=2;
Ngrids_wrf=0;

%step 2a: Enter the grids for the ROMS model:
roms_grids{1}='inlet_test_grid.nc';
roms_grids{2}='inlet_test_grid_ref3.nc';

%step 2b: Enter the grids for the SWAN model:
%Inlet_test_refined
 swan_coord{1}='inlet_test_grid_coord.grd';
 swan_coord{2}='inlet_test_grid_coord_ref3.grd';
 Numx{1}=77; Numy{1}=72;   % this was for inlet_test_grid_coord
 Numx{2}=92; Numy{2}=50;   % this was for inlet_test_grid_ref3_coord
 cartesian{1}=1;  % this was in meters
 cartesian{2}=1;  % this was in meters
 bath_file{1}='inlet_test_bathy.bot';
 bath_file{2}='inlet_test_bathy_ref3.bot';

%step 2c: Enter the grids for the WRF model:
wrf_grids{1}='wrfinput_d01';

%step 3: enter location of scrip.exe
scrip_exe='c:/work/models/COAWST/Lib/SCRIP/scrip.exe';

%step 4: enter working dir
wdir='c:\work\models\COAWST\Projects\Inlet_test\Refined';

%step 5: Select the process steps to create the SCRIP files.

% step5a) The "create_scrip_files" converts roms, swan, and wrf grids to 
% a format that SCRIP likes.
create_scrip_masks=1;

% step5b) The "create_scrip_weights" calls scrip to compute the weights 
% and produces the netcdf weights files.
create_scrip_files=1;

%%%%%%%  END USER Input Section  %%%%%%%%%%%%%

eval(['cd ',wdir])

if (create_scrip_masks)
  create_scrip_masks_driver
end

if (create_scrip_files)
  create_scrip_files_driver
end

display('finished creating scrip weights')

