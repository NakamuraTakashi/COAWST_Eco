export HDF5_DISABLE_VERSION_CHECK=1
mpirun -use-hwthread-cpus -np 10 ./scrip_coawst.exe scrip_coawst_panay.in 
#