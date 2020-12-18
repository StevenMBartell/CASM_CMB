# CASM_CMB
Source code for combined farm pond, stream, and wetland CASM

The casm_CMB.f file is the fortran source code for the combined CASM version. This code has been compiled and executed using gfortran under Windows 10.

The casmCMB_control.dat file is read by the casm_CMB application to identify the necessary input files and other input information to execute the casm_CMB application. The current control file is for a simulation of the wetland version of the model. Remarks describing the control file inputs are provided in the file. 

File naming conventions

.\CMB_Bio_Data\casm_WL_bio_parms.dat contains the bioenergetics and habitat parameters for the populations of aquatic producers and consumers in the wetland food web

.\CMB_Web_Data\casm_WL.dat           contains the prey preference, assimilation efficiency, and handling efficiency for the wetland trophic interactions

.\CMB_Env_Data\env_casmWL_Scenario2.prn contains the daily values of the environmental input data required by the model


The above sub-directories are assumed to be found at c:\casm_CMB that the user must create and populate with the necessary file prior to execution. 
