biophysics-fortran
==================

This is the calculation engine for dynamics runs.

This program takes an input file generated by a GUI and runs the appropriate
calculation, ideally faster than it would run in the GUI.

Shared Memory Runs with OpenMP

To use OpenMP capabilities, edit the namelist OMP.nml. Here you can set the max number 
of threads, and turn on/off the dynamic allocation and nested parallelism features of 
OpenMP. In addition, if you would like to use the environment variables

  $OMP_NUM_THREADS, $OMP_DYNAMIC & $OMP_NESTED

rather than the namelist, then you can set the variable 'useNameList' to .FALSE., 
in which case the compiler defaults will be used, unless the above environment 
variables are set, in which case they take precedence. 
Any variables omitted from the namelist will go to their program defaults:

numThreads = OMP_get_num_threads()
dynamicAllocation = .FALSE.
nestedParallelism = .FALSE.
useNamelist = .TRUE.

The OMPTools module is designed to allow the program to setup the OMP environment,
print the status of its variables and print the properties of a parallel region, 
where that latter two of these are intended for debugging and testing the
parallelisation of the application. It consists of the following routines:

  OMP_setup(exitTest)

This subroutine attempts setup of the OMP environment, returning .TRUE. if no
pathological errors are found. A log file is created called "OpenMP-Data.txt"
which is used to store all output from the module.

  OMP_check_capabilities()

This subroutine tests whether dynamic allocation and nested parallelism are
available to the user.

  OMP_teardown()

This subroutine cleans up the module, closing the output file.

  OMP_read_namelist()

This subroutine sets up the program defaults for the OMP variables that will be
used for any variables not specified by the user. It then attempts to read the 
namelist OMP.nml. If succesful, the values in that file are honored and program
defaults are used for the variables. If the file is not present or not properly
formatted, the compiler defaults will be used.

  OMP_get_parallel_info(message)

This subroutine tests whether the current region is parallel, and if so returns
the information made available by the OMP API about the region. The output is 
labelled with the message passed to the routine.

  OMP_get_general_info(message)

This subroutine prints general information about the OMP environment that is
available to the program via the OMP API. The output is labelled with the 
message passed to the routine.

        Timing

Three intrinsic routines are used to time the execution. CPU_TIME gives the CPU
time, SYSTEM_CLOCK gives the wall time and DATE_AND_TIME gives the date and time.
These are wrapped in a module named TimeTools.f90, which allows the starting and
measurement of the two clocks, and the printing of the differences in time. A logical
flag tells the measurement routines whether to print or not. In comparing CPU and wall
times, it is recommended that you measure the two times first, then print them to avoid
delay.
