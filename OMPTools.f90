MODULE OMPTools

use omp_lib

IMPLICIT NONE

CONTAINS

SUBROUTINE StartUpChecks
IMPLICIT NONE

LOGICAL :: isDynamic, isNested
10 FORMAT (A32)
20 FORMAT (A32,I4)

WRITE(*,10) "****** OpenMP INFORMATION ******"
WRITE(*,*)

WRITE(*,20) "NUMBER OF PROCESSORS AVAILABLE: ", OMP_get_num_procs()
WRITE(*,20) "MAXIMUM THREADS CURRENTLY SET : ", OMP_get_max_threads()

isDynamic = OMP_get_dynamic()
if (isDynamic .EQV. .TRUE.) then
  WRITE(*,10) "DYNAMIC ADJUSTMENT IS ENABLED   "
else
  WRITE(*,10) "DYNAMIC ADJUSTMENT IS DISABLED  "
endif

isNested = OMP_get_nested() 
if (isNested .EQV. .TRUE.) then  
  WRITE(*,10) "NESTED PARALLELISM IS ENABLED   "
else
  WRITE(*,10) "NESTED PARALLELISM IS DISABLED  "
endif

WRITE(*,*)
WRITE(*,10) "********************************"

END SUBROUTINE StartUpchecks

SUBROUTINE GetParallelInformation
IMPLICIT NONE

LOGICAL :: isParallel
10 FORMAT (A32)
20 FORMAT (A32,I4)

isParallel = OMP_in_parallel()

if (isParallel .EQV. .TRUE.) then
  WRITE(*,10) "REGION IS PARALLEL"
  WRITE(*,20) "NUMBER OF ACTIVE THREADS:       ", OMP_get_num_threads()
else
  WRITE(*,*) "REGION IS NOT PARALLEL"
endif

END SUBROUTINE GetParallelInformation

END MODULE OMPTools
