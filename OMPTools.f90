MODULE OMPTools

  use omp_lib
  use Toolbox

IMPLICIT NONE

  INTEGER, PARAMETER :: out = 10
  INTEGER            :: ios

  CONTAINS

SUBROUTINE OMP_setup
IMPLICIT NONE

LOGICAL :: isDynamic, isNested
CHARACTER(15), PARAMETER :: fileName = "OpenMP-Data.txt"
10 FORMAT (A32)
20 FORMAT (A32,I4)

OPEN(UNIT=out,FILE=fileName,IOSTAT=ios,STATUS="REPLACE",ACCESS="SEQUENTIAL",FORM="FORMATTED",ACTION="WRITE")
if (ios /= 0) call CheckFileOpen(ios,fileName,out)

WRITE(out,10) "****** OpenMP INFORMATION ******"
WRITE(out,*)

WRITE(out,20) "NUMBER OF PROCESSORS AVAILABLE: ", OMP_get_num_procs()
WRITE(out,20) "MAXIMUM THREADS CURRENTLY SET : ", OMP_get_max_threads()

isDynamic = OMP_get_dynamic()
if (isDynamic .EQV. .TRUE.) then
  WRITE(out,10) "DYNAMIC ADJUSTMENT IS ENABLED   "
else
  WRITE(out,10) "DYNAMIC ADJUSTMENT IS DISABLED  "
endif

isNested = OMP_get_nested() 
if (isNested .EQV. .TRUE.) then  
  WRITE(out,10) "NESTED PARALLELISM IS ENABLED   "
else
  WRITE(out,10) "NESTED PARALLELISM IS DISABLED  "
endif

WRITE(out,*)
WRITE(out,10) "********************************"

END SUBROUTINE OMP_setup

SUBROUTINE GetParallelInformation
IMPLICIT NONE

LOGICAL :: isParallel
10 FORMAT (A32)
20 FORMAT (A32,I4)

isParallel = OMP_in_parallel()

if (isParallel .EQV. .TRUE.) then
  WRITE(out,10) "REGION IS PARALLEL"
  WRITE(out,20) "NUMBER OF ACTIVE THREADS:       ", OMP_get_num_threads()
else
  WRITE(out,*) "REGION IS NOT PARALLEL"
endif

END SUBROUTINE GetParallelInformation

SUBROUTINE OMP_teardown
IMPLICIT NONE

CLOSE(out,IOSTAT=ios)

END SUBROUTINE OMP_teardown

END MODULE OMPTools
