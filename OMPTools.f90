MODULE OMPTools

  use omp_lib
  use Toolbox

  IMPLICIT NONE

  NAMELIST /OMP/ numThreads, dynamicAllocation, nestedParallelism, useNamelist
  INTEGER :: numThreads
  LOGICAL :: dynamicAllocation, nestedParallelism, useNameList

  INTEGER, PARAMETER, PRIVATE       :: out = 10, nml = 20
  INTEGER, PRIVATE                  :: ios
  CHARACTER(15), PARAMETER, PRIVATE :: fileName = "OpenMP-Data.txt"
  LOGICAL, PRIVATE                  :: dynamicPossible = .FALSE., nestedPossible = .FALSE.

CONTAINS

!*

  SUBROUTINE OMP_setup(exitTest)

    IMPLICIT NONE

    LOGICAL, INTENT(OUT) :: exitTest

    10 FORMAT (A32)

    OPEN(UNIT=out,FILE=fileName,IOSTAT=ios,STATUS="REPLACE",ACCESS="SEQUENTIAL",FORM="FORMATTED",ACTION="WRITE")
    IF (ios /= 0) then
      CALL CheckFileOpen(ios,fileName,out)
      exitTest = .FALSE.
    ELSE
      WRITE(out,10) "****** OpenMP INFORMATION ******"
      CALL OMP_check_capabilities()
      CALL OMP_read_namelist()
      IF (numThreads <= 0) THEN
        WRITE(*,*) "ERROR IN OMP_setup: LESS THAN ONE THREAD SPECIFIED"
        exitTest = .FALSE.
      ELSE
        IF (useNamelist .EQV. .TRUE.) THEN
          CALL OMP_set_num_threads(numThreads)
          IF (dynamicPossible .EQV. .TRUE.) CALL OMP_set_dynamic(dynamicAllocation)
          IF (nestedPossible  .EQV. .TRUE.) CALL OMP_set_nested(nestedParallelism)
          CALL OMP_get_general_info("USING NAMELIST VALUES")
        ELSE
          CALL OMP_get_general_info("USING ENV/COMPILER DEFAULT VALUES")
        ENDIF
        exitTest = .TRUE.
      ENDIF
    ENDIF

  END SUBROUTINE OMP_setup

!*

  SUBROUTINE OMP_get_general_info(message)

    IMPLICIT NONE
  
    LOGICAL :: isDynamic, isNested
    CHARACTER(*) :: message
    10 FORMAT (A32)
    20 FORMAT (A32,I4)

    WRITE(out,*)
    WRITE(out,*) "*** "//trim(adjustl(message))//" ***"
    WRITE(out,*)
    WRITE(out,20) "NUMBER OF PROCESSORS AVAILABLE: ", OMP_get_num_procs()
    WRITE(out,20) "MAXIMUM THREADS CURRENTLY SET : ", OMP_get_max_threads()

    isDynamic = OMP_get_dynamic()
    IF (isDynamic .EQV. .TRUE.) THEN
      WRITE(out,10) "DYNAMIC ADJUSTMENT IS ENABLED   "
    ELSE
      WRITE(out,10) "DYNAMIC ADJUSTMENT IS DISABLED  "
    ENDIF

    isNested = OMP_get_nested()
    IF (isNested .EQV. .TRUE.) THEN
      WRITE(out,10) "NESTED PARALLELISM IS ENABLED   "
    ELSE
      WRITE(out,10) "NESTED PARALLELISM IS DISABLED  "
    ENDIF

  END SUBROUTINE OMP_get_general_info

!*

  SUBROUTINE OMP_get_parallel_info(message)

    IMPLICIT NONE

    LOGICAL :: isParallel
    CHARACTER(*) :: message
    10 FORMAT (A32)
    20 FORMAT (A32,I4)

    WRITE(out,*)
    WRITE(out,10) "*** "//trim(adjustl(message))//" ***"
    WRITE(out,*)

    isParallel = OMP_in_parallel()

    IF (isParallel .EQV. .TRUE.) then
      WRITE(out,10) "REGION IS PARALLEL"
      WRITE(out,20) "NUMBER OF ACTIVE THREADS:       ", OMP_get_num_threads()
    ELSE
      WRITE(out,10) "REGION IS NOT PARALLEL"
    ENDIF

  END SUBROUTINE OMP_get_parallel_info

!*

  SUBROUTINE OMP_check_capabilities

    IMPLICIT NONE

    LOGICAL :: active, store
    10 FORMAT (A32)

    WRITE(out,*)
    WRITE(out,*) "*** CAPABILITY CHECK ***"
    WRITE(out,*)

    store = OMP_get_dynamic()
    CALL OMP_set_dynamic(.TRUE.)
    active = OMP_get_dynamic()
    IF (active .EQV. .TRUE.) THEN
      WRITE(out,10) "DYNAMIC ADJUSTMENT IS AVAILABLE "
      dynamicPossible = .TRUE.
    ELSE
      WRITE(out,10) "DYNAMIC ADJUSTMENT NOT AVAILABLE"  
    ENDIF
    CALL OMP_set_dynamic(store)

    store = OMP_get_nested()
    CALL OMP_set_nested(.TRUE.)
    active = OMP_get_nested()
    IF (active .EQV. .TRUE.) THEN
      WRITE(out,10) "NESTED PARALLELISM IS AVAILABLE "
      nestedPossible = .TRUE.
    ELSE
      WRITE(out,10) "NESTED PARALLELISM NOT AVAILABLE"
    ENDIF
    CALL OMP_set_nested(store)

  END SUBROUTINE OMP_check_capabilities

!*

  SUBROUTINE OMP_teardown

    IMPLICIT NONE
  
    INTEGER :: ios
    10 FORMAT (A32)

    WRITE(out,*)
    WRITE(out,10) "********************************"

    CLOSE(out,IOSTAT=ios)
    CALL CheckFileClose(ios,fileName,out)

  END SUBROUTINE OMP_teardown

!*

  SUBROUTINE OMP_read_namelist

    IMPLICIT NONE

    !Set the default values for these variables if they are not in the namelist
    numThreads = OMP_get_num_threads()
    dynamicAllocation = .FALSE.
    nestedParallelism = .FALSE.
    useNamelist = .FALSE.

    OPEN(nml,FILE='OMP.nml',IOSTAT=ios)
    IF (ios /= 0) CALL CheckFileOpen(ios,fileName,nml)

    READ(nml,nml=OMP)

    CLOSE(nml,IOSTAT=ios)
    IF (ios /= 0) CALL CheckFileClose(ios,fileName,nml)

  END SUBROUTINE OMP_read_namelist

END MODULE OMPTools
