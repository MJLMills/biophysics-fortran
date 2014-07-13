MODULE TimeTools

  IMPLICIT NONE

  INTEGER(8) :: WallCountMax, WallCountRate, wallStart, wallEnd
  REAL(8)    :: wcMax, wcRate
  REAL(4)    :: cpuStart, cpuEnd

  CONTAINS

  SUBROUTINE WallClockProperties
    
    IMPLICIT NONE

    call system_clock(count_rate=WallCountRate)
    call system_clock(count_max=WallCountMax)
    wcMax  = DBLE(WallCountMax)
    wcRate = DBLE(WallCountRate)
  
    WRITE(*,'(A29)')          "*** WALL CLOCK PROPERTIES ***"
    WRITE(*,*)
    WRITE(*,'(A25,E10.2,A8)') "COUNT RATE             = ", wcRate, " TICKS/S"
    WRITE(*,'(A25,E10.2)')    "COUNT MAX              = ", wcMax
    WRITE(*,'(A25,E10.2,A5)') "TIME BEFORE WRAPAROUND = ", wcMax / wcRate / (60.0d0*24.0d0), " DAYS"
    WRITE(*,*)
    WRITE(*,'(A3)') "***" 
    WRITE(*,*)

  END SUBROUTINE WallClockProperties

!*

SUBROUTINE StartCPUClock

  IMPLICIT NONE

  CALL CPU_TIME(cpuStart)

END SUBROUTINE StartCPUClock

!*

SUBROUTINE MeasureCPUClock(printTime)

  IMPLICIT NONE
  LOGICAL, INTENT(IN) :: printTime

  CALL CPU_TIME(cpuEnd)
  IF (printTime .EQV. .TRUE.) call PrintCPUTime()

END SUBROUTINE MeasureCPUClock

!*

  SUBROUTINE PrintCPUTime()

    IMPLICIT NONE

    WRITE(*,*) "CPU TIME =  ", cpuStart - cpuEnd

  END SUBROUTINE PrintCPUTime

!*

  SUBROUTINE StartWallClock
  
    IMPLICIT NONE

    CALL system_clock(wallStart)    

  END SUBROUTINE StartWallClock

!*

  SUBROUTINE MeasureWallClock(printTime)

    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: printTime

    CALL system_clock(wallEnd)
    IF (printTime .EQV. .TRUE.) CALL PrintWallTime()

  END SUBROUTINE MeasureWallClock

!*

  SUBROUTINE PrintWallTime

    IMPLICIT NONE

    WRITE(*,*) "WALL TIME = ", DBLE(wallEnd - wallStart) / wcRate

  END SUBROUTINE PrintWallTime

!*

  SUBROUTINE PrintTimeAndDate
  
    IMPLICIT NONE

    CHARACTER(8)  :: date
    CHARACTER(10) :: time
    CHARACTER(5)  :: zone

    CALL date_and_time(DATE=date,TIME=time,ZONE=zone)
    WRITE(*,'(A21)') "*** TIME AND DATE ***"
    WRITE(*,*)
    WRITE(*,'(A6,A4,A1,A2,A1,A2,1X,A6,A2,A1,A2,A1,A2,1X,A10,A5)') "DATE: ",date(1:4),"-",date(5:6),"-",date(7:8),"TIME: ",time(1:2),":",time(3:4),":",time(5:6),"ZONE: UTC+",zone
    WRITE(*,*)
    WRITE(*,'(A3)') "***"
    WRITE(*,*)

    END SUBROUTINE PrintTimeAndDate

END MODULE TimeTools
