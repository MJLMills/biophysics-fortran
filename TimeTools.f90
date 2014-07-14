MODULE TimeTools

  IMPLICIT NONE

  INTEGER(8),     PRIVATE :: WallCountMax, WallCountRate, wallStart, wallEnd, wallPrev=0
  REAL(8),        PRIVATE :: wcMax, wcRate
  REAL(4),        PRIVATE :: cpuStart=0.0d0, cpuEnd=0.0d0, cpuPrev=0.0d0
  CHARACTER(100), PRIVATE :: wallTag, prevWallTag

CONTAINS

  SUBROUTINE InitialiseWallClock
    
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

  END SUBROUTINE InitialiseWallClock

!*

  SUBROUTINE StartWallClock
  
    IMPLICIT NONE

    CALL system_clock(wallStart)
    wallPrev = wallStart
    wallTag = "CLOCK STARTED"

  END SUBROUTINE StartWallClock

!*

  SUBROUTINE MeasureWallClock(tag)

    IMPLICIT NONE
    CHARACTER(*) :: tag

    wallPrev = wallEnd
    prevWallTag = wallTag
    wallTag = tag
    CALL system_clock(wallEnd)

  END SUBROUTINE MeasureWallClock

!*

  SUBROUTINE PrintWallTime(measureFromStart)

    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: measureFromStart

    IF (measureFromStart .EQV. .FALSE.) WRITE(*,*) "WALL TIME BETWEEN ", TRIM(ADJUSTL(prevWallTag)), " AND ", TRIM(ADJUSTL(wallTag)), ": ", DBLE(wallEnd - wallPrev)  / wcRate, " s"
    IF (measureFromStart .EQV. .TRUE.)  WRITE(*,*) "WALL TIME BETWEEN CLOCK STARTED AND ",                    TRIM(ADJUSTL(wallTag)), ": ", DBLE(wallEnd - wallStart) / wcRate, " s"
    WRITE(*,*)

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

!*

  SUBROUTINE StartCPUClock

    IMPLICIT NONE

    CALL CPU_TIME(cpuStart)
    cpuPrev = cpuStart

  END SUBROUTINE StartCPUClock

!*

  SUBROUTINE MeasureCPUClock

    IMPLICIT NONE

    cpuPrev = cpuEnd
    CALL CPU_TIME(cpuEnd)

  END SUBROUTINE MeasureCPUClock

!*

  SUBROUTINE PrintCPUTime

    IMPLICIT NONE

    WRITE(*,*) "CPU TIME SINCE PREVIOUS CALL  = ", cpuEnd - cpuPrev
    WRITE(*,*) "CPU TIME SINCE START CALL     = ", cpuEnd - cpuStart

  END SUBROUTINE PrintCPUTime

END MODULE TimeTools
