MODULE Toolbox

IMPLICIT NONE

CONTAINS

SUBROUTINE CheckFileOpen(ios,fileName,openUnit)
IMPLICIT NONE

INTEGER, INTENT(IN) :: ios, openUnit
CHARACTER(*), INTENT(IN) :: fileName

if (ios < 0) then
  WRITE(*,*) "END-OF-FILE OR END-OF-RECORD OCCURED OPENING", trim(adjustl(fileName)), "ON UNIT", openUnit
else if (ios > 0) then
  WRITE(*,*) "ERROR CONDITION OCCURED OPENING", trim(adjustl(fileName)) ,"ON UNIT", openUnit
endif

END SUBROUTINE CheckFileOpen

SUBROUTINE CheckFileClose(ios,fileName,unit)
IMPLICIT NONE

INTEGER, INTENT(IN) :: ios, unit
CHARACTER(*), INTENT(IN) :: fileName

if (ios < 0) then
  WRITE(*,*) "END-OF-FILE OR END-OF-RECORD OCCURED CLOSING", trim(adjustl(fileName)), "ON UNIT", unit
else if (ios > 0) then
  WRITE(*,*) "ERROR CONDITION OCCURED CLOSING", trim(adjustl(fileName)) ,"ON UNIT", unit
endif

ENDSUBROUTINE CheckFileClose


  PURE REAL(8) FUNCTION CleanTrigArgument(x) result(x_clean); IMPLICIT NONE

  real(8), intent(in) :: x

    if (abs(x - 1.0D0) < 1.0D-7) then
      x_clean = 1.0D0
    else if (abs(x+1.0D0) < 1.0D-7) then
      x_clean = -1.0D0
    else
      x_clean = x
    endif

  END FUNCTION CleanTrigArgument

END MODULE Toolbox
