PROGRAM Interface

IMPLICIT NONE

integer :: nSteps, step
character(len=8), allocatable :: CalculationArray(:)

!Determine the calculation type array requested, or expand a single keyword to an array.
do step=1,nSteps
  !Log the start time
  select case (trim(adjustl(CalculationArray(step))))
  case ("PROPAGATE")
  case default
    !Calculation type is not recognised
  end select
  !Print the end time
enddo

ENDPROGRAM Interface
