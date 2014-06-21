MODULE VectorMath

IMPLICIT NONE

CONTAINS

!**** FUNCTIONS RETURNING SCALARS

  PURE REAL(8) FUNCTION DotProduct(p,q,n) result(pdotq); IMPLICIT NONE

  integer, intent(in) :: n
  real(8), intent(in) :: p(n), q(n)
  integer :: i

    pdotq = 0.0d0

    do i = 1, n
      pdotq = pdotq + (p(i) * q(i))
    enddo

  END FUNCTION DotProduct

!*

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

!*

  PURE REAL(8) FUNCTION EuclideanNorm(vector,n) result(r); IMPLICIT NONE

  integer, intent(in) :: n
  real(8), intent(in) :: vector(n)

    r = 0.0d0; r = dsqrt(DotProduct(vector,vector,n))

  END FUNCTION EuclideanNorm

!**** FUNCTIONS RETURNING VECTORS

  PURE FUNCTION NormaliseVector(p,n); IMPLICIT NONE

  integer, intent(in) :: n
  real(8), intent(in) :: p(n)
  real(8) :: NormaliseVector(n)

    NormaliseVector = p / EuclideanNorm(p,n)

  END FUNCTION NormaliseVector

!*

  PURE FUNCTION CrossProduct(p,q); IMPLICIT NONE

  real(8), intent(in)  :: p(3), q(3)
  real(8) :: CrossProduct(3)

    CrossProduct(1) = p(2)*q(3) - p(3)*q(2)
    CrossProduct(2) = p(3)*q(1) - p(1)*q(3)
    CrossProduct(3) = p(1)*q(2) - p(2)*q(1)

  END FUNCTION CrossProduct

!*

END MODULE VectorMath
