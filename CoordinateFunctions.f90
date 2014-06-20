MODULE CoordinateFunctions
IMPLICIT NONE

  CONTAINS

!*
  
  PURE REAL(8) FUNCTION EuclideanDistance(origin,coords,n) result(r)

  real(8), intent(in) :: origin(n), coords(n)
  integer, intent(in) :: n

    r = dsqrt(DotProduct(coords(:) - origin(:),coords(:) - origin(:),n))
  
  END FUNCTION EuclideanDistance

!*

  PURE REAL(8) FUNCTION Angle(origin,a,b) result(theta)

  real(8), intent(in) :: a(3), b(3), origin(3)
  real(8) :: ao(3), bo(3)

    ao(:) = a(:) - origin(:)
    bo(:) = b(:) - origin(:)
    theta = DotProduct(ao, bo, 3) / (EuclideanNorm(ao,3) * EuclideanNorm(bo,3))

  END FUNCTION Angle

!*

  PURE REAL(8) FUNCTION EuclideanNorm(vector,n) result(r)

  real(8), intent(in) :: vector(n)
  integer, intent(in) :: n
  integer :: i

    r = dsqrt(DotProduct(vector,vector,n))

  END FUNCTION EuclideanNorm

!*

  PURE REAL(8) FUNCTION DotProduct(p,q,n) result(pdotq)

  real(8), intent(in) :: p(n), q(n)
  integer, intent(in) :: n
  integer :: i

    do i = 1, n
      pdotq = pdotq + ( p(i) * q(i) )
    enddo

  END FUNCTION DotProduct

END MODULE CoordinateFunctions

!*

!  PURE REAL(8) FUNCTION EuclideanDistanceDerivative(origin,coords,n,i) result(der)
!  
!  real(8), intent(in) :: origin(3), coords(3)
!  integer :: n, i
!  
!    der = -1.0d0 / (2.0d0 * EuclideanDistance(coord(:)-origin(:))) * 2.0d0 * coord(i)-origin(i)
!  
!  END FUNCTION EuclideanDistanceDerivative
  
!*
!  PURE REAL(8) FUNCTION CrossProduct(p,q), result(pcrossq)
!
!  real(8), intent(in) :: p(3), q(3)
!  real(8), intent(out) :: pcrossq(3)
!
!    pcrossq(1) = p(2)*q(3) - p(3)*q(2)
!    pcrossq(2) = p(3)*q(1) - p(1)*q(3)
!    pcrossq(3) = p(1)*q(2) - p(2)*q(1)
!
!  END FUNCTION CrossProduct
  
!END MODULE CoordinateFunctions
