MODULE CoordinateFunctions
IMPLICIT NONE

  CONTAINS

!*
  
  PURE REAL(8) FUNCTION EuclideanDistance(origin,coords,n) result(r)

  integer, intent(in) :: n
  real(8), intent(in) :: origin(n), coords(n)

    r = 0.0d0; r = dsqrt(DotProduct(coords(:) - origin(:),coords(:) - origin(:),n))
  
  END FUNCTION EuclideanDistance

!*

  PURE REAL(8) FUNCTION Angle(origin,a,b) result(theta)

  real(8), intent(in) :: a(3), b(3), origin(3)
  real(8) :: ao(3), bo(3)

    ao(:) = a(:) - origin(:)
    bo(:) = b(:) - origin(:)

    theta = 0.0d0;
    theta = dacos(DotProduct(ao, bo, 3) / (EuclideanNorm(ao,3) * EuclideanNorm(bo,3)))

  END FUNCTION Angle

!*

  PURE REAL(8) FUNCTION EuclideanNorm(vector,n) result(r)

  integer, intent(in) :: n
  real(8), intent(in) :: vector(n)

    r = 0.0d0; r = dsqrt(DotProduct(vector,vector,n))

  END FUNCTION EuclideanNorm

!*

  PURE REAL(8) FUNCTION DotProduct(p,q,n) result(pdotq)

  integer, intent(in) :: n
  real(8), intent(in) :: p(n), q(n)
  integer :: i

  pdotq = 0.0d0

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
