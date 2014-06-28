MODULE CoordinateFunctions

  use VectorMath

IMPLICIT NONE

  CONTAINS

!*
  
  PURE REAL(8) FUNCTION EuclideanDistance(origin,coords,n) result(r); IMPLICIT NONE

  integer, intent(in) :: n
  real(8), intent(in) :: origin(n), coords(n)

      r = dsqrt(DotProduct(coords(:) - origin(:),coords(:) - origin(:),n))
  
  END FUNCTION EuclideanDistance

!*

  PURE REAL(8) FUNCTION Angle(origin,a,b) result(theta); IMPLICIT NONE

  real(8), intent(in) :: a(3), b(3), origin(3)
  real(8) :: ao(3), bo(3)

    ao(:) = a(:) - origin(:)
    bo(:) = b(:) - origin(:)

    theta = dacos(DotProduct(ao, bo, 3) / (EuclideanNorm(ao,3) * EuclideanNorm(bo,3)))

  END FUNCTION Angle

!*

  PURE REAL(8) FUNCTION TorsionAngle(vec_i,vec_j,vec_k,vec_l) result(phi); IMPLICIT NONE

  real(8), intent(in) :: vec_i(3), vec_j(3), vec_k(3), vec_l(3)
  double precision norm_vec_ij(3), norm_vec_jk(3), norm_vec_kl(3), Cross_onetwo(3), Cross_twothree(3)
  double precision ij_dot_jk, jk_dot_kl,Sin_ij_dot_jk, Sin_jk_dot_kl, numerator, CosW

    norm_vec_ij(:) = NormaliseVector(vec_j(:)-vec_i(:),3)
    norm_vec_jk(:) = NormaliseVector(vec_k(:)-vec_j(:),3)
    norm_vec_kl(:) = NormaliseVector(vec_l(:)-vec_k(:),3)

    !Get the two cross products of the planes
    Cross_onetwo   = CrossProduct(norm_vec_ij, norm_vec_jk)
    Cross_twothree = CrossProduct(norm_vec_jk, norm_vec_kl)

    ij_dot_jk = DotProduct(norm_vec_ij(:),norm_vec_jk(:),3) 
    jk_dot_kl = DotProduct(norm_vec_jk(:),norm_vec_kl(:),3)

    Sin_ij_dot_jk = dsqrt(1.0D0 - ij_dot_jk*ij_dot_jk)
    Sin_jk_dot_kl = dsqrt(1.0D0 - jk_dot_kl*jk_dot_kl)
    numerator = DotProduct(Cross_onetwo(:),Cross_twothree(:),3)

    CosW = numerator / (Sin_ij_dot_jk*Sin_jk_dot_kl)

    phi = dacos(CleanTrigArgument(CosW))

    if (DotProduct(Cross_twothree(:), vec_j(:)-vec_i(:), 3) < 0.0D0) then; phi = -1.0D0 * phi; endif
        
  END FUNCTION TorsionAngle

!*

  PURE REAL(8) FUNCTION EuclideanDistanceDerivative(origin,coords,n,i) result(der); IMPLICIT NONE
  
  integer, intent(in) :: n, i
  real(8), intent(in) :: origin(n), coords(n)
  
    der = (-1.0d0 / (2.0d0 * EuclideanDistance(origin(:),coords(:),3))) * 2.0d0 * (coords(i)-origin(i))
  
  END FUNCTION EuclideanDistanceDerivative

!*

  PURE REAL(8) FUNCTION AngleDerivative result(der); IMPLICIT NONE

  END FUNCTION AngleDerivative
  
END MODULE CoordinateFunctions

