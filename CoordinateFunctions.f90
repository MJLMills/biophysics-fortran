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

double precision function TorsionAngle(vecA,vecB,vecC,vecD)
implicit none
double precision vecA(3), vecB(3), vecC(3), vecD(3)
double precision vec_ij(3), vec_jk(3), vec_kl(3)
double precision Dijsq, Djksq, Dklsq, Dij, Djk, Dkl, torsionMag, SENSE
double precision norm_vec_ij(3), norm_vec_jk(3), norm_vec_kl(3), Cross_onetwo(3), Cross_twothree(3)
double precision ij_dot_jk, jk_dot_kl,Sin_ij_dot_jk, Sin_jk_dot_kl, numerator, CosW
integer cart
TorsionAngle = 0.0D0

	!Get the dot products of the bondvectors with themselves
	vec_ij = 0.0D0; vec_jk = 0.0D0; vec_kl = 0.0D0
	Dijsq = 0.0D0; Djksq = 0.0D0; Dklsq = 0.0D0
	do cart = 1,3
		!Get the bond vectors
		vec_ij(cart) = vecB(cart) - vecA(cart)
		vec_jk(cart) = vecC(cart) - vecB(cart)
		vec_kl(cart) = vecD(cart) - VecC(cart)

		Dijsq = Dijsq + vec_ij(cart)*vec_ij(cart)
		Djksq = Djksq + vec_jk(cart)*vec_jk(cart)
		Dklsq = Dklsq + vec_kl(cart)*vec_kl(cart)
	enddo

	!And then their magnitudes

	Dij = dsqrt(Dijsq); Djk = dsqrt(Djksq); Dkl = dsqrt(Dklsq);

	!Normalise the bond vectors (Eij)
	do cart = 1,3
		norm_vec_ij(cart) = vec_ij(cart) / Dij
		norm_vec_jk(cart) = vec_jk(cart) / Djk
		norm_vec_kl(cart) = vec_kl(cart) / Dkl
	enddo
	
	!Get the two cross products of the planes
	call CrossProduct(norm_vec_ij, norm_vec_jk, Cross_onetwo)
	call CrossProduct(norm_vec_jk, norm_vec_kl, Cross_twothree)

	ij_dot_jk = 0.0D0; jk_dot_kl = 0.0D0
	do cart=1,3
		ij_dot_jk = ij_dot_jk + norm_vec_ij(cart) * norm_vec_jk(cart)
		jk_dot_kl = jk_dot_kl + norm_vec_jk(cart) * norm_vec_kl(cart)
	enddo

	Sin_ij_dot_jk = dsqrt(1.0D0 - ij_dot_jk*ij_dot_jk)
	Sin_jk_dot_kl = dsqrt(1.0D0 - jk_dot_kl*jk_dot_kl)

	numerator = 0.0D0
	do cart = 1, 3	
		numerator = numerator + Cross_onetwo(cart) * Cross_twothree(cart) 
	enddo

	CosW = numerator / (Sin_ij_dot_jk*Sin_jk_dot_kl)

	!Fix edging values
	if (abs(CosW - 1.0D0) < 1.0D-7) then
	CosW = 1.0D0
	else if (abs(CosW+1) < 1.0D-7) then
	CosW = -1.0D0
	endif
	
		if (CosW < -1.0D0) then
			write(*,'(a26,4i4)'), "ERROR IN COMPUTING TORSION"
			print*, "Cosine of angle is outside range -1 < W < +1"
			print*, CosW
			stop
		else if (CosW > 1.0D0) then
			write(*,'(a26,4i4)'), "ERROR IN COMPUTING TORSION"
			print*, "Cosine of angle is outside range -1 < W < +1"
			print*, CosW
			stop
		else

		torsionMag = dacos(CosW) !EQVAL

		!Determine the sign
		
		SENSE = 0.0D0
		do cart = 1,3
			SENSE = SENSE + Cross_twothree(cart) * vec_ij(cart)
		enddo
		
		if (SENSE < 0.0D0) then 
		TorsionAngle = -1.0D0 * torsionMag
        else
        TorsionAngle = torsionMag
        endif
        
		endif

end function TorsionAngle

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
