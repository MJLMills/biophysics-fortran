MODULE ForceFieldFunctions

  IMPLICIT NONE

  REAL(8), PARAMETER, PRIVATE :: h = 1.0d-5, twoh = 2.0d-5, hsq = 1.0d-10
  LOGICAL, PARAMETER, PRIVATE :: check = .true.

CONTAINS

  PURE REAL(8) FUNCTION MorseEnergy(D_e, r_e, a, r) result(E); IMPLICIT NONE

    real(8), intent(in)  :: D_e, r_e, a, r
    real(8)              :: exponential, delta

    exponential = dexp( (-1.0d0) * a * (r - r_e) )
    delta = 1.0D0 - exponential
    E = D_e * delta * delta

  END FUNCTION MorseEnergy

!*

  PURE REAL(8) FUNCTION HarmonicEnergy(K, r_0, r) result(E); IMPLICIT NONE

    real(8), intent(in)  :: K, r_0, r

    E = K * (r - r_0) * (r - r_0)

  END FUNCTION HarmonicEnergy

!*

  PURE REAL(8) FUNCTION HarmonicFirstDerivative_dr(K, r_0, r) result(d); IMPLICIT NONE

    real(8), intent(in)  :: K, r_0, r
    real(8) :: fd

    d = 2.0d0 * K * (r - r_0)

    if (check .eqv. .true.) then
      fd = (HarmonicEnergy(K, r_0, r+h) - HarmonicEnergy(K, r_0, r-h)) / twoh
      if (abs(d - fd) > hsq) then
!        write(1,*) "Error: Innacurate Derivative"
      endif
    endif

  END FUNCTION HarmonicFirstDerivative_dr

!*

  PURE REAL(8) FUNCTION HarmonicSecondDerivative_dr(K, r_0, r) result(d); IMPLICIT NONE

    real(8), intent(in)  :: K, r_0, r
    real(8) :: fd

    d = 2.0d0 * K
    
    if (check .eqv. .true.) then
      fd = (HarmonicEnergy(K, r_0, r+h) - (2.0d0 * HarmonicEnergy(K, r_0, r)) + HarmonicEnergy(K, r_0, r-h)) / hsq
      if (abs(d - fd) > hsq) then
!        write(1,*) "Error: Innacurate Derivative"
      endif
    endif

  END FUNCTION HarmonicSecondDerivative_dr
!*  
  PURE REAL(8) FUNCTION HarmonicFirstDerivative_dr0(K, r_0, r) result(d); IMPLICIT NONE

    real(8), intent(in) :: K, r_0, r
    real(8) :: fd
  
    d = (-2.0d0) * K * (r - r_0)

    if (check .eqv. .true.) then
      fd = (HarmonicEnergy(K, r_0+h, r) - HarmonicEnergy(K, r_0-h, r)) / twoh
      if (abs(d - fd) > hsq) then
!        write(1,*) "Error: Innacurate Derivative"
      endif
    endif

  END FUNCTION HarmonicFirstDerivative_dr0

!*  

  PURE REAL(8) FUNCTION HarmonicSecondDerivative_dr0(K,r_0,r) result(d); IMPLICIT NONE

    real(8), intent(in)  :: K, r_0, r
    real(8) :: fd

    d = 2.0d0 * K

    if (check .eqv. .true.) then
      fd = (HarmonicEnergy(K, r_0+h, r) - (2.0d0 * HarmonicEnergy(K, r_0, r)) + HarmonicEnergy(K, r_0-h, r)) / hsq
      if (abs(d - fd) > hsq) then
!        write(1,*) "Error: Innacurate Derivative"
      endif
    endif
    
  END FUNCTION HarmonicSecondDerivative_dr0

!*  

  PURE REAL(8) FUNCTION HarmonicFirstDerivative_dK(K, r_0, r) result(d); IMPLICIT NONE
  
    real(8), intent(in)  :: K, r_0, r
    real(8) :: fd, delta
  
    delta = (r - r_0)
    d = delta * delta
  
    if (check .eqv. .true.) then
      fd = (HarmonicEnergy(K+h, r_0, r) - HarmonicEnergy(K-h, r_0, r)) / twoh
      if (abs(d - fd) > hsq) then
!        write(1,*) "Error: Innacurate Derivative"
      endif
    endif
  
  END FUNCTION HarmonicFirstDerivative_dK

!*

  PURE REAL(8) FUNCTION HarmonicSecondDerivative_dK(K, r_0, r) result(d); IMPLICIT NONE
  
    real(8), intent(in) :: K, r_0, r
    real(8) :: fd

    d = 0.0d0
    
    if (check .eqv. .true.) then
      fd = (HarmonicEnergy(K+h, r_0, r) - (2.0d0 * HarmonicEnergy(K, r_0, r)) + HarmonicEnergy(K-h, r_0, r)) / hsq
      if (abs(d - fd) > hsq) then
!        write(1,*) "Error: Innacurate Derivative"
      endif
    endif
  
  END FUNCTION HarmonicSecondDerivative_dK

!*  

  PURE REAL(8) FUNCTION HarmonicSecondDerivative_dr0dK(r_0, r)
  
    real(8), intent(in) :: r_0, r
  
    HarmonicSecondDerivative_dr0dK = (-2.0d0) * (r - r_0)
    
  END FUNCTION HarmonicSecondDerivative_dr0dK
  
!*

  PURE REAL(8) FUNCTION TorsionEnergy(V_n, n, omega, gamma) result(E); IMPLICIT NONE
  
    real(8), intent(in) :: V_n, omega, gamma
    integer, intent(in) :: n

    E = 0.5d0 * V_n * (1.0d0 + cos(n * omega - gamma))

  END FUNCTION TorsionEnergy
  
!*

  PURE REAL(8) FUNCTION ElectrostaticEnergy(q_A,q_B,r) result(E); IMPLICIT NONE

    real(8), intent(in) :: q_A, q_B, r
  
    E = (q_A * q_B) / r;
  
  END FUNCTION ElectrostaticEnergy

!  PURE REAL(8) FUNCTION MorseDerivative() result(d); IMPLICIT NONE

!  END FUNCTION MorseDerivative

END MODULE ForceFieldFunctions
