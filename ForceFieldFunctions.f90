MODULE ForceFieldFunctions

IMPLICIT NONE

CONTAINS

  PURE REAL(8) FUNCTION MorseEnergy(D_e, r_e, a, r)

  real(8), intent(in)  :: D_e, r_e, a, r
  real(8)              :: exponential

    exponential = dexp( (-1.0d0) * a * (r - r_e) )
    MorseEnergy = D_e * (1 - exponential) * (1 - exponential)

  END FUNCTION MorseEnergy

!*

  PURE REAL(8) FUNCTION HarmonicEnergy(K, r_0, r)

  real(8), intent(in)  :: K, r_0, r

    HarmonicEnergy = K * (r - r_0) * (r - r_0)

  END FUNCTION HarmonicEnergy

  PURE REAL(8) FUNCTION HarmonicFirstDerivative_dr(K, r_0, r)

  real(8), intent(in)  :: K, r_0, r

    HarmonicFirstDerivative_dr = 2.0d0 * K * (r - r_0)

  END FUNCTION HarmonicFirstDerivative_dr

  PURE REAL(8) FUNCTION HarmonicSecondDerivative_dr(K)

  real(8), intent(in)  :: K

    HarmonicSecondDerivative_dr = 2.0d0 * K

  END FUNCTION HarmonicSecondDerivative_dr
  
  PURE REAL(8) FUNCTION HarmonicFirstDerivative_dr0(K, r0, r)

  real(8), intent(in) :: K, r0, r
  
    HarmonicFirstDerivative_dr0 = (-2.0d0) * K * (r - r0)

  END FUNCTION HarmonicFirstDerivative_dr0
  
  PURE REAL(8) FUNCTION HarmonicSecondDerivative_dr0(K)

  real(8), intent(in)  :: K

    HarmonicSecondDerivative_dr0 = 2.0d0 * K

  END FUNCTION HarmonicSecondDerivative_dr0
  
  PURE REAL(8) FUNCTION HarmonicFirstDerivative_dK(r0, r)
  
  real(8), intent(in)  :: r0, r
  
    HarmonicFirstDerivative_dK = (r - r0) * (r - r0)
  
  END FUNCTION HarmonicFirstDerivative_dK
  
!*

  PURE REAL(8) FUNCTION TorsionEnergy(V_n, n, omega, gamma)
  
  real(8), intent(in) :: V_n, omega, gamma
  integer, intent(in) :: n

    TorsionEnergy = 0.5d0 * V_n * (1.0d0 + cos(n * omega - gamma))

  END FUNCTION TorsionEnergy
  
!*
  PURE REAL(8) FUNCTION ElectrostaticEnergy(q_A,q_B,r)

  real(8), intent(in) :: q_A, q_B, r
  
    ElectrostaticEnergy = (q_A * q_B) / r;
  
  END FUNCTION ElectrostaticEnergy

END MODULE ForceFieldFunctions
