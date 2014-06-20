MODULE ChemicalSystem
IMPLICIT NONE

Character(LEN=4), allocatable :: BondTypes(:), AngleTypes(:)
integer :: nAtoms, nBonds, nAngles, nTorsions
real(8), allocatable :: BondIDs(:,:), BondForceConstants(:), BondReferences(:)
real(8), allocatable :: AngleIDs(:,:), AngleForceConstants(:,:), AngleReferences(:)

END MODULE ChemicalSystem
