MODULE ChemicalSystem
IMPLICIT NONE

character(LEN=4), allocatable :: BondTypes(:), AngleTypes(:)
integer :: nAtoms, nBonds, nAngles, nTorsions
integer, allocatable :: BondIDs(:,:), AngleIDs(:,:)
real(8), allocatable :: BondForceConstants(:), BondReferences(:)
real(8), allocatable :: AngleForceConstants(:,:), AngleReferences(:)

  CONTAINS

SUBROUTINE AllocateArrays

    allocate(BondTypes(nBonds));   BondTypes(:)  = ""
    allocate(BondIDs(nBonds,2));   BondIDs(:,:)  = 0
    allocate(AngleTypes(nAngles)); AngleTypes(:) = ""
    allocate(AngleIDs(nAngles,3)); AngleIDs(:,:) = 0

END SUBROUTINE AllocateArrays

END MODULE ChemicalSystem
