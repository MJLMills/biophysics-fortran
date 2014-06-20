MODULE Conformation
use ChemicalSystem
IMPLICIT NONE

real(8), allocatable :: CartesianCoordinates(:,:)
real(8), allocatable :: BondEnergies(:), AngleEnergies(:)
real(8), allocatable :: BondValues(:), AngleValues(:)
real(8), allocatable :: BondForces(:), AngleForces(:), AtomicForces(:,:)

CONTAINS

SUBROUTINE AllocateConformationArrays

    allocate(CartesianCoordinates(nAtoms,3)); CartesianCoordinates(:,:) = 0.0d0
    allocate(BondValues(nBonds));             BondValues(:)             = 0.0d0
    allocate(AngleValues(nAngles));           AngleValues(:)            = 0.0d0

    allocate(BondEnergies(nBonds));           BondEnergies(:)           = 0.0d0
    allocate(AngleEnergies(nAngles));         AngleEnergies(:)          = 0.0d0

    allocate(AtomicForces(nAtoms,3));         AtomicForces(:,:)         = 0.0d0
    allocate(BondForces(nBonds));             BondForces(:)             = 0.0d0
    allocate(AngleForces(nAngles));           AngleForces(:)            = 0.0d0

END SUBROUTINE AllocateConformationArrays

END MODULE Conformation
