PROGRAM TestBond
use ForceFieldFunctions
IMPLICIT NONE

Character(LENGTH=5), allocatable :: BondTypes(:)
integer :: nAtoms
integer, allocatable :: bondMatrix(:,:), BondIDs(:,:)
integer :: i, j
real(8), allocatable :: bondEnergies(:), bondForceConstants(:), bondReferences(:)
real(8), allocatable :: cartesianCoordinates(:,:), atomicForces(:,:)

nAtoms = 2

allocate(cartesianCoordinates(nAtoms,3))
allocate(bondMatrix(nAtoms,nAtoms))
bondMatrix(1,2) = 1

do i = 1, nAtoms
  do j = i+1, nAtoms
    if (bondMatrix(i,j) == 1) then
      nBonds = nBonds + 1
    endif
  enddo
enddo

allocate(BondTypes(nBonds))
allocate(BondIDs(nBonds,2))
BondIDS(1,1) = 1
BondIDS(1,2) = 2

allocate(BondEnergies(nBonds))
BondEnergies(:) = 0.0d0

bondTypes(1) = "HARM"
bondEnergies(1) = 0.0d0

do i = 1, nBonds
  r = EuclideanDistance(cartesianCoordinates(BondIDs(i,1),:),cartesianCoordinates(BondIDs(i,2)),3)
  select case (trim(ladjust(BondTypes(i))))
  case ("HARM")
    bondEnergies(i) = HarmonicEnergy(bondForceConstants(i),)
    do j = 1, 3
      AtomicForces(BondIDs(i,1),j) += HarmonicEnergyDerivative(j)
      AtomicForces(BondIDs(i,2),j) -= HarmonicEnergyDerivative(j)
    enddo
  case ("MORSE")
    bondEnergies(i) = MorseEnergy()
  case default
    !Bond Type is unknown
  end select
enddo

END PROGRAM TestBond
