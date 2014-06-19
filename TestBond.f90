PROGRAM TestBond
use ForceFieldFunctions
use CoordinateFunctions
IMPLICIT NONE

Character(LEN=4), allocatable :: BondTypes(:)
Character(LEN=4) :: type_i
integer :: nAtoms, nBonds
integer, allocatable :: bondMatrix(:,:), BondIDs(:,:)
integer :: i, j
real(8), allocatable :: bondEnergies(:), bondForceConstants(:), bondReferences(:)
real(8), allocatable :: cartesianCoordinates(:,:), atomicForces(:,:)
real(8) :: r

nAtoms = 2

allocate(cartesianCoordinates(nAtoms,3))
allocate(bondMatrix(nAtoms,nAtoms))
bondMatrix(1,2) = 1

!Count the Bonds
do i = 1, nAtoms
  do j = i+1, nAtoms
    if (bondMatrix(i,j) == 1) then
      nBonds = nBonds + 1
    endif
  enddo
enddo
!

allocate(BondTypes(nBonds))
allocate(BondIDs(nBonds,2))
BondIDS(1,1) = 1
BondIDS(1,2) = 2

allocate(BondEnergies(nBonds))
BondEnergies(:) = 0.0d0

bondTypes(1) = "HARM"
bondEnergies(1) = 0.0d0

do i = 1, nBonds
    write(*,*) "BOND ", i, " between ", bondIDs(i,1), " and ",&
&               bondIDs(i,2), " IS TYPE ", bondTypes(i)
enddo

do i = 1, nBonds
  r = EuclideanDistance(cartesianCoordinates(BondIDs(i,1),:),&
&                       cartesianCoordinates(BondIDs(i,2),:),3)

  type_i = trim(adjustl(BondTypes(i)))  
  select case (type_i)

  case ("HARM")
    bondEnergies(i) = HarmonicEnergy(bondForceConstants(i),)
    do j = 1, 3
      AtomicForces(BondIDs(i,1),j) = AtomicForces(BondIDs(i,1),j) + &
&                                    HarmonicEnergyDerivative(j)
      AtomicForces(BondIDs(i,2),j) = AtomicForces(BondIDs(i,2),j) - &
&                                    HarmonicEnergyDerivative(j)
    enddo
  !case ("MORSE")
    !bondEnergies(i) = MorseEnergy()
  case default
    write(*,*) "Bond Type is unknown"
  end select
enddo

END PROGRAM TestBond
