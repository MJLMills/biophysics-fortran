PROGRAM TestBond
use ForceFieldFunctions
use CoordinateFunctions
use ChemicalSystem
IMPLICIT NONE

Character(LEN=4) :: type_i
integer :: i, j
real(8), allocatable :: bondEnergies(:), cartesianCoordinates(:,:), bondForces(:), atomicForces(:,:)
real(8) :: r

nAtoms = 2

allocate(cartesianCoordinates(nAtoms,3))
allocate(AtomicForces(nAtoms,3))

cartesianCoordinates(1,1) = 0.0d0
cartesianCoordinates(1,2) = 0.0d0
cartesianCoordinates(1,3) = 0.0d0
cartesianCoordinates(2,1) = 1.0d0
cartesianCoordinates(2,2) = 0.0d0
cartesianCoordinates(2,3) = 0.0d0

nBonds = 1
call AllocateArrays

allocate(BondEnergies(nBonds)); bondEnergies(:) = 0.0d0

BondIDS(1,1) = 1
BondIDS(1,2) = 2
bondTypes(1) = "HARM"
bondForceConstants(1) = 50.0d0
bondReferences(1)     =  1.2d0

do i = 1, nBonds
    write(*,*) "BOND ", i, " is between ", bondIDs(i,1), " and ",&
&               bondIDs(i,2), " IS TYPE ", bondTypes(i)
enddo

do i = 1, nBonds
  r = EuclideanDistance(cartesianCoordinates(BondIDs(i,1),:),&
&                       cartesianCoordinates(BondIDs(i,2),:),3)
write(*,*) r

  type_i = trim(adjustl(BondTypes(i)))
  select case (type_i)

  case ("HARM")
    bondEnergies(i) = HarmonicEnergy(bondForceConstants(i),bondReferences(i),r)
    write(*,*) bondEnergies(i)
    do j = 1, 3
      BondForces(BondIDs(i,1)) = AtomicForces(BondIDs(i,1),j) + &
&                 HarmonicFirstDerivative_dr(bondForceConstants(i),&
&                                             bondReferences(i),r)
      BondForces(BondIDs(i,2)) = -1.0d0 * AtomicForces(BondIDs(i,1),j)

    enddo

  case default
    write(*,*) "Bond Type is unknown"
  end select
enddo

END PROGRAM TestBond
