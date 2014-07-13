PROGRAM TestBond

  use ChemicalSystem
  use Conformation
  use Dynamics
  use OMPTools

IMPLICIT NONE

  NAMELIST /OMP/ numThreads, dynamicAllocation, nestedParallelism
  INTEGER :: numThreads
  LOGICAL :: dynamicAllocation, nestedParallelism

OPEN(20,FILE='OMP.nml',IOSTAT=ios)
if (ios /= 0) CALL CheckFileOpen(ios,fileName,20)

READ(20,nml=OMP)

CLOSE(20,IOSTAT=ios)
if (ios /= 0) Call CheckFileClose(ios,fileName,out)

PRINT*, "N THREAD = ", numThreads
PRINT*, "DYNAMIC  = ", dynamicAllocation
PRINT*, "NESTED   = ", nestedParallelism

call OMP_setup
call OMP_get_general_info("INITIAL SETUP")

call CreateChemicalSystem(3,2,1,0)

!Fill the created arrays of ChemicalSystem with their values - replace with read
Elements(1) = "H "
Elements(2) = "O "
Elements(3) = "H "

AtomicMasses(1) = 1.00794d0
AtomicMasses(2) = 15.9994d0
AtomicMasses(3) = 1.00794d0

BondIDS(1,1) = 1
BondIDS(1,2) = 2
BondIDs(2,1) = 2
BondIDs(2,2) = 3
AngleIDs(1,1) = 1
AngleIDs(1,2) = 2
AngleIDs(1,3) = 3

BondTypes(:) = "HARM"
BondForceConstants(:)  = 50.00d0
BondReferences(:)      =  1.00d0
AngleTypes(:) = "HARM"
AngleForceConstants(:) = 100.00d0
AngleReferences(:)     =  1.21d0

call CreateConformation

CartCoords(1,1) = 0.00d0; CartCoords(1,2) = 0.00d0; CartCoords(1,3) = 0.0d0;
CartCoords(2,1) = 0.95d0; CartCoords(2,2) = 0.00d0; CartCoords(2,3) = 0.0d0;
CartCoords(3,1) = 1.18786100427d0; CartCoords(3,2) = 0.91974025825d0; CartCoords(3,3) = 0.0d0;

!call CartesianToRedundantInternal
!call PrintRedundantCoordinates

call VelocityVerlet(0.001d-1,2)

call DestroyConformation
call DestroyChemicalSystem
call OMP_teardown

END PROGRAM TestBond
