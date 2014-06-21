PROGRAM TestBond

  use ChemicalSystem
  use Conformation

IMPLICIT NONE

call CreateChemicalSystem(3,2,1,0)

!Fill the created arrays of ChemicalSystem with their values - replace with read
BondIDS(1,1) = 1
BondIDS(1,2) = 2
BondIDs(2,1) = 2
BondIDs(2,2) = 3
AngleIDs(1,1) = 1
AngleIDs(1,2) = 2
AngleIDs(1,3) = 3

BondTypes(:) = "HARM"
BondForceConstants(:)  = 50.00d0
BondReferences(:)      =  0.92d0
AngleTypes(:) = "HARM"
AngleForceConstants(:) = 75.00d0
AngleReferences(:)     =  1.82d0

call CreateConformation

CartCoords(1,1) = 0.00d0; CartCoords(1,2) = 0.00d0; CartCoords(1,3) = 0.0d0;
CartCoords(2,1) = 0.95d0; CartCoords(2,2) = 0.00d0; CartCoords(2,3) = 0.0d0;
CartCoords(3,1) = 1.18786100427d0; CartCoords(3,2) = 0.91974025825d0; CartCoords(3,3) = 0.0d0;

call CartesianToRedundantInternal
call PrintRedundantCoordinates
call CalculateBondEnergy
call CalculateAngleEnergy
call PrintEnergyAndForces

call DestroyConformation
call DestroyChemicalSystem

END PROGRAM TestBond
