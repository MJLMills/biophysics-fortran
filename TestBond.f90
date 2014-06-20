PROGRAM TestBond

  use ForceFieldFunctions
  use CoordinateFunctions
  use ChemicalSystem
  use Conformation

IMPLICIT NONE

call CreateChemicalSystem(2,1,0,0)

!Fill the created arrays of ChemicalSystem with their values
BondIDS(1,1) = 1
BondIDS(1,2) = 2
bondTypes(1) = "HARM"
bondForceConstants(1) = 50.0d0
bondReferences(1)     =  1.2d0

END PROGRAM TestBond
