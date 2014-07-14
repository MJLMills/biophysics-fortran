MODULE ChemicalSystem
IMPLICIT NONE

character(LEN=4), allocatable :: BondTypes(:), AngleTypes(:), TorsionTypes(:)
character(LEN=2), allocatable :: Elements(:)
integer                       :: nAtoms, nBonds, nAngles, nTorsions
integer, allocatable          :: BondIDs(:,:), AngleIDs(:,:), TorsionIDs(:,:)
real(8), allocatable          :: AtomicMasses(:)
real(8), allocatable          :: BondForceConstants(:), BondReferences(:)
real(8), allocatable          :: AngleForceConstants(:), AngleReferences(:)

  CONTAINS

SUBROUTINE CreateChemicalSystem(setAtoms,setBonds,setAngles,setTorsions)

    integer :: setAtoms, setBonds, setAngles, setTorsions

    nAtoms = setAtoms; nBonds = setBonds; nAngles = setAngles; nTorsions = setTorsions

    !Allocate arrays with variable dimension nAtoms
    if (.NOT. allocated(AtomicMasses)) then; allocate(AtomicMasses(nAtoms)); AtomicMasses(:) = 0.0d0; endif
    if (.NOT. allocated(Elements))     then; allocate(Elements(nAtoms))    ; Elements(:)     = ""   ; endif

    !Allocate arrays with variable dimension nBonds
    if (.NOT. allocated(BondTypes))    then; allocate(BondTypes(nBonds));       BondTypes(:)    = ""; endif
    if (.NOT. allocated(BondIDs))      then; allocate(BondIDs(nBonds,2));       BondIDs(:,:)    = 0 ; endif
    if (.NOT. allocated(BondForceConstants))  then; allocate(BondForceConstants(nBonds));  BondForceConstants(:)  = 0; endif
    if (.NOT. allocated(BondReferences))      then; allocate(BondReferences(nBonds));      BondReferences(:)      = 0; endif

    !Allocate arrays with variable dimension nAngles
    if (.NOT. allocated(AngleTypes))   then; allocate(AngleTypes(nAngles));     AngleTypes(:)   = ""; endif
    if (.NOT. allocated(AngleIDs))     then; allocate(AngleIDs(nAngles,3));     AngleIDs(:,:)   = 0 ; endif
    if (.NOT. allocated(AngleForceConstants)) then; allocate(AngleForceConstants(nBonds)); AngleForceConstants(:) = 0; endif
    if (.NOT. allocated(AngleReferences))     then; allocate(AngleReferences(nBonds));     AngleReferences(:)     = 0; endif

    !Allocate arrays with variable dimension nTorsions
    if (.NOT. allocated(TorsionTypes)) then; allocate(TorsionTypes(nTorsions)); TorsionTypes(:) = ""; endif
    if (.NOT. allocated(TorsionIDs))   then; allocate(TorsionIDs(nTorsions,4)); TorsionIDs(:,:) = 0 ; endif

END SUBROUTINE CreateChemicalSystem

SUBROUTINE DestroyChemicalSystem

    if (allocated(AtomicMasses)) deallocate(AtomicMasses)
    if (allocated(Elements))     deallocate(Elements)

    if (allocated(BondTypes))    deallocate(BondTypes)
    if (allocated(BondIDs))      deallocate(BondIDs)
    if (allocated(BondForceConstants))  deallocate(BondForceConstants) 
    if (allocated(BondReferences))      deallocate(BondReferences) 

    if (allocated(AngleTypes))   deallocate(AngleTypes)
    if (allocated(AngleIDs))     deallocate(AngleIDs)
    if (allocated(AngleForceConstants)) deallocate(AngleForceConstants) 
    if (allocated(AngleReferences))     deallocate(AngleReferences)   

    if (allocated(TorsionTypes)) deallocate(TorsionTypes)
    if (allocated(TorsionIDs))   deallocate(TorsionIDs)

END SUBROUTINE DestroyChemicalSystem

END MODULE ChemicalSystem
