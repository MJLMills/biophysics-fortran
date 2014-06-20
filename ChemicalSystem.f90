MODULE ChemicalSystem
IMPLICIT NONE

character(LEN=4), allocatable :: BondTypes(:), AngleTypes(:)
integer                       :: nAtoms, nBonds, nAngles, nTorsions
integer, allocatable          :: BondIDs(:,:), AngleIDs(:,:)
real(8), allocatable          :: BondForceConstants(:), BondReferences(:)
real(8), allocatable          :: AngleForceConstants(:), AngleReferences(:)

  CONTAINS

SUBROUTINE CreateChemicalSystem(setAtoms,setBonds,setAngles,setTorsions)

    integer :: setAtoms, setBonds, setAngles, setTorsions

    nAtoms = setAtoms; nBonds = setBonds; nAngles = setAngles; nTorsions = setTorsions

    if (.NOT. allocated(BondTypes))  then; allocate(BondTypes(nBonds));   BondTypes(:)  = ""; endif
    if (.NOT. allocated(BondIDs))    then; allocate(BondIDs(nBonds,2));   BondIDs(:,:)  = 0 ; endif
    if (.NOT. allocated(AngleTypes)) then; allocate(AngleTypes(nAngles)); AngleTypes(:) = ""; endif
    if (.NOT. allocated(AngleIDs))   then; allocate(AngleIDs(nAngles,3)); AngleIDs(:,:) = 0 ; endif

    if (.NOT. allocated(BondForceConstants))  then; allocate(BondForceConstants(nBonds));  BondForceConstants(:)  = 0 ; endif
    if (.NOT. allocated(BondReferences))      then; allocate(BondReferences(nBonds));      BondReferences(:)      = 0 ; endif
    if (.NOT. allocated(AngleForceConstants)) then; allocate(AngleForceConstants(nBonds)); AngleForceConstants(:) = 0 ; endif
    if (.NOT. allocated(AngleReferences))     then; allocate(AngleReferences(nBonds));     AngleReferences(:)     = 0 ; endif

END SUBROUTINE CreateChemicalSystem

SUBROUTINE DestroyChemicalSystem

    if (allocated(BondTypes))  then; deallocate(BondTypes);  endif
    if (allocated(BondIDs))    then; deallocate(BondIDs);    endif
    if (allocated(AngleTypes)) then; deallocate(AngleTypes); endif
    if (allocated(AngleIDs))   then; deallocate(AngleIDs);   endif

    if (allocated(BondForceConstants)) then; deallocate(BondForceConstants); endif
    if (allocated(BondReferences))     then; deallocate(BondReferences)    ; endif
    if (allocated(AngleForceConstants)) then; deallocate(AngleForceConstants); endif
    if (allocated(AngleReferences))     then; deallocate(AngleReferences)    ; endif

END SUBROUTINE DestroyChemicalSystem

END MODULE ChemicalSystem
