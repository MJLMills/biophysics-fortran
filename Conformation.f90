MODULE Conformation

  use ChemicalSystem
  use CoordinateFunctions
  use ForceFieldFunctions

IMPLICIT NONE

  real(8), allocatable :: CartCoords(:,:)
  real(8), allocatable :: BondEnergies(:), AngleEnergies(:)
  real(8), allocatable :: BondValues(:), AngleValues(:), TorsionValues(:)
  real(8), allocatable :: AtomicAccelerations(:)
  real(8), allocatable :: BondForces(:), AngleForces(:), AtomicForces(:,:)
  real(8)              :: TotalBondEnergy, TotalAngleEnergy, TotalTorsionEnergy
  INTEGER, PARAMETER, PRIVATE :: out = 15
  CHARACTER(14), PARAMETER, PRIVATE :: fileName = "Trajectory.xyz"

CONTAINS

  SUBROUTINE CreateConformation

    IMPLICIT NONE
    INTEGER :: ios

    if (.NOT. allocated(CartCoords))          allocate(CartCoords(nAtoms,3));          CartCoords(:,:)        = 0.0d0
    if (.NOT. allocated(AtomicForces))        allocate(AtomicForces(nAtoms,3));        AtomicForces(:,:)      = 0.0d0
    if (.NOT. allocated(AtomicAccelerations)) allocate(AtomicAccelerations(3*nAtoms)); AtomicAccelerations(:) = 0.0d0
    if (.NOT. allocated(BondValues))          allocate(BondValues(nBonds));            BondValues(:)          = 0.0d0
    if (.NOT. allocated(BondEnergies))        allocate(BondEnergies(nBonds));          BondEnergies(:)        = 0.0d0
    if (.NOT. allocated(BondForces))          allocate(BondForces(nBonds));            BondForces(:)          = 0.0d0
    if (.NOT. allocated(AngleValues))         allocate(AngleValues(nAngles));          AngleValues(:)         = 0.0d0
    if (.NOT. allocated(AngleEnergies))       allocate(AngleEnergies(nAngles));        AngleEnergies(:)       = 0.0d0
    if (.NOT. allocated(AngleForces))         allocate(AngleForces(nAngles));          AngleForces(:)         = 0.0d0
    if (.NOT. allocated(TorsionValues))       allocate(TorsionValues(nTorsions));      TorsionValues(:)       = 0.0d0

    OPEN(UNIT=out,FILE=fileName,IOSTAT=ios,STATUS="REPLACE",ACCESS="SEQUENTIAL",FORM="FORMATTED",ACTION="WRITE")
    IF (ios /= 0) CALL CheckFileOpen(ios,fileName,out)

  END SUBROUTINE CreateConformation

!*

  SUBROUTINE DestroyConformation

    IMPLICIT NONE
    INTEGER :: ios

    if (allocated(CartCoords))          deallocate(CartCoords)
    if (allocated(BondValues))          deallocate(BondValues)
    if (allocated(AngleValues))         deallocate(AngleValues)
    if (allocated(TorsionValues))       deallocate(TorsionValues)
    if (allocated(BondEnergies))        deallocate(BondEnergies)
    if (allocated(AngleEnergies))       deallocate(AngleEnergies)
    if (allocated(AtomicForces))        deallocate(AtomicForces)
    if (allocated(BondForces))          deallocate(BondForces)
    if (allocated(AngleForces))         deallocate(AngleForces)
    if (allocated(AtomicAccelerations)) deallocate(AtomicAccelerations)
   
    CLOSE(out,IOSTAT=ios)
    CALL CheckFileClose(ios,fileName,out)

  END SUBROUTINE DestroyConformation

!*

  SUBROUTINE CartesianToRedundantInternal

    IMPLICIT NONE

    integer :: i

    !$OMP PARALLEL DO
    do i = 1, nBonds
      BondValues(i) = EuclideanDistance(CartCoords(BondIDs(i,1),:), &
      &                                 CartCoords(BondIDs(i,2),:), &
      &                                 3                           )
    enddo
    !$OMP END PARALLEL DO

    !$OMP PARALLEL DO
    do i = 1, nAngles
      AngleValues(i) = Angle(CartCoords(AngleIDs(i,2),:), &
      &                      CartCoords(AngleIDs(i,1),:), &
      &                      CartCoords(AngleIDs(i,3),:)  )
    enddo
    !$OMP END PARALLEL DO

    !$OMP PARALLEL DO
    do i = 1, nTorsions
      TorsionValues(i) = TorsionAngle(CartCoords(TorsionIDs(i,1),:), &
      &                               CartCoords(TorsionIDs(i,2),:), &
      &                               CartCoords(TorsionIDs(i,3),:), &
      &                               CartCoords(TorsionIDs(i,4),:)  )
    enddo
    !$OMP END PARALLEL DO

  END SUBROUTINE CartesianToRedundantInternal

!*

  SUBROUTINE PrintRedundantCoordinates

    IMPLICIT NONE

    integer :: i

    write(*,*)
    do i = 1, nBonds
      write(*,'(A8,I4,A3,F9.6)') "BOND    ", i, " = ", BondValues(i)
    enddo

    do i = 1, nAngles
      write(*,'(A8,I4,A3,F9.6)') "ANGLE   ", i, " = ", AngleValues(i)
    enddo

    do i = 1, nTorsions
      write(*,'(A8,I4,A3,F9.6)') "TORSION ", i, " = ", TorsionValues(i)
    enddo

  END SUBROUTINE PrintRedundantCoordinates

!*

  SUBROUTINE PrintCartesianCoordinates

    IMPLICIT NONE

    integer :: atom

    write(out,'(I1)') nAtoms; write(out,*)
    do atom = 1, nAtoms
      write(out,'(A2,I4,3F9.6)') Elements(atom), atom, CartCoords(atom,:)
    enddo

  END SUBROUTINE PrintCartesianCoordinates

!*

  SUBROUTINE CalculateBondEnergy

    IMPLICIT NONE

    integer :: i, j

    TotalBondEnergy = 0.0d0

    SumOverBonds: DO i = 1, nBonds

      select case (trim(adjustl(BondTypes(i))))

      case ("HARM")

        bondEnergies(i) = HarmonicEnergy(bondForceConstants(i), &
        &                                bondReferences(i),     &
        &                                BondValues(i)          )

        BondForces(i) =  HarmonicFirstDerivative_dr(bondForceConstants(i), &
        &                                           bondReferences(i),     &
        &                                           BondValues(i)          )

      case default

        write(*,*) "Bond Type is Unknown"

      end select

      SumOverCartesians: DO j = 1, 3

        AtomicForces(BondIDs(i,1),j) = AtomicForces(BondIDs(i,1),j) +                                            &
        &                              BondEnergies(i) * EuclideanDistanceDerivative(CartCoords(BondIDs(i,1),:), &
        &                              CartCoords(BondIDs(i,2),:),                                               &
        &                              3,                                                                        &
        &                              j                                                                         )

        AtomicForces(BondIDs(i,2),j) = AtomicForces(BondIDs(i,1),j) +                                            &
        &                              BondEnergies(i) * EuclideanDistanceDerivative(CartCoords(BondIDs(i,2),:), &
        &                              CartCoords(BondIDs(i,1),:),                                               &
        &                              3,                                                                        &
        &                              j                                                                         )

      END DO SumOverCartesians
  
    TotalBondEnergy = TotalBondEnergy + bondEnergies(i)

    END DO SumOverBonds

  END SUBROUTINE CalculateBondEnergy

!*

  SUBROUTINE CalculateAccelerations

    IMPLICIT NONE

    INTEGER :: atom, cart, j

    !This has to fill the array AtomicAccelerations for the dynamics runner

    CALL CalculateBondEnergy  ! gets bond energies and internal forces
    CALL CalculateAngleEnergy ! gets angle energies and internal forces

    j = 1

    do atom = 1, nAtoms

      do cart = 1, 3

        AtomicAccelerations(j) = -1.0D0 * AtomicForces(atom,cart) / AtomicMasses(atom)
        j = j+1

      enddo

    enddo

  END SUBROUTINE CalculateAccelerations

!*

SUBROUTINE CalculateAngleEnergy
IMPLICIT NONE

  integer :: i, cart

  TotalAngleEnergy = 0.0d0

  SumOverAngles: DO i = 1, nAngles

    select case (trim(adjustl(AngleTypes(i))))

      case ("HARM")

        AngleEnergies(i) = HarmonicEnergy(AngleForceConstants(i), &
        &                                 AngleReferences(i),     &
        &                                 AngleValues(i)          )

        AngleForces(i)   = HarmonicFirstDerivative_dr(AngleForceConstants(i), &
        &                                             AngleReferences(i),     &
        &                                             AngleValues(i)          )

      case default

        write(*,*) "Angle Type is Unknown"

    end select

  TotalAngleEnergy = TotalAngleEnergy + AngleEnergies(i)

    SumOverCartesians: DO cart = 1, 3
!      AtomicForces(AngleIDs(i,1)) = AtomicForces(AngleIDs(i,1)) + AngleForces(i)
! TODO
    END DO SumOverCartesians

  END DO SumOverAngles

END SUBROUTINE CalculateAngleEnergy

!*

SUBROUTINE PrintEnergyAndForces

  integer :: i

  do i = 1, nBonds
    write(*,'(A6,I4,A5,F9.6,A5,F9.6)') "BOND  ", i, " E = ", BondEnergies(i),  " F = ", BondForces(i)
  enddo

  do i = 1, nAngles
    write(*,'(A6,I4,A5,F9.6,A5,F9.6)') "ANGLE ", i, " E = ", AngleEnergies(i), " F = ", AngleForces(i)
  enddo

END SUBROUTINE PrintEnergyAndForces

END MODULE Conformation
