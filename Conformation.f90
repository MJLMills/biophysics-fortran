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
  integer              :: CartTrajecUnit = 15

CONTAINS

SUBROUTINE CreateConformation
IMPLICIT NONE

    if (.NOT. allocated(CartCoords))           then; allocate(CartCoords(nAtoms,3))          ; CartCoords(:,:)        = 0.0d0; endif
    if (.NOT. allocated(BondValues))           then; allocate(BondValues(nBonds))            ; BondValues(:)          = 0.0d0; endif
    if (.NOT. allocated(AngleValues))          then; allocate(AngleValues(nAngles))          ; AngleValues(:)         = 0.0d0; endif
    if (.NOT. allocated(TorsionValues))        then; allocate(TorsionValues(nTorsions))      ; TorsionValues(:)       = 0.0d0; endif
    if (.NOT. allocated(BondEnergies))         then; allocate(BondEnergies(nBonds))          ; BondEnergies(:)        = 0.0d0; endif
    if (.NOT. allocated(AngleEnergies))        then; allocate(AngleEnergies(nAngles))        ; AngleEnergies(:)       = 0.0d0; endif
    if (.NOT. allocated(AtomicForces))         then; allocate(AtomicForces(nAtoms,3))        ; AtomicForces(:,:)      = 0.0d0; endif
    if (.NOT. allocated(BondForces))           then; allocate(BondForces(nBonds))            ; BondForces(:)          = 0.0d0; endif
    if (.NOT. allocated(AngleForces))          then; allocate(AngleForces(nAngles))          ; AngleForces(:)         = 0.0d0; endif
    if (.NOT. allocated(AtomicAccelerations))  then; allocate(AtomicAccelerations(3*nAtoms)) ; AtomicAccelerations(:) = 0.0d0; endif

END SUBROUTINE CreateConformation

SUBROUTINE DestroyConformation
IMPLICIT NONE

    if (allocated(CartCoords))          then; deallocate(CartCoords)         ; endif
    if (allocated(BondValues))          then; deallocate(BondValues)         ; endif
    if (allocated(AngleValues))         then; deallocate(AngleValues)        ; endif
    if (allocated(TorsionValues))       then; deallocate(TorsionValues)      ; endif
    if (allocated(BondEnergies))        then; deallocate(BondEnergies)       ; endif
    if (allocated(AngleEnergies))       then; deallocate(AngleEnergies)      ; endif
    if (allocated(AtomicForces))        then; deallocate(AtomicForces)       ; endif
    if (allocated(BondForces))          then; deallocate(BondForces)         ; endif
    if (allocated(AngleForces))         then; deallocate(AngleForces)        ; endif
    if (allocated(AtomicAccelerations)) then; deallocate(AtomicAccelerations); endif

END SUBROUTINE DestroyConformation

!*

SUBROUTINE CartesianToRedundantInternal
IMPLICIT NONE

  integer :: i

  do i = 1, nBonds
    BondValues(i) = EuclideanDistance(CartCoords(BondIDs(i,1),:), &
    &                                 CartCoords(BondIDs(i,2),:), &
    &                                 3                           )
  enddo

  do i = 1, nAngles
    AngleValues(i) = Angle(CartCoords(AngleIDs(i,2),:), &
    &                      CartCoords(AngleIDs(i,1),:), &
    &                      CartCoords(AngleIDs(i,3),:)  )
  enddo

  do i = 1, nTorsions
    TorsionValues(i) = TorsionAngle(CartCoords(TorsionIDs(i,1), &
    &                               CartCoords(TorsionIDs(i,2), &
    &                               CartCoords(TorsionIDs(i,3), &
    &                               CartCoords(TorsionIDs(i,4)  )
  enddo

END SUBROUTINE CartesianToRedundantInternal

!*
SUBROUTINE PrintRedundantCoordinates

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

integer :: atom

  write(CartTrajecUnit,'(I1)') nAtoms; write(CartTrajecUnit,*)
  do atom = 1, nAtoms
    write(CartTrajecUnit,'(A2,I4,3F9.6)') Elements(atom), atom, CartCoords(atom,:)
  enddo

END SUBROUTINE PrintCartesianCoordinates

SUBROUTINE CalculateBondEnergy

integer :: i, j

TotalBondEnergy = 0.0d0

do i = 1, nBonds

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

  do j = 1, 3

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

  enddo
  
  TotalBondEnergy = TotalBondEnergy + bondEnergies(i)

enddo

END SUBROUTINE CalculateBondEnergy

!*

SUBROUTINE CalculateAccelerations
IMPLICIT NONE

integer :: atom, cart, j

!This has to fill the array AtomicAccelerations for the dynamics runner

call CalculateBondEnergy  ! gets bond energies and internal forces
call CalculateAngleEnergy ! gets angle energies and internal forces

j = 1

do atom = 1, nAtoms

  do cart = 1, 3

    AtomicAccelerations(j) = -1.0D0 * AtomicForces(atom,cart) / AtomicMasses(atom)
    j = j+1

  enddo
  print*, 

enddo


END SUBROUTINE CalculateAccelerations

!*

SUBROUTINE CalculateAngleEnergy
IMPLICIT NONE

  integer :: i, cart

  TotalAngleEnergy = 0.0d0

  do i = 1, nAngles

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

    do cart = 1, 3
!      AtomicForces(AngleIDs(i,1)) = AtomicForces(AngleIDs(i,1)) + AngleForces(i)
    enddo

  enddo

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
