MODULE Dynamics

  use Conformation

IMPLICIT NONE

real(8), allocatable :: positions(:),     accelerations(:),     velocities(:)
real(8), allocatable :: nextPositions(:), nextAccelerations(:), nextVelocities(:)

  CONTAINS

SUBROUTINE AllocateArrays
IMPLICIT NONE

if (.NOT. allocated(positions))         then; allocate(positions(3*nAtoms))         ; positions(:)         = 0.0d0; endif
if (.NOT. allocated(velocities))        then; allocate(velocities(3*nAtoms))        ; velocities(:)        = 0.0d0; endif
if (.NOT. allocated(accelerations))     then; allocate(accelerations(3*nAtoms))     ; accelerations(:)     = 0.0d0; endif
if (.NOT. allocated(nextAccelerations)) then; allocate(nextAccelerations(3*nAtoms)) ; nextAccelerations(:) = 0.0d0; endif
if (.NOT. allocated(nextPositions))     then; allocate(nextPositions(3*nAtoms))     ; nextPositions(:)     = 0.0d0; endif
if (.NOT. allocated(nextVelocities))    then; allocate(nextVelocities(3*nAtoms))    ; nextVelocities(:)    = 0.0d0; endif

END SUBROUTINE AllocateArrays

SUBROUTINE DeallocateArrays
IMPLICIT NONE

if (allocated(positions))         then; deallocate(positions)         ; endif
if (allocated(velocities))        then; deallocate(velocities)        ; endif
if (allocated(accelerations))     then; deallocate(accelerations)     ; endif
if (allocated(nextAccelerations)) then; deallocate(nextAccelerations) ; endif
if (allocated(nextPositions))     then; deallocate(nextPositions)     ; endif
if (allocated(nextVelocities))    then; deallocate(nextVelocities)    ; endif

END SUBROUTINE DeallocateArrays

SUBROUTINE VelocityVerlet(timestep,maxsteps)
IMPLICIT NONE

integer, intent(in) :: maxSteps
real(8), intent(in) :: timestep
real(8) :: half_sq_timestep, half_timestep
integer :: tstep

half_timestep = 0.5d0 * timestep
half_sq_timestep = half_timestep * timestep

  call AllocateArrays

  call VectoriseCartesians
  call CalculateAccelerations; accelerations = AtomicAccelerations

!  velocities = generateRandom !IMPLEMENT

  do tstep = 1, maxSteps

!    write(*,'(I4,F12.9)') tstep, tstep*timestep; write(*,*)
!    call WriteData

    nextPositions(:) = positions(:) + timestep*velocities(:) + half_sq_timestep * accelerations(:)

    call UpdateCartesians
    call CalculateAccelerations; accelerations(:) = AtomicAccelerations(:)
    nextVelocities(:) = velocities(:) + half_timestep * (accelerations(:) + nextAccelerations(:))

    positions(:)     = nextPositions(:)     ; nextPositions(:)     = 0.0d0
    velocities(:)    = nextVelocities(:)    ; nextVelocities(:)    = 0.0d0
    accelerations(:) = nextAccelerations(:) ; nextAccelerations(:) = 0.0d0

  enddo

  call DeallocateArrays

END SUBROUTINE VelocityVerlet

SUBROUTINE VectoriseCartesians
IMPLICIT NONE

integer :: atom, cart, j

  j = 1
  do atom = 1, nAtoms

    do cart = 1, 3
      positions(j) = CartCoords(atom,cart)
      j=j+1
    enddo

  enddo

END SUBROUTINE VectoriseCartesians

SUBROUTINE UpdateCartesians
IMPLICIT NONE

integer :: atom, cart, j

  j = 1
  do atom = 1, nAtoms

    do cart = 1, 3
      CartCoords(atom,cart) = nextPositions(j)
      j=j+1
    enddo

  enddo

  call PrintCartesianCoordinates
  call CartesianToRedundantInternal
!  call PrintRedundantCoordinates

END SUBROUTINE UpdateCartesians

!*

SUBROUTINE WriteData
IMPLICIT NONE

integer :: atom, cart, j

j = 1
write(*,*) "ATOM POSITION VELOCITY ACCELERATION"
do atom = 1, nAtoms
  do cart = 1, 3
    write(*,'(I4,9F10.6)') atom, positions(j), velocities(j), accelerations(j)
    j = j+1
  enddo
enddo

END SUBROUTINE WriteData

END MODULE Dynamics

!Velocity Verlet (better than leapfrog (gives r(t), v(t), a(t)) and Verlet (includes v(t) and no differencing of big numbers)

!A) r(t+dt) = r(t) + dt * v(t) + 1/2 * dt^2 * a(t)
!B) v(t+dt) = v(t) + 1/2 * dt * [a(t) + a(t+dt)]
!0) Initialise: source r(0), guess v(0) and compute a(0) from r(0)

!1) Write properties at t
!2) calculate r(t+dt) from r(t), v(t) and a(t) using A
!3) calculate a(t+dt) from r(t+dt)
!4) calculate v(t+dt) from v(t), a(t) and a(t+dt) using B
!5) t+dt -> t, write properties at t, go to 1

!Beeman's algorithm (more accurate velocities than velocity verlet, hence better energy conservation. Isn't as fast as vV)
!Also note that calculating r(t+dt) requires a(t) and a(t-dt). This raises the question of where a(t-dt) comes from.
!It can be roughly obtained by truncating the taylor series after the first term, then:

!r(0-dt) = r(0) - dt * v(0) after v(0) has been assigned.

!r(t+dt) = r(t) + dt * v(t) + 2/3 * dt^2 * a(t) - 1/6 * dt^2 * a(t-dt)
!v(t+dt) = v(t) + 1/3 * dt * a(t+dt) + 5/6 *dt * a(t) - 1/6 * dt * a(t-dt)

!0) Initialise: source r(0), guess v(0), compute a(0) from r(0), get a(t-dt)?

!1) Write properties at t
!2) calculate r(t+dt) from r(t), v(t), a(0) and a(-dt) using A
!3) calcualte a(t+dt) from r(t+dt)
!4) calculate v(t+dt) from v(t), a(t), a(t+dt) and a(t-dt)
!5) t+dt -> t, go to 1

!Assigning initial velocities:

!A) choose from a uniform random distribution
!B) choose from a gaussian random distribution
!C) choose from a maxwell boltzmann distribution at T

!This is NVP ensemble, to get NVEP, adjust so that the total system momentum is zero:

!        sum the atomic momenta along each axis, i.e. P_j = sum(i=0,i<N) p_i,j (i indexes atoms, j = {x,y,z})
!        divide P_j by total mass M = sum(i=0,i<n) m_i
!        subtract P_j from each atomic velocity v_i,j

!Calculating a(t) from r(t)

!a_i,j = F_i,j / m_i (i is an atom, j = {x,y,z}) 

!F_i,j = - d E / d i,j - here is all the computational expense and maths hell

!Q: What ensemble does MOLARIS run in? NVT?
