!Velocity Verlet (better than leapfrog (gives r(t), v(t), a(t)) and Verlet (includes v(t) and no differencing of big numbers)

A) r(t+dt) = r(t) + dt * v(t) + 1/2 * dt^2 * a(t)
B) v(t+dt) = v(t) + 1/2 * dt * [a(t) + a(t+dt)]

0) Initialise: source r(0), guess v(0) and compute a(0) from r(0)

1) Write properties at t
2) calculate r(t+dt) from r(t), v(t) and a(t) using A
3) calculate a(t+dt) from r(t+dt)
4) calculate v(t+dt) from v(t), a(t) and a(t+dt) using B
5) t+dt -> t, write properties at t, go to 1

!Beeman's algorithm (more accurate velocities than velocity verlet, hence better energy conservation. Isn't as fast as vV)
!Also note that calculating r(t+dt) requires a(t) and a(t-dt). This raises the question of where a(t-dt) comes from.
!It can be roughly obtained by truncating the taylor series after the first term, then:

r(0-dt) = r(0) - dt * v(0) after v(0) has been assigned.

r(t+dt) = r(t) + dt * v(t) + 2/3 * dt^2 * a(t) - 1/6 * dt^2 * a(t-dt)
v(t+dt) = v(t) + 1/3 * dt * a(t+dt) + 5/6 *dt * a(t) - 1/6 * dt * a(t-dt)

0) Initialise: source r(0), guess v(0), compute a(0) from r(0), get a(t-dt)?

1) Write properties at t
2) calculate r(t+dt) from r(t), v(t), a(0) and a(-dt) using A
3) calcualte a(t+dt) from r(t+dt)
4) calculate v(t+dt) from v(t), a(t), a(t+dt) and a(t-dt)
5) t+dt -> t, go to 1

!Assigning initial velocities:

A) choose from a uniform random distribution
B) choose from a gaussian random distribution
C) choose from a maxwell boltzmann distribution at T

This is NVP ensemble, to get NVEP, adjust so that the total system momentum is zero:

        sum the atomic momenta along each axis, i.e. P_j = sum(i=0,i<N) p_i,j (i indexes atoms, j = {x,y,z})
        divide P_j by total mass M = sum(i=0,i<n) m_i
        subtract P_j from each atomic velocity v_i,j

Calculating a(t) from r(t)

a_i,j = F_i,j / m_i (i is an atom, j = {x,y,z}) 

F_i,j = - d E / d i,j - here is all the computational expense and maths hell

Q: What ensemble does MOLARIS run in? NVT?
