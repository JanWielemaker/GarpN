distance(object) := distance(object) + velocity(object) * dt.
velocity(object) := velocity(object) + acceleration(object) * dt.
acceleration(object) := net_force(object)/m.
gravity(object) := m*g.
air_resistance(object) := 0.5*cw*rho*opp*velocity(object)^2.
net_force(object) := gravity(object) - air_resistance(object).

t := t + dt.
t := 0.
dt := 0.01.

distance(object) := 0.
velocity(object) := 0.
m := 0.270.
g := 9.81.
opp := 0.0347.
rho := 1.293.
cw := 0.5.
