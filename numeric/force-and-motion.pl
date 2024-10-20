y := y + stroom_1 * dt.
stroom_1 := vy.
vy := vy + ay*dt.
ay := fnetto/m.
fz := m*g.
flucht := 0.5*cw*rho*opp*vy^2.
fnetto := fz + flucht.

t := t + dt.
t := 0.
dt := 0.1.

y := 70.
vy := 0.
m := 0.270.
g := -9.81.
opp := 0.0347.
rho := 1.293.
cw := 0.5.
