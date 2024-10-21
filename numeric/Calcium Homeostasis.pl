calcitonin(thyroid) := calcitonin(thyroid) + inflow_calcitonin * dt.
pth(parathyriod_gland) := pth(parathyriod_gland) + inflow_pth * dt.

t := t + dt.

ca_ions(bones) := calcitonin(thyroid)*c_calcitonin_ca_bones-pth(parathyriod_gland)*c_pth_ca_bones.
resorption_ca_ions(kidneys) := ca_ions(bones)*c_ca_bones_resorption_ca.
ca_ions(blood) := ca_ions(bones)*c_ca_bones_ca_blood- resorption_ca_ions(kidneys)*c_resorption_ca_ca_blood.
difference(thyroid) := ca_ions(blood)-norm(thyroid).
inflow_calcitonin := difference(thyroid)*c_calcitonin.
inflow_pth := difference(thyroid)*c_pth.

t := 0.
dt := 0.1.

calcitonin(thyroid) := 20.
pth(parathyriod_gland) := 10.
norm(thyroid) := 10.
c_calcitonin := 0.4.
c_pth := -0.4.
c_calcitonin_ca_bones := 0.3.
c_pth_ca_bones := 0.1.
c_ca_bones_resorption_ca := 0.3.
c_resorption_ca_ca_blood := 0.5.
c_ca_bones_ca_blood := -0.5.
