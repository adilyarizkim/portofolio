# Load EpiModel
suppressMessages(library(EpiModel))
control<- control.icm(type = "SIR", nsteps = 104, nsims = 1)
init<- init.icm(s.num = 407, i.num = 1, r.num = 0)
param<- param.icm(inf.prob = 0.018, act.rate = 7, rec.rate = 0.00044, 
                  a.rate = 0.0000009, ds.rate = 0.000007, di.rate = 0.000007, 
                  dr.rate = 0.000008)
sim     <- icm(param, init, control)
plot(sim)
baseline=Periodeawal$`terkonfirmasi`
lines(baseline, type="l", col = "red", lwd=2)

