memory.size(50000)
memory.limit(50000)
library(EpiModel)
library(GA)
library(readxl)
Periodeawal<- read_excel("Periodeawal.xlsx")
baseline=Periodeawal$`terkonfirmasi`

#estimasi parameter GA
start=Sys.time()
model=ga(type = 'real-valued',
         fitness = function(par) {
           control<- control.icm(type = "SIR", nsteps = NROW(baseline), nsims = 1)
           init<- init.icm(s.num = 407, i.num = 1, r.num = 0)
           param<- param.icm(inf.prob = par[1], act.rate = par[2], rec.rate = par[3], 
                              a.rate = par[4], ds.rate = par[5], di.rate = par[6], 
                              dr.rate = par[7])
          
            sim     <- icm(param, init, control)
           sim.si=sim[["epi"]][["i.num"]]
           si.diff=(baseline-sim.si)^2
           mean.si.diff=-mean(colSums(si.diff))},
         maxiter = 100,
         popSize = 100,
         keepBest = T,
lower = c(0.0178,6.8,0.00438,0.00000088,0.0000068,0.0000068,0.0000078),
upper = c(0.018,7,0.0044,0.0000009,0.000007,0.000007,0.000008))

end=Sys.time()
end-start

