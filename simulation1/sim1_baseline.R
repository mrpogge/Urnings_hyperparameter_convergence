library(tidyverse)
set.seed(13181913)

sim1_urnings1_central = readRDS("sim1_urnings1_central.rds")
sim1_urnings1_better = readRDS("sim1_urnings1_better.rds")
sim1_urnings1_worse = readRDS("sim1_urnings1_worse.rds")
sim1_urnings2_central = readRDS("sim1_urnings2_central.rds")
sim1_urnings2_better = readRDS("sim1_urnings2_better.rds")
sim1_urnings2_worse = readRDS("sim1_urnings2_worse.rds")

results_u1 = rbind(sim1_urnings1_central, 
                   sim1_urnings1_better, 
                   sim1_urnings1_worse)

results = rbind(sim1_urnings2_central, 
                sim1_urnings2_better, 
                sim1_urnings2_worse)

rm(sim1_urnings1_central, 
   sim1_urnings1_better, 
   sim1_urnings1_worse, 
   sim1_urnings2_central, 
   sim1_urnings2_better, 
   sim1_urnings2_worse)

pars = results[,c(1,9)]
pars_u1 = results_u1[,c(1,9)]

rm(results, results_u1)

baseline = matrix(0, nrow = nrow(pars), ncol = 500)
baseline_u1 = matrix(0, nrow = nrow(pars_u1), ncol = 500)

for(i in 1:nrow(pars)){
  baseline[i, ] = rbinom(500, as.integer(pars[i,1]), prob = pars[i,2]) / as.integer(pars[i,1]) 
  baseline_u1[i, ] = rbinom(500, as.integer(pars_u1[i,1]), prob = pars_u1[i,2]) / as.integer(pars_u1[i,1])
}

sim1_baseline = list(baseline, baseline_u1)

saveRDS(sim1_baseline, "sim1_baseline.rds")

sim1_baseline_MSE = list((baseline - pars[,2])^2, (baseline - pars[,2])^2)
saveRDS(sim1_baseline_MSE, "sim1_baseline_MSE.rds")

