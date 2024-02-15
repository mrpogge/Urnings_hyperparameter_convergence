################################################################################  
#fix params
################################################################################
sd = 1
nplayers = 900
reps = 1000
n_games = 500


################################################################################  
#parameters influencing the baseline mse
################################################################################
dist_type = c(1, 0, -1)
urn_size = c(8,16,32,64, 128)

combs = expand.grid(dist_type, urn_size)
combs[,3] = rep(0, nrow(combs))

for(i in 1:nrow(combs)){
  holder = matrix(0, nrow = reps, ncol = n_games)
  for(r in 1:reps){
    pi_pl = qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers), combs[i,1], sd)
    pi_pl = exp(pi_pl) / (1 + exp(pi_pl)) 
    pi_pl = sort(pi_pl)
    
    sim_vals = t(sapply(pi_pl,rbinom,n = 500,size = combs[i,2]) / combs[i,2])
    
    holder[r, ] = colMeans((sim_vals - pi_pl)^2)
  }
  
  combs[i, 3] = mean(apply(holder, MARGIN = 2, quantile, probs = 0.975))
  print(combs[i, 3])
}


saveRDS(combs, file = "output/mse_baseline_975.rds")
