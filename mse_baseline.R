################################################################################  
#fix params
################################################################################
sd = 1
nplayers = 900
reps = 10000
n_games = 500

calculate_mse_baseline = function(pi, u){
  return(mean((sapply(pi, rbinom, n = 1, size = u) / u - pi)^2))
}
################################################################################  
#parameters influencing the baseline mse
################################################################################
dist_type = c(1, 0, -1)
urn_size = c(8,16,32,64)

combs = expand.grid(dist_type, urn_size)
combs[,3] = rep(0, nrow(combs))

for(i in 1:nrow(combs)){
  print(combs[i,])
  pi_pl = qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers), combs[i,1], sd)
  pi_pl = exp(pi_pl) / (1 + exp(pi_pl)) 
  pi_pl = sort(pi_pl)
  
  mse_container = numeric(length = reps)
  for(j in 1:reps){
    mse_container[j] = calculate_mse_baseline(pi = pi_pl, u = combs[i,2])
  }
  
  combs[i,3] = quantile(mse_container, probs = 0.975)

}


saveRDS(combs, file = "output/mse_baseline_975.rds")

################################################################################
#mse baseline for cold start with 50% new players
################################################################################
for(i in 1:nrow(combs)){
  print(combs[i,])
  pi_pl = qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers), combs[i,1], sd)
  pi_pl = exp(pi_pl) / (1 + exp(pi_pl)) 
  pi_pl = sort(pi_pl)
  
  pi_pl = pi_pl[1:nplayers %% 2 == 0]
  
  mse_container = numeric(length = reps)
  for(j in 1:reps){
    mse_container[j] = calculate_mse_baseline(pi = pi_pl, u = combs[i,2])
  }
  
  combs[i,3] = quantile(mse_container, probs = 0.975)
  
}
saveRDS(combs, file = "output/mse_baseline_975_50.rds")

################################################################################
#mse baseline for cold start with 10% new players
################################################################################
for(i in 1:nrow(combs)){
  print(combs[i,])
  pi_pl = qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers), combs[i,1], sd)
  pi_pl = exp(pi_pl) / (1 + exp(pi_pl)) 
  pi_pl = sort(pi_pl)
  
  pi_pl = pi_pl[1:nplayers %% 10 == 0]
  
  mse_container = numeric(length = reps)
  for(j in 1:reps){
    mse_container[j] = calculate_mse_baseline(pi = pi_pl, u = combs[i,2])
  }
  
  combs[i,3] = quantile(mse_container, probs = 0.975)
  
}

saveRDS(combs, file = "output/mse_baseline_975_10.rds")


