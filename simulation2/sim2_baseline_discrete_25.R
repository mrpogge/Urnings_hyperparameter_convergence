################################################################################
#simulation with discrete change
################################################################################
sim2_discrete_better_25 = readRDS("sim2_discrete_better_25.rds")
sim2_discrete_worse_25 = readRDS("sim2_discrete_worse_25.rds")
sim2_discrete_central_25 = readRDS("sim2_discrete_central_25.rds")

better_label = data.frame("dist_type" = rep("better", times = nrow(sim2_discrete_better_25)))
worse_label = data.frame("dist_type" = rep("worse", times = nrow(sim2_discrete_worse_25)))
central_label = data.frame("dist_type" = rep("central", times = nrow(sim2_discrete_central_25)))

sim2_discrete_better_25 = cbind(better_label, sim2_discrete_better_25)
sim2_discrete_worse_25 = cbind(worse_label, sim2_discrete_worse_25)
sim2_discrete_central_25 = cbind(central_label, sim2_discrete_central_25)

rm(better_label, worse_label, central_label)

sim2_discrete_25 = rbind(sim2_discrete_better_25, sim2_discrete_central_25, sim2_discrete_worse_25)
rm(sim2_discrete_better_25, sim2_discrete_central_25, sim2_discrete_worse_25)


################################################################################
#recreating discrete change
################################################################################
change_matrix_discrete_25 = matrix(rep(sim2_discrete_25[,"true_value_first"], 500), nrow = nrow(sim2_discrete_25), ncol = 500)
change_matrix_discrete_25 = log(change_matrix_discrete_25 / (1- change_matrix_discrete_25))

jumps = rep(0:24, each = 20)
change_per_jump = sim2_discrete_25$amount_of_change

change_matrix_discrete_25 = change_matrix_discrete_25 + outer(change_per_jump, jumps, "*")
change_matrix_discrete_25 = exp(change_matrix_discrete_25) / (1 + exp(change_matrix_discrete_25))

################################################################################
#calculating baseline
################################################################################
baseline_matrix = change_matrix_discrete_25
for(i in 1:nrow(change_matrix_discrete_25)){
  for(j in 1:ncol(change_matrix_discrete_25)){
    baseline_matrix[i,j] = rbinom(1, size = as.numeric(sim2_discrete_25[i, "player_urn_size"]), prob = change_matrix_discrete_25[i,j])
    if(i %% 1000 == 0){
      print(c("i am at: ", i))
    }
  }
}

saveRDS(baseline_matrix, "sim2_baseline_discrete_25.rds")