################################################################################
#simulation with discrete change
################################################################################
sim2_discrete_better_10 = readRDS("sim2_discrete_better_10.rds")
sim2_discrete_worse_10 = readRDS("sim2_discrete_worse_10.rds")
sim2_discrete_central_10 = readRDS("sim2_discrete_central_10.rds")

better_label = data.frame("dist_type" = rep("better", times = nrow(sim2_discrete_better_10)))
worse_label = data.frame("dist_type" = rep("worse", times = nrow(sim2_discrete_worse_10)))
central_label = data.frame("dist_type" = rep("central", times = nrow(sim2_discrete_central_10)))

sim2_discrete_better_10 = cbind(better_label, sim2_discrete_better_10)
sim2_discrete_worse_10 = cbind(worse_label, sim2_discrete_worse_10)
sim2_discrete_central_10 = cbind(central_label, sim2_discrete_central_10)

rm(better_label, worse_label, central_label)

sim2_discrete_10 = rbind(sim2_discrete_better_10, sim2_discrete_central_10, sim2_discrete_worse_10)
rm(sim2_discrete_better_10, sim2_discrete_central_10, sim2_discrete_worse_10)


################################################################################
#recreating discrete change
################################################################################
change_matrix_discrete_10 = matrix(rep(sim2_discrete_10[,"true_value_first"], 500), nrow = nrow(sim2_discrete_10), ncol = 500)
change_matrix_discrete_10 = log(change_matrix_discrete_10 / (1- change_matrix_discrete_10))

jumps = rep(0:9, each = 50)
change_per_jump = sim2_discrete_10$amount_of_change

change_matrix_discrete_10 = change_matrix_discrete_10 + outer(change_per_jump, jumps, "*")
change_matrix_discrete_10 = exp(change_matrix_discrete_10) / (1 + exp(change_matrix_discrete_10))

################################################################################
#calculating baseline
################################################################################
baseline_matrix = change_matrix_discrete_10
for(i in 1:nrow(change_matrix_discrete_10)){
  for(j in 1:ncol(change_matrix_discrete_10)){
    baseline_matrix[i,j] = rbinom(1, size = as.numeric(sim2_discrete_10[i, "player_urn_size"]), prob = change_matrix_discrete_10[i,j])
    if(i %% 1000 == 0){
      print(c("i am at: ", i))
    }
  }
}

saveRDS(baseline_matrix, "sim2_baseline_discrete_10.rds")