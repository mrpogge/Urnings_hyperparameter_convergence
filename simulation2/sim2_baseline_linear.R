################################################################################
#simulation with linear change
################################################################################
sim2_linear_better = readRDS("sim2_linear_better.rds")
sim2_linear_worse = readRDS("sim2_linear_worse.rds")
sim2_linear_central = readRDS("sim2_linear_central.rds")

better_label = data.frame("dist_type" = rep("better", times = nrow(sim2_linear_better)))
worse_label = data.frame("dist_type" = rep("worse", times = nrow(sim2_linear_worse)))
central_label = data.frame("dist_type" = rep("central", times = nrow(sim2_linear_central)))

sim2_linear_better = cbind(better_label, sim2_linear_better)
sim2_linear_worse = cbind(worse_label, sim2_linear_worse)
sim2_linear_central = cbind(central_label, sim2_linear_central)

rm(better_label, worse_label, central_label)

sim2_linear = rbind(sim2_linear_better, sim2_linear_central, sim2_linear_worse)
rm(sim2_linear_better, sim2_linear_central, sim2_linear_worse)
colnames(sim2_linear)[4] = "amount_of_change"

################################################################################
#recreating linear change
################################################################################
change_matrix_linear = matrix(0, nrow = nrow(sim2_linear), ncol = 500)
change_matrix_linear[,1] = log(sim2_linear[,"true_value_first"] / (1-sim2_linear[,"true_value_first"]))
for(cl in 2:ncol(change_matrix_linear)){
  change_matrix_linear[, cl] = change_matrix_linear[, cl-1] + sim2_linear[, "amount_of_change"]
}
change_matrix_linear = exp(change_matrix_linear) / (1 + exp(change_matrix_linear))

################################################################################
#calculating baseline
################################################################################
baseline_matrix = change_matrix_linear
#baseline_coverage_matrix = change_matrix_linear
for(i in 1:nrow(change_matrix_linear)){
  for(j in 1:ncol(change_matrix_linear)){
    baseline_matrix[i,j] = rbinom(1, size = as.numeric(sim2_linear[i, "player_urn_size"]), prob = change_matrix_linear[i,j])
   # CI = binconf(baseline_matrix[i,j], as.numeric(sim2_linear[i, "player_urn_size"]), method = "wilson")[2:3]
  #  baseline_coverage_matrix[i,j] = as.numeric(CI[1] < change_matrix_linear[i,j] & change_matrix_linear[i,j] < CI[2])
    if(i %% 1000 == 0){
      print(c("i am at: ", i))
    }
  }
}

baseline_linear = baseline_matrix

saveRDS(baseline_linear, "sim2_baseline.rds")