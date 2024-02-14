library(Hmisc)
post_hoc_multijump = readRDS("post_hoc_multi_jump.rds")


#recreating change
#discrete positive
jump_matrix = matrix(0, nrow = nrow(post_hoc_multijump), ncol = 500)
jump_matrix[,c(100,200,300,400)] = rep(1, times = nrow(post_hoc_multijump))

change_matrix = matrix(rep(post_hoc_multijump[,"true_value_first"], 500), nrow = nrow(post_hoc_multijump), ncol = 500)
change_matrix = log(change_matrix / (1- change_matrix))
for(i in 2:500){
  change_matrix[,i] = change_matrix[,i-1] + jump_matrix[,i-1] * (post_hoc_multijump[,"amount_of_change"]/4)
}

change_matrix = exp(change_matrix) / (1 + exp(change_matrix))

################################################################################
#calculating baseline
################################################################################
baseline_matrix = change_matrix
baseline_coverage_matrix = change_matrix
for(i in 1:nrow(change_matrix)){
  for(j in 1:ncol(change_matrix)){
    baseline_matrix[i,j] = rbinom(1, size = as.numeric(post_hoc_multijump[i, "player_urn_size"]), prob = change_matrix[i,j])
    CI = binconf(baseline_matrix[i,j], as.numeric(post_hoc_multijump[i, "player_urn_size"]), method = "wilson")[2:3]
    baseline_coverage_matrix[i,j] = as.numeric(CI[1] < change_matrix[i,j] & change_matrix[i,j] < CI[2])
    if(i %% 10000 == 0){
      print(c("i am at: ", i))
    }
  }
}

baseline_discrete = list(baseline_matrix, baseline_coverage_matrix)

saveRDS(baseline_discrete, "post_hoc_baseline_multijump.rds")

