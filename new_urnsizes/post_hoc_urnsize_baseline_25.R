library(Hmisc)
################################################################################
#simulation with linear change
################################################################################
post_hoc_urnsize = readRDS("post_hoc_urnsize_discrete_25.rds")
post_hoc_urnsize_better = readRDS("post_hoc_urnsize_discrete_25_better.rds")
post_hoc_urnsize_worse = readRDS("post_hoc_urnsize_discrete_25_worse.rds")

post_hoc_urnsize = rbind(post_hoc_urnsize_better, post_hoc_urnsize, post_hoc_urnsize_worse)
rm(post_hoc_urnsize_better, post_hoc_urnsize_worse)
colnames(post_hoc_urnsize)[3] = "amount_of_change" #check the right indexing
################################################################################
#recreating linear change
################################################################################
change_matrix_discrete_25 = matrix(rep(post_hoc_urnsize[,"true_value_first"], 500), nrow = nrow(post_hoc_urnsize), ncol = 500)
change_matrix_discrete_25 = log(change_matrix_discrete_25 / (1- change_matrix_discrete_25))

jumps = rep(0:24, each = 20)
change_per_jump = post_hoc_urnsize$amount_of_change

change_matrix_discrete_25 = change_matrix_discrete_25 + outer(change_per_jump, jumps, "*")
change_matrix_discrete_25 = exp(change_matrix_discrete_25) / (1 + exp(change_matrix_discrete_25))

################################################################################
#calculating baseline
################################################################################
baseline_matrix = change_matrix_discrete_25
#baseline_coverage_matrix = change_matrix_linear
for(i in 1:nrow(change_matrix_discrete_25)){
  for(j in 1:ncol(change_matrix_discrete_25)){
    baseline_matrix[i,j] = rbinom(1, size = as.numeric(post_hoc_urnsize[i, "player_urn_size"]), prob = change_matrix_discrete_25[i,j])
    #CI = binconf(baseline_matrix[i,j], as.numeric(post_hoc_urnsize[i, "player_urn_size"]), method = "wilson")[2:3]
    #baseline_coverage_matrix[i,j] = as.numeric(CI[1] < change_matrix_linear[i,j] & change_matrix_linear[i,j] < CI[2])
    if(i %% 1000 == 0){
      print(c("i am at: ", i))
    }
  }
}

baseline_linear = baseline_matrix

saveRDS(baseline_linear, "post_hoc_urnsize_baseline_25.rds")