library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(png)
library(grid)

################################################################################
################################################################################
#LARGER URN SIZES POST HOC SIMULATION 
################################################################################
################################################################################

################################################################################
#simulation with linear change
################################################################################
post_hoc_urnsize = readRDS("post_hoc_urnsize.rds")
colnames(post_hoc_urnsize)[3] = "amount_of_change"

################################################################################
#recreating linear change
################################################################################
change_matrix_linear = matrix(0, nrow = nrow(post_hoc_urnsize), ncol = 500)
change_matrix_linear[,1] = log(post_hoc_urnsize[,"true_value_first"] / (1-post_hoc_urnsize[,"true_value_first"]))
for(cl in 2:ncol(change_matrix_linear)){
  change_matrix_linear[, cl] = change_matrix_linear[, cl-1] + post_hoc_urnsize[, "amount_of_change"]
}
change_matrix_linear = exp(change_matrix_linear) / (1 + exp(change_matrix_linear))

################################################################################
#calculating MSE
################################################################################
linear_mse_helper = (post_hoc_urnsize %>% select(starts_with("iter")) - change_matrix_linear)^2

linear_mse = post_hoc_urnsize %>% select(-starts_with("coverage")) 
linear_mse[,6:506] = linear_mse_helper
rm(linear_mse_helper)

################################################################################
#calculating coverage
################################################################################

covered = rowMeans(post_hoc_urnsize %>% select(starts_with("coverage")))
post_hoc_urnsize = cbind(post_hoc_urnsize, covered)
colnames(post_hoc_urnsize)[1006] = "covered"

################################################################################
#calculating baseline
################################################################################
baseline_linear = readRDS("post_hoc_urnsize_baseline.rds")
baseline_results = baseline_linear[[1]]
baseline_coverage = baseline_linear[[2]]
rm(baseline_linear)

baseline_results = baseline_res = baseline_results / as.numeric(post_hoc_urnsize[,1])
baseline_results = (baseline_results - change_matrix_linear) ^ 2
baseline_results = cbind(post_hoc_urnsize[,1:5], baseline_results)
baseline_coverage = cbind(post_hoc_urnsize[,1:5], baseline_coverage)
baseline_res = cbind(post_hoc_urnsize[,1:5], baseline_res)
colnames(baseline_results) = c(colnames(post_hoc_urnsize[,1:5]), paste0("iter", c(1:500)))
colnames(baseline_coverage) = c(colnames(post_hoc_urnsize[,1:5]), paste0("coverage", c(1:500)))
colnames(baseline_res) = c(colnames(post_hoc_urnsize[,1:5]), paste0("iter", c(1:500)))

################################################################################
#main effect of adaptivity
################################################################################
adapt_me = post_hoc_urnsize %>%
  group_by(adapt) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(adapt,starts_with("coverage"))

plot(as.vector(unlist(adapt_me[4,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "Coverage")
lines(as.vector(unlist(adapt_me[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(adapt_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(adapt_me[2,-c(1,2)])), col = 4)

b_adapt_me = baseline_coverage %>%
  group_by(adapt) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(adapt,starts_with("coverage"))

adapt_squared_difference = adapt_me[,-1] - b_adapt_me[,-1]

plot(as.vector(unlist(adapt_squared_difference[4,])), type = "l", ylim = c(-0.05, 0.05), ylab = "Coverage Difference")
lines(as.vector(unlist(adapt_squared_difference[1,])), col = 2)
lines(as.vector(unlist(adapt_squared_difference[3,])), col = 3)
lines(as.vector(unlist(adapt_squared_difference[2,])), col = 4)

adapt_me = linear_mse %>%
  group_by(adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(adapt,starts_with("iter"))


plot(as.vector(unlist(adapt_me[4,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE")
lines(as.vector(unlist(adapt_me[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(adapt_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(adapt_me[2,-c(1,2)])), col = 4)

b_adapt_me = baseline_results %>%
  group_by(adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(adapt,starts_with("iter"))

adapt_squared_difference = adapt_me[,c(-1)] - b_adapt_me[,-1]

plot(as.vector(unlist(adapt_squared_difference[4,])), type = "l", ylim = c(-0.05, 0.05), ylab = "MSE Difference")
lines(as.vector(unlist(adapt_squared_difference[1,])), col = 2)
lines(as.vector(unlist(adapt_squared_difference[3,])), col = 3)
lines(as.vector(unlist(adapt_squared_difference[2,])), col = 4)


################################################################################
#main effect of urn size
################################################################################
urn_size_me = post_hoc_urnsize %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("coverage"))

plot(as.vector(unlist(urn_size_me[4,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "Coverage")
lines(as.vector(unlist(urn_size_me[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_size_me[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_size_me[3,-c(1,2)])), col = 4)

b_urn_size_me = baseline_coverage %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("coverage"))

urn_size_squared_difference = urn_size_me[,-1] - b_urn_size_me[,-1]

plot(as.vector(unlist(urn_size_squared_difference[4,])), type = "l", ylim = c(-0.05, 0.05), ylab = "Coverage Difference")
lines(as.vector(unlist(urn_size_squared_difference[1,])), col = 2)
lines(as.vector(unlist(urn_size_squared_difference[2,])), col = 3)
lines(as.vector(unlist(urn_size_squared_difference[3,])), col = 4)

urn_size_me = linear_mse %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))


plot(as.vector(unlist(urn_size_me[4,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE")
lines(as.vector(unlist(urn_size_me[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_size_me[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_size_me[3,-c(1,2)])), col = 4)

b_urn_size_me = baseline_results %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

urn_size_squared_difference = urn_size_me[,c(-1,-2)] - b_urn_size_me[,-1]

plot(as.vector(unlist(urn_size_squared_difference[4,])), type = "l", ylim = c(-0.005, 0.005), ylab = "MSE Difference")
lines(as.vector(unlist(urn_size_squared_difference[1,])), col = 2)
lines(as.vector(unlist(urn_size_squared_difference[2,])), col = 3)
lines(as.vector(unlist(urn_size_squared_difference[3,])), col = 4)


################################################################################
#main effect amount of change
################################################################################
change_me = post_hoc_urnsize %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("coverage"))

plot(as.vector(unlist(change_me[1,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "Coverage")
lines(as.vector(unlist(change_me[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1,2)])), col = 5)

b_change_me = baseline_coverage %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("coverage"))

change_squared_error = change_me[, -1] - b_change_me[, -1]

plot(as.vector(unlist(change_squared_error[1,])), type = "l", ylim = c(-0.05, 0.05), ylab = "Coverage Difference")
lines(as.vector(unlist(change_squared_error[2,])), col = 2)
lines(as.vector(unlist(change_squared_error[3,])), col = 3)
lines(as.vector(unlist(change_squared_error[4,])), col = 4)
lines(as.vector(unlist(change_squared_error[5,])), col = 5)

change_me = linear_mse %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

b_change_me = baseline_results %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

change_squared_error = change_me[, -c(1,2)] - b_change_me[, -1]

plot(as.vector(unlist(change_squared_error[1,])), type = "l", ylim = c(-0.01, 0.01), ylab = "MSE Difference")
lines(as.vector(unlist(change_squared_error[2,])), col = 2)
lines(as.vector(unlist(change_squared_error[3,])), col = 3)
lines(as.vector(unlist(change_squared_error[4,])), col = 4)
lines(as.vector(unlist(change_squared_error[5,])), col = 5)

change_me = post_hoc_urnsize %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

plot(as.vector(unlist(change_me[1,-c(1,2)])), type = "l", ylim = c(0.4, 0.8), ylab = "Coverage")
lines(as.vector(unlist(change_me[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1,2)])), col = 5)

b_change_me_res = baseline_res %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

lines(as.vector(unlist(b_change_me_res[1,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[2,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[4,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[5,-c(1,2)])), col = 5, lty = "dotted")

################################################################################
################################################################################
#DIFFERENT LATENT CHANGE TYPE
################################################################################
################################################################################

################################################################################
#simulation with linear change POSITIVE
################################################################################
post_hoc_positive = readRDS("post_hoc_positive.rds")
colnames(post_hoc_positive)[3] = "amount_of_change"

################################################################################
#recreating linear change
################################################################################
change_matrix_linear = matrix(0, nrow = nrow(post_hoc_positive), ncol = 500)
change_matrix_linear[,1] = log(post_hoc_positive[,"true_value_first"] / (1-post_hoc_positive[,"true_value_first"]))
for(cl in 2:ncol(change_matrix_linear)){
  change_matrix_linear[, cl] = change_matrix_linear[, cl-1] + post_hoc_positive[, "amount_of_change"]
}
change_matrix_linear = exp(change_matrix_linear) / (1 + exp(change_matrix_linear))

################################################################################
#calculating MSE
################################################################################
linear_mse_helper = (post_hoc_positive %>% select(starts_with("iter")) - change_matrix_linear)^2

linear_mse = post_hoc_positive %>% select(-starts_with("coverage")) 
linear_mse[,6:506] = linear_mse_helper
rm(linear_mse_helper)

################################################################################
#calculating coverage
################################################################################

covered = rowMeans(post_hoc_positive %>% select(starts_with("coverage")))
post_hoc_positive = cbind(post_hoc_positive, covered)
colnames(post_hoc_positive)[1006] = "covered"

################################################################################
#calculating baseline
################################################################################
baseline_linear = readRDS("post_hoc_positive_baseline.rds")
baseline_results = baseline_linear[[1]]
baseline_coverage = baseline_linear[[2]]
rm(baseline_linear)

baseline_results = baseline_res = baseline_results / as.numeric(post_hoc_positive[,1])
baseline_results = (baseline_results - change_matrix_linear) ^ 2
baseline_results = cbind(post_hoc_positive[,1:5], baseline_results)
baseline_coverage = cbind(post_hoc_positive[,1:5], baseline_coverage)
baseline_res = cbind(post_hoc_positive[,1:5], baseline_res)
colnames(baseline_results) = c(colnames(post_hoc_positive[,1:5]), paste0("iter", c(1:500)))
colnames(baseline_coverage) = c(colnames(post_hoc_positive[,1:5]), paste0("coverage", c(1:500)))
colnames(baseline_res) = c(colnames(post_hoc_positive[,1:5]), paste0("iter", c(1:500)))

################################################################################
#main effect amount of change
################################################################################
change_me = post_hoc_positive %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("coverage"))

plot(as.vector(unlist(change_me[1,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "Coverage")
lines(as.vector(unlist(change_me[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1,2)])), col = 5)

b_change_me = baseline_coverage %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("coverage"))

change_squared_error = change_me[, -1] - b_change_me[, -1]

plot(as.vector(unlist(change_squared_error[1,])), type = "l", ylim = c(-0.05, 0.05), ylab = "Coverage Difference")
lines(as.vector(unlist(change_squared_error[2,])), col = 2)
lines(as.vector(unlist(change_squared_error[3,])), col = 3)
lines(as.vector(unlist(change_squared_error[4,])), col = 4)
lines(as.vector(unlist(change_squared_error[5,])), col = 5)

change_me = linear_mse %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

b_change_me = baseline_results %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

change_squared_error = change_me[, -c(1,2)] - b_change_me[, -1]

plot(as.vector(unlist(change_squared_error[1,])), type = "l", ylim = c(-0.01, 0.01), ylab = "MSE Difference")
lines(as.vector(unlist(change_squared_error[2,])), col = 2)
lines(as.vector(unlist(change_squared_error[3,])), col = 3)
lines(as.vector(unlist(change_squared_error[4,])), col = 4)
lines(as.vector(unlist(change_squared_error[5,])), col = 5)

change_me = post_hoc_positive %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

plot(as.vector(unlist(change_me[1,-c(1,2)])), type = "l", ylim = c(0.4, 0.8), ylab = "Coverage")
lines(as.vector(unlist(change_me[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1,2)])), col = 5)

b_change_me_res = baseline_res %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

lines(as.vector(unlist(b_change_me_res[1,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[2,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[4,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[5,-c(1,2)])), col = 5, lty = "dotted")

################################################################################
#simulation with linear change SYMMETRIC
################################################################################

post_hoc_symmetric = readRDS("post_hoc_symmetric.rds")
colnames(post_hoc_symmetric)[3] = "amount_of_change"

################################################################################
#recreating linear change
################################################################################
change_matrix_linear = matrix(0, nrow = nrow(post_hoc_symmetric), ncol = 500)
change_matrix_linear[,1] = log(post_hoc_symmetric[,"true_value_first"] / (1-post_hoc_symmetric[,"true_value_first"]))
for(cl in 2:ncol(change_matrix_linear)){
  change_matrix_linear[, cl] = change_matrix_linear[, cl-1] + post_hoc_symmetric[, "amount_of_change"]
}
change_matrix_linear = exp(change_matrix_linear) / (1 + exp(change_matrix_linear))

################################################################################
#calculating MSE
################################################################################
linear_mse_helper = (post_hoc_symmetric %>% select(starts_with("iter")) - change_matrix_linear)^2

linear_mse = post_hoc_symmetric %>% select(-starts_with("coverage")) 
linear_mse[,6:506] = linear_mse_helper
rm(linear_mse_helper)

################################################################################
#calculating coverage
################################################################################

covered = rowMeans(post_hoc_symmetric %>% select(starts_with("coverage")))
post_hoc_symmetric = cbind(post_hoc_symmetric, covered)
colnames(post_hoc_symmetric)[1006] = "covered"

################################################################################
#calculating baseline
################################################################################
baseline_linear = readRDS("post_hoc_symmetric_baseline.rds")
baseline_results = baseline_linear[[1]]
baseline_coverage = baseline_linear[[2]]
rm(baseline_linear)

baseline_results = baseline_res = baseline_results / as.numeric(post_hoc_symmetric[,1])
baseline_results = (baseline_results - change_matrix_linear) ^ 2
baseline_results = cbind(post_hoc_symmetric[,1:5], baseline_results)
baseline_coverage = cbind(post_hoc_symmetric[,1:5], baseline_coverage)
baseline_res = cbind(post_hoc_symmetric[,1:5], baseline_res)
colnames(baseline_results) = c(colnames(post_hoc_symmetric[,1:5]), paste0("iter", c(1:500)))
colnames(baseline_coverage) = c(colnames(post_hoc_symmetric[,1:5]), paste0("coverage", c(1:500)))
colnames(baseline_res) = c(colnames(post_hoc_symmetric[,1:5]), paste0("iter", c(1:500)))

################################################################################
#main effect amount of change
################################################################################
change_me = post_hoc_symmetric %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("coverage"))

plot(as.vector(unlist(change_me[1,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "Coverage")
lines(as.vector(unlist(change_me[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1,2)])), col = 5)

b_change_me = baseline_coverage %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("coverage"))

change_squared_error = change_me[, -1] - b_change_me[, -1]

plot(as.vector(unlist(change_squared_error[1,])), type = "l", ylim = c(-0.05, 0.05), ylab = "Coverage Difference")
lines(as.vector(unlist(change_squared_error[2,])), col = 2)
lines(as.vector(unlist(change_squared_error[3,])), col = 3)
lines(as.vector(unlist(change_squared_error[4,])), col = 4)
lines(as.vector(unlist(change_squared_error[5,])), col = 5)

change_me = linear_mse %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

b_change_me = baseline_results %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

change_squared_error = change_me[, -c(1,2)] - b_change_me[, -1]

plot(as.vector(unlist(change_squared_error[1,])), type = "l", ylim = c(-0.01, 0.01), ylab = "MSE Difference")
lines(as.vector(unlist(change_squared_error[2,])), col = 2)
lines(as.vector(unlist(change_squared_error[3,])), col = 3)
lines(as.vector(unlist(change_squared_error[4,])), col = 4)
lines(as.vector(unlist(change_squared_error[5,])), col = 5)

change_me = post_hoc_symmetric %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

plot(as.vector(unlist(change_me[1,-c(1,2)])), type = "l", ylim = c(0.2, 0.8), ylab = "Coverage")
lines(as.vector(unlist(change_me[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1,2)])), col = 5)

b_change_me_res = baseline_res %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

lines(as.vector(unlist(b_change_me_res[1,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[2,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[4,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[5,-c(1,2)])), col = 5, lty = "dotted")


################################################################################
#simulation with linear change MIRRORRED
################################################################################

post_hoc_mirrorred = readRDS("post_hoc_mirrorred.rds")
colnames(post_hoc_mirrorred)[3] = "amount_of_change"

################################################################################
#recreating linear change
################################################################################
change_matrix_linear = matrix(0, nrow = nrow(post_hoc_mirrorred), ncol = 500)
change_matrix_linear[,1] = log(post_hoc_mirrorred[,"true_value_first"] / (1-post_hoc_mirrorred[,"true_value_first"]))
for(cl in 2:ncol(change_matrix_linear)){
  change_matrix_linear[, cl] = change_matrix_linear[, cl-1] + post_hoc_mirrorred[, "amount_of_change"]
}
change_matrix_linear = exp(change_matrix_linear) / (1 + exp(change_matrix_linear))

################################################################################
#calculating MSE
################################################################################
linear_mse_helper = (post_hoc_mirrorred %>% select(starts_with("iter")) - change_matrix_linear)^2

linear_mse = post_hoc_mirrorred %>% select(-starts_with("coverage")) 
linear_mse[,6:506] = linear_mse_helper
rm(linear_mse_helper)

################################################################################
#calculating coverage
################################################################################

covered = rowMeans(post_hoc_mirrorred %>% select(starts_with("coverage")))
post_hoc_mirrorred = cbind(post_hoc_mirrorred, covered)
colnames(post_hoc_mirrorred)[1006] = "covered"

################################################################################
#calculating baseline
################################################################################
baseline_linear = readRDS("post_hoc_mirrorred_baseline.rds")
baseline_results = baseline_linear[[1]]
baseline_coverage = baseline_linear[[2]]
rm(baseline_linear)

baseline_results = baseline_res = baseline_results / as.numeric(post_hoc_mirrorred[,1])
baseline_results = (baseline_results - change_matrix_linear) ^ 2
baseline_results = cbind(post_hoc_mirrorred[,1:5], baseline_results)
baseline_coverage = cbind(post_hoc_mirrorred[,1:5], baseline_coverage)
baseline_res = cbind(post_hoc_mirrorred[,1:5], baseline_res)
colnames(baseline_results) = c(colnames(post_hoc_mirrorred[,1:5]), paste0("iter", c(1:500)))
colnames(baseline_coverage) = c(colnames(post_hoc_mirrorred[,1:5]), paste0("coverage", c(1:500)))
colnames(baseline_res) = c(colnames(post_hoc_mirrorred[,1:5]), paste0("iter", c(1:500)))

################################################################################
#main effect amount of change
################################################################################
change_me = post_hoc_mirrorred %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("coverage"))

plot(as.vector(unlist(change_me[1,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "Coverage")
lines(as.vector(unlist(change_me[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1,2)])), col = 5)

b_change_me = baseline_coverage %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("coverage"))

change_squared_error = change_me[, -1] - b_change_me[, -1]

plot(as.vector(unlist(change_squared_error[1,])), type = "l", ylim = c(-0.05, 0.05), ylab = "Coverage Difference")
lines(as.vector(unlist(change_squared_error[2,])), col = 2)
lines(as.vector(unlist(change_squared_error[3,])), col = 3)
lines(as.vector(unlist(change_squared_error[4,])), col = 4)
lines(as.vector(unlist(change_squared_error[5,])), col = 5)

change_me = linear_mse %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

b_change_me = baseline_results %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

change_squared_error = change_me[, -c(1,2)] - b_change_me[, -1]

plot(as.vector(unlist(change_squared_error[1,])), type = "l", ylim = c(-0.01, 0.01), ylab = "MSE Difference")
lines(as.vector(unlist(change_squared_error[2,])), col = 2)
lines(as.vector(unlist(change_squared_error[3,])), col = 3)
lines(as.vector(unlist(change_squared_error[4,])), col = 4)
lines(as.vector(unlist(change_squared_error[5,])), col = 5)

change_me = post_hoc_mirrorred %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

plot(as.vector(unlist(change_me[1,-c(1,2)])), type = "l", ylim = c(0.2, 0.8), ylab = "Coverage")
lines(as.vector(unlist(change_me[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1,2)])), col = 5)

b_change_me_res = baseline_res %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

lines(as.vector(unlist(b_change_me_res[1,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[2,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[4,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[5,-c(1,2)])), col = 5, lty = "dotted")
