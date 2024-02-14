library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(png)
library(grid)

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
change_matrix_linear[,1] = log(sim2_linear[,"true_value_first"] / (1-sim2_linear[,"true_value_first"])) + sim2_linear[, "amount_of_change"]
for(cl in 2:ncol(change_matrix_linear)){
  change_matrix_linear[, cl] = change_matrix_linear[, cl-1] + sim2_linear[, "amount_of_change"]
}
change_matrix_linear = exp(change_matrix_linear) / (1 + exp(change_matrix_linear))

################################################################################
#calculating MSE
################################################################################
linear_mse_helper = (sim2_linear %>% select(starts_with("iter")) - change_matrix_linear)^2

linear_mse = sim2_linear %>% select(-starts_with("coverage")) 
linear_mse[,7:506] = linear_mse_helper
rm(linear_mse_helper)

################################################################################
#calculating coverage
################################################################################
covered = rowMeans(sim2_linear %>% select(starts_with("coverage")))
sim2_linear = cbind(sim2_linear, covered)
colnames(sim2_linear)[1007] = "covered"

################################################################################
#calculating baseline
################################################################################
baseline_linear = readRDS("sim2_baseline.rds")
baseline_results = baseline_linear[[1]]
baseline_coverage = baseline_linear[[2]]
rm(baseline_linear)

baseline_results = baseline_res = baseline_results / as.numeric(sim2_linear[,2])
baseline_results = (baseline_results - change_matrix_linear) ^ 2
baseline_results = cbind(sim2_linear[,1:6], baseline_results)
baseline_coverage = cbind(sim2_linear[,1:6], baseline_coverage)
baseline_res = cbind(sim2_linear[,1:6], baseline_res)
colnames(baseline_results) = c(colnames(sim2_linear[,1:6]), paste0("iter", c(1:500)))
colnames(baseline_coverage) = c(colnames(sim2_linear[,1:6]), paste0("coverage", c(1:500)))
colnames(baseline_res) = c(colnames(sim2_linear[,1:6]), paste0("iter", c(1:500)))

change_matrix_avg = cbind(sim2_linear[,1:6], change_matrix_linear)
colnames(change_matrix_avg) = c(colnames(sim2_linear[,1:6]), paste0("iter", c(1:500)))
################################################################################
#main effect of adaptivity
################################################################################
adapt_me = sim2_linear %>%
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

adapt_squared_difference = adapt_me[,-1] - b_adapt_me[,-1]

plot(as.vector(unlist(adapt_squared_difference[4,])), type = "l", ylim = c(-0.05, 0.05), ylab = "MSE Difference")
lines(as.vector(unlist(adapt_squared_difference[1,])), col = 2)
lines(as.vector(unlist(adapt_squared_difference[3,])), col = 3)
lines(as.vector(unlist(adapt_squared_difference[2,])), col = 4)


################################################################################
#main effect of urn size
################################################################################
urn_size_me = sim2_linear %>%
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

urn_size_squared_difference = urn_size_me[,-1] - b_urn_size_me[,-1]

plot(as.vector(unlist(urn_size_squared_difference[4,])), type = "l", ylim = c(-0.005, 0.005), ylab = "MSE Difference")
lines(as.vector(unlist(urn_size_squared_difference[1,])), col = 2)
lines(as.vector(unlist(urn_size_squared_difference[2,])), col = 3)
lines(as.vector(unlist(urn_size_squared_difference[3,])), col = 4)

one_slice = linear_mse[linear_mse[,2] == 8, 200]
samples = numeric(20000)
for(i in 1:20000){
  samples[i] = mean(sample(one_slice, 500, replace = FALSE))
}

hist(samples)
t.test(samples, mu = 0)$conf.int[2]- t.test(samples, mu = 0)$conf.int[1]

for(i in 1:500){
  one_slice = sim2_linear[sim2_linear[,2] == 8, 600]
  samples = numeric(2000)
  for(i in 1:2000){
    samples[i] = mean(sample(one_slice, 500, replace = FALSE))
  }
}

hist(samples)
t.test(samples, mu = 0)$conf.int[2]- t.test(samples, mu = 0)$conf.int[1]

################################################################################
#main effect amount of change
################################################################################
change_me = sim2_linear %>%
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

change_squared_error = change_me[, -1] - b_change_me[, -1]

plot(as.vector(unlist(change_squared_error[1,])), type = "l", ylim = c(-0.01, 0.01), ylab = "MSE Difference")
lines(as.vector(unlist(change_squared_error[2,])), col = 2)
lines(as.vector(unlist(change_squared_error[3,])), col = 3)
lines(as.vector(unlist(change_squared_error[4,])), col = 4)
lines(as.vector(unlist(change_squared_error[5,])), col = 5)

# this is the analysis we are looking for
change_me = sim2_linear %>%
  group_by(amount_of_change) %>%
  filter(dist_type == "better") %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

plot(as.vector(unlist(change_me[1,-c(1,2)][1:100])), type = "l", ylim = c(0.45, 0.55), ylab = "Coverage")
lines(as.vector(unlist(change_me[2,-c(1,2)][1:100])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1,2)][1:100])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1,2)][1:100])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1,2)][1:100])), col = 5)

b_change_me_res = baseline_res %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

true_change_me = change_matrix_avg %>%
  group_by(amount_of_change) %>%
  filter(dist_type == "better") %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))


lines(as.vector(unlist(b_change_me_res[1,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[2,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[4,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(b_change_me_res[5,-c(1,2)])), col = 5, lty = "dotted")

lines(as.vector(unlist(true_change_me[1,-c(1,2)][1:100])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_change_me[2,-c(1,2)][1:100])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_change_me[3,-c(1,2)][1:100])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_change_me[4,-c(1,2)][1:100])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_change_me[5,-c(1,2)][1:100])), col = 5, lty = "dotted")

################################################################################
#main effect of distribution type
################################################################################
dist_me = sim2_linear %>%
  group_by(dist_type) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(dist_type,starts_with("coverage"))

plot(as.vector(unlist(dist_me[2,-c(1)])), type = "l", ylim = c(0.9, 1), ylab = "Coverage")
lines(as.vector(unlist(dist_me[1,-c(1)])), col = 2)
lines(as.vector(unlist(dist_me[3,-c(1)])), col = 3)

b_dist_me = baseline_coverage %>%
  group_by(dist_type) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(dist_type,starts_with("coverage"))

dist_squared_error = dist_me[, -1] - b_dist_me[, -1]

plot(as.vector(unlist(dist_squared_error[2,-c(1)])), type = "l", ylim = c(-0.05, 0.05), ylab = "Coverage")
lines(as.vector(unlist(dist_squared_error[1,-c(1)])), col = 2)
lines(as.vector(unlist(dist_squared_error[3,-c(1)])), col = 3)

dist_me = linear_mse %>%
  group_by(dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type,starts_with("iter"))

plot(as.vector(unlist(dist_me[2,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE")
lines(as.vector(unlist(dist_me[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(dist_me[3,-c(1,2)])), col = 3)

b_dist_me = baseline_results %>%
  group_by(dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type,starts_with("iter"))

dist_squared_error = dist_me[, -1] - b_dist_me[, -1]

plot(as.vector(unlist(dist_squared_error[2,-c(1)])), type = "l", ylim = c(-0.05, 0.05), ylab = "Coverage")
lines(as.vector(unlist(dist_squared_error[1,-c(1)])), col = 2)
lines(as.vector(unlist(dist_squared_error[3,-c(1)])), col = 3)


################################################################################
#urn sizes and change amount
################################################################################
urn_sizeXchange = sim2_linear %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(covered, ~ mean(.))) %>%
  select(player_urn_size, amount_of_change, covered)

urn_sizeXchange_mat = matrix(0, 4, 5)
rownames(urn_sizeXchange_mat) = c("8", "16", "32", "64")
colnames(urn_sizeXchange_mat) = c("-0.001", "0", "0.0005", "0.001", "0.002")
urn_sizeXchange = as.data.frame(urn_sizeXchange)

urn_sizes = c("8", "16", "32", "64")
changes = c(-0.001, 0, 0.0005, 0.001, 0.002)

for(i in 1:length(rownames(urn_sizeXchange_mat))){
  for(j in 1:length(colnames(urn_sizeXchange_mat))){
    urn_sizeXchange_mat[i,j] = urn_sizeXchange[urn_sizeXchange[,1] == urn_sizes[i] & urn_sizeXchange[,2] == changes[j], 3]
  }
}

col_fun = colorRamp2(c(min(unlist(urn_sizeXchange_mat)), mean(unlist(urn_sizeXchange_mat)), max(unlist(urn_sizeXchange_mat))), c("red", "white",  "green"))
cell_fun = function(data, j, i, x, y, width, height, fill) {
  grid.text(sprintf("%.1f", data[i,j]), x, y, gp = gpar(fontsize = 10))}

h1 = Heatmap(urn_sizeXchange_mat, cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Coverage"),
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.2f", urn_sizeXchange_mat[i,j]), x, y, gp = gpar(fontsize = 10))
             })
h1

#MSE based results
urn_sizeXchange = linear_mse %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change, player_urn_size,starts_with("iter")) %>%
  arrange(amount_of_change)

plot(as.vector(unlist(urn_sizeXchange[4,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = -0.001")
lines(as.vector(unlist(urn_sizeXchange[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[3,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[8,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = 0")
lines(as.vector(unlist(urn_sizeXchange[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[6,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[7,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[12,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = 0.0005")
lines(as.vector(unlist(urn_sizeXchange[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[11,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[16,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = 0.001")
lines(as.vector(unlist(urn_sizeXchange[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[14,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[15,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[20,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = 0.002")
lines(as.vector(unlist(urn_sizeXchange[17,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[18,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[19,-c(1,2)])), col = 4)

b_urn_sizeXchange = baseline_results %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change, player_urn_size,starts_with("iter")) %>%
  arrange(amount_of_change)

urn_sizeXchange_squared_error = urn_sizeXchange[,-c(1,2)] - b_urn_sizeXchange[,-c(1,2)]

plot(as.vector(unlist(urn_sizeXchange_squared_error[4,-c(1,2)])), type = "l", ylim = c(-0.01, 0.01), ylab = "MSE Difference", main = "Change = -0.001")
lines(as.vector(unlist(urn_sizeXchange_squared_error[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange_squared_error[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange_squared_error[3,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange_squared_error[8,-c(1,2)])), type = "l", ylim = c(-0.01, 0.01), ylab = "MSE Difference", main = "Change = 0")
lines(as.vector(unlist(urn_sizeXchange_squared_error[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange_squared_error[6,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange_squared_error[7,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange_squared_error[12,-c(1,2)])), type = "l", ylim = c(-0.01, 0.01), ylab = "MSE Difference", main = "Change = 0.0005")
lines(as.vector(unlist(urn_sizeXchange_squared_error[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange_squared_error[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange_squared_error[11,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange_squared_error[16,-c(1,2)])), type = "l", ylim = c(-0.01, 0.01), ylab = "MSE Difference", main = "Change = 0.001")
lines(as.vector(unlist(urn_sizeXchange_squared_error[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange_squared_error[14,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange_squared_error[15,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange_squared_error[20,-c(1,2)])), type = "l", ylim = c(-0.005, 0.005), ylab = "MSE Difference", col = rgb(0,0,0, alpha = 0.7))
lines(as.vector(unlist(urn_sizeXchange_squared_error[17,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange_squared_error[18,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange_squared_error[19,-c(1,2)])), col = 4)
lines(rep(0,times = 500), col = 1, lty = "dotted")



#cumulative coverage based results
urn_sizeXchange = sim2_linear %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change, player_urn_size,starts_with("coverage")) %>%
  arrange(amount_of_change)

plot(as.vector(unlist(urn_sizeXchange[4,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "MSE", main = "Change = -0.001")
lines(as.vector(unlist(urn_sizeXchange[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[3,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[8,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "MSE", main = "Change = 0")
lines(as.vector(unlist(urn_sizeXchange[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[6,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[7,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[12,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "MSE", main = "Change = 0.0005")
lines(as.vector(unlist(urn_sizeXchange[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[11,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[16,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "MSE", main = "Change = 0.001")
lines(as.vector(unlist(urn_sizeXchange[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[14,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[15,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[20,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "MSE", main = "Change = 0.002")
lines(as.vector(unlist(urn_sizeXchange[17,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[18,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[19,-c(1,2)])), col = 4)

b_urn_sizeXchange = baseline_coverage %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change, player_urn_size,starts_with("coverage")) %>%
  arrange(amount_of_change)

urn_sizeXchange_squared_error = urn_sizeXchange[,-c(1,2)] - b_urn_sizeXchange[,-c(1,2)]

###difference
plot(as.vector(unlist(urn_sizeXchange_squared_error[4,])), type = "l", ylim = c(-0.05, 0.05), ylab = "Coverage Difference")
lines(as.vector(unlist(urn_sizeXchange_squared_error[1,])), col = 2)
lines(as.vector(unlist(urn_sizeXchange_squared_error[2,])), col = 3)
lines(as.vector(unlist(urn_sizeXchange_squared_error[3,])), col = 4)

plot(as.vector(unlist(urn_sizeXchange_squared_error[8,])), type = "l", ylim = c(-0.05, 0.05), ylab = "MSE", main = "Change = 0")
lines(as.vector(unlist(urn_sizeXchange_squared_error[5,])), col = 2)
lines(as.vector(unlist(urn_sizeXchange_squared_error[6,])), col = 3)
lines(as.vector(unlist(urn_sizeXchange_squared_error[7,])), col = 4)

plot(as.vector(unlist(urn_sizeXchange_squared_error[12,])), type = "l", ylim = c(-0.05, 0.05), ylab = "MSE", main = "Change = 0.0005")
lines(as.vector(unlist(urn_sizeXchange_squared_error[9,])), col = 2)
lines(as.vector(unlist(urn_sizeXchange_squared_error[10,])), col = 3)
lines(as.vector(unlist(urn_sizeXchange_squared_error[11,])), col = 4)

plot(as.vector(unlist(urn_sizeXchange_squared_error[16,])), type = "l", ylim = c(-0.05, 0.05), ylab = "MSE", main = "Change = 0.001")
lines(as.vector(unlist(urn_sizeXchange_squared_error[13,])), col = 2)
lines(as.vector(unlist(urn_sizeXchange_squared_error[14,])), col = 3)
lines(as.vector(unlist(urn_sizeXchange_squared_error[15,])), col = 4)

plot(as.vector(unlist(urn_sizeXchange_squared_error[20,])), type = "l", ylim = c(-0.05, 0.05), ylab = "Coverage Difference")
lines(as.vector(unlist(urn_sizeXchange_squared_error[17,])), col = 2)
lines(as.vector(unlist(urn_sizeXchange_squared_error[18,])), col = 3)
lines(as.vector(unlist(urn_sizeXchange_squared_error[19,])), col = 4)


################################################################################
#urn sizes and change amount per adaptivity
################################################################################
urn_sizeXchangeXadapt = sim2_linear %>%
  group_by(player_urn_size, amount_of_change, adapt) %>%
  summarise(across(covered, ~ mean(.))) %>%
  select(adapt, player_urn_size, amount_of_change, covered)

adapt = c("adaptive50", "adaptive70", "adaptive_sigma", "n_adaptive")

mat_list = list()
for(a in 1:length(adapt)){
  mat_list[[a]] = matrix(0, 4, 5)
  rownames(mat_list[[a]]) = c("8", "16", "32", "64")
  colnames(mat_list[[a]]) = c("-0.001", "0", "0.0005", "0.001", "0.002")
  mat_list[[a]] = as.data.frame(mat_list[[a]])
  for(i in 1:length(rownames(mat_list[[a]]))){
    for(j in 1:length(colnames(mat_list[[a]]))){
      mat_list[[a]][i,j] = urn_sizeXchangeXadapt[urn_sizeXchangeXadapt[,1] == adapt[a] & 
                                                   urn_sizeXchangeXadapt[,2] == urn_sizes[i] &
                                                   urn_sizeXchangeXadapt[,3] == changes[j], 4]
    }
  }
}


col_fun = colorRamp2(c(min(unlist(mat_list)), mean(unlist(mat_list)), max(unlist(mat_list))), c("red", "white",  "green"))
h_adapt_50 = Heatmap(as.matrix(mat_list[[1]]), cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Coverage"),
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.2f", as.matrix(mat_list[[1]])[i,j]), x, y, gp = gpar(fontsize = 10))
             })
h_adapt_50

h_adapt_70 = Heatmap(as.matrix(mat_list[[2]]), cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Coverage"),
                     cell_fun = function(j, i, x, y, width, height, fill) {
                       grid.text(sprintf("%.2f", as.matrix(mat_list[[2]])[i,j]), x, y, gp = gpar(fontsize = 10))
                     })
h_adapt_70

h_adapt_sigma = Heatmap(as.matrix(mat_list[[3]]), cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Coverage"),
                     cell_fun = function(j, i, x, y, width, height, fill) {
                       grid.text(sprintf("%.2f", as.matrix(mat_list[[3]])[i,j]), x, y, gp = gpar(fontsize = 10))
                     })
h_adapt_sigma

h_n_adaptive = Heatmap(as.matrix(mat_list[[4]]), cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Coverage"),
                        cell_fun = function(j, i, x, y, width, height, fill) {
                          grid.text(sprintf("%.2f", as.matrix(mat_list[[4]])[i,j]), x, y, gp = gpar(fontsize = 10))
                        })
h_n_adaptive

#MSE based results
urn_sizeXchange = linear_mse %>%
  filter(adapt == "adaptive70") %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change, player_urn_size,starts_with("iter")) %>%
  arrange(amount_of_change)

plot(as.vector(unlist(urn_sizeXchange[4,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = -0.001")
lines(as.vector(unlist(urn_sizeXchange[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[3,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[8,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = 0")
lines(as.vector(unlist(urn_sizeXchange[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[6,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[7,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[12,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = 0.0005")
lines(as.vector(unlist(urn_sizeXchange[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[11,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[16,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = 0.001")
lines(as.vector(unlist(urn_sizeXchange[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[14,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[15,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[20,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = 0.002")
lines(as.vector(unlist(urn_sizeXchange[17,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[18,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[19,-c(1,2)])), col = 4)

#cumulative coverage based results
urn_sizeXchange = sim2_linear %>%
  filter(adapt == "adaptive_sigma", ) %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change, player_urn_size,starts_with("coverage")) %>%
  arrange(amount_of_change)

plot(as.vector(unlist(urn_sizeXchange[4,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "MSE", main = "Change = -0.001")
lines(as.vector(unlist(urn_sizeXchange[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[3,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[8,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "MSE", main = "Change = 0")
lines(as.vector(unlist(urn_sizeXchange[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[6,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[7,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[12,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "MSE", main = "Change = 0.0005")
lines(as.vector(unlist(urn_sizeXchange[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[11,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[16,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "MSE", main = "Change = 0.001")
lines(as.vector(unlist(urn_sizeXchange[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[14,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[15,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[20,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "MSE", main = "Change = 0.002")
lines(as.vector(unlist(urn_sizeXchange[17,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[18,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[19,-c(1,2)])), col = 4)

################################################################################
#adaptivity and distribution type
################################################################################
#cumulative coverage based results
adaptXdist_type = sim2_linear %>%
  group_by(adapt, dist_type) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(dist_type, adapt, starts_with("coverage")) %>%
  arrange(dist_type)

plot(as.vector(unlist(adaptXdist_type[4,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "MSE", main = "Better")
lines(as.vector(unlist(adaptXdist_type[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(adaptXdist_type[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(adaptXdist_type[3,-c(1,2)])), col = 4)

plot(as.vector(unlist(adaptXdist_type[8,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "MSE", main = "Central")
lines(as.vector(unlist(adaptXdist_type[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(adaptXdist_type[6,-c(1,2)])), col = 3)
lines(as.vector(unlist(adaptXdist_type[7,-c(1,2)])), col = 4)

plot(as.vector(unlist(adaptXdist_type[12,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "MSE", main = "Worse")
lines(as.vector(unlist(adaptXdist_type[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(adaptXdist_type[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(adaptXdist_type[11,-c(1,2)])), col = 4)

b_adaptXdist_type = baseline_coverage %>%
  group_by(adapt, dist_type) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(dist_type, adapt, starts_with("coverage")) %>%
  arrange(dist_type)

adaptXdist_type_error = adaptXdist_type[, -c(1,2)] -  b_adaptXdist_type[, -c(1,2)]

plot(as.vector(unlist(adaptXdist_type_error[12,-c(1,2)])), type = "l", ylim = c(-0.05, 0.05), ylab = "MSE")
lines(as.vector(unlist(adaptXdist_type_error[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(adaptXdist_type_error[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(adaptXdist_type_error[11,-c(1,2)])), col = 4)

#mse based results
adaptXdist_type = linear_mse %>%
  group_by(adapt, dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, adapt, starts_with("iter")) %>%
  arrange(dist_type)

plot(as.vector(unlist(adaptXdist_type[4,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Better")
lines(as.vector(unlist(adaptXdist_type[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(adaptXdist_type[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(adaptXdist_type[3,-c(1,2)])), col = 4)

plot(as.vector(unlist(adaptXdist_type[8,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Central")
lines(as.vector(unlist(adaptXdist_type[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(adaptXdist_type[6,-c(1,2)])), col = 3)
lines(as.vector(unlist(adaptXdist_type[7,-c(1,2)])), col = 4)

plot(as.vector(unlist(adaptXdist_type[12,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Worse")
lines(as.vector(unlist(adaptXdist_type[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(adaptXdist_type[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(adaptXdist_type[11,-c(1,2)])), col = 4)


################################################################################
################################################################################
################################################################################
################################################################################

################################################################################
#simulation with discrete change
################################################################################
sim2_discrete_better = readRDS("sim2_discrete_1jump_better.rds")
sim2_discrete_worse = readRDS("sim2_discrete_1jump_central.rds")
sim2_discrete_central = readRDS("sim2_discrete_1jump_worse.rds")

better_label = data.frame("dist_type" = rep("better", times = nrow(sim2_discrete_better)))
worse_label = data.frame("dist_type" = rep("worse", times = nrow(sim2_discrete_worse)))
central_label = data.frame("dist_type" = rep("central", times = nrow(sim2_discrete_central)))

sim2_discrete_better = cbind(better_label, sim2_discrete_better)
sim2_discrete_worse = cbind(worse_label, sim2_discrete_worse)
sim2_discrete_central = cbind(central_label, sim2_discrete_central)

rm(better_label, worse_label, central_label)

sim2_discrete = rbind(sim2_discrete_better, sim2_discrete_central, sim2_discrete_worse)
rm(sim2_discrete_better, sim2_discrete_central, sim2_discrete_worse)
colnames(sim2_discrete)[4] = "amount_of_change"

#recreating change
change_matrix_1jump_first = matrix(rep(sim2_discrete[,"true_value_first"], 249), nrow = nrow(sim2_discrete), ncol = 249)
change_matrix_1jump_second = log(change_matrix_1jump_first / (1-change_matrix_1jump_first)) + sim2_discrete[,"amount_of_change"]
change_matrix_1jump_second = exp(change_matrix_1jump_second) / (1 + exp(change_matrix_1jump_second))
change_matrix_1jump = cbind(change_matrix_1jump_first, change_matrix_1jump_second)
rm(change_matrix_1jump_first, change_matrix_1jump_second)

################################################################################
#calculating MSE
################################################################################
discrete_mse_helper = (sim2_discrete %>% select(starts_with("iter")) - change_matrix_1jump)^2

discrete_mse = sim2_discrete %>% select(-starts_with("coverage")) 
discrete_mse[,7:506] = discrete_mse_helper
rm(discrete_mse_helper)

################################################################################
#calculating coverage
################################################################################
covered = rowMeans(sim2_discrete %>% select(starts_with("coverage")))
sim2_discrete = cbind(sim2_discrete, covered)
colnames(sim2_discrete)[1007] = "covered"


################################################################################
#main effect of adaptivity
################################################################################
adapt_me = sim2_discrete %>%
  group_by(adapt) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(adapt,starts_with("coverage"))

plot(as.vector(unlist(adapt_me[4,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "Coverage")
lines(as.vector(unlist(adapt_me[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(adapt_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(adapt_me[2,-c(1,2)])), col = 4)


adapt_me = discrete_mse %>%
  group_by(adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(adapt,starts_with("iter"))


plot(as.vector(unlist(adapt_me[4,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE")
lines(as.vector(unlist(adapt_me[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(adapt_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(adapt_me[2,-c(1,2)])), col = 4)

################################################################################
#main effect of urn size
################################################################################
urn_size_me = sim2_discrete %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("coverage"))

plot(as.vector(unlist(urn_size_me[4,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "Coverage")
lines(as.vector(unlist(urn_size_me[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_size_me[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_size_me[3,-c(1,2)])), col = 4)


urn_size_me = discrete_mse %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))


plot(as.vector(unlist(urn_size_me[4,-c(1,2)])), type = "l", ylim = c(0, 0.04), ylab = "MSE")
lines(as.vector(unlist(urn_size_me[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_size_me[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_size_me[3,-c(1,2)])), col = 4)

################################################################################
#main effect of amount of change
################################################################################
change_me = sim2_discrete %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("coverage"))

plot(as.vector(unlist(change_me[1,-c(1,2)])), type = "l", ylim = c(0.9, 1), ylab = "Coverage")
lines(as.vector(unlist(change_me[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1,2)])), col = 5)


change_me = discrete_mse %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))


plot(as.vector(unlist(change_me[1,-c(1,2)])), type = "l", ylim = c(0, 0.05), ylab = "MSE")
lines(as.vector(unlist(change_me[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1,2)])), col = 5)

################################################################################
#main effect of distribution type
################################################################################
dist_me = sim2_discrete %>%
  group_by(dist_type) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(dist_type,starts_with("coverage"))

plot(as.vector(unlist(dist_me[2,-c(1)])), type = "l", ylim = c(0.9, 1), ylab = "Coverage")
lines(as.vector(unlist(dist_me[1,-c(1)])), col = 2)
lines(as.vector(unlist(dist_me[3,-c(1)])), col = 3)


dist_me = discrete_mse %>%
  group_by(dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type,starts_with("iter"))

plot(as.vector(unlist(dist_me[2,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE")
lines(as.vector(unlist(dist_me[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(dist_me[3,-c(1,2)])), col = 3)

################################################################################
#urn sizes and change amount
################################################################################
urn_sizeXchange = sim2_discrete %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(covered, ~ mean(.))) %>%
  select(player_urn_size, amount_of_change, covered)

urn_sizeXchange_mat = matrix(0, 4, 5)
rownames(urn_sizeXchange_mat) = c("8", "16", "32", "64")
colnames(urn_sizeXchange_mat) = c("-0.001", "0", "0.0005", "0.001", "0.002")
urn_sizeXchange = as.data.frame(urn_sizeXchange)

urn_sizes = c("8", "16", "32", "64")
changes = c(-0.5, 0, 0.25, 0.5, 1)

for(i in 1:length(rownames(urn_sizeXchange_mat))){
  for(j in 1:length(colnames(urn_sizeXchange_mat))){
    urn_sizeXchange_mat[i,j] = urn_sizeXchange[urn_sizeXchange[,1] == urn_sizes[i] & urn_sizeXchange[,2] == changes[j], 3]
  }
}

col_fun = colorRamp2(c(min(unlist(urn_sizeXchange_mat)), mean(unlist(urn_sizeXchange_mat)), max(unlist(urn_sizeXchange_mat))), c("red", "white",  "green"))
cell_fun = function(data, j, i, x, y, width, height, fill) {
  grid.text(sprintf("%.1f", data[i,j]), x, y, gp = gpar(fontsize = 10))}

h1 = Heatmap(urn_sizeXchange_mat, cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Coverage"),
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.2f", urn_sizeXchange_mat[i,j]), x, y, gp = gpar(fontsize = 10))
             })
h1

#MSE based results
urn_sizeXchange = discrete_mse %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change, player_urn_size,starts_with("iter")) %>%
  arrange(amount_of_change)

plot(as.vector(unlist(urn_sizeXchange[4,-c(1,2,502)])), type = "l", ylim = c(0, 0.08), ylab = "MSE", main = "Change = -0.001")
lines(as.vector(unlist(urn_sizeXchange[1,-c(1,2,502)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[2,-c(1,2,502)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[3,-c(1,2,502)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[8,-c(1,2,502)])), type = "l", ylim = c(0, 0.08), ylab = "MSE", main = "Change = 0")
lines(as.vector(unlist(urn_sizeXchange[5,-c(1,2,502)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[6,-c(1,2,502)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[7,-c(1,2,502)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[12,-c(1,2,502)])), type = "l", ylim = c(0, 0.08), ylab = "MSE", main = "Change = 0.0005")
lines(as.vector(unlist(urn_sizeXchange[9,-c(1,2,502)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[10,-c(1,2,502)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[11,-c(1,2,502)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[16,-c(1,2,502)])), type = "l", ylim = c(0, 0.08), ylab = "MSE", main = "Change = 0.001")
lines(as.vector(unlist(urn_sizeXchange[13,-c(1,2,502)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[14,-c(1,2,502)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[15,-c(1,2,502)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[20,-c(1,2,501:502)])), type = "l", ylim = c(0, 0.08), ylab = "MSE")
lines(as.vector(unlist(urn_sizeXchange[17,-c(1,2,501:502)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[18,-c(1,2,501:502)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[19,-c(1,2,501:502)])), col = 4)

urn_sizeXchange[1,494:502]
#cumulative coverage based results
urn_sizeXchange = sim2_discrete %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change, player_urn_size,starts_with("coverage")) %>%
  arrange(amount_of_change)

plot(as.vector(unlist(urn_sizeXchange[4,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "MSE", main = "Change = -0.001")
lines(as.vector(unlist(urn_sizeXchange[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[3,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[8,-c(1,2)])), type = "l", ylim = c(0.8, 1), ylab = "MSE", main = "Change = 0")
lines(as.vector(unlist(urn_sizeXchange[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[6,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[7,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[12,-c(1,2)])), type = "l", ylim = c(0.8, 1), ylab = "MSE", main = "Change = 0.0005")
lines(as.vector(unlist(urn_sizeXchange[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[11,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[16,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "MSE", main = "Change = 0.001")
lines(as.vector(unlist(urn_sizeXchange[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[14,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[15,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[20,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "Coverage")
lines(as.vector(unlist(urn_sizeXchange[17,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[18,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[19,-c(1,2)])), col = 4)

################################################################################
#urn sizes and change amount per adaptivity
################################################################################
urn_sizeXchangeXadapt = sim2_discrete %>%
  group_by(player_urn_size, amount_of_change, adapt) %>%
  summarise(across(covered, ~ mean(.))) %>%
  select(adapt, player_urn_size, amount_of_change, covered)

adapt = c("adaptive50", "adaptive70", "adaptive_sigma", "n_adaptive")

mat_list = list()
for(a in 1:length(adapt)){
  mat_list[[a]] = matrix(0, 4, 5)
  rownames(mat_list[[a]]) = c("8", "16", "32", "64")
  colnames(mat_list[[a]]) = c("-0.001", "0", "0.0005", "0.001", "0.002")
  mat_list[[a]] = as.data.frame(mat_list[[a]])
  for(i in 1:length(rownames(mat_list[[a]]))){
    for(j in 1:length(colnames(mat_list[[a]]))){
      mat_list[[a]][i,j] = urn_sizeXchangeXadapt[urn_sizeXchangeXadapt[,1] == adapt[a] & 
                                                   urn_sizeXchangeXadapt[,2] == urn_sizes[i] &
                                                   urn_sizeXchangeXadapt[,3] == changes[j], 4]
    }
  }
}


col_fun = colorRamp2(c(min(unlist(mat_list)), mean(unlist(mat_list)), max(unlist(mat_list))), c("red", "white",  "green"))
h_adapt_50 = Heatmap(as.matrix(mat_list[[1]]), cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Coverage"),
                     cell_fun = function(j, i, x, y, width, height, fill) {
                       grid.text(sprintf("%.2f", as.matrix(mat_list[[1]])[i,j]), x, y, gp = gpar(fontsize = 10))
                     })
h_adapt_50

h_adapt_70 = Heatmap(as.matrix(mat_list[[2]]), cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Coverage"),
                     cell_fun = function(j, i, x, y, width, height, fill) {
                       grid.text(sprintf("%.2f", as.matrix(mat_list[[2]])[i,j]), x, y, gp = gpar(fontsize = 10))
                     })
h_adapt_70

h_adapt_sigma = Heatmap(as.matrix(mat_list[[3]]), cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Coverage"),
                        cell_fun = function(j, i, x, y, width, height, fill) {
                          grid.text(sprintf("%.2f", as.matrix(mat_list[[3]])[i,j]), x, y, gp = gpar(fontsize = 10))
                        })
h_adapt_sigma

h_n_adaptive = Heatmap(as.matrix(mat_list[[4]]), cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Coverage"),
                       cell_fun = function(j, i, x, y, width, height, fill) {
                         grid.text(sprintf("%.2f", as.matrix(mat_list[[4]])[i,j]), x, y, gp = gpar(fontsize = 10))
                       })
h_n_adaptive

#MSE based results
urn_sizeXchange = discrete_mse %>%
  filter(adapt == "adaptive70") %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change, player_urn_size,starts_with("iter")) %>%
  arrange(amount_of_change)

plot(as.vector(unlist(urn_sizeXchange[4,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = -0.001")
lines(as.vector(unlist(urn_sizeXchange[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[3,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[8,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = 0")
lines(as.vector(unlist(urn_sizeXchange[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[6,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[7,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[12,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = 0.0005")
lines(as.vector(unlist(urn_sizeXchange[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[11,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[16,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = 0.001")
lines(as.vector(unlist(urn_sizeXchange[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[14,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[15,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[20,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Change = 0.002")
lines(as.vector(unlist(urn_sizeXchange[17,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[18,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[19,-c(1,2)])), col = 4)

#cumulative coverage based results
urn_sizeXchange = sim2_discrete %>%
  filter(adapt == "adaptive50", ) %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(amount_of_change, player_urn_size,starts_with("coverage")) %>%
  arrange(amount_of_change)

plot(as.vector(unlist(urn_sizeXchange[4,-c(1,2)])), type = "l", ylim = c(0.8, 1), ylab = "MSE", main = "Change = -0.001")
lines(as.vector(unlist(urn_sizeXchange[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[3,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[8,-c(1,2)])), type = "l", ylim = c(0.8, 1), ylab = "MSE", main = "Change = 0")
lines(as.vector(unlist(urn_sizeXchange[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[6,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[7,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[12,-c(1,2)])), type = "l", ylim = c(0.8, 1), ylab = "MSE", main = "Change = 0.0005")
lines(as.vector(unlist(urn_sizeXchange[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[11,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[16,-c(1,2)])), type = "l", ylim = c(0.8, 1), ylab = "MSE", main = "Change = 0.001")
lines(as.vector(unlist(urn_sizeXchange[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[14,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[15,-c(1,2)])), col = 4)

plot(as.vector(unlist(urn_sizeXchange[20,-c(1,2)])), type = "l", ylim = c(0.8, 1), ylab = "MSE", main = "Change = 0.002")
lines(as.vector(unlist(urn_sizeXchange[17,-c(1,2)])), col = 2)
lines(as.vector(unlist(urn_sizeXchange[18,-c(1,2)])), col = 3)
lines(as.vector(unlist(urn_sizeXchange[19,-c(1,2)])), col = 4)


#cumulative coverage based results
adaptXdist_type = sim2_discrete %>%
  group_by(adapt, dist_type) %>%
  summarise(across(starts_with("coverage"), ~ mean(.))) %>%
  select(dist_type, adapt, starts_with("coverage")) %>%
  arrange(dist_type)

plot(as.vector(unlist(adaptXdist_type[4,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "MSE", main = "Better")
lines(as.vector(unlist(adaptXdist_type[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(adaptXdist_type[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(adaptXdist_type[3,-c(1,2)])), col = 4)

plot(as.vector(unlist(adaptXdist_type[8,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "MSE", main = "Central")
lines(as.vector(unlist(adaptXdist_type[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(adaptXdist_type[6,-c(1,2)])), col = 3)
lines(as.vector(unlist(adaptXdist_type[7,-c(1,2)])), col = 4)

plot(as.vector(unlist(adaptXdist_type[12,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "MSE", main = "Worse")
lines(as.vector(unlist(adaptXdist_type[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(adaptXdist_type[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(adaptXdist_type[11,-c(1,2)])), col = 4)

#mse based results
adaptXdist_type = discrete_mse %>%
  group_by(adapt, dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, adapt, starts_with("iter")) %>%
  arrange(dist_type)

plot(as.vector(unlist(adaptXdist_type[4,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Better")
lines(as.vector(unlist(adaptXdist_type[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(adaptXdist_type[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(adaptXdist_type[3,-c(1,2)])), col = 4)

plot(as.vector(unlist(adaptXdist_type[8,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Central")
lines(as.vector(unlist(adaptXdist_type[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(adaptXdist_type[6,-c(1,2)])), col = 3)
lines(as.vector(unlist(adaptXdist_type[7,-c(1,2)])), col = 4)

plot(as.vector(unlist(adaptXdist_type[12,-c(1,2)])), type = "l", ylim = c(0, 0.03), ylab = "MSE", main = "Worse")
lines(as.vector(unlist(adaptXdist_type[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(adaptXdist_type[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(adaptXdist_type[11,-c(1,2)])), col = 4)