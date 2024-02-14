library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(png)
library(grid)

################################################################################
#simulation with discrete change (25)
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
#recreating change
################################################################################
change_matrix_discrete_25 = matrix(rep(sim2_discrete_25[,"true_value_first"], 500), nrow = nrow(sim2_discrete_25), ncol = 500)
change_matrix_discrete_25 = log(change_matrix_discrete_25 / (1- change_matrix_discrete_25))

jumps = rep(0:24, each = 20)
change_per_jump = sim2_discrete_25$amount_of_change

change_matrix_discrete_25 = change_matrix_discrete_25 + outer(change_per_jump, jumps, "*")
change_matrix_discrete_25 = exp(change_matrix_discrete_25) / (1 + exp(change_matrix_discrete_25))

change_matrix_discrete_25_avg = cbind(sim2_discrete_25[,1:6], change_matrix_discrete_25)
colnames(change_matrix_discrete_25_avg) = c(colnames(sim2_discrete_25[,1:6]), paste0("iter", c(1:500)))

#calculating mean squared error
discrete_mse_helper = (sim2_discrete_25 %>% select(starts_with("iter")) - change_matrix_discrete_25)^2

discrete_25_mse = sim2_discrete_25 %>% select(-starts_with("coverage")) 
discrete_25_mse[,7:506] = discrete_mse_helper
rm(discrete_mse_helper)

mMSE_d25 = rowMeans(discrete_25_mse[,406:506])

################################################################################
#Main effect change
################################################################################
# this is the analysis we are looking for
change_me = sim2_discrete_25 %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

plot(as.vector(unlist(change_me[1,-c(1)])), type = "l", ylim = c(0.35, 0.85), ylab = "Mean Estimate")
lines(as.vector(unlist(change_me[2,-c(1)])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1)])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1)])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1)])), col = 5)

true_change_me = change_matrix_discrete_25_avg %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

lines(as.vector(unlist(true_change_me[1,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_change_me[2,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_change_me[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_change_me[4,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_change_me[5,-c(1,2)])), col = 5, lty = "dotted")

d25_abs_difference = abs(change_me[,-1] - true_change_me[,-1])[,400:500]
mean(rowMeans(d25_abs_difference))




################################################################################
#Interaction between urn sizes and change
################################################################################
# this is the analysis we are looking for
changeXurnsize = sim2_discrete_25 %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, amount_of_change, starts_with("iter"))

true_changeXurnsize = change_matrix_discrete_25_avg %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, amount_of_change, starts_with("iter"))

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4), 4, 4, byrow = TRUE))

plot(as.vector(unlist(changeXurnsize[16,-c(1,2)])), type = "l", ylim = c(0.35, 0.85), ylab = "Mean Estimate", main = "Urn size = 8")
lines(as.vector(unlist(changeXurnsize[17,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXurnsize[18,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXurnsize[19,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXurnsize[20,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXurnsize[16,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[17,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[18,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[19,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[20, -c(1,2)])), col = 5, lty = "dotted")


plot(as.vector(unlist(changeXurnsize[1,-c(1,2)])), type = "l", ylim = c(0.35, 0.85), ylab = "Mean Estimate", main = "Urn size = 16")
lines(as.vector(unlist(changeXurnsize[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXurnsize[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXurnsize[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXurnsize[5,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXurnsize[1,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[2,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[4,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[5,-c(1,2)])), col = 5, lty = "dotted")


plot(as.vector(unlist(changeXurnsize[6,-c(1,2)])), type = "l", ylim = c(0.35, 0.85), ylab = "Mean Estimate", main = "Urn size = 32")
lines(as.vector(unlist(changeXurnsize[7,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXurnsize[8,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXurnsize[9,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXurnsize[10,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXurnsize[6,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[7,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[8,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[9,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[10,-c(1,2)])), col = 5, lty = "dotted")


plot(as.vector(unlist(changeXurnsize[11,-c(1,2)])), type = "l", ylim = c(0.35, 0.85), ylab = "Mean Estimate", main = "Urn size = 64")
lines(as.vector(unlist(changeXurnsize[12,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXurnsize[13,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXurnsize[14,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXurnsize[15,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXurnsize[11,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[12,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[13,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[14,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[15,-c(1,2)])), col = 5, lty = "dotted")

################################################################################
#Linear change and adaptivity
################################################################################
# this is the analysis we are looking for
changeXadapt = sim2_discrete_25 %>%
  group_by(adapt, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(adapt, amount_of_change, starts_with("iter"))

true_changeXadapt = change_matrix_discrete_25_avg %>%
  group_by(adapt, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(adapt, amount_of_change, starts_with("iter"))

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4), 4, 4, byrow = TRUE))

plot(as.vector(unlist(changeXadapt[16,-c(1,2)])), type = "l", ylim = c(0.35, 0.85), ylab = "Mean Estimate", main = "Non-adaptive")
lines(as.vector(unlist(changeXadapt[17,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXadapt[18,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXadapt[19,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXadapt[20,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXadapt[16,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[17,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[18,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[19,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[20, -c(1,2)])), col = 5, lty = "dotted")


plot(as.vector(unlist(changeXadapt[1,-c(1,2)])), type = "l", ylim = c(0.35, 0.85), ylab = "Mean Estimate", main = "Adaptive 0.5")
lines(as.vector(unlist(changeXadapt[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXadapt[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXadapt[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXadapt[5,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXadapt[1,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[2,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[4,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[5,-c(1,2)])), col = 5, lty = "dotted")


plot(as.vector(unlist(changeXadapt[6,-c(1,2)])), type = "l", ylim = c(0.35, 0.85), ylab = "Mean Estimate", main = "Adaptive 0.7")
lines(as.vector(unlist(changeXadapt[7,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXadapt[8,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXadapt[9,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXadapt[10,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXadapt[6,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[7,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[8,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[9,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[10,-c(1,2)])), col = 5, lty = "dotted")

plot(as.vector(unlist(changeXadapt[11,-c(1,2)])), type = "l", ylim = c(0.35, 0.85), ylab = "Mean Estimate", main = "Adaptive sigma")
lines(as.vector(unlist(changeXadapt[12,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXadapt[13,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXadapt[14,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXadapt[15,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXadapt[11,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[12,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[13,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[14,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[15,-c(1,2)])), col = 5, lty = "dotted")

################################################################################
#deviation from the baseline
################################################################################
baseline_discrete_25 = readRDS("sim2_baseline_discrete_25.rds")

baseline_discrete_25 = baseline_discrete_25 / as.numeric(sim2_discrete_25[,2])
baseline_discrete_25 = (baseline_discrete_25 - change_matrix_discrete_25) ^ 2
baseline_discrete_25 = cbind(sim2_discrete_25[,1:6], baseline_discrete_25)
colnames(baseline_discrete_25) = c(colnames(sim2_discrete_25[,1:6]), paste0("iter", c(1:500)))

################################################################################
#post-hoc urnsizes
################################################################################
post_hoc_urnsize = readRDS("post_hoc_urnsize_discrete_25.rds")
post_hoc_urnsize_better = readRDS("post_hoc_urnsize_discrete_25_better.rds")
post_hoc_urnsize_worse = readRDS("post_hoc_urnsize_discrete_25_worse.rds")

post_hoc_urnsize = cbind(rep(c("better", "central", "worse"), each = 72000),
                         rbind(post_hoc_urnsize_better, post_hoc_urnsize, post_hoc_urnsize_worse))
rm(post_hoc_urnsize_better, post_hoc_urnsize_worse)
colnames(post_hoc_urnsize)[c(1,4)] = c("dist", "amount_of_change")

change_matrix_discrete_25_ph = matrix(rep(post_hoc_urnsize[,"true_value_first"], 500), nrow = nrow(post_hoc_urnsize), ncol = 500)
change_matrix_discrete_25_ph = log(change_matrix_discrete_25_ph / (1- change_matrix_discrete_25_ph))

jumps = rep(0:24, each = 20)
change_per_jump = post_hoc_urnsize$amount_of_change

change_matrix_discrete_25_ph = change_matrix_discrete_25_ph + outer(change_per_jump, jumps, "*")
change_matrix_discrete_25_ph = exp(change_matrix_discrete_25_ph) / (1 + exp(change_matrix_discrete_25_ph))

post_hoc_helper = (post_hoc_urnsize %>% select(starts_with("iter")) - change_matrix_discrete_25_ph)^2
post_hoc_mse = post_hoc_urnsize %>% select(-starts_with("coverage")) 
post_hoc_mse[,6:505] = post_hoc_helper
rm(post_hoc_helper)

################################################################################
#baseline post_hoc urn sizes
################################################################################
baseline_post_hoc = readRDS("post_hoc_urnsize_baseline_25.rds")
baseline_post_hoc = baseline_post_hoc / as.numeric(post_hoc_urnsize[,1])
baseline_post_hoc = (baseline_post_hoc - change_matrix_discrete_25_ph) ^ 2
baseline_post_hoc = cbind(post_hoc_urnsize[,1:5], baseline_post_hoc)
colnames(baseline_post_hoc) = c(colnames(post_hoc_urnsize[,1:5]), paste0("iter", c(1:500)))



################################################################################
#simulation with discrete change
################################################################################
urn_size_me = discrete_25_mse %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

post_hoc_me = post_hoc_mse %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

b_urn_size_me = baseline_discrete_25 %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

b_post_hoc_me = baseline_post_hoc %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

urn_size_difference = urn_size_me[,-1] - b_urn_size_me[,-1]
post_hoc_difference = post_hoc_me[,-1] - b_post_hoc_me[,-1]

plot(as.vector(unlist(urn_size_difference[4,])), type = "l", ylim = c(-0.001, 0.004), ylab = "MSE Difference")
lines(as.vector(unlist(urn_size_difference[1,])), col = 2)
lines(as.vector(unlist(urn_size_difference[2,])), col = 3)
lines(as.vector(unlist(urn_size_difference[3,])), col = 4)
lines(as.vector(unlist(post_hoc_difference[3,])), col = 5)
lines(as.vector(unlist(post_hoc_difference[4,])), col = 6)
lines(as.vector(unlist(post_hoc_difference[1,])), col = 7)
lines(as.vector(unlist(post_hoc_difference[2,])), col = 8)



