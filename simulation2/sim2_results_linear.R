library(tidyverse)
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
change_matrix_linear[,1] = log(sim2_linear[,"true_value_first"] / (1-sim2_linear[,"true_value_first"]))
for(cl in 2:ncol(change_matrix_linear)){
  change_matrix_linear[, cl] = change_matrix_linear[, cl-1] + sim2_linear[, "amount_of_change"]
}
change_matrix_linear = exp(change_matrix_linear) / (1 + exp(change_matrix_linear))

change_matrix_avg = cbind(sim2_linear[,1:6], change_matrix_linear)
colnames(change_matrix_avg) = c(colnames(sim2_linear[,1:6]), paste0("iter", c(1:500)))

#calculating mse
linear_mse_helper = ((sim2_linear %>% select(starts_with("iter"))) - change_matrix_linear)^2

linear_mse = sim2_linear %>% select(-starts_with("coverage")) 
linear_mse[,7:506] = linear_mse_helper
rm(linear_mse_helper)



#mMSE_total = data.frame("mMSE" = c(mMSE_l, mMSE_d10, mMSE_d25),
#                           "type" = rep(c("l", "d10", "d25"), each = length(mMSE_l)))

#kruskal.test(mMSE~type, mMSE_total)
#conover.test(mMSE_total[,"mMSE"], as.factor(mMSE_total[,"type"]))
################################################################################
#Main effect change
################################################################################
# this is the analysis we are looking for
change_me = sim2_linear %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

plot(as.vector(unlist(change_me[1,-c(1)])), type = "l", ylim = c(0.35, 0.85), ylab = "Mean Estimate")
lines(as.vector(unlist(change_me[2,-c(1)])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1)])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1)])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1)])), col = 5)

true_change_me = change_matrix_avg %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

lines(as.vector(unlist(true_change_me[1,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_change_me[2,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_change_me[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_change_me[4,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_change_me[5,-c(1,2)])), col = 5, lty = "dotted")

################################################################################
#Interaction between urn sizes and change
################################################################################
# this is the analysis we are looking for
changeXurnsize = sim2_linear %>%
  group_by(player_urn_size, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, amount_of_change, starts_with("iter"))

true_changeXurnsize = change_matrix_avg %>%
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
changeXadapt = sim2_linear %>%
  group_by(adapt, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(adapt, amount_of_change, starts_with("iter"))

true_changeXadapt = change_matrix_avg %>%
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
#Linear change and dist type
################################################################################
# this is the analysis we are looking for
changeXdist = sim2_linear %>%
  filter(adapt == "n_adaptive") %>%
  group_by(dist_type, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, amount_of_change, starts_with("iter"))

true_changeXdist = change_matrix_avg %>%
  group_by(dist_type, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, amount_of_change, starts_with("iter"))

layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))


plot(as.vector(unlist(changeXdist[1,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "Mean Estimate", main = "N(1,1)")
lines(as.vector(unlist(changeXdist[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXdist[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXdist[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXdist[5,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXdist[1,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXdist[2,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXdist[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXdist[4,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXdist[5,-c(1,2)])), col = 5, lty = "dotted")


plot(as.vector(unlist(changeXdist[6,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "Mean Estimate", main = "N(0,1)")
lines(as.vector(unlist(changeXdist[7,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXdist[8,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXdist[9,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXdist[10,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXdist[6,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXdist[7,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXdist[8,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXdist[9,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXdist[10,-c(1,2)])), col = 5, lty = "dotted")


plot(as.vector(unlist(changeXdist[11,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "Mean Estimate", main = "N(-1,1)")
lines(as.vector(unlist(changeXdist[12,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXdist[13,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXdist[14,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXdist[15,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXdist[11,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXdist[12,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXdist[13,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXdist[14,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXdist[15,-c(1,2)])), col = 5, lty = "dotted")

################################################################################
#deviation from the baseline
################################################################################
layout(matrix(c(1), 1, 1, byrow = TRUE))
baseline_linear = readRDS("sim2_baseline.rds")

baseline_linear = baseline_linear / as.numeric(sim2_linear[,2])
baseline_linear = (baseline_linear - change_matrix_linear) ^ 2
baseline_linear = cbind(sim2_linear[,1:6], baseline_linear)
colnames(baseline_linear) = c(colnames(sim2_linear[,1:6]), paste0("iter", c(1:500)))

#adding the posthoc urn sizes
################################################################################
#simulation with linear change
################################################################################
post_hoc_urnsize = readRDS("post_hoc_urnsize.rds")
post_hoc_urnsize_better = readRDS("post_hoc_urnsize_better.rds")
post_hoc_urnsize_worse = readRDS("post_hoc_urnsize_worse.rds")

post_hoc_urnsize = cbind(rep(c("better", "central", "worse"), each = 72000),
                         rbind(post_hoc_urnsize_better, post_hoc_urnsize, post_hoc_urnsize_worse))
rm(post_hoc_urnsize_better, post_hoc_urnsize_worse)
colnames(post_hoc_urnsize)[c(1,4)] = c("dist", "amount_of_change")

################################################################################
#recreating linear change for post_hoc urn sizes
################################################################################
change_matrix_ph = matrix(0, nrow = nrow(post_hoc_urnsize), ncol = 500)
change_matrix_ph[,1] = log(post_hoc_urnsize[,"true_value_first"] / (1-post_hoc_urnsize[,"true_value_first"]))
for(cl in 2:ncol(change_matrix_ph)){
  change_matrix_ph[, cl] = change_matrix_ph[, cl-1] + post_hoc_urnsize[, "amount_of_change"]
}
change_matrix_ph = exp(change_matrix_ph) / (1 + exp(change_matrix_ph))

change_matrix_avg = cbind(post_hoc_urnsize[,1:6], change_matrix_ph)
colnames(change_matrix_avg) = c(colnames(post_hoc_urnsize[,1:6]), paste0("iter", c(1:500)))

post_hoc_helper = (post_hoc_urnsize %>% select(starts_with("iter")) - change_matrix_ph)^2

post_hoc_mse = post_hoc_urnsize %>% select(-starts_with("coverage")) 
post_hoc_mse[,7:506] = post_hoc_helper
rm(post_hoc_helper)

################################################################################
#baseline post_hoc urn sizes
################################################################################
baseline_post_hoc = readRDS("post_hoc_urnsize_baseline.rds")
baseline_post_hoc = baseline_post_hoc / as.numeric(post_hoc_urnsize[,2])
baseline_post_hoc = (baseline_post_hoc - change_matrix_ph) ^ 2
baseline_post_hoc = cbind(post_hoc_urnsize[,1:6], baseline_post_hoc)
colnames(baseline_post_hoc) = c(colnames(post_hoc_urnsize[,1:6]), paste0("iter", c(1:500)))

urn_size_me = linear_mse %>%
  filter(dist_type == "central") %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

post_hoc_me = post_hoc_mse %>%
  filter(dist == "central") %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

b_urn_size_me = baseline_linear %>%
  filter(dist_type == "central") %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

b_post_hoc_me = baseline_post_hoc %>%
  filter(dist == "central") %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

urn_size_difference = urn_size_me[,-1] - b_urn_size_me[,-1]
post_hoc_difference = post_hoc_me[,-1] - b_post_hoc_me[,-1]

layout(matrix(c(1,2), 1, 2, byrow = TRUE))
plot(as.vector(unlist(urn_size_difference[4,])), type = "l", ylim = c(-0.001, 0.004), ylab = "MSE Difference")
lines(as.vector(unlist(urn_size_difference[1,])), col = 2)
lines(as.vector(unlist(urn_size_difference[2,])), col = 3)
lines(as.vector(unlist(urn_size_difference[3,])), col = 4)
lines(as.vector(unlist(post_hoc_difference[3,])), col = 5)
lines(as.vector(unlist(post_hoc_difference[4,])), col = 6)
lines(as.vector(unlist(post_hoc_difference[1,])), col = 7)
lines(as.vector(unlist(post_hoc_difference[2,])), col = 8)

plot(as.vector(unlist(urn_size_me[4,-1])), type = "l", ylim = c(0, 0.025), ylab = "MSE")
lines(as.vector(unlist(urn_size_me[1,-1])), col = 2)
lines(as.vector(unlist(urn_size_me[2,-1])), col = 3)
lines(as.vector(unlist(urn_size_me[3,-1])), col = 4)
lines(as.vector(unlist(post_hoc_me[3,-1])), col = 5)
lines(as.vector(unlist(post_hoc_me[4,-1])), col = 6)
lines(as.vector(unlist(post_hoc_me[1,-1])), col = 7)
lines(as.vector(unlist(post_hoc_me[2,-1])), col = 8)

################################################################################
#baseline diff based on dist type and urn size
################################################################################
urn_size_worse = linear_mse %>%
  filter(dist_type == "worse", adapt== "adaptive70") %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

post_hoc_worse = post_hoc_mse %>%
  filter(dist == "worse",  adapt== "adaptive70") %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

b_urn_size_worse = baseline_linear %>%
  filter(dist_type == "worse",  adapt== "adaptive70") %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

b_post_hoc_worse = baseline_post_hoc %>%
  filter(dist == "central",  adapt== "adaptive70") %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

urn_worse_difference = urn_size_worse[,-1] - b_urn_size_worse[,-1]
post_hoc_worse_difference = post_hoc_worse[,-1] - b_post_hoc_worse[ ,-1]
  
plot(as.vector(unlist(urn_worse_difference[4,])), type = "l", ylim = c(-0.001, 0.008), ylab = "MSE Difference")
lines(as.vector(unlist(urn_worse_difference[1,])), col = 2)
lines(as.vector(unlist(urn_worse_difference[2,])), col = 3)
lines(as.vector(unlist(urn_worse_difference[3,])), col = 4)
lines(as.vector(unlist(post_hoc_worse_difference[3,])), col = 5)
lines(as.vector(unlist(post_hoc_worse_difference[4,])), col = 6)
lines(as.vector(unlist(post_hoc_worse_difference[1,])), col = 7)
lines(as.vector(unlist(post_hoc_worse_difference[2,])), col = 8)

plot(as.vector(unlist(urn_size_worse[4,-1])), type = "l", ylim = c(0, 0.025), ylab = "MSE")
lines(as.vector(unlist(urn_size_worse[1,-1])), col = 2)
lines(as.vector(unlist(urn_size_worse[2,-1])), col = 3)
lines(as.vector(unlist(urn_size_worse[3,-1])), col = 4)
lines(as.vector(unlist(post_hoc_worse[3,-1])), col = 5)
lines(as.vector(unlist(post_hoc_worse[4,-1])), col = 6)
lines(as.vector(unlist(post_hoc_worse[1,-1])), col = 7)
lines(as.vector(unlist(post_hoc_worse[2,-1])), col = 8)

################################################################################
#making tables
################################################################################
table_mse_helper = linear_mse %>%
  group_by(dist_type, player_urn_size, adapt, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, player_urn_size, adapt, amount_of_change,starts_with("iter"))

table_mse = cbind(table_mse_helper[,1:4], numeric(nrow(table_mse_helper)))
table_mse[,5] = rowMeans(table_mse_helper[, -c(1:4)])
colnames(table_mse)[5] = "mse"

table_baseline_mse_helper = baseline_linear %>%
  group_by(dist_type, player_urn_size, adapt, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, player_urn_size, adapt, amount_of_change, starts_with("iter"))

table_mse_wide = table_mse %>% pivot_wider(names_from = adapt, values_from = mse)

table_baseline_mse = cbind(table_baseline_mse_helper[,1:4], numeric(nrow(table_baseline_mse_helper)))
table_baseline_mse[,5] = rowMeans(table_baseline_mse_helper[, -c(1:4)])

table_mse_diff = table_mse
table_mse_diff_helper = table_mse[,5] - table_baseline_mse[,5]


###############################################################################
#testing and reporting
###############################################################################
linear_mMSE = cbind(linear_mse[,c(1:6)], rowMeans(linear_mse[,c(407:506)]))
colnames(linear_mMSE)[7] = "mMSE"

linear_reduced = linear_mMSE %>%
  group_by(dist_type, player_urn_size, adapt, amount_of_change) %>%
  summarise(mMSE = mean(mMSE))
#testing differences between dist types
kruskal.test(mMSE ~ dist_type, linear_reduced)
conover.test(as.numeric(unlist(linear_reduced[,"mMSE"])), 
             as.factor(unlist(linear_reduced[,"dist_type"])),
             method = "bonferroni")

dist_mMSE = linear_mMSE %>%
  group_by(dist_type) %>%
  summarise(mMSE = mean(mMSE))

dist_bias = sim2_linear %>%
  group_by(dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

dist_true = change_matrix_avg %>%
  group_by(dist) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

dist_bias = dist_bias[,-1] - dist_true[,-1]
dist_type_bias_final =rowMeans(dist_bias[,c(401:500)])

dist_mMSE_70 = linear_mMSE %>%
  filter(adapt == "adaptive70") %>%
  group_by(dist_type) %>%
  summarise(mMSE = mean(mMSE))

dist_bias = sim2_linear %>%
  filter(adapt == "adaptive70") %>%
  group_by(dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

dist_true = change_matrix_avg %>%
  filter(adapt == "adaptive70") %>%
  group_by(dist) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

dist_bias = dist_bias[,-1] - dist_true[,-1]
dist_type70_bias_final = rowMeans(dist_bias[,c(401:500)])

dist_mMSE_n = linear_mMSE %>%
  filter(adapt == "n_adaptive") %>%
  group_by(dist_type) %>%
  summarise(mMSE = mean(mMSE))

dist_bias = sim2_linear %>%
  filter(adapt == "n_adaptive") %>%
  group_by(dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

dist_true = change_matrix_avg %>%
  filter(adapt == "n_adaptive") %>%
  group_by(dist) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

dist_bias = dist_bias[,-1] - dist_true[,-1]
dist_type_n_bias_final = rowMeans(dist_bias[,c(401:500)])

######## filtering negative change
dist_mMSE_70_neg = linear_mMSE %>%
  filter(adapt == "adaptive70", amount_of_change == -0.001) %>%
  group_by(dist_type) %>%
  summarise(mMSE = mean(mMSE))

dist_bias = sim2_linear %>%
  filter(adapt == "adaptive70", amount_of_change == -0.001) %>%
  group_by(dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

dist_true = change_matrix_avg %>%
  filter(adapt == "adaptive70", amount_of_change == -0.001) %>%
  group_by(dist) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

dist_bias = dist_bias[,-1] - dist_true[,-1]
dist_type70_neg_bias_final = rowMeans(dist_bias[,c(401:500)])

dist_mMSE_n_neg = linear_mMSE %>%
  filter(adapt == "n_adaptive", amount_of_change == -0.001) %>%
  group_by(dist_type) %>%
  summarise(mMSE = mean(mMSE))

dist_bias = sim2_linear %>%
  filter(adapt == "n_adaptive", amount_of_change == -0.001) %>%
  group_by(dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

dist_true = change_matrix_avg %>%
  filter(adapt == "n_adaptive", amount_of_change == -0.001) %>%
  group_by(dist) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

dist_bias = dist_bias[,-1] - dist_true[,-1]
dist_type_n_neg_bias_final = rowMeans(dist_bias[,c(401:500)])

dist_mMSE_neg = linear_mMSE %>%
  filter(amount_of_change == -0.001) %>%
  group_by(dist_type) %>%
  summarise(mMSE = mean(mMSE))

dist_bias = sim2_linear %>%
  filter(amount_of_change == -0.001) %>%
  group_by(dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

dist_true = change_matrix_avg %>%
  filter(amount_of_change == -0.001) %>%
  group_by(dist) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

dist_bias = dist_bias[,-1] - dist_true[,-1]
dist_type_neg_bias_final = rowMeans(dist_bias[,c(401:500)])

dist_mMSE
dist_mMSE_70
dist_mMSE_n
dist_mMSE_70_neg
dist_mMSE_n_neg
dist_mMSE_neg

dist_type_bias_final
dist_type70_bias_final
dist_type_n_bias_final
dist_type70_neg_bias_final
dist_type_n_neg_bias_final
dist_type_neg_bias_final

###################################### checking mse diffs in the last 100 iterations and 
# mse testing

rowMeans(urn_size_difference[401:500])[c(4,1,2,3)] #4123
rowMeans(post_hoc_difference[401:500])[c(3,4,1,2)] #3412

colnames(post_hoc_mse)[1] = "dist_type"
testing_df = rbind(cbind(linear_mse[,1:6], mMSE = rowMeans(linear_mse[,407:506])),
                   cbind(post_hoc_mse[,1:6], mMSE = rowMeans(post_hoc_mse[,407:506])))

testing_df = testing_df %>%
  group_by(dist_type, player_urn_size, adapt, amount_of_change) %>%
  summarise(mMSE = mean(mMSE))

#testing differences between urn sizes
kruskal.test(mMSE ~ player_urn_size, testing_df[testing_df[,"dist_type"] == "central",])
conover.test(as.numeric(unlist(testing_df[testing_df[,"dist_type"] == "central","mMSE"])), 
             as.factor(unlist(testing_df[testing_df[,"dist_type"] == "central","player_urn_size"])),
             method = "bonferroni")

testing_df %>%
  filter(dist_type == "central") %>% 
  group_by(player_urn_size) %>%
  summarise(mMSE = mean(mMSE)) %>%
  mutate_at(1, as.numeric) %>%
  arrange(player_urn_size)

# worse with adaptive 70
rowMeans(urn_worse_difference[401:500])[c(4,1,2,3)] #4123
rowMeans(post_hoc_worse_difference[401:500])[c(3,4,1,2)] #3412

testing_df_worse = testing_df %>%
  filter(dist_type == "worse")
#testing differences between urn sizes
kruskal.test(mMSE ~ player_urn_size, testing_df_worse)
conover.test(as.numeric(unlist(testing_df_worse[,"mMSE"])), 
             as.factor(unlist(testing_df_worse[,"player_urn_size"])),
             method = "bonferroni")

testing_df %>%
  filter(dist_type == "worse", adapt == "adaptive70") %>% 
  group_by(player_urn_size) %>%
  summarise(mMSE = mean(mMSE)) %>%
  mutate_at(1, as.numeric) %>%
  arrange(player_urn_size)

testing_df %>%
  filter(dist_type == "worse", adapt == "n_adaptive") %>% 
  group_by(player_urn_size) %>%
  summarise(mMSE = mean(mMSE)) %>%
  mutate_at(1, as.numeric) %>%
  arrange(player_urn_size)

testing_df %>%
  filter(dist_type == "central", adapt == "adaptive70") %>% 
  group_by(player_urn_size) %>%
  summarise(mMSE = mean(mMSE)) %>%
  mutate_at(1, as.numeric) %>%
  arrange(player_urn_size)

testing_df %>%
  filter(dist_type == "central", adapt == "n_adaptive") %>% 
  group_by(player_urn_size) %>%
  summarise(mMSE = mean(mMSE)) %>%
  mutate_at(1, as.numeric) %>%
  arrange(player_urn_size)

testing_df %>%
  filter(dist_type == "better", adapt == "adaptive70") %>% 
  group_by(player_urn_size) %>%
  summarise(mMSE = mean(mMSE)) %>%
  mutate_at(1, as.numeric) %>%
  arrange(player_urn_size)

testing_df %>%
  filter(dist_type == "better", adapt == "n_adaptive") %>% 
  group_by(player_urn_size) %>%
  summarise(mMSE = mean(mMSE)) %>%
  mutate_at(1, as.numeric) %>%
  arrange(player_urn_size)
