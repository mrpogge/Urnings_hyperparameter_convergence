library(tidyverse)
library(grid)
library(conover.test)

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
  group_by(dist_type) %>%
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
  group_by(dist_type) %>%
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
  group_by(dist_type) %>%
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
  group_by(dist_type) %>%
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
  group_by(dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

dist_bias = dist_bias[,-1] - dist_true[,-1]
dist_type_n_neg_bias_final = rowMeans(dist_bias[,c(401:500)])

dist_mMSE
dist_mMSE_70
dist_mMSE_n
dist_mMSE_70_neg
dist_mMSE_n_neg

dist_type_bias_final
dist_type70_bias_final
dist_type_n_bias_final
dist_type70_neg_bias_final
dist_type_n_neg_bias_final
