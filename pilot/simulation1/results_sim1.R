library(tidyverse)
library(Hmisc)

set.seed(13181913)
#dir.create("figures", showWarnings = FALSE)
################################################################################
#READING THE SIMULATION OUTPUTS
################################################################################
sim1_urnings1_npu = readRDS("sim1_urnings1_npu.rds")
sim1_urnings1_pu = readRDS("sim1_urnings1_pu.rds")
sim1_urnings2_npu = readRDS("sim1_urnings2_npu.rds")
sim1_urnings2_pu = readRDS("sim1_urnings2_pu.rds")

sim1_output = rbind(sim1_urnings1_npu, sim1_urnings1_pu, sim1_urnings2_npu, sim1_urnings2_pu)
rm(sim1_urnings1_npu, sim1_urnings1_pu, sim1_urnings2_npu, sim1_urnings2_pu)

################################################################################
#Calculating mean squared error across all conditions
################################################################################
sqe_output = sim1_output %>% 
  mutate(across(starts_with("iter"), ~ (. - true_value)^2))

################################################################################
#Calculating coverage across all conditions
################################################################################
urn_sizes = c(rep(8, 9),rep(16, 17), rep(32, 33), rep(64, 65))
urnings_values =  c(seq(0,8,1), seq(0,16,1), seq(0,32,1),seq(0,64,1))
urn_sizes_helper = rep(urn_sizes, each = 9)
urnings_values_helper = rep(urnings_values, each = 9)
true_vals = rep(seq(0.1, 0.9, 0.1), times = length(urn_sizes))
confint_matrix = cbind(urn_sizes_helper, urnings_values_helper, true_vals)
confint_matrix = cbind(confint_matrix, matrix(0, nrow = nrow(confint_matrix), ncol = 3))
confint_matrix[, 4:5] = binconf(confint_matrix[,2], confint_matrix[,1], method = "wilson")[,2:3]
confint_matrix[,6] = ifelse(confint_matrix[,3] < confint_matrix[,5]  & confint_matrix[,3] > confint_matrix[,4], 1, 0)
rm(urn_sizes, urnings_values, urn_sizes_helper, urnings_values_helper, true_vals)

sim1_output_helper =  sim1_output
sim1_output_helper = sim1_output_helper %>%
  mutate_at(1, as.integer) %>%
  mutate(across(starts_with("iter"), ~ (. * player_urn_size))) 

for(i in 1:nrow(confint_matrix)){
  cond_urn = sim1_output_helper[,1] == confint_matrix[i ,1]
  cond_true_val = sim1_output_helper[,7] == confint_matrix[i ,3]
  indexes = which(cond_urn & cond_true_val)
  rm(cond_urn, cond_true_val)
  helper = sim1_output_helper[indexes, ]
  helper_iters = helper[, 8:507]
  rm(helper)
  helper_iters[helper_iters == confint_matrix[i ,2]] = confint_matrix[i ,6]
  sim1_output_helper[indexes, 8:507] = helper_iters
  print(i)
}
rm(helper_iters, confint_matrix)


sim1_coverage <- sim1_output_helper 
sim1_coverage[,8:507] = t(apply(sim1_coverage[, 8:507],1,  cumsum)) / 500
rm(sim1_output_helper)

################################################################################
#Creating baseline
################################################################################
baseline = sim1_output[, -c(508:1007)]

param_combinations =cbind(as.integer(baseline[,1]), baseline[,7]) 
colnames(param_combinations) = c("size", "prob")
baseline[, 8:507] = t(mapply(function(size, prob) {
  rbinom(500, size = size, prob = prob) / size
}, param_combinations[,1], param_combinations[,2]))

rm(param_combinations)

sqe_output_baseline = baseline %>% 
  mutate(across(starts_with("iter"), ~ (. - true_value)^2))

sim1_output_helper =  baseline
sim1_output_helper = sim1_output_helper %>%
  mutate_at(1, as.integer) %>%
  mutate(across(starts_with("iter"), ~ (. * player_urn_size))) 

for(i in 1:nrow(confint_matrix)){
  cond_urn = sim1_output_helper[,1] == confint_matrix[i ,1]
  cond_true_val = sim1_output_helper[,7] == confint_matrix[i ,3]
  indexes = which(cond_urn & cond_true_val)
  rm(cond_urn, cond_true_val)
  helper = sim1_output_helper[indexes, ]
  helper_iters = helper[, 8:507]
  rm(helper)
  helper_iters[helper_iters == confint_matrix[i ,2]] = confint_matrix[i ,6]
  sim1_output_helper[indexes, 8:507] = helper_iters
  print(i)
}
rm(helper_iters, confint_matrix)

baseline_coverage <- sim1_output_helper 
baseline_coverage[,8:507] = t(apply(baseline_coverage[, 8:507],1,  cumsum)) / 500
rm(sim1_output_helper)

################################################################################
#MAIN EFFECT: Algorithm type (Urnings1 vs Urnings2)
################################################################################
#Mean squared error  
urnings1vs2 = sqe_output %>% 
  group_by(algo) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(algo,starts_with('iter'))

urnings1vs2_baseline = sqe_output_baseline %>% 
  group_by(algo) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(algo,starts_with('iter'))

#Bias
urnings1vs2_bias = sim1_output %>%
  group_by(true_value, algo) %>%
  summarise(across(starts_with("iter"), ~ (mean(.) - true_value)^2)) %>%
  ungroup(true_value) %>%
  summarise(across(starts_with("iter"), ~ mean(.)))

#Bias
urnings1vs2_bias_baseline = baseline %>%
  group_by(true_value, algo) %>%
  summarise(across(starts_with("iter"), ~ (mean(.) - true_value)^2)) %>%
  ungroup(true_value) %>%
  summarise(across(starts_with("iter"), ~ mean(.)))

#coverage
urnings1vs2_coverage = sim1_coverage %>%
  group_by(algo) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

urnings1vs2_coverage_baseline = baseline_coverage %>%
  group_by(algo) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

#mean absolute prediction error
urnings1vs2_pred = sim1_output %>%
  group_by(algo) %>%
  summarise(across(starts_with("pred"), ~ mean(.))) 

#creating the plot
file_path = file.path("figures/algo_type.png")
png(file_path, width = 5.5, height = 7, units = "in", res = 400)

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4,
                5,5,5,5), 5, 4, byrow = TRUE))
plot(as.vector(unlist(urnings1vs2[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
lines(as.vector(unlist(urnings1vs2[2,-1])), col = 2)
lines(as.vector(unlist(urnings1vs2_baseline[1,-1])), col = 5, lty = "dotted")
lines(as.vector(unlist(urnings1vs2_baseline[2,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(urnings1vs2_pred[1,-1])), type = "l", ylim = c(0.2, .6), ylab = "Mean Absolute Prediction Error")
lines(as.vector(unlist(urnings1vs2_pred[2,-1])), col = 2)


plot(as.vector(unlist(urnings1vs2_bias[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Bias")
lines(as.vector(unlist(urnings1vs2_bias[2,-1])), col = 2)
lines(as.vector(unlist(urnings1vs2_bias_baseline[1,-1])), col = 5, lty = "dotted")
lines(as.vector(unlist(urnings1vs2_bias_baseline[2,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(urnings1vs2_coverage[1,-1])), type = "l", ylim = c(0, 1), ylab = "Mean Coverage")
lines(as.vector(unlist(urnings1vs2_coverage[2,-1])), col = 2)
lines(as.vector(unlist(urnings1vs2_coverage_baseline[1,-1])), col = 5, lty = "dotted")
lines(as.vector(unlist(urnings1vs2_coverage_baseline[2,-1])), col = 5, lty = "dotted")

plot.new()
# Create a legend manually using the legend() function
legend("center", legend = c("Urnings 1", "Urnings 2", "baseline"),
       col = c("black", "red", 5), pch = c(NA, NA, 1), lty = c(1,1, NA),
       bty = "n", cex = 1.3, horiz = TRUE)

dev.off()


################################################################################
#MAIN EFFECT: Paired_update (No paired update vs Paired update)
################################################################################
puvsnpu = sqe_output %>% 
  group_by(paired_update) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(paired_update,starts_with('iter'))

puvsnpu_baseline = sqe_output_baseline %>% 
  group_by(paired_update) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(paired_update,starts_with('iter'))

#Bias
puvsnpu_bias = sim1_output %>%
  group_by(true_value, paired_update) %>%
  summarise(across(starts_with("iter"), ~ (mean(.) - true_value)^2)) %>%
  ungroup(true_value) %>%
  summarise(across(starts_with("iter"), ~ mean(.)))

puvsnpu_bias_baseline = baseline %>%
  group_by(true_value, paired_update) %>%
  summarise(across(starts_with("iter"), ~ (mean(.) - true_value)^2)) %>%
  ungroup(true_value) %>%
  summarise(across(starts_with("iter"), ~ mean(.)))

#coverage
puvsnpu_coverage = sim1_coverage %>%
  group_by(paired_update) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

puvsnpu_coverage_baseline = baseline_coverage %>%
  group_by(paired_update) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

#mean absolute prediction error
puvsnpu_pred = sim1_output %>%
  group_by(paired_update) %>%
  summarise(across(starts_with("pred"), ~ mean(.))) 

#creating the plot
file_path = file.path("figures/paired_update.png")
png(file_path, width = 6, height = 7, units = "in", res = 400)

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4,
                5,5,5,5), 5, 4, byrow = TRUE))
plot(as.vector(unlist(puvsnpu[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
lines(as.vector(unlist(puvsnpu[2,-1])), col = 2)
lines(as.vector(unlist(puvsnpu_baseline[1,-1])), col = 5, lty = "dotted")
lines(as.vector(unlist(puvsnpu_baseline[2,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(puvsnpu_pred[1,-1])), type = "l", ylim = c(0.2, .6), ylab = "Mean Absolute Prediction Error")
lines(as.vector(unlist(puvsnpu_pred[2,-1])), col = 2)


plot(as.vector(unlist(puvsnpu_bias[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Bias")
lines(as.vector(unlist(puvsnpu_bias[2,-1])), col = 2)
lines(as.vector(unlist(puvsnpu_bias_baseline[1,-1])), col = 5, lty = "dotted")
lines(as.vector(unlist(puvsnpu_bias_baseline[2,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(puvsnpu_coverage[1,-1])), type = "l", ylim = c(0, 1), ylab = "Mean Coverage")
lines(as.vector(unlist(puvsnpu_coverage[2,-1])), col = 2)
lines(as.vector(unlist(puvsnpu_coverage_baseline[1,-1])), col = 5, lty = "dotted")
lines(as.vector(unlist(puvsnpu_coverage_baseline[2,-1])), col = 5, lty = "dotted")

plot.new()
# Create a legend manually using the legend() function
legend("center", legend = c("Paired update: ", "FALSE", "TRUE", "baseline"),
       col = c("black","black", "red", 5), pch = c(NA,NA,NA, 1), lty = c(NA,1, 1, NA),
       bty = "n", cex = 1.3, horiz = TRUE)

dev.off()

################################################################################
#MAIN EFFECT: Item urn sizes (16 vs 32 vs 64 vs 128)
################################################################################
item_urnsizes = sqe_output %>% 
  group_by(item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(item_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

item_urnsizes_baseline = sqe_output_baseline %>% 
  group_by(item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(item_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

#Bias
item_urnsizes_bias = sim1_output %>%
  group_by(true_value, item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ (mean(.) - true_value)^2)) %>%
  ungroup(true_value) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

item_urnsizes_bias_baseline = baseline %>%
  group_by(true_value, item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ (mean(.) - true_value)^2)) %>%
  ungroup(true_value) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

#coverage
item_urnsizes_coverage = sim1_coverage %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%  
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

#coverage
item_urnsizes_coverage_baseline = baseline_coverage %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%  
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

#mean absolute prediction error
item_urnsizes_pred = sim1_output %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("pred"), ~ mean(.))) %>%  
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)


#creating plot
file_path = file.path("figures/item_urn_sizes.png")
png(file_path, width = 6, height = 7, units = "in", res = 400)

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4,
                5,5,5,5), 5, 4, byrow = TRUE))
plot(as.vector(unlist(item_urnsizes[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
lines(as.vector(unlist(item_urnsizes_baseline[1,-1])), col = 5, lty = "dotted")
for (i in 2:4) {
  lines(as.vector(unlist(item_urnsizes[i,-1])), col = i)
  lines(as.vector(unlist(item_urnsizes_baseline[i,-1])), lty = "dotted", col = 5)
}

plot(as.vector(unlist(item_urnsizes_pred[1,-1])), type = "l", ylim = c(0.2, 0.6), ylab = "Mean Absolute Prediction Error")
for (i in 2:4) {
  lines(as.vector(unlist(item_urnsizes_pred[i,-1])), col = i)
}

plot(as.vector(unlist(item_urnsizes_bias[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Bias")
lines(as.vector(unlist(item_urnsizes_bias_baseline[1,-1])), col = 5, lty = "dotted")
for (i in 2:4) {
  lines(as.vector(unlist(item_urnsizes_bias[i,-1])), col = i)
  lines(as.vector(unlist(item_urnsizes_bias_baseline[i,-1])), col = 5, lty = "dotted")
}

plot(as.vector(unlist(item_urnsizes_coverage[1,-1])), type = "l", ylim = c(0, 1), ylab = "Mean Coverage")
lines(as.vector(unlist(item_urnsizes_coverage_baseline[1,-1])), col = 5, lty = "dotted")
for (i in 2:4) {
  lines(as.vector(unlist(item_urnsizes_coverage[i,-1])), col = i)
  lines(as.vector(unlist(item_urnsizes_coverage_baseline[i,-1])), col = 5, lty = "dotted")
}

plot.new()
# Create a legend manually using the legend() function
legend("center", legend = c("Item Urn Sizes: ","16", "32", "64", "128", "baseline"),
       col = c(1,1:4,5), pch = c(NA,NA,NA, NA,NA, 1), lty = c(NA,1, 1, 1,1,NA), bty = "n", 
       cex = 0.7, horiz = TRUE)


dev.off()

################################################################################
#MAIN EFFECT: Player urn sizes (8 vs 16 vs 32 vs 64)
################################################################################
player_urnsizes = sqe_output %>% 
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

player_urnsizes_baseline = sqe_output_baseline %>% 
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

#Bias
player_urnsizes_bias = sim1_output %>%
  group_by(true_value, player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ (mean(.) - true_value)^2)) %>%
  ungroup(true_value) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

player_urnsizes_bias_baseline = baseline %>%
  group_by(true_value, player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ (mean(.) - true_value)^2)) %>%
  ungroup(true_value) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

#coverage
player_urnsizes_coverage = sim1_coverage %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%  
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

#coverage
player_urnsizes_coverage_baseline = baseline_coverage %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%  
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

#mean absolute prediction error
player_urnsizes_pred = sim1_output %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("pred"), ~ mean(.))) %>%  
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

#creating plots
file_path = file.path("figures/player_urn_sizes.png")
png(file_path, width = 6, height = 7, units = "in", res = 400)

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4,
                5,5,5,5), 5, 4, byrow = TRUE))
plot(as.vector(unlist(player_urnsizes[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
lines(as.vector(unlist(player_urnsizes_baseline[1,-1])), col = 5, lty = "dotted")
for (i in 2:4) {
  lines(as.vector(unlist(player_urnsizes[i,-1])), col = i)
  lines(as.vector(unlist(player_urnsizes_baseline[i,-1])), col = 5, lty = "dotted")
}

plot(as.vector(unlist(player_urnsizes_pred[1,-1])), type = "l", ylim = c(0.2, 0.6), ylab = "Mean Absolute Prediction Error")
for (i in 2:4) {
  lines(as.vector(unlist(player_urnsizes_pred[i,-1])), col = i)
}

plot(as.vector(unlist(player_urnsizes_bias[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Bias")
lines(as.vector(unlist(player_urnsizes_bias_baseline[1,-1])), col = 5, lty = "dotted")
for (i in 2:4) {
  lines(as.vector(unlist(player_urnsizes_bias[i,-1])), col = i)
  lines(as.vector(unlist(player_urnsizes_bias_baseline[1,-1])), col = 5, lty = "dotted")
}

plot(as.vector(unlist(player_urnsizes_coverage[1,-1])), type = "l", ylim = c(0, 1), ylab = "Mean Coverage")
lines(as.vector(unlist(player_urnsizes_coverage_baseline[i,-1])), col = 5, lty = "dotted")
for (i in 2:4) {
  lines(as.vector(unlist(player_urnsizes_coverage[i,-1])), col = i)
  lines(as.vector(unlist(player_urnsizes_coverage_baseline[i,-1])), col = 5, lty = "dotted")
}

plot.new()
# Create a legend manually using the legend() function
legend("center", legend = c("Player Urn Sizes: ","8", "16", "32", "64", "baseline"),
       col = c(1,1:4,5), pch = c(NA,NA,NA, NA,NA, 1), lty = c(NA,1, 1, 1,1,NA), bty = "n", 
       cex = 0.7, horiz = TRUE)

dev.off()


################################################################################
#MAIN EFFECT: Item starting value (half vs invariant)
################################################################################
item_startings = sqe_output %>% 
  group_by(item_starting) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(item_starting, starts_with('iter')) 

item_startings_baseline = sqe_output_baseline %>% 
  group_by(item_starting) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(item_starting, starts_with('iter')) 


#Bias
item_startings_bias = sim1_output %>%
  group_by(true_value, item_starting) %>%
  summarise(across(starts_with("iter"), ~ (mean(.) - true_value)^2)) %>%
  ungroup(true_value) %>%
  summarise(across(starts_with("iter"), ~ mean(.)))

item_startings_bias_baseline = baseline %>%
  group_by(true_value, item_starting) %>%
  summarise(across(starts_with("iter"), ~ (mean(.) - true_value)^2)) %>%
  ungroup(true_value) %>%
  summarise(across(starts_with("iter"), ~ mean(.)))

#coverage
item_startings_coverage = sim1_coverage %>%
  group_by(item_starting) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

#coverage
item_startings_coverage_baseline = baseline_coverage %>%
  group_by(item_starting) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) 

#mean absolute prediction error
item_startings_pred = sim1_output %>%
  group_by(item_starting) %>%
  summarise(across(starts_with("pred"), ~ mean(.))) 

#creating the plot
file_path = file.path("figures/item_starting.png")
png(file_path, width = 6, height = 7, units = "in", res = 400)

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4,
                5,5,5,5), 5, 4, byrow = TRUE))
plot(as.vector(unlist(item_startings[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
lines(as.vector(unlist(item_startings[2,-1])), col = 2)
lines(as.vector(unlist(item_startings_baseline[2,-1])), col = 5, lty = "dotted")
lines(as.vector(unlist(item_startings_baseline[2,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(item_startings_pred[1,-1])), type = "l", ylim = c(0.2, .6), ylab = "Mean Absolute Prediction Error")
lines(as.vector(unlist(item_startings_pred[2,-1])), col = 2)


plot(as.vector(unlist(item_startings_bias[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Bias")
lines(as.vector(unlist(item_startings_bias[2,-1])), col = 2)
lines(as.vector(unlist(item_startings_bias_baseline[2,-1])), col = 5, lty = "dotted")
lines(as.vector(unlist(item_startings_bias_baseline[2,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(item_startings_coverage[1,-1])), type = "l", ylim = c(0, 1), ylab = "Mean Coverage")
lines(as.vector(unlist(item_startings_coverage[2,-1])), col = 2)
lines(as.vector(unlist(item_startings_coverage_baseline[2,-1])), col = 5, lty = "dotted")
lines(as.vector(unlist(item_startings_coverage_baseline[2,-1])), col = 5, lty = "dotted")

plot.new()
# Create a legend manually using the legend() function
legend("center", legend = c("Item starting: ", "half", "invariant", "baseline"),
       col = c("black","black", "red", 5), pch = c(NA,NA,NA,1), lty = c(NA, 1,1,NA), 
       bty = "n", cex = 1.3, horiz = TRUE)

dev.off()


################################################################################
#INTERACTION EFFECT: Player urn sizes * adaptivity 
################################################################################
urnXadaptivity = sqe_output %>% 
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, adapt, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)


#creating plot
file_path = file.path("figures/urnXadaptivity_mse.png")
png(file_path, width = 6, height = 7, units = "in", res = 400)

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4,
                5,5,5,5), 5, 4, byrow = TRUE))
plot(as.vector(unlist(urnXadaptivity[4,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "Urn size = 8")
lines(as.vector(unlist(urnXadaptivity[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[2,-c(1,2)])), col = 4)
lines(as.vector(unlist(player_urnsizes_baseline[1,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity[8,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "Urn size = 16")
lines(as.vector(unlist(urnXadaptivity[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[7,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[6,-c(1,2)])), col = 4)
lines(as.vector(unlist(player_urnsizes_baseline[2,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity[12,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "Urn size = 32")
lines(as.vector(unlist(urnXadaptivity[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[11,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[10,-c(1,2)])), col = 4)
lines(as.vector(unlist(player_urnsizes_baseline[3,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity[16,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "Urn size = 64")
lines(as.vector(unlist(urnXadaptivity[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[15,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[14,-c(1,2)])), col = 4)
lines(as.vector(unlist(player_urnsizes_baseline[4,-1])), col = 5, lty = "dotted")

plot.new()
# Create a legend manually using the legend() function
legend("center", legend = c("Adaptivity: ", "U(0,1)", "N(0,0.5)", "N(0,0.25)", "N(0.85,0.5)"),
       col = c(1,1:4), lty = c(NA, 1,1,1,1), bty = "n", 
       cex = 1.3, horiz = TRUE)

dev.off()

#Bias
urnXadaptivity_bias = sim1_output %>%
  group_by(true_value, player_urn_size, adapt) %>%
  summarise(across(starts_with("iter"), ~ (mean(.) - true_value)^2)) %>%
  ungroup(true_value) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)


file_path = file.path("figures/urnXadaptivity_bias.png")
png(file_path, width = 8.75, height = 7.5, units = "in", res = 300)

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4,
                5,5,5,5), 5, 4, byrow = TRUE))
plot(as.vector(unlist(urnXadaptivity_bias[4,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Bias", main = "Urn size = 8")
lines(as.vector(unlist(urnXadaptivity_bias[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity_bias[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity_bias[2,-c(1,2)])), col = 4)
lines(as.vector(unlist(player_urnsizes_bias_baseline[1,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity_bias[8,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Bias", main = "Urn size = 16")
lines(as.vector(unlist(urnXadaptivity_bias[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity_bias[7,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity_bias[6,-c(1,2)])), col = 4)
lines(as.vector(unlist(player_urnsizes_bias_baseline[2,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity_bias[12,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Bias", main = "Urn size = 32")
lines(as.vector(unlist(urnXadaptivity_bias[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity_bias[11,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity_bias[10,-c(1,2)])), col = 4)
lines(as.vector(unlist(player_urnsizes_bias_baseline[3,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity_bias[16,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Bias", main = "Urn size = 64")
lines(as.vector(unlist(urnXadaptivity_bias[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity_bias[15,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity_bias[14,-c(1,2)])), col = 4)
lines(as.vector(unlist(player_urnsizes_bias_baseline[4,-1])), col = 5, lty = "dotted")

plot.new()
# Create a legend manually using the legend() function
legend("center", legend = c("Adaptivity: ", "U(0,1)", "N(0,0.5)", "N(0,0.25)", "N(0.85,0.5)"),
       col = c(1,1:4), lty = c(NA, 1,1,1,1), bty = "n", 
       cex = 1.3, horiz = TRUE)

dev.off()


#coverage
urnXadaptivity_coverage = sim1_coverage %>%
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%  
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

#coverage
urnXadaptivity_coverage_baseline = baseline_coverage %>%
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%  
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

file_path = file.path("figures/urnXadaptivity_coverage.png")
png(file_path, width = 8.75, height = 7.5, units = "in", res = 300)
layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4,
                5,5,5,5), 5, 4, byrow = TRUE))
plot(as.vector(unlist(urnXadaptivity_coverage[4,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "Mean Coverage", main = "Urn size = 8")
lines(as.vector(unlist(urnXadaptivity_coverage[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity_coverage[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity_coverage[2,-c(1,2)])), col = 4)
lines(as.vector(unlist(player_urnsizes_coverage_baseline[1,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity_coverage[8,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "Mean Coverage", main = "Urn size = 16")
lines(as.vector(unlist(urnXadaptivity_coverage[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity_coverage[7,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity_coverage[6,-c(1,2)])), col = 4)
lines(as.vector(unlist(player_urnsizes_coverage_baseline[2,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity_coverage[12,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "Mean Coverage", main = "Urn size = 32")
lines(as.vector(unlist(urnXadaptivity_coverage[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity_coverage[11,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity_coverage[10,-c(1,2)])), col = 4)
lines(as.vector(unlist(player_urnsizes_coverage_baseline[3,-1])), col = 5, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity_coverage[16,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "Mean Coverage", main = "Urn size = 64")
lines(as.vector(unlist(urnXadaptivity_coverage[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity_coverage[15,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity_coverage[14,-c(1,2)])), col = 4)
lines(as.vector(unlist(player_urnsizes_coverage_baseline[4,-1])), col = 5, lty = "dotted")

plot.new()
# Create a legend manually using the legend() function
legend("center", legend = c("Adaptivity: ", "U(0,1)", "N(0,0.5)", "N(0,0.25)", "N(0.85,0.5)"),
       col = c(1,1:4), lty = c(NA, 1,1,1,1), bty = "n", 
       cex = 1.3, horiz = TRUE)

dev.off()

#mean absolute prediction error
urnXadaptivity_pred = sim1_output %>%
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("pred"), ~ mean(.))) %>%  
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

#saving plot 
file_path = file.path("figures/urnXadaptivity_pred.png")
png(file_path, width = 8.75, height = 7.5, units = "in", res = 300)

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4,
                5,5,5,5), 5, 4, byrow = TRUE))
plot(as.vector(unlist(urnXadaptivity_pred[4,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "Mean Absolute Prediction Error", main = "Urn size = 8")
lines(as.vector(unlist(urnXadaptivity_pred[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity_pred[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity_pred[2,-c(1,2)])), col = 4)

plot(as.vector(unlist(urnXadaptivity_pred[8,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "Mean Absolute Prediction Error", main = "Urn size = 16")
lines(as.vector(unlist(urnXadaptivity_pred[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity_pred[7,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity_pred[6,-c(1,2)])), col = 4)

plot(as.vector(unlist(urnXadaptivity_pred[12,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "Mean Absolute Prediction Error", main = "Urn size = 32")
lines(as.vector(unlist(urnXadaptivity_pred[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity_pred[11,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity_pred[10,-c(1,2)])), col = 4)

plot(as.vector(unlist(urnXadaptivity_pred[16,-c(1,2)])), type = "l", ylim = c(0, 1), ylab = "Mean Absolute Prediction Error", main = "Urn size = 64")
lines(as.vector(unlist(urnXadaptivity_pred[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity_pred[15,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity_pred[14,-c(1,2)])), col = 4)

plot.new()
# Create a legend manually using the legend() function
legend("center", legend = c("Adaptivity: ", "U(0,1)", "N(0,0.5)", "N(0,0.25)", "N(0.85,0.5)"),
       col = c(1,1:4), lty = c(NA, 1,1,1,1), bty = "n", 
       cex = 1.3, horiz = TRUE)
dev.off()


################################################################################
#INTERACTION EFFECT: Player urn sizes * player true value * adaptivity 
################################################################################

persontrueXurnsize = sim1_output %>% 
  group_by(true_value, player_urn_size, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, adapt, true_value, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(adapt, true_value, player_urn_size)

file_path = file.path("figures/true_valXurnsizeXadaptivity.png")
png(file_path, width = 8.5, height = 7, units = "in", res = 400)

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4,
                5,5,5,5), 5, 4, byrow = TRUE))

plot(rep(0.1, times = 500), type = 'l', 
     lty = "dotted", ylim = c(0,1), ylab = "Ability", main = "U(0,1)")
for(tv in seq(0.2,0.9,0.1)){
  print(tv)
  lines(rep(tv, times = 500), lty = "dotted")
}

#n_adaptive
persontrueXurnsize_helper = persontrueXurnsize %>%
  filter(adapt == "n_adaptive") %>%
  select(-adapt)
for(i in 1:nrow(persontrueXurnsize_helper)){
  lines(unlist(persontrueXurnsize_helper[i,-c(1,2)]), col =  ifelse(i %% 4 != 0, i %% 4, 4))
}

plot(rep(0.1, times = 500), type = 'l', 
     lty = "dotted", ylim = c(0,1), ylab = "Ability", main = "N(0,0.5)")
for(tv in seq(0.2,0.9,0.1)){
  print(tv)
  lines(rep(tv, times = 500), lty = "dotted")
}


#adaptive 50
persontrueXurnsize_helper = persontrueXurnsize %>%
  filter(adapt == "adaptive50") %>%
  select(-adapt)
for(i in 1:nrow(persontrueXurnsize_helper)){
  lines(unlist(persontrueXurnsize_helper[i,-c(1,2)]), col = ifelse(i %% 4 != 0, i %% 4, 4))
}


plot(rep(0.1, times = 500), type = 'l', 
     lty = "dotted", ylim = c(0,1), ylab = "Ability", main = "N(0,0.25)")
for(tv in seq(0.2,0.9,0.1)){
  print(tv)
  lines(rep(tv, times = 500), lty = "dotted")
}


#adaptive sigma
persontrueXurnsize_helper = persontrueXurnsize %>%
  filter(adapt == "adaptive_sigma") %>%
  select(-adapt)
for(i in 1:nrow(persontrueXurnsize_helper)){
  lines(unlist(persontrueXurnsize_helper[i,-c(1,2)]), col =  ifelse(i %% 4 != 0, i %% 4, 4))
}


plot(rep(0.1, times = 500), type = 'l', 
     lty = "dotted", ylim = c(0,1), ylab = "Ability", main = "N(0.85,0.5)")
for(tv in seq(0.2,0.9,0.1)){
  print(tv)
  lines(rep(tv, times = 500), lty = "dotted")
}


#adaptive 70
persontrueXurnsize_helper = persontrueXurnsize %>%
  filter(adapt == "adaptive70") %>%
  select(-adapt)
for(i in 1:nrow(persontrueXurnsize_helper)){
  lines(unlist(persontrueXurnsize_helper[i,-c(1,2)]), col =  ifelse(i %% 4 != 0, i %% 4, 4))
}

plot.new()
# Create a legend manually using the legend() function
legend("center", legend = c("Player Urn Sizes: ","8", "16", "32", "64"),
       col = c(1,1:4), lty = c(NA, 1,1,1,1), bty = "n", 
       cex = 1.3, horiz = TRUE)

dev.off()

