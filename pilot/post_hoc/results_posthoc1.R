library(tidyverse)
library(Hmisc)

set.seed(13181913)
################################################################################
#READING THE SIMULATION OUTPUTS
################################################################################
sim_posthoc = readRDS("sim_posthoc1.rds")

################################################################################
#Calculating mean squared error across all conditions
################################################################################
sqe_output = sim_posthoc %>% 
  mutate(across(starts_with("iter"), ~ (. - true_value)^2))

helper = (sqe_output[,6:504] - sqe_output[,5:503])
change_sqe = sqe_output[,-c(504:1007)]
change_sqe[, 5:503] = helper
rm(helper)

################################################################################
#Calculating coverage across all conditions
################################################################################
urn_sizes = c(rep(64, 65))
urnings_values =  c(seq(0,64,1))
urn_sizes_helper = rep(urn_sizes, each = 9)
urnings_values_helper = rep(urnings_values, each = 9)
true_vals = rep(seq(0.1, 0.9, 0.1), times = length(urn_sizes))
confint_matrix = cbind(urn_sizes_helper, urnings_values_helper, true_vals)
confint_matrix = cbind(confint_matrix, matrix(0, nrow = nrow(confint_matrix), ncol = 3))
confint_matrix[, 4:5] = binconf(confint_matrix[,2], confint_matrix[,1], method = "wilson")[,2:3]
confint_matrix[,6] = ifelse(confint_matrix[,3] < confint_matrix[,5]  & confint_matrix[,3] > confint_matrix[,4], 1, 0)
rm(urn_sizes, urnings_values, urn_sizes_helper, urnings_values_helper, true_vals)

sim1_output_helper =  sim_posthoc
sim1_output_helper = sim1_output_helper %>%
  mutate_at(1, as.integer) %>%
  mutate(across(starts_with("iter"), ~ (. * player_urn_size))) 

for(i in 1:nrow(confint_matrix)){
  cond_urn = sim1_output_helper[,1] == confint_matrix[i ,1]
  cond_true_val = sim1_output_helper[,7] == confint_matrix[i ,3]
  indexes = which(cond_urn & cond_true_val)
  rm(cond_urn, cond_true_val)
  helper = sim1_output_helper[indexes, ]
  helper_iters = helper[, 6:505]
  rm(helper)
  helper_iters[helper_iters == confint_matrix[i ,2]] = confint_matrix[i ,6]
  sim1_output_helper[indexes,  6:505] = helper_iters
  print(i)
}
rm(helper_iters)

sim_coverage <- sim1_output_helper 
sim_coverage[, 6:505] = t(apply(sim_coverage[,  6:505],1,  cumsum)) / 500
rm(sim1_output_helper)

################################################################################
#MAIN EFFECT: Percentage of new players 
################################################################################
par(mfrow = c(1,1))
percentage_sqe = sqe_output %>% 
  filter(player_label == "new") %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)

plot(as.vector(unlist(percentage_sqe[1,-1])), type = "l", ylim = c(0, 0.17), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(percentage_sqe[i,-1])), col = i)
}

################################################################################
#Interaction EFFECT: Percentage of new players x adaptivity
################################################################################

#MSE
percXadapt_sqe = sqe_output %>% 
  filter(player_label == "new") %>%
  group_by(player_percent, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,adapt, starts_with("iter")) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)

file_path = file.path("figures/percXadapt_mse.png")
png(file_path, width = 8.75, height = 7.5, units = "in", res = 300)
layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4,
                5,5,5,5), 5, 4, byrow = TRUE))

plot(as.vector(unlist(percXadapt_sqe[4,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE (New Players)", main = "Percentage of new 10%")
lines(as.vector(unlist(percXadapt_sqe[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(percXadapt_sqe[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(percXadapt_sqe[2,-c(1,2)])), col = 4)


plot(as.vector(unlist(percXadapt_sqe[8,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE (New Players)", main = "Percentage of new 30%")
lines(as.vector(unlist(percXadapt_sqe[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(percXadapt_sqe[7,-c(1,2)])), col = 3)
lines(as.vector(unlist(percXadapt_sqe[6,-c(1,2)])), col = 4)

plot(as.vector(unlist(percXadapt_sqe[12,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE (New Players)", main = "Percentage of new 50%")
lines(as.vector(unlist(percXadapt_sqe[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(percXadapt_sqe[11,-c(1,2)])), col = 3)
lines(as.vector(unlist(percXadapt_sqe[10,-c(1,2)])), col = 4)

plot(as.vector(unlist(percXadapt_sqe[16,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE (New Players)", main = "Percentage of new 100%")
lines(as.vector(unlist(percXadapt_sqe[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(percXadapt_sqe[15,-c(1,2)])), col = 3)
lines(as.vector(unlist(percXadapt_sqe[14, -c(1,2)])), col = 4)

plot.new()
# Create a legend manually using the legend() function
legend("center", legend = c("Adaptivity: ", "U(0,1)", "N(0,0.5)", "N(0,0.25)", "N(0.85,0.5)"),
       col = c(1,1:4), lty = c(NA, 1,1,1,1), bty = "n", 
       cex = 1.3, horiz = TRUE)

dev.off()

#BIAS
percXadapt_bias = sim_posthoc %>%
  filter(player_label == "new") %>%
  group_by(player_percent, adapt, true_value) %>%
  summarise(across(starts_with("iter"), ~ (mean(.) - true_value)^2)) %>%
  ungroup(true_value) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent, adapt)

file_path = file.path("figures/percXadapt_bias.png")
png(file_path, width = 8.75, height = 7.5, units = "in", res = 300)
layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4,
                5,5,5,5), 5, 4, byrow = TRUE))

plot(as.vector(unlist(percXadapt_bias[4,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Bias (New Players)", main = "Percentage of new 10%")
lines(as.vector(unlist(percXadapt_bias[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(percXadapt_bias[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(percXadapt_bias[2,-c(1,2)])), col = 4)


plot(as.vector(unlist(percXadapt_bias[8,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Bias (New Players)", main = "Percentage of new 30%")
lines(as.vector(unlist(percXadapt_bias[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(percXadapt_bias[7,-c(1,2)])), col = 3)
lines(as.vector(unlist(percXadapt_bias[6,-c(1,2)])), col = 4)

plot(as.vector(unlist(percXadapt_bias[12,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Bias (New Players)", main = "Percentage of new 50%")
lines(as.vector(unlist(percXadapt_bias[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(percXadapt_bias[11,-c(1,2)])), col = 3)
lines(as.vector(unlist(percXadapt_bias[10,-c(1,2)])), col = 4)

plot(as.vector(unlist(percXadapt_bias[16,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Bias (New Players)", main = "Percentage of new 100%")
lines(as.vector(unlist(percXadapt_bias[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(percXadapt_bias[15,-c(1,2)])), col = 3)
lines(as.vector(unlist(percXadapt_bias[14, -c(1,2)])), col = 4)

plot.new()
# Create a legend manually using the legend() function
legend("center", legend = c("Adaptivity: ", "U(0,1)", "N(0,0.5)", "N(0,0.25)", "N(0.85,0.5)"),
       col = c(1,1:4), lty = c(NA, 1,1,1,1), bty = "n", 
       cex = 1.3, horiz = TRUE)

dev.off()
################################################################################
#INTERACTION EFFECT: Player percent * player true value * adaptivity 
################################################################################
persontrueXpercXadapt = sim_posthoc %>% 
  filter(player_label == "new")%>%
  group_by(true_value, player_percent, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent, adapt, true_value, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(adapt, true_value, player_percent)

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
persontrueXpercXadapt_helper = persontrueXpercXadapt %>%
  filter(adapt == "n_adaptive") %>%
  select(-adapt)
for(i in 1:nrow(persontrueXpercXadapt_helper)){
  lines(unlist(persontrueXpercXadapt_helper[i,-c(1,2)]), col =  ifelse(i %% 4 != 0, i %% 4, 4))
}

#adaptive50
plot(rep(0.1, times = 500), type = 'l', 
     lty = "dotted", ylim = c(0,1), ylab = "Ability", main = "N(0,0.5)")
for(tv in seq(0.2,0.9,0.1)){
  print(tv)
  lines(rep(tv, times = 500), lty = "dotted")
}

#n_adaptive
persontrueXpercXadapt_helper = persontrueXpercXadapt %>%
  filter(adapt == "adaptive50") %>%
  select(-adapt)
for(i in 1:nrow(persontrueXpercXadapt_helper)){
  lines(unlist(persontrueXpercXadapt_helper[i,-c(1,2)]), col =  ifelse(i %% 4 != 0, i %% 4, 4))
}



