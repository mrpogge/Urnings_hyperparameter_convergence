library(tidyverse)
library(Hmisc)

set.seed(13181913)
################################################################################
#READING THE SIMULATION OUTPUTS
################################################################################
sim_posthoc = readRDS("sim_posthoc2.rds")

################################################################################
#Calculating mean squared error across all conditions
################################################################################
sqe_output = sim_posthoc %>% 
  mutate(across(starts_with("iter"), ~ (. - true_value)^2))

helper = abs(sqe_output[,9:507] - sqe_output[,8:506])
change_sqe = sqe_output[,-c(508:1007)]
change_sqe[, 8:506] = helper
rm(helper)


################################################################################
#MAIN EFFECT: Percentage of new players 
################################################################################
layout(matrix(c(1,1,1,1,
                2,2,2,2), 2, 4, byrow = TRUE))
percentage_sqe = sqe_output %>% 
filter(player_label == "new", true_dist_player == "normal_quantiles",  true_dist_item == "normal_quantiles") %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)

plot(as.vector(unlist(percentage_sqe[1,-1])), type = "l", ylim = c(0, 0.17), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(percentage_sqe[i,-1])), col = i)
}

percentage_sqe = sqe_output %>% 
  filter(player_label == "new", true_dist_player == "normal_quantiles",  true_dist_item == "normal_quantiles_ncentral") %>%
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
#Interaction EFFECT: Percentage of new players x adaptivity (NQ NQ)
################################################################################

#MSE
percXadapt_sqe = sqe_output %>% 
  filter(player_label == "new", true_dist_player == "normal_quantiles",  true_dist_item == "normal_quantiles") %>%
  group_by(player_percent, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,adapt, starts_with("iter")) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)

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


################################################################################
#Interaction EFFECT: Percentage of new players x adaptivity (DU NQ)
################################################################################
#MSE
percXadapt_sqe = sqe_output %>% 
  filter(player_label == "new", true_dist_player == "discrete_uniform",  true_dist_item == "normal_quantiles") %>%
  group_by(player_percent, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,adapt, starts_with("iter")) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)

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


################################################################################
#Interaction EFFECT: Percentage of new players x adaptivity (NQNC NQ)
################################################################################
percXadapt_sqe = sqe_output %>% 
  filter(player_label == "new", true_dist_player == "normal_quantiles",  true_dist_item == "normal_quantiles_ncentral") %>%
  group_by(player_percent, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,adapt, starts_with("iter")) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)

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


################################################################################
#Interaction: Percentage of new players x true dist player
################################################################################
layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4), 4, 4, byrow = TRUE))

percentage_sqe = sqe_output %>% 
  filter(player_label == "new", true_dist_player == "discrete_uniform") %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)


plot(as.vector(unlist(percentage_sqe[1,-1])), type = "l", ylim = c(0, 0.1), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(percentage_sqe[i,-1])), col = i)
}

percentage_sqe = sqe_output %>% 
  filter(player_label == "new", true_dist_player == "uniform") %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)


plot(as.vector(unlist(percentage_sqe[1,-1])), type = "l", ylim = c(0, 0.1), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(percentage_sqe[i,-1])), col = i)
}

percentage_sqe = sqe_output %>% 
  filter(player_label == "new", true_dist_player == "normal_quantiles") %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)


plot(as.vector(unlist(percentage_sqe[1,-1])), type = "l", ylim = c(0, 0.1), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(percentage_sqe[i,-1])), col = i)
}

percentage_sqe = sqe_output %>% 
  filter(player_label == "new", true_dist_player == "normal_quantiles_ncentral") %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)


plot(as.vector(unlist(percentage_sqe[1,-1])), type = "l", ylim = c(0, 0.1), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(percentage_sqe[i,-1])), col = i)
}

################################################################################
#Interaction: Percentage of new players x true dist item
################################################################################
layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4), 4, 4, byrow = TRUE))

percentage_sqe = sqe_output %>% 
  filter(player_label == "new", true_dist_item == "discrete_uniform") %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)


plot(as.vector(unlist(percentage_sqe[1,-1])), type = "l", ylim = c(0, 0.1), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(percentage_sqe[i,-1])), col = i)
}

percentage_sqe = sqe_output %>% 
  filter(player_label == "new", true_dist_item == "uniform") %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)


plot(as.vector(unlist(percentage_sqe[1,-1])), type = "l", ylim = c(0, 0.1), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(percentage_sqe[i,-1])), col = i)
}

percentage_sqe = sqe_output %>% 
  filter(player_label == "new", true_dist_item == "normal_quantiles") %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)


plot(as.vector(unlist(percentage_sqe[1,-1])), type = "l", ylim = c(0, 0.1), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(percentage_sqe[i,-1])), col = i)
}

percentage_sqe = sqe_output %>% 
  filter(player_label == "new", true_dist_item == "normal_quantiles_ncentral") %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)


plot(as.vector(unlist(percentage_sqe[1,-1])), type = "l", ylim = c(0, 0.1), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(percentage_sqe[i,-1])), col = i)
}

################################################################################
#Interaction: Percentage of true dist player x true dist item
################################################################################
layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4), 4, 4, byrow = TRUE))

percentage_sqe = change_sqe %>% 
  filter(player_label == "new") %>%
  group_by(true_dist_player, true_dist_item) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(true_dist_player, true_dist_item,starts_with('iter'))




plot(as.vector(unlist(percentage_sqe[1,-c(1,2)])), type = "l", ylim = c(0, 0.01), ylab = "MSE (New Players)", main = "N(0,0.5)")
lines(as.vector(unlist(percentage_sqe[4,-c(1,2)])), col = 2)
lines(as.vector(unlist(percentage_sqe[2,-c(1,2)])), col = 3)
lines(as.vector(unlist(percentage_sqe[3,-c(1,2)])), col = 4)


plot(as.vector(unlist(percentage_sqe[5,-c(1,2)])), type = "l",  ylim = c(0, 0.01), ylab = "MSE (New Players)", main = "N(0.85, 0.5)")
lines(as.vector(unlist(percentage_sqe[8,-c(1,2)])), col = 2)
lines(as.vector(unlist(percentage_sqe[6,-c(1,2)])), col = 3)
lines(as.vector(unlist(percentage_sqe[7,-c(1,2)])), col = 4)

plot(as.vector(unlist(percentage_sqe[9,-c(1,2)])), type = "l",  ylim = c(0, 0.01), ylab = "MSE (New Players)", main = "N(0,0.25)")
lines(as.vector(unlist(percentage_sqe[12,-c(1,2)])), col = 2)
lines(as.vector(unlist(percentage_sqe[10,-c(1,2)])), col = 3)
lines(as.vector(unlist(percentage_sqe[11,-c(1,2)])), col = 4)

plot(as.vector(unlist(percentage_sqe[13,-c(1,2)])), type = "l",  ylim = c(0, 0.01), ylab = "MSE (New Players)", main = "U(0,1)")
lines(as.vector(unlist(percentage_sqe[16,-c(1,2)])), col = 2)
lines(as.vector(unlist(percentage_sqe[14,-c(1,2)])), col = 3)
lines(as.vector(unlist(percentage_sqe[15, -c(1,2)])), col = 4)
