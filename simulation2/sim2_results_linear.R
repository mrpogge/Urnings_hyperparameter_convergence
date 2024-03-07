library(tidyverse)
library(grid)
library(patchwork)

################################################################################
#simulation with linear change
################################################################################
sim2_linear_better = readRDS("output/sim2_linear_better.rds")
sim2_linear_worse = readRDS("output/sim2_linear_worse.rds")
sim2_linear_central = readRDS("output/sim2_linear_central.rds")

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

################################################################################
#Main effect change
################################################################################
# this is the analysis we are looking for
change_me = sim2_linear %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

true_change_me = change_matrix_avg %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

df_h21 = rbind(change_me, true_change_me) %>%
  ungroup() %>%
  mutate(res_type = c(rep("a", times = nrow(change_me)),
                      rep("b", times = nrow(true_change_me)))) %>%
  relocate(res_type, .before = 1) %>%
  pivot_longer(cols = starts_with("iter"),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = as.numeric(gsub("iter", "", variable))) %>%
  mutate(amount_of_change = as.character(amount_of_change))

df_h21$amount_of_change = factor(df_h21$amount_of_change,
                                levels = c("-0.001", "0", "0.001", "0.002", "0.004"),
                                labels = c("-0.5", "0", "0.5", "1", "2"))

plot_h21 = df_h21 %>%
  ggplot(aes(x = variable, y = value, color = amount_of_change, linetype = res_type)) +
  geom_line() +
  labs(x = "Iterations", y = "Mean Ratings") +
  scale_linetype_manual(values = c("a" = "solid", "b" = "dotted"),
                        name = "",
                        labels = c("Ratings", "True")) +
  scale_color_manual(values = c("-0.5" = "black",
                                "0" = "red",
                                "0.5" = "green",
                                "1" = "blue",
                                "2" = "purple"),
                     name = "Student urn sizes") +
  guides(color = guide_legend(order = 2),
         linetype = guide_legend(order = 1)) + 
  jtools::theme_apa(legend.font.size = 10) 

mmse_neg = mean(as.numeric(unlist(change_me[1,c(401:500)])) - as.numeric(unlist(true_change_me[1,c(401:500)])))
mmse_0 = mean(as.numeric(unlist(change_me[2,c(401:500)])) - as.numeric(unlist(true_change_me[2,c(401:500)])))
mmse_half = mean(as.numeric(unlist(change_me[3,c(401:500)])) - as.numeric(unlist(true_change_me[3,c(401:500)])))
mmse_one = mean(as.numeric(unlist(change_me[4,c(401:500)])) - as.numeric(unlist(true_change_me[4,c(401:500)])))
mmse_two = mean(as.numeric(unlist(change_me[5,c(401:500)])) - as.numeric(unlist(true_change_me[5,c(401:500)])))
print(c(mmse_neg, mmse_0, mmse_half, mmse_one, mmse_two))

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
  filter(adapt == "adaptive70") %>%
  group_by(dist_type, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, amount_of_change, starts_with("iter"))

true_changeXdist = change_matrix_avg %>%
  group_by(dist_type, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, amount_of_change, starts_with("iter"))


df_h22 = rbind(changeXdist, true_changeXdist) %>%
  ungroup() %>%
  mutate(res_type = c(rep("a", times = nrow(changeXdist)),
                      rep("b", times = nrow(true_changeXdist)))) %>%
  relocate(res_type, .before = 1) %>%
  pivot_longer(cols = starts_with("iter"),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = as.numeric(gsub("iter", "", variable)))

df_h22$dist_type = factor(df_h22$dist_type,
                          levels = c("better","central", "worse"),
                          labels = c("N(1,1)", "N(0,1)", "N(-1,1)"))

df_h22$amount_of_change = factor(df_h22$amount_of_change,
                                 levels = c("-0.001", "0", "0.001", "0.002", "0.004"),
                                 labels = c("-0.5", "0", "0.5", "1", "2"))

df_h22 %>%
  ggplot(aes(x = variable, y = value, color = amount_of_change, linetype = res_type)) +
  facet_wrap(dist_type ~ ., nrow = 1) +
  geom_line() +
  labs(x = "Iterations", y = "Mean Ratings") +
  scale_linetype_manual(values = c("a" = "solid", "b" = "dotted"),
                        name = "",
                        labels = c("Ratings", "True")) +
  scale_color_manual(values = c("-0.5" = "black",
                                "0" = "red",
                                "0.5" = "green",
                                "1" = "blue",
                                "2" = "purple"),
                     name = "Student urn sizes") +
  guides(color = guide_legend(order = 2),
         linetype = guide_legend(order = 1)) + 
  jtools::theme_apa(legend.font.size = 10) 


################################################################################
#deviation from the baseline
################################################################################
layout(matrix(c(1), 1, 1, byrow = TRUE))
baseline_linear = readRDS("output/sim2_baseline.rds")

baseline_linear = baseline_linear / as.numeric(sim2_linear[,2])
baseline_linear = (baseline_linear - change_matrix_linear) ^ 2
baseline_linear = cbind(sim2_linear[,1:6], baseline_linear)
colnames(baseline_linear) = c(colnames(sim2_linear[,1:6]), paste0("iter", c(1:500)))

#adding the posthoc urn sizes
################################################################################
#simulation with linear change
################################################################################
post_hoc_urnsize = readRDS("output/post_hoc_urnsize.rds")
post_hoc_urnsize_better = readRDS("output/post_hoc_urnsize_better.rds")
post_hoc_urnsize_worse = readRDS("output/post_hoc_urnsize_worse.rds")

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
baseline_post_hoc = readRDS("output/post_hoc_urnsize_baseline.rds")
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

urn_size_difference = as_tibble(cbind(urn_size_me[,1], urn_size_me[,-1] - b_urn_size_me[,-1]))
post_hoc_difference = as_tibble(cbind(post_hoc_me[,1], post_hoc_me[,-1] - b_post_hoc_me[,-1]))

df_h23 = rbind(urn_size_me, 
               post_hoc_me) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("iter"),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = as.numeric(gsub("iter", "", variable)))

df_h23B = rbind(urn_size_difference, 
               post_hoc_difference) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("iter"),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = as.numeric(gsub("iter", "", variable)))

df_h23$player_urn_size = factor(df_h23$player_urn_size,
                          levels = c("8","16", "32", "64", "80", "96", "112", "128"),
                          labels = c("8","16", "32", "64", "80", "96", "112", "128"))
df_h23B$player_urn_size = factor(df_h23B$player_urn_size,
                                levels = c("8","16", "32", "64", "80", "96", "112", "128"),
                                labels = c("8","16", "32", "64", "80", "96", "112", "128"))

plot_h23 = df_h23 %>%
  ggplot(aes(x = variable, y = value, color = player_urn_size)) +
  geom_line() +
  labs(x = "Iterations", y = "MSE") +
  scale_color_manual(values = c("8" = "black",
                                "16" = "red",
                                "32" = "green",
                                "64" = "blue",
                                "80" = "purple",
                                "96" = "gold",
                                "112" = "grey",
                                "128" = "aquamarine3"),
                     name = "Student urn sizes") +
  jtools::theme_apa(legend.font.size = 10) 

plot_h23B = df_h23B %>%
  ggplot(aes(x = variable, y = value, color = player_urn_size)) +
  geom_line() +
  labs(x = "Iterations", y = "MSE Difference") +
  scale_color_manual(values = c("8" = "black",
                                "16" = "red",
                                "32" = "green",
                                "64" = "blue",
                                "80" = "purple",
                                "96" = "gold",
                                "112" = "grey",
                                "128" = "aquamarine3"),
                     name = "Student urn sizes") +
  jtools::theme_apa(legend.font.size = 10) 

plot_h23 + plot_h23B + plot_layout(ncol = 2, guides = "collect")


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

urn_worse_difference = cbind(urn_size_worse[,1], urn_size_worse[,-1] - b_urn_size_worse[,-1])
post_hoc_worse_difference = cbind(post_hoc_worse[,1],post_hoc_worse[,-1] - b_post_hoc_worse[ ,-1])
  
df_h23 = rbind(urn_size_worse, 
               post_hoc_worse) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("iter"),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = as.numeric(gsub("iter", "", variable)))

df_h23B = rbind(urn_worse_difference, 
                post_hoc_worse_difference) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("iter"),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = as.numeric(gsub("iter", "", variable)))

df_h23$player_urn_size = factor(df_h23$player_urn_size,
                                levels = c("8","16", "32", "64", "80", "96", "112", "128"),
                                labels = c("8","16", "32", "64", "80", "96", "112", "128"))
df_h23B$player_urn_size = factor(df_h23B$player_urn_size,
                                 levels = c("8","16", "32", "64", "80", "96", "112", "128"),
                                 labels = c("8","16", "32", "64", "80", "96", "112", "128"))

plot_h23 = df_h23 %>%
  ggplot(aes(x = variable, y = value, color = player_urn_size)) +
  geom_line() +
  labs(x = "Iterations", y = "MSE") +
  scale_color_manual(values = c("8" = "black",
                                "16" = "red",
                                "32" = "green",
                                "64" = "blue",
                                "80" = "purple",
                                "96" = "gold",
                                "112" = "grey",
                                "128" = "aquamarine3"),
                     name = "Student urn sizes") +
  jtools::theme_apa(legend.font.size = 10) 

plot_h23B = df_h23B %>%
  ggplot(aes(x = variable, y = value, color = player_urn_size)) +
  geom_line() +
  labs(x = "Iterations", y = "MSE Difference") +
  scale_color_manual(values = c("8" = "black",
                                "16" = "red",
                                "32" = "green",
                                "64" = "blue",
                                "80" = "purple",
                                "96" = "gold",
                                "112" = "grey",
                                "128" = "aquamarine3"),
                     name = "Student urn sizes") +
  jtools::theme_apa(legend.font.size = 10) 

plot_h23 + plot_h23B + plot_layout(ncol = 2, guides = "collect")

################################################################################
#making tables
################################################################################

last_mean = function(v, last){
  return(mean(v[(length(v)-last-1):length(v)]))
}

table_mse_helper = linear_mse %>%
  group_by(dist_type, player_urn_size, adapt, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.,))) %>%
  select(dist_type, player_urn_size, adapt, amount_of_change,starts_with("iter"))

table_mse = cbind(table_mse_helper[,1:4], numeric(nrow(table_mse_helper)))
table_mse[,5] = rowMeans(table_mse_helper[, -c(1:4)])
colnames(table_mse)[5] = "mse"


best_us = matrix(0, nrow = 60, ncol = 4)
counter = 1
for(i in unique(table_mse$dist_type)){
  for(j in unique(table_mse$adapt)){
    for(k in unique(table_mse$amount_of_change)){
      
      condition = table_mse$dist_type == i & table_mse$adapt == j & table_mse$amount_of_change == k
      tab = table_mse[condition, ]
      best_us[counter, ] = c(i,j,k,unlist(tab[which.min(tab$mse),"player_urn_size"]))
      counter = counter + 1
    }
  }
}

table_baseline_mse_helper = baseline_linear %>%
  group_by(dist_type, player_urn_size, adapt, amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, player_urn_size, adapt, amount_of_change, starts_with("iter"))

table_baseline_mse = cbind(table_baseline_mse_helper[,1:4], numeric(nrow(table_baseline_mse_helper)))
table_baseline_mse[,5] = rowMeans(table_baseline_mse_helper[, -c(1:4)])

table_mse_diff = table_mse
table_mse_diff[,5] = table_mse[,5] - table_baseline_mse[,5]

table_balance = table_mse
table_balance[,5] = table_mse[,5] / (table_mse[,5] - table_baseline_mse[,5])

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
