library(tidyverse)
library(Hmisc)

set.seed(13181913)
dir.create("figures", showWarnings = FALSE)
################################################################################
#READING THE SIMULATION OUTPUTS
################################################################################
sim1_urnings1_central = readRDS("sim1_urnings1_central.rds")
sim1_urnings1_better = readRDS("sim1_urnings1_better.rds")
sim1_urnings1_worse = readRDS("sim1_urnings1_worse.rds")
sim1_urnings2_central = readRDS("sim1_urnings2_central.rds")
sim1_urnings2_better = readRDS("sim1_urnings2_better.rds")
sim1_urnings2_worse = readRDS("sim1_urnings2_worse.rds")

results_u1 = rbind(sim1_urnings1_central, 
                sim1_urnings1_better, 
                sim1_urnings1_worse)

results = rbind(sim1_urnings2_central, 
                   sim1_urnings2_better, 
                   sim1_urnings2_worse)

rm(sim1_urnings1_central, 
   sim1_urnings1_better, 
   sim1_urnings1_worse, 
   sim1_urnings2_central, 
   sim1_urnings2_better, 
   sim1_urnings2_worse)

################################################################################
#Calculating mean squared error across all conditions
################################################################################
results = results %>% 
  mutate(across(starts_with("iter"), ~ (. - true_value)^2))

results_u1 = results_u1 %>% 
  mutate(across(starts_with("iter"), ~ (. - true_value)^2))


#saveRDS(results, "sim1_MSE.rds")
################################################################################
#Creating baselines
################################################################################
true_vals = results[,"true_value"]
urn_sizes = results[,"player_urn_size"]
true_vals = unique(true_vals)
urn_sizes = as.numeric(unique(urn_sizes))

baseline_conditions = matrix(0, nrow = length(true_vals)* 4, ncol = 5)
baseline_conditions[,1] = rep(true_vals, each = 4)
baseline_conditions[,2] = rep(urn_sizes, times = length(true_vals))

baseline = matrix(0, nrow = nrow(baseline_conditions), ncol = 500)
for(i in 1:ncol(baseline)){
  baseline[i, ] = ((rbinom(500, baseline_conditions[i,2], baseline_conditions[i,1]) / baseline_conditions[i,2]) - baseline_conditions[i,1])^2
  baseline_conditions[i,4:5] = t.test(baseline[i, ])$conf.int[1:2]
}

baseline_conditions[,3] = rowMeans(baseline)

################################################################################
#MAIN EFFECT: Algorithm type (Urnings1 vs Urnings2)
################################################################################
#Mean squared error  
urnings1 = results_u1 %>% 
  filter(paired_update == "TRUE") %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(starts_with('iter'))

urnings2 = results %>% 
  filter(paired_update == "TRUE") %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(starts_with('iter'))

baseline_maineffects = colMeans(baseline)

plot(as.vector(unlist(urnings1)), type = "l", ylim = c(0, 0.05), ylab = "Mean Squared Error")
lines(as.vector(unlist(urnings2)), col = 2)
lines(baseline_maineffects, col = 6, lty = "dotted")


################################################################################
#MAIN EFFECT: Paired_update (No paired update vs Paired update)
################################################################################
layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4), 4, 4, byrow = TRUE))

puvsnpu = results %>%
  group_by(paired_update) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(paired_update,starts_with('iter'))

plot(as.vector(unlist(puvsnpu[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Main effect")
lines(as.vector(unlist(puvsnpu[2,-1])), col = 2)
lines(baseline_maineffects, col = 6, lty = "dotted")

#interaction between dist_type and paired update
puvsnpu_dt = results %>%
  group_by(paired_update, dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, paired_update,starts_with('iter'))

plot(as.vector(unlist(puvsnpu_dt[1,-c(1:2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Mean = 1")
lines(as.vector(unlist(puvsnpu_dt[4,-c(1:2)])), col = 2)

plot(as.vector(unlist(puvsnpu_dt[2,-c(1:2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Mean = 0")
lines(as.vector(unlist(puvsnpu_dt[5,-c(1:2)])), col = 2)

plot(as.vector(unlist(puvsnpu_dt[3,-c(1:2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Mean = -1")
lines(as.vector(unlist(puvsnpu_dt[6,-c(1:2)])), col = 2)


################################################################################
#MAIN EFFECT: Item urn sizes (16 vs 32 vs 64 vs 128) 
################################################################################
item_urnsizes = results %>%
  filter(paired_update == "TRUE") %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(item_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

plot(as.vector(unlist(item_urnsizes[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(item_urnsizes[i,-1])), col = i)
}
lines(baseline_maineffects, col = 6, lty = "dotted")

#interaction between dist type and paired update

################################################################################
#MAIN EFFECT: Player urn sizes (8 vs 16 vs 32 vs 64)
################################################################################
player_urnsizes = results %>% 
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

baseline_urnsizes = matrix(0, nrow = 4, ncol = 500)
baseline_urnsizes[1, ] = colMeans(baseline[baseline_conditions[,2] == 8,])
baseline_urnsizes[2, ] = colMeans(baseline[baseline_conditions[,2] == 16,])
baseline_urnsizes[3, ] = colMeans(baseline[baseline_conditions[,2] == 32,])
baseline_urnsizes[4, ] = colMeans(baseline[baseline_conditions[,2] == 64,])

plot(as.vector(unlist(player_urnsizes[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(player_urnsizes[i,-1])), col = i)
}

for(i in 1:4){
  lines(baseline_urnsizes[i, ], col = 6, lty = "dotted")
}

################################################################################
#MAIN EFFECT: cold start
################################################################################

player_percent = results %>%
  filter(player_label == 1, paired_update == "TRUE") %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent, starts_with('iter')) 

plot(as.vector(unlist(player_percent[1,-1])), type = "l")
lines(as.vector(unlist(player_percent[3,-1])), col = 2)
lines(as.vector(unlist(player_percent[2,-1])), col = 3)
lines(as.vector(unlist(player_percent[4,-1])), col = 4)

################################################################################
#INTERACTION EFFECT: Player urn sizes * adaptivity 
################################################################################
urnXadaptivity = results %>%
  filter(player_percent != 999) %>%
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, adapt, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4), 4, 4, byrow = TRUE))

plot(as.vector(unlist(urnXadaptivity[4,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "Urn size = 8")
lines(as.vector(unlist(urnXadaptivity[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[2,-c(1,2)])), col = 4)

plot(as.vector(unlist(urnXadaptivity[8,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "Urn size = 16")
lines(as.vector(unlist(urnXadaptivity[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[7,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[6,-c(1,2)])), col = 4)

plot(as.vector(unlist(urnXadaptivity[12,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "Urn size = 32")
lines(as.vector(unlist(urnXadaptivity[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[11,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[10,-c(1,2)])), col = 4)

plot(as.vector(unlist(urnXadaptivity[16,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "Urn size = 64")
lines(as.vector(unlist(urnXadaptivity[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[15,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[14,-c(1,2)])), col = 4)

################################################################################
#INTERACTION EFFECT: Player percent * adaptivity 
################################################################################
percentXadaptivity = results %>%
  filter(player_label != 0) %>%
  group_by(player_percent, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent, adapt, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4), 4, 4, byrow = TRUE))

plot(as.vector(unlist(percentXadaptivity[4,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "New players 10%")
lines(as.vector(unlist(percentXadaptivity[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(percentXadaptivity[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(percentXadaptivity[2,-c(1,2)])), col = 4)

plot(as.vector(unlist(percentXadaptivity[8,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "New players 50%")
lines(as.vector(unlist(percentXadaptivity[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(percentXadaptivity[7,-c(1,2)])), col = 3)
lines(as.vector(unlist(percentXadaptivity[6,-c(1,2)])), col = 4)

plot(as.vector(unlist(percentXadaptivity[12,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "New players 100%")
lines(as.vector(unlist(percentXadaptivity[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(percentXadaptivity[11,-c(1,2)])), col = 3)
lines(as.vector(unlist(percentXadaptivity[10,-c(1,2)])), col = 4)

plot(as.vector(unlist(percentXadaptivity[16,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "Total cold start")
lines(as.vector(unlist(percentXadaptivity[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(percentXadaptivity[15,-c(1,2)])), col = 3)
lines(as.vector(unlist(percentXadaptivity[14,-c(1,2)])), col = 4)

################################################################################
#INTERACTION EFFECT: dist_type * adaptivity 
################################################################################
distxadapt = results %>%
  filter(paired_update == "TRUE") %>%
  group_by(dist_type, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, adapt, starts_with('iter'))

layout(matrix(c(1,1,2,2,3,3,1,1,2,2,3,3), 2, 6, byrow = TRUE))
plot(as.vector(unlist(distxadapt[4,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "Mean = 1")
lines(as.vector(unlist(distxadapt[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(distxadapt[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(distxadapt[2,-c(1,2)])), col = 4)

plot(as.vector(unlist(distxadapt[8,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "Mean = 0")
lines(as.vector(unlist(distxadapt[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(distxadapt[7,-c(1,2)])), col = 3)
lines(as.vector(unlist(distxadapt[6,-c(1,2)])), col = 4)

plot(as.vector(unlist(distxadapt[12,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "MSE", main = "Mean = -1")
lines(as.vector(unlist(distxadapt[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(distxadapt[11,-c(1,2)])), col = 3)
lines(as.vector(unlist(distxadapt[10,-c(1,2)])), col = 4)






