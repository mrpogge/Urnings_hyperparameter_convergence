library(tidyverse)
library(Hmisc)
library(conover.test)

set.seed(13181913)

CI_lm = function(v){
  res = confint(lm(v ~ 1))[1:2]
  return(res)
}

hitting_time = function(v, upper, lower){
  hitting = upper > v & v > lower
  return(which(hitting)[1])
}
#dir.create("figures", showWarnings = FALSE)
################################################################################
#READING THE SIMULATION OUTPUTS
################################################################################
sim1_urnings1_central = readRDS("sim1_urnings1_central.rds")
sim1_urnings1_better = readRDS("sim1_urnings1_better.rds")
sim1_urnings1_worse = readRDS("sim1_urnings1_worse.rds")
sim1_urnings2_central = readRDS("sim1_urnings2_central.rds")
sim1_urnings2_better = readRDS("sim1_urnings2_better.rds")
sim1_urnings2_worse = readRDS("sim1_urnings2_worse.rds")

index = as.matrix(rep(seq(1,1536, 1), each = 900))
colnames(index) = "index"

results_u1 = cbind(index, rbind(sim1_urnings1_central, 
                                sim1_urnings1_better, 
                                sim1_urnings1_worse))

results = cbind(index, rbind(sim1_urnings2_central, 
                             sim1_urnings2_better, 
                             sim1_urnings2_worse))

#results_ht = cbind(index, rbind(sim1_urnings2_central, 
#                             sim1_urnings2_better, 
#                             sim1_urnings2_worse))

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
baseline = readRDS("sim1_baseline.rds")
baseline_MSE = baseline[[1]]
baseline_MSE_u1 = baseline[[2]]

colnames(baseline_MSE) = colnames(baseline_MSE_u1) = paste0("iter", 1:500)
baseline_MSE = cbind(results[,2:10], baseline_MSE)
baseline_MSE_u1 = cbind(results_u1[,2:10], baseline_MSE_u1)
#baseline differences
baseline_MSE = baseline_MSE %>% 
  mutate(across(starts_with("iter"), ~ (. - true_value)^2))

baseline_MSE_u1 = baseline_MSE_u1 %>% 
  mutate(across(starts_with("iter"), ~ (. - true_value)^2))



results = results %>% 
  group_by(player_urn_size,
           item_urn_size,
           adapt,
           player_percent,
           algo,
           paired_update,
           dist_type,
           player_label) %>%
  summarise(across(starts_with("iter"), ~ mean(.)))

results_u1 = results_u1 %>% 
  group_by(player_urn_size,
           item_urn_size,
           adapt,
           player_percent,
           algo,
           paired_update,
           dist_type,
           player_label) %>%
  summarise(across(starts_with("iter"), ~ mean(.)))



baseline_MSE = baseline_MSE %>% 
  group_by(player_urn_size,
           item_urn_size,
           adapt,
           player_percent,
           algo,
           paired_update,
           dist_type,
           player_label) %>%
  summarise(across(starts_with("iter"), ~ mean(.)))

baseline_MSE_u1 = baseline_MSE_u1 %>% 
  group_by(player_urn_size,
           item_urn_size,
           adapt,
           player_percent,
           algo,
           paired_update,
           dist_type,
           player_label) %>%
  summarise(across(starts_with("iter"), ~ mean(.)))

results = rbind(results, results_u1)
baseline_MSE = rbind(baseline_MSE, baseline_MSE_u1)
saveRDS(results, "collapsed_results.rds")
saveRDS(baseline_MSE, "collapsed_baseline_MSE.rds")
rm(results_u1, baseline_MSE_u1)
results = readRDS("collapsed_results.rds")
baseline_MSE = readRDS("collapsed_baseline_MSE.rds")

################################################################################
#hitting time
################################################################################
nplayers = 900
us_parameters = c(8,16,32,64)
dt_parameters = c("better", "worse", "central")
parameter_combinations = expand.grid(us_parameters, dt_parameters)
better_dist =  qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers),1,1)
central_dist = qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers))
worse_dist = qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers),-1,1)
dists = data.frame("better" = better_dist,
                   "central" = central_dist,
                   "worse" = worse_dist)
dists = exp(dists) / (1 + exp(dists))

rm(better_dist, central_dist, worse_dist, us_parameters, dt_parameters)

parameter_combinations = cbind(parameter_combinations, matrix(0,nrow(parameter_combinations),2))
for(i in 1:nrow(parameter_combinations)){
  tru_vals = dists[,parameter_combinations[i,2]]
  us = as.numeric(parameter_combinations[i,1])
  print(c(parameter_combinations[i,2], us))
  chain = numeric(1000)
  for(j in 1:1000){
    chain[j] = mean(colMeans(((t(sapply(tru_vals,rbinom,n = 500,size = us)) / us) - tru_vals)^2))
  }
  parameter_combinations[i,3] = quantile(chain,0.975)
  parameter_combinations[i,4] = quantile(chain,0.025)
}

hitting_times = cbind(results[,1:8], numeric(nrow(results)))
colnames(hitting_times)[9] = "ht"
for(i in 1:nrow(results)){
  if(i %% 500 == 0){
    print("oi mate")
  }
  hitting_times[i, 9] = hitting_time(results[i, 9:500],
    upper = parameter_combinations[
      which(parameter_combinations[,1] == unlist(results[i,"player_urn_size"]) & 
            parameter_combinations[,2] == unlist(results[i,"dist_type"])) , 3],
    lower = parameter_combinations[
      which(parameter_combinations[,1] == unlist(results[i,"player_urn_size"]) & 
              parameter_combinations[,2] == unlist(results[i,"dist_type"])) , 4])
}

saveRDS(hitting_times, "hitting_times.rds")
hitting_times = readRDS("hitting_times.rds")
write.csv(hitting_times, "hitting_times.csv")
################################################################################
#MAIN EFFECT: Paired_update (No paired update vs Paired update) + 
#Interaction between dist_type and paired update
################################################################################
layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4), 4, 4, byrow = TRUE))

puvsnpu = results %>%
  filter(player_label == 1) %>%
  group_by(paired_update) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(paired_update,starts_with('iter'))

mMSE_pu = rowMeans(puvsnpu[401:501])

b_puvsnpu = baseline_MSE %>%
  filter(player_label == 1) %>%
  group_by(paired_update) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(paired_update,starts_with('iter'))

plot(as.vector(unlist(puvsnpu[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Averaged")
lines(as.vector(unlist(puvsnpu[2,-1])), col = 2)
lines(as.vector(unlist(b_puvsnpu[1,-1])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_puvsnpu[2,-1])), col = 2, lty = "dotted")

#interaction between dist_type and paired update
puvsnpu_dt = results %>%
  filter(player_label == 1) %>%
  group_by(paired_update, dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, paired_update,starts_with('iter'))

b_puvsnpu_dt = baseline_MSE %>%
  filter(player_label == 1) %>%
  group_by(paired_update, dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, paired_update,starts_with('iter'))

plot(as.vector(unlist(puvsnpu_dt[1,-c(1:2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Mean = 1")
lines(as.vector(unlist(puvsnpu_dt[4,-c(1:2)])), col = 2)
lines(as.vector(unlist(b_puvsnpu_dt[1,-c(1:2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_puvsnpu_dt[4,-c(1:2)])), col = 2, lty = "dotted")

plot(as.vector(unlist(puvsnpu_dt[2,-c(1:2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Mean = 0")
lines(as.vector(unlist(puvsnpu_dt[5,-c(1:2)])), col = 2)
lines(as.vector(unlist(b_puvsnpu_dt[2,-c(1:2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_puvsnpu_dt[5,-c(1:2)])), col = 2, lty = "dotted")

plot(as.vector(unlist(puvsnpu_dt[3,-c(1:2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Mean = -1")
lines(as.vector(unlist(puvsnpu_dt[6,-c(1:2)])), col = 2)
lines(as.vector(unlist(b_puvsnpu_dt[3,-c(1:2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_puvsnpu_dt[6,-c(1:2)])), col = 2, lty = "dotted")

options(pillar.sigfig=7)
pu_ht =hitting_times %>%
  filter(player_label == 1) %>%
  group_by(paired_update) %>%
  summarise(median(ht, na.rm = TRUE)) 

hitting_times = hitting_times %>% filter(player_label == 1)

wilcox.test(ht ~ paired_update, hitting_times,
  na.action = "na.omit"
)


################################################################################
#MAIN EFFECT: Algorithm type (Urnings1 vs Urnings2)
################################################################################
#Mean squared error  
alg_type = results %>% 
  filter(paired_update == "TRUE", player_label != 0) %>%
  group_by(algo) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(algo, starts_with('iter'))

b_alg_type = baseline_MSE %>% 
  filter(paired_update == "TRUE", player_label != 0) %>%
  group_by(algo) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(algo, starts_with('iter'))

plot(as.vector(unlist(alg_type[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
lines(as.vector(unlist(alg_type[2,-1])), col = 2)
lines(as.vector(unlist(b_alg_type[1,-1])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_alg_type[2,-1])), col = 2, lty = "dotted")

ht_alg = hitting_times %>% 
  filter(paired_update == "TRUE") %>%
  group_by(algo) %>%
  summarise(across(starts_with("ht"), ~ median(., na.rm = TRUE))) %>%
  select(algo, starts_with('ht'))

hitting_times = hitting_times %>% filter(paired_update == "TRUE")

wilcox.test(
  as.vector(unlist(hitting_times[hitting_times[,"algo"] == "Urnings2", "ht"])),
  as.vector(unlist(hitting_times[hitting_times[,"algo"] == "Urnings1", "ht"])),
  na.action = "na.omit"
)

wilcox.test( ht ~ algo, hitting_times, na.action = "na.omit")


################################################################################
#REDUCING DATA SIZE DUE TO THE PAIRED UPDATE RESULTS 
################################################################################
results = results %>% 
  filter(paired_update == "TRUE", player_label != 0)

################################################################################
#MAIN EFFECT: Item urn sizes (16 vs 32 vs 64 vs 128) + interaction cold start and item urn sizes
################################################################################
item_urnsizes = results %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(item_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

plot(as.vector(unlist(item_urnsizes[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(item_urnsizes[i,-1])), col = i)
}

b_item_urnsizes = baseline_MSE %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(item_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)


for (i in 1:4) {
  lines(as.vector(unlist(b_item_urnsizes[i,-1])), col = i, lty = "dotted")
}

ht_item_urnsizes = hitting_times %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("ht"), ~ median(., na.rm = TRUE))) %>%
  select(item_urn_size,starts_with('ht')) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

kruskal.test(ht ~ item_urn_size, hitting_times, na.action = "na.omit")

#interaction cold start and item urn sizes
item_urnsizes_cs = results %>%
  filter(player_percent == "999") %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(item_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

plot(as.vector(unlist(item_urnsizes_cs[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(item_urnsizes_cs[i,-1])), col = i)
}

b_item_urnsizes_cs = baseline_MSE %>%
  filter(player_percent == "999") %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(item_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

for (i in 1:4) {
  lines(as.vector(unlist(b_item_urnsizes_cs[i,-1])), col = i, lty = "dotted")
}

ht_item_urnsizes_cs = hitting_times %>%
  filter(player_percent == "999") %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("ht"), ~ mean(.,na.rm = TRUE))) %>%
  select(item_urn_size,starts_with('ht')) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)


################################################################################
#MAIN EFFECT: Player urn sizes (8 vs 16 vs 32 vs 64)
################################################################################
player_urnsizes = results %>% 
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

mMSE_urnsizes = rowMeans(player_urnsizes[401:501])

b_player_urnsizes = baseline_MSE %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

plot(as.vector(unlist(player_urnsizes[1,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
for (i in 2:4) {
  lines(as.vector(unlist(player_urnsizes[i,-1])), col = i)
}

for (i in 1:4) {
  lines(as.vector(unlist(b_player_urnsizes[i,-1])), col = i, lty = "dotted")
}

for(i in 1:4){
  print(hitting_time(player_urnsizes[i,-1], b_player_urnsizes[i,-1] + 0.01,  b_player_urnsizes[i,-1] -0.01))
}

ht_player_urnsizes = hitting_times %>% 
  group_by(player_urn_size) %>%
  summarise(across(starts_with("ht"), ~ median(., na.rm = TRUE))) %>%
  select(player_urn_size,starts_with('ht')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

kruskal.test(ht~player_urn_size, hitting_times, na.action = "na.omit")
conover.test(as.numeric(unlist(hitting_times[,"ht"])), 
             as.factor(unlist(hitting_times[,"player_urn_size"])),
             method = "bonferroni")

mMSE_urnsizes_total = cbind(results[,c(1:8)], rowMeans(results[,408:508]))
colnames(mMSE_urnsizes_total)[9] = "mMSE"
kruskal.test(mMSE~player_urn_size, mMSE_urnsizes_total, na.action = "na.omit")
conover.test(as.numeric(unlist(mMSE_urnsizes_total[,"mMSE"])), 
             as.factor(unlist(mMSE_urnsizes_total[,"player_urn_size"])),
             method = "bonferroni")

################################################################################
#MAIN EFFECT: Adaptivity
################################################################################
adapt = results %>% 
  group_by(adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(adapt,starts_with('iter')) 

plot(as.vector(unlist(adapt[4,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
lines(as.vector(unlist(adapt[1,-1])), col = 2)
lines(as.vector(unlist(adapt[2,-1])), col = 3)
lines(as.vector(unlist(adapt[3,-1])), col = 4)

b_adapt = baseline_MSE %>% 
  group_by(adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(adapt,starts_with('iter')) 

lines(as.vector(unlist(b_adapt[4,-1])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_adapt[1,-1])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_adapt[2,-1])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_adapt[3,-1])), col = 4, lty = "dotted")

ht_adapt = hitting_times %>% 
  group_by(adapt) %>%
  summarise(across(starts_with("ht"), ~ median(., na.rm = TRUE))) %>%
  select(adapt,starts_with('ht')) 

kruskal.test(ht~adapt, hitting_times, na.action = "na.omit")
conover.test(as.numeric(unlist(hitting_times[,"ht"])), 
             as.factor(unlist(hitting_times[,"adapt"])),
             method = "bonferroni")
################################################################################
#MAIN EFFECT: dist
################################################################################
dist = results %>% 
  group_by(dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type,starts_with('iter')) 

b_dist = baseline_MSE %>% 
  group_by(dist_type) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type,starts_with('iter')) 

plot(as.vector(unlist(dist[2,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
lines(as.vector(unlist(dist[1,-1])), col = 2)
lines(as.vector(unlist(dist[3,-1])), col = 3)

lines(as.vector(unlist(b_dist[2,-1])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_dist[1,-1])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_dist[3,-1])), col = 3, lty = "dotted")

ht_dist = hitting_times %>% 
  group_by(dist_type) %>%
  summarise(across(starts_with("ht"), ~ mean(.,na.rm = TRUE))) %>%
  select(dist_type,starts_with('ht')) 

################################################################################
#MAIN EFFECT: cold start
################################################################################
player_percent = results %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent, starts_with('iter')) 

b_player_percent = baseline_MSE %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent, starts_with('iter')) 

plot(as.vector(unlist(player_percent[1,-1])),  ylim = c(0, 0.07), type = "l", ylab = "Mean Squared Error")
lines(as.vector(unlist(player_percent[3,-1])), col = 2)
lines(as.vector(unlist(player_percent[2,-1])), col = 3)
lines(as.vector(unlist(player_percent[4,-1])), col = 4)

lines(as.vector(unlist(b_player_percent[1,-1])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_player_percent[3,-1])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_player_percent[2,-1])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_player_percent[4,-1])), col = 4, lty = "dotted")

ht_player_percent = hitting_times %>%
  group_by(player_percent) %>%
  summarise(across(starts_with("ht"), ~ median(., na.rm = TRUE))) %>%
  select(player_percent, starts_with('ht')) 

kruskal.test(ht ~ player_percent, hitting_times)
conover.test(as.numeric(unlist(hitting_times[,"ht"])), 
             as.factor(unlist(hitting_times[,"player_percent"])),
             method = "bonferroni")

################################################################################
#INTERACTION EFFECT: Player urn sizes * adaptivity + coldstart or no coldstart
################################################################################
urnXadaptivity = results %>%
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, adapt, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

b_urnXadaptivity = baseline_MSE %>%
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, adapt, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4), 4, 4, byrow = TRUE))

plot(as.vector(unlist(urnXadaptivity[4,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Urn size = 8")
lines(as.vector(unlist(urnXadaptivity[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[2,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_urnXadaptivity[4,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[1,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[2,-c(1,2)])), col = 4, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity[8,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Urn size = 16")
lines(as.vector(unlist(urnXadaptivity[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[7,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[6,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_urnXadaptivity[8,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[5,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[7,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[6,-c(1,2)])), col = 4, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity[12,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Urn size = 32")
lines(as.vector(unlist(urnXadaptivity[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[11,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[10,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_urnXadaptivity[12,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[9,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[11,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[10,-c(1,2)])), col = 4, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity[16,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Urn size = 64")
lines(as.vector(unlist(urnXadaptivity[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[15,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[14,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_urnXadaptivity[16,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[13,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[15,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[14,-c(1,2)])), col = 4, lty = "dotted")

ht_urnXadaptivity = hitting_times %>%
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("ht"), ~ median(., na.rm = TRUE))) %>%
  select(player_urn_size, adapt, starts_with('ht')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

kruskal.test(ht ~ adapt, hitting_times[hitting_times[,"player_urn_size"] == "8",])
conover.test(as.numeric(unlist(hitting_times[hitting_times[,"player_urn_size"] == "8","ht"])), 
             as.factor(unlist(hitting_times[hitting_times[,"player_urn_size"] == "8","adapt"])),
             method = "bonferroni")

kruskal.test(ht ~ adapt, hitting_times[hitting_times[,"player_urn_size"] == "16",])
conover.test(as.numeric(unlist(hitting_times[hitting_times[,"player_urn_size"] == "16","ht"])), 
             as.factor(unlist(hitting_times[hitting_times[,"player_urn_size"] == "16","adapt"])),
             method = "bonferroni")

kruskal.test(ht ~ adapt, hitting_times[hitting_times[,"player_urn_size"] == "32",])
conover.test(as.numeric(unlist(hitting_times[hitting_times[,"player_urn_size"] == "32","ht"])), 
             as.factor(unlist(hitting_times[hitting_times[,"player_urn_size"] == "32","adapt"])),
             method = "bonferroni")

kruskal.test(ht ~ adapt, hitting_times[hitting_times[,"player_urn_size"] == "64",])
conover.test(as.numeric(unlist(hitting_times[hitting_times[,"player_urn_size"] == "64","ht"])), 
             as.factor(unlist(hitting_times[hitting_times[,"player_urn_size"] == "64","adapt"])),
             method = "bonferroni")


urnXadaptivity = results %>%
  filter(player_percent != 999) %>%
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, adapt, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

b_urnXadaptivity = baseline_MSE %>%
  filter(player_percent != 999) %>%
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, adapt, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

ht_urnXadaptivity = hitting_times %>%
  filter(player_percent != 999) %>%
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("ht"), ~ median(.,na.rm = TRUE))) %>%
  select(player_urn_size, adapt, starts_with('ht')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

reduced_hitting_times = hitting_times %>% filter(player_percent != 999)

kruskal.test(ht ~ adapt, reduced_hitting_times[reduced_hitting_times[,"player_urn_size"] == "8",])
conover.test(as.numeric(unlist(reduced_hitting_times[reduced_hitting_times[,"player_urn_size"] == "8","ht"])), 
             as.factor(unlist(reduced_hitting_times[reduced_hitting_times[,"player_urn_size"] == "8","adapt"])),
             method = "bonferroni")

kruskal.test(ht ~ adapt, reduced_hitting_times[reduced_hitting_times[,"player_urn_size"] == "16",])
conover.test(as.numeric(unlist(reduced_hitting_times[reduced_hitting_times[,"player_urn_size"] == "16","ht"])), 
             as.factor(unlist(reduced_hitting_times[reduced_hitting_times[,"player_urn_size"] == "16","adapt"])),
             method = "bonferroni")

kruskal.test(ht ~ adapt, reduced_hitting_times[reduced_hitting_times[,"player_urn_size"] == "32",])
conover.test(as.numeric(unlist(reduced_hitting_times[reduced_hitting_times[,"player_urn_size"] == "32","ht"])), 
             as.factor(unlist(reduced_hitting_times[reduced_hitting_times[,"player_urn_size"] == "32","adapt"])),
             method = "bonferroni")

kruskal.test(ht ~ adapt, reduced_hitting_times[reduced_hitting_times[,"player_urn_size"] == "64",])
conover.test(as.numeric(unlist(reduced_hitting_times[reduced_hitting_times[,"player_urn_size"] == "64","ht"])), 
             as.factor(unlist(reduced_hitting_times[reduced_hitting_times[,"player_urn_size"] == "64","adapt"])),
             method = "bonferroni")

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4), 4, 4, byrow = TRUE))

plot(as.vector(unlist(urnXadaptivity[4,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Urn size = 8")
lines(as.vector(unlist(urnXadaptivity[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[2,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_urnXadaptivity[4,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[1,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[2,-c(1,2)])), col = 4, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity[8,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Urn size = 16")
lines(as.vector(unlist(urnXadaptivity[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[7,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[6,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_urnXadaptivity[8,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[5,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[7,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[6,-c(1,2)])), col = 4, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity[12,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Urn size = 32")
lines(as.vector(unlist(urnXadaptivity[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[11,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[10,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_urnXadaptivity[12,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[9,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[11,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[10,-c(1,2)])), col = 4, lty = "dotted")

plot(as.vector(unlist(urnXadaptivity[16,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Urn size = 64")
lines(as.vector(unlist(urnXadaptivity[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(urnXadaptivity[15,-c(1,2)])), col = 3)
lines(as.vector(unlist(urnXadaptivity[14,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_urnXadaptivity[16,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[13,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[15,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_urnXadaptivity[14,-c(1,2)])), col = 4, lty = "dotted")

################################################################################
#INTERACTION EFFECT: Player percent * adaptivity 
################################################################################
percentXadaptivity = results %>%
  group_by(player_percent, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent, adapt, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)

b_percentXadaptivity = baseline_MSE %>%
  group_by(player_percent, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_percent, adapt, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)

layout(matrix(c(1,1,2,2,
                1,1,2,2,
                3,3,4,4,
                3,3,4,4), 4, 4, byrow = TRUE))

plot(as.vector(unlist(percentXadaptivity[4,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "New players 10%")
lines(as.vector(unlist(percentXadaptivity[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(percentXadaptivity[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(percentXadaptivity[2,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_percentXadaptivity[4,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_percentXadaptivity[1,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_percentXadaptivity[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_percentXadaptivity[2,-c(1,2)])), col = 4, lty = "dotted")

plot(as.vector(unlist(percentXadaptivity[8,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "New players 50%")
lines(as.vector(unlist(percentXadaptivity[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(percentXadaptivity[7,-c(1,2)])), col = 3)
lines(as.vector(unlist(percentXadaptivity[6,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_percentXadaptivity[8,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_percentXadaptivity[5,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_percentXadaptivity[7,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_percentXadaptivity[6,-c(1,2)])), col = 4, lty = "dotted")

plot(as.vector(unlist(percentXadaptivity[12,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "New players 100%")
lines(as.vector(unlist(percentXadaptivity[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(percentXadaptivity[11,-c(1,2)])), col = 3)
lines(as.vector(unlist(percentXadaptivity[10,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_percentXadaptivity[12,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_percentXadaptivity[9,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_percentXadaptivity[11,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_percentXadaptivity[10,-c(1,2)])), col = 4, lty = "dotted")

plot(as.vector(unlist(percentXadaptivity[16,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Total cold start")
lines(as.vector(unlist(percentXadaptivity[13,-c(1,2)])), col = 2)
lines(as.vector(unlist(percentXadaptivity[15,-c(1,2)])), col = 3)
lines(as.vector(unlist(percentXadaptivity[14,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_percentXadaptivity[16,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_percentXadaptivity[13,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_percentXadaptivity[15,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_percentXadaptivity[14,-c(1,2)])), col = 4, lty = "dotted")

ht_percentXadaptivity = hitting_times %>%
  group_by(player_percent, adapt) %>%
  summarise(across(starts_with("ht"), ~ median(., na.rm = TRUE))) %>%
  select(player_percent, adapt, starts_with('ht')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)

kruskal.test(ht ~ player_percent, hitting_times[hitting_times[,"adapt"] == "n_adaptive",])
conover.test(as.numeric(unlist(hitting_times[hitting_times[,"adapt"] == "n_adaptive","ht"])), 
             as.factor(unlist(hitting_times[hitting_times[,"adapt"] == "n_adaptive","player_percent"])),
             method = "bonferroni")

kruskal.test(ht ~ player_percent, hitting_times[hitting_times[,"adapt"] == "adaptive50",])
conover.test(as.numeric(unlist(hitting_times[hitting_times[,"adapt"] == "adaptive50","ht"])), 
             as.factor(unlist(hitting_times[hitting_times[,"adapt"] == "adaptive50", "player_percent"])),
             method = "bonferroni")

kruskal.test(ht ~ player_percent, hitting_times[hitting_times[,"adapt"] == "adaptive70",])
conover.test(as.numeric(unlist(hitting_times[hitting_times[,"adapt"] == "adaptive70","ht"])), 
             as.factor(unlist(hitting_times[hitting_times[,"adapt"] == "adaptive70", "player_percent"])),
             method = "bonferroni")

kruskal.test(ht ~ player_percent, hitting_times[hitting_times[,"adapt"] == "adaptive_sigma",])
conover.test(as.numeric(unlist(hitting_times[hitting_times[,"adapt"] == "adaptive_sigma","ht"])), 
             as.factor(unlist(hitting_times[hitting_times[,"adapt"] == "adaptive_sigma", "player_percent"])),
             method = "bonferroni")



################################################################################
#INTERACTION EFFECT: dist_type * adaptivity 
################################################################################
distxadapt = results %>%
  group_by(dist_type, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, adapt, starts_with('iter'))

b_distxadapt = baseline_MSE %>%
  group_by(dist_type, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(dist_type, adapt, starts_with('iter'))


layout(matrix(c(1,1,2,2,3,3,1,1,2,2,3,3), 2, 6, byrow = TRUE))
plot(as.vector(unlist(distxadapt[4,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Mean = 1")
lines(as.vector(unlist(distxadapt[1,-c(1,2)])), col = 2)
lines(as.vector(unlist(distxadapt[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(distxadapt[2,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_distxadapt[4,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_distxadapt[1,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_distxadapt[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_distxadapt[2,-c(1,2)])), col = 4, lty = "dotted")

plot(as.vector(unlist(distxadapt[8,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Mean = 0")
lines(as.vector(unlist(distxadapt[5,-c(1,2)])), col = 2)
lines(as.vector(unlist(distxadapt[7,-c(1,2)])), col = 3)
lines(as.vector(unlist(distxadapt[6,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_distxadapt[8,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_distxadapt[5,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_distxadapt[7,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_distxadapt[6,-c(1,2)])), col = 4, lty = "dotted")

plot(as.vector(unlist(distxadapt[12,-c(1,2)])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error", main = "Mean = -1")
lines(as.vector(unlist(distxadapt[9,-c(1,2)])), col = 2)
lines(as.vector(unlist(distxadapt[11,-c(1,2)])), col = 3)
lines(as.vector(unlist(distxadapt[10,-c(1,2)])), col = 4)

lines(as.vector(unlist(b_distxadapt[12,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_distxadapt[9,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_distxadapt[11,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_distxadapt[10, -c(1,2)])), col = 4, lty = "dotted")

ht_distxadapt = hitting_times %>%
  group_by(dist_type, adapt) %>%
  summarise(across(starts_with("ht"), ~ median(.,na.rm = TRUE))) %>%
  select(dist_type, adapt, starts_with('ht'))


kruskal.test(ht ~ adapt, hitting_times[hitting_times[,"dist_type"] == "better",])
conover.test(as.numeric(unlist(hitting_times[hitting_times[,"dist_type"] == "better","ht"])), 
             as.factor(unlist(hitting_times[hitting_times[,"dist_type"] == "better", "adapt"])),
             method = "bonferroni")

kruskal.test(ht ~ adapt, hitting_times[hitting_times[,"dist_type"] == "central",])
conover.test(as.numeric(unlist(hitting_times[hitting_times[,"dist_type"] == "central","ht"])), 
             as.factor(unlist(hitting_times[hitting_times[,"dist_type"] == "central", "adapt"])),
             method = "bonferroni")

kruskal.test(ht ~ adapt, hitting_times[hitting_times[,"dist_type"] == "worse",])
conover.test(as.numeric(unlist(hitting_times[hitting_times[,"dist_type"] == "worse","ht"])), 
             as.factor(unlist(hitting_times[hitting_times[,"dist_type"] == "worse", "adapt"])),
             method = "bonferroni")

save.image(file = "sim1_global_env.RData")






