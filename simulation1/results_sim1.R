library(tidyverse)
library(patchwork)

set.seed(13181913)

#dir.create("figures", showWarnings = FALSE)
################################################################################
#READING THE SIMULATION OUTPUTS
################################################################################
sim1_urnings1_central = readRDS("output/sim1_urnings1_central.rds")
sim1_urnings1_better = readRDS("output/sim1_urnings1_better.rds")
sim1_urnings1_worse = readRDS("output/sim1_urnings1_worse.rds")
sim1_urnings2_central = readRDS("output/sim1_urnings2_central.rds")
sim1_urnings2_better = readRDS("output/sim1_urnings2_better.rds")
sim1_urnings2_worse = readRDS("output/sim1_urnings2_worse.rds")

index = as.matrix(rep(seq(1,1536, 1), each = 900))
colnames(index) = "index"

results_u1 = cbind(index, rbind(sim1_urnings1_central, 
                                sim1_urnings1_better, 
                                sim1_urnings1_worse))

results = cbind(index, rbind(sim1_urnings2_central, 
                             sim1_urnings2_better, 
                             sim1_urnings2_worse))

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


################################################################################
#Creating baselines
################################################################################
baseline = readRDS("output/sim1_baseline.rds")
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
saveRDS(results, "output/collapsed_results.rds")
saveRDS(baseline_MSE, "output/collapsed_baseline_MSE.rds")
rm(results_u1, baseline_MSE_u1)
results = readRDS("output/collapsed_results.rds")
baseline_MSE = readRDS("output/collapsed_baseline_MSE.rds")

################################################################################
#hitting time
################################################################################
hitting_times = readRDS("output/hitting_times.rds")
#write.csv(hitting_times, "hitting_times.csv")

################################################################################
#MAIN EFFECT: Paired_update (No paired update vs Paired update) + 
#Interaction between dist_type and paired update
################################################################################
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

#plotting
df_h11 = rbind(puvsnpu_dt, puvsnpu, b_puvsnpu_dt, b_puvsnpu) %>% 
          ungroup() %>%
          mutate(res_type = c(rep("a", times = nrow(puvsnpu_dt) + nrow(puvsnpu)), rep("b", times = nrow(b_puvsnpu_dt) + nrow(b_puvsnpu)))) %>%
          relocate(res_type, .before = 1)

df_h11 = pivot_longer(df_h11,
                       cols = starts_with("iter"),
                       names_to = "variable",
                       values_to = "value") %>%
          mutate(variable = as.numeric(gsub("iter", "", variable))) %>%
          replace_na(list(dist_type = "Averaged"))

df_h11$dist_type = factor(df_h11$dist_type, levels = c("Averaged", "central","better", "worse"),
                           labels = c("Averaged","N(0,1)", "N(1,1)","N(-1,1)"))

df_h11 %>%
  ggplot(aes(x = variable, y = value, color = paired_update, linetype = res_type)) +
  facet_wrap(dist_type ~ ., ncol = 2) +
  geom_line() +
  labs(x = "Iterations", y = "MSE") +
  scale_linetype_manual(values = c("a" = "solid", "b" = "dotted"),
                        name = "",
                        labels = c("MSE", "baseline MSE")) +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"),
                     name = "",
                     labels = c("Paired update", "No paired update")) +
  jtools::theme_apa(legend.font.size = 10)

pu_ht =hitting_times %>%
  group_by(paired_update) %>%
  summarise(mean(ht, na.rm = TRUE)) 

hitting_times = hitting_times %>% filter(player_label == 1)

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

df_h12 = rbind(alg_type, b_alg_type) %>%
         ungroup() %>%
          mutate(res_type = c(rep("a", times = nrow(alg_type)),
                              rep("b", times = nrow(b_alg_type)))) %>%
          relocate(res_type, .before = 1) %>%
          pivot_longer(cols = starts_with("iter"),
                       names_to = "variable",
                       values_to = "value") %>%
          mutate(variable = as.numeric(gsub("iter", "", variable)))

df_h12 %>%
  ggplot(aes(x = variable, y = value, color = algo, linetype = res_type)) +
  geom_line() +
  labs(x = "Iterations", y = "MSE") +
  scale_linetype_manual(values = c("a" = "solid", "b" = "dotted"),
                        name = "",
                        labels = c("MSE", "baseline MSE")) +
  scale_color_manual(values = c("Urnings1" = "blue", "Urnings2" = "red"),
                     name = "",
                     labels = c("Urnings 1", "Urnings 2")) +
  jtools::theme_apa(legend.font.size = 10)

ht_alg = hitting_times %>% 
  filter(paired_update == "TRUE") %>%
  group_by(algo) %>%
  summarise(across(starts_with("ht"), ~ median(., na.rm = TRUE))) %>%
  select(algo, starts_with('ht'))

hitting_times = hitting_times %>% filter(paired_update == "TRUE")

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

b_item_urnsizes = baseline_MSE %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(item_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

df_h13 = rbind(item_urnsizes, b_item_urnsizes) %>%
        ungroup() %>%
        mutate(res_type = c(rep("a", times = nrow(item_urnsizes)),
                            rep("b", times = nrow(b_item_urnsizes)))) %>%
        relocate(res_type, .before = 1) %>%
        pivot_longer(cols = starts_with("iter"),
                     names_to = "variable",
                     values_to = "value") %>%
        mutate(variable = as.numeric(gsub("iter", "", variable))) %>%
        mutate(item_urn_size = as.character(item_urn_size))

df_h13$item_urn_size = factor(df_h13$item_urn_size,
                              levels = c("16", "32", "64", "128"),
                              labels = c("16", "32", "64", "128"))


plot_h13 = df_h13 %>%
  ggplot(aes(x = variable, y = value, color = item_urn_size, linetype = res_type)) +
  geom_line() +
  labs(x = "", y = "MSE") +
  scale_linetype_manual(values = c("a" = "solid", "b" = "dotted"),
                        name = "",
                        labels = c("MSE", "baseline MSE")) +
  scale_color_manual(values = c("16" = "black",
                                "32" = "red",
                                "64" = "green",
                                "128" = "blue"),
                     name = "Item urn sizes") +
  jtools::theme_apa(legend.font.size = 10)

ht_item_urnsizes = hitting_times %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("ht"), ~ mean(., na.rm = TRUE))) %>%
  select(item_urn_size,starts_with('ht')) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

#interaction cold start and item urn sizes
item_urnsizes_cs = results %>%
  filter(player_percent == "999") %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(item_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

b_item_urnsizes_cs = baseline_MSE %>%
  filter(player_percent == "999") %>%
  group_by(item_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(item_urn_size,starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(item_urn_size)

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

df_h14 = rbind(player_urnsizes, b_player_urnsizes) %>%
  ungroup() %>%
  mutate(res_type = c(rep("a", times = nrow(player_urnsizes)),
                      rep("b", times = nrow(b_player_urnsizes)))) %>%
  relocate(res_type, .before = 1) %>%
  pivot_longer(cols = starts_with("iter"),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = as.numeric(gsub("iter", "", variable))) %>%
  mutate(player_urn_size = as.character(player_urn_size))

df_h14$player_urn_size = factor(df_h14$player_urn_size,
                              levels = c("8", "16", "32", "64"),
                              labels = c("8", "16", "32", "64"))

plot_h14 = df_h14 %>%
  ggplot(aes(x = variable, y = value, color = player_urn_size, linetype = res_type)) +
  geom_line() +
  labs(x = "Iterations", y = "MSE") +
  scale_linetype_manual(values = c("a" = "solid", "b" = "dotted"),
                        name = "",
                        labels = c("MSE", "baseline MSE")) +
  scale_color_manual(values = c("8" = "black",
                                "16" = "red",
                                "32" = "green",
                                "64" = "blue"),
                     name = "Student urn sizes") +
  guides(color = guide_legend(order = 2),
         linetype = guide_legend(order = 1)) + 
  jtools::theme_apa(legend.font.size = 10) 


plot_h13 + plot_h14 + plot_layout(nrow = 2, guides = "auto")


ht_player_urnsizes = hitting_times %>% 
  group_by(player_urn_size) %>%
  summarise(across(starts_with("ht"), ~ mean(., na.rm = TRUE))) %>%
  select(player_urn_size,starts_with('ht')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

################################################################################
#MAIN EFFECT: Adaptivity
################################################################################
adapt = results %>% 
  group_by(adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(adapt,starts_with('iter')) 

b_adapt = baseline_MSE %>% 
  group_by(adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(adapt,starts_with('iter')) 

plot(as.vector(unlist(adapt[4,-1])), type = "l", ylim = c(0, 0.07), ylab = "Mean Squared Error")
lines(as.vector(unlist(adapt[1,-1])), col = 2)
lines(as.vector(unlist(adapt[2,-1])), col = 3)
lines(as.vector(unlist(adapt[3,-1])), col = 4)
lines(as.vector(unlist(b_adapt[4,-1])), col = 1, lty = "dotted")
lines(as.vector(unlist(b_adapt[1,-1])), col = 2, lty = "dotted")
lines(as.vector(unlist(b_adapt[2,-1])), col = 3, lty = "dotted")
lines(as.vector(unlist(b_adapt[3,-1])), col = 4, lty = "dotted")

ht_adapt = hitting_times %>% 
  group_by(adapt) %>%
  summarise(across(starts_with("ht"), ~ mean(., na.rm = TRUE))) %>%
  select(adapt,starts_with('ht')) 

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

df_h15 = rbind(urnXadaptivity, b_urnXadaptivity) %>%
  ungroup() %>%
  mutate(res_type = c(rep("a", times = nrow(urnXadaptivity)),
                      rep("b", times = nrow(b_urnXadaptivity)))) %>%
  relocate(res_type, .before = 1) %>%
  pivot_longer(cols = starts_with("iter"),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = as.numeric(gsub("iter", "", variable))) %>%
  mutate(player_urn_size = as.character(player_urn_size))

df_h15$player_urn_size = factor(df_h15$player_urn_size,
                                levels = c("8", "16", "32", "64"),
                                labels = c("8", "16", "32", "64"))
df_h15$adapt = factor(df_h15$adapt,
                                levels = c("n_adaptive", "adaptive50", "adaptive70", "adaptive_sigma"),
                                labels = c("Non-adaptive", "NK(0.5,0.5)", "NK(0.7,0.5)", "NK(0.5,0.25)"))

df_h15 %>%
  ggplot(aes(x = variable, y = value, color = adapt, linetype = res_type)) +
  facet_wrap(player_urn_size ~ ., ncol = 2) +
  geom_line() +
  labs(x = "Iterations", y = "MSE") +
  scale_linetype_manual(values = c("a" = "solid", "b" = "dotted"),
                        name = "",
                        labels = c("MSE", "baseline MSE")) +
  scale_color_manual(values = c("Non-adaptive" = "black",
                                "NK(0.5,0.5)" = "red",
                                "NK(0.7,0.5)" = "blue",
                                "NK(0.5,0.25)" = "green"),
                     name = "") +
  jtools::theme_apa(legend.font.size = 10)

ht_urnXadaptivity = hitting_times %>%
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("ht"), ~ median(., na.rm = TRUE))) %>%
  select(player_urn_size, adapt, starts_with('ht')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_urn_size)

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

df_h16 = rbind(percentXadaptivity, b_percentXadaptivity) %>%
  ungroup() %>%
  mutate(res_type = c(rep("a", times = nrow(percentXadaptivity)),
                      rep("b", times = nrow(b_percentXadaptivity)))) %>%
  relocate(res_type, .before = 1) %>%
  pivot_longer(cols = starts_with("iter"),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = as.numeric(gsub("iter", "", variable))) %>%
  mutate(player_percent = as.character(player_percent))

df_h16$player_percent = factor(df_h16$player_percent,
                                levels = c("999","100", "50", "10"),
                                labels = c("Total cold start", "100% new student", "50% new student", "10% new student"))
df_h16$adapt = factor(df_h16$adapt,
                      levels = c("n_adaptive", "adaptive50", "adaptive70", "adaptive_sigma"),
                      labels = c("Non-adaptive", "NK(0.5,0.5)", "NK(0.7,0.5)", "NK(0.5,0.25)"))

df_h16 %>%
  ggplot(aes(x = variable, y = value, color = adapt, linetype = res_type)) +
  facet_wrap(player_percent ~ ., ncol = 2) +
  geom_line() +
  labs(x = "Iterations", y = "MSE") +
  scale_linetype_manual(values = c("a" = "solid", "b" = "dotted"),
                        name = "",
                        labels = c("MSE", "baseline MSE")) +
  scale_color_manual(values = c("Non-adaptive" = "black",
                                "NK(0.5,0.5)" = "red",
                                "NK(0.7,0.5)" = "blue",
                                "NK(0.5,0.25)" = "green"),
                     name = "") +
  jtools::theme_apa(legend.font.size = 10)

ht_percentXadaptivity = hitting_times %>%
  group_by(player_percent, adapt) %>%
  summarise(across(starts_with("ht"), ~ median(., na.rm = TRUE))) %>%
  select(player_percent, adapt, starts_with('ht')) %>%
  mutate_at(1, as.integer) %>%
  arrange(player_percent)

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


df_h17 = rbind(distxadapt, b_distxadapt) %>%
  ungroup() %>%
  mutate(res_type = c(rep("a", times = nrow(distxadapt)),
                      rep("b", times = nrow(b_distxadapt)))) %>%
  relocate(res_type, .before = 1) %>%
  pivot_longer(cols = starts_with("iter"),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = as.numeric(gsub("iter", "", variable)))

df_h17$dist_type = factor(df_h17$dist_type,
                               levels = c("better","central", "worse"),
                               labels = c("N(1,1)", "N(0,1)", "N(-1,1)"))
df_h17$adapt = factor(df_h17$adapt,
                      levels = c("n_adaptive", "adaptive50", "adaptive70", "adaptive_sigma"),
                      labels = c("Non-adaptive", "NK(0.5,0.5)", "NK(0.7,0.5)", "NK(0.5,0.25)"))

df_h17 %>%
  ggplot(aes(x = variable, y = value, color = adapt, linetype = res_type)) +
  facet_wrap(dist_type ~ ., nrow = 1) +
  geom_line() +
  labs(x = "Iterations", y = "MSE") +
  scale_linetype_manual(values = c("a" = "solid", "b" = "dotted"),
                        name = "",
                        labels = c("MSE", "baseline MSE")) +
  scale_color_manual(values = c("Non-adaptive" = "black",
                                "NK(0.5,0.5)" = "red",
                                "NK(0.7,0.5)" = "blue",
                                "NK(0.5,0.25)" = "green"),
                     name = "") +
  jtools::theme_apa(legend.font.size = 10)


ht_distxadapt = hitting_times %>%
  group_by(dist_type, adapt) %>%
  summarise(across(starts_with("ht"), ~ median(.,na.rm = TRUE))) %>%
  select(dist_type, adapt, starts_with('ht'))


save.image(file = "sim1_global_env.RData")






