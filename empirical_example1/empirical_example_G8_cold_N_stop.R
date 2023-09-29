library(haven)
library(eRm)
library(tidyverse)
source("urnings_handler.R")

set.seed(13181913)
#reading sav files
df_8 = read_sav("OKM_2014_08.sav")

################################################################################
# 6th grade mathematics
################################################################################
#selecting the math items
df_8 = as.matrix(df_8 %>%
                   select(starts_with("M"), -starts_with("M_zpsc")))

#selecting items with binary outcomes
indicator = logical(ncol(df_8))
for(i in 1:ncol(df_8)){
  indicator[i] = 2 %in% df_8[, i]
}

df_8 = df_8[,!indicator]

item_names = colnames(df_8)
#fitting rasch for the 6th grade
rasch_8 = RM(df_8)
pp8 = person.parameter(rasch_8)
pp8 = as.vector(coef(pp8))
ip8 = -rasch_8[["betapar"]]
IRT_8 = list(pp8, ip8)

################################################################################
# Preparing data for Urnings based analysis
################################################################################
colnames(df_8) = 1:ncol(df_8)
n_players = nrow(df_8)
n_items = ncol(df_8)


subdata = list()
outcomes = list()
subIRT = list()
N_s = c(50,500,5000)
counter = 1
for(n in N_s){
  player_codes = data.frame("players" = 1:n)
  indicies = sample(1:n_players, n)
  df_sub = df_8[indicies, ]
  df_sub = cbind(player_codes, df_sub)
  
  outcome = matrix(0, nrow = 1 + 16, ncol = n)
  outcome[1,] = IRT_8[[1]][indicies]
  
  irt_sub = list(IRT_8[[1]][indicies], IRT_8[[2]])
  
  long_df_8 = pivot_longer(df_sub, cols = -players, names_to = "items", values_to = "result", values_drop_na = TRUE)
  random_indicies = sample(nrow(long_df_8))
  long_df_8 = long_df_8[random_indicies, ]
  long_df_8$items = as.integer(long_df_8$items)
  long_df_8 = as.matrix(long_df_8)
  
  outcomes[[counter]] = outcome
  subdata[[counter]] = long_df_8
  subIRT[[counter]] = irt_sub
  counter = counter + 1
}

################################################################################
# Preparing settings for Urnings based analysis
################################################################################
player_urn_sizes = c(8,16,32,64)
item_urn_sizes = c(16,32,64,128)

counter = 2
for(pus in player_urn_sizes){
  for(ius in item_urn_sizes){
    for(sdt in 1:length(subdata)){
      r_pl = numeric(N_s[sdt]) + as.integer(pus / 2)
      r_it = numeric(n_items) + as.integer(ius / 2)
      game_type = urnings_analysis(subdata[[sdt]],
                                   r_pl,
                                   r_it,
                                   pus,
                                   ius,
                                   "Urnings2",
                                   TRUE,
                                   compare_IRT = subIRT[[sdt]])
      
      game = analyse(game_type, omit_message = FALSE, save_iter = 1000000, stop = TRUE)
      
      outcomes[[sdt]][counter,] = game$n_games_per_student
    }
    counter = counter + 1
  }
}

saveRDS(outcomes, "empirical_example_G8_cold_N_stop.rds")
