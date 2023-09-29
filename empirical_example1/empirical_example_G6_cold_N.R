library(haven)
library(eRm)
library(tidyverse)
source("urnings_handler.R")

set.seed(13181913)
#reading sav files
df_6 = read_sav("OKM_2014_06.sav")

################################################################################
# 6th grade mathematics
################################################################################
#selecting the math items
df_6 = as.matrix(df_6 %>%
                   select(starts_with("M"), -starts_with("M_zpsc")))

#selecting items with binary outcomes
indicator = logical(ncol(df_6))
for(i in 1:ncol(df_6)){
  indicator[i] = 2 %in% df_6[, i]
}

df_6 = df_6[,!indicator]

item_names = colnames(df_6)
#fitting rasch for the 6th grade
rasch_6 = RM(df_6)
pp6 = person.parameter(rasch_6)
pp6 = as.vector(coef(pp6))
ip6 = -rasch_6[["betapar"]]
IRT_6 = list(pp6, ip6)

################################################################################
# Preparing data for Urnings based analysis
################################################################################
colnames(df_6) = 1:ncol(df_6)
n_players = nrow(df_6)
n_items = ncol(df_6)


subdata = list()
outcomes = list()
N_s = c(50,500,5000)
counter = 1
for(n in N_s){
  player_codes = data.frame("players" = 1:n)
  indicies = sample(1:n_players, n)
  df_sub = df_6[indicies, ]
  df_sub = cbind(player_codes, df_sub)
  
  outcome = matrix(0, nrow = 1 + 16, ncol = n)
  outcome[1,] = IRT_6[[1]][indicies]
  
  long_df_6 = pivot_longer(df_sub, cols = -players, names_to = "items", values_to = "result", values_drop_na = TRUE)
  random_indicies = sample(nrow(long_df_6))
  long_df_6 = long_df_6[random_indicies, ]
  long_df_6$items = as.integer(long_df_6$items)
  long_df_6 = as.matrix(long_df_6)
  
  outcomes[[counter]] = outcome
  subdata[[counter]] = long_df_6
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
                                 compare_IRT = NULL)
    
    game = analyse(game_type, omit_message = FALSE, save_iter = 1000000, stop = FALSE)
    
    outcomes[[sdt]][counter,] = game$est_player
    }
    counter = counter + 1
  }
}

saveRDS(outcomes, "empirical_example_G6_cold_N.rds")
