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
player_codes = data.frame("players" = 1:n_players)
df_8 = cbind(player_codes, df_8)


long_df_8 = pivot_longer(df_8, cols = -players, names_to = "items", values_to = "result", values_drop_na = TRUE)
random_indicies = sample(nrow(long_df_8))
long_df_8 = long_df_8[random_indicies, ]
long_df_8$items = as.integer(long_df_8$items)
long_df_8 = as.matrix(long_df_8)
rm(player_codes, random_indicies)

################################################################################
# Preparing settings for Urnings based analysis
################################################################################
player_urn_sizes = c(8,16,32,64)
item_urn_sizes = c(16,32,64,128)

outcome = matrix(0, nrow = 2 + 16, ncol = n_players)
outcome[1,] = IRT_8[[1]]
counter = 2
for(pus in player_urn_sizes){
  for(ius in item_urn_sizes){
    r_pl = numeric(n_players) + as.integer(pus / 2)
    r_it = numeric(n_items) + as.integer(ius / 2)
    game_type = urnings_analysis(long_df_8,
                                 r_pl,
                                 r_it,
                                 pus,
                                 ius,
                                 "Urnings2",
                                 TRUE,
                                 compare_IRT = IRT_8)
    
    game = analyse(game_type, omit_message = FALSE, save_iter = 1000000, stop = FALSE)
    
    outcome[counter,] = game$est_player
    counter = counter + 1
  }
}


saveRDS(outcome, "empirical_example_G8.rds")
