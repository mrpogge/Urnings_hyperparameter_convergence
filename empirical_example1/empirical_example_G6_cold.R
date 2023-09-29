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
player_codes = data.frame("players" = 1:n_players)
df_6 = cbind(player_codes, df_6)


long_df_6 = pivot_longer(df_6, cols = -players, names_to = "items", values_to = "result", values_drop_na = TRUE)
random_indicies = sample(nrow(long_df_6))
long_df_6 = long_df_6[random_indicies, ]
long_df_6$items = as.integer(long_df_6$items)
long_df_6 = as.matrix(long_df_6)
rm(player_codes, random_indicies)

################################################################################
# Preparing settings for Urnings based analysis
################################################################################
player_urn_sizes = c(8,16,32,64)
item_urn_sizes = c(16,32,64,128)

outcome = matrix(0, nrow = 2 + 16, ncol = n_players)
outcome[1,] = IRT_6[[1]]
counter = 2
for(pus in player_urn_sizes){
  for(ius in item_urn_sizes){
    r_pl = numeric(n_players) + as.integer(pus / 2)
    r_it = numeric(n_items) + as.integer(ius / 2)
    game_type = urnings_analysis(long_df_6,
                                 r_pl,
                                 r_it,
                                 pus,
                                 ius,
                                 "Urnings2",
                                 TRUE,
                                 compare_IRT = IRT_6)
    
    game = analyse(game_type, omit_message = FALSE, save_iter = 1000000, stop = FALSE)
    
    outcome[counter,] = game$est_player
    counter = counter + 1
  }
}

saveRDS(outcome, "empirical_example_G6.rds")
