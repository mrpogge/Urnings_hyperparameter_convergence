library(haven)
library(eRm)
library(tidyverse)
source("urnings_handler.R")

set.seed(13181913)
#reading sav files
df_10 = read_sav("OKM_2014_10.sav")

################################################################################
# 10th grade mathematics
################################################################################
#selecting the math items
df_10 = as.matrix(df_10 %>%
                    select(starts_with("M"), -starts_with("M_zpsc")))

#selecting items with binary outcomes
indicator = logical(ncol(df_10))
for(i in 1:ncol(df_10)){
  indicator[i] = 2 %in% df_10[, i]
}

df_10 = df_10[,!indicator]

#fitting rasch for the 10th grade
rasch_10 = RM(df_10)
pp10 = person.parameter(rasch_10)
pp10 = as.vector(coef(pp10))
ip10 = -rasch_10[["betapar"]]
IRT_10 = list(pp10, ip10)
################################################################################
# Preparing data for Urnings based analysis
################################################################################
#grade 10
colnames(df_10) = 1:ncol(df_10)
n_players = nrow(df_10)
n_items = ncol(df_10)
player_codes = data.frame("players" = 1:n_players)
df_10 = cbind(player_codes, df_10)


long_df_10 = pivot_longer(df_10, cols = -players, names_to = "items", values_to = "result", values_drop_na = TRUE)
random_indicies = sample(nrow(long_df_10))
long_df_10 = long_df_10[random_indicies, ]
long_df_10$items = as.integer(long_df_10$items)
long_df_10 = as.matrix(long_df_10)
rm(player_codes, random_indicies)

################################################################################
# Preparing settings for Urnings based analysis
################################################################################
player_urn_sizes = c(8,16,32,64)
item_urn_sizes = c(16,32,64,128)

item_true_like = exp(ip10)/(1+exp(ip10))

outcome = matrix(0, nrow = 2 + 16, ncol = n_players)
outcome[1,] = IRT_10[[1]]
counter = 2
for(pus in player_urn_sizes){
  for(ius in item_urn_sizes){
    r_pl = numeric(n_players) + as.integer(pus / 2)
    r_it = unlist(lapply(item_true_like, rbinom, n = 1, size = ius))
    game_type = urnings_analysis(long_df_10,
                                 r_pl,
                                 r_it,
                                 pus,
                                 ius,
                                 "Urnings2",
                                 TRUE,
                                 compare_IRT = IRT_10)
    
    game = analyse(game_type, omit_message = FALSE, save_iter = 1000000, stop = FALSE)
    
    outcome[counter,] = game$est_player
    counter = counter + 1
  }
}

saveRDS(outcome, "empirical_example_G10_warm.rds")
