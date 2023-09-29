library(haven)
library(eRm)
library(tidyverse)
source("urnings_handler.R")

#reading sav files
df_6 = read_sav("OKM_2014_06.sav")
df_8 = read_sav("OKM_2014_08.sav")
df_10 = read_sav("OKM_2014_10.sav")

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
# 8th grade mathematics
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

#fitting rasch for the 8th grade
rasch_8 = RM(df_8)
pp8 = person.parameter(rasch_8)
pp8 = as.vector(coef(pp8))
ip8 = -rasch_8[["betapar"]]
IRT_8 = list(pp8, ip8)
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
# grade6
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

#grade 8 
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
n_i = n_j = 32
r_pl = numeric(n_players) + as.integer(n_i / 2)
r_it = numeric(n_items) + as.integer(n_j / 2)

game_type = urnings_analysis(long_df_6,
                             r_pl,
                             r_it,
                             n_i,
                             n_j,
                             "Urnings2",
                             TRUE,
                             compare_IRT = IRT_6)

game = analyse(game_type, omit_message = FALSE, save_iter = 100000, stop = FALSE)

plot(game$compare_IRT[1,], type = "l", ylim = c(0,1))
plot(game$est_player, exp(IRT_6[[1]]) / (1 + exp(IRT_6[[1]])))
abline(0,1)
cor(game$est_player, exp(IRT_6[[1]]) / (1 + exp(IRT_6[[1]])))

    