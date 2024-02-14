library(tidyverse)
library(png)
library(grid)

################################################################################
#simulation with linear change
################################################################################
post_hoc_urnsize = readRDS("post_hoc_urnsize.rds")
colnames(post_hoc_urnsize)[3] = "amount_of_change"

################################################################################
#recreating linear change
################################################################################
change_matrix_linear = matrix(0, nrow = nrow(post_hoc_urnsize), ncol = 500)
change_matrix_linear[,1] = log(post_hoc_urnsize[,"true_value_first"] / (1-post_hoc_urnsize[,"true_value_first"]))
for(cl in 2:ncol(change_matrix_linear)){
  change_matrix_linear[, cl] = change_matrix_linear[, cl-1] + post_hoc_urnsize[, "amount_of_change"]
}
change_matrix_linear = exp(change_matrix_linear) / (1 + exp(change_matrix_linear))

change_matrix_avg = cbind(post_hoc_urnsize[,1:5], change_matrix_linear)
colnames(change_matrix_avg) = c(colnames(post_hoc_urnsize[,1:5]), paste0("iter", c(1:500)))

################################################################################
#Main effect change
################################################################################
# this is the analysis we are looking for
change_me = post_hoc_urnsize %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

plot(as.vector(unlist(change_me[1,-c(1)])), type = "l", ylim = c(0.35, 0.7), ylab = "Mean Estimate")
lines(as.vector(unlist(change_me[2,-c(1)])), col = 2)
lines(as.vector(unlist(change_me[3,-c(1)])), col = 3)
lines(as.vector(unlist(change_me[4,-c(1)])), col = 4)
lines(as.vector(unlist(change_me[5,-c(1)])), col = 5)

true_change_me = change_matrix_avg %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change,starts_with("iter"))

lines(as.vector(unlist(true_change_me[1,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_change_me[2,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_change_me[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_change_me[4,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_change_me[5,-c(1,2)])), col = 5, lty = "dotted")

################################################################################
#Interaction between urn sizes and change
################################################################################
# this is the analysis we are looking for
changeXurnsize = post_hoc_urnsize %>%
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

plot(as.vector(unlist(changeXurnsize[11,-c(1,2)])), type = "l", ylim = c(0.35, 0.7), ylab = "Mean Estimate", main = "Urn size = 80")
lines(as.vector(unlist(changeXurnsize[12,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXurnsize[13,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXurnsize[14,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXurnsize[15,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXurnsize[11,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[12,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[13,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[14,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[15,-c(1,2)])), col = 5, lty = "dotted")

plot(as.vector(unlist(changeXurnsize[16,-c(1,2)])), type = "l", ylim = c(0.35, 0.7), ylab = "Mean Estimate", main = "Urn size = 96")
lines(as.vector(unlist(changeXurnsize[17,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXurnsize[18,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXurnsize[19,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXurnsize[20,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXurnsize[16,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[17,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[18,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[19,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[20, -c(1,2)])), col = 5, lty = "dotted")


plot(as.vector(unlist(changeXurnsize[1,-c(1,2)])), type = "l", ylim = c(0.35, 0.7), ylab = "Mean Estimate", main = "Urn size = 112")
lines(as.vector(unlist(changeXurnsize[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXurnsize[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXurnsize[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXurnsize[5,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXurnsize[1,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[2,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[4,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[5,-c(1,2)])), col = 5, lty = "dotted")


plot(as.vector(unlist(changeXurnsize[6,-c(1,2)])), type = "l", ylim = c(0.35, 0.7), ylab = "Mean Estimate", main = "Urn size = 128")
lines(as.vector(unlist(changeXurnsize[7,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXurnsize[8,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXurnsize[9,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXurnsize[10,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXurnsize[6,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[7,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[8,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[9,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXurnsize[10,-c(1,2)])), col = 5, lty = "dotted")




################################################################################
#Linear change and adaptivity
################################################################################
# this is the analysis we are looking for
changeXadapt = post_hoc_urnsize %>%
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

plot(as.vector(unlist(changeXadapt[16,-c(1,2)])), type = "l", ylim = c(0.35, 0.7), ylab = "Mean Estimate", main = "Non-adaptive")
lines(as.vector(unlist(changeXadapt[17,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXadapt[18,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXadapt[19,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXadapt[20,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXadapt[16,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[17,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[18,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[19,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[20, -c(1,2)])), col = 5, lty = "dotted")


plot(as.vector(unlist(changeXadapt[1,-c(1,2)])), type = "l", ylim = c(0.35, 0.7), ylab = "Mean Estimate", main = "Adaptive 0.5")
lines(as.vector(unlist(changeXadapt[2,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXadapt[3,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXadapt[4,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXadapt[5,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXadapt[1,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[2,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[3,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[4,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[5,-c(1,2)])), col = 5, lty = "dotted")


plot(as.vector(unlist(changeXadapt[6,-c(1,2)])), type = "l", ylim = c(0.35, 0.7), ylab = "Mean Estimate", main = "Adaptive 0.7")
lines(as.vector(unlist(changeXadapt[7,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXadapt[8,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXadapt[9,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXadapt[10,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXadapt[6,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[7,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[8,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[9,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[10,-c(1,2)])), col = 5, lty = "dotted")


plot(as.vector(unlist(changeXadapt[11,-c(1,2)])), type = "l", ylim = c(0.35, 0.7), ylab = "Mean Estimate", main = "Adaptive sigma")
lines(as.vector(unlist(changeXadapt[12,-c(1,2)])), col = 2)
lines(as.vector(unlist(changeXadapt[13,-c(1,2)])), col = 3)
lines(as.vector(unlist(changeXadapt[14,-c(1,2)])), col = 4)
lines(as.vector(unlist(changeXadapt[15,-c(1,2)])), col = 5)

lines(as.vector(unlist(true_changeXadapt[11,-c(1,2)])), col = 1, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[12,-c(1,2)])), col = 2, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[13,-c(1,2)])), col = 3, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[14,-c(1,2)])), col = 4, lty = "dotted")
lines(as.vector(unlist(true_changeXadapt[15,-c(1,2)])), col = 5, lty = "dotted")

