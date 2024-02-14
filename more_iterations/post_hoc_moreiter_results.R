#adding the posthoc urn sizes
################################################################################
#simulation with linear change
################################################################################
post_hoc_moreiter = readRDS("post_hoc_moreiter.rds")
colnames(post_hoc_moreiter)[3] = "amount_of_change"

################################################################################
#recreating linear change for post_hoc urn sizes
################################################################################
change_matrix_ph = matrix(0, nrow = nrow(post_hoc_moreiter), ncol = 1000)
change_matrix_ph[,1] = log(post_hoc_moreiter[,"true_value_first"] / (1-post_hoc_moreiter[,"true_value_first"]))
for(cl in 2:ncol(change_matrix_ph)){
  change_matrix_ph[, cl] = change_matrix_ph[, cl-1] + post_hoc_moreiter[, "amount_of_change"]
}
change_matrix_ph = exp(change_matrix_ph) / (1 + exp(change_matrix_ph))

change_matrix_avg = cbind(post_hoc_moreiter[,1:5], change_matrix_ph)
colnames(change_matrix_avg) = c(colnames(post_hoc_moreiter[,1:5]), paste0("iter", c(1:1000)))

post_hoc_helper = (post_hoc_moreiter %>% select(starts_with("iter")) - change_matrix_ph)^2

post_hoc_mse = post_hoc_moreiter %>% select(-starts_with("coverage")) 
post_hoc_mse[,6:1005] = post_hoc_helper
rm(post_hoc_helper)

################################################################################
#baseline post_hoc urn sizes
################################################################################
baseline_post_hoc = readRDS("post_hoc_moreiter_baseline.rds")
baseline_post_hoc = baseline_post_hoc / as.numeric(post_hoc_moreiter[,1])
baseline_post_hoc = (baseline_post_hoc - change_matrix_ph) ^ 2
baseline_post_hoc = cbind(post_hoc_moreiter[,1:5], baseline_post_hoc)
colnames(baseline_post_hoc) = c(colnames(post_hoc_moreiter[,1:5]), paste0("iter", c(1:1000)))


post_hoc_me = post_hoc_mse %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

b_post_hoc_me = baseline_post_hoc %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

post_hoc_difference = post_hoc_me[,-1] - b_post_hoc_me[,-1]

plot(as.vector(unlist(post_hoc_difference[6,])), type = "l", ylim = c(-0.001, 0.003), ylab = "MSE Difference")
lines(as.vector(unlist(post_hoc_difference[3,])), col = 2)
lines(as.vector(unlist(post_hoc_difference[4,])), col = 3)
lines(as.vector(unlist(post_hoc_difference[5,])), col = 4)
lines(as.vector(unlist(post_hoc_difference[7,])), col = 5)
lines(as.vector(unlist(post_hoc_difference[8,])), col = 6)
lines(as.vector(unlist(post_hoc_difference[1,])), col = 7)
lines(as.vector(unlist(post_hoc_difference[2,])), col = 8)

plot(as.vector(unlist(post_hoc_difference[5,])), type = "l", ylim = c(-0.001, 0.003), ylab = "MSE Difference", col = 4)
lines(as.vector(unlist(post_hoc_difference[7,])), col = 5)
lines(as.vector(unlist(post_hoc_difference[8,])), col = 6)
lines(as.vector(unlist(post_hoc_difference[1,])), col = 7)
lines(as.vector(unlist(post_hoc_difference[2,])), col = 8)

