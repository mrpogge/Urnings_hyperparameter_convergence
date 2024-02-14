################################################################################
#post-hoc urnsizes
################################################################################
post_hoc_urnsize = readRDS("post_hoc_moreiter_discrete_25.rds")
colnames(post_hoc_urnsize)[3] = "amount_of_change"

change_matrix_discrete_25_ph = matrix(rep(post_hoc_urnsize[,"true_value_first"], 1000), nrow = nrow(post_hoc_urnsize), ncol = 1000)
change_matrix_discrete_25_ph = log(change_matrix_discrete_25_ph / (1- change_matrix_discrete_25_ph))

jumps = rep(0:24, each = 40)
change_per_jump = post_hoc_urnsize$amount_of_change

change_matrix_discrete_25_ph = change_matrix_discrete_25_ph + outer(change_per_jump, jumps, "*")
change_matrix_discrete_25_ph = exp(change_matrix_discrete_25_ph) / (1 + exp(change_matrix_discrete_25_ph))

post_hoc_helper = (post_hoc_urnsize %>% select(starts_with("iter")) - change_matrix_discrete_25_ph)^2
post_hoc_mse = post_hoc_urnsize %>% select(-starts_with("coverage")) 
post_hoc_mse[,6:1005] = post_hoc_helper
rm(post_hoc_helper)

################################################################################
#baseline post_hoc urn sizes
################################################################################
baseline_post_hoc = readRDS("post_hoc_moreiter_baseline_25.rds")
baseline_post_hoc = baseline_post_hoc / as.numeric(post_hoc_urnsize[,1])
baseline_post_hoc = (baseline_post_hoc - change_matrix_discrete_25_ph) ^ 2
baseline_post_hoc = cbind(post_hoc_urnsize[,1:5], baseline_post_hoc)
colnames(baseline_post_hoc) = c(colnames(post_hoc_urnsize[,1:5]), paste0("iter", c(1:1000)))



################################################################################
#simulation with discrete change
################################################################################
post_hoc_me = post_hoc_mse %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

b_post_hoc_me = baseline_post_hoc %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size,starts_with("iter"))

post_hoc_difference = post_hoc_me[,-1] - b_post_hoc_me[,-1]

plot(as.vector(unlist(urn_size_difference[4,])), type = "l", ylim = c(-0.001, 0.004), ylab = "MSE Difference")
lines(as.vector(unlist(urn_size_difference[1,])), col = 2)
lines(as.vector(unlist(urn_size_difference[2,])), col = 3)
lines(as.vector(unlist(urn_size_difference[3,])), col = 4)
lines(as.vector(unlist(post_hoc_difference[3,])), col = 5)
lines(as.vector(unlist(post_hoc_difference[4,])), col = 6)
lines(as.vector(unlist(post_hoc_difference[1,])), col = 7)
lines(as.vector(unlist(post_hoc_difference[2,])), col = 8)



