library(tidyverse)

#Urnings1 vs Urnings 2 total
results = readRDS("collapsed_results.rds")

results_u2 = results %>% filter(algo == "Urnings2")
results_u1 = results %>% filter(algo == "Urnings1")

diff_u1_u2 = results_u2[,-c(1:7)] - results_u1[,-c(1:7)]

plot(rowMeans(diff_u1_u2))
which.max(rowMeans(diff_u1_u2))
mean(rowMeans(diff_u1_u2))

plot(unlist(results_u2[1250,-c(1:7)]), type = "l", ylim = c(0,0.07))
lines(unlist(results_u1[1250,-c(1:7)]), col = "red")
results_u2[1300,c(1:7)]
results_u1[1300,c(1:7)]
results_u2[1250:1536,c(1:7)]

#paired_update total
results_TRUE = results %>% filter(dist_type == "central", paired_update == "TRUE")
results_FALSE = results %>% filter(dist_type == "central", paired_update == "FALSE")

diff_pu = results_TRUE[,-c(1:7)] - results_FALSE[,-c(1:7)]
plot(rowMeans(diff_pu))

which.max(rowMeans(diff_pu))
mean(rowMeans(diff_pu))

plot(unlist(results_TRUE[500,-c(1:7)]), type = "l", ylim = c(0,0.07))
lines(unlist(results_FALSE[500,-c(1:7)]), col = "red")
results_TRUE[472,c(1:7)]
results_FALSE[472,c(1:7)]


#do it with the last (few) iterations, and the baseline "should" be an interval 
#write this to the paper about the paired update: it converges to the right invariant distribution
baseline = readRDS("collapsed_baseline_MSE.rds")
baseline_TRUE = baseline %>% filter(paired_update == "TRUE")
results_TRUE = results %>% filter(paired_update == "TRUE") 

diff_baseline = results_TRUE[,-c(1:7)] - baseline_TRUE[,-c(1:7)]
plot(rowMeans(diff_baseline))
which.max(rowMeans(diff_baseline))
plot(unlist(results_TRUE[1234,-c(1:7)]), type = "l", ylim = c(0,0.07))
lines(unlist(baseline_TRUE[1234,-c(1:7)]), col = "red")

baseline_FALSE = baseline %>% filter(paired_update == "FALSE")
results_FALSE = results %>% filter(paired_update == "FALSE") 
diff_baseline_FALSE = results_FALSE[,-c(1:7)] - baseline_FALSE[,-c(1:7)]
plot(rowMeans(diff_baseline_FALSE))
which.max(rowMeans(diff_baseline_FALSE))
plot(unlist(results_FALSE[468,-c(1:7)]), type = "l", ylim = c(0,0.07))
lines(unlist(baseline_FALSE[468,-c(1:7)]), col = "red")
