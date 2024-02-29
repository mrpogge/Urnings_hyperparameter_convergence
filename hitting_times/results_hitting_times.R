################################################################################
#reading the ht results
################################################################################
nreps = 100
res_HT = rbind(readRDS("output/ht_better.rds"),
               readRDS("output/ht_central.rds"),
               readRDS("output/ht_worse.rds"),
               readRDS("output/ht_better_tcs.rds"),
               readRDS("output/ht_central_tcs.rds"),
               readRDS("output/ht_worse_tcs.rds"),
               readRDS("output/ht_better_50.rds"),
               readRDS("output/ht_central_50.rds"),
               readRDS("output/ht_worse_50.rds"),
               readRDS("output/ht_better_10.rds"),
               readRDS("output/ht_central_10.rds"),
               readRDS("output/ht_worse_10.rds"))

res_HT_avg = cbind(res_HT[,1:7], rowMeans(res_HT[,8:(nreps+8-1)]))
colnames(res_HT_avg)[8] = "ht"
saveRDS(res_HT_avg, "output/hitting_times.rds")
