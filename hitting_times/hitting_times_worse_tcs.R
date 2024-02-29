source("hitting_times/hitting_times_wrapper.R")

#environment settings
ngames = 500
nplayers = 900
nitems = 200
nreps = 100

#between simulation factors
# urn sizes
player_urn_sizes  =  c(8,16,32,64)
item_urn_sizes = c(16,32,64,128)

true_mus = c(1, 0, -1)


#adaptive item selection
ad = matrix(0, nrow = 4, ncol = 2)
ad[1:2,1] = 0
ad[3,1] = log(0.7/0.3)
ad[c(1,3), 2] = 0.5
ad[2,2] = 0.25
ad[4, ] = c(0,0.5)
ad_text = c("adaptive", "adaptive", "adaptive", "n_adaptive")
ad_name = c("adaptive50", "adaptive_sigma", "adaptive70", "n_adaptive")
ad_index = 1:4

#paired update
pu = c(TRUE)

#design matrix
Design = expand.grid(player_urn_sizes, item_urn_sizes, true_mus, ad_index, pu)
Design= Design[Design[,3] == -1, ]

#baseline 975 quantile
baseline_mse_total = readRDS("output/mse_baseline_975.rds")
baseline_mse_total = baseline_mse_total[baseline_mse_total[, 1] == -1, 2:3]

# output
results = matrix(0, nrow = nrow(Design), ncol = 7 + nreps)
results = as.data.frame(results)
colnames_fix = c("player_urn_size",
                 "item_urn_size",
                 "adapt",
                 "player_percent",
                 "algo",
                 "paired_update",
                 "dist_type")
colnames_iter = paste0("HT", 1:(nreps))
colnames(results) = c(colnames_fix, colnames_iter)

#simulation 
counter = 1
for(i in 1:nrow(Design)){
  for(r in 1:nreps){
    #parameter combination
    indexing = sample(1:nplayers, replace = FALSE)
    #starting values and urning values
    pi_pl = qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers), Design[i,3], 1)
    pi_it = qnorm(seq(1/(nitems+1),nitems/(nitems+1),length=nitems))
    pi_pl = pi_pl[indexing]
    
    
    r_pl = numeric(length = nplayers) + as.integer(Design[i,1] / 2)
    r_it = numeric(length = nitems) + as.integer(Design[i,2] / 2)
    r_pl = r_pl[indexing]
    
    Theta = matrix(rep(pi_pl, times = ngames), ncol = ngames, nrow = nplayers, byrow = TRUE)
    Delta = matrix(rep(pi_it, times = ngames), ncol = ngames, nrow = nitems, byrow = TRUE)
    
    mse_baseline = baseline_mse_total[baseline_mse_total[,1] == Design[i,1], 2]
    
    ad_text
    
    HTs = hitting_times(r_pl,
                        r_it,
                        pi_pl,
                        pi_it,
                        nplayers,
                        nitems,
                        ngames,
                        Design[i,1],
                        Design[i,2],
                        as.integer(Design[i,4] < 4),
                        ad[Design[i,4],1],
                        ad[Design[i,4], 2],
                        as.integer(Design[i,5]),
                        mse_baseline,
                        returns = "total")[[23]]
    
    #saving results
    #creating fix cols
    dist_type = switch(as.character(Design[i,3]), "1" = "better", "0" = "central", "-1" = "worse")
    params = c(Design[i,1],Design[i,2],ad_name[Design[i,4]], 999, "Urnings2",TRUE ,dist_type)
    results[counter, 1:7] = params
    results[counter, r+7] = HTs
  }
  counter = counter + 1
  print(Design[i,])
}



saveRDS(results, "output/ht_worse_tcs.rds")

