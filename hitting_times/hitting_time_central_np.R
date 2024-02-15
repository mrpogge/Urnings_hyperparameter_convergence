source("hitting_times/hitting_times_wrapper.R")

#environment settings
ngames = 1000
nplayers = 900
nitems = 200
nreps = 100

#fixed factors
pi_pl = qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers), 0, 1)
pi_pl = exp(pi_pl) / (1 + exp(pi_pl)) 
pi_pl = sort(pi_pl)
pi_it = qnorm(seq(1/(nitems+1),nitems/(nitems+1),length=nitems))
pi_it = exp(pi_it) / (1 + exp(pi_it)) 
pi_it = sort(pi_it)

#between simulation factors
# urn sizes
player_urn_sizes  =  c(8,16,32,64)
item_urn_sizes = c(16,32,64,128)

#adaptive item selection
ad = matrix(0, nrow = 4, ncol = 2)
ad[1:2,1] = 0
ad[3,1] = log(0.7/0.3)
ad[c(1,3), 2] = 0.5
ad[2,2] = 0.25
ad[4, ] = c(0,0.5)
ad_text = c("adaptive", "adaptive", "adaptive", "n_adaptive")
ad_name = c("adaptive50", "adaptive_sigma", "adaptive70", "n_adaptive")

#player percent being invariant
player_percent = c(999,100, 50, 10) # 999 means total cold start

#paired update

pu = c(FALSE)

#baseline 975 quantile
baseline_mse_total = readRDS("output/mse_baseline_975.rds")
baseline_mse_total = baseline_mse_total[baseline_mse_total[, 1] == 0, 2:3]

# output
results = matrix(0, nrow = 4*4*4*4*2, ncol = 7 + nreps)
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
for(pus in player_urn_sizes){ #4
  for(ius in item_urn_sizes){ #4
    for(ads in 1:length(ad_text)){ #4
      for(p in pu){ #2
        for(cs in player_percent){
          for(r in 1:nreps){
            
            #parameter combination
            message = paste0(pus," ",
                             ius," ",
                             p," ",
                             ad_name[ads], " ",
                             "Urnings2", " ",
                             "better", " ",
                             cs)
            print(message)
            
            #starting values and urning values
            indexes = sample(1:nplayers, nplayers, replace = FALSE)
            if(cs == 999){
              r_pl = numeric(length = nplayers) + as.integer(pus / 2)
              r_it = numeric(length = nitems) + as.integer(ius / 2)
              player_label = rep(1, times = nplayers)
            } else {
              r_pl = numeric(length = nplayers) + as.integer(pus / 2)
              r_it = numeric(nitems)
              first_half = unlist(lapply(pi_it[1:(nitems/2)], rbinom, n = 1, size = ius))
              second_half = ius - rev(first_half)
              r_it = c(first_half, second_half)
              r_pl[seq(1,nplayers,1) %% (100 / cs) != 0] = unlist(lapply(pi_pl[seq(1,nplayers,1) %% (100 / cs) != 0], 
                                                                         rbinom, n = 1, size = pus))
              player_label = as.numeric(seq(1,nplayers,1) %% (100 / cs) == 0)
              player_label = player_label[indexes]
            }
            
            #randomly permutating players
            r_pl = r_pl[indexes]
            pi_pl = pi_pl[indexes]
            Theta = log(pi_pl / (1 - pi_pl))
            Delta = log(pi_it / (1 - pi_it))
            
            mse_baseline = baseline_mse_total[baseline_mse_total[,1] == pus, 2]
            
            
            HTs = hitting_times(r_pl,
                                r_it,
                                Theta,
                                Delta,
                                nplayers,
                                nitems,
                                ngames,
                                pus,
                                ius,
                                as.integer(ad_text[ads] == "adaptive"),
                                ad[ads,1],
                                ad[ads, 2],
                                as.integer(p),
                                mse_baseline,
                                returns = "simple")
            
            #saving results
            #creating fix cols
            params = c(pus,ius,ad_name[ads], cs, "Urnings2",p ,"better")
            results[counter, 1:7] = params
            results[counter, r+7] = HTs
            
          }
          counter = counter + 1
        }
      }
    }
  }
}
saveRDS(results, "output/ht_central_np.rds")
