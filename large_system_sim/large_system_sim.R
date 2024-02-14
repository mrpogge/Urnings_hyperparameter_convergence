source("urnings_handler.R")

set.seed(13181913)

#environment settings
ngames = 500
nplayers = 45000
nitems = 1500

#fixed factors
pi_pl = qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers))
pi_pl = exp(pi_pl) / (1 + exp(pi_pl)) 
pi_pl = sort(pi_pl)
pi_it = qnorm(seq(1/(nitems+1),nitems/(nitems+1),length=nitems))
pi_it = exp(pi_it) / (1 + exp(pi_it)) 
pi_it = sort(pi_it)

#between simulation factors
# urn sizes
player_urn_sizes  =  c(8,16,32,64)
item_urn_sizes = 64

#adaptive item selection
ad = matrix(c(0,0.5), nrow = 1, ncol = 2)
ad_text = c("adaptive")
ad_name = c("adaptive50")

#player percent being invariant
player_percent = c(100) # 999 means total cold start

#paired update

pu = c(TRUE)

# output
results = matrix(0, nrow = nplayers*4, ncol = 9 + ngames)
results = as.data.frame(results)
colnames_fix = c("player_urn_size",
                 "item_urn_size",
                 "adapt",
                 "player_percent",
                 "algo",
                 "paired_update",
                 "dist_type",
                 "player_label",
                 "true_value")
colnames_iter = paste0("iter", 1:(ngames))
colnames(results) = c(colnames_fix, colnames_iter)

#simulation 
counter = 1
for(pus in player_urn_sizes){ #4
  for(ius in item_urn_sizes){ #4
    for(ads in 1:length(ad_text)){ #4
      for(p in pu){ #2
        for(cs in player_percent){
          
          #parameter combination
          message = paste0(pus," ",
                           ius," ",
                           p," ",
                           ad_name[ads], " ",
                           "Urnings2", " ",
                           "central", " ",
                           cs)
          print(message)
          
          #starting values and urning values
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
          }
          
          #setting up the urnings factory 
          game_type = urnings_game(r_pl, 
                                   r_it,
                                   pus,
                                   ius,
                                   pi_pl,
                                   pi_it,
                                   ngames,
                                   "Urnings2",
                                   ad_text[ads],
                                   p,
                                   mu = ad[ads,1],
                                   sigma = ad[ads, 2],
                                   coverage = FALSE)
          game = play(game_type, omit_message = TRUE)
          
          #saving results
          #creating fix cols
          params = c(pus,ius,ad_name[ads], cs, "Urnings2",p ,"central")
          params = matrix(rep(params, times = nplayers), 
                          nrow = nplayers, 
                          ncol = length(params), 
                          byrow=TRUE)
          results[counter:(counter+nplayers-1), 1:7] = params
          results[counter:(counter+nplayers-1), 8] = player_label
          results[counter:(counter+nplayers-1), 9] = pi_pl
          results[counter:(counter+nplayers-1), 10:(9+ngames)] = game
          
          counter = counter + nplayers
        }
      }
    }
  }
}
saveRDS(results, "large_systems_sim.rds")
