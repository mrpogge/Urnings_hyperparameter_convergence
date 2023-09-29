source("./urnings_handler.R") 

set.seed(13181913)

#environment settings
ngames = 500
nplayers = 900
nitems = 200

#true values
pi_pl = rep(seq(0.1,0.9,0.1), each = 100)
pi_it = pi_it = qnorm(seq(1/(nitems+1),nitems/(nitems+1),length=nitems))
pi_it = exp(pi_it) / (1 + exp(pi_it)) 
pi_it = sort(pi_it)

#starting values
player_start_types = c('half')
item_start_types = c('half', 'invariant')

#paired update
pu_bool = c(TRUE)

#algorithm type
alg_type = c("Urnings1")

#adaptive item selection
ad = matrix(0, nrow = 4, ncol = 2)
ad[1:2,1] = 0
ad[3,1] = log(0.7/0.3)
ad[c(1,3), 2] = 0.5
ad[2,2] = 0.25
ad[4, ] = c(0,0)
ad_text = c("adaptive", "adaptive", "adaptive", "n_adaptive")
ad_name = c("adaptive50", "adaptive_sigma", "adaptive70", "n_adaptive")

# urn sizes
player_urn_sizes  =  c(8,16,32,64)
item_urn_sizes = c(16,32,64,128)

# output
results = matrix(0, nrow = nplayers*4*4*4*2, ncol = 8 + ngames + ngames -1)
results = as.data.frame(results)
colnames_fix = c("player_urn_size",
                 "item_urn_size",
                 "item_starting",
                 "adapt",
                 "algo",
                 "paired_update",
                 "true_value")
colnames_iter = paste0("iter", 1:(ngames))
colnames_pred = paste0("pred", 1:(ngames))
colnames(results) = c(colnames_fix, colnames_iter, colnames_pred)

#simulation 
counter = 1
for(pus in player_urn_sizes){ #4
  for(ius in item_urn_sizes){ #4
    for(ads in 1:length(ad_text)){ #4
      for(its in item_start_types){ #2
        
        #parameter combination
        message = paste0(pus," ",
                         ius," ",
                         its," ",
                         ad_name[ads], " ",
                         "Urnings1", " ",
                         TRUE)
        print(message)
        
        #starting values and urning values
        r_pl = numeric(nplayers) + as.integer(pus/2)
        if(its == "invariant"){
          r_it = numeric(nitems)
          first_half = unlist(lapply(pi_it[1:100], rbinom, n = 1, size = ius))
          second_half = ius - first_half
          r_it = c(first_half, second_half)
        } else {
          r_it = numeric(nitems) + as.integer(ius/2)        
        }
        
        #setting up the urnings factory 
        game_type = urnings_game(r_pl, 
                                 r_it,
                                 pus,
                                 ius,
                                 pi_pl,
                                 pi_it,
                                 ngames,
                                 "Urnings1",
                                 ad_text[ads],
                                 FALSE,
                                 mu = ad[ads,1],
                                 sigma = ad[ads, 2],
                                 predict = TRUE)
        game = play(game_type, omit_message = TRUE)
        
        #saving results
        #creating fix cols
        params = c(pus,ius,its,ad_name[ads],"Urnings1",FALSE)
        params = matrix(rep(params, times = nplayers), 
                        nrow = nplayers, 
                        ncol = length(params), 
                        byrow=TRUE)
        results[counter:(counter+nplayers-1), 1:6] = params
        results[counter:(counter+nplayers-1), 7] = pi_pl
        results[counter:(counter+nplayers-1), 8:(8+ngames-1)] = game[[1]]
        results[counter:(counter+nplayers-1), (8+ngames):(8+ngames+ngames-1)] = game[[2]]
        
        counter = counter + nplayers
      }
    }
  }
}
saveRDS(results, "sim1_urnings1_npu.rds")

