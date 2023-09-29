source("urnings_handler.R")
source("outcome_measures.R")

set.seed(13181913)

#environment settings
ngames = 500
nplayers = 900
nitems = 200

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
player_urn_sizes  =  c(64)

#player percent being invariant
player_percent = c(100, 50, 30, 10)

#true distribution type
true_dist_player = c("uniform", "normal_quantiles", "discrete_uniform", "normal_quantiles_ncentral")
true_dist_item = c("uniform", "normal_quantiles", "discrete_uniform", "normal_quantiles_ncentral")

# output
results = matrix(0, nrow = nplayers*4*4*4*4, ncol = 8 + ngames + ngames -1)
results = as.data.frame(results)
colnames_fix = c("player_urn_size",
                 "adapt",
                 "player_percent",
                 "true_dist_player",
                 "true_dist_item",
                 "player_label",
                 "true_value")
colnames_iter = paste0("iter", 1:(ngames))
colnames_pred = paste0("pred", 1:(ngames))
colnames(results) = c(colnames_fix, colnames_iter, colnames_pred)

#simulation 
counter = 1
for(pus in player_urn_sizes){ 
  for(ads in 1:length(ad_text)){
    for(ppc in player_percent){
      for(tdp in true_dist_player){
        for(tdi in true_dist_item){
        
      
      message = paste0(pus," ",
                       ad_name[ads]," ",
                       ppc," ",
                       tdp, " ",
                       tdi)
      print(message)
      
      #setting true_values
      pi_pl = switch(
        tdp,
        "uniform" = runif(nplayers, 0,1),
        "normal_quantiles" = exp(qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers))) /
          (1+(exp(qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers))))),
        "discrete_uniform" = sample(seq(0.1,0.9,0.1), nplayers, replace = TRUE),
        "normal_quantiles_ncentral" = exp(qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers), 
          mean = exp(1)/(1+exp(1)))) / 
          (1 + exp(qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers),
          mean = exp(1)/(1+exp(1)))))
      )
      pi_it = switch(
        tdi,
        "uniform" = runif(nitems, 0,1),
        "normal_quantiles" = exp(qnorm(seq(1/(nitems+1),nitems/(nitems+1),length=nitems))) /
          (1+(exp(qnorm(seq(1/(nitems+1),nitems/(nitems+1),length=nitems))))),
        "discrete_uniform" = sample(seq(0.1,0.9,0.1), nitems, replace = TRUE),
        "normal_quantiles_ncentral" = exp(qnorm(seq(1/(nitems+1),nitems/(nitems+1),length=nitems), 
                                                mean = exp(1)/(1+exp(1)))) / 
          (1 + exp(qnorm(seq(1/(nitems+1),nitems/(nitems+1),length=nitems),
                         mean = exp(1)/(1+exp(1)))))
      )
      
      #starting values
      r_it = numeric(nitems)
      first_half = unlist(lapply(pi_it[1:100], rbinom, n = 1, size = 64))
      second_half = 64 - first_half
      r_it = c(first_half, second_half)
      
      if(ppc == 100){
        r_pl = numeric(nplayers) + as.integer(pus/2)
      } else {
        for(k in 1:length(r_pl)){
          r_pl[k] =  rbinom(1, pus, pi_pl[k])
        }
        
        for(i in 1:9){
          for(j in 1:ppc){
            r_pl[(100 *i) -100 +j] = as.integer(pus/2)
          }
        }
      }
      
      player_label = rep(c(rep("new", times = ppc), rep("invar", times = 100 - ppc)), times = 9)
      
      #setting up the urnings factory 
      game_type = urnings_game(r_pl, 
                               r_it,
                               pus,
                               64,
                               pi_pl,
                               pi_it,
                               ngames,
                               "Urnings2",
                               ad_text[ads],
                               TRUE,
                               mu = ad[ads,1],
                               sigma = ad[ads, 2],
                               predict = TRUE,
                               change = FALSE)
      game = play(game_type, omit_message = TRUE)
      
      #saving results
      #creating fix cols
      params = c(pus,ad_name[ads],ppc, tdp, tdi)
      params = matrix(rep(params, times = nplayers), 
                      nrow = nplayers, 
                      ncol = length(params), 
                      byrow=TRUE)
      results[counter:(counter+nplayers-1), 1:5] = params
      results[counter:(counter+nplayers-1), 6] = player_label
      results[counter:(counter+nplayers-1), 7] = pi_pl
      results[counter:(counter+nplayers-1), 8:(8+ngames-1)] = game[[1]]
      results[counter:(counter+nplayers-1), (8 + ngames):(8+ngames+ngames-1)] = game[[2]]
      
      counter = counter + nplayers
        
        }
      }
    }
  }
}

saveRDS(results, "sim_posthoc2.rds")