source("urnings_handler.R")

set.seed(13181913)

#environment settings
ngames = 500
nplayers = 4500
nitems = 1000

#true values
#true values
pi_pl = qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers), 1,1)
pi_pl = exp(pi_pl) / (1 + exp(pi_pl)) 
pi_pl = sample(pi_pl, length(pi_pl), replace = FALSE)
pi_it = pi_it = qnorm(seq(1/(nitems+1),nitems/(nitems+1),length=nitems))
pi_it = exp(pi_it) / (1 + exp(pi_it)) 
pi_it = sort(pi_it)

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

#item starting
r_it = numeric(nitems)
first_half = unlist(lapply(pi_it[1:500], rbinom, n = 1, size = 64))
second_half = 64 - rev(first_half)
r_it = c(first_half, second_half)

#change
#discrete positive
logit_changes = c(-0.5, 0, 0.5, 1,2)
change_per_jump = rep(logit_changes / 10, times = 900)
jump_matrix = matrix(0, nrow = nplayers, ncol = ngames)
jump_matrix[,seq(51,451,50)] = rep(1, times = nplayers)

change_matrix = matrix(0, nrow = nplayers, ncol = ngames)
change_matrix[,1] = log(pi_pl / (1-pi_pl)) + change_per_jump
for(i in 2:ngames){
  change_matrix[,i] = change_matrix[,i-1] + jump_matrix[,i] * change_per_jump
}

change_matrix = exp(change_matrix) / (1 + exp(change_matrix))
# output
results = matrix(0, nrow = nplayers*4*4, ncol = 6 + ngames + ngames -1)
results = as.data.frame(results)
colnames_fix = c("player_urn_size",
                 "adapt",
                 "amount_of_change",
                 "when",
                 "true_value_first")
colnames_iter = paste0("iter", 1:(ngames))
colnames_pred = paste0("coverage", 1:(ngames))
colnames(results) = c(colnames_fix, colnames_iter, colnames_pred)

#simulation 
counter = 1
for(pus in player_urn_sizes){ #4
  for(ads in 1:length(ad_text)){ #4
    
    #parameter combination
    message = paste0(pus," ",
                     ad_name[ads], " ",
                     "discrete")
    print(message)
    
    #starting values and urning values
    r_pl = numeric(nplayers) + unlist(lapply(pi_pl, rbinom, n = 1, size = pus))
    
    
    #setting up the urnings factory 
    game_type = urnings_game(r_pl, 
                             r_it,
                             pus,
                             64,
                             change_matrix,
                             pi_it,
                             ngames,
                             "Urnings2",
                             ad_text[ads],
                             TRUE,
                             mu = ad[ads,1],
                             sigma = ad[ads, 2],
                             coverage = TRUE,
                             change = TRUE)
    game = play(game_type, omit_message = TRUE)
    
    #saving results
    #creating fix cols
    params = c(pus,ad_name[ads])
    params = matrix(rep(params, times = nplayers), 
                    nrow = nplayers, 
                    ncol = length(params), 
                    byrow=TRUE)
    results[counter:(counter+nplayers-1), 1:2] = params
    results[counter:(counter+nplayers-1), 3] = change_per_jump
    results[counter:(counter+nplayers-1), 4] = rep(250, times = nplayers)
    results[counter:(counter+nplayers-1), 5] = change_matrix[,1]
    results[counter:(counter+nplayers-1), 6:(6+ngames+ngames-1)] = game
    
    counter = counter + nplayers
  }
}

saveRDS(results, "sim2_discrete_better_10.rds")

