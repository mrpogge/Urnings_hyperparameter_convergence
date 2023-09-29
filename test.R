source("urnings_handler.R")
source("outcome_measures.R")

#params
ngames = 500
nplayers = 900
nitems = 300

player_urn_size = n_i = c(8,16,32,64)
item_urn_size = n_j = 64

pi_pl = runif(900)

pi_it = runif(300)

ad = matrix(0, nrow = 4, ncol = 2)
ad[1:2,1] = 0
ad[3,1] = log(0.7/0.3)
ad[c(1,3), 2] = 0.5
ad[2,2] = 0.25
ad[4, ] = c(0,0)
ad_text = c("adaptive", "adaptive", "adaptive", "n_adaptive")

#containers
r_pl_8 = numeric(nplayers) + as.integer(8/2)
r_pl_64 =numeric(nplayers) + as.integer(64/2)
r_it = numeric(nitems) +  unlist(lapply(pi_it, rbinom, n = 1, size = item_urn_size))

results = matrix(0, nrow = 16, ncol = ngames)

counter = 1
for(us in player_urn_size){
  for(a in 1:nrow(ad)){
    print(c(us, ad[a, ]))
    r_pl = numeric(nplayers) + as.integer(us/2)
    game_type = urnings_game(r_pl, 
                             r_it,
                             us,
                             n_j,
                             pi_pl,
                             pi_it,
                             ngames,
                             "Urnings1",
                             ad_text[a],
                             TRUE,
                             mu = ad[a,1],
                             sigma = ad[a, 2],
                             predict = TRUE)
    game = play(game_type)
    
    results[counter, ] = colMeans(game[[2]])
    counter = counter + 1
  }
}


for(us in player_urn_size){
  for(a in 1:nrow(ad)){
    print(c(us, ad[a, ]))}}
par(mfrow=c(2,2))

plot(results[1, ], type = "l", ylab = "MSE", ylim = c(0.1,0.6), main = "8")
lines(results[2, ], col = 2)
lines(results[3, ], col = 3)
lines(results[4, ], col = 4)

plot(results[5, ], type = "l", ylab = "MSE",ylim = c(0.1,0.6), main = "16")
lines(results[6, ], col = 2)
lines(results[7, ], col = 3)
lines(results[8, ], col = 4)

plot(results[9, ], type = "l", ylab = "MSE", ylim = c(0.1,0.6), main = "32")
lines(results[10, ], col = 2)
lines(results[11, ], col = 3)
lines(results[12, ], col = 4)

plot(results[13, ], type = "l", ylab = "MSE", ylim = c(0.1,0.6), main = "64")
lines(results[14, ], col = 2)
lines(results[15, ], col = 3)
lines(results[16, ], col = 4)

#showing different true value change
#linear positive
pi_pl = rep(seq(0.1,0.9,0.1), each = 100)
logit_changes = c(0, 0.25, 0.5, 1)
change_per_iteration = sample(logit_changes, 900, replace = TRUE) / ngames
change_matrix = matrix(0, nrow = nplayers, ncol = ngames)
change_matrix[,1] = log(pi_pl / (1-pi_pl))
for(i in 2:ngames){
  change_matrix[,i] = change_matrix[,i-1] + change_per_iteration
}

change_matrix = exp(change_matrix) / (1 + exp(change_matrix))
plot(change_matrix[1, ], type = "l", col = 1, ylim = c(0,1), ylab = "ability", main = "Linear positive")
for(i in 2:nplayers){
  lines(change_matrix[i, ], col = 1)
}

#linear symmetric
#showing different true value change
pi_pl = rep(seq(0.1,0.9,0.1), each = 100)
logit_changes = c(0, 0.25, 0.5, 1)
change_per_iteration = sample(logit_changes, 900, replace = TRUE) / ngames
direction = sample(c(-1,1), 900, replace = TRUE)
change_matrix = matrix(0, nrow = nplayers, ncol = ngames)
change_matrix[,1] = log(pi_pl / (1-pi_pl))
for(i in 2:ngames){
  change_matrix[,i] = change_matrix[,i-1] + direction * change_per_iteration
}

change_matrix = exp(change_matrix) / (1 + exp(change_matrix))
plot(change_matrix[1, ], type = "l", col = 1, ylim = c(0,1), ylab = "ability", main = "Linear symmetric")
for(i in 2:nplayers){
  lines(change_matrix[i, ], col = 1)
}

#discrete positive
pi_pl = rep(seq(0.1,0.9,0.1), each = 100)
logit_changes = c(0, 0.25, 0.5, 1)
change_per_jump = sample(logit_changes, 900, replace = TRUE) 
jump_matrix = matrix(0, nrow = nplayers, ncol = ngames)
jump = sample(1:500, 900, replace = TRUE)
for(i in 1:900){
  jump_matrix[i, jump[i]] = 1
}

change_matrix = matrix(0, nrow = nplayers, ncol = ngames)
change_matrix[,1] = log(pi_pl / (1-pi_pl))
for(i in 2:ngames){
  change_matrix[,i] = change_matrix[,i-1] + jump_matrix[,i-1] * change_per_jump
}

change_matrix = exp(change_matrix) / (1 + exp(change_matrix))
plot(change_matrix[1, ], type = "l", col = 1, ylim = c(0,1), ylab = "ability", main = "Discrete positive")
for(i in 2:nplayers){
  lines(change_matrix[i, ], col = 1)
}

#discrete symmetric
pi_pl = rep(seq(0.1,0.9,0.1), each = 100)
logit_changes = c(0, 0.25, 0.5, 1)
direction = sample(c(-1,1), 900, replace = TRUE)
change_per_jump = sample(logit_changes, 900, replace = TRUE) 
jump_matrix = matrix(0, nrow = nplayers, ncol = ngames)
jump = sample(1:500, 900, replace = TRUE)
for(i in 1:900){
  jump_matrix[i, jump[i]] = 1
}

change_matrix = matrix(0, nrow = nplayers, ncol = ngames)
change_matrix[,1] = log(pi_pl / (1-pi_pl))
for(i in 2:ngames){
  change_matrix[,i] = change_matrix[,i-1] + direction * jump_matrix[,i-1] * change_per_jump
}

change_matrix = exp(change_matrix) / (1 + exp(change_matrix))
plot(change_matrix[1, ], type = "l", col = 1, ylim = c(0,1), ylab = "ability", main = "Discrete symmetric")
for(i in 2:nplayers){
  lines(change_matrix[i, ], col = 1)
}

#matching probability densities
pi_pl = qnorm(seq(1/(nplayers+1),nplayers/(nplayers+1),length=nplayers))
pi_pl = exp(pi_pl) / (1 + exp(pi_pl)) 
r_pl_ph = numeric(length = nplayers) + unlist(lapply(pi_pl, rbinom, n = 1, size = 8))

probs = c()
counter = 1
for(i in 1:nplayers){
  for(j in 1:nitems){
    probs[counter] = normal_method_helper(r_pl_ph[i], r_it[j], 8, 64,0,1)
    counter = counter + 1
  }
}

hist(probs)


game_type_8 = urnings_game(r_pl_8, 
                         r_it,
                         8,
                         n_j,
                         pi_pl,
                         pi_it,
                         ngames,
                         "Urnings2",
                         ad_text[a],
                         TRUE,
                         mu = ad[a,1],
                         sigma = 0.1,
                         predict = FALSE,
                         post_hoc_probs = TRUE)
game_8 = play(game_type_8)

game_type_64 = urnings_game(r_pl_64, 
                           r_it,
                           64,
                           n_j,
                           pi_pl,
                           pi_it,
                           ngames,
                           "Urnings2",
                           ad_text[a],
                           TRUE,
                           mu = ad[a,1],
                           sigma = ad[a, 2],
                           predict = FALSE,
                           post_hoc_probs = TRUE)
game_64 = play(game_type_64)

hist(log(game_8[[2]][,400:500] / (1- game_8[[2]][,400:500])),breaks=100)
hist(log(game_64[[2]][,400:500] / (1- game_64[[2]][,400:500])),breaks=100)

mean(log(game_8[[2]][,400:500] / (1- game_8[[2]][,400:500])))
sd(log(game_8[[2]][,400:500] / (1- game_8[[2]][,400:500])))

mean(log(game_64[[2]][,400:500] / (1- game_64[[2]][,400:500])))
sd(log(game_64[[2]][,400:500] / (1- game_64[[2]][,400:500])))
