urningsSimFactory = function(r_pl,
                             r_it,
                             n_i,
                             n_j,
                             pi_pl,
                             pi_it,
                             ngames,
                             item_selection,
                             expected_outcome,
                             update_rule,
                             metropolis_urnings,
                             metropolis_adapt,
                             paired_update_function,
                             mu, 
                             sigma,
                             change = FALSE,
                             coverage = FALSE,
                             omit_message = FALSE,
                             save_item_urnings = FALSE)
{
  nplayers = length(r_pl)
  nitems = length(r_it)
  #mse
  outcome = matrix(0, nrow = nplayers, ncol = ngames)
  if(coverage == TRUE){
    coverage_results = matrix(0, nrow = nplayers, ncol = ngames)
  }


  
  #source
  source("item_selection.R")
  source("rasch.R")
  source("paired_update.R")
  require(Hmisc)
  
  #adaptive matrix
  queue = numeric(nitems)

  
  for(ng in 1:ngames){
    if(ng %% 50 == 0 & omit_message == FALSE){
      print(paste0("Est. game: " , ng))
    }
    for(pl in 1:nplayers){
      #selecting player
      R_i = r_pl[pl]
      
      if(change == FALSE){
        pi_i = pi_pl[pl]
      } else {
        pi_i = pi_pl[pl,ng]
      }
      
      
      #randomly selecting item
      item_props = item_selection(nitems = length(r_it), R_i, r_it, n_i, n_j, mu, sigma)
      it = item_props[1]
      prob_select = item_props[2]
      
      R_j = r_it[it]
      pi_j = pi_it[it]
      
      #calculating true outcome
      p_ij = true_outcome(pi_i, pi_j)
      X_ij = simulated_outcome(p_ij)
      
      #calculating expected outcome
      p_hat = expected_outcome(R_i, R_j, n_i, n_j, X_ij)
      X_hat = simulated_outcome(p_hat)

      #update
      proposal = update_rule(R_i, R_j, n_i, n_j, X_ij, X_hat)
      R_i_prop = proposal[1]
      R_j_prop = proposal[2]
      
      #metropolis step 
      if(R_j_prop != R_j){
        adapt_correction = metropolis_adapt(prob_select, n_i, n_j, R_i_prop, R_j_prop, r_it, it, mu, sigma)
        urnings_correction = metropolis_urnings(R_i, R_j, n_i, n_j, R_i_prop, R_j_prop)
        
        alpha = min(1, adapt_correction*urnings_correction)
        
        if(runif(1) < alpha){
          r_pl[pl] = R_i_prop
          
          pu_objects = paired_update_function(R_j, R_j_prop, n_j, it, r_it, queue)
          
          r_it = pu_objects[[1]]
          queue = pu_objects[[2]]
        }
      }
      
      outcome[pl, ng] = r_pl[pl] / n_i 
      if(coverage == TRUE){
        CI = binconf(r_pl[pl], n_i, method = "wilson")[2:3]
        coverage_results[pl, ng] = as.numeric(CI[1] < pi_i & pi_i < CI[2])
      }
      #this is just for the check 
      
    }
  }

  
  if(coverage == FALSE){
    return(outcome)
  } else {
    return(cbind(outcome, coverage_results))
  }
}

urningsFactory = function(data,
                          r_pl,
                          r_it,
                          n_i,
                          n_j,
                          expected_outcome,
                          update_rule,
                          metropolis_urnings,
                          paired_update_function,
                          compare_IRT = NULL,
                          omit_message = FALSE,
                          save_iter = 10000,
                          stop = FALSE)
{
  #ests
  n_players = length(r_pl)
  n_items = length(r_it)
  n_games = nrow(data)
  outcome = new.env()
  
  outcome$est_player = r_pl 
  outcome$est_item = r_it
  outcome$n_games_per_student = numeric(n_players)

  
  print("I am here")
  
  #source
  source("rasch.R")
  source("paired_update.R")
  
  #adaptive matrix
  queue = numeric(n_items)
  
  counter = 1 
  for(ng in 1:n_games){
    if(ng %% save_iter == 0 & omit_message == FALSE){
      print(paste0("Est. game: " , ng))
    } 
      
      #selecting player
      pl = data[ng,1]
      it = data[ng,2]
      R_i = r_pl[pl]
      R_j = r_it[it]
      X_ij = data[ng,3]
      
      #calculating expected outcome
      p_hat = expected_outcome(R_i, R_j, n_i, n_j, X_ij)
      X_hat = simulated_outcome(p_hat)
      
      #update
      proposal = update_rule(R_i, R_j, n_i, n_j, X_ij, X_hat)
      R_i_prop = proposal[1]
      R_j_prop = proposal[2]
      
      #metropolis step 
      if(R_j_prop != R_j){
        urnings_correction = metropolis_urnings(R_i, R_j, n_i, n_j, R_i_prop, R_j_prop)
        
        alpha = min(1, urnings_correction)
        
        if(runif(1) < alpha){
          r_pl[pl] = R_i_prop
          
          pu_objects = paired_update_function(R_j, R_j_prop, n_j, it, r_it, queue)
          
          r_it = pu_objects[[1]]
          queue = pu_objects[[2]]
        }
      }
      
      outcome$est_player = r_pl / n_i
      outcome$est_item = r_it / n_j
      outcome$n_games_per_student[pl] = outcome$n_games_per_student[pl] + 1 
      
      
      if(sd(r_pl / n_i) != 0 & stop == TRUE & ng %% n_players){
        if(cor(exp(compare_IRT[[1]]) / (1+exp(compare_IRT[[1]])), r_pl / n_i) > 0.7){
          break
        }
      }
  }
  return(outcome)
}



