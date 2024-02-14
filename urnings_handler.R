source("urnings_function.R")
source("rasch.R")
source("item_selection.R")
source("paired_update.R")

urnings_game = function(player_urnings, 
                        item_urnings,
                        player_urn_size,
                        item_urn_size,
                        player_true_value,
                        item_true_value,
                        n_games,
                        alg_type,
                        adaptivity,
                        is_paired_update,
                        mu = 0.5,
                        sigma = 2,
                        change = FALSE,
                        coverage = FALSE,
                        save_item_urnings = FALSE){
  #selecting alg_type function
  algo = switch(
    alg_type,
    "Urnings1" = expected_outcome_u1,
    "Urnings2" = expected_outcome_u2
  )
  
  update_rule = switch(
    alg_type,
    "Urnings1" = updating_u1,
    "Urnings2" = updating_u2
  )
  
  
  metropolis_corrector = switch(
    alg_type,
    "Urnings1" = metro_correct_u1,
    "Urnings2" = metro_correct_u2
  )
  
  #implement adaptivity  
  adaptivity_algo = switch(
    adaptivity,
    "n_adaptive" = uniform,
    "adaptive" = normal
  )
  
  adaptivity_corrector = switch(
    adaptivity,
    "n_adaptive" = n_adapt_corrector,
    "adaptive" = adapt_corrector
  )
  
  paired_update_rule = ifelse(is_paired_update, paired_update, no_paired_update)
  
  selected_functions = list(r_pl = player_urnings, 
                            r_it = item_urnings,
                            n_i = player_urn_size,
                            n_j = item_urn_size,
                            pi_pl = player_true_value,
                            pi_it = item_true_value,
                            n_games = n_games,
                            algorithm = algo, 
                            update_rule = update_rule,
                            adaptivity_algorithm = adaptivity_algo,
                            metropolis_corrector = metropolis_corrector,
                            adaptivity_corrector = adaptivity_corrector,
                            paired_update_fun = paired_update_rule,
                            mu = mu,
                            sigma = sigma,
                            change = change,
                            coverage = coverage,
                            save_item_urnings = save_item_urnings)
  class(selected_functions) = "game_type"
  
  return(selected_functions)
}

play = function(game_type, omit_message = FALSE){
source("urnings_function.R")
  game = urningsSimFactory(r_pl = game_type[["r_pl"]],
                           r_it = game_type[["r_it"]],
                           n_i = game_type[["n_i"]],
                           n_j = game_type[["n_j"]],
                           pi_pl = game_type[["pi_pl"]],
                           pi_it = game_type[["pi_it"]],
                           ngames = game_type[["n_games"]],
                           item_selection = game_type[["adaptivity_algorithm"]], #normal vs uniform
                           expected_outcome = game_type[["algorithm"]],
                           update_rule = game_type[["update_rule"]],
                           metropolis_urnings = game_type[["metropolis_corrector"]],
                           metropolis_adapt = game_type[["adaptivity_corrector"]],
                           paired_update_function = game_type[["paired_update_fun"]],
                           mu = game_type[["mu"]], 
                           sigma = game_type[["sigma"]],
                           change = game_type[["change"]],
                           coverage = game_type[["coverage"]],
                           save_item_urnings = game_type[["save_item_urnings"]],
                           omit_message = omit_message)
  return(game)
}

urnings_analysis = function(data,
                            player_urnings, 
                            item_urnings,
                            player_urn_size,
                            item_urn_size,
                            alg_type,
                            is_paired_update,
                            compare_IRT = NULL){
  #selecting alg_type function
  algo = switch(
    alg_type,
    "Urnings1" = expected_outcome_u1,
    "Urnings2" = expected_outcome_u2
  )
  
  update_rule = switch(
    alg_type,
    "Urnings1" = updating_u1,
    "Urnings2" = updating_u2
  )
  
  
  metropolis_corrector = switch(
    alg_type,
    "Urnings1" = metro_correct_u1,
    "Urnings2" = metro_correct_u2
  )
  
  
  paired_update_rule = ifelse(is_paired_update, paired_update, no_paired_update)
  
  selected_functions = list(data = data,
                            r_pl = player_urnings, 
                            r_it = item_urnings,
                            n_i = player_urn_size,
                            n_j = item_urn_size,
                            algorithm = algo, 
                            update_rule = update_rule,
                            metropolis_corrector = metropolis_corrector,
                            paired_update_fun = paired_update_rule,
                            compare_IRT = compare_IRT)
  class(selected_functions) = "game_type"
  
  return(selected_functions)
}

analyse = function(game_type, omit_message = FALSE, save_iter = 10000, stop = FALSE){
  source("urnings_function.R")
  game = urningsFactory(data = game_type[["data"]],
                        r_pl = game_type[["r_pl"]],
                        r_it = game_type[["r_it"]],
                        n_i = game_type[["n_i"]],
                        n_j = game_type[["n_j"]],
                        expected_outcome = game_type[["algorithm"]],
                        update_rule = game_type[["update_rule"]],
                        metropolis_urnings = game_type[["metropolis_corrector"]],
                        paired_update_function = game_type[["paired_update_fun"]],
                        compare_IRT = game_type[["compare_IRT"]],
                        omit_message = omit_message,
                        save_iter = save_iter,
                        stop = stop)
  return(game)
}