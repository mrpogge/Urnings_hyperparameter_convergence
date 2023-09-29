#random item selection function
uniform = function(nitems = NULL, R_i, r_it, n_i, n_j, mu, sigma){
  return(sample(1:nitems, 1))
}

#normal distribution based item selection helper
normal_method_helper = function(R_i, R_j, n_i, n_j, mu, sigma){
  logit_R_i = log((R_i + 1) / (n_i-R_i+1))
  logit_R_j = log((R_j + 1) / (n_j - R_j + 1))
  prob = exp(-0.5 * (((logit_R_i - logit_R_j) - mu) / sigma)^2)
  return(prob)
  }

# initialising adaptive matrix, so we dont need to recalculate every time
init_adaptivity_matrix = function(n_i, n_j, mu, sigma){
  adaptive_matrix_binned = matrix(0, n_i + 1, n_j + 1)
  for(p in 0:(n_i)){
    for(i in 0:(n_j)){
      adaptive_matrix_binned[p+1,i+1] = normal_method_helper(p,i,n_i, n_j, mu, sigma)
    }
  }
  return(adaptive_matrix_binned)
}

#initialising a counter for for each item score
init_item_bins = function(r_it, n_j){
  item_bins = numeric(length = n_j + 1)
  for(i in 1:length(r_it)){
    place = r_it[i]
    item_bins[place+1] = item_bins[place+1] + 1
  }
  return(item_bins)
}

#item selection
normal = function(nitems = NULL, R_i, r_it, n_i, n_j, mu, sigma){
  p_select = normal_method_helper(R_i, r_it, n_i, n_j, mu, sigma)
  p_select = p_select / sum(p_select)
  it = sample(1:length(r_it), 1, prob = p_select)
  return(c(it, p_select[it]))
}

#adaptivity correction
adapt_corrector = function(prob_select, n_i, n_j, R_i_prop, R_j_prop, r_it, it, mu, sigma){
  curr_prob = prob_select
  
  prop_prob_un = normal_method_helper(R_i_prop, R_j_prop, n_i, n_j, mu, sigma)
  r_it_prop = r_it
  r_it_prop[it] = R_j_prop
  prop_prob_total = normal_method_helper(R_i_prop, r_it_prop, n_i, n_j, mu, sigma)
  prop_prob = prop_prob_un / sum(prop_prob_total)
  
  return(prop_prob/curr_prob)
}

n_adapt_corrector = function(prob_select, n_i, n_j, R_i_prop, R_j_prop, r_it, it, mu, sigma){
  return(1) 
}


