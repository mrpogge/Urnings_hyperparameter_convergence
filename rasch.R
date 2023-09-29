true_outcome = function(pi_i, pi_j){
  p_ij = (pi_i * (1-pi_j)) / ((pi_i * (1-pi_j)) + (pi_j * (1-pi_i)))
  return(p_ij)
}

expected_outcome_u1 = function(R_i, R_j, n_i, n_j, X_ij){
  if(R_i/n_i == R_j/n_j){
    return(0.5)
  }
  
  if(R_i/n_i != R_j/n_j){
    num = R_i/n_i* (1-R_j/n_j)
    denom = R_i/n_i * (1-R_j/n_j) + R_j/n_j  * (1-R_i/n_i)
    p_hat = num / denom
    return(p_hat)
  }
}

simulated_outcome = function(expected_outcome){
  return(rbinom(1,1,expected_outcome))
}

expected_outcome_u2 = function(R_i, R_j, n_i, n_j, X_ij){
  R_i_hat = R_i + X_ij
  R_j_hat = R_j + (1 - X_ij)
  
  num = (R_i_hat/(n_i + 1) * (1-R_j_hat/(n_j + 1)))
  denom = (R_i_hat/(n_i + 1) * (1-R_j_hat/(n_j + 1))) + (R_j_hat/(n_j + 1) * (1-R_i_hat/(n_i + 1)))
  p_hat = num / denom
  return(p_hat)
}

metro_correct_u1 = function(R_i, R_j, n_i, n_j, R_i_prop, R_j_prop){
  old_score = R_i * (n_i - R_j) + (n_j - R_i) * R_j
  new_score = R_i_prop * (n_i - R_j_prop) + (n_j - R_i_prop) * R_j_prop
  
  return(old_score/new_score)
}

metro_correct_u2 = function(R_i, R_j, n_i, n_j, R_i_prop, R_j_prop){
  return(1)
}

updating_u1 = function(R_i, R_j, n_i, n_j, X_ij, X_hat){
  R_i_prop = R_i + X_ij - X_hat
  R_j_prop = R_j + (1-X_ij) - (1-X_hat)
  
  if(R_i_prop > n_i) R_i_prop = n_i
  if(R_j_prop > n_j) R_j_prop = n_j
  if(R_i_prop < 0) R_i_prop = 0
  if(R_j_prop < 0) R_j_prop = 0
  
  return(c(R_i_prop, R_j_prop))
}

updating_u2 = function(R_i, R_j, n_i, n_j, X_ij, X_hat){
  R_i_prop = R_i + X_ij - X_hat
  R_j_prop = R_j + (1-X_ij) - (1-X_hat)
  
  return(c(R_i_prop, R_j_prop))
}