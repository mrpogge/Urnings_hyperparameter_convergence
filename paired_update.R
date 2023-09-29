
paired_update = function(R_j, R_j_prop, n_j, it, r_it, queue){
  R_candidate_prev = NULL
  R_candidate = NULL
  
  if(R_j_prop > R_j){
    P = queue==(-1) # P is the probability of selecting the item from the queue
    P[r_it==0] = 0 # items which only have 0 balls can not be selected
    P[it] = 0 # the current item can not be selected
    if(sum(P)==0){ # if no items can be selected from the queue
      queue[it] = 1 # the current item is put in the positive queue
      r_it[it] = R_j_pu = R_j
      
    }
    if(sum(P)!=0){ 
      it_candidate = sample(1:length(r_it),1,prob=P) 
      R_candidate_prev = r_it[it_candidate] 
      r_it[it_candidate]  = R_candidate = r_it[it_candidate]-1 # negative update of the selected item 
      queue[it_candidate] = 0 # selected item is removed from the negative queue
      r_it[it] = R_j_pu  =  R_j_prop # the current item is updated
    }
  }
  if(R_j_prop < R_j){# if the proposed update is positive
    P<- queue==1 
    P[r_it==n_j] = 0
    P[it] = 0
    if(sum(P)==0){
      r_it[it] = R_j_pu = R_j
      queue[it] = -1
    }
    if(sum(P)!=0){
      it_candidate = sample(1:length(r_it),1,prob=P)
      R_candidate_prev = r_it[it_candidate] 
      r_it[it_candidate] = R_candidate = r_it[it_candidate]+1
      queue[it_candidate] = 0
      r_it[it] = R_j_pu  = R_j_prop
    }
  }
  
  return(list(r_it, queue, R_j_pu, R_candidate, R_candidate_prev))
  
}

no_paired_update = function(R_j, R_j_prop, n_j, it, r_it, queue){
  R_candidate_prev = NULL
  R_candidate = NULL
  r_it[it] = R_j_prop
  R_j_pu = R_j_prop
  return(list(r_it, queue, R_j_pu, R_candidate, R_candidate_prev))
}


