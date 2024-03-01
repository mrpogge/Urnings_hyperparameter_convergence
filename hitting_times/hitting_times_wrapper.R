source("item_selection.R")

hitting_times = function(student_starting,
                         item_starting,
                         Theta,
                         Delta,
                         n_students,
                         n_items,
                         n_games,
                         student_urn_size,
                         item_urn_size,
                         adaptive = 0,
                         m_adapt = 0.5,
                         sd_adapt = 0.1,
                         paired = 1,
                         mse_baseline = NULL,
                         returns = "simple",
                         OS = "MAC"){
  ################################################################################  
  #Parameters
  ################################################################################
  # FUNCTION ARGUMENTS
  # student_starting : (vector<int>) starting values for the student 
  #  item_starting : (vector<int>) starting values for the items 
  #  Theta : matrix[n_students, n_games]<double> matrix of true values
  #  Delta : matrix[n_items, n_games]<double> matrix of true values
  #  n_students: <int> number of students in the system
  #  n_items: <int> number of items in the system
  #  n_games: <int> number of items solved by each student
  #  student_urn_size: <int> the size of the student urns
  #  item_urn_size: <int> the size of the item urns
  #  weight = 1: <int> size or the stake of an update
  #  adaptive = 0: <int>[0,1] indicator 1 = adaptive, 0 = uniform item selection
  #  m_adapt = 0.5: <double> mu parameter of the Normal Kernel Method (probability)
  #  sd_adapt = 0.1: <double> sigma parameter of the Normal Kernel Method (probability)
  #  paired = 1: <int>[0,1] indicator 1 = paired update, 0 = no paired update
  #  returns = "simple": <char>["simple", "total"], "simple" = return student urnings,
  #                                                 "total" = return the whole list
  #  OS = "MAC" : <char>["MAC", "WINDOWS", "LINUX"]: selects the right file for the OS
  
  #RETURN:
  #if simple: matrix[n_students, n_games]<integer>
  #if total: list
  
  ################################################################################  
  #loading the right compiled version of the C file 
  ################################################################################
  switch (OS,
          "MAC" = dyn.load("hitting_times.so"),
          "WINDOWS" = dyn.load("hitting_times.dll"),
          "LINUX" = dyn.load("hitting_times.o")
  )
  
  ################################################################################  
  #create the normal kernel values for the possible combinations
  ################################################################################
  Prob2=init_adaptivity_matrix(student_urn_size, item_urn_size, m_adapt, sd_adapt)
  
  ################################################################################
  #create helpers for the paired update
  ################################################################################
  Upd=t(matrix(c(0,1,1,0),nrow=2))
  n_options=c(0,1,2)
  Score=c(-1,1)
  n_scores=2
  queue = rep(0,n_items)
  LL=rep(0,n_scores*n_items)
  LLsum=rep(0,n_scores)
  
  HT = 0
  MSE = (student_starting/student_urn_size - Theta)^2
  ################################################################################
  #call the C function to perform the simulation
  ################################################################################
  tmp<-.C("urnings_simpleX_HT",
          as.integer(adaptive), #indicator of the use of adaptive item selection
          as.integer(paired), #indicator for the inclusion of paired update
          as.integer(student_starting), #student starting values
          as.integer(item_starting), #item starting values
          as.double(Theta), #nplayers x niteration matrix of student true values
          as.double(Delta), #n_items x niteration matrix of item true values
          as.integer(n_students), #number of students
          as.integer(n_items), #number of items
          as.integer(n_games), #number of games
          as.integer(student_urn_size), # urn size for students
          as.integer(item_urn_size), #urn sizes for items
          as.double(Prob2), #normal kernel matrix
          as.double(rep(0,n_items+1)), #no idea, but probably a helper for calculating the normalising constant
          as.integer(Score), #possible updates
          as.integer(n_scores), #number of possible updates other than 0
          as.integer(n_options), #number of possible updates as a vector? 
          as.integer(Upd), #helper for the paired update I guess
          as.integer(queue), #queue for the paired update
          as.integer(LL), #helpers for the paired update again
          as.integer(LLsum),
          as.double(MSE),
          as.double(mse_baseline),
          as.integer(HT))#and again
  
  ################################################################################
  #returning the results
  ################################################################################
  if(returns == "simple"){
    U=tmp[[23]]
    return(U)
  } else {
    return(tmp)
  }
}


hitting_times_CS = function(student_starting,
                         item_starting,
                         player_label,
                         Theta,
                         Delta,
                         n_students,
                         n_items,
                         n_games,
                         student_urn_size,
                         item_urn_size,
                         adaptive = 0,
                         m_adapt = 0.5,
                         sd_adapt = 0.1,
                         paired = 1,
                         mse_baseline = NULL,
                         returns = "simple",
                         OS = "MAC"){
  ################################################################################  
  #Parameters
  ################################################################################
  # FUNCTION ARGUMENTS
  # student_starting : (vector<int>) starting values for the student 
  #  item_starting : (vector<int>) starting values for the items 
  #  Theta : matrix[n_students, n_games]<double> matrix of true values
  #  Delta : matrix[n_items, n_games]<double> matrix of true values
  #  n_students: <int> number of students in the system
  #  n_items: <int> number of items in the system
  #  n_games: <int> number of items solved by each student
  #  student_urn_size: <int> the size of the student urns
  #  item_urn_size: <int> the size of the item urns
  #  weight = 1: <int> size or the stake of an update
  #  adaptive = 0: <int>[0,1] indicator 1 = adaptive, 0 = uniform item selection
  #  m_adapt = 0.5: <double> mu parameter of the Normal Kernel Method (probability)
  #  sd_adapt = 0.1: <double> sigma parameter of the Normal Kernel Method (probability)
  #  paired = 1: <int>[0,1] indicator 1 = paired update, 0 = no paired update
  #  returns = "simple": <char>["simple", "total"], "simple" = return student urnings,
  #                                                 "total" = return the whole list
  #  OS = "MAC" : <char>["MAC", "WINDOWS", "LINUX"]: selects the right file for the OS
  
  #RETURN:
  #if simple: matrix[n_students, n_games]<integer>
  #if total: list
  
  ################################################################################  
  #loading the right compiled version of the C file 
  ################################################################################
  switch (OS,
          "MAC" = dyn.load("hitting_timesCS.so"),
          "WINDOWS" = dyn.load("hitting_timesCS.dll"),
          "LINUX" = dyn.load("hitting_timesCS.o")
  )
  
  ################################################################################  
  #create the normal kernel values for the possible combinations
  ################################################################################
  Prob2=init_adaptivity_matrix(student_urn_size, item_urn_size, m_adapt, sd_adapt)
  
  ################################################################################
  #create helpers for the paired update
  ################################################################################
  Upd=t(matrix(c(0,1,1,0),nrow=2))
  n_options=c(0,1,2)
  Score=c(-1,1)
  n_scores=2
  queue = rep(0,n_items)
  LL=rep(0,n_scores*n_items)
  LLsum=rep(0,n_scores)
  
  HT = 0
  MSE = (student_starting/student_urn_size - Theta)^2
  ################################################################################
  #call the C function to perform the simulation
  ################################################################################
  tmp<-.C("urnings_simpleCS_HT",
          as.integer(adaptive), #indicator of the use of adaptive item selection
          as.integer(paired), #indicator for the inclusion of paired update
          as.integer(student_starting), #student starting values
          as.integer(item_starting), #item starting values
          as.integer(player_label),
          as.double(Theta), #nplayers x niteration matrix of student true values
          as.double(Delta), #n_items x niteration matrix of item true values
          as.integer(n_students), #number of students
          as.integer(n_items), #number of items
          as.integer(n_games), #number of games
          as.integer(student_urn_size), # urn size for students
          as.integer(item_urn_size), #urn sizes for items
          as.double(Prob2), #normal kernel matrix
          as.double(rep(0,n_items+1)), #no idea, but probably a helper for calculating the normalising constant
          as.integer(Score), #possible updates
          as.integer(n_scores), #number of possible updates other than 0
          as.integer(n_options), #number of possible updates as a vector? 
          as.integer(Upd), #helper for the paired update I guess
          as.integer(queue), #queue for the paired update
          as.integer(LL), #helpers for the paired update again
          as.integer(LLsum),
          as.double(MSE),
          as.double(mse_baseline),
          as.integer(HT))#and again
  
  ################################################################################
  #returning the results
  ################################################################################
  if(returns == "simple"){
    U=tmp[[23]]
    return(U)
  } else {
    return(tmp)
  }
}