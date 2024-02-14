#include <R.h>
#include <math.h>
#include <Rmath.h>

void urnings_simpleX_HT(int*adaptive,int*paired,int*u,int*v,double*theta,double*delta,int*N,int*K,int*Rep,int*k1,int*k2,double*P,double*cumsum,int*Score,int*n_scores,int*n_options,int*Upd,int*queue,int*LL,int*LLsum,double*MSE,double*mse_baseline,int*HT){
  double L=0;
  double p=0;
  double Mp=0;
  double Mp1=0;
  int oldU=0;
  int oldV=0;
  int newV=0;
  int newU=0;
  int j=0;
  int x=0;
  int y=0;
  int success=0;
  int Q=0;
  int s=0;
  int jj=0;
  double Correct=0;
  double Incorrect=0;
  double dif=0;
  GetRNGstate();
  for(int rep=0;rep<Rep[0];rep++){/* loop over iterations*/
    if(MSE[0]>mse_baseline[0]){/* only if MSE is still about the baseline, do the iteration*/
      HT[0]=rep;/*the current iteration is saved as potential hitting time, if after this iteration MSE goes below the baseline then this would be the recorded HT.*/
      for(int i=0;i<N[0];i++){/* loop over persons*/
    if(adaptive[0]==0){/* sample an item with equal probabilities*/
    p=runif(0,1.00*K[0]);
      j=0;
      for(int s=1;s<K[0];s++){
        if(p>s){
          j=j+1;
        }
      }
    }
    if(adaptive[0]==1){      /*adaptive item selection, kernels are pre-specified in matrix P*/
    Mp=0;		
      for(int s=0;s<K[0];s++){
        Mp=Mp+P[u[i]+v[s]*(k1[0]+1)];
        cumsum[s+1]=cumsum[s]+P[u[i]+v[s]*(k1[0]+1)];
      }
      p=runif(0,Mp);
      j=0;
      for(int s=1;s<K[0];s++){
        if(p>cumsum[s]){
          j=j+1;
        }
      }
    }
    /* save the current values of the person and the item */
    oldU=u[i];
    oldV=v[j];
    /* generate the observed accuracy*/
    L=1/(1+exp((delta[j]-theta[i]))); /* true probability correct, W is discrimination parameter, 1 is default value*/
    p=runif(0,1.00);
    x=1*(L>p);
    /* add the balls based on the observed response to the urns*/                      
    u[i]=u[i]+x;                             
    v[j]=v[j]+(1-x);	
    /* compute the probability of X=1 given the urn configurations*/
    Correct=u[i]*(k2[0]-v[j]);
    Incorrect=v[j]*(k1[0]-u[i]);   
    L=Correct/(Correct+Incorrect);      
    /* generate the simulated response */
    p=runif(0,1.00);
    y=1*(L>p);
    /* remove the balls based on the simulated response from the urns: These would be the proposed values*/
    u[i]=u[i]-y;
    v[j]=v[j]-(1-y);
    
    if(adaptive[0]==1){/*Metropolis step for when item selection is adaptive*/
    newU=u[i];
      newV=v[j];
      u[i]=oldU;
      v[j]=oldV;
      if(newU!=oldU){
        /*Compute the normalising constant for the selection probability given the proposed values*/
        Mp1=P[newU+newV*(k1[0]+1)];
        for(int s=0;s<K[0];s++){
          if(s!=j){
            Mp1=Mp1+P[newU+v[s]*(k1[0]+1)];
          }
        }
        /* compute the MH acceptance probability*/
        L=P[newU+newV*(k1[0]+1)]/Mp1*Mp/P[oldU+oldV*(k1[0]+1)];
        /* Generate a random uniform (0,1) number to decide whether to accept the proposal*/
        p=runif(0,1);
        if(p<L){
          u[i]=newU;
          v[j]=newV;
        }
      }
    }
    if(paired[0]==1){/*paired update for when it is on*/
        /*set item j to the old value*/
        newV=v[j];
      v[j]=oldV;
      /*If the proposed value is different from the old one, check if a paired update can be performed*/ 
      if(newV!=oldV){
        /*Find which of the possible update values (saved in vector Score) has been proposed*/
        for(int t=0;t<n_scores[0];t++){
          if((newV-oldV)==Score[t]){
            s=t;
          }
        }
        /*for every possible value of an update, count how many items are in the queue and can be updated*/
        for(int t=0;t<n_scores[0];t++){
          LLsum[t]=0;
          for(int ii=0;ii<K[0];ii++){
            LL[ii+t*K[0]]=1*(queue[ii]==Score[t])*(v[ii]+Score[t]<k2[0]+1)*(v[ii]+Score[t]>(-1))*(ii!=j);
            LLsum[t]=LLsum[t]+LL[ii+t*K[0]];
          }
        }
        success=0;  
        for(int d=n_options[s];d<n_options[s+1];d++){/*Loop over possible options of a paired update for the particular proposed update (in the rasch model case there is only one possible option for a paired update (-1 if item j gets an update +1, and +1 ifitem j gets an update of -1))*/
        if(success==0){/*this is done only if the paired update has not been found yet*/
        /*check whether for the particular possible update there are enough items in the queue*/
        Q=1;
          for(int t=0;t<n_scores[0];t++){
            Q=Q*(1-(LLsum[t]<Upd[t+d*n_scores[0]]));
          }
          if(Q>0){/*If there are enough items*/
        success=1;/*set succes to 1, such that we will not look further*/
        for(int t=0;t<n_scores[0];t++){/*loop over possible update values*/
        if(Upd[t+d*n_scores[0]]>0){/*if this update value is part of the paired update option*/
        for(int h=1;h<(Upd[t+d*n_scores[0]]+1);h++){
          /*update the values in the cumsum vector which is used for sampling from a multinomial distribution*/
          for(int k=0;k<K[0];k++){
            cumsum[k+1]=cumsum[k]+1.00*LL[k+t*K[0]];
          }
          /*select an item randomly among those that are in the queue (where LL is not 0)*/
          p=runif(0,1.00*LLsum[t]);
          jj=0;
          for(int k=1;k<K[0];k++){
            if(p>cumsum[k]){
              jj=jj+1;
            }
          }	
          v[jj]=v[jj]+Score[t];/*update the selected item*/
          queue[jj]=0;/*remove the selected item from the queue*/
          LLsum[t]=LLsum[t]-LL[jj+t*K[0]];/*update the normalising constant for the selection probabilities*/	
          LL[jj+t*K[0]]=0;/*set the selection probability for the item to 0 such that it will not be selected again*/
        }
        }
        }
        v[j]=v[j]+Score[s];	/*update item j*/
          }
        }
        }
        if(success==0){/*If no option for the paired update was found, put item j in the queue*/
          queue[j]=Score[s];
        }  
      }
    }
      }
      /*compute the average MSE across persons*/
      MSE[0]=0;
      for(int i=0;i<N[0];i++){
        dif=1.00*u[i]/k1[0]-1.00/(1+exp(-theta[i]));/*difference between the rating and the true value on the [0,1] scale*/
        MSE[0]=MSE[0]+dif*dif/N[0];
      }
    }
  }
  
  PutRNGstate();
}  
