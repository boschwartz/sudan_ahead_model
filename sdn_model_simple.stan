data {
  int<lower=0> TT; // Number of time steps
  int<lower=0> K; // Number of independent variables in regression
  int<lower=0> S; // Number of series
  matrix<lower=-1> [TT,S] y;
  matrix[TT,S] x1;
  matrix[TT,S] x2;
  matrix[TT,S] x3;
  matrix[TT,S] x4;
  int<lower=0> N_missing; // Number of missing
  array[N_missing, 2] int Jmi; // where are the missings
}
parameters {
  vector[K] beta;
  real<lower=0> tau;
  real gamma;
  
  real khi;
  
  real<lower=-1, upper=1> theta;
  vector[S] phi_raw;
  
  vector<lower=1, upper=1300000>[S] initial_values;

  vector<lower=1, upper=3000000>[N_missing] y_mis;
  
  
}
transformed parameters {
  vector<lower=0>[S] phi;
  for(s in 1:S){
    phi[s] = exp(gamma + tau*phi_raw[s]);
  }

  matrix<lower=1>[TT,S] Yl;
  Yl = y;
  
  for(n in 1:N_missing){
    Yl[Jmi[n,1], Jmi[n,2]] = y_mis[n];
  }
  
  matrix<lower=0.01>[TT,S] mu_log;
  matrix[TT,S] err;
  
  for(s in 1:S){
    mu_log[1, s] = initial_values[s];
    err[1, s] = 0;
  }

   for(t in 2:TT){
      for(s in 1:S){
        if(t==2){
          mu_log[t, s] = exp(beta[1]*x1[t,s] + beta[2]*x2[t,s] + beta[3]*x3[t,s] + beta[4]*x4[t,s] + log(Yl[t-1,s]) + theta*err[t-1, s]);
          err[t, s] = log(Yl[t, s]) - log(mu_log[t, s]);
        } else{
          mu_log[t, s] = exp(beta[1]*x1[t,s] + beta[2]*x2[t,s] + beta[3]*x3[t,s] + beta[4]*x4[t,s] + log(Yl[t-1,s]) + khi*(log(Yl[t-1,s]) - log(Yl[t-2,s])) + theta*err[t-1, s]);
          err[t, s] = log(Yl[t, s]) - log(mu_log[t, s]);
        }
    }
    
  }
  

}


// 
model {

  beta ~ normal(0, 0.5);


  phi_raw ~ std_normal();


  gamma ~ normal(5,4);
  tau ~ exponential(0.5);
  
  theta ~ normal(0,0.5);
  khi ~ normal(0, 0.1);
  
  for(t in 1:TT){
    for(s in 1:S){
      Yl[t,s] ~ gamma(mu_log[t,s]/phi[s], 1/phi[s]);
    }
  }
  
}
