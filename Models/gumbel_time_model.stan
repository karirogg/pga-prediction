data {
  int<lower=0> N; // The number of data points
  int<lower=0> Q; // The number of players;
  
  //Data 
  vector[N] y;
  
  // Covariates
  vector[N] t; // Time measured on [-1;1]
  int<lower=1, upper=Q> g[N]; // Player playing given tournament
}

transformed data {
  vector[N] negative_y = -y;
  
  vector[N] t_squared;
  vector[N] t_cubed;
  for(i in 1:N) {
    t_squared[i] = t[i]*t[i];
    t_cubed[i] = t_squared[i]*t[i];
  }
}

parameters {
  vector[Q] log_beta;
  
  matrix[Q, 4] alpha;
  
  vector[4] mu_alpha;
  vector<lower=0>[4] sigma2_alpha;

  real mu_beta;
  real<lower=0> sigma2_beta;
}

model {
  // Hyperpriors
  mu_beta ~ normal(1,5);
  sigma2_beta ~ scaled_inv_chi_square(1,5);
  
  mu_alpha ~ normal(0,5);
  sigma2_alpha ~ scaled_inv_chi_square(1,5);
  
  for(i in 1:Q) {
    alpha[i,] ~ normal(mu_alpha, sqrt(sigma2_alpha));
  }
  
  log_beta ~ normal(mu_beta, sqrt(sigma2_beta));
  
  for(n in 1:N) {
    negative_y[n] ~ gumbel(-(alpha[g[n],1] + alpha[g[n],2]*t[n] +  
      alpha[g[n],3]*t_squared[n] +  alpha[g[n],4]*t_cubed[n]), 
        exp(log_beta[g[n]]));
  }
}

