data {
  int<lower=0> N; // The number of data points
  int<lower=0> Q; // The number of players;
  
  //Data 
  vector[N] y;
  int<lower=1, upper=Q> g[N]; // Player playing given tournament
}

transformed data {
  vector[N] negative_y = -y;
}

parameters {
  vector[Q] mu;
  vector<lower=0>[Q] beta;
  
  real gamma_mu;
  real<lower=0> tau_mu;
  
  real gamma_beta;
  real<lower=0> tau_beta;
}

model {
  gamma_mu ~ normal(0, 5);
  tau_mu ~ scaled_inv_chi_square(5, 5);
  
  gamma_beta ~ normal(3,5);
  tau_beta ~ scaled_inv_chi_square(5,5);
  
  mu ~ normal(gamma_mu, tau_mu);
  beta ~ normal(gamma_beta, tau_beta);
  
  negative_y[1:N] ~ gumbel(-mu[g[1:N]], beta[g[1:N]]);
}

