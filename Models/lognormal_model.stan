data {
  int<lower=0> N; // The number of data points
  int<lower=0> Q; // The number of players;
  
  //Data 
  vector[N] y;
  int<lower=1, upper=Q> g[N]; // Player playing given tournament
}

//transformed data {
//  vector[N] translated_y = -y+5;
//}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[Q] translated_mu;
  vector<lower=0>[Q] sigma;
  
  real gamma_mu;
  real<lower=0> tau_mu;
  
  real gamma_sigma;
  real<lower=0> tau_sigma;
}

model {
  gamma_mu ~ normal(5, 5);
  tau_mu ~ scaled_inv_chi_square(5, 5);
  
  gamma_sigma ~ normal(3,5);
  tau_sigma ~ scaled_inv_chi_square(5,5);
  
  translated_mu ~ normal(gamma_mu, tau_mu);
  sigma ~ normal(gamma_sigma, tau_sigma);
  
  y[1:N] ~ lognormal(translated_mu[g[1:N]], sigma[g[1:N]]);
}