data {
  int<lower=1> N; // Number of observations
  int<lower=1> K; // Number of autoregressive terms
  int<lower=1> Q; // Number of players
  // Model Input
  vector[N] y; // Shots gained for given tournament
  matrix[N, K] x;// Shots gained in prevoius K tournaments
  int<lower=1, upper=Q> g[N]; // Player playing given tournament
}

parameters {
  // weights
  simplex[K] w;
  // Hyperparameters
  real<lower=0> nu;
  real<lower=0> tau;
  //real<lower=0> gamma;
  // Player specific variance
  vector<lower=0>[Q] sigma2;
}

model {
  // Hyperpriors
  nu ~ exponential(1000);
  tau ~ exponential(1000);
  //gamma ~ exponential(1000);
  // Latent level
  sigma2 ~ scaled_inv_chi_square(nu, tau);
  w ~ dirichlet(rep_vector(1,K));
  // Data level
  y[1:N] ~ normal(x*w, sqrt(sigma2[g[1:N]]));
}

