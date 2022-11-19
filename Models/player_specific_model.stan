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
  simplex[K] w[Q];
  // Hyperparameters
  real<lower=0> nu;
  real<lower=0> tau;
  vector<lower=0>[K] alpha;
  //real<lower=0> gamma;
  // Player specific variance
  vector<lower=0>[Q] sigma2;
}

model {
  // Hyperpriors
  nu ~ exponential(1000);
  tau ~ exponential(1000);
  alpha ~ exponential(1000);
  // Latent level
  sigma2 ~ scaled_inv_chi_square(nu, tau);
  
  for(k in 1:Q) {
    w[k] ~ dirichlet(alpha);
  }
  // Data level
  for(n in 1:N) {
    y[n] ~ normal(x[n,]*w[g[n]], sqrt(sigma2[g[n]]));
  }
}

