data {
  int<lower=1> N; // Number of observations
  int<lower=1> K; // Number of autoregressive terms
  // Model Input
  vector[N] y; // Shots gained for given tournament
  matrix[N, K] x;// Shots gained in prevoius K tournaments
}

parameters {
  simplex[K] w;
  real<lower=0> sigma;
}

model {
  y ~ normal(x * w, sigma);
}

