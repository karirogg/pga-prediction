data {
  int<lower=0> N;
  vector[N] y;
}

parameters {
  real mu;
  real beta;
}

model {
  -y ~ gumbel(-mu, exp(beta));
}

