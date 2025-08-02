data {
  int<lower=0> N;          // Number of observations
  array[N] int<lower=0> y; // Response variable (count data)
  array[N] int<lower=0,upper=1> trt; // Treatment indicator (0=placebo, 1=drug)
}
parameters {
  real alpha;          // Intercept
  real beta_trt;      // Coefficient for treatment effect
  real<lower=0> phi;   // Negative binomial dispersion parameter
}

model {
  // Priors
  alpha ~ normal(0, 5);          // Weakly informative prior for intercept
  beta_trt ~ normal(0, 2);       // Weakly informative prior for treatment effect
  phi ~ cauchy(0, 3);          // Weakly informative prior for dispersion parameter

  // Likelihood
  for (n in 1:N) {
    y[n] ~ neg_binomial_2_log(alpha + beta_trt * trt[n], phi); 
  }
}

