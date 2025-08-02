# _targets.R file


#loading target verse
library(targets)
library(tarchetypes)
library(stantargets)

# other packages
library(tidyverse)
library(quarto)

# set up the Stan model code
# need to rewrite for negative binomial ###################################

lines <-"data {
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
"
writeLines(lines, "reg.stan")

# setting up the pipeline
tar_source() # loads any functions from /R/functions.R
# such as gen_stan_code, gen_assumptions, gen_scenarios, simulate and report

tar_option_set(packages = c("readr", "dplyr", "ggplot2"))

list(
  tar_map(
    values <- gen_scenarios(), # this creates a list of parameters for sim_dat
    tar_stan_mcmc_rep_draws(
      model_data,
      "reg.stan",
      data = sim_dat(
        rr = rr,
        ss_treatment = ss_treatment,
        mu_placebo = 3,
        dispersion_placebo = 2,
        randomization_ratio = 1), # Runs once per rep.
      batches = 4, # Number of branch targets.
      reps = 2, # Number of model reps per branch target.
      # batches and reps are per scenario. They should be large.
      variables = "beta_trt",
      memory = "persistent", # this seems necessary to stan to work with tar_map
      stdout = R.utils::nullfile(),
      stderr = R.utils::nullfile(),
      transform = gonogo
    )
  )
)
