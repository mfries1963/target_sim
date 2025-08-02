library(targets)
library(stantargets)
library(rstan) 
library(tarchetypes)
library(tibble)
library(tidyverse)

source("R/functions.R")

mapped_models <- tar_map(
  values <- gen_scenarios(), 
  tar_stan_mcmc_rep_draws(model_data, 
                          "reg.stan", 
                          data = sim_dat(
                            rr = rr, 
                            ss_treatment = ss_treatment, 
                            mu_placebo = 3, 
                            dispersion_placebo = 2, 
                            randomization_ratio = 1), 
                          batches = 4, 
                          reps = 2, 
                          variables = "beta_trt", 
                          memory = "persistent", 
                          stdout = R.utils::nullfile(), 
                          stderr = R.utils::nullfile(),
                          transform = gonogo)
  )
# extract simulation results with group variables (in this example, N and rr)
results1 <- tar_combine(model_results, mapped_models$model_data_reg, command = dplyr::bind_rows(!!!.x))
# create a new target that summarizes for each value of N and rr, the proportion of "go", "no go", and "pause" based on the variable decision
calc_oc <- tar_target(
  name = oc,
  command = oc_rates(model_results)
)
# provide a quarto based report of the oc results
# that has a nicely formatted table and a figure with
# the figure as a line plot with RR as the x axis
# and the y axis as the probability of go, no go, pause
# use a green line for go, red for no go, orange for pause
report <- tar_quarto(
  name = oc_report,
  quiet = FALSE,
  path = "/home/mike/R_projects/target_sim/oc_report.qmd"
)
 

list(mapped_models, results1, calc_oc)