# R/functions.R

# notes: 
# scale is relative risk (treatment rate / comparator rate)

# create a file with the stan code



gen_assumptions <- function(){
  # version for negative binomial
  list(
    mu_placebo = 3,
    dispersion_placebo = 2,
    prob_exceed_tv = 0.30,
    prob_exceed_lrv = 0.80,
    tv = .6,
    lrv = 1,
    randomization_ratio = 1
  )
}

# gen_assumptions()

gen_scenarios <- function() {
  data1 <- expand.grid(
    rr = seq(from = 1, to = .3, by = -.2),
    ss_treatment = seq(from = 20, to = 220, by = 100)
  )
  data2 <- gen_assumptions()
  scenarios <- data.frame(data1, data2)
  scenarios
}

# gen_scenarios()
# x <- gen_scenarios()
# is.list(x)
# tail(x)

# calculate_exceedance_probability <- function(draws, threshold) {
#   mean(draws > threshold)
# }


# complicate this example later.  Add exposure time, drop out, etc.
# the output is a list that is ready for running in the Stan code
sim_dat <- function(
    mu_placebo = 3,
    dispersion_placebo = 2,
    randomization_ratio = 1,
    rr = 1, 
    ss_treatment = 100){
  ss_placebo <- ceiling(ss_treatment / randomization_ratio)
  N <- ss_placebo + ss_treatment
  mu_treatment <- mu_placebo * rr
  event_placebo <- rnbinom(ss_placebo, size = dispersion_placebo, mu = mu_placebo)
  event_treatment <- rnbinom(ss_treatment, size = dispersion_placebo, mu = mu_treatment)
  list(
    N = N,
    y = c(event_placebo, event_treatment),
    trt = c(rep(0, ss_placebo), rep(1, ss_treatment)),
    .join_data = list(
      rr = rr, 
      N = N)
  )
}

# sim_dat(10, 2, 1, .5, 5)
# test_dat <- sim_dat(10, 2, 1, .5, 5)
# testing model
# library(cmdstanr)
# library(posterior)
# # mod <- cmdstan_model("reg.stan")
# fit <- mod$sample(
#   data = test_dat,
#   chains = 4,   # Number of MCMC chains
#   parallel_chains = 4, # Run chains in parallel
#   iter_warmup = 1000, # Warmup iterations
#   iter_sampling = 1000, # Sampling iterations
#   seed = 1234 # Set a seed for reproducibility
# )
# fit

# In this definition, if a probability of having a lower mean (exp(beta_trt)) rate than the TV
# does not meet the threshold (prob_tv), we declare a "no go".
# a pause is a unexpectedly uncertain result, with large probabilities above 1 
#  (suggesting no effect) and large probabilities below TV (suggesting target effect is possible)
gonogo <- function(data, draws) {
  # data is a data frame with one row, containing the parameters
  # add the assumptions data from gen_assumptions
  assumptions <- gen_assumptions() 
  draws <- select(draws, beta_trt)
  prob_lrv <- mean(exp(draws$beta_trt) < assumptions$lrv[1])
  prob_tv <- mean(exp(draws$beta_trt) < assumptions$tv[1])
  if (prob_lrv > assumptions$prob_exceed_lrv[1] & prob_tv > assumptions$prob_exceed_tv[1]) {decision = "go"} else
    if (prob_lrv <= assumptions$prob_exceed_lrv[1] & prob_tv > assumptions$prob_exceed_tv[1]) {decision = "pause"} else
      decision = "no go"
  data.frame(
    mu_placebo = assumptions$mu_placebo,
    dispersion_placebo = assumptions$dispersion_placebo,
    prob_exceed_tv = assumptions$prob_exceed_tv,
    prob_exceed_lrv = assumptions$prob_exceed_lrv,
    tv = assumptions$tv,
    lrv = assumptions$lrv,
    randomization_ratio = assumptions$randomization_ratio,
    prob_lrv = prob_lrv,
    prob_tv = prob_tv,
    decision = decision,
    rr = data$.join_data$rr,
    N = data$.join_data$N)
  
}


oc_rates <- function(data) {
  data %>%
  group_by(N, rr) %>%
  summarize(
    n = n(),
    prop_go = mean(decision == "go"),
    prop_no_go = mean(decision == "no go"),
    prop_pause = mean(decision == "pause")
  )
}
# testing, works
# draws <- fit$draws(format = "draws_df")
# gonogo(test_dat, draws)

# stub change_param_beta_poisson <- function(mu = 3, dispersion = 2){
  # add change to a beta-poisson parameterization
# }

# stub change_param_neg_binomial

# testing
# sim_dat(
#   mu_placebo = scenarios$mu_placebo,
#   dispersion_placebo = scenarios$dispersion_placebo,
#   randomization_ratio = scenarios$randomization_ratio,
#   rr = scenarios$rr[1],
#   ss_treatment = scenarios$ss_treatment[1])



# 
# report <-tar_quarto(
#        name = test_report,
#        quiet = FALSE,
#        path = "/home/mike/R_projects/target_sim/test_report.qmd"
# )
