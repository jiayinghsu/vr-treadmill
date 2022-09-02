library(tidyverse)
library(rstan)
library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Load data set
data = read_csv("Raw_data//master_concat_data.csv")

#Remove extraneous data
math_data = data[,c('age', 'interception', 'Ckat_Tracing',
                    'Ckat_aiming', 'Ckat_Tracking', 'Open', 'Closed', 'Attainment_Maths')]


#Drop missing values
math_data = math_data[complete.cases(math_data),]

y = math_data[,c("interception", "Ckat_Tracing", "Ckat_aiming", "Ckat_Tracking", "Open", "Closed")]
x = model.matrix(~ age, data = math_data)

stan_data = list(y = as.matrix(y), x = x, N = dim(y)[1], K = dim(y)[2], J = 2, corr_prior = 4)

fit1 <- stan(
  file = "SUR.stan",  # Stan program
  data = stan_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1500,          # number of warmup iterations per chain
  iter = 4000,            # total number of iterations per chain
  cores = 4,            # number of cores (using 2 just for the vignette)
  control = list(adapt_delta = 0.8)
)

launch_shinystan(fit1)


sims = extract(fit1)

sigma = sims$sigma ##Standard deviation parameters
