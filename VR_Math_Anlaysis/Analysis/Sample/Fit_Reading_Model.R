library(tidyverse)
library(rstan)
library(shinystan)
library(loo)
library(bayesplot)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Load data set
# data = read_csv("master_ALL_METRICS.csv")
data = read_csv("Raw_data//master_concat_data.csv")
#Remove extraneous data

reading_data = data[,c('age', "I_30250", "I_30400", "I_30550", "I_40250", "I_40400", "I_40550", "I_50250", "I_50400", "I_50550", 'interception', 'Ckat_Tracing',
                       'Ckat_aiming', 'Ckat_Tracking', 'Open', 'Closed', 'Attainment_Reading')]

reading_data = reading_data[complete.cases(reading_data),]
write.csv(reading_data, "Raw_data//reading_data.csv")
#Drop missing values


#-----------------------------------------------------------#
#Fit the full reading model (All predictors)
y = reading_data$Attainment_Reading
x = model.matrix(~ age + interception + Ckat_Tracing + Ckat_aiming + Ckat_Tracking + Open + Closed, data = reading_data)

stan_data_reading = list(y = y, X = x, N = length(y), J = max(y), Q = dim(x)[2])

fit1 <- stan(
  file = "Ordered_Probit.stan",  # Stan program
  data = stan_data_reading,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 6000,            # total number of iterations per chain
  cores = 4,            # number of cores (using 2 just for the vignette)
  control = list(adapt_delta = 0.999),
  pars = c("theta"),
  include = FALSE
)

#Flush sims to file
readings_samples = extract(fit1, pars = c("theta", "y_rep", "log_lik", "cuts_raw"), include = FALSE)
write.csv(readings_samples, "MCMC_samples//readings_samples.csv")

