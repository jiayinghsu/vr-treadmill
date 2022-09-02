library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Load data set
# data = read_csv("master_ALL_METRICS.csv")
data = read_csv("/Users/newxjy/Desktop/VR_Math_Anlaysis/Raw_data/Sample/master_concat_data.csv")

#Remove extraneous data

math_data = data[,c('age', "I_30250", "I_30400", "I_30550", "I_40250", "I_40400", "I_40550", "I_50250", "I_50400", "I_50550", 'interception', 'Ckat_Tracing',
                    'Ckat_aiming', 'Ckat_Tracking', 'Open', 'Closed', 'Attainment_Maths')]

#Drop missing values
math_data = math_data[complete.cases(math_data),]
write.csv(math_data, "/Users/newxjy/Desktop/VR_Math_Anlaysis/Raw_data/Sample/maths_data.csv")



#------------------------------------------------------------#
#Fit the main math model (All predictors)

y = math_data$Attainment_Maths
x = model.matrix(~ age + interception + Ckat_Tracing + Ckat_aiming + Ckat_Tracking + Open + Closed, data = math_data)

stan_data_math = list(y = y, X = x, N = length(y), J = max(y), Q = dim(x)[2])

fit1 <- stan(
  file = "order.stan",  # Stan program
  data = stan_data_math,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 6000,            # total number of iterations per chain
  cores = 4,            # number of cores (using 2 just for the vignette)
  control = list(adapt_delta = 0.999),
  pars = c("theta"),
  include = FALSE)

#Flush sims to file
maths_samples = extract(fit1, pars = c("theta", "log_lik", "cuts_raw"), include = FALSE)
write.csv(maths_samples, "/Users/newxjy/Desktop/VR_Math_Anlaysis/Raw_data/Sample/maths_samples_tmp.csv")


#------------------------------------------------------------#
#Fit the main math model with attainment (All predictors)
#Remove extraneous data
math_data = data[,c('age', "I_30250", "I_30400", "I_30550", "I_40250", "I_40400", "I_40550", "I_50250", "I_50400", "I_50550", 'interception', 'Ckat_Tracing',
                    'Ckat_aiming', 'Ckat_Tracking', 'Open', 'Closed', 'Attainment_Maths', 'Attainment_Reading', 'Attainment_Writing')]

#Drop missing values
math_data = math_data[complete.cases(math_data),]

y = math_data$Attainment_Maths
x = model.matrix(~ age + interception + Ckat_Tracing + Ckat_aiming + Ckat_Tracking + Open + Closed + Attainment_Reading + Attainment_Writing, data = math_data)

stan_data_math = list(y = y, X = x, N = length(y), J = max(y), Q = dim(x)[2])

fit2 <- stan(
  file = "order.Stan",  # Stan program
  data = stan_data_math,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 6000,            # total number of iterations per chain
  cores = 4,            # number of cores (using 2 just for the vignette)
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  pars = c("theta"),
  include = FALSE)

#Flush sims to file
maths_samples_2 = extract(fit2, pars = c("theta", "log_lik", "cuts_raw"), include = FALSE)
write.csv(maths_samples_2, "/Users/newxjy/Desktop/VR_Math_Anlaysis/Raw_data/Sample/maths_samples_w_attainment.csv")


#------------------------------------------------------------#
#Fit the main math model with all interceptive predictors (All predictors)
# 
# y = math_data$Attainment_Maths
# x = model.matrix(~ age + I_30250 +I_30400 + I_30550 + I_40250 + I_40400 + I_40550 + I_50250 +I_50400 +I_50550  + Ckat_Tracing + Ckat_aiming + Ckat_Tracking + Open + Closed, data = math_data)
# 
# stan_data_math = list(y = y, X = x, N = length(y), J = max(y), Q = dim(x)[2])
# 
# fit_all_int <- stan(
#   file = "Ordered_Probit.stan",  # Stan program
#   data = stan_data_math,    # named list of data
#   chains = 4,             # number of Markov chains
#   warmup = 2000,          # number of warmup iterations per chain
#   iter = 6000,            # total number of iterations per chain
#   cores = 4,            # number of cores (using 2 just for the vignette)
#   control = list(adapt_delta = 0.999),
#   pars = c("theta"),
#   include = FALSE
# )

