/**Ordered probit model with the simplex trick to get an ordered vector with fixed upper and lower bound.
Thanks to Bob Carpenter and Conor Goold for their help with the model
*/

data { 

  int<lower = 2> J;  //Number of outcome levels  
  int<lower = 1> Q; //Number of predictor variables (plus 1 for the intercept)  

  int<lower=0> N;  // data length  
  matrix[N, Q] X; // linear predictor

  int<lower=1,upper=J> y[N]; // response 
  
  
}
  
transformed data { 

  real L;
  real<lower = 1.5> U;
  real<lower = 0> diff; 
  int<lower = 1> K;

  K = J - 1; //Number of cutoff points 
  L = 1.5; //Lower cut off value
  U = J - 0.5; //Upper cut off value
  diff = U - L; 
}
  
parameters { 
  simplex[K - 1] cuts_raw;   
  vector[Q] beta;
  real<lower=0> sigma;  

}


transformed parameters {
  vector[J] theta[N];
  ordered[K] cuts; 

  cuts[1] = L; 
  cuts[2:K] = L + diff * cumulative_sum(cuts_raw);   

  for(n in 1:N) { 
	real eta;
    eta = X[n] * beta;
    theta[n,1] = normal_cdf( cuts[1] , eta, sigma);
    for (l in 2:K)
        theta[n,l] = normal_cdf(cuts[l], eta, sigma) - normal_cdf(cuts[l-1], eta , sigma);
    theta[n,J] = 1 - normal_cdf(cuts[K] , eta , sigma);
    }
}

model{    

    sigma ~ cauchy(0, 100);
    beta ~ normal(0, 500);

    for(n in 1:N) {         
    	y[n] ~ categorical(theta[n]);
    }
}

generated quantities{
	
	int y_rep[N]; //Replicate data
	real log_lik[N]; //Log likelihood 

	//Get log_lik and replicate data
	for (n in 1:N){
		y_rep[n] = categorical_rng(theta[n]);
		log_lik[n] = categorical_lpmf(y[n] | theta[n]);

	}
}