// The input data is a vector 'y' of length 'N'.
data {
int<lower=0> N;   // numero di tempi
int<lower=0> C;   // numero di controlli
vector[N] y ;     
matrix[N,C]x;
int<lower=0> N_new;  // number of new observations
matrix[N_new,C] X_new;  // new predictor matrix

}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> sigma_sq;
  simplex[C] gp1;
}

transformed parameters{
  real<lower=0> sigma;
  sigma = sqrt(sigma_sq);
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  sigma_sq ~ inv_gamma(4,2); // implies that P(ssq < 1) ~ 85%.
  gp1  ~ normal(0,1) ;
  
  // observational model

   y ~ normal(x*gp1, sigma);
}
  generated quantities {
  real y_new[N_new]; // new outcome vector
  y_new = normal_rng(X_new * gp1, sigma);
}
