data {
  int<lower=1> n_t;   ////// number of outcomes, bands in our case
  int<lower=1> n_c;   ////// number of predictors for each unit, control units in our case
  int<lower=0> t0;   ////// number of observation for each unit, pre-treatment times in our case
  int<lower=0> tt;   ////// number of observation for each unit, full times in our case
  vector[n_t] y[t0];   ////// array of treated units
  vector[n_c] x[t0];   ////// array of control units, pre-treatment times
  vector[n_c] xnn[tt];   ////// array of control units, full times
  real h[n_t];           //////// vector of relative distances
}
parameters {
  matrix[n_t, n_c] beta;  ////// matrix of coef, n_t=number of bands, n_c=number of control units
  real <lower=0> rho;
  real <lower=0> alpha;
  real <lower=0, upper=1> w;
  real <lower=0> rho_b;
  real <lower=0> alpha_b[n_c];
  real b[n_c];
 }
model {
  // temp parameter
  matrix[n_t,n_t] K1;
  matrix[n_t,n_t] L;
  matrix[n_t,n_t] K_beta[n_c];
  matrix[n_t,n_t] L_beta[n_c];
  vector[n_t] mu[t0]; //// linear predictor
  K1 = gp_exp_quad_cov(h, w*alpha, rho) + diag_matrix(rep_vector((1-w)*alpha, n_t)) ;
  L = cholesky_decompose(K1);
   for(ii in 1:n_c)
   K_beta[ii] = gp_exp_quad_cov(h, alpha_b[ii], rho_b);
   for(ii in 1:n_c)
   L_beta[ii] = cholesky_decompose(K_beta[ii]);
  //K_beta = gp_exponential_cov(h, alpha_b, rho_b);
  //L_beta = cholesky_decompose(K_beta);
  // sampling
  // for (ii in 1:n_c)
  // beta[,ii] ~ multi_normal_cholesky(rep_vector(0,n_t), L_beta[ii]);
  for (ii in 1:n_c)
  beta[,ii] ~ multi_normal_cholesky(rep_vector(b[ii],n_t), L_beta[ii]);
  rho  ~ gamma(0.5,2);
  b ~ normal(0,1);
  alpha ~ inv_gamma(2,0.25);
  rho_b  ~ gamma(0.5,2);
  w  ~ uniform(0,1);
  alpha_b  ~ inv_gamma(2.7, 0.1);
  for (n in 1:t0)
  mu[n] =  beta*x[n];
  y ~ multi_normal_cholesky(mu, L);  ////// observational model
}
generated quantities{
  // temp parameter
  matrix[n_t,n_t] K11;
  matrix[n_t,n_t] L1;
  vector[n_t] ynn[tt];   ////// array of treated units
  vector[n_t] munn[tt]; //// linear predictor
  K11 = gp_exp_quad_cov(h, w*alpha, rho)  +  diag_matrix(rep_vector((1-w)*alpha, n_t));
  L1 = cholesky_decompose(K11);
  for (n in 1:tt)
  munn[n] = beta * xnn[n];
  ynn = multi_normal_cholesky_rng(munn, L1);
}
