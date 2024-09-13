data {
  int<lower=1> n_t;   // number of outcomes, bands in our case
  int<lower=1> n_c;   // number of predictors for each unit, control units in our case
  int<lower=1> t0;   // number of observation for each unit, pre-treatment times in our case
  int<lower=1> tt;   // number of observation for each unit, full times in our case
  vector[n_t] y[t0];   // array of treated units
  vector[n_c] x[t0];   // array of control units, pre-treatment times
  vector[n_c] xnn[tt];   // array of control units, full times
  real<lower=0,upper=1> h[n_t];  // vector of radii, have to be from 0 to 1.
}
parameters {
  matrix[n_t, n_c] beta;  // matrix of coef, n_t=number of bands, n_c=number of control units
  real <lower=0> rho; // Lengthscale for errors, it is squared in the covariance matrix.
  real <lower=0> alpha;  // Variance for the errors.
  real <lower=0, upper=1> w;  // Mixture of spatial and iid errors.
  real <lower=0> rho_b;  // Lengthscale for coefficients.
  real <lower=0> alpha_b[n_c];  // Variance for the coefficients.
  real b[n_c];  // Overall coefficient for each control.
 }
transformed parameters {
  // We transform the covariance matrices' variance to SD.
  // -GP- Check all alpha_sqrt and alpha_sqrt_b and their use.
  real<lower=0> alpha_sqrt;  // SD for the errors.
  real <lower=0> alpha_sqrt_b[n_c];  // SD for the coefficients.
  alpha_sqrt = sqrt(alpha);
  alpha_sqrt_b = sqrt(alpha_b);
}
model {
  //
  // ------- Parameter definitions. --------- //
  //
  matrix[n_t,n_t] K1;  // spatial covariance matrix for the outcome errors
  matrix[n_t,n_t] L;
  matrix[n_t,n_t] K_beta[n_c];  // spatial covariance matrix for the coefficients.
  matrix[n_t,n_t] L_beta[n_c];
  vector[n_t] mu[t0];  // Linear predictor of the outcome model.
  //
  // Covariance matrix for the error terms:
  // -GP- Check that I need
  K1 = gp_exp_quad_cov(h, sqrt(w) * alpha_sqrt, rho) + diag_matrix(rep_vector((1-w) * alpha, n_t)) ;
  L = cholesky_decompose(K1);
  // Covariance matrix for the coefficients:
  for(ii in 1:n_c) {
    K_beta[ii] = gp_exp_quad_cov(h, alpha_sqrt_b[ii], rho_b);
    L_beta[ii] = cholesky_decompose(K_beta[ii]);
    beta[,ii] ~ multi_normal_cholesky(rep_vector(b[ii],n_t), L_beta[ii]);
  }
  // Linear predictor.
  for (i in 1:t0) {  // loop over time periods.
    mu[i] =  beta * x[i];
  }
  //
  // ------- Priors. --------- //
  //
  // Prior on the overall importance of controls:
  b ~ normal(0, 1);
  // Priors on length-scale. Stan uses parameterization with mean alpha / beta.
  // This prior gives: P(< 0.1) ~ 40%, P(< 0.3) ~ 0.67, P(< 0.5) ~ 0.78, and
  // P(< 1) ~ 92%.
  rho  ~ gamma(0.5, 1.5);
  // This prior gives: P(< 0.1) ~ 47%, P(< 0.3) ~ 0.72, P(< 0.5) ~ 0.83, and
  // P(< 1) ~ 95%.
  rho_b  ~ gamma(0.5, 2);
  // Prior on the variance of the residuals. This prior specifies that
  // P(< 0.1) ~ 0.13, P(< 0.3) ~ 0.67, P(< 0.5) ~ 0.85, P(< 1) ~ 0.96.
  alpha ~ inv_gamma(2, 0.35);
  // Prior on the spatial variance of the betas. This prior specifies that
  // P(< 0.05) ~ 0.04, P(< 0.1) ~ 0.35, P(< 0.3) ~ 0.88, P(< 0.5) ~ 0.96.
  alpha_b  ~ inv_gamma(2.7, 0.3);
  // Proportion of outcome error that is spatial.
  w  ~ uniform(0, 1);
  //
  // ------- Outcome model. --------- //
  //
  y ~ multi_normal_cholesky(mu, L);
}
generated quantities{
  // temp parameter
  matrix[n_t,n_t] K11;
  matrix[n_t,n_t] L1;
  vector[n_t] ynn[tt];   // array of treated units
  vector[n_t] munn[tt]; // linear predictor
  K11 = gp_exp_quad_cov(h, sqrt(w) * alpha_sqrt, rho)  +  diag_matrix(rep_vector((1-w)*alpha, n_t));
  L1 = cholesky_decompose(K11);
  for (i in 1:tt) {
    munn[i] = beta * xnn[i];
  }
  ynn = multi_normal_cholesky_rng(munn, L1);
}
