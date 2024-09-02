 data {
int<lower=0> K;
int<lower=0> D;
int<lower=0> N;
int<lower=0> C;
vector[N] y;
real h[D];
matrix[N,K]x;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> sigma;
  real<lower=0> alpha;
  real<lower=0> rho;
  real delta;
  vector[K] gp1;
//  vector[D] prior;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  matrix[D, D] L_K;
  matrix[D, D] K1 = cov_exp_quad(h,alpha, rho);
  // vector[K]beta;
  for (n in 1:D)
  K1[n, n] = K1[n, n] + 0.001;
  L_K = cholesky_decompose(K1);
  rho ~ normal(0,1);  // prior meno informativa sotto 0.51
  alpha ~ normal(0,1);
  sigma ~ normal(0,1);
  delta ~ normal(0,1);
//  prior~normal(0,4);
  for (i in 1:C)
  gp1[(((i-1)*D)+1):D*i] ~ multi_normal_cholesky(rep_vector(0,D), L_K);
   y ~ normal(delta + x*gp1, sigma);
}

