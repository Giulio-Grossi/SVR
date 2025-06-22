# Checking our priors:

# b ~ normal(0, 1);
# 
# Priors on length-scale. Stan uses parameterization with mean alpha / beta.
# rho  ~ gamma(0.5, 1.5);
# rho_b  ~ gamma(0.5, 2);
#
# Prior on the variance of the residuals.
# alpha ~ inv_gamma(2, 0.35);
# 
# Prior on the spatial variance of the betas.
# alpha_b  ~ inv_gamma(2.7, 0.3);
#
# Proportion of outcome error that is spatial.
# w  ~ uniform(0, 1);

bands <- 20
treated_radius <- seq(0, 1, length.out = bands)

# covariance matrix of the beta GP 
Sigma <- (curr_alpha * (exp(- D_xloc ^ 2 / (2*(curr_rho ^ 2)))) + 
            sp_nugget * diag(1, length(treated_radius)))


# --------- Checking the betas that arise from this prior. ----------- #

# Essentially checking priors on b, rho_b, alpha_b.

# Getting the matrix of distances.
D_xloc <- as.matrix(dist(sort(treated_radius))) # distance to be used - spatial GP 

B <- 1000
betas <- matrix(NA, bands, B)

set.seed(123)

for (bb in 1 : B) {
  bi <- rnorm(1, 0, 1)
  curr_alpha <- MCMCpack::rinvgamma(1, 2.7, 0.3)
  curr_rho <- rgamma(1, 0.5, rate = 2)
  Sigma <- curr_alpha * (exp(- D_xloc ^ 2 / (2*(curr_rho ^ 2)))) + 0.001 * diag(1, bands)
  betas[, bb] <- bi + t(chol(Sigma)) %*% as.vector(rnorm(bands, 0,1))
}

plot(1, type = 'n', xlim = c(0, 1), ylim = c(- 3, 3), xlab = 'Treated units',
     ylab = expression(beta[c]),
     main = 'Induced prior distribution on the regression coefficients')
for (bb in 1 : 30) {
  lines(treated_radius, betas[, bb])
  Sys.sleep(0.1)
}


# --------- Checking the error covariance from this prior. ----------- #

Sigmas <- array(NA, dim = c(B, bands, bands))

for (bb in 1 : B) {
  curr_alpha <- MCMCpack::rinvgamma(1, 2, 0.35)
  curr_rho <- rgamma(1, 0.5, 1.5)
  w <- runif(1, 0, 1)
  Sigmas[bb, , ] <- curr_alpha * (w * exp(- D_xloc ^ 2 / (2*(curr_rho ^ 2))) +
                                    (1 - w) * diag(1, bands))
}

# Correlation across radii:
x <- Sigmas[, 1, ] / Sigmas[, 1, 1]

# Note that 10 out of 20 bands, so half the distance.
par(mfrow = c(4, 5), mar = rep(2, 4), oma = rep(3, 4))
for (ii in 2 : bands) {
  hist(x[, ii], main = paste('radius', round(treated_radius[ii], 2)), freq = F)
}
title(outer = TRUE, main = 'Induced prior distribution of the residual spatial correlation')



