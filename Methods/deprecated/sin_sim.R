# Setting up simulations for the project.
#
# Create a number of control regions and one treated region over time.
# Businesses in these regions are generated in the beginning from a homogeneous
# poisson process. At every time point, a business might die and a new business
# might be born.
#
# This is a first step towards simulations. At the end of the code, I discuss
# some of the improvements that we need to make eventually.


args <- commandArgs(TRUE)
index <- as.numeric(args[1])

sin_sim<-function(num_controls, time_periods, t0, bands){


library(boot)
expit <- function(x) exp(x) / (1 + exp(x))

# ----------------------- PART A ---------------------- #
# ------------------- Specifications ------------------ #

# Number of control regions and how big they are:
#num_controls <- 10
num_important <-round(num_controls/2)

# the radii of the treated region:
#treated_radius <- c(0.07, 0.1, 0.15, 0.25)
R <- bands

# number of time periods
time_periods <- time_periods
overall_var <- 0.1


# --------------  End of specifications ------------- #


# ----------------------- PART B ---------------------- #
# ------------------- Mean structure ------------------ #

# The mean of the time series.
Ymean <- matrix(NA, nrow = num_controls + 1, ncol = time_periods)
Ymean[1 : (num_important + 1), ] <- rep(sin(pi * (1 : time_periods) / time_periods * 4),
                                        each = num_important + 1)
plot(Ymean[1, ], type = 'l')
if (num_controls > num_important) {
  Ymean[- c(1 : (num_important + 1)), ] <- rep(cos(pi * (1 : time_periods) / time_periods * 4),
                                               each = num_controls - num_important)
  lines(Ymean[num_important + 2, ], col = 'red')
}


# --------------- PART C ----------------- #
# ---------- Generating outcomes --------- #

Ytreat <- matrix(NA, nrow = time_periods, ncol = length(treated_radius))
Ycon <- matrix(NA, nrow = time_periods, ncol = num_controls)

# # Creating correlation matrix for the treated radii:
# treated_Sigma <- diag(1, nrow = R)
# for (ii in 1 : R) {
#   treated_Sigma[ii, ] <- exp(- (treated_radius[ii] - treated_radius) ^ 2 / 0.13)
# }
# treated_Sigma <- overall_var * treated_Sigma

treated_Sigma <- diag(1, bands)
for (ii in 1 : bands) {
  treated_Sigma[ii, ] <- exp(- (treated_radius[ii] - treated_radius) ^ 2 / rho_val ^ 2)
}

treated_Sigma <- overall_var*treated_Sigma + 0.01 * diag(bands)

chol(treated_Sigma)

for (tt in 1 : time_periods) {
  Ytreat[tt, ] <- Ymean[1, tt] + mvnfast::rmvn(n = 1, mu = rep(0, R), sigma = treated_Sigma)
  Ycon[tt, ] <- Ymean[- 1, tt] + rnorm(num_controls, sd = sqrt(overall_var))
}


sim=cbind(Ytreat, Ycon)
return(sim)
}
