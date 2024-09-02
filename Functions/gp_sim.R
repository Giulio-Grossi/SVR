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


gp_sim<-function(num_controls, time_periods, bands){
expit <- function(x) exp(x) / (1 + exp(x))
#set.seed(124)

# ----------------------- PART A ---------------------- #
# ------------------- Specifications ------------------ #

# Number of control regions and how big they are:
num_controls <- num_controls
num_important <- round(num_controls/2)
control_radius <- runif(num_controls, min = 0.05, max = 0.23)

# Based on these numberr of controls, we create a window that can fit them all.
y_shift <- 0
W <- owin(xrange = c(0, 1 + ceiling(num_controls) / 4), yrange = c(0, 1) + y_shift)

# the radii of the treated region:
treated_radius <- treated_radius

# number of time periods
time_periods <- time_periods

# Need to specify GP parameters.
alpha_loc <- 1
alpha_time <- 1
rho_xloc <- diff(W$xrange / 10)
rho_yloc <- diff(W$yrange / 10)
rho_time <- time_periods / 10


# --------------  End of specifications ------------- #


# ----------------------- PART B ---------------------- #
# ---------------------- Windows ---------------------- #


# Creating the treated windows:
treated <- list()
for (rr in 1 : length(treated_radius)) {
  treated[[rr]] <- disc(radius = treated_radius[rr], centre = c(0.5, 0.5 + y_shift))
}

# Creating the control windows:
controls <- list()
for (ww in 1 : num_controls) {
  center_x <- ceiling(ww / 2) * 0.5 + 1 - 0.25
  center_y <- (ww %% 2) * 0.5 + 0.25 + y_shift
  controls[[ww]] <- disc(radius = control_radius[ww], centre = c(center_x, center_y))
}

# Splitting the big window in separate areas.
Wsplit <- NULL
Wsplit[[1]] <- owin(c(0, 1), c(0, 1) + y_shift)
ss <- 2
curr_x <- 1.25
while (curr_x <= W$xrange[2]) {
  Wsplit[[ss]] <- owin(c(curr_x - 0.25, curr_x + 0.25), c(0, 0.5) + y_shift)
  ss <- ss + 1
  Wsplit[[ss]] <- owin(c(curr_x - 0.25, curr_x + 0.25), c(0.5, 1) + y_shift)
  ss <- ss + 1
  curr_x <- curr_x + 0.5
}


# Plotting them everything.
plot(W) # The whole region
axis(1)
axis(2)
# Plotting the splits:
plot(as.tess(Wsplit), add = TRUE)
# Plotting the regions we follow:
for (ii in 1 : length(treated_radius)) { # The treated
  plot(treated[[ii]], add = TRUE)
}
for (ii in 1 : num_controls) { # The controls
  plot(controls[[ii]], add = TRUE, col = 'green')
}



# ------------------- PART C -------------------- #
# ------ Setting up the covariance matrix ------- #

x_locations <- seq(W$xrange[1], W$xrange[2], length.out = diff(W$xrange) * 30)
y_locations <- seq(W$yrange[1], W$yrange[2], length.out = diff(W$yrange) * 30)
all_times <- 1 : time_periods

all_locations <- expand.grid(x_locations, y_locations)
points(all_locations, pch = 16, cex = 0.1)

D_xloc <- as.matrix(dist(all_locations[, 1]))
D_yloc <- as.matrix(dist(all_locations[, 2]))
D_time <- as.matrix(dist(all_times))

Sigma_loc <- alpha_loc ^ 2 * (exp(- D_xloc ^ 2 / rho_xloc ^ 2 - D_yloc ^ 2 / rho_yloc) + 0.01 * diag(nrow(all_locations)))
Sigma_time <- alpha_time ^ 2 * (exp(- D_time ^ 2 / rho_time ^ 2) + 0.01 * diag(time_periods))

# corrplot::corrplot(Sigma_time[1 : 40, 1 : 40], is.corr = F)
# corrplot::corrplot(Sigma_loc[1 : 75, 1 : 75], is.corr = F)
# 
# corrplot::corrplot(Sigma_loc[seq(1, nrow(Sigma_loc), by = 75),
#                              seq(1, nrow(Sigma_loc), by = 75)], is.corr = F)

chol_loc <- chol(Sigma_loc)
chol_time <- chol(Sigma_time)


# ---------------------------- PART D ---------------------------- #
# -------- Generating from the spatio-temporal GP -------- #

Y_dim <- c(nrow(all_locations), time_periods)

# Underlying spatial pattern:
spat_pattern <- t(chol_loc) %*% as.vector(rnorm(nrow(all_locations)))
# Underlying temporal pattern:
temp_pattern <- t(chol_time) %*% as.vector(rnorm(time_periods))
plot(1 : time_periods, temp_pattern, type = 'l')

# Time-specific spatial pattern and space-specific temporal pattern.
all_spat <- array(NA, dim = Y_dim)
all_temp <- array(NA, dim = Y_dim)
for (tt in 1 : time_periods) {
  all_spat[, tt] <- spat_pattern + 0.9 * t(chol_loc) %*% as.vector(rnorm(nrow(all_locations)))
}
for (ss in 1 : Y_dim[1]) {
  all_temp[ss, ] <- temp_pattern + 0.9 * t(chol_time) %*% as.vector(rnorm(time_periods))
}

Y <- all_spat + all_temp


# Plotting the first few as a function of space.
for (tt in 1 : 10) {
  fields::image.plot(x = x_locations, y = y_locations, z = matrix(Y[, tt], nrow = length(x_locations)))
  plot(as.tess(Wsplit), add = TRUE)
  # Plotting the regions we follow:
  for (ii in 1 : length(treated_radius)) { # The treated
    plot(treated[[ii]], add = TRUE)
  }
  for (ii in 1 : num_controls) { # The controls
    plot(controls[[ii]], add = TRUE)
  }
  Sys.sleep(0.5)
}

# Plotting a few pixels as a function of time.
for (ss in 1 : 10) {
  s_ind <- sample(1 : nrow(all_locations), 1)
  plot(Y[s_ind, ], type = 'l')
  Sys.sleep(0.5)
}



# ------------------- PART E ---------------------- #
# ---- Calculating averages within each region ---- #

Ytreat <- matrix(NA, nrow = time_periods, ncol = length(treated_radius))
Ycon <- matrix(NA, nrow = time_periods, ncol = num_controls)

for (tt in 1 : time_periods) {
  
  this_im <- matrix(Y[, tt], nrow = length(x_locations), ncol = length(y_locations))
  this_im <- im(mat = t(this_im), xcol = x_locations, yrow = y_locations)
  
  # Averaging in regions.
  Ytreat[tt, ] <- sapply(treated, function(W) mean(as.im(this_im, W = W)))
  Ycon[tt, ] <- sapply(controls, function(W) mean(as.im(this_im, W = W)))
  
}


# Plotting all of them as a function of time:
plot(1, type = 'n', ylim = range(c(Ytreat, Ycon)), xlim = c(1, time_periods))
for (rr in 1 : length(treated_radius)) {
  lines(1 : time_periods, Ytreat[, rr], col = 'red')
}
for (cc in 1 : num_important) {
  lines(1 : time_periods, Ycon[, cc], lwd = 0.6)
}
if (num_controls > num_important) {
  for (cc in (num_important + 1) : num_controls) {
    lines(1 : time_periods, Ycon[, cc], col = 'yellow', lwd = 0.6)
  }
}


# Plotting all of them as a function of time:
plot(1, type = 'n', ylim = c(- 3, 3), xlim = c(1, time_periods))
for (rr in 1 : length(treated_radius)) {
  lines(1 : time_periods, (Ytreat[, rr] - mean(Ytreat[, rr])) / sd(Ytreat[, rr]), col = 'red')
}
for (cc in 1 : num_important) {
  Sys.sleep(0.5)
  lines(1 : time_periods, (Ycon[, cc] - mean(Ycon[, cc])) / sd(Ycon[, cc]), lwd = 0.5)
}
for (cc in (num_important + 1) : num_controls) {
  Sys.sleep(0.5)
  lines(1 : time_periods, (Ycon[, cc] - mean(Ycon[, cc])) / sd(Ycon[, cc]), col = 'yellow', lwd = 0.8)
}


corrplot::corrplot(cor(cbind(Ytreat, Ycon)))
sim=cbind(Ytreat, Ycon)
return(sim)
}
