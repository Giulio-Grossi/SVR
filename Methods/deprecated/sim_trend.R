# Setting up simulations for the project.
#
# Create a number of control regions and one treated region over time.
# Businesses in these regions are generated in the beginning from a homogeneous
# poisson process. At every time point, a business might die and a new business
# might be born.
#
# This is a first step towards simulations. At the end of the code, I discuss
# some of the improvements that we need to make eventually.
simulations<-function(num_controls, time_periods, t0, bands){
library(spatstat)
library(boot)
expit <- function(x) exp(x) / (1 + exp(x))


# ----------------------- PART A ---------------------- #
# ------------------- Specifications ------------------ #

# Number of control regions and how big they are:
num_controls <- num_controls
control_radius <- runif(num_controls, min = 0.05, max = 0.23)

# Based on these numberr of controls, we create a window that can fit them all.
W <- owin(xrange = c(0, 1 + ceiling(num_controls) / 4), yrange = c(0, 1))

# the radii of the treated region:
treated_radius <- sort(runif(bands, min=0.03, max=0.4))

# number of time periods
time_periods <- time_periods

# time of intervention
t0 <- t0

# Initial rate of creation
baseline_rate <- 100


# Rate of death for a businesses
death_prob_control <- 0.05 + runif(2, min = - 0.03, 0.03)

# Deciding on the birth rate such that the expected number of businesses over
# time is constant.
# baseline_rate * area(W) is how many businesses I start with.
birth_rate_control <- baseline_rate * area(W) * death_prob_control

# Not used now:
# death_prob_treated <- 0.01
# birth_rate_treated <- 3


# --------------  End of specifications ------------- #


# ----------------------- PART B ---------------------- #
# ---------------------- Windows ---------------------- #


# Creating the treated windows:
treated <- list()
for (rr in 1 : length(treated_radius)) {
  treated[[rr]] <- disc(radius = treated_radius[rr], centre = c(0.5, 0.5))
}

# Creating the control windows:
controls <- list()
for (ww in 1 : num_controls) {
  center_x <- ceiling(ww / 2) * 0.5 + 1 - 0.25
  center_y <- (ww %% 2) * 0.5 + 0.25
  controls[[ww]] <- disc(radius = control_radius[ww], centre = c(center_x, center_y))
}

# Splitting the big window in separate areas.
Wsplit <- NULL
Wsplit[[1]] <- owin(c(0, 1), c(0, 1))
ss <- 2
curr_x <- 1.25
while (curr_x <= W$xrange[2]) {
  Wsplit[[ss]] <- owin(c(curr_x - 0.25, curr_x + 0.25), c(0, 0.5))
  ss <- ss + 1
  Wsplit[[ss]] <- owin(c(curr_x - 0.25, curr_x + 0.25), c(0.5, 1))
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
# ---- progression of birth/death by area ------- #

# Matrix where the death rates are saved:
# Rows are for the different regions of the map. Columns for time periods.
death_all <- matrix(NA, nrow = length(Wsplit), ncol = time_periods)
death_all[, 1 : 5] <- death_prob_control[1]
death_all[, - c(1 : 5)] <- death_prob_control[2]
# Entry 1 is the treated area, and 2 - 11 are the control areas.
for (tt in 2 : time_periods) {
  death_all[1 : 5, tt] <- expit(logit(death_all[1, tt - 1]) + rnorm(1, 0, sd = 0.02))
  death_all[6 : (num_controls + 1), tt] <- expit(logit(death_all[6, tt - 1]) + rnorm(1, 0, sd = 0.02))
}

plot(death_all[1, ], type = 'l', ylim = range(death_all))
lines(death_all[6, ], col = 'red')

# Need to do something similar for birth rates.
birth_all <- matrix(NA, nrow = length(Wsplit), ncol = time_periods)
birth_all[, 1 : 5] <- birth_rate_control[1]
birth_all[, - c(1 : 5)] <- birth_rate_control[1]
for (tt in 2 : time_periods) {
  birth_all[1 : 5, tt] <- exp(log(birth_all[1, tt - 1]) + rnorm(1, 0, sd = 0.02))
  birth_all[6 : (num_controls + 1), tt] <- exp(log(birth_all[6, tt - 1]) + rnorm(1, 0, sd = 0.02))
}

plot(birth_all[1, ], type = 'l', ylim = range(birth_all))
lines(birth_all[6, ], col = 'red')




# ---------------------------- PART D ---------------------------- #
# -------- Generating the starting location of businesses -------- #

set.seed(124)

# I need the locations of businesses across time for each of the regions.

Ymap <- list()

# Generating the surface at the first time period such that there are
# in expectation baseline_rate points per unit of area.
Ymap[[1]] <- rpoispp(lambda = baseline_rate * area(W), win = W)
plot(Ymap[[1]])


# Based on the starting locations and the location-time specific birth and
# death rates, continue generating data.
# At each time period, points might die, and new ones will come up.
for (tt in 2 : time_periods) {
  use_points <- cbind(x = Ymap[[tt - 1]]$x, y = Ymap[[tt - 1]]$y)
  # Find which area the point belongs to:
  in_area <- rep(NA, nrow(use_points))
  for (ii in 1 : length(Wsplit)) {
    area_points <- which(inside.owin(Ymap[[tt - 1]], w = Wsplit[[ii]]))
    in_area[area_points] <- ii
  }
  # Kill some of them by area:
  kill_these <- rbinom(nrow(use_points), 1, prob = death_all[in_area, tt])
  use_points <- use_points[kill_these != 1, ]
  # Add some from the birth process:
  for (ii in 1 : length(Wsplit)) {
    add_points <- rpoispp(lambda = birth_all[ii, tt], win = Wsplit[[ii]])
    use_points <- rbind(use_points, cbind(x = add_points$x, y = add_points$y))
  }
  Ymap[[tt]] <- as.ppp(X = use_points, W = W)
}

# Plotting the first few:
for (tt in 1 : 10) {
  plot(Ymap[[tt]], main = tt)
  Sys.sleep(0.2)
}

# Number of businesses over time everywhere:
plot(sapply(1 : time_periods, function(tt) Ymap[[tt]]$n))


# --------------- PART E ----------------- #
# ---- Counting points in each region ---- #

Ytreat <- matrix(NA, nrow = time_periods, ncol = length(treated_radius))
Ycon <- matrix(NA, nrow = time_periods, ncol = num_controls)

for (tt in 1 : time_periods) {
  
  # Counting the number of points in the treated regions:
  Ytreat[tt, ] <- sapply(treated, function(W) sum(inside.owin(x = Ymap[[tt]], w = W)))
  
  # Counting number of points in the control regions:
  Ycon[tt, ] <- sapply(controls, function(W) sum(inside.owin(x = Ymap[[tt]], w = W)))
  
}


# Plotting all of them as a function of time:
plot(1, type = 'n', ylim = range(c(Ytreat, Ycon)), xlim = c(1, time_periods))
for (cc in 1 : num_controls) {
  lines(1 : time_periods, Ycon[, cc])
}
for (rr in 1 : length(treated_radius)) {
  lines(1 : time_periods, Ytreat[, rr], col = 'red')
}



corrplot::corrplot(cor(cbind(Ytreat, Ycon)))
sim=cbind(Ytreat, Ycon)
return(sim)
}