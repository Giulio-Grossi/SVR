# Setting up simulations for the project.
#
# Create a number of control regions and one treated region over time.
# Businesses in these regions are generated in the beginning from a homogeneous
# poisson process. At every time point, a business might die and a new business
# might be born.
#
# This is a first step towards simulations. At the end of the code, I discuss
# some of the improvements that we need to make eventually.

simulations_2<-function(num_controls, time_periods, t0, bands){
  library(spatstat)
  set.seed(123)
  
  # ----------------------- PART A ---------------------- #
  # ------------------- Specifications ------------------ #
  
  # Number of control regions and how big they are:
  #num_controls <- 10
  control_radius <- runif(num_controls, min = 0.1, max = sqrt(bands)*0.1 )
  
  # Based on these numberr of controls, we create a window that can fit them all.
  W <- owin(xrange = c(0, 1.5 + ceiling(num_controls) / 4), yrange = c(0, 1))
  
  # the radii of the treated region:
  #treated_radius <- c(0.15, sqrt(2)*0.15, sqrt(3)*0.15, sqrt(4)*0.15)
  treated_radius <-sqrt(seq(1:bands))*0.1
  
  # # number of time periods
  # time_periods <- 40
  # 
  # # time of intervention
  # t0 <- t0
  
  # Initial rate of creation
  baseline_rate <- 100
  
  
  # Rate of death for a businesses
  death_prob_control <- 0.05
  
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
    center_x <- ceiling(ww / 2) * 0.5 + 1
    center_y <- (ww %% 2) * 0.5 + 0.25
    controls[[ww]] <- disc(radius = control_radius[ww], centre = c(center_x, center_y))
  }
  
  
  # Plotting them:
  
  plot(W) # The whole region
  axis(1)
  axis(2)
  abline(v = 1) # Treated on the left, control on the right
  for (ii in 1 : length(treated_radius)) { # The treated
    plot(treated[[ii]], add = TRUE)
  }
  for (ii in 1 : num_controls) { # The controls
    plot(controls[[ii]], add = TRUE, col = 'green')
  }
  
  
  # ----------------------- PART C ------------------------ #
  # -------- Generating the location of businesses -------- #
  
  set.seed(124)
  
  # I need the locations of businesses across time for each of the regions.
  
  Ymap <- list()
  
  # Generating the surface at the first time period such that there are
  # in expectation baseline_rate points per unit of area.
  Ymap[[1]] <- rpoispp(lambda = baseline_rate * area(W), win = W)
  plot(Ymap[[1]])
  
  # Based on this, I will create the outcome at subsequent time periods. At each
  # time period, points might die, and new ones will come up.
  for (tt in 2 : time_periods) {
    use_points <- cbind(x = Ymap[[tt - 1]]$x, y = Ymap[[tt - 1]]$y)
    # Kill some of them:
    kill_these <- rbinom(nrow(use_points), 1, prob = death_prob_control)
    use_points <- use_points[kill_these != 1, ]
    # Add some from the birth process:
    add_points <- rpoispp(lambda = birth_rate_control, win = W)
    use_points <- rbind(use_points, cbind(x = add_points$x, y = add_points$y))
    Ymap[[tt]] <- as.ppp(X = use_points, W = W)
  }
  
  # Plotting the first few:
  for (tt in 1 : 10) {
    plot(Ymap[[tt]], main = tt)
    Sys.sleep(0.2)
  }
  
  # Number of businesses over time everywhere:
  plot(sapply(1 : time_periods, function(tt) Ymap[[tt]]$n))
  
  
  # --------------- PART D ----------------- #
  # ---- Counting points in each region ---- #
  
  Ytreat <- matrix(NA, nrow = time_periods, ncol = length(treated_radius))
  Ycon <- matrix(NA, nrow = time_periods, ncol = num_controls)
  
  for (tt in 1 : time_periods) {
    
    # Counting the number of points in the treated regions:
    Ytreat[tt, ] <- sapply(treated, function(W) sum(inside.owin(x = Ymap[[tt]], w = W)))
    
    # Counting number of points in the control regions:
    Ycon[tt, ] <- sapply(controls, function(W) sum(inside.owin(x = Ymap[[tt]], w = W)))
    
  }
  
  # Ensuring that the order is increasing in the 4 radii of the treated:
  plot(Ytreat[, 1], Ytreat[, 2])
  
  abline(a = 0, b = 1)
  
  # Plotting all of them as a function of time:
  plot(1, type = 'n', ylim = range(c(Ytreat, Ycon)), xlim = c(1, time_periods))
  for (cc in 1 : num_controls) {
    lines(1 : time_periods, Ycon[, cc])
  }
  for (rr in 1 : length(treated_radius)) {
    lines(1 : time_periods, Ytreat[, rr], col = 'red')
  }
  
  sim=cbind(Ytreat, Ycon)
}
