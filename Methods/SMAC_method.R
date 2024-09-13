#' Performing separate SMAC. 
#' 
#' @param ym.pre Matrix T0 x bands for the pre-intervention period and the
#' outcomes units.
#' @param x.pre Matrix T0 x (# controls) for the pre-intervention period and
#' the control units.
#' #' @param x Matrix T x (# controls) for the pre-intervention and 
#' post-intervention period and the control units.
#' 
#' @param treated_radius vector of (# treated units) distances across treated
#' 
#' Depends on the MGP.stan file, and rstan library
#' 
SMAC <- function(ym.pre, x.pre, x, treated_radius) {
  
  # arguments
  bands <- ncol(ym.pre)
  num_controls <- ncol(x.pre)
  time_periods=nrow(x)
  t0=nrow(x.pre)

  # Without an intercept:
  ss_data = list(
    x = x.pre,
    y = ym.pre,
    xnn = x,
    h = treated_radius,
    tt = time_periods,
    t0 = t0,
    n_t = bands, 
    n_c = num_controls
  )
  
  # With an intercept it would be:
  #   x = cbind(1, x.pre),
  #   xnn = cbind(1, x),
  #   n_c=num_controls+1
  
  parameters <- rstan::stan(
    file = "Methods/SMAC.stan",  # SMAC.
    data = ss_data,
    cores = 3,
    iter = iter,
    chains = 3,
    verbose = F,
    warmup = warm,
    control = list(
      max_treedepth = 15,
      stepsize = 0.02,
      adapt_delta = 0.99
    )
  )
  
  return(parameters)
  
}

