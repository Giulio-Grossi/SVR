gaussian_kernel <-
  function(dmat,
           range_parameter = 1,
           scale_parameter = 1) {
    return(scale_parameter * exp(-abs(dmat) ^ 2 / (2 * range_parameter ^ 2)))
  }
