# Load necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(cvxr)
library(purrr)



## step 0: calculate the data under the null hypothesis by subtracting the 
## null hypothesis from the actual value of the observed outcome 

treated_new<- sim[,1:bands] - null

## now define the matrix of treated and control units
## for the conformal estimation
 
X0 <- sim[,(bands+1):ncol(sim)]

for(ii in 1:bands){
  beta_new <- sepSC(treated_new[,ii], X0)
  
  
}






# Define test statistic function
test_statistic <- function(u_hat, q = 1) {
  return(mean(abs(u_hat)^q)^(1 / q))
}

# Define p-value computation
p_value <- function(resid_df, q = 1) {
  u <- resid_df$residuals
  post_intervention <- resid_df$post_intervention
  
  n <- length(u)
  statistics <- sapply(0:(n - 1), function(permutation) {
    u_permuted <- c(u[(permutation + 1):n], u[1:permutation])
    test_statistic(u_permuted[post_intervention], q = q)
  })
  
  p_val <- mean(statistics >= statistics[1])
  
  return(p_val)
}

# Define function to compute p-values over a grid of null hypotheses
p_val_grid <- function(df, state, nulls, intervention_start, period, model) {
  df_aug <- df %>% filter(year < intervention_start) %>% bind_rows(df %>% filter(year == period))
  
  p_vals <- sapply(nulls, function(null) {
    resid_df <- residuals_function(
      df_aug,
      state = state,
      null = null,
      intervention_start = period,
      window = 1,
      model = model
    )
    p_value(resid_df, q = 1)
  })
  
  p_vals_df <- data.frame(nulls = nulls, p_values = p_vals)
  colnames(p_vals_df)[2] <- as.character(period)
  
  return(p_vals_df)
}

# Compute p-values over the grid
model <- SyntheticControl()
nulls <- seq(-20, 20, length.out = 100)

p_values_df <- p_val_grid(
  data,
  state = "california",
  nulls = nulls,
  intervention_start = 1988,
  period = 1988,
  model = model
)

p_values_df

# Function to compute confidence interval from p-values
confidence_interval_from_p_values <- function(p_values_df, alpha = 0.1) {
  big_p_values <- p_values_df[p_values_df$p_values >= alpha, ]
  
  ci_lower <- min(big_p_values$nulls)
  ci_upper <- max(big_p_values$nulls)
  
  ci_df <- data.frame(
    period = colnames(p_values_df)[2],
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
  colnames(ci_df)[2:3] <- paste0((1 - alpha) * 100, "%_", c("ci_lower", "ci_upper"))
  
  return(ci_df)
}

# Function to compute confidence interval for a given period
compute_period_ci <- function(df, state, nulls, intervention_start, period, model, alpha = 0.1) {
  p_vals_df <- p_val_grid(
    df = df,
    state = state,
    nulls = nulls,
    intervention_start = intervention_start,
    period = period,
    model = model
  )
  
  ci_df <- confidence_interval_from_p_values(p_vals_df, alpha = alpha)
  
  return(ci_df)
}

# Function to compute confidence intervals over a window of periods
confidence_interval <- function(df, state, nulls, intervention_start, window, model, alpha = 0.1) {
  periods <- seq(intervention_start, intervention_start + window - 1)
  
  ci_list <- lapply(periods, function(period) {
    compute_period_ci(df, state, nulls, intervention_start, period, model, alpha)
  })
  
  ci_df <- bind_rows(ci_list)
  return(ci_df)
}

# Compute the confidence intervals
model <- SyntheticControl()
nulls <- seq(-60, 20, length.out = 100)

ci_df <- confidence_interval(
  data,
  state = "california",
  nulls = nulls,
  intervention_start = 1988,
  window = 2000 - 1988 + 1,
  model = model,
  alpha = 0.1
)

ci_df
