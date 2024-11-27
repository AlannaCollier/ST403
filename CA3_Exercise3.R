
df <- with(mtcars, data.frame(y = mpg, x1 = disp, x2 = hp, x3 = wt))


nll_lm <- function(data, par) {
  n <- nrow(data)
  y <- data$y
  X <- as.matrix(cbind(1, data$x1, data$x2, data$x3))  # Add intercept
  
  beta <- par[1:4]  
  sigma <- par[5]  
  
  if (sigma <= 0) return(Inf) 
  
  
  epsilon <- y - X %*% beta
  
  
  nll <- (n / 2) * log(2 * pi * sigma^2) + sum(epsilon^2) / (2 * sigma^2)
  return(nll)
}


init <- c(mean(df$y), 0, 0, 0, 1)  

fit <- optim(par = init, fn = nll_lm, data = df, method = "L-BFGS-B",
             lower = c(-Inf, -Inf, -Inf, -Inf, 1e-8),  # Lower bounds for sigma
             upper = c(Inf, Inf, Inf, Inf, Inf))       # No upper bound for sigma


beta_hat <- fit$par[1:4]  
sigma_hat <- fit$par[5]   

cat("MLE Estimates:\n")
cat("Beta coefficients:", beta_hat, "\n")
cat("Sigma:", sigma_hat, "\n")
