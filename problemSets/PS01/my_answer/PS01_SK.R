#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

#####################
# Problem 1
#####################

set.seed(123)

# [Task] Generate 1000 Cauchy random variables
# NOTE: Defined 'data' here so the template code below runs correctly
data <- rcauchy(1000, location = 0, scale = 1)

# create empirical distribution of observed data
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))

print(paste("KS Statistic D:", D))

# [Task] Calculate p-value using the infinite series formula
calculate_ks_pvalue <- function(d) {
  sum_term <- 0
  # Iterate 1000 times for approximation
  for(k in 1:1000) {
    term <- exp(-((2*k - 1)^2 * pi^2) / (8 * d^2))
    sum_term <- sum_term + term
    
    # Check convergence
    if(term < 1e-15) break
  }
  p_val <- (sqrt(2 * pi) / d) * sum_term
  return(p_val)
}

p_value <- calculate_ks_pvalue(D)
print(paste("Calculated p-value:", p_value))


#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# [Task] Estimate OLS using BFGS (optim) vs lm

# 1. Define Loss Function (Sum of Squared Residuals)
ols_loss_fn <- function(theta, y, x) {
  beta0 <- theta[1]
  beta1 <- theta[2]
  
  # Linear model prediction
  y_pred <- beta0 + beta1 * x
  
  # Sum of Squared Errors
  sum((y - y_pred)^2)
}

# 2. Run Optimization using BFGS
# Initial guess (Intercept=0, Slope=0)
initial_params <- c(0, 0)

opt_res <- optim(par = initial_params, 
                 fn = ols_loss_fn, 
                 y = data$y, 
                 x = data$x, 
                 method = "BFGS")

print("--- BFGS Results ---")
print(opt_res$par)

# 3. Compare with standard lm()
lm_res <- lm(y ~ x, data = data)

print("--- lm Results ---")
print(coef(lm_res))

# 4. Check difference
diff <- sum(abs(opt_res$par - coef(lm_res)))
print(paste("Difference:", diff))