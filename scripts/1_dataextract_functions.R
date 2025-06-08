library(tidyverse)
library(stringr)
library(broom)
library(nlraa)
library(stringi)

### functions to fit single phase exponential decay curve models to each data set of biomass or carbon loss over time 

# first start with starting values of a=100, k = 0.3

fun1 <- function(tmp) {
  
  fit = nls(percent_remaining ~ a * (exp(-k*time_days)),
            data = tmp,
            start = list(a=100, k = 0.3))
  lm <- lm(percent_remaining ~ predict(fit), data = tmp)
  k <- summary(fit)$coef[2]
  b <- NA
  n <- shapiro.test(summary(fit)$residuals)[2]$p.value
  rsquared <- broom::glance(lm)$adj.r.squared
  ss <- broom::glance(lm)$df.residual + broom::glance(lm)$df + 1
  table <-data.frame (name = unique(tmp$name), model = "nls", refractory = "no", k = k, b = b, AIC = AIC(fit), BIC = BIC(fit), rsquared = rsquared,normalresids = n, n = ss)
  
  plot(percent_remaining ~ time_days, tmp, pch = 20, ylim = c(0,110))
  lines(fitted(fit) ~ time_days, tmp, col = "red")
  legend("topright", bty = "n", cex = 0.7, legend = capture.output(fit))
  title(main = paste("no refractory - AIC:", round(AIC(fit), 2), "R-squared:", round(broom::glance(lm)$adj.r.squared, 5)), sub = unique(tmp$name))
  
  return(table)
  
}

# add a refractory term b to the model 

fun2 <- function(tmp) {

  fit = nls(percent_remaining ~ a * (exp(-k*time_days)) + b ,
            data = tmp,
            start = list(a=100, k = 0.3, b = 10 )) # we assume a small refractory component that does not degrade
  lm <- lm(percent_remaining ~ predict(fit), data = tmp) 
  k <- summary(fit)$coef[2]
  b <- summary(fit)$coef[3]
  n <- shapiro.test(summary(fit)$residuals)[2]$p.value
  rsquared <- broom::glance(lm)$adj.r.squared
  ss <- broom::glance(lm)$df.residual + broom::glance(lm)$df + 1
  table <-data.frame (name = unique(tmp$name), model = "nls", refractory = "yes",  k = k, b = b, AIC = AIC(fit), BIC = BIC(fit), rsquared = rsquared,normalresids = n, n = ss)
  
  plot(percent_remaining ~ time_days, tmp, pch = 20, ylim = c(0,110))
  lines(fitted(fit) ~ time_days, tmp, col = "red")
  legend("topright", bty = "n", cex = 0.7, legend = capture.output(fit))
  title(main = paste("with refractory - AIC:", round(AIC(fit), 2), "R-squared:", round(broom::glance(lm)$adj.r.squared, 5)), sub = unique(tmp$name))
  
  return(table)
}  


#pdf("fittedK_extracteddata.pdf")
#map(mylist_tables, possibly(my_function)) %>% bind_rows()
#dev.off()

# the model with these starting parameters are not suitable for some of the data sets
# change starting parameters to lower k values, otherwise models remain the same as previous functions

fun3 <- function(tmp) {
  
  fit = nls(percent_remaining ~ a * (exp(-k*time_days)),
            data = tmp,
            start = list(a=100, k = 0.1))
  lm <- lm(percent_remaining ~ predict(fit), data = tmp)
  k <- summary(fit)$coef[2]
  b <- NA
  n <- shapiro.test(summary(fit)$residuals)[2]$p.value
  rsquared <- broom::glance(lm)$adj.r.squared
  ss <- broom::glance(lm)$df.residual + broom::glance(lm)$df + 1
  table <-data.frame (name = unique(tmp$name), model = "nls smaller k", refractory = "no", k = k, b = b, AIC = AIC(fit), BIC = BIC(fit), rsquared = rsquared, normalresids = n, n = ss)
  
  plot(percent_remaining ~ time_days, tmp, pch = 20, ylim = c(0,110))
  lines(fitted(fit) ~ time_days, tmp, col = "red")
  legend("topright", bty = "n", cex = 0.7, legend = capture.output(fit))
  title(main = paste("no refractory - AIC:", round(AIC(fit), 2), "R-squared:", round(broom::glance(lm)$adj.r.squared, 5)), sub = unique(tmp$name))
  
  return(table)
} 

fun4 <- function(tmp) {
  
  fit = nls(percent_remaining ~ a * (exp(-k*time_days)) + b ,
              data = tmp,
              start = list(a=100, k = 0.1, b = 10 ))
  lm <- lm(percent_remaining ~ predict(fit), data = tmp) 
  k <- summary(fit)$coef[2]
  b <- summary(fit)$coef[3]
  n <- shapiro.test(summary(fit)$residuals)[2]$p.value
  rsquared <- broom::glance(lm)$adj.r.squared
  ss <- broom::glance(lm)$df.residual + broom::glance(lm)$df + 1
  table <-data.frame (name = unique(tmp$name), model = "nls smaller k", refractory = "yes", k = k, b = b, AIC = AIC(fit), BIC = BIC(fit), rsquared = rsquared, normalresids = n, n = ss)
  
  plot(percent_remaining ~ time_days, tmp, pch = 20, ylim = c(0,110))
  lines(fitted(fit) ~ time_days, tmp, col = "red")
  legend("topright", bty = "n", cex = 0.7, legend = capture.output(fit))
  title(main = paste("with refractory - AIC:", round(AIC(fit), 2), "R-squared:", round(broom::glance(lm)$adj.r.squared, 5)), sub = unique(tmp$name))
  
  return(table)
}  

# models converge to give same k and r values for most data sets with these 2 different starting parameters, but not all data sets are modellable 


# we can try a self starting function, but the syntax is slightly different
# https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/

# first with exponential function that has no refractory 
# then with asymptotic function that does have refractory component

fun5 <- function(tmp) {
  
  fit = nls(percent_remaining ~ SSexpf(time_days, a, k),
            data = tmp)
  lm <- lm(percent_remaining ~ predict(fit), data = tmp)
  k <- (summary(fit)$coef[2])*-1
  b <- NA
  n = shapiro.test(summary(fit)$residuals)[2]$p.value
  rsquared <- broom::glance(lm)$adj.r.squared
  ss <- broom::glance(lm)$df.residual + broom::glance(lm)$df + 1
  table <-data.frame (name = unique(tmp$name), model = "nls SS", refractory = "no", k = k, b = b, AIC = AIC(fit), BIC = BIC(fit), rsquared = rsquared, normalresids = n, n = ss)
  
  plot(percent_remaining ~ time_days, tmp, pch = 20, ylim = c(0,110))
  lines(fitted(fit) ~ time_days, tmp, col = "red")
  legend("topright", bty = "n", cex = 0.7, legend = capture.output(fit))
  title(main = paste("no refractory SS - AIC:", round(AIC(fit), 2), "R-squared:", round(broom::glance(lm)$adj.r.squared, 5)), sub = unique(tmp$name))

  return(table)
  
}  

fun6 <- function(tmp) {
  
  fit = nls(percent_remaining ~ SSasymp(time_days, b, a, log_k),
            data = tmp)
  lm <- lm(percent_remaining ~ predict(fit), data = tmp)
  k <- exp(summary(fit)$coef[3])
  b <- summary(fit)$coef[1]
  n <- shapiro.test(summary(fit)$residuals)[2]$p.value
  rsquared <- broom::glance(lm)$adj.r.squared
  ss <- broom::glance(lm)$df.residual + broom::glance(lm)$df + 1
  table <-data.frame (name = unique(tmp$name), model = "nls SS", refractory = "yes", k = k, b = b, AIC = AIC(fit), BIC = BIC(fit), rsquared = rsquared, normalresids = n, n = ss)
  
  plot(percent_remaining ~ time_days, tmp, pch = 20, ylim = c(0,110))
  lines(fitted(fit) ~ time_days, tmp, col = "red")
  legend("topright", bty = "n", cex = 0.7, legend = capture.output(fit))
  title(main = paste("with refractory SS - AIC:", round(AIC(fit), 2), "R-squared:", round(broom::glance(lm)$adj.r.squared, 5)), sub = unique(tmp$name))
  
  return(table)
  
} 

