# Example 3.3.2 but with AR(1), phi=0.6, n=100
library(forecast)
library(TSA)
set.seed(123)
y <- arima.sim(model = list(ar = 0.6), n = 100,
               rand.gen = rnorm)
plot(y, type = 'o', col = 'red', xlim = c(0,120))

est = arima(y, order = c(1,0,0))
est$coef

fc = forecast(y, model=est)
lines(fc$mean, type = 'o')


# alternative option
plot(est, n.ahead = 20, type = 'o')


## Example 4.2.1.1
# Consider the population of the USA shown in Example 1.1.1
uspop = read.table('uspopulation.txt')
plot(x=seq(from = 1790, to = 1990, by = 10), uspop$V1, type='o', xlab ='t',
     ylab = 'US Population')

plot(x=1:21, uspop$V1, type='o', xlab ='t',
     ylab = 'US Population')

# We attempt to model the visible nonlinear trend as a quadratic 
time = 1:21
time2 = time^2
quadfit = lm(uspop$V1 ~ time + time2)

# We plot the fit with the original data.
lines(seq(from = 1790, to = 1990, by = 10), fitted(quadfit), col = 'red')

# We plot the residuals 
plot(x=seq(from = 1790, to = 1990, by = 10), resid(quadfit), type='o',
     xlab = 't', ylab = 'Residuals')
# At least much of the trend appears to have been eliminated
# There is no clear pattern or structure in the residual values.
# Visually, the residuals are not consistent with iid noise.
# The effect of the anomaly in the years 1940-1960 is also visible.


# Example 4.2.1.2
globtemp = read.table('globaltemperature.txt')

# We plot the data
plot(globtemp$V1, type='o', col='green', ylab = 'Global Temperature', xlab = 't')
# The plot does not indicaate that a linear model is suitable

# But, we compute a least squares line fit for a linear trend anyway
time = 1:142
fit = lm(globtemp$V1 ~ time)
summary(fit)

# This is simple model does not capture the trend in the data.
lines(time, fitted(fit), col = 'red')
# There may be some trend remaining, perhaps quadratic

# We plot the residuals.
plot(resid(fit), type='o', col = 'green')
# There may be some trend remaining, perhaps quadratic



## Example 4.2.2.2
# We consider data for the number of strikes in US from 1951 to 1980
strikes = read.table('US-Strikes_data_avg_res.txt')

# We plot the data and the averaged values for q = 2
plot(strikes$V1, type = 'o', col = 'green', xlab = 'time', ylab = 'US Strikes')
avgval = stats::filter(strikes$V1, filter = rep(1/5,5), sides = 2)
lines(avgval, col='red')

# We plot the corresponding residuals, which do not
# have an apparent trend though the variance seems to vary with time 
plot(strikes$V1[3:28]-avgval[3:28], type = 'l', col='blue', ylab='Residuals', xlab = 'Time')
# The model fits to different degrees in different time regions.


## Example 4.2.3.1



