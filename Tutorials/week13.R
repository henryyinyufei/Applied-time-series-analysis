# Example 6.1.2

# We show a 200 point realization of the model with normal noise,
# sigma2 = 1, phi1 = 0.8, and x0 = 0.
set.seed(1)
y <- arima.sim(model = list(order = c(1,1,0), ar = 0.8), n = 200)
plot(y, type = 'o', col = 'green')

# We next plot the sample acf and pacf.
acf(y, lag.max = 40)
# The slowly decreasing acf values, one characteristic of a linear trend, suggests that an ARIMA model might be appropriate.
pacf(y, lag.max = 40)

# We difference the data once to obtain a new set of data. We display the results.
diffy = diff(y, lag = 1)
plot(diffy, type = 'o')
# The data appear to be fairly stationary.

# We next plot the sample acf and pacf for the differenced data
acf(diffy, lag.max = 40)
pacf(diffy, lag.max = 40)
# We see a rapid decay in the acf values.
# These plots suggest and AR(1) model for {Yt} = {delXt}.

# The estimates phihat1 and sigmahat2 computed are
ywest = ar.yw(diffy, order = 1)
mle = arima(diffy, order = c(1,0,0), method = 'ML')
mle

# We plot the sample and model acf/pacf together.
acf(diffy, lag.max = 40, col = 'green')
lines(x = 0:40, ARMAacf(ar = 0.7949, lag.max = 40), type = 'h', col = 'red')

pacf(diffy, lag.max = 40, col = 'green')
lines(ARMAacf(ar = 0.7949, lag.max = 40, pacf = TRUE), type = 'h', col = 'red')
# The model acf values match the sample acf values for lags 1, 2 fairly well but do not match after that.
# The model pacf value matches the significant sample pacf value at lag 1 fairly well.

# We analyze the modle residuals, beginning with a plot of the standardized residuals.
library(TSA)
plot(rstandard(mle), type = 'o')
# The standardized residuals appear stationary

# Next we plot the sample acf and pacf for the standardized model residuals.
sresid = rstandard(mle)
acf(sresid, lag.max = 40)
pacf(sresid, lag.max = 40)
# There are 1-2 sample acf values that are just over the bounds for significance.
# This does not support rejection of the iid noise hypothesis.

# We run statistical tests on the model residuals.
library(itsmr)
test(sresid)
# This does not support rejection of the iid hypothesis

library(forecast)
auto.arima(diffy)

# We conclude by considering what happens if we try to model without first differencing the data 
# From the pacf values above, we might try an AR(2) model 
# MLE estimate:
mle2 = stats::arima(y, order = c(2,0,0), method = 'ML')
mle2

# We plot the sample and model acf/pacf together
acf(y, lag.max = 40, col = 'green')
lines(x = 0:40, ARMAacf(ar = c(1.7930,-0.8008), lag.max = 40), type = 'h', col = 'red')

pacf(y, lag.max = 40, col = 'green')
lines(ARMAacf(ar = c(1.7930,-0.8008), lag.max = 40, pacf = TRUE), type = 'h', col = 'red')
# These results do not indicate a reasonable model fit.



# Example 6.3.2
# We analyze data about sedimentary deposits, called varves, measuring thickness of the sediments at a given
#location for 634 years beginning over 11,000 years ago.
varve = read.table("varve.dat")

# It is known that the thickness increases in proportion to the amount deposited, which suggests that a long transformation
#might be useful.
# We plot the original data
plot(varve$V1, type = 'o', col = 'green')
which.max(varve$V1)
varve = varve[-70,]
plot(varve, type = 'o', col = 'green')

# We plot the sample acf/pacf.
acf(varve, lag.max = 40)
pacf(varve, lag.max = 40)
# The data suggest that the variance is increasing.
# The sample acf indicates some kind of trend behaviour

# We plot the log of the data.
plot(log(varve), type = 'o', col = 'green')
# There may be a hint of a trend.

# We plot the sample acf/pacf
acf(log(varve), lag.max = 40)
pacf(log(varve), lag.max = 40)
# It appears there is a slow decay in the acf values, which could reflect a trend

# We consider differencing. We plot the results of a single difference
plot(diff(log(varve), lag = 1), type = 'o', col = 'green')
# The variance seems stable

# We plot the sample acf and pacf
acf(diff(log(varve), lag = 1), lag.max = 40)
pacf(diff(log(varve), lag = 1), lag.max = 40)
# The sample acf suggests a cutoff after lag 1 while 
#the pacf suggests exponential decay

# This suggests using a MA(1) model for {del log(Xt)}.
# In other words, we are considering an ARIMA(0, 1, 1) model for {log(Xt)}
# Using the innovations algorithm, we obtain the model:
difflogvarve = diff(log(varve), lag = 1)
est = ia(difflogvarve, q = 1)
est

# We plot the modle and sample acf/pacf.
acf(difflogvarve, lag.max = 40, col = 'green')
lines(x= 0:40, ARMAacf(ma = -0.654078, lag.max = 40), type = 'h', col = 'red')

pacf(difflogvarve, lag.max = 40, col = 'green', ylim = c(-1,1))
lines(ARMAacf(ma = -0.654078, lag.max = 40, pacf = TRUE), type = 'h', col = 'red')


# The leading significant values of the acf and pacf are in reasonable agreement.
# The significant pacf values for lags larger than 2 do not match as well.

# We plot the standardized residuals.
sresids = Resid(difflogvarve, a = est)
plot(sresids, type = 'o', col = 'blue')
# Those appear stationary.

# We plot the acf/pacf values for the model residuals.
acf(sresids, lag.max = 40)
pacf(sresids, lag.max = 40)
# There are too many values that exceed the bound to support the hypothesis that the model residuals behave like iid noise.

test(sresids)
# In balance, we reject the hypothesis that the model residuals behave like iid noise.

# We try an ARIMA(1, 1, 1) model. The MLE is 
est111 = arima(difflogvarve, order = c(1, 0 ,1), method = 'ML') # diff already so order = c(1,0,1)
est111

# We plot hte model and sample acf/pacf
acf(difflogvarve, lag.max = 40, col = 'green')
lines(x = 0:40, ARMAacf(ar = 0.2341, ma = -0.8871, lag.max = 40), type = 'h', col = 'red')

pacf(difflogvarve, lag.max = 40, col = 'green', ylim = c(-1,1))
lines(ARMAacf(ar = 0.2341, ma = -0.8871, lag.max = 40, pacf = TRUE), type = 'h', col = 'red')
# We see a very good match in the significant values.

# We plot the acf/pacf values for the model residuals
acf(est111$residuals, lag.max = 40)
pacf(est111$residuals, lag.max = 40)
# These values suggest that the model residuals behave like iid noise

test(est111$residuals)
# We conclude that the model residuals are behaving like iid noise

# Overall, we conclude that the ARIMA(1, 1, 1) model is a good fit for the {log(Xt)}.