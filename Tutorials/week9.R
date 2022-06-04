deaths = read.table('US_deaths.txt')

# Example 4.3.3.1: Regression with trigonometric polynomials with and without
# We show the results of fitting a trigonometric polynomials with two frequencies to the accidental death data 
# of Example 4.3.1.3
# We choose f1 = n/12 = 6 and f2 = n/6 = 12
# This is experimental: We might try one and three frequencies or more
# We plot the "pure" trigonometric estimate that ignores trend.
plot(deaths$V1, type = 'l', xlab = 't',
     col = 'green', ylab = 'US deaths')
f1 = 6
f2 = 12
t = 1:72
trigest = lm(deaths$V1 ~ cos(2*pi*t/f2) + sin(2*pi*t/f2)
             + cos(2*pi*t/f1) + sin(2*pi*t/f1), data = deaths)
lines(fitted(trigest), col = 'red')
# We capture the periodic behavior but miss the trend.

# The residuals also show this
plot(deaths$V1 - fitted(trigest), type = 'o', col = 'green')

# Next, we use regression on a linear combination of a polynomial and a trigonometric polynomial
t2 = t^2
combest = lm(deaths$V1 ~ cos(2*pi*t/f2) + sin(2*pi*t/f2)
             + cos(2*pi*t/f1) + sin(2*pi*t/f1) + t + t2,
             data = deaths)
plot(deaths$V1, type = 'l', xlab = 't',
     col = 'green', ylab = 'US deaths')
lines(fitted(combest), col = 'red')

# residual plot
plot(deaths$V1 - fitted(combest), type = 'o', col = 'green')
plot(resid(combest), type = 'o', col = 'green')

# we show the sample acf for regression
# omitting and including additional trend model.
acf(deaths$V1 - fitted(trigest), lag.max = 40)
acf(deaths$V1 - fitted(combest), lag.max = 40)
# including a trend term definitely affects the sample acf.
# The plots do not support the hypothesis of iid residuals
# Judgment is required in these evaluations.

library(itsmr)
test(deaths$V1 - fitted(combest))

# Example 4.4.2.3
# We consider the accidental deaths data in Example 4.3.1.3
# Recall that we tried different approaches:
################
# Example 4.3.1.3: Special 12 months moving average
# with and without additional trend estimation
# We plot the estimated seasonal component {St hat}.
deathsts = ts(deaths$V1, frequency = 12)
seasoncomp = decompose(deathsts)

# Next, we plot the deseasonalized data {Zt hat}.
plot(deaths$V1 - seasoncomp$seasonal, type = 'o', col = 'green',
     ylab = 'Deseasonalized Data', xlab = 'Time')
# There is a clear trend


## CHANGE HERE
# We estimated {mt doulbe hat} as quadratic.
time = 1:72
time2 = time^2
quadfit = lm(deaths$V1 - seasoncomp$seasonal ~ time + time2)

# Finally, we plot the estimated noise residual.
plot(deaths$V1 - fitted(quadfit) - seasoncomp$seasonal, type = 'o', col = 'green', 
     ylab = 'Estimated Noise Residuals', xlab = 'Time')
# There is no apparent trend or seasonality


# We plot the sample acf for both approaches
acf(deaths$V1 - seasoncomp$seasonal, lag.max = 40)
acf(deaths$V1 - fitted(quadfit) - seasoncomp$seasonal, lag.max = 40)
# Eliminating both seasonality and trend has an obvious effect on the correlogram
# The hypothesis of iid residuals is not supported

test(deaths$V1 - fitted(quadfit) - seasoncomp$seasonal)


# Example 4.3.2.1:     12 month difference then adding
# an additional difference
# We apply del 12 to the accidental death data of Example 4.3.1.3 to obtain {del12Xt, 13 <= t <= 72}
plot(x = 13:72, y = diff(deaths$V1, lag = 12), type = 'o', xlab = 't',
     col = 'green', ylab = 'US Deaths')
# The seasonality is removed but there is still a trend left, which looks linear

# We apply del to this data to obtain the data {deldel12Xt, 13 <= t <= 72}.
plot(x = 14:72, y = diff(diff(deaths$V1, lag = 12)), type = 'o', xlab = 't',
     col = 'green', ylab = 'US Deaths')
# The trend and seasonality has apparently been removed.

# We show the effect of including the additional differencing to remove a linear trend
acf(diff(deaths$V1, lag = 12), lag.max = 40)
acf(diff(diff(deaths$V1, lag = 12)), lag.max = 40)
# Based on this, for the latter, the sample acf supports the hypothesis of iid residuals.

test(diff(diff(deaths$V1, lag = 12)))
# We find QLB = 34.95 with p-value .02 and we reject the iid hypothesis at level .05
# We find QML = 18.5 with p-value .557, so this does not support rejection of the hypothesis 

# Example 4.4.5.1
# We consider a model of "signal + noise" by simulating the series 
# Xt = 1/2 cos(t/10) + Zt, Zt ~ WN(0, 1). We plot the data and the sample acf 
set.seed(100)
n = 200
z = rnorm(n)
x = vector("numeric", n)
for (t in 1:n){
  x[t] = 0.5*cos(t/10)+z[t]
}
plot(x, type = 'o')
acf(x, lag.max = 40)
# The correlogram shows about 20% of the values exceed +- 19.6/sqrt(200)
# and we reject the iid hypothesis

Box.test(x, type = 'Ljung-Box', lag = 20)


library(TSA)
ml = McLeod.Li.test(y = x, gof.lag = 20, plot = FALSE)
ml$p.values[20]

library(itsmr)
test(x)




