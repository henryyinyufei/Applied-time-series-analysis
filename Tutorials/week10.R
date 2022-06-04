# Example 4.3.3.1

deaths = read.table("US_deaths.txt")

plot(deaths$V1, type = 'l', xlab = 't',
    col = 'green', ylab = 'US deaths')
f1 = 6
f2 = 12
t = 1:72
trigest = lm(deaths$V1 ~ cos(2*pi*t/f2) + sin(2*pi*t/f2)
             + cos(2*pi*t/f1) + sin(2*pi*t/f1), data = deaths)
lines(fitted(trigest), col = 'red')

plot(deaths$V1 - fitted(trigest), type = 'o', col = 'green')


t2 = t^2
combest = lm(deaths$V1 ~ cos(2*pi*t/f2) + sin(2*pi*t/f2)
             + cos(2*pi*t/f1) + sin(2*pi*t/f1) + t + t2,
             data = deaths)
plot(deaths$V1, type = 'l', xlab = 't',
     col = 'green', ylab = 'US deaths')
lines(fitted(combest), col = 'red')


plot(deaths$V1 - fitted(combest), type = 'o', col = 'green')
plot(resid(combest), type = 'o', col = 'green')


acf(deaths$V1 - fitted(trigest), lag.max = 40)
acf(deaths$V1 - fitted(combest), lag.max = 40)

####
library(itsmr)
test(deaths$V1 - fitted(combest))


# Example 4.3.1.3
deathsts = ts(deaths$V1, frequency = 12)
seasoncomp = decompose(deathsts)


plot(deaths$V1 - seasoncomp$seasonal, type = 'o', col = 'green',
     ylab = 'Deseasonalized Data', xlab = 'Time')

time = 1:72
time2 = time^2
quadfit = lm(deaths$V1 - seasoncomp$seasonal ~ time + time2)

plot(deaths$V1 - fitted(quadfit) - seasoncomp$seasonal, type = 'o', col = 'green', 
     ylab = 'Estimated Noise Residuals', xlab = 'Time')

acf(deaths$V1 - seasoncomp$seasonal, lag.max = 40)
acf(deaths$V1 - fitted(quadfit) - seasoncomp$seasonal, lag.max = 40)

####
test(deaths$V1 - fitted(quadfit) - seasoncomp$seasonal)


# Example 4.3.2.1

plot(x = 13:72, y = diff(deaths$V1, lag = 12), type = 'o', xlab = 't',
     col = 'green', ylab = 'US Deaths')

plot(x = 14:72, y = diff(diff(deaths$V1, lag = 12)), type = 'o', xlab = 't',
     col = 'green', ylab = 'US Deaths')

acf(diff(deaths$V1, lag = 12), lag.max = 40)
acf(diff(diff(deaths$V1, lag = 12)), lag.max = 40)

####
test(diff(diff(deaths$V1, lag = 12)))


# Example 4.5.5.3
# We consider the level of Lake Huron (shifted downwards by 570) from 1875-1972.
lake = read.table('lake.txt')
plot(lake$V1, type = 'o', col = 'green')
# There seems to be a mild downward linear trend.
# We assume a linear trend model mt = a0 + a1t.

time = 1:98
linfit = lm(lake$V1 ~ time)
summary(linfit)
# Using least squares, we obtain ahat0 = 10.202037 and ahat1 = -0.024201.

# We plot the data with the fit
lines(fitted(linfit), col = 'red')

# Next we plot the residuals {yhatt} after removing the estimated trend
plot(resid(linfit), type = 'o', col = 'blue')
# We analyze the residuals.
# First we note there are long stretches when the
# residuals have the same sign, which is not likely to
# occur if the residuals are iid noise.

# We plot the sample acf.
acf(resid(linfit), lag.max = 40)
# The values for lags 1, 2, and 3 are all outside the bounds.

test(resid(linfit))
# We find that QLB = 107.83 with p-value -10^-6 and we
# reject the iid hypothesis at level 0.05.
# QML = 68.7 with p-value -10^-6 and we reject the idd
# hypothesis at level 0.05.
# These tests do not support the hypothesis that {yhatt} is iid.

# We consider the 97 data pairs {yhat1, yhat2}, ... {yhat97 yhat98}.
resid = lake$V1 - fitted(linfit)
plot(x = resid[1:97], y = resid[2:98], col = 'blue')
# The graph is suggestive of a linear relationship.

# We compute a linear least squares estimate for yhatt = yhatt - 1
xt = resid[1:97]
linfit2 = lm(resid[2:98] ~ xt)
summary(linfit2)
# This yields the model Yt = 0.791Yt-1 + Zt where {Zt} is iid noise with variance 0.5024

abline(linfit2, col = 'red')
mean((resid[2:98] - 0.79112 * resid[1:97])^2) # 0.5024

# We plot the estimated noise sequence zhatt = yhatt - 0.791 yhatt-1
plot(resid[2:98]-0.79112*resid[1:97], type = 'o', col = 'green')

# We plot the sample acf
acf(resid[2:98]-0.79112*resid[1:97], lag.max = 40)
# The sample acf values are inside the bounds except lag 1, which is consistent with iid noise.


# Example 5.1.1
# We show a realization of ARMA(3,2) process 
# with phi1 = 0.8, phi2 = -0.4, phi3 = 0.2, theta1 = 0.5, theta2 = 0.3, and 
# iid noise standard normal noise {Zt}.
set.seed(123)
y <- arima.sim(model = list(ar = c(0.8,-0.4,0.2), 
                            ma = c(0.5, 0.3)),
               n = 200)
plot(y, type = 'o', col = 'green', main = 'ARMA(3,2)')

# We also plot of the sample acf versus lag.
acf(y, lag.max = 40)
