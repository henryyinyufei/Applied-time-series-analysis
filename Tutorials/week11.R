# Example 5.2.1
# We consider the ARMA(2 ,1) model (5.1.1) with phi1 = 0.84, phi2 = -0.34, theta1 = 0.2, and sigma2 = 1
# we plot a simulation
set.seed(123)
y <- arima.sim(model = list(ar = c(0.84, -0.34),
                            ma = 0.2),
               n = 150)
plot(y, type = 'o', col = 'green')

# We plot the sample acf, model acf for the true parameters, and the model acf for the estimated parameters.
acf(y, lag.max = 40)
plot(x=1:40, ARMAacf(ar = c(0.84, -0.34),
                     ma = 0.2, lag.max = 40)[2:41], 
     type = 'h')
library(itsmr)
arma(y, p = 2, q = 1)
plot(ARMAacf(ar = c(0.407514555, -0.005697621),
             ma = 0.5875028, lag.max = 40)[2:41],
     type = 'h')




# Example 5.3.4
# We pot the model acf and pacf for the AR(3) time series with phi1 = 0.8, phi2 = -0.3 and phi3 = 0.08
plot(x=1:40, ARMAacf(ar = c(0.8, -0.3, 0.08), lag.max = 40)[2:41], type = 'h')
plot(ARMAacf(ar = c(0.8, -0.3, 0.08), lag.max = 40, pacf = TRUE), type = 'h')






# Example 5.3.6
# We plot the model acf and pacf for the ARMA(2,2) time series with 
#phi1 = 0.8, phi2 = -0.6, theta1 = -0.6, and theta2 = 0.2
plot(x = 1:40, ARMAacf(ar = c(0.8, -0.6),
                       ma = c(-0.6, 0.2), lag.max = 40)[2:41],
     type = 'h')
plot(x = 1:40, ARMAacf(ar = c(0.8, -0.6),
                       ma = c(-0.6, 0.2), lag.max = 40, pacf = TRUE),
     type = 'h')





# Example 5.3.7
# We consider data showing the number of fish in a region in the ocean for 453 months from 1950-1987
fish = read.table("recruit.txt")

# We plot the data after substracting the mean
removemean = fish$V1 - mean(fish$V1)
plot(removemean, type = 'o', col = 'green')

# We plot the sample acf and pacf versus lag next.
acf(removemean, lag.max = 40, col = 'green')
pacf(removemean, lag.max = 40, col = 'green')
# These suggest that an AR(2) model might be reasonable.
# The sample acf has behaviour consistent with periodic behaviour with period 12 months
# The sample pacf has significant values for h = 1 and h = 2 and insiginificant values for larger lags.

# To fit an AR(2) model, we use regression on triplets
tripfit = lm(fish$V1[3:453] ~ fish$V1[2:452] + fish$V1[1:451])
summary(tripfit)
(summary(tripfit)$sigma)^2

# First we compare the sample acf and pacf to the model acf and pacf for the AR(2) time series with the fitted coefficient
acf(removemean, lag.max = 40, col = 'green')
lines(ARMAacf(ar = c(1.35, -0.46), lag.max = 40)[2:41], 
      type = 'h', col = 'red')

pacf(removemean, lag.max = 40, col = 'green')
lines(ARMAacf(ar = c(1.35, -0.46), lag.max = 40, pacf = TRUE), 
      type = 'h', col = 'red')

# We see good agreement.

# Next, we consider the "scaled" residuals
zt = rstandard(tripfit)
plot(zt, type = 'o', col = 'blue')

# We plot the rescaled residuals and their sample acf/pacf.
acf(zt, lag.max = 40)
pacf(zt, lag.max = 40)
# The plots of the sample acf and pacf have some of the characteristics of white noise, 
#but both exhibit too many significant values
test(zt)





# Example 5.3.8
# We consider data on annual sunspot numbers, 1770-1869.
sunspots = read.table('sunspots.txt')
# We plot the data.
plot(sunspots$V1, type = 'o', col = 'green')
# We plot the sample acf/pacf.
acf(sunspots$V1, lag.max = 40)
pacf(sunspots$V1, lag.max = 40)
# All of the sample pacf values past lag 2 fall within the significance bounds, so an AR(2) model is suggested
#for the mean corrected data.
# The sample acf values are also consistent with such a model.
acvf(sunspots$V1)

# To estimate phi1, phi2 and sigma2 in the AR(2) model, we match sample acvf values at lags 0, 1, and 2 with the 
#model values
yuleest = ar.yw(sunspots$V1, order = 2)
yuleest
# This approach to model fitting is called Yule-Walker estimation.

rt = mean((yuleest$resid[3:100])^2)/298.2
zt = yuleest$resid[3:100]/sqrt(rt)
  
# We plot the sample acf/pacf of the scaled residuals.
# The plots of the sample acf and pacf support the iid noise hypothesis.
acf(zt, lag.max = 40)
pacf(zt, lag.max = 40)
test(zt)





