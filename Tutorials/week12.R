# Example 5.5.5.2
# We plot the Dow Jones Utilities Index data along with the sample acf 
index = read.table('DJUtilIndex.txt')
plot(index$V1, type = 'o')
acf(index$V1, lag.max = 40)
# The data and the slowly decaying sample suggests there is a lienar trend.

# We apply a single differences with lag 1 to remove a linear trend
index1 = diff(index$V1, lag = 1)
# We plot the differenced data, which appears fairly stationary, along with the sample acf and pacf
plot(index1, type = 'o')
acf(index1, lag.max = 40)
pacf(index1, lag.max = 40)
# Considering the pacf suggests an AR(1) model.

# After subtracting the sample mean. 0.1336, the Yule-Walker estimate is 
index1 = index1 - mean(index1)
yuleest = ar.yw(index1, order = 1)
yuleest

# The Burg's Algorithm estimate is 
burgest = ar.burg(index1, order = 1)
burgest

# The MLE is
ar1fit = arima(index1, order = c(1,0,0), method = 'ML')
ar1fit$loglik

# We plot the sample acf and pacf(green) and the model acf/pacf for the fitted model.
acf(index1, lag.max = 40, col='green')
lines(x = 0:40, ARMAacf(ar = 0.4479, lag.max = 40), type = 'h', col = 'red')

pacf(index1, lag.max = 40, col='green')
lines(ARMAacf(ar = 0.4479, lag.max = 40, pacf = TRUE), type = 'h', col = 'red')

# We obtain the MLE for a MA(2) model
ma2fit = arima(index1, order = c(0,0,2), method = 'ML')
ma2fit$loglik

# We plot the sample acf and pacf(green) and the model acf/pacf for the fitted model.
acf(index1, lag.max = 40, col='green')
lines(x = 0:40, ARMAacf(ma = c(0.4112, 0.2079), lag.max = 40), type = 'h', col = 'red')

pacf(index1, lag.max = 40, col='green')
lines(ARMAacf(ma = c(0.4112, 0.2079), lag.max = 40, pacf = TRUE), type = 'h', col = 'red')




# Example 5.5.5.3
# We consider the Lake Huron data
lake = read.table('lake.txt')
# We plot the sample acf and pacf 
acf(lake$V1, lag.max = 40)
pacf(lake$V1, lag.max = 40)
# If we consider the sample acf values as trailing off (suggesting a linear trend) and 
#the sample pacf values for lag 1 and 2 as significant, then we could choose an AR(2) model.

# The Yule-Walker estimate is 
yuleest = ar.yw(lake, order = 2)
yuleest

# The Burg's Algorithm estimate is 
burgest = ar.burg(lake, order = 2)
burgest

# The MLE is 
ar2fit = arima(lake$V1, order = c(2,0,0), method = 'ML')
ar2fit$loglik

# We plot the sample acf and pacf (green) and the model acf/pacf for the fitted model.
acf(lake, lag.max = 40, col = 'green')
lines(x = 0:40, ARMAacf(ar = c(1.0436, -0.2495), lag.max = 40), type = 'h', col = 'red')

pacf(lake, lag.max = 40, col = 'green')
lines(ARMAacf(ar = c(1.0436, -0.2495), lag.max = 40, pacf = TRUE), type = 'h', col = 'red')


# The MLE for the ARMA(1,1) model is 
armafit = arima(lake$V1, order = c(1,0,1), method = 'ML')
armafit$loglik

# We plot the sample acf and pacf(green) and the model acf/pacf for the fitted model.
acf(lake$V1, lag.max = 40, col ='green')
lines(x = 0:40, ARMAacf(ar = 0.7449, ma = 0.3206, lag.max = 40), type = 'h', col = 'red')

pacf(lake$V1, lag.max = 40, col = 'green')
lines(ARMAacf(ar = 0.7449, ma = 0.3206, lag.max = 40, pacf = TRUE), type = 'h', col = 'red')
