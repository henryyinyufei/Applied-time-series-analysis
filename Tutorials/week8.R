# Example 4.2.3.1
# We return to the strike data of Example 4.2.2.2
library(forecast)
strikes = read.table('US-Strikes_data_avg_res.txt')

# We show the values obtained by smoothed averages
# corresponding to alpha = 0.6 and alpha = 0.3

plot(strikes$V1, type = 'o', col = 'green', xlab = 'time', ylab = 'US Strikes')
expsmooth = ses(strikes$V1, initial = 'simple', alpha = 0.6)
lines(c(expsmooth$fitted[2:30], expsmooth$mean[1]), col = 'red')

plot(strikes$V1, type = 'o', col = 'green', xlab = 'time', ylab = 'US Strikes')
expsmooth2 = ses(strikes$V1, initial = 'simple', alpha = 0.3)
lines(c(expsmooth2$fitted[2:30], expsmooth2$mean[1]), col = 'red')

# Finally, we show the residuals, which does not present strong evidence of trend
plot(strikes$V1 - c(expsmooth2$fitted[2:30], expsmooth2$mean[1]),
     col = 'red', type = 'l')
lines(strikes$V1 - c(expsmooth$fitted[2:30], expsmooth$mean[1]),
     col = 'blue', type = 'l')



# Example 4.2.4.1
# We consider the USA population data in Example # 4.2.1.1
# We show the data and the results of first, second, and third order differencing
uspop = read.table('uspopulation.txt')
plot(x = 1:21, uspop$V1, type = 'o', xlab = 't', ylab = 'US Population')
plot(diff(uspop$V1), type = 'o', xlab = 't', ylab = 'US Population')
plot(diff(uspop$V1, differences = 2), type = 'o', xlab = 't', ylab = 'US Population')
plot(diff(uspop$V1, differences = 3), type = 'o', xlab = 't', ylab = 'US Population')
# The data obtained by second and third differencing do not appear a trend






# Example 4.3.1.3
deaths = read.table("US_deaths.txt")
# We plot the data along with the moving average values
plot(deaths$V1, type = 'l', xlab = 't', ylab = 'US deaths')
avgval = stats::filter(deaths$V1, filter = c(1/24, rep(1/12,11),1/24), sides = 2)
lines(avgval, col = 'red')
# The data is seasonal with a maximum in July and a minimum in February but trend is not as obvious

# We plot the residuals of the moving average estimated {mt hat}.
plot(deaths$V1[7:66]-avgval[7:66], type = 'l', col = 'blue', ylab = 'Residuals', xlab = 'time')
# There is a weak hint of trend remaining, which we ignore.

# We plot the estimated seasonal component {st hat}
deathsts = ts(deaths$V1, frequency = 12)
seasoncomp = decompose(deathsts)
plot(seasoncomp$seasonal, type = 'l', col = 'red', 
     ylab = 'Estimated Seasonla Component', xlab = 'Time')

# Next, we plot the deseasonalized data {zt hat}
plot(deaths$V1 - seasoncomp$seasonal, type = 'o', col = 'green',
     ylab = 'Deseasonalized Data', xlab = 'Time')
# There is a clear trend

# We estimated {mt double hat} as quadratic
time = 1:72
time2 = time^2
quadfit = lm(deaths$V1 ~ time + time2)

# We plot the data with the estimated trend and seasonality
plot(deaths$V1, type = 'l', xlab = 't', col = 'green', ylab = 'US deaths')
lines(c(fitted(quadfit)+seasoncomp$seasonal), col = 'red')

# Finally, we plot the estimated noise residual
plot(deaths$V1 - fitted(quadfit) - seasoncomp$seasonal, type = 'l', col = 'green',
     ylab = 'Estimated Noise Residuals', xlab = 'Time')
# There is no apparent trend or seasonality.




# Example 4.3.2.1
# We apply ???12 to the accidental death data of Example 4.3.1.3 to obtain {???12xt, 13 ¡Ü t ¡Ü 72}
plot(x = 13:72, y = diff(deaths$V1, lag = 12), type = 'o', xlab = 't', col = 'green', ylab = 'US Deaths')
# The seasonality is removed but there is still a trend left, which looks linear

# We apply ??? to this data to obtain the data {??????12xt, 13 ¡Ü t ¡Ü 72}
plot(x = 14:72, y = diff(diff(deaths$V1, lag = 12)), type = 'o', xlab = 't', col = 'green', ylab = 'US Deaths')
# The trend and seasonality has apparently been removed.







# Example 4.3.3.1