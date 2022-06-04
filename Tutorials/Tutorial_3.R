# Tutorial 3 D-105
# John Wang 

# MA(1)
MA_1 =  arima.sim(model =  list(order = c(0, 0, 1), ma = 0.5),
                  n = 300, 
                  sd = 1)
plot.ts(MA_1,
        col = "red",
        type = "l",
        main = "MA(1) with theta = 0.2 versus time",
        xlab = "t",
        ylab = expression(X[t]))

points(MA_1,
       pch = 16, col = "lightseagreen",)
# References: 
#   1. http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#   2. http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r

MA_1.acf = acf(MA_1, lag.max = 40, 
               plot = F)
plot(MA_1.acf, main = "")
title( "Sample ACF",line=0.5)

# AR(1)

# phi = 0.5
AR_1 =  arima.sim(n = 300, 
                  model =  list(order = c(1, 0, 0), ar = 0.5), 
                  sd = 1)
plot.ts(AR_1,
        col = "blue",
        main = "AR(1) with phi = 0.5 versus time",
        xlab = "t",
        ylab = expression(X[t]))

AR_2 =  arima.sim(n = 300, 
                  model =  list(order = c(1, 0, 0), ar = 0.99), 
                  sd = 1)

# phi = 0.99
plot.ts(AR_2,
        col = "darkgreen",
        main = "AR(1) with phi = 0.99 versus time",
        xlab = "t",
        ylab = expression(X[t]))
par(mfrow = c(1,2))
acf(AR_1,main = " ",lag = 40)
title( expression(paste(phi, " = 0.5 ")),line=0.5)
acf(AR_2,main = " ",lag = 40)
title( expression(paste(theta, " = 0.99 ")),line=0.5)


