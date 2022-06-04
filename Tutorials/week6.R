# 2.5.4

wine = read.table('wine.txt')
plot(wine$V1, type = 'o', ylab = "Sale of Red Wine", xlab = 't')

acf(wine$V1, lag.max = 40)

# The time series has an upward trend and a seasonal pattern with peak values in July and low values in January
# The variance increases with time sa well.
# We treat the data as a realization of a stationary


# Time series model and comupte the sample acf
wine = read.table('wine.txt')
plot(wine$V1, type = 'o', ylab = "Sale of Red Wine", xlab = 't')

acf(Wine$V1, lag.max = 40)


#### 3.3.1

# We also show the square differences.

# sd 1
set.seed(123)
y <- arima.sim(model = list(ar = 0.5), n = 100,
               rand.gen = rnorm)
plot(y, type = 'o', ylim = c(-3,3), col = 'red')
predy = vector("numeric", 100)
predy[1] = NA

est = arima(y, order = c(1,0,0))
est$coef
# ar1 (phi = 0.5)
# intercept (mean = 0)

for(t in 2:100){
  predy[t] = est$coef[1]*y[t-1]
}
lines(predy, type = 'o', col = 'blue')
# square differences
sqdiff = vector("numeric", 100)
for(t in 2:100){
  sqdiff[t] = (predy[t]- y[t])^2
}
plot(sqdiff, type = 'l', col = 'green', ylim = c(0,7))
# Estimated MSE
mean(sqdiff[2:100])


# sd 0.1
set.seed(123)
y <- arima.sim(model = list(ar = 0.5), n = 100,
               rand.gen = rnorm, sd = 0.1)
plot(y, type = 'o', ylim = c(-0.4,0.4), col = 'red')
predy = vector("numeric", 100)
predy[1] = NA

est = arima(y, order = c(1,0,0))
est$coef

for(t in 2:100){
  predy[t] = est$coef[1]*y[t-1]
}
lines(predy, type = 'o', col = 'blue')
# square differences
sqdiff = vector("numeric", 100)
for(t in 2:100){
  sqdiff[t] = (predy[t]- y[t])^2
}
plot(sqdiff, type = 'l', col = 'green')
# Estimated MSE
mean(sqdiff[2:100])



#### 3.4.2
# Variance = 1
set.seed(1)
y <- arima.sim(model = list(ar = 0.5), n = 100,
               rand.gen = rnorm)
plot(y, type = 'o', col = 'red')
predy = vector("numeric", 100)
predy[1] = NA

est = arima(y, order = c(1,0,0))
est$coef

for(t in 2:99){
  predy[t] = (est$coef[1]/(1+est$coef[1]^2))*y[t-1]+(est$coef[1]/(1+est$coef[1]^2))*y[t+1]
}
lines(predy, type = 'o', col = 'blue')
# square differences
sqdiff = vector("numeric", 99)
for(t in 2:99){
  sqdiff[t] = (predy[t]- y[t])^2
}
plot(sqdiff, type = 'l', col = 'green')
# Estimated MSE
mean(sqdiff[2:99])


# variance = 0.1
set.seed(1)
y <- arima.sim(model = list(ar = 0.5), n = 100,
               rand.gen = rnorm, sd = sqrt(0.1))
plot(y, type = 'o', col = 'red')
predy = vector("numeric", 100)
predy[1] = NA

est = arima(y, order = c(1,0,0))
est$coef

for(t in 2:99){
  predy[t] = (est$coef[1]/(1+est$coef[1]^2))*y[t-1]+(est$coef[1]/(1+est$coef[1]^2))*y[t+1]
}
lines(predy, type = 'o', col = 'blue')
# square differences
sqdiff = vector("numeric", 99)
for(t in 2:99){
  sqdiff[t] = (predy[t]- y[t])^2
}
plot(sqdiff, type = 'l', col = 'green')
# Estimated MSE
mean(sqdiff[2:99])