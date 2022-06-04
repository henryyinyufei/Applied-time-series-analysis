library(TSA)

## Example 2.2.5 First Order Moving Average MA(1)
# Simulate MA(1) process with theta = 0.5
set.seed(123)
ma <- arima.sim(model = list(ma = 0.5), n = 50)
plot(ma, type = 'o', ylab = 'data')

# Simulate MA(1) process with theta = 0.9
set.seed(123)
y2 <- arima.sim(model = list(ma = 0.9), n = 50)
plot(y2, type = 'o')

# Simulate MA(1) process with theta = 0.1
set.seed(123)
y2 <- arima.sim(model = list(ma = 0.1), n = 50)
plot(y2, type = 'o')


## Example 2.5.2
### Sample acf for MA(1) with theta = 0.5
acf(ma)



## Example 2.2.6 First Order Autoregressive Process
# Simulate AR(1) process with phi = 0.5
set.seed(56)
y3 <- arima.sim(model = list(ar = 0.5), n = 50, rand.gen = rnorm)
plot(y3, type = 'o')

# Simulate AR(1) process with phi = 0.99
set.seed(123)
y4 <- arima.sim(model = list(ar = 0.99), n = 50, rand.gen = rnorm)
plot(y4, type = 'o')



# Simulate AR(1) process with phi > 1 (non-stationary)
# empty vector for process
set.seed(123)
x <- vector('numeric', 50)
# process errors (we will use gaussian white noise)
z <- rnorm(50)
phi = 1.01
# set first time
x[1] <- z[1]
# simulate AR(1)
for (t in 2:50){
  x[t] <- phi*x[t-1] + z[t]
}
plot(x, type = 'l')


## Example 2.5.3
### Sample acf for AR(1) with phi = 0.5
acf(y3)
### Sample acf for AR(1) with phi = 0.99
acf(y4)



## Example 2.2.9
set.seed(100)
n = 200
ma <- arima.sim(model = list(ma = 0.5), n = n)
plot(ma, type = 'o', ylab = 'data')
w = rnorm(n)
y = vector("numeric", n)
for (t in 1:n){
  y[t] = exp((ma[t])/2)*w[t]
}
plot(y, type = 'o', ylim = c(-10,10))

