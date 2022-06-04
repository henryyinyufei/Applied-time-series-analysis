library(TSA)


# Example 2.1.5
## Random Walk
n = 60
# run with a few different seeds
set.seed(126) # initialize the random number so that the simulation can be reproducible
sim.random.walk = ts(cumsum(rnorm(n, 0, 1)), freq = 1, start = 1)
plot(sim.random.walk, type = 'o', ylab = 'Random Walk')

# Simple Random Walk
n = 400
set.seed(123)
simple.random.walk = ts(cumsum(sample(c(-1,1), n, replace = TRUE, prob = c(0.5,0.5))))
plot(simple.random.walk, type = 'o', ylab = 'Simple Random Walk')



## Example 2.2.1
# We show an example with random variables that have the Uniform(-1,1) distribution
n=250
set.seed(123)
unif_ex = ts(runif(n,-1,1), freq = 1, start = 1)
plot(unif_ex, type = 'o', ylab = 'Uniform(-1,1) Example')

# We can see that the series changes step-to-step




## Example 2.2.5 First Order Moving Average MA(1)
# Simulate MA(1) process with theta=0.1
set.seed(123)
y <- arima.sim(model = list(ma = 0.1), n = 400)
plot(y)

# Simulate MA(1) process with theta=10
y2 <- arima.sim(model = list(ma = 10), n = 400)
plot(y2)



## Example 2.2.6 First Order Autoregressive Process
# Simulate AR(1) process with phi=0.1
set.seed(123)
y3 <- arima.sim(model = list(ar = 0.1), n = 400, rand.gen = rnorm)
plot(y3)

# Simulate AR(1) process with phi=0.9
set.seed(123)
y4 <- arima.sim(model = list(ar = 0.9), n = 400, rand.gen = rnorm)
plot(y4)




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





