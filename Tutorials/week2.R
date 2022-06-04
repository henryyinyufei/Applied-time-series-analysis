#### Example 1.1.1
# We plot the U.S.A. population at ten year intervals from 1790-1990.

uspop = read.table('uspopulation.txt')
plot(x=seq(from = 1790, to = 1990, by = 10), y=uspop$V1, col='red', ylab = 'U.S. Population', xlab = 'time',
     main = "U.S.A. population at ten year intervals from 1790-1990")
# title("U.S.A. population at ten year intervals from 1790-1990")
lines(x=seq(from = 1790, to = 1990, by = 10), y=uspop$V1, col = 'green')


plot(x=seq(from = 1790, to = 1990, by = 10), y=uspop$V1, col='red', ylab = 'U.S. Population', xlab = 'time',
     main = "U.S.A. population at ten year intervals from 1790-1990", type = 'b')

# There is a definite upward trend.
# There is a slight change in shape/structure around the time of the baby boom.
# The behavior is nonlinear, maybe the curve is polynomial or exponential in shape.
# A quadratic regression fits well. This also gives good predictions of future behavior. 


#### Example 1.1.2
# We show monthly totals of international airline passengers from 1949-1961

airpassenger = read.table('airpassenger.txt')
plot(airpassenger$V1, col = 'red', ylab = 'Monthly Totals of International Airline passengers', xlab = 't')
lines(airpassenger$V1, col = 'green')

# We observe a strong upward trend, seasonality on a 12 month interval, and increasing variablity.


#### Example 1.1.4
# We show quarterly Austrian industrial production

indprod = read.table('austrianindprod.txt')
plot(indprod$V1, ylab = 'Industiral Production', xlab = 'Quarter')
lines(indprod$V1)

# Here is an example of an strong upward trend with seasonality, but there is a 'structural break' late in the data.
# It is not as dramatic as in the previous example but it still complicates making predictions about future behavior. 




