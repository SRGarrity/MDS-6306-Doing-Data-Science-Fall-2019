library(fpp)

data(ausair)
class(ausair) # it is a timeseries

air <- window(ausair, start=1990, end=2004)
plot(air, ylab="Airline Passengers", xlab="Year")
fit1 <- ses(air, alpha=0.2, initial="simple", h=3)
fit2 <- ses(air, alpha=0.6, initial="simple", h=3)
fit3 <- ses(air, h=3)
plot(air, ylab="Airline Passengers", main="Simple Exponential Smooth", xlim=c(1990, 2008))
lines(fitted(fit1), col="blue", type="o", pch=16)
lines(fitted(fit2), col="red", type="o", pch=16)
lines(fitted(fit3), col="green", type="o", pch=16)
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft", lty=1, col=c("blue","red","green"), 
       c(expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)), pch=1)



library(tswge)
data(airline)

plot(airline, type="l")
# seasonal data, and probably multiplicative...try a HW multiplicative model:

air = ts(airline, start = c(1949,1), frequency=12) # create time series object
air2 = window(air, start=1949, end=1956) # just use 1949-1956 to train model
fit1 = hw(air2,seasonal = "multiplicative",h = 40) # fit model
plot(air2, ylab="Airline Passengers", xlab="Year", xlim=c(1949, 1960), ylim=c(100,500), type="o", pch=16) # plot observations
lines(fitted(fit1), col="red", type="l") # plot model fit
lines(fit1$mean, col="red", type="l") # plot forecast

legend("topleft", lty=1, col=c("black","red"), 
       c("observed", "forecast"), pch=1)









#3. Seasonal Trend  (Activty 3)
#Load the data
data("austourists")
?austourists

plot(austourists)

aust = window(austourists,start = 1999, end = 2004)((#fit an additive and multiplicative model(fit1s = hw(aust,seasonal = "additive",h = 40)(fit2s = hw(aust,seasonal = "multiplicative",h = 40)((#Plot the original data(plot(aust,ylab = "Australian Tourists", xlab = "Year", type = "o", xlim = c(1999, 2014),ylim = c(15,60))(#add the fitted values from the model (of the training data)(lines(fitted(fit1s),col = "blue", type= "o")(lines(fitted(fit2s), col = "red", type= "o")((#Now add the forecasts (add these one at a time)(lines(fit1s$mean, col = "blue", type= "o")(lines(fit2s$mean,col = "red", type= "o")((#Compare the accuracy(accuracy(fit1s,austourists)(accuracy(fit2s,austourists)((#add the actual values to visually compare the forecasts to the actual values. (points(austourists, type = "o"))