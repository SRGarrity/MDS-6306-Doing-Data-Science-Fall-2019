library("fpp2")
library("dygraphs")
library("xts")

# Number 1: In this exercise, we will use Temperature data
autoplot(maxtemp, xlab="Year", ylab="Temperature (Celsius)")
ts<-window(maxtemp, start=1990)

# Use SES
fit<-ses(ts, h=5)

# Comparing Forecast Fit
plot(fit,ylab="Temperature, Celsius", xlab= "Year", main="Comparing forecast fit")
lines(fitted(fit), col="blue")
lines(fit$mean, col="blue", type="o")

# Finding AICc
fit$model

# Use Holt Fit and damped
holtfit<- holt(ts, initial="optimal", h=5, damped = TRUE)

plot(holtfit, ylab="Temperature, Celsius", xlab= "Year", main="Comparing forecast fit")
lines(fitted(holtfit), col="blue", type="o")
lines(holtfit$mean, col="red")

# Finding AICc
holtfit$model

# The one with the Lowest AICc is SES

# Number 2: Harry Potter
df<-read.csv("/Users/stevengarrity/SMU_MSDS/DS6306_DoingDataScience/DDS_Git/Unit 11/Unit12TimeSeries_Ollivander.csv", header=FALSE)
df$V1<-as.Date(df$V1, format="%m/%d/%Y")
df<-xts(df$V2, order.by=df$V1)

df2<-read.csv("/Users/stevengarrity/SMU_MSDS/DS6306_DoingDataScience/DDS_Git/Unit 11/Unit12TimeSeries_Gregorovitch.csv", header=FALSE)
df2$V1<-as.Date(df2$V1, format="%m/%d/%Y")
df2<-xts(df2$V2, order.by=df2$V1)

wands<-cbind(df, df2)

dygraph(wands, main="Wand Maker Sales across the past decades", ylab="Wands Sold", xlab="Year") %>%
  dySeries("..1", label = "Ollivander") %>%
  dySeries("..2", label = "Gregorovitch") %>%
  dyOptions(stackedGraph = TRUE, colors = RColorBrewer::brewer.pal(n=3, "Dark2")) %>%
  dyRangeSelector(height = 100) %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
  dyShading(from = "1995-1-1", to = "1999-1-1", color = "#ffd3d3")