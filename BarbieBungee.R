# Barbie Bungee


bands <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9)
distance <- c(14,14.1,13.7,17.5,18.1,18.5,21.4,22,22.2,24.6,
              24.8,25.5,30.5,29.5,29.5,34.5,35,34,40.5,41,40.5,45.5,43.2,43,48,53,50)

barbie <- data.frame(bands=bands, distance=distance)



barbie_model <- lm(distance~bands, data=barbie)
summary(barbie_model)

barbie %>% ggplot(aes(x=bands, y=distance)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="GoGo Dolls bungee model", x="# Bands", y="Distance (inches)")

confint(barbie_model, level=0.95)

# Hypothesis test:
# Step 1: H0: b1 is equal to 0, Ha: b1 is not equal to 0
# Step 2: Calculate t-crit
qt(0.975, dim(barbie)[1]-2)
# Step 3: Calculate t-stat (-29.73)
# Step 4: Calculate p-value
# Step 5: Reject H0
# Step 6: Conclusions and confidence interval
confint(barbie_model, level=0.95)

newdf <- data.frame(bands = 24, distance=NA)

newbarbie <- data.frame(bands=c(barbie$bands,23), distance=c(barbie$distance,120))
newbarbie_model <- lm(distance~bands, data=newbarbie)
summary(newbarbie_model)
confint(newbarbie_model, level=0.95)

newbarbie %>% ggplot(aes(x=bands, y=distance)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="GoGo Dolls bungee model (new model)", x="# Bands", y="Distance (inches)")


# trying to predict the number of bands to get 268 inches and NO MORE!
newdf <- data.frame(bands = 54, distance=NA)
predict.lm(barbie_model, newdata=newdf, interval = "prediction")