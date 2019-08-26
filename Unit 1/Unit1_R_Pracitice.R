#vectors
age = c(22,21,24,19,20,23)
age
age[2]

yrs_math_ed = c(4,5,2,5,3,5)
yrs_math_ed

# Make a data frame
df1 = data.frame(Age=age, Years=yrs_math_ed)
df1
df1[4,2]
df1[2,]
df1$Age?
str(df1)

############################################################
a = c("Mary","Martha","Kim","Kristen","Amy","Sam")
b = c("English","Math","Sociology","Math","Music","Dance")

df2 = data.frame(Name = a, Major = b)
df2

df3 = data.frame(Age = age, Years = yrs_math_ed, Name = a, Major = b)
df3
str(df3)

df4 = cbind(df1,df2) # cbind combines columns
df4
str(df4)
class(df4$Name)
summary(df4)

# define a new row/student
d = c(19,4,"John","Math")

df5 = rbind(df4,d) # rbind combines rows [[[DOESN'T WORK]]]. Try this instead:
dfCopy = df4
dfCopy$Name = as.character(df4$Name)
dfCopy$Major = as.character(df4$Major)
summary(dfCopy)
# now add the new row
df5 = rbind(dfCopy,d)
df5
str(df5)
# fix the classes of variables
df5$Age = as.numeric(df5$Age)
df5$Years = as.numeric(df5$Years)
df5$Name = as.factor(df5$Name)
df5$Major = as.factor(df5$Major)
str(df5)

# filtering the data frame
# All students with more than 4 years of Math
df5[df5$Years > 4,]
#All students with more than 4 years of Math and are 21 years of age or older
df5[(df5$Years > 4 & df5$Age >=21),]
#All students that are majoring in math
df5[df5$Major == "Math",]

##########################
# Importing data: Two ways:

Example1 = read.csv("/Users/stevengarrity/AroyaAnalytics/PVLI_Roof_Experiment/Configuration-5-Table-1.csv", header = TRUE)
Example1
Example1$Ts

Example2 = read.csv(file.choose(), header = TRUE)
Example2[,1]

#########################
# Plotting data

#scatterplot
plot(Example1$Port.1,Example1$Port.2, pch=20, xlab = "Port1 [W/m2]", ylab = "Port2 [W/m2]", main="Port1 vs Port2")
abline(h=600, col="red", lwd=0.5)
abline(v=600, col="blue", lwd=0.5)

#play with the built-in MPG dataset
require(ggplot2)
?mpg
plot(mpg$hwy,mpg$cty, pch=20, xlab="Highway MPG", ylab="City MPG", main="Highway vs City MPG")

#play with the built-in Iris dataset
iris
plot(iris$Petal.Length,iris$Sepal.Length, pch=20, xlab="Petal length", ylab="Sepal length", main="Petal vs Sepal Length")

iris_vir = iris[iris$Species == "virginica",]
plot(iris_vir$Sepal.Length,iris_vir$Petal.Length, col="blue", ylim=c(0,7), xlim=c(4,8))
iris_vers = iris[iris$Species == "versicolor",]
points(iris_vers$Sepal.Length, iris_vers$Petal.Length, col="red")
iris_set = iris[iris$Species == "setosa",]
points(iris_set$Sepal.Length, iris_set$Petal.Length, col="green")

dev.off()

#histograms
hist(mpg$cty, col="blue")

hist(iris$Petal.Length, col="blue")

#boxplots
boxplot(cty~class, data=mpg, main="Boxplot:City MPG v. Class")

#subplots
par(mfrow=c(1,2))
hist(mpg$cty, col="blue", main = "Histogram of MPG", xlab="MPG")
boxplot(cty~cyl, data=mpg, main="Boxplot:City MPG v. Cylinders")

#barplots
age = c(22,21,24,19,20,23)
yrs_math_ed = c(4,5,2,5,3,5)
names = c("Mary","Martha","Kim","Kristen","Amy","Sam")
subject = c("English","Math","Sociology","Math","Music","Dance")

df3 = data.frame(Age=age, Years=yrs_math_ed, Name=names, Subject=subject)

barplot(df3$Years, names.arg=df3$Name, ylab="Years of Math Education")

#barplot of frequency of each class of car.
#note hwo the data is in a different format for mpg.
#it is not one row per relevant observation with the value at the end.
#we need to count the number of eeach class and then plot

#turn class into a factor and then let summary count for us:
summary(mpg$class)

mpg$classFact = as.factor(mpg$class)
head(mpg)
summary(mpg$classFact)

barplot(summary(mpg$classFact), main="Number of Cars by Class", ylab="Number of Cars")

#Quiz
pairs(~mpg+disp+drat+wt, data=mtcars, main="Simple Scatterplot Matrix")

##############################################
#Lecture Part 3

#draw a sample from a standard normal distribution
#run many times varying sample size and look at histogram and mean
sample1 = rnorm(1000,0,1) #sample size, mean, standard deviation
hist(sample1)
mean(sample1)
sd(sample1)

#another way to do the same thing:
population = rnorm(1000000,0,1)
hist(population)
sample1 = sample(population,100) # take a sample of size 1000 from the population
hist(sample1)
mean(sample1)
sd(sample1)

#make a function where you pass sample size, the number of samples to be drawn

#######################################
#Function:  xbarGenerator
#Arguments: sampleSize: the size of the sample that each sample mean is based on.
#           number_of_samples: the number of samples and thus sample means we will draw
#Author:    Steven Garrity
#######################################
xBarVec = c() #Global vector to thold the sample means
population = rnorm(1000000,10,3) #Simulating a population of 1,000,000 with mean = 10 and standard deviation = 3

xbarGenerator = function(sampleSize = 30, number_of_samples = 100)
{
  for(i in 1:number_of_samples)
  {
    theSample = sample(population,sampleSize)
    xbar = mean(theSample)
    xBarVec = c(xBarVec, xbar)
  }
  return(xBarVec)
}

xbars = xbarGenerator(30,1000)
length(xbars)
hist(xbars)

#Modify the function
xBarVec = c() #Global vector to thold the sample means
population = rnorm(1000000,10,3) #Simulating a population of 1,000,000 with mean = 10 and standard deviation = 3

xbarGenerator = function(sampleSize = 50, number_of_samples = 1000, mean = 60, sd = 10)
{
  for(i in 1:number_of_samples)
  {
    theSample = rnorm(sampleSize, mean, sd)
    xbar = mean(theSample)
    xBarVec = c(xBarVec, xbar)
  }
  return(xBarVec)
}

xbars = xbarGenerator(50,10000,60,10)
length(xbars)
hist(xbars)
mean(xbars)
sd(xbars)
summary(xbars)
10/sqrt(50) # standard error

#Data Science Profile
labels = c("Data Viz","ML","Math","Stats","CS", "Comms","Domain")
myscores = c(2,2.5,1,2,1.5,2,0)
myprofile = data.frame(Category=labels, Score=myscores)
barplot(myprofile$Score, names.arg=myprofile$Category, col='blue', 
        ylab="Score", main="Garrity's Data Science Profile", ylim=c(0,5))


### 95% Confidence Interval with known sigma
samplex = c(21,34,14,35,70,54,33,25)
xbar = mean(samplex)
sigmax = 11
n = 8
xbar
xbar-(1.96*(sigmax/sqrt(n)))
xbar+(1.96*(sigmax/sqrt(n)))

### 95% Confidence Interval with unknown sigma
samplex = c(21,34,14,35,70,54,33,25)
xbar = mean(samplex)
sigmax = 11
n = 8
xbar
xbar-(2.365*(sigmax/sqrt(n)))
xbar+(2.365*(sigmax/sqrt(n)))


