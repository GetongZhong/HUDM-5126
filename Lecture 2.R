# least squares estimates for the production, p.19

production <- read.table("production.txt",header=TRUE)
attach(production)
m1 <- lm(RunTime~RunSize)
summary(m1)


# Regression Output from R: 95% confidence intervals, p.24

round(confint(m1,level=0.95),3)


# Ninety-five percent confidence intervals for the population regression line, p.27

predict(m1,newdata=data.frame(RunSize=c(50,100,150,200,250,300,350)),interval="confidence",level=0.95)


# Ninety-five percent prediction intervals for the actual value of Y, p.27

predict(m1,newdata=data.frame(RunSize=c(50,100,150,200,250,300,350)),interval="prediction",level=0.95)
detach(production)

# Regression Output from R p.31

changeover_times <- read.table("changeover_times.txt",header=TRUE)
attach(changeover_times)
m1 <- lm(Changeover~New)
summary(m1)


# scatter plot and box plots of the change-over time data, p.32

par(mfrow=c(2,2)) # this divides the space of plotting into relevant sections
plot(New,Changeover,xlab="Dummy variable, New",ylab="Change Over Time")
abline(lsfit(New,Changeover))
boxplot(Changeover~New,xlab="Dummy variable, New",ylab="Change Over Time")
boxplot(Changeover~Method,ylab="Change Over Time",xlab="Method")


## Extras

# How to look at the names and structure of your R data

str(production)

# The regression line slopes down, how significant is it?
confint(m1)
confint(m1, level = .99)
# the answer of significance of 0 changes!

# lsfit vs lm
# lsfit might be faster than lm in most cases

attach(production)
summary(lm(RunTime~RunSize))
summary(lsfit(RunSize, RunTime))

# If you just need coefficents, better to use lsfit than lm

# plotiing with various matrix settings
# 3 rows, 1 column
par(mfrow = c(3, 1))
plot(New,Changeover,xlab="Dummy variable, New",ylab="Change Over Time")
abline(lsfit(New,Changeover))
boxplot(Changeover~New,xlab="Dummy variable, New",ylab="Change Over Time")
boxplot(Changeover~Method,ylab="Change Over Time",xlab="Method")

# 1 row, 3 columns
par(mfrow = c(1, 3))
plot(New,Changeover,xlab="Dummy variable, New",ylab="Change Over Time")
abline(lsfit(New,Changeover))
boxplot(Changeover~New,xlab="Dummy variable, New",ylab="Change Over Time")
boxplot(Changeover~Method,ylab="Change Over Time",xlab="Method")
