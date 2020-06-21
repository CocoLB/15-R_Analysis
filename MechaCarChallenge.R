library(tidyverse)

# read the CSV file into a DataFrame
mechaCarMpg <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
head(mechaCarMpg)
lm(mpg ~ vehicle length + vehicle weight + spoiler angle + ground clearance + AWD,data=mechaCarMpg)
# change columns names - as "lm" method couldn't read the space in the column names
names(mechaCarMpg)[names(mechaCarMpg)=="vehicle length"] <- "vl"
mechaCarMpg
names(mechaCarMpg)[names(mechaCarMpg)=="vehicle weight"] <- "vw"
names(mechaCarMpg)[names(mechaCarMpg)=="spoiler angle"] <- "sa"
names(mechaCarMpg)[names(mechaCarMpg)=="ground clearance"] <- "gc"
head(mechaCarMpg)

# multiple linear regression
lm(mpg ~ vl + vw + sa + gc + AWD,data=mechaCarMpg)
summary(lm(mpg ~ vl + vw + sa + gc + AWD,data=mechaCarMpg))
# only vl and gc have a significant impact on mpg
# mpg = -0.0104 + 6.267*vl + 3.546*gc + e

cor(mechaCarMpg$mpg, mechaCarMpg$vl)
cor(mechaCarMpg$mpg, mechaCarMpg$gc)

# linear model mpg~vl
plt <- ggplot(mechaCarMpg,aes(x=vl,y=mpg))
plt + geom_point()
model <- lm(mpg ~ vl,mechaCarMpg)
yvals <- model$coefficients['vl']*mechaCarMpg$vl +
  model$coefficients['(Intercept)'] 
plt + geom_point() + geom_line(aes(y=yvals, color="red"))

# linear model mpg~gc
plt <- ggplot(mechaCarMpg,aes(x=gc,y=mpg))
plt + geom_point()
model <- lm(mpg ~ gc,mechaCarMpg)
yvals <- model$coefficients['gc']*mechaCarMpg$gc +
  model$coefficients['(Intercept)'] 
plt + geom_point() + geom_line(aes(y=yvals, color="red"))

# read the Suspension Coil csv into a Dataframe
suspensionCoil <- read.csv(file='Suspension_Coil.csv',check.name=F,stringsAsFactors = F)
head(suspensionCoil)
# get the summary
summary(suspensionCoil$PSI)
# get each metric to create the statistics summary table
minPSI <- min(suspensionCoil$PSI)
maxPSI <- max(suspensionCoil$PSI)
meanPSI <- mean(suspensionCoil$PSI)
medPSI <- median(suspensionCoil$PSI)
varPSI <- var(suspensionCoil$PSI)
stdevPSI <- sd(suspensionCoil$PSI)
statsLabels = c("min","max","mean","median","variance","standard deviation")
statsResults = c(minPSI,maxPSI,meanPSI,medPSI,varPSI,stdevPSI )
statisticsTable = tibble(statsLabels, statsResults)
statisticsTable

plt <- ggplot(suspensionCoil, aes(x=PSI))
plt + geom_density()
plt + geom_boxplot()

# Suspension Coil T-Test
# plot densities for the whole population and for a sample

plt <- ggplot(suspensionCoil, aes(x=PSI))
plt + geom_density()
samples <- suspensionCoil %>% sample_n(50)
plt <- ggplot(samples, aes(x=PSI))
plt + geom_density()

t.test(samples$PSI,mu=mean(suspensionCoil$PSI))

samples <- suspensionCoil %>% sample_n(30)
t.test(samples$PSI,mu=mean(suspensionCoil$PSI))

