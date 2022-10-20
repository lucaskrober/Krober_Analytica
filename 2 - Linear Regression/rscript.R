dir()
data<-read.csv('FuelEconomy.csv')
library(janitor)
data<-janitor::clean_names(data)

attach(data)
boxplot(fuel_economy_mpg)
boxplot(horse_power)

cor(horse_power,fuel_economy_mpg)

mod<-lm(fuel_economy_mpg~horse_power)
summary(mod)

detach(data)
data$estimated_fuel_ec<-predict(mod)

attach(data)
residuals<-fuel_economy_mpg - estimated_fuel_ec




x11()
plot(fuel_economy_mpg,residuals,pch=20,ylim=range(-4,4))
abline(h=0)

# Opens the locator, after locating the odd points, click on stop AT THE GRAPH WINDOW 
ii<-identify(fuel_economy_mpg,residuals); 

data[ii,];

data<-data[-ii,]
# detach data and run linear regression again


plot(fuel_economy_mpg,horse_power)
plot(fuel_economy_mpg,residuals)

