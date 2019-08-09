#set working directory
setwd("~/???????????????")


#library dependencies
library(ggplot2)

#file dependencies
dengueweek2 <- read.csv("dengueweek2.csv")
imported <- read.csv("imported.csv")
local <- read.csv("local.csv")
aggregateweek <- read.csv("aggregateweek.csv")


#plot  for imported cases  in a single plot for the 3 cities(months)
ggplot(data=aggregateweek, aes(x=as.numeric(Months), y=log(Numbers.of.Monthly.Dengue.Cases), color=County))+geom_line()+geom_point()+ facet_wrap(~Types.of.Cases)

#plot for imported and local cases for each city in single plot(months)
ggplot(data=aggregateweek, aes(x=as.numeric(Months), y=log(Numbers.of.Monthly.Dengue.Cases), color=Types.of.Cases))+geom_line()+geom_point()+ facet_wrap(~County)



#aggragate (years)
aggregateyear <- aggregate(Numbers.of.Monthly.Dengue.Cases~Years+County+Types.of.Cases,data=aggregateweek,sum)

#plot  for imported cases  in a single plot for the 3 cities(years)
ggplot(data=aggregateyear, aes(x=as.numeric(Years), y=log(Numbers.of.Monthly.Dengue.Cases), color=County))+geom_line()+geom_point()+ facet_wrap(~Types.of.Cases)

#plot for imported and local cases for each city in single plot(years)
ggplot(data=aggregateyear, aes(x=as.numeric(Years), y=log(Numbers.of.Monthly.Dengue.Cases), color=Types.of.Cases))+geom_line()+geom_point()+ facet_wrap(~County)



#join
imported <- read.csv("imported.csv")
local <- read.csv("local.csv")
join <- merge(imported,local,by=c("Y.M","MOI_County_living"),all=TRUE)

#scatter plot
ggplot(data=join, aes(x=as.numeric(Y.M), y=log(Number_of_confirmed_cases), color=MOI_County_living))+geom_line()+geom_point()+ facet_wrap(~Imported)

ggplot(data=join, aes(x=import,y=local))+geom_point()+geom_smooth(method = "lm")+labs(x="Numbers.of.Impoted.Cases",y="Numbers.of.Local.Cases")
joinlm <- lm(local~import, data = join)
ggplot(data=join, aes(x=as.numeric(Y.M), y=log(Number.of.Imported.cases), color=MOI_County_living))+geom_line()+geom_point()

#fitted values
autoplot(join[,'local'],series="Data")+autolayer(fitted(fit.consMR),series="Fitted")

