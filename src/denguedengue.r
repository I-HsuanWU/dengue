setwd("~/登革熱資料")
local<-read.csv("local.csv")
dengue <- read.csv("dengue2.csv")
View(dengue)
summary(dengue)

dengueweek <- read.csv("dengueweek.csv")
View(dengueweek)
aggregateweek <- aggregate(Number_of_confirmed_cases~year+weeknum+month+MOI_County_living+Imported,data=dengueweek,sum)
View(aggregateweek)


setwd("~/登革熱資料")
dengueweek2 <- read.csv("dengueweek2.csv")
View(dengueweek2)
library(ggplot2)
p <- ggplot(data = dengueweek2, x="year", y = "Number_of_confirmed_cases",colour = "MOI_County_living")
print(p)



setwd("~/登革熱資料")
dengueweek2 <- read.csv("dengueweek2.csv")
View(dengueweek2)
aggregateweek <- aggregate(Number_of_confirmed_cases~year+month+Y.M+MOI_County_living+Imported,data=dengueweek2,sum)
library(ggplot2)
ggplot(data=aggregateweek, aes(x=Y.M, y=Number_of_confirmed_cases, color=MOI_County_living)) +
  +     geom_line()+
  +     geom_point()+ facet_wrap(~Imported)


setwd("~/登革熱資料")
dengueweek2 <- read.csv("dengueweek2.csv")
#View(dengueweek2)
aggregateweek <- aggregate(Number_of_confirmed_cases~year+month+Y.M+MOI_County_living+Imported,data=dengueweek2,sum)
library(ggplot2)
#Time series plot  for imported cases  in a single plot for the 3 cities
ggplot(data=aggregateweek, aes(x=as.numeric(Y.M), y=log(Number_of_confirmed_cases), color=MOI_County_living))+geom_line()+geom_point()+ facet_wrap(~Imported)
#更改y軸與項目名稱
p <- ggplot(data=aggregateweek, aes(x=as.numeric(Y.M), y=log(Number_of_confirmed_cases), color=MOI_County_living))+geom_line()+geom_point()+ facet_wrap(~Imported)
p+labs(y="log(Number of confirmed cases)")+labs(colour="County")+labs(x="months")

#Time series plot for imported and local cases for each city in single plot  
ggplot(data=aggregateweek, aes(x=as.numeric(Y.M), y=log(Number_of_confirmed_cases), color=Imported))+geom_line()+geom_point()+ facet_wrap(~MOI_County_living)
#更改y軸與項目名稱
Q <- ggplot(data=aggregateweek, aes(x=as.numeric(Y.M), y=log(Number_of_confirmed_cases), color=Imported))+geom_line()+geom_point()+ facet_wrap(~MOI_County_living)
Q+labs(y="log(Number of confirmed cases)")+labs(colour="County")+labs(x="months")

#輸出
write.table(aggregateweek, file="aggregateweek.csv",sep=",",row.names=F)
local <- read.csv("local.csv")
imported <- read.csv("imported.csv")


#Time series plot  for imported cases  in a single plot for the 3 cities
ggplot(data=join, aes(x=as.numeric(Y.M), y=log(Number_of_confirmed_cases), color=MOI_County_living))+geom_line()+geom_point()+ facet_wrap(~Imported)

imported <- read.csv("imported.csv")
local <- read.csv("local.csv")
join <- merge(imported,local,by=c("Y.M","MOI_County_living"),all=TRUE)
library(ggplot2)
ggplot(data=join, aes(x=as.numeric(Y.M), y=log(Number.of.Imported.cases), color=MOI_County_living))+geom_line()+geom_point()


aggregateweek <- read.csv("aggregateweek.csv")
library(ggplot2)
ggplot(data=aggregateweek, aes(x=as.numeric(Months), y=log(Numbers.of.Monthly.Dengue.Cases), color=County))+geom_line()+geom_point()+ facet_wrap(~Types.of.Cases)
ggplot(data=aggregateweek, aes(x=as.numeric(Months), y=log(Numbers.of.Monthly.Dengue.Cases), color=Types.of.Cases))+geom_line()+geom_point()+ facet_wrap(~County)

aggregateyear <- aggregate(Numbers.of.Monthly.Dengue.Cases~Years+County+Types.of.Cases,data=aggregateweek,sum)
  ggplot(data=aggregateyear, aes(x=as.numeric(Years), y=log(Numbers.of.Monthly.Dengue.Cases), color=County))+geom_line()+geom_point()+ facet_wrap(~Types.of.Cases)
ggplot(data=aggregateyear, aes(x=as.numeric(Years), y=log(Numbers.of.Monthly.Dengue.Cases), color=Types.of.Cases))+geom_line()+geom_point()+ facet_wrap(~County)

ggplot(data=join, aes(x=import,y=local))+geom_point()+geom_smooth(method = "lm")+labs(x="Numbers.of.Impoted.Cases",y="Numbers.of.Local.Cases")
joinlm <- lm(local~import, data = join)
joinlm
summary(joinlm)
ggplot(data=join, aes(x=log(import),y=log(local)))+geom_point()+geom_smooth(method = "lm")+labs(x="log(Numbers.of.Impoted.Cases)",y="log(Numbers.of.Local.Cases)")                      

       