#Kaohsiung weather1-8    model1
setwd("~/weather data")
data <- read.csv("1998-2019 KaohsiungRMAX+TMIN.csv", sep = ",", header = TRUE) 

library('MASS')

startyear <- 1999
T <- 0
R <- 0
df <- data.frame()

for (YEAR in 1999:2016) {
  print(YEAR)
  for (MONTH in 1:8) {
    T[MONTH] <- data[data$MONTH==MONTH & data$YEAR==YEAR,"TMIN"]
    R[MONTH] <- data[data$MONTH==MONTH & data$YEAR==YEAR,"RMAX"] 
  }
  df <- rbind(df, c(YEAR,1,T , R))
}
setwd("~/登革熱資料")
local_Kaohsiung <- read.csv("local-Kaohsiung2.csv")
loca_yr_Kaohsiung2<- aggregate(local~year, data=local_Kaohsiung, sum)

names(df)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','T8','R1','R2','R3','R4','R5','R6','R7','R8')
df$CASES <- loca_yr_Kaohsiung2$local
dfnew <- df
#Calculating AIC
model1 <- stepAIC(glm(CASES ~ 1, data = dfnew, family=poisson(link=log)),
                  scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8 + R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8,
                  direction = "forward")
summary(model1) #AIC: 2368







#Kaohsiung weather1-8 + IMPORT6-9(1VARIANT)    model2
setwd("~/weather data")
data <- read.csv("1998-2019 KaohsiungRMAX+TMIN.csv", sep = ",", header = TRUE) 

library('MASS')

startyear <- 1999
T <- 0
R <- 0
I <- 0
df <- data.frame()

for (YEAR in 1999:2016) {
  print(YEAR)
  for (MONTH in 1:8) {
    T[MONTH] <- data[data$MONTH==MONTH & data$YEAR==YEAR,"TMIN"]
    R[MONTH] <- data[data$MONTH==MONTH & data$YEAR==YEAR,"RMAX"] 
    for (MONTH in 1)
      I[MONTH] <- data[data$MONTH==MONTH & data$YEAR==YEAR,"I6_9"]
    
  }
  df <- rbind(df, c(YEAR,1,T , R, I))
}
setwd("~/登革熱資料")
local_Kaohsiung <- read.csv("local-Kaohsiung.csv")
loca_yr_Kaohsiung <- aggregate(local~year, data=local_Kaohsiung, sum)

names(df)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','T8','R1','R2','R3','R4','R5','R6','R7','R8','I6_9')
df$CASES <- loca_yr_Kaohsiung2$local
dfnew <- df
#Calculating AIC
model2 <- stepAIC(glm(CASES ~ 1, data = dfnew),
                  scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8 + R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8 + I6_9,
                  direction = "forward", family=poisson(link=log))
summary(model2) #AIC: 298.76





#Q-Q plot to analysis residuals of model
res1 = residuals(model1, type = "deviance")
res2 = residuals(model2, type = "deviance")

par(mfrow = c(1,2))
qqnorm(res1)
qqnorm(res2)



