#Tainan 1-7 AIC: 204.67
setwd("~/weather data")
data <- read.csv("1999-2018 TainanRMAX+TMIN.csv", sep = ",", header = TRUE) 

library('MASS')

startyear <- 1999
T <- 0
R <- 0
df <- data.frame()

for (YEAR in 1999:2015) {
  print(YEAR)
  for (MONTH in 1:7) {
    T[MONTH] <- data[data$MONTH==MONTH & data$YEAR==YEAR,"TMIN"]
    R[MONTH] <- data[data$MONTH==MONTH & data$YEAR==YEAR,"RMAX"] 
  }
  df <- rbind(df, c(YEAR,1,T , R))
}
setwd("~/登革熱資料")
local_Kaohsiung <- read.csv("1999-2015 L4-Kaohsiung+Tainan.csv")


names(df)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df$CASES <- local_Kaohsiung$L4_Tainan
dfnew <- df
#Calculating AIC
modelA <- stepAIC(glm(CASES ~ 1, data = dfnew, family=poisson(link=log)),
                  scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7  + R1 + R2 + R3 + R4 + R5 + R6 + R7 ,
                  direction = "forward")
summary(modelA) 

#Tainan 1-7 + IMPORT4-9(1VARIANT) AIC: 326.29
setwd("~/weather data")
data <- read.csv("1999-2018 TainanRMAX+TMIN.csv", sep = ",", header = TRUE) 

library('MASS')

startyear <- 1999
T <- 0
R <- 0
I <- 0
df <- data.frame()

for (YEAR in 1999:2015) {
  print(YEAR)
  for (MONTH in 1:7) {
    T[MONTH] <- data[data$MONTH==MONTH & data$YEAR==YEAR,"TMIN"]
    R[MONTH] <- data[data$MONTH==MONTH & data$YEAR==YEAR,"RMAX"] 
    for (MONTH in 1)
      I[MONTH] <- data[data$MONTH==MONTH & data$YEAR==YEAR,"I4_9"]
    
  }
  df <- rbind(df, c(YEAR,1,T , R, I))
}
setwd("~/登革熱資料")
local_Kaohsiung <- read.csv("1999-2015 L4-Kaohsiung+Tainan.csv")

names(df)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7','I4_9')
df$CASES <- local_Kaohsiung$L4_Kaohsiung
dfnew <- df
#Calculating AIC
modelB <- stepAIC(glm(CASES ~ 1, data = dfnew),
                  scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7  + R1 + R2 + R3 + R4 + R5 + R6 + R7 + I4_9,
                  direction = "forward", family=poisson(link=log))
summary(modelB) 


resA = residuals(modelA, type = "deviance")
resB = residuals(modelB, type = "deviance")

par(mfrow = c(1,2))
qqnorm(resA)
qqnorm(resB)
