library('MASS')

startyear <- 1997
T <- 0
R <- 0
df <- data.frame()

for (Year in 1997:2019) {
  print(Year)
  for (Month in 1:7) {
    T[Month] <- data[data$MONTH==Month & data$YEAR==Year,'TMIN']
    R[Month] <- Data[Data$MONTH==Month & data$YEAR==Year,'RMAX'] 
  }
  df <- rbind(df, c(Year, 1,T , R))
}
names(df)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df$CASES <- data$local_cases
dfnew <- df

#Calculating AIC
glm <- stepAIC(glm(CASES ~ 1, data = dfnew),
                   scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7,
                   direction = "forward")