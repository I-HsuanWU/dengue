setwd("~/登革熱資料")
data <- read.csv("1998-2016 Kaohsiung_local_average.csv")

data_df=data  
data_df$month_text=month.abb[data_df$MONTH] 



data_df$month_text <- factor(data_df$month_text, levels=c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))



library(ggplot2)
p <- ggplot(data=data_df , aes(x=month_text,y= Local_average,group = 1)) + 
  geom_line(aes(month_text,Local_average), outlier.shape = NA)

p

