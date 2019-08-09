
data$month_txt = month.abb[data$MONTH] 

jitter <- position_jitter(width = 0.25, height = 0)
ggplot(data=data , aes(x=month_txt,y= RMAX)) + 
  geom_boxplot(aes(month_txt,RMAX), outlier.shape = NA) +
  geom_point(aes(colour = cut(YEAR, c(1997,2018,2019))),alpha = 0.5,position=jitter,size=2) + 
  scale_color_manual(name = "Years",
                     values = c("(1997,2018]" = "Blue",
                                "(2018,2019]" = "Red"),
                     labels = c("1998-2018", "2019")) +  
  scale_x_discrete(limits=month.abb[1:12]) + 
  labs(x = "Month") + 
  labs(y = "Monthly Maximum  Rainfall (mm)")+
  ggtitle("Monthly Maximum Rainfall - Kaohsiung (1998-2019)")

