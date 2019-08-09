setwd("~/weather data")
# Creating Data Frame
data <- read.csv("1998-2019 KaohsiungRMAX+TMIN.csv", sep = ",", header = TRUE) 
data_df=data  
data_df$month_text=month.abb[data_df$MONTH] 



#  Creating Date Colomn from Month and Year Colomn ( to use 'Date'as x-axis in time series plot)
data_df$Date <- with(data_df, sprintf("%s-%02s", MONTH, YEAR))


# Changing the "Date" colomn class from 'Charcter' to 'Date' class
library(lubridate)
data_df$Date <- mdy(data_df$Date)
data_df$Date=as.Date(data_df$Date)
Local_Cases <- data_df$LOCAL
Imported_Cases <- data_df$IMPORT
Monthly_Min_Temperature <- data_df$TMIN
Monthly_Max_Rain <- data_df$RMAX

# --> construct separate plots for each series
library(lattice)
Local_Cases <- xyplot(Local_Cases ~ Date, data_df, type = "l" , lwd=2)
Imported_Cases <- xyplot(Imported_Cases ~ Date, data_df, type = "l" , lwd=2)
Monthly_Min_Temperature <- xyplot(Monthly_Min_Temperature ~ Date, data_df, type = "l", lwd=2)
Monthly_Max_Rain <-xyplot(Monthly_Max_Rain ~ Date, data_df, type = "l", lwd=2)

# --> Make the plot with second y axis:
library(RColorBrewer)
library(latticeExtra)
doubleYScale(Local_Cases, Monthly_Min_Temperature, text = c("Monthly Local Dengue Cases", "Monthly Minimum Temperature") , add.ylab2 = TRUE)
doubleYScale(Local_Cases, Monthly_Max_Rain, text = c("Monthly Local Dengue Cases", "Monthly Maximum Rain") , add.ylab2 = TRUE)
doubleYScale(Local_Cases, Imported_Cases, text = c("Monthly Local Dengue Cases", "Monthly Imported Dengue Cases") , add.ylab2 = TRUE)


#
setwd("~/weather data")
# Creating Data Frame
data2 <- read.csv("1999-2016 TainanRMAX+TMIN.csv", sep = ",", header = TRUE) 
data2_df=data2  
data2_df$month_text=month.abb[data2_df$MONTH] 



#  Creating Date Colomn from Month and Year Colomn ( to use 'Date'as x-axis in time series plot)
data2_df$Date <- with(data2_df, sprintf("%s-%02s", MONTH, YEAR))


# Changing the "Date" colomn class from 'Charcter' to 'Date' class
library(lubridate)
data2_df$Date <- mdy(data2_df$Date)
data2_df$Date=as.Date(data2_df$Date)
Local_Cases <- data2_df$LOCAL
Imported_Cases <- data2_df$IMPORT
Monthly_Min_Temperature <- data2_df$TMIN
Monthly_Max_Rain <- data2_df$RMAX

# --> construct separate plots for each series
library(lattice)
Local_Cases <- xyplot(Local_Cases ~ Date, data2_df, type = "l" , lwd=2)
Imported_Cases <- xyplot(Imported_Cases ~ Date, data2_df, type = "l" , lwd=2)
Monthly_Min_Temperature <- xyplot(Monthly_Min_Temperature ~ Date, data2_df, type = "l", lwd=2)
Monthly_Max_Rain <-xyplot(Monthly_Max_Rain ~ Date, data2_df, type = "l", lwd=2)

# --> Make the plot with second y axis:
library(RColorBrewer)
library(latticeExtra)
doubleYScale(Local_Cases, Monthly_Min_Temperature, text = c("Monthly Local Dengue Cases", "Monthly Minimum Temperature") , add.ylab2 = TRUE)
doubleYScale(Local_Cases, Monthly_Max_Rain, text = c("Monthly Local Dengue Cases", "Monthly Maximum Rain") , add.ylab2 = TRUE)
doubleYScale(Local_Cases, Imported_Cases, text = c("Monthly Local Dengue Cases", "Monthly Imported Dengue Cases") , add.ylab2 = TRUE)



