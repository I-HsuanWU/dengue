setwd("~/登革熱資料")
# Creating Data Frame
data <- read.csv("1999-2016 Kaohsiung_local_average.csv", sep = ",", header = TRUE) 
data_df=data  
data_df$Month=month.abb[data_df$MONTH] 

                     
data_df$Month <- factor(data_df$Month, levels=c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))              
                     
                     
                     
# Changing the "Date" colomn class from 'Charcter' to 'Date' class
library(lubridate)
Local_Cases <- data_df$Local_average_K
Imported_Cases <- data_df$Import_average_K


# --> construct separate plots for each series
library(lattice)
Local_Cases <- xyplot(Local_Cases ~ Month, data_df, type = "l" , lwd=2)
Imported_Cases <- xyplot(Imported_Cases ~ Month, data_df, type = "l" , lwd=2)
# --> Make the plot with second y axis:
library(RColorBrewer)
library(latticeExtra)
doubleYScale(Local_Cases, Imported_Cases, text = c("Monthly Local Dengue Cases", "Monthly Imported Dengue Cases") , add.ylab2 = TRUE) 




setwd("~/登革熱資料")
# Creating Data Frame
data <- read.csv("1999-2016 Tainan_local_average.csv", sep = ",", header = TRUE) 
data_df=data  
data_df$Month=month.abb[data_df$MONTH] 


data_df$Month <- factor(data_df$Month, levels=c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))              



# Changing the "Date" colomn class from 'Charcter' to 'Date' class
library(lubridate)
Local_Cases <- data_df$Local_average
Imported_Cases <- data_df$Import_average


# --> construct separate plots for each series
library(lattice)
Local_Cases <- xyplot(Local_Cases ~ Month, data_df, type = "l" , lwd=2)
Imported_Cases <- xyplot(Imported_Cases ~ Month, data_df, type = "l" , lwd=2)
# --> Make the plot with second y axis:
library(RColorBrewer)
library(latticeExtra)
doubleYScale(Local_Cases, Imported_Cases, text = c("Monthly Local Dengue Cases", "Monthly Imported Dengue Cases") , add.ylab2 = TRUE) 



setwd("~/登革熱資料")
# Creating Data Frame
data <- read.csv("1999-2016 Kaohsiung_local_average.csv", sep = ",", header = TRUE) 
data_df=data  
data_df$Month=month.abb[data_df$MONTH] 


data_df$Month <- factor(data_df$Month, levels=c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))              



# Changing the "Date" colomn class from 'Charcter' to 'Date' class
library(lubridate)
Kaohsiung_Imported_Cases <- data_df$Import_average_K
Tainan_Imported_Cases <- data_df$Import_average_T


# --> construct separate plots for each series
library(lattice)
Kaohsiung_Imported_Cases <- xyplot(Kaohsiung_Imported_Cases ~ Month, data_df, type = "l" , lwd=2)
Tainan_Imported_Cases <- xyplot(Tainan_Imported_Cases ~ Month, data_df, type = "l" , lwd=2)
# --> Make the plot with second y axis:
library(RColorBrewer)
library(latticeExtra)
doubleYScale(Kaohsiung_Imported_Cases, Tainan_Imported_Cases, text = c("Monthly Kaohsiung Imported Dengue Cases", "Monthly Tainan Imported Dengue Cases") , add.ylab2 = TRUE) 

