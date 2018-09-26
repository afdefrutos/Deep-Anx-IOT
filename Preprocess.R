

#packages
install.packages("tidyr")

install.packages("dplyr") 
install.packages("lubridate")
library(lubridate)
library(readr)
library(dplyr)

#import
setwd("~/Desktop")
household_power_consumption <- read_delim("Deep Anx Task 1/household_power_consumption.txt", 
                                          ";", escape_double = FALSE, trim_ws = TRUE)


#Merging Date and Time
householdpc <-cbind(household_power_consumption,paste(household_power_consumption$Date,household_power_consumption$Time), stringsAsFactors=FALSE)

#Making DateTime first column and renaming
householdpc <- householdpc[,c(ncol(householdpc), 1:(ncol(householdpc)-1))]
colnames(householdpc)[1] <-"DateTime"
summary(householdpc)
View(householdpc)

#from charachter to Time, Date, DateTime
householdpc$DateTime <- dmy_hms(householdpc$DateTime)

householdpc$Date <- as.Date(householdpc$Date, "%d/%m/%Y")

householdpc$Time <- hms(householdpc$Time)

hpc$Hour <- hour(hpc$DateTime)
  
  
hpc<- householdpc
View(hpc
     )

#By 30 min
summary(hpc)
time_index <- seq(from = as.POSIXct("2006-12-16 17:00:00"), 
                  to = as.POSIXct("2010-11-26 21:02:00"), by = "hour")

View(time_index)

hpcbn <- 
  hpc %>%
  select(Year, Month, Day, Hour,  ActiveH, ReactiveH, Total) %>%
  group_by(Year, Month, Day, Hour) %>%
  summarise_at(vars(ActiveH, ReactiveH,Total), funs(sum))
  
View(hpcbn)

#A lot of NA's
summary(hpc)
View(hpc)

colnames(hpc)[8] <- "Kitchen"
colnames(hpc)[9] <- "Laundry"
colnames(hpc)[10] <- "Air Conditioning"
colnames(hpc)[4] <- "Active"
colnames(hpc)[5] <- "Reactive"




#correlation
corrData <- correlation(hpcnotime)

View(corrData[["correlation"]])

#global active power, and global intensity are the same therefore 
hpc$Global_intensity <- NULL


#RIght units
hpc$ActiveH <- (hpc$Active*1000)/60
hpc$ReactiveH <- (hpc$Reactive*1000)/60

hpc$Active <- NULL  
hpc$Reactive <- NULL

View(hpc)


#New columns for date, time, etc

hpc$Month<- month(hpc$DateTime)
hpc$Day <- day(hpc$DateTime)
hpc$Year <- year(hpc$DateTime)


#Total energy

hpc$Total <- hpc$ActiveH+hpc $ReactiveH


#replace missing values with mean

hpc[is.na(hpc)] <- 0

#Filter 
hpc %>%
  filter(Month==6) %>%
  summarise(mean(Total))


hpc %>%
  group_by (Year) %>%
  summarise(sum(hpc$ReactiveH))



ggpplot(hpcbn)

str(hpc)






