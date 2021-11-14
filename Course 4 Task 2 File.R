#install.packages("RMySQL")
#install.packages("tidyverse")
#install.packages("funModeling")
#install.packages("Hmisc")
#install.packages("lubridate")
#install.packages("gcookbook")
#install.packages("ggfortify")
#install.packages("forecast")

library(RMySQL)
library(dplyr)
library(lubridate)
library(funModeling)
library(Hmisc)
library(tidyverse)
library(gcookbook) 
library(plotly)
library(ggplot2)
library(ggfortify)
library(forecast)

#sub_metering_1: (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered). 
#sub_metering_2: (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light. 
#sub_metering_3: (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.


#Connecting to database
con = dbConnect(MySQL(), user = 'deepAnalytics',
                password = 'Sqltask1234!',
                dbname = 'dataanalytics2018',
                host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

#list tables in database
dbListTables(con)

irisAll <- dbGetQuery(con, "SELECT * FROM iris")
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

dbListFields(con, "yr_2006")

yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr_2007<- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr_2008<- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr_2009<- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr_2010<- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

str(yr_2006)
summary(yr_2006)
tail(yr_2006)

str(yr_2007)
summary(yr_2007)
tail(yr_2007)

str(yr_2008)
summary(yr_2008)
tail(yr_2008)

str(yr_2009)
summary(yr_2009)
tail(yr_2009)

str(yr_2010)
summary(yr_2010)
tail(yr_2010)

#combine tables
newDF <- bind_rows(yr_2007, yr_2008, yr_2009)
str(newDF)
summary(newDF)
head(newDF)
tail(newDF)

#Combine Data and Time
newDF <- cbind(newDF,paste(newDF$Date, newDF$Time), stringAsFactors=FALSE)
colnames(newDF)[6] <- "DateTime"
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]
head(newDF)
newDF <- subset(newDF, select = -c(stringAsFactors))

#Convert DateTime attribute data type
newDF$DateTime <- as.POSIXct(newDF$DateTime,"%Y/%m/%d %H:%M:%S")

#Timezone
attr(newDF$DateTime, "tzone") <- "Europe/Paris"
str(newDF)

#Creating a parsed date & time attributes
newDF$year <- year(newDF$DateTime)
newDF$month <- month(newDF$Date)
newDF$quarter <- quarter(newDF$DateTime)
newDF$week <- week(newDF$DateTime)
newDF$weekday <- wday(newDF$DateTime)
newDF$hour <- hour(newDF$DateTime)
newDF$minute <- minute(newDF$DateTime)

#Data Exploration
summary(newDF)

#sub-meters 1 and 2 don't measure much energy used. The means are 1.122 and 1.299 respectively.
#Sub-meter 3 measures a mean of 6.458. 
describe(newDF)
plot_num(newDF)

plot(density(newDF$Sub_metering_1), main = "Density for Energy Usage by different Sub-meters")
lines(density(newDF$Sub_metering_2), col = "red")
lines(density(newDF$Sub_metering_3), col = "blue")

boxplot(newDF$Sub_metering_1~newDF$weekday, main = "", xlabel="Weekday")
boxplot(newDF$Sub_metering_2~newDF$weekday, main = "", xlabel="Weekday")
boxplot(newDF$Sub_metering_3~newDF$weekday, main = "", xlabel="Weekday")


par(
  mfrow=c(1,3),
  mar=c(4,4,1,0)
)
hist(newDF$Sub_metering_1, col="red")
hist(newDF$Sub_metering_2, col="blue", add=TRUE)
hist(newDF$Sub_metering_3, col="green", add=TRUE)

firstcol <- c("Sub-Metering 1","Sub-Metering 2","Sub-Metering 3")
secondcol <- c(sum(newDF$Sub_metering_1),
               sum(newDF$Sub_metering_2),
               sum(newDF$Sub_metering_3))

sums <- data.frame(firstcol,secondcol)

ggplot(sums, aes(x = firstcol, y = secondcol)) +
  geom_col() +
  geom_text(aes(label = secondcol), vjust = 1.5, colour = "white") +
  labs(x="", y="Kilowatt/Hours of Energy")


newDF$DateTime[1:5]
group_by(newDF, year)

plot(newDF$Sub_metering_1)

#Subset Week 2 of 2008
houseWeek <- filter(newDF, year==2008 & week==2)
plot(houseWeek$Sub_metering_1)

#Subset day 3 of week 1 of January 2008
houseDay <- filter(newDF, year==2008, month==1, week ==1, weekday==3)
plot_ly(houseDay,x=~houseDay$DateTime, y=~houseDay$Sub_metering_1, type='scatter', mode='lines')

#Plotting all three submeters
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, 
        name = "Kitchen",
        type = "scatter",
        mode = "lines") %>%
  add_trace(y = ~houseDay$Sub_metering_2,
            name = "Laundry",
            mode = "lines") %>%
  add_trace(y = ~houseDay$Sub_metering_3,
            name = "Water Heater/AC",
            mode = "lines") %>%
  layout(title="Power consumption January 2008 Week 1 Day 3",
         xaxis = list(title="Time"),
         yaxis = list(title="Power (watt-hours)"))

#Power only measured once every ten minutes for a day
houseDay10 <- filter(newDF, year == 2008 & week == 1 & weekday == 3 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, 
        name = "Kitchen",
        type = "scatter",
        mode = "lines") %>%
  add_trace(y = ~houseDay10$Sub_metering_2,
            name = "Laundry",
            mode = "lines") %>%
  add_trace(y = ~houseDay10$Sub_metering_3,
            name = "Water Heater/AC",
            mode = "lines") %>%
  layout(title="Power consumption January 2008 WeekDay 3",
         xaxis = list(title="Time"),
         yaxis = list(title="Power (watt-hours)"))

#Power measured over one week every half hour
houseWeek30 <- filter(newDF, year == 2008 & week == 20 & (minute == 0 | minute == 30))

plot_ly(houseWeek30, x = ~houseWeek30$DateTime, y = ~houseWeek30$Sub_metering_1, 
        name = "Kitchen",
        type = "scatter",
        mode = "lines") %>%
  add_trace(y = ~houseWeek30$Sub_metering_2,
            name = "Laundry",
            mode = "lines") %>%
  add_trace(y = ~houseWeek30$Sub_metering_3,
            name = "Water Heater/AC",
            mode = "lines") %>%
  layout(title="Power consumption May 2008 Week 2",
         xaxis = list(title="Time"),
         yaxis = list(title="Power (watt-hours)"))

#Power Measured once every quarter
houseSunday <- filter(newDF, year == 2008 & weekday == 3 & (minute == 0))

plot_ly(houseSunday, x = ~houseSunday$DateTime, y = ~houseSunday$Sub_metering_1, 
        name = "Kitchen",
        type = "scatter",
        mode = "lines") %>%
  add_trace(y = ~houseSunday$Sub_metering_2,
            name = "Laundry",
            mode = "lines") %>%
  add_trace(y = ~houseSunday$Sub_metering_3,
            name = "Water Heater/AC",
            mode = "lines") %>%
  layout(title="Power consumption Sundays in 2008",
         xaxis = list(title="Time"),
         yaxis = list(title="Power (watt-hours)"))

#____________________________________>>>>
#Subset for Mondays at 8pm
house070809weekly <- filter(newDF, weekday==2, hour==20, minute==1)
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency = 52, start = c(2007,1))

autoplot(tsSM3_070809weekly, ts.colour = "red", xlab = "Time", ylab = "Watt Hours", main = "Sub 3")

#Subset for monthly sampling
house070809monthly <- filter(newDF, month==1 & weekday==2 & hour==10 & minute==1)
tsSM1_070809monthly <- ts(house070809monthly$Sub_metering_1, frequency = 12, start = c(2007, 1))

autoplot(tsSM1_070809monthly, ts.colour = "blue", xlab = "Time", ylab = "Watt Hours", main = "Sub 1")

#Subset for quarterly sampling
house070809quarterly <- filter(newDF, weekday==2 & hour==10 & minute==1 & 
                                 quarter==1)
tsSM2_070809quarterly <- ts(house070809quarterly$Sub_metering_2, frequency = 4, start = c(2007, 1), end=c(2009, 12))

autoplot(tsSM2_070809quarterly, ts.colour = "green", xlab = "Time", ylab = "Watt Hours", main = "Sub 2")
#________________________________________>>>>

#FORECASTING
fitSM1 <- tslm(tsSM1_070809monthly ~ trend + season)
summary(fitSM1)

forecastfitSM1 <- forecast(fitSM1, h=20, level=c(50,75))
plot(forecastfitSM1, ylim = c(0,40),ylab="Watt Hours",xlab="Time", main=" ForecastSub 1")

fitSM2 <- tslm(tsSM2_070809quarterly ~ trend + season)
summary(fitSM2)

forecastfitSM2 <- forecast(fitSM2, h=20, level=c(70,80))
plot(forecastfitSM2, ylim=c(0,40), ylab="Watt Hours", xlab="Time", main="Forecast Sub 2")

fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season)
summary(fitSM3)

forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))
plot(forecastfitSM3c, ylim=c(0,40),ylab="Watt Hours",xlab="Time", main="Forecast Sub 3")

rmseScores <- c(7.559, 4.297, 6.871)
rSquared <- c(.06254, .005057, .3831)
compScores <- data.frame(rmseScores,rSquared)
plot(compScores$rmseScores)
plot(compScores$rSquared)
#_______________________END FORECASTING

#Seasonality
components070809SM1monthly <- decompose(tsSM1_070809monthly)
plot(components070809SM1monthly)
summary(components070809SM1monthly)

components070809SM2quarterly <- decompose(tsSM2_070809quarterly)
plot(components070809SM2quarterly)
summary(components070809SM2quarterly)

components070809SM3weekly <- decompose(tsSM3_070809weekly)
plot(components070809SM3weekly)
summary(components070809SM3weekly)

#Comparison Chart between Summary Stats__________________
plot(components070809SM1monthly, col="red")
plot(components070809SM2quarterly, col="blue")
plot(components070809SM3weekly, col="purple")

sub_meters <- c("Sub 1", "Sub 2", "Sub 3")
plot(components070809SM1monthly$seasonal)
par(new=TRUE)
plot(components070809SM2quarterly$seasonal, col="green", axes=FALSE)
par(new=TRUE)
plot(components070809SM3weekly$seasonal, col="purple", axes = FALSE, main="Seasonal")

plot(components070809SM1monthly$trend)
par(new=TRUE)
plot(components070809SM2quarterly$trend, col="green", axes = FALSE)
par(new=TRUE)
plot(components070809SM3weekly$trend, col="purple", axes = FALSE, main="Trend")

plot(components070809SM1monthly$random)
par(new=TRUE)
plot(components070809SM2quarterly$trend, col="green", axes = FALSE)
par(new=TRUE)
plot(components070809SM3weekly$random, col="purple", axes = FALSE, main="Random")

#Seasonal Adjusting______________
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
plot(tsSM3_070809Adjusted)
plot(decompose(tsSM3_070809Adjusted))

#no time periods showing up here.>>>>>>>>>>>
tsSM1_070809Adjusted <- tsSM1_070809monthly - components070809SM1monthly$seasonal
plot(tsSM1_070809Adjusted)
plot(decompose(tsSM1_070809Adjusted))

tsSM2_070809Adjusted <- tsSM3_070809quarterly - components070809SM2quarterly$seasonal
plot(tsSM2_070809Adjusted)
plot(decompose(tsSM2_070809Adjusted))

#Forecast with HoltWinters____________________

tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta = FALSE, gamma = FALSE)
plot(tsSM1_HW070809, ylim=c(0,25))

tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM1_HW070809for, ylim=c(0,20), ylab="Watt Hours", xlab="Time - Sub 1")

tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))
plot(tsSM1_HW070809forC, ylim = c(0,20), ylab = "Watt Hours", xlab = "Time - Sub 1", start(2010))


tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta = FALSE, gamma = FALSE)
plot(tsSM2_HW070809, ylim=c(0,25))

tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
plot(tsSM2_HW070809for, ylim=c(0,20), ylab="Watt Hours", xlab="Time - Sub 2")

tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
plot(tsSM2_HW070809forC, ylim = c(0,3), ylab = "Watt Hours", xlab = "Time - Sub 2", start(2010))


tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta = FALSE, gamma = FALSE)
plot(tsSM3_HW070809, ylim=c(0,25))

tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim=c(0,20), ylab="Watt Hours", xlab="Time - Sub 3")

tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=500, level=c(10,25))
plot(tsSM3_HW070809forC, ylim = c(0,20), ylab = "Watt Hours", xlab = "Time - Sub 3", start(2010))
