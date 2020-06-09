#Read data, 726676 obs
path <-'/Users/JoeyYang/Desktop/IBM/201701-citibike-tripdata.csv'
path2 <- '/Users/JoeyYang/Desktop/IBM/Weather_nyc.csv'
data <-read.csv(path)

#Load libraries
install.packages(c("dplyr", "tidyverse"))
options(buildtools.check = function(action) TRUE )
devtools::install_github('topepo/caret/pkg/caret', build_vignettes = FALSE)
install.packages(c("randomForest","geosphere"))
library(randomForest)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(grid)
library(caret)
library(geosphere)
library(lubridate)
library(chron)

#Delete NAs, 697600 obs remaining
data <- na.omit(data)

#Create duration variable, convert seconds to minutes
data$Duration_min <- data$Trip.Duration/60

#Create age variable, base year is 2017
data$Age <- 2017 - data$Birth.Year
# remove age outliers, 697338 observations remain
data <- data[data$Age<100,]
#create bins for Age
data$Agecat[data$Age <= 35]<-"youth"
data$Agecat[data$Age >35 & data$Age <=60]<-"middle age"
data$Agecat[data$Age > 60]<-"Elderly"

#Recode gender
data$Gender <- ifelse(data$Gender ==1,"Male",ifelse(data$Gender ==2, "Female","Unknown"))

#create time indicators based on Start Time
Sys.setenv(TZ="America/Toronto")
Sys.getenv("TZ")
data$StartDate <- as.Date(factor(data$Start.Time),format="%Y-%m-%d")
data$StartTime <- format(as.POSIXct(data$Start.Time) ,format = "%H:%M:%S")

Morning_p1 <- "08:00:00"
Morning_p1 <-chron(times=Morning_p1)
Morning_p2 <- "10:00:00"
Morning_p2 <-chron(times=Morning_p2)
After_p1 <- "16:00:00"
After_p1 <-chron(times=After_p1)
After_p2 <- "19:00:00"
After_p2 <-chron(times=After_p2)

#get day of week
data$wday <- weekdays(data$StartDate)
#create variable that indicate peak hours (8-10AM or 4-7PM during weekdays)
data$peaktime <- ifelse((data$wday %in% c("Monday", "Tuesday","Wednesday","Thursday","Friday")) & 
                          Morning_p1<data$StartTime &
                          Morning_p2>data$StartTime, "Peak",
                        ifelse((data$wday %in% c("Monday", "Tuesday","Wednesday","Thursday","Friday")) &
                                 After_p1<data$StartTime &
                                 After_p2>data$StartTime,"Peak","NonPeak"))

#check quantile and clear outliers in duration (minutes)
quantile(data$Duration_min, probs = c(0.0005,0.9995))

#clear outliers in duration (minutes), 696583 obs left in the dataset
data1 <- data[data$Duration_min<289.98,]
data1 <- data1[data1$Duration_min>1.084,]

#Introduce external weather data (contains daily average temperature and dummy of rain)
weather<-read.csv(path2)
weather$date <- make_date(year = weather$Year,month = weather$Month,day=weather$Day)
weather<-weather[,c(22,5,17)]

#Merge weather data with main dataset, using the date as key
data1 <-merge(data1,weather,by.x="StartDate", by.y="date")

#Create distance and speed variables
newdata <- data1[,c(7,8,11,12,15:19,21:24)]
newdata$distance <- distHaversine(newdata[,c(2,1)], newdata[,c(4,3)])
newdata$speed_kmh <- (newdata$distance/1000)/(newdata$Duration_min/60)

#Retain only 7 predictors and 1 response variable (Duration in minutes)
finaldata <- newdata[,c(6,7,9:14)]
finaldata$Rain <-ifelse(finaldata$Rain==1,"Rain","NoRain")
sapply(finaldata, class)
finaldata=finaldata %>% mutate_if(is.character, as.factor)

#Split dataset into training and test sets
n = nrow(finaldata)*0.2
test_index = sample(nrow(finaldata), nrow(finaldata) - n)
X_train<-finaldata[test_index,-2]
Y_train<-finaldata[test_index,2]
Training<-cbind(X_train,Y_train)
X_test<-finaldata[-test_index,-2]
Y_test<-finaldata[-test_index,2]
Test<-cbind(X_test,Y_test)

write.csv(Training,file="training set.csv")
write.csv(Test,file="test set.csv")

#Build random forest algorithm 
set.seed(123)
rf_model = randomForest(Y_train ~ ., data=Training, ntree=100, mtry=2, importance=TRUE)


train.control <- trainControl(method = "repeatedcv", 
                              number = 8, repeats = 3)
rf_model <- train(Y_train ~., data = Training, method = "rf",
                  trControl = train.control)
