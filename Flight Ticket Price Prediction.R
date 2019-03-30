#PREDICTION OF FLIGHT FARES: #MachineHack Hackathon:

rm(list=ls())

getwd()

#Importing Dataset:
library(readxl)
data_train <- read_excel("C:/Users/Ranjith P/Desktop/Flight Data/Data_Train.xlsx")
View(data_train)

train=data_train
View(train)
summary(train)
str(train)


#Data Preparations:
#Importing Libraries:
library(stringr) 
library(stringi) 
library(MASS) 
library(DMwR)
library(plyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(corrgram)
library(caret)
library(lubridate)
library(tidyverse)
library(rpart)


#Missing Values Analysis:
sum(is.na(train))
mv=data.frame(apply(train, 2, function(x){sum(is.na(x))}))
mv
train=na.omit(train)
dim(train)
sum(is.na(train))


#Understanding & Preparing variables:
#Variable 1:Airline:
unique(train$Airline)

#Plotting frequecy count of each airline:
ggplot(train,aes(x=train$Airline,fill=train$Airline))+
  geom_bar(position="dodge")+labs(title = "Counts of each Airline")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Plotting mean price of each airline:
ggplot(train, aes(x=train$Airline, y=train$Price)) + stat_summary(fun.y="mean", geom="bar")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title = "Mean price of each airline")

#Boxplot of each airline vs price:
ggplot(aes(y = train$Price, x = train$Airline, fill = train$Price), data = train) + geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title = "Boxplot of each level in airline with price")

#From the above chart it's evident that levels such as "Jet Airways Business", "Multiple carriers Premium economy" ,"Trujet","Vistara Premium economy"   have very less no.of observations.So removing these levels. 
#So creating Subset:
train=subset(train, train$Airline != "Vistara Premium economy" & train$Airline != "Jet Airways Business" & train$Airline  != "Multiple carriers Premium economy" & train$Airline != "Trujet")
unique(train$Airline)
#Thus, removed the less proportionate levels from Airline variable.


#Variable 2: Date_of_Journey: 
unique(train$Date_of_Journey)
#Replacing same dates of different formats (1/03/2019 and 01/03/2019) to same format:
train$Date_of_Journey <- str_replace_all(train$Date_of_Journey, "01/03/2019",  "1/03/2019")
train$Date_of_Journey <- str_replace_all(train$Date_of_Journey, "03/03/2019",  "3/03/2019")
train$Date_of_Journey <- str_replace_all(train$Date_of_Journey, "06/03/2019",  "6/03/2019")
train$Date_of_Journey <- str_replace_all(train$Date_of_Journey, "09/03/2019",  "9/03/2019")
unique(train$Date_of_Journey)
#Changing / to - :
str(train$Date_of_Journey)
train$Date_of_Journey=str_replace_all(train$Date_of_Journey, "[/]",  "-")
unique(train$Date_of_Journey)


#Variable 3: Source:
unique(train$Source)


#Variable 4: Destination:
unique(train$Destination)
#Replacing New Delhi to Delhi:
train$Destination=str_replace_all(train$Destination, "New Delhi",  "Delhi")
unique(train$Destination)


#Variable 5: Route:
unique(train$Route)   #My Perception: It's not important variable. Since, Total_Stops variable explains the same.


#Variable 6: Dep_Time:
unique(train$Dep_Time)
#Combining Date_of_Journey & Dep_Time to new variable:
train$departure=paste(train$Date_of_Journey, train$Dep_Time, sep=' ') 
#Tranforming the departure to date time format:
train$departure=as.POSIXlt(train$departure, format = "%d-%m-%Y %H:%M")
#Sorting the dataset based on departure:
train=train[ order(train$departure , decreasing = FALSE ),]
class(train$departure)
str(train)
#Thus, I created a new variable named departure by uniting Date_of_Journey & Dep_Tme.
#The departure variable has been changed to datetime format.
#We can extract day , month, hour seperately in to new columns


#Variable 7: Arrival_Time:
unique(train$Arrival_Time) 
#My Perception: Dep_Time & Arrival_Time will be explained by duration variable. So, we can leave this variable later.
#arrival=data.frame(str_split_fixed(train$Arrival_Time, " ", 2))


#Variable 8 :Duration:
unique(train$Duration)
str(train$Duration)
#Duration is in categorical format. It has to be changed to numeric. 
#So, trying to remove the h and m from the variable.
train$dur=str_replace_all(train$Duration, "h ",  ".")
train$dur=str_replace_all(train$dur, "m",  "")
train$dur=str_replace_all(train$dur, "h",  ".00")
class(train$dur)

train$dur1=hm(train$dur)
str(train$dur1)
sum(is.na(train$dur1))
class(train$dur1)
summary(train$dur1)
#train$dur2=as.numeric(train$dur1) #in seconds
train$duration=round(as.duration(train$dur1)/dhours(1)) #in hours (important)
#Duration --> dur --> dur1 --> duration


#Variable 9: Total_Stops:
unique(train$Total_Stops)


#Variable 10: Additional Info:
unique(train$Additional_Info)
train$Additional_Info=str_replace_all(train$Additional_Info, "No Info",  "No info")
unique(train$Additional_Info)


#Variable 11: Price:
summary(train$Price)
boxplot(train$Price)
colnames(train)


#Dealing with departure variable:
#Initially I united the "Date_of_Journey" & "Dep_Time" to form a new variable named "departure". I changed this type to date-time format.
#Now taking only hour values form it.
#Extracting the hour:
train$dep_hour <- format(train$departure, "%H")
#Creating morning, day, evening, night, midnight timestamp using dep_hour variable:
str(train$dep_hour)
train$dep_hour=as.numeric(train$dep_hour)
train$dep_time_slot = ifelse(train$dep_hour < 5, "Pre_Morning", ifelse(train$dep_hour < 10,"Morning",ifelse(train$dep_hour < 17,"Day_Time",ifelse(train$dep_hour < 22,"Evening","Late_Night"))))
train$dep_time_slot=as.factor(train$dep_time_slot)
summary(train$dep_time_slot)
#Thus created the flight dep time slot by using the flight departure hour. Now, the flights are acheduled as "Day_Time     Evening  Late_Night     Morning Pre_Morning ".

#Boxplot of each dep_time_slot vs price:
ggplot(aes(y = train$Price, x = train$dep_time_slot, fill = train$Price), data = train) + geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title = "Boxplot of each level in dep_time_slot with price")

#Plotting mean price of each dep_time_slot: 
ggplot(train, aes(x=train$dep_time_slot, y=train$Price)) + stat_summary(fun.y="mean", geom="bar")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title = "Mean price of each airline")
#(Means are more or less same (good))


#Creating dep_day (date+month):
train$dep_day <- format(train$departure, "%d%b")


summary(train)

#dep->full form is departure*
#Related variables:
#duration --> Duration, dur, dur1,Arrival_Time
#dep_time_slot --> Dep_Time, departure, dep_hour
#departure --> Date_of_Journey, Dep_Time 
#dep_day --> departure, Date_of_Journey
#Here, Dep_Time can be explained by derived variable named dep_time_slot.
#Also, dep_day explains the date and month of departure.


#Creating a new dataframe:
train1=train


#Dropping some variable:
colnames(train1)
#Since "Total_Stops"  and "Route" variables are denoting same thing. I'm removing "Route" variable from the data:
train$Route=NULL 
#I have derived many variables. So removing old variables:
train1$Date_of_Journey=NULL 
train1$Dep_Time=NULL
train1$Arrival_Time=NULL
train1$Duration=NULL
train1$dur=NULL
train1$dep_hour=NULL


summary(train1)
str(train1)


###########################################################################################################################
#Data Visualization:

#Plotting frequecy count of each airline:
ggplot(train1,aes(x=train1$Airline,fill=train1$Airline))+
              geom_bar(position="dodge")+labs(title = "Counts of each Airline")+
              geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Plotting mean price of each airline:
ggplot(train1, aes(x=train1$Airline, y=train1$Price)) + stat_summary(fun.y="mean", geom="bar")+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title = "Mean price of each airline")

#Plotting mean price of each Source: 
ggplot(train1, aes(x=train1$Source, y=train1$Price)) + stat_summary(fun.y="mean", geom="bar")+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title = "Mean price of each Source")

#Plotting Source vs destination to count no.of flights:
ggplot(train1, aes(train1$Source, ..count..)) + geom_bar(aes(fill = train1$Destination), position = "dodge")

ggplot(train1,aes(x=train1$Source,fill=train1$Destination))+
              geom_bar(position="dodge")+labs(title = "Source vs Destination")+
              geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))

#It's clear that 
  #Banglore flights go only to Delhi
      #Chennai flights go only to Kolkata
          #Delhi flights go only to Cochin
              #Kolkata flights go only to Banglore
                  #Mumbai flights go only to Hyderabad.

#Plotting frequecy count of each airline vs source:
ggplot(train1,aes(x=train1$Airline,fill=train1$Source))+
              geom_bar(position="dodge")+labs(title = "Counts of each Airline from Source")+
              geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Plotting mean price Vs Total_Stops:
ggplot(train1, aes(x=train1$Total_Stops, y=train1$Price)) + stat_summary(fun.y="mean", geom="bar")+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title = "Mean price vs total stops")

#Plotting mean price Vs duration:
ggplot(train1, aes(x=train1$duration, y=train1$Price)) + stat_summary(fun.y="mean", geom="bar")+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title = "Mean price vs duration")

#Plotting duration vs price: #important plot 
ggplot(train1, aes(x = train1$duration, y = train1$Price)) + geom_point()+ geom_smooth(method = "lm") 

#Plotting mean price of each dep_time_slot: 
ggplot(train1, aes(x=train1$dep_time_slot, y=train1$Price)) + stat_summary(fun.y="mean", geom="bar")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title = "Mean price vs dep_time_slot")
#(Means are more or less same (good))

#Plotting frequency count of each airline vs dep_time_slot:
ggplot(train1,aes(x=train1$Airline,fill=train1$dep_time_slot))+
  geom_bar(position="dodge")+labs(title = "Counts of each Airline vs dep_time_slot")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Plotting frequency count of each Additional Info:
ggplot(train,aes(x=train1$Additional_Info,fill=train1$Additional_Info))+
  geom_bar(position="dodge")+labs(title = "Counts of each Additional Info")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot(train1$departure,train1$Price)

#Plotting frequency count of each dep_day:
ggplot(train,aes(x=train1$dep_day,fill=train1$dep_day))+
  geom_bar(position="dodge")+labs(title = "Counts of each dep_day")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Plotting mean price of each dep_day: 
ggplot(train1, aes(x=train1$dep_day, y=train1$Price)) + stat_summary(fun.y="mean", geom="bar")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title = "Mean price of each dep_day")


#With the understanding from the above visualizations, Dropping Some variables:

train1$Additional_Info=NULL
train1$dur1=NULL
train1$departure=NULL
train1$Route=NULL
###########################################################################################################################


#Creating Dummy variables for categorical data:
#install.packages("fastDummies")
library(fastDummies)
colnames(train1)
train2=dummy_cols(train1, select_columns = c("Airline","Source","Destination","Total_Stops","dep_time_slot","dep_day" ),
                remove_first_dummy = TRUE)
unique(train2$Total_Stops)


train3=train2
colnames(train3)


#Again removing the original variables, since I have created the dummy variables.
train3=train3[, -c(1:4)]
train3=train3[,-c(3,4)]


summary(train3)
str(train3)
dim(train3)
sum(is.na(train3))
train3=na.omit(train3)
#Thus train data is ready for modelling.


###########################################################################################################################
#TEST DATA: 
#(I'm doing data preparations on test data seperately, because in real time we will get test data(new) seperately after dealing with train & modelling)

data_test=read_excel("C:/Users/Ranjith P/Desktop/Flight Data/Test_set.xlsx")


#Doing same data prepartions (as did on train data) on test data:
test=data_test
View(test)
summary(test)
str(test)
dim(test)


#Missing Values Analysis:
sum(is.na(test))
mv_test=data.frame(apply(test, 2, function(x){sum(is.na(x))}))
mv_test


#Variable 1:Airline:
unique(test$Airline)

#Plotting frequecy count of each airline in test:
ggplot(test,aes(x=test$Airline,fill=test$Airline))+
  geom_bar(position="dodge")+labs(title = "Counts of each Airline")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#True jet is not at all given in the test$Airline. Also, removing levels in test which I removed in train.
#Subset:
test=subset(test, test$Airline != "Vistara Premium economy" & test$Airline != "Jet Airways Business" & test$Airline  != "Multiple carriers Premium economy" )
unique(test$Airline)


#Variable 2: Date_of_Journey: 
unique(test$Date_of_Journey)
#Replacing same dates od different formats (1/03/2019 and 01/03/2019) to same format:
test$Date_of_Journey <- str_replace_all(test$Date_of_Journey, "01/03/2019",  "1/03/2019")
test$Date_of_Journey <- str_replace_all(test$Date_of_Journey, "03/03/2019",  "3/03/2019")
test$Date_of_Journey <- str_replace_all(test$Date_of_Journey, "06/03/2019",  "6/03/2019")
test$Date_of_Journey <- str_replace_all(test$Date_of_Journey, "09/03/2019",  "9/03/2019")
unique(test$Date_of_Journey)
str(test$Date_of_Journey)
test$Date_of_Journey=str_replace_all(test$Date_of_Journey, "[/]",  "-")
unique(test$Date_of_Journey)


#Variable 3: Source:
unique(test$Source)
#unique(train$Source) #same source in train & test.


#Variable 4: Destination:
unique(test$Destination)
#unique(train$Destination) #same destination in train & test.
#Replacing New Delhi to Delhi:
test$Destination=str_replace_all(test$Destination, "New Delhi",  "Delhi")
unique(test$Destination)


#Variable 5: Route:
unique(test$Route)   #Not Important variable. Since, Total_Stops variable explains the same.


#Variable 6: Dep_Time:
unique(test$Dep_Time)
#Combining Date_of_Journey & Dep_Time to new variable: #derived variable:
test$departure=paste(test$Date_of_Journey, test$Dep_Time, sep=' ') #paste date & time together.
test$departure=as.POSIXlt(test$departure, format = "%d-%m-%Y %H:%M")
test=test[ order(test$departure , decreasing = FALSE ),]
class(test$departure)


#Variable 7: Arrival_Time:
unique(test$Arrival_Time)
arrival_test=data.frame(str_split_fixed(test$Arrival_Time, " ", 2)) #just to see the variables seperately, I've formed this dataframe.
test=data.frame(test,arrival_test$X2)


#Variable 8 :Duration:
unique(test$Duration)
str(test$Duration)
#Duration is in categorical format. It has to be changed to numeric. 
#So, trying to remove the h and m from the variable.
test$dur=str_replace_all(test$Duration, "h ",  ".") #derived variable
test$dur=str_replace_all(test$dur, "m",  "")
test$dur=str_replace_all(test$dur, "h",  ".00")
class(test$dur)

test$dur1=hm(test$dur)
str(test$dur1)
sum(is.na(test$dur1))
class(test$dur1)
summary(test$dur1)

test$duration=round(as.duration(test$dur1)/dhours(1)) #important


#Variable 9: Total_Stops:
unique(test$Total_Stops)


#Variable 10: Additional Info:
unique(test$Additional_Info)


#Derived Variable:
#Extract the hour and day data from the request time
test$dep_hour <- format(test$departure, "%H")
#Creating morning, day, evening, night, midnight timestamp using dep_hour variable:
str(test$dep_hour)
test$dep_hour=as.numeric(test$dep_hour)
test$dep_time_slot = ifelse(test$dep_hour < 5, "Pre_Morning", ifelse(test$dep_hour < 10,"Morning",ifelse(test$dep_hour < 17,"Day_Time",ifelse(test$dep_hour < 22,"Evening","Late_Night"))))
test$dep_time_slot=as.factor(test$dep_time_slot)
summary(test$dep_time_slot)


#Creating dep_day (date+month):
test$dep_day <- format(test$departure, "%d%b")


summary(test)


test1=test


#Dropping some variable:
colnames(test1)
test1$Route=NULL
test1$Date_of_Journey=NULL
test1$Dep_Time=NULL
test1$Arrival_Time=NULL
test1$Duration=NULL
test1$dur=NULL
test1$dep_hour=NULL
test1$Additional_Info=NULL
test1$dur1=NULL
test1$departure=NULL


#Creating Dummy variables for categorical data:
#install.packages("fastDummies")
library(fastDummies)
colnames(train1)
test2=dummy_cols(test1, select_columns = c("Airline","Source","Destination","Total_Stops","dep_time_slot","dep_day" ),
                  remove_first_dummy = FALSE)


#Omitting the variables in test that are omitted in train during dummy variable creation:
test2$`Airline_Multiple carriers`=NULL
test2$Source_Delhi=NULL
test2$Destination_Cochin=NULL
test2$`Total_Stops_1 stop`=NULL
test2$dep_time_slot_Pre_Morning=NULL
test2$dep_day_01Mar=NULL


test3=test2
colnames(test3)


test3=test3[, -c(1:5)]
test3=test3[,-c(2,3)]


summary(test3)
str(test3)
dim(test3)
sum(is.na(test3))
test3=na.omit(test3)
#Thus formed the test data for testing.


colnames(train3)
colnames(test3)


##########################################################################################################################
#Modelling:
#LINEAR REGRESSION MODEL:

model1=lm(Price~.,data=train3)
summary(model1)

#Step AIC:
#stepAIC(model1)

model2=lm(Price ~ duration + `Airline_Air India` + Airline_IndiGo + 
            `Airline_Jet Airways` + Airline_SpiceJet + `Airline_Air Asia` + 
            Airline_Vistara + Airline_GoAir + Source_Banglore + Source_Mumbai + 
            Source_Kolkata + `Total_Stops_non-stop` + `Total_Stops_2 stops` + 
            `Total_Stops_3 stops` + dep_time_slot_Morning + dep_time_slot_Late_Night + 
            dep_day_03Mar + dep_day_06Mar + dep_day_09Mar + dep_day_12Mar + 
            dep_day_15Mar + dep_day_18Mar + dep_day_21Mar + dep_day_24Mar + 
            dep_day_27Mar + dep_day_01Apr + dep_day_03Apr + dep_day_06Apr + 
            dep_day_09Apr + dep_day_12Apr + dep_day_15Apr + dep_day_18Apr + 
            dep_day_21Apr + dep_day_24Apr + dep_day_27Apr + dep_day_01May + 
            dep_day_03May + dep_day_06May + dep_day_09May + dep_day_12May + 
            dep_day_15May + dep_day_18May + dep_day_21May + dep_day_24May + 
            dep_day_27May + dep_day_01Jun + dep_day_03Jun + dep_day_06Jun + 
            dep_day_09Jun + dep_day_12Jun + dep_day_15Jun + dep_day_18Jun + 
            dep_day_21Jun + dep_day_24Jun + dep_day_27Jun, data = train3)

summary(model2)
library(car)
vif(model2) #all variables in model2 have less vif. So, no multicollinearity.
model2$coefficients


#Evaluating Linear Regression model:
mape=function(y,yhat){
  mean(abs((y-yhat)/y))
}

mape(train3$Price,model2$fitted.values)


library(Metrics)
rmse(train3$Price,model2$fitted.values)


#Testing using LM:
x=train3[,2:64]
y=train3[,1]
x_test=test3


pred1=predict(model2,x_test)
pred1


#TOP MOST FEATURES IMPACTING THE FLIGHT PRICE ARE (FROM LINEAR REGRESSION MODEL);
summary(model2)
model2$coefficients
#Variable Importance:
important=as.data.frame(varImp(model2, scale = FALSE))
important=data.frame(overall = important$Overall, features = rownames(important))
important=important[order(important$overall,decreasing = TRUE),]


##########################################################################################################################

#CROSS VALIDATION:
set.seed(100)
# Define train control for k fold cross validation
ctrl<-trainControl(method='cv',number = 10)


model_cv<-train(Price ~ ., data = train3, method ='lm',trControl = ctrl,metric='Rsquared')
summary(model_cv)


pred2=predict(model_cv,x_test)
pred2


###########################################################################################################################

#REGULARISATION:
library(glmnet)
library(ISLR)
library(dplyr)
library(tidyr)
library(Metrics)


set.seed(100)
train_x=as.matrix(train3[,2:64])
train_y=as.matrix(train3[,1])
test_x=as.matrix(test3)


custom=trainControl(method='repeatedcv',number=10,repeats=5,verboseIter=TRUE)


#RIDGE REGRESSION:
ridge=train(Price~.,train3,method='glmnet',tuneGrid=expand.grid(alpha=0,lambda=seq(0.001,1,length=5)),
            trControl=custom)

plot(ridge)

ridge

plot(ridge$finalModel,xvar = 'lambda',label=T)

plot(ridge$finalModel,xvar = 'dev',label=T)

plot(varImp(ridge,scale = T))

pred3=predict(ridge,test_x)
pred3


#LASSO REGRESSION:
set.seed(100)

lasso=train(Price~.,train3,method='glmnet',tuneGrid=expand.grid(alpha=1,lambda=seq(0.001,0.5,length=5)),
            trControl=custom)
plot(lasso)

lasso

plot(lasso$finalModel,xvar = 'lambda',label=T)

plot(lasso$finalModel,xvar = 'dev',label=T)

plot(varImp(ridge,scale = T))

pred4=predict(lasso,test_x)
pred4


#ELASTIC NET REGRESSION:
set.seed(100)
elastic=train(Price~.,train3,method='glmnet',tuneGrid=expand.grid(alpha=seq(0,1,length=10),lambda=seq(0.0001,1,length=5)),
            trControl=custom)

plot(elastic)

elastic

plot(elastic$finalModel,xvar = 'lambda',label=T)

plot(elastic$finalModel,xvar = 'dev',label=T)

plot(varImp(elastic,scale = T))

pred5=predict(elastic,test_x)
pred5


#Comparision of Regularisation techniques:
model_list=list(Ridge=ridge,Lasso=lasso,Elasticnet=elastic)
result=resamples(model_list)
summary(result)
bwplot(result)



###########################################################################################################################

#Best model: Finalising elastic net model based on summary of result.
elastic$bestTune
best=elastic$finalModel
coef(best,s=elastic$bestTune$lambda) #Coefficients from elastic net model.


#Prediction using elastic net model:
Predicted_Price=pred5
prediction_flight_fares=data.frame(test3,Predicted_Price)


#Ranking of variables that impact the flight fares:
plot(varImp(elastic,scale = T))


write.csv(Predicted_Price,"Prediction_Output.csv",row.names=F)
###########################################################################################################################

#Thank you...
