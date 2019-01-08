dataset <- read.csv(file = "Satisfaction Survey.csv",na.strings = "",sep =",")
View(dataset)
str(dataset)
summary(dataset)

#converting all integer columns to numeric 
dataset$Age <- as.numeric(dataset$Age)
dataset$Flight.time.in.minutes <- as.numeric(dataset$Flight.time.in.minutes)
dataset$Day.of.Month <- as.numeric(dataset$Day.of.Month)
dataset$Flight.Distance <- as.numeric(dataset$Flight.Distance)
dataset[5:8] <- lapply(dataset[5:8],as.numeric)
dataset[10:12] <- lapply(dataset[10:12],as.numeric)
dataset[22:24] <- lapply(dataset[10:12],as.numeric)

#checking for any rows which are not complete
sum(!complete.cases(dataset)) 

ncol(dataset)
nrow(dataset)

#Taking only the data whose flights are not cancelled
Airdata <- dataset[dataset$Flight.cancelled=="No",]
nrow(Airdata)
str(Airdata)

#Airdata <- data.frame(lapply(Airdata, trimws), stringsAsFactors = FALSE)
#str(Airdata)

#checking for any rows which are not complete for the new dataset
sum(!complete.cases(Airdata))
sum(is.na(Airdata$Arrival.Delay.in.Minutes))
sum(is.na(Airdata$Flight.time.in.minutes))
sum(is.na(Airdata$Departure.Delay.in.Minutes))

is.null(Airdata)
View(Airdata)
nrow(Airdata)
nrow(dataset)
str(Airdata)

#converting all integer columns to numeric 
Airdata$Age <- as.numeric(Airdata$Age)
Airdata$Flight.time.in.minutes <- as.numeric(Airdata$Flight.time.in.minutes)
Airdata$Day.of.Month <- as.numeric(Airdata$Day.of.Month)
Airdata$Flight.Distance <- as.numeric(Airdata$Flight.Distance)
Airdata[5:8] <- lapply(Airdata[5:8],as.numeric)
Airdata[10:12] <- lapply(Airdata[10:12],as.numeric)
Airdata[22:24] <- lapply(Airdata[10:12],as.numeric)
View(Airdata)

#replacing na values with mean
for(i in 1:ncol(Airdata)){
  Airdata[is.na(Airdata[,i]), i] <- mean(Airdata[,i], na.rm = TRUE)
}

#Validating that the data is cleaned
sum(!complete.cases(Airdata))

is.null(Airdata)
View(Airdata)
nrow(Airdata)
nrow(dataset)
str(Airdata)

#removing unwanted columns from the Survey dataset
Airdata <- Airdata[-c(6,14,19,21,25,27,28)]
str(Airdata)
summary(Airdata)
View(Airdata)

#Converting Satisfaction of Airdata for nps 
Airdata$Satisfaction <- as.numeric(Airdata$Satisfaction)

#Converting the Satisfaction Column to numeric - Run this line only when required
Airdata$Satisfaction <- as.numeric(as.character(Airdata$Satisfaction))
str(Airdata)

#Airdata$Satisfaction <- as.factor(Airdata$Satisfaction)

#Applying Linear Modelling for the entire data
Air.model <- lm(formula = Satisfaction ~ .,data = Airdata)
summary(Air.model)

Air.model1 <-lm(formula = Satisfaction ~ Age + Price.Sensitivity + Gender + Type.of.Travel+Airline.Status+X..of.Flight.with.other.Airlines+Type.of.Travel+No..of.other.Loyalty.Cards+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Class+Scheduled.Departure.Hour+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes   , data = Airdata)
summary(Air.model1)

Air.model2 <-lm(formula =Satisfaction ~ Age+Price.Sensitivity+Gender+Type.of.Travel+Airline.Status+No..of.other.Loyalty.Cards,data = Airdata)
summary(Air.model2)

Air.model3 <- lm(formula = Satisfaction ~ Airline.Status + Type.of.Travel, data = Airdata)
summary(Air.model3)

#Using the Net promoter score
install.packages("NPS")
library(NPS)

Airdata.NPS <- nps(Airdata$Satisfaction,breaks = list(0:2.5,3,3.5:5))
Airdata.NPS

unique(Airdata$Airline.Name)

  Airdata$Airline.Name <- trimws(Airdata$Airline.Name)
  Airdata$Airline.Name <- as.factor(Airdata$Airline.Name)
View(Airdata$Airline.Name)
str(Airdata)

Southeast <- Airdata[Airdata$Airline.Name=="Southeast Airlines Co.",]
nrow(Southeast)
View(Southeast)
str(Southeast)
Southeast.NPS <- nps(Southeast$Satisfaction)
Southeast.NPS

#1
EnjoyFlying <- Airdata[Airdata$Airline.Name=="EnjoyFlying Air Services",]
nrow(EnjoyFlying)
EnjoyFlying.NPS <- nps(EnjoyFlying$Satisfaction)
EnjoyFlying.NPS

#2
FlyFast <- Airdata[Airdata$Airline.Name=="FlyFast Airways Inc.",]
nrow(FlyFast)
FlyFast.NPS <- nps(FlyFast$Satisfaction)
FlyFast.NPS

#3
Cheapseats <- Airdata[Airdata$Airline.Name=="Cheapseats Airlines Inc.",]
nrow(Cheapseats)
CheapSeats.NPS <- nps(Cheapseats$Satisfaction,breaks = list(0:1.5, 2:2.5, 3.5:5) )
CheapSeats.NPS

#4
FlyHere <- Airdata[Airdata$Airline.Name=="FlyHere Airways",]
nrow(FlyHere)
FlyHere.NPS <- nps(FlyHere$Satisfaction,breaks = list(0:1.5, 2:2.5, 3.5:5) )
FlyHere.NPS

#5
FlyToSun <- Airdata[Airdata$Airline.Name=="FlyToSun Airlines Inc.",]
nrow(FlyToSun)
FlyToSun.NPS <- nps(FlyToSun$Satisfaction, breaks = list(0:1.5, 2:2.5, 3.5:5))
FlyToSun.NPS

#6
GoingNorth <- Airdata[Airdata$Airline.Name=="GoingNorth Airlines Inc.",]
nrow(GoingNorth)
GoingNorth.NPS <- nps(GoingNorth$Satisfaction, breaks = list(0:1.5, 2:2.5, 3.5:5))
GoingNorth.NPS

#7
WestAirways<-Airdata[Airdata$Airline.Name=="West Airways Inc.",]
nrow(WestAirways)
WestAirways.NPS <- nps(WestAirways$Satisfaction, breaks = list(0:1.5, 2:2.5, 3.5:5))
WestAirways.NPS 

#8
OnlyJets <- Airdata[Airdata$Airline.Name=="OnlyJets Airlines Inc.",]
nrow(OnlyJets)
OnlyJets.NPS <- nps(OnlyJets$Satisfaction, breaks = list(0:1.5, 2:2.5, 3.5:5))
OnlyJets.NPS

#9
NorthWestBusiness <- Airdata[Airdata$Airline.Name=="Northwest Business Airlines Inc.",]
nrow(NorthWestBusiness)
NorthWestBusiness.NPS <- nps(NorthWestBusiness$Satisfaction, breaks = list(0:1.5, 2:2.5, 3.5:5))
NorthWestBusiness.NPS

#10
Oursin <- Airdata[Airdata$Airline.Name=="Oursin Airlines Inc.",]
nrow(Oursin)
Oursin.NPS <- nps(Oursin$Satisfaction, breaks = list(0:1.5, 2:2.5, 3.5:5))
Oursin.NPS

#11
PaulSmith <- Airdata[Airdata$Airline.Name=="Paul Smith Airlines Inc.",]
nrow(PaulSmith)
PaulSmith.NPS<- nps(PaulSmith$Satisfaction, breaks = list(0:1.5, 2:2.5, 3.5:5))
PaulSmith.NPS

#12
Sigma <- Airdata[Airdata$Airline.Name=="Sigma Airlines Inc.",]
nrow(Sigma)
Sigma.NPS <- nps(Sigma$Satisfaction, breaks = list(0:1.5, 2:2.5, 3.5:5))
Sigma.NPS

#13
CoolYoung <- Airdata[Airdata$Airline.Name=="Cool&Young Airlines Inc.",]
nrow(CoolYoung)
CoolYoung.NPS <- nps(CoolYoung$Satisfaction, breaks = list(0:1.5, 2:2.5, 3.5:5))
CoolYoung.NPS

# We can compare the number of rows for each airlines and their nps value
# The Airline with lower nps value and higher number of rows can be used for analysis and we can run our models on it.


#Trying some new things
install.packages("plyr")
library(plyr)

count(Airdata$Satisfaction)

#tabluating the promoters detractors etc using npc method
table(Airdata$Satisfaction,npc(Airdata$Satisfaction))


#Cleaning of the data removing factors of less then
CoolYoung <- CoolYoung[-15]
str(CoolYoung)
CoolYoung <- CoolYoung[-14]


#Applying Linear Modelling for the Airlines
CoolYoung.model<- lm(formula = Satisfaction ~ .,data = CoolYoung)
summary(CoolYoung.model)

CoolYoung.model1 <- lm(formula = Satisfaction ~ Airline.Status + Type.of.Travel + Gender + Age + No.of.Flights.p.a. + Flight.date, data = CoolYoung)
summary(CoolYoung.model1)

Cheapseats <- Cheapseats[-14:-15]
Cheapseats.model <- lm(formula = Satisfaction ~ ., data = Cheapseats)
summary(Cheapseats.model)


Cheapseats.model2 <- lm(formula = Satisfaction ~ Type.of.Travel + Price.Sensitivity + Age + Gender + Class + Flight.date + No..of.other.Loyalty.Cards, data = Cheapseats)
summary(Cheapseats.model2)
