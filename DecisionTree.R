str(Airdata)
install.packages("party")
library(party)
Airdata <- na.omit(Airdata)
dt1 <- ctree(Satisfaction ~ Age + Airline.Status + Type.of.Travel, data = Airdata)
dt2 <- ctree(Satisfaction ~ Gender + Price.Sensitivity + X..of.Flight.with.other.Airlines, data = Airdata)
print(dt1)
plot(dt1)

plot(dt2)

install.packages("rpart")
library(rpart)

dt3 <- rpart(Satisfaction ~ Age + Airline.Status + Type.of.Travel,method = "anova",data = Airdata)
printcp(dt3)

plot(dt3, uniform=TRUE)
text(dt3, use.n=TRUE, all=TRUE, cex=1)
summary(dt3)


install.packages("randomForest")
library(randomForest)
install.packages("reprtree")
library(reprtree)

install.packages("devtools")
library(devtools)
devtools::install_github('araastat/reprtree')
library(reprtree)


rf <- randomForest(Satisfaction ~ Airline.Status + Type.of.Travel, data=Airdata )
importance(rf)
print(rf)
plot(rf)
plot.getTree(rf)
plot(getTree(rf,k=400,labelVar = FALSE))

reprtree:::plot.getTree(rf)
reprtree:::plot.getTree(rf,k=400)
 
rf1 <- randomForest(Satisfaction ~ Age + Gender, data=Airdata )
reprtree:::plot.getTree(rf1)

rf2 <- randomForest(Satisfaction ~ Type.of.Travel + Gender, data=Airdata )
reprtree:::plot.getTree(rf2)

rf3 <- randomForest(Satisfaction ~ Type.of.Travel + Class, data=Airdata)
reprtree:::plot.getTree(rf3)

rf4 <- randomForest(Satisfaction ~ Age + Shopping.Amount.at.Airport, data=Airdata)
reprtree:::plot.getTree(rf4)
