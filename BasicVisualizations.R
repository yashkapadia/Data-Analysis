
#Prepaing some Basic Visualizations
hist(Airdata$Satisfaction, breaks=1:5, col=c(rep("red",2), rep("yellow",1), rep("green", 2)))

install.packages("igraph")
library(igraph)
plot(result_Sati.new_1, method = "matrix", measure = "lift",control = list(type ="items"))
plot(result_Sati.new_1, measure = c("support", "lift"), shading = "confidence")
plot(result_Sati.new_1, method = "two-key plot")

#final plot for unsatisfied customers
library(arulesViz)
plot(result_Sati.new_1, method = "paracoord")
plot(result_Sati.new_1, method = "paracoord", control = list(reorder = TRUE))

#final plot for satisfied customers
result_Sati.new_3.top <- head(result_Sati.new_3,15)
plot(result_Sati.new_3.top, method = "paracoord")

plot(result_Sati.new_3.top, method ="grouped", control = list(k = 50,gpar()))
inspect(result_Sati.new_3.top)



Airdata$Airline.Status <- trimws(Airdata$Airline.Status)
Airdata$Airline.Status <- as.factor(Airdata$Airline.Status)
new.air.blue <- Airdata[Airdata$Airline.Status=="Blue"& Airdata$Type.of.Travel=="Personal Travel",]

new.air.business <- Airdata[Airdata$Type.of.Travel=="Business travel",]
Airdata$Type.of.Travel<- trimws(Airdata$Type.of.Travel)
Airdata$Type.of.Travel<- as.factor(Airdata$Type.of.Travel)

new.air.blue <- new.air.blue[-2,-8]
