#Association rule minning for each Airline 
#Using Association rule modeling to refine to the existing model
#convert useful column into factors
levelit <- function(v){
  new.v <- v
  level4 <- quantile(v[v>0],probs = c(0.3,0.7))
  new.v[v<level4[1]] <- "Low"
  new.v[level4[1]<=v & v<level4[2]] <- "Average"
  new.v[level4[2]<=v & v<level4[3]] <- "High"
  new.v <- factor(new.v)
  return(new.v)
}
#Airdata_new <- Airdata

normit <- function(Airdata_new,Airdata){
Airdata_new$Satisfaction <- "Average"
Airdata_new$Satisfaction[Airdata$Satisfaction>3.5] <- "High"
Airdata_new$Satisfaction[Airdata$Satisfaction<3] <- "Low"

for (i in c(3,5,6,7,9,10,11,18,19,20,21)){
  Airdata_new[,i] <- levelit(Airdata_new[,i])
}

for (i in 1:21){
  Airdata_new[,i] <- factor(Airdata_new[,i])
}
table(Airdata_new$Satisfaction)
return(Airdata_new)
}
#Southeast Airlines
Southeast_new <- Southeast
Southeast_new <- normit(Southeast_new,Southeast)
Southeast_new <- Southeast_new[-14:-15]
table(Southeast_new$Satisfaction)

install.packages("arules")
library(arules) 

#Southeast Airlines - Applying arules
newSoutheastSurvey <- as(Southeast_new,"transactions")
inspect(head(newSoutheastSurvey,10))
itemFrequency(newSoutheastSurvey)
itemFrequencyPlot(newSoutheastSurvey,paramter = list(confidence = 0.9))

Southeast.result_Sati <- apriori(newSoutheastSurvey)#,parameter = list(confidence=0.9)

Southeast.result_Sati_3 <- apriori(newSoutheastSurvey,parameter = list(confidence=0.6),appearance = list(default="lhs",rhs="Satisfaction=High"))
summary(Southeast.result_Sati_3)
result_Sati.new_3 <- Southeast.result_Sati_3[order(-quality(Southeast.result_Sati_3)$lift),]
inspect(head(result_Sati.new_3,15))

Southeast.result_Sati_2 <- apriori(newSoutheastSurvey,appearance = list(rhs="Satisfaction=Average"))
summary(Southeast.result_Sati_2)
Southeast.result_Sati.new_2 <- Southeast.result_Sati_2[order(-quality(Southeast.result_Sati_2)$lift),]
inspect(Southeast.result_Sati.new_2)

Southeast.result_Sati_1 <- apriori(newSoutheastSurvey,parameter = list(confidence=0.3),appearance = list(default="lhs",rhs="Satisfaction=Low"))
summary(Southeast.result_Sati_1)
Southeast.result_Sati.new_1 <- Southeast.result_Sati_1[order(-quality(Southeast.result_Sati_1)$lift),]
inspect(Southeast.result_Sati.new_1)






#Association rules for airlines with blue status and Personal travel
new.air.blue.1 <- new.air.blue
new.air.blue.1 <- normit(new.air.blue.1,new.air.blue)

new.air.blue.1$Satisfaction <- "Average"
new.air.blue.1$Satisfaction[new.air.blue$Satisfaction>3.5] <- "High"
new.air.blue.1$Satisfaction[new.air.blue$Satisfaction<3] <- "Low"

for (i in c(3,5,6,7,8,9,10,17,18,19,20)){
  new.air.blue.1[,i] <- levelit(new.air.blue.1[,i])
}

for (i in 1:21){
  new.air.blue.1[,i] <- factor(new.air.blue.1[,i])
}



newBlueSurvey <- as(new.air.blue.1,"transactions")

Blue.result_Sati_3 <- apriori(newBlueSurvey,parameter = list(confidence=0.1),appearance = list(default="lhs",rhs="Satisfaction=High"))
summary(Blue.result_Sati_3)
Blue.result_Sati.new_3 <- Blue.result_Sati_3[order(-quality(Blue.result_Sati_3)$lift),]
inspect(head(Blue.result_Sati.new_3,15))

Southeast.result_Sati_2 <- apriori(newSoutheastSurvey,appearance = list(rhs="Satisfaction=Average"))
summary(Southeast.result_Sati_2)
Southeast.result_Sati.new_2 <- Southeast.result_Sati_2[order(-quality(Southeast.result_Sati_2)$lift),]
inspect(Southeast.result_Sati.new_2)

Blue.result_Sati_1 <- apriori(newBlueSurvey,parameter = list(confidence=0.6),appearance = list(default="lhs",rhs="Satisfaction=Low"))
summary(Blue.result_Sati_1)
Blue.result_Sati.new_1 <- Blue.result_Sati_1[order(-quality(Blue.result_Sati_1)$lift),]
inspect(head(Blue.result_Sati.new_1,15))
