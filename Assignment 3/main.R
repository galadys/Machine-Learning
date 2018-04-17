library(randomForest)
library(ggplot2)
library(xtable)
load("C:/Users/loren/Desktop/Machine Learning/Assignment 3/ReefCheck974.rdata")
#Renaming factors
levels(ReefCheck$Ocean)[levels(ReefCheck$Ocean) == ""] <-"unknown"
levels(ReefCheck$Storms)[levels(ReefCheck$Storms) == "none"] <-"unknown"
levels(ReefCheck$Storms)[levels(ReefCheck$Storms) == "y"] <-"yes"
levels(ReefCheck$HumanImpact)[levels(ReefCheck$HumanImpact) == ""] <-"none"
levels(ReefCheck$Storms)[levels(ReefCheck$Storms) == "unknown"] <-"no"
levels(ReefCheck$Siltation)[levels(ReefCheck$Siltation) == "unknown"] <-"never"
levels(ReefCheck$Siltation)[levels(ReefCheck$Siltation) == "Occasionally"] <-"occasionally"

levels(ReefCheck$Dynamite)[levels(ReefCheck$Dynamite) == ""] <-"none"
levels(ReefCheck$Poison)[levels(ReefCheck$Poison) == ""] <-"none"
levels(ReefCheck$Sewage)[levels(ReefCheck$Sewage) == ""] <-"none"
levels(ReefCheck$Sewage)[levels(ReefCheck$Sewage) == "k"] <-"moderate"

levels(ReefCheck$Industrial)[levels(ReefCheck$Industrial) == ""] <-"none"
levels(ReefCheck$Commercial)[levels(ReefCheck$Commercial) == ""] <-"none"

levels(ReefCheck$HumanImpact)[levels(ReefCheck$HumanImpact) == "unknown"] <-"none"
levels(ReefCheck$Poison)[levels(ReefCheck$Poison) == "unknown"] <-"none"

ReefCheck$Sewage = factor(ReefCheck$Sewage, levels = c("none", "low", "moderate", "high")) #Where applicable, I change the order to reflect the severity. 
ReefCheck$HumanImpact = factor(ReefCheck$HumanImpact, levels = c("none", "low", "moderate", "high"))
ReefCheck$Siltation = factor(ReefCheck$Siltation, levels = c("never", "occasionally", "often", "always"))
ReefCheck$Dynamite = factor(ReefCheck$Dynamite, levels = c("none", "prior", "low", "moderate", "high"))#why prior?
ReefCheck$Poison = factor(ReefCheck$Poison, levels = c("none", "low", "moderate", "high"))
ReefCheck$Industrial = factor(ReefCheck$Industrial, levels = c("none", "low", "moderate", "high"))
ReefCheck$Commercial = factor(ReefCheck$Commercial, levels = c("none", "low", "moderate", "high"))

save(ReefCheck, file ="CleanedReefCheck")

x = ReefCheck$Ocean[-which(ReefCheck$Ocean == "unknown")]
levels(x)[levels(x) == "unknown"] <- "Indian" #no unknown observations, can choose any level to drop it in to.
barplot(prop.table(table(x)), main = "Reef Location")


ggplot(ReefCheck, aes(Depth))+
  geom_histogram(col="grey", fill ="grey65", bins = 15)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Depth of Reefs (meters)")

ggplot(ReefCheck, aes(Year))+
  geom_density(col="grey", fill ="grey65")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Year of Reef Data Observations")

par(mfrow=c(2,2))
barplot(prop.table(table(ReefCheck$HumanImpact)), main = "Human Impact on Reefs")
barplot(prop.table(table(ReefCheck$Siltation)), main = "Siltation of Reefs" ) #why so often never?
barplot(prop.table(table(ReefCheck$Poison)), main = "Presence of Poison")
barplot(prop.table(table(ReefCheck$Sewage)), main = "Presence of Sewage") #why so often never?
par(mfrow=c(1,1))

#ReefCopy = ReefCheck
#ReefCopy$BleachingStorms = with(ReefCopy, interaction(ReefCopy$Bleaching, ReefCopy$Storms))
#ggplot(ReefCopy, aes(BleachingStorms))+ geom_bar(col="grey", fill ="grey65")+ theme(plot.title = element_text(hjust = 0.5))+  labs(title="Bleaching vs Storms")

#Bivariate analysis insight: Consider the conditional distribution of all independent variables, conditional on bleaching being true. 
#Are the statistics of independent variables of this conditional distribution different than when bleaching is false?
ReefNo = ReefCheck[which(ReefCheck$Bleaching == "No"),]
ReefYes = ReefCheck[which(ReefCheck$Bleaching == "Yes"),]

x1 = ReefYes$Ocean[-which(ReefCheck$Ocean == "unknown")]
levels(x1)[levels(x1) == "unknown"] <- "Indian"
x2 = ReefNo$Ocean[-which(ReefCheck$Ocean == "unknown")]
levels(x2)[levels(x2) == "unknown"] <- "Indian"

par(mfrow=c(2,1))
barplot(prop.table(table(x1)), main = "Location of Bleached Reefs")
barplot(prop.table(table(x2)), main = "Location of Healthy Reefs")
par(mfrow=c(1,1))


ggplot(ReefCheck, aes(Depth))+
  geom_density(col="grey", fill ="grey65")+
  facet_grid(Bleaching ~.)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Depth of Reefs - Conditional on Reef Bleaching")

ggplot(ReefCheck, aes(Year))+
  geom_density(col="grey", fill ="grey65")+
  facet_grid(Bleaching ~.)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Year of Reefs - Conditional on Reef Bleaching") #strange???

par(mfrow=c(3,4))
barplot(prop.table(table(ReefYes$Storms)), main = "Bleached-Storms") #no noticeable difference
barplot(prop.table(table(ReefNo$Storms)), main = "Healthy-Storms")
barplot(prop.table(table(ReefYes$HumanImpact)), main = "Bleached-HImpact") #in cases where there was higher levels of human impact, it's more likely to have had bleaching.
barplot(prop.table(table(ReefNo$HumanImpact)), main = "Healthy-HImpact")
barplot(prop.table(table(ReefYes$Siltation)), main = "Bleached-Silt") #very interesting! It seems that reefs that underwent bleaching never had siltation. No SM knowledge to understand this.
barplot(prop.table(table(ReefNo$Siltation)), main = "Healthy-Silt")
barplot(prop.table(table(ReefYes$Sewage)), main = "Bleached-Sewage") #Again, note that in the case of no sewage, bleaching is less likely.
barplot(prop.table(table(ReefNo$Sewage)), main = "Healthy-Sewage")
barplot(prop.table(table(ReefYes$Commercial)), main = "Bleached-Commerce") #interesting. Correlation between bleaching and no commercial activity near the reefs. 
barplot(prop.table(table(ReefNo$Commercial)), main = "Healthy-Commerce")
par(mfrow=c(1,1))




Index<-sample(1:12392, 9913, replace=FALSE)
Test <- ReefCheck[-Index,]
Train <- ReefCheck[Index,]
  

rf <- randomForest(Bleaching ~., data = Train, importance = T, sampsize = c(1150,187)) #Need to tweak the cost ratio.
print(rf)

rfPredict <- predict(rf, newdata = Test)
table(Test$Bleaching, rfPredict)

varImpPlot(rf,main="Important variables for Coral Reef Bleaching")
par(mfrow=c(3,1))
partialPlot(rf, Train, Year)
partialPlot(rf, Train, Depth)
#partialPlot(rf, Train, Ocean)
partialPlot(rf, Train, HumanImpact)
#partialPlot(rf, Train, Sewage)


