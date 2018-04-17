library(rpart)
library(rpart.plot)
library(xtable)

load("~/R/MLDeathPenalty.rdata") #loading dataset
copyDP = DeathPenalty
copyDP = subset(copyDP, select = -c(vehicle))
copyDP = na.omit(copyDP)
copyDP$education = as.factor(copyDP$education)
levels(copyDP$education) = c("HS Degree", "No degree", "Other")
copyDP$death = as.factor(copyDP$death)
levels(copyDP$death) = c("noDS", "DS")


regTree = rpart(death~. , data = copyDP, cp = 0.01)
rpart.plot(regTree)

regTree = rpart(death~., data = copyDP, parms=list(loss=matrix(c(0,3,1,0),2,2,byrow=TRUE)), cp = .01)
rpart.plot(regTree, extra = 1)

pred = predict(regTree, copyDP, type = "class")
table(copyDP$death, pred)
