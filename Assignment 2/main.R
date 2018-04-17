library(rpart)
library(rpart.plot)
library(ipred)
library(xtable)

load("~/R/MLDeathPenalty.rdata") #loading dataset

summary(DeathPenalty)
copy = na.omit(DeathPenalty)

require(Amelia)
missmap(DeathPenalty, main="Death Penalty Data - Missings Map", col=c("red", "green"),
        legend=FALSE)
#library(mice)
#tempData <- mice(DeathPenalty,m=5,maxit=50,meth='pmm',seed=500)

missing = is.na.data.frame(DeathPenalty[,c(1,2,3,4,5,7)])
missing = as.data.frame(missing)
whichMissing = which((missing$black | missing$white | missing$death | missing$gender | missing$hisp | missing$birthplace) != FALSE)

copyDP = DeathPenalty[-whichMissing,]
missmap(copyDP, main="Death Penalty Data - Missings Map", col=c("red", "green"),
        legend=FALSE)
copyDP = subset(copyDP, select = -c(vehicle))

copyDP = na.omit(copyDP)
missmap(copyDP, main="Death Penalty Data - Missings Map", col=c("red", "green"),
        legend=FALSE)
copyDP$education = as.factor(copyDP$education)
levels(copyDP$education) = c("HS Degree", "No degree", "Other")


x= rep(0,30)

for (i in c(1:30)){
  x[i] = length(unique(copyDP[,i]))
}
#confirms that all variables are binary except 2. 

factors = which(x == 2)
factorCopy = copyDP
factorCopy[,factors] <- lapply(factorCopy[,factors], factor)
summary(factorCopy)
#for nice univariate summary statistics

proportions = rep(0,ncol(factorCopy))

for (i in 1:length(proportions)){
  proportions[i] = substr(as.character(summary(factorCopy)[2,i]), start =3, stop = 6)
}
proportions = as.integer(na.omit((as.integer(proportions))))/nrow(factorCopy)
#proportions for binary variables
namesList = names(factorCopy)
namesList = namesList[-c(6, 23)]
props = data.frame(proportions, namesList)
#proportions
xtable(props)

library(ggplot2)

ggplot(copyDP, aes(numbervictim))+
  geom_histogram(col="grey", fill ="grey65")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Number of Victims")

ggplot(copyDP, aes(education))+
  geom_bar()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Education")


factorCopy$educationDeath = rep(0,nrow(factorCopy))
levels(factorCopy$death) = c("no.death", "death")

factorCopy$educationDeath = with(factorCopy, interaction(factorCopy$death, factorCopy$education))
xtable(table(factorCopy$educationDeath))

ggplot(factorCopy, aes(educationDeath))+
  geom_bar(col="grey", fill ="grey65")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Education versus Death Sentence")

#death proportions: 31.8% HS degree,  29.5% no HS degree, 19.7% other

factorCopy$manyVictims = factor(ifelse(factorCopy$numbervictim == 1, "1victim", "manyvictims"))
factorCopy$numbervictimDeath = with(factorCopy, interaction(factorCopy$death, factorCopy$manyVictims))

ggplot(factorCopy, aes(numbervictimDeath))+
  geom_bar(col="grey", fill ="grey65")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Number of Victims versus Death Sentence")

factorCopy$numbervictimDeath2 = with(factorCopy, interaction(factorCopy$death, factorCopy$numbervictim))
nvictimDeathProb = c(79/(312+79), 21/(65+21), 22/(41+22), 3/(2+3), 5/(9+5), 6/6, 3/(4+3), 3/(6+3), 7/(3+7), 5/(5+1))
victimcount = c(1,2,3,4,5,6,7,9,10,14)
plot(victimcount, nvictimDeathProb, pch = 16, main = "Victim Count versus Death Sentence Probability", xlab = "Victim Count", ylab = "Death Sentence Frequency")
abline(lm(nvictimDeathProb ~ victimcount))

levels(factorCopy$black) = c("notBlack", "Black")
levels(factorCopy$vwhite) = c("notwhiteV", "whiteV")
factorCopy$BlackWhite = with(factorCopy, interaction(factorCopy$black, factorCopy$vwhite, factorCopy$death))
xtable(table(factorCopy$BlackWhite))

levels(factorCopy$hisp) = c("notHisp", "Hisp")
levels(factorCopy$vwhite) = c("notwhiteV", "whiteV")
factorCopy$HispWhite = with(factorCopy, interaction(factorCopy$hisp, factorCopy$vwhite, factorCopy$death))
xtable(table(factorCopy$HispWhite))

levels(factorCopy$white) = c("notWhite", "White")
levels(factorCopy$vwhite) = c("notwhiteV", "whiteV")
factorCopy$WhiteWhiteV = with(factorCopy, interaction(factorCopy$white, factorCopy$vwhite, factorCopy$death))
xtable(table(factorCopy$WhiteWhiteV))
#Tables for information about crimes against white victims. 

