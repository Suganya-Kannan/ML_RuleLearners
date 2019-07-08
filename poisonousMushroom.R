####--------------------- Rule Learners --------------------------

mush <- read.csv("D:/Way to go as DS/ML/Data/mushrooms.csv", stringsAsFactors = T)
head(mush)
str(mush)
mush$veil.type <- NULL
plot(mush$class)
table(mush$class)

## About 52 percent of the mushroom samples (N = 4,208) are edible, while 48 percent
##(N = 3,916) are poisonous.

## using 1R classifier
## 1) Training the model
library("RWeka")
mush_1R <- OneR(mush$class~., data = mush)

mush_1R ## To exaamine the rules it created

## here, the odor feature was selected for RULE GENERATION
## 8004/8124 are correctly predicted
(8004/8124)*100 ## 98.5 (nearly with 99% accuracy)


## 2) Evaluating model performance
summary(mush_1R)

## let's see if we can add a few more rules and develop an even better classifier.

## 3) Improving model performance
library(RWeka)
mush_jrip <- JRip(class ~ ., data = mush)
mush_jrip ## to examine the rules

test <- mush[order(runif(100)),]
predictedClass <- predict(mush_jrip, test)
library(gmodels)
CrossTable(test$class, predictedClass,
           prop.chisq = F, prop.r = F, prop.c = F, prop.t = F,
           dnn = c("Actual", "Predicted"))

## hence we predicted with 100% accuracy

