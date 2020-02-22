setwd("F://R//files")
bankdata <- read.csv("bank-full.csv", sep=";")
attach(bankdata)
View(bankdata)
library(plyr)
table(y)

bankdata$yny[bankdata$y=="yes"] <- 1
bankdata$yny[bankdata$y=="no"] <- 0

View(bankdata)

table(poutcome)

library(dummies)
a <- dummy(poutcome)
b <- dummy(marital)
c <- dummy(education)
d <- dummy(default)
e <- dummy(housing)
f <- dummy(loan)
g <- dummy(contact)
h <- dummy(month)
i <- dummy(job)

bankdata <- cbind(bankdata[,-c(2,3,4,5,7,8,9,11,16,17)], a,b,c,d,e,f,g,h,i)
View(bankdata)
#structure
str(bankdata)
#ploting first model
bankdata$yny <- as.factor(bankdata$yny)
model1 <- glm(yny ~ ., data = bankdata, family = "binomial")
summary(model1)

library(car)

influenceIndexPlot(model1)
influencePlot(model1)
#to removing influencing factor
model2 <- glm(yny ~ ., data = bankdata[-c(29183, 39990, 24149),], family = "binomial")
summary(model2)

avPlots(model1)


#odds ratio
exp(coef(finalmodel))

#confusion table matrix
prob <- predict(finalmodel, bankdata, type = "response")

confusion <- table(prob>0.5, bankdata$yny)
confusion

#modelaccuracy 
accuracy <- sum(diag(confusion)/ sum(confusion))
accuracy #final accuracy is 89%

pred_values <- NULL
yes_no <- NULL

#to adding tables
pred_values <- ifelse(prob>0.5, 1, 0)
yes_no <- ifelse(prob>0.5, "yes", "no")

bankdata[, "prob" ] <- prob
bankdata[, "pred_values"] <- pred_values
bankdata[, "yes_no"] <- yes_no

table(pred_values, bankdata$yny)

#ploting logistic curve
library(ROCR)

pred <- prediction(prob, bankdata$yny)
perfom <- performance(pred, "tpr", "fpr")

plot(perfom, col = T , text.adj = c(-0.2, 1.7))

