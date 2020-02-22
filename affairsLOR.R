install.packages("AER")
library(AER)
library(plyr)
affairs <- data("Affairs")
View(Affairs)

affairs1 <- Affairs
summary(affairs1)

table(affairs1$affairs)

affairs1$ynaffairs[affairs1$affairs >0 ] <- 1
affairs1$ynaffairs[affairs1$affairs == 0] <- 0
View(affairs1)
table(affairs1$ynaffairs)

library(dummies)
a <- dummy(affairs1$gender)
b <- dummy(affairs1$children)
affairs <- cbind(affairs1[,-c(2,5)], a,b)
View(affairs)
attach(affairs1)
affairs <- affairs[,-1]
model1 <- glm(ynaffairs ~ ., data = affairs, family = "binomial")
summary(model1)


influenceIndexPlot(model1)

#1622,1294
affairs <- affairs[-c(1622,1294),]
model2 <- glm(ynaffairs ~ ., data = affairs, family = "binomial")
summary(model2)
influenceIndexPlot(model2)
vif(model1)

prob <- predict(model1, affairs, type = "response")

confusion <- table(prob>0.5, affairs1$ynaffairs)
confusion


Accuracy <- sum(diag(confusion)/sum(confusion))
Accuracy

#model accuracy is 76%
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>0.5,1,0)
yes_no <- ifelse(prob>0.5, "yes", "no")


affairs1[,"prob"] <- prob
affairs1[,"pred_values"] <- pred_values
affairs1[,"yes_no"] <- yes_no

View(affairs1[,c(1,9:11)])

table(affairs1$ynaffairs, affairs1$pred_values)
install.packages("ROCR")
library(ROCR)


rocrpred <- prediction(prob, affairs1$ynaffairs)
rocrperf <- performance(rocrpred, 'tpr', 'fpr')

str(rocrperf)

plot(rocrperf, colorize = T, text.adj = c(-0.2, 1.7))
