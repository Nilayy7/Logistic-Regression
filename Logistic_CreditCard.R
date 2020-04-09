#Classify whether application accepted or not using Logistic regression
#Here Card factor is our dependent (y) variable
#We want to know which factors influence the 'Card'factor
#Card is a binary variable (application will be accepted or not.)
#Here we will use logistic regression to predict whether the application will be accepted or not

install.packages("AER")
library(AER)
library(plyr)

summary(creditcard)

table(creditcard$card)

#Change Character to numeric
creditcard$owner <- as.factor(revalue(creditcard$owner,c("yes"=1,"no"=0)))
creditcard$selfemp <- as.factor(revalue(creditcard$selfemp,c("yes"=1,"no"=0)))
attach(creditcard)
View(creditcard)

sum(is.na(creditcard))
dim(creditcard)
colnames(creditcard)

# Preparing a linear regression 
lm_mod <- lm(card~.,data = creditcard)
pred <- predict(lm_mod,creditcard)
plot(creditcard$card,pred)
plot(pred)

# GLM function use sigmoid curve to produce desirable results 
lg_model <-glm(card~.,data = creditcard,family='binomial')
exp(coef(lg_model))

# Confusion matrix table 
prob <- predict(lg_model,creditcard,type="response")
summary(lg_model)

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,creditcard$card)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
#Accuracy is 87%

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
creditcard[,"prob"] <- prob
creditcard[,"pred_values"] <- pred_values
creditcard[,"yes_no"] <- yes_no
View(creditcard)

table(creditcard$card,creditcard$pred_values)


# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
rocrpred<-prediction(prob,creditcard$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
