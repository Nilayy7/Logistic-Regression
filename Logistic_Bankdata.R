##Check Whether the client has subscribed a term deposit or not
View(bank.full)
bank_data <- bank.full
colnames(bank_data)[which(names(bank_data) == "y")] <- "term_deposit"

attach(bank_data)
colnames(bank_data)
class(bank_data)
View(bank_data)

#Preparing a linear regression at first
linear_model <- lm(term_deposit~.,data = bank_data)
pred1 <- predict(linear_model,bank_data)
plot(age,pred1)
plot(pred1)

#Logistic Regression Model
log_reg <- glm(term_deposit~.,data = bank_data,family = "binomial")
exp(coef(log_reg))
summary(log_reg)

#Creating Confusion Matrix 
a <- predict(log_reg,bank_data,type ='response')
Cmatrix <- table(a>0.5,bank_data$term_deposit)
Cmatrix

#Accuracy of the model
Acc <- sum(diag(Cmatrix)/sum(Cmatrix))
Acc
#90% Accuracy.

#creating empty columns
pred_val <- NULL
yes_no <-NULL

pred_val <- ifelse(a>=0.5,1,0)
yes_no <- ifelse(a>=0.5,"yes","no")


bank_data[,"a"] <- a
bank_data[,"pred_val"] <- pred_val
bank_data[,"yes_no"] <- yes_no

View(bank_data)

table(bank_data$term_deposit,bank_data$pred_val)

#ROC curve
# ROC curve is used to evaluate betterness of logistic model
install.packages("ROCR")
library(ROCR)
#plot ROC graph 
n <- prediction(a,bank_data$term_deposit)
nPerf <- performance(n,'tpr','fpr')
plot(nPerf)


## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(nPerf)
rocr_cutoff <- data.frame(cut_off = nPerf@alpha.values[[1]],fpr=nPerf@x.values,tpr=nPerf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
