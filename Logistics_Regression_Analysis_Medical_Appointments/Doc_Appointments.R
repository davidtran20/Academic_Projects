#Importing the Libraries 
library(caTools)
library(ggplot2)
library(glmnet)
library(ROCR)
library(nnet)
library(aod)

#Import the Data & Summaries 
Data = read.csv(file="C:/Users/zdtra/OneDrive/Desktop/School/SMU/Classes/AUG2017/MSDS 6372 - Applied Statistics - Inference and Modeling/Project 3/KaggleV2-May-2016.csv", header=TRUE, sep=",")
Data_Sub <- subset(Data, select=c(3,6,8,9,10,11,12,13,14))
head(Data_Sub)
str(Data_Sub)
sapply(Data_Sub, sd)
xtabs(~NOSHOW + Age, data = Data_Sub)

#Split dataset into "Train" *(80%) and "Test" (20%)
Split <- sample(2, nrow(Data_Sub), replace=TRUE, prob =c(0.8, 0.2))
Train <- Data_Sub[Split==1,]
Test <- Data_Sub[Split==2,]

# Fitting the Model 
model <- glm(NOSHOW ~., family=binomial(link='logit'), data=Train)
model2 <- glm(NOSHOW ~ Age + Scholarship + Hypertension + Diabetes + Alcoholism  + SMS_received, 
              family=binomial(link='logit'), data=Train)
predict <- predict(model, type ='response')

summary(model)
summary(model2)
anova(model, test="Chisq")
confint(model)

#Accessing the predicability of the model
fitted.results <- predict(model, newdata=subset(Test, select=c(1,2,3,4,5,6,7,8)), type='response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
misClasificError <- mean(fitted.results != Test$NOSHOW)
print(paste('Accuracy', 1-misClasificError))

# Wald Test 
wald.test(b = coef(model), Sigma = vcov(model), Terms =2)
wald.test(b = coef(model), Sigma = vcov(model), Terms =3)
wald.test(b = coef(model), Sigma = vcov(model), Terms =4)
wald.test(b = coef(model), Sigma = vcov(model), Terms =5)
wald.test(b = coef(model), Sigma = vcov(model), Terms =6)
wald.test(b = coef(model), Sigma = vcov(model), Terms =7)
wald.test(b = coef(model), Sigma = vcov(model), Terms =8)
wald.test(b = coef(model), Sigma = vcov(model), Terms =9)

# Misclassification Rate 
p <- predict(model, Data_Sub)
table <- table(p, Data_Sub$NOSHOW)
table
Classification_Rate = sum(diag(table))/sum(table)
Classification_Rate
Misclassification_Rate = 1- sum(diag(table))/sum(table)
Misclassification_Rate

table(Data_Sub$NOSHOW)
88208/(88208+22319)

#Model Performance Evaluation 
pred <- predict(model, Train, type= "response")
head(pred)
head(Train)
hist(pred)
predf <- prediction(pred, Train$NOSHOW)
eval <- performance(predf, "acc")
plot(eval)
abline(h=0.80, v=0.35)

#Reciever Operating Characteristic (ROC) Curve
pred2 <- prediction(pred, Data_Sub$NOSHOW)
roc <- performance(pred2, "tpr", "fpr")
plot(roc,
     colorize=T,
     main = "ROC Curve",
     ylab = "Sensitivity",
     xlab = "1 - Specificity")
abline(a=0, b=1)

#ROCR
p <- predict(model2, newdata=subset(Test,select=c(1,2,3,4,5,6,7,8)), type='response')
pr <- prediction(p, Test$NOSHOW)
prf <- performance(pr, measure = "tpr", x.measure ="fpr")
plot(prf)
abline(a=0, b=1)

# Area Under Curve (AUC)
auc <- performance(p, "auc")
auc2 <- unlist(slot(auc, "y.values"))
auc <- round(auc2, 4)
legend(.6, .2, auc, title = "AUC", cex =.5)

# Identify Best Values
max <- which.max(slot(eval, "y.values")[[1]])
acc <- slot(eval, "y.values")[[1]][max]
cut <- slot(eval, "x.values")[[1]][max]
print(c(Accuracy=acc, Cutoff=cut))
