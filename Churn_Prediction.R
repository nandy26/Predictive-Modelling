#Mini project to predict the churn in Telecom industry
rm(list = ls())
setwd("D:/BABI/Predictive_Modelling_M5/Mini_Project")
library(readxl)
library(caTools)
library(rlang)
Churn_Predict_Dataset=read_excel("Cellphone.xlsx",2)
str(Churn_Predict_Dataset)
set.seed(1000)
Churn_Predict_Dataset$Churn=as.factor(Churn_Predict_Dataset$Churn)
Churn_Predict_Dataset$ContractRenewal=as.factor(Churn_Predict_Dataset$ContractRenewal)
Churn_Predict_Dataset$DataPlan=as.factor(Churn_Predict_Dataset$DataPlan)

summary(Churn_Predict_Dataset)
table(Churn_Predict_Dataset$Churn)
names(Churn_Predict_Dataset)
dim(Churn_Predict_Dataset)#11 variables
#Checking for missing values
sum(is.na(Churn_Predict_Dataset))#no missing value
##  check that no datapoint is missing, otherwise we need to fix the dataset.
apply(Churn_Predict_Dataset,2,function(x) sum(is.na(x)))
#when MARGIN=1, it applies over rows, whereas with MARGIN=2, it works over columns. 
#Note that when you use the construct MARGIN=c(1,2), it applies to both rows and columns;
#Outliers
boxplot(Churn_Predict_Dataset$AccountWeeks,col="gray",main="AccountWeeks")
boxplot(Churn_Predict_Dataset$DataUsage,col="gray",main="DataUsage")
boxplot(Churn_Predict_Dataset$RoamMins,col="gray",main="RoamMins")
boxplot(Churn_Predict_Dataset$DayMins,col="gray",main="DayMins")
boxplot(Churn_Predict_Dataset$OverageFee,col="gray",main="OverageFee")
boxplot(Churn_Predict_Dataset$CustServCalls,col="gray",main="CustServCalls")
boxplot(Churn_Predict_Dataset$DayCalls,col="gray",main="DayCalls")
boxplot(Churn_Predict_Dataset$MonthlyCharge,col="gray",main="MonthlyCharge")
boxplot(Churn_Predict_Dataset$DayMins,col="gray",main="DayMins")

#Uni-variate analysis
plot(Churn_Predict_Dataset$Churn,col="BLUE",main="Customer Churn")
hist(Churn_Predict_Dataset$AccountWeeks,col="BLUE",main="AccountWeeks")
hist(Churn_Predict_Dataset$DataUsage,col="BLUE",main="DataUsage")
hist(Churn_Predict_Dataset$RoamMins,col="BLUE",main="RoamMins")
hist(Churn_Predict_Dataset$DayMins,col="BLUE",main="DayMins")
hist(Churn_Predict_Dataset$OverageFee,col="BLUE",main="OverageFee")



#Bi-variate analysis
par(mfrow=c(2,2))
plot(Churn_Predict_Dataset$Churn,Churn_Predict_Dataset$CustServCalls,main="Churn vs Customer Service Calls",col="red",xlab="Churn",ylab="CustServCalls")
plot(Churn_Predict_Dataset$Churn,Churn_Predict_Dataset$DayMins,main="Churn vs DayMins",col="red",xlab="Churn",ylab="DayMins")#people who went out are having better daymins than people who are staying
plot(Churn_Predict_Dataset$Churn,Churn_Predict_Dataset$DayCalls,main="Churn vs DayCalls",col="red",xlab="Churn",ylab="DayCalls")#no info
plot(Churn_Predict_Dataset$Churn,Churn_Predict_Dataset$MonthlyCharge,main="Churn vs MonthlyCharge",col="red",xlab="Churn",ylab="MonthlyCharge")#no info
plot(Churn_Predict_Dataset$Churn,Churn_Predict_Dataset$OverageFee,main="Churn vs OverageFee",col="red",xlab="Churn",ylab="OverageFee")
plot(Churn_Predict_Dataset$Churn,Churn_Predict_Dataset$RoamMins,main="Churn vs RoamMins",col="red",xlab="Churn",ylab="RoamMins")

# Multi-Collinearity
Correlation_Matrix=cor(Churn_Predict_Dataset[,-c(1,3,4)])
library(corrplot)
corrplot(Correlation_Matrix)#Data Usage and monthly charge are highly correlated
###Insights from EDA
## Check visual association pattern for continous predictor variables
library(GGally)
ggpairs(Churn_Predict_Dataset[, c("CustServCalls","DayMins",
                                  "DayCalls","MonthlyCharge",
                                  "OverageFee","RoamMins","AccountWeeks","DataUsage")],ggplot2::aes(colour = as.factor(Churn_Predict_Dataset$Churn)))

# contingency table of dicotomous variables with target variable
ct.data <- subset(Churn_Predict_Dataset, select = c(ContractRenewal,DataPlan))
# also create a subset of numeric data
num.data <- subset(Churn_Predict_Dataset, select = c(AccountWeeks,DataUsage,
                                                     CustServCalls,DayMins,DayCalls,
                                                     MonthlyCharge,OverageFee,RoamMins))


par(mfrow=c(1,1)) # To put multiple graphs in a single plot by setting some graphical parameters with the help of par() function.

for (i in names(ct.data)) {
  print(i)
  print(table(Churn_Predict_Dataset$Churn, ct.data[[i]]))
  barplot(table(Churn_Predict_Dataset$Churn, ct.data[[i]]),
          col=c("grey","red"),
          main = names(ct.data[i]))
}
par(mfrow=c(1,1))
#categorical variables
ct.data$Churn = Churn_Predict_Dataset$Churn
Log_Model_1=glm(Churn_Predict_Dataset$Churn~.,data =ct.data,family ="binomial" )
summary(Log_Model_1)

sample=sample.split(Churn_Predict_Dataset$Churn,SplitRatio =0.7)
Churn_Predict_Train=subset(Churn_Predict_Dataset,sample==TRUE)
Churn_Predict_Test=subset(Churn_Predict_Dataset,sample==FALSE)

###################Logistics regression##################
#Individual relationship between Predictors and  Target variable
summary(glm(Churn_Predict_Train$Churn~Churn_Predict_Train$AccountWeeks,data =Churn_Predict_Train,family ="binomial" ))#Insignificant
summary(glm(Churn_Predict_Train$Churn~Churn_Predict_Train$DayCalls,data =Churn_Predict_Train,family ="binomial" ))#Insignificant

summary(glm(Churn_Predict_Train$Churn~Churn_Predict_Train$ContractRenewal,data =Churn_Predict_Train,family ="binomial" ))#significant
summary(glm(Churn_Predict_Train$Churn~Churn_Predict_Train$DataPlan,data =Churn_Predict_Train,family ="binomial" ))#significant
summary(glm(Churn_Predict_Train$Churn~Churn_Predict_Train$DataUsage,data =Churn_Predict_Train,family ="binomial" ))#significant
summary(glm(Churn_Predict_Train$Churn~Churn_Predict_Train$CustServCalls,data =Churn_Predict_Train,family ="binomial" ))#significant
summary(glm(Churn_Predict_Train$Churn~Churn_Predict_Train$DayMins,data =Churn_Predict_Train,family ="binomial" ))#significant
summary(glm(Churn_Predict_Train$Churn~Churn_Predict_Train$MonthlyCharge,data =Churn_Predict_Train,family ="binomial" ))#significant
summary(glm(Churn_Predict_Train$Churn~Churn_Predict_Train$OverageFee,data =Churn_Predict_Train,family ="binomial" ))#significant
summary(glm(Churn_Predict_Train$Churn~Churn_Predict_Train$RoamMins,data =Churn_Predict_Train,family ="binomial" ))#significant

Log_Model_2=glm(Churn_Predict_Train$Churn~.,data =Churn_Predict_Train,family ="binomial" )
summary(Log_Model_2)
library(car)
vif(Log_Model_2)


Log_Model_3=glm(Churn_Predict_Train$Churn~ContractRenewal+DataPlan+CustServCalls+OverageFee+RoamMins+DayMins,data =Churn_Predict_Train,family ="binomial" )
summary(Log_Model_3)
vif(Log_Model_3)
print(exp(Log_Model_3$coefficients))

Churn_Predict_Test$Prob_test=predict(Log_Model_3,newdata=Churn_Predict_Test,type ="response")
Churn_Predict_Test$Predicted_data=ifelse(Churn_Predict_Test$Prob_test<0.20,0,1)


######################Model Performance of logistic regression######################

#Confusion matrix
Confusion_matrix_lg=table(Churn_Predict_Test$Churn,Churn_Predict_Test$Predicted_data)
#imbalanced data,we can't conclude with accuracy
Accuracy_lg=(Confusion_matrix_lg[1,1]+Confusion_matrix_lg[2,2])/1000
Error_rate_lg=round((1-Accuracy_lg),4)
#Sensitivity/Recall/True positive rate(out of total one's present , how many are correct)
Sensitivity_lg=(Confusion_matrix_lg[2,2])/(Confusion_matrix_lg[2,1]+Confusion_matrix_lg[2,2])
#Specificity /True Negative Rate
Specificity_lg=(Confusion_matrix_lg[1,1])/(Confusion_matrix_lg[1,2]+Confusion_matrix_lg[1,1])
#Precision(out of predicted one's , how many are correct)
Precision_lg=(Confusion_matrix_lg[2,2])/(Confusion_matrix_lg[1,2]+Confusion_matrix_lg[2,2])
#Prevalance rate-(actual postive /total data)
table(Churn_Predict_Test$Churn)
Prevalance_lg=(145/1000)

##########################ROC,AUC ,GINI and KS-Logistic regression############################
library(ROCR)
ROCRpred = prediction(Churn_Predict_Test$Prob_test, Churn_Predict_Test$Churn)
AUC_lg=as.numeric(performance(ROCRpred, "auc")@y.values)
perf_lg = performance(ROCRpred, "tpr","fpr")
plot(perf_lg,col="black",lty=2, lwd=2)
plot(perf_lg,lwd=3,colorize = TRUE,main="ROC-Logistic Regression")
KS_lg = max(perf_lg@y.values[[1]]-perf_lg@x.values[[1]])
library(ineq)
gini_lg = ineq(Churn_Predict_Test$Prob_test, type="Gini")


#3Deciling code-Rank ordering
qs_lg=quantile(Churn_Predict_Test$Prob_test,prob = seq(0,1,length=11))
print(qs_lg)
print(qs_lg[10])
threshold=qs_lg[10]
mean((Churn_Predict_Test$Churn[Churn_Predict_Test$Prob_test>threshold])=="1")
Churn_Predict_Test$Deciles=cut(Churn_Predict_Test$Prob_test,unique(qs_lg),include.lowest = TRUE,right = FALSE)
head(Churn_Predict_Test)

#Rank ordering
library(data.table)
#Loan_Dataset_lg$Personal.Loan=as.numeric(Loan_Dataset_lg$Personal.Loan)
DT_lg=data.table(Churn_Predict_Test)
#Aggregate columns

Rtable_lg=DT_lg[,list(cnt=length(Churn),
                      cnt_tar1 = sum(Churn==1),
                      cnt_tar0 = sum(Churn==0)),by=Deciles][order(-Deciles)]
print(Rtable_lg)
Rtable_lg$rrate = round(Rtable_lg$cnt_tar1 / Rtable_lg$cnt,4)*100;
Rtable_lg$cum_resp = cumsum(Rtable_lg$cnt_tar1)
Rtable_lg$cum_non_resp = cumsum(Rtable_lg$cnt_tar0)
Rtable_lg$cum_rel_resp = round(Rtable_lg$cum_resp / sum(Rtable_lg$cnt_tar1),4)*100;
Rtable_lg$cum_rel_non_resp = round(Rtable_lg$cum_non_resp / sum(Rtable_lg$cnt_tar0),4)*100;
Rtable_lg$ks = abs(Rtable_lg$cum_rel_resp - Rtable_lg$cum_rel_non_resp);
print(Rtable_lg)

# Concordance Function
library(InformationValue)
Concordance(actuals=Churn_Predict_Test$Churn, predictedScores=Churn_Predict_Test$Prob_test)


#########KNN(k-Nearest Neighbours)############################
Churn_Predict_Train_KNN=subset(Churn_Predict_Dataset,sample==TRUE)
Churn_Predict_Test_KNN=subset(Churn_Predict_Dataset,sample==FALSE)
str(Churn_Predict_Train_KNN)
dim(Churn_Predict_Train_KNN)
#Load class package to build KNN model
library(class)
Churn_Predict_Test_KNN$Status=knn(Churn_Predict_Train_KNN[,-c(1)],Churn_Predict_Test_KNN[,-c(1)],Churn_Predict_Train_KNN$Churn,k=3)
knn <- class::knn(Churn_Predict_Train_KNN[,-c(1)], Churn_Predict_Test_KNN[,-c(1,12)],Churn_Predict_Train_KNN$Churn, k=3, prob=TRUE)
Churn_Predict_Test_KNN$prob_knn <- attr(knn, "prob")
Churn_Predict_Test_KNN$prob_knn <- 2*ifelse(knn == "-1", 1-Churn_Predict_Test_KNN$prob_knn, Churn_Predict_Test_KNN$prob_knn) - 1
##########################ROC,AUC ,GINI and KS-knn ############################
library(ROCR)
ROCRpred = prediction(Churn_Predict_Test_KNN$prob_knn, Churn_Predict_Test_KNN$Churn)
AUC_KNN=as.numeric(performance(ROCRpred, "auc")@y.values)
perf_kNN = performance(ROCRpred, "tpr","fpr")
plot(perf_kNN,col="black",lty=2, lwd=2)
plot(perf_kNN,lwd=3,colorize = TRUE,main="ROC-KNN")
KS_KNN = max(perf_kNN@y.values[[1]]-perf_kNN@x.values[[1]])
library(ineq)
gini_knn = ineq(Churn_Predict_Test_KNN$prob_knn, type="Gini")
######################Model performance on KNN(k-Nearest Neighbours)######################

#Confusion matrix
Confusion_matrix_KNN=table(Churn_Predict_Test_KNN$Churn,Churn_Predict_Test_KNN$Status)
Accuracy_KNN=(Confusion_matrix_KNN[1,1]+Confusion_matrix_KNN[2,2])/1000#imbalanced data,we can't conclude with accuracy
Error_rate_KNN=round((1-Accuracy_KNN),4)
#Sensitivity/Recall/True positive rate(out of total one's present , how many are correct)
Sensitivity_KNN=(Confusion_matrix_KNN[2,2])/(Confusion_matrix_KNN[2,1]+Confusion_matrix_KNN[2,2])
#Specificity /True Negative Rate
Specificity_KNN=(Confusion_matrix_KNN[1,1])/(Confusion_matrix_KNN[1,2]+Confusion_matrix_KNN[1,1])
#Precision(out of predicted one's , how many are correct)
Precision_KNN=(Confusion_matrix_KNN[2,2])/(Confusion_matrix_KNN[1,2]+Confusion_matrix_KNN[2,2])
#Prevalance rate-(actual postive /total data)
table(Churn_Predict_Test_KNN$Churn)
Prevalance_KNN=(145/1000)
#3Deciling code-Rank ordering
qs_KNN=quantile(Churn_Predict_Test_KNN$prob_knn,prob = seq(0,1,length=11))
print(qs_KNN)
print(qs_KNN[2])
threshold=qs_KNN[2]
mean((Churn_Predict_Test_KNN$Churn[Churn_Predict_Test_KNN$prob_knn>threshold])=="1")
Churn_Predict_Test_KNN$Deciles=cut(Churn_Predict_Test_KNN$prob_knn,unique(qs_KNN),include.lowest = TRUE,right = FALSE)
head(Churn_Predict_Test_KNN)
#Rank ordering
library(data.table)
#Loan_Dataset_lg$Personal.Loan=as.numeric(Loan_Dataset_lg$Personal.Loan)
DT_KNN=data.table(Churn_Predict_Test_KNN)
#Aggregate columns

Rtable_kNN=DT_KNN[,list(cnt=length(Churn_Predict_Test_KNN$Churn),
                      cnt_tar1 = sum(Churn==1),
                      cnt_tar0 = sum(Churn==0)),by=Deciles][order(-Deciles)]
print(Rtable_kNN)
Rtable_KNN$rrate = round(Rtable_kNN$cnt_tar1 / Rtable_kNN$cnt,4)*100;
Rtable_kNN$cum_resp = cumsum(Rtable_kNN$cnt_tar1)
Rtable_kNN$cum_non_resp = cumsum(Rtable_kNN$cnt_tar0)
Rtable_kNN$cum_rel_resp = round(Rtable_kNN$cum_resp / sum(Rtable_kNN$cnt_tar1),4)*100;
Rtable_kNN$cum_rel_non_resp = round(Rtable_kNN$cum_non_resp / sum(Rtable_kNN$cnt_tar0),4)*100;
Rtable_kNN$ks = abs(Rtable_kNN$cum_rel_resp - Rtable_kNN$cum_rel_non_resp);
print(Rtable_kNN)


