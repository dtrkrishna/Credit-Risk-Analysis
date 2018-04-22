setwd("C://Users//PK//Desktop//imarticus//by me in class//credit risk")
getwd()
train<- read.csv("Credit_Risk_Train_data.csv", header=T, sep=",",
                 na.strings="")
test<- read.csv("Credit_Risk_Test_data.csv", stringsAsFactors = T)

summary(train)
str(train)
#replace "" in gender with male
train[which(train$Gender==""),2] ="Male"

 #train$Gender <- ifelse(train$Gender=="", "Male", train$Gender)
#train$Gender<- as.factor(train$Gender)
summary(train)
train[which(train$Married==""),3] ="Yes"
summary(train)
train[which(train$Self_Employed==""),6] ="No"
summary(train)
train[which(train$Dependents==""),4] ="0"
#---------- new
# to chekd the missing values
# sapply(train, function(x) sum(is.na(x)))
levels(train$Dependents)[4]=3
levels(train$Dependents)
summary(train)
train$Credit_History[which(is.na(train$Credit_History))]=1
train$Married[which(is.na(train$Married))]="Yes"
train$Gender[which(is.na(train$Gender))]="Male"
train$Self_Employed[which(is.na(train$Self_Employed))]="No"
train$Loan_Amount_Term[which(is.na(train$Loan_Amount_Term))]=360
train$LoanAmount[which(is.na(train$LoanAmount))]<- median(train$LoanAmount, na.rm=T)
train$Dependents[which(is.na(train$Dependents))]<- 0
summary(train)
#----Now analyasing-----
a <- plot(train$Gender,train$Loan_Status)
b<- plot(train$Married, train$Loan_Status)
c <- plot(train$Dependents, train$Loan_Status)
d <- plot(train$Education, train$Loan_Status)
e<- plot(train$Self_Employed, train$Loan_Status)
f <- plot(train$ApplicantIncome, train$Loan_Status)
g <- plot(train$CoapplicantIncome, train$Loan_Status)

#-- by him
hist(train$ApplicantIncome)
hist(log(train$ApplicantIncome))
hist(train$CoapplicantIncome)
hist(log(train$CoapplicantIncome))
train$join_income<- train$ApplicantIncome+train$CoapplicantIncome
hist(log(train$join_income))
hist(train$Loan_Amount_Term)
hist(train$LoanAmount)
hist(log(train$LoanAmount))

row<- nrow(train)
row
test_index = list()
train_index =list()
for (i in 1:row){ 
  if(i%%5 == 0){
    test_index=c(test_index,i)
    
  } else{
  train_index= c(train_index,i)
  }
}
train_index= unlist(train_index)
test_index= unlist(test_index)
new_train= train[train_index, 2:13]
new_test=train[test_index, 2:13]
new_train
model<- glm(formula = Loan_Status~ ., family = 'binomial', data=new_train)

summary(model)
predicted_train <- predict(model,type='response', new_train)
predicted_train
predicted_test <- predict(model, type= 'response', new_test)

pred_train<- ifelse(predicted_train>0.5,"Y","N")
pred_train
pred_test <- ifelse(predicted_test>0.5,"Y","N")
pred_test
for_trian <- accuracy(new_train$Loan_Status, pred_train)
for_trian
for_trial <- accuracy(new_test$Loan_Status, pred_test)
for_trial
table(new_train$Loan_Status, pred_train)
table(new_test$Loan_Status, pred_test) #-- confusion matrix

#--roc curve which gives confusion matrix in the form of curves.
?roc
plot(roc(ifelse(new_train$Loan_Status=="Y",1,0), ifelse(pred_train=="Y",1,0),         direction= "<"), col="red", lwd=3, main="rama")








