rm(list = ls(all=TRUE))
library(caret)
library(dplyr)




Dataset <- read.csv("C:/Users/user/Desktop/Data Science/Projects/Imarticus/R projects/Lect/Loan.csv")
Loan <- Dataset
summary(Loan)
# experience is having negative values... remove negative experience data if they are less in no,.
Loan <- Loan[Loan$Experience>0,]
hist(Loan$Income)

LoanNew <- Loan
t = LoanNew[,"PersonalLoan"]== 1
Training_init <- LoanNew[t,]                  # Dataset having personal loan = 1
Testing_init <- LoanNew[!t,]                  # dataset having personal loan = 0

# Create data partition (2 sets having equal no of personal = 1 and 0) 
#Using Create Data Partition

Intrain <- createDataPartition(Training_init$Experience, p=0.5, list = FALSE)
Intest <- createDataPartition(Testing_init$Experience, p=0.5, list = FALSE)  
t1 <- Training_init[Intrain,]
t2 <- Training_init[-Intrain,]
t3 <- Testing_init[Intest,]
t4 <- Testing_init[-Intest,]
NewTraining <- rbind(t1, t3)

NewTesting <- rbind(t2, t4)


# Using Sampling Method for Data Partition

set.seed(1)
z = LoanNew[,"PersonalLoan"]== 1

classone <- LoanNew[z,]        # same as training set
z1 <- sample(1:nrow(classone), floor(0.5*nrow(classone)))
classoneTraining <- classone[z1,]
classoneTesting <- classone[-z1,]

#do for class zero and add them...

classzero <- LoanNew[!z,]
z2 <- sample(1:nrow(classzero), floor(0.5*nrow(classzero)))
classzeroTraining <- classzero[z2,]
classzeroTesting <- classzero[-z2,]


Training <- rbind(classoneTraining, classzeroTraining)
Testing <- rbind(classoneTesting, classzeroTesting)





# using index

PlIndex =  which(names(Loan) %in%  "PersonalLoan" )
# xtrain = rbind(classoneTraining[, -PlIndex], classzeroTraining[, -PlIndex])

# xtest = rbind(classoneTesting[,-PlIndex], classzeroTesting[, -PlIndex])

# ytrain = c(classoneTraining[, PlIndex], classzeroTraining[, PlIndex])

# ytest = c(classoneTesting[,PlIndex], classzeroTesting[, PlIndex])

attach(Training)
attach(Testing)
xtrain <- select(Training,-PersonalLoan)
ytrain <- Training$PersonalLoan

xtest <- select(Testing, -PersonalLoan)
ytest <- Testing$PersonalLoan

# Logistic Regression Model

Log_Model1 = glm(ytrain ~ Age + Experience + Income + as.factor(Family) + CCAvg
                 + as.factor(Education) + Mortgage + as.factor(SecuritiesAccount)+
                   as.factor(CDAccount) + as.factor(Online) + as.factor(CreditCard), family = binomial, 
                 data = data.frame(xtrain))
Sum1 <- summary(Log_Model1)


Log_Model2 = glm(ytrain ~   Income + as.factor(Family) + CCAvg
                 + as.factor(Education) + as.factor(SecuritiesAccount)+
                   as.factor(CDAccount) + as.factor(Online) + as.factor(CreditCard), family = binomial, 
                 data = data.frame(xtrain))
(Sum2 <- summary(Log_Model2))

summary(residuals(Log_Model2))


#Estimate the log odds
IncomeEstimate = Sum2$coefficients["Income", "Estimate"]
OddRatio_Income <- ((exp(IncomeEstimate)-1)*100)  
## For every unit rise in Income, the chance 
# of taking loan to not taking loan increses by OddRatio value

## Now predict the values & Model Diagnostics

PredTest = predict(Log_Model2, newdata = data.frame(xtest), type = "response")
classify = floor(PredTest+0.5)
classifyTable = table(ytest,classify)
error = ((classifyTable[1,2]+ classifyTable[2,1])/length(PredTest))*100


library(pscl)
(pR2(Log_Model2))  #McFadden 60% of inpendent variables explains the dependent variable

library(ROCR)    
pr <- prediction(PredTest, ytest)  
#TPR = True Positive rate (sensitivity), FPR = False Positive rate (specificity)
prf <- performance(pr, measure = 'tpr', x.measure = "fpr")
plot(prf)



# Area under ROC (AUR)
library(InformationValue)
plotROC(actuals = ytest, predictedScores = classify)  
