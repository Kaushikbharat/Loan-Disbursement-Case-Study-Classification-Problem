setwd("E:/Bharat/R/Final Case Studies/Bank Maketing Case Study")



#reading the training Traindata
Traindata <- read.csv("Train.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Understanding the Traindata
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(dplyr)
str(Traindata)
names(Traindata)

library(psych)
summary1 <- psych::describe(Traindata)
write.csv(summary1,"summary1.csv")


#Changing the Date type and format
Traindata$DOB <- as.character(Traindata$DOB)

.t <- as.Date(Traindata$DOB,format = "%d-%b-%y")
Traindata$DOB <- as.Date(format(.t,"19%y-%m-%d"))

.p <- as.Date(Traindata$Lead_Creation_Date,format="%d-%b-%y")
Traindata$Lead_Creation_Date <- as.Date(format(.p,"20%y-%m-%d"))
class(Traindata$Lead_Creation_Date)

Traindata$Age <- Traindata$Lead_Creation_Date-Traindata$DOB
Traindata$Age <- Traindata$Age/365.25

Traindata$Age <- round(Traindata$Age,0)
Traindata$Age <- as.numeric(Traindata$Age)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Removing unwanted columns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Traindata$Var1 <- NULL
Traindata$Var5 <- NULL
Traindata$Var2 <- NULL
Traindata$Source <- NULL
Traindata$Var4 <- NULL
Traindata$Lead_Creation_Date <- NULL
Traindata$DOB <- NULL


dim(Traindata)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# missing values in TrainTraindata
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
apply(is.na(Traindata[,]),2,sum)

str(Traindata)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Imputing missing values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Traindata$Loan_Amount_Applied[is.na(Traindata$Loan_Amount_Applied)] <- median(Traindata$Loan_Amount_Applied,na.rm = T)
Traindata$Loan_Tenure_Applied[is.na(Traindata$Loan_Tenure_Applied)] <- median(Traindata$Loan_Tenure_Applied,na.rm = T)
Traindata$Existing_EMI[is.na(Traindata$Existing_EMI)] <- mean(Traindata$Existing_EMI,na.rm = T)
Traindata$Loan_Amount_Submitted[is.na(Traindata$Loan_Amount_Submitted)] <- median(Traindata$Loan_Amount_Submitted,na.rm = T)
Traindata$Loan_Tenure_Submitted[is.na(Traindata$Loan_Tenure_Submitted)] <- median(Traindata$Loan_Tenure_Submitted,na.rm = T)
Traindata$Interest_Rate[is.na(Traindata$Interest_Rate)] <- median(Traindata$Interest_Rate,na.rm = T)
Traindata$Processing_Fee[is.na(Traindata$Processing_Fee)] <- median(Traindata$Processing_Fee,na.rm = T)
Traindata$EMI_Loan_Submitted[is.na(Traindata$EMI_Loan_Submitted)] <- median(Traindata$EMI_Loan_Submitted,na.rm = T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create user defined function for descriptive analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
str(Traindata)

Desc_Analysis=function(x){
  if(class(x)=="numeric"|class(x)=="integer"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    miss_pct <- nmiss/length(x)*100
    mean<-mean(x,na.rm=T)
    median <- median(x,na.rm = T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    pctl <- quantile(x, na.rm=T, p=c(0.01,0.05,0.1,0.25,0.5,0.75,0.9, 0.95,0.99,1.00))
    max<-max(x,na.rm=T)
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,miss_pct=miss_pct,mean=mean,median=median,std=std,var=var,min=min,pctl=pctl))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating vectors for different Variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
num_vars <- sapply(Traindata,is.numeric)
cat_vars <- !sapply(Traindata,is.numeric)

str(Traindata[num_vars])
str(Traindata[cat_vars])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Applying above defined function on numerical variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_num_Traindata<-t(data.frame(apply(Traindata[num_vars], 2, Desc_Analysis)))
my_cat_Traindata<-apply(Traindata[cat_vars], 2, Desc_Analysis)
View(my_num_Traindata)
View(my_cat_Traindata)

Traindata_Cat <- data.frame(Traindata[cat_vars])
Traindata_num <- data.frame(Traindata[num_vars])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function for Outlier Treatment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

outlier_treat <- function(x){
  UC1 = quantile(x, p=0.99,na.rm=T)
  LC1 = quantile(x, p=0.01,na.rm=T)
  #UC1 = mean(x,na.rm=T) + 3*sd(x, na.rm=T)
  #LC1 = mean(x,na.rm=T) - 3*sd(x, na.rm=T)
  
  x=ifelse(x>UC1, UC1, x)
  x=ifelse(x<LC1, LC1, x)
  #x[x>UC1]=UC1
  #x[x<LC1]=LC1
  return(x)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Treating Outliers by using the user defined function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Traindata_num <- data.frame(apply(Traindata_num,2,outlier_treat))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Assumption Check
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cor_mat<-cor(Traindata_num)
cor_mat

require(corrplot)
corrplot::corrplot(corr = cor_mat,method = "number")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Checking the Significance of Categorical Variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

str(Traindata[cat_vars])
summary(aov(Traindata$Disbursed~Traindata$Gender))            #significant
summary(aov(Traindata$Disbursed~Traindata$Mobile_Verified))   #significant
summary(aov(Traindata$Disbursed~Traindata$Filled_Form))       #significant
summary(aov(Traindata$Disbursed~Traindata$Device_Type))       #significant


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Creating dummy variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(caret)
dv1 <- caret::dummyVars(~Gender,Traindata_Cat)
D_Gender = data.frame(predict(dv1, Traindata_Cat))[-1]

dv2 <- caret::dummyVars(~Mobile_Verified,Traindata_Cat)
D_Mobile = data.frame(predict(dv2, Traindata_Cat))[-1]

dv3 <- caret::dummyVars(~Filled_Form,Traindata_Cat)
D_Filled = data.frame(predict(dv3, Traindata_Cat))[-1]

dv4 <- caret::dummyVars(~Device_Type,Traindata_Cat)
D_Device = data.frame(predict(dv4, Traindata_Cat))[-1]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Combining the dummy variable and Numerical Variable to comeup with training set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Training <- data.frame(cbind(D1,D_Gender,D_Device,D_Mobile,D_Filled))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Changing the Variable Names in the Training Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Training$Gender <- Training$Gender.Male
Training$Gender.Male <- NULL

Training$Filled_Form <- Training$Filled_Form.Y
Training$Filled_Form.Y <- NULL

Training$Mobile_Verified <- Training$Mobile_Verified.Y
Training$Mobile_Verified.Y <- NULL

Training$Device_Type <- Training$Device_Type.Web.browser
Training$Device_Type.Web.browser <- NULL

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Splitting Traindata into Development, Validaton Dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
train_ind <- sample(1:nrow(Training), size = floor(0.80 * nrow(Training)))

dev <-Training[train_ind,]
val <-Training[-train_ind,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#checking the no.of rows in Develpment and Validation Dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nrow(dev)
nrow(val)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Building Models for training Traindataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fit<-glm(Disbursed~.,data = dev,family = binomial(logit))
summary(fit)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Output of Logistic Regression
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary(fit)
ls(fit)
fit$model



coeff<-fit$coef #Coefficients of model
write.csv(coeff, "coeff.csv")

require(InformationValue)

ls("package:InformationValue")

train1<- cbind(dev, Prob=predict(fit, type="response")) 
View(train1)

Concordance(train1$Disbursed, train1$Prob)

cut1<-InformationValue::optimalCutoff(train1$Disbursed, train1$Prob, optimiseFor = "Both", returnDiagnostics = TRUE)

ROCTable<-data.frame(cut1$sensitivityTable)
View(ROCTable)

require(dplyr)

ks_table<-ks_stat(train1$Disbursed, train1$Prob, returnKSTable=TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Performing Stepwise Regression
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step1 <- step(fit)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Final Model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fit1 <- glm(Disbursed ~ Loan_Amount_Applied + Processing_Fee  + 
              Gender + Filled_Form + Mobile_Verified + Device_Type,data = dev,family = binomial("logit"))


summary(fit1)

train<- cbind(dev, Prob=predict(fit1, type="response")) 

Concordance(train$Disbursed, train$Prob)

cut<-optimalCutoff(train$Disbursed, train$Prob, optimiseFor = "Both", returnDiagnostics = TRUE)

ROCTable_final<-data.frame(cut$sensitivityTable)
View(ROCTable)

confusionMatrix(train$Disbursed, train$Prob, threshold=0.27)

plotROC(train$Disbursed, train$Prob, Show.labels=F)

InformationValue::somersD(train$Disbursed,train$Prob)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Checking on Validation Set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test<- cbind(val, Prob=predict(fit1,newdata=val, type="response")) 
View(test)

confusionMatrix(test$Disbursed, test$Prob, threshold=0.27)

ks_table_val<-ks_stat(test$Disbursed, test$Prob, returnKSTable=TRUE)

#save.image("~/Bankmark.RData")
      