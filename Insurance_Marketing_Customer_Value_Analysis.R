setwd("C:\\Users\\Ritwik Sinha\\Desktop\\Ritwik\\Projects")
getwd()
read.csv("Insurance_Marketing-Customer-Value-Analysis.csv", header = TRUE )-> data
str(data)
summary(data)
View(data)


# Customer.Lifetime.Value is the dependent variable & known as " No. of dollars spend "#
# Install these neccessary packages, if not installed

install.packages("boot")
install.packages("car")
install.packages("QuantPsyc")
install.packages("lmtest")
install.packages("sandwich")
install.packages("vars")
install.packages("nortest")
install.packages("MASS")
install.packages("caTools")
install.packages("dplyr")


library(boot)
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)


data1<- data
rename(data1, clv = Customer.Lifetime.Value)-> data1
View(data1)
# second method to change the column names :: colnames(data1)[which(data)=="Customer.Lifetime.Value")]="clv"

max(data1$clv); min(data1$clv)

# finding and removing outliers # using quantile # we can only remove max to max (7 to 10) % of outliers from the original data.
# if we remove more than 10 or 11 % ,the data should not give the best-fit of the model #

boxplot(data1$clv)
quantile(data1$clv, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))

data2 <- data1[data1$clv <50000, ]
boxplot(data2$clv)
quantile(data2$clv, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.97,0.98,0.985,0.99,0.995,1))
nrow(data1)-nrow(data2)

data3 <- data2[data2$clv <17000, ]
boxplot(data3$clv)
quantile(data3$clv, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.97,0.98,0.985,0.99,0.995,1))
nrow(data1)-nrow(data3)



sapply(data3, function(x)sum(is.na(x)))    #one method to check #
as.data.frame(colSums(is.na(data3)))      #second method to check #

data3 <- subset(data3, select = -c(Customer,State, Effective.To.Date))
#data3 <- select(data3, -c(Customer,State, Effective.To.Date))
View(data3)

# set.seed(123) is used for randomizing the data resulting in better results #
# we leave 30% of data for testing & 70 % for training #

set.seed(123)
sample.split(data3$clv, SplitRatio = 0.70)-> s_value
subset(data3, s_value==T)-> train 
View(train)
subset(data3, s_value==F)-> test
View(test)
dim(train); dim(test)

# Linear Regression Modelling #
# . is consider for all independent variable #

LM1<- lm(clv ~ . , data = train)
summary(LM1)
# we only consider the significant values (means * values)
# Y=Betanot + Beta1X1 + beta2X2 + ...  #
# Y = clv, Betanot = intercept, (X1,X2,X3,...)=(ResponseYes,CoverageExtended,CoveragePremium,...)  #
# * values has a significant role in the linear regression model #

LinearModel1=lm(clv~Response+ Coverage+ Education+ EmploymentStatus+ Gender+ Income+ Location.Code+ Marital.Status+ Monthly.Premium.Auto+ Months.Since.Last.Claim+ Months.Since.Policy.Inception+ Number.of.Open.Complaints+ Number.of.Policies+ Renew.Offer.Type+ Sales.Channel+ Total.Claim.Amount+ Vehicle.Class+ Vehicle.Size,data=train)
summary(LinearModel1)


LinearModel2=lm(clv~ Education+ EmploymentStatus+ Monthly.Premium.Auto+ Months.Since.Last.Claim+ Number.of.Open.Complaints+ Number.of.Policies+ Renew.Offer.Type+ Vehicle.Class ,data=train)
summary(LinearModel2)


LinearModel3=lm(clv~ EmploymentStatus+ Monthly.Premium.Auto+ Months.Since.Last.Claim+ Number.of.Open.Complaints+ Number.of.Policies+ Renew.Offer.Type+ I(Vehicle.Class=="SUV") + I(Vehicle.Size=="Small") ,data=train)
summary(LinearModel3)


LinearModel4=lm(clv ~ Monthly.Premium.Auto + Number.of.Open.Complaints + Number.of.Policies + Renew.Offer.Type + I(Vehicle.Class== "SUV"), data = train )
summary(LinearModel4)


FinalModel=lm(clv ~ Monthly.Premium.Auto + Number.of.Open.Complaints + Number.of.Policies + I(Renew.Offer.Type == "Offer2") + I(Vehicle.Class== "SUV"), data = train )
summary(FinalModel)

# checking Multicolinearity in the model #
# If VIF value is > 1.7 then multicolinearity exist otherwise doesn't exist #

vif(FinalModel)

# Get the fitted values or predicted values #
fitted(FinalModel)-> x
View(x)
View(train$clv)
par(mfrow=c(2,2))
plot(FinalModel)

## MAPE 

pred_train<- predict(FinalModel, type = "response")
train$pred_train<-fitted(FinalModel)
attach(train)
MAPE<-print((sum((abs(clv-pred_train))/clv))/nrow(train))
write.csv(train,"Predicted_CLV.csv")
res <- train

res$stu_res <- studres(FinalModel) ##Studentized residuals
res$stud.del.resids <- rstudent(FinalModel) ##studentized deleted residuals
res$leverage <- hatvalues(FinalModel) ## leverage values (hi)
res$cooks_dis <- cooks.distance(FinalModel) ## Cook's distance
res$dffits <- dffits(FinalModel) ## Dffit
res$dfbetas <- dfbetas(FinalModel) ## Dfbetas
res$cov_ratio <- covratio(FinalModel) ## Covariance Ratio

write.csv(res,"res.csv")

dwt(FinalModel)
#Since, the p-value is >0.05, we fail to reject H0: (No Autocorrelation)

# Checking multicollinearity
vif(FinalModel) # should be within 1.7. If it is greater than 10 then serious problem

################ Constant error variance ##########Heteroscedasticity


# Breusch-Pagan test
bptest(FinalModel)  # Null hypothesis -> error is non-homogenious (p value should be more than 0.05)


#Cook-Weisberg test

ncvTest(lm(clv~ 	Coverage +	I(Education=="College") 
           + I(EmploymentStatus=="Medical Leave")+ I(EmploymentStatus=="Retired")+ I(EmploymentStatus=="Unemployed")
           +Monthly.Premium.Auto  +	Number.of.Open.Complaints +	Number.of.Policies +	Renew.Offer.Type 
           +	I(Vehicle.Class=="SUV")  , data=train))

## Normality testing Null hypothesis is data is normal.

resids <- FinalModel$residuals


ad.test(resids) #get Anderson-Darling test for normality 
cvm.test(resids) #get Cramer-von Mises test for normaility 
lillie.test(resids) #get Lilliefors (Kolmogorov-Smirnov) test for normality 
pearson.test(resids) #get Pearson chi-square test for normaility 

qqnorm(resids)

###########################################################################################################################
############## Testing the model on test data ############################################################################
###########################################################################################################################


fit1<- lm(clv~ 	Coverage +	I(Education=="College") 
          + I(EmploymentStatus=="Medical Leave")+ I(EmploymentStatus=="Retired")+ I(EmploymentStatus=="Unemployed")
          +Monthly.Premium.Auto  +	Number.of.Open.Complaints +	Number.of.Policies +	Renew.Offer.Type 
          +	I(Vehicle.Class=="SUV")  , data=test)
summary(fit1)


##Remove I(EmploymentStatus=="Medical Leave"), I(EmploymentStatus=="Retired"),I(Education=="College") 
fit1<- lm(clv~ 	Coverage +  I(EmploymentStatus=="Unemployed")
          +Monthly.Premium.Auto  +	Number.of.Open.Complaints +	Number.of.Policies +	Renew.Offer.Type 
          +	I(Vehicle.Class=="SUV")  , data=test)
summary(fit1)

##Remove Coverage Extended
fit1<- lm(clv~ 	I(Coverage=="Premium")  +  I(EmploymentStatus=="Unemployed")
          +Monthly.Premium.Auto  +	Number.of.Open.Complaints +	Number.of.Policies +	Renew.Offer.Type 
          +	I(Vehicle.Class=="SUV")  , data=test)
summary(fit1)


fit1<- lm(clv~ 	I(Coverage=="Premium")  +  I(EmploymentStatus=="Unemployed")
          +Monthly.Premium.Auto  +	Number.of.Policies +	I(Renew.Offer.Type == "2") +  I(Renew.Offer.Type == "4")
          +	I(Vehicle.Class=="SUV")  , data=test)
summary(fit1)


##Final Model
fit1<- lm(clv~ 	I(Coverage=="Premium")  +  I(EmploymentStatus=="Unemployed")
          +Monthly.Premium.Auto  +	Number.of.Policies
          +	I(Vehicle.Class=="SUV")  , data=test)
summary(fit1)


#Check Vif, vif>1.7 means presence of multicollinearity
vif(fit1)


## Get the predicted or fitted values
fitted(fit1)

test$pred <- fitted(fit1)
write.csv(test,"Test_CLV.csv")

#Calculating MAPE
attach(test)
(sum((abs(clv-pred))/clv))/nrow(test)



############ Residual Analysis ############################################################################

res1 <- test

res1$stu_res <- studres(fit1) ##Studentized residuals
res1$stud.del.resids <- rstudent(fit1) ##studentized deleted residuals
res1$leverage <- hatvalues(fit1) ## leverage values (hi)
res1$cooks_dis <- cooks.distance(fit1) ## Cook's distance
res1$dffits <- dffits(fit1) ## Dffit
res1$dfbetas <- dfbetas(fit1) ## Dfbetas
res1$cov_ratio <- covratio(fit1) ## Covariance Ratio

write.csv(res1,"res1.csv")

##################################### Checking of Assumption ############################################

# residuals should be uncorrelated ##Autocorrelation
# Null H0: residuals from a linear regression are uncorrelated. Value should be close to 2. 
#Less than 1 and greater than 3 -> concern
## Should get a high p value

dwt(fit1)

# Checking multicollinearity
vif(fit1) # should be within 2. If it is greater than 10 then serious problem

################ Constant error variance ##########Heteroscedasticity


# Breusch-Pagan test
bptest(fit1)  # Null hypothesis -> error is homogenious (p value should be more than 0.05)


#Cook-Weisberg test
# hypothesis of constant error variance against the alternative that the error variance changes with the level of the  response 
# p value should be more than 0.05
ncvTest(lm(clv~ 	I(Coverage=="Premium")  +  I(EmploymentStatus=="Unemployed")
           +Monthly.Premium.Auto  +	Number.of.Policies
           +	I(Vehicle.Class=="SUV")  , data=test))


## Normality testing Null hypothesis is data is normal.

resids1 <- fit1$residuals


ad.test(resids1) #get Anderson-Darling test for normality 
cvm.test(resids1) #get Cramer-von Mises test for normaility 
lillie.test(resids1) #get Lilliefors (Kolmogorov-Smirnov) test for normality 
pearson.test(resids1) #get Pearson chi-square test for normaility 
sf.test(resids1) #get Shapiro-Francia test for normaility 

qqnorm(resids1)



######################################## END ################################################################