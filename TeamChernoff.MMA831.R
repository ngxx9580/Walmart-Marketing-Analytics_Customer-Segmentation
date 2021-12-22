library(dplyr)
library(mice)
library(glmnet)
library(caTools)
library(pROC)
library(sqldf)

main_data<-read.csv(file.choose())

# 1. Transform Year_Birth into Age
main_data$age<-as.integer(format(as.Date(Sys.Date(),"%Y%m%d"),"%Y"))-main_data$Year_Birth

# 2. Transform Dt_Customer into Enrollment_Length
main_data$Year<-as.integer(format(as.Date(main_data$Dt_Customer,"%m/%d/%Y"),"%Y"))
main_data$Enrollment_Length<-as.integer(format(as.Date(Sys.Date(),"%Y%m%d"),"%y"))-main_data$Year

# 3. Transform Currency format into numbers
main_data$Income<- gsub("\\,", "", main_data$Income)
main_data$Income<- gsub("\\$", "", main_data$Income)
main_data$Income<- as.numeric(main_data$Income)

# 4. Recode categorical variable
main_data$Education<- gsub("2n Cycle","Master",main_data$Education)
main_data$Education<- gsub("Basic","Highschool",main_data$Education)
main_data$Education<- gsub("Graduation","Undergrad",main_data$Education)

main_data$Marital_Status<- gsub("Absurd","Single",main_data$Marital_Status)
main_data$Marital_Status<- gsub("Alone","Single",main_data$Marital_Status)
main_data$Marital_Status<- gsub("YOLO","Single",main_data$Marital_Status)

#5. Missing value imputation
main_data$Income[is.na(main_data$Income)]= median(main_data$Income, na.rm = TRUE)

#6. Dummy variable creation -----------------------------------------------------
main_data$Divorced<- ifelse(main_data$Marital_Status=='Divorced',1,0)  
main_data$Single<- ifelse(main_data$Marital_Status=='Single',1,0)
main_data$Married <- ifelse(main_data$Marital_Status=='Married',1,0)
main_data$Together<- ifelse(main_data$Marital_Status=='Together',1,0) 
# main_data$Widow<- ifelse(main_data$Marital_Status=='Widow',1,0)

main_data$Undergrad<-ifelse(main_data$Education=='Undergrad',1,0)  
main_data$PhD <-ifelse(main_data$Education=='PhD',1,0)        
main_data$Master <-ifelse(main_data$Education=='Master',1,0)     
# main_data$Highschool <-ifelse(main_data$Education=='Highschool',1,0) 

main_data$Australia<-ifelse(main_data$Country=='AUS',1,0)  
main_data$Canada <-ifelse(main_data$Country=='CA',1,0)        
main_data$Germany <-ifelse(main_data$Country=='GER',1,0)     
main_data$India <-ifelse(main_data$Country=='IND',1,0)
main_data$Mexico <-ifelse(main_data$Country=='ME',1,0)
main_data$SouthAfrica <-ifelse(main_data$Country=='SA',1,0)
main_data$Spain <-ifelse(main_data$Country=='SP',1,0)
# main_data$USA <-ifelse(main_data$Country=='US',1,0)

md.pattern(main_data) # no missing data
hist(main_data)

rmcol = c("ID", "Year_Birth", "Education", "Marital_Status", "Dt_Customer","Year","Country")
main_data = main_data[ , !(names(main_data) %in% rmcol) ]

write.csv(main_data,"C:\\Users\\16477\\Documents\\1Post-Grad Work\\1. Course Work\\MMA 831 - Marketing Analytics\\Final Project\\MarketingDataClean.csv", row.names = FALSE)

#-------------------------------- Logistic model
set.seed(6)
inx <- sample.split(seq_len(nrow(main_data)), 0.8)
train <- main_data[inx, ]
test <-  main_data[!inx, ]

# model with significant features
modelL1 = glm(Response ~ Teenhome+Recency+MntMeatProducts+NumDealsPurchases+NumWebPurchases+
                         NumWebVisitsMonth+NumStorePurchases+NumCatalogPurchases+AcceptedCmp1+AcceptedCmp2+
                         AcceptedCmp3+AcceptedCmp4+AcceptedCmp5+Enrollment_Length+Married+Together+PhD, 
                         data=train, family=binomial)

summary(modelL1)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -1.015e+01  1.130e+00  -8.983  < 2e-16 ***
#   Teenhome            -9.146e-01  2.075e-01  -4.407 1.05e-05 ***
#   Recency             -2.831e-02  3.292e-03  -8.601  < 2e-16 ***
#   MntMeatProducts      1.805e-03  5.058e-04   3.568  0.00036 ***
  #   NumDealsPurchases    1.467e-01  5.246e-02   2.796  0.00517 ** 
  #   NumWebPurchases      8.776e-02  3.381e-02   2.596  0.00944 ** 
  #   NumWebVisitsMonth    1.126e-01  4.999e-02   2.252  0.02431 *  
#   NumStorePurchases   -1.412e-01  3.553e-02  -3.974 7.06e-05 ***
#   NumCatalogPurchases  8.762e-02  4.321e-02   2.028  0.04257 *  
#   AcceptedCmp1         1.397e+00  3.085e-01   4.530 5.90e-06 ***
#   AcceptedCmp2         1.469e+00  6.464e-01   2.273  0.02305 *  
#   AcceptedCmp3         1.924e+00  2.595e-01   7.412 1.24e-13 ***
#   AcceptedCmp4         9.607e-01  3.008e-01   3.194  0.00140 ** 
#   AcceptedCmp5         1.879e+00  3.062e-01   6.136 8.48e-10 ***
#   Enrollment_Length    1.059e+00  1.414e-01   7.488 7.01e-14 ***
#   Married             -1.337e+00  2.045e-01  -6.536 6.33e-11 ***
#   Together            -1.479e+00  2.413e-01  -6.131 8.71e-10 ***
#   PhD                  8.199e-01  2.008e-01   4.083 4.45e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1466.27  on 1791  degrees of freedom
# Residual deviance:  903.98  on 1774  degrees of freedom
# AIC: 939.98
# 
# Number of Fisher Scoring iterations: 6

PredictL1 = predict(modelL1, newdata=test, type="response")

rocL1 = roc(test$Response ~ PredictL1, plot = TRUE, print.auc = TRUE)
# AUC:0.902 

confusion.matrix<-table(test$Response, PredictL1 >= 0.15)
Count.correct<-confusion.matrix[1,1]+confusion.matrix[2,2]
Count.wrong<-confusion.matrix[1,2]+confusion.matrix[2,1]
confusion.matrix
#    FALSE TRUE
# 0   304   65
# 1    12   67

Accuracy.rate<-Count.correct/(Count.correct+Count.wrong)
# 0.828

#------------------------------------- Regression model

#create a new dataset for regression model with Aggregate total spend 
main_data2<-main_data
main_data2$total_spend<-rowSums(select(main_data2,contains("Mnt")))
main_data2<-select(main_data2,-contains("Mnt"))

set.seed(6)
inxR <- sample.split(seq_len(nrow(main_data2)), 0.8)
trainR <- main_data2[inxR, ]
testR <- main_data2[!inxR, ]

str(trainR)
hist(trainR$total_spend) # left_skewed ---> log transformation

modelR2 <- lm(log(total_spend) ~.,trainR) 
summary(modelR2) 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          2.517e+00  1.903e-01  13.224  < 2e-16 ***
#   Income               6.497e-06  7.233e-07   8.982  < 2e-16 ***
#   Kidhome             -4.253e-01  3.296e-02 -12.904  < 2e-16 ***
#   Teenhome            -1.679e-01  2.822e-02  -5.950 3.23e-09 ***
#   Recency              1.017e-03  4.744e-04   2.145  0.03211 *  
#   NumDealsPurchases    9.807e-02  9.138e-03  10.733  < 2e-16 ***
#   NumWebPurchases      1.582e-01  6.365e-03  24.860  < 2e-16 ***
#   NumCatalogPurchases  1.323e-01  7.118e-03  18.589  < 2e-16 ***
#   NumStorePurchases    1.171e-01  5.921e-03  19.778  < 2e-16 ***
#   NumWebVisitsMonth   -6.089e-02  8.438e-03  -7.216 7.88e-13 ***
#   AcceptedCmp4         2.607e-01  5.627e-02   4.634 3.85e-06 ***
#   AcceptedCmp3         8.358e-02  5.558e-02   1.504  0.13276    
#   AcceptedCmp2        -2.928e-02  1.335e-01  -0.219  0.82634    
#   AcceptedCmp1         8.662e-02  6.200e-02   1.397  0.16257    
#   AcceptedCmp5         1.787e-01  6.288e-02   2.842  0.00453 ** 
#   Response             1.253e-01  4.503e-02   2.783  0.00545 ** 
#   Enrollment_Length    1.400e-01  2.159e-02   6.483 1.16e-10 ***
#   Undergrad            2.256e-01  8.869e-02   2.544  0.01104 *  
#   PhD                  1.942e-01  9.227e-02   2.104  0.03549 *  
#   Master               2.332e-01  9.031e-02   2.582  0.00990 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5614 on 1772 degrees of freedom
# Multiple R-squared:  0.8585,	Adjusted R-squared:  0.857 
# F-statistic: 565.8 on 19 and 1772 DF,  p-value: < 2.2e-16

predict_R2 = predict(modelR2, newdata=testR)
plot(predict_R2,log(testR$total_spend),xlab="predicted",ylab="actual") 
MSE2 <- mean((log(testR$total_spend)-predict_R2)^2) # 0.274434
MAPE2 <- mean(abs(predict_R2-log(testR$total_spend))/log(testR$total_spend)*100) # 7.841577



# --------------------------- Business Decision/Recommendation

target <- main_data2[,c(15,33)]

summary(target) # mean 605.80

target$spend_group <- ifelse(target$total_spend >= 605.8,'H','L')

target_agg <- sqldf('select spend_group,Response,count(*) from target 
                           group by spend_group, Response')

target_agg
# spend_group Response count(*)
#            H        0      715      more analysis needed to find preferred marketing channel
#            H        1      203      focus on customer loyalty and avoid churns 
#            L        0     1191      target on the product they tend to purchase and cross-sell
#            L        1      131      customized offers to increase their basket size

  
