# FILE: r_project_fiska_syrmos.R 
# PROJECT: R PROJECT

# INSTALL AND LOAD PACKAGES ###################################################

# install "pacman manager" if needed
if(!require("pacman")) install.packages("pacman")

# load contributed packages with pacman
pacman::p_load(pacman,party,tidyverse,knitr,readr,dplyr,ggplot2,mlr,cowplot)

# LOADING DATASET #############################################################

# adding file
df <- read.table("heart.dat", header=F)

# adding header
names(df) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num") 


# DATA EXPLORATION ############################################################ 

cor(df)

head(df,5)

df %>% cor() %>% round(2)
summary(df)
dim(df)
str(df)
summarizeColumns(df)
summarizeColumns(df) %>% knitr::kable( caption =  'Heart Data')

#check how many had heart disease
df$num[df$num > 1] <- 2
col <- c("#7FFFD4", "#EE2C2C")
barplot(table(df$num), main="Heart disease", col=col)

# change a few predictor variables from integer to factors
convert.types <- function(obj,types){
  for (i in 1:length(obj)){
    FUN <- switch(types[i],character = as.character, 
                  numeric = as.numeric, 
                  factor = as.factor)
    obj[,i] <- FUN(obj[,i])
  }
  obj
}

classes <-c("numeric","factor","factor","numeric","numeric","factor","factor","numeric","factor","numeric","factor","factor","factor","factor")

df <- convert.types(df,classes)

heart = df #add labels only for plot
levels(heart$num) = c("No disease","Disease")
levels(heart$sex) = c("female","male","")
mosaicplot(heart$sex ~ heart$num,
           main="Fate by Gender", shade=FALSE,color=TRUE,
           xlab="Gender", ylab="Heart disease")

boxplot(heart$age ~ heart$num,
        main="Fate by Age",
        ylab="Age",xlab="Heart disease")

#test train split data
library(caret)

set.seed(10)
TrainRows <- createDataPartition(df$num,p=0.70,list=TRUE)
trainData <- df[TrainRows$Resample1,]
testData <-  df[-TrainRows$Resample1,]
nrow(trainData)/(nrow(testData)+nrow(trainData)) #checking if really 70% -> OK

#exploratory analysis
plot(chol ~ age, data=trainData, pch=20, col=trainData$num)
grid()

plot(thalach ~ age, data=trainData, pch=20, col=trainData$num)
grid()


# PREDICTION MODELS ########################################################## 

# with cross-validation, Random Forest & Logistic regression Models 
cv10 = trainControl(method = "cv", number = 10)

#Feature Importance
# train the model
hd_feat <- caret::train(num ~ ., data=trainData, method="lvq", preProcess="scale", trControl=cv10)
# estimate variable importance
importance <- varImp(hd_feat, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


# Random Forest
hd_rf =  caret::train(
  form = num ~ + thal + ca + cp + oldpeak + thalach + slope + exang,
  data = trainData,
  method = "rf",
  trControl = cv10
)
hd_rf

# Logistic Regression
hd_lm =  caret::train(
  form = num ~ + thal + ca + cp + oldpeak + thalach + slope + exang, 
  data = trainData,
  method = "glm",
  trControl = cv10
)
hd_lm

# predict on test dataset
hd_rf_pred<-predict(hd_rf,testData)
hd_lm_pred<-predict(hd_lm,testData)


# RESULTS #####################################################################
confusionMatrix(hd_rf_pred,testData$num)
confusionMatrix(hd_lm_pred,testData$num)

# CLEAR ENVIRONMENT ###########################################################
rm(list = ls())
