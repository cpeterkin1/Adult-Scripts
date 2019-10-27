setwd("C:/Users/Chris/Desktop/R Scripts/Adult/Adult Data")

#read data
adult.data <- read.csv("adult_df.csv")

adult.test <- read.csv("test_df.csv")

#load packages
library(rpart)
library(caret)
library(e1071)
library(nnet)

#create test and training data
ratio = sample(1:nrow(adult.data), size = 0.25*nrow(adult.data))
test.data = adult.data[ratio,] #Test dataset 25% of total
train.data = adult.data[-ratio,] #Train dataset 75% of total

#Logistic Regression
glm.fit<- glm(income~., family=binomial(link='logit'),data = train.data)
set.seed(1234)
glm.pred<- predict(glm.fit, test.data, type = "response")
table(actual= test.data$income, predicted= glm.pred>0.5)

#Decision Tree
tree.model<- rpart(income~., data=train.data, method="class", minbucket=20)
tree.predict<- predict(tree.model, test.data, type = "class")
confusionMatrix(test.data$income, tree.predict)

#SVM
svm.model<- svm(income~., data = train.data,kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, test.data)
confusionMatrix(test.data$income, svm.predict)

# Random Forest
rf.model<- randomForest(income~., 
                        data = train.data, 
                        importance=TRUE,
                        keep.forest=TRUE)
rf.predict <- predict(rf.model, test.data)
confusionMatrix(test.data$income, rf.predict)

#Neural Network

covariates <- paste("age", "workclass", "education",
                    "marital_status", "occupation", "relationship",
                    "race", "sex", "native_region", "hours_w",
                    "cap_gain", "cap_loss", sep = "+")
form <- as.formula(paste("income ~", covariates))
neural.fit <- nnet(formula = form,
                   data = train.data,
                   size = 12,
                   decay = 2,
                   maxit = 500,
                   trace = FALSE)
nnet.predict <- predict(neural.fit, 
                          newdata = test.data,
                          type = "class")
#confusionMatrix(test.data$income, nnet.predict)
confusionMatrix(data = nnet.predict, 
                reference = test.data$income,
                positive = levels(test.data$income)[2])


#Naive Bayes
model<-naiveBayes(income~.,data = train.data)
nb.predict<-predict(model,test.data)
confusionMatrix(test.data$income, nb.predict)

#Box Plot
# barchart with added parameters
accuracy = c(86.01,85.42,85.09,84.07,81.96)
barplot(accuracy)
barplot(accuracy,
        main = "Predictive Analysis Accuracy",
        xlab = "Algorithm",
        ylab = "Accuracy (%)",
        names.arg = c("RANDOM FOREST","SVM","LOGISTIC REGRESSION", "DECISION TREE (CART)","NAIVE BAYES"),
        col = "blue",
        horiz = FALSE,
        ylim=c(0,100))