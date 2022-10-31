rm(list = ls())

#lets read the dataset
framinghan = read.csv("framingham.csv")

#install.packages("caTools")
library(caTools)
#splitting the dataset
set.seed(5)#getting the same random nos for all
#split1 = sample.split(framinghan,SplitRatio = 0.8)#random sampling of cols
split2 = sample.split(framinghan$TenYearCHD,SplitRatio = 0.8)#random sampling of rows

train = subset(framinghan, split2 == TRUE)
test = subset(framinghan, split2 == FALSE)

#run the logistic regression
model1 = glm(TenYearCHD ~ .,data = train,family = binomial())
#all were predictor variables
summary(model1)

#create a model2
#starred ones are the predictor variables
model2 = glm(TenYearCHD ~ male+age+cigsPerDay+prevalentHyp+sysBP+glucose, data = train,family = binomial())
summary(model2)

#predict
predictions = predict(model2,type = "response",newdata = test)
predDF = data.frame(test,predictions)

#create column for predicting heart problems
predDF$HeartPrediction = predDF$predictions>0.5

#confuse matrix

cf = as.data.frame.matrix(table(predDF$TenYearCHD,predDF$HeartPrediction))

#metrics of the model
accuracy = (643+9)/(643+9+11+109)
#important for study
truepositive = (643)/(643+9+11+109)
truenegative = (9)/(643+9+11+109)
falsepositive = (109)/(643+9+11+109)#most harmful one
falsenegative = (11)/(643+9+11+109)

