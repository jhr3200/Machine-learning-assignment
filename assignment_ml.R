#
library(ISLR)
library(ggplot2)
library(caret)

setwd("C:/Users/james/Desktop/cousera notes/8.Machine learning/Assignment")
#setwd("C:/Users/200017747/Desktop/current coursera notes")


#===Load script to preprocess raw data
source("preprocessdat.R")

#===read data set
rawtrainingset <- read.csv('pml-training.csv')
rawtestingset <- read.csv('pml-testing.csv')

#process training set
trainingdata <- preprocesssdat(rawtrainingset)

names<-c(colnames(trainingdata))
names<-names[-1]# remove classe from list

#set testing set with the same columns
testing <- rawtestingset[ ,names]


# create traing and validation subset in original training set 
trainset <- createDataPartition(y=trainingdata$classe,p=0.75, list=FALSE)
training <- trainingdata[trainset,]
validate <- trainingdata[-trainset,]


#===Exploratory data analysis of training set
library(ISLR)
library(ggplot2)

#Belt rotation 
qplot(classe,roll_belt+yaw_belt+pitch_belt,colour=user_name,
      data=training)+geom_smooth(method='lm',formula=y~x)
#Belt acelleration
qplot(classe,total_accel_belt,colour=user_name,
      data=training)+geom_smooth(method='lm',formula=y~x)
#Belt gyroscope
qplot(classe,gyros_belt_x+gyros_belt_y+gyros_belt_z,colour=user_name,
      data=training)+geom_smooth(method='lm',formula=y~x)
#Belt acelleration
qplot(classe,accel_belt_x+accel_belt_y+accel_belt_z,colour=user_name,
      data=training)+geom_smooth(method='lm',formula=y~x)
#Belt magnet
qplot(classe,magnet_belt_x+magnet_belt_y+magnet_belt_z,colour=user_name,
      data=training)+geom_smooth(method='lm',formula=y~x)

#arm 

#.princ.compsA analysis??
#=============
library(caret)

preProc <- preProcess(training,method="pca",thresh=0.8)
train.princ.comps <- predict(preProc,training)
table(train.princ.comps,4)

#need 12 Principal components to capture 80% variance
# 
# classe      .princ.comps1      .princ.comps2       .princ.comps3       .princ.comps4       .princ.comps5      .princ.comps6        .princ.comps7       .princ.comps8
# 1      A 4.179489 2.077986 -2.762338 0.9379338 -1.274400 2.084078 -0.2529145 -2.712174
# 2      A 4.220294 2.089321 -2.764216 0.9414992 -1.346089 2.158437 -0.2933177 -2.658138
# 3      A 4.187864 2.104716 -2.763748 0.9382302 -1.279939 2.092871 -0.2792318 -2.686537
# 4      A 4.201863 2.098271 -2.749779 0.9365667 -1.292128 2.131370 -0.3047637 -2.654728
# 7      A 4.186491 2.077580 -2.736903 0.9349106 -1.282752 2.104887 -0.3005093 -2.699943
# 8      A 4.187665 2.107379 -2.754515 0.9422096 -1.306591 2.130832 -0.2982379 -2.705329
# .princ.comps9       .princ.comps10      .princ.comps11       .princ.comps12
# 1 -0.09546441 -0.2314456 0.6910796 -0.8037505
# 2 -0.06094221 -0.2653404 0.6735802 -0.7126133
# 3 -0.06231489 -0.2543898 0.6811846 -0.7315637
# 4 -0.03925643 -0.2636963 0.6893296 -0.7588719
# 7 -0.07182123 -0.2625963 0.6825602 -0.7994052
# 8 -0.07375460 -0.2509574 0.6683989 -0.7724135
# 

#============================================================

#Basic GLM model
#=========

# run model on classe using all predictors
modelFit <- train(as.numeric(training$classe)~.,method="glm",data=training[,-1])

# calculate .princ.compss for validation data
validate.predictions <- predict(modelFit,newdata=validate[,-1])

# compare results with confusion to get the accuracy
confusionMatrix(as.numeric(validate$classe),round(validate.predictions))

#==========================================
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  -1   0   1   2   3   4   5   6   7
# -1   0   0   0   0   0   0   0   0   0
# 0    0   0   0   0   0   0   0   0   0
# 1    1  23 540 595 229   7   0   0   0
# 2    0   4  42 386 443  73   0   0   1
# 3    0   0   1 189 547 116   2   0   0
# 4    0   0   1  95 391 292  25   0   0
# 5    0   0   0  47 278 380 145  46   5
# 6    0   0   0   0   0   0   0   0   0
# 7    0   0   0   0   0   0   0   0   0
# 
# Overall Statistics
# 
# Accuracy : 0.3895          
# 95% CI : (0.3758, 0.4033)
# No Information Rate : 0.385           
# P-Value [Acc > NIR] : 0.2638          
# 
# *** 39% accuracy
#

#Repeat GLM with Principal Component analysis 

# run model on classe and principle components use as.numeric to convert factors
modelFit <- train(as.numeric(training$classe) ~ .,method="glm",data=train.princ.comps)

# calculate .princ.compss for validation data
validate.princ.comps <- predict(preProc,validate[,-1])

# used test data motel to predict validation data
validatepredictions<-predict(modelFit,validate.princ.comps)

#..........note "validatepredictions" predictions are not integers 
validatepredictions<-round(predict(modelFit,validate.princ.comps))

# compare results with confusion to get the accuracy
confusionMatrix(as.numeric(validate$classe),validatepredictions)

#==========================================
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   1   2   3   4   5
# 1 242 553 534  66   0
# 2   7 277 591  74   0
# 3   0 316 525  14   0
# 4   0  93 473 234   4
# 5   5 130 515 236  15
# 
# Overall Statistics
# 
# Accuracy : 0.2637          
# 95% CI : (0.2514, 0.2762)
# No Information Rate : 0.5379          
# P-Value [Acc > NIR] : 1

# ***Only 26% accuracy

#not much over chance!

#============================================================

#Regression tree model
#=====================

#install.packages("rattle")
install.packages("rpart.plot")
library(rpart.plot)#tree plot
library(caret)

#==========

set.seed(125)
modelFit<-train(classe ~.,method = "rpart",data = training)

# plot tree
rpart.plot(modelFit$finalModel)
  
# calculate for validation data
validatepredictions <- predict(modelFit,newdata=validate[,-1])

# compare actual validation data compared to model prediction
confusionMatrix(validate$classe,validatepredictions)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    A    B    C    D    E
# A 1250   23  120    0    2
# B  378  320  251    0    0
# C  398   22  435    0    0
# D  367  136  301    0    0
# E  119  111  255    0  416
# 
# Overall Statistics
# 
# Accuracy : 0.4937          
# 95% CI : (0.4796, 0.5078)
# No Information Rate : 0.5122          
# P-Value [Acc > NIR] : 0.9955          
# 
# Kappa : 0.3391          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.4976  0.52288   0.3194       NA  0.99522
# Specificity            0.9394  0.85345   0.8814   0.8361  0.89189
# Pos Pred Value         0.8961  0.33720   0.5088       NA  0.46171
# Neg Pred Value         0.6404  0.92617   0.7711       NA  0.99950
# Prevalence             0.5122  0.12480   0.2777   0.0000  0.08524
# Detection Rate         0.2549  0.06525   0.0887   0.0000  0.08483
# Detection Prevalence   0.2845  0.19352   0.1743   0.1639  0.18373
# Balanced Accuracy      0.7185  0.68816   0.6004       NA  0.94355

50% accuracy

#==================================================================


#Run a GBM boosted model with trees
#==================================
set.seed(125)

#training control notes
#For a gradient boosting machine (GBM) model, there are three main tuning parameters: 
# . number of iterations, i.e. trees, (called n.trees in the gbm function)
# . complexity of the tree, called interaction.depth
# . learning rate: how quickly the algorithm adapts, called shrinkage 
# . the minimum number of training set samples in a node to commence splitting (n.minobsinnode)

gbmcontrol <-  expand.grid(interaction.depth = c(1, 2, 4),
                           n.trees = 100,
                           shrinkage = 0.1,
                           n.minobsinnode = 3)

modelFit <- train(classe ~.,method="gbm",data=training,verbose=FALSE,tuneGrid = gbmcontrol)

# calculate .princ.compss for validation data
validatepredictions <- predict(modelFit,newdata=validate[,-1])

# compare actual validation data compared to model prediction
confusionMatrix(validate$classe,validatepredictions)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    A    B    C    D    E
# A 1030    5    0    1    0
# B   16  698   14    1    0
# C    0   17  615    3    0
# D    0    0   12  583    3
# E    1    2    4    5  685
# 
# Overall Statistics
# 
# Accuracy : 0.9773          
# 95% CI : (0.9719, 0.9818)
# No Information Rate : 0.2834          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9713          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9838   0.9668   0.9535   0.9831   0.9956
# Specificity            0.9977   0.9896   0.9934   0.9952   0.9960
# Pos Pred Value         0.9942   0.9575   0.9685   0.9749   0.9828
# Neg Pred Value         0.9936   0.9919   0.9902   0.9968   0.9990
# Prevalence             0.2834   0.1954   0.1746   0.1605   0.1862
# Detection Rate         0.2788   0.1889   0.1664   0.1578   0.1854
# Detection Prevalence   0.2804   0.1973   0.1719   0.1618   0.1886
# Balanced Accuracy      0.9907   0.9782   0.9735   0.9892   0.9958


97 % accuracy
#=================

#Rerun smaller coarser model?
gbmcontrol <-  expand.grid(interaction.depth = c(1, 2, 4),
                           n.trees = 50,
                           shrinkage = 0.1,
                           n.minobsinnode = 3)

modelFit <- train(classe ~.,method="gbm",data=training,verbose=FALSE,tuneGrid = gbmcontrol)

# calculate .princ.compss for validation data
validatepredictions <- predict(modelFit,newdata=validate[,-1])

# compare actual validation data compared to model prediction
confusionMatrix(validate$classe,validatepredictions)


# 
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    A    B    C    D    E
# A 1011   13    4    6    2
# B   49  640   35    5    0
# C    0   31  597    7    0
# D    4    5   24  561    4
# E    3   11   18    7  658
# 
# Overall Statistics
# 
# Accuracy : 0.9383        
# 95% CI : (0.93, 0.9458)
# No Information Rate : 0.2888        
# P-Value [Acc > NIR] : < 2.2e-16     
# 
# Kappa : 0.9219        
# Mcnemar's Test P-Value : 4.261e-10     
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9475   0.9143   0.8805   0.9573   0.9910
# Specificity            0.9905   0.9703   0.9874   0.9881   0.9871
# Pos Pred Value         0.9759   0.8779   0.9402   0.9381   0.9440
# Neg Pred Value         0.9789   0.9798   0.9735   0.9919   0.9980
# Prevalence             0.2888   0.1894   0.1835   0.1586   0.1797
# Detection Rate         0.2736   0.1732   0.1616   0.1518   0.1781
# Detection Prevalence   0.2804   0.1973   0.1719   0.1618   0.1886
# Balanced Accuracy      0.9690   0.9423   0.9340   0.9727   0.9890

94% accuracy
#====================================================================
#Run a random forest model on the data
modelFit <- train(classe ~.,method="rf",data=training,prox=TRUE,ntree = 2)


# calculate .princ.compss for validation data
validatepredictions <- predict(modelFit,newdata=validate)

# compare actual validation data compared to model prediction
confusionMatrix(validate$classe,validatepredictions)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    A    B    C    D    E
# A 1347   28    9    8    3
# B   33  850   33   22   11
# C    8   29  788   24    6
# D    6   11   21  758    8
# E    7   11   16   12  855
# 
# Overall Statistics
# 
# Accuracy : 0.9376          
# 95% CI : (0.9305, 0.9442)
# No Information Rate : 0.2857          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.9211          
# Mcnemar's Test P-Value : 0.297           
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9615   0.9150   0.9089   0.9199   0.9683
# Specificity            0.9863   0.9751   0.9834   0.9887   0.9886
# Pos Pred Value         0.9656   0.8957   0.9216   0.9428   0.9489
# Neg Pred Value         0.9846   0.9800   0.9805   0.9839   0.9930
# Prevalence             0.2857   0.1894   0.1768   0.1680   0.1801
# Detection Rate         0.2747   0.1733   0.1607   0.1546   0.1743
# Detection Prevalence   0.2845   0.1935   0.1743   0.1639   0.1837
# Balanced Accuracy      0.9739   0.9450   0.9461   0.9543   0.9784
