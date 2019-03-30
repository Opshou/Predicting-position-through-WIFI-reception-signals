####UBIQUM MODULE 3, TASK 2: WIFI ALLOCATION###
#
#MAXIMILIÀ GOLDSTON MARÍ
#INITIAL RELEASE (VERSION 1.0): 21/02/2019
#
#FUNCTIONALITY####
#
#We have information on WIFI signals received by different phones and users
#inside of 3 different buildings. Our goal is to find a model that will pred-
#ict the position of a user just by looking at the signals he/ she is getting.


#DATASET & INFORMATION: 
#http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc

#OVERVIEW OF DATASET AND HOW IT WAS COLLECTED:
#https://s3.amazonaws.com/gbstool/courses/614/docs/UJIIndoorLoc%20-%20A%20New%
#20Multi-building%20and%20Multi-floor%20Database%20for%20WLAN%20Fingerprint-
#based%20Indoor%20Localization%20Problems.pdf?AWSAccessKeyId=AKIAJBIZLMJQ2O6D
#KIAA&Expires=1550826000&Signature=YM8OG49nMr38kjS3wIia5PW7Bbs%3D

#CODE STRUCTURE####
#============================================================================#
#/////////LOADING LIBRARIES                                                  #
#============================================================================#
#/////////SET DIRECTORY, IMPORT DATA                                         #
#============================================================================#
#/////////FUNCTIONS:                                                         #
#         -Function 1 spotting_errors: Plotting misclassified values.        #
#         -Function 2 long_lat_knn: Predicting longitude using predicted lat-#
#          itude (or viceversa).                                             #
#============================================================================#
#/////////DATA PREPROCESSING (Done on both original and validation datasets) #
#         -Change 100 values to -105.                                        #
#         -Take out columns and rows that have only -105 values.             #
#         -Taking out duplicated rows.                                       #
#         -Normalizing by rows.                                              #
#         -Doing PCA (we get 127 PCs).                                       #
#============================================================================#
#/////////PREDINCTING BUILDING                                               #
#         -Subsampling in order to have same number of observations per buil-#
#          ding.                                                             #
#         -Training and test set (.85 split).                                #
#         -Knn (CV split = 10, 60 neighbors).                                #
#         -Predicting on testset.                                            #
#         -Plotting original (black) and predicted values (red) -filtered by #
#          building using spotting_errors function.                          #
#         -Applying same preprocess to validation set as for training set.   #
#         -Plotting original (black) and predicted values (red) -filtered by #
#          building) using spotting_errors function.                         #
#============================================================================#
#/////////PREDICTING FLOOR                                                   #
#         -Training and test set (.85 split).                                #
#         -KNN (CV split = 10, 45 neighbors).                                #
#         -Predicting on testset.                                            #
#         -Plotting original (black) and predicted values (red) -filtered by #
#          building using spotting_errors function.                          #
#         -Predicting on validation set.                                     #
#         -Plotting original (black) and predicted values (red) -filtered by #
#          building using spotting_errors function.                          #
#============================================================================#
#/////////PREDICTING LONGITUDE AND LATITUDE                                  #
#         -Using long_lat_knn to predict longitude and latitude: first we    #
#          predict one and use that prediction to get the other one.         #
#         -Visualizing results.                                              #
#         -Plotting original and predicted values.                           #
#============================================================================#

#LOADING LIBRARIES####

library(FactoMineR)     #For PCA
library(factoextra)     #For PCA
library(dplyr) 		#Used for easily selecting data
library(plyr) 		#For ddply function
library(ggplot2) 		#Plotting tool
library(reshape) 		#Used when transposing the df that comes from normalizing
library(caret) 		#Used for Knn
library(scales) 		#Used for rescaling by rows

#============================================================================#
#============================================================================#

#SET DIRECTORY, IMPORT DATA####
setwd("C:/Users/mgold/Desktop/Ubiqum/Module 3/Task 2 Wifi Alocation/
      UJIndoorLoc/UJIndoorLoc")

#Reading the dataset
wifi <- read.csv("trainingData.csv", header = TRUE)

#============================================================================#
#============================================================================#

#FUNCTIONS####
#============================================================================#
#============================================================================#
#                 FUNCTION 1: Plotting missclassified values                 #
#                                                                            #
#This function plots all missclassified values for floor or building on top  #
#of the real values. The colors represent what was predicted.                #
#                                                                            #
#INPUT PARAMETERS: predicted: vector of predicted values <- predict(model,   #
#                  testset).                                                 #
#                  original: testset or validation set.                      #
#                  target_var: string with the name of the target variable.  # 
#OUTPUT PARAMETERS: Plot containing missclassified values on top of real val-#
#                   ues.                                                     #
#============================================================================#
#============================================================================#

spotting_errors <- function(predicted, original, target_var){
  #Creating new column of predicted in original df
  original$Prediction <- predicted
  #Creating new column with correct (1) and incorrect(0)
  original$Correct <- apply(original,1,function(x)
  {ifelse(x[(c(target_var))] == x[c("Prediction")], 1, 0)})
  #Getting the incorrectly predicted observations
  errors <- original %>% filter(Correct == 0)
#Plotting the errors
  ggplot(data = original, aes(LONGITUDE, LATITUDE)) +
    geom_point() + geom_point(data = errors, aes(color= Prediction))
}

#============================================================================#
#============================================================================#
#             FUNCTION 2: Two models for Longitude and Latitude              #
#                                                                            #
#This function predicts LONGITUDE and uses this prediction in order to later #
#find LATITUDE (or viceversa).                                               #
#                                                                            #
#INPUT PARAMETERS: training: training set.                                   #
#                  test_set: testset.                                        #
#                  val_set: validation set.                                  #
#                  first: makes the function decide what to predict first.   # 
#                  Takes the string values of  "LONGITUDE" and "LATITUDE".   #
#OUTPUT: A list with 4 columns: 2 for the Long and Lat predicted for the test#
#        set, and 2 for the Long and Lat predicted for the validation set.   #
#============================================================================#
#============================================================================#

long_lat_knn <- function(training, test_set, val_set, first, neighbors) {
  i <- 0
  while(i<2){
    if(first == "LONGITUDE"){
      #if i is not 0, Longitude will be predicted with the predicted Latitude.
      #These values are in the position ncol(training).
      ifelse(i == 0, columns <- c(1:127, (ncol(training)-9)),
            columns <- c(1:127, (ncol(training)-10), ncol(training)))
      ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
      knnFit <- train(LONGITUDE ~ .,
                      data = training[,columns],
                      method = "knn",
                      trControl = ctrl,
                      preProcess = c("center","scale"),
                      tuneGrid = expand.grid(k = neighbors))
    
      training$predLon <- predict(knnFit, training)
      test_set$predLon <- predict(knnFit, test_set)
      val_set$predLon <- predict(knnFit, val_set)
      i <- i + 1
      first <- "LATITUDE"

    }else{
      #if i is not 0, Longitude will be predicted with the predicted Longitude.
      #These values are in the position ncol(training).
      ifelse(i == 0, columns <- c(1:127, (ncol(training)-8)),
           columns <- c(1:127, (ncol(training)-9), ncol(training)))
      ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
      knnFit <- train(LATITUDE ~ .,
                      data = training[,columns],
                      method = "knn",
                      trControl = ctrl,
                      preProcess = c("center","scale"),
                      tuneGrid = expand.grid(k = neighbors))
    
      training$predLat <- predict(knnFit, training)
      test_set$predLat <- predict(knnFit, test_set)
      val_set$predLat <- predict(knnFit, val_set)
      i <- i + 1
      first <- "LONGITUDE"
    }
  }
  predictions <- list(test_lon = test_set$predLon, test_lat = test_set$predLat,
                      val_lon = val_set$predLon, val_lat = val_set$predLat)
  return(predictions)
}

#DATA PREPROCESSING####

#Putting factors
wifi$FLOOR <- as.factor(wifi$FLOOR)
wifi$BUILDINGID <- as.factor(wifi$BUILDINGID)
wifi$SPACEID <- as.factor(wifi$SPACEID)
wifi$RELATIVEPOSITION <- as.factor(wifi$RELATIVEPOSITION)
wifi$USERID <- as.factor(wifi$USERID)
wifi$PHONEID <- as.factor(wifi$PHONEID)

#subsituting 100 for the min value of the WAPs - 1 (-105).
#we find the min with this: min(wifi[1:(ncol(wifi)-9)])
wifi[wifi==100]<--105

#Taking columns that contain only -105 out.
noSignal <- (as.data.frame(lapply((apply(wifi, 2, max)), function(x)
  {ifelse (x == -105, TRUE, FALSE)})))
wifi <- wifi[,!noSignal]

#Taking out rows that have no WAP measurements
wifi <- wifi[apply(wifi[,1:(ncol(wifi)-9)], 1, function(x) 
  {ifelse(max(x) == -105, FALSE, TRUE)}),]

#Taking out duplicates
wifi <- wifi[!duplicated(wifi[1:(ncol(wifi)-3)]),]

#Normalizing for rows
wifi <- cbind(as.data.frame(t(apply(wifi[,1:(ncol(wifi)-9)], 1, rescale))), 
              wifi[(ncol(wifi)-8):ncol(wifi)])

#Doing PCA in order to reduce dimensionality and optimize the meaning of each 
#dimension.
wifi.pca <- prcomp(wifi[1:(ncol(wifi)-9)],
                   center = TRUE,
                   scale. = TRUE)

#From get_eigenvalue(wifi.pca):
#We choose the threshold of eigenvalues >1 (128PCs). This is justified here: 
#Eigenvalues section.
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/#r-code)
wifi <- as.data.frame(append(as.data.frame(wifi.pca$x[,1:127]), 
                                      wifi[(ncol(wifi)-8):ncol(wifi)]))

#PREPROCESSING OUR VALIDATION DATASET####
#We do the exact same process we did for our original dataset.
validation_set <- read.csv("validationData.csv", header = TRUE)

#Factorizing
validation_set$FLOOR <- as.factor(validation_set$FLOOR)
validation_set$BUILDINGID <- as.factor(validation_set$BUILDINGID)
validation_set$SPACEID <- as.factor(validation_set$SPACEID)
validation_set$RELATIVEPOSITION <- as.factor(validation_set$RELATIVEPOSITION)
validation_set$USERID <- as.factor(validation_set$USERID)
validation_set$PHONEID <- as.factor(validation_set$PHONEID)
validation_set[validation_set==100]<--105
validation_set <- validation_set[,!noSignal]

#Normalizing by rows
WAPS_normalized_val <- t(apply(validation_set[,1:(ncol(validation_set)-9)],
                               1, rescale))
WAPS_normalized_val <- as.data.frame(WAPS_normalized_val)
validation_set <- cbind(
  WAPS_normalized_val, 
  validation_set[(ncol(validation_set)-8):ncol(validation_set)])

#PCA: predict PCA, take 127 PCs and append to the nonWAP variables of our vali-
#dation set
validation_set <- as.data.frame(append(
  as.data.frame(predict(wifi.pca, newdata = validation_set)[,1:127]), 
  validation_set[(ncol(validation_set)-8):ncol(validation_set)]))

#PREDICTING BUILDING####

#Randomly sampling building in order to have the same number of observations 
#for each building. This is done to prevent bias.
building0 <- wifi %>% filter(BUILDINGID == 0)
building0 <- building0[sample(nrow(wifi %>% 
                                     filter(BUILDINGID == 0)), nrow(building1)),]
building1 <- wifi %>% filter(BUILDINGID == 1)
building2 <- wifi %>% filter(BUILDINGID == 2)
building2<- building2[sample(nrow(wifi %>% 
                                    filter(BUILDINGID == 2)),  nrow(building1)),]

#Variable that will be used for KNN
for_building <- rbind(building0, rbind(building1, building2))

#Training and Test sets
set.seed(234)

inTrain <- createDataPartition(
  y = for_building$BUILDINGID,
  p = .85,
  list = FALSE
)

trainingB <- for_building[inTrain,]
testingB  <- for_building[-inTrain,]

#Building the KNN with cross-validation
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
knnFitB <- train(BUILDINGID ~ .,
                 data = trainingB[,c(1:127, (ncol(trainingB)-5))],
                 method = "knn",
                 trControl = ctrl,
                 preProcess = c("center","scale"),
                 tuneGrid = expand.grid(k = 60))

#We use 60 neighbors because we want to find a model that is underfitted, in
#order to generalize better with new variables.
# Mention components of PCA - 1 instead of 128 : Shekhar
#Visualizing Accuracy and Kappa
KnnPredictionB <- predict(knnFitB, testingB[c(1:127, (ncol(trainingB)-5))])
postResample(KnnPredictionB, testingB$BUILDINGID)
confusionMatrix(KnnPredictionB, testingB$BUILDINGID)

#Spotting the errors:
spotting_errors(KnnPredictionB, testingB, "BUILDINGID")

#Predicting building on our validation set.
KnnpredictionB_valset <- predict(knnFitB, 
                                 validation_set[c(1:127, (ncol(trainingB)-5))])
postResample(KnnpredictionB_valset, validation_set$BUILDINGID)
confusionMatrix(KnnpredictionB_valset, validation_set$BUILDINGID)

#Spotting and plotting the errors:
spotting_errors(KnnpredictionB_valset, validation_set, "BUILDINGID")


#PREDICTING FLOOR####

#Creating equal subsets for every building
set.seed(234)
#Creating Train and Test sets
inTrain <- createDataPartition(
  y = wifi$FLOOR,
  p = .85,
  list = FALSE)

trainingF <- wifi[inTrain,]
testingF  <- wifi[-inTrain,]

ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
start_time <- Sys.time()
knnFitF <- train(FLOOR ~ .,
                 data = trainingF[,c(1:127, (ncol(trainingF)-6))],
                 method = "knn",
                 trControl = ctrl,
                 tuneGrid = expand.grid(k = 45),
                 preProc = c("center","scale"))
end_time <- Sys.time()
end_time - start_time

#Predicting for Test set
predictionF_knn <- predict(knnFitF, testingF)
postResample(predictionF_knn, testingF$FLOOR)
confusionMatrix(predictionF_knn, testingF$FLOOR)

#spotting the errors:
spotting_errors(predictionF_knn, testingF, "FLOOR")

#Comparing with validation set
KnnpredictionF_valset <- predict(knnFitF, 
                                 validation_set[c(1:127, (ncol(trainingF)-6))])
postResample(KnnpredictionF_valset, validation_set$FLOOR)
confusionMatrix(KnnpredictionF_valset, validation_set$FLOOR)

#spotting the errors:
spotting_errors(KnnpredictionF_valset, validation_set, "FLOOR")

#KNN FOR LATITUDE AND LONGITUDE####

#Building a column that has all the information of a specific point in space in
#the format of Build_Floor_SpaceID_RelPos (i.e. 0_2_103_2)
wifi$factors <- paste(wifi$BUILDINGID, wifi$FLOOR, 
                      wifi$SPACEID, wifi$RELATIVEPOSITION, 
                      sep = "_")

#This function takes subsamples of 18 measurements for every location in a spe-
#cific building. We do this because it's the median value for measurements.
for_long_lat <- ddply(wifi,.(factors),function(x) 
  x[sample(nrow(x),18, replace = TRUE),] )

#Creating Train and Test sets
inTrain <- createDataPartition(
  y = for_long_lat$LONGITUDE,
  p = .85,
  list = FALSE
)

#We take 0.8
trainingLon_Lat <- for_long_lat[inTrain,]
testingLon_Lat  <- for_long_lat[-inTrain,]

#Predicting longitude and using these values to predict latitude on both our
#test and validation sets. We use our second function long_lat_knn.
#If you want to predict Latitude first, change the "first" parameter in the
#function.

start_time <- Sys.time()
predLongLat <- long_lat_knn(trainingLon_Lat, testingLon_Lat, validation_set, 
                            first = "LATITUDE", neighbors = 10)
end_time <- Sys.time()
end_time - start_time

#Longitude prediction on test set
postResample(predLongLat[[1]], testingLon_Lat$LONGITUDE)
#Latitude prediction on test set
postResample(predLongLat[[2]], testingLon_Lat$LATITUDE)

#Plotting our predictions
testingLon_Lat$LonPred <- predLongLat[[1]]
testingLon_Lat$LatPred <- predLongLat[[2]]

#We use different shapes in order to better distinguish predictions and real.
ggplot(data = testingLon_Lat, aes(LONGITUDE, LATITUDE)) +
  geom_point(col = "black", shape = 1) +
  geom_point(aes(LonPred, LatPred), col = "red", shape = 42)

#Longitude prediction on validation set
postResample(predLongLat[[3]], validation_set$LONGITUDE)
#Latitude prediction on validation set
postResample(predLongLat[[4]], validation_set$LATITUDE)

#Plotting our predictions
validation_set$LonPred <- predLongLat[[3]]
validation_set$LatPred <- predLongLat[[4]]

#We use different shapes in order to better distinguish predictions and real.
ggplot(data = validation_set, aes(LONGITUDE, LATITUDE)) +
  geom_point(col = "black", shape = 1) +
  geom_point(aes(LonPred, LatPred), col = "red", shape = 42)
