# San Diego Padres - Data Project
# By Christian A. Hanish (Nov. 2019)

# Import Data
systemA <- read.csv("/Users/christianhanish/Desktop/San Diego Padres/systemA.csv")
systemB <- read.csv("/Users/christianhanish/Desktop/San Diego Padres/systemB.csv")

# ------ Begin Data Pre-Processing & Multiple Imputation by Chained Equations (MICE) ------ #

# Check for Missing Data
summary(systemA)
summary(systemB)

# Remove Categorical Data
head(systemA[ , -c(1,2,3,4)])
head(systemB[ , -c(1,2,3,4)])

# Create Function for Checking Percentage of Missing Data
percentMiss = function(x) { sum(is.na(x)) / length(x) * 100 }

# Calculate Percentage of Missing Data for Each Column
apply(systemA[ , -c(1,2,3,4)], 2, percentMiss)
apply(systemB[ , -c(1,2,3,4)], 2, percentMiss)

# Calculate Percentage of Missing Data for Each Row in System A
missingA = apply(systemA[ , -c(1,2,3,4)], 1, percentMiss)
summary(missingA)
table(missingA)

# Calculate Percentage of Missing Data for Each Row in System B
missingB = apply(systemB[ , -c(1,2,3,4)], 1, percentMiss)
summary(missingB)
table(missingB)

# Subset Out the Bad Rows in System A
replacePitchesA = systemA[ missingA < 26, ]
dontReplacePitchesA = systemA[ missingA > 26, ]

# Subset Out the Bad Rows in System B
replacePitchesB = systemB[ missingB < 26, ]
dontReplacePitchesB = systemB[ missingB > 26, ]

# Figure Out Which Columns to Exclude in System A
replaceColumnA = replacePitchesA[ , -c(1,2,3,4)]
dontReplaceColumnA = replacePitchesA[ , c(1,2,3,4)]

# Figure Out Which Columns to Exclude in System B
replaceColumnB = replacePitchesB[ , -c(1,2,3,4)]
dontReplaceColumnB = replacePitchesB[ , c(1,2,3,4)]

# Now Run MICE
# Load MICE Library
library(mice)

# Set a Temporary Place Holder & Apply MICE 
# (This function will figure out what data is missing and how to replace it.)
tempNoMissingA = mice(replaceColumnA)
tempNoMissingB = mice(replaceColumnB)

# Because tempNoMissingA Finds No Missing Values
noMissingA = replaceColumnA

# Now, Put Replaced Data Back Into Our Dataset
noMissingB = complete(tempNoMissingB,1)

# Confirm That There's No More Missing Data
summary(noMissingA)
summary(noMissingB)

# Put Everything Back Together
# Taking Our Replaced Data and Adding Back in the Columns We Couldn't Replace
combinedA = cbind(dontReplaceColumnA, noMissingA) ##listwise
combinedB = cbind(dontReplaceColumnB, noMissingB) ##listwise

# And Then Add Back in the Rows We Could Not Replace
combinedMissingA = rbind(combinedA, dontReplacePitchesA) ##pairwise
combinedMissingB = rbind(combinedB, dontReplacePitchesB) ##pairwise

write.csv(combinedA, file = "CleanedSystemA.csv")
write.csv(combinedB, file = "CleanedSystemB.csv")

# ------ Complete Data Pre-Processing & Multiple Imputation by Chained Equations (MICE) ------ #

# ------ Begin Machine Learning Analysis with k-Nearest Neighbors Algorithm ------ #

# Import Training and Testing Sets for Systems A and B
trainingA <- read.csv("/Users/christianhanish/Desktop/San Diego Padres/trainingA.csv")
testingA <- read.csv("/Users/christianhanish/Desktop/San Diego Padres/testingA.csv")

trainingB <- read.csv("/Users/christianhanish/Desktop/San Diego Padres/trainingB.csv")
testingB <- read.csv("/Users/christianhanish/Desktop/San Diego Padres/testingB.csv")

# Create Normalization Function
normalize <- function(x) {
  + return( (x - min(x)) / (max(x) - min(x)) ) 
  }

# Normalize Numerical Features of Training Sets
normalTrainingA <- as.data.frame(lapply(trainingA[ , c(2,3,5,6,7)], normalize))
normalTrainingB <- as.data.frame(lapply(trainingB[ , c(2,3,5,6,7,8)], normalize))

# Normalize Numerical Features of Testing Sets
normalTestingA <- as.data.frame(lapply(testingA[ , c(2,3,5,6,7)], normalize))
normalTestingB <- as.data.frame(lapply(testingB[ , c(2,3,5,6,7,8)], normalize))

# Determine Target Variable in Training Sets
trainingTargetA <- trainingA[4]
trainingTargetB <- trainingB[4]

# Determine Target Variable in Testing Sets
testingTargetA <- testingA[4]
testingTargetB <- testingB[4]

# Load Class Package
require(class)

# Determine K Values
kvalueA <- sqrt(2498)
kvalueB <- sqrt(2475)

# Build kNN Algorithms
classA = trainingTargetA[,1]
knnA <- knn(train = normalTrainingA, test = normalTestingA, classA, kvalueA)

classB = trainingTargetB[,1]
knnB <- knn(train = normalTrainingB, test = normalTestingB, classB, kvalueB)

# View Predictions and Summaries of kNN Algorithms
knnA
summary(knnA)

knnB
summary(knnB)

# Combine kNN Predictions with Testing Sets
predictionsSystemA <- cbind(testingA[ , -c(4)], knnA)
predictionsSystemB <- cbind(testingB[ , -c(4)], knnB)

# Export Predictions as CSV Fies
write.csv(predictionsSystemA, file = "System_A_Predictions.csv")
write.csv(predictionsSystemB, file = "System_B_Predictions.csv")

# ------ Complete Machine Learning Analysis with k-Nearest Neighbors Algorithm ------ #







# ------ Additional: This is how you could test the accuracy of your kNN algorithm. ------ #

# Divide Training Set Into Training and Testing Subsets using 75:25 Ratio Rule
trainingAccuracyTestA <- trainingA[1:1873,]
testingAccuracyTestA <- trainingA[1874:2498,]

trainingAccuracyTestB <- trainingB[1:1856,]
testingAccuracyTestB <- trainingB[1857:2475,]

# Create Normalization Function
normalize <- function(x) {
  + return( (x - min(x)) / (max(x) - min(x)) ) 
}

# Normalize Numerical Features of Training Sets
normalTrainingAccuracyTestA <- as.data.frame(lapply(trainingAccuracyTestA[, c(2,3,5,6,7)], normalize))
normalTrainingAccuracyTestB <- as.data.frame(lapply(trainingAccuracyTestB[, c(2,3,5,6,7,8)], normalize))

# Normalize Numerical Features of Testing Sets
normalTestingAccuracyTestA <- as.data.frame(lapply(testingAccuracyTestA[, c(2,3,5,6,7)], normalize))
normalTestingAccuracyTestB <- as.data.frame(lapply(testingAccuracyTestB[, c(2,3,5,6,7,8)], normalize))

# Load class package
require(class)

# Determine K Values
kvalueAccuracyTestA <- sqrt(1873)
kvalueAccuracyTestB <- sqrt(1856)

# Determine Target Variable in Training Sets
trainingTargetAccuracyTestA <- trainingAccuracyTestA[, 4]
trainingTargetAccuracyTestB <- trainingAccuracyTestB[, 4]

# Determine Target Variable in Testing Sets
testingTargetAccuracyTestA <- testingAccuracyTestA[, 4]
testingTargetAccuracyTestB <- testingAccuracyTestB[, 4]

# Build kNN Algorithms
knnAccuracyTestA <- knn(train = normalTrainingAccuracyTestA, test = normalTestingAccuracyTestA, trainingTargetAccuracyTestA, kvalueAccuracyTestA)
knnAccuracyTestB <- knn(train = normalTrainingAccuracyTestB, test = normalTestingAccuracyTestB, trainingTargetAccuracyTestB, kvalueAccuracyTestB) 

# View Predictions and Summaries of kNN Algorithms
knnAccuracyTestA
summary(knnAccuracyTestA)
str(knnAccuracyTestA)

knnAccuracyTestB
summary(knnAccuracyTestB)
str(knnAccuracyTestB)

# Check Accuracy of kNN Algorithm (X Axis: Predictions, Y Axis: Observed)
table(testingTargetAccuracyTestA, knnAccuracyTestA)
table(testingTargetAccuracyTestB, knnAccuracyTestB)

# ------ END, THANK YOU ------ #