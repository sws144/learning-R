#3.8 Applied Pred --------

# 0 required packages ------
packages <- c("AppliedPredictiveModeling", "caret")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# 1 preprocess data ------
library(AppliedPredictiveModeling)
data(segmentationOriginal)

segData <- subset(segmentationOriginal, Case == "Train")

cellID <- segData$Cell
class <- segData$Class
case <- segData$Case
# Now remove the columns
segData <- segData[, -(1:3)]

statusColNum <- grep("Status", names(segData))

statusColNum

segData <- segData[, -statusColNum]

library(e1071)

# 2 transformations -----
# for one predictor:
skewness(segData$AngleCh1)

# Since all the predictors are numeric columns, the apply function can
# be used to compute the skewness across columns.
skewValues <- apply(segData, 2, skewness)
head(skewValues)

# Box-Cox 
library(caret)
Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
Ch1AreaTrans

# The original data
head(segData$AreaCh1)

# After transformation
predict(Ch1AreaTrans, head(segData$AreaCh1))

(819^(-.9) - 1)/(-.9)

# PCA transformation
pcaObject <- prcomp(segData,
                    center = TRUE, scale. = TRUE)
# Calculate the cumulative percentage of variance which each component
# accounts for.
percentVariance <- pcaObject$sd^2/sum(pcaObject$sd^2)*100
percentVariance[1:3]

# transformed values
head(pcaObject$x[, 1:5])

# another sub-object called rotation stores the variable loadings, where
# rows correspond to predictor variables and columns are associated with the
# components
head(pcaObject$rotation[, 1:3])

