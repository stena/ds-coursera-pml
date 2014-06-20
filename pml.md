Title
========================================================

One thing that people regularly do is quantify how much of a particular activity they do, 
but they rarely quantify how well they do it. In this project, your goal will be to use data 
from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. 
They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 
More information is available from the website here: 
http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 






```r
library(caret)
library(doParallel)

cl <- makeCluster(4)  # Use 4 cores
registerDoParallel(cl)

set.seed(123)
pml <- read.csv("pml-training.csv", header = TRUE)
pmltest <- read.csv("pml-testing.csv", header = TRUE)

# Reducing dataset
NAs <- apply(pml, 2, function(x) {
    sum(is.na(x))
})

validData <- pml[, which(NAs < 19216)]

NAs <- apply(validData, 2, function(x) {
    sum(is.na(x))
})

sum(NAs) == 0  # True! No more NAs in validData
```

```
## [1] TRUE
```

```r
length(validData)  # 93
```

```
## [1] 93
```

```r
validData <- validData[, 8:93]
length(validData)  # 86
```

```
## [1] 86
```

```r

vd <- validData[sapply(validData[-86], function(x) is.integer(x) | is.numeric(x))]
length(vd)  # 53
```

```
## [1] 53
```

```r

a <- names(vd)
c <- pmltest[a[-53]]

# Create trianing and cross validation sets
trainindex <- createDataPartition(vd$classe, p = 0.5)[[1]]
training <- vd[trainindex, ]
crossval <- vd[-trainindex, ]



# ptm <- proc.time() d <- ''
modFit <- train(training$classe ~ ., method = "rf", trControl = trainControl(method = "cv", 
    number = 4, allowParallel = T, verboseIter = F), data = training[, -53])
# proc.time() - ptm
modFit
```

```
## Random Forest 
## 
## 9812 samples
##   51 predictors
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (4 fold) 
## 
## Summary of sample sizes: 7359, 7359, 7360, 7358 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##   2     1         1      0.003        0.004   
##   30    1         1      0.004        0.005   
##   50    1         1      0.006        0.007   
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27.
```

```r

confusionMatrix(crossval$classe, predict(modFit, crossval[, -53]))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2783    6    0    0    1
##          B   22 1872    4    0    0
##          C    0   27 1680    4    0
##          D    0    0   21 1584    3
##          E    0    0    5    3 1795
## 
## Overall Statistics
##                                         
##                Accuracy : 0.99          
##                  95% CI : (0.988, 0.992)
##     No Information Rate : 0.286         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.988         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.992    0.983    0.982    0.996    0.998
## Specificity             0.999    0.997    0.996    0.997    0.999
## Pos Pred Value          0.997    0.986    0.982    0.985    0.996
## Neg Pred Value          0.997    0.996    0.996    0.999    1.000
## Prevalence              0.286    0.194    0.174    0.162    0.183
## Detection Rate          0.284    0.191    0.171    0.161    0.183
## Detection Prevalence    0.284    0.193    0.174    0.164    0.184
## Balanced Accuracy       0.996    0.990    0.989    0.996    0.998
```

```r


# d <- predict(modFit, c) d == res[[1]]
```


