Predicting Volunteers Using Decision Trees - Supervised Learning - classification and ensembles
================

**Factors: Age, RFM, Lifetime Raised, Number of degrees, Years since Grad, Gender, Alum, Inclination, Number of Events Attended (not as a volunteer)**
**Target: Volunteer y/n**
**Models attempted: Class, Random Forest**

**To begin, read and numericize numeric data. Remember to edit the filepath to refer to where you've saved the csv.**

``` r
volunteers<-read.csv("W:/BI and Analytics/Data Analytics/DataSets/R/APRA2018/Volunteers.csv", quote = "", row.names = NULL)

numeric<-c("Age", "RFM", "LifetimeDonation", "Degrees" ,"YearsSinceGrad", "Male" ,"Alum", "InclinationScore", "EventsAttended")
volunteers[,numeric]<-sapply(volunteers[,numeric], as.numeric)
```

CART
----

#### A classification tree creates a model that predicts whether a person is a volunteer based on our input variables.

**We view the proportions of 1's and 0's in the data. The proportion of Volunteers (1's) is 4% ( &lt;15%, which is typically considered a rare event.)**

``` r
print(table(volunteers$Target))
```

    ## 
    ##    0    1 
    ## 3900  200

``` r
print(prop.table(table(volunteers$Target)))
```

    ## 
    ##          0          1 
    ## 0.95121951 0.04878049

**Splitting the training and testing datasets**

``` r
library(caret) 
```

    ## Warning: package 'caret' was built under R version 3.4.3

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.4.3

``` r
set.seed(1234)
splitIndex<-createDataPartition(volunteers$Target, p = .8,
                                                  list = FALSE,
                                                  times = 1)

voltrainSplit <- volunteers[ splitIndex,]
voltestSplit <- volunteers[-splitIndex,]
voltrainSplit$Target <- as.factor(voltrainSplit$Target)
voltestSplit$Target <- as.factor(voltestSplit$Target)
```

**We fit the model and see what it looks like.**

``` r
library(rpart)
clModel<-rpart(Target ~ Age + RFM + LifetimeDonation +  Degrees + YearsSinceGrad  + Male  + Alum +  InclinationScore  + EventsAttended,
            data = voltrainSplit,
           method="class")

library(rattle)
```

    ## Warning: package 'rattle' was built under R version 3.4.4

    ## Rattle: A free graphical interface for data science with R.
    ## Version 5.1.0 Copyright (c) 2006-2017 Togaware Pty Ltd.
    ## Type 'rattle()' to shake, rattle, and roll your data.

``` r
fancyRpartPlot(clModel)
```

![](Volunteers_files/figure-markdown_github/unnamed-chunk-4-1.png)

**Here we make the predictions.**

``` r
predCLASS <- predict(clModel, newdata = voltestSplit,type = "class", na.action = na.pass)
```

**And show the confusion matrix (no SMOTE, model = class).**

``` r
confusionMatrix(data = predCLASS, voltestSplit$Target, positive = "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 785   6
    ##          1   0  29
    ##                                           
    ##                Accuracy : 0.9927          
    ##                  95% CI : (0.9841, 0.9973)
    ##     No Information Rate : 0.9573          
    ##     P-Value [Acc > NIR] : 1.133e-09       
    ##                                           
    ##                   Kappa : 0.9025          
    ##  Mcnemar's Test P-Value : 0.04123         
    ##                                           
    ##             Sensitivity : 0.82857         
    ##             Specificity : 1.00000         
    ##          Pos Pred Value : 1.00000         
    ##          Neg Pred Value : 0.99241         
    ##              Prevalence : 0.04268         
    ##          Detection Rate : 0.03537         
    ##    Detection Prevalence : 0.03537         
    ##       Balanced Accuracy : 0.91429         
    ##                                           
    ##        'Positive' Class : 1               
    ## 

**Because there's such a small % of volunteers in the data, we use Synthetic Minority Oversampling Technique to balance out the data.Then we can see if this more balanced dataset helps the model score better than the rare events dataset. Let's try SMOTE-ing the training data to see if we can improve on model's performance.**

``` r
library(DMwR)
```

    ## Warning: package 'DMwR' was built under R version 3.4.4

    ## Loading required package: grid

``` r
voltrainSplit$Target<-as.factor(voltrainSplit$Target)
voltrainSplit <- SMOTE(Target ~ numeric, voltrainSplit, perc.under = 100)
print(table(voltrainSplit$Target))
```

    ## 
    ##   0   1 
    ## 330 495

``` r
print(prop.table(table(voltrainSplit$Target)))
```

    ## 
    ##   0   1 
    ## 0.4 0.6

**Training the model again, this time on smoted dataset**

``` r
clModel_SMOTE<-rpart(Target ~ Age + RFM + LifetimeDonation +  Degrees + YearsSinceGrad  + Male  + Alum +  InclinationScore  + EventsAttended,
            data = voltrainSplit,
           method="class")

fancyRpartPlot(clModel_SMOTE)
```

![](Volunteers_files/figure-markdown_github/unnamed-chunk-8-1.png)

**Predicting on Test data after having trained the model on SMOTE'd data. (Note that test dataset did not undergo SMOTE-ing, just the training set**

``` r
predCLASS_SMOTE <- predict(clModel_SMOTE, newdata = voltestSplit,type = "class", na.action = na.pass)
```

**Based on the Confusion Matrix (SMOTE, model = class), the ability of the classification model to predict volunteers is better using the SMOTE'd data (undersampled to balance out the proportions). Out of the 35 volunteers in the test data, it predicted 32 (instead of 29 before).**

``` r
confusionMatrix(data = predCLASS_SMOTE, voltestSplit$Target, positive="1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 718   3
    ##          1  67  32
    ##                                           
    ##                Accuracy : 0.9146          
    ##                  95% CI : (0.8934, 0.9329)
    ##     No Information Rate : 0.9573          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.4424          
    ##  Mcnemar's Test P-Value : 5.076e-14       
    ##                                           
    ##             Sensitivity : 0.91429         
    ##             Specificity : 0.91465         
    ##          Pos Pred Value : 0.32323         
    ##          Neg Pred Value : 0.99584         
    ##              Prevalence : 0.04268         
    ##          Detection Rate : 0.03902         
    ##    Detection Prevalence : 0.12073         
    ##       Balanced Accuracy : 0.91447         
    ##                                           
    ##        'Positive' Class : 1               
    ## 

Random Forest
-------------

#### The Random Forest, a supervised learning model, is a decision tree-based mode, with two parameters - the number of decision trees, and the number of features to search over to find the best feature that splits the node. By randomly selecting a feature subset for each split, it improves variance by reducing correlation between trees.The method creates a collection of totally unique trees that all make their classifications differently - and the majority decision is chosen. Each tree grows out fully and overfits, but in different ways, so mistakes each makes are averaged out over all of them.

**Splitting the training and testing datasets**

``` r
set.seed(1234)
splitIndex<-createDataPartition(volunteers$Target, p = .8,
                                                  list = FALSE,
                                                  times = 1)
voltrainSplit <- volunteers[ splitIndex,]
voltestSplit <- volunteers[-splitIndex,]
voltrainSplit$Target <- as.factor(voltrainSplit$Target)
voltestSplit$Target <- as.factor(voltestSplit$Target)
```

**We tune the model to confirm the best number of mtry, aka the number of obs randomly selected by the model to use to make splitting decisions at each split. The tuner says 2 (the number of mtry that minimizes the out of bag (prediction) error**

``` r
library(randomForest)
```

    ## Warning: package 'randomForest' was built under R version 3.4.4

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:rattle':
    ## 
    ##     importance

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
set.seed(1)
res<-tuneRF(x=subset(voltrainSplit, select= -Target), y=voltrainSplit$Target, ntreeTry=500)
```

    ## mtry = 3  OOB error = 1.01% 
    ## Searching left ...
    ## mtry = 2     OOB error = 1.04% 
    ## -0.03030303 0.05 
    ## Searching right ...
    ## mtry = 6     OOB error = 1.1% 
    ## -0.09090909 0.05

![](Volunteers_files/figure-markdown_github/unnamed-chunk-12-1.png) **Training the model**

``` r
rfModel<- randomForest(Target ~ Age + RFM + LifetimeDonation +  Degrees + YearsSinceGrad  + Male  + Alum +  InclinationScore  + EventsAttended, data = voltrainSplit, ntree = 500, mtry = 2)
```

**Predicting on Test data**

``` r
predRF <- predict(rfModel, newdata = voltestSplit)
```

**Confusion Matrix (no SMOTE, model = Random Forest)**

``` r
confusionMatrix(data = predRF, voltestSplit$Target, positive="1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 785   6
    ##          1   0  29
    ##                                           
    ##                Accuracy : 0.9927          
    ##                  95% CI : (0.9841, 0.9973)
    ##     No Information Rate : 0.9573          
    ##     P-Value [Acc > NIR] : 1.133e-09       
    ##                                           
    ##                   Kappa : 0.9025          
    ##  Mcnemar's Test P-Value : 0.04123         
    ##                                           
    ##             Sensitivity : 0.82857         
    ##             Specificity : 1.00000         
    ##          Pos Pred Value : 1.00000         
    ##          Neg Pred Value : 0.99241         
    ##              Prevalence : 0.04268         
    ##          Detection Rate : 0.03537         
    ##    Detection Prevalence : 0.03537         
    ##       Balanced Accuracy : 0.91429         
    ##                                           
    ##        'Positive' Class : 1               
    ## 

**The model's performance in identifying volunteers is pretty good but let's try SMOTE-ing the training data to get it better trained to sort 1's.**

``` r
voltrainSplit <- SMOTE(Target ~ numeric, voltrainSplit, perc.under = 100)
```

**Training the model again, this time on smoted dataset**

``` r
rfModel<- randomForest(Target ~ Age + RFM + LifetimeDonation +  Degrees + YearsSinceGrad  + Male  + Alum +  InclinationScore  + EventsAttended, data = voltrainSplit, ntree = 500, mtry = 3,  keep.forest=TRUE)
```

**Predicting on Test data**

``` r
predRF_SMOTE <- predict(rfModel, newdata = voltestSplit)
```

**Confusion Matrix (after SMOTE, model = Random Forest) shows improvement in ability to predict volunteers, but due to its increased sensitivity it'll be more likely now to sense volunteers where there aren't any.**

``` r
confusionMatrix(data = predRF_SMOTE, voltestSplit$Target, positive="1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 756   5
    ##          1  29  30
    ##                                           
    ##                Accuracy : 0.9585          
    ##                  95% CI : (0.9425, 0.9711)
    ##     No Information Rate : 0.9573          
    ##     P-Value [Acc > NIR] : 0.476           
    ##                                           
    ##                   Kappa : 0.6178          
    ##  Mcnemar's Test P-Value : 7.998e-05       
    ##                                           
    ##             Sensitivity : 0.85714         
    ##             Specificity : 0.96306         
    ##          Pos Pred Value : 0.50847         
    ##          Neg Pred Value : 0.99343         
    ##              Prevalence : 0.04268         
    ##          Detection Rate : 0.03659         
    ##    Detection Prevalence : 0.07195         
    ##       Balanced Accuracy : 0.91010         
    ##                                           
    ##        'Positive' Class : 1               
    ## 

**Smote-ing the data improves sensitivity to the the prediction of volunteers. SMOTE-ing also lowers slightly the ability to predict non-volunteers, but it was only so high prior to SMOTE-ing because there were so few volunteers, it would almost always be correct in predicting that it wasn't a volunteer!**

#### Now we address the potential for overfitting the sample.

Overfitting = building a model on training data and it picks up not only relationships between the outcome and predictors, but also noise specific to the training set. Testing on a separate test set can help prevent this, but where the dataset is not very large, this can be a challenging balance - to get better training, you take away from the test set, and to get a more representative test set, you take away from the training. Although our dataset is technically large, our rare-events problem (with very few volunteers) means it's actually a 'small' dataset.

To get around this, we use k-fold cross validation to get a sense of the out-of-sample accuracy. We create several testing/training sets by splitting the data into k equally sized subsets, and treat a single subsample as the testing set and the remaining data as the training set. We run and test models on all k datasets and average the estimates. After, we take it further and repeat the k-fold CV a number of times and take the mean of this estimate - this will alow us to get an estimate of the precision of the out-of-sample accuracy by creating a confidence interval.

#### We try the k-fold cross validation to predict the efficiency of the method we're using to classify our data

**We create a function, k.folds, that takes a specified colummn containing classifiers (volunteer$Target, in our case) and creates k=10 folds using indices. For this example we test the random forest model**

``` r
volunteers$Target<-as.factor(volunteers$Target)

k.folds <- function(k) {
    folds <- createFolds(volunteers$Target, k = k, list = TRUE,   returnTrain = TRUE)
    for (i in 1:k) {
        model <- randomForest(Target ~ ., data = volunteers[folds[[i]],], ntree = 500, mtry = 2)
        predictions <- predict(object = model, newdata = volunteers[-folds[[i]],], type = "response")
        accuracies.dt <- c(accuracies.dt, 
                           confusionMatrix(predictions, volunteers[-folds[[i]], ]$Target)$overall[[1]])
    }
    accuracies.dt
}

set.seed(100)
accuracies.dt <- c()
accuracies.dt <- k.folds(100)
accuracies.dt
```

    ##   [1] 1.0000000 1.0000000 0.9512195 1.0000000 0.9756098 1.0000000 1.0000000
    ##   [8] 0.9756098 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [15] 1.0000000 1.0000000 0.9756098 0.9756098 0.9756098 0.9756098 0.9756098
    ##  [22] 1.0000000 1.0000000 1.0000000 0.9756098 1.0000000 0.9512195 0.9512195
    ##  [29] 1.0000000 1.0000000 0.9756098 1.0000000 0.9756098 0.9756098 1.0000000
    ##  [36] 1.0000000 1.0000000 1.0000000 0.9756098 1.0000000 1.0000000 1.0000000
    ##  [43] 0.9756098 0.9756098 1.0000000 0.9756098 1.0000000 0.9756098 1.0000000
    ##  [50] 0.9756098 1.0000000 0.9756098 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [57] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [64] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 0.9756098
    ##  [71] 1.0000000 1.0000000 0.9756098 1.0000000 0.9756098 0.9756098 1.0000000
    ##  [78] 1.0000000 0.9756098 1.0000000 0.9756098 1.0000000 0.9756098 1.0000000
    ##  [85] 0.9756098 0.9756098 0.9756098 0.9756098 0.9756098 0.9756098 1.0000000
    ##  [92] 1.0000000 0.9756098 0.9756098 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [99] 0.9756098 1.0000000

**The mean accuracy is 99%, which is pretty good**

``` r
mean.accuracies<-mean(accuracies.dt)
mean.accuracies
```

    ## [1] 0.9902439

**Prepare and export the predicted volunteers who have not yet actually volunteered**

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     combine

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
predictedvols<-cbind(voltestSplit, predRF_SMOTE)
predictednewvols<-subset(predictedvols, Target == 0 & predRF_SMOTE == 1)
predictednewvols<-predictednewvols %>% select(ID, Target, predRF_SMOTE)
write.csv(predictednewvols,"PredictedVolunteers.csv", row.names = FALSE)
```

------------------------------------------------------------------------

##### **Data Source**: Internal CRM

##### **Date Prepared**: July 2018

##### **Presentation**: APRA 2018 Data Analytics Symmposium - "Smart Data"

##### **Questions and Comments**: <Claudia.Rangel@ubc.ca>, <Mai.Bui@ubc.ca>
