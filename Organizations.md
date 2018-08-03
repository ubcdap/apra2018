Grouping and Classifying Organizations
================

**Factors: Number of designations (areas) a donor has given to, Lifetime donations, Number of relationships in the CRM, Is a donor within the last three years, Gives to Universities**
**Target: Gift Level (below major giving or above)**
**Models attempted: kmeans, knn, rpart2, rpart**

#### First we try an unsupervised method with kmeans

**We start by loading and keeping a subset of the variables we'll use in this example. Kmeans works best with continuous variables so we keep only those for this example. Remember to edit the filepath to refer to where you've saved the csv.**

``` r
org<- read.csv("W:/BI and Analytics/Data Analytics/DataSets/R/APRA2018/Organizations.csv", quote = "", row.names = NULL)
keeps<-c("CNT_Designations", "RFM_ORG", "CNT_Relationships", "LifetimeDonation") 
orgkmeans<-org[keeps]
```

**Numeric-ising the numbers, then scaling the numeric data (used for ML algorithms that use distance measurements)**

``` r
numeric<-c("CNT_Designations", "RFM_ORG", "CNT_Relationships", "LifetimeDonation") 
orgkmeans[,numeric]<-sapply(orgkmeans[,numeric], as.numeric)

orgkmeans<-scale(orgkmeans[,numeric])
orgkmeans<-data.frame(orgkmeans)

#checking they're scaled
summary(orgkmeans$CNT_Designations)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -0.5400 -0.5400 -0.5400  0.0000  0.2398  4.1385

``` r
summary(orgkmeans$RFM_ORG)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.0425 -0.7662 -0.3408  0.0000  0.5101  2.6372

``` r
summary(orgkmeans$DonorLast3Yrs)
```

    ## Length  Class   Mode 
    ##      0   NULL   NULL

``` r
summary(orgkmeans$GivesToUniversities)
```

    ## Length  Class   Mode 
    ##      0   NULL   NULL

``` r
summary(orgkmeans$CNT_Relationships)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -0.34018 -0.34018 -0.20248  0.00000 -0.06479 18.38658

``` r
apply(orgkmeans,2,sd)
```

    ##  CNT_Designations           RFM_ORG CNT_Relationships  LifetimeDonation 
    ##                 1                 1                 1                 1

**For k means unsupervised clustering, we need to determine how many clusters we should aim for, using the wss plot**

``` r
wssplot <- function(orgkmeans, nc=20, seed=1234){
               wss <- (nrow(data)-1)*sum(apply(orgkmeans,2,var))
               for (i in 1:nc){
                    set.seed(seed)
                    wss[i] <- sum(kmeans(orgkmeans, centers=i)$withinss)}
                plot(1:nc, wss, type="b", xlab="Number of Clusters",
                     ylab="Within groups sum of squares")}

#determine optimal # of clusters via the elbow
wssplot(orgkmeans)
```

![](Organizations_files/figure-markdown_github/unnamed-chunk-3-1.png)

**The graph seems to indicate we lose gains after 4 clusters (although you can try also 7 clusters due to the elbow between 6-7 and compare.)**

``` r
set.seed(1234)
fitkm<-kmeans(orgkmeans,centers=4, nstart = 25) #generates 25 initial configurations and reports on the best one
library("fpc")
```

    ## Warning: package 'fpc' was built under R version 3.4.4

``` r
library("cluster")
```

    ## Warning: package 'cluster' was built under R version 3.4.4

``` r
clusplot(orgkmeans,fitkm$cluster, color = TRUE, main="Cluster plot for org k means")
```

![](Organizations_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
plotcluster(orgkmeans, fitkm$cluster)
```

![](Organizations_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
fitkm$centers #display cluster centers 
```

    ##   CNT_Designations    RFM_ORG CNT_Relationships LifetimeDonation
    ## 1        1.0316348  1.2654634         0.1356390        0.1633351
    ## 2        3.2169451  2.0377682         5.3992719        0.6491738
    ## 3        1.1061430  2.3536173         0.3024051       11.6021303
    ## 4       -0.4113137 -0.4544441        -0.2012873       -0.1379316

``` r
table(fitkm$cluster) #cout of data points in each cluster
```

    ## 
    ##    1    2    3    4 
    ##  450   44    9 1497

**Looking at the table of fitkm$centers, we can see what figures heavily into each of the clusters (RFM with first cluster, relationships with second, giving over lifetime for third.) Cluster 4 seems to be a catch-all cluster, perhaps including more/other variables would provide some better insight for this very large group.**

#### Now we try the KNN supervised method.

**Since it's supervised we need a "target" variable for the categorization - we use the Lifetime Donation variable to divide the observations into those who have given at the major gift level, versus those who have given below that. Then we scale the numeric variables**

``` r
org<- read.csv("W:/BI and Analytics/Data Analytics/DataSets/R/APRA2018/Organizations.csv", quote = "", row.names = NULL)
orgknn<-org[,c("CNT_Designations", "LifetimeDonation", "RFM_ORG", "CNT_Relationships", "DonorLast3Yrs", "GivesToUniversities")]

orgknn$GiftLevel<-""
orgknn$GiftLevel[orgknn$LifetimeDonation>=25000]<-"MajorDonor"
orgknn$GiftLevel[orgknn$LifetimeDonation>0 & orgknn$LifetimeDonation<25000]<-"BelowMGDonor"

orgknn$LifetimeDonation<-NULL
print(table(orgknn$GiftLevel))
```

    ## 
    ## BelowMGDonor   MajorDonor 
    ##         1274          726

``` r
print(prop.table(table(orgknn$GiftLevel))) ##we see that only 10% (3k) are of gift level MajorDonor, with the rest evenly split between below MG and non-donor. model may have trouble training on major giving 
```

    ## 
    ## BelowMGDonor   MajorDonor 
    ##        0.637        0.363

``` r
numeric<-c("CNT_Designations", "RFM_ORG", "CNT_Relationships", "DonorLast3Yrs", "GivesToUniversities") 
orgknn[,numeric]<-sapply(orgknn[,numeric], as.numeric)
orgknn<-as.data.frame(cbind((scale(orgknn[,numeric])), GiftLevel=orgknn$GiftLevel))
```

**dividing dataframe into train and test**

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 3.4.3

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.4.3

``` r
set.seed(1234)
splitIndex <- createDataPartition(orgknn$GiftLevel, p=0.75,list=FALSE)
trainSplit<- orgknn[splitIndex, ]
testSplit <- orgknn[-splitIndex, ]
```

**Train the KNN model on training data, then test on the test data**

``` r
modelknn<-train(trainSplit[,1:5],trainSplit[,6], method="knn")
predictionsknn<-predict(object=modelknn, testSplit[,1:5])
table(predictionsknn)
```

    ## predictionsknn
    ## BelowMGDonor   MajorDonor 
    ##          345          154

``` r
confusionMatrix(predictionsknn,testSplit$GiftLevel, positive = "MajorDonor")
```

    ## Confusion Matrix and Statistics
    ## 
    ##               Reference
    ## Prediction     BelowMGDonor MajorDonor
    ##   BelowMGDonor          298         47
    ##   MajorDonor             20        134
    ##                                           
    ##                Accuracy : 0.8657          
    ##                  95% CI : (0.8326, 0.8944)
    ##     No Information Rate : 0.6373          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.6999          
    ##  Mcnemar's Test P-Value : 0.001491        
    ##                                           
    ##             Sensitivity : 0.7403          
    ##             Specificity : 0.9371          
    ##          Pos Pred Value : 0.8701          
    ##          Neg Pred Value : 0.8638          
    ##              Prevalence : 0.3627          
    ##          Detection Rate : 0.2685          
    ##    Detection Prevalence : 0.3086          
    ##       Balanced Accuracy : 0.8387          
    ##                                           
    ##        'Positive' Class : MajorDonor      
    ## 

#### Now we try another model - rpart2. This model implements the decision tree model, using the max tree depth paramter maxdepth.

``` r
modelrpart2<-train(trainSplit[,1:5],trainSplit$GiftLevel, method="rpart2")
```

**Make predictions and evaluate**

``` r
predictionsrpart2<-predict(object=modelrpart2, testSplit[,1:5])
table(predictionsrpart2)
```

    ## predictionsrpart2
    ## BelowMGDonor   MajorDonor 
    ##          365          134

``` r
confusionMatrix(predictionsrpart2,testSplit$GiftLevel, positive = "MajorDonor")
```

    ## Confusion Matrix and Statistics
    ## 
    ##               Reference
    ## Prediction     BelowMGDonor MajorDonor
    ##   BelowMGDonor          302         63
    ##   MajorDonor             16        118
    ##                                           
    ##                Accuracy : 0.8417          
    ##                  95% CI : (0.8066, 0.8726)
    ##     No Information Rate : 0.6373          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.6373          
    ##  Mcnemar's Test P-Value : 2.274e-07       
    ##                                           
    ##             Sensitivity : 0.6519          
    ##             Specificity : 0.9497          
    ##          Pos Pred Value : 0.8806          
    ##          Neg Pred Value : 0.8274          
    ##              Prevalence : 0.3627          
    ##          Detection Rate : 0.2365          
    ##    Detection Prevalence : 0.2685          
    ##       Balanced Accuracy : 0.8008          
    ##                                           
    ##        'Positive' Class : MajorDonor      
    ## 

#### Let's try another model - rpart. This differs from rpart2 because it uses the complexity parameter cp instead. CP=the amount by which splitting that node improved the relative error. it controls the size of the decision tree and to select the optimal tree size. If the cost of adding another variable to the decision tree from the current node is above the value of CP, then the tree building does not continue.

``` r
modelrpart<-train(trainSplit[,1:5],trainSplit$GiftLevel, method="rpart")
```

**Make predictions and evaluate**

``` r
predictionsrpart<-predict(object=modelrpart, testSplit[,1:5])
table(predictionsrpart)
```

    ## predictionsrpart
    ## BelowMGDonor   MajorDonor 
    ##          341          158

``` r
confusionMatrix(predictionsrpart,testSplit$GiftLevel, positive = "MajorDonor")
```

    ## Confusion Matrix and Statistics
    ## 
    ##               Reference
    ## Prediction     BelowMGDonor MajorDonor
    ##   BelowMGDonor          292         49
    ##   MajorDonor             26        132
    ##                                           
    ##                Accuracy : 0.8497          
    ##                  95% CI : (0.8153, 0.8799)
    ##     No Information Rate : 0.6373          
    ##     P-Value [Acc > NIR] : < 2e-16         
    ##                                           
    ##                   Kappa : 0.6657          
    ##  Mcnemar's Test P-Value : 0.01107         
    ##                                           
    ##             Sensitivity : 0.7293          
    ##             Specificity : 0.9182          
    ##          Pos Pred Value : 0.8354          
    ##          Neg Pred Value : 0.8563          
    ##              Prevalence : 0.3627          
    ##          Detection Rate : 0.2645          
    ##    Detection Prevalence : 0.3166          
    ##       Balanced Accuracy : 0.8238          
    ##                                           
    ##        'Positive' Class : MajorDonor      
    ## 

##### **Data Source**: Internal CRM, iWave

##### **Date Prepared**: July 2018

##### **Presentation**: APRA 2018 Data Analytics Symmposium - "Smart Data"

##### **Questions and Comments**: <Claudia.Rangel@ubc.ca>, <Mai.Bui@ubc.ca>
