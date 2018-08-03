Predicting Volunteers Using Decision Trees - Supervised Learning - classification and ensembles
================

#### Factors: Age, RFM, Lifetime Raised, Number of degrees, Years since Grad, Gender, Alum, Inclination, Number of Events Attended (not as a volunteer)

#### Target: Volunteer y/n

#### Models attempted: Class, Treebag, Random Forest, SVM

#### In all models, the full dataset was used, EXCEPT for the SVM model (because it took too long to fit the parameters). For SVM, a subset of 10000 was used. For each model, we go through the process of fitting the model and predicting using the data as-is and evaluating the performance, then using SMOTE on the data training set and re-fitting, predicting, and re-evaluating the performance. Finally, we use cross-validation on the best method to make sure it's not overfitting the sample, and then export the predicted volunteers (who are not already volunteers).

**Reading and Normalizing numeric data**

``` r
volunteers<-read.csv("W:/BI and Analytics/Data Analytics/DataSets/R/APRA2018/Volunteers.csv", quote = "", row.names = NULL)

numeric<-c("Age", "RFM", "LifetimeDonation", "Degrees" ,"YearsSinceGrad", "Male" ,"Alum", "InclinationScore", "EventsAttended")
volunteers[,numeric]<-sapply(volunteers[,numeric], as.numeric)
```
