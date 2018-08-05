Fuzzy matching external names with internal names
================

We had external data on organizations that we wanted to add to our dataset, which required matching on the name of the organizations. Since there are multiple ways to spell an org name - IBM, International Business Machines Corporation, IBM Corp, etc - we needed to use a fuzzy matching algorithm to ensure we're capturing external matches.

The method we chose was based on the code by adamishere shown at <https://github.com/Adamishere/Fuzzymatching>. The Readme at this link explains the code well, but to sum it up here, the code loops through each row of the external data - taking IBM Corp, for example - and runs it through every corporation name in our internal data and assigns a distance "score" for each corp name. Then it keeps the best match. Then...on to the next corp name in your external data.

With the above method, we would choose the distance method we prefer the algorithm to use: Damerau-Levenshtein, Hamming, Levenshtein, qgram, cosine, jaccard distance, Jaro, Jaro-Winkler, etc. There are pros and cons to each method that we won't get into here.

However, for our purposes, we wanted to be able to look at all of the distance methods. So we modified this method a bit by having it loop through the different distance methods, using code based on this article: <https://www.joyofdata.de/blog/comparison-of-string-distance-algorithms/> .

Here's an example of what it looks like:

**Let's pretend this is our corporation names data from our CRM**

``` r
name <- c("Hog's Head Inn", "IBM", "Staples", "Flourish and Blotts")
ID <- c("A", "B", "C", "D")
orgInternalData <- data.frame(ID, name, stringsAsFactors = FALSE)
```

**Let's pretend this is our external corporation names data that we're trying to find matches for**

``` r
name <- c("The Leaky Cauldron", "Eeylops Owl Emporium", "Flourish n Blotts Inc", "Hogshead Inn")
orgExternalData <- data.frame(name, stringsAsFactors = FALSE)
```

**Note that in the external data, only Flourish n Blotts Inc and Hogshead Inn are actually in our CRM data (spelled differently though). These are the ones a human would match up, but let's see if the algorithm suggests a match as well.**

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(splitstackshape)
```

    ## Warning: package 'splitstackshape' was built under R version 3.4.3

    ## Loading required package: data.table

    ## Warning: package 'data.table' was built under R version 3.4.3

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
library(stringdist)
```

    ## Warning: package 'stringdist' was built under R version 3.4.3

``` r
#Create the dataframe M from which stringdist will draw the relevant info. m = method, q = q-gram size (nonnegative and only applies when method is qgram, jaccard, or cosine), and p = penalty parameter for jarowinkler distance, with 0 < p < 0.25. We can test which parameters work best and program it in.

M <- data.frame(
    m = c("osa", "lv", "dl", "lcs", "qgram", "qgram", "qgram",
        "cosine", "cosine", "cosine", "jaccard", "jaccard", "jaccard",
        "jw", "jw", "jw"), 
    q = c(0,0,0,0,1,2,3,1,2,3,1,2,3,0,0,0), 
    p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.1,0.2)
)

#lowercasing the names in the two name datasets and removing punctuation
orgInternalData <- orgInternalData%>%  mutate(name = tolower(name)) %>% mutate(name = gsub("[[:punct:]]", "", .$name))
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.3

``` r
orgExternalData <- orgExternalData %>%  mutate(name = tolower(name)) %>% mutate(name = gsub("[[:punct:]]", "", .$name))

#Specify a colummn consisting of all the names in InternalData
b <- c(orgInternalData[["name"]])

#loop over the rows in external data and take the dfs created by each iteration and save them in bestmatchlist. Note, using 1:100 will take 10 mins.
bestmatchlist=list() #empty list to fill
for (i in 1:4){
a <- rep(orgExternalData[i,"name"], length(b)) #a contains the external data name of org`i', repeated length(b) times.
R <- round(apply(M, 1, 
    function(m) stringdist(a, b, method=m["m"], q=m["q"], p=m["p"])),3) #apply the stringdist fxn using parameters set in M, row-wise(1), and round to 3 decimals

colnames(R) <- M$m #use strings in column M for R's column names
rownames(R) <- paste(format(paste(a, sep=""), width=14), "=",
    format(paste(b, sep=""), width=17), sep="") #rownames are to be "external dataname`i'=InternalDataname"

df <- cSplit(data.frame(namepairs=rownames(R),R),"namepairs","=") #split rownames up using "=" and transform from rownames to column elements

setnames(df,old=c("namepairs_1","namepairs_2"),new=c("external dataname", "name")) #rename columns

bestmatchvalue<-min(df$cosine,na.rm=TRUE) #define the best match stringdist value as the min of the cosine method/parameter, ignoring NA's (which would otherwise be the minimums all the time)

dfbest<-data.frame(df[(df$cosine ==bestmatchvalue )]) #create df consisting of the bestmatch rows over all external names
dfbest$i<-i #record the iteration number i in each df
bestmatchlist[[i]]<-dfbest #a list containing all of the df's, from i:N
}
```

**Appending**

``` r
bestmatchesappend <- do.call(rbind,bestmatchlist) #append all of the bestmatchlists together to form one dataframe
```

**Looking at the bestmatchesappend we can see that indeed "Flourish n Blotts Inc" and "Hogshead Inn" found matches in our internal data as "Flourish and Blotts" and "Hog's Head Inn". The algorithm did the best it could with the rest (which do not appear in our internal data at all), but we can tell by their relatively higher distance values that they were deemed not as good a match.**

**Merging back all the InternalData data, including ID**

``` r
lookup<- unique(bestmatchesappend)
MergedDF<-(merge(lookup,orgInternalData, by = 'name'))
```

##### **Data Source**: Harry Potter universe

##### **Date Prepared**: Aug 2018

##### **Presentation**: APRA 2018 Data Analytics Symmposium - "Smart Data"

##### **Questions and Comments**: <Claudia.Rangel@ubc.ca>, <Mai.Bui@ubc.ca>
