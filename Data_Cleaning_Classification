### Pipeline that allow us to input the data into a predictive model (using random forests and decision tree). 
### In the first iteration we considered important the follow variables : Categorization, the weekday, 
### the period of the day and, the objective variable, if the user saw the program (saw_it).

### We already had a dataset with the information we need that came from the cluster analysis. 
### We retrieved the original sample where the granularity is for the session and not the user/device. 
### We needed to copy the dataset and then extract the program duration (Endtime - StarTime), 
### the percentage of the user visualization for each program (since we have stream time we can do this calculation)
### and had a column where we will have our objective variable, populated with only "Falses" (for now) and it gives 
### us the information if the user saw the program or not.

library(tidyr)
library(dplyr)
library(lubridate)
library(data.table)

dataset_test <- sessoes_subset_join_epg_decomposed_without_biggerdevice
dataset_test$Program_Duration <- as.numeric( difftime(dataset_test$End_Time, dataset_test$StartTime, units = "secs") )
dataset_test$Percentage_Visualization <- (dataset_test$Stream_Time / dataset_test$Program_Duration) * 100
dataset_test$saw_it <- FALSE

### We used a loop to check if the session is a full visualization of the program or not. We considered a full
### visualization if the user saw the final 40%, or more, of the program and if the percentage of visualization is over
### 90% (i.e. a user enters seconds after the beggining and logs out seconds before the end)

for (i in 1:nrow(dataset_test)) {
    if( dataset_test[i,13] >= 40 && ((dataset_test[i, 7] + dataset_test[i, 11]) >= dataset_test[i, 9]) )
    {
      dataset_test[i, 14] = TRUE
      next
    }
    if( dataset_test[i,13] >= 90 )
    {
      dataset_test[i, 14] = TRUE
      next
    }
  }
  
### Once we had the objective variable fully populated, we had to get the other ones.
### First we managed to extract in which period of the day was the show whatched, then extract the weekday the program was on.
### The categorization was already populated soi we just needed to reduce to the variables we needed.

### After that, the sample was sorted chronologically since we need the December and January data to try and predict the behavior for February.
### In this small script we also added the time of the program in a second iteration.

breaks <- c(0,5,9,12,19,22,23)
#labels for the breaks
labels_breaks <- c("Madrugada", "StartUp", "Manha", "Tarde", "PrimeTime", "Noite")
#Create column with TimeofDay
dataset_test$TimeofDay <- cut(hour(dataset_test$StartTime), breaks = breaks, labels = labels_breaks, include.lowest = TRUE)
dataset_test$weekday <- wday(dataset_test$End_Time)

dataset_with_programtime <- dataset_test[,c(8, 10, 12, 15, 16, 14)]

dataset_with_programtime_chron <- dataset_with_programtime[order(dataset_test1$StartTime), ]

### Just in case Rapidminer wasn't able to read the sample and divide them (70% , 30%) with the chronologically 
### order two samples were generated from the original one.

### We runned Rapidmineer and yes, the software is able to train and test our model the way we inteded.

### Finally, we just needed to export the data to a csv file to then input in the predictive model.

dataset_with_programtime_chron <- dataset_with_programtime_chron[, 2:6]

write.csv(dataset_test_70percent_1, file = "****") 
write.csv(dataset_test_30percent_1, file = "*****") 
write.csv(dataset_all, file = "****") 
write.csv(dataset_test_debug_chron, file = "*****")
  
