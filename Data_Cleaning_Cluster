### Having the tables we need to combine them by the CallLetter (channel) and the beginning (StarTime) and the end (EndTime) of a
## program in combination with the LoginTime (TuneTime)
library(data.table)
library(fuzzyjoin)
library(dplyr)
library(tidyr)
library(sqldf)

random_rows <- sample(1:nrow(sessoeslive_unique), 25000)

sessoes_subset <- sessoeslive_unique[random_rows,]

sessoes_subset_join_epg <- sqldf("SELECT
    s.Device,
    s.CallLetter as Channel,
    e.Title as Program_Title,
    e.Programid as Program_Title_ID,
    e.Genres,
    s.TuneDuration as Stream_Time,
    s.TuneTime as Login_Time,
    e.StartDate as StartTime,
    e.EndDate as End_Time,
    e.Categorizations
FROM
    sessoes_subset s
LEFT JOIN
    epg e 
ON s.CallLetter = e.CallLetter
AND s.TuneTime > e.StartDate
AND s.TuneTime <= e.EndDate")

sessoes_subset_join_epg$Login_Time <- as.POSIXct(sessoes_subset_join_epg$Login_Time, origin = "1970-01-01 00:00:00")
sessoes_subset_join_epg$StartTime <- as.POSIXct(sessoes_subset_join_epg$StartTime, origin = "1970-01-01 00:00:00")
sessoes_subset_join_epg$End_Time <- as.POSIXct(sessoes_subset_join_epg$End_Time, origin = "1970-01-01 00:00:00")

## the table has NS's for icomplete visualizations

## eliminate the NA's since we don't have the information needed to analyse

index_with_NAs <- which(is.na(sessoes_subset_join_epg$Program_Title))

if (length(index_with_NAs) != 0) {
sessoes_subset_join_epg <- sessoes_subset_join_epg[-index_with_NAs,] 
}

## Outlier : Stream time over 20k seconds
sessoes_subset_join_epg_decomposed <- sessoes_subset_join_epg
sessoes_subset_join_epg_decomposed$TotalTime <- sessoes_subset_join_epg_decomposed$Stream_Time

## Decompose the total visualization time into the Stream Time
for (i in 1:nrow(sessoes_subset_join_epg_decomposed)) {
  d = 0
  a = 0
  b = sessoes_subset_join_epg_decomposed[i, 7] + sessoes_subset_join_epg[i, 6]  ## Login time + Visualization
  c = sessoes_subset_join_epg_decomposed[i, 8] ## Startime of the program in the original join
  d = sessoes_subset_join_epg_decomposed[i, 9] ## Endtime of the program in the original join
  e = sessoes_subset_join_epg_decomposed[i, 6] ## Visualization time
  f = sessoes_subset_join_epg_decomposed[i, 6]
  channel_programs <- filter(epg, epg$CallLetter == sessoes_subset_join_epg_decomposed[i, 2]) ## Dataset only with the scheadule of one specific channel
  
  if (b > d)
  {
    ## Program time
    sessoes_subset_join_epg_decomposed[i, 6] <- as.numeric(difftime(d, sessoes_subset_join_epg_decomposed[i, 7], units = "secs"))  
    repeat
    {
      e = as.numeric( difftime(b,d, units = "secs")) ## Difference between the Logout Time and the End time of a program
      e = as.integer(e)
      if (e < 0) {
        e = as.numeric( difftime(d,b, units = "secs")) 
      }
      a = which(channel_programs$StartDate == d) ## Index of a specific program
      if(length(a) == 0)
      {
        break
      }
      d = channel_programs[a, 4] ## Replace the program endtime (i) for the end of the next one (i + 1)
      
      sessoes_subset_join_epg_decomposed <- add_row(sessoes_subset_join_epg_decomposed, 
              Device = sessoes_subset_join_epg_decomposed[i, 1], 
              Channel = sessoes_subset_join_epg_decomposed[i, 2], 
              Program_Title = channel_programs[a, 6], 
              Program_Title_ID = channel_programs[a, 1],
              Genres = channel_programs[a, 12],
              Stream_Time = e, 
              Login_Time = sessoes_subset_join_epg_decomposed[i, 7], 
              StartTime = channel_programs[a, 3], 
              End_Time = channel_programs[a, 4],
              Categorizations = channel_programs[a, 11],
              TotalTime = f)
      
      if (b < d)
      {
        break
      }
    }
  }
}

## loop to correct the stream time for each program
for (i in 1:nrow(sessoes_subset_join_epg_decomposed)) {
  if(sessoes_subset_join_epg_decomposed[i, 6] > as.numeric( difftime(sessoes_subset_join_epg_decomposed[i, 9], sessoes_subset_join_epg_decomposed[i, 8], units = "secs")))
  {
    sessoes_subset_join_epg_decomposed[i, 6] <- as.numeric( difftime(sessoes_subset_join_epg_decomposed[i, 9], sessoes_subset_join_epg_decomposed[i, 8], units = "secs"))
  }
  print(i)
}

## data.frame with all the rows
sessoes_subset_cluster_total <- sessoes_subset_join_epg_decomposed

y <- which(sessoes_subset_join_epg_decomposed$Device == (names(sort(table(sessoes_subset_join_epg_decomposed$Device), decreasing = TRUE))[1]))

sessoes_subset_join_epg_decomposed_without_biggerdevice <- sessoes_subset_join_epg_decomposed[-y,]
## export to csv with outliers, without Stream time over 20k, 15k, 10k and 5k

x <- which(sessoes_subset_join_epg_decomposed_without_biggerdevice$Stream_Time > 20000)
x1 <- which(sessoes_subset_join_epg_decomposed_without_biggerdevice$Stream_Time > 15000)
x2 <- which(sessoes_subset_join_epg_decomposed_without_biggerdevice$Stream_Time > 10000)
x3 <- which(sessoes_subset_join_epg_decomposed_without_biggerdevice$Stream_Time > 5000)

sessoes_subset_cluster_without_20kover <- sessoes_subset_join_epg_decomposed_without_biggerdevice[-x,]
sessoes_subset_cluster_without_15kover <- sessoes_subset_join_epg_decomposed_without_biggerdevice[-x1,]
sessoes_subset_cluster_without_10kover <- sessoes_subset_join_epg_decomposed_without_biggerdevice[-x2,]
sessoes_subset_cluster_without_5kover <- sessoes_subset_join_epg_decomposed_without_biggerdevice[-x3,]
sessoes_subset_cluster_total <- sessoes_subset_join_epg_decomposed_without_biggerdevice

write.csv2(sessoes_subset_cluster_total, file = "******")
write.csv2(sessoes_subset_cluster_without_20kover, file = "*****")
write.csv2(sessoes_subset_cluster_without_15kover, file = "*******")
write.csv2(sessoes_subset_cluster_without_10kover, file = "********")
write.csv2(sessoes_subset_cluster_without_5kover, file = "********")


### With a complete data.frame we can now start making the changes we need to retrieve a dataset we consider adequate to a cluster analysis using k-means.
## The variables we will use are : 
# Device / TimeDay (mean of visualizations) / Weekend / Weekday / mean of StreamTime per day / Total time of Stream / Categorizations

### Considering the information we still don't have summed up, first variable to extract is TimeofDay. This will be done to all the dataframes extracted above.

### For this purpose, instead of using the Login Time, the StarTime of the program will be used. This variable is more accurate to give us the TimeofDay since
## the Login hour is constant and independent of the StreamTime. There may be some errors since we are categorizing the program, but is assumed that the StartTime
# will set this variable.

library(lubridate)
library(data.table)
###create breaks 
breaks <- c(0,5,9,12,19,22,23)
#labels for the breaks
labels_breaks <- c("Madrugada", "StartUp", "Manha", "Tarde", "PrimeTime", "Noite")
#Create column with TimeofDay
sessoes_subset_cluster_total$TimeofDay <- cut(hour(sessoes_subset_cluster_total$StartTime), breaks = breaks, labels = labels_breaks, include.lowest = TRUE)

sessoes_subset_cluster_without_20kover$TimeofDay <- cut(hour(sessoes_subset_cluster_without_20kover$StartTime), breaks = breaks, labels = labels_breaks, include.lowest = TRUE)
sessoes_subset_cluster_without_15kover$TimeofDay <- cut(hour(sessoes_subset_cluster_without_15kover$StartTime), breaks = breaks, labels = labels_breaks, include.lowest = TRUE)
sessoes_subset_cluster_without_10kover$TimeofDay <- cut(hour(sessoes_subset_cluster_without_10kover$StartTime), breaks = breaks, labels = labels_breaks, include.lowest = TRUE)
sessoes_subset_cluster_without_5kover$TimeofDay <- cut(hour(sessoes_subset_cluster_without_5kover$StartTime), breaks = breaks, labels = labels_breaks, include.lowest = TRUE)

### Add the variable "Weekend" to the data.frame as a 1 (Weekend) and 0 (not Weekend).
## Add, from a sequence from 0 to 7, the day of the week for each visualization. 1: Sunday, 2: Monday, ..., 7: Saturday

library(chron)
library(expss)
library(labelled)
library(data.table)

sessoes_subset_cluster_total$Weekend <- as.numeric(is.weekend(sessoes_subset_cluster_total$StartTime))
sessoes_subset_cluster_without_20kover$Weekend <- as.numeric(is.weekend(sessoes_subset_cluster_without_20kover$StartTime))
sessoes_subset_cluster_without_15kover$Weekend <- as.numeric(is.weekend(sessoes_subset_cluster_without_15kover$StartTime))
sessoes_subset_cluster_without_10kover$Weekend <- as.numeric(is.weekend(sessoes_subset_cluster_without_10kover$StartTime))
sessoes_subset_cluster_without_5kover$Weekend <- as.numeric(is.weekend(sessoes_subset_cluster_without_5kover$StartTime))

sessoes_subset_cluster_total$weekday_program <-wday( sessoes_subset_cluster_total$StartTime )
sessoes_subset_cluster_without_20kover$weekday_program <-wday( sessoes_subset_cluster_without_20kover$StartTime )
sessoes_subset_cluster_without_15kover$weekday_program <-wday( sessoes_subset_cluster_without_15kover$StartTime )
sessoes_subset_cluster_without_10kover$weekday_program <-wday( sessoes_subset_cluster_without_10kover$StartTime )
sessoes_subset_cluster_without_5kover$weekday_program <-wday( sessoes_subset_cluster_without_5kover$StartTime )

### Group by device to have the granularity for each device/user. First tackle the mean time of the stream time and then get the most viewed Categorization by the TimeofDay.
sessoes_mean_total_time <-
  sessoes_subset_join_epg %>%
  group_by(Device) %>%
  summarise(mean_time_visualization_total = mean(Stream_Time), TotalTime = sum(Stream_Time))
  
## The most viewed Categorization by TimeofDay
sessoes_cluster_groupby_total_categorizations <- 
  sessoes_subset_cluster_total %>%
  group_by(Device) %>%
  summarise(categorie_most_watched = names(sort(table(Categorizations), decreasing = TRUE))[1])

unpivot_Categorizations_TimeofDay_total <- sessoes_cluster_groupby_total_categorizations   ## These unpivot names came from previous iterations

## mean StreamTime by TimeofDay
means_TimeofDay_total <- 
  sessoes_subset_cluster_total %>% 
  group_by(Device, TimeofDay)%>%
  summarize(MediaPeriodoDia=mean(Stream_Time))%>%
  ungroup

unpivot_means_TimeofDay_total <- spread(means_TimeofDay_total, TimeofDay, MediaPeriodoDia)   ## These unpivot names came from previous iterations

sessoes_cluster_groupby_Weekend_total <- 
  sessoes_subset_cluster_total %>%
  group_by(Device) %>%
  summarise( Weekend = names(sort(table(Weekend), decreasing = TRUE))[1] )

### Join of all the data.frames leaving the scale function only to be ready to export into a csv for a Rapidminer/k-means analysis
join_total <- left_join(sessoes_cluster_groupby_Weekend_total, sessoes_mean_total_time, by="Device")

join_total <- left_join(join_total, unpivot_Categorizations_TimeofDay_total, by="Device")

join_total <- left_join(join_total, unpivot_means_TimeofDay_total, by="Device")

### Create dummies columns for the categorization_per_TimeofDay

library(fastDummies)

colunas <- c("Madrugada.x", "StartUp.x", "Manha.x", "Tarde.x", "PrimeTime.x", "Noite.x")
join_total_cluster <- dummy_cols(join_total, select_columns = c("categorie_most_watched"), remove_first_dummy = FALSE)
join_total_cluster <- join_total_cluster[, -5]
join_total_cluster[is.na(join_total_cluster)] = 0

### join_total_cluster<-scale(join_total_cluster[,c(2:ncol(join_total_cluster))])

write.csv2(join_total_cluster, "*******")

### Determine number of clusters
library(purrr)
library(cluster)
library(factoextra)

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(join_total_cluster, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(join_total_cluster))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k_values <- 2:25

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k_values, avg_sil)

plot(k_values, avg_sil_values,
       type = "b", pch = 19, frame = FALSE, 
       xlab = "Number of clusters K",
       ylab = "Average Silhouettes")

### Evaluate the optimal number of clusters without a visual aproximation but instead with graphical support -> We reached an outcome of 8 clusters to be performed
library(factoextra)
fviz_nbclust(join_total_cluster, kmeans, method = "silhouette")

### The method summarised previously was made to all the samples without the total visualization time 
## over 20k, 15k, 10k, 5k. We reached a conclusion that partitioning the samples the outcomes were similar, 
# so, we decided to keep all the values except the ones that had the device with a high frequence in our dataset.
# That device, using 9 clusters was isolated in one cluster.
