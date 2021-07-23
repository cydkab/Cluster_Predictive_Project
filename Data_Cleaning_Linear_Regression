chronological_programtime <- read.csv2("******", sep = ",")

cluster <- read.csv2("******", sep = ";")

str(cluster)

library(dplyr)
cluster$program_duration <- as.numeric(difftime(cluster$End_Time, cluster$StartTime, units = "secs"))

cluster$churn_time <- cluster$Stream_Time/cluster$program_duration

cluster<-cluster[order(cluster$Login_Time),]

regression_dataprep<-cluster[,c(7,8,11,15,16,17)]

install.packages("caTools")
library(caTools)
split<-sample.split(regression_dataprep$churn_time, SplitRatio = 0.7)
regression_train<-subset(regression_dataprep, split==TRUE)
regression_test<-subset(regression_dataprep, split==FALSE)

library(fastDummies)
regression_train_withdummy <- dummy_cols(regression_train, 
                                         select_columns = c("Categorizations", "TimeofDay"), 
                                         remove_first_dummy = FALSE)
regression_test_withdummy <- dummy_cols(regression_test, 
                                        select_columns = c("Categorizations", "TimeofDay"), 
                                        remove_first_dummy = FALSE)
                                        
regression_train_dataset<- regression_train_withdummy[,-c(2,3)]
regression_test_dataset<- regression_test_withdummy[,-c(2,3)]
regression_train_dataset_norm<- scale(regression_train_dataset)
regression_test_dataset_norm<- scale(regression_test_dataset)

km_regression_train<- kmeans(regression_train_dataset_norm[,-4],8)
km_regression_test<- kmeans(regression_test_dataset_norm[,-4],8)

regressiom_train_1<- subset(regression_train_dataset_norm, km_regression_train=1)
regressiom_train_2<- subset(regression_train_dataset_norm, km_regression_train=2)
regressiom_train_3<- subset(regression_train_dataset_norm, km_regression_train=3)
regressiom_train_4<- subset(regression_train_dataset_norm, km_regression_train=4)
regressiom_train_5<- subset(regression_train_dataset_norm, km_regression_train=5)
regressiom_train_6<- subset(regression_train_dataset_norm, km_regression_train=6)
regressiom_train_7<- subset(regression_train_dataset_norm, km_regression_train=7)
regressiom_train_8<- subset(regression_train_dataset_norm, km_regression_train=8)

regression_test_1<- subset(regression_test_dataset_norm, km_regression_test=1)
regression_test_2<- subset(regression_test_dataset_norm, km_regression_test=2)
regression_test_3<- subset(regression_test_dataset_norm, km_regression_test=3)
regression_test_4<- subset(regression_test_dataset_norm, km_regression_test=4)
regression_test_5<- subset(regression_test_dataset_norm, km_regression_test=5)
regression_test_6<- subset(regression_test_dataset_norm, km_regression_test=6)
regression_test_7<- subset(regression_test_dataset_norm, km_regression_test=7)
regression_test_8<- subset(regression_test_dataset_norm, km_regression_test=8)

write.csv(regressiom_train_1,"********", row.names = FALSE)
write.csv(regressiom_train_2,"********", row.names = FALSE)
write.csv(regressiom_train_3,"********", row.names = FALSE)
write.csv(regressiom_train_4,"********", row.names = FALSE)
write.csv(regressiom_train_5,"********", row.names = FALSE)
write.csv(regressiom_train_6,"********", row.names = FALSE)
write.csv(regressiom_train_7,"********", row.names = FALSE)
write.csv(regressiom_train_8,"********", row.names = FALSE)

write.csv(regression_test_1,"********", row.names = FALSE)
write.csv(regression_test_2,"********", row.names = FALSE)
write.csv(regression_test_3,"********", row.names = FALSE)
write.csv(regression_test_4,"********", row.names = FALSE)
write.csv(regression_test_5,"********", row.names = FALSE)
write.csv(regression_test_6,"********", row.names = FALSE)
write.csv(regression_test_7,"********", row.names = FALSE)
write.csv(regression_test_8,"********", row.names = FALSE)

