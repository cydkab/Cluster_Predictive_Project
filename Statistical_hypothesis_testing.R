library(dplyr)
#t.teste for totaltime between weekday and weekend

a<-cluster_total%>%select(X.TotalTime.)%>%group_by(cluster_total$X.Weekend.)%>%summarise(media=mean(X.TotalTime.))
t.test(a$media,0,conf.level = 0.95)

t.test(cluster_total$X.TotalTime.[cluster_total$X.Weekend.=="1"],cluster_total$X.TotalTime.[cluster_total$X.Weekend.=="0"],mu=0)

### p.value>>alpha (0.05) so we cannot reject H0, which means there is no statistical difference.

a<-cluster_total%>%select(X.Stream_Time.)%>%group_by(cluster_total$X.Categorizations.)%>%summarise(media=mean(X.Stream_Time.))

#test ANOVA for Streamtime Categories
anova(lm(cluster_total$X.Stream_Time.~cluster_total$X.Categorizations.))
### We can reject H0 which means that there is a difference between at least one pair of of mean times of Visualization by Categorie.

summary(cluster_total)

anova(lm(cluster_total$X.TotalTime.~cluster_total$X.weekday_program.))
### We reject H0

b<-cluster_total%>%select(X.Stream_Time.)%>%group_by(cluster_total$X.weekday_program.)%>%summarise(media=mean(X.Stream_Time.))
