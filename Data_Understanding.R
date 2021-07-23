### Access to DB
drv<- dbDriver("MariaDB")
con<- dbConnect(drv, 
                host =NULL, 
                user ="*******",
                password ="*******",
                dbname ="********")
epg<- dbReadTable(con, "epg_v2")
sessoeslive <- dbReadTable(con, "sessoeslive_canais")
dbDisconnect(con)

### Simple Random Sample
sample_sessoes<- sample_n(sessoeslive, 30000, replace=F)
sample_10<- sample_n(sessoeslive, 10000, replace=F)
sample_Cluster <-sample_n(sample_Rapid2, 50, replace=F)
write.csv(sample_Cluster, )

### Frequencies
summary(sample_sessoes)
sd_sessoes<-sd(sample_sessoes$TuneDuration)
var_sessoes<- var(sample_sessoes$TuneDuration)
mean_sessoes<- mean(sample_sessoes$TuneDuration)
max_sessoes_TD<- max(sample_sessoes$TuneDuration)
summary(epg)
table(sample_sessoes$CallLetter)

### String to factor
class(sample_sessoes$CallLetter)
sample_sessoes$CallLetter<- as.factor(sample_sessoes$CallLetter)
epg$CallLetter<-as.factor(epg$CallLetter)
epg$Categories<-as.factor(epg$Categories)
epg$Categorizations<-as.factor(epg$Categorizations)
epg$Genres<-as.factor(epg$Genres)
epg$IsAdultContent<-as.factor(epg$IsAdultContent)
epg$ProgramId<-as.factor(epg$ProgramId)
epg$SeriesId<-as.factor(epg$SeriesId)

install.packages("psych")

### Descriptive analysis
StatDesc_sessoes<- round(describe(sample_sessoes$TuneDuration, IQR=TRUE))

### Boxplot of the sample
boxplot(sample_sessoes$TuneDuration)
boxplot(sample_sessoes$TuneDuration~sample_sessoes$CallLetter, las=2)

### Plot
plot(sample_sessoes$TuneDuration)

### Visualizations by channel
visual_canais <- sample_sessoes %>% 
  group_by(CallLetter)%>%
  summarize(total_canais=n(), media=round(mean(TuneDuration)), mediana=round(median(TuneDuration)), total_durat=sum(TuneDuration))%>%
  arrange(desc(total_canais))%>%
  ungroup
visual_canais

visual_canais_top20<- visual_canais[1:20, ]
visual_canais_top20<- arrange(visual_canais_top20, total_durat)

### Visualizations by Device
acessos_device2 <- sessoes_join_epg_decomposed %>% 
  group_by(Device)%>%
  summarize(total_acessos=n(), media=round(mean(Stream_Time)), mediana=round(median(Stream_Time)), total_durat=sum(Stream_Time))%>%
  arrange(desc(total_durat))%>%
  ungroup
acessos_device2

### Highest frequence on a weekday
weekday_rank <- sample_sessoes %>% 
  group_by(WeekDay)%>%
  summarize(total=n(), media=round(mean(TuneDuration)), mediana=round(median(TuneDuration)), total_durat=sum(TuneDuration))%>%
  arrange(desc(total_durat))%>%
  ungroup
acessos_device

