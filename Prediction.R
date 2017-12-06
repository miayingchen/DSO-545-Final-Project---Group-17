setwd("D:/R")
library(plyr)
library(leaflet)
library(shapefiles)
library(ggplot2)
library(dplyr)
library(tidyr)
library(xlsx)
library(geojson)
library(RCurl)
library(rjson)
library(R.utils)
library(stringr)
library(lubridate)
library(ggmap)
library(maps)
library(gridExtra)
library(maptools)

## LM predict homeless 2018
#sh<-read.csv("311_calls_w_CTs20171102103144.csv")
CT2017_all<-read.csv("CT2017_all.csv")  # with crime and shelter
CT2017_2<-read.xlsx("homeless-count-2017-results-by-census-tract.xlsx",4)
CT2017_2$Year=2017
CT2017<-CT2017_2[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")]
CT2016<-read.csv("CT2016.csv")
CT2015<-read.csv("CT2015.csv")
CT_all<-rbind(CT2017[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")],
              CT2016[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")],
              CT2015[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")])
# CT_all, all 3 years
# com$Year<-as.character(com$Year)


model<-CT_all%>%group_by(Community)%>%mutate(Count=n())
#rg<-lm(data=model[model$Community=="Adams",],totPeople~Year)
#summary(rg)

?lm
#install.packages("lme4")
library(lme4)
fits1 <- lmList(totSheltPeople ~ Year | Community, data=model)
fits2 <- lmList(totUnsheltPeople ~ Year | Community, data=model)
com<-model%>%distinct(Community)%>%arrange(Community)
#com=data.frame()
i=1
for (i in 1:220 ){
  
  com[i,"Intercetp_U"]=fits2[[i]][["coefficients"]][["(Intercept)"]]
  com[i,"Slope_U"]=fits2[[i]][["coefficients"]][["Year"]]
  com[i,"Intercetp_S"]=fits1[[i]][["coefficients"]][["(Intercept)"]]
  com[i,"Slope_S"]=fits1[[i]][["coefficients"]][["Year"]]
  
}
com<-com%>%mutate(totUnsheltPeople=Intercetp_U+Slope_U*2018,totSheltPeople=Intercetp_S+Slope_S*2018,Year=2018)
com$totUnsheltPeople<-ifelse(com$totUnsheltPeople<0,0,com$totUnsheltPeople)
com$totSheltPeople<-ifelse(com$totSheltPeople<0,0,com$totSheltPeople)
com<-com%>%mutate(totPeople=totUnsheltPeople+totSheltPeople)

### predidct crime
ggplot(CT2017_all,aes(x=totSheltPeople,y=count_crime))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE, fullrange=FALSE, level=0.95)
ggplot(CT2017_all,aes(x=totUnsheltPeople,y=count_crime))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE, fullrange=FALSE, level=0.95)
model1<-lm(data=CT2017_all,count_crime~totUnsheltPeople+totSheltPeople)
summary(model1)
model1
com<-com%>%mutate(Crime=model1[["coefficients"]][["(Intercept)"]]+model1[["coefficients"]][["totUnsheltPeople"]]*totUnsheltPeople+model1[["coefficients"]][["totSheltPeople"]]*totSheltPeople)
com$Crime<-ifelse(com$Crime<0,0,com$Crime)
com<-com[-170,] # exclude valley
#(com%>%arrange(totPeople))[1:10,]
# for comparison plot

# top10
ggplot(data=(com%>%arrange(-totPeople))[1:10,],aes(x=reorder(Community,totPeople,desc),y=totPeople))+
  geom_bar(stat = "identity",fill="lightblue")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=14,face="bold"),
        plot.title=element_text(size=30,face="bold"),
        axis.title.y = element_text(size=14,face="bold"))+
  labs(title="Predicted 2018 Top 10 Homeless Commnunity",x="Community",y="Number Of Homeless People")

ggplot(data=(com%>%arrange(-Crime))[1:10,],aes(x=reorder(Community,Crime,desc),y=Crime))+
  geom_bar(stat = "identity",fill="lightblue")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=14,face="bold"),
        plot.title=element_text(size=30,face="bold"),
        axis.title.y = element_text(size=14,face="bold"))+
  labs(title="Predicted 2018 Top 10 Crime Commnunity",x="Community",y="Crime Count")

## trend
com$Year<-as.character(com$Year)
com1<-com[,c(1,6,7,8,9)]
#model$Year<-as.numeric(model$Year)
model$Year<-as.character(model$Year)
t<-rbind(model[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")],
         com1[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")])
ggplot(t[t$Community %in% c("Hollywood","South Central"),],aes(x=Community,y=totPeople,fill=Year))+
  geom_bar(stat = "identity",position="dodge")+
  theme(axis.text.x = element_text(size=12,face="bold"), 
        plot.title=element_text(size=30,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        legend.text=element_text(size=12))+
  labs(title="Predict The Trend Of Homeless Count",x="Community",y="Number Of Homeless People")
ggplot(t[t$Community %in% c("Hollywood","South Central") & t$Year %in% c("2017","2016","2015"),],aes(x=Community,y=totPeople,fill=Year))+
  geom_bar(stat = "identity",position="dodge")+
  theme(axis.text.x = element_text(size=12,face="bold"), 
        plot.title=element_text(size=30,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        legend.text=element_text(size=14))+
  labs(title="Predict The Trend of Homeless Count",x="Community",y="Number Of Homeless People")
