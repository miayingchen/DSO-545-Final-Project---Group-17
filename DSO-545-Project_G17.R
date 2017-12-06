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
## read Shapefile
area<-readShapePoly("Homeless_Count_2017_Results_CD.shp")
area <- read.shapefile("Homeless_Count_2017_Results_CD")
area.point<-fortify(area)
zone[Victim$Lon[2]<zone$Lon_max & Victim$Lon[2]>zone$Lon_min & 
       Victim$Lat[2]<zone$Lat_max & Victim$Lat[2]>zone$Lat_min,]
write.csv(Victim,"Victim.csv")  

## read.
crime<-read.csv("crime_w_CTs20171102103130.csv")

ggplot()+
  geom_polygon(data=area.point,aes(x=long,y=lat,group=id,color="white"))+
  geom_point(data=Victim,aes(x=Lon,y=Lat,fill="blue"))+
  scale_x_continuous(limits = c(-118.8,-118.0))+
  scale_y_continuous(limits = c(33.6,34.4))


Victim%>%leaflet() %>% 
  addTiles() %>%
  addMarkers(~Lon, ~Lat)
# Get Map of LosAngeles
h_map<-get_map("City of Los Angeles", zoom=10,maptype="roadmap")
h_map1<-get_map("Los Angeles County",zoom=11)
h_map<-get_map(c(-118.25,34.05), zoom=10)
gps<-read.csv("gps.csv")


# Get 2017 Geospatial
C2017<-read.csv("Homeless_Count_2017_Results_CD.csv")
colnames(C2017)[1]="FID"
C2017_map=left_join(gps,C2017,by="FID")  ## A merge table containg GPS
C2017_map<-C2017_map%>%mutate(Homeless_Density=totPeople/Shape__Area,
                              Unshelter_ratio=totUnshelt/totPeople,
                              Shelter_ratio=totSheltPe/totPeople)
C<-C2017_map%>%filter(FID %in% c(540,541))

ggplot()+
  geom_polygon(data=C,aes(x=Lon,y=Lat,group=FID,color="black",fill=FID))+
  geom_point(data=Victim[2,],aes(x=Lon,y=Lat))

C2017_map1=C2017_map%>%filter(totPeople!=0)
C2017_map2=C2017_map%>%filter(totPeople<1000)
C2017_map3=C2017_map%>%filter(totPeople>=1000)


# 2017 Geospatial Plot

ggmap(h_map)+
  geom_polygon(data=C2017_map,aes(x=Lon,y=Lat,group=FID,fill=totPeople,alpha=totPeople))+
  scale_fill_gradient(low="white",high="darkred")+
  scale_alpha(range = c(0.7, 1))
  scale_x_continuous(limits =c(-118.6,-118.1), expand = c(0, 0))+
    geom_point(data=SNS,aes(x=Lon,y=Lat,color=source))


# 2017 Map
  ggmap(h_map)+
    stat_density2d(data=HER311,aes(x=Longitude,y=Latitude,fill=..level..,alpha=..level..),geom="polygon")+
    scale_fill_gradient(low="white",high="darkred")+
    scale_alpha(range = c(0.7, 1))
  
  
ggmap(h_map)+
  geom_polygon(data=C2017_map,aes(x=Lon,y=Lat,group=FID,fill=totUnshelt,alpha=totUnshelt))+
  scale_fill_gradient(low="white",high="darkred")+
  geom_point(data=SNS,aes(x=Lon,y=Lat,color=source))

ggmap(h_map)+
  geom_polygon(data=C2017_map,aes(x=Lon,y=Lat,group=FID,fill=totPeople,alpha=1))+
  
  scale_fill_gradient(low="white",high="darkred")+
  geom_point(data=Victim_most,aes(x=Lon,y=Lat,color=Crime.Code.Description))+
  guides(alpha=FALSE,Color=FALSE)

ggplot(data=C2017_map,aes(x=Lon,y=Lat,group=FID,fill=totPeople))+
  geom_polygon(color="black")+
  scale_fill_gradient(low="white",high="darkred")+
  ggtitle("Total Count of Homeless in Los Angeles By FID")+
  geom_point(data=SNS,aes(x=Lon,y=Lat,color=source))

ggplot()+
  geom_polygon(data=C2017_map,aes(x=Lon,y=Lat,group=FID,fill=Unshelter_ratio),color="black")+
  scale_fill_gradient(low="white",high="darkred")+
  ggtitle("Unsheltered Homeless Ratio in Los Angeles By FID")+
  geom_point(data=SNS,aes(x=Lon,y=Lat,color=source))

ggplot(data=C2017_map,aes(x=Lon,y=Lat,group=FID,fill=Unshelter_ratio))+
  geom_polygon(color="black")+
  scale_fill_gradient(low="white",high="darkred")+
  ggtitle("Unsheltered Homeless Ratio in Los Angeles By FID")+
  geom_point(data=SNS,aes(x=Lon,y=Lat))


ggplot(C2017_map,aes(x=Lon,y=Lat,group=FID,fill=Shelter_ratio))+
  geom_polygon(color="black")+
  scale_fill_gradient(low="white",high="darkred")+
  ggtitle("Sheltered Homeless Ratio in Los Angeles By FID")


g2<-ggplot(C2017_map,aes(x=Lon,y=Lat,group=FID,fill=Shape__Area))+
  geom_polygon(color="black")+
  scale_fill_gradient(low="white",high="darkred")+
  ggtitle("Area of Los Angeles By FID")
grid.arrange(g1,g2,ncol=2 )

g3<-ggplot(C2017_map,aes(x=Lon,y=Lat,group=FID,fill=Homeless_Density))+
  geom_polygon(color="black")+
  scale_fill_gradient(low="white",high="darkred")+
  ggtitle("Area of Los Angeles By FID")
grid.arrange(g1,g2,g3,ncol=3 )

## Summary By Commnuity 2017
summary(CT2017_2[,c("totUnsheltPeople","totSheltPeople","totPeople")])
CT2017_1<-read.xlsx("homeless-count-2017-results-by-census-tract.xlsx",3)
CT2017_2<-read.xlsx("homeless-count-2017-results-by-census-tract.xlsx",4)
CT2017_rank_tot<-CT2017_2[order(-CT2017_2$totPeople), ]
CT2017_rank_unshelter<-CT2017_2[order(-CT2017_2$totUnsheltPeople), ]
CT2017_rank_shelter<-CT2017_2[order(-CT2017_2$totSheltPeople), ]
CT2017_top10<-CT2017_2[order(-CT2017_2$totPeople)[1:10], ]
CT2017_top10$Year="2017"
ggplot(data=CT2017_rank_tot[1:10,],aes(x=reorder(Community,totPeople,desc),y=totPeople))+
  geom_bar(stat = "identity")+
  labs(title="Top 10 Homeless Commnunity")
ggplot(data=CT2017_rank_unshelter[1:10,],aes(x=reorder(Community,totUnsheltPeople,desc),y=totUnsheltPeople))+
  geom_bar(stat = "identity")+
  labs(title="Top 10 Unshelter Homeless Commnunity")
ggplot(data=CT2017_rank_shelter[1:10,],aes(x=reorder(Community,totSheltPeople,desc),y=totSheltPeople))+
  geom_bar(stat = "identity")+
  labs(title="Top 10 ShelterHomeless Commnunity")


#2016
CT2016_1<-read.xlsx("HC2016.xlsx",2)
colnames(CT2016_1)[4]="Community"
CT2016<-CT2016_1%>%
  group_by(Community)%>%summarise(totUnsheltPeople=sum(totUnsheltPeople),
         totSheltPeople=sum(totSheltPeople),
         totPeople=sum(totPeople),Year="2016")

CT2016_top10<-CT2016%>%filter(Community %in% CT2017_top10[,"Community"]) # according to 2017
#CT2016_top10$Year="2016"
CT2016_rank_tot<-CT2016[order(-CT2016$totPeople), ]
CT2016_rank_unshelter<-CT2016[order(-CT2016$totUnsheltPeople), ]
CT2016_rank_shelter<-CT2016[order(-CT2016$totSheltPeople), ]
ggplot(data=CT2016_rank_tot[1:10,],aes(x=reorder(Community,totPeople,desc),y=totPeople))+
  geom_bar(stat = "identity")+
  labs(title="Top 10 Homeless Commnunity")
ggplot(data=CT2016_rank_unshelter[1:10,],aes(x=reorder(Community,totUnsheltPeople,desc),y=totUnsheltPeople))+
  geom_bar(stat = "identity")+
  labs(title="Top 10 Unshelter Homeless Commnunity")
ggplot(data=CT2016_rank_shelter[1:10,],aes(x=reorder(Community,totSheltPeople,desc),y=totSheltPeople))+
  geom_bar(stat = "identity")+
  labs(title="Top 10 ShelterHomeless Commnunity")


## 2016, 2017 top10 comparison
#CT2016_2017<-left_join(CT2017_top10,CT2016,by=c("Community"="CommunityName"))
CT2016_2017<-rbind(CT2017_top10[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")],
                   CT2016_top10[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")])


ggplot(data=CT2016_2017,aes(x=reorder(Community,-totPeople),y=totPeople,fill=Year))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="Top 10 Homeless Commnunity")


##2015
CT2015_1<-read.xlsx("HC2015_CensusTracts_LACOC.xlsx",1)
#CT2015_1[CT2015_1$Community=="Downtown Los Angeles",]
colnames(CT2015_1)[13]="Community"
CT2015<-CT2015_1%>%group_by(Community)%>%summarise(totUnsheltPeople=sum(Unsheltered),
                                                        totSheltPeople=sum(Sheltered),
                                                        totPeople=sum(X2015_Total),Year="2015")

CT2015_top10<-CT2015%>%filter(Community %in% CT2017_top10[,"Community"])

class(CT2017_top10[,1])
# according to 2017
#CT2016_top10$Year="2016"
CT2015_rank_tot<-CT2015[order(-CT2015$totPeople), ]
CT2015_rank_unshelter<-CT2015[order(-CT2015$totUnsheltPeople), ]
CT2015_rank_shelter<-CT2015[order(-CT2015$totSheltPeople), ]
ggplot(data=CT2015_rank_tot[1:10,],aes(x=reorder(Community,totPeople,desc),y=totPeople))+
  geom_bar(stat = "identity")+
  labs(title="Top 10 Homeless Commnunity")
ggplot(data=CT2015_rank_unshelter[1:10,],aes(x=reorder(Community,totUnsheltPeople,desc),y=totUnsheltPeople))+
  geom_bar(stat = "identity")+
  labs(title="Top 10 Unshelter Homeless Commnunity")
ggplot(data=CT2015_rank_shelter[1:10,],aes(x=reorder(Community,totSheltPeople,desc),y=totSheltPeople))+
  geom_bar(stat = "identity")+
  labs(title="Top 10 ShelterHomeless Commnunity")

CT2015_2016_2017<-rbind(CT2017_top10[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")],
                   CT2016_top10[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")],
                   CT2015_top10[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")])


CT2015_top10<-rbind(CT2015[47,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")],
             CT2015_top10[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")])
# comparison cross three years
ggplot(data=CT2015_2016_2017,aes(x=reorder(Community,-totPeople),y=totPeople,fill=Year))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="Top 10 Homeless Commnunity",x="Community",y="Count")+
  theme(axis.text.x = element_text(size=10,face = "bold"))


sum2015<-CT2015%>%summarise(totUnsheltPeople=sum(totUnsheltPeople),
                            totSheltPeople=sum(totSheltPeople),
                            totPeople=sum(totPeople),
                            Year="2015")

sum2016<-CT2016%>%summarise(totUnsheltPeople=sum(totUnsheltPeople),
                            totSheltPeople=sum(totSheltPeople),
                            totPeople=sum(totPeople),
                            Year="2016")

sum2017<-CT2017_2%>%summarise(totUnsheltPeople=sum(totUnsheltPeople),
                              totSheltPeople=sum(totSheltPeople),
                              totPeople=sum(totPeople),
                              Year="2017")

Compare<-rbind(sum2015,sum2016,sum2017)

ggplot(data=Compare,aes(x=Year,y=totPeople,fill=Year))+
  geom_bar(stat = "identity")+
  labs(title="Homeless Count By Year")


# Victim
Victim<-read.csv("Crime__Homeless_Victim_8_16_-_8_17.csv")
Victim$Crime.Code<-factor(Victim$Crime.Code)
Victim$Date.Occurred<-mdy(Victim$Date.Occurred)
Victim$Date.Reported<-mdy(Victim$Date.Reported)
Victim$Time.Occurred<-sprintf("%04d", Victim$Time.Occurred)
Victim$Time.Occurred<-gsub('^([0-9]{2})([0-9]+)$','\\1:\\2', Victim$Time.Occurred)
#Victim$Time.Occurred<-toString(Victim$Time.Occurred)
Victim<-Victim%>%mutate(timestamp=hm(Time.Occurred))
Victim$Location<-gsub("[()]", "", Victim$Location)
Victim<-Victim%>%separate(col=Location,into=c("Lat","Lon"),sep=",",convert = T)
Victim<-Victim%>%mutate(day=wday(Date.Occurred,label=T,abbr=T),hour=hour(timestamp))
Victim_type<-Victim%>%group_by(Crime.Code,Crime.Code.Description)%>%summarise(Count=n())
Victim_type<-Victim_type%>%arrange(-Count)
#Victim<-Victim%>%ifelse(nchar(Time.Occurred==3),
#separate(col=Time.Occurred,into=c("Hour","Min"),sep=1),
#separate(col=Time.Occurred,into=c("Hour","Min"),sep=2))

## Top 5 victim Type
ggplot(Victim_type[1:5,],aes(x=reorder(Crime.Code.Description,-Count),y=Count,fill=Crime.Code))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Victim_most=Victim%>%filter(Crime.Code %in% c("210","230","440","624","626"))


#dt_victim<-dt_victim%>%mutate(wday=wday(Date.Occurred,label=T),hour=hour(dt_victim$timestamp))

ggmap(h_map1)+
  stat_density2d(data=Victim,
                 aes(x=Lon,y=Lat,fill=..level..), 
                 geom="polygon") +
  scale_fill_gradient(low="white",high = "darkred")+
  facet_wrap(~day,nrow=2)  

ggmap(h_map1)+
  geom_density2d(data=Victim,
                 aes(x=Lon,y=Lat,fill=..level..), 
                 geom="polygon") +
  scale_fill_gradient(low="white",high = "darkred")+
  geom_point(data=Victim,              aes(x=Lon,y=Lat)                )



heatmap_data<-dt_victim%>%group_by(day,hour)%>%summarise(Count=n())
heatmap_data1<-Victim%>%group_by(day,hour)%>%summarise(Count=n())

ggplot(heatmap_data1,
       aes(x = day, y = factor(hour), fill = Count))+       # factor(hour) to show every hour
  geom_tile() +
  scale_fill_continuous(low = "white", high = "darkred")+
  xlab(" ")+
  ylab("Count")
c<-"a"
contains("a")

CT2017_11<-CT2017_1%>%group_by(Community_Name)%>%summarise(n=sum(totPeople))
area

## merge Crime and CT2017
crime<-read.csv("crime_w_CTs20171102103130.csv")
CT2017_crime<-left_join(CT2017_1,crime,by=c("tract"="CT10"))
CT2017_311<-left_join(CT2017_1,sh,by=c("tract"="CT10"))
CT2017_crime<-CT2017_crime%>%
  filter(!is.na(GEOID10))
CT2017_crime_comm<-CT2017_crime%>%
  group_by(Community_Name)%>%
  summarise(count_crime=n())

CT2017_311<-CT2017_311%>%
  filter(!is.na(GEOID10))
CT2017_311_comm<-CT2017_311%>%
  group_by(Community_Name)%>%
  summarise(count_311=n())

CT2017_all<-left_join(CT2017_2,CT2017_311_comm,by=c("Community"="Community_Name"))
CT2017_all<-left_join(CT2017_all,CT2017_crime_comm,by=c("Community"="Community_Name"))
CT2017_all[is.na(CT2017_all$count_311),"count_311"]=0
CT2017_all[is.na(CT2017_all$count_crime),"count_crime"]=0
write.csv(CT2017_all,"CT2017_all.csv")
## test app
t<-CT2017_2$totPeople
ggplot(data=CT2017_2[order(-t)[1:10],],
       aes(x=CT2017_2[order(-t)[1:10],"Community"],y=t[order(-t)[1:10]]))+
         geom_bar(stat = "identity")
##Bar Chart of 2017 census 


ggplot(data=CT2017_rank[1:10,],aes(x=reorder(Community,totPeople,desc),y=totPeople))+
  geom_bar(stat = "identity")+
  labs(title="Top 10 Homeless Commnunity")
ggplot(data=CT2017_rank_unshelter[1:10,],aes(x=reorder(Community,totPeople,desc),y=totPeople))+
  geom_bar(stat = "identity")+
  labs(title="Top 10 Unshelter Homeless Commnunity")
ggplot(data=CT2017_rank_shelter[1:10,],aes(x=reorder(Community,totPeople,desc),y=totPeople))+
  geom_bar(stat = "identity")+
  labs(title="Top 10 ShelterHomeless Commnunity")





#SNS
SNS<-read.csv("Homeless_Shelters_and_Services.csv")
colnames(SNS)[1]="Lon"
colnames(SNS)[2]="Lat"
SNS<-SNS[-145,]
#SNS<-read.csv("SNS.csv")
SNS%>%leaflet() %>% 
  addTiles() %>%
  addMarkers(~Lon, ~Lat)
zone<-read.csv("LACity_Zoning.csv")


## 311
fund<-read.csv("Funds_Relating_to_Housing_and_Homelessness.csv")
HER311<-read.csv("311_Homeless_Encampments_Requests.csv")


ggmap(h_map)+
  stat_density2d(data=Victim,
                 aes(x=Lon,y=Lat,fill=tot), 
                 geom="polygon") +
  scale_fill_gradient(low="white",high = "darkred")+
  facet_wrap(~day,nrow=2) 

# 
# C2017G<-gather(C2017,lon_min,lon_max,key = "Lon",value="Lon_V")
# C2017GG<-gather(C2017G,lat_min,lat_max,key = "Lat",value="Lat_V")

ggmap(h_map)+
  stat_density2d(data=C2017GG,
                 aes(x=Lon_V,y=Lat_V,fill=..level..,alpha=..level..), 
                 geom="polygon") +
  scale_fill_gradient(low="white",high="darkred")

ggmap(h_map)+
  geom_point(data=filter(C2017GG,Tract==980028),                 aes(x=Lon_V,y=Lat_V)) 
          
ggplot(data=filter(C2017GG,Tract==980028),aes(x=Lon_V,y=Lat_V,fill=totPeople))+
  geom_polygon(color="black")+
  scale_fill_gradient(low="white",high="darkred")


ggplot(data=HER311,aes(x=Longitude,y=Latitude))+
  geom_point()+
  stat_density2d(data=HER311,aes(x=Longitude,y=Latitude,fill=..level..),geom="polygon")


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
