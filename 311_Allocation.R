setwd("D:/R")
library(htmltools)
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
library(data.table)
library(geosphere)

## Load 311
#top10<-read.csv("Top10.csv")
HER311<-read.csv("311_Homeless_Encampments_Requests.csv")
HER311<-HER311%>%filter(!is.na(Longitude))
#Load SNS
SNS<-read.csv("Homeless_Shelters_and_Services.csv")
colnames(SNS)[1]="Lon"
colnames(SNS)[2]="Lat"
SNS<-SNS[-145,]
#SNS<-read.csv("SNS.csv")
# top10%>%leaflet() %>% 
#   addTiles() %>%
#   addMarkers(~long, ~lat)

# merge
assign_311<-merge(HER311,SNS)
assign_dis<-assign_311[,c(1,2,22,23,36,34,35)]

## compute distance in miles
rad <- pi/180
assign_dis$Lat1 <- assign_dis$Latitude * rad
assign_dis$Lon1 <- assign_dis$Longitude * rad
assign_dis$Lat2 <- assign_dis$Lat* rad
assign_dis$Lon2 <- assign_dis$Lon * rad
assign_dis$dlon <- assign_dis$Lon1 -assign_dis$Lon2
assign_dis$dlat <- assign_dis$Lat1 -assign_dis$Lat2
assign_dis$a <- (sin(assign_dis$dlat/2))^2 + cos(assign_dis$Lat1) * cos(assign_dis$Lat2) * (sin(assign_dis$dlon/2))^2
assign_dis$c <- 2 * atan2(sqrt(assign_dis$a), sqrt(1 - assign_dis$a))
R <- 6378.145
assign_dis$distance <- R * assign_dis$c
assign_dis$distance=assign_dis$distance/1.609344
assign_dis<-assign_dis[,c(1,2,5,6,7,16)]
#write.csv(assign_dis,"assign_dis")
#read.csv("assign_dis.csv")

# order by date,SR#
assign_dis1<-assign_dis%>%arrange(CreatedDate,SRNumber)
assign_dis<-assign_dis1

# compare
# assign_dis_old<-assign_311[,c(1,2,22,23,36,34,35)]
# assign_dis_old<- assign_dis_old%>%mutate(distance=(Longitude-Lon)^2+(Latitude-Lat)^2)
# 
# summary(HER311$Longitude)
# write.csv(assign_o,"assign_o")

# assign

#assign_dis<-assign_dis1
assign_dis$count=0
ass=data.frame()
ss=data.frame()
result_s=data.frame()
temp=data.frame()
for (i in 1:nrow(HER311))
  {
  a=(i-1)*181+1
  b=181*i
  ass<-assign_dis[a:b,]%>%
    filter(!is.na(distance))%>%
    group_by(SRNumber) %>% 
  mutate(Score_dis = findInterval(-distance, 
                                  quantile(-distance, probs=0:99/99)),
         Score_cap =findInterval(-count, 
                                  quantile(-count, probs=0:99/99)))
  ass<-ass%>%mutate(Score_avg=(Score_dis*0.8+Score_cap*0.2))
  if (i==1){
    temp<-ass %>% 
      group_by(SRNumber) %>% 
      slice(which.max(Score_avg))
    result_s<-temp
  }else{
    temp<-ass %>% 
    group_by(SRNumber) %>% 
    slice(which.max(Score_avg))
    result_s=rbind(temp,result_s)
  }
  ss<-result_s%>%group_by(OBJECTID)%>%summarise(count=n())
  assign_dis[assign_dis$OBJECTID==temp$OBJECTID,"count"]=ss[ss$OBJECTID==temp$OBJECTID,"count"]+100
}
i
# assign_o<-assign_dis_s[,c(1,4,5,6,15,18,19)]  
# ss$cap<-ss$Count/1500

# assign_dis1
# result_old<-assign_dis_old %>%
#   group_by(SRNumber) %>%
#   slice(which.min(distance))

## by distance
result<-assign_dis1 %>%
  group_by(SRNumber) %>%
  slice(which.min(distance))


# result_s<-ass %>% 
#   group_by(SRNumber) %>% 
#   slice(which.max(Score_avg))
# write.csv(result_s,"result_s.csv")
#result_s<-read.csv("result_s.csv")

# HER<-left_join(HER311,result,by="SRNumber")
# t<-assign_dis%>%distinct(SRNumber)
# t<-assign_dis%>%group_by(SRNumber)%>%summarise(dis=min(distance))
# summary(assign_dis$distance)
# s_old<-result_old%>%group_by(OBJECTID,Lon,Lat)%>%summarise(Count=n())
s<-result%>%group_by(OBJECTID,Lon,Lat)%>%summarise(Count=n())    ## distance model
ss<-result_s%>%group_by(OBJECTID,Lon,Lat)%>%summarise(Count=n())  ## score model
## boxplot
ggplot()+
  geom_boxplot(data=ss,aes(x="Score Model",y=Count))+
  geom_boxplot(data=s,aes(x="Distance Model",y=Count))+
  labs(title="Number of Homeless People Assigned To Each Shelter",x="",y="Count")+
  theme(axis.text.x = element_text(size=14,face="bold"),
        title = element_text(size=15))

ggplot()+
  geom_boxplot(data=result_s,aes(x="Score Model",y=distance))+
  geom_boxplot(data=result,aes(x="Distance Model",y=distance))+
  labs(title="Distance between 311 requests and assigned shelters",x="",y="Distance (Miles)")+
  theme(axis.text.x = element_text(size=14,face="bold"),
        title = element_text(size=15))

summary(result$distance)
summary(result_s$distance)
sd(result$distance)
sd(result_s$distance)
sd(s$Count)
sd(ss$count)
# 
#ss<-result_s%>%group_by(OBJECTID)%>%summarise(Lon=mean(Lon.x),Lat=mean(Lat.x),Count=n())
# ss$cap<-ss$Count/1500

#ss1<-result_s%>%group_by(OBJECTID,Lon,Lat)%>%summarise(Count=n())
# score
(ss%>%arrange(desc(Count)))%>%
    leaflet() %>% 
  addTiles() %>%
  addMarkers(~Lon, ~Lat,
             popup = ~as.character(paste("311 Request Count=",Count)), 
             label = ~as.character(OBJECTID),
             labelOptions = labelOptions(noHide = T),
             clusterOptions = markerClusterOptions(),
             options = popupOptions(closeButton = FALSE))

# pops

(ss%>%arrange(desc(Count)))%>%
  leaflet() %>% 
  addTiles() %>%
  addMarkers(~Lon, ~Lat,
             label = ~as.character(OBJECTID),
             labelOptions = labelOptions(noHide = T),
             clusterOptions = markerClusterOptions(),
             options = popupOptions(closeButton = FALSE))%>%
  addPopups(~Lon,~Lat,popup = ~as.character(paste("311 Request Count=",Count)),
            popupOptions( textsize = "15px"))




# distance
(s%>%arrange(desc(Count)))%>%
  leaflet() %>% 
  addTiles() %>%
  addMarkers(~Lon, ~Lat,
             popup = ~as.character(paste("311 Request Count=",Count)),
             
             label = ~as.character(OBJECTID),
             labelOptions = labelOptions(noHide = T),
             clusterOptions = markerClusterOptions(),
             options = popupOptions(closeButton = FALSE,textsize = "25px"))

(s%>%arrange(desc(Count)))%>%
  leaflet() %>% 
  addTiles() %>%
  addMarkers(~Lon, ~Lat,
             label = ~as.character(OBJECTID),
             labelOptions = labelOptions(noHide = T),
             clusterOptions = markerClusterOptions())%>%
  addPopups(~Lon,~Lat,popup = ~as.character(paste("311 Request Count=",Count)))
          

## SNS & 311 distribution
  
SNS%>%leaflet() %>% 
  addTiles() %>%
  addMarkers(~Lon, ~Lat,
             label = ~as.character(OBJECTID),
             labelOptions = labelOptions(noHide = T),
             clusterOptions = markerClusterOptions())%>%
  addCircleMarkers(~HER311$Lon, ~HER311$Lat,
           label = ~as.character(OBJECTID),
           labelOptions = labelOptions(noHide = T),
           clusterOptions = markerClusterOptions())
  ggmap(h_map)+
    stat_density2d(data=HER311,aes(x=Longitude,y=Latitude,fill=..level..,alpha=..level..),geom="polygon")+
    scale_fill_gradient("311 Requests",low="white",high="darkred")+
    guides(alpha=FALSE)+
    scale_alpha(range = c(0.7, 1))+
    geom_point(data=SNS,aes(x=Lon,y=Lat,color=Shelter))+
    scale_color_manual("",values = "green")+
    ggtitle("311 Requests Heatmap and Shelters Distribution")
  
  SNS$Shelter="Shelter"
    
  # write.csv(ass,"ass_final.csv")
  # write.csv(result_s,"result_s_final.csv")
  # write.csv(result,"result_old.csv")
  # write.csv(s,"s_old.csv")
#ass<-  read.csv("ass_final.csv")
# result_s<-read.csv("result_s_final.csv")
# ss<-result_s%>%group_by(OBJECTID,Lon,Lat)%>%summarise(Count=n())

#ss1<-result_s%>%group_by(OBJECTID,Lon,Lat)%>%summarise(Count=n())
# score
# (ss%>%arrange(desc(Count)))%>%
#   leaflet() %>% 
#   addTiles() %>%
#   addMarkers(~Lon, ~Lat,
#              popup = ~as.character(paste("311 Request Count=",Count)), 
#              label = ~as.character(OBJECTID),
#              labelOptions = labelOptions(noHide = T),
#              clusterOptions = markerClusterOptions(),
#              options = popupOptions(closeButton = FALSE))
