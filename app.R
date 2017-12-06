setwd("D:/R/")
library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(maps)
library(xlsx)
library(stringr)
library(lubridate)
library(ggmap)
library(gridExtra)
library(leaflet)
library(shinythemes)
options(show.error.messages = FALSE)
h_map<-get_map("City of Los Angeles", zoom=10,maptype="roadmap")
gps<-read.csv("gps.csv")
C2017<-read.csv("Homeless_Count_2017_Results_CD.csv")
colnames(C2017)[1]="FID"
C2017_map=left_join(gps,C2017,by="FID")  ## A merge table containg GPS
C2017_map<-C2017_map%>%mutate(Homeless_Density=totPeople/Shape__Area,
                              Unshelter_ratio=totUnshelt/totPeople,
                              Shelter_ratio=totSheltPe/totPeople)
CT2017_2<-read.xlsx("homeless-count-2017-results-by-census-tract.xlsx",4)
CT2017_2$Year="2017"
#CT2017_2<-CT2017_2[-170,]
CT2016<-read.csv("CT2016.csv")
CT2015<-read.csv("CT2015.csv")
CT2017_2<-CT2017_2[-170,]
CT2015<-CT2015[-171,]
CT2016<-CT2016[-169,]
CT2017_top10<-CT2017_2[order(-CT2017_2$totPeople)[1:10], ]
CT2016_top10<-CT2016%>%filter(Community %in% CT2017_top10[,"Community"])
CT2015_top10<-CT2015%>%filter(Community %in% CT2017_top10[,"Community"])
CT2015_2016_2017<-rbind(CT2017_top10[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")],
                        CT2016_top10[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")],
                        CT2015_top10[,c("Community","totUnsheltPeople","totSheltPeople","totPeople","Year")])

CT2017_all<-read.csv("CT2017_all.csv")
## victim
Victim<-read.csv("Crime__Homeless_Victim_8_16_-_8_17.csv")
Victim$Crime.Code<-factor(Victim$Crime.Code)
Victim$Date.Occurred<-mdy(Victim$Date.Occurred)
Victim$Date.Reported<-mdy(Victim$Date.Reported)
Victim$Time.Occurred<-sprintf("%04d", Victim$Time.Occurred)
Victim$Time.Occurred<-gsub('^([0-9]{2})([0-9]+)$','\\1:\\2', Victim$Time.Occurred)
Victim<-Victim%>%mutate(timestamp=hm(Time.Occurred))
Victim$Location<-gsub("[()]", "", Victim$Location)
Victim<-Victim%>%separate(col=Location,into=c("Lat","Lon"),sep=",",convert = T)
Victim<-Victim%>%mutate(day=wday(Date.Occurred,label=T,abbr=T),hour=hour(timestamp))
Victim<-Victim%>%filter(Lat!=0,Lon!=0)
Victim = Victim %>%
  mutate(Seriousness = ifelse(Crime.Code.1 >= 100 & Crime.Code.1 <= 399, "Serious", 
                              ifelse(Crime.Code.1 >=400 & Crime.Code.1 <= 699, "Moderate", "Least Serious")))
Victim_type<-Victim%>%group_by(Crime.Code,Crime.Code.Description)%>%summarise(Count=n())
Victim_type<-Victim_type%>%arrange(-Count)
Victim_most=Victim%>%filter(Weapon.Used.Code %in% c(100:199))
heatmap_data1<-Victim%>%group_by(day,hour)%>%summarise(Count=n())
##add
Victim_CrimeCode= Victim %>%
  group_by(Crime.Code, Crime.Code.Description, day) %>%
  summarise(count = n()) %>%
  arrange(-count)

Victim_CrimeCode_top35 = Victim_CrimeCode[1:35, ]
##
############## 2017_CRIME_311 COMBINED DATASET ###
merge_2017 = read.csv("CT2017_all.csv")

data1 = merge_2017 %>%
  select(Community, totPeople, count_crime) %>%
  arrange(-totPeople)

top_10_totalHomeless = data1[c(1,3:10,13), ]

### Crime ratio of the Top 10 Community in the City of Los Angeles ###



##
#SNS
SNS<-read.csv("Homeless_Shelters_and_Services.csv")
colnames(SNS)[1]="Lon"
colnames(SNS)[2]="Lat"
SNS<-SNS[-145,]
#311
HER311<-read.csv("311_Homeless_Encampments_Requests.csv")
#UI

ui <-fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  titlePanel("City of Los Angeles Homeless Project"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "var1",
                    label = "Year",
                    choices = list("2017","2016","2015"),
                    selected = "2017"),
      
      selectInput(inputId = "var2", label = "Theme",
                                   choices = list("Homeless","Shelter Services","Crime","311 Request"),
                                   selected ="Homeless" ),
      selectInput(inputId = "var3", label = "Type",
                  choices = list("Summary","Distribution_Map","Distribution_Google"),
                  selected ="Summary"),
      selectInput(inputId = "var4",
                                   label = "Statistics",
                                   choices = list("Total","Unshelter","Shelter"),
                  selected ="Total")
      ),
      
    mainPanel(
       
        #plotOutput(outputId = "plot")
        tabsetPanel(
          tabPanel("Plot", plotOutput("plot")),
          tabPanel("Leafmap", leafletOutput("plot1")))
        )
      
    
  
 
  ))

server<-function(input, output,session) {
 
   # output$selected_var=renderText(
   #   paste("You have selected",input$var1,input$var2,input$var3)
   #  )
   observe({
     choices=switch  (
       input$var1,
       "2017" =list("Homeless","Shelter Services","Crime","311 Request"),
       "2016" =list("Homeless"),
       "2015" =list("Homeless"))
     updateSelectInput(session, "var2", choices=choices,selected = choices[[1]])
   })
  observe({
    temp<-paste(input$var1,input$var2)
   
    choices2=switch  (
      temp,
      "2017 Homeless" =list("Summary","Distribution_Map","Distribution_Google"),
      "2017 Shelter Services"   =list("Distribution"),
      "2017 Crime"   =list("Summary","Distribution"),
      "2017 311 Request"   =list("Distribution"),
      "2016 Homeless"   =list("Summary"),
      "2015 Homeless"  =list("Summary"))
      updateSelectInput(session, "var3", choices=choices2,selected = choices2[[1]])
 
       })
  observe({
    temp2<-paste(input$var1,input$var2,input$var3)
    
    choices3=switch  (
      temp2,
      "2017 Homeless Summary" =list("Total","Unshelter","Shelter","Cross_Compare"),
      "2017 Homeless Distribution_Map"   =list("Total","Unshelter","Shelter"),
      "2017 Homeless Distribution_Google"   =list("Total","Unshelter","Shelter"),
      "2017 Crime Summary"    =list("Top5_All","Top5_Day","Top10_All","Crime_Ratio"),
      "2017 Crime Distribution"  =list("By_Hour_Day","By_Hour_All","By_Severity",
                               "By_Location","By_Age"),
      "2017 Shelter Services Distribution"   =list("Distribution"),
      "2017 311 Request Distribution"   =list("Distribution"),
      "2016 Homeless Summary"   =list("Total","Unshelter","Shelter"),
      "2015 Homeless Summary"  =list("Total","Unshelter","Shelter"))
    updateSelectInput(session, "var4", choices=choices3,selected = choices3[[1]])
    
  })
  output$plot1= renderLeaflet({
    switch(input$var2,
          "Shelter Services"=SNS%>%leaflet()%>% 
      addTiles() %>%
      addMarkers(~Lon, ~Lat,
                 label = ~as.character(OBJECTID),
                 labelOptions = labelOptions(noHide = T),
                 clusterOptions = markerClusterOptions(),
                 options = popupOptions(closeButton = FALSE)),
      "Crime"=Victim%>%leaflet() %>% 
        addTiles() %>%
        addMarkers(~Lon, ~Lat,
                   label = ~as.character(Crime.Code),
                   labelOptions = labelOptions(noHide = T),
                   clusterOptions = markerClusterOptions(),
                   options = popupOptions(closeButton = FALSE)),
      "311 Request"=HER311%>%leaflet()%>% 
        addTiles() %>%
        addMarkers(~Longitude, ~Latitude,
                   labelOptions = labelOptions(noHide = T),
                   clusterOptions = markerClusterOptions(),
                   options = popupOptions(closeButton = FALSE))
       )
  })
  
  
  output$plot=renderPlot({
    type1=switch(input$var1,
                 "2017"=CT2017_2,
                 "2016"=CT2016,
                 "2015"=CT2015)
    type2=switch(input$var4,
                 "Total"=type1$totPeople,
                 "Unshelter"=type1$totUnsheltPeople,
                 "Shelter"=type1$totSheltPeople
                 )
    type3=switch(input$var4,
                "Total"=C2017_map$totPeople,
                "Unshelter"=C2017_map$Unshelter_ratio,
                "Shelter"=C2017_map$Shelter_ratio
                )
    type4=switch(input$var4,
                 "Top5_All"=5,
                 "Top10_All"=10
    )
    type5=switch(input$var4,
      "Total"="Total",
      "Shelter"="Sheltered",
      "Unshelter"="Unsheltered"
    )
    type6=switch(input$var4,
                 "Total"="U",
                 "Shelter"="U",
                 "Unshelter"="U",
                 "Cross_Compare"="C"
      
    )
    type7=switch(input$var4,
                 "Total"="Total Homeless Count",
                 "Shelter"="Sheltered Homeless Ratio",
                 "Unshelter"="Unsheltered Homeless Ratio"
                
    )
    type8=switch (input$var4,
      "Top5_All" = "All",
      "Top10_All" = "All",
      "Top5_Day" = "Day",
      "Crime_Ratio"="Ratio"
    )
    
  switch(input$var1,
         "2017"=switch(input$var2,
                       "Homeless"=tryCatch(switch (input$var3,
                         "Summary" = switch(type6,
                                            
                           "U"=ggplot((type1%>%arrange(-type2))[1:10,],
                                            aes(x=reorder(Community,type2[order(-type2)[1:10]],desc),y=type2[order(-type2)[1:10]]))+
                           geom_bar(stat = "identity",fill="lightblue")+
                           theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12,face="bold"))+
                           labs(title=paste(input$var1,"Top 10",type5,"Homeless Community"),x="Community",y="Count"),
                           "C"=ggplot(data=CT2015_2016_2017,aes(x=reorder(Community,-totPeople),y=totPeople,fill=Year))+
                             geom_bar(stat = "identity",position = "dodge")+
                             labs(title="Top 10 Homeless Commnunity_2015-2017",x="Community",y="Count")+
                             theme(axis.text.x = element_text(angle=45,hjust=1,size=12,face = "bold"))),
                         "Distribution_Map"=ggplot(data=C2017_map,aes(x=Lon,y=Lat,group=FID,fill=type3))+
                           geom_polygon(color="black")+
                           scale_fill_gradient(input$var4,low="white",high="darkred")+
                           labs(title = paste(type7,"in Los Angeles By FID")),
                         "Distribution_Google"=switch(input$var4,
                                                      "Total"=ggmap(h_map)+
                                                        geom_polygon(data=C2017_map,aes(x=Lon,y=Lat,group=FID,fill=totPeople,alpha=totPeople))+
                                                        scale_fill_gradient(input$var4,low="white",high="darkred")+
                                                        scale_alpha(range = c(0.7, 1))+
                                                        ggtitle(paste("Count of Homeless in Los Angeles By FID On Map"))+
                                                        guides(alpha=FALSE),
                                                      "Shelter"=ggmap(h_map)+
                                                        geom_polygon(data=C2017_map,aes(x=Lon,y=Lat,group=FID,fill=totSheltPe,alpha=totSheltPe))+
                                                        scale_fill_gradient(input$var4,low="white",high="darkred")+
                                                        scale_alpha(range = c(0.7, 1))+
                                                        ggtitle(paste("Count of Sheltered Homeless in Los Angeles By FID On Map"))+
                                                        guides(alpha=FALSE),
                                                      "Unshelter"=ggmap(h_map)+
                                                        geom_polygon(data=C2017_map,aes(x=Lon,y=Lat,group=FID,fill=totUnshelt,alpha=totUnshelt))+
                                                        scale_fill_gradient(input$var4,low="white",high="darkred")+
                                                        scale_alpha(range = c(0.7, 1))+
                                                        ggtitle(paste("Count of Unsheltered Homeless in Los Angeles By FID On Map"))+
                                                        guides(alpha=FALSE))
                           
                       ),error=function(e) NULL),
                       "Shelter Services"=ggmap(h_map)+
                         geom_point(data=SNS,aes(x=Lon,y=Lat,color=source))+
                         ggtitle(paste("Distribution of Shelters"))+
                         guides(alpha=FALSE),
                       "311 Request"= ggmap(h_map)+
                         stat_density2d(data=HER311,aes(x=Longitude,y=Latitude,fill=..level..,alpha=..level..),geom="polygon")+
                         scale_fill_gradient(low="white",high="darkred")+
                         ggtitle("Distribution of 311 Requests")+
                         scale_alpha(range = c(0.7, 1)),
                       "Crime"=tryCatch(switch(input$var3,
                         "Summary"=switch(type8,
                           "All"=ggplot(Victim_type[1:type4,],aes(x=reorder(Crime.Code,-Count),y=Count,fill=Crime.Code.Description))+
                           geom_bar(stat = "identity")+
                           theme(axis.text.x = element_text(angle = 45, hjust = 1,size=14,face="bold"))+
                           labs(title=paste("Top",type4,"Type of Homeless"),x="Victim Type",y="Count"),
                           "Day"=ggplot(Victim_CrimeCode_top35, aes(x = day, y = count, 
                                                                    fill = reorder(Crime.Code.Description,-count))) +
                             geom_bar(stat = "identity", position = "dodge") +
                             labs(x = "Day of Week", y = "Count", title = "Top 5 Crime Types by Day of Week", fill = "Crime Code Description"),
                           "Ratio"=ggplot(top_10_totalHomeless, aes(x = reorder(Community, -totPeople), y = count_crime / totPeople)) +
                             geom_bar(stat = "identity", fill = "lightblue") +
                             labs(x = "Community", y = "Crime Ratio", 
                                  title = "Crime Ratio of the Top 10 Community in the City of Los Angeles") +
                             theme(axis.text.x=element_text(size=12,face="bold"))
                           
                           
                           ),
                         "Distribution"=switch(input$var4,
                           "By_Hour_Day"=ggplot(heatmap_data1,aes(x = day, y = factor(hour), fill = Count))+   
                           geom_tile() +
                           scale_fill_continuous(low = "white", high = "darkred")+
                           xlab(" ")+
                           ylab("Count")+
                           ggtitle("Time Heatmap of Crime"),
                           "By_Location"=ggmap(h_map)+
                             geom_point(data=Victim,aes(x=Lon,y=Lat,color = Crime.Code))+
                             ggtitle("Distribution of Crime"),
                           "By_Age"=Victim %>%
                             filter(Victim.Age != 0, Victim.Age != "NA") %>%
                             ggplot(aes(x = Victim.Age, y = ..density..)) +
                             geom_histogram(fill = "lightblue", color = "white", size = 0.2) +
                             geom_line(stat = "density", adjust = 1) +
                             xlim(10,100) +
                             scale_x_continuous(breaks = seq(10,100,10)) +
                             labs(x = "Age of Victim", y = "Density", title = "Distribution of Victim Age"),
                           "By_Severity"=Victim %>%
                             group_by(hour, day, Seriousness) %>%
                             summarize(count = n()) %>%
                             ggplot(aes(x = day, y = factor(hour), fill = count)) +
                             geom_tile() +
                             facet_wrap(~Seriousness) +
                             scale_fill_continuous(low= "white",high= "darkred") +
                             xlab("Day") +
                             ylab("Hour") +
                             ggtitle("Time Heatmap of Crime by Severity"),
                           "By_Hour_All"=Victim %>%
                             group_by(hour) %>%
                             ggplot(aes(x = hour)) +
                             geom_bar(fill = "lightblue", size = 0.2) +
                             scale_x_continuous(breaks = seq(0,23,1)) +
                             labs(x = "Hour of Day", y = "Count", title = "Number of Crimes Happened in Each Time Period"))
                        ),error=function(e) NULL)
                          
                       
                      ),
         "2016"=switch(input$var2,
                        "Homeless"=switch (input$var3,
                                           "Summary" = ggplot((type1%>%arrange(-type2))[1:10,],
                                                              aes(x=reorder(Community,type2[order(-type2)[1:10]],desc),y=type2[order(-type2)[1:10]]))+
                                             geom_bar(stat = "identity",fill="lightblue")+
                                             theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12,face="bold"))+
                                             labs(title=paste(input$var1,"Top 10",type5,"Homeless Community"),x="Community",y="Count")
         )),
         "2015"=switch(input$var2,
                       "Homeless"=switch (input$var3,
                                          "Summary" = ggplot((type1%>%arrange(-type2))[1:10,],
                                                             aes(x=reorder(Community,type2[order(-type2)[1:10]],desc),y=type2[order(-type2)[1:10]]))+
                                            geom_bar(stat = "identity",fill="lightblue")+
                                            theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12,face="bold"))+
                                            labs(title=paste(input$var1,"Top 10",type5,"Homeless Community"),x="Community",y="Count")
                       ))
         )
    
  },height = 600, width = 600)
  
 
}
shinyApp(ui, server)