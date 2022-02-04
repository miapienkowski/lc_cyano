#cropping composites to st. albans
#then classifying each pixel as bloom or not based on threshold
#then analyzing images with blooms

#0. load libraries
library(raster)
library(rasterVis)
library(ggpubr)
library(scico)
library(gsubfn)
library(rgdal)
library(ggplot2)
library(sf)

# 1. import composites
(composites <- Sys.glob('C:/Users/miapi/Downloads/champ/cicyano_composites_1014/*MAXIMUM_7day.tif'))
(composites2 <- Sys.glob('C:/Users/miapi/Downloads/lakechamplain_7day_max/lakechamplain_7day_max/*MAXIMUM_7day.tif'))
tempComposites<-stack(composites)
tempComposites2<-stack(composites2)
tempComposites<-stack(tempComposites, tempComposites2)
cropComposites<-stack()


#2. crop to land extent
name<- "C:/Users/miapi/Downloads/All_Segments/All_Segments.shp"
region<-readOGR(dsn=name, layer = "All_Segments") 
region <- spTransform(x = region, CRSobj = '+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs')
region@data
segNames<-c("Missisquoi Bay")
seg.sub <- region[as.character(region@data$SEG_NAME) %in% segNames, ]


for(i in 1:nlayers(tempComposites)){
  tempRaster<-tempComposites[[i]]
  # Crop  data by extent of state subset
  ci.sub <- crop(tempRaster, extent(seg.sub))
  ci.sub <- mask(ci.sub, seg.sub)
  cropComposites<-stack(cropComposites, ci.sub)
}

#3. reclassify
(reclass_df <- c(0,NA, 250, NA, 251, NA, 252, NA, 253, NA, 254, NA, 255, NA))
(reclass_m <- matrix(reclass_df,
                     ncol = 2,
                     byrow = TRUE))
for(i in 1:nlayers(cropComposites)){
  cropComposites[[i]] <- reclassify(cropComposites[[i]], reclass_m)
}

#scaling pixels
adjustedStack <- setNames(calc(cropComposites, fun=function(x){10^((3/250)*x-4.2)}), names(cropComposites))


##########################################################################################
#############################    #building threshold      ################################
##########################################################################################

thresholdSpecial <- .001

fourteenDF <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(fourteenDF) <- c('index','date','label', 'altdate', 'validity', 'classWithThreshold', 'bloomCount')

blooms<-stack()
noblooms<-stack()

bloomStack <- stack()


fourteenDF$altdate<-as.Date(character(0), format="%m/%d/%Y")


for(i in 1:nlayers(adjustedStack)){
  
  #################################
  #classify
  ##################################
  class <- NA
  classWithThreshold <- NA
  valid<-0
  vals<-getValues(adjustedStack[[i]]) #returns vector
  
  if(length(which(as.vector(vals) > thresholdSpecial)) > 0 & length(which(as.vector(vals) < 250)) > 0){
    blooms<-stack(blooms, adjustedStack[[i]])
    class <-1
  }
  else{
    noblooms<-stack(noblooms,adjustedStack[[i]])
    class <- 0
  }
  if(length(as.vector(vals[!(vals %in% c(0,250,251,252,253,254,255,NA))]))>0){
    blooms<-stack(blooms, adjustedStack[[i]])
    valid <-1
  }
  
  #################################
  #date
  ##################################
  filename <- names(adjustedStack[[i]])
  (date<-strapply(filename,"[0-9]{4}.[0-9]{2}[0-9]{2}.[0-9]{2}[0-9]{2}", simplify = TRUE))
  year <- substr(date, 1, 4)
  month <- substr(date, 6,7)
  day <- substr(date, 8,9)
  hfourteen <-substr(date, 11, 12)
  minute <- substr(date, 13, 14)
  
  
  (finalDate<-paste(year,month,day, sep='-'))
  (finalTime<-paste(hfourteen, minute, sep=':'))
  (finalDate2<-paste(month,day,year, sep='/'))
  
  (dateTime<-paste(finalDate, finalTime, sep=" "))
  v<-c(as.POSIXct(dateTime, format="%Y-%m-%d %H:%M"))
  date2<-as.Date(finalDate2, format="%m/%d/%Y")
  
  
  #################################
  #spatial threshold
  ################################
  bloomsWithThreshold<-stack()
  nobloomsWithThreshold<-stack()
  
  
  mat<-adjustedStack[[i]]
  mat[ mat >= .001 ] <- 1
  mat[ mat < .001 ] <- 0
  mat[ is.na(mat)] <- 0
  
  r <- mat    
  rc <- clump(r, directions=8)
  
  bloomChart<-adjustedStack[[i]]
  count<-0
  for(m in 1:length(unique(rc))){
    if(sum(rc[!is.na(rc)] == m) >= 4){
      count = count +1
      bloomChart[rc == 1] <- 1000
      
    }
  }
  bloomStack<-stack(bloomStack, bloomChart)
  vals2<-getValues(bloomStack[[i]]) #returns vector
  if(length(as.vector(vals2[(vals2 %in% c(1000))]))>0){
    bloomsWithThreshold<-stack(bloomsWithThreshold, adjustedStack[[i]])
    classWithThreshold <-1
  }
  else{
    nobloomsWithThreshold<-stack(nobloomsWithThreshold,adjustedStack[[i]])
    classWithThreshold <- 0
  }
  
  #################################
  #build row
  ##################################
  newRow <- data.frame(index = i, date = v, label = class, altdate=date2, 
                       validity =valid, classWithThreshold = classWithThreshold, bloomCount = count )
  fourteenDF <- rbind(fourteenDF, newRow)
  
}


fourteenDF$startDate<-0
newdata22 <- fourteenDF
newdata22$date<- as.Date(newdata22$date)
View(newdata22)

newdata4001<-newdata22[order(as.Date(newdata22$date, format="%d/%m/%Y")),]
View(newdata4001)

for(i in 1:nrow(newdata4001)){
  if (i != 1){
    if(newdata4001$classWithThreshold[i] == 1 & newdata4001$classWithThreshold[i-1] == 0){
      newdata4001$startDate[i] = 1
    }
  }
  else{
    if (newdata4001$classWithThreshold[i] == 1){
      newdata4001$startDate[i] = 1
    }
  }
}

newdata4001$endDate<-0

for(i in 1:nrow(newdata4001)){
  if (i != 1 & i != nrow(newdata4001)){
    if(newdata4001$classWithThreshold[i] == 1 & newdata4001$classWithThreshold[i-1] == 1 & newdata4001$classWithThreshold[i+1] == 0){
      newdata4001$endDate[i] = 1
    }
  }
  else{
    if (newdata4001$classWithThreshold[i] == 1){
      newdata4001$endDate[i] = 1
    }
  }
}
#         GRAPHS
#################################################################
######################################################################
newdata4001$marker<-'not start/end'
newdata4001$binmarker<-0
for(i in 1:nrow(newdata4001)){
  #start date marker - 'start'
  if(newdata4001$startDate[i] == 1){
    newdata4001$marker[i] = 'start date'
    newdata4001$binmarker[i] = 1
  }
  #end date marker - 'end'
  if(newdata4001$endDate[i] == 1){
    newdata4001$marker[i] = 'end date'
    newdata4001$binmarker[i] = 1
  }
}


newdata4001$countMarker <- ifelse(newdata4001$bloomCount == 2, 1, 0)


newdata4001$marker2<-'not bloom'
newdata4001$binmarker2<-0
for(i in 1:nrow(newdata4001)){
  
  if(newdata4001$classWithThreshold[i] ==1){
    #start date marker - 'start'
    if(newdata4001$startDate[i] == 1){
      newdata4001$marker2[i] = 'start date'
      newdata4001$binmarker2[i] = 1
    }
    #end date marker - 'end'
    else if(newdata4001$endDate[i] == 1){
      newdata4001$marker2[i] = 'end date'
      newdata4001$binmarker2[i] = 1
    }
    else{
      newdata4001$marker2[i] = 'mid bloom'
      newdata4001$binmarker2[i] = 1
    }
  }
  else{
    if(newdata4001$validity[i]==0){
      newdata4001$marker2[i] = 'inconclusive'
      newdata4001$binmarker2[i] = 0
    }
  }
}

p4<- ggplot(newdata4001, aes(x=as.Date(date), y=binmarker2, 
                             color=marker2, shape=factor(countMarker))) +
  geom_point() + 
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = seq(0, 1))+
  theme(legend.title=element_blank())+
  scale_color_manual(values = c("end date" = "#f0e442",
                                "start date"="#cc79a7",
                                "mid bloom"="#009e73",
                                "not bloom"="#0072b2",
                                "inconclusive" = "black"))+
  scale_shape_discrete(labels = c("", "2+ blooms")) 




q<-p4+scale_x_date(limit=c(as.Date("2016-07-01"),as.Date("2016-10-31")))+ggtitle("2016")
w<-p4+scale_x_date(limit=c(as.Date("2017-07-01"),as.Date("2017-10-31")))+ggtitle("2017")
e<-p4+scale_x_date(limit=c(as.Date("2018-07-01"),as.Date("2018-10-31")))+ggtitle("2018")
r<-p4+scale_x_date(limit=c(as.Date("2019-07-01"),as.Date("2019-10-31")))+ggtitle("2019")
t<-p4+scale_x_date(limit=c(as.Date("2020-07-01"),as.Date("2020-10-31")))+ggtitle("2020")
y<-p4+scale_x_date(limit=c(as.Date("2021-07-01"),as.Date("2021-10-31")))+ggtitle("2021")

plot<-ggarrange(q,w,e,r,t,y, rows=3, common.legend=TRUE,legend='right') 

annotate_figure(plot, top = text_grob("Bloom Dates - Missisquoi Bay", 
                                      face = "bold", size = 14))
library(plotly)
ggplotly(y)
p4=p4+
  geom_text(aes(color = 'black', label=ifelse(binmarker2==1,format(date, format = "%m/%d"),'')),hjust='inward',vjust='inward')
#################################################################
######################################################################

plot9


#########################
#########################
#ggplot2016
#####################
newdata4001<-newdata4001[order(newdata4001$index),]
date1<-format(newdata4002$date[184], format = "%m/%d")
label1<-newdata4002$marker2[184]
title1<-paste(date1, label1, sep =': ' )

plot1 <-gplot(adjustedStack[[184]]) + 
  geom_tile(aes(fill = value)) +
  scale_fill_scico(palette = 'batlow') +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  ggtitle(title1)+
  xlab("")+
  ylab("")+
  coord_equal()
plot1


###############
#########
p4<- ggplot(newdata4001, aes(x=as.Date(date), y=binmarker2, 
                             color=marker2, shape=factor(countMarker))) +
  geom_point() + 
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = seq(0, 1))+
  theme(legend.title=element_blank())+
  scale_color_manual(values = c("end date" = "#f0e442",
                                "start date"="#cc79a7",
                                "mid bloom"="#009e73",
                                "not bloom"="#0072b2",
                                "inconclusive" = "black"))+
  scale_shape_discrete(labels = c("", "2+ blooms")) 




q<-p4+scale_x_date(limit=c(as.Date("2016-01-01"),as.Date("2016-12-31")))+ggtitle("2016")
w<-p4+scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-12-31")))+ggtitle("2017")
e<-p4+scale_x_date(limit=c(as.Date("2018-01-01"),as.Date("2018-12-31")))+ggtitle("2018")
r<-p4+scale_x_date(limit=c(as.Date("2019-01-01"),as.Date("2019-12-31")))+ggtitle("2019")
t<-p4+scale_x_date(limit=c(as.Date("2020-01-01"),as.Date("2020-12-31")))+ggtitle("2020")
y<-p4+scale_x_date(limit=c(as.Date("2021-01-01"),as.Date("2021-12-31")))+ggtitle("2021")

plot<-ggarrange(q,w,e,r,t,y, rows=3, common.legend=TRUE,legend='right') 

annotate_figure(plot, top = text_grob("Bloom Dates - Missisquoi Bay", 
                                      face = "bold", size = 14))
