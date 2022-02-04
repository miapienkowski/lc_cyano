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

#check
tempComposites[[2]]
plot(tempComposites[[133]])
raster(tempComposites[[2]])


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

#4. sum
(sum1 <-stackApply(cropComposites, indices = rep(1, nlayers(cropComposites)), fun = sum))
plot(sum1)
plot1 <-gplot(sum1) + 
  geom_tile(aes(fill = value)) +
  scale_fill_scico(palette = 'batlow') +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  ggtitle("M. Bay")+
  xlab("")+
  ylab("")+
  coord_equal()
plot1

#5. max
(max1 <-stackApply(cropComposites, indices = rep(1, nlayers(cropComposites)), fun = max))
plot(max1)
plot2 <-gplot(max1) + 
  geom_tile(aes(fill = value)) +
  scale_fill_scico(palette = 'batlow') +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  ggtitle("M. Bay")+
  xlab("")+
  ylab("")+
  coord_equal()
plot2

ggarrange(plot1,plot2, ncol=2, nrow=1)


#scaling pixels
adjustedStack <- calc(cropComposites, fun=function(x){10^((3/250)*x-4.2)})
adjustedStack


#threshold
threshold = .004
threshold2 = .001
#classify imgs based on whether or not they have bloom pixel or not

#1
blooms<-stack()
noblooms<-stack()
for(i in 1:283){
  vals<-getValues(adjustedStack[[i]]) #returns vector
  if(length(which(as.vector(vals) > threshold)) > 0){
    blooms<-stack(blooms, adjustedStack[[i]])
  }
  else{noblooms<-stack(noblooms,adjustedStack[[i]])}
}
blooms #43
noblooms #240

#2
blooms<-stack()
noblooms<-stack()
for(i in 1:283){
  vals<-getValues(adjustedStack[[i]]) #returns vector
  if(length(which(as.vector(vals) > threshold2)) > 0){
    blooms<-stack(blooms, adjustedStack[[i]])
  }
  else{noblooms<-stack(noblooms,adjustedStack[[i]])}
}
blooms #63
noblooms #220

#exploring
plot(blooms[[2]])
plot(blooms[[60]])

#get dates ( chooose earliest date)
dateDF <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(dateDF) <- c('index','date','altdate')

for(i in 1:nlayers(blooms)){
  filename <- names(blooms[[i]])
  (date<-strapply(filename,"[0-9]{4}.[0-9]{2}[0-9]{2}.[0-9]{2}[0-9]{2}", simplify = TRUE))
  year <- substr(date, 1, 4)
  month <- substr(date, 6,7)
  day <- substr(date, 8,9)
  hour <-substr(date, 11, 12)
  minute <- substr(date, 13, 14)
  
  
  (finalDate<-paste(year,month,day, sep='-'))
  (finalTime<-paste(hour, minute, sep=':'))
  
  (dateTime<-paste(finalDate, finalTime, sep=" "))
  v<-c(as.POSIXct(dateTime, format="%Y-%m-%d %H:%M"))
  
  newRow1 <- data.frame(index = i, date = v)
  dateDF <- rbind(dateDF, newRow1)
  
}

View(dateDF)
###############################
###############################
###############################
#build dataset with bloom label
ourDF <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(ourDF) <- c('index','date','label', 'altdate')

blooms<-stack()
noblooms<-stack()

ourDF$altdate<-as.Date(character(0), format="%m/%d/%Y")


for(i in 1:nlayers(adjustedStack)){
  
  #################################
  #classify
  ##################################
  class <- NA
  vals<-getValues(adjustedStack[[i]]) #returns vector
  if(length(which(as.vector(vals) > threshold2)) > 0){
    blooms<-stack(blooms, adjustedStack[[i]])
    class <-1
  }
  else{
    noblooms<-stack(noblooms,adjustedStack[[i]])
    class <- 0
  }
  
  #################################
  #date
  ##################################
  filename <- names(adjustedStack[[i]])
  (date<-strapply(filename,"[0-9]{4}.[0-9]{2}[0-9]{2}.[0-9]{2}[0-9]{2}", simplify = TRUE))
  year <- substr(date, 1, 4)
  month <- substr(date, 6,7)
  day <- substr(date, 8,9)
  hour <-substr(date, 11, 12)
  minute <- substr(date, 13, 14)
  
  
  (finalDate<-paste(year,month,day, sep='-'))
  (finalTime<-paste(hour, minute, sep=':'))
  (finalDate2<-paste(month,day,year, sep='/'))
  
  (dateTime<-paste(finalDate, finalTime, sep=" "))
  v<-c(as.POSIXct(dateTime, format="%Y-%m-%d %H:%M"))
  date2<-as.Date(finalDate2, format="%m/%d/%Y")
  
  #################################
  #build row
  ##################################
  newRow <- data.frame(index = i, date = v, label = class, altdate=date2)
  ourDF <- rbind(ourDF, newRow)
  
}

View(ourDF)

plot(ourDF$date, ourDF$label)
par(mfrow=c(1,1))

p<- ggplot(ourDF, aes(x=as.Date(date), y=label)) +
  geom_point() + 
  xlab("")
p
p+scale_x_date(date_labels = "%b")
p+scale_x_date(date_labels = "%m-%Y")


q<-p+scale_x_date(limit=c(as.Date("2016-01-01"),as.Date("2016-12-31")))+ggtitle("2016")
w<-p+scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-12-31")))+ggtitle("2017")
e<-p+scale_x_date(limit=c(as.Date("2018-01-01"),as.Date("2018-12-31")))+ggtitle("2018")
r<-p+scale_x_date(limit=c(as.Date("2019-01-01"),as.Date("2019-12-31")))+ggtitle("2019")
t<-p+scale_x_date(limit=c(as.Date("2020-01-01"),as.Date("2020-12-31")))+ggtitle("2020")
y<-p+scale_x_date(limit=c(as.Date("2021-01-01"),as.Date("2021-12-31")))+ggtitle("2021")

ggarrange(q,w,e,r,t,y, rows=1, cols=6)


q<-p+scale_x_date(limit=c(as.Date("2016-07-01"),as.Date("2016-10-31")))+ggtitle("2016")
w<-p+scale_x_date(limit=c(as.Date("2017-07-01"),as.Date("2017-10-31")))+ggtitle("2017")
e<-p+scale_x_date(limit=c(as.Date("2018-07-01"),as.Date("2018-10-31")))+ggtitle("2018")
r<-p+scale_x_date(limit=c(as.Date("2019-07-01"),as.Date("2019-10-31")))+ggtitle("2019")
t<-p+scale_x_date(limit=c(as.Date("2020-07-01"),as.Date("2020-10-31")))+ggtitle("2020")
y<-p+scale_x_date(limit=c(as.Date("2021-07-01"),as.Date("2021-10-31")))+ggtitle("2021")

ggarrange(q,w,e,r,t,y, rows=1, cols=6)


#focus on start date

#find where there are two 1's in a row
#label first as a start date
ourDF$startDate<-0
newdata2 <- ourDF
newdata2$date<- as.Date(newdata2$date)
View(newdata2)

newdata400<-newdata2[order(as.Date(newdata2$date, format="%d/%m/%Y")),]
View(newdata400)

for(i in 1:nrow(newdata400)){
  if (i != 1){
    if(newdata400$label[i] == 1 & newdata400$label[i-1] == 0){
      newdata400$startDate[i] = 1
    }
  }
  else{
    if (newdata400$label[i] == 1){
      newdata400$startDate[i] = 1
    }
  }
}

p<- ggplot(newdata400, aes(x=as.Date(date), y=startDate)) +
  geom_point() + 
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = seq(0, 1))

q<-p+scale_x_date(limit=c(as.Date("2016-07-01"),as.Date("2016-10-31")))+ggtitle("2016")
w<-p+scale_x_date(limit=c(as.Date("2017-07-01"),as.Date("2017-10-31")))+ggtitle("2017")
e<-p+scale_x_date(limit=c(as.Date("2018-07-01"),as.Date("2018-10-31")))+ggtitle("2018")
r<-p+scale_x_date(limit=c(as.Date("2019-07-01"),as.Date("2019-10-31")))+ggtitle("2019")
t<-p+scale_x_date(limit=c(as.Date("2020-07-01"),as.Date("2020-10-31")))+ggtitle("2020")
y<-p+scale_x_date(limit=c(as.Date("2021-07-01"),as.Date("2021-10-31")))+ggtitle("2021")

plot<-ggarrange(q,w,e,r,t,y, rows=1, cols=6) 
annotate_figure(plot, top = text_grob("Start Dates", 
                                     face = "bold", size = 14))


#onset analysis?
#duration
#find end dates
#methodology - single days are being labeled as start dates, not end dates as of now

newdata400$endDate<-0

for(i in 1:nrow(newdata400)){
  if (i != 1 & i != nrow(newdata400)){
    if(newdata400$label[i] == 1 & newdata400$label[i-1] == 1 & newdata400$label[i+1] == 0){
      newdata400$endDate[i] = 1
    }
  }
  else{
    if (newdata400$label[i] == 1){
      newdata400$endDate[i] = 1
    }
  }
}

p2<- ggplot(newdata400, aes(x=as.Date(date), y=endDate)) +
  geom_point() + 
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = seq(0, 1))

q<-p2+scale_x_date(limit=c(as.Date("2016-07-01"),as.Date("2016-10-31")))+ggtitle("2016")
w<-p2+scale_x_date(limit=c(as.Date("2017-07-01"),as.Date("2017-10-31")))+ggtitle("2017")
e<-p2+scale_x_date(limit=c(as.Date("2018-07-01"),as.Date("2018-10-31")))+ggtitle("2018")
r<-p2+scale_x_date(limit=c(as.Date("2019-07-01"),as.Date("2019-10-31")))+ggtitle("2019")
t<-p2+scale_x_date(limit=c(as.Date("2020-07-01"),as.Date("2020-10-31")))+ggtitle("2020")
y<-p2+scale_x_date(limit=c(as.Date("2021-07-01"),as.Date("2021-10-31")))+ggtitle("2021")

plot<-ggarrange(q,w,e,r,t,y, rows=1, cols=6) 
annotate_figure(plot, top = text_grob("End Dates", 
                                      face = "bold", size = 14))

#plot start dates and end dates on same plot
###################################
#######################################
newdata400$marker<-'not start/end'
newdata400$binmarker<-0
for(i in 1:nrow(newdata400)){
  #start date marker - 'start'
  if(newdata400$startDate[i] == 1){
    newdata400$marker[i] = 'start date'
    newdata400$binmarker[i] = 1
  }
  #end date marker - 'end'
  if(newdata400$endDate[i] == 1){
    newdata400$marker[i] = 'end date'
    newdata400$binmarker[i] = 1
  }
}

p3<- ggplot(newdata400, aes(x=as.Date(date), y=binmarker, color=marker)) +
  geom_point() + 
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = seq(0, 1))+
  theme(legend.title=element_blank())+
  scale_color_manual(values = c("end date" = "blue",
                                "start date"="red",
                                "not start/end date"="black")) 

q<-p3+scale_x_date(limit=c(as.Date("2016-07-01"),as.Date("2016-10-31")))+ggtitle("2016")
w<-p3+scale_x_date(limit=c(as.Date("2017-07-01"),as.Date("2017-10-31")))+ggtitle("2017")
e<-p3+scale_x_date(limit=c(as.Date("2018-07-01"),as.Date("2018-10-31")))+ggtitle("2018")
r<-p3+scale_x_date(limit=c(as.Date("2019-07-01"),as.Date("2019-10-31")))+ggtitle("2019")
t<-p3+scale_x_date(limit=c(as.Date("2020-07-01"),as.Date("2020-10-31")))+ggtitle("2020")
y<-p3+scale_x_date(limit=c(as.Date("2021-07-01"),as.Date("2021-10-31")))+ggtitle("2021")

plot<-ggarrange(q,w,e,r,t,y, rows=3, common.legend=TRUE,legend='right') 
annotate_figure(plot, top = text_grob("Start and End Dates", 
                                      face = "bold", size = 14))


# plotting all bloom dates together color coded
###################################
#######################################
newdata400$marker2<-'not bloom'
newdata400$binmarker2<-0
for(i in 1:nrow(newdata400)){
  
  if(newdata400$label[i] ==1){
    #start date marker - 'start'
    if(newdata400$startDate[i] == 1){
      newdata400$marker2[i] = 'start date'
      newdata400$binmarker2[i] = 1
    }
    #end date marker - 'end'
    else if(newdata400$endDate[i] == 1){
      newdata400$marker2[i] = 'end date'
      newdata400$binmarker2[i] = 1
    }
    else{
      newdata400$marker2[i] = 'mid bloom'
      newdata400$binmarker2[i] = 1
    }
  }
}

p4<- ggplot(newdata400, aes(x=as.Date(date), y=binmarker2, color=marker2)) +
  geom_point() + 
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = seq(0, 1))+
  theme(legend.title=element_blank())+
  scale_color_manual(values = c("end date" = "blue",
                                "start date"="red",
                                "mid bloom"="green",
                                "not bloom"="black")) +
  geom_text(aes(color = 'black', label=ifelse(marker2=='start date',as.Date(altdate, format='%Y-%m-%d'),'')),hjust='inward',vjust='inward')

q<-p4+scale_x_date(limit=c(as.Date("2016-07-01"),as.Date("2016-10-31")))+ggtitle("2016")
w<-p4+scale_x_date(limit=c(as.Date("2017-07-01"),as.Date("2017-10-31")))+ggtitle("2017")
e<-p4+scale_x_date(limit=c(as.Date("2018-07-01"),as.Date("2018-10-31")))+ggtitle("2018")
r<-p4+scale_x_date(limit=c(as.Date("2019-07-01"),as.Date("2019-10-31")))+ggtitle("2019")
t<-p4+scale_x_date(limit=c(as.Date("2020-07-01"),as.Date("2020-10-31")))+ggtitle("2020")
y<-p4+scale_x_date(limit=c(as.Date("2021-07-01"),as.Date("2021-10-31")))+ggtitle("2021")

plot<-ggarrange(q,w,e,r,t,y, rows=3, common.legend=TRUE,legend='right') 
annotate_figure(plot, top = text_grob("Bloom Dates", 
                                      face = "bold", size = 14))
########################################################
library(ggrepel)

p4<- ggplot(newdata400, aes(x=as.Date(date), y=binmarker2, color=marker2)) +
  geom_point() + 
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = seq(0, 1))+
  theme(legend.title=element_blank())+
  scale_color_manual(values = c("end date" = "blue",
                                "start date"="red",
                                "mid bloom"="green",
                                "not bloom"="black")) +
  geom_text(aes(color = 'black', label=ifelse(marker2=='start date',format(date, format = "%m/%d"),'')),hjust='inward',vjust='inward')

q<-p4+scale_x_date(limit=c(as.Date("2016-07-01"),as.Date("2016-10-31")))+ggtitle("2016")
w<-p4+scale_x_date(limit=c(as.Date("2017-07-01"),as.Date("2017-10-31")))+ggtitle("2017")
e<-p4+scale_x_date(limit=c(as.Date("2018-07-01"),as.Date("2018-10-31")))+ggtitle("2018")
r<-p4+scale_x_date(limit=c(as.Date("2019-07-01"),as.Date("2019-10-31")))+ggtitle("2019")
t<-p4+scale_x_date(limit=c(as.Date("2020-07-01"),as.Date("2020-10-31")))+ggtitle("2020")
y<-p4+scale_x_date(limit=c(as.Date("2021-07-01"),as.Date("2021-10-31")))+ggtitle("2021")

plot<-ggarrange(q,w,e,r,t,y, rows=3, common.legend=TRUE,legend='right') 
annotate_figure(plot, top = text_grob("Bloom Dates", 
                                      face = "bold", size = 14))
########################################################


q<-p4+scale_x_date(limit=c(as.Date("2016-01-01"),as.Date("2016-12-31")))+ggtitle("2016")
w<-p4+scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-12-31")))+ggtitle("2017")
e<-p4+scale_x_date(limit=c(as.Date("2018-01-01"),as.Date("2018-12-31")))+ggtitle("2018")
r<-p4+scale_x_date(limit=c(as.Date("2019-01-01"),as.Date("2019-12-31")))+ggtitle("2019")
t<-p4+scale_x_date(limit=c(as.Date("2020-01-01"),as.Date("2020-12-31")))+ggtitle("2020")
y<-p4+scale_x_date(limit=c(as.Date("2021-01-01"),as.Date("2021-12-31")))+ggtitle("2021")



####################################################
#PIXEL CLASSIFICATION VALIDATION
#check there is at least one valid pixel
########################################################################################
##############################   01/28/2022    #########################################
########################################################################################


##################trying to see max pixel value in noblooms
naSearch<- getValues(noblooms)
max(noblooms)
max(blooms)
min(blooms)# no cell values associated with this
# no value data showing up for either of them, very weird

max(noblooms, na.rm=TRUE) #.008
max(blooms, na.rm=TRUE) #63

