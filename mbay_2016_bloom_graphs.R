#########################
#########################
#ggplot2016
#####################

#0
newdata4001<-newdata4001[order(newdata4001$index),]
date0<-format(newdata4001$date[183], format = "%m/%d")
label0<-newdata4001$marker2[183]
title0<-paste(date0, label0, sep =': ' )

plot0 <-gplot(adjustedStack[[183]]) + 
  geom_tile(aes(fill = value)) +
  scale_fill_scico(palette = 'batlow') +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  ggtitle(title0)+
  xlab("")+
  ylab("")+
  coord_equal()+ theme(plot.title = element_text(size=8))




#1
newdata4001<-newdata4001[order(newdata4001$index),]
date1<-format(newdata4001$date[184], format = "%m/%d")
label1<-newdata4001$marker2[184]
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
  coord_equal()+ theme(plot.title = element_text(size=8))



#2
newdata4001<-newdata4001[order(newdata4001$index),]
date2<-format(newdata4001$date[185], format = "%m/%d")
label2<-newdata4001$marker2[185]
title2<-paste(date2, label2, sep =': ' )

plot2 <-gplot(adjustedStack[[185]]) + 
  geom_tile(aes(fill = value)) +
  scale_fill_scico(palette = 'batlow') +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  ggtitle(title2)+
  xlab("")+
  ylab("")+
  coord_equal()+ theme(plot.title = element_text(size=8))


#3
newdata4001<-newdata4001[order(newdata4001$index),]
date3<-format(newdata4001$date[186], format = "%m/%d")
label3<-newdata4001$marker2[186]
title3<-paste(date3, label3, sep =': ' )

plot3<-gplot(adjustedStack[[186]]) + 
  geom_tile(aes(fill = value)) +
  scale_fill_scico(palette = 'batlow') +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  ggtitle(title3)+
  xlab("")+
  ylab("")+
  coord_equal()+ theme(plot.title = element_text(size=8))


#4
newdata4001<-newdata4001[order(newdata4001$index),]
date4<-format(newdata4001$date[187], format = "%m/%d")
label4<-newdata4001$marker2[187]
title4<-paste(date4, label4, sep =': ' )

plot4 <-gplot(adjustedStack[[187]]) + 
  geom_tile(aes(fill = value)) +
  scale_fill_scico(palette = 'batlow') +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  ggtitle(title4)+
  xlab("")+
  ylab("")+
  coord_equal()+ theme(plot.title = element_text(size=8))


#5
newdata4001<-newdata4001[order(newdata4001$index),]
date5<-format(newdata4001$date[189], format = "%m/%d")
label5<-newdata4001$marker2[189]
title5<-paste(date5, label5, sep =': ' )

plot5<-gplot(adjustedStack[[189]]) + 
  geom_tile(aes(fill = value)) +
  scale_fill_scico(palette = 'batlow') +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  ggtitle(title5)+
  xlab("")+
  ylab("")+
  coord_equal()+ theme(plot.title = element_text(size=8))


#6
newdata4001<-newdata4001[order(newdata4001$index),]
date6<-format(newdata4001$date[190], format = "%m/%d")
label6<-newdata4001$marker2[190]
title6<-paste(date6, label6, sep =': ' )

plot6<-gplot(adjustedStack[[190]]) + 
  geom_tile(aes(fill = value)) +
  scale_fill_scico(palette = 'batlow') +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  ggtitle(title6)+
  xlab("")+
  ylab("")+
  coord_equal()+ theme(plot.title = element_text(size=8))

#7
newdata4001<-newdata4001[order(newdata4001$index),]
date7<-format(newdata4001$date[191], format = "%m/%d")
label7<-newdata4001$marker2[191]
title7<-paste(date7, label7, sep =': ' )

plot7<-gplot(adjustedStack[[191]]) + 
  geom_tile(aes(fill = value)) +
  scale_fill_scico(palette = 'batlow') +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  ggtitle(title7)+
  xlab("")+
  ylab("")+
  coord_equal() + theme(plot.title = element_text(size=8))

#8
newdata4001<-newdata4001[order(newdata4001$index),]
date8<-format(newdata4001$date[192], format = "%m/%d")
label8<-newdata4001$marker2[192]
title8<-paste(date8, label8, sep =': ' )

plot8<-gplot(adjustedStack[[192]]) + 
  geom_tile(aes(fill = value)) +
  scale_fill_scico(palette = 'batlow') +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  ggtitle(title8)+
  xlab("")+
  ylab("")+
  coord_equal() + theme(plot.title = element_text(size=8))

#9
date9<-format(newdata4001$date[193], format = "%m/%d")
label9<-newdata4001$marker2[193]
title9<-paste(date9, label9, sep =': ' )

plot9<-gplot(adjustedStack[[193]]) + 
  geom_tile(aes(fill = value)) +
  scale_fill_scico(palette = 'batlow') +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  ggtitle(title9)+
  xlab("")+
  ylab("")+
  coord_equal() + theme(plot.title = element_text(size=8))

#10

date10<-format(newdata4001$date[194], format = "%m/%d")
label10<-newdata4001$marker2[194]
title10<-paste(date10, label10, sep =': ' )

plot10<-gplot(adjustedStack[[194]]) + 
  geom_tile(aes(fill = value)) +
  scale_fill_scico(palette = 'batlow') +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  ggtitle(title10)+
  xlab("")+
  ylab("")+
  coord_equal()+ theme(plot.title = element_text(size=8))

#plot all
plot<-ggarrange(plot0,plot1,plot2,plot3,plot4,plot5,plot6,plot7,
                plot8, plot9,plot10
                , nrow=3,ncol=5, common.legend=TRUE,legend='right') 
annotate_figure(plot, top = text_grob("2016 Bloom - Missisquoi Bay", 
                                      face = "bold", size = 14))




###################
plot<-ggarrange(plot0,plot1,plot2,plot3,plot4,plot5,plot6,plot7,
                plot8, plot9,plot10
                , nrow=3,ncol=5, common.legend=TRUE,legend='right') 
annotate_figure(plot, top = text_grob("2016 Bloom - Missisquoi Bay", 
                                      face = "bold", size = 14))

plot0
