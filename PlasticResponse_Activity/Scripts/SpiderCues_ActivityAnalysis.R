## Analysis and plots of Spider cues Activity Data:

  source('packages.R')
  source('SpiderCues_ActivityClean.R')

  act_hour$Cues <- act_hour$Treatment
  spi_plot <- ggplot(act_hour, aes(x=hour, y= activity_counts, colour=Cues)) + ylim(0,400) + xlim(0,24)
  spi_plot2 <- spi_plot + geom_jitter(size=0.5) + geom_smooth(size=1, method="loess") + 
    annotate("rect", fill = "yellow", alpha = 0.2, 
             xmin = 10, xmax = 22,
             ymin = 0, ymax = 400) +
    labs(y="Hourly Activity Counts", 
         x="Hour") +
    theme(text = element_text(size=15),
          legend.text=element_text(size=12),
          axis.text.x= element_text(size=12.5), axis.text.y= element_text(size=12.5)) +
    scale_colour_manual(values=c("#999999", "#E69F00"))
  print(spi_plot2)
  
  
## Will need the hour turned into pi*hour/12 for lmer:
  act_hour$hour2 <- (pi*act_hour$hour/12)

  spider_mod_spli_2 <- lmer(activity_counts ~ Treatment*light + sin(hour2) + cos(hour2)  + monitor
                            + (1 + light | individual), data=act_hour)
  
  summary(spider_mod_spli_2)
  #pacf(resid(spider_mod_spli_2))
  car::Anova(spider_mod_spli_2)
  
  
  spider_plot <- effect("Treatment*light", spider_mod_spli_2)
  spider_plot <- as.data.frame(spider_plot)
  spider_plot$Cues <- spider_plot$Treatment
  spider_plot2 <- ggplot(spider_plot, 
                         aes(y=fit, x=light, shape=Cues)) + 
    geom_point(stat="identity", 
               position=position_dodge(0.5), size=6) + 
    geom_linerange(aes(ymin=lower, ymax=upper), 
                   position = position_dodge(0.5), size=1.5) + 
    labs(y="Hourly Activity Count", 
         x="Phase") +
    theme(text = element_text(size=15),
          legend.text=element_text(size=12),
          axis.text.x= element_text(size=12.5), axis.text.y= element_text(size=12.5)) +
    scale_shape_manual(values=c(15, 17))+
    scale_colour_manual(values=
                          c("#999999", "#E69F00"))
  print(spider_plot2)
  
  spider_plot3 <- spider_plot2 + theme(legend.justification=c(0,1), legend.position=c(0.05,0.95))
  spi_plot3 <- spi_plot2 + theme(legend.justification=c(0,1), legend.position=c(0.05,0.95))

  multiplot(spider_plot3, spi_plot3, cols = 2)
  
  
  