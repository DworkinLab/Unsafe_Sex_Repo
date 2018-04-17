## Analysis and plots of mantid cues Activity Data:

  source('packages.R')
  source('MantidCues_ActivityClean.R')

# Visualization Plot:
  Mantid_hour$Cues <- Mantid_hour$Treatment
  Man_plot <- ggplot(Mantid_hour, aes(x=hour, y= activity_counts, colour=Cues)) + xlim(0,24) + ylim(0,100)
  Man_plot2 <- Man_plot + geom_jitter(size=0.5) + geom_smooth(method = "loess") + 
    annotate("rect", fill = "yellow", alpha = 0.2, 
             xmin = 10, xmax = 22,
             ymin = 0, ymax = 100) +
    labs(y="Hourly Activity Counts", 
         x="Hour") +
    theme(text = element_text(size=15),
          legend.text=element_text(size=12),
          axis.text.x= element_text(size=12.5), axis.text.y= element_text(size=12.5)) +
    scale_colour_manual(values=c("#999999",  "#56B4E9"))
  print(Man_plot2)
  
  
## Will need the hour turned into pi*hour/12 for lmer:
  Mantid_hour$hour2 <- (pi*Mantid_hour$hour/12)

# Model:  
  mantid_mod_spli_2 <- lmer(activity_counts ~ Treatment*light + sin(hour2) + cos(hour2)  + monitor
                            + (1 + light | individual), data=Mantid_hour)
  
  summary(mantid_mod_spli_2)
  #pacf(resid(mantid_mod_spli_2))
  car::Anova(mantid_mod_spli_2)
  
  mantid_plot <- effect("Treatment*light", mantid_mod_spli_2)
  mantid_plot <- as.data.frame(mantid_plot)
  mantid_plot$Cues <-  mantid_plot$Treatment
  mantid_plot3 <- ggplot(mantid_plot, 
                         aes(y=fit, x=light, shape=Cues, size=Cues)) + 
    geom_point(stat="identity", 
               position=position_dodge(0.5)) + #, size=5) + 
    geom_linerange(aes(ymin=lower, ymax=upper), 
                   position = position_dodge(0.5), size=1.5) + 
    labs(y="Hourly Activity Count", 
         x="Phase") +
    theme(text = element_text(size=15),
          legend.text=element_text(size=12),
          axis.text.x= element_text(size=12.5), axis.text.y= element_text(size=12.5)) +
    scale_shape_manual(values=c(15, 18))+
    scale_size_manual(values = c(6,8)) +
    scale_colour_manual(values=
                          c("#999999",  "#56B4E9"))
  print(mantid_plot3)

  mantid_plot4 <- mantid_plot3 + theme(legend.justification=c(0,1), legend.position=c(0.05,0.95))
  Man_plot3 <- Man_plot2 + theme(legend.justification=c(0,1), legend.position=c(0.05,0.95))
  multiplot(mantid_plot3, Man_plot2, cols = 2)
  multiplot(mantid_plot4, Man_plot3, cols = 2)
  
  
  