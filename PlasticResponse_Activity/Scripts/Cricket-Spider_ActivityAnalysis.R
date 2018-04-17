## Analysis and plots of Cricket v. Spider cues, Activity Data:

  source('packages.R')
  source('Cricket-SpiderCues_ActivityClean.R')

  Exp2_hour$Cues <- Exp2_hour$Treatment
  plot_Exp2 <- ggplot(Exp2_hour, aes(x=hour, y= activity_counts, colour=Cues)) + xlim(0,25) + ylim(0,400)
  plot_Exp2_2 <- plot_Exp2 + geom_jitter(size=0.5) + geom_smooth(size=1, method="loess") + 
    annotate("rect", fill = "yellow", alpha = 0.2, 
             xmin = 10, xmax = 22,
             ymin = 0, ymax = 400) +
    labs(y="Hourly Activity Counts", 
         x="Hour") +
    theme(text = element_text(size=15),
          legend.text=element_text(size=12),
          axis.text.x= element_text(size=12.5),  axis.text.y= element_text(size=12.5)) +
    scale_colour_manual(values=c("grey20", "#E69F00"))
  print(plot_Exp2_2)

## Will need the hour turned into pi*hour/12 for lmer:
  Exp2_hour$hour2 <- (pi*Exp2_hour$hour/12)

# Model:
  Exp2_mod_spli_2 <- lmer(activity_counts ~ sin(hour2) + cos(hour2) + Treatment*light + monitor
                          + (1 + light | individual), data=Exp2_hour)
  
  summary(Exp2_mod_spli_2)
  #pacf(resid(Exp2_mod_spli_2))
  car::Anova(Exp2_mod_spli_2)

  Exp2_plot <- effect("Treatment*light", Exp2_mod_spli_2)
  Exp2_plot <- as.data.frame(Exp2_plot)
  Exp2_plot$Cues <- Exp2_plot$Treatment
  Exp2_plot3 <- ggplot(Exp2_plot, aes(y=fit, x=light, shape=Cues)) + 
    geom_point(stat="identity", 
               position=position_dodge(0.5), size=6) + 
    geom_linerange(aes(ymin=lower, ymax=upper), 
                   position = position_dodge(0.5), size=1.5) + 
    labs(y="Hourly Activity Count", 
         x="Phase") +
    theme(text = element_text(size=15),
          legend.text=element_text(size=12),
          axis.text.x= element_text(size=12.5),  axis.text.y= element_text(size=12.5)) +
    scale_shape_manual(values=c(16, 17))+
    scale_colour_manual(values=
                          c("grey20", "#E69F00"))
  print(Exp2_plot3)


  Exp2_plot4 <-  Exp2_plot3 + theme(legend.justification=c(0,1), legend.position=c(0.05,0.95))
  plot_Exp2_3 <- plot_Exp2_2 + theme(legend.justification=c(0,1), legend.position=c(0.05,0.95))
  multiplot(Exp2_plot3, plot_Exp2_2, cols = 2)
  multiplot(Exp2_plot4, plot_Exp2_3, cols = 2)
  