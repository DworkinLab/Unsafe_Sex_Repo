## Analysis and plots of Complex cues Activity Data:

  source('packages.R')
  source('ComplexCues_ActivityClean.R')

# Visualization Plot:

  plot_Exp3 <- ggplot(Exp3_hour, aes(x=hour, y= activity_counts, colour=Cues)) #+ xlim(0,24) + ylim(0,500)
  plot_Exp3_2 <- plot_Exp3 + geom_jitter(size=0.5) + geom_smooth(size=1, method="loess") + 
    annotate("rect", fill = "yellow", alpha = 0.2, 
             xmin = 10, xmax = 22,
             ymin = 0, ymax = 500) +
    labs(y="Hourly Counts", 
         x="Hour") +
    theme(text = element_text(size=15),
          legend.text=element_text(size=12),
          axis.text.x= element_text(size=12.5), axis.text.y= element_text(size=12.5)) +
    scale_colour_manual(values=c("grey20", "darkorange3", "#E69F00","thistle4"))
  print(plot_Exp3_2)
  
  
  
## Will need the hour turned into pi*hour/12 for lmer:
  Exp3_hour$hour2 <- (pi*Exp3_hour$hour/12)

  # Two methods to account for daily trends in activity: ns and sin/cos --> similar results, sin cos chosen.
  #Exp3_mod_spli <- lmer(activity_counts ~ ns(hour, 5) + monitor + Treatment*light +  (1 + ns(hour, 5)) + (1 + light | individual), data=Exp3_hour)

## Sin Cos lmer
  Exp3_mod_spli_2 <- lmer(activity_counts ~ sin(hour2) + cos(hour2) + Cues*light + monitor
                          + (1 + light | individual), data=Exp3_hour)
  
# Summary Stats 
  summary(Exp3_mod_spli_2)
  #pacf(resid(Exp3_mod_spli_2))
  car::Anova(Exp3_mod_spli_2)
  anova(Exp3_mod_spli_2)
#Extract effects
  Exp3_plot <- effect("Cues*light", Exp3_mod_spli_2)
  Exp3_plot <- as.data.frame(Exp3_plot)

# Plot:
  Exp3_plot2 <- ggplot(Exp3_plot, aes(y=fit, x=light, shape=Cues, size=Cues)) + 
    geom_point(stat="identity", position=position_dodge(0.5), size=6) + 
    geom_linerange(aes(ymin=lower, ymax=upper), position = position_dodge(0.5), size=1.5) +
    labs(y="Hourly Activity Count", x="Phase") +
    scale_shape_manual(values=c(16, 1, 6, 17))+
    theme(text = element_text(size=15),
          legend.text=element_text(size=12),
          axis.text.x= element_text(size=12.5), axis.text.y= element_text(size=12.5)) +
    scale_size_manual(values = c(6,9,6,6)) +
    scale_colour_manual(values=c("grey20",  "thistle4", "darkorange3", "#E69F00"))
  #Can Change the Cues names in Clean script (bottom)
  print(Exp3_plot2)
  
  Exp3_plot3 <-  Exp3_plot2 + theme(legend.justification=c(1,0), legend.position=c(0.99,0.01))
  plot_Exp3_3 <- plot_Exp3_2 + theme(legend.justification=c(1,1), legend.position=c(0.99,0.99))
  #multiplot(Exp3_plot2, plot_Exp3_2, cols = 2)
  multiplot(Exp3_plot3, plot_Exp3_3, cols = 2)

  
    
  