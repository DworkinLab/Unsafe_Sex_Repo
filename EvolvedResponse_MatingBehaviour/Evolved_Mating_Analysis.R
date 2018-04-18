# Packages sourced in "Evolved_Mating_CleanData.R"

# Source cleaning data script:
  source("Evolved_Mating_CleanData.R")
  

### Models: 4 analysis done: Courtship Latency, Copulation Latency, Copulation Duration, and copulation occurance (proportion)
  
  
# Courtship Latency:
  
  mod_court <- lmer(Rel_Court_lat ~ 1 + Treatment*AgeBin + 
                      (1|Date) + (1|Treatment:Rep), 
                    data = AP_Data)
# Summary of model
  summary(mod_court)
  car::Anova(mod_court)
  
# Extract Effects and Plot:
  courtLat <- effect("Treatment*AgeBin", mod_court)
  courtLat <- as.data.frame(courtLat)
  
  latenCourt <- ggplot(courtLat, 
                       aes(y=fit, x=AgeBin, shape=Treatment, size=Treatment))
  
  latenCourt2 <- latenCourt + 
    geom_point(stat="identity", 
               position=position_dodge(0.5)) + 
    geom_linerange(aes(ymin=lower, ymax=upper), 
                   position = position_dodge(0.5), size=1.5) + 
    labs(y="Copulation Latency (sec)", 
         x="Age Bins") +
    #ggtitle("Courtship Latency") + 
    theme(text = element_text(size=15), 
          axis.text.x= element_text(size=12.5), axis.text.y= element_text(size=12.5)) +
    scale_shape_manual(values=c(15, 18, 17))+
    scale_size_manual(values = c(6,8,6)) +
    scale_colour_manual(values=
                          c("#999999", "#56B4E9", "#E69F00"))
  
  
  print(latenCourt2)
  
# Copulation Latency:
  
  mod_copl_plot <- lmer(Rel_Cop_lat ~ 1+ Treatment*AgeBin + 
                          (1|Date) + (1|Treatment:Rep), 
                        data = AP_Data)
  
  summary(mod_copl_plot)
  Anova(mod_copl_plot)
  
  copLate <- effect("Treatment*AgeBin", mod_copl_plot)
  copLate <- as.data.frame(copLate)
  
  LatenCop <- ggplot(copLate, 
                     aes(y=fit, x=AgeBin, shape=Treatment, size=Treatment))
  
  LatenCop2 <-  LatenCop + 
    geom_point(stat="identity", 
               position=position_dodge(0.5)) + 
    geom_linerange(aes(ymin=lower, ymax=upper), 
                   position = position_dodge(0.5), size=1.5) + 
    labs(y="Copulation Latency (sec)", 
         x="Age Bins") +
    #ggtitle("Copulation Latency") + 
    theme(text = element_text(size=15), 
          axis.text.x= element_text(size=12.5), axis.text.y= element_text(size=12.5)) +
    scale_shape_manual(values=c(15, 18, 17))+
    scale_size_manual(values = c(6,8,6)) +
    scale_colour_manual(values=
                          c("#999999", "#56B4E9", "#E69F00"))
  
  print(LatenCop2)
  
# Copulation Duration:
  
  mod_copd_plot <- lmer(Rel_Cop_dur ~ 1+ Treatment*AgeBin + 
                          (1|Date) + (1|Treatment:Rep), 
                        data = AP_Data)
  
  
  summary(mod_copd_plot)
  Anova(mod_copd_plot)
  
  
  copdur_plot <- effect("Treatment*AgeBin", mod_copd_plot)
  copdur_plot <- as.data.frame(copdur_plot)
  
  DuratCop <- ggplot(copdur_plot, 
                     aes(y=fit, x=AgeBin, shape=Treatment, size=Treatment))
  
  DuratCop2 <- DuratCop + 
    geom_point(stat="identity", 
               position=position_dodge(0.5)) + 
    geom_linerange(aes(ymin=lower, ymax=upper), 
                   position = position_dodge(0.5), size=1.5) + 
    labs(y="Copulation Duration (sec)", 
         x="Age Bins") +
    #ggtitle("Copulation Duration") + 
    theme(text = element_text(size=15), 
          axis.text.x= element_text(size=12.5), axis.text.y= element_text(size=12.5)) +
    scale_shape_manual(values=c(15, 18, 17))+
    scale_size_manual(values = c(6,8,6)) +
    scale_colour_manual(values=
                          c("#999999", "#56B4E9", "#E69F00"))
  
  
  print(DuratCop2)
  
  
  # Paul and Ian need to finish figuring out convergence issues for mixed glm. 
  mod_cop_count <- glmer(Copulation ~ 1 + Treatment*AgeBin + 
                           (1|Date) + (1|Treatment:Rep), 
                         family = "binomial", 
                         data = AP_Data)
  require(glmmTMB)
  TMB_mod_cop_count <- glmmTMB(Copulation ~ 1 + Treatment*AgeBin + 
                                (1|Date) + (1|Treatment:Rep), 
                           family = "binomial", 
                            data = AP_Data)
  
  summary(TMB_mod_cop_count)
  summary(mod_cop_count)
  
  #Anova(TMB_mod_cop_count)
  Anova(mod_cop_count)
  
  cop_prop_plot_glmer <- effect("Treatment*AgeBin", mod_cop_count)
  cop_prop_plot_glmer <- as.data.frame(cop_prop_plot_glmer)
  
  propCop_glmer <- ggplot(cop_prop_plot_glmer, 
                          aes(y=fit, x=AgeBin, shape=Treatment, size=Treatment))
  
  propCop_glmer_2 <- propCop_glmer + 
    geom_point(stat="identity", 
               position=position_dodge(0.5)) + 
    geom_linerange(aes(ymin=lower, ymax=upper), 
                   position = position_dodge(0.5), size=1.5) + 
    labs(y="Proportion of copulations", 
         x="Age Bins") +
    #ggtitle("Copulation Proportion") + 
    theme(text = element_text(size=15), 
          axis.text.x= element_text(size=15)) +
    scale_shape_manual(values=c(15, 18, 17))+
    scale_size_manual(values = c(6,8,6)) +
    scale_colour_manual(values=
                          c("#999999", "#56B4E9", "#E69F00"))
  
  propCop_glmer_2
  
  

  #Use regular glm for moment.
  mod_cop_count_glm <- glm(Copulation ~ 1 + Treatment*AgeBin, 
                           family = "binomial", 
                           data = AP_Data)
  
  summary(mod_cop_count_glm)
  Anova(mod_cop_count_glm)
  
  cop_prop_plot2 <- effect("Treatment*AgeBin", mod_cop_count_glm)
  cop_prop_plot2 <- as.data.frame(cop_prop_plot2)
  
#Change the error bars of Spider Age bin 4 (all = 1.00, so error == 0)
  cop_prop_plot2$lower <- ifelse(cop_prop_plot2$Treatment=="Spiders" & cop_prop_plot2$AgeBin==4, 1.00, cop_prop_plot2$lower )
  
  propCop4 <- ggplot(cop_prop_plot2, 
                     aes(y=fit, x=AgeBin, shape=Treatment, size=Treatment))
  
  propCop3 <- propCop4 + 
    geom_point(stat="identity", 
               position=position_dodge(0.5)) + 
    geom_linerange(aes(ymin=lower, ymax=upper), 
                   position = position_dodge(0.5), size=1.5) + 
    labs(y="Proportion of Copulations", 
         x="Age Bins") +
    #ggtitle("Copulation Proportion") + 
    theme(text = element_text(size=15), 
          axis.text.x= element_text(size=12.5), axis.text.y= element_text(size=12.5)) +
    scale_shape_manual(values=c(15, 18, 17))+
    scale_size_manual(values = c(6,8,6)) +
    scale_colour_manual(values=
                          c("#999999", "#56B4E9", "#E69F00"))
  
  propCop3
  
  
  multiplot(latenCourt2, LatenCop2, DuratCop2, propCop3, cols=2)
