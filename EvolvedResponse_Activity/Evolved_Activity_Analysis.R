# Source Cleaned data (and packages)
#setwd("../../EvolvedResponse_Activity/")
source('Evolved_Activity_Clean.R')

# Visualization Plots:

LT_plot <- ggplot(dat.hourly, aes(x=hour, y= Hourly_activity, colour=Treatment)) + xlim(0,24) + ylim(0,600)
LT_plot2 <- LT_plot + geom_jitter(size=0.5) + geom_smooth(method = "loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
           xmin = 10, xmax = 22,
           ymin = 0, ymax = 600) +
  labs(y="Hourly Activity Counts", 
       x="Hour") +
  theme(text = element_text(size=15),
        legend.text=element_text(size=12),
        axis.text.x= element_text(size=12.5), axis.text.y= element_text(size=12.5)) +
  scale_colour_manual(values=c("#999999",  "#56B4E9", "#E69F00"))
print(LT_plot2)


# Model: Two methods tested (ns vs. sin/cos: Similar outputs so chose the Sin/Cos method)
#mod_trial_1 <- lmer(Hourly_activity ~ Treatment + Treatment:Population + light + light:Treatment + ns(hour, 5) + monitor + start_day + (1 + ns(hour, 5) + light | individual), data=dat.hourly)

# For sin cos: need hour2 == pi*hour/12
dat.hourly$hour2 <- (pi*dat.hourly$hour/12)

mod_trial_2 <- lmer(Hourly_activity ~ sin(hour2) + cos(hour2) + Treatment + Treatment:Population + light + light:Treatment + start_day + monitor
                    + (1 + light | individual), data=dat.hourly)

# Summary of model:
summary(mod_trial_2)
pacf(resid(mod_trial_2))
car::Anova(mod_trial_2)

#Extract Treatment by light effects:
Evolve_plot <- effect("Treatment*light", mod_trial_2)
Evolve_plot <- as.data.frame(Evolve_plot)

Evolve_plot2 <- ggplot(Evolve_plot, 
                       aes(y=fit, x=light, shape=Treatment, size=Treatment))

Evolve_plot3 <- Evolve_plot2 +
  geom_point(stat="identity", 
             position=position_dodge(0.5))+#, size=5) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5), size=1.5) + 
  labs(y="Hourly Activity Count", 
       x="Phase") +
  theme(text = element_text(size=15),
        legend.text=element_text(size=12),
        axis.text.x= element_text(size=12.5), axis.text.y= element_text(size=12.5)) +
  scale_shape_manual(values=c(15, 18, 17))+
  scale_size_manual(values=c(6,8,6))+
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))
print(Evolve_plot3)

#Evolve_plot3 + theme(legend.position = c(0.9, 0.22))
Evolve_plot4 <- Evolve_plot3 + theme(legend.justification=c(0,1), legend.position=c(0.01,0.99))
LT_plot3 <- LT_plot2 + theme(legend.justification=c(0,1), legend.position=c(0.01,0.99))
multiplot(Evolve_plot3, LT_plot2, cols = 2)
multiplot(Evolve_plot4, LT_plot3, cols = 2)


