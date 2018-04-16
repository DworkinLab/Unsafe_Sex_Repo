### Packages Requires:
  require(lme4)
  require(ggplot2)
  require(effects)
  require(car)

### Read in the data:
  court <- read.table("Courtship_FINAL.txt", h=T)

### Rename treatment to full names and make factor
  court$Treatment <- ifelse(court$Trt=="P", "Mantids",
                            ifelse(court$Trt=="C","Control", 
                                   "Spiders"))

  court$Treatment <- as.factor(court$Treatment)

### Change Dark and light to lower case and factor
  court$Phase <- ifelse(court$Phase=="Dark", "dark", "light")
  
  court$Phase <- as.factor(court$Phase)

### Run glm on proportion of time courting in 15 minutes:
  stuff2 <- lmer(P_court ~ Treatment*Phase + Observer + 
                   (1|Day) + (1|Treatment:Population), 
                 data=court)
  
### Output from Model:
  summary(stuff2)
  car::Anova(stuff2)

### Plot all effects (if interested)
  plot(allEffects(stuff2))

### Extract the Treatment*Phase effect and create data.frame of data:
  stuff_plot <- effect("Treatment*Phase", stuff2)
  stuff_plot <- as.data.frame(stuff_plot)

### Create Plot of data:
stuff_plot2 <- ggplot(stuff_plot, 
                      aes(y=fit, x=Phase, shape=Treatment)) +
  geom_point(stat="identity", 
             position=position_dodge(0.5), size=7) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5), size=1.5) + 
  labs(y="Proportion", 
       x="Phase") +
  theme(text = element_text(size=20), 
        axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15))
  # If colours wanted: create colour=Treatment
  #scale_colour_manual(values=
  #                     c("#999999", "#56B4E9", "#E69F00"))

print(stuff_plot2)

