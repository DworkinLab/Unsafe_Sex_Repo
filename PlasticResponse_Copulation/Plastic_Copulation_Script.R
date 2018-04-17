# Packages:
  source('packages.R')

# Read in Data:
  copulation <- read.csv("Mature.csv",h=T)

# Change box to treatment and to Predator vs. Control and factor:
  copulation$Treatment <- ifelse(copulation$Box=="C", 
                               "Predator", 
                               "Control")
  copulation$Treatment <- factor(copulation$Treatment)

#Remove Temp, and other unneccessary columns (covered in Day effects)
  copulation <- subset(copulation, select = 
                       -c(Box, File, Time_In, Cop_start, Cop_end, 
                          Temp, Humidity, BP.12.00.am, BP.8.00.Am, BP.Room))

#Copulation Proportion: If latency == 0, no copulation occues (i.e ==0)

  copulation$copulationSuccess <- ifelse(copulation$Cop_latency==0, 
                                       0, 1)

# Copulation proportion model:
  copprop_mod <- glmer(copulationSuccess ~ Treatment + (1|Date),
                     data = copulation, family = "binomial")

  copprop_eff <- effect("Treatment", copprop_mod)
  copprop_eff <- as.data.frame(copprop_eff)
  copprop_eff$Behaviour <- "Proportion Copulation"
  summary(copprop_mod)
  car::Anova(copprop_mod)

# Plot of effects:
gg_copprop <- ggplot(copprop_eff, aes(x=Treatment, y=fit, shape=Treatment))
gg_copprop2 <- gg_copprop +   
  ylab("Proportion of Copulation Success") + 
  xlab("") +
  ylim(0,1) +
  geom_point(stat="identity", 
             position = position_dodge(.9), size=6, show.legend = F) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.9), 
                size = 1.2, 
                width = 0.2, show.legend = F) + 
  theme(text = element_text(size=15), 
        #     axis.text.x=element_blank(),
        axis.text.x= element_text(size=12.5),
        axis.ticks.x=element_blank()) +
  scale_shape_manual(values=c(15, 17))+
  scale_color_manual(values=c("#999999", "#E69F00"))

print(gg_copprop2)


#Removing all values with no copulation: not interesting for latency/duration
copulation2 <- copulation[!(copulation$Cop_latency==0),]

# Copulation Latency

copul_lat_mod <- lmer(Cop_latency ~ Treatment + (1|Date), 
                      data = copulation2)

coplat_0_eff <- effect("Treatment", copul_lat_mod)
coplat_0_eff <- as.data.frame(coplat_0_eff)
coplat_0_eff$Behaviour <- "Copulation Latency"
summary(copul_lat_mod)
car::Anova(copul_lat_mod)


gg_coplat <- ggplot(coplat_0_eff, aes(x=Treatment, y=fit, shape=Treatment))
gg_coplat2 <- gg_coplat +
  ylab("Copulation Latency (sec)") + 
  xlab("") +
  ylim(300,800) +
  geom_point(stat="identity", 
             position = position_dodge(.9), size=6, show.legend = F) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.9), 
                size = 1.2, 
                width = 0.2, show.legend = F) + 
  theme(text = element_text(size=15), 
        #     axis.text.x=element_blank(),
        axis.text.x= element_text(size=12.5),
        axis.ticks.x=element_blank()) +
  scale_shape_manual(values=c(15, 17))+
  scale_color_manual(values=c("#999999", "#E69F00"))

print(gg_coplat2)

# Copulation Duration:

copul_dur_Mod <- lmer(Cop_Duration ~ Treatment + (1|Date), 
                      data = copulation2)

copdur_0_eff <- effect("Treatment", copul_dur_Mod)
copdur_0_eff <- as.data.frame(copdur_0_eff)
copdur_0_eff$Behaviour <- "Copulation Duration"
summary(copul_dur_Mod)
car::Anova(copul_dur_Mod)

gg_copdur <- ggplot(copdur_0_eff, aes(x=Treatment, y=fit, shape=Treatment))
gg_copdur2 <- gg_copdur + 
  ylab("Copulation Duration (sec)") + 
  xlab("") +
  ylim(700,1000) +
  geom_point(stat="identity", 
             position = position_dodge(.9), size=6, show.legend = F) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.9), 
                size = 1.2, 
                width = 0.2, show.legend = F) + 
  theme(text = element_text(size=15), 
        #     axis.text.x=element_blank(),
        axis.text.x= element_text(size=12.5),
        axis.ticks.x=element_blank()) +
  scale_shape_manual(values=c(15, 17))+
  scale_color_manual(values=c("#999999", "#E69F00"))

print(gg_copdur2)


multiplot(gg_copdur2, gg_coplat2, gg_copprop2, cols=3)
