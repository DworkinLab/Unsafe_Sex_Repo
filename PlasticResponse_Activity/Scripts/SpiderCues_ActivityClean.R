  source('packages.R')

# Spider Cues: Clean and Read

  Mon1 <- read.table("../Data/DrosophilaActivity_Spider_cues_Monitor1_May2016.txt")
  Mon2 <- read.table("../Data/DrosophilaActivity_Spider_cues_Monitor2_May2016.txt")

# Variable for monitor:
  Mon1$V43 <- 1
  Mon2$V43 <- 2

  col.names <- c("bin", "date", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4", "unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'vial17', 'vial18', 'vial19', 'vial20', 'vial21', 'vial22', 'vial23', 'vial24', 'vial25', 'vial26', 'vial27', 'vial28', 'vial29', 'vial30', 'vial31', 'vial32', 'monitor')

  colnames(Mon1) <- col.names
  colnames(Mon2) <- col.names

# Remove unknowns
  Mon1 <- Mon1[,-c(5:9)]
  Mon2 <- Mon2[,-c(5:9)]

# Remove unneeded vials (i.e vials 9 - 24)
  Mon1 <- Mon1[,-c(14:29)]
  Mon2 <- Mon2[,-c(14:29)]

#Change Day format
  Mon1$datetime <- as.POSIXct(paste(Mon1$date, Mon1$time), 
                              format="%m/%d/%y %H:%M:%S")
  Mon2$datetime <- as.POSIXct(paste(Mon2$date, Mon2$time), 
                            format="%m/%d/%y %H:%M:%S")

# Reshape to long
  Mon1_long <- gather(Mon1, Vial, Activity_counts, vial1:vial32, factor_key = FALSE)
  Mon2_long <- gather(Mon2, Vial, Activity_counts, vial1:vial32, factor_key = FALSE)

# To get minute
  Mon1_long$minute <- as.numeric(strftime(Mon1_long$datetime, format ="%M"))
  Mon2_long$minute <- as.numeric(strftime(Mon2_long$datetime, format ="%M"))

# To get hour
  Mon1_long$hour <- as.numeric(strftime(Mon1_long$datetime, format ="%H"))
  Mon2_long$hour <- as.numeric(strftime(Mon2_long$datetime, format ="%H"))

# To get day
  Mon1_long$day <- as.numeric(strftime(Mon1_long$datetime, format = "%d"))
  Mon2_long$day <- as.numeric(strftime(Mon2_long$datetime, format = "%d"))

  Mon1_long$Treatment <- ifelse(Mon1_long$Vial == "vial1", "Control", ifelse (Mon1_long$Vial == "vial2", "Control", ifelse(Mon1_long$Vial == "vial3", "Control", ifelse(Mon1_long$Vial == "vial4", "Control", ifelse(Mon1_long$Vial == "vial5", "Control", ifelse(Mon1_long$Vial == "vial6", "Control", ifelse(Mon1_long$Vial == "vial7", "Control", ifelse(Mon1_long$Vial == "vial8", "Control","Spider"))))))))

  Mon2_long$Treatment <- ifelse(Mon2_long$Vial == "vial1", "Spider", ifelse (Mon2_long$Vial == "vial2", "Spider", ifelse(Mon2_long$Vial == "vial3", "Spider", ifelse(Mon2_long$Vial == "vial4", "Spider", ifelse(Mon2_long$Vial == "vial5", "Spider", ifelse(Mon2_long$Vial == "vial6", "Spider", ifelse(Mon2_long$Vial == "vial7", "Spider", ifelse(Mon2_long$Vial == "vial8", "Spider","Control"))))))))

  Act_long <- rbind(Mon1_long, Mon2_long)

  Act_long$monitor <- as.factor(Act_long$monitor)
  Act_long$Treatment <- as.factor(Act_long$Treatment)
  Act_long$day <- as.factor(Act_long$day)
  Act_long$Vial <- as.factor(Act_long$Vial)

  day_act <- Act_long %>%
    group_by(Treatment, monitor, Vial, day) %>%
    summarise(mean_activity=mean(Activity_counts))

  act_hour <- Act_long %>%
    group_by(Treatment, Vial, monitor, day, hour) %>%
    summarise(activity_counts = sum(Activity_counts))

  act_hour$individual <- with(act_hour, interaction(Vial, monitor,
                                                    drop=FALSE))

  act_hour$light <- with(act_hour, 
                         ifelse(hour >= 10 & hour < 22, 
                                "light", 
                                "dark"))
  
  act_hour$Treatment <- as.factor(act_hour$Treatment)
  act_hour$light <- as.factor(act_hour$light)

  summary(act_hour$individual)  
  