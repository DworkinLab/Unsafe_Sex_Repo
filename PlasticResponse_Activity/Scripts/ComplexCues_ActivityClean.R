source('packages.R')

## Experiment 3 (complex cues): Clean and Read

  Exp3_Mon1 <- read.table("../Data/Exp3_Monitor_1.txt")
  Exp3_Mon2 <- read.table("../Data/Exp3_Monitor_2.txt")

# Monitor variable

  Exp3_Mon1$monitor <- 1
  Exp3_Mon2$monitor <- 2

# Remove unneeded columns:

  Exp3_Mon1 <- Exp3_Mon1[,-c(5:9)]
  Exp3_Mon2 <- Exp3_Mon2[,-c(5:9)]

  Exp3_Mon1 <- Exp3_Mon1[,-c(2)]
  Exp3_Mon2 <- Exp3_Mon2[,-c(2)]

# DateTime variable
  Exp3_Mon1$num <- c(1:1560)
  Exp3_Mon2$num <- c(1:1560)

# Reform Date:
  Exp3_Mon1$datetime <- as.POSIXct(paste(Exp3_Mon1$X, Exp3_Mon1$X.2), format="%Y-%m-%d %H:%M:%S")
  Exp3_Mon2$datetime <- as.POSIXct(paste(Exp3_Mon2$X, Exp3_Mon2$X.2), format="%Y-%m-%d %H:%M:%S")

# To get minute
  Exp3_Mon1$minute <- as.numeric(strftime(Exp3_Mon1$datetime, format ="%M"))
  Exp3_Mon2$minute <- as.numeric(strftime(Exp3_Mon2$datetime, format ="%M"))

# To get hour
  Exp3_Mon1$hour <- as.numeric(strftime(Exp3_Mon1$datetime, format ="%H"))
  Exp3_Mon2$hour <- as.numeric(strftime(Exp3_Mon2$datetime, format ="%H"))

# To get day
  Exp3_Mon1$day <- as.numeric(strftime(Exp3_Mon1$datetime, format = "%d"))
  Exp3_Mon2$day <- as.numeric(strftime(Exp3_Mon2$datetime, format = "%d"))

#Column Names:
# Make F or C == Vial1_fly and vial2_cricket. etc.
# Mon1: date, time, signal,Light, SF SF.1 SF.2 SF.3 SF.4 SF.5 SF.6 SF.7 SC SC.1 SC.2 SC.3 SC.4 SC.5 SC.6 SC.7 F,  F.1 F.2 F.3 F.4 F.5 F.6 F.7 C C.1 C.2 C.3 C.4 C.5 monitor            datetime minute hour day
  colnames(Exp3_Mon1) <- c("date", "time", "signal", "lightON",'vial1_SF', 'vial2_SF', 'vial3_SF', 'vial4_SF', 'vial5_SF', 'vial6_SF', 'vial7_SF', 'vial8_SF', 'vial9_SC', 'vial10_SC', 'vial11_SC', 'vial12_SC', 'vial13_SC', 'vial14_SC', 'vial15_SC', 'vial16_SC','vial17_F','vial18_F','vial19_F','vial20_F','vial21_F','vial22_F','vial23_F','vial24_F','vial25_C','vial26_C','vial27_C','vial28_C','vial29_C','vial30_C', 'monitor','num', 'datetime', 'minute', 'hour', 'day')

#Mon2:
##  date time signal Light  F F.1 F.2 F.3 F.4 F.5 F.6 C C.1 C.2 C.3 C.4 SF SF.1 SF.2 SF.3 SF.4 SF.5 SF.6,  SC SC.1 SC.2 SC.3 SC.4 SC.5 SC.6 SC.7 monitor datetime minute hour day 

  colnames(Exp3_Mon2) <- c("date", "time", "signal", "lightON",'vial1_F', 'vial2_F', 'vial3_F', 'vial4_F', 'vial5_F', 'vial6_F', 'vial7_F', 'vial8_C', 'vial9_C', 'vial10_C', 'vial11_C', 'vial12_C', 'vial13_SF', 'vial14_SF', 'vial15_SF', 'vial16_SF','vial17_SF','vial18_SF','vial19_SF','vial20_SC','vial21_SC','vial22_SC','vial23_SC','vial24_SC','vial25_SC','vial26_SC','vial27_SC', 'monitor','num','datetime', 'minute', 'hour', 'day')

## Make long:
  Exp3_Mon1_long <- gather(Exp3_Mon1, Vial, activity_counts, vial1_SF:vial30_C, factor_key = FALSE)
  Exp3_Mon2_long <- gather(Exp3_Mon2, Vial, activity_counts, vial1_F:vial27_SC, factor_key = FALSE)

## Split vial and treatment (SF, F, SC, and C (or need to change how this is done...))

  Exp3_Mon1_long <- Exp3_Mon1_long %>%
    separate(Vial, c("Vial", "Treatment"), "_")
  Exp3_Mon2_long <- Exp3_Mon2_long %>%
    separate(Vial, c("Vial", "Treatment"), "_")

# Combine into one data set:
  Exp3_long <- rbind(Exp3_Mon1_long, Exp3_Mon2_long)
  Exp3_long$monitor <- as.factor(Exp3_long$monitor)
  Exp3_long$Treatment <- as.factor(Exp3_long$Treatment)
  Exp3_long$day <- as.factor(Exp3_long$day)
  Exp3_long$Vial <- as.factor(Exp3_long$Vial)

#By hour:

  Exp3_hour <- Exp3_long %>%
    group_by(Treatment, Vial, monitor, day, hour, hour) %>%
    summarise(activity_counts=sum(activity_counts))

  Exp3_hour$individual <- with(Exp3_hour, interaction(Vial, monitor, drop=FALSE))


  Exp3_hour$hour <- as.numeric(Exp3_hour$hour)
  Exp3_hour$light <- with(Exp3_hour, ifelse(hour >= 10 & hour < 22, "light", "dark"))

  Exp3_hour$Treatment <- as.factor(Exp3_hour$Treatment)
  Exp3_hour$light <- as.factor(Exp3_hour$light)
  
  #Change Treatment:
  summary(Exp3_hour$Treatment)
  Exp3_hour <- within(Exp3_hour, {Cues = ifelse (Treatment == "C", "Cricket", ifelse(Treatment == "F", "Fly", ifelse(Treatment == "SC", "Spider Fed Crickets", "Spider Fed Flies")))})
  
  Exp3_hour$Cues <- as.factor(Exp3_hour$Cues)
  
  summary(Exp3_hour$individual)
  