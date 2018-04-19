## Evolved Population Read and Clean Data

  source('packages.R')
# Read in data:
  DAM1 <- read.table("PredationActivityDAM1_May2015_RD.txt")
  DAM2 <- read.table("PredationActivityDAM2_May2015_RD.txt")

# Monitor variable (Dam 1 or Dam 2)
  DAM2$v45 <- 2
  DAM1$v45 <- 1
  DAM_data <- rbind(DAM1, DAM2)

# Sample information is in a seperate csv
  sample_info <- read.csv("Predation_ActivityMetaData_May2015_RD.csv")

# Change Column Names:
  colnames(DAM_data) <- c("bin", "day", "month", "year", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4","unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'vial17','vial18', 'vial19', 'vial20', 'vial21', 'vial22', 'vial23', 'vial24', 'vial25', 'vial26','vial27', 'vial28', 'vial29', 'vial30', 'vial31', 'vial32', 'monitor')

# Remove removable columns
  DAM_data2 <- DAM_data[, -c(7:11, 37:44)]
  
# Setting time to proper format
  DAM_data2$datetime <- as.POSIXct( strptime( paste( DAM_data2$day,DAM_data2$month,DAM_data2$year, DAM_data2$time), 
                                            "%d %B %y %H:%M:%S") )

    DAM_data2$monitor <- as.factor(DAM_data2$monitor)

  DAM_long <- gather(DAM_data2, Vial, activity_counts, vial1:vial24, factor_key = FALSE)

# munge the sample_info to have consistent naming conventions with DAM.
  sample_info$vial <- paste("vial", sample_info$Location, sep="")
  colnames(sample_info)[3] <- "day"
  sample_info$day.vial <- interaction(sample_info$day, sample_info$vial) # day is the start day.

  start_day <- c(19,21,23,26,28) # The starting days of the experiment

# Make the start days match up for the experiments
  DAM_long$start_day <- ifelse((DAM_long$day %in% start_day), 
                             DAM_long$day, 
                             (DAM_long$day-1))

  DAM_long$day.vial <- interaction(DAM_long$start_day, DAM_long$Vial)

# Merge all info together
  DAM_long2 <- merge(DAM_long, sample_info, by="day.vial")

#Remove redundant info
  DAM_long2 <- subset(DAM_long2, select = 
                      -c(day.x, month, year, time, 
                         signal, Vial, Location, day.y) )

# To get minute
DAM_long2$minute <- as.numeric(strftime(DAM_long2$datetime, format ="%M"))
# To get hour
DAM_long2$hour <- as.numeric(strftime(DAM_long2$datetime, format ="%H"))

# To create a variable for each individual
DAM_long2$individual <- with(DAM_long2, interaction(day.vial, monitor, drop=FALSE))

dat.hourly <- DAM_long2 %>%
  group_by(vial, monitor, start_day, hour, Trt, Population) %>%
  summarise(Hourly_activity=sum(activity_counts))

dat.hourly$individual <- with(dat.hourly, interaction(start_day, vial, monitor, drop=FALSE))

# Light schedule: on between 10am and 10 pm
dat.hourly$light <- with(dat.hourly, ifelse(hour >= 10 & hour < 22, "light", "dark"))

dat.hourly <- within(dat.hourly, { 
  Predation = ifelse (Trt == "C", "Control", ifelse(Trt == "S", "Spider", "Mantids"))})

dat.hourly$Predation <- as.factor(dat.hourly$Predation)
dat.hourly$light <- as.factor(dat.hourly$light)
dat.hourly$Treatment <- dat.hourly$Predation

