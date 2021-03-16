# Reads csv and saves in .RDS
library(data.table)

temp <- data.table(read.csv("./data/raw/ROS_data_reSubmission_Scenario.5.csv"))
temp[, Rain_Start := as.POSIXct(Rain_Start, format = "%m/%d/%Y %H:%M")]
colnames(temp)[1] <- 'Event.Nr'

saveRDS(temp, "./data/ROS_data.RDs")

