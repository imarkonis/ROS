#Make summary table

library(data.table) #data wrangling

ros <- readRDS('./data/ROS_data.RDs') 
ros <- readRDS('Catchment ROS/Results/ROS_events/Summaries/ROS_data.RDs') #Alternative path

ros_subset <- ros[, .(Rain.fraction_mean,     #Fraction of the water input consists by rainwater during the first hour of the event.  
                      Melt.fraction_mean,     #Fraction of the water input consists by melt water during the entire event.  
                      EventPrec,              #precipitation amount   
                      Q_coeff,                #Basic runoff coefficient = EventQ / EventPrec
                      Runoff_cont.area_mean,  #Fraction of Runoff contributing area
                      ROS.area_mean,          #Fraction of Runoff contributing area of ROS
                      SWE.ROS.mean,           #snow water equivalent of ROS event
                      Water.input,            #Total liquid water input [WI] in mm/hour – melt water [M] + rain water [R].  
                      AveTLo,                 #the air temperature at lowest point of catchment
                      AveTLo_ROS,             ##the air temperature at lowest point of catchment during ROS
                      API_start,              #catchment wetness
                      Qmax_time_norm,         #when the Qmax was reached normalized
                      Water.balance.EventQ,   #EventQ – EventPrec [mm]
                      EventMelt,              #meltwater amount
                      Q_group)]

my_funcs = function(x) list(mean = round(mean(x, na.rm = T), 2), 
                            median = round(median(x, na.rm = T), 2),
                            sd = round(sd(x, na.rm = T), 2),
                            min = round(min(x, na.rm = T), 2),
                            max <- round(max(x, na.rm = T), 2))

my_summary <- function(x){
  x_tidy <- melt(x, id.vars = "Q_group")
  x_tidy[, mean(value, na.rm = T), .(variable, Q_group)]
  temp <- x_tidy[, sapply(.SD, my_funcs), .SDcols = 'value', .(variable, Q_group)]
  setorder(temp, "variable", 'Q_group')
  colnames(temp)[3:7] <- c('mean', 'median', 'sd', 'min', 'max')
  return(temp)
}