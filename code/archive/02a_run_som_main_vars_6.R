# som with variables in summary table (important ones)
# A 4 x 4 SOM is chosen with no further classification (16 groups)

library(data.table) #data wrangling
library(kohonen) #som
library(ggplot2); library(fmsb) #graphics

ros <- readRDS('./data/ROS_data.RDs') 
ros <- readRDS('Catchment ROS/Results/ROS_events/Summaries/ROS_data.RDs') #Alternative path

#Colors for ploting
my_palette <- colorRampPalette(c("#4C3F54", "#D13525", "#F2C057", "#486824"))

#############################
### Preprocessing
#############################

count_ros <- 1:nrow(ros)

#Adding extra vars
ros$ID <- paste0('ros_', count_ros)

ros[, extr_events := ifelse(WL1_ex > 0 & Q1_ex > 0, T, F)] #extreme events

ros[, month := month(Rain_Start)]

#Shuffling
ros <- ros[sample(count_ros, replace = F), ]

#Variable selection - Removed the ones that were correlated, e.g., AveTHi
ros_subset <- ros[, .(ID,
                      AveTLo,                 #the air temperature at lowest point of catchment
                      EventPrec,              #precipitation amount   
                      Qmax_time_norm,         #when the Qmax was reached normalized
                      SWE.ROS.mean,           #snow water equivalent of ROS event
                      API_start,              #catchment wetness
                      Q_coeff,                #Basic runoff coefficient = EventQ / EventPrec
                      EventMelt,              #meltwater amount
                      RainDuration,
                      Rain.fraction_mean,     #Fraction of the water input consists by rainwater during the first hour of the event.  
                      Water.balance.EventQ,   #EventQ – EventPrec [mm]
                      Water.input,            #Total liquid water input [WI] in mm/hour – melt water [M] + rain water [R].  
                      maxQ,                   #Maximum Q normalized per catchment  
                      ROS.area_mean,          #Fraction of Runoff contributing area of ROS
                      Runoff_cont.area_mean,  #Fraction of Runoff contributing area
                      Snow.area_mean,         #Fraction of catchment covered by snow   
                      Q_group)]

#NA removal
ros_subset <- ros_subset[complete.cases(ros_subset), ] #REDUCES values from 423 to 420

#Homogenization
vars_for_classification <- 1:6 #The 1st one is ID and is not used, but needed here
n_var <- length(vars_for_classification)
ros_for_som <- as.matrix(apply(ros_subset[, -1], 2, scale)) # ID column should not be used in cluster analysis
ros_for_som <- ros_for_som[, vars_for_classification]

cor(ros_for_som) # To check for variables correlation

#Catchments

ros_cat <- ros[, .(ID = paste0("ros_", Event.Nr), catchment = where, mountain = hora)]

#############################
### Clustering
#############################

#SOM algorithm
som_dim <- 4
n_nodes <- som_dim^2
ros_som <- supersom(ros_for_som, somgrid(som_dim, som_dim, "hexagonal"),
                 rlen = 10000, 
                 maxNA.fraction = .5)

ros_subset$node <- as.factor(ros_som$unit.classif)
ros <- merge(ros, ros_subset[, c('ID', 'node')], by = 'ID')

save(ros_som, ros_subset, ros_cat, my_palette, n_var, n_nodes, ros, file = './data/som_4x4_main_6_vars_rev.rdata')
save(ros_som, ros_subset, ros_cat, my_palette, n_var, n_nodes, ros, file = 'som_4x4_main_6_vars_rev.rdata') #Alternative path
write.csv(ros, file = 'ros_som_6_original.csv')




