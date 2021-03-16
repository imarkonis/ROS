# som with variables in summary table (important ones)
# A 4 x 4 SOM is chosen with no further classification (16 groups)

library(data.table) 
library(kohonen) 
library(ggplot2); library(fmsb)

ros <- readRDS('./data/ROS_data.RDs') 


#############################
### Preprocessing
#############################

count_ros <- 1:nrow(ros)

#Adding extra vars
ros$ID <- paste0('ros_', count_ros)
ros[, extr_events := ifelse(WL1_ex > 0 & Q1_ex > 0, T, F)] #extreme events
ros[, month := month(Rain_Start)]

#Shuffling
set.seed(1999)
(ros <- ros[sample(count_ros, replace = F), ])

#Variable selection - Removed the ones that were correlated, e.g., AveTHi
ros_subset <- ros[, .(ID,
                      EventPrec,
                      EventMelt,              #meltwater amount (estimated linearly from T, adjusted for catchment)
                      Q_coeff,                #Basic runoff coefficient = EventQ / EventPrec
                      SWE.ROS_first,          #snow water equivalent of ROS event
                      mean.Catchment.temp,    #the mean air temperature of the catchment
                      RainDuration,
                      Water.balance.EventQ,   #EventQ – EventPrec [mm]
                      Water.input,            #Total liquid water input [WI] in mm/hour – melt water [M] + rain water [R].  
                      EventQ, 
                      maxQ,                     #Maximum Q normalized per catchment  
                      Qmax_time_norm_II,        #when the Qmax was reached normalized
                      ROS.area.mean_II,          #Fraction of Runoff contributing area of ROS
                      Runoff_cont.area_mean,  #Fraction of Runoff contributing area
                      Snow.area_mean)]         #Fraction of catchment covered by snow   

ros_subset <- ros_subset[complete.cases(ros_subset), ] 

#Homogenization
vars_for_classification <- c('EventPrec', 'EventMelt', 'Q_coeff', 'SWE.ROS_first') 
ros_for_som <- as.matrix(apply(ros_subset[, -1], 2, scale)) # ID column should not be used in cluster analysis
ros_for_som <- ros_for_som[, vars_for_classification]

cor(ros_for_som) # To check for variables' correlation (< 0.4)

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
ros_subset <- merge(ros_subset, ros[, c('ID', 'Q_baseGroup')], by = 'ID')
save(ros_som, ros_subset, ros_cat, my_palette, n_var, n_nodes, ros, file = './results/som_4x4_4_vars.rdata')
write.csv(ros, file = './results/ros_som_4.csv')




