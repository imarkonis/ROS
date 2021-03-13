# Experiment of classification according only in climatic variables. 
# In this way, groups of similar climatic conditions are created.

library(data.table) #data wrangling
library(kohonen) #som
library(ggplot2); library(fmsb) #graphics

ros <- readRDS('./data/ROS_data.RDs') 
ros <- readRDS('Catchment ROS/Results/ROS_events/Summaries/ROS_data.RDs') #Alternative path

set.seed(999)

#############################
### Preprocessing
#############################

#Adding extra vars
count_ros <- 1:nrow(ros)
ros$ID <- paste0('ros_', count_ros)
ros[, extr_events := ifelse(WL1_ex > 0 & Q1_ex > 0, T, F)] #extreme events
ros[, month := month(Rain_Start)]

#Shuffling
ros <- ros[sample(count_ros, replace = F), ]

#Filtering out outliers
ros <- ros[EventPrec >= 5 & 
             Event_length <= 144 & 
             Qmax_time <= 48 & 
             EventQ >= 0]     

#Variable selection - Removed the ones that were correlated, e.g., AveTHi

ros_subset <- ros[, .(ID,
                      Qmax_time,
                      maxQ,          
                      EventQ,         
                      minQ)]      

classif_var <- colnames(ros_subset)
n_var <- ncol(ros_subset)

#NA removal
ros_subset <- ros_subset[complete.cases(ros_subset), ] #REDUCES values from 512 to 507

#Homogenization
ros_for_som <- as.matrix(apply(ros_subset[, -1], 2, scale)) # ID column should not be used in cluster analysis

#############################
### Clustering
#############################

#Colors for ploting
my_palette <- colorRampPalette(c("#4C3F54", "#D13525", "#F2C057", "#486824"))

#SOM algorithm
som_dim <- 4
n_nodes <- som_dim^2
ros_som <- supersom(ros_for_som, somgrid(som_dim, som_dim, "hexagonal"),
                    rlen = 10000,
                    maxNA.fraction = .5)

save(ros_som, ros_subset, my_palette, n_var, n_nodes, ros, file = './data/som_4x4_hydrog.Rdata')

#Hierarchical clustering for further classification
load('./data/som_4x4_hydrog.Rdata')
groups <- 16 # number of groups with similar properties

ros_som_hc <- cutree(hclust(dist(ros_som$codes[[1]])), groups) 
plot(ros_som, 
     type = "mapping", 
     main = "Cluster Map", 
     bgcol = my_palette(groups)[ros_som_hc])
add.cluster.boundaries(ros_som, ros_som_hc)

ros_group <- ros_som_hc[ros_som$unit.classif] #group id of each ros event

ros_subset$hyd_group <- as.factor(as.numeric(ros_group))
ros_subset$node <- as.factor(ros_som$unit.classif)

#############################
### Exploration
#############################

#Summary statistics for groups
ros_subset[, n_ros_group := .N, by = hyd_group] #number of ros events per hclust group

to_plot_groups <- data.table(melt(ros_subset[n_ros_group > 35, 2:(n_var + 1)], 
                                  id.vars = c('hyd_group'))) #tidy up
dummy <- ros_subset[, 2:(n_var + 1)]
group_means <- dummy[, lapply(.SD, mean, na.rm = T), by = hyd_group] 

group_means <- group_means[order(EventPrec), ]
group_means$order_id <- factor(1:nrow(group_means))
to_plot <- data.table(melt(group_means, id.vars = c('order_id', 'hyd_group'))) #tidy up
group_means <- group_means[order(hyd_group), ] 
dummy <- unique(to_plot[, .(hyd_group, order_id)])
to_plot_groups <- dummy[to_plot_groups, on = c('hyd_group')]

ggplot(to_plot_groups, aes(x = order_id, y = value, fill = hyd_group)) +  #ploting boxplots ordered by EventMelt means
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") + 
  theme(legend.position = "none") +
  scale_fill_manual(values = my_palette(groups)) +
  theme_minimal()

#Months with most events per group
ros_months <- ros_subset[ros[, .(ID, 
                                 month = as.factor(month))], 
                         on = 'ID']
group_months <- ros_months[, .N, .(hyd_group, month)]
group_months <- group_months[complete.cases(group_months)]
most_events_month <- group_months[group_months[, .I[N == max(N)], by = hyd_group]$V1]
most_events_month <- most_events_month[order(hyd_group)]

# Group characteristics with radar chart
group_var_range <- apply(group_means, 2, function(x) range(as.numeric(x)))
n_ros_group <- unique(ros_subset[, .(hyd_group, n_ros_group)])
n_ros_group <- n_ros_group[order(hyd_group), ]

ros_subset[, hyd_group_label := hyd_group]

ros_subset[, hyd_group_label := factor(hyd_group_label, 
                                       labels = c("LQ", "LQ HmaxT", "LQ HmaxQ", 
                                                  "MQ", "HQ", "LQ HmaxQ"))]
op <- par(mar = c(1, 1, 1, 1), 
          mfrow = c(3, 2))

for(group_id in 1:groups){
  group_for_radar <- rbind(group_var_range[2:1, ], group_means[hyd_group == group_id, ])[, 2:n_var]
  
  radarchart(group_for_radar, 
             axistype = 4,
             title = paste0("Group ", group_id, " (N: ", n_ros_group[group_id, 2], 
                            ", month: ",  most_events_month[group_id]$month, ")"), 
             pcol = rgb(0.2, 0.5, 0.5, 0.9), 
             pfcol = rgb(0.2, 0.5, 0.5, 0.5), 
             plwd = 4, 
             vlcex = 1,
             cglcol = "grey", 
             cglty = 1, 
             axislabcol = "grey", 
             cglwd = 1.2)
}
dev.copy(pdf, './results/plots/00_radar_hydrog_groups.pdf')
dev.off()

#Groups per month
ggplot(group_months, aes(x = as.factor(hyd_group), y = N, fill = as.factor(hyd_group))) +
  geom_bar(stat = 'identity') +
  facet_wrap(~month) +
  coord_polar() +
  scale_fill_manual(values = my_palette(12)) +
  theme(legend.position = "none") +
  theme_minimal()

saveRDS(ros_subset, './data/hydrog_groups.rds')

#Runoff groups
q_groups <- ros[, .(ID, Q_group)]
ros_subset <- ros_subset[q_groups, on = 'ID']
ros_groups <- ros_subset[, .(hyd_group, Q_group)]

to_plot <- ros_groups[, .N, .(hyd_group, Q_group)]

ggplot(to_plot, aes(x = as.factor(hyd_group), y = N, fill = as.factor(hyd_group))) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Q_group) +
  coord_polar() +
  scale_fill_manual(values = my_palette(12)) +
  theme(legend.position = "none") +
  theme_minimal()

colnames(ros_subset)[7] <- 'hydrog_node'

saveRDS(ros_subset[, .(ID, hyd_group, hydrog_node)], "./data/hydrog_SOM.rds")


## THE END ##

## Rest of code is not used - Keep it for future experiments

#Checking nodes
ros_subset[, n_ros_node := .N, by = node] #number of ros events per som node

to_plot_nodes <- data.table(melt(ros_subset[, 2:(n_var + 2)], 
                                 id.vars = c('node', 'group'))) #tidy up

ggplot(to_plot_nodes, aes(x = node, y = value, fill = node)) +
  geom_boxplot() +
  stat_summary(aes(color = value), fun.y = mean, geom = "point", shape = 20, size = 2) +
  facet_wrap(~variable, scales = "free") + 
  theme(legend.position = "none") +
  scale_fill_manual(values = my_palette(n_nodes)) +
  theme_minimal()

#Summary statistics for nodes
dummy <- ros_subset[, 2:(n_var + 2)]
dummy[, group := NULL]
node_means <- dummy[, lapply(.SD, mean, na.rm = T), by = node] 

node_means <- node_means[order(EventMelt), ]
node_means$order_id <- 1:nrow(node_means)
to_plot <- data.table(melt(node_means, id.vars = c('order_id', 'node'))) #tidy up

ggplot(to_plot, aes(x = order_id, y = value, col = variable)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free") + 
  theme(legend.position = "none") +
  scale_color_manual(values = my_palette(n_var)) +
  theme_minimal()

#Checking groups with properties that were not used in classification, for instance catchments
ros_catchments <- ros_subset[ros[, .(ID, 
                                     where = factor(where), 
                                     hora = factor(hora))], 
                             on = 'ID']

ros_catchments[, no_events := .N, where]

dummy <- ros_subset[, 1:(n_var + 1)]
ros_Q <- dummy[ros[, .(ID, Total)], on = 'ID']     #Total volume of runout water per RuE event in mm

#keeping the major groups
ros_cat_group <- ros_catchments[no_events > 10, .N, .(group, where)] 
ros_cat_group <- ros_cat_group[N > 10]

ggplot(ros_cat_group, aes(x = where, y = N, fill = group)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = my_palette(4)) +
  theme_minimal()

to_plot <- data.table(melt(ros_Q, 
                           id.vars = c('group', 'ID'))) #tidy up

to_plot <- to_plot[complete.cases(to_plot)]
ggplot(to_plot, aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") + 
  theme(legend.position = "none") +
  scale_fill_manual(values = my_palette(groups)) +
  theme_minimal()

#checking these high TotalQ/low TimeToMaxPeak group 10

ros_catchments[group == 10, table(where)] #Checking where the appear
ros_merger <- ros_subset[, .(ID, group)]
ros_group_10 <- ros_merger[ros, on = 'ID']
ros_group_10 <- ros_group_10[group == 10] 




