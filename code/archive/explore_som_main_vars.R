library(ggplot2); library(data.table); library(fmsb)
library(kohonen) 

#############################
### Exploration
#############################

#Part 1: we examine node properties

#Hierarchical clustering for further classification set to 16 -> groups = nodes  

load('./data/som_4x4_main_vars')

groups <- 16 # number of groups with similar properties | each node is a group

ros_som_hc <- cutree(hclust(dist(ros_som$codes[[1]])), groups) 
ros_group <- ros_som_hc[ros_som$unit.classif] #group id of each ros event

ros_subset$group <- as.factor(as.numeric(ros_group))
ros_subset$node <- as.factor(ros_som$unit.classif)
ros_subset[, n_ros_group := .N, by = group] #number of ros events per hclust group

pdf('./results/plots/01_nodes_main_vars_hc.pdf', width = 6, height = 4)
plot(ros_som, 
     type = "mapping", 
     main = "Cluster Map", 
     bgcol = my_palette(groups)[ros_som_hc])
add.cluster.boundaries(ros_som, ros_som_hc)
dev.off()

#Summary statistics for groups
to_plot_groups <- data.table(melt(ros_subset[, 2:(n_var + 1)], 
                                  id.vars = c('group'))) #tidy up

dummy <- ros_subset[, 2:(n_var + 1)]
group_means <- dummy[, lapply(.SD, mean, na.rm = T), by = group] 
group_means <- group_means[order(EventMelt), ]
group_means$order_id <- factor(1:nrow(group_means))
to_plot <- data.table(melt(group_means, id.vars = c('order_id', 'group'))) #tidy up
dummy <- unique(to_plot[, .(group, order_id)])
to_plot_groups <- dummy[to_plot_groups, on = c('group')]

g1 <- ggplot(to_plot_groups[variable != 'Q_group'], 
             aes(x = order_id, y = value, fill = group)) +  #ploting boxplots ordered by EventMelt means
  geom_boxplot() +
  scale_x_discrete(labels = group_ids) +
  facet_wrap(~variable, scales = "free") + 
  scale_fill_manual(values = my_palette(groups)) +
  theme(legend.position = "none") +
  theme_minimal()

ggsave('./results/plots/01_nodes_main_vars_boxplot.pdf', g1, 'pdf', 
       width = 30, height = 20, units = 'cm')

#Months with most events per node
my_palette <- colorRampPalette(c("#4C3F54", "#486824", "#F2C057", "#D13525"))
ros_months <- ros_subset[ros[, .(ID, 
                                 month = as.factor(month))], 
                         on = 'ID']
group_months <- ros_months[, .N, .(group, month)]
group_months <- group_months[complete.cases(group_months)]
most_events_month <- group_months[group_months[, .I[N == max(N)], by=group]$V1]
most_events_month <- most_events_month[order(group)]

# Node characteristics with radar chart
group_var_range <- apply(group_means, 2, function(x) range(as.numeric(x)))
n_ros_group <- unique(ros_subset[, .(group, n_ros_group)])
n_ros_group <- n_ros_group[order(group), ]

op <- par(mar = c(1, 1, 1, 1), 
          mfrow = c(4, 4))

for(group_id in 1:groups){
  group_for_radar <- rbind(group_var_range[2:1, ], group_means[group == group_id, ])[, 2:n_var]
  
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
dev.copy(pdf, './results/plots/01_nodes_main_vars_radar.pdf')
dev.off()

#Months per node
group_months <- group_months[unique(to_plot_groups[, .(group, order_id)]), on = 'group']
to_plot <- group_months[, perc := N / sum(N), group]

g2 <- ggplot(to_plot, aes(x = "", y = perc, fill = as.factor(month))) +
  geom_bar(stat = 'identity') +
  facet_wrap(~group) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = my_palette(8)) +
  labs(fill = "Month") +
  theme(legend.position = "none") +
  theme_void()

ggsave('./results/plots/01_nodes_month.pdf', g2, 'pdf', 
       width = 30, height = 20, units = 'cm')

#Runoff groups vs SOM nodes
to_plot <- ros_subset[, .N, .(group, Q_group)]
to_plot <- to_plot[complete.cases(to_plot)]
to_plot <- to_plot[, perc := N / sum(N), group]

g3 <- ggplot(to_plot, aes(x = "", y = perc, fill = as.factor(Q_group))) +
  geom_bar(stat = 'identity') +
  facet_wrap(~group) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = my_palette(6)[4:1]) +
  theme(legend.position = "none") +
  guides(fill=guide_legend(title="Q group")) +
  theme_void()

ggsave('./results/plots/01_nodes_Qgroup.pdf', g3, 'pdf', 
       width = 30, height = 20, units = 'cm')

#Climate properties
climate_som <- readRDS('./data/climate_groups.rds')
ros_subset <- ros_subset[climate_som, on = 'ID']

to_plot <- ros_subset[, .N, .(group, cl_group_label)]
to_plot <- to_plot[complete.cases(to_plot)]
to_plot <- to_plot[, perc := N / sum(N), group]

g4 <- ggplot(to_plot, aes(x = "", y = perc, fill = as.factor(cl_group_label))) +
  geom_bar(stat = 'identity') +
  facet_wrap(~group) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = my_palette(6)[6:1]) +
  theme(legend.position = "none") +
  guides(fill=guide_legend(title = "Climate group")) +
  theme_void()

ggsave('./results/plots/01_nodes_climate.pdf', g4, 'pdf', 
       width = 30, height = 20, units = 'cm')

############################################

#Part 2 we examine group properties



## THE END ##

## Rest of code is not used - Keep it for future experiments

#Hierarchical clustering for further classification set to 4 -> groups 

load('./data/som_4x4_main_vars')

groups <- 6 # number of groups with similar properties 

ros_som_hc <- cutree(hclust(dist(ros_som$codes[[1]])), groups) 

pdf('./results/plots/02_groups_main_vars_hc.pdf', width = 6, height = 4)
plot(ros_som, 
     type = "mapping", 
     main = "Cluster Map", 
     bgcol = my_palette(groups)[ros_som_hc])
add.cluster.boundaries(ros_som, ros_som_hc)
dev.off()

ros_group <- ros_som_hc[ros_som$unit.classif] #group id of each ros event
ros_subset$group <- as.factor(as.numeric(ros_group))
ros_subset$node <- as.factor(ros_som$unit.classif)

#Summary statistics for groups
ros_subset[, n_ros_group := .N, by = group] #number of ros events per hclust group

to_plot_groups <- data.table(melt(ros_subset[, 2:(n_var + 1)], 
                                  id.vars = c('group'))) #tidy up

dummy <- ros_subset[, 2:(n_var + 1)]

group_means <- dummy[, lapply(.SD, mean, na.rm = T), by = group] 

group_means <- group_means[order(EventMelt), ]
group_means$order_id <- factor(1:nrow(group_means))
to_plot <- data.table(melt(group_means, id.vars = c('order_id', 'group'))) #tidy up
dummy <- unique(to_plot[, .(group, order_id)])
to_plot_groups <- dummy[to_plot_groups, on = c('group')]

g1 <- ggplot(to_plot_groups[variable != 'Q_group'], 
             aes(x = order_id, y = value, fill = group)) +  #ploting boxplots ordered by EventMelt means
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") + 
  theme(legend.position = "none") +
  scale_fill_manual(values = my_palette(groups)) +
  theme_minimal()

ggsave('./results/plots/02_groups_main_vars_boxplot.pdf', g1, 'pdf', 
       width = 30, height = 20, units = 'cm')

#Months with most events per group
my_palette <- colorRampPalette(c("#4C3F54", "#486824", "#F2C057", "#D13525"))
ros_months <- ros_subset[ros[, .(ID, 
                                 month = as.factor(month))], 
                         on = 'ID']
group_months <- ros_months[, .N, .(group, month)]
group_months <- group_months[complete.cases(group_months)]
most_events_month <- group_months[group_months[, .I[N == max(N)], by=group]$V1]
most_events_month <- most_events_month[order(group)]

# Group characteristics with radar chart
group_var_range <- apply(group_means, 2, function(x) range(as.numeric(x)))
n_ros_group <- unique(ros_subset[, .(group, n_ros_group)])
n_ros_group <- n_ros_group[order(group), ]

op <- par(mar = c(1, 1, 1, 1), 
          mfrow = c(3, 2))

for(group_id in 1:groups){
  group_for_radar <- rbind(group_var_range[2:1, ], group_means[group == group_id, ])[, 2:n_var]
  
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

dev.copy(pdf, './results/plots/02_groups_main_vars_radar.pdf')
dev.off()

#Months per group
group_months <- group_months[unique(to_plot_groups[, .(group, order_id)]), on = 'group']
to_plot <- group_months[, perc := N / sum(N), group]

g2 <- ggplot(to_plot, aes(x = "", y = perc, fill = as.factor(month))) +
  geom_bar(stat = 'identity') +
  facet_wrap(~group) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = my_palette(8)) +
  labs(fill = "Month") +
  theme(legend.position = "none") +
  theme_void()

ggsave('./results/plots/02_groups_month.pdf', g2, 'pdf', 
       width = 30, height = 20, units = 'cm')

#Runoff groups vs SOM groups
to_plot <- ros_subset[, .N, .(group, Q_group)]
to_plot <- to_plot[complete.cases(to_plot)]
to_plot <- to_plot[, perc := N / sum(N), group]

g3 <- ggplot(to_plot, aes(x = "", y = perc, fill = as.factor(Q_group))) +
  geom_bar(stat = 'identity') +
  facet_wrap(~group) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = my_palette(6)[4:1]) +
  theme(legend.position = "none") +
  guides(fill=guide_legend(title = "Q group")) +
  theme_void()

ggsave('./results/plots/02_groups_Qgroup.pdf', g3, 'pdf', 
       width = 30, height = 20, units = 'cm')

#Climate properties
climate_som <- readRDS('./data/climate_groups.rds')
ros_subset <- ros_subset[climate_som, on = 'ID']

to_plot <- ros_subset[, .N, .(group, cl_group_label)]
to_plot <- to_plot[complete.cases(to_plot)]
to_plot <- to_plot[, perc := N / sum(N), group]

g4 <- ggplot(to_plot, aes(x = "", y = perc, fill = as.factor(cl_group_label))) +
  geom_bar(stat = 'identity') +
  facet_wrap(~group) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = my_palette(6)[6:1]) +
  theme(legend.position = "none") +
  guides(fill=guide_legend(title = "Climate group")) +
  theme_void()

ggsave('./results/plots/02_groups_climate.pdf', g4, 'pdf', 
       width = 30, height = 20, units = 'cm')


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
