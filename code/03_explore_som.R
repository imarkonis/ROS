#EDA for soms

library(ggplot2); library(data.table); library(fmsb)
library(kohonen) 

load('./results/som_4x4_4_vars.rdata') 
my_palette <- colorRampPalette(c("#4C3F54", "#486824", "#F2C057", "#D13525"))
n_groups <- 16 # number of groups with similar properties | each node is a group

ros_subset[, n_ros_group := .N, by = node] #number of ros events per hclust group

#Summary statistics for groups
colnames(ros_subset)
ros_events <- ros_subset[, 2:16] #change this to match number of variables

group_medians <- ros_events[, lapply(.SD, median, na.rm = T), by = node] 
group_medians <- group_medians[order(EventPrec, decreasing = T), ]              #sets main variable for box plot order
group_medians$P_group <- factor(1:n_groups) #Adding group ids according to precipitation order

group_node <- group_medians[, .(node, P_group)]
ros_subset <- group_node[ros_subset, on = 'node']

group_types <- ros_subset[, .(Q_group_size = .N), .(P_group, Q_baseGroup)]
most_events_type <- group_types[group_types[, .I[Q_group_size == max(Q_group_size)], by = P_group]$V1]
most_events_type <- most_events_type[order(P_group)]
most_events_type <- most_events_type[-4]
write.csv(ros_subset[, -1], file = './results/ros_som_4_groups.csv')
write.csv(group_medians, file = './results/ros_som_4_group_medians.csv')
          
#Boxplot
to_plot_groups <- data.table(melt(ros_subset, id.vars = c('ID', 'node', 'P_group', 'Q_baseGroup'))) #tidy up
vars_to_plot <- unique(to_plot_groups$variable)[c(1:5, 10, 13, 14)]

to_plot_groups <- to_plot_groups[variable %in% vars_to_plot]
to_plot_groups$variable <- relevel(to_plot_groups$variable, "EventPrec")
to_plot_groups$variable <- droplevels(to_plot_groups$variable)
levels(to_plot_groups$variable) <- 
  c("P event", "Snowmelt", "C", "SWE", "T mean", 
    "Q max", "RCA", "SCA")

aa <- most_events_type[, .(P_group, Q_baseGroup)]
aa <- aa[-4]
to_plot_groups[, Q_baseGroup := NULL]
to_plot_groups <- aa[to_plot_groups, on = 'P_group']
to_plot_groups$Q_baseGroup <- factor(to_plot_groups$Q_baseGroup)

g1 <- ggplot(to_plot_groups, 
             aes(x = P_group, y = value, fill = Q_baseGroup)) +  #ploting boxplots ordered by EventMelt medians
  geom_boxplot() +
  scale_x_discrete(labels = to_plot_groups) +
  facet_wrap(~variable, scales = "free", ncol = 2) + 
  scale_fill_manual(values = my_palette(4)) +
  xlab("Group") +
  ylab("Variable values") +
  theme_minimal() + 
  theme(legend.direction = "horizontal", legend.position = "bottom",
        panel.spacing = unit(0.8, "cm"), 
        strip.text.x = element_text(size = 12)) + 
  guides(fill = guide_legend(title = "Main runoff type"))
        
ggsave('./results/plots/nodes_4_vars_boxplot.png', g1, 'png', 
               width = 15, height = 17, units = 'cm')
        
        #Months with most events per node
ros_months <- ros_subset[ros[, .(ID, 
                                 month = as.factor(month))], 
                         on = 'ID']

group_months <- ros_months[, .N, .(P_group, month)]
group_months <- group_months[complete.cases(group_months)]

most_events_month <- group_months[group_months[, .I[N == max(N)], by = P_group]$V1]
most_events_month <- most_events_month[order(P_group)]

# Node characteristics with radar chart
colnames(group_medians)[2:(n_var + 1)] <- 
  c("P", "S/melt", "C", "SWE")

group_var_range <- apply(group_medians, 2, function(x) range(as.numeric(x)))
n_ros_group <- unique(ros_subset[, .(P_group, n_ros_group)])
n_ros_group <- n_ros_group[order(P_group), ]

op <- par(mar = c(1, 1, 1, 1), 
          mfrow = c(4, 4))


for(group_id in 1:n_groups){
  group_for_radar <- rbind(group_var_range[2:1, ], 
                           group_medians[P_group == group_id, ])[, 2:(n_var + 1)]
  
  radarchart(group_for_radar, 
             axistype = 4,
             title = paste0("Group ", group_id, " (N: ", n_ros_group[group_id, 2], 
                            ", Q type: ",  most_events_type[group_id]$Q_baseGroup, ")"), 
             pcol = rgb(0.2, 0.5, 0.5, 0.9), 
             pfcol = rgb(0.2, 0.5, 0.5, 0.5), 
             plwd = 4, 
             vlcex = 1.5,
             cglcol = "grey", 
             cglty = 1, 
             axislabcol = "grey", 
             cglwd = 1.2)
}
dev.copy(png, './results/plots/nodes_4_vars_radar.png', 
         width = 700, height = 600)
dev.off()

#Months per group
to_plot <- group_months[, perc := N / sum(N), P_group]

g2 <- ggplot(to_plot, aes(x = "", y = perc, fill = as.factor(month))) +
  geom_bar(stat = 'identity') +
  facet_wrap(~P_group) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = my_palette(8)) +
  labs(fill = "Month") +
  theme(legend.position = "none") +
  theme_void()

ggsave('./results/plots/nodes_month_4_vars.pdf', g2, 'pdf', 
       width = 30, height = 20, units = 'cm')

#Runoff groups vs SOM nodes
to_plot <- ros_subset[, .N, .(P_group, Q_baseGroup)]
to_plot <- to_plot[, perc := N / sum(N), P_group]

g3 <- ggplot(to_plot, aes(x = "", y = N, fill = as.factor(Q_baseGroup))) +
  geom_bar(stat = 'identity', position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(~P_group, scales = 'free') +
  scale_fill_manual(values = my_palette(6)[4:1]) +
  theme(legend.position = "none") +
  guides(fill=guide_legend(title="Q type")) +
  theme_light()

ggsave('./results/plots/nodes_Qgroup_4_vars.pdf', g3, 'pdf', 
       width = 30, height = 20, units = 'cm')

#Mountains vs SOM nodes
to_plot <- ros_cat[ros_subset, on = 'ID']
to_plot <- to_plot[complete.cases(to_plot)]
to_plot <- to_plot[, .N, .(P_group, mountain)]
to_plot <- to_plot[, perc := N / sum(N), P_group]

g4 <- ggplot(to_plot, aes(x = "", y = perc, fill = as.factor(mountain))) +
  geom_bar(stat = 'identity') +
  facet_wrap(~P_group) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = my_palette(14)[c(3,7)]) +
  theme(legend.position = "none") +
  guides(fill=guide_legend(title="Catchment")) +
  theme_void()

ggsave('./results/plots/nodes_mountain_4_vars.pdf', g4, 'pdf', 
       width = 30, height = 20, units = 'cm')
