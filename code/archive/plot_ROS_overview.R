# script to plot ROS overview
# created by @Johanna.Bloecher

# source ROS events
source("data\ prep/readROS.R")
source(paste0(.sourcedir,"source/themes.R"))
# load libaries
library(ggplot2)

# output
outdir <- paste0(.resultsdir,"/ROS_summary_plots")

if(!dir.exists(outdir)){
  dir.create(outdir)
}


# COunt figures

countdir <- paste0(outdir,"/count_figures")

if(!dir.exists(countdir)){
  dir.create(countdir)
}

# Group figures

groupdir <- paste0(outdir,"/group_figures")

if(!dir.exists(groupdir)){
  dir.create(groupdir)
}


# Plots of preliminary groups

#4.	Meteo predictors distribution within events and preliminary groups- boxplots - separate Krkonose and Jeseniky again - Results
#a.	Events length
#b.	SWE at the beginning of ROS
#c.	Mean temperature during the ROS
#d.	Precipitation/melt/water input amount during ROS events
ggplot(ROS, aes(x = as.factor(Q_group), y = Water.input , fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+  
  labs(x = "Preliminary groups", y ="Water input [mm]", fill = "Mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  theme_generic
ggsave(paste0(groupdir,"/groups_waterinput_mountain.png"))

ggplot(ROS, aes(x = as.factor(Q_group), y = EventMelt , fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+  
  labs(x = "Preliminary groups", y ="Melt [mm]", fill = "Mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  theme_generic
ggsave(paste0(groupdir,"/groups_melt_mountain.png"))

ggplot(ROS, aes(x = as.factor(Q_group), y = EventPrec , fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+  
  labs(x = "Preliminary groups", y ="Precipitation [mm]", fill = "Mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  theme_generic
ggsave(paste0(groupdir,"/groups_prec_mountain.png"))

ggplot(ROS, aes(x = as.factor(Q_group), y = Event_length, fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+  
  labs(x = "Preliminary groups", y ="Event length [h]", fill = "Mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  theme_generic
ggsave(paste0(groupdir,"/groups_duration_mountain.png"))

ggplot(ROS, aes(x = as.factor(Q_group), y = AveTLo_ROS+AveTHi_ROS , fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+  
  labs(x = "Preliminary groups", y ="Mean temperature [deg C]", fill = "Mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  theme_generic
ggsave(paste0(groupdir,"/groups_meantemp_mountain.png"))


ggplot(ROS, aes(x = as.factor(Q_group), y = SWE.first , fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+  
  labs(x = "Preliminary groups", y ="Snow water equivalent [mm]", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  theme_generic
ggsave(paste0(groupdir,"/groups_SWE_mountain.png"))

ggplot(ROS, aes(x = as.factor(Q_group), y = Qmax_time_norm, fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+  
  labs(x = "Preliminary groups", y ="Culmination time [h]", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  theme_generic
ggsave(paste0(groupdir,"/groups_SWE_mountain.png"))


# Number of events per year

ggplot(ROS, aes(x = year(Rain_Start), fill = where), title = scenario)+
  geom_bar(position="dodge")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  theme_classic()+  
  coord_cartesian(xlim=c(2004,2014))+
  labs(x = "year", y ="number of events", fill = "catchment")+
  scale_fill_manual(values = palette_vuv(15))+
  theme_generic
ggsave(paste0(countdir,"/filtered_year_count_catchment_text.png"))


ggplot(ROS, aes(x = year(Rain_Start), fill = hora), title = scenario)+
  geom_bar(position="dodge")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  theme_classic()+  
  coord_cartesian(xlim=c(2004,2014))+
  labs(x = "year", y ="number of events", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  theme_generic
ggsave(paste0(countdir,"/filtered_year_count_mountain_range_text.png"))

ggplot(ROS, aes(x = year(Rain_Start), fill = where), title = scenario)+
  geom_bar(position="dodge")+
  theme_classic()+  
  coord_cartesian(xlim=c(2004,2014))+
  labs(x = "year", y ="number of events", fill = "catchment")+
  scale_fill_manual(values = palette_vuv(15))+
  theme_generic
ggsave(paste0(countdir,"/filtered_year_count_catchment.png"))


ggplot(ROS, aes(x = year(Rain_Start), fill = hora), title = scenario)+
  geom_bar(position="dodge")+
  theme_classic()+  
  coord_cartesian(xlim=c(2004,2014))+
  labs(x = "year", y ="number of events", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  theme_generic
ggsave(paste0(countdir,"/filtered_year_count_mountain_range.png"))

# Number of events per month

ggplot(ROS, aes(x = factor(month(Rain_Start)), fill = where), title = scenario)+
  geom_bar(position="dodge")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  theme_classic()+
  labs(x = "Month", y ="number of events", fill = "catchment")+
  scale_fill_manual(values = palette_vuv(15))+
  scale_x_discrete(limits=c("11","12","1","2","3","4","5"), labels = c("Nov","Dec","Jan","Feb", "Mar", "Apr", "May"))+
  theme_generic
ggsave(paste0(countdir,"/filtered_month_count_catchment_text.png"))

ggplot(ROS, aes(x = factor(month(Rain_Start)), fill = hora), title = scenario)+
  geom_bar(position="dodge")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  theme_classic()+
  labs(x = "Month", y ="number of events", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  scale_x_discrete(limits=c("11","12","1","2","3","4","5"), labels = c("Nov","Dec","Jan","Feb", "Mar", "Apr", "May"))+
  theme_generic
ggsave(paste0(countdir,"/filtered_month_count_mountain_range_text.png"))

# Roman 23.9.2019
ggplot(ROS, aes(x = factor(month(Rain_Start)), fill = Q_group), title = scenario)+
  geom_bar(position="dodge")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  theme_classic()+
  labs(x = "Month", y ="number of events", fill = "Q_group")+
  scale_fill_manual(values = c("darkorange","royalblue4", "green", "black"))+
  scale_x_discrete(limits=c("11","12","1","2","3","4","5"), labels = c("Nov","Dec","Jan","Feb", "Mar", "Apr", "May"))+
  theme_generic
ggsave(paste0(countdir,"/filtered_month_count_group_text.png"))
####

ggplot(ROS, aes(x = factor(month(Rain_Start)), fill = where), title = scenario)+
  geom_bar(position="dodge")+
  theme_classic()+
  labs(x = "Month", y ="number of events", fill = "mountain range")+
  scale_fill_manual(values = palette_vuv(15))+
  scale_x_discrete(limits=c("11","12","1","2","3","4","5"), labels = c("Nov","Dec","Jan","Feb", "Mar", "Apr", "May"))+
  theme_generic
ggsave(paste0(countdir,"/filtered_month_count_catchment.png"))

ggplot(ROS, aes(x = factor(month(Rain_Start)), fill = hora), title = scenario)+
  geom_bar(position="dodge")+
  theme_classic()+
  labs(x = "Month", y ="number of events", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  scale_x_discrete(limits=c("11","12","1","2","3","4","5"), labels = c("Nov","Dec","Jan","Feb", "Mar", "Apr", "May"))+
  theme_generic
ggsave(paste0(countdir,"/filtered_month_count_mountain_range.png"))


# Range events year
ggplot(ROS, aes(x = as.factor(where), y = AveTLo_ROS , fill = where), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+  
  labs(x = "Catchment", y ="temperature at lower station [deg C]", fill = "mountain range")+
  scale_fill_manual(values = palette_vuv(15))+
  theme_generic
ggsave(paste0(outdir,"/filtered_temp_catchment.png"))

ggplot(ROS, aes(x = as.factor(where), y = SCE_start , fill = where), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+  
  labs(x = "Catchment", y ="Snow depth [cm]", fill = "mountain range")+
  scale_fill_manual(values = palette_vuv(15))+
  theme_generic
ggsave(paste0(outdir,"/filtered_sce_catchment.png"))

ggplot(ROS, aes(x = as.factor(where), y = SWE.first , fill = where), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+  
  labs(x = "Catchment", y ="Snow Water equivalent [mm]", fill = "mountain range")+
  scale_fill_manual(values = palette_vuv(15))+
  theme_generic
ggsave(paste0(outdir,"/filtered_SWE_catchment.png"))

ggplot(ROS, aes(x = as.factor(where), y = EventPrec , fill = where), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+  
  labs(x = "Catchment", y ="Event precipitation [mm]", fill = "mountain range")+
  scale_fill_manual(values = palette_vuv(15))+
  theme_generic
ggsave(paste0(outdir,"/filtered_precp_catchment.png"))

ggplot(ROS, aes(x = as.factor(where), y = Event_length , fill = where), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+  
  labs(x = "Catchment", y ="Event length [h]", fill = "mountain range")+
  scale_fill_manual(values = palette_vuv(15))+
  theme_generic
ggsave(paste0(outdir,"/filtered_duration_catchment.png"))

ggplot(ROS, aes(x = factor(month(Rain_Start)), y = AveTLo_ROS , fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+
  labs(x = "Month", y ="temperature at lower station [deg C]", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  scale_x_discrete(limits=c("11","12","1","2","3","4","5"), labels = c("Nov","Dec","Jan","Feb", "Mar", "Apr", "May"))+
  theme_generic
ggsave(paste0(outdir,"/filtered_temp_month.png"))


ggplot(ROS, aes(x = factor(month(Rain_Start)), y = (AveTLo_ROS+AveTHi_ROS)/2 , fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+
  labs(x = "Month", y ="Mean temperature [deg C]", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  scale_x_discrete(limits=c("11","12","1","2","3","4","5"), labels = c("Nov","Dec","Jan","Feb", "Mar", "Apr", "May"))+
  theme_generic
ggsave(paste0(outdir,"/filtered_mean_temp_month.png"))


ggplot(ROS, aes(x = factor(month(Rain_Start)), y = SCE_start , fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+
  labs(x = "Month", y ="Snow depth [cm]", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  scale_x_discrete(limits=c("11","12","1","2","3","4","5"), labels = c("Nov","Dec","Jan","Feb", "Mar", "Apr", "May"))+
  theme_generic
ggsave(paste0(outdir,"/filtered_sce_month.png"))

ggplot(ROS, aes(x = factor(month(Rain_Start)), y = SWE.first , fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+
  labs(x = "Month", y ="SWE [mm]", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  scale_x_discrete(limits=c("11","12","1","2","3","4","5"), labels = c("Nov","Dec","Jan","Feb", "Mar", "Apr", "May"))+
  theme_generic
ggsave(paste0(outdir,"/filtered_SWE_month.png"))

ggplot(ROS, aes(x = factor(month(Rain_Start)), y = EventPrec, fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+
  labs(x = "Month", y ="Event precipitation [mm]", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  scale_x_discrete(limits=c("11","12","1","2","3","4","5"), labels = c("Nov","Dec","Jan","Feb", "Mar", "Apr", "May"))+
  theme_generic
ggsave(paste0(outdir,"/filtered_prec_month.png"))

ggplot(ROS, aes(x = factor(month(Rain_Start)), y = Water.input, fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+
  labs(x = "Month", y ="Water input [mm]", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  scale_x_discrete(limits=c("11","12","1","2","3","4","5"), labels = c("Nov","Dec","Jan","Feb", "Mar", "Apr", "May"))+
  theme_generic
ggsave(paste0(outdir,"/filtered_waterin_month.png"))

ggplot(ROS, aes(x = factor(month(Rain_Start)), y = EventMelt, fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+
  labs(x = "Month", y ="Melt [mm]", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  scale_x_discrete(limits=c("11","12","1","2","3","4","5"), labels = c("Nov","Dec","Jan","Feb", "Mar", "Apr", "May"))+
  theme_generic
ggsave(paste0(outdir,"/filtered_melt_month.png"))

ggplot(ROS, aes(x = factor(month(Rain_Start)), y = Event_length, fill = hora), title = scenario)+
  geom_boxplot(position="dodge2")+
  theme_classic()+
  labs(x = "Month", y ="Event length [h]", fill = "mountain range")+
  scale_fill_manual(values = c( "darkorange","royalblue4"))+
  scale_x_discrete(limits=c("11","12","1","2","3","4","5"), labels = c("Nov","Dec","Jan","Feb", "Mar", "Apr", "May"))+
  theme_generic
ggsave(paste0(outdir,"/filtered_duration_month.png"))

# SCATTERPLOTS
# decrease of snow depth and Q/P in relation to temperature and event precipitation

a <- ggplot(ROS[(SCE_start-SCE_end) > 0],
            aes(y = SCE_start-SCE_end, x = AveTLo_ROS))+
  labs(x = "Temperature at lower station [deg C]", y = "Decrease in snow depth")+
  geom_smooth(method ="lm")+
  geom_point()+
  theme_generic

b <- ggplot(ROS[(SCE_start-SCE_end) > 0],
            aes(y = SCE_start-SCE_end, x = EventPrec))+
  labs(y = "Decrease in snow depth", x = "Event Precipitation [mm]")+
  geom_smooth(method ="lm")+
  geom_point()+
  theme_generic


c <- ggplot(ROS[],
            aes(y = Q_coeff, x = AveTLo_ROS))+
  labs(y = "Q/P", x = "Temperature at lower station [deg C]")+
  geom_smooth(method ="lm")+
  geom_point()+
  theme_generic

d <- ggplot(ROS[],
            aes(y = Q_coeff, x = EventPrec))+
  labs(y = "Q/P", x = "Event Precipitation [mm]")+
  geom_smooth(method ="lm")+
  geom_point()+
  theme_generic

# Retention

e <- ggplot(ROS[(SCE_start-SCE_end) > 0],
            aes(y = Water.input- EventQ, x = SCE_start))+
  labs(y = "Storage [mm]", x = "Snow depth [cm]")+
  geom_smooth(method ="lm")+
  geom_point()+
  theme_generic

f <- ggplot(ROS[(SCE_start-SCE_end) > 0],
            aes(y = Water.input- EventQ, x = SWE.first))+
  labs(y = "Storage [mm]", x = "SWE [mm]")+
  geom_smooth(method ="lm")+
  geom_point()+
  theme_generic

ggarrange(a, b, c, d, e, f, ncol = 2, nrow = 3)

ggsave(paste0(outdir,"/filtered_retention.png"))

# Explorative plots
plot(sort(ROS$Event_length[ROS$Event_length <500]))

ggplot(ROS[Event_length<24*5],aes(y = Event_length, x = EventPrec, color = as.factor(month(Rain_Start))))+
  geom_point()+
  geom_smooth(method = lm)+
  scale_color_manual(values =  palette_vuv(9)) +
  labs(x = "Event Precipitation [mm]", y = "Event length [h]", color = "Month of the year")+
  theme_generic

ggplot(ROS[Event_length<24*5],aes(y = Event_length, x = EventPrec, color = as.factor(year(Rain_Start))))+
  geom_point()+
  geom_smooth(method = lm)+
  scale_color_manual(values =  palette_vuv(11)) +
  labs(x = "Event Precipitation [mm]", y = "Event length [h]", color = "Year")+
  theme_generic

ggplot(ROS[Event_length<24*10],aes(y = Event_length, x = EventPrec, color = as.factor(where)))+
  geom_point()+
  geom_smooth(method = lm)+
  scale_color_manual(values =  palette_vuv(16)) +
  labs(x = "Event Precipitation [mm]", y = "Event length [h]", color = "Catchment")+
  theme_generic

ggplot(ROS[Event_length<24*10],aes(y = Event_length, x = EventPrec, color = as.factor(hora)))+
  geom_point()+
  geom_smooth(method = lm)+
  scale_color_manual(values =  palette_vuv(2)) +
  labs(x = "Event Precipitation [mm]", y = "Event length [h]", color = "Mountain range")+
  theme_generic

ggplot(ROS[Event_length<24*5 & (SCE_start-SCE_end) >= 0],aes(y = SCE_start-SCE_end, x = EventPrec, color = as.factor(hora)))+
  geom_point()+
  geom_smooth(method = lm)+
  scale_color_manual(values =  palette_vuv(2)) +
  labs(x = "Event Precipitation [mm]", y = "Decrease in snow depth [cm]", color = "Mountain range")+
  theme_generic


ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) >= 0],aes(y = Event_length, x = EventPrec, color = as.factor(where)))+
  geom_point()+
  geom_smooth(method = lm)+
  scale_color_manual(values =  palette_vuv(16)) +
  labs(x = "Event Precipitation [mm]", y = "Event length [h]", color = "Catchment")+
  theme_generic

ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) >= 0],aes(y = SCE_start-SCE_end, x = EventPrec, color = as.factor(where)))+
  geom_point()+
  geom_smooth(method = lm)+
  scale_color_manual(values =  palette_vuv(16)) +
  labs(x = "Event Precipitation [mm]", y = "Decrease in snow depth [cm]", color = "Catchment")+
  theme_generic

ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) >= 0],aes(y = SCE_start-SCE_end, x = EventPrec, color = AveTLo_ROS))+
  geom_point()+
  scale_colour_continuous(type =  "viridis") +
  labs(x = "Event Precipitation [mm]", y = "Decrease in snow depth [cm]", color = "Temperature deg C")+
  theme_generic

ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0],aes(y = SCE_start-SCE_end, x = EventPrec, color = round(AveTLo_ROS)))+
  geom_point()+
  scale_colour_continuous(type =  "viridis") +
  labs(x = "Event Precipitation [mm]", y = "Decrease in snow depth [cm]", color = "Temperature deg C")+
  theme_generic

ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0],aes(y = (SCE_start-SCE_end)/SCE_start, x = EventPrec, color = round(AveTHi_ROS)))+
  geom_point()+
  scale_colour_continuous(type =  "viridis") +
  labs(x = "Event Precipitation [mm]", y = "Decrease in snow depth [%]", color = "Temperature deg C")+
  theme_generic

ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ > 0],aes(y = (SCE_start-SCE_end)/SCE_start, x = EventQ, size = EventPrec, color = as.factor(round(AveTHi_ROS))))+
  geom_point()+
  scale_color_manual(values = rev(palette_jo(14))) +
  labs(x = "Event Q [mm]", y = "Decrease in snow depth [%]", color = "Temperature deg C", size = "Event Precipitation [mm]")+
  theme_generic

ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ > 0],aes(y = (SCE_start-SCE_end), x = EventQ, size = EventPrec, color = as.factor(round(AveTHi_ROS))))+
  geom_point()+
  scale_color_manual(values = rev(palette_jo(14))) +
  labs(x = "Event Q [mm]", y = "Decrease in snow depth [cm]", color = "Temperature deg C", size = "Event Precipitation [mm]")+
  theme_generic

ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ_ef > 0 & Q_coeff_ef < 5],aes(y = (SCE_start-SCE_end), x = Q_coeff_ef, size = EventPrec, color = as.factor(round(AveTLo_ROS))))+
  geom_point()+
  scale_color_manual(values = rev(palette_jo(18))) +
  labs(x = "Q_coeff [-]", y = "Decrease in snow depth [cm]", color = "Temperature deg C", size = "Event Precipitation [mm]")+
  geom_vline(xintercept = 1)+
  theme_generic

ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ_ef > 0 & Q_coeff_ef < 5],
       aes(y = (SCE_start-SCE_end), x = Q_coeff_ef, size = EventPrec, color = as.factor(round(API_start*1e+6))))+
  geom_point()+
  scale_color_manual(values = rev(palette_jo(7))) +
  labs(x = "Q_coeff [-]", y = "Decrease in snow depth [cm]", color = "API", size = "Event Precipitation [mm]")+
  geom_vline(xintercept = 1)+
  theme_generic


ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ > 0 & Q_coeff_ef < 5],aes(y = (SCE_start-SCE_end), x = Q_coeff, size = EventPrec, color = as.factor(round(AveTLo_ROS))))+
  geom_point()+
  scale_color_manual(values = rev(palette_jo(18))) +
  labs(x = "Q_coeff [-]", y = "Decrease in snow depth [cm]", color = "Temperature deg C", size = "Event Precipitation [mm]")+
  geom_vline(xintercept = 1)+
  theme_generic

ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ > 0 & Q_coeff_ef < 5])+
  geom_point(aes(x = EventQ, y = EventPrec), color = "royalblue1")+
  geom_point(aes(x = EventQ, y = Water.input), color = "royalblue4")+
  geom_point(aes(x = EventQ, y = Water.input- EventQ), color = "darkorange")+
  geom_abline(xintercept = 0, slope = 1, lwd = 2)+
  theme_generic

ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ > 0 & Q_coeff_ef < 5])+
  geom_point(aes(x = EventQ, y = EventPrec, size = AveTHi_ROS), color = "royalblue1")+
  geom_point(aes(x = EventQ, y = Water.input), color = "royalblue4")+
  geom_point(aes(x = EventQ, y = Water.input- EventQ), color = "darkorange")+
  geom_abline(xintercept = 0, slope = 1, lwd = 2)+
  theme_generic

ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ > 0 & Q_coeff_ef < 5])+
  geom_point(aes(x = AveTLo_ROS, y =  Water.input-EventPrec ), color = "royalblue1")+
  labs(y = "Melt [mm]", x = "Temperature at lower station [deg C]")+
  theme_generic

ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ > 0 & Q_coeff_ef < 5])+
  geom_point(aes(x = AveTLo_ROS, y =  Water.input- EventQ ), color = "darkorange")+
  labs(y = "Storage [mm]", x = "Temperature at lower station [deg C]")+
  theme_generic


ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ > 0 & Q_coeff_ef < 5])+
  geom_point(aes(x = AveTLo_ROS, y =  Water.input- EventQ, color = as.factor(where) ))+
  scale_color_manual(values =  palette_vuv(16)) +
  labs(y = "Storage [mm]", x = "Temperature at lower station [deg C]", color = "Catchment")+
  theme_generic


ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ > 0 & Q_coeff_ef < 5])+
  geom_boxplot(aes(x = where, y = Water.input- EventQ, fill = hora))+
  labs(y = "Storage [mm]", x = "Catchment", group = "Catchment")+
  scale_fill_manual(values =  c( "darkorange","royalblue4")) +
  theme_generic


ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ > 0 & Q_coeff_ef < 5 & hora == "Krkonose"])+
  geom_point(aes(x = AveTLo_ROS, y =  Water.input- EventQ, color = as.factor(where) ))+
  scale_color_manual(values =  palette_vuv(16)) +
  labs(y = "Storage [mm]", x = "Temperature at lower station [deg C]", color = "Catchment")+
  theme_generic


ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ > 0 & Q_coeff_ef < 5 & hora == "Krkonose"])+
  geom_point(aes(x = SCE_start-SCE_end, y =  Water.input- EventQ, color = as.factor(where) ))+
  scale_color_manual(values =  palette_vuv(16)) +
  labs(y = "Storage [mm]", x = "Decrease in snow depth [cm]", color = "Catchment")+
  theme_generic

ggplot(ROS[Event_length<24*10 & (SCE_start-SCE_end) > 0 & EventQ > 0 & Q_coeff_ef < 5 & hora == "Krkonose"])+
  geom_point(aes(x = SCE_start-SCE_end, y =  Water.input- EventPrec, color = as.factor(month(Rain_Start))))+
  geom_smooth(method = "lm", aes(x = SCE_start-SCE_end, y =  Water.input- EventPrec, color = as.factor(month(Rain_Start))))+
  scale_color_manual(values =  palette_vuv(12)) +
  labs(y = "Melt [mm]", x = "Decrease in snow depth [cm]", color = "Month of the year")+
  theme_generic

# @Roman
ggplot(ROS[Q_coeff_ef < 5])+
  geom_point(aes(x = API_start, y =  Qmax_time_norm, color = as.factor(month(Rain_Start))))+
  geom_smooth(method = "lm", aes(x = API_start, y =  Qmax_time_norm, color = as.factor(month(Rain_Start))))+
  scale_color_manual(values =  palette_vuv(12)) +
  labs(y = "Qmax time [min]", x = "API_start [mm]", color = "Month of the year")+
  theme_generic

ggplot(ROS[Q_coeff_ef < 5 & (WL1_ex > 0 | WL2_ex > 0 | WL3_ex > 0 | Q1_ex > 0 | Q5_ex > 0 | Q10_ex > 0)])+
  geom_point(aes(x = SWE.first, y =  Qmax_time_norm, color = as.factor(hora)))+
  geom_smooth(method = "lm", aes(x = SWE.first, y =  Qmax_time_norm, color = as.factor(hora)))+
  #scale_color_manual(values =  palette_vuv(14)) +
  labs(y = "Qmax time [h]", x = "API_start [mm]", color = "Month of the year")+
  theme_generic

ggplot(ROS[Q_coeff_ef < 5 & !is.na(T_BaseQ.ex)])+
  geom_point(aes(x = API_start, y =  Qmax_time_norm, color = as.factor(hora)))+
  geom_smooth(method = "lm", aes(x = API_start, y =  Qmax_time_norm, color = as.factor(hora)))+
  scale_color_manual(values =  palette_vuv(14)) +
  labs(y = "Qmax time [h]", x = "API_start [mm]", color = "Month of the year")+
  theme_generic

ggplot(ROS[Q_coeff_ef < 5 & !is.na(T_BaseQ.ex)])+
  geom_point(aes(x = API_start, y =  maxQ/Q_first, color = as.factor(hora)))+
  geom_smooth(method = "lm", aes(x = API_start, y = maxQ/Q_first, color = as.factor(hora)))+
  #scale_color_manual(values =  palette_vuv(14)) +
  labs(y = "maxQ/Q_first", x = "API_start [mm]", color = "Month of the year")+
  theme_generic

# Rain intensity
ggplot(ROS[Q_coeff_ef < 5 & !is.na(T_BaseQ.ex)])+
  geom_point(aes(x = (EventPrec)/SWE.first, y =  Qmax_time_norm, color = as.factor(hora)))+
  geom_smooth(method = "lm", aes(x = (EventPrec)/SWE.first, y =  Qmax_time_norm, color = as.factor(hora)))+
  #scale_color_manual(values =  palette_vuv(16)) +
  labs(y = "Qmax time [h]", x = "Rain intensity", color = "Month of the year")+
  theme_generic

ggplot(ROS[Q_coeff_ef < 5 & !is.na(T_BaseQ.ex)])+
  geom_point(aes(x = CumTemp_first, y =  maxQ, color = as.factor(hora)))+
  geom_smooth(method = "lm", aes(x = CumTemp_first, y =  maxQ, color = as.factor(hora)))+
  #scale_color_manual(values =  palette_vuv(16)) +
  labs(y = "Qmax [mm]", x = "Cum Temp", color = "Month of the year")+
  theme_generic
