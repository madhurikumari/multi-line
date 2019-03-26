#Vehicles Costs
#4 figures of multiple line plots. each line labeled. common legend.

library(ggplot2)
library(directlabels)
library(ggrepel)
# Vehicle Costs, Multiple Lines per plot, 4 plots total

#set working directory
setwd("/Users/mkumari/Downloads")

#read file
df_veh_costs <- read.csv("MK_FromMarshall - MK_VehicleCosts.csv", header = TRUE)

#Function to create plot with multiple lines, each one labeled. 
multi_lines <- function(df){
  df_2 <- select(df, -Vehicle)
  df_Melted <- reshape2::melt(df_2, id.var= "Year")
  colourCount = length(unique(df_Melted$variable))
  p <- df_Melted %>%
    mutate(label = if_else(Year == min(Year), as.character(variable), NA_character_)) %>%
    ggplot(aes(y = value,x = Year,color = variable))+
    geom_line(size=1.2) + scale_color_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount))+
    theme_bw()+ 
    theme(  legend.title = element_blank(),legend.text = element_text(size=13), plot.title = element_text(hjust = 0.5, size=15),
            axis.title.x = element_blank(), axis.text.x = element_text(size=13),
            axis.text.y = element_text(size = 13), axis.title.y = element_text(size=13))+
    #ylim(20000,120000)+
    geom_label_repel(aes(label = label), na.rm = TRUE, show.legend = FALSE)
  return(p)
}

#Plot Cars
df_cars <- rename(filter(df_veh_costs, Vehicle == "Cars"))
p1 <- multi_lines(df_cars) + scale_y_continuous(breaks = seq(20000,120000, 10000))+  labs(title = "Car Costs", y="Cost ($)")


#Plot Light-Duty Trucks
df_ldv <- rename(filter(df_veh_costs, Vehicle == "LD Trucks"))
p2 <- multi_lines(df_ldv) + scale_y_continuous(breaks = seq(20000,120000, 10000)) + labs(title = "Light-Duty Trucks Costs", y="Cost ($)")


#Plot Long-Haul Trucks
df_lh <- rename(filter(df_veh_costs, Vehicle == "Long-haul Trucks"))
p3 <- multi_lines(df_lh) + scale_y_continuous(breaks = seq(50000,600000, 50000))+ labs(title = "Long-Haul Trucks Costs", y="Cost ($)")


#Plot Heavy-duty Pickups and Vans
df_hd <- rename(filter(df_veh_costs, Vehicle == "Heavy-duty Pickups and Vans"))
p4 <- multi_lines(df_hd) + scale_y_continuous(breaks = seq(0,150000, 12500)) +
  labs(title = "Heavy-Duty Pickups and Vans Costs", y="Cost ($)")


#Arrange Plots
ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, legend = "right", common.legend = TRUE)

