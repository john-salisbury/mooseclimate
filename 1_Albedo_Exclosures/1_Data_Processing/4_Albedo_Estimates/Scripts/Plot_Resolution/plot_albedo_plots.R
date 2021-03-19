## Script to produce plots of albedo estimates for all 37 'used sites' (Trøndelag, Hedmark, Telemark)
## across all years of available volume data. 

## Note: volume (m3/ha) and albedo estimates are on the PLOT level


##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(gridExtra)
        library(cowplot)
        library(sf)
        library(raster)
        library(zoo)
        library(beepr)
        library(wesanderson)
        library(ggsci)

        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        #Mean albedo estimates by treatment AND REGION
        albedo_reg <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plot_Resolution/mean_plot_albedo_by_region.csv", header = T)

#END INITIAL DATA IMPORT --------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#PLOT ALBEDO - MONTH ON X-AXIS ------------------------------------------------------------------

        
        #Set strip text labels
        months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        month_labs <- function(variable,value){
                return(months[value])
        }
        
        #Facet by region & choose odd YSE
        
                #Subset to data in even YSE
                albedo_filt <- albedo_reg[albedo_reg$Years_Since_Exclosure %in% c(2,4,6,8,10) & albedo_reg$Group == "Composite",]
        
                #Set YSE as factor (for plot colors)
                albedo_filt$Years_Since_Exclosure <- as.factor(albedo_filt$Years_Since_Exclosure)
        #Plot
                #Graph parameters
                pd <- position_dodge(0.1)
                pal <- wes_palette("FantasticFox1")
                plot_labs <- c("2 yr - Browsed",
                               "2 yr - Unbrowsed",
                               "4 yr - Browsed",
                               "4 yr - Unbrowsed",
                               "6 yr - Browsed",
                               "6 yr - Unbrowsed",
                               "8 yr - Browsed",
                               "8 yr - Unbrowsed",
                               "10 yr - Browsed",
                               "10 yr - Unbrowsed")
                
                plot_pal <- c("#f4e61e",
                              "#f4e61e",
                              "#65cb5e",
                              "#65cb5e",
                              "#22928c",
                              "#22928c",
                              "#38598b",
                              "#38598b",
                              "#440e57",
                              "#440e57")
                
                albedo_filt$Treatment <- as.factor(albedo_filt$Treatment)
                
                
        #PLOT WITHOUT ERROR -------
        ggplot(data = albedo_filt, aes(x = Month, y = Mean_Plot_Albedo, color = interaction(Treatment, Years_Since_Exclosure), linetype = interaction(Treatment, Years_Since_Exclosure))) +
                geom_point(size = 1, shape = 1, position = pd) +
                geom_line(position = pd, alpha = 0.6) +
                labs(y = "Albedo", color = "Treatment", linetype = "Treatment") +
                facet_wrap(~Region, ncol = 2) +
                scale_x_continuous(breaks = c(1:12)) +
                scale_linetype_manual(labels = plot_labs,
                                      values = c(1,4,1,4,1,4,1,4,1,4)) +
                scale_color_manual(labels = plot_labs,
                                   values = plot_pal) +
                theme_bw() +
                theme(
                        legend.title = element_blank(),
                        legend.background = element_rect(fill="#fafafa",
                                                         size=0.1, linetype="solid", 
                                                         colour ="#666666"),
                        legend.position = c(0.75,0.25),
                        axis.title.x = element_text(margin = margin(t = 10)),
                        axis.title.y = element_text(margin = margin(r = 10)),
                        panel.grid.minor = element_blank()
                ) +
                guides(color=guide_legend(nrow=5,byrow=TRUE)) +
                guides(linetype=guide_legend(nrow=5,byrow=TRUE))
                
                
        #PLOT WITH ERROR --------
        ggplot(data = albedo_filt, aes(x = Month, y = Mean_Plot_Albedo, color = interaction(Treatment, Years_Since_Exclosure), linetype = interaction(Treatment, Years_Since_Exclosure))) +
                geom_ribbon(aes(ymin = (Mean_Plot_Albedo - SE), ymax = (Mean_Plot_Albedo + SE), fill = interaction(Treatment, Years_Since_Exclosure)), alpha = 0.15, lwd = 0) +
                geom_point(size = 1, shape = 1, position = pd) +
                geom_line(position = pd, alpha = 0.6) +
                labs(y = "Albedo", color = "Treatment", linetype = "Treatment", fill = "Treatment") +
                facet_wrap(~Region, ncol = 2) +
                scale_x_continuous(breaks = c(1:12)) +
                scale_linetype_manual(labels = plot_labs,
                                      values = c(1,4,1,4,1,4,1,4,1,4)) +
                scale_color_manual(labels = plot_labs,
                                   values = plot_pal) +
                scale_fill_manual(labels = plot_labs,
                                   values = plot_pal) +
                theme_bw() +
                theme(
                        legend.title = element_blank(),
                        legend.background = element_rect(fill="#fafafa",
                                                         size=0.1, linetype="solid", 
                                                         colour ="#666666"),
                        legend.position = c(0.75,0.25),
                        axis.title.x = element_text(margin = margin(t = 10)),
                        axis.title.y = element_text(margin = margin(r = 10)),
                        panel.grid.minor = element_blank()
                ) +
                guides(color=guide_legend(nrow=5,byrow=TRUE)) +
                guides(linetype=guide_legend(nrow=5,byrow=TRUE))        
                
                
#END PLOT ALBEDO - MONTH ON X-AXIS ------------------------------------------------------------------
                
