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
        library(viridis)

        
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
        
        #Facet by region & choose even YSE
        
                #NOTE:
                #For temporal plot (month on x-axis), use 2, 6, and 10 YSE (since not much difference)
                #For delta albedo plot, use 2,4,6,8, and 10 YSE
        
                #Subset to data in even YSE
                albedo_filt_sm <- albedo_reg[albedo_reg$Years_Since_Exclosure %in% c(2,5,8) & albedo_reg$Group == "Composite",]
                albedo_filt <- albedo_reg[albedo_reg$Years_Since_Exclosure %in% c(2,4,6,8,10) & albedo_reg$Group == "Composite",]
        
                #Set YSE as factor (for plot colors)
                albedo_filt$Years_Since_Exclosure <- as.factor(albedo_filt$Years_Since_Exclosure)
                albedo_filt_sm$Years_Since_Exclosure <- as.factor(albedo_filt_sm$Years_Since_Exclosure)
              
                  
        #Plot
                #Graph parameters
                pd <- position_dodge(0.1)
                
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
                
                plot_labs_sm <- c("2 yr - Browsed",
                               "2 yr - Unbrowsed",
                               "5 yr - Browsed",
                               "5 yr - Unbrowsed",
                               "8 yr - Browsed",
                               "8 yr - Unbrowsed")
                
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
                
                plot_pal_sm <- c("#f4e61e",
                                 "#f4e61e",
                              "#22928c",
                              "#22928c",
                              "#440e57",
                              "#440e57")
                
                albedo_filt$Treatment <- as.factor(albedo_filt$Treatment)
                albedo_filt_sm$Treatment <- as.factor(albedo_filt_sm$Treatment)
                
                
        #PLOT WITHOUT ERROR -------
        ggplot(data = albedo_filt_sm, aes(x = Month, y = Mean_Plot_Albedo, color = interaction(Treatment, Years_Since_Exclosure), linetype = interaction(Treatment, Years_Since_Exclosure))) +
                geom_point(size = 1, shape = 1, position = pd) +
                geom_line(position = pd, alpha = 0.6) +
                labs(y = "Albedo", color = "Treatment", linetype = "Treatment") +
                facet_wrap(~Region, ncol = 2) +
                scale_x_continuous(breaks = c(1:12)) +
                scale_linetype_manual(labels = plot_labs_sm,
                                      values = c(1,4,1,4,1,4)) +
                scale_color_manual(labels = plot_labs_sm,
                                   values = plot_pal_sm) +
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
                guides(color=guide_legend(nrow=3,byrow=TRUE)) +
                guides(linetype=guide_legend(nrow=3,byrow=TRUE))
                
                        #Export @ 650x500px
                
                
        #PLOT DOUBLE-FACETED BY SEASON (WINTER, SUMMER)
                
                #Winter (Months 1-3)
                g1 <- ggplot(data = subset(albedo_filt_sm, Season %in% c("Winter")), aes(x = Month, y = Mean_Plot_Albedo, color = interaction(Treatment, Years_Since_Exclosure), linetype = interaction(Treatment, Years_Since_Exclosure))) +
                        geom_point(size = 1, shape = 1, position = pd) +
                        geom_line(position = pd, alpha = 0.6) +
                        labs(y = "Albedo", color = "Treatment", linetype = "Treatment") +
                        facet_wrap(~Region) +
                        scale_x_continuous(breaks = c(1:12)) +
                        scale_y_continuous(limits = c(0.30, 0.47)) +
                        scale_linetype_manual(labels = plot_labs_sm,
                                              values = c(1,4,1,4,1,4)) +
                        scale_color_manual(labels = plot_labs_sm,
                                           values = plot_pal_sm) +
                        ggtitle("Winter (Jan - Mar)") +
                        theme_bw() +
                        theme(
                                legend.position = "none",
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10)),
                                panel.grid.minor = element_blank(),
                                plot.title = element_text(hjust = 0, face = "bold", margin = margin(b = 6), size = 12),
                                panel.grid.major = element_blank(),
                                
                        ) +
                        guides(color=guide_legend(nrow=3,byrow=TRUE)) +
                        guides(linetype=guide_legend(nrow=3,byrow=TRUE))
                
                g1
                
                #Summer (Months 7-9)
                g2 <- ggplot(data = subset(albedo_filt_sm, Season %in% c("Summer")), aes(x = Month, y = Mean_Plot_Albedo, color = interaction(Treatment, Years_Since_Exclosure), linetype = interaction(Treatment, Years_Since_Exclosure))) +
                        geom_point(size = 1, shape = 1, position = pd) +
                        geom_line(position = pd, alpha = 0.6) +
                        labs(y = "Albedo", color = "Treatment", linetype = "Treatment") +
                        facet_wrap(~Region) +
                        scale_x_continuous(breaks = c(1:12)) +
                        scale_y_continuous(limits = c(0.05,0.2), breaks = c(0.05,0.1,0.15,0.2)) +
                        scale_linetype_manual(labels = plot_labs_sm,
                                              values = c(1,4,1,4,1,4)) +
                        scale_color_manual(labels = plot_labs_sm,
                                           values = plot_pal_sm) +
                        ggtitle("Summer (Jul - Sep)") +
                        theme_bw() +
                        theme(
                                legend.position = "none",
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10)),
                                panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank(),
                                plot.title = element_text(hjust = 0, face = "bold", margin = margin(b = 6), size = 12),
                                
                        ) +
                        guides(color=guide_legend(nrow=3,byrow=TRUE)) +
                        guides(linetype=guide_legend(nrow=3,byrow=TRUE))
                
                g2
                
                #Shared Legend
                
                        #Legend function
                        extract_legend <- function(my_ggp) {
                                step1 <- ggplot_gtable(ggplot_build(my_ggp))
                                step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
                                step3 <- step1$grobs[[step2]]
                                return(step3)
                        }
                        
                g3 <- ggplot(data = subset(albedo_filt_sm, Season %in% c("Summer")), aes(x = Month, y = Mean_Plot_Albedo, color = interaction(Treatment, Years_Since_Exclosure), linetype = interaction(Treatment, Years_Since_Exclosure))) +
                        geom_point(size = 1, shape = 1, position = pd) +
                        geom_line(position = pd, alpha = 0.6) +
                        labs(y = "Albedo", color = "Treatment", linetype = "Treatment") +
                        facet_wrap(~Region) +
                        scale_x_continuous(breaks = c(1:12)) +
                        scale_linetype_manual(labels = plot_labs_sm,
                                              values = c(1,4,1,4,1,4)) +
                        scale_color_manual(labels = plot_labs_sm,
                                           values = plot_pal_sm) +
                        ggtitle("Summer (Jul - Sep)") +
                        theme_bw() +
                        theme(
                                legend.title = element_blank(),
                                legend.background = element_rect(fill="#fafafa",
                                                                 size=0.1, linetype="solid", 
                                                                 colour ="#666666"),
                                legend.position = "bottom",
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10)),
                                panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank(),
                                plot.title = element_text(hjust = 0, face = "bold", margin = margin(b = 6), size = 12)
                        ) +
                        guides(color=guide_legend(nrow=3,byrow=TRUE)) +
                        guides(linetype=guide_legend(nrow=3,byrow=TRUE))
                
                shared_legend <- extract_legend(g3)
                
                complex <- plot_grid(g1, NULL, g2, NULL, shared_legend, ncol = 1, rel_heights = c(0.35, 0.025, 0.35, 0.025, 0.15))
                complex
                
                        #Export @ 600x700px
                
                
        
                
#END PLOT ALBEDO - MONTH ON X-AXIS ------------------------------------------------------------------
                
