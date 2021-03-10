## Script to produce plots of albedo estimates for all 37 'used sites' (Trøndelag, Hedmark, Telemark)
## across all years of available volume data. 

## Note: volume (m3/ha) and albedo estimates are on the subplot level
## Albedo estimates are calculated for each 'group' within each subplot, and then a weighted average 
## (based on tree species proportions within each subplot) is used to calculate a 'composite'
## subplot albedo value

## Note: albedo estimates are produced using average T and SWE data for EACH LOCALITYNAME (i.e. study site)
## (produced by taking mean of climate data across all years of available SustHerb tree data
## within each site)

## Note: I chose to include herbivore density data from 2015 in the final analysis, as this represents
## a nice temporal midpoint in the 2009-2019 tree data



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

        #Raw albedo estimates for each subplot
        final_albedo <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/subplot_albedo_estimates.csv", header = T)

        #Mean albedo estimates by treatment (all sites/regions unified)
        albedo_means <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/mean_subplot_albedo.csv", header = T)

        #Mean albedo estimates by treatment AND REGION
        albedo_reg <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/mean_subplot_albedo_by_region.csv", header = T)

        #Mean albedo estimates by treatment AND SITE
        albedo_site <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/mean_subplot_albedo_by_site.csv", header = T)
        
#END INITIAL DATA IMPORT --------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#PLOT ALBEDO - **MEANS - ALL REGIONS**  ---------------------------------------------------------------------------------

        #Set strip text labels
        months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        month_labs <- function(variable,value){
                return(months[value])
        }
        
        #Set palette
        pal <- wes_palette('Darjeeling1')
        
        #Add treatment 'nice names'
        albedo_means$Treatment_NN <- as.character('')
        albedo_means$Treatment_NN[albedo_means$Treatment == "B"] <- "Browsed"
        albedo_means$Treatment_NN[albedo_means$Treatment == "UB"] <- "Unbrowsed"
        
        all_means <- ggplot(data = subset(albedo_means, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, group = Treatment_NN)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment_NN), alpha = 0.35, lwd = 0.1) +
                                geom_point(aes(shape = Treatment_NN, color = Treatment_NN), size = 0.6) +
                                facet_wrap(~Month, labeller = labeller(Month = month_labs), ncol = 6) +
                                labs(x = "Years Since Exclosure", y = "Albedo", color = "Treatment: ", shape = "Treatment: ", fill = "Treatment: ") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_manual(labels = c("Browsed", "Unbrowsed"), values = pal) +
                                theme_bw() +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.text.x = element_text(size = 8),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 13),
                                        panel.grid.major = element_line(color = "#fafafa")
                                ) +
                                guides(colour = guide_legend(override.aes = list(size=2)))
        
        all_means
        
        #Save as SVG
        ggsave('subplot_albedo_all.svg',
               all_means,
               "svg",
               '1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots',
               scale = 1.25)
        
#END PLOT ALBEDO - **MEANS - ALL REGIONS**  ---------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#PLOT ALBEDO - **MEANS WITHIN EACH REGION**  ---------------------------------------------------------------------------------
        
        #NOTE: These plots use the mean values (and SE) TAKEN WITHIN EACH REGION 
        ##This is the 'albedo_reg' df
        
        #PLOT 1A - Composite albedo on one plot (stacked) --------------
        
                #All Sites
                pd <- position_dodge(0.1)
        
                #Build complex graph using cowplot package
                
                        #Trøndelag graph
                        g1 <- ggplot(data = subset(albedo_reg, Group == "Composite" & Region == "Trøndelag"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_line(alpha = 0.5) +
                                geom_point(aes(shape = Treatment), size = 0.8) +
                                facet_wrap(~Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                scale_color_manual(values = pal) +
                                ggtitle("Trøndelag") +
                                theme_bw() +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.text.x = element_text(size = 8),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 13),
                                        panel.grid.major = element_line(color = "#fafafa")
                                )
                        g1
                        
                        #Hedmark graph
                        g2 <- ggplot(data = subset(albedo_reg, Group == "Composite" & Region == "Hedmark"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_line(alpha = 0.5) +
                                geom_point(aes(shape = Treatment), size = 0.8) +
                                facet_wrap(~Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                scale_color_manual(values = pal) +
                                ggtitle("Hedmark") +
                                theme_bw() +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.text.x = element_text(size = 8),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 13),
                                        panel.grid.major = element_line(color = "#fafafa")
                                )
                        g2
                        
                        #Telemark graph
                        g3 <- ggplot(data = subset(albedo_reg, Group == "Composite" & Region == "Telemark"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_line(alpha = 0.5) +
                                geom_point(aes(shape = Treatment), size = 0.8) +
                                facet_wrap(~Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                scale_color_manual(values = pal) +
                                ggtitle("Telemark") +
                                theme_bw() +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.text.x = element_text(size = 8),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 13),
                                        panel.grid.major = element_line(color = "#fafafa")
                                )
                        g3
                        
                        
                        #Legend function
                        extract_legend <- function(my_ggp) {
                                step1 <- ggplot_gtable(ggplot_build(my_ggp))
                                step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
                                step3 <- step1$grobs[[step2]]
                                return(step3)
                        }
                        
                        #Additional graph to extract legend from
                        
                                #Add treatment 'nice names'
                                albedo_reg$Treatment_NN <- as.character('')
                                albedo_reg$Treatment_NN[albedo_reg$Treatment == "B"] <- "Browsed"
                                albedo_reg$Treatment_NN[albedo_reg$Treatment == "UB"] <- "Unbrowsed"
                                
                        g1_l <- ggplot(data = subset(albedo_reg, Group == "Composite" & Region == "Telemark"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, group = Treatment_NN)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment_NN), alpha = 0.35, lwd = 0.1) +
                                geom_point(size = 2, aes(color = Treatment_NN, shape = Treatment_NN)) +
                                facet_wrap(~Month, ncol = 12) +
                                labs(x = "Years Since Exclosure", y = "Albedo", color = "Treatment: ", shape = "Treatment: ", fill = "Treatment: ") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_manual(values = pal) +
                                ggtitle("Telemark") +
                                theme_bw() +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.text.x = element_text(size = 8),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 10),
                                        panel.grid.major = element_line(color = "#fafafa")
                                )
                        g1_l
                        
                        #Extract legend
                        shared_legend <- extract_legend(g1_l)

                        complex_plot <- plot_grid(g1, NULL, g2, NULL, g3, NULL, shared_legend, ncol = 1, rel_heights = c(0.3, 0.01666667, 0.3, 0.01666667, 0.3, 0.01666667, 0.05))
                        complex_plot
                        
                        #Save as SVG
                        ggsave('subplot_albedo_by_region.svg',
                               complex_plot,
                               "svg",
                               '1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots',
                               scale = 1.25)
        

        #PLOT 1B - Composite albedo on one plot (facet_grid) --------------
                        
                complex_plot_2 <- ggplot(data = subset(albedo_reg, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, group = Treatment_NN)) +
                                        geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment_NN), alpha = 0.35, lwd = 0.1) +
                                        geom_point(aes(shape = Treatment_NN, color = Treatment_NN), size = 0.6) +
                                        facet_grid(Region~Month, labeller = labeller(Month = month_labs)) +
                                        labs(x = "Years Since Exclosure", y = "Albedo", color = "Treatment: ", shape = "Treatment: ", fill = "Treatment: ") +
                                        scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                        scale_color_manual(labels = c("Browsed", "Unbrowsed"), values = pal) +
                                        theme_bw() +
                                        theme(
                                                legend.position = "bottom",
                                                axis.title.x = element_text(margin = margin(t = 10)),
                                                axis.text.x = element_text(size = 8),
                                                axis.title.y = element_text(margin = margin(r = 10)),
                                                panel.grid.minor = element_blank(),
                                                plot.title = element_text(hjust = 0.5, size = 13),
                                                panel.grid.major = element_line(color = "#fafafa")
                                        ) +
                                        guides(colour = guide_legend(override.aes = list(size=2)))
                complex_plot_2
                        
                #Save as SVG
                ggsave('subplot_albedo_by_region_v2.svg',
                       complex_plot_2,
                       "svg",
                       '1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots',
                       scale = 1.25)
       
                                 
#END PLOT ALBEDO - **MEANS WITHIN EACH REGION**  ---------------------------------------------------------------------------------
                
                
                
                
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                
                
                
                
#PLOT ALBEDO - **MEANS WITHIN EACH LOCALITYNAME** ------------------------------------------------------------------
                
        #PLOT 1 - FACETED BY SITE
        all_sites_plot <- ggplot(data = subset(albedo_site, Group == "Composite" & Month == 1), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo,group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_line(alpha = 0.5, aes(color = Treatment)) +
                                geom_point(aes(shape = Treatment, color = Treatment), size = 0.8) +
                                facet_wrap(~LocalityName) +
                                labs(x = "Years Since Exclosure", y = "Albedo", color = "Treatment: ", shape = "Treatment: ", fill = "Treatment: ") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(breaks = c(0.30, 0.40, 0.50)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                #scale_color_manual(values = pal) +
                                theme_bw() +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.text.x = element_text(size = 8),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 13),
                                        panel.grid.major = element_line(color = "#fafafa")
                                ) +
                                guides(fill = F) +
                                guides(shape = F)
                
                #Save as SVG
                ggsave('subplot_albedo_by_site.svg',
                       all_sites_plot,
                       "svg",
                       '1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots',
                       scale = 1.25)
                
#END PLOT ALBEDO - **MEANS WITHIN EACH LOCALITYNAME** ------------------------------------------------------------------
                
                
                
                
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#PLOT ALBEDO - MONTH ON X-AXIS ------------------------------------------------------------------
                
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
                
                ggplot(data = albedo_filt, aes(x = Month, y = Mean_Subplot_Albedo, color = interaction(Treatment, Years_Since_Exclosure), linetype = interaction(Treatment, Years_Since_Exclosure))) +
                        geom_point(size = 1.8, shape = 4, position = pd) +
                        geom_line(position = pd, alpha = 0.8) +
                        labs(y = "Albedo", color = "Treatment", linetype = "Treatment") +
                        facet_wrap(~Region, ncol = 3) +
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
                                legend.position = "bottom",
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10)),
                                panel.grid.minor = element_blank()
                        ) 
                
                
#END PLOT ALBEDO - MONTH ON X-AXIS ------------------------------------------------------------------
                
