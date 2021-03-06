## Script to produce plots of albedo estimates for all 37 'used sites' (Trøndelag, Hedmark, Telemark)
## across all years of available volume data. 

## Note: volume (m3/ha) and albedo estimates are on the subplot level
## Albedo estimates are calculated for each 'group' within each subplot, and then a weighted average 
## (based on tree species proportions within each subplot) is used to calculate a 'composite'
## subplot albedo value

## Note: albedo estimates are produced using average T and SWE data for each of the REGIONS
## (produced by taking mean of climate data across all years of available SustHerb tree data
## within each region)

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
        library(lme4)
        library(lmerTest)
        library(beepr)
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        #Raw albedo estimates for each subplot
        final_albedo <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/subplot_albedo_estimates.csv", header = T)

        #Mean albedo estimates by treatment
        albedo_means <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/mean_subplot_albedo.csv", header = T)

        #Mean albedo estimates by treatment AND REGION
        albedo_reg <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/mean_subplot_albedo_by_region.csv", header = T)

#END INITIAL DATA IMPORT --------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#PLOT ALBEDO AT SUBPLOT LEVEL ---------------------------------------------------------------------------------
        
                
        #COMPOSITE ALBEDO -----------
                
                #Set strip text labels
                months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
                month_labs <- function(variable,value){
                        return(months[value])
                }
                
                #All Sites
                pd <- position_dodge(0.1)
                
                
                #COMPLEX PLOT  -------------
                
                        #Winter
                        g1 <- ggplot(subset(albedo_means, Group == "Composite" & Season == "Winter"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_grid(Region ~ Month, labeller = labeller(Month = c("Jan", "Feb", "Mar"))) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.3, 0.525)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Winter") +
                                theme(
                                        #legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g1
                                
                        #Spring
                        g2 <- ggplot(subset(albedo_means, Group == "Composite" & Season == "Spring"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.1425, 0.35)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Spring") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g2
                        
                        #Summer
                        g3 <- ggplot(subset(albedo_means, Group == "Composite" & Season == "Summer"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.05, 0.25)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Summer") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g3
                        
                        #Autumn
                        g4 <- ggplot(subset(albedo_means, Group == "Composite" & Season == "Autumn"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.1425, 0.35)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Autumn") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g4
                                
                                
                        #Common legend
                        
                                #Legend function
                                extract_legend <- function(my_ggp) {
                                        step1 <- ggplot_gtable(ggplot_build(my_ggp))
                                        step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
                                        step3 <- step1$grobs[[step2]]
                                        return(step3)
                                }
                        
                                g1_l <- ggplot(subset(albedo_means, Group == "Composite" & Season == "Winter"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                        geom_errorbar(position = pd, aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE)), colour="#666666", width=0.7) +
                                        geom_line(position = pd, aes(linetype = Treatment), lwd = 0.5) +
                                        geom_point(position = pd, size = 1.4) +
                                        theme_bw() +
                                        facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                        labs(x = "Years Since Exclosure", y = "Mean Albedo") +
                                        scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                        scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                        ggtitle("Winter") +
                                        theme(
                                                legend.position = "top",
                                                legend.background = element_rect(fill = "#fafafa", color = "#e6e6e6")
                                        ) +
                                        guides(shape = F) +
                                        guides(linetype = F)

                                #Extract legend
                                shared_legend <- extract_legend(g1_l)
                                
                        #Build complex plot
                        middle_rows <- plot_grid(g1, NULL, g2, NULL, NULL, NULL, g3, NULL, g4, ncol = 3, nrow = 3, rel_widths = c(0.475,0.05,0.475), rel_heights = c(0.475,0.05, 0.475))
                        final_plot <- plot_grid(shared_legend, middle_rows, ncol = 1, rel_heights = c(0.1,0.9) )
                        
                        #Save as SVG    
                        ggsave('albedo_comp_subplots_complex.svg',
                               final_plot,
                               "svg",
                               '1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots/Subplot_Resolution/Composite',
                               scale = 1.25)
                        
        
                
                #"STANDARD" FACETED PLOTS
                                
                        #ERROR BARS FOR SE
                        ggplot(subset(albedo_means, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment)) +
                                geom_errorbar(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE)), colour="#666666", width=0.55, position = pd) +
                                geom_line(position = pd, aes(color = Treatment), lwd = 0.5) +
                                geom_point(position = pd, aes(shape = Treatment, color = Treatment), size = 1) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                scale_y_continuous(limits = c(0.14, 0.457), breaks = c(0.15,0.2,0.25,0.3,0.35,0.4,0.45)) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        
                                #Export manually
                        
                        #SHADING FOR SE
                        ggplot(subset(albedo_means, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.3, lwd = 0) +
                                geom_point(position = pd, aes(shape = Treatment, color = Treatment), size = 0.6) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                scale_y_continuous(limits = c(0.14, 0.457), breaks = c(0.15,0.2,0.25,0.3,0.35,0.4,0.45)) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                ) +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(fill = F)
                        
                        
                        
 
                        
     
                        
                        
                        
                        
                        
                      
                        
                        
                        
                        
                        
                        
                        
        #EXPLORE/FACET BY REGION (AS A PROXY FOR MOOSE DENSITY) --------------  
                        
                #Calculate means within regions
                        
                        albedo_comp <- final_albedo[final_albedo$Group == "Composite",]
                        
                        albedo_reg <- aggregate(albedo_comp$Albedo, by = list("Treatment" = albedo_comp$Treatment,
                                                                               "Region" = albedo_comp$Region,
                                                                               "Years_Since_Exclosure" = albedo_comp$Years_Since_Exclosure,
                                                                               "Month" = albedo_comp$Month), FUN = mean)
                        colnames(albedo_reg)[5] <- "Mean_Subplot_Albedo"
                        
                        #Calculate standard error for each mean
                        
                                #Add placeholder columns
                                albedo_reg$SE <- as.numeric('')
                                
                                #Calculate SEs for each species/group, month, and year
                                for(i in 1:nrow(albedo_reg)){
                                        
                                        #Get variables
                                        tr <- albedo_reg[i, "Treatment"]
                                        reg <- albedo_reg[i, "Region"]
                                        yse <- albedo_reg[i, "Years_Since_Exclosure"]
                                        mt <- albedo_reg[i, "Month"]
                                        gr <- "Composite"
                                        
                                        #Calculate SE for albedo
                                        se <- std(final_albedo$Albedo[final_albedo$Treatment == tr &
                                                                              final_albedo$Region == reg &
                                                                              final_albedo$Group == gr &
                                                                              final_albedo$Years_Since_Exclosure == yse &
                                                                              final_albedo$Month == mt])
                                        
                                        #Add to df
                                        albedo_reg[i, "SE"] <- se
                                        
                                }
                                
                        #Add a 'season' variable for further grouping
                        
                        #Placeholder column
                        albedo_reg$Season <- as.character('')
                        
                        #Conditions
                        albedo_reg$Season[albedo_reg$Month %in% c(1:3)] <- "Winter"
                        albedo_reg$Season[albedo_reg$Month %in% c(4:6)] <- "Spring"
                        albedo_reg$Season[albedo_reg$Month %in% c(7:9)] <- "Summer"
                        albedo_reg$Season[albedo_reg$Month %in% c(10:12)] <- "Autumn"
                        
                        #Set as factor
                        albedo_reg$Region <- as.factor(albedo_reg$Region)
                        albedo_reg$Season <- as.factor(albedo_reg$Season)
                        
                        #Set month "nice names"
                        albedo_reg$Month_Name <- month.abb[albedo_reg$Month]
                        
                        
                        
                        
        
                #Winter months, faceted by region ------------
                        pd = position_dodge(0.35)
                        
                        ggplot(data = subset(albedo_reg, Month == 1), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, group = Treatment)) +
                                geom_errorbar(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE)), colour="#666666", width=0.3, position = pd) +
                                geom_line(position = pd, aes(color = Treatment), lwd = 0.5) +
                                geom_point(position = pd, aes(shape = Treatment, color = Treatment), size = 1) +
                                theme_bw() +
                                facet_wrap(~Region, ncol = 1) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                #scale_y_continuous(limits = c(0.13, 0.425), breaks = c(0.15,0.2,0.25,0.3,0.35,0.4)) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        plot.title = element_text(hjust = 0.5)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F) +
                                ggtitle("January Albedo")
                        
                        
                                        # NOTE: Hedmark has unexpected albedo in early years of succession
                                        # I believe this is due to presence of large spruce in unbrowsed sites,
                                        # as well as few sites early on (most available tree data for Hedmark sites
                                        # starts 3 years after exclosure)
                                
                        
                        
                        
                                
#END PLOT ALBEDO AT SUBPLOT LEVEL ---------------------------------------------------------------------------------

                        
                        
                        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                        
                        
                        
                        
#CALCULATE DELTA MEAN ALBEDO + SE AT PLOT LEVEL ---------------------------------------------------------------------------------
                        
        # NOTE: This section has seven steps:
                        
                #(1) Calculate a 'mean albedo' value for each plot (i.e. LocalityCode) - for all groups (composite, pine
                # deciduous, spruce) across all months and years of data.
                        
                #(2) At each study location (i.e. LocalityName), calculate DELTA ALBEDO between exclosure and open plots. 
                        
                #(3) Calculate mean values across all "years since exclosure" and months for DELTA ALBEDO (between excl./open plots)
                        
                #(4) Calculate standard error for each of these mean values
                        
                #(5) Plot mean delta albedo values w/ SE
                        
                #(6) Plot mean delta albedo values w/ SE by region
                        
                #(7) Plots of delta albedo vs herbivore densities at each LocalityCode
                        

        # The figure resulting from this data should look similar to Figure 6 in Cherubini et al. (2017) - delta albedo on Y-axis,
        # and years since exclosure on X-axis (faceted by month).
                        
        # The goal here is to produce a figure that allows us to nicely visualize the treatment effect of exclosure on albedo (since
        # the other albedo plots don't show the treatment effect well - small difference between treatments but range of values makes
        # plotting difficult). This will also allow us to visualize the individual species-specific albedos together with the composite albedos.
                        
        # Similar to the earlier steps, climate data is ONE SET OF VALUES (averages from climate data at all
        # sites across the entire study period - 2009-2019)
        
                        
        #STEP 1 -------------
        
                #CALCULATE AVERAGE ALBEDO PER SPECIES PER PLOT
                plot_means <- aggregate(final_albedo$Albedo, by = list("LocalityName" = final_albedo$LocalityName,
                                                                       "LocalityCode" = final_albedo$LocalityCode,
                                                                       "Treatment" = final_albedo$Treatment,
                                                                       "Group" = final_albedo$Group,
                                                                       "Years_Since_Exclosure" = final_albedo$Years_Since_Exclosure,
                                                                       "Month" = final_albedo$Month), FUN = mean)
                colnames(plot_means)[7] <- "Avg_Plot_Albedo"               
                
                
                
                
        #STEP 2 -------------
                
                #CALCULATE DELTA ALBEDO AT EACH SITE
                #At each study site, calculate difference in mean plot albedo between exclosure and open plot
                plot_diff <- aggregate(plot_means$Avg_Plot_Albedo, by = list("LocalityName" = plot_means$LocalityName,
                                                                             "Group" = plot_means$Group,
                                                                             "Years_Since_Exclosure" = plot_means$Years_Since_Exclosure,
                                                                             "Month" = plot_means$Month), FUN = diff)
                colnames(plot_diff)[5] <- "Mean_Albedo_Diff"
                
                #Fix numeric(0) list error
                plot_diff$Mean_Albedo_Diff <- as.numeric(plot_diff$Mean_Albedo_Diff)
                
                
        
        #STEP 3 ------------
                
                #CALCULATE MEAN DELTA ALBEDO
                        
                        #Remove NA values to allow aggregation
                        plot_diff <- plot_diff[!is.na(plot_diff$Mean_Albedo_Diff),]
                        
                        #Aggregate means
                        diff_means <- aggregate(plot_diff$Mean_Albedo_Diff, by = list("Group" = plot_diff$Group,
                                                                                      "Years_Since_Exclosure" = plot_diff$Years_Since_Exclosure,
                                                                                      "Month" = plot_diff$Month), FUN = mean)
                        
                        colnames(diff_means)[4] <- "Mean_Albedo_Diff"
                        
                        
        #STEP 4 ------------
                        
                #Add placeholder columns
                diff_means$SE <- as.numeric('')
                
                #Calculate SEs for each species, month, and year
                for(i in min(diff_means$Years_Since_Exclosure):max(diff_means$Years_Since_Exclosure)){
                        
                        #Loop through each month
                        for(j in 1:12){
                                
                                #Birch mean albedo diff SE
                                
                                diff_means$SE[diff_means$Group == "Deciduous" & diff_means$Years_Since_Exclosure == i & diff_means$Month == j] <- std(plot_diff$Mean_Albedo_Diff[plot_diff$Years_Since_Exclosure == i & plot_diff$Group == "Deciduous" & plot_diff$Month == j])
                                
                                
                                #Spruce mean albedo diff SE
                                
                                diff_means$SE[diff_means$Group == "Spruce" & diff_means$Years_Since_Exclosure == i & diff_means$Month == j] <- std(plot_diff$Mean_Albedo_Diff[plot_diff$Years_Since_Exclosure == i & plot_diff$Group == "Spruce" & plot_diff$Month == j])
                                
                                
                                #Pine mean albedo diff SE
                                
                                diff_means$SE[diff_means$Group == "Pine" & diff_means$Years_Since_Exclosure == i & diff_means$Month == j] <- std(plot_diff$Mean_Albedo_Diff[plot_diff$Years_Since_Exclosure == i & plot_diff$Group == "Pine" & plot_diff$Month == j])
                                
                                
                                #Composite mean albedo diff SE
                                
                                diff_means$SE[diff_means$Group == "Composite" & diff_means$Years_Since_Exclosure == i & diff_means$Month == j] <- std(plot_diff$Mean_Albedo_Diff[plot_diff$Years_Since_Exclosure == i & plot_diff$Group == "Composite" & plot_diff$Month == j])
                                
                        }
                        
                }
                        
                        
        #STEP 5 -----------------------
                        
                #GENERATE PLOTS OF DELTA MEAN ALBEDO W/ SE FOR COMPOSITE VERSION
                        
                        #Wide plot
                        png(filename = "1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots/Delta_Albedo/mean_albedo_differences_comp_wide.png",
                            width = 2200,
                            height = 1200,
                            bg = "white")
                        
                        ggplot(subset(diff_means, Group %in% c("Spruce", "Pine", "Deciduous")), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff, color = Group, group = Group)) +
                                geom_hline(lwd = 1, color = "#e6e6e6", yintercept = 0) +
                                #geom_errorbar(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), colour="black", width=0.7, position = pd, alpha = 0.4) +
                                geom_line(lwd = 1.2, position = pd, aes(linetype = Group), alpha = 0.4) +
                                geom_point(size = 2.2, position = pd, aes(shape = Group), alpha = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Tree Species:") +
                                scale_x_continuous(limits = c(0,12), breaks = c(1,3,5,7,9,11)) +
                                scale_y_continuous(limits = c(-0.018, 0.012), breaks = c(-0.01, 0.00, 0.01)) +
                                scale_color_discrete(labels = c("Deciduous", "Pine", "Spruce")) +
                                geom_errorbar(data = subset(diff_means, Group == "Composite"), aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), colour="black", width=0.7, position = pd, alpha = 1) +
                                geom_point(data = subset(diff_means, Group == "Composite"), size = 2.2, position = pd, color = "#333333") +
                                geom_line(data = subset(diff_means, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff), color = "#333333", linetype = 1, size = 1.3) +
                                theme(plot.title = element_text(hjust = 0.5, size = 50, margin = margin(t = 40, b = 40)),
                                      axis.text.x = element_text(size = 20, margin = margin(t=16)),
                                      axis.text.y = element_text(size = 22, margin = margin(r=16)),
                                      axis.title.x = element_text(size = 34, margin = margin(t=40, b = 24)),
                                      axis.title.y = element_text(size = 34, margin = margin(r=40)),
                                      strip.text.x = element_text(size = 22),
                                      legend.title = element_text(size = 28),
                                      legend.text = element_text(size = 26, margin = margin(t=10)),
                                      legend.position = "bottom") +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(color = guide_legend(override.aes = list(size = 5) ) )
                        
                        dev.off()
                        
                        
                        #Tall plot
                        png(filename = "1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots/Delta_Albedo/mean_albedo_differences_comp_tall.png",
                            width = 1800,
                            height = 1800,
                            bg = "white")
                        
                        ggplot(subset(diff_means, Group %in% c("Spruce", "Pine", "Deciduous")), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff, color = Group, group = Group)) +
                                geom_hline(lwd = 1, color = "#e6e6e6", yintercept = 0) +
                                #geom_errorbar(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), colour="black", width=0.7, position = pd, alpha = 0.4) +
                                geom_line(lwd = 1.2, position = pd, aes(linetype = Group), alpha = 0.4) +
                                geom_point(size = 2.2, position = pd, aes(shape = Group), alpha = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 4, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Tree Species:") +
                                scale_x_continuous(limits = c(0,12), breaks = c(1,3,5,7,9,11)) +
                                scale_y_continuous(limits = c(-0.018, 0.012), breaks = c(-0.01, 0.00, 0.01)) +
                                scale_color_discrete(labels = c("Deciduous", "Pine", "Spruce")) +
                                geom_errorbar(data = subset(diff_means, Group == "Composite"), aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), colour="black", width=0.7, position = pd, alpha = 1) +
                                geom_point(data = subset(diff_means, Group == "Composite"), size = 2.2, position = pd, color = "#333333") +
                                geom_line(data = subset(diff_means, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff), color = "#333333", linetype = 1, size = 1.3) +
                                theme(plot.title = element_text(hjust = 0.5, size = 50, margin = margin(t = 40, b = 40)),
                                      axis.text.x = element_text(size = 20, margin = margin(t=16)),
                                      axis.text.y = element_text(size = 22, margin = margin(r=16)),
                                      axis.title.x = element_text(size = 34, margin = margin(t=40, b = 24)),
                                      axis.title.y = element_text(size = 34, margin = margin(r=40)),
                                      strip.text.x = element_text(size = 22),
                                      legend.title = element_text(size = 28),
                                      legend.text = element_text(size = 26, margin = margin(t=10)),
                                      legend.position = "bottom") +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(color = guide_legend(override.aes = list(size = 5) ) )
                        
                        dev.off()
                        
                        
        
                        
        #STEP 6 ------------------------
                        
                #Get df of delta mean albedo values w/ region
                        
                        #Copy plot_diff df
                        reg_diff <- plot_diff
                        
                        #Remove NA values to allow aggregation
                        reg_diff <- reg_diff[!is.na(reg_diff$Mean_Albedo_Diff),]
                        
                        #Placeholder column
                        reg_diff$Region <- as.character('')
                        
                        #Add region data from site_data 
                        for(i in 1:nrow(reg_diff)){
                                loc <- reg_diff[i, "LocalityName"]
                                site_code <- site_data$LocalityCode[site_data$LocalityName == loc][1]
                                reg_diff[i, "Region"] <- plot_volumes$Region[plot_volumes$LocalityCode == site_code][1]
                        }
                        
                #Calculate means by region
                        
                        #Aggregate means
                        reg_diff_means <- aggregate(reg_diff$Mean_Albedo_Diff, by = list("Group" = reg_diff$Group,
                                                                                         "Region" = reg_diff$Region,
                                                                                      "Years_Since_Exclosure" = reg_diff$Years_Since_Exclosure,
                                                                                      "Month" = reg_diff$Month), FUN = mean)
                        
                        colnames(reg_diff_means)[5] <- "Mean_Albedo_Diff"
                        
                #Calculate SE
                        
                        #Add placeholder columns
                        reg_diff_means$SE <- as.numeric('')
                        
                        for(i in 1:nrow(reg_diff_means)){
                                
                                #Get variables for row i
                                gr <- reg_diff_means[i, "Group"]
                                re <- reg_diff_means[i, "Region"]
                                yse <- reg_diff_means[i, "Years_Since_Exclosure"]
                                mt <- reg_diff_means[i, "Month"]
                                
                                #Calculate SE
                                se <- std(reg_diff$Mean_Albedo_Diff[reg_diff$Group == gr &
                                                                            reg_diff$Region == re &
                                                                            reg_diff$Years_Since_Exclosure == yse &
                                                                            reg_diff$Month == mt])
                                
                                #Add to df
                                reg_diff_means[i, "SE"] <- se
                        }
                        
                
                        
                #Generate plots by region
                        
                        #Formatting for plot labels
                        reg_diff_means$Region <- as.factor(reg_diff_means$Region)
                        reg_diff_means$Month_Name <- month.abb[reg_diff_means$Month]
                        
                        #Plot w/ species-specific lines
                        png(filename = "1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots/Delta_Albedo/By_Region/mean_albedo_differences_winter_all.png",
                            width = 1800,
                            height = 1600,
                            bg = "white")
                        
                        ggplot(subset(reg_diff_means, Group %in% c("Spruce", "Pine", "Deciduous") & Month %in% c(1:3)), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff, color = Group, group = Group)) +
                                geom_hline(lwd = 1, color = "#e6e6e6", yintercept = 0) +
                                #geom_errorbar(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), colour="black", width=0.7, position = pd, alpha = 0.4) +
                                geom_line(lwd = 1.2, position = pd, aes(linetype = Group), alpha = 0.4) +
                                geom_point(size = 2.2, position = pd, aes(shape = Group), alpha = 0.4) +
                                theme_bw() +
                                facet_grid(Region~Month_Name) +
                                labs(x = "Years Since Exclosure", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Tree Species:") +
                                scale_x_continuous(limits = c(0,12), breaks = c(1,3,5,7,9,11)) +
                                #scale_y_continuous(limits = c(-0.018, 0.012), breaks = c(-0.01, 0.00, 0.01)) +
                                scale_color_discrete(labels = c("Deciduous", "Pine", "Spruce")) +
                                geom_errorbar(data = subset(reg_diff_means, Group == "Composite" & Month %in% c(1:3)), aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), colour="black", width=0.7, position = pd, alpha = 1) +
                                geom_point(data = subset(reg_diff_means, Group == "Composite" & Month %in% c(1:3)), size = 2.2, position = pd, color = "#333333") +
                                geom_line(data = subset(reg_diff_means, Group == "Composite" & Month %in% c(1:3)), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff), color = "#333333", linetype = 1, size = 1.3) +
                                theme(plot.title = element_text(hjust = 0.5, size = 50, margin = margin(t = 40, b = 40)),
                                      axis.text.x = element_text(size = 20, margin = margin(t=16)),
                                      axis.text.y = element_text(size = 22, margin = margin(r=16)),
                                      axis.title.x = element_text(size = 34, margin = margin(t=40, b = 24)),
                                      axis.title.y = element_text(size = 34, margin = margin(r=40)),
                                      strip.text.x = element_text(size = 22),
                                      strip.text.y = element_text(size = 22),
                                      legend.title = element_text(size = 28),
                                      legend.text = element_text(size = 26, margin = margin(t=10)),
                                      legend.position = "bottom") +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(color = guide_legend(override.aes = list(size = 5) ) )
                        
                        dev.off()
                        
                        
                        #Plot w/ composite only
                        png(filename = "1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots/Delta_Albedo/By_Region/mean_albedo_differences_winter_comp.png",
                            width = 1800,
                            height = 1600,
                            bg = "white")
                        
                        ggplot(subset(reg_diff_means, Group %in% c("Spruce", "Pine", "Deciduous") & Month %in% c(1:3)), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff, color = Group, group = Group)) +
                                geom_hline(lwd = 1, color = "#e6e6e6", yintercept = 0) +
                                #geom_errorbar(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), colour="black", width=0.7, position = pd, alpha = 0.4) +
                                #geom_line(lwd = 1.2, position = pd, aes(linetype = Group), alpha = 0.4) +
                                #geom_point(size = 2.2, position = pd, aes(shape = Group), alpha = 0.4) +
                                theme_bw() +
                                facet_grid(Region~Month_Name) +
                                labs(x = "Years Since Exclosure", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Tree Species:") +
                                scale_x_continuous(limits = c(0,12), breaks = c(1,3,5,7,9,11)) +
                                #scale_y_continuous(limits = c(-0.018, 0.012), breaks = c(-0.01, 0.00, 0.01)) +
                                scale_color_discrete(labels = c("Deciduous", "Pine", "Spruce")) +
                                geom_errorbar(data = subset(reg_diff_means, Group == "Composite" & Month %in% c(1:3)), aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), colour="black", width=0.7, position = pd, alpha = 1) +
                                geom_point(data = subset(reg_diff_means, Group == "Composite" & Month %in% c(1:3)), size = 2.2, position = pd, color = "#333333") +
                                geom_line(data = subset(reg_diff_means, Group == "Composite" & Month %in% c(1:3)), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff), color = "#333333", linetype = 1, size = 1.3) +
                                theme(plot.title = element_text(hjust = 0.5, size = 50, margin = margin(t = 40, b = 40)),
                                      axis.text.x = element_text(size = 20, margin = margin(t=16)),
                                      axis.text.y = element_text(size = 22, margin = margin(r=16)),
                                      axis.title.x = element_text(size = 34, margin = margin(t=40, b = 24)),
                                      axis.title.y = element_text(size = 34, margin = margin(r=40)),
                                      strip.text.x = element_text(size = 22),
                                      strip.text.y = element_text(size = 22),
                                      legend.title = element_text(size = 28),
                                      legend.text = element_text(size = 26, margin = margin(t=10)),
                                      legend.position = "bottom") +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(color = guide_legend(override.aes = list(size = 5) ) )
                        
                        dev.off()
                       

                        
                        
        #STEP 7 ---------------------------
        
                #Add herbivore density data to 'plot_diff' dataframe
                
                        #Make copy of plot_diff df
                        plot_diff_hd <- plot_diff
                
                        #Add DistrictID/Kommune number from 'site_data' df and herbivore densities
                        #from hbiomass df
                
                                #Placeholder columns
                                plot_diff_hd$DistrictID <- as.character('')
                                plot_diff_hd$Moose_Density <- as.numeric('')
                                plot_diff_hd$Red_Deer_Density <- as.numeric('')
                                plot_diff_hd$Roe_Deer_Density <- as.numeric('')
                                
                        
                                #Loop through rows
                                for(i in 1:nrow(plot_diff_hd)){
                                        
                                        #District ID ----
                                        
                                                #Get loc name
                                                loc <- plot_diff_hd[i, "LocalityName"]
                                                
                                                #Get district ID from site_data
                                                id <- as.character(site_data$DistrictID[site_data$LocalityName == loc][1])
                                                
                                                #Format to match hbiomass2015 df (3-digit codes need 0 in front)
                                                if(nchar(id) == 3){
                                                        id <- paste("0", id, sep = "")
                                                }
                                                
                                                plot_diff_hd[i, "DistrictID"] <- id
                                                
                                        #Herbivore Densities ---
                                                
                                                plot_diff_hd[i, "Moose_Density"] <- hbiomass2015$`hbiomass$Ms_2015`[hbiomass2015$kommnnr == id]
                                                plot_diff_hd[i, "Red_Deer_Density"] <- hbiomass2015$`hbiomass$Rd__2015`[hbiomass2015$kommnnr == id]
                                                plot_diff_hd[i, "Roe_Deer_Density"] <- hbiomass2015$`hbiomass$R_d_2015`[hbiomass2015$kommnnr == id]
                                                
                                }
                             
                                   
                #GENERATE PLOTS ------
                
                        #RAW SCATTERPLOTS -----
                                
                                #MOOSE
                                ggplot(data = subset(plot_diff_hd, Group == "Composite"), aes(x = Moose_Density, y = Mean_Albedo_Diff)) +
                                        geom_point() +
                                        facet_wrap(~ Month)
                        
                                #RED DEER
                                ggplot(data = subset(plot_diff_hd, Group == "Composite"), aes(x = Red_Deer_Density, y = Mean_Albedo_Diff)) +
                                        geom_point() +
                                        facet_wrap(~ Month)
                        
                                #ROE DEER
                                ggplot(data = subset(plot_diff_hd, Group == "Composite"), aes(x = Roe_Deer_Density, y = Mean_Albedo_Diff)) +
                                        geom_point() +
                                        facet_wrap(~ Month)
                                
                                        #No obvious trends
                                
                                

                        
#END CALCULATE DELTA MEAN ALBEDO + SE AT PLOT LEVEL ---------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#PLOT MOOSE DENSITY VS BROWSED ALBEDO ---------------------------------------------------------------------------------
                                
        #Rationale: browsed sites represent a range of moose densities - theoretically, trends in albedo vs. 
        #MD should be visible
                                
                #Filter albedo values to 'B' plots
                browsed <- final_albedo[final_albedo$Treatment == "B",]
                                
                #Add moose density values
                                
                        #Placeholder columns
                        browsed$DistrictID <- as.character('')
                        browsed$Moose_Density <- as.numeric('')
                        browsed$Red_Deer_Density <- as.numeric('')
                        browsed$Roe_Deer_Density <- as.numeric('')
                        
                        
                        #Loop through rows
                        for(i in 1:nrow(browsed)){
                                
                                print(i)
                                
                                #District ID ----
                                
                                #Get loc name
                                loc <- browsed[i, "LocalityName"]
                                
                                #Get district ID from site_data
                                id <- as.character(site_data$DistrictID[site_data$LocalityName == loc][1])
                                
                                #Format to match hbiomass2015 df (3-digit codes need 0 in front)
                                if(nchar(id) == 3){
                                        id <- paste("0", id, sep = "")
                                }
                                
                                browsed[i, "DistrictID"] <- id
                                
                                #Herbivore Densities ---
                                
                                browsed[i, "Moose_Density"] <- hbiomass2015$`hbiomass$Ms_2015`[hbiomass2015$kommnnr == id]
                                browsed[i, "Red_Deer_Density"] <- hbiomass2015$`hbiomass$Rd__2015`[hbiomass2015$kommnnr == id]
                                browsed[i, "Roe_Deer_Density"] <- hbiomass2015$`hbiomass$R_d_2015`[hbiomass2015$kommnnr == id]
                                
                        }
                                
                #Aggregate mean albedo values by group, moose density, month, and YSE
                br_means <- aggregate(browsed$Albedo, by = list("Group" = browsed$Group,
                                                                "Moose_Density" = browsed$Moose_Density,
                                                                "Month" = browsed$Month,
                                                                "Years_Since_Exclosure" = browsed$Years_Since_Exclosure), FUN = mean)
                colnames(br_means)[5] <- 'Mean_Albedo'
                
                #Calculate rolling means
                
                        #Loop through months
                        for(i in 1:12){
                                
                                #Loop through YSE
                                for(j in 1:11){
                                        
                                        #By group
                                        
                                                #Composite
                                                br_means$Rolling_Mean_Albedo[br_means$Group == "Composite" &
                                                                                     br_means$Month == i &
                                                                                     br_means$Years_Since_Exclosure == j]  <- rollmean(br_means$Mean_Albedo[br_means$Group == "Composite" &
                                                                                                                                                                    br_means$Month == i &
                                                                                                                                                                    br_means$Years_Since_Exclosure == j], 5, align = "right", na.pad = T)
                                        
                                                #Deciduous
                                                br_means$Rolling_Mean_Albedo[br_means$Group == "Deciduous" &
                                                                                     br_means$Month == i &
                                                                                     br_means$Years_Since_Exclosure == j]  <- rollmean(br_means$Mean_Albedo[br_means$Group == "Deciduous" &
                                                                                                                                                                    br_means$Month == i &
                                                                                                                                                                    br_means$Years_Since_Exclosure == j], 5, align = "right", na.pad = T)
                                        
                                                #Pine
                                                br_means$Rolling_Mean_Albedo[br_means$Group == "Pine" &
                                                                                     br_means$Month == i &
                                                                                     br_means$Years_Since_Exclosure == j]  <- rollmean(br_means$Mean_Albedo[br_means$Group == "Pine" &
                                                                                                                                                                    br_means$Month == i &
                                                                                                                                                                    br_means$Years_Since_Exclosure == j], 5, align = "right", na.pad = T)
                                        
                                                #Spruce
                                                br_means$Rolling_Mean_Albedo[br_means$Group == "Spruce" &
                                                                                     br_means$Month == i &
                                                                                     br_means$Years_Since_Exclosure == j]  <- rollmean(br_means$Mean_Albedo[br_means$Group == "Spruce" &
                                                                                                                                                                    br_means$Month == i &
                                                                                                                                                                    br_means$Years_Since_Exclosure == j], 5, align = "right", na.pad = T)
                                }
                                
                        }
                
                
                #Generate rolling means plots (JAN and JUL + odd YSE only, to simplify plotting)
                
                        #Vector of YSE
                        num <- c(1,6,11)
                        
                        #Generate plot labels
                        
                                #YSE
                                years <- c("1 Year", "6 Years", "11 Years")
                                yse_labs <- function(variable,value){
                                        return(years[value])
                                }
                                
                                mts <- c("Jan", "Jul")
                                jan_jul <- function(variable,value){
                                        return(mts[value])
                                }
                                
                                
                                
                        #Plot
                        ggplot(data = subset(br_means, Month %in% c(1,7) & Years_Since_Exclosure %in% num & Group != "Composite"), aes(x = Moose_Density, y = Rolling_Mean_Albedo, color = Group, group = Group)) +
                                geom_line(alpha = 0.3) +
                                geom_point(alpha = 0.3, size = 0.8) +
                                facet_grid(Years_Since_Exclosure~Month, labeller = labeller(Years_Since_Exclosure = yse_labs, Month = jan_jul)) +
                                theme_bw() +
                                geom_line(data = subset(br_means, Month %in% c(1,7) & Years_Since_Exclosure %in% num & Group == "Composite"), color = "#333333") +
                                geom_point(data = subset(br_means, Month %in% c(1,7) & Years_Since_Exclosure %in% num & Group == "Composite"), color = "#333333", size = 0.9) +
                                labs(x = "Moose Metabolic Biomass" ~(kg/km^2), y = "Mean Albedo") +
                                theme(
                                        panel.grid.minor = element_blank(),
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                )
                        
                        
                                
#END PLOT MOOSE DENSITY VS BROWSED ALBEDO ---------------------------------------------------------------------------------
                                