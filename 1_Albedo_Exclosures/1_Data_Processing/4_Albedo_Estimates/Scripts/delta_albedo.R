## Script to produce plots of DELTA albedo estimates (i.e. exclosure albedo - open albedo for a given LocalityName)
## for all 37 'used sites' (Trøndelag, Hedmark, Telemark) across all years of available volume data. 

## Note: to produce DELTA albedo estimates, average albedo values are computed for each plot (LocalityCode) in a given
## LocalityName (i.e. study site). This allows for direct comparison and computation of difference in albedo

## Note: original subplot albedo estimates are produced using average T and SWE data for EACH SITE
## (produced by taking mean of climate data across all years of available SustHerb tree data
## within each site)

## Note: I chose to include herbivore density data from 2015 in the final analysis, as this represents
## a nice temporal midpoint in the 2009-2019 tree data



##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(ggpubr)
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

        #Get 'cleaned' site data from adjacent 'Sites' folder
        site_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/cleaned_data.csv', header = TRUE)

        #Load subplot volumes
        plot_volumes <- read.csv('1_Albedo_Exclosures/1_Data_Processing/3_Volume_Estimates/Output/subplot_tree_volumes.csv', header = TRUE)
        
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
                
                #NOTE: At LocalityNames where a species appears in one treatment but not the other,
                #a numeric(0) error appears. Instead of setting this to 0, it should be set as NA 
                #(since a difference can't actually be calculated)
                
                        plot_diff$Mean_Albedo_Diff <- as.character(plot_diff$Mean_Albedo_Diff)
                        plot_diff$Mean_Albedo_Diff[plot_diff$Mean_Albedo_Diff == "numeric(0)"] <- NA
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
                        
                #Define function
                std <- function(x) sd(x)/sqrt(length(x))
                        
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
                        
                #Palette
                pal <- wes_palette("Darjeeling1")
                
                #Set strip text labels
                months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
                month_labs <- function(variable,value){
                        return(months[value])
                }
                
                pd <- position_dodge(0.5)
                
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
                        
                #Spaghetti plot, colored by region
                        
                        ggplot(data = subset(reg_diff, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff, color = Region, group = LocalityName)) +
                                geom_line(alpha = 0.8) +
                                facet_wrap(~Month, labeller = month_labs, ncol = 6) +
                                scale_x_continuous(limits = c(0,12), breaks = c(2,4,6,8,10)) +
                                labs(x = "Years Since Exclosure", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Region:") +
                                scale_color_manual(values = pal) +
                                theme_bw() +
                                theme(
                                        legend.position = "bottom",
                                        panel.grid.minor = element_blank(),
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                )
                        
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
                        
                        reg_filt <- reg_diff_means[reg_diff_means$Years_Since_Exclosure %in% c(2,4,6,8,10),]
                        
                        plot_pal <- c("#f4e61e",
                                      "#65cb5e",
                                      "#22928c",
                                      "#38598b",
                                      "#440e57")
                        
                        #SINGLE COMPOSITE PLOT W/ TEXT ANNOTATION
                        reg_filt$Years_Since_Exclosure <- as.factor(reg_filt$Years_Since_Exclosure)
                        
                        #Text label
                        delta_sym <- '\U0394'
                        first <- paste("*Positive values of ", delta_sym, " albedo indicate higher albedo in", sep = "")
                        sec <- "exclosures relative to corresponding open plots"
                        lab <- paste(first, sec, sep = "\n")
                        text_annotation <- text_grob(lab, face = "italic", color = "#333333", size = 9)
                        
                        g1 <- ggplot(data = subset(reg_filt, Group == "Composite"), aes(x = Month, y = Mean_Albedo_Diff, group = Years_Since_Exclosure, color = Years_Since_Exclosure, fill = Years_Since_Exclosure)) +
                                        geom_ribbon(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), alpha = 0.15, lwd = 0) +
                                        geom_point(size = 1.8, position = pd) +
                                        geom_line(position = pd, alpha = 0.8) +
                                        labs(x = "Month", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Years Since Exclosure:", fill = "Years Since Exclosure:", shape = "Years Since Exclosure:") +
                                        facet_wrap(~Region, ncol = 3) +
                                        scale_x_continuous(breaks = c(1:12)) +
                                        scale_y_continuous(limits = c(-0.031, 0.031)) +
                                        scale_color_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                        scale_fill_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                        theme_bw() +
                                        theme(
                                                legend.background = element_rect(fill="#fafafa",
                                                                                 size=0.1, linetype="solid", 
                                                                                 colour ="#666666"),
                                                legend.position = "bottom",
                                                axis.title.x = element_text(margin = margin(t = 10)),
                                                axis.title.y = element_text(margin = margin(r = 10)),
                                                panel.grid.minor = element_blank()
                                        )
                        g1
                        
                        stacked <- plot_grid(NULL, text_annotation, NULL, g1, ncol = 1, rel_heights = c(0.01, 0.05, 0.025, 0.915))
                        stacked
                        
                        
                        
                        #COMPLEX STACKED PLOT WITH DIFFERENT SPECIES
                        
                                #pal <- c("#fcae12", "#e9602c", '#b0315a', '#2f0c5b')
                        
                                #Composite - top-left
                                g1 <- ggplot(data = subset(reg_filt, Group == "Composite"), aes(x = Month, y = Mean_Albedo_Diff, group = Years_Since_Exclosure, color = Years_Since_Exclosure, fill = Years_Since_Exclosure)) +
                                                geom_ribbon(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), alpha = 0.15, lwd = 0) +
                                                geom_point(size = 1.3, position = pd) +
                                                geom_line(position = pd, alpha = 0.8) +
                                                labs(x = "Month", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Years Since Exclosure:", fill = "Years Since Exclosure:", shape = "Years Since Exclosure:") +
                                                facet_wrap(~Region, ncol = 3) +
                                                ggtitle("(a) Composite Albedo") +
                                                scale_x_continuous(breaks = c(1:12)) +
                                                scale_y_continuous(limits = c(-0.031, 0.031)) +
                                                scale_color_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                                scale_fill_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                                theme_bw() +
                                                theme(
                                                        legend.position = "none",
                                                        axis.title.x = element_text(margin = margin(t = 4)),
                                                        axis.title.y = element_text(margin = margin(r = 4)),
                                                        panel.grid.minor = element_blank(),
                                                        plot.title = element_text(hjust = 0, face = "bold", margin = margin(b = 6), size = 12),
                                                        axis.text.x = element_text(size = 6),
                                                        axis.text.y = element_text(size = 8)
                                                ) 
                                g1
                                
                                #Spruce - top-right
                                g2 <- ggplot(data = subset(reg_filt, Group == "Spruce"), aes(x = Month, y = Mean_Albedo_Diff, group = Years_Since_Exclosure, color = Years_Since_Exclosure, fill = Years_Since_Exclosure)) +
                                                geom_ribbon(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), alpha = 0.15, lwd = 0) +
                                                geom_point(size = 1.3, position = pd) +
                                                geom_line(position = pd, alpha = 0.8) +
                                                labs(x = "Month", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Years Since Exclosure:", fill = "Years Since Exclosure:", shape = "Years Since Exclosure:") +
                                                facet_wrap(~Region, ncol = 3) +
                                                ggtitle("(b) Spruce Albedo") +
                                                scale_x_continuous(breaks = c(1:12)) +
                                                scale_y_continuous(limits = c(-0.031, 0.031)) +
                                                scale_color_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                                scale_fill_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                                theme_bw() +
                                                theme(
                                                        legend.position = "none",
                                                        axis.title.x = element_text(margin = margin(t = 4)),
                                                        axis.title.y = element_text(margin = margin(r = 4)),
                                                        panel.grid.minor = element_blank(),
                                                        plot.title = element_text(hjust = 0, face = "bold", margin = margin(b = 6), size = 12),
                                                        axis.text.x = element_text(size = 6),
                                                        axis.text.y = element_text(size = 8)
                                                )
                                g2
                                
                                
                                #Pine - bottom-left
                                g3 <- ggplot(data = subset(reg_filt, Group == "Pine"), aes(x = Month, y = Mean_Albedo_Diff, group = Years_Since_Exclosure, color = Years_Since_Exclosure, fill = Years_Since_Exclosure)) +
                                        geom_ribbon(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), alpha = 0.15, lwd = 0) +
                                        geom_point(size = 1.3, position = pd) +
                                        geom_line(position = pd, alpha = 0.8) +
                                        labs(x = "Month", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Years Since Exclosure:", fill = "Years Since Exclosure:", shape = "Years Since Exclosure:") +
                                        facet_wrap(~Region, ncol = 3) +
                                        ggtitle("(c) Pine Albedo") +
                                        scale_x_continuous(breaks = c(1:12)) +
                                        scale_y_continuous(limits = c(-0.031, 0.031)) +
                                        scale_color_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                        scale_fill_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                        theme_bw() +
                                        theme(
                                                legend.position = "none",
                                                axis.title.x = element_text(margin = margin(t = 4)),
                                                axis.title.y = element_text(margin = margin(r = 4)),
                                                panel.grid.minor = element_blank(),
                                                plot.title = element_text(hjust = 0, face = "bold", margin = margin(b = 6), size = 12),
                                                axis.text.x = element_text(size = 6),
                                                axis.text.y = element_text(size = 8)
                                        )
                                g3
                        
                                
                                #Deciduous - bottom-right
                                g4 <- ggplot(data = subset(reg_filt, Group == "Deciduous"), aes(x = Month, y = Mean_Albedo_Diff, group = Years_Since_Exclosure, color = Years_Since_Exclosure, fill = Years_Since_Exclosure)) +
                                        geom_ribbon(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), alpha = 0.15, lwd = 0) +
                                        geom_point(size = 1.3, position = pd) +
                                        geom_line(position = pd, alpha = 0.8) +
                                        labs(x = "Month", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Years Since Exclosure:", fill = "Years Since Exclosure:", shape = "Years Since Exclosure:") +
                                        facet_wrap(~Region, ncol = 3) +
                                        ggtitle("(d) Deciduous Albedo") +
                                        scale_x_continuous(breaks = c(1:12)) +
                                        scale_y_continuous(limits = c(-0.031, 0.031)) +
                                        scale_color_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                        scale_fill_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                        theme_bw() +
                                        theme(
                                                legend.position = "none",
                                                axis.title.x = element_text(margin = margin(t = 4)),
                                                axis.title.y = element_text(margin = margin(r = 4)),
                                                panel.grid.minor = element_blank(),
                                                plot.title = element_text(hjust = 0, face = "bold", margin = margin(b = 6), size = 12),
                                                axis.text.x = element_text(size = 6),
                                                axis.text.y = element_text(size = 8)
                                                
                                        ) 
                                g4
                                
                                
                                #Legend function
                                extract_legend <- function(my_ggp) {
                                        step1 <- ggplot_gtable(ggplot_build(my_ggp))
                                        step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
                                        step3 <- step1$grobs[[step2]]
                                        return(step3)
                                }
                                
                                #Additional graph to extract legend from
                                
                                g1_l <- ggplot(data = subset(reg_filt, Group == "Deciduous"), aes(x = Month, y = Mean_Albedo_Diff, group = Years_Since_Exclosure, color = Years_Since_Exclosure, fill = Years_Since_Exclosure)) +
                                        geom_ribbon(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), alpha = 0.15, lwd = 0) +
                                        geom_point(size = 1.3, position = pd) +
                                        geom_line(position = pd, alpha = 0.8) +
                                        labs(x = "Month", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Years Since Exclosure:", fill = "Years Since Exclosure:", shape = "Years Since Exclosure:") +
                                        facet_wrap(~Region, ncol = 3) +
                                        scale_x_continuous(breaks = c(1:12)) +
                                        scale_y_continuous(limits = c(-0.025, 0.031)) +
                                        scale_color_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                        scale_fill_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                        theme_bw() +
                                        theme(
                                                legend.background = element_rect(fill="#fafafa",
                                                                                 size=0.1, linetype="solid", 
                                                                                 colour ="#666666"),
                                                legend.position = "top",
                                                legend.title = element_text(size = 8),
                                                panel.grid.minor = element_blank()
                                        )

                                #Extract legend
                                shared_legend <- extract_legend(g1_l)
                                
                                #Generate complex plot
                                delta_sym <- '\U0394'
                                first <- paste("*Positive values of ", delta_sym, " albedo indicate higher albedo in", sep = "")
                                sec <- "exclosures relative to corresponding open plots"
                                lab <- paste(first, sec, sep = "\n")
                                text_annotation <- text_grob(lab, face = "italic", color = "#333333", size = 9)
                                top_row <- plot_grid(text_annotation, ncol = 1)
                                middle_row <- plot_grid(g1, NULL, g2, ncol = 3, rel_widths = c(0.49, 0.02, 0.49))
                                spacer_row <- plot_grid(NULL, ncol = 1)
                                bottom_row <- plot_grid(g3, NULL, g4, ncol = 3, rel_widths = c(0.49, 0.02, 0.49))
                                legend <- plot_grid(shared_legend, ncol = 1)
                                complex_plot <- plot_grid(top_row, spacer_row, middle_row, spacer_row, bottom_row, legend, ncol = 1, rel_heights = c(0.035, 0.025, 0.425, 0.025, 0.425, 0.065))
                                complex_plot
                                
                                #Save as SVG
                                ggsave('delta_albedo_region_group.svg',
                                       complex_plot,
                                       "svg",
                                       '1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots/Delta_Albedo',
                                       scale = 1.25)

                        
                        
       
                                

                        
#END CALCULATE DELTA MEAN ALBEDO + SE AT PLOT LEVEL ---------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#PLOT ALBEDO VS SNOW ---------------------------------------------------
        
        years <- c("2 Years", "4 Years", "6 Years", "8 Years")
        year_labs <- function(variable,value){
                return(years[value])
        }            
        
        ggplot(data = subset(final_albedo, Group == "Composite" & Years_Since_Exclosure %in% c(2,4,6,8)), aes(x = SWE_mm, y = Albedo, color = Treatment, group = Treatment)) +
                geom_point(alpha = 0.3, aes(shape = Region)) +
                geom_smooth(method = "loess", se = F, span = 0.6) +
                labs(x = "Snow water equivalent (mm)", y = "Albedo") +
                facet_wrap(~Years_Since_Exclosure, labeller = labeller(Years_Since_Exclosure = c(8,9,10,11))) +
                theme_bw()
                                
                                
#END PLOT ALBEDO VS SNOW ------------------------------------------------
                                
                                