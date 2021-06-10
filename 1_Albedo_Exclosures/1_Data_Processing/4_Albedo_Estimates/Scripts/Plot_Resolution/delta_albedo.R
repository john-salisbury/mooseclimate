## Script to produce plots of DELTA albedo estimates (i.e. exclosure albedo - open albedo for a given LocalityName)
## for all 37 'used sites' (Trøndelag, Hedmark, Telemark) across all years of available volume data. 

## Note: to produce DELTA albedo estimates, average subplot albedo values are computed for each plot (LocalityCode) in a given
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
        site_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/all_sites_data.csv', header = TRUE)

        #Load PLOT-level albedo estimates
        plot_albedo <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plot_Resolution/mean_plot_albedo.csv", header = T)
        
        #SUBPLOT STAND VOLUMES (m3/ha) (Trondelag/Hedmark/Telemark: 2009-2019) - circular subplots within each plot/LocalityCode
        
                #Load subplot volumes
                plot_volumes <- read.csv('1_Albedo_Exclosures/1_Data_Processing/3_Volume_Estimates/Output/subplot_tree_volumes.csv', header = TRUE)
                
        
#END INITIAL DATA IMPORT --------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#CALCULATE DELTA MEAN ALBEDO + SE AT PLOT LEVEL ---------------------------------------------------------------------------------
                        
        # NOTE: This section has two steps:
                        
                #(1) At each study location (i.e. LocalityName), calculate DELTA ALBEDO between exclosure and open plots. 
                        
                #(2) Calculate/plot mean delta albedo values w/ SE (grouped by region)
                        

        # The figure resulting from this data should look similar to Figure 6 in Cherubini et al. (2017) - delta albedo on Y-axis,
        # and years since exclosure on X-axis (faceted by month).
                        
        # The goal here is to produce a figure that allows us to nicely visualize the treatment effect of exclosure on albedo (since
        # the other albedo plots don't show the treatment effect well - small difference between treatments but range of values makes
        # plotting difficult). This will also allow us to visualize the individual species-specific albedos together with the composite albedos.
                        
        # Similar to the earlier steps, climate data is ONE SET OF VALUES (averages from climate data at all
        # sites across the entire study period - 2009-2019)
        
                        
        #STEP 1 -------------
        
                #CALCULATE DELTA ALBEDO AT EACH SITE
                #At each study site, calculate difference in mean plot albedo between exclosure and open plot
                
                plot_diff <- aggregate(plot_albedo$Plot_Albedo, by = list("LocalityName" = plot_albedo$LocalityName,
                                                                             "Group" = plot_albedo$Group,
                                                                             "Years_Since_Exclosure" = plot_albedo$Years_Since_Exclosure,
                                                                             "Month" = plot_albedo$Month), FUN = diff)
                colnames(plot_diff)[5] <- "Albedo_Diff"
                
                #NOTE: At LocalityNames where a species appears in one treatment but not the other,
                #a numeric(0) error appears. Instead of setting this to 0, it should be set as NA 
                #(since a difference can't actually be calculated)
                
                        plot_diff$Albedo_Diff <- as.character(plot_diff$Albedo_Diff)
                        plot_diff$Albedo_Diff[plot_diff$Albedo_Diff == "numeric(0)"] <- NA
                        plot_diff$Albedo_Diff <- as.numeric(plot_diff$Albedo_Diff)
                        
                
        
        #STEP 2 ------------
                
                
                #Get df of delta mean albedo values w/ region
                        
                        #Copy plot_diff df
                        reg_diff <- plot_diff
                        
                        #Remove NA values to allow aggregation
                        reg_diff <- reg_diff[!is.na(reg_diff$Albedo_Diff),]
                        
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
                        reg_diff_means <- aggregate(reg_diff$Albedo_Diff, by = list("Group" = reg_diff$Group,
                                                                                         "Region" = reg_diff$Region,
                                                                                      "Years_Since_Exclosure" = reg_diff$Years_Since_Exclosure,
                                                                                      "Month" = reg_diff$Month), FUN = mean)
                        
                        colnames(reg_diff_means)[5] <- "Mean_Albedo_Diff"
                        
                #Calculate SE
                        
                        #Define function
                        std <- function(x) sd(x)/sqrt(length(x))
                        
                        #Add placeholder columns
                        reg_diff_means$SE <- as.numeric('')
                        
                        for(i in 1:nrow(reg_diff_means)){
                                
                                #Get variables for row i
                                gr <- reg_diff_means[i, "Group"]
                                re <- reg_diff_means[i, "Region"]
                                yse <- reg_diff_means[i, "Years_Since_Exclosure"]
                                mt <- reg_diff_means[i, "Month"]
                                
                                #Calculate SE
                                se <- std(reg_diff$Albedo_Diff[reg_diff$Group == gr &
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
                        first <- paste("*Negative values of ", delta_sym, " albedo indicate higher albedo in", sep = "")
                        sec <- "open plots relative to corresponding exclosures"
                        lab <- paste(first, sec, sep = "\n")
                        text_annotation <- text_grob(lab, face = "italic", color = "#333333", size = 9)
                        
                        #Position
                        pd <- position_dodge(0)
                        
                        #REPLACE REGION LABELS WITH COUNTY LABELS -----
                        reg_filt$Counties[reg_filt$Region == "Trøndelag"] <- "Trøndelag"
                        reg_filt$Counties[reg_filt$Region == "Hedmark"] <- "Innlandet and Viken"
                        reg_filt$Counties[reg_filt$Region == "Telemark"] <- "Vestfold and Telemark"
                        
                        
                        #Complex plot
                        g1 <- ggplot(data = subset(reg_filt, Group == "Composite"), aes(x = Month, y = Mean_Albedo_Diff, group = Years_Since_Exclosure, color = Years_Since_Exclosure, fill = Years_Since_Exclosure)) +
                                        geom_hline(yintercept = 0, linetype = 2, color = "#666666") +
                                        geom_ribbon(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), alpha = 0.15, lwd = 0) +
                                        geom_point(size = 1.8, position = pd) +
                                        geom_line(position = pd, alpha = 0.8) +
                                        labs(x = "Month", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Years Since Exclosure:", fill = "Years Since Exclosure:", shape = "Years Since Exclosure:") +
                                        facet_wrap(~ Counties, ncol = 3) +
                                        scale_x_continuous(breaks = c(1:12)) +
                                        scale_y_continuous(limits = c(-0.018, 0.01)) +
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
                        
                        stacked <- plot_grid(NULL, text_annotation, NULL, g1, ncol = 1, rel_heights = c(0.05, 0.1, 0.05, 0.8))
                        stacked
                        
                        
                        
                        #COMPLEX STACKED PLOT WITH DIFFERENT SPECIES
                        
                                #pal <- c("#fcae12", "#e9602c", '#b0315a', '#2f0c5b')
                        
                                #Composite - top-left
                                g1 <- ggplot(data = subset(reg_filt, Group == "Composite"), aes(x = Month, y = Mean_Albedo_Diff, group = Years_Since_Exclosure, color = Years_Since_Exclosure, fill = Years_Since_Exclosure)) +
                                                geom_ribbon(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), alpha = 0.15, lwd = 0) +
                                                geom_point(size = 1.3, position = pd) +
                                                geom_line(position = pd, alpha = 0.8) +
                                                labs(x = "Month", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Years Since Exclosure:", fill = "Years Since Exclosure:", shape = "Years Since Exclosure:") +
                                                facet_wrap(~Counties, ncol = 3) +
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
                                       '1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plot_Resolution/Plots',
                                       scale = 1.25)

                        
        
        #VARIATION OF COMPLEX PLOT ABOVE

                #Hedmark - top-left
                g1 <- ggplot(data = subset(reg_filt, Region == "Hedmark"), aes(x = Month, y = Mean_Albedo_Diff, group = Years_Since_Exclosure, color = Years_Since_Exclosure, fill = Years_Since_Exclosure)) +
                        geom_hline(yintercept = 0, linetype = 2, color = "#666666") +
                        geom_ribbon(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), alpha = 0.15, lwd = 0) +
                        geom_point(size = 1.3, position = pd) +
                        geom_line(position = pd, alpha = 0.8) +
                        labs(x = "Month", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Years Since Exclosure:", fill = "Years Since Exclosure:", shape = "Years Since Exclosure:") +
                        facet_wrap(~Group, ncol = 4) +
                        ggtitle("(a) Innlandet and Viken") +
                        scale_x_continuous(breaks = c(1:12)) +
                        scale_y_continuous(limits = c(-0.025, 0.013)) +
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
                
                
                
                #Trøndelag - bottom-left
                g2 <- ggplot(data = subset(reg_filt, Region == "Trøndelag"), aes(x = Month, y = Mean_Albedo_Diff, group = Years_Since_Exclosure, color = Years_Since_Exclosure, fill = Years_Since_Exclosure)) +
                        geom_hline(yintercept = 0, linetype = 2, color = "#666666") +
                        geom_ribbon(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), alpha = 0.15, lwd = 0) +
                        geom_point(size = 1.3, position = pd) +
                        geom_line(position = pd, alpha = 0.8) +
                        labs(x = "Month", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Years Since Exclosure:", fill = "Years Since Exclosure:", shape = "Years Since Exclosure:") +
                        facet_wrap(~Group, ncol = 4) +
                        ggtitle("(b) Trøndelag") +
                        scale_x_continuous(breaks = c(1:12)) +
                        scale_y_continuous(limits = c(-0.025, 0.013)) +
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
                
                
                #Telemark
                g3 <- ggplot(data = subset(reg_filt, Region == "Telemark"), aes(x = Month, y = Mean_Albedo_Diff, group = Years_Since_Exclosure, color = Years_Since_Exclosure, fill = Years_Since_Exclosure)) +
                        geom_hline(yintercept = 0, linetype = 2, color = "#666666") +
                        geom_ribbon(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), alpha = 0.15, lwd = 0) +
                        geom_point(size = 1.3, position = pd) +
                        geom_line(position = pd, alpha = 0.8) +
                        labs(x = "Month", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Years Since Exclosure:", fill = "Years Since Exclosure:", shape = "Years Since Exclosure:") +
                        facet_wrap(~Group, ncol = 4) +
                        ggtitle("(c) Vestfold and Telemark") +
                        scale_x_continuous(breaks = c(1:12)) +
                        scale_y_continuous(limits = c(-0.025, 0.013)) +
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
                
                
                #Legend function
                extract_legend <- function(my_ggp) {
                        step1 <- ggplot_gtable(ggplot_build(my_ggp))
                        step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
                        step3 <- step1$grobs[[step2]]
                        return(step3)
                }
                
                #Additional graph to extract legend from
                
                g1_l <- ggplot(data = subset(reg_filt, Region == "Trøndelag"), aes(x = Month, y = Mean_Albedo_Diff, group = Years_Since_Exclosure, color = Years_Since_Exclosure, fill = Years_Since_Exclosure)) +
                                geom_ribbon(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), alpha = 0.15, lwd = 0) +
                                geom_point(size = 1.3, position = pd) +
                                geom_line(position = pd, alpha = 0.8) +
                                labs(x = "Month", y = expression(Delta*' Albedo (Excl. - Open)'), color = "Years Since Exclosure:", fill = "Years Since Exclosure:", shape = "Years Since Exclosure:") +
                                facet_wrap(~Group, ncol = 4) +
                                ggtitle("(c) Trøndelag") +
                                scale_x_continuous(breaks = c(1:12)) +
                                scale_y_continuous(limits = c(-0.031, 0.031)) +
                                scale_color_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                scale_fill_manual(labels = c("2 yrs", "4 yrs", "6 yrs", "8 yrs", "10 yrs"), values = plot_pal) +
                                theme_bw() +
                                theme(
                                        legend.position = "bottom",
                                        legend.box.just = "center",
                                        legend.background = element_rect(fill="#fafafa",
                                                                         size=0.1, linetype="solid", 
                                                                         colour ="#666666"),
                                        axis.title.x = element_text(margin = margin(t = 4)),
                                        axis.title.y = element_text(margin = margin(r = 4)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0, face = "bold", margin = margin(b = 6), size = 12),
                                        axis.text.x = element_text(size = 6),
                                        axis.text.y = element_text(size = 8)
                                )  +
                                guides(color=guide_legend(nrow=1,byrow=TRUE))
                g1_l

                #Extract legend
                shared_legend <- extract_legend(g1_l)
                
                #Generate complex plot
                delta_sym <- '\U0394'
                first <- paste("*Negative values of ", delta_sym, " albedo indicate higher albedo in", sep = "")
                sec <- "open plots relative to corresponding exclosures"
                lab <- paste(first, sec, sep = "\n")
                text_annotation <- text_grob(lab, face = "italic", color = "#333333", size = 9)
                complex_plot <- plot_grid(text_annotation, NULL, g1, NULL, g2, NULL, g3, NULL, shared_legend, ncol = 1, rel_heights = c(0.05, 0.025, 0.275, 0.025, 0.275, 0.025, 0.275, 0.025, 0.05))
                complex_plot
                
                #EXPORT @ 800x900px
               
                
                
       
                                

                        
#END CALCULATE DELTA MEAN ALBEDO + SE AT PLOT LEVEL ---------------------------------------------------------------------------------
                
                