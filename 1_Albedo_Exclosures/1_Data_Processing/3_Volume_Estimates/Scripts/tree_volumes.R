## Script to calculate (1) individual tree volumes (m3), and (2) tree volume by 'group' (deciduous, pine, spruce)
## for each subplot across all years of data.

## Note: this uses data for all 'used sites' (Trøndelag, Hedmark, and Telemark) through 2019.
## Volume is calculated using biomass estimates calculated from the backfitted 
## "height-only" allometric models and height class only

##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(grid)
        library(cowplot)
        library(zoo)
        library(sf)
        library(wesanderson)
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        #Import CSV to dataframe
        data <- read.csv('1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/tree_biomass.csv', header = TRUE)

#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#CALCULATE VOLUMES FOR EACH SUBPLOT ---------------------------------------------------
        
        #Calculate volume for each tree
        
                #SPECIFIC WOOD DENSITIES TO CONVERT BIOMASS TO VOLUME
                ##Note: These are average wood densities for spruce, pine, and birch - provided by Repola (2006)
                
                        #This will likely be a major source of error - according to Repola (2006), density varies widely vertically
                
                        #Spruce density (kg/m3 converted to g/cm3)
                        s_density <- 385.3 / 1000
                        
                        #Pine density (kg/m3 converted to g/cm3)
                        p_density <- 412.6 / 1000
                        
                        #Birch density (kg/m3 converted to g/cm3)
                        b_density <- 475 / 1000
                        
                #Based on species, calculate volume & assign to either birch, pine, or spruce
                        
                        #Add blank column for volume
                        data$Volume_m3 <- ''
                        
                        #Add blank column for group
                        data$Group <- ''
                        
                        #Birch -----
                        
                                #Calculate volume
                                data$Volume_m3[data$Taxa == "Betula pubescens (Bjørk)" |
                                                       data$Taxa == "Betula pendula (Lavlandbjørk)" |
                                                       data$Taxa == "Salix caprea (Selje)"] <- (data$Subplot_Total_Biomass_g[data$Taxa == "Betula pubescens (Bjørk)" |
                                                                                                                       data$Taxa == "Betula pendula (Lavlandbjørk)" |
                                                                                                                       data$Taxa == "Salix caprea (Selje)"] / b_density) / 1e06
                                #Assign group to deciduous
                                data$Group[data$Taxa == "Betula pubescens (Bjørk)" |
                                                       data$Taxa == "Betula pendula (Lavlandbjørk)" |
                                                       data$Taxa == "Salix caprea (Selje)"] <- "Deciduous"
                                
                        
                        #Spruce ------
                                
                                #Calculate volume
                                data$Volume_m3[data$Taxa == "Picea abies (Gran)" |
                                                data$Taxa == "Juniperus communis (Einer)"] <- (data$Subplot_Total_Biomass_g[data$Taxa == "Picea abies (Gran)" |
                                                                                                                      data$Taxa == "Juniperus communis (Einer)"] / s_density) / 1e06
                                
                                #Assign group to spruce
                                data$Group[data$Taxa == "Picea abies (Gran)" |
                                                       data$Taxa == "Juniperus communis (Einer)"] <- "Spruce"
                                
                                
                        #Pine -------
                                
                                #Calculate volume
                                data$Volume_m3[data$Taxa == "Pinus sylvestris (Furu)"] <- (data$Subplot_Total_Biomass_g[data$Taxa == "Pinus sylvestris (Furu)"] / p_density) / 1e06
                                
                                #Assign group to pine
                                data$Group[data$Taxa == "Pinus sylvestris (Furu)"] <- "Pine"
                                
                                
                        #Rowan (NOTE: using birch-specific density due to lack of rowan-specific density) ------
                                
                                #Calculate volume
                                data$Volume_m3[data$Taxa == "Sorbus aucuparia (Rogn)"] <- (data$Subplot_Total_Biomass_g[data$Taxa == "Sorbus aucuparia (Rogn)"] / b_density) / 1e06
                        
                                #Assign group
                                data$Group[data$Taxa == "Sorbus aucuparia (Rogn)"] <- "Deciduous"
        
                                
                #Ensure that 'Volume' column is numeric
                data$Volume_m3 <- as.numeric(data$Volume_m3)
                
                #Ensure that "Group' column is factor
                data$Group <- as.factor(data$Group)
                
        

        #Calculate plot sampling area in hectares (used to convert m3 to m3/ha) -------
                
                #Each subplot has a radius of 2m - A = pi*r^2
                subplot_area <- pi*(2^2) #12.57m2
                
                #Convert m2 to hectares (ha) - 1m2 = 0.0001 ha (divide by 10,000)
                subplot_area_ha <- subplot_area/10000
                
        
        #Aggregate volume (m3) for EACH SPECIES by subplots within main plots
                
                vol <- aggregate(data$Volume_m3, by = list(data$Region,
                                                                 data$LocalityName,
                                                                 data$LocalityCode,
                                                                 data$Treatment,
                                                                 data$Plot,
                                                                 data$Years_Since_Exclosure,
                                                                 data$Group), FUN = sum)
                
                colnames(vol) <- c("Region", "LocalityName", "LocalityCode", "Treatment", "Subplot", "Years_Since_Exclosure", "Group", "Volume_m3")
                
                #Create volume/area (m3/ha) column - divide m3 by subplot area 
                        
                        vol$Volume_m3ha <- vol$Volume_m3 / subplot_area_ha
                        
                        
                        
        #Aggregate TOTAL VOLUME (m3) by subplots within main plots
                        
                vol_total <- aggregate(data$Volume_m3, by = list(data$Region,
                                                           data$LocalityName,
                                                           data$LocalityCode,
                                                           data$Treatment,
                                                           data$Plot,
                                                           data$Years_Since_Exclosure), FUN = sum)
                
                colnames(vol_total) <- c("Region", "LocalityName", "LocalityCode", "Treatment", "Subplot", "Years_Since_Exclosure", "Volume_m3")
                
                #Create volume/area (m3/ha) column - divide m3 by subplot area 
                
                        vol_total$Volume_m3ha <- vol_total$Volume_m3 / subplot_area_ha
                
                
#END CALCULATE VOLUMES FOR EACH SUBPLOT ---------------------------------------------------
                
                
                
                
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                
                
                
                
#CALCULATE MEAN VOLUMES & SE ------------------------------------------------------
                
        #Define SE function
        std <- function(x) sd(x)/sqrt(length(x))
                
        #TOTAL MEAN SUBPLOT VOLUME ------------
                
                #Aggregate means 
                tot_means <- aggregate(vol_total$Volume_m3ha, by = list("Treatment" = vol_total$Treatment,
                                                                        "Years_Since_Exclosure" = vol_total$Years_Since_Exclosure), FUN = mean)
                
                colnames(tot_means)[3] <- "Average_vol_m3_ha"
                
                #Calculate SE
                
                        #Add placeholder columns
                        tot_means$SE <- as.numeric('')
                        
                        #Calculate SEs for each year
                        
                        for(i in 1:nrow(tot_means)){
                                
                                #Get variables
                                tr <- tot_means[i, "Treatment"]
                                yr <- tot_means[i, "Years_Since_Exclosure"]
                                
                                #Calculate SE
                                se <- std(vol_total$Volume_m3ha[vol_total$Treatment == tr & vol_total$Years_Since_Exclosure == yr])
                                
                                #Add to df
                                tot_means[i, "SE"] <- se
                        }
                        
                        
        #MEAN SUBPLOT VOLUME BY GROUP (PINE, SPRUCE, DECIDUOUS) ---------
                        
                #Aggregate means
                group_means <- aggregate(vol$Volume_m3ha, by = list("Treatment" = vol$Treatment,
                                                                    "Years_Since_Exclosure" = vol$Years_Since_Exclosure,
                                                                    "Group" = vol$Group), FUN = mean)
                colnames(group_means)[4] <- "Average_vol_m3_ha"
                
                #Calculate SE
                
                        #Add placeholder columns
                        group_means$SE <- as.numeric('')
                
                        for(i in 1:nrow(group_means)){
                                
                                #Get variables
                                tr <- group_means[i, "Treatment"]
                                yr <- group_means[i, "Years_Since_Exclosure"]
                                gr <- as.character(group_means[i, "Group"])
                                
                                #Calculate SE
                                se <- std(vol$Volume_m3ha[vol$Treatment == tr &
                                                                  vol$Years_Since_Exclosure == yr &
                                                                  vol$Group == gr])
                                
                                #Add to df
                                group_means[i, "SE"] <- se
                        }
                
                        
        #TOTAL MEAN SUBPLOT VOLUME BY REGION
                        
                #Aggregate means
                tot_reg_means <- aggregate(vol_total$Volume_m3ha, by = list("Treatment" = vol_total$Treatment,
                                                                            "Years_Since_Exclosure" = vol_total$Years_Since_Exclosure,
                                                                            "Region" = vol_total$Region), FUN = mean)
                colnames(tot_reg_means)[4] <- "Average_vol_m3_ha"
                
                #Calculate SE
                
                        #Add placeholder columns
                        tot_reg_means$SE <- as.numeric('')
                        
                        for(i in 1:nrow(tot_reg_means)){
                                
                                #Get variables
                                tr <- tot_reg_means[i, "Treatment"]
                                yr <- tot_reg_means[i, "Years_Since_Exclosure"]
                                re <- tot_reg_means[i, "Region"]
                                
                                #Calculate SE
                                se <- std(vol_total$Volume_m3ha[vol_total$Treatment == tr &
                                                                  vol_total$Years_Since_Exclosure == yr &
                                                                  vol_total$Region == re])
                                
                                #Add to df
                                tot_reg_means[i, "SE"] <- se
                        }
                
                        
        
        #MEAN SUBPLOT VOLUME BY GROUP AND REGION -----------------------
                
                #Aggregate means
                reg_means <- aggregate(vol$Volume_m3ha, by = list("Treatment" = vol$Treatment,
                                                                  "Years_Since_Exclosure" = vol$Years_Since_Exclosure,
                                                                  "Group" = vol$Group,
                                                                  "Region" = vol$Region), FUN = mean)
                colnames(reg_means)[5] <- "Average_vol_m3_ha"
                        
                #Calculate SE
                        
                        #Add placeholder columns
                        reg_means$SE <- as.numeric('')
                        
                        for(i in 1:nrow(reg_means)){
                                
                                #Get variables
                                tr <- reg_means[i, "Treatment"]
                                yr <- reg_means[i, "Years_Since_Exclosure"]
                                gr <- as.character(reg_means[i, "Group"])
                                re <- reg_means[i, "Region"]
                                
                                #Calculate SE
                                se <- std(vol$Volume_m3ha[vol$Treatment == tr &
                                                                  vol$Years_Since_Exclosure == yr &
                                                                  vol$Group == gr &
                                                                  vol$Region == re])
                                
                                #Add to df
                                reg_means[i, "SE"] <- se
                        }
                        
                        
        #DELTA SUBPLOT VOLUME BY Treatment and GROUP (complex plot clumped by region)
        #Note: This is a step to troubleshoot the weird albedo estimates we're seeing in Hedmark
        #The goal is to determine if there are any sites with huge differences in volume of the 3 species
                
                #(1) Aggregate means for each PLOT (localityCode)
                site_means <- aggregate(vol$Volume_m3ha, by = list("LocalityName" = vol$LocalityName,
                                                                   "Treatment" = vol$Treatment,
                                                                    "Years_Since_Exclosure" = vol$Years_Since_Exclosure,
                                                                    "Group" = vol$Group,
                                                                    "Region" = vol$Region), FUN = mean)
                colnames(site_means)[6] <- "Average_vol_m3_ha"
                
                #(2) For each localityName, calculate delta volume for each species in each year
                sites <- levels(as.factor(site_means$LocalityName))
                groups <- levels(as.factor(site_means$Group))
                
                #(3) Remove 3DRUB data from year 1 (no matching data in 3DRB)
                site_means <- site_means[!(site_means$LocalityName == "drangedal3" &
                                                   site_means$Years_Since_Exclosure == 1 &
                                                   site_means$Treatment == "UB"),]
                
                #(4) Calculate delta vol at each LocalityName
                
                        #Create placeholder df
                        delta_vol <- data.frame("LocalityName" = as.character(),
                                                "Region" = as.character(),
                                                "Group" = as.character(),
                                                "Years_Since_Exclosure" = as.integer(),
                                                "Delta_Volume" = as.numeric())
                       
                        #Loop through each site and calculate delta volume for each group
                        #NOTE: can't use aggregate(), as this throws an error when a species is present in one plot
                        #but not another at a given study site
                        for(i in 1:length(sites)){
                                
                                #Get site
                                site <- sites[i]
                                
                                #Get region
                                reg <- site_means$Region[site_means$LocalityName == site][1]
                                
                                #Min and max years
                                min_year <- min(site_means$Years_Since_Exclosure[site_means$LocalityName == site])
                                max_year <- max(site_means$Years_Since_Exclosure[site_means$LocalityName == site])
                                
                                #Loop through years and calculate delta for each species
                                for(j in min_year:max_year){
                                        
                                        #Loop through each species/group
                                        for(k in 1:length(groups)){
                                                
                                                group <- groups[k]
                                                
                                                #Get exclosure volume
                                                if( length(site_means$Average_vol_m3_ha[site_means$LocalityName == site & site_means$Years_Since_Exclosure == j & site_means$Group == group & site_means$Treatment == "UB"]) > 0 ){
                                                        excl <- site_means$Average_vol_m3_ha[site_means$LocalityName == site & site_means$Years_Since_Exclosure == j & site_means$Group == group & site_means$Treatment == "UB"]
                                                } else {
                                                        
                                                        #Set volume to 0 if absent
                                                        excl <- 0
                                                }
                                                
                                                #Get open volume
                                                if( length(site_means$Average_vol_m3_ha[site_means$LocalityName == site & site_means$Years_Since_Exclosure == j & site_means$Group == group & site_means$Treatment == "B"]) > 0 ){
                                                        open <- site_means$Average_vol_m3_ha[site_means$LocalityName == site & site_means$Years_Since_Exclosure == j & site_means$Group == group & site_means$Treatment == "B"]
                                                } else {
                                                        
                                                        #Set volume to 0 if absent
                                                        open <- 0
                                                }
                                                
                                                #Calculate delta
                                                delta <- excl - open
                                                
                                                #Temporary dataframe
                                                temp <- data.frame("LocalityName" = site,
                                                                        "Region" = reg,
                                                                        "Group" = group,
                                                                        "Years_Since_Exclosure" = j,
                                                                        "Delta_Volume" = delta)
                                                
                                                delta_vol <- rbind(delta_vol, temp)
                                                
                                        }
                                        
                                }
                                
                        }
                        

                #(5) Generate plots
                        
                        #Palette
                        pal <- wes_palette("Darjeeling1")
                
                        #Hedmark
                        g1 <- ggplot(data = subset(delta_vol, Region == "Hedmark"), aes(x = Years_Since_Exclosure, y = Delta_Volume, color = Group)) +
                                        geom_point(size = 1) +
                                        geom_line() +
                                        ggtitle("Hedmark") +
                                        labs(x = "Years Since Exclosure", y = "\U0394 Volume " ~(m^3/ha) ~ "  (Excl. - Open)" ) +
                                        scale_color_manual(values = pal) +
                                        facet_wrap(~LocalityName, ncol = 5) +
                                        scale_x_continuous(limits = c(1, 11), breaks = c(2,4,6,8,10)) +
                                        scale_y_continuous(limits = c(-25,72)) +
                                        theme_bw() +
                                        theme(
                                                legend.position = "none",
                                                axis.title.x = element_text(margin = margin(t = 4)),
                                                axis.title.y = element_text(margin = margin(r = 4)),
                                                plot.title = element_text(hjust = 0.5, face = "bold")
                                        )
                        g1
                        
                        #Telemark
                        g2 <- ggplot(data = subset(delta_vol, Region == "Telemark"), aes(x = Years_Since_Exclosure, y = Delta_Volume, color = Group)) +
                                geom_point(size = 1) +
                                geom_line() +
                                ggtitle("Telemark") +
                                labs(x = "Years Since Exclosure", y = "\U0394 Volume " ~(m^3/ha) ~ "  (Excl. - Open)" ) +
                                scale_color_manual(values = pal) +
                                facet_wrap(~LocalityName, ncol = 5) +
                                scale_x_continuous(limits = c(1, 11), breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(-25,72)) +
                                theme_bw() +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 4)),
                                        axis.title.y = element_text(margin = margin(r = 4)),
                                        plot.title = element_text(hjust = 0.5, face = "bold")
                                )
                        g2
                        
                        #Trøndelag
                        g3 <- ggplot(data = subset(delta_vol, Region == "Trøndelag"), aes(x = Years_Since_Exclosure, y = Delta_Volume, color = Group)) +
                                geom_point(size = 1) +
                                geom_line() +
                                ggtitle("Trøndelag") +
                                labs(x = "Years Since Exclosure", y = "\U0394 Volume " ~(m^3/ha) ~ "  (Excl. - Open)" ) +
                                scale_color_manual(values = pal) +
                                facet_wrap(~LocalityName, ncol = 5) +
                                scale_x_continuous(limits = c(1, 11), breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(-25,72)) +
                                theme_bw() +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 4)),
                                        axis.title.y = element_text(margin = margin(r = 4)),
                                        plot.title = element_text(hjust = 0.5, face = "bold")
                                )
                        g3
                        
                        #Extract legend
                        extract_legend <- function(my_ggp) {
                                step1 <- ggplot_gtable(ggplot_build(my_ggp))
                                step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
                                step3 <- step1$grobs[[step2]]
                                return(step3)
                        }
                        
                        #Additional graph to extract legend from
                        g1_l <- ggplot(data = subset(delta_vol, Region == "Trøndelag"), aes(x = Years_Since_Exclosure, y = Delta_Volume, color = Group)) +
                                geom_point(size = 1) +
                                geom_line() +
                                ggtitle("Trøndelag") +
                                labs(x = "Years Since Exclosure", y = "\U0394 Volume " ~(m^3/ha) ~ "  (Excl. - Open)" ) +
                                scale_color_manual(values = pal) +
                                facet_wrap(~LocalityName, ncol = 5) +
                                scale_x_continuous(limits = c(1, 11), breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(-25,30)) +
                                theme_bw() +
                                theme(
                                        legend.position = "bottom",
                                        plot.title = element_text(hjust = 0.5)
                                )
                        
                        #Extract legend
                        shared_legend <- extract_legend(g1_l)
                        
                        complex <- plot_grid(g1, NULL, g2, NULL, g3, shared_legend, ncol = 1, rel_heights = c(0.23, 0.015, 0.3, 0.015, 0.3, 0.025))
                        complex
                        
                        #Save as SVG
                        ggsave('delta_volume_by_site.svg',
                               complex,
                               "svg",
                               '1_Albedo_Exclosures/1_Data_Processing/3_Volume_Estimates/Output/Plots',
                               width = 8,
                               height = 10,
                               units = "in",
                               dpi = "retina")

                        
                        
                
#END CALCULATE MEAN VOLUMES & SE ---------------------------------------------------

        


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#DATA VISUALIZATION ------------------------------------------------------------------------------------------
        
        
        #Plots ------
        plot_count_label <- c("0", "1\nn=36", "2\nn=37", "3\nn=37", "4\nn=37", "5\nn=37", "6\nn=37", "7\nn=29", "8\nn=29", "9\nn=28", "10\nn=15")
                
                #MEAN SUBPLOT VOLUME (TOTAL VOLUME) BY TREATMENT --------
                        
                        #Treatment nice names
                        tot_means$TNN[tot_means$Treatment == "B"] <- "Browsed"
                        tot_means$TNN[tot_means$Treatment == "UB"] <- "Unbrowsed"
                        
                        #Plot
                        ggplot(data = tot_means, aes(x = Years_Since_Exclosure, y = Average_vol_m3_ha, color = TNN, group = TNN)) +
                                geom_errorbar(aes(ymin = (Average_vol_m3_ha - SE), ymax = (Average_vol_m3_ha + SE)), colour="black", width=0.5, position = position_dodge(0.3)) +
                                geom_point(aes(shape = TNN), size = 1.75, position = position_dodge(0.3)) +
                                geom_line(aes(linetype = TNN)) +
                                labs(x = "Years Since Exclosure", y = "Volume "~(m^3/ha), color = "Treatment:", shape = "Treatment:", linetype = "Treatment:") +
                                scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
                                scale_color_manual(values = pal) +
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                                
                                
                #MEAN SUBPLOT VOLUME BY GROUP & TREATMENT -------
                        
                        #Treatment nice names
                        group_means$TNN[group_means$Treatment == "B"] <- "Browsed"
                        group_means$TNN[group_means$Treatment == "UB"] <- "Unbrowsed"
                        
                        #Plot
                        ggplot(data = group_means, aes(x = Years_Since_Exclosure, y = Average_vol_m3_ha, color = TNN, group = TNN)) +
                                geom_errorbar(aes(ymin = (Average_vol_m3_ha - SE), ymax = (Average_vol_m3_ha + SE)), colour="black", width=0.5, position = position_dodge(0.3)) +
                                geom_point(aes(shape = TNN), size = 1.75, position = position_dodge(0.3)) +
                                geom_line(aes(linetype = TNN)) +
                                facet_wrap(~Group, nrow = 3) +
                                labs(x = "Years Since Exclosure", y = "Volume "~(m^3/ha), color = "Treatment:", shape = "Treatment:", linetype = "Treatment:") +
                                scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
                                scale_color_manual(values = pal) +
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                        
                        
                #MEAN SUBPLOT VOLUME BY REGION & TREATMENT -------
                        
                        #Treatment nice names
                        tot_reg_means$TNN[tot_reg_means$Treatment == "B"] <- "Browsed"
                        tot_reg_means$TNN[tot_reg_means$Treatment == "UB"] <- "Unbrowsed"
                
                        #Plot
                        ggplot(data = tot_reg_means, aes(x = Years_Since_Exclosure, y = Average_vol_m3_ha, color = TNN, group = TNN)) +
                                geom_errorbar(aes(ymin = (Average_vol_m3_ha - SE), ymax = (Average_vol_m3_ha + SE)), colour="black", width=0.5, position = position_dodge(0.3)) +
                                geom_point(aes(shape = TNN), size = 1.75, position = position_dodge(0.3)) +
                                geom_line(aes(linetype = TNN)) +
                                facet_wrap(~Region, ncol = 1) +
                                labs(x = "Years Since Exclosure", y = "Volume "~(m^3/ha), color = "Treatment:", shape = "Treatment:", linetype = "Treatment:") +
                                scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
                                scale_color_manual(values = pal) +
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                                
                        
                #MEAN SUBPLOT VOLUME BY GROUP, REGION, & TREATMENT -------
                        
                        #Treatment nice names
                        reg_means$TNN[reg_means$Treatment == "B"] <- "Browsed"
                        reg_means$TNN[reg_means$Treatment == "UB"] <- "Unbrowsed"
                        
                        ggplot(data = reg_means, aes(x = Years_Since_Exclosure, y = Average_vol_m3_ha, color = TNN, group = TNN)) +
                                geom_errorbar(aes(ymin = (Average_vol_m3_ha - SE), ymax = (Average_vol_m3_ha + SE)), colour="black", width=0.5, position = position_dodge(0.3)) +
                                geom_point(aes(shape = TNN), size = 1.75, position = position_dodge(0.3)) +
                                geom_line(aes(linetype = TNN)) +
                                facet_grid(Region~Group) +
                                labs(x = "Years Since Exclosure", y = "Volume "~(m^3/ha), color = "Treatment:", shape = "Treatment:", linetype = "Treatment:") +
                                scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
                                scale_color_manual(values = pal) +
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                        
                        
        #PLOTS OF VOLUME VS MOOSE DENSITY --------------
                        
                #Only using BROWSED sites and plotting them against moose density (to see if there are any trends)
                        
                        
                        
                #SPECIES-SPECIFIC PLOTS -------
                        
                        #Filter to browsed plots
                        spec_vol <- vol[vol$Treatment == "B",]
                
                        #Filter to years in Years 1,3,5,7,9,11 (for plotting simplicity)
                        spec_vol <- spec_vol[spec_vol$Years_Since_Exclosure %in% c(1,3,5,7,9,11),]
                        
                        #Load herbivore density data & SustHerb site data to get herbivore densities for each site
                        hd <- st_read("1_Albedo_Exclosures/z_Data_Library/Herbivore_Density_Data/Usable_Data/NorwayLargeHerbivores.shp")
                        site_data <- read.csv("1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/cleaned_data.csv", header = T)
                        
                                #Placeholder columns
                                spec_vol$DistrictID <- as.character('')
                                spec_vol$Moose_Density <- as.numeric('')
                                spec_vol$Red_Deer_Density <- as.numeric('')
                                spec_vol$Roe_Deer_Density <- as.numeric('')
                                
                                #Fix stangeskovene eidskog issue
                                spec_vol$LocalityName[spec_vol$LocalityName == 'stangeskovene eidskog '] <- 'stangeskovene_eidskog'
                                
                                #Loop through rows
                                for(i in 1:nrow(spec_vol)){
                                        
                                        #District ID ----
                                        
                                        #Get loc name
                                        loc <- spec_vol[i, "LocalityName"]
                                        
                                        #Get district ID from site_data
                                        id <- as.character(site_data$DistrictID[site_data$LocalityName == loc][1])
                                        
                                        #Format to match hbiomass2015 df (3-digit codes need 0 in front)
                                        if(nchar(id) == 3){
                                                id <- paste("0", id, sep = "")
                                        }
                                        
                                        spec_vol[i, "DistrictID"] <- id
                                        
                                        #Herbivore Densities ---
                                        
                                        spec_vol[i, "Moose_Density"] <- hd$Ms_2015[hd$kommnnr == id]
                                        spec_vol[i, "Red_Deer_Density"] <- hd$Rd__2015[hd$kommnnr == id]
                                        spec_vol[i, "Roe_Deer_Density"] <- hd$R_d_2015[hd$kommnnr == id]
                                        
                                }
                                
                        #Plot of species-specific volumes vs moose density
                        ggplot(data = spec_vol, aes(x = Moose_Density, y = Volume_m3ha, color = Group)) +
                                geom_point() +
                                facet_wrap(~Years_Since_Exclosure) +
                                theme_bw()
                        
                                #SCATTERPLOT DOESN'T REVEAL MUCH - GOING TO TRY ROLLING MEANS FOR EACH 
                        
                        #CALCULATE ROLLING MEANS
                        
                                #Calculate means by group and moose density
                                spec_means <- aggregate(spec_vol$Volume_m3ha, by = list("Years_Since_Exclosure" = spec_vol$Years_Since_Exclosure,
                                                                                        "Group" = spec_vol$Group,
                                                                                        "Moose_Density" = spec_vol$Moose_Density), FUN = mean)
                                colnames(spec_means)[4] <- "Mean_Volume_m3ha"
                                
                                #Calculate rolling means
                                nums <- c(1,3,5,7,9,11)
                                for(num in nums){
                                        
                                        #Add nice name
                                        if(num == 1){
                                                spec_means$Year_Name[spec_means$Years_Since_Exclosure == num] <- "1 Year"
                                        } else {
                                                spec_means$Year_Name[spec_means$Years_Since_Exclosure == num] <- paste(num, " Years", sep = "")
                                        }

                                        #Birch
                                        spec_means$Rolling_Mean_Vol[spec_means$Years_Since_Exclosure == num & spec_means$Group == "Deciduous"] <- rollmean(spec_means$Mean_Volume_m3ha[spec_means$Years_Since_Exclosure == num & spec_means$Group == "Deciduous"], 5, align = "right", na.pad = T)
                                        
                                        #Pine
                                        spec_means$Rolling_Mean_Vol[spec_means$Years_Since_Exclosure == num & spec_means$Group == "Pine"] <- rollmean(spec_means$Mean_Volume_m3ha[spec_means$Years_Since_Exclosure == num & spec_means$Group == "Pine"], 5, align = "right", na.pad = T)
                                        
                                        #Spruce
                                        spec_means$Rolling_Mean_Vol[spec_means$Years_Since_Exclosure == num & spec_means$Group == "Spruce"] <- rollmean(spec_means$Mean_Volume_m3ha[spec_means$Years_Since_Exclosure == num & spec_means$Group == "Spruce"], 5, align = "right", na.pad = T)
                                        
                                }
                                
                                #Labels for plot
                                years <- c("1 Year", "3 Years", "5 Years", "7 Years", "9 Years", "11 Years")
                                year_labs <- function(variable,value){
                                        return(years[value])
                                }
                                
                                #Generate plots
                                
                                ggplot(data = spec_means, aes(x = Moose_Density, y = Rolling_Mean_Vol, color = Group)) +
                                        geom_line() +
                                        geom_point() +
                                        facet_wrap(~Years_Since_Exclosure, ncol = 3, labeller = labeller(Years_Since_Exclosure = year_labs)) +
                                        labs(x = "Moose Metabolic Biomass" ~(kg/km^2), y = "Mean Subplot Volume "~(m^3/ha)) +
                                        theme_bw() +
                                        theme(
                                                axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                                axis.title.y = element_text(size = 12, margin = margin(r=10))
                                        )
                                
                                
                #TOTAL VOLUME PLOTS -----
                                
                        #Filter to browsed plots
                        spec_vol_total <- vol_total[vol_total$Treatment == "B",]
                        
                        #Filter to years in Years 1,3,5,7,9,11 (for plotting simplicity)
                        spec_vol_total <- spec_vol_total[spec_vol_total$Years_Since_Exclosure %in% c(1,3,5,7,9,11),]
                        
                        #Placeholder columns
                        spec_vol_total$DistrictID <- as.character('')
                        spec_vol_total$Moose_Density <- as.numeric('')
                        spec_vol_total$Red_Deer_Density <- as.numeric('')
                        spec_vol_total$Roe_Deer_Density <- as.numeric('')
                        
                        #Fix stangeskovene eidskog issue
                        spec_vol_total$LocalityName[spec_vol_total$LocalityName == 'stangeskovene eidskog '] <- 'stangeskovene_eidskog'
                        
                        #Loop through rows
                        for(i in 1:nrow(spec_vol_total)){
                                
                                #District ID ----
                                
                                #Get loc name
                                loc <- spec_vol_total[i, "LocalityName"]
                                
                                #Get district ID from site_data
                                id <- as.character(site_data$DistrictID[site_data$LocalityName == loc][1])
                                
                                #Format to match hbiomass2015 df (3-digit codes need 0 in front)
                                if(nchar(id) == 3){
                                        id <- paste("0", id, sep = "")
                                }
                                
                                spec_vol[i, "DistrictID"] <- id
                                
                                #Herbivore Densities ---
                                
                                spec_vol_total[i, "Moose_Density"] <- hd$Ms_2015[hd$kommnnr == id]
                                spec_vol_total[i, "Red_Deer_Density"] <- hd$Rd__2015[hd$kommnnr == id]
                                spec_vol_total[i, "Roe_Deer_Density"] <- hd$R_d_2015[hd$kommnnr == id]
                                
                        }
                        
                        
                        #CALCULATE ROLLING MEANS
                        
                        #Calculate means by group and moose density
                        spec_means_total <- aggregate(spec_vol_total$Volume_m3ha, by = list("Years_Since_Exclosure" = spec_vol_total$Years_Since_Exclosure,
                                                                                "Moose_Density" = spec_vol_total$Moose_Density), FUN = mean)
                        colnames(spec_means_total)[3] <- "Mean_Volume_m3ha"
                        
                        #Calculate rolling means
                        nums <- c(1,3,5,7,9,11)
                        for(num in nums){
                                
                                #Add nice name
                                if(num == 1){
                                        spec_means_total$Year_Name[spec_means_total$Years_Since_Exclosure == num] <- "1 Year"
                                } else {
                                        spec_means_total$Year_Name[spec_means_total$Years_Since_Exclosure == num] <- paste(num, " Years", sep = "")
                                }
                                
                                #Birch
                                spec_means_total$Rolling_Mean_Vol[spec_means_total$Years_Since_Exclosure == num] <- rollmean(spec_means_total$Mean_Volume_m3ha[spec_means_total$Years_Since_Exclosure == num], 5, align = "right", na.pad = T)
                                
                                #Pine
                                spec_means_total$Rolling_Mean_Vol[spec_means_total$Years_Since_Exclosure == num] <- rollmean(spec_means_total$Mean_Volume_m3ha[spec_means_total$Years_Since_Exclosure == num], 5, align = "right", na.pad = T)
                                
                                #Spruce
                                spec_means_total$Rolling_Mean_Vol[spec_means_total$Years_Since_Exclosure == num] <- rollmean(spec_means_total$Mean_Volume_m3ha[spec_means_total$Years_Since_Exclosure == num], 5, align = "right", na.pad = T)
                                
                        }
                        
                        #PLOT TOTAL WITH SPECIES-SPECIFIC
                        pd <- position_dodge(0.5)
                        ggplot(data = spec_means, aes(x = Moose_Density, y = Rolling_Mean_Vol, color = Group)) +
                                geom_line(alpha = 0.4) +
                                geom_point(alpha = 0.25, size = 0.5) +
                                facet_wrap(~Years_Since_Exclosure, ncol = 3, labeller = labeller(Years_Since_Exclosure = year_labs)) +
                                geom_point(data = spec_means_total, size = 0.8, position = pd, color = "#333333") +
                                geom_line(data = spec_means_total, size = 0.7, position = pd, color = "#333333") +
                                labs(x = "Moose Metabolic Biomass" ~(kg/km^2), y = "Mean Subplot Volume "~(m^3/ha)) +
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                        


                
#END DATA VISUALIZATION ----------------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                
                
                
                
#EXPORT PLOTS --------------------------------------------------------------------------------------

        #WRITE CSVs
                        
                #Main Volumes Dataset
                write.csv(vol, "1_Albedo_Exclosures/1_Data_Processing/3_Volume_Estimates/Output/subplot_tree_volumes.csv")
                        

        

#END EXPORT PLOTS ---------------------------------------------------------------------------------- 
