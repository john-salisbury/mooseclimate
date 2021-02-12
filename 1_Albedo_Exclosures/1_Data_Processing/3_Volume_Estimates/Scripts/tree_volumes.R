## Script to calculate tree volumes for all used sites using biomass estimates
## calculated from the backfitted "height-only" allometric models and height class only

##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(grid)
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        #Import CSV w/ individual tree biomass estimates to dataframe
        data <- read.csv('1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/tree_biomass.csv', header = TRUE)

#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#CALCULATE VOLUME FOR EACH TREE ---------------------------------------------------
        
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
                                                       data$Taxa == "Salix caprea (Selje)"] <- (data$Biomass_g[data$Taxa == "Betula pubescens (Bjørk)" |
                                                                                                                       data$Taxa == "Betula pendula (Lavlandbjørk)" |
                                                                                                                       data$Taxa == "Salix caprea (Selje)"] / b_density) / 1e06
                                #Assign group to birch
                                data$Group[data$Taxa == "Betula pubescens (Bjørk)" |
                                                       data$Taxa == "Betula pendula (Lavlandbjørk)" |
                                                       data$Taxa == "Salix caprea (Selje)"] <- "Birch"
                                
                        
                        #Spruce ------
                                
                                #Calculate volume
                                data$Volume_m3[data$Taxa == "Picea abies (Gran)" |
                                                data$Taxa == "Juniperus communis (Einer)"] <- (data$Biomass_g[data$Taxa == "Picea abies (Gran)" |
                                                                                                                      data$Taxa == "Juniperus communis (Einer)"] / s_density) / 1e06
                                
                                #Assign group to spruce
                                data$Group[data$Taxa == "Picea abies (Gran)" |
                                                       data$Taxa == "Juniperus communis (Einer)"] <- "Spruce"
                                
                                
                        #Pine -------
                                
                                #Calculate volume
                                data$Volume_m3[data$Taxa == "Pinus sylvestris (Furu)"] <- (data$Biomass_g[data$Taxa == "Pinus sylvestris (Furu)"] / p_density) / 1e06
                                
                                #Assign group to pine
                                data$Group[data$Taxa == "Pinus sylvestris (Furu)"] <- "Pine"
                                
                                
                        #Rowan (NOTE: using birch-specific density due to lack of rowan-specific density) ------
                                
                                #Calculate volume
                                data$Volume_m3[data$Taxa == "Sorbus aucuparia (Rogn)"] <- (data$Biomass_g[data$Taxa == "Sorbus aucuparia (Rogn)"] / b_density) / 1e06
                        
                                #Assign group
                                data$Group[data$Taxa == "Sorbus aucuparia (Rogn)"] <- "Birch"
        
                                
                #Ensure that 'Volume' column is numeric
                data$Volume_m3 <- as.numeric(data$Volume_m3)
                
                #Ensure that "Group' column is factr
                data$Group <- as.factor(data$Group)
                
                
        #Filter out data from after 2019 (since we only have climate data until 2018 at the moment)
        #data <- data[data$Year <= 2018,]
        
        #Sum volume (by species) for each plot for each year -----------
                
                #Construct placeholder dataframe
                volumes <- data.frame("Region" = character(),
                                      "LocalityName" = character(),
                                      "LocalityCode" = character(),
                                      "Treatment" = character(),
                                      "Year" = integer(),
                                      "Years_Since_Exclosure" = integer(),
                                      "Birch_Volume_m3" = numeric(),
                                      "Birch_Volume_m3_ha" = numeric(),
                                      "Pine_Volume_m3" = numeric(),
                                      "Pine_Volume_m3_ha" = numeric(),
                                      "Spruce_Volume_m3" = numeric(),
                                      "Spruce_Volume_m3_ha" = numeric())
                
        #Calculate plot sampling area in hectares (used to convert m3 to m3/ha) -------
                
                #Each subplot has a radius of 2m - A = pi*r^2
                subplot_area <- pi*(2^2) #12.57m2
                
                #Sum 4 subplots together to get total plot area in m2
                plot_area <- 4*subplot_area #50.26548m2
                
                #Convert m2 to hectares (ha) - 1m2 = 0.0001 ha (divide by 10,000)
                plot_area_ha <- plot_area/10000
                
                
        #Loop through each LocalityCode--------
                
                #Get sites to loop through
                data$LocalityCode <- as.factor(data$LocalityCode)
                sites <- levels(data$LocalityCode)
                
                for(i in 1:length(sites)){
                        
                        #Construct dataframe for LocalityCode i (same as volumes df)
                        temp <- data.frame("Region" = character(),
                                           "LocalityName" = character(),
                                           "LocalityCode" = character(),
                                           "Treatment" = character(),
                                           "Year" = integer(),
                                           "Years_Since_Exclosure" = integer(),
                                           "Birch_Volume_m3" = numeric(),
                                           "Birch_Volume_m3_ha" = numeric(),
                                           "Pine_Volume_m3" = numeric(),
                                           "Pine_Volume_m3_ha" = numeric(),
                                           "Spruce_Volume_m3" = numeric(),
                                           "Spruce_Volume_m3_ha" = numeric())
                        
                        #Get LocalityCode i
                        loc <- as.character(sites[i])
                        
                        #Get first year of data for LocalityCode i
                        first_year <- min(data$Year[data$LocalityCode == loc])
                        
                        #Get last year of data for LocalityCode i
                        last_year <- max(data$Year[data$LocalityCode == loc])
                        
                                #Number of years
                                num <- last_year - first_year + 1
                                
                        #Add # of rows corresponding to # of years of data
                        temp[nrow(temp)+num,] <- NA
                        
                        #Calculate volume of birch, spruce, and pine for LocalityCode i 
                        #from first year to last year of data
                        
                                #NOTE - this step loses some resolution of data - deciduous species
                                #are lumped together as "birch", while J. communis (juniper) is 
                                #lumped together with spruce
                        
                                for(j in 1:num){
                                        
                                        #Year
                                        year <- first_year + j - 1
                                        
                                        #Sum for year j by species
                                        birch_sum <- sum(data$Volume_m3[data$Group == "Birch" &
                                                                            data$LocalityCode == loc &
                                                                            data$Year == year])
                                        
                                        pine_sum <- sum(data$Volume_m3[data$Group == "Pine" &
                                                                               data$LocalityCode == loc &
                                                                               data$Year == year])
                                        
                                        spruce_sum <- sum(data$Volume_m3[data$Group == "Spruce" &
                                                                                 data$LocalityCode == loc &
                                                                                 data$Year == year])
                                        
                                        
                                        #Add info to temp df
                                        
                                                #Add year
                                                temp[j, "Year"] <- year
                                                
                                                #Add years since exclosure
                                                temp[j, "Years_Since_Exclosure"] <- j
                                                
                                                #Add birch volume
                                                temp[j, "Birch_Volume_m3"] <- birch_sum
                                                
                                                #Add birch volume/ha
                                                temp[j, "Birch_Volume_m3_ha"] <- birch_sum / plot_area_ha
                                                
                                                #Add pine volume
                                                temp[j, "Pine_Volume_m3"] <- pine_sum
                                                
                                                #Add pine volume/ha
                                                temp[j, "Pine_Volume_m3_ha"] <- pine_sum / plot_area_ha
                                                
                                                #Add spruce volume
                                                temp[j, "Spruce_Volume_m3"] <- spruce_sum
                                                
                                                #Add spruce volume/ha
                                                temp[j, "Spruce_Volume_m3_ha"] <- spruce_sum / plot_area_ha
                                                
                                                


                                }
                        
                        #Add other site data to entire temp df ---
                        
                                #Region
                                temp$Region <- data$Region[data$LocalityCode == loc][1]
                                
                                #LocalityName
                                temp$LocalityName <- data$LocalityName[data$LocalityCode == loc][1]
                                
                                #LocalityCode
                                temp$LocalityCode <- loc
                                
                                #Treatment
                                temp$Treatment <- data$Treatment[data$LocalityCode == loc][1]
                                
                        #Bind temp df to main "volumes" df ----
                                
                                volumes <- rbind(volumes, temp)
                        
                } #END OF LOOP
                
                
        #Make sure data has correct classes
        volumes$Birch_Volume_m3 <- as.numeric(volumes$Birch_Volume_m3)
        volumes$Birch_Volume_m3_ha <- as.numeric(volumes$Birch_Volume_m3_ha)
        volumes$Pine_Volume_m3 <- as.numeric(volumes$Pine_Volume_m3)
        volumes$Pine_Volume_m3_ha <- as.numeric(volumes$Pine_Volume_m3_ha)
        volumes$Spruce_Volume_m3 <- as.numeric(volumes$Spruce_Volume_m3)
        volumes$Spruce_Volume_m3_ha <- as.numeric(volumes$Spruce_Volume_m3_ha)
        volumes$Treatment <- as.factor(volumes$Treatment)

               

#END CALCULATE VOLUME FOR EACH TREE --------------------------------------------------------
        


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#DATA VISUALIZATION ------------------------------------------------------------------------------------------
        
        
        #Plots ------
                
        plot_theme <- theme(plot.title = element_text(hjust = 0.5, size = 50, margin = margin(t = 40, b = 40)),
                            legend.position = "none",
                            axis.text.x = element_text(size = 22, margin = margin(t=16)),
                            axis.text.y = element_text(size = 22, margin = margin(r=16)),
                            axis.title.x = element_text(size = 34, margin = margin(t=40, b = 40)),
                            axis.title.y = element_text(size = 34, margin = margin(r=40)),
                            strip.text.x = element_text(size = 22))
        
        plot_count_label <- c("0", "1\nn=36", "2\nn=37", "3\nn=37", "4\nn=37", "5\nn=37", "6\nn=37", "7\nn=29", "8\nn=29", "9\nn=28", "10\nn=15")
                
                #Stand volume (by species) ------
        
                        #Drangedal3 in 2010 seems erroneous (only UB but not B) - going to drop for now to plot further
                        volumes <- volumes[!(volumes$LocalityName == "drangedal3" & volumes$Year == 2010),]

        
                #Something strange happening with aggregate function - try changing sign of all values for
                #exclosures
        
                        #Copy volumes df
                        volumes_copy <- volumes
                        
                        #Change sign of all open plot values
                        volumes_copy$Birch_Volume_m3_ha[volumes_copy$Treatment == "B"] <- -(volumes_copy$Birch_Volume_m3_ha[volumes_copy$Treatment == "B"])
                        volumes_copy$Pine_Volume_m3_ha[volumes_copy$Treatment == "B"] <- -(volumes_copy$Pine_Volume_m3_ha[volumes_copy$Treatment == "B"])
                        volumes_copy$Spruce_Volume_m3_ha[volumes_copy$Treatment == "B"] <- -(volumes_copy$Spruce_Volume_m3_ha[volumes_copy$Treatment == "B"])
                        
                        #Birch
                        vol_diff_birch <- aggregate(volumes_copy$Birch_Volume_m3_ha,
                                                       by = list(Year = volumes$Year,
                                                                 LocalityName = volumes$LocalityName,
                                                                 Region = volumes$Region,
                                                                 Years_Since_Exclosure = volumes$Years_Since_Exclosure),
                                                       FUN = sum)
                        colnames(vol_diff_birch)[5] <- "Birch_Vol_Diff"
                        vol_diff_birch$Birch_Vol_Diff <- as.numeric(vol_diff_birch$Birch_Vol_Diff)
                        
                        #Spruce
                        vol_diff_spruce <- aggregate(volumes_copy$Spruce_Volume_m3_ha,
                                                        by = list(Year = volumes$Year,
                                                                  LocalityName = volumes$LocalityName,
                                                                  Region = volumes$Region,
                                                                  Years_Since_Exclosure = volumes$Years_Since_Exclosure),
                                                        FUN = sum)
                        colnames(vol_diff_spruce)[5] <- "Spruce_Vol_Diff"
                        vol_diff_spruce$Spruce_Vol_Diff <- as.numeric(vol_diff_spruce$Spruce_Vol_Diff)
                        
                        
                        #Pine
                        vol_diff_pine <- aggregate(volumes_copy$Pine_Volume_m3_ha,
                                                      by = list(Year = volumes$Year,
                                                                LocalityName = volumes$LocalityName,
                                                                Region = volumes$Region,
                                                                Years_Since_Exclosure = volumes$Years_Since_Exclosure),
                                                      FUN = sum)
                        colnames(vol_diff_pine)[5] <- "Pine_Vol_Diff"
                        vol_diff_pine$Pine_Vol_Diff <- as.numeric(vol_diff_pine$Pine_Vol_Diff)
                        
                        
                        #Rbind into df for visualization
                        vol_diff <- cbind(vol_diff_birch, vol_diff_pine$Pine_Vol_Diff, vol_diff_spruce$Spruce_Vol_Diff)
                        colnames(vol_diff)[6] <- "Pine_Vol_Diff"
                        colnames(vol_diff)[7] <- "Spruce_Vol_Diff"
                        
                        #Ensure numeric
                        vol_diff$Spruce_Vol_Diff <- as.numeric(vol_diff$Spruce_Vol_Diff)
                        vol_diff$Pine_Vol_Diff <- as.numeric(vol_diff$Pine_Vol_Diff)
                        vol_diff$Birch_Vol_Diff <- as.numeric(vol_diff$Birch_Vol_Diff)
        
                        
                        
                #PLOT
                      
                        #Faceted by site-----
                        
                                #Birch
                                png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/scatterplots/volume_diff_birch_faceted.png",
                                    width = 1600,
                                    height = 1600,
                                    bg = "white")
                                
                                ggplot(data = vol_diff, aes(x = Years_Since_Exclosure, y = Birch_Vol_Diff))+
                                        geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                                        geom_point(size = 3) +
                                        geom_smooth(span = 100, color = "black", lwd = 0.8) + 
                                        facet_wrap(~ LocalityName) +
                                        theme_bw() +
                                        labs(x = "Years Since Exclosure", y = expression(atop("Difference in Birch Stand Volume "~(m^3/ha), "(Excl. - Open)")) ) +
                                        scale_x_continuous(limits = c(0, 11), breaks = c(0, 2, 4, 6, 8, 10)) +
                                        plot_theme
                                
                                dev.off()
                                
                                
                                #Spruce
                                png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/scatterplots/volume_diff_spruce_faceted.png",
                                    width = 1600,
                                    height = 1600,
                                    bg = "white")
                                
                                ggplot(data = vol_diff, aes(x = Years_Since_Exclosure, y = Spruce_Vol_Diff))+
                                        geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                                        geom_point(size = 3) +
                                        geom_smooth(span = 100, color = "black", lwd = 0.8) + 
                                        facet_wrap(~ LocalityName) +
                                        theme_bw() +
                                        labs(x = "Years Since Exclosure", y = expression(atop("Difference in Spruce Stand Volume "~(m^3/ha), "(Excl. - Open)")) ) +
                                        scale_x_continuous(limits = c(0, 11), breaks = c(0, 2, 4, 6, 8, 10)) +
                                        plot_theme
                                
                                dev.off()
                                
                                #Pine
                                png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/scatterplots/volume_diff_pine_faceted.png",
                                    width = 1600,
                                    height = 1600,
                                    bg = "white")
                                
                                ggplot(data = vol_diff, aes(x = Years_Since_Exclosure, y = Pine_Vol_Diff))+
                                        geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                                        geom_point(size = 3) +
                                        geom_smooth(span = 100, color = "black", lwd = 0.8) + 
                                        facet_wrap(~ LocalityName) +
                                        theme_bw() +
                                        labs(x = "Years Since Exclosure", y = expression(atop("Difference in Pine Stand Volume "~(m^3/ha), "(Excl. - Open)")) ) +
                                        scale_x_continuous(limits = c(0, 11), breaks = c(0, 2, 4, 6, 8, 10)) +
                                        plot_theme
                                
                                dev.off()
                        
                        #All sites -------
                                
                                #Birch
                                png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/scatterplots/volume_diff_birch.png",
                                    width = 1300,
                                    height = 1300,
                                    bg = "white")
                                
                                ggplot(data = vol_diff, aes(x = Years_Since_Exclosure, y = Birch_Vol_Diff))+
                                        geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                                        geom_point(size = 3) +
                                        geom_smooth(span = 100, color = "black", lwd = 0.8) + 
                                        theme_bw() +
                                        labs(x = "Years Since Exclosure", y = expression(atop("Difference in Birch Stand Volume "~(m^3/ha), "(Excl. - Open)")) ) +
                                        scale_x_continuous(limits = c(1, 11), breaks = c(0:10), labels = plot_count_label) +
                                        plot_theme
                                
                                dev.off()
                                
                                #Pine
                                png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/scatterplots/volume_diff_pine.png",
                                    width = 1300,
                                    height = 1300,
                                    bg = "white")
                                
                                ggplot(data = vol_diff, aes(x = Years_Since_Exclosure, y = Pine_Vol_Diff))+
                                        geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                                        geom_point(size = 3) +
                                        geom_smooth(span = 100, color = "black", lwd = 0.8) + 
                                        theme_bw() +
                                        labs(x = "Years Since Exclosure", y = expression(atop("Difference in Pine Stand Volume "~(m^3/ha), "(Excl. - Open)")) ) +
                                        scale_x_continuous(limits = c(1, 11), breaks = c(0:10), labels = plot_count_label) +
                                        plot_theme
                                
                                dev.off()
                                
                                
                                #Spruce
                                png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/scatterplots/volume_diff_spruce.png",
                                    width = 1300,
                                    height = 1300,
                                    bg = "white")
                                
                                ggplot(data = vol_diff, aes(x = Years_Since_Exclosure, y = Spruce_Vol_Diff))+
                                        geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                                        geom_point(size = 3) +
                                        geom_smooth(span = 100, color = "black", lwd = 0.8) + 
                                        theme_bw() +
                                        labs(x = "Years Since Exclosure", y = expression(atop("Difference in Spruce Stand Volume "~(m^3/ha), "(Excl. - Open)")) ) +
                                        scale_x_continuous(limits = c(1, 11), breaks = c(0:10), labels = plot_count_label) +
                                        plot_theme
                                
                                dev.off()
                        

                #Species ---------
                                

                        #Faceted by region
                        png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/scatterplots/volume_diff_spec_faceted.png",
                            width = 1600,
                            height = 1600,
                            bg = "white")
                                
                        ggplot(vol_diff, aes(x = Years_Since_Exclosure)) +
                                geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                                geom_point(aes(y = Birch_Vol_Diff, color = "Birch"), size = 3) +
                                geom_smooth(aes(y = Birch_Vol_Diff, color = "Birch"), span = 50) +
                                geom_point(aes(y = Pine_Vol_Diff, color = "Pine"), size = 3) +
                                geom_smooth(aes(y = Pine_Vol_Diff, color = "Pine"), span = 50) +
                                geom_point(aes(y = Spruce_Vol_Diff, color = "Spruce"), size = 3) +
                                geom_smooth(aes(y = Spruce_Vol_Diff, color = "Spruce"), span = 50) +
                                theme_bw() +
                                facet_wrap(~ LocalityName) +
                                labs(x = "Years Since Exclosure", y = expression(atop("Difference in Stand Volume "~(m^3/ha), "(Excl. - Open)")), color = "Species") +
                                scale_x_continuous(limits = c(0, 11), breaks = c(0,2,4,6,8,10)) +
                                plot_theme +
                                theme(
                                        legend.position = "bottom",
                                        legend.title = element_text(size = 30, margin = margin(t=16)),
                                        legend.text = element_text(size = 22, margin = margin(t=16))
                                )

                        dev.off()
                        
                        #No faceting
                        png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/scatterplots/volume_diff_spec.png",
                            width = 1200,
                            height = 1200,
                            bg = "white")
                        
                        ggplot(vol_diff, aes(x = Years_Since_Exclosure)) +
                                geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                                geom_point(aes(y = Birch_Vol_Diff, color = "Birch"), alpha = 0.3, size = 3) +
                                geom_point(aes(y = Pine_Vol_Diff, color = "Pine"), alpha = 0.3, size = 3) +
                                geom_point(aes(y = Spruce_Vol_Diff, color = "Spruce"), alpha = 0.3, size = 3) +
                                geom_smooth(aes(y = Spruce_Vol_Diff, color = "Spruce"), span = 50) +
                                geom_smooth(aes(y = Birch_Vol_Diff, color = "Birch"), span = 50) +
                                geom_smooth(aes(y = Pine_Vol_Diff, color = "Pine"), span = 50) +
                                theme_bw() +
                                labs(x = "Years Since Exclosure", y = expression(atop("Difference in Stand Volume "~(m^3/ha), "(Excl. - Open)")), color = "Species") +
                                scale_x_continuous(limits = c(0, 11), breaks = c(0,2,4,6,8,10)) +
                                plot_theme +
                                theme(
                                        legend.position = "bottom",
                                        legend.title = element_text(size = 30, margin = margin(t=16)),
                                        legend.text = element_text(size = 22, margin = margin(t=16))
                                )
                        
                        dev.off()
                        

                        
                        
                
#END DATA VISUALIZATION ----------------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#CALCULATE + PLOT MEAN VOLUME DIFFERENCES FOR EACH YEAR --------------------------------------------------------------------------------------

        #BY SPECIES

                #Calculate mean volume difference of each species by site 
                        
                        #Birch
                        site_bm <- aggregate(vol_diff$Birch_Vol_Diff, by = list(Years_Since_Exclosure = vol_diff$Years_Since_Exclosure), FUN = mean)
                        colnames(site_bm)[2] <- "Mean_Birch_Vol_Diff"
                        
                        #Pine
                        site_pm <- aggregate(vol_diff$Pine_Vol_Diff, by = list(Years_Since_Exclosure = vol_diff$Years_Since_Exclosure), FUN = mean)
                        colnames(site_pm)[2] <- "Mean_Pine_Vol_Diff"
                        
                        #Spruce
                        site_sm <- aggregate(vol_diff$Spruce_Vol_Diff, by = list(Years_Since_Exclosure = vol_diff$Years_Since_Exclosure), FUN = mean)
                        colnames(site_sm)[2] <- "Mean_Spruce_Vol_Diff"
                        
                #Calculate standard error for each mean
                        
                        #Define function
                        std <- function(x) sd(x)/sqrt(length(x))
                        
                        #Add placeholder columns
                        site_bm$SE <- as.numeric('')
                        site_pm$SE <- as.numeric('')
                        site_sm$SE <- as.numeric('')
                        
                        
                        #Calculate SEs for each year
                        
                                for(i in min(vol_diff$Years_Since_Exclosure):max(vol_diff$Years_Since_Exclosure)){
                                        
                                        #Birch volume diff SE
                                        site_bm$SE[site_bm$Years_Since_Exclosure == i] <- std(vol_diff$Birch_Vol_Diff[vol_diff$Years_Since_Exclosure == i])
                                        
                                        #Spruce volume diff SE
                                        site_sm$SE[site_bm$Years_Since_Exclosure == i] <- std(vol_diff$Spruce_Vol_Diff[vol_diff$Years_Since_Exclosure == i])

                                        #Pine volume diff SE
                                        site_pm$SE[site_bm$Years_Since_Exclosure == i] <- std(vol_diff$Pine_Vol_Diff[vol_diff$Years_Since_Exclosure == i])

                                        
                                }
                                
                #PLOT
                        
                        #Birch
                        png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/means/mean_diff_birch.png",
                            width = 1400,
                            height = 1000,
                            units = "px",
                            bg = "white")
                        
                        ggplot(data = site_bm, aes(x = Years_Since_Exclosure, y = Mean_Birch_Vol_Diff)) +
                                geom_hline(yintercept = 0, linetype = 2, color = "gray") +
                                geom_errorbar(aes(ymin = (Mean_Birch_Vol_Diff - SE), ymax = (Mean_Birch_Vol_Diff + SE)), colour="black", width=.3) +
                                geom_line(lwd = 1.3) +
                                geom_point(size = 4) +
                                scale_x_continuous(limits=c(0,11), breaks = c(0:10), labels = plot_count_label) + 
                                scale_y_continuous(limits = c(-6, 8)) +
                                labs(x = "Years Since Exclosure", y = "Mean Difference in Birch Stand Volume (m3/ha)\n(Excl.-Open)") +
                                theme_bw() +
                                theme(plot.title = element_text(hjust = 0.5, size = 40, margin = margin(t = 40, b = 40)),
                                      legend.title = element_text(size = 24),
                                      legend.position = "bottom",
                                      legend.text = element_text(size = 20),
                                      strip.text = element_text(size = 20),
                                      axis.text.x = element_text(size = 24, margin = margin(t=16)),
                                      axis.text.y = element_text(size = 20, margin = margin(r=16)),
                                      axis.title.x = element_text(size = 30, margin = margin(t=40, b = 40)),
                                      axis.title.y = element_text(size = 30, margin = margin(r=40)))
                        
                        dev.off()
                        
                        
                        #Pine
                        png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/means/mean_diff_pine.png",
                            width = 1400,
                            height = 1000,
                            units = "px",
                            bg = "white")
                        
                        ggplot(data = site_pm, aes(x = Years_Since_Exclosure, y = Mean_Pine_Vol_Diff)) +
                                geom_hline(yintercept = 0, linetype = 2, color = "gray") +
                                geom_errorbar(aes(ymin = (Mean_Pine_Vol_Diff - SE), ymax = (Mean_Pine_Vol_Diff + SE)), colour="black", width=.3) +
                                geom_line(lwd = 1.3) +
                                geom_point(size = 4) +
                                scale_x_continuous(limits=c(0,11), breaks = c(0:10), labels = plot_count_label) + 
                                scale_y_continuous(limits = c(-6, 8)) +
                                labs(x = "Years Since Exclosure", y = "Mean Difference in Pine Stand Volume (m3/ha)\n(Excl.-Open)") +
                                theme_bw() +
                                theme(plot.title = element_text(hjust = 0.5, size = 40, margin = margin(t = 40, b = 40)),
                                      legend.title = element_text(size = 24),
                                      legend.position = "bottom",
                                      legend.text = element_text(size = 20),
                                      strip.text = element_text(size = 20),
                                      axis.text.x = element_text(size = 24, margin = margin(t=16)),
                                      axis.text.y = element_text(size = 20, margin = margin(r=16)),
                                      axis.title.x = element_text(size = 30, margin = margin(t=40, b = 40)),
                                      axis.title.y = element_text(size = 30, margin = margin(r=40)))
                        
                        dev.off()
                        
                        
                        #Spruce
                        png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/means/mean_diff_spruce.png",
                            width = 1400,
                            height = 1000,
                            units = "px",
                            bg = "white")
                        
                        ggplot(data = site_sm, aes(x = Years_Since_Exclosure, y = Mean_Spruce_Vol_Diff)) +
                                geom_hline(yintercept = 0, linetype = 2, color = "gray") +
                                geom_errorbar(aes(ymin = (Mean_Spruce_Vol_Diff - SE), ymax = (Mean_Spruce_Vol_Diff + SE)), colour="black", width=.3) +
                                geom_line(lwd = 1.3) +
                                geom_point(size = 4) +
                                scale_x_continuous(limits=c(0,11), breaks = c(0:10), labels = plot_count_label) + 
                                scale_y_continuous(limits = c(-6, 8)) +
                                labs(x = "Years Since Exclosure", y = "Mean Difference in Spruce Stand Volume (m3/ha)\n(Excl.-Open)") +
                                theme_bw() +
                                theme(plot.title = element_text(hjust = 0.5, size = 40, margin = margin(t = 40, b = 40)),
                                      legend.title = element_text(size = 24),
                                      legend.position = "bottom",
                                      legend.text = element_text(size = 20),
                                      strip.text = element_text(size = 20),
                                      axis.text.x = element_text(size = 24, margin = margin(t=16)),
                                      axis.text.y = element_text(size = 20, margin = margin(r=16)),
                                      axis.title.x = element_text(size = 30, margin = margin(t=40, b = 40)),
                                      axis.title.y = element_text(size = 30, margin = margin(r=40)))
                        
                        dev.off()
                        
                        
        #MEAN VOLUMES ON ONE PLOT (COLORED BY SPECIES)
        
                #Unify means df's
                
                        #Add species column to each & rename volume columns
                        
                                #Birch
                                site_bm$Species <- "Birch"
                                colnames(site_bm)[2] <- "Vol_Diff"
                                
                                #Spruce
                                site_sm$Species <- "Spruce"
                                colnames(site_sm)[2] <- "Vol_Diff"
                                
                                #Pine
                                site_pm$Species <- "Pine"
                                colnames(site_pm)[2] <- "Vol_Diff"
                        
                        #Row bind dfs
                        unified <- rbind(site_bm, site_sm, site_pm)
                        
                #PLOT
                        pd <- position_dodge(0.5)
                        
                        png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/means/mean_diff_all_species.png",
                            width = 1400,
                            height = 1000,
                            units = "px",
                            bg = "white")
                        
                        ggplot(data = unified, aes(x = Years_Since_Exclosure, y = Vol_Diff, color = Species, group = Species)) +
                                geom_hline(yintercept = 0, linetype = 2, color = "gray") +
                                geom_errorbar(aes(ymin = (Vol_Diff - SE), ymax = (Vol_Diff + SE)), colour="black", width=.4, position = pd) +
                                geom_line(lwd = 1.3, aes(linetype = Species), position = pd) +
                                geom_point(aes(shape = Species), position = pd, size = 4) +
                                scale_x_continuous(limits=c(0,11),
                                                   breaks = c(0:10),
                                                   labels = plot_count_label) + 
                                scale_y_continuous(limits = c(-6, 8)) +
                                labs(x = "Years Since Exclosure", y = "Mean Difference in Stand Volume (m3/ha)\n(Excl.-Open)") +
                                theme_bw() +
                                theme(plot.title = element_text(hjust = 0.5, size = 40, margin = margin(t = 40, b = 40)),
                                      legend.title = element_text(size = 24),
                                      legend.position = "bottom",
                                      legend.text = element_text(size = 20),
                                      strip.text = element_text(size = 20),
                                      axis.text.x = element_text(size = 24, margin = margin(t=16)),
                                      axis.text.y = element_text(size = 20, margin = margin(r=16)),
                                      axis.title.x = element_text(size = 30, margin = margin(t=40, b = 40)),
                                      axis.title.y = element_text(size = 30, margin = margin(r=40)))
                        
                        dev.off()
                        
                        
                        
#END CALCULATE + PLOT MEAN VOLUME DIFFERENCES FOR EACH YEAR --------------------------------------------------------------------------------------
                        
                        
                        
                        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#EXAMINE VOLUME BY REGION --------------------------------------------------------------------------------------
         
        #Each species - facet by region
                        
                #Spruce
                png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/by_region/spruce_by_region.png",
                    width = 1400,
                    height = 1000,
                    units = "px",
                    bg = "white")
                        
                ggplot(data = vol_diff, aes(x = Years_Since_Exclosure, y = Spruce_Vol_Diff))+
                        geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                        geom_point(size = 3) +
                        geom_smooth(span = 100, color = "black", lwd = 0.8) + 
                        facet_wrap(~ Region) +
                        theme_bw() +
                        labs(x = "Years Since Exclosure", y = expression(atop("Difference in Spruce Stand Volume "~(m^3/ha), "(Excl. - Open)")) ) +
                        scale_x_continuous(limits = c(0, 11), breaks = c(0, 2, 4, 6, 8, 10)) +
                        plot_theme
                
                dev.off()
                
                
                #Birch
                png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/by_region/birch_by_region.png",
                    width = 1400,
                    height = 1000,
                    units = "px",
                    bg = "white")
                
                ggplot(data = vol_diff, aes(x = Years_Since_Exclosure, y = Birch_Vol_Diff))+
                        geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                        geom_point(size = 3) +
                        geom_smooth(span = 100, color = "black", lwd = 0.8) + 
                        facet_wrap(~ Region) +
                        theme_bw() +
                        labs(x = "Years Since Exclosure", y = expression(atop("Difference in Birch Stand Volume "~(m^3/ha), "(Excl. - Open)")) ) +
                        scale_x_continuous(limits = c(0, 11), breaks = c(0, 2, 4, 6, 8, 10)) +
                        plot_theme
                
                dev.off()
                
                
                #Pine
                png(filename = "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/by_region/pine_by_region.png",
                    width = 1400,
                    height = 1000,
                    units = "px",
                    bg = "white")
                
                ggplot(data = vol_diff, aes(x = Years_Since_Exclosure, y = Pine_Vol_Diff))+
                        geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                        geom_point(size = 3) +
                        geom_smooth(span = 100, color = "black", lwd = 0.8) + 
                        facet_wrap(~ Region) +
                        theme_bw() +
                        labs(x = "Years Since Exclosure", y = expression(atop("Difference in Pine Stand Volume "~(m^3/ha), "(Excl. - Open)")) ) +
                        scale_x_continuous(limits = c(0, 11), breaks = c(0, 2, 4, 6, 8, 10)) +
                        plot_theme
                
                dev.off()
                        
#END EXAMINE VOLUME BY REGION --------------------------------------------------------------------------------------
                        
                        
                        
                        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                
                        
                        

#EXAMINE VOLUME DIFFERENCE BY MOOSE DENSITY --------------------------------------------------------------------------------------

        #Load herbivore density data
        
                
                
                
#END EXAMINE VOLUME DIFFERENCE BY MOOSE DENSITY --------------------------------------------------------------------------------------
                
                
  
                              
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                
                
                
                
#EXPORT PLOTS --------------------------------------------------------------------------------------

        #WRITE CSV
        write.csv(volumes, "1_Albedo_Exclosures/Approach_1A/Output/tree_volumes/tree_volumes.csv")
        

#END EXPORT PLOTS ---------------------------------------------------------------------------------- 
