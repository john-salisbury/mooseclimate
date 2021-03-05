## This is a script to calculate rolling means for species-specific volume in 
## Trøndelag and Hedmark (from the unified/processed SatSkog shapefile filtered to these two regions)


#PACKAGES ----------------------------------------------------------------------

        #Spatial Data Packages
        library(sf)
        
        #Data Manipulation + Visualization
        library(ggplot2)
        library(raster)
        library(lattice)
        library(dplyr)
        library(wesanderson)
        library(beepr)
        library(zoo)

#END PACKAGES ----------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#IMPORT & FORMAT DATA --------------------------------------------------------------------------------------------------

        #Load unified & FILTERED shapefile w/ all variables (from all years)
        data <- st_read("2_Albedo_Regional/1_Data_Processing/1_Filter_Regions/Output/regions.shp")
        beep(8)
        

        
#END IMPORT & FORMAT DATA --------------------------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#VOLUME ROLLING MEANS -----------------------------------------------------------------------------------
        
        #(1) Calculate mean volume for each species within each region
        
                #Isolate to MONTH 1 (since each volume is repeated 12x)
                mt_1_sub <- data[data$Month == 1,]
        
                #Aggregate means by REGION & MOOSE DENSITY (i.e. by kommune, since moose density
                #is on a kommune scale)
        
                #NOTE: USING HOMOGENOUS PIXELS (75% coverage or more)
        
                        #Pine
                        mv_pine <- mt_1_sub[mt_1_sub$fur_pct >= 75,]
                        mv_pine <- aggregate(mv_pine$vuprhaf, by = list("Plot" = mv_pine$Plot,
                                                                         "Ms_Dnst" = mv_pine$Ms_Dnst), FUN = mean)
                        colnames(mv_pine)[3] <- 'Mean_Stand_Volume_m3_ha'

                        #Spruce
                        mv_spruce <- mt_1_sub[mt_1_sub$grn_pct >= 75,]
                        mv_spruce <- aggregate(mv_spruce$vuprhag, by = list("Plot" = mv_spruce$Plot,
                                                                           "Ms_Dnst" = mv_spruce$Ms_Dnst), FUN = mean)
                        colnames(mv_spruce)[3] <- 'Mean_Stand_Volume_m3_ha'

                        #Deciduous
                        mv_decid <- mt_1_sub[mt_1_sub$lav_pct >= 75,]
                        mv_decid <- aggregate(mv_decid$vuprhal, by = list("Plot" = mv_decid$Plot,
                                                                           "Ms_Dnst" = mv_decid$Ms_Dnst), FUN = mean)
                        colnames(mv_decid)[3] <- 'Mean_Stand_Volume_m3_ha'

                            
                        
                        
        #(2) Calculate rolling means of each species (by region) -------
                
                #Trøndelag --------
                        
                        #Isolate Trondelag (remove "plot" column for rollmeans function)
                        tron_s <- mv_spruce[mv_spruce$Plot == "Trondelag",c(2:3)]
                        tron_p <- mv_pine[mv_spruce$Plot == "Trondelag",c(2:3)]
                        tron_d <- mv_decid[mv_spruce$Plot == "Trondelag",c(2:3)]
                        
                        #Rolling means
                        #NOTE: na.pad = T to allow for joining w/ moose density values
                        #Additionally, rolling window is 'right aligned' - since we have a lot of volume values at low moose densities
                        #(and are more interested in high densities), it's OK to lose a few at low densities
                        
                                #Spruce
                                roll_spruce_t <- as.data.frame(rollmean(tron_s$Mean_Stand_Volume_m3_ha, 7, align = "right", na.pad = T))
                                colnames(roll_spruce_t)[1] <- "Mean_Stand_Volume_m3_ha"
                                
                                #Pine
                                roll_pine_t <- as.data.frame(rollmean(tron_p$Mean_Stand_Volume_m3_ha, 7, align = "right", na.pad = T))
                                colnames(roll_pine_t)[1] <- "Mean_Stand_Volume_m3_ha"
                                
                                #Deciduous
                                roll_decid_t <- as.data.frame(rollmean(tron_d$Mean_Stand_Volume_m3_ha, 7, align = "right", na.pad = T))
                                colnames(roll_decid_t)[1] <- "Mean_Stand_Volume_m3_ha"
                                
                        #Rejoin each with respective set of moose density values
                                
                                #Spruce
                                roll_spruce_t <- cbind(roll_spruce_t, tron_s$Ms_Dnst)
                                colnames(roll_spruce_t)[2] <- 'Ms_Dnst'
                                
                                #Pine
                                roll_pine_t <- cbind(roll_pine_t, tron_p$Ms_Dnst)
                                colnames(roll_pine_t)[2] <- 'Ms_Dnst'
                                
                                #Deciduous
                                roll_decid_t <- cbind(roll_decid_t, tron_d$Ms_Dnst)
                                colnames(roll_decid_t)[2] <- 'Ms_Dnst'
                                
                        
                        #Add 'group' identifier
                        roll_spruce_t$Group <- "Spruce"
                        roll_pine_t$Group <- "Pine"
                        roll_decid_t$Group <- "Deciduous"
                        
                        #Unify
                        roll_means_t <- rbind(roll_spruce_t, roll_pine_t, roll_decid_t)
                        
                        #Add 'region'/'plot' identifier (i.e. Trøndelag)
                        roll_means_t$Region <- "Trøndelag"
                        

                        
                        
                #Hedmark --------
                        
                        #Isolate Hedmark (remove "plot" column for rollmeans function)
                        hed_s <- mv_spruce[mv_spruce$Plot == "Hedmark",c(2:3)]
                        hed_p <- mv_pine[mv_spruce$Plot == "Hedmark",c(2:3)]
                        hed_d <- mv_decid[mv_spruce$Plot == "Hedmark",c(2:3)]
                        
                        #Rolling means
                        #NOTE: na.pad = T to allow for joining w/ moose density values
                        #Rolling window (k = 7) is 'right aligned'
                        
                                #Spruce
                                roll_spruce_h <- as.data.frame(rollmean(hed_s$Mean_Stand_Volume_m3_ha, 7, align = "right", na.pad = T))
                                colnames(roll_spruce_h)[1] <- "Mean_Stand_Volume_m3_ha"
                                
                                #Pine
                                roll_pine_h <- as.data.frame(rollmean(hed_p$Mean_Stand_Volume_m3_ha, 7, align = "right", na.pad = T))
                                colnames(roll_pine_h)[1] <- "Mean_Stand_Volume_m3_ha"
                                
                                #Deciduous
                                roll_decid_h <- as.data.frame(rollmean(hed_d$Mean_Stand_Volume_m3_ha, 7, align = "right", na.pad = T))
                                colnames(roll_decid_h)[1] <- "Mean_Stand_Volume_m3_ha"
                        
                        #Rejoin each with respective set of moose density values
                        
                                #Spruce
                                roll_spruce_h <- cbind(roll_spruce_h, hed_s$Ms_Dnst)
                                colnames(roll_spruce_h)[2] <- 'Ms_Dnst'
                                
                                #Pine
                                roll_pine_h <- cbind(roll_pine_h, hed_p$Ms_Dnst)
                                colnames(roll_pine_h)[2] <- 'Ms_Dnst'
                                
                                #Deciduous
                                roll_decid_h <- cbind(roll_decid_h, hed_d$Ms_Dnst)
                                colnames(roll_decid_h)[2] <- 'Ms_Dnst'
                        
                        
                        #Add 'group' identifier
                        roll_spruce_h$Group <- "Spruce"
                        roll_pine_h$Group <- "Pine"
                        roll_decid_h$Group <- "Deciduous"
                        
                        #Unify
                        roll_means_h <- rbind(roll_spruce_h, roll_pine_h, roll_decid_h)
                        
                        #Add 'region'/'plot' identifier (i.e. Hedmark)
                        roll_means_h$Region <- "Hedmark"
                        
                        
                #JOIN HEDMARK AND TRONDELAG TOGETHER ----
                        
                        final_roll_means <- rbind(roll_means_t, roll_means_h)
                        
                        
                        
                        
        #(3) WRITE FINAL CSV ------------
                        
                #Write CSV
                write.csv(final_roll_means, "2_Albedo_Regional/1_Data_Processing/2_Rolling_Means/Output/Volume/volume_rolling_means.csv")
                
                        
        #(4) GENERATE PLOTS ---------------
                        
                #Start here if loading CSV
                final_roll_means <- read.csv("2_Albedo_Regional/1_Data_Processing/2_Rolling_Means/Output/Volume/volume_rolling_means.csv", header = T)
        
                #VOLUME VS MOOSE DENSITY (FACETED BY SPECIES & GROUPED BY TREATMENT)
                        
                        pd <- position_dodge(2)
                        
                        #Generate plot
                        ggplot(data = final_roll_means, aes(x = Ms_Dnst, y = Mean_Stand_Volume_m3_ha, group = Region)) +
                                geom_point(aes(shape = Region), size = 1, position = pd) +
                                geom_line(aes(linetype = Region), position = pd) +
                                facet_wrap(~Group) +
                                labs(x = "Moose Metabolic Biomass  " ~(kg/km^2), y = "Mean Stand Volume  " ~(m^3/ha)) +
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                )
                        

        ##SEEING UNEXPECTED TREND FOR DECIDUOUS VOLUME IN BOTH REGIONS
                        
        ##GOING TO GROUP BY (1) ELEVATION RANGE & (2) AGE TO SEE IF WE CAN BETTER DELINEATE IDENTIFY TRENDS 
                        
                        
                                
                                
                        
                        
#END VOLUME ROLLING MEANS --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#VOLUME ROLLING MEANS BY ELEVATION -----------------------------------------------------------------------------------
        
        #GOING TO FILTER BY ELEVATION RANGE FOR EACH REGION TO BETTER DELINEATE TRENDS
        
        #(1) Identify elevation ranges to use
                        
                #What do the average plot elevations look like in both regions?
                ggplot(data = data, aes(x = dem)) +
                        geom_histogram() +
                        facet_wrap(~Plot) +
                        labs(x = "Average Forest Plot Elevation (m)", y = "Count") +
                        theme_bw() +
                        theme(
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10)),
                                panel.grid.minor = element_blank()
                        )
                        
                #Going to go with 3 elevation ranges - 0-300m, 300-600m, and 600-900m
                        
                        
        #(2) Split data into relevation ranges
                
                #Create "Elevation Range" Column w/ 3 ranges
                data$Elevation_Range[data$dem <= 300 & data$dem > 1] <- '1 - 300m'
                data$Elevation_Range[data$dem <= 600 & data$dem > 300] <- '301 - 600m'
                data$Elevation_Range[data$dem <= 900 & data$dem > 600] <- '601 - 900m'
                
        #(3) Calculate means for each region, species, and age group
                
                #Isolate to MONTH 1 (since each volume is repeated 12x)
                mt_1_sub <- data[data$Month == 1,]
                
                #Aggregate means by REGION, MOOSE DENSITY, & ELEV RANGE (i.e. by kommune, since moose density
                #is on a kommune scale)
                
                #NOTE: Each species is filtered to 75% HOMOGENOUS FOREST (as in Hu et al.)
                

                        #Pine
                        mv_pine <- mt_1_sub[mt_1_sub$fur_pct >= 75,]
                        mv_pine <- aggregate(mv_pine$vuprhaf, by = list("Plot" = mv_pine$Plot,
                                                                        "Ms_Dnst" = mv_pine$Ms_Dnst,
                                                                        "Elevation_Range" = mv_pine$Elevation_Range), FUN = mean)
                        colnames(mv_pine)[4] <- 'Mean_Stand_Volume_m3_ha'
                        
                        #Spruce
                        mv_spruce <- mt_1_sub[mt_1_sub$grn_pct >= 75,]
                        mv_spruce <- aggregate(mv_spruce$vuprhag, by = list("Plot" = mv_spruce$Plot,
                                                                            "Ms_Dnst" = mv_spruce$Ms_Dnst,
                                                                            "Elevation_Range" = mv_spruce$Elevation_Range), FUN = mean)
                        colnames(mv_spruce)[4] <- 'Mean_Stand_Volume_m3_ha'
                        
                        #Deciduous
                        mv_decid <- mt_1_sub[mt_1_sub$lav_pct >= 75,]
                        mv_decid <- aggregate(mv_decid$vuprhal, by = list("Plot" = mv_decid$Plot,
                                                                          "Ms_Dnst" = mv_decid$Ms_Dnst,
                                                                          "Elevation_Range" = mv_decid$Elevation_Range), FUN = mean)
                        colnames(mv_decid)[4] <- 'Mean_Stand_Volume_m3_ha'
                
                     
                        
                           
        # (4) CALCULATE ROLLING MEANS --------
                        
                #Trondelag -----
                        
                        #Deciduous
                        mv_decid$Rolling_Mean_Vol[mv_decid$Plot == "Trondelag" & mv_decid$Elevation_Range == "1 - 300m"] <- rollmean(mv_decid$Mean_Stand_Volume_m3_ha[mv_decid$Plot == "Trondelag" & mv_decid$Elevation_Range == "1 - 300m"], 5, align = "right", na.pad = T)    
                        mv_decid$Rolling_Mean_Vol[mv_decid$Plot == "Trondelag" & mv_decid$Elevation_Range == "301 - 600m"] <- rollmean(mv_decid$Mean_Stand_Volume_m3_ha[mv_decid$Plot == "Trondelag" & mv_decid$Elevation_Range == "301 - 600m"], 5, align = "right", na.pad = T)    
                        mv_decid$Rolling_Mean_Vol[mv_decid$Plot == "Trondelag" & mv_decid$Elevation_Range == "601 - 900m"] <- rollmean(mv_decid$Mean_Stand_Volume_m3_ha[mv_decid$Plot == "Trondelag" & mv_decid$Elevation_Range == "601 - 900m"], 5, align = "right", na.pad = T)    
                        
                        #Pine
                        mv_pine$Rolling_Mean_Vol[mv_pine$Plot == "Trondelag" & mv_pine$Elevation_Range == "1 - 300m"] <- rollmean(mv_pine$Mean_Stand_Volume_m3_ha[mv_pine$Plot == "Trondelag" & mv_pine$Elevation_Range == "1 - 300m"], 5, align = "right", na.pad = T)    
                        mv_pine$Rolling_Mean_Vol[mv_pine$Plot == "Trondelag" & mv_pine$Elevation_Range == "301 - 600m"] <- rollmean(mv_pine$Mean_Stand_Volume_m3_ha[mv_pine$Plot == "Trondelag" & mv_pine$Elevation_Range == "301 - 600m"], 5, align = "right", na.pad = T)    
                        mv_pine$Rolling_Mean_Vol[mv_pine$Plot == "Trondelag" & mv_pine$Elevation_Range == "601 - 900m"] <- rollmean(mv_pine$Mean_Stand_Volume_m3_ha[mv_pine$Plot == "Trondelag" & mv_pine$Elevation_Range == "601 - 900m"], 5, align = "right", na.pad = T)    
                        
                        #Spruce
                        mv_spruce$Rolling_Mean_Vol[mv_spruce$Plot == "Trondelag" & mv_spruce$Elevation_Range == "1 - 300m"] <- rollmean(mv_spruce$Mean_Stand_Volume_m3_ha[mv_spruce$Plot == "Trondelag" & mv_spruce$Elevation_Range == "1 - 300m"], 5, align = "right", na.pad = T)    
                        mv_spruce$Rolling_Mean_Vol[mv_spruce$Plot == "Trondelag" & mv_spruce$Elevation_Range == "301 - 600m"] <- rollmean(mv_spruce$Mean_Stand_Volume_m3_ha[mv_spruce$Plot == "Trondelag" & mv_spruce$Elevation_Range == "301 - 600m"], 5, align = "right", na.pad = T)    
                        mv_spruce$Rolling_Mean_Vol[mv_spruce$Plot == "Trondelag" & mv_spruce$Elevation_Range == "601 - 900m"] <- rollmean(mv_spruce$Mean_Stand_Volume_m3_ha[mv_spruce$Plot == "Trondelag" & mv_spruce$Elevation_Range == "601 - 900m"], 5, align = "right", na.pad = T)    
                        
                        
                #Hedmark ----
                        
                        #Deciduous
                        mv_decid$Rolling_Mean_Vol[mv_decid$Plot == "Hedmark" & mv_decid$Elevation_Range == "1 - 300m"] <- rollmean(mv_decid$Mean_Stand_Volume_m3_ha[mv_decid$Plot == "Hedmark" & mv_decid$Elevation_Range == "1 - 300m"], 5, align = "right", na.pad = T)    
                        mv_decid$Rolling_Mean_Vol[mv_decid$Plot == "Hedmark" & mv_decid$Elevation_Range == "301 - 600m"] <- rollmean(mv_decid$Mean_Stand_Volume_m3_ha[mv_decid$Plot == "Hedmark" & mv_decid$Elevation_Range == "301 - 600m"], 5, align = "right", na.pad = T)    
                        mv_decid$Rolling_Mean_Vol[mv_decid$Plot == "Hedmark" & mv_decid$Elevation_Range == "601 - 900m"] <- rollmean(mv_decid$Mean_Stand_Volume_m3_ha[mv_decid$Plot == "Hedmark" & mv_decid$Elevation_Range == "601 - 900m"], 5, align = "right", na.pad = T)    
                        
                        #Pine
                        mv_pine$Rolling_Mean_Vol[mv_pine$Plot == "Hedmark" & mv_pine$Elevation_Range == "1 - 300m"] <- rollmean(mv_pine$Mean_Stand_Volume_m3_ha[mv_pine$Plot == "Hedmark" & mv_pine$Elevation_Range == "1 - 300m"], 5, align = "right", na.pad = T)    
                        mv_pine$Rolling_Mean_Vol[mv_pine$Plot == "Hedmark" & mv_pine$Elevation_Range == "301 - 600m"] <- rollmean(mv_pine$Mean_Stand_Volume_m3_ha[mv_pine$Plot == "Hedmark" & mv_pine$Elevation_Range == "301 - 600m"], 5, align = "right", na.pad = T)    
                        mv_pine$Rolling_Mean_Vol[mv_pine$Plot == "Hedmark" & mv_pine$Elevation_Range == "601 - 900m"] <- rollmean(mv_pine$Mean_Stand_Volume_m3_ha[mv_pine$Plot == "Hedmark" & mv_pine$Elevation_Range == "601 - 900m"], 5, align = "right", na.pad = T)    
                        
                        #Spruce
                        mv_spruce$Rolling_Mean_Vol[mv_spruce$Plot == "Hedmark" & mv_spruce$Elevation_Range == "1 - 300m"] <- rollmean(mv_spruce$Mean_Stand_Volume_m3_ha[mv_spruce$Plot == "Hedmark" & mv_spruce$Elevation_Range == "1 - 300m"], 5, align = "right", na.pad = T)    
                        mv_spruce$Rolling_Mean_Vol[mv_spruce$Plot == "Hedmark" & mv_spruce$Elevation_Range == "301 - 600m"] <- rollmean(mv_spruce$Mean_Stand_Volume_m3_ha[mv_spruce$Plot == "Hedmark" & mv_spruce$Elevation_Range == "301 - 600m"], 5, align = "right", na.pad = T)    
                        mv_spruce$Rolling_Mean_Vol[mv_spruce$Plot == "Hedmark" & mv_spruce$Elevation_Range == "601 - 900m"] <- rollmean(mv_spruce$Mean_Stand_Volume_m3_ha[mv_spruce$Plot == "Hedmark" & mv_spruce$Elevation_Range == "601 - 900m"], 5, align = "right", na.pad = T)    
                        
                        #Add species-specific columns
                        mv_decid$Group <- 'Deciduous'
                        mv_pine$Group <- 'Pine'
                        mv_spruce$Group <- 'Spruce'
                        
                        #Unify
                        roll_means_elev <- rbind(mv_decid, mv_pine, mv_spruce)
                        
                        #Fix class
                        roll_means_elev$Rolling_Mean_Vol <- as.numeric(roll_means_elev$Rolling_Mean_Vol)
                        
                        
        # (5) GENERATE PLOTS ---------
                        
                #Generate plot
                ggplot(data = roll_means_elev, aes(x = Ms_Dnst, y = Rolling_Mean_Vol, group = Plot)) +
                        geom_point(aes(shape = Plot), size = 1, position = pd) +
                        geom_line(aes(linetype = Plot), position = pd) +
                        facet_grid(Group~Elevation_Range) +
                        labs(x = "Moose Metabolic Biomass  " ~(kg/km^2), y = "Mean Stand Volume  " ~(m^3/ha)) +
                        theme_bw() +
                        theme(
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10))
                                
                        )
        
#END VOLUME ROLLING MEANS BY ELEVATION --------------------------------------------------------------------------------        
                        
                        
                        
                        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                        
                        
                        
                        
#VOLUME ROLLING MEANS BY AGE GROUPS -----------------------------------------------------------------------------------
                        
        #GOING TO FILTER BY AGE GROUP FOR EACH REGION TO BETTER DELINEATE TRENDS
        
        #(1) Identify age ranges to use
        
                #What do the ages look like in both regions?
                ggplot(data = data, aes(x = alder)) +
                        geom_histogram() +
                        facet_wrap(~Plot) +
                        labs(x = "Forest Plot Age", y = "Count") +
                        theme_bw() +
                        theme(
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10)),
                                panel.grid.minor = element_blank()
                        )
                        
        #(2) Split into age groups
                        
                #Create "Age Group" Column w/ 3 age groups
                data$Age_Group[data$alder <= 10 & data$alder > 0] <- '1 - 10 Years'
                data$Age_Group[data$alder <= 20 & data$alder > 10] <- '11 - 20 Years'
                data$Age_Group[data$alder <= 30 & data$alder > 20] <- '21 - 30 Years'
                
                          
        #(3) Calculate means for each region, species, and age group
                                
                #Isolate to MONTH 1 (since each volume is repeated 12x)
                mt_1_sub <- data[data$Month == 1,]
                                
                #Aggregate means by REGION, MOOSE DENSITY, & AGE RANGE (i.e. by kommune, since moose density
                #is on a kommune scale)
                                
                #NOTE: Each species is filtered to 75% HOMOGENOUS FOREST (as in Hu et al.)
                
                        #Pine
                        mv_pine <- mt_1_sub[mt_1_sub$fur_pct >= 75,]
                        mv_pine <- aggregate(mv_pine$vuprhaf, by = list("Plot" = mv_pine$Plot,
                                                                        "Ms_Dnst" = mv_pine$Ms_Dnst,
                                                                        "Age_Group" = mv_pine$Age_Group), FUN = mean)
                        colnames(mv_pine)[4] <- 'Mean_Stand_Volume_m3_ha'
                        
                        #Spruce
                        mv_spruce <- mt_1_sub[mt_1_sub$grn_pct >= 75,]
                        mv_spruce <- aggregate(mv_spruce$vuprhag, by = list("Plot" = mv_spruce$Plot,
                                                                            "Ms_Dnst" = mv_spruce$Ms_Dnst,
                                                                            "Age_Group" = mv_spruce$Age_Group), FUN = mean)
                        colnames(mv_spruce)[4] <- 'Mean_Stand_Volume_m3_ha'
                        
                        #Deciduous
                        mv_decid <- mt_1_sub[mt_1_sub$lav_pct >= 75,]
                        mv_decid <- aggregate(mv_decid$vuprhal, by = list("Plot" = mv_decid$Plot,
                                                                          "Ms_Dnst" = mv_decid$Ms_Dnst,
                                                                          "Age_Group" = mv_decid$Age_Group), FUN = mean)
                        colnames(mv_decid)[4] <- 'Mean_Stand_Volume_m3_ha'
                        

                
                        
        # (4) CALCULATE ROLLING MEANS --------
                                
                #Trondelag -----

                        #Deciduous
                        mv_decid$Rolling_Mean_Vol[mv_decid$Plot == "Trondelag" & mv_decid$Age_Group == "1 - 10 Years"] <- rollmean(mv_decid$Mean_Stand_Volume_m3_ha[mv_decid$Plot == "Trondelag" & mv_decid$Age_Group == "1 - 10 Years"], 5, align = "right", na.pad = T)    
                        mv_decid$Rolling_Mean_Vol[mv_decid$Plot == "Trondelag" & mv_decid$Age_Group == "11 - 20 Years"] <- rollmean(mv_decid$Mean_Stand_Volume_m3_ha[mv_decid$Plot == "Trondelag" & mv_decid$Age_Group == "11 - 20 Years"], 5, align = "right", na.pad = T)    
                        mv_decid$Rolling_Mean_Vol[mv_decid$Plot == "Trondelag" & mv_decid$Age_Group == "21 - 30 Years"] <- rollmean(mv_decid$Mean_Stand_Volume_m3_ha[mv_decid$Plot == "Trondelag" & mv_decid$Age_Group == "21 - 30 Years"], 5, align = "right", na.pad = T)    
                
                        #Pine
                        mv_pine$Rolling_Mean_Vol[mv_pine$Plot == "Trondelag" & mv_pine$Age_Group == "1 - 10 Years"] <- rollmean(mv_pine$Mean_Stand_Volume_m3_ha[mv_pine$Plot == "Trondelag" & mv_pine$Age_Group == "1 - 10 Years"], 5, align = "right", na.pad = T)    
                        mv_pine$Rolling_Mean_Vol[mv_pine$Plot == "Trondelag" & mv_pine$Age_Group == "11 - 20 Years"] <- rollmean(mv_pine$Mean_Stand_Volume_m3_ha[mv_pine$Plot == "Trondelag" & mv_pine$Age_Group == "11 - 20 Years"], 5, align = "right", na.pad = T)    
                        mv_pine$Rolling_Mean_Vol[mv_pine$Plot == "Trondelag" & mv_pine$Age_Group == "21 - 30 Years"] <- rollmean(mv_pine$Mean_Stand_Volume_m3_ha[mv_pine$Plot == "Trondelag" & mv_pine$Age_Group == "21 - 30 Years"], 5, align = "right", na.pad = T)    
                        
                        #Spruce
                        mv_spruce$Rolling_Mean_Vol[mv_spruce$Plot == "Trondelag" & mv_spruce$Age_Group == "1 - 10 Years"] <- rollmean(mv_spruce$Mean_Stand_Volume_m3_ha[mv_spruce$Plot == "Trondelag" & mv_spruce$Age_Group == "1 - 10 Years"], 5, align = "right", na.pad = T)    
                        mv_spruce$Rolling_Mean_Vol[mv_spruce$Plot == "Trondelag" & mv_spruce$Age_Group == "11 - 20 Years"] <- rollmean(mv_spruce$Mean_Stand_Volume_m3_ha[mv_spruce$Plot == "Trondelag" & mv_spruce$Age_Group == "11 - 20 Years"], 5, align = "right", na.pad = T)    
                        mv_spruce$Rolling_Mean_Vol[mv_spruce$Plot == "Trondelag" & mv_spruce$Age_Group == "21 - 30 Years"] <- rollmean(mv_spruce$Mean_Stand_Volume_m3_ha[mv_spruce$Plot == "Trondelag" & mv_spruce$Age_Group == "21 - 30 Years"], 5, align = "right", na.pad = T)    
                        
                        
                #Hedmark ----
                        
                        #Deciduous
                        mv_decid$Rolling_Mean_Vol[mv_decid$Plot == "Hedmark" & mv_decid$Age_Group == "1 - 10 Years"] <- rollmean(mv_decid$Mean_Stand_Volume_m3_ha[mv_decid$Plot == "Hedmark" & mv_decid$Age_Group == "1 - 10 Years"], 5, align = "right", na.pad = T)    
                        mv_decid$Rolling_Mean_Vol[mv_decid$Plot == "Hedmark" & mv_decid$Age_Group == "11 - 20 Years"] <- rollmean(mv_decid$Mean_Stand_Volume_m3_ha[mv_decid$Plot == "Hedmark" & mv_decid$Age_Group == "11 - 20 Years"], 5, align = "right", na.pad = T)    
                        mv_decid$Rolling_Mean_Vol[mv_decid$Plot == "Hedmark" & mv_decid$Age_Group == "21 - 30 Years"] <- rollmean(mv_decid$Mean_Stand_Volume_m3_ha[mv_decid$Plot == "Hedmark" & mv_decid$Age_Group == "21 - 30 Years"], 5, align = "right", na.pad = T)    
                        
                        #Pine
                        mv_pine$Rolling_Mean_Vol[mv_pine$Plot == "Hedmark" & mv_pine$Age_Group == "1 - 10 Years"] <- rollmean(mv_pine$Mean_Stand_Volume_m3_ha[mv_pine$Plot == "Hedmark" & mv_pine$Age_Group == "1 - 10 Years"], 5, align = "right", na.pad = T)    
                        mv_pine$Rolling_Mean_Vol[mv_pine$Plot == "Hedmark" & mv_pine$Age_Group == "11 - 20 Years"] <- rollmean(mv_pine$Mean_Stand_Volume_m3_ha[mv_pine$Plot == "Hedmark" & mv_pine$Age_Group == "11 - 20 Years"], 5, align = "right", na.pad = T)    
                        mv_pine$Rolling_Mean_Vol[mv_pine$Plot == "Hedmark" & mv_pine$Age_Group == "21 - 30 Years"] <- rollmean(mv_pine$Mean_Stand_Volume_m3_ha[mv_pine$Plot == "Hedmark" & mv_pine$Age_Group == "21 - 30 Years"], 5, align = "right", na.pad = T)    
                        
                        #Spruce
                        mv_spruce$Rolling_Mean_Vol[mv_spruce$Plot == "Hedmark" & mv_spruce$Age_Group == "1 - 10 Years"] <- rollmean(mv_spruce$Mean_Stand_Volume_m3_ha[mv_spruce$Plot == "Hedmark" & mv_spruce$Age_Group == "1 - 10 Years"], 5, align = "right", na.pad = T)    
                        mv_spruce$Rolling_Mean_Vol[mv_spruce$Plot == "Hedmark" & mv_spruce$Age_Group == "11 - 20 Years"] <- rollmean(mv_spruce$Mean_Stand_Volume_m3_ha[mv_spruce$Plot == "Hedmark" & mv_spruce$Age_Group == "11 - 20 Years"], 5, align = "right", na.pad = T)    
                        mv_spruce$Rolling_Mean_Vol[mv_spruce$Plot == "Hedmark" & mv_spruce$Age_Group == "21 - 30 Years"] <- rollmean(mv_spruce$Mean_Stand_Volume_m3_ha[mv_spruce$Plot == "Hedmark" & mv_spruce$Age_Group == "21 - 30 Years"], 5, align = "right", na.pad = T)    
                        
                #Add species-specific columns
                mv_decid$Group <- 'Deciduous'
                mv_pine$Group <- 'Pine'
                mv_spruce$Group <- 'Spruce'
                
                #Unify
                roll_means_age <- rbind(mv_decid, mv_pine, mv_spruce)
                
                #Fix class
                roll_means_age$Rolling_Mean_Vol <- as.numeric(roll_means_age$Rolling_Mean_Vol)
                
        
                
        # (5) GENERATE PLOTS ---------
                
                #Generate plot
                ggplot(data = roll_means_age, aes(x = Ms_Dnst, y = Rolling_Mean_Vol, group = Plot)) +
                        geom_point(aes(shape = Plot), size = 1, position = pd) +
                        geom_line(aes(linetype = Plot), position = pd) +
                        facet_grid(Group~Age_Group) +
                        labs(x = "Moose Metabolic Biomass  " ~(kg/km^2), y = "Mean Stand Volume  " ~(m^3/ha)) +
                        theme_bw() +
                        theme(
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10))
                                
                        )
                        

#END VOLUME ROLLING MEANS BY AGE GROUPS --------------------------------------------------------------------------------
                
                
                
                
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                
                
                
                
#HISTOGRAMS BY AGE, ELEV, & REGION ------------------------------------------------------------------------------------
                
        #Create dataframe of all homogenous pixels (remove mixed forest)
                
                #By species
                
                        #Pine
                        homog_pine <- data[data$Month == 1 & data$fur_pct >= 75,]
                        homog_pine$Forest_Type <- 'Pine'
                
                        #Spruce
                        homog_spruce <- data[data$Month == 1 & data$grn_pct >= 75,]
                        homog_spruce$Forest_Type <- 'Spruce'
                        
                        #Deciduous
                        homog_decid <- data[data$Month == 1 & data$lav_pct >= 75,]
                        homog_decid$Forest_Type <- 'Deciduous'
                        
                
                #Unify
                homog <- rbind(homog_pine, homog_spruce, homog_decid) #19469 total observations
                
        #AGE HISTOGRAMS
        ggplot(data = homog, aes(x = alder)) +
                geom_histogram() +
                facet_grid(Forest_Type~Plot) +
                labs(x = "Forest Plot Age (years)", y = "Count") +
                theme_bw() +
                theme(
                        axis.title.x = element_text(margin = margin(t = 10)),
                        axis.title.y = element_text(margin = margin(r = 10))
                        
                )
        
        #ELEVATION HISTOGRAMS
        ggplot(data = homog, aes(x = dem)) +
                geom_histogram() +
                facet_grid(Forest_Type~Plot) +
                labs(x = "Mean Forest Plot Elevation (m)", y = "Count") +
                theme_bw() +
                theme(
                        axis.title.x = element_text(margin = margin(t = 10)),
                        axis.title.y = element_text(margin = margin(r = 10))
                        
                )

                
#END HISTOGRAMS BY AGE, ELEV, & REGION --------------------------------------------------------------------------------        
                