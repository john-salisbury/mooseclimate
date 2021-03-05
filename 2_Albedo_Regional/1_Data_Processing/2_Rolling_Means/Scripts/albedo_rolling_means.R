## This is a script to calculate rolling means for species-specific albedo values in 
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




#ALBEDO ROLLING MEANS --------------------------------------------------------------------------------------
        
        #NOTE: Too many levels to this data to plot all months of albedo
        #Going to filter to January and July (as in albedo spreadsheet)
        
                #Filter to Jan and Jul (months = 1 | 7)
                albedo_df <- data[data$Month %in% c(1,7),]
                rm(data)
                
        #(1)  #Aggregate means by MONTH, REGION, & MOOSE DENSITY (i.e. by kommune, since moose density
        #is on a kommune scale)
                
                #NOTE: USING HOMOGENOUS PIXELS (75% coverage or more)
                
                        #Pine
                        p_alb <- albedo_df[albedo_df$fur_pct >= 75,]
                        p_alb <- aggregate(p_alb$Pn_Albd, by = list("Region" = p_alb$Plot,
                                                                        "Ms_Dnst" = p_alb$Ms_Dnst,
                                                                        "Month" = p_alb$Month), FUN = mean)
                        colnames(p_alb)[4] <- 'Mean_Albedo'
                        
                        
                        #Spruce
                        s_alb <- albedo_df[albedo_df$grn_pct >= 75,]
                        s_alb <- aggregate(s_alb$Sprc_Al, by = list("Region" = s_alb$Plot,
                                                                    "Ms_Dnst" = s_alb$Ms_Dnst,
                                                                    "Month" = s_alb$Month), FUN = mean)
                        colnames(s_alb)[4] <- 'Mean_Albedo'
                        
                        #Deciduous
                        d_alb <- albedo_df[albedo_df$lav_pct >= 75,]
                        d_alb <- aggregate(d_alb$Dcd_Alb, by = list("Region" = d_alb$Plot,
                                                                    "Ms_Dnst" = d_alb$Ms_Dnst,
                                                                    "Month" = d_alb$Month), FUN = mean)
                        colnames(d_alb)[4] <- 'Mean_Albedo'
        
        
        
        
        #(2) CALCULATE ROLLING MEANS --------
                        

                #Trondelag -----
                        
                        #Deciduous
                        d_alb$Rolling_Mean_Albedo[d_alb$Region == "Trondelag" & d_alb$Month == 1] <- rollmean(d_alb$Mean_Albedo[d_alb$Region == "Trondelag" & d_alb$Month == 1], 5, align = "right", na.pad = T)    
                        d_alb$Rolling_Mean_Albedo[d_alb$Region == "Trondelag" & d_alb$Month == 7] <- rollmean(d_alb$Mean_Albedo[d_alb$Region == "Trondelag" & d_alb$Month == 7], 5, align = "right", na.pad = T)    
                        
                        #Pine
                        p_alb$Rolling_Mean_Albedo[p_alb$Region == "Trondelag" & p_alb$Month == 1] <- rollmean(p_alb$Mean_Albedo[p_alb$Region == "Trondelag" & p_alb$Month == 1], 5, align = "right", na.pad = T)    
                        p_alb$Rolling_Mean_Albedo[p_alb$Region == "Trondelag" & p_alb$Month == 7] <- rollmean(p_alb$Mean_Albedo[p_alb$Region == "Trondelag" & p_alb$Month == 7], 5, align = "right", na.pad = T)    
                        
                        #Spruce
                        s_alb$Rolling_Mean_Albedo[s_alb$Region == "Trondelag" & s_alb$Month == 1] <- rollmean(s_alb$Mean_Albedo[s_alb$Region == "Trondelag" & s_alb$Month == 1], 5, align = "right", na.pad = T)    
                        s_alb$Rolling_Mean_Albedo[s_alb$Region == "Trondelag" & s_alb$Month == 7] <- rollmean(s_alb$Mean_Albedo[s_alb$Region == "Trondelag" & s_alb$Month == 7], 5, align = "right", na.pad = T)    

                        
                #Hedmark -----
                        
                        #Deciduous
                        d_alb$Rolling_Mean_Albedo[d_alb$Region == "Hedmark" & d_alb$Month == 1] <- rollmean(d_alb$Mean_Albedo[d_alb$Region == "Hedmark" & d_alb$Month == 1], 5, align = "right", na.pad = T)    
                        d_alb$Rolling_Mean_Albedo[d_alb$Region == "Hedmark" & d_alb$Month == 7] <- rollmean(d_alb$Mean_Albedo[d_alb$Region == "Hedmark" & d_alb$Month == 7], 5, align = "right", na.pad = T)    
                        
                        #Pine
                        p_alb$Rolling_Mean_Albedo[p_alb$Region == "Hedmark" & p_alb$Month == 1] <- rollmean(p_alb$Mean_Albedo[p_alb$Region == "Hedmark" & p_alb$Month == 1], 5, align = "right", na.pad = T)    
                        p_alb$Rolling_Mean_Albedo[p_alb$Region == "Hedmark" & p_alb$Month == 7] <- rollmean(p_alb$Mean_Albedo[p_alb$Region == "Hedmark" & p_alb$Month == 7], 5, align = "right", na.pad = T)    
                        
                        #Spruce
                        s_alb$Rolling_Mean_Albedo[s_alb$Region == "Hedmark" & s_alb$Month == 1] <- rollmean(s_alb$Mean_Albedo[s_alb$Region == "Hedmark" & s_alb$Month == 1], 5, align = "right", na.pad = T)    
                        s_alb$Rolling_Mean_Albedo[s_alb$Region == "Hedmark" & s_alb$Month == 7] <- rollmean(s_alb$Mean_Albedo[s_alb$Region == "Hedmark" & s_alb$Month == 7], 5, align = "right", na.pad = T)    
                        
                        
                        
                        
                        
                        #Add species-specific columns
                        d_alb$Group <- 'Deciduous'
                        p_alb$Group <- 'Pine'
                        s_alb$Group <- 'Spruce'
                        
                        #Unify
                        roll_means <- rbind(d_alb, p_alb, s_alb)
                        
                        #Fix class
                        roll_means$Rolling_Mean_Albedo <- as.numeric(roll_means$Rolling_Mean_Albedo)
                        
                        #Add month "nice name" column
                        roll_means$Month_Name <- as.character('')
                        roll_means$Month_Name[roll_means$Month == 1] <- 'Jan'
                        roll_means$Month_Name[roll_means$Month == 7] <- 'Jul'
                        
                        
                        
        #(3) WRITE FINAL CSV ------------
                        
                #Write CSV
                write.csv(roll_means, "2_Albedo_Regional/1_Data_Processing/2_Rolling_Means/Output/Albedo/albedo_rolling_means.csv")
                
                
                        
        #(4) GENERATE PLOTS ---------------
                        

                #ALBEDO VS MOOSE DENSITY FOR MONTHS 1 and 7 (FACETED BY SPECIES & GROUPED BY TREATMENT)
                
                pd <- position_dodge(2)
                
                #Generate plot
                ggplot(data = roll_means, aes(x = Ms_Dnst, y = Rolling_Mean_Albedo, color = Region)) +
                        geom_point(aes(shape = Region), size = 1, position = pd) +
                        geom_line(aes(linetype = Region), position = pd) +
                        facet_grid(Group~Month_Name) +
                        labs(x = "Moose Metabolic Biomass  " ~(kg/km^2), y = "Albedo") +
                        theme_bw() +
                        theme(
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10))
                        )
                        
                        
                        
#END ALBEDO ROLLING MEANS -----------------------------------------------------------------------------------
        
      