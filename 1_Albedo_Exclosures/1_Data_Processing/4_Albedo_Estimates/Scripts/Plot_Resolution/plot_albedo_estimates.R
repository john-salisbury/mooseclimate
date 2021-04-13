## Script to calculate albedo estimates for all 37 'used sites' (Trøndelag, Hedmark, Telemark)
## across all years of available volume data. 

## STEP 1: volume (m3/ha) and albedo estimates are (separately) estimated on the subplot level
## STEP 2: subplot albedo estimates are then averaged for each plot (i.e. LocalityCode)

## Note: albedo estimates are produced using average T and SWE data for CLIMATE AVERAGES SPECIFIC TO EACH LOCALITYNAME (i.e. study site)
## (produced by taking mean of climate data across all years of available SustHerb tree data
## at each site)

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
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        ## SITE DATA
        
                #Get 'cleaned' site data from adjacent 'Sites' folder
                site_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/all_sites_data.csv', header = TRUE)

                
        #HERBIVORE DENSITY DATA (2015 - MUNICIPALITY RESOLUTION)
                        
                #Read in herbivore biomass data (2015) from SpatialPolygons object (isolate dataframe)
                hbiomass_shp <- shapefile("1_Albedo_Exclosures/z_Data_Library/Herbivore_Density_Data/Usable_Data/NorwayLargeHerbivores")
                
                #Pull out dataframe
                hbiomass <- hbiomass_shp@data
                
                #Isolate to 2015 moose, roe deer, and red deer density data
                hbiomass2015 <- cbind(hbiomass[,c(1:10)], hbiomass$Ms_2015, hbiomass$Rd__2015, hbiomass$R_d_2015)
                
       #SUBPLOT ALBEDO ESTIMATES (ALREADY CALCULATED)
                
                subplot_albedo <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Subplot_Resolution/subplot_albedo_estimates.csv", header = T)
                
                
#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#STEP 1: AVERAGE SUBPLOT ALBEDOS FOR EACH PLOT --------------------------------------------------------------------------------------

        #Aggregate means by LocalityCode
        plot_albedo <- aggregate(subplot_albedo$Albedo, by = list("Region" = subplot_albedo$Region,
                                                                  "LocalityName" = subplot_albedo$LocalityName,
                                                                  "LocalityCode" = subplot_albedo$LocalityCode,
                                                                  "Treatment" = subplot_albedo$Treatment,
                                                                  "Group" = subplot_albedo$Group,
                                                                  "Month" = subplot_albedo$Month,
                                                                  "Years_Since_Exclosure" = subplot_albedo$Years_Since_Exclosure), FUN = mean)
        colnames(plot_albedo)[8] <- "Plot_Albedo"
        
        #WRITE CSV OF MEAN PLOT ALBEDO VALUES
        write.csv(plot_albedo, "1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plot_Resolution/mean_plot_albedo.csv")
        
        
#END STEP 1: AVERAGE SUBPLOT ALBEDOS FOR EACH PLOT ---------------------------------------------------------------------------------- 
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



                
#CALCULATE MEAN ALBEDO **BY REGION** AT PLOT LEVEL -------------------------------------------------------------------------------------
        
                
        #Aggregate means for each group, month, and year since exclosure (WITHIN EACH REGION)
        #Note: not doing anything with 'year' variable, since we're using a single set of
        #average climate values per region
        
        albedo_reg <- aggregate(plot_albedo$Plot_Albedo, by = list("Region" = plot_albedo$Region,
                                                                 "Treatment" = plot_albedo$Treatment,
                                                                 "Years_Since_Exclosure" = plot_albedo$Years_Since_Exclosure,
                                                                 "Group" = plot_albedo$Group,
                                                                 "Month" = plot_albedo$Month), FUN = mean)
        
        colnames(albedo_reg)[6] <- "Mean_Plot_Albedo"
        
                #VERIFIED THAT AGGREGATE PRODUCED CORRECT MEANS HERE
        
        #Calculate standard error for each mean
        
                #Define function
                std <- function(x) sd(x)/sqrt(length(x))
                
                #Add placeholder columns
                albedo_reg$SE <- as.numeric('')
        
                #Calculate SEs for each species/group, month, and year
                for(i in 1:nrow(albedo_reg)){
                        
                        #Get variables
                        reg <- albedo_reg[i, "Region"]
                        tr <- albedo_reg[i, "Treatment"]
                        yse <- albedo_reg[i, "Years_Since_Exclosure"]
                        mt <- albedo_reg[i, "Month"]
                        gr <- albedo_reg[i, "Group"]
                        
                        #Calculate SE for albedo
                        se <- std(plot_albedo$Plot_Albedo[plot_albedo$Region == reg &
                                                             plot_albedo$Treatment == tr &
                                                             plot_albedo$Group == gr &
                                                             plot_albedo$Years_Since_Exclosure == yse &
                                                             plot_albedo$Month == mt])
                        
                        #Add to df
                        albedo_reg[i, "SE"] <- se
                        
                }
                beep(8)
                
        #Add a 'season' variable for further grouping
                
                #Placeholder column
                albedo_reg$Season <- as.character('')
                
                #Conditions
                albedo_reg$Season[albedo_reg$Month %in% c(1:3)] <- "Winter"
                albedo_reg$Season[albedo_reg$Month %in% c(4:6)] <- "Spring"
                albedo_reg$Season[albedo_reg$Month %in% c(7:9)] <- "Summer"
                albedo_reg$Season[albedo_reg$Month %in% c(10:12)] <- "Autumn"
                
                #Set as factor
                albedo_reg$Season <- as.factor(albedo_reg$Season)
                
        
        #WRITE CSV OF MEAN PLOT ALBEDO VALUES BY REGION
        write.csv(albedo_reg, "1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plot_Resolution/mean_plot_albedo_by_region.csv")
                
                
#END CALCULATE MEAN ALBEDO BY REGION AT SUBPLOT LEVEL ----------------------------------------------------------------------------------
