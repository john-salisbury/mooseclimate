## Script to calculate albedo estimates for all 37 'used sites' (Trøndelag, Hedmark, Telemark)
## across all years of available volume data. 

## STEP 1: volume (m3/ha) and albedo estimates are estimated on the subplot level
## Albedo estimates are calculated for each 'group' within each subplot, and then a weighted average 
## (based on tree species proportions within each subplot) is used to calculate a 'composite'
## subplot albedo value

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
        library(beepr)
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        ## SITE DATA
        
                #Get 'cleaned' site data from adjacent 'Sites' folder
                site_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/all_sites_data.csv', header = TRUE)


        #SUBPLOT STAND VOLUMES (m3/ha) (Trondelag/Hedmark/Telemark: 2009-2019) - circular subplots within each plot/LocalityCode

                #Load subplot volumes
                plot_volumes <- read.csv('1_Albedo_Exclosures/1_Data_Processing/3_Volume_Estimates/Output/subplot_tree_volumes.csv', header = TRUE)
        
        
        #AVG. SENORGE SWE (mm) and TEMP (K) DATA (one set of values, averaged from data across 2009-2019) 
                
                clim <- read.csv('1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Averages/average_climate_data_by_site.csv', header = TRUE)
                
                
        #HERBIVORE DENSITY DATA (2015 - MUNICIPALITY RESOLUTION)
                        
                #Read in herbivore biomass data (2015) from SpatialPolygons object (isolate dataframe)
                hbiomass_shp <- shapefile("1_Albedo_Exclosures/z_Data_Library/Herbivore_Density_Data/Usable_Data/NorwayLargeHerbivores")
                
                #Pull out dataframe
                hbiomass <- hbiomass_shp@data
                
                #Isolate to 2015 moose, roe deer, and red deer density data
                hbiomass2015 <- cbind(hbiomass[,c(1:10)], hbiomass$Ms_2015, hbiomass$Rd__2015, hbiomass$R_d_2015)
                
                
        ## SUSTHERB SITE PRODUCTIVITY INDICES
                
                #Productivity Data
                productivity <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/productivity_all_sites.csv', header = TRUE)
                productivity$LocalityName <- tolower(productivity$LocalityName)
                
                #Correct LocalityName items in productivity CSV
                
                        #Didrik Holmsen
                        productivity$LocalityName[productivity$LocalityName == "didrik holmsen"] <- "didrik_holmsen"
                        
                        #Fet 3
                        productivity$LocalityName[productivity$LocalityName == "fet 3"] <- "fet_3"
                        
                        #Fritsøe 1
                        productivity$LocalityName[productivity$LocalityName == "fritsøe1"] <- "fritsoe1"
                        
                        #Fritsøe 2
                        productivity$LocalityName[productivity$LocalityName == "fritsøe2"] <- "fritsoe2"
                        
                        #Halvard Pramhus
                        productivity$LocalityName[productivity$LocalityName == "halvard pramhus"] <- "halvard_pramhus"
                        
                        #Singsaas
                        productivity$LocalityName[productivity$LocalityName == "singsås"] <- "singsaas"
                        
                        #Stangeskovene Aurskog
                        productivity$LocalityName[productivity$LocalityName == "stangeskovene aurskog"] <- "stangeskovene_aurskog"
                        
                        #Stangeskovene Eidskog
                        productivity$LocalityName[productivity$LocalityName == "stangeskovene eidskog"] <- "stangeskovene_eidskog"
                        
                        #Stig Dahlen
                        productivity$LocalityName[productivity$LocalityName == "stig dæhlen"] <- "stig_dahlen"
                        
                        #Truls Holm
                        productivity$LocalityName[productivity$LocalityName == "truls holm"] <- "truls_holm"
                        
                        #Nes 1
                        productivity$LocalityName[productivity$LocalityName == "nes 1"] <- "nes_1"
                        
                        #Nes 2
                        productivity$LocalityName[productivity$LocalityName == "nes 2"] <- "nes_2"
                        
                        #Kongsvinger 1
                        productivity$LocalityName[productivity$LocalityName == "kongsvinger 1"] <- "kongsvinger_1"
                        
                        #Kongsvinger 2
                        productivity$LocalityName[productivity$LocalityName == "kongsvinger 2"] <- "kongsvinger_2"
                        
                        #Maarud 1-3
                        productivity$LocalityName[productivity$LocalityName == "maarud 1"] <- "maarud_1"
                        productivity$LocalityName[productivity$LocalityName == "maarud 2"] <- "maarud_2"
                        productivity$LocalityName[productivity$LocalityName == "maarud 3"] <- "maarud_3"
                        
                        #Sørum 1
                        productivity$LocalityName[productivity$LocalityName == "sørum 1"] <- "sorum_1"
                        
                        

        ## SUSTHERB TREE SPECIES PROPORTIONS (SUBPLOT LEVEL - 2009-2019)
                        
                #Tree density data for Trøndelag, Telemark, and Hedmark (2009-2019)
                tree_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/Tree_Data/Usable_Data/tree_species_proportions_subplot_level.csv', header = TRUE)
                tree_data <- tree_data[tree_data$Year <= 2019, 2:8] #94 different LocalityCodes
                

#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#STEP 1: CALCULATE ALBEDO AT SUBPLOT RESOLUTION --------------------------------------------------------------------------------------

   
        #Calculate albedo for each species/group within each subplot (across all years of data - 2009-2019)
        
                #Prepare dataframe
                        
                        #Copy volumes dataset (add columns to it)
                        albedo <- plot_volumes
                        
                        #Duplicate each observation 12x (once for each month)
                        albedo <- albedo[rep(seq_len(nrow(albedo)), each = 12), ]
                        
                        #Add placeholder columns
                        albedo$Year <- as.integer('')
                        albedo$Month <- as.integer('')
                        albedo$SWE_mm <- as.numeric('')
                        albedo$Temp_K <- as.numeric('')
                        albedo$Albedo <- as.numeric('')
                        
                        #Add vector of months to "Month" column
                        albedo$Month <- rep(c(1:12), times = (nrow(albedo) / 12))
                        
                        #Fix issue w/ stangeskovene eidskog trailing space (which throws an error)
                        albedo$LocalityName[albedo$LocalityName == "stangeskovene eidskog "] <- "stangeskovene_eidskog"

                        
                        #Produce albedo estimates
                        for(i in 1:nrow(albedo)){
                                
                                print(i)
                                
                                #Site info
                                
                                        #LocalityCode
                                        a <- albedo[i, "LocalityCode"]
                                        
                                        #LocalityName
                                        b <- albedo[i, "LocalityName"]
                                
                                        #Treatment
                                        tr <- albedo[i, "Treatment"]
                                        
                                        #Region
                                        reg <- albedo[i, "Region"]
                                        
                                        #Year of Exclosure
                                        yr_1 <- site_data$Year.initiated[site_data$LocalityCode == a]
                                        
                                #Species Info
                                        
                                        #Species
                                        spec <- albedo[i, "Group"]
                                        
                                #Date info
                                        
                                        #Current Year
                                        yr_curr <- albedo[i, "Years_Since_Exclosure"] + yr_1
                                        albedo[i, "Year"] <- yr_curr
                                        
                                        #Month
                                        mt <- albedo[i, "Month"]
                                
                                #Get climate data for corresponding month in CORRESPONDING REGION ------
                                        
                                        #Use average values from across study period (2009-2019) for all years
                                        #AT EACH SUSTHERB SITE (connect via LocalityName variable)
                                        
                                                #Temperature (K)
                                                albedo[i, "Temp_K"] <- clim$Temperature_K[clim$Month == mt &
                                                                                                  clim$LocalityName == b]
                                        
                                                #SWE (mm)
                                                albedo[i, "SWE_mm"] <- clim$SWE_mm[clim$Month == mt &
                                                                                           clim$LocalityName == b]
                                
                                    
                                #Calculate albedo (with species-specific equation)
                                        
                                        if(spec == "Deciduous") {
                                                
                                                #Deciduous-specific equation
                                                albedo[i, "Albedo"] <- 0.085+0.089*(1-1/(1+exp(-2.414*(albedo[i, "Temp_K"] - 273.393))))+0.169*(1/(1+exp(-0.107*(albedo[i, "SWE_mm"] - 37.672))))+0.245*exp(-0.023 * albedo[i, "Volume_m3ha"])*(1-0.7*exp(-0.004 * albedo[i, "SWE_mm"]))

                                        } else if (spec == "Pine") {
                                                
                                                #Pine-specific equation
                                                albedo[i, "Albedo"] <- 0.069+0.084*(1-1/(1+exp(-1.965*(albedo[i, "Temp_K"] - 273.519))))+0.106*(1/(1+exp(-0.134*(albedo[i, "SWE_mm"] - 30.125))))+0.251*exp(-0.016 * albedo[i, "Volume_m3ha"])*(1-0.7*exp(-0.008 * albedo[i, "SWE_mm"]))
                                                
                                        } else if (spec == "Spruce"){
                                                
                                                #Spruce-specific equation
                                                albedo[i, "Albedo"] <- 0.077+0.072*(1-1/(1+exp(-2.354*(albedo[i, "Temp_K"] - 273.433))))+0.074*(1/(1+exp(-0.191*(albedo[i, "SWE_mm"] - 33.093))))+0.252*exp(-0.023 * albedo[i, "Volume_m3ha"])*(1-0.7*exp(-0.011 * albedo[i, "SWE_mm"]))

                                        }
                                        
                        }
                        
                beep(8)
                        
                #Remove arbitrary index column
                albedo <- albedo[,c(2:15)]
                
                #Remove "Volume_m3" column
                albedo <- albedo[,c(1:7,9:14)]
                        
                
                        
        #CALCULATE SINGLE COMPOSITE ALBEDO VALUE IN SEPARATE DATAFRAME
                        
                #Construct placeholder df w/ same structure as albedo df
                comp <- albedo[0,]
                
                #Get vector of localitycodes
                used_sites <- levels(as.factor(albedo$LocalityCode))
        
                #Loop through all plots of all sites in all months of all years
                for(i in 1:length(used_sites)){
                        
                        print(i)
                        
                        #Site info
                        
                                #Region (MAKE SURE TO GET UPPERCASE VERSION OF REGION - grab from 'plot_volumes' df)
                                reg <- plot_volumes$Region[plot_volumes$LocalityCode == used_sites[i]][1]
                        
                                #LocalityName
                                site <- site_data$LocalityName[site_data$LocalityCode == used_sites[i]]
                        
                                #LocalityCode
                                loc <- as.character(used_sites[i])
                                
                                #LocalityName
                                loc_name <- as.character(site_data$LocalityName[site_data$LocalityCode == loc])
                        
                                #Treatment
                                tr <- site_data$Treatment[site_data$LocalityCode == used_sites[i]]
                                
                                #Group
                                grp <- "Composite"
                                
                                #Year of Exclosure
                                yr_1 <- site_data$Year.initiated[site_data$LocalityCode == used_sites[i]]
                                
                                #Min year of data (exclosure year + 1)
                                min_year <- min(albedo$Year[albedo$LocalityCode == used_sites[i]])
                                
                                #Max year of data
                                max_year <- max(albedo$Year[albedo$LocalityCode == used_sites[i]])
                                
                        #Loop through each year
                        for(j in min_year:max_year){
                                
                                #Current year
                                yr_curr <- j
                                
                                #Years Since Exclosure
                                yse <- yr_curr - yr_1
                                
                                #Loop through each subplot
                                subplots <- levels(as.factor(albedo$Subplot[albedo$LocalityCode == used_sites[i] & albedo$Year == j]))
                                
                                for(k in 1:length(subplots)){

                                        #Loop through each month
                                        for(l in 1:12){
                                                
                                                #Month
                                                month <- l
                                                
                                                #SWE (from corresponding region)
                                                swe <- clim$SWE_mm[clim$Month == l &
                                                                           clim$LocalityName == loc_name]
                                                
                                                #Temp (from corresponding region)
                                                temps <- clim$Temperature_K[clim$Month == l &
                                                                                    clim$LocalityName == loc_name]
                                                
                                                #CALCULATE COMPOSITE ALBEDO (WEIGHTED AVG BASED ON SPECIES PROPORTIONS)
                                                
                                                        #Deciduous----
                                                
                                                                #Deciduous prop
                                                                if(length(tree_data$Prop_birch[tree_data$LocalityCode == used_sites[i] & tree_data$Year == j & tree_data$Plot == subplots[k]]) == 0){
                                                                        b_prop <- 0
                                                                } else {
                                                                        b_prop <- tree_data$Prop_birch[tree_data$LocalityCode == used_sites[i] & tree_data$Year == j & tree_data$Plot == subplots[k]]
                                                                }
                                                                
                                                                #Deciduous albedo
                                                                if(length(albedo$Albedo[albedo$LocalityCode == used_sites[i] & albedo$Subplot == subplots[k] & albedo$Year == j & albedo$Month == l & albedo$Group == "Deciduous"]) == 0){
                                                                        b_alb <- 0
                                                                } else {
                                                                        b_alb <- albedo$Albedo[albedo$LocalityCode == used_sites[i] & albedo$Subplot == subplots[k] & albedo$Year == j & albedo$Month == l & albedo$Group == "Deciduous"]
                                                                }

                                                        #Pine----
                                                        
                                                                #Pine prop
                                                                if(length(tree_data$Prop_pine[tree_data$LocalityCode == used_sites[i] & tree_data$Year == j & tree_data$Plot == subplots[k]]) == 0){
                                                                        p_prop <- 0
                                                                } else {
                                                                        p_prop <- tree_data$Prop_pine[tree_data$LocalityCode == used_sites[i] & tree_data$Year == j & tree_data$Plot == subplots[k]]
                                                                }
                                                                
                                                                #Pine albedo
                                                                if(length(albedo$Albedo[albedo$LocalityCode == used_sites[i] & albedo$Subplot == subplots[k] & albedo$Year == j & albedo$Month == l & albedo$Group == "Pine"]) == 0){
                                                                        p_alb <- 0
                                                                } else {
                                                                        p_alb <- albedo$Albedo[albedo$LocalityCode == used_sites[i] & albedo$Subplot == subplots[k] & albedo$Year == j & albedo$Month == l & albedo$Group == "Pine"]
                                                                }
                                                        
                                                        #Spruce----
                                                        
                                                                #Spruce prop
                                                                if(length(tree_data$Prop_spruce[tree_data$LocalityCode == used_sites[i] & tree_data$Year == j & tree_data$Plot == subplots[k]]) == 0){
                                                                        s_prop <- 0
                                                                } else {
                                                                        s_prop <- tree_data$Prop_spruce[tree_data$LocalityCode == used_sites[i] & tree_data$Year == j & tree_data$Plot == subplots[k]]
                                                                }
                                                                
                                                                #Spruce albedo
                                                                if(length(albedo$Albedo[albedo$LocalityCode == used_sites[i] & albedo$Subplot == subplots[k] & albedo$Year == j & albedo$Month == l & albedo$Group == "Spruce"]) == 0){
                                                                        s_alb <- 0
                                                                } else {
                                                                        s_alb <- albedo$Albedo[albedo$LocalityCode == used_sites[i] & albedo$Subplot == subplots[k] & albedo$Year == j & albedo$Month == l & albedo$Group == "Spruce"]
                                                                }
                                                
                                                
                                                        
                                                                
                                                        #WEIGHTED AVERAGE OF ALBEDO (using corrected values from "NA check")
                                                        
                                                                comp_alb <- (b_prop*b_alb) + (s_prop*s_alb) + (p_prop*p_alb)
                                                                
                                                        
                                                                
                                                                
                                                #ADD ROW TO "COMP" DATAFRAME
                                                row <- data.frame("Region" = reg,
                                                                  "LocalityName" = site,
                                                                  "LocalityCode" = loc,
                                                                  "Treatment" = tr,
                                                                  "Subplot" = subplots[k],
                                                                  "Years_Since_Exclosure" = yse,
                                                                  "Group" = grp,
                                                                  "Volume_m3ha" = NA,
                                                                  "Year" = j,
                                                                  "Month" = l,
                                                                  "SWE_mm" = swe,
                                                                  "Temp_K" = temps,
                                                                  "Albedo" = comp_alb)
                                                
                                                comp <- rbind(comp, row)
                                        }
                                }
                                

                        }
                        
                }
                
                beep(8)
        
                
        #BIND COMPOSITE ALBEDO DF TO INITIAL ALBEDO DF
        final_albedo <- rbind(albedo, comp)
               

        #WRITE ALBEDO DATASET TO CSV
        write.csv(final_albedo, "1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Subplot_Resolution/subplot_albedo_estimates.csv")

        
#END CALCULATE ALBEDO ---------------------------------------------------------------------------------- 
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



                
#CALCULATE MEAN ALBEDO AT SUBPLOT LEVEL -------------------------------------------------------------------------------------
        
        #START HERE IF ALBEDO ALREADY SAVED TO CSV ------------------
        final_albedo <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Subplot_Resolution/subplot_albedo_estimates.csv", header = T)
        
        #Aggregate means for each group, month, and year since exclosure
        #Note: not doing anything with 'year' variable, since we're using a single set of
        #average climate values per region
        albedo_means <- aggregate(final_albedo$Albedo, by = list("Treatment" = final_albedo$Treatment,
                                                               "Years_Since_Exclosure" = final_albedo$Years_Since_Exclosure,
                                                               "Group" = final_albedo$Group,
                                                               "Month" = final_albedo$Month), FUN = mean)
        
        colnames(albedo_means)[5] <- "Mean_Subplot_Albedo"
        
        
        #Calculate standard error for each mean
        
                #Define function
                std <- function(x) sd(x)/sqrt(length(x))
                
                #Add placeholder columns
                albedo_means$SE <- as.numeric('')
                
                #Calculate SEs for each species/group, month, and year
                for(i in 1:nrow(albedo_means)){
                        
                        #Get variables
                        tr <- albedo_means[i, "Treatment"]
                        yse <- albedo_means[i, "Years_Since_Exclosure"]
                        mt <- albedo_means[i, "Month"]
                        gr <- albedo_means[i, "Group"]
                        
                        #Calculate SE for albedo
                        se <- std(final_albedo$Albedo[final_albedo$Treatment == tr &
                                                              final_albedo$Group == gr &
                                                              final_albedo$Years_Since_Exclosure == yse &
                                                              final_albedo$Month == mt])
                        
                        #Add to df
                        albedo_means[i, "SE"] <- se
                        
                }
                beep(8)
        
                
        #Add a 'season' variable for further grouping
                
                #Placeholder column
                albedo_means$Season <- as.character('')
                
                #Conditions
                albedo_means$Season[albedo_means$Month %in% c(1:3)] <- "Winter"
                albedo_means$Season[albedo_means$Month %in% c(4:6)] <- "Spring"
                albedo_means$Season[albedo_means$Month %in% c(7:9)] <- "Summer"
                albedo_means$Season[albedo_means$Month %in% c(10:12)] <- "Autumn"
                
                #Set as factor
                albedo_means$Season <- as.factor(albedo_means$Season)
                
                
                #WRITE CSV OF MEAN SUBPLOT ALBEDO VALUES BY REGION
                write.csv(albedo_means, "1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Subplot_Resolution/mean_subplot_albedo.csv")
                
                
        
#END CALCULATE MEAN ALBEDO AT SUBPLOT LEVEL -------------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#CALCULATE MEAN ALBEDO **BY REGION** AT SUBPLOT LEVEL -------------------------------------------------------------------------------------
        
                
        #Aggregate means for each group, month, and year since exclosure (WITHIN EACH REGION)
        #Note: not doing anything with 'year' variable, since we're using a single set of
        #average climate values per region
        
        albedo_reg <- aggregate(final_albedo$Albedo, by = list("Region" = final_albedo$Region,
                                                                 "Treatment" = final_albedo$Treatment,
                                                                 "Years_Since_Exclosure" = final_albedo$Years_Since_Exclosure,
                                                                 "Group" = final_albedo$Group,
                                                                 "Month" = final_albedo$Month), FUN = mean)
        
        colnames(albedo_reg)[6] <- "Mean_Subplot_Albedo"
        
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
                        se <- std(final_albedo$Albedo[final_albedo$Region == reg &
                                                              final_albedo$Treatment == tr &
                                                              final_albedo$Group == gr &
                                                              final_albedo$Years_Since_Exclosure == yse &
                                                              final_albedo$Month == mt])
                        
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
                
        
        #WRITE CSV OF MEAN SUBPLOT ALBEDO VALUES BY REGION
        write.csv(albedo_reg, "1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Subplot_Resolution/mean_subplot_albedo_by_region.csv")
                
                
#END CALCULATE MEAN ALBEDO BY REGION AT SUBPLOT LEVEL ----------------------------------------------------------------------------------
        
        
        

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#CALCULATE MEAN ALBEDO **BY LOCALITYNAME** AT SUBPLOT LEVEL -------------------------------------------------------------------------------------

        #Aggregate means for each group, month, and year since exclosure (WITHIN EACH REGION)
        #Note: not doing anything with 'year' variable, since we're using a single set of
        #average climate values per region
        
        albedo_site <- aggregate(final_albedo$Albedo, by = list("LocalityName" = final_albedo$LocalityName,
                                                               "Treatment" = final_albedo$Treatment,
                                                               "Years_Since_Exclosure" = final_albedo$Years_Since_Exclosure,
                                                               "Group" = final_albedo$Group,
                                                               "Month" = final_albedo$Month), FUN = mean)
        
        colnames(albedo_site)[6] <- "Mean_Subplot_Albedo"
        
        #VERIFIED THAT AGGREGATE PRODUCED CORRECT MEANS HERE
        
        #Calculate standard error for each mean
        
                #Define function
                std <- function(x) sd(x)/sqrt(length(x))
                
                #Add placeholder columns
                albedo_site$SE <- as.numeric('')
        
                #Calculate SEs for each species/group, month, and year
                for(i in 1:nrow(albedo_site)){
                        
                        print(i)
                        
                        #Get variables
                        loc <- albedo_site[i, "LocalityName"]
                        tr <- albedo_site[i, "Treatment"]
                        yse <- albedo_site[i, "Years_Since_Exclosure"]
                        mt <- albedo_site[i, "Month"]
                        gr <- albedo_site[i, "Group"]
                        
                        #Calculate SE for albedo
                        se <- std(final_albedo$Albedo[final_albedo$LocalityName == loc &
                                                              final_albedo$Treatment == tr &
                                                              final_albedo$Group == gr &
                                                              final_albedo$Years_Since_Exclosure == yse &
                                                              final_albedo$Month == mt])
                        
                        #Add to df
                        albedo_site[i, "SE"] <- se
                        
                }
                beep(8)
        
        
        
        #WRITE CSV OF MEAN SUBPLOT ALBEDO VALUES BY REGION
        write.csv(albedo_site, "1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Subplot_Resolution/mean_subplot_albedo_by_site.csv")
        
        
        
        
#END CALCULATE MEAN ALBEDO **BY LOCALITYNAME** AT SUBPLOT LEVEL -------------------------------------------------------------------------------------
        
        
        