#This script exports a CSV with USED SITES (44 total) that has data necessary to create a map & tables 

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(sf)
        library(raster)


        #Import original CSV into data frame
        data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Original_Data/sustherb_all_sites.csv', header = TRUE)

        #Clean site names (to lowercase and replace spaces w/ underscores)
        data$LocalityName <- tolower( sub(' ', '_', data$LocalityName) )
        
        #Clean region names (to lowercase)
        data$Region <- tolower( data$Region )
        
        #Standardize LocalityNames in "data" df 

                #Fritsøe 1
                data$LocalityName[data$LocalityName == "fritsøe1"] <- "fritsoe1"
                
                #Fritsøe 2
                data$LocalityName[data$LocalityName == "fritsøe2"] <- "fritsoe2"

                #Singsaas
                data$LocalityName[data$LocalityName == "singsås"] <- "singsaas"
                
                #Stangeskovene Eidskog
                data$LocalityName[data$LocalityName == "stangeskovene eidskog"] <- "stangeskovene_eidskog"
                data$LocalityName[data$LocalityName == "stangeskovene eidskog "] <- "stangeskovene_eidskog"
                data$LocalityName[data$LocalityName == "stangeskovene_eidskog "] <- "stangeskovene_eidskog"
                
                
                #Stig Dahlen
                data$LocalityName[data$LocalityName == "stig_dæhlen"] <- "stig_dahlen"
               
                #Sørum 1
                data$LocalityName[data$LocalityName == "sørum_1"] <- "sorum_1"
                
                
        ##FILTER OUT 3 TRØNDELAG SITES THAT WERE ACCIDENTALLY THINNED IN 2015 (sites #9, 10, 13)----
                
                #SITE 9: MALVIK (MAB + MAUB)
                
                #SITE 10: SELBU_FLUB (FLB + FLUB)
                
                #SITE 13: HI_TYDAL (HIB + HIUB)
                
                bad_sites <- c("MAB", "MAUB", "FLB", "FLUB", "HIB", "HIUB")
                data <- data[!data$LocalityCode %in% bad_sites,] #Brings us to 44 LocalityNames (looks good)
                
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
                        productivity$LocalityName[productivity$LocalityName == "stangeskovene eidskog "] <- "stangeskovene_eidskog"
                        productivity$LocalityName[productivity$LocalityName == "stangeskovene eidskog"] <- "stangeskovene_eidskog"
                        
                        #Stig Dahlen
                        productivity$LocalityName[productivity$LocalityName == "stig dæhlen"] <- "stig_dahlen"
                        
                        #Truls Holm
                        productivity$LocalityName[productivity$LocalityName == "truls holm"] <- "truls_holm"
                        
                        #Kongsvinger 1 & 2
                        productivity$LocalityName[productivity$LocalityName == "kongsvinger 1"] <- "kongsvinger_1"
                        productivity$LocalityName[productivity$LocalityName == "kongsvinger 2"] <- "kongsvinger_2"
                        
                        #Maarud 1,2,3
                        productivity$LocalityName[productivity$LocalityName == "maarud 1"] <- "maarud_1"
                        productivity$LocalityName[productivity$LocalityName == "maarud 2"] <- "maarud_2"
                        productivity$LocalityName[productivity$LocalityName == "maarud 3"] <- "maarud_3"
                        
                        #Nes 1,2
                        productivity$LocalityName[productivity$LocalityName == "nes 1"] <- "nes_1"
                        productivity$LocalityName[productivity$LocalityName == "nes 2"] <- "nes_2"
                        
                        #Sørum 1
                        productivity$LocalityName[productivity$LocalityName == "sørum 1"] <- "sorum_1"
                
                

        #Add site # from north to south (i.e. high to low latitude)
                
                #Add site_number column
                data$Site_Number <- as.integer('')
        
                #Sort by latitude column (high to low)
                sorted <- data[order(-data$Latitude),]
                
                #Create vector of site numbers (same site number for B and UB)
                nums <- rep(c(1:44), each = 2)
                
                #Add to df
                sorted$Site_Number <- nums
                
                
                
        #Add productivity & herbivore density data
                
                #Placeholder column
                sorted$Productivity_Index <- as.numeric('')
                
                #Loop through sites (LocalityName) in sorted
                sites <- as.character(levels(as.factor(data$LocalityName)))
                for(i in 1:length(sites)){
                        s <- sites[i]
                        sorted$Productivity_Index[sorted$LocalityName == s] <- productivity$Productivity[productivity$LocalityName == s]
                }
                
                #Add herbivore density data (and district id) ---
                
                #Placeholder columns
                sorted$Moose_Density <- as.numeric('')
                sorted$Red_Deer_Density <- as.numeric('')
                sorted$Roe_Deer_Density <- as.numeric('')
                
                for(i in 1:length(sites)){
                        
                        s <- sites[i]
                        
                        #Get district ID from site_data
                        id <- as.character(sorted$DistrictID[sorted$LocalityName == s][1])
                        
                        #Format to match hbiomass2015 df (3-digit codes need 0 in front)
                        if(nchar(id) == 3){
                                id <- paste("0", id, sep = "")
                        }
                        
                        sorted$DistrictID[sorted$LocalityName == s] <- id
                        
                        #Herbivore Densities ---
                        
                        sorted$Moose_Density[sorted$LocalityName == s] <- hbiomass2015$`hbiomass$Ms_2015`[hbiomass2015$kommnnr == id]
                        sorted$Red_Deer_Density[sorted$LocalityName == s] <- hbiomass2015$`hbiomass$Rd__2015`[hbiomass2015$kommnnr == id]
                        sorted$Roe_Deer_Density[sorted$LocalityName == s] <- hbiomass2015$`hbiomass$R_d_2015`[hbiomass2015$kommnnr == id]
                        
                }
                
        #Filter to B only (to get site-level data)
        sorted <- sorted[sorted$Treatment == "B",]
        
        
        #Add climate data
        clim <- read.csv('1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Averages/average_climate_data_by_site.csv', header = T)
        
                #Add placeholder climate columns
                sorted$Min_Temp <- as.numeric('')
                sorted$Max_Temp <- as.numeric('')
                sorted$Min_SWE <- as.numeric('')
                sorted$Max_SWE <- as.numeric('')
                sorted$Temp_Range <- as.numeric('')
                sorted$SWE_Range <- as.numeric('')
                
                #Loop through
                for(i in 1:length(sites)){
                        
                        s <- sites[i]
                        
                        #Temp ----
                        
                                #Min
                                min_t <- min(clim$Temperature_K[clim$LocalityName == s])
                        
                                #Max
                                max_t <- max(clim$Temperature_K[clim$LocalityName == s])
                                
                                
                        #SWE ----
                                
                                #Min
                                min_s <- min(clim$SWE_mm[clim$LocalityName == s])
                                
                                #Max
                                max_s <- max(clim$SWE_mm[clim$LocalityName == s])
                        
                        #Add to df ---
                                
                                sorted$Min_Temp[sorted$LocalityName == s] <- min_t
                                sorted$Max_Temp[sorted$LocalityName == s] <- max_t
                                sorted$Min_SWE[sorted$LocalityName == s] <- min_s
                                sorted$Max_SWE[sorted$LocalityName == s] <- max_s
                                sorted$Temp_Range[sorted$LocalityName == s] <- max_t - min_t
                                sorted$SWE_Range[sorted$LocalityName == s] <- max_s - min_s
                                
                }
                
                
                
                
#WRITE TO CSV
                
        #Export data
        write.csv(sorted, '1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/sites_map_data.csv', row.names = TRUE)
        