## Script to calculate radiative forcing using albedo and delta biomass (kg/m2)

## DELTA BIOMASS - difference in plot-level biomass (tonC/ha) between each exclosure and
## open plot across all years of available data. Thus, delta biomass will be at the 
## 'study site' (LocalityName) resolution.

## DELTA MONTHLY MEAN ALBEDO - difference in monthly mean albedo (plot-level) between each exclosure and
## open plot across all months and years of available data. Thus, delta albedo will also be at the
## 'study site' (LocalityName) resolution.

## NOTE: biomass is converted from kg dry weight to tonC 


##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(gridExtra)
        library(cowplot)
        library(beepr)
        library(wesanderson)
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT + FORMATTING ----------------------------------------------------------------------------------------------

        ##PLOT-LEVEL ALBEDO DATA ----

                #Read CSV
                albedo <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plot_Resolution/mean_plot_albedo.csv", header = T)
        
        ##PLOT-LEVEL BIOMASS DATA ----
        
                #Read CSV
                biomass <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_biomass.csv", header = T)
        
                
#END INITIAL DATA IMPORT + FORMATTING --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#CONVERT BIOMASS (KG) TO TONS OF CARBON/HA --------------------------------------------------------------------------------------
                
                
        #Generic conversion of 0.5 for ton biomass to ton C
        #(Gower et al., Houghton et al., Nelson et al.)
                
        #(1) Convert biomass (kg) to biomass (ton) ------
                
                #Conversion in new column
                biomass$Plot_Biomass_ton <- biomass$Mean_Plot_Biomass_kg*0.00110231
                
        #(2) Convert biomass to tonC -------
                
                #Use generic conversion of 0.5 for biomass to carbon
                biomass$Plot_Carbon_ton <- biomass$Plot_Biomass_ton*0.5
                
        #(3) Convert tonC to tonC/ha (divide by subplot area in ha) -------
                
                #Calculate subplot area in hectares
                
                        #Each subplot has a radius of 2m - A = pi*r^2
                        subplot_area <- pi*(2^2) #12.57m2
                        
                        #Convert m2 to hectares (ha) - 1m2 = 0.0001 ha (divide by 10,000)
                        subplot_area_ha <- subplot_area/10000
                        
                #Divide total tonC by subplot area (ha)
                        
                        biomass$Plot_tonC_ha <- biomass$Plot_Carbon_ton / subplot_area_ha
        
                
                
#END CONVERT BIOMASS (KG) TO TONS OF CARBON/HA --------------------------------------------------------------------------------------
                        
                        
                        
                        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                        
                        
                        
                        
#CREATE DATAFRAMES FOR RF CALCULATIONS --------------------------------------------------------------------------------------

        #OBJECTIVE: Create a dataframe that has:
        #  (1) Delta albedo for each study site/LocalityName (across all months and years)
        #  (2) Delta biomass (kg/m2) for each study site/LocalityName (across all years)
        #  (3) Delta tonC/ha for each study site/LocalityName (across all years)

        #Construct placeholder dateframe
        final <- data.frame("Region" = as.character(),
                            "LocalityName" = as.character(),
                            "Month" = as.integer(),
                            "Years_Since_Exclosure" = as.integer(),
                            "Delta_Biomass_kg_m2" = as.numeric(),
                            "Delta_tonC_ha" = as.numeric(),
                            "Delta_Albedo" = as.numeric())
                        
                        
        #Get all LocalityName's (i.e. study sites) to loop through
        sites <- levels(as.factor(biomass$LocalityName)) #37 total, 2 plots each
        
        #REMOVE DRANGEDAL3 UB w/ 1 YSE (DRANGEDAL3 B doesn't appear until 2 YSE, thus delta isn't possible at this site)
        
                #Remove from biomass
                biomass <- biomass[!(biomass$LocalityCode == "3DRUB" & biomass$Years_Since_Exclosure == 1),]
                
                #Remove from albedo
                albedo <- albedo[!(albedo$LocalityCode == "3DRUB" & albedo$Years_Since_Exclosure == 1),]
        
        
        #Loop through each LocalityName and calculate deltas
        for(i in 1:length(sites)){
                
                #Define LocalityName from vector of sites
                site <- sites[i]
                print(paste(i, site, sep=""))
                
                #Grab other variables for site i (using biomass df as reference)
                
                        #Region 
                        reg <- biomass$Region[biomass$LocalityName == site][1]
                        
                        #LocalityCodes
                        
                                #Exclosure
                                excl <- biomass$LocalityCode[biomass$LocalityName == site & biomass$Treatment == "UB"][1]
                                
                                #Open
                                open <- biomass$LocalityCode[biomass$LocalityName == site & biomass$Treatment == "B"][1]
                                
                        #Min YSE
                        min_yse <- min(biomass$Years_Since_Exclosure[biomass$LocalityName == site])
                        
                        #Max YSE
                        max_yse <- max(biomass$Years_Since_Exclosure[biomass$LocalityName == site])
                        
                
                #Loop through all years of data and calculate deltas
                for(j in min_yse:max_yse){
                        
                        #(1) Calculate delta biomass (kg/m2)
                        
                                b_ub <- biomass$Mean_Plot_Biomass_kg_m2[biomass$LocalityCode == excl &
                                                                             biomass$Years_Since_Exclosure == j] 
                                
                                b_b <- biomass$Mean_Plot_Biomass_kg_m2[biomass$LocalityCode == open &
                                                                          biomass$Years_Since_Exclosure == j] 
                                
                                delt_bio <- b_ub - b_b
                        
                        
                        #(2) Calculate delta tonC/ha
                                
                                d_ub <- biomass$Plot_tonC_ha[biomass$LocalityCode == excl &
                                                                             biomass$Years_Since_Exclosure == j] 
                                
                                d_b <- biomass$Plot_tonC_ha[biomass$LocalityCode == open &
                                                                            biomass$Years_Since_Exclosure == j] 
                                
                                delt_c <- d_ub - d_b
                        
                        
                        #(3) Loop through each month (1-12) and calculate delta albedo (composite and species-specific)
                        for(k in 1:12){
                                
                                #(3a) Delta composite albedo (in month k and year j)
                                
                                        comp_alb_ub <- albedo$Plot_Albedo[albedo$LocalityCode == excl &
                                                                                  albedo$Group == "Composite" &
                                                                                  albedo$Years_Since_Exclosure == j &
                                                                                  albedo$Month == k]
                                        
                                        comp_alb_b <- albedo$Plot_Albedo[albedo$LocalityCode == open &
                                                                                  albedo$Group == "Composite" &
                                                                                  albedo$Years_Since_Exclosure == j &
                                                                                  albedo$Month == k]
                                        
                                        delt_comp_alb <- comp_alb_ub - comp_alb_b
                                
                                        
                               
                                        
                                #APPEND TEMP DF WITH VARIABLES TO FINAL DF
                                        
                                        #Define temp df to append to 'final' df
                                        temp <- data.frame("Region" = reg,
                                                           "LocalityName" = site,
                                                           "Month" = k,
                                                           "Years_Since_Exclosure" = j,
                                                           "Delta_Biomass_kg_m2" = delt_bio,
                                                           "Delta_tonC_ha" = delt_c,
                                                           "Delta_Albedo" = delt_comp_alb) 
                                        
                                        #Append temp df to final df
                                        final <- rbind(final, temp)
                                        
                        }
                        
                }
                
        }
        beep(8)
        
        
        #ADD LATITUDE AND LONGITUDE OF EACH SITE FOR CRISTINA --------
        
                #Load site data
                site_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/all_sites_data.csv', header = TRUE)
        
                #Add placeholder columns
                final$Latitude <- as.character('')
                final$Longitude <- as.character('')
                
                #Add site name text fixes
                final$LocalityName[final$LocalityName == "nes 1"] <- "nes_1"
                final$LocalityName[final$LocalityName == "nes 2"] <- "nes_2"
                final$LocalityName[final$LocalityName == "kongsvinger 1"] <- "kongsvinger_1"
                final$LocalityName[final$LocalityName == "kongsvinger 2"] <- "kongsvinger_2"
                final$LocalityName[final$LocalityName == "maarud 1"] <- "maarud_1"
                final$LocalityName[final$LocalityName == "maarud 2"] <- "maarud_2"
                final$LocalityName[final$LocalityName == "maarud 3"] <- "maarud_3"
                final$LocalityName[final$LocalityName == "sørum 1"] <- "sorum_1"
                
                #Loop through and add
                for(i in 1:nrow(final)){
                        
                        print(i)
                        
                        #Get LocalityName
                        loc <- final[i, "LocalityName"]
                        
                        #Get latitude and longitude from site data (USING BROWSED PLOT as reference)
                        lat <- site_data$Latitude[site_data$LocalityName == loc & site_data$Treatment == "B"]
                        lon <- site_data$Longitude[site_data$LocalityName == loc & site_data$Treatment == "B"]
                        
                        #Add to row
                        final[i, "Latitude"] <- lat
                        final[i, "Longitude"] <- lon
                }
        
        #WRITE FINAL CSV OF DELTAS
        write.csv(final, "1_Albedo_Exclosures/1_Data_Processing/6_Radiative_Forcing/Output/delta_carbon_albedo.csv")
        
        
        #Create simplified list of lat and lon for each LocalityName (for Cristina)
        coords <- data.frame("Region" = as.character(),
                             "LocalityName" = as.character(),
                             "Latitude" = as.character(),
                             "Longitude" = as.character())
        
        locs <- levels(as.factor(final$LocalityName))
        
        for(i in 1:length(locs)){
                
                loc <- locs[i]
                reg <- final$Region[final$LocalityName == loc][1]
                lat <- final$Latitude[final$LocalityName == loc][1]
                lon <- final$Longitude[final$LocalityName == loc][1]
                
                temp <- data.frame("Region" = reg,
                                   "LocalityName" = loc,
                                   "Latitude" = lat,
                                   "Longitude" = lon)
                
                coords <- rbind(coords, temp)
                
        }
        
        write.csv(coords, "1_Albedo_Exclosures/1_Data_Processing/6_Radiative_Forcing/Output/locality_coordinates.csv")
        
        
        


        
#ENDCREATE DATAFRAMES FOR RF CALCULATIONS --------------------------------------------------------------------------------------
        