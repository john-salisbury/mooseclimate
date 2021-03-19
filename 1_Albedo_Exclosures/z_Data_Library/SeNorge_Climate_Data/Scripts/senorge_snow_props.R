# This script computes avg. proportion of days per month of snow (i.e. SWE > 0) for each SustHerb study site 

# For a given site, the snow prop in month i across all years of vegetation data collection is averaged


#LOAD PACKAGES -----------------------------

        library(dplyr)
        library(ggplot2)
        library(cowplot)
        library(wesanderson)
        library(beepr)

#END LOAD PACKAGES -----------------------------



#////////////////////////////////////////////



#LOAD DATA -----------------------------

        #Import SeNorge Climate Data for all sites (data from 2000-2019)
        climate_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Original_Data/tro_hed_tel_utm33_2001_2019.csv', header = T)

        #Import SustHerb locality code key (which connects climate data to each individual LocalityCode)
        #NOTE: 'FID' variable is unique ID for each LocalityCode
        senorge_locs <- read.csv('1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Original_Data/tro_hed_tel_utm33_localities.csv', header = T)
        
        #Load site data for SustHerb sites used in project analysis
        site_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/cleaned_data.csv', header = T)
        

        
#END LOAD DATA -----------------------------
        
        
        
#////////////////////////////////////////////



#FORMAT/FILTER DATA -----------------------------

        #Filter SeNorge data to sites that are used in rest of analysis (as they have full data from Snøan's project)
        #(Note: These are the 74 sites/37 locality names in the 'site_data' CSV)
        
                #Use 'LocalityCode' variable to filter out FIDs that correspond to unused SustHerb sites
                        
                        #Get vector of LocalityCodes for used sites
                        site_data$LocalityCode <- as.factor(site_data$LocalityCode)
                        used_sites <- levels(site_data$LocalityCode)
                        
                        #Filter down sites in 'senorge_locs' df
                        senorge_locs$LocalityCode <- as.factor(senorge_locs$LocalityCode)
                        senorge_locs <- senorge_locs[senorge_locs$LocalityCode %in% used_sites,]
                        
                                #Filters to 37 localities, which matches up with used sites (looks good)
                                #Now have only 'used sites' in senorge_locs df
                        
                #Filter SeNorge data by FIDs corresponding to 'used sites' (i.e. those in senorge_locs df)
                        
                        #Get vector of FIDs
                        fids <- senorge_locs$FID
                        
                        #Filter SeNorge climate data
                        climate_filt <- climate_data[climate_data$trondelag_ %in% fids,]
                        
                                #Looks good - filtered properly
                        
        #Filter date range of climate data to min and max of SustHerb study period
                        
                #Get earliest 'year initiated' from used sites
                min_year <- min(site_data$Year.initiated)
                
                #Max year is 2019 (most recent SustHerb tree data available)
                max_year <- 2019
                
                #Filter climate data between min year and max year
                climate_filt <- climate_filt[climate_filt$X_Year >= min_year &
                                                     climate_filt$X_Year <= max_year,]
                
                        #Looks good - climate data is filtered to 'used sites' and years of
                        #SustHerb study period
                
                
        #Convert temperatures from C to K
        climate_filt$Temperature_K <- climate_filt$tm_celsius + 273.15
        
        
        #Assign REGION (Trøndelag, Telemark, and Hedmark) and LOCALITYCODE to data
        
                #Placeholder columns
                climate_filt$Region <- as.character('')
                climate_filt$LocalityCode <- as.character('')
                
                #Loop through observations
                for(i in 1:nrow(climate_filt)){
                        
                        print(i)
                        
                        #Get FID from climate data
                        id <- climate_filt[i, "trondelag_"]
                        
                        #Get variables corresponding to FID from locs data
                        
                                #Region
                                reg <- senorge_locs$Region[senorge_locs$FID == id]
                                
                                #LocalityCode
                                loc <- as.character(senorge_locs$LocalityCode[senorge_locs$FID == id][1])

                        #Add to main df
                        climate_filt[i, "Region"] <- reg
                        climate_filt[i, "LocalityCode"] <- loc
                }
                
                beep(8)
                
        
        
        

#END FORMAT/FILTER DATA -----------------------------
        
        
        
#////////////////////////////////////////////



#COMPUTE AVERAGE T & SWE **BY SITE** --------------------------------------------
                
        #Define function to calculate SE
        std <- function(x) sd(x)/sqrt(length(x))
                
        #Construct blank final df
        final_data <- data.frame("Region" = character(),
                                 "LocalityName" = character(),
                                 "Month" = integer(),
                                 "Avg_Prop_Days_Snow" = numeric())
                
        #Loop through each LocalityName and calculate average SWE & Temp for all months
        
                #Create vector of 'used' LocalityNames (in correct format)
                used_locs <- levels(as.factor(site_data$LocalityName))
                
                #Loop
                for(i in 1:length(used_locs)){
                        
                        print(i)
                        
                        #Grab locality name from vector
                        loc <- used_locs[i]
                        
                        #Get region
                        reg <- site_data$Region[site_data$LocalityName == loc][1]
                        
                                #Put region into correct/uniform format
                                if(reg == "trondelag"){
                                        reg <- "Trøndelag"
                                } else if (reg == "hedmark"){
                                        reg <- "Hedmark"
                                } else if (reg == "telemark"){
                                        reg <- "Telemark"
                                }
                        
                        #Get LocalityCode of 'browsed' site in LocalityName i
                        #(site FID is associated w/ browsed site)
                        br <- as.character(site_data$LocalityCode[site_data$LocalityName == loc & site_data$Treatment == "open"])
                        
                        
                        #Loop through months 1-12 and calculate average prop of days w/ SWE > 0 for each LocalityName
                        for(j in 1:12){
                                
                                #Temp df
                                temp <- data.frame("Region" = character(),
                                                   "LocalityName" = character(),
                                                   "Month" = integer(),
                                                   "Avg_Prop_Days_Snow" = numeric())
                                
                                temp[nrow(temp)+1,] <- NA
                
                                #In each year, calculate prop of days in month j with SWE > 0
                                
                                        #Min year
                                        min_year <- min(climate_filt$X_Year[climate_filt$X_Month == j &
                                                                                    climate_filt$LocalityCode == br])
                                        
                                        #Max year
                                        max_year <- max(climate_filt$X_Year[climate_filt$X_Month == j &
                                                                                    climate_filt$LocalityCode == br])
                                        
                                        #Create vector to hold proportions
                                        props <- vector()
                                        
                                        #Loop through years
                                        for(k in min_year:max_year){
                                                
                                                #Calculate prop of days w/ snow
                                                        
                                                        #Total days
                                                        days <- nrow(climate_filt[climate_filt$X_Month == j &
                                                                                          climate_filt$LocalityCode == br &
                                                                                          climate_filt$X_Year == k,])
                                                        
                                                        #Days w/ snow
                                                        snow_days <- nrow(climate_filt[climate_filt$X_Month == j &
                                                                                               climate_filt$LocalityCode == br &
                                                                                               climate_filt$X_Year == k &
                                                                                               climate_filt$swe_mm > 0,])
                                                        
                                                        #Proportion
                                                        prop <- snow_days/days
                                                        
                                                #Add prop to vector
                                                props <- c(props, prop)
                                                
                                        }
                                        
                                #Average annual values of prop snow in month j
                                avg_prop <- mean(props)


                                #Add values to temp df
                                
                                temp$Region <- reg
                                temp$LocalityName <- loc
                                temp$Month <- j
                                temp$Avg_Prop_Days_Snow <- avg_prop
                                
                                
                                #Append temp df to final df
                                final_data <- rbind(final_data, temp)
                                
                                
                        }
                        
                        
                        
                }
                
                beep(8)
                
                
        #WRITE CSV OF SITE-SPECIFIC CLIMATE AVERAGES -----------
        write.csv(final_data, '1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Snow_Proportions/average_prop_day_with_snow.csv')
                
        