#This script computes average temperature and SWE from SeNorge data for all Sustherb sites
#Averages are computed for each month using all years of available data

#LOAD PACKAGES -----------------------------

        library(dplyr)
        library(ggplot2)

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
        
        #Load tree observations for SustHerb sites 
        
        
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
        
        

#END FORMAT/FILTER DATA -----------------------------
        
        
        
#////////////////////////////////////////////



#COMPUTE AVERAGE T & SWE -----------------------------
        
        #Define function to calculate SE
        std <- function(x) sd(x)/sqrt(length(x))

        #MONTHLY AVERAGES FOR EACH YEAR -------
        #Note: these are generated solely for plotting purposes
        
                #Create placeholder df
                monthly_data <- data.frame("Month" = integer(),
                                           "Year" = integer(),
                                           'Temperature_K' = numeric(),
                                           'Temperature_SE' = numeric(),
                                           'SWE_mm' = numeric(),
                                           'SWE_SE' = numeric())
        
                #Loop through years
                for(i in min_year:max_year){
                        
                        #Loop through each month
                        for(j in 1:12){
                                
                                #Temp df
                                temp <- data.frame("Month" = integer(),
                                                   "Year" = integer(),
                                                   'Temperature_K' = numeric(),
                                                   'Temperature_SE' = numeric(),
                                                   'SWE_mm' = numeric(),
                                                   'SWE_SE' = numeric())
                                
                                temp[nrow(temp)+1,] <- NA
                                
                                #Add month
                                temp$Month <- j
                                
                                #Add year
                                temp$Year <- i
                                
                                #Avg. Temp (K) ----
                                
                                        #Calculate mean
                                        monthly_mean_temp <- mean(climate_filt$Temperature_K[climate_filt$X_Year == i & climate_filt$X_Month == j])
                                
                                        #Calculate SE for temp
                                        monthly_temp_se <- std(climate_filt$Temperature_K[climate_filt$X_Year == i & climate_filt$X_Month == j])
                                
                                        #Add to df
                                        temp$Temperature_K <- monthly_mean_temp
                                        temp$Temperature_SE <- monthly_temp_se
                                
                                #Avg. SWE (mm) ----
                                        
                                        #Calculate mean
                                        monthly_mean_swe <- mean(climate_filt$swe_mm[climate_filt$X_Year == i & climate_filt$X_Month == j])
                                        
                                        #Calculate SE for SWE
                                        monthly_swe_se <- std(climate_filt$swe_mm[climate_filt$X_Year == i & climate_filt$X_Month == j])
                                        
                                        #Add to df
                                        temp$SWE_mm <- monthly_mean_swe
                                        temp$SWE_SE <- monthly_swe_se
                                        
                                #Append temp row to main df
                                monthly_data <- rbind(monthly_data, temp)
                        }
                }
        
        
        
        #MONTHLY AVERAGES USING ALL YEARS OF DATA -------
        #Note: this is what will be used in final analysis
        
                #Create placeholder df
                final_data <- data.frame("Month" = integer(),
                                         'Temperature_K' = numeric(),
                                         'Temperature_SE' = numeric(),
                                         'SWE_mm' = numeric(),
                                         'SWE_SE' = numeric())
                        
                #Add 12 blank rows
                final_data[nrow(final_data)+12,] <- NA
                
                #Add months
                final_data$Month <- c(1:12)
                        
                #Compute average temperature (K) and average SWE (mm) for each month (using all years of data)
                
                        for(i in 1:12){
                                
                                #Avg. Temp ----
                                
                                        #Mean temp (K)
                                        mean_temp_k <- mean(climate_filt$Temperature_K[climate_filt$X_Month == i])
                                        
                                        #Temp SE
                                        temp_se <- std(climate_filt$Temperature_K[climate_filt$X_Month == i])
                                        
                                        #Add to df
                                        final_data[i, "Temperature_K"] <- mean_temp_k
                                        final_data[i, "Temperature_SE"] <- temp_se
                                        
                                #Avg. SWE ----
                                        
                                        #Mean SWE (mm)
                                        mean_swe_mm <- mean(climate_filt$swe_mm[climate_filt$X_Month == i])
                                        
                                        #SWE SE
                                        swe_se <- std(climate_filt$swe_mm[climate_filt$X_Month == i])
                                        
                                        #Add to df
                                        final_data[i, "SWE_mm"] <- mean_swe_mm
                                        final_data[i, "SWE_SE"] <- swe_se
                                
                        }
        

#END COMPUTE AVERAGE T & SWE -----------------------------
        
        
        
#////////////////////////////////////////////



#GENERATE PLOTS -----------------------------
                
        #Set strip text labels
        months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        month_labs <- function(variable,value){
                return(months[value])
        }
        
        #Position dodge
        pd <- position_dodge(0.5)
        
                
        
        #Temperature (K)
        
                #Mean temps across study period (by month)
                
                        #Boxplot
                        ggplot(monthly_data, aes(x = as.factor(Month), y = Temperature_K)) +
                                geom_boxplot() +
                                labs(x = "Month", y = "Mean Temperature (K)") + 
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                
                        #Faceted plot
                        ggplot(monthly_data, aes(x = Year, y = Temperature_K)) +
                                geom_errorbar(aes(ymin = (Temperature_K - Temperature_SE), ymax = (Temperature_K + Temperature_SE)), colour="black", width=1) +
                                geom_point() +
                                geom_line(alpha = 0.8) +
                                facet_wrap(~ Month, labeller = labeller(month_labs)) +
                                labs(x = "Year", y = "Mean Temperature (K)") + 
                                theme_bw() +
                                scale_x_continuous(limits = c(2007, 2020), breaks = c(2010, 2014, 2018)) +
                                theme(
                                        axis.text.x = element_text(size = 8),
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
        
                #Final mean temp values
                ggplot(final_data, aes(x = Month, y = Temperature_K)) +
                        geom_errorbar(aes(ymin = (Temperature_K - Temperature_SE), ymax = (Temperature_K + Temperature_SE)), colour="#666666", width=0.5) +
                        geom_point() +
                        geom_line() +
                        labs(x = "Month", y = "Mean Temperature (K)") + 
                        theme_bw() +
                        scale_x_continuous(limits = c(1, 12), breaks = c(1:12)) +
                        theme(
                                axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                axis.title.y = element_text(size = 12, margin = margin(r=10))
                        )
        
        #SWE (mm)
        
                #Mean SWE across study period (by month)
                
                        #Boxplot
                        ggplot(monthly_data, aes(x = as.factor(Month), y = SWE_mm)) +
                                geom_boxplot() +
                                labs(x = "Month", y = "Mean SWE (mm)") + 
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                        
                        #Faceted plot
                        ggplot(monthly_data, aes(x = Year, y = SWE_mm)) +
                                geom_errorbar(aes(ymin = (SWE_mm - SWE_SE), ymax = (SWE_mm + SWE_SE)), colour="#666666", width=1) +
                                geom_point() +
                                geom_line(alpha = 0.8) +
                                facet_wrap(~ Month, labeller = labeller(month_labs)) +
                                labs(x = "Year", y = "Mean SWE (mm)") + 
                                theme_bw() +
                                scale_x_continuous(limits = c(2007, 2020), breaks = c(2010, 2014, 2018)) +
                                theme(
                                        axis.text.x = element_text(size = 8),
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                        
                #Final mean SWE values
                ggplot(final_data, aes(x = Month, y = SWE_mm)) +
                        geom_errorbar(aes(ymin = (SWE_mm - SWE_SE), ymax = (SWE_mm + SWE_SE)), colour="#666666", width=0.5) +
                        geom_point() +
                        geom_line() +
                        labs(x = "Month", y = "Mean SWE (mm)") + 
                        theme_bw() +
                        scale_x_continuous(limits = c(1, 12), breaks = c(1:12)) +
                        theme(
                                axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                axis.title.y = element_text(size = 12, margin = margin(r=10))
                        )
        
        
        
        
#END GENERATE PLOTS -----------------------------
        
        

#////////////////////////////////////////////



#WRITE FINAL CSV -----------------------------
        
        #Write CSV
        write.csv(final_data, '1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Averages/average_climate_data.csv')

        
#END WRITE FINAL CSV -----------------------------
        