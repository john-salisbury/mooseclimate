#This script computes average temperature and SWE from SeNorge data for all Sustherb sites
#Averages are computed for each month using all years of available data in each region
#The final output is one set of monthly climate averages for each region

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
        
        
        #Assign REGION (Trøndelag, Telemark, and Hedmark) to data
        
                #Placeholder column
                climate_filt$Region <- as.character('')
                
                #Loop through observations
                for(i in 1:nrow(climate_filt)){
                        
                        print(i)
                        
                        #Get FID from climate data
                        id <- climate_filt[i, "trondelag_"]
                        
                        #Get region corresponding to FID from locs data
                        reg <- senorge_locs$Region[senorge_locs$FID == id]
                        
                        #Add to main df
                        climate_filt[i, "Region"] <- reg
                }
                
                beep(8)
        
        

#END FORMAT/FILTER DATA -----------------------------
        
        
        
#////////////////////////////////////////////



#COMPUTE AVERAGE T & SWE -----------------------------
        
        #Define function to calculate SE
        std <- function(x) sd(x)/sqrt(length(x))

        #MONTHLY AVERAGES FOR EACH YEAR IN EACH REGION -------
        #Note: these are generated solely for plotting purposes
        
                #Create placeholder df
                monthly_data <- data.frame("Region" = character(),
                                           "Month" = integer(),
                                           "Year" = integer(),
                                           'Temperature_K' = numeric(),
                                           'Temperature_SE' = numeric(),
                                           'SWE_mm' = numeric(),
                                           'SWE_SE' = numeric())
        
                #Loop through each region
                regions <- c("Trøndelag", "Hedmark", "Telemark")
                
                for(b in 1:length(regions)){
                        
                        #Loop through years
                        for(i in min_year:max_year){
                                
                                #Loop through each month
                                for(j in 1:12){
                                        
                                        #Temp df
                                        temp <- data.frame("Region" = character(),
                                                           "Month" = integer(),
                                                           "Year" = integer(),
                                                           'Temperature_K' = numeric(),
                                                           'Temperature_SE' = numeric(),
                                                           'SWE_mm' = numeric(),
                                                           'SWE_SE' = numeric())
                                        
                                        temp[nrow(temp)+1,] <- NA
                                        
                                        #Add region
                                        reg <- regions[b]
                                        temp$Region <- reg
                                        
                                        #Add month
                                        temp$Month <- j
                                        
                                        #Add year
                                        temp$Year <- i
                                        
                                        #Avg. Temp (K) in Region B ----
                                        
                                                #Calculate mean
                                                monthly_mean_temp <- mean(climate_filt$Temperature_K[climate_filt$X_Year == i &
                                                                                                             climate_filt$X_Month == j &
                                                                                                             climate_filt$Region == reg])
                                        
                                                #Calculate SE for temp
                                                monthly_temp_se <- std(climate_filt$Temperature_K[climate_filt$X_Year == i &
                                                                                                          climate_filt$X_Month == j &
                                                                                                          climate_filt$Region == reg])
                                        
                                                #Add to df
                                                temp$Temperature_K <- monthly_mean_temp
                                                temp$Temperature_SE <- monthly_temp_se
                                        
                                        #Avg. SWE (mm) in Region B ----
                                                
                                                #Calculate mean
                                                monthly_mean_swe <- mean(climate_filt$swe_mm[climate_filt$X_Year == i &
                                                                                                     climate_filt$X_Month == j &
                                                                                                     climate_filt$Region == reg])
                                                
                                                #Calculate SE for SWE
                                                monthly_swe_se <- std(climate_filt$swe_mm[climate_filt$X_Year == i &
                                                                                                  climate_filt$X_Month == j &
                                                                                                  climate_filt$Region == reg])
                                                
                                                #Add to df
                                                temp$SWE_mm <- monthly_mean_swe
                                                temp$SWE_SE <- monthly_swe_se
                                                
                                        #Append temp row to main df
                                        monthly_data <- rbind(monthly_data, temp)
                                }
                        }
                        
                }
        
                beep(8)
        
        
        #MONTHLY AVERAGES (FOR EACH REGION) USING ALL YEARS OF DATA  -------
        #Note: this is what will be used in final analysis
        
                #Create placeholder df
                final_data <- data.frame("Region" = character(),
                                         "Month" = integer(),
                                         'Temperature_K' = numeric(),
                                         'Temperature_SE' = numeric(),
                                         'SWE_mm' = numeric(),
                                         'SWE_SE' = numeric())
                        
                #Add 12 blank rows for each region (36 total)
                final_data[nrow(final_data)+36,] <- NA
                
                #Add months (3 sets, one for each region)
                final_data$Month <- c(1:12, 1:12, 1:12)
                
                #Add regions
                final_data$Region <- c(rep("Trøndelag", 12), rep("Hedmark", 12), rep("Telemark", 12))
                
                #Compute average temperature (K) and average SWE (mm) for each month (using all years of data)
                for(i in 1:nrow(final_data)){
                        
                        #Get month
                        mt <- final_data[i, "Month"]
                        
                        #Get region
                        reg <- final_data[i, "Region"]

                        #Avg. Temp ----
                        
                                #Mean temp (K)
                                mean_temp_k <- mean(climate_filt$Temperature_K[climate_filt$X_Month == mt &
                                                                                       climate_filt$Region == reg])
                                
                                #Temp SE
                                temp_se <- std(climate_filt$Temperature_K[climate_filt$X_Month == mt &
                                                                                  climate_filt$Region == reg])
                                
                                #Add to df
                                final_data[i, "Temperature_K"] <- mean_temp_k
                                final_data[i, "Temperature_SE"] <- temp_se
                                
                        #Avg. SWE ----
                                
                                #Mean SWE (mm)
                                mean_swe_mm <- mean(climate_filt$swe_mm[climate_filt$X_Month == mt &
                                                                                climate_filt$Region == reg])
                                
                                #SWE SE
                                swe_se <- std(climate_filt$swe_mm[climate_filt$X_Month == mt &
                                                                          climate_filt$Region == reg])
                                
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
        
        #Palette
        pal <- wes_palette("Darjeeling1")
        
        #Temperature (K)
        
                #Final mean temp values
                g1 <- ggplot(final_data, aes(x = Month, y = Temperature_K, color = Region, fill = Region)) +
                        geom_ribbon(aes(ymin = (Temperature_K - Temperature_SE), ymax = (Temperature_K + Temperature_SE)), alpha = 0.5, lwd = 0.1) +
                        geom_point(size = 0.5) +
                        labs(x = "Month", y = "Temperature (K)") + 
                        theme_bw() +
                        scale_x_continuous(limits = c(1, 12), breaks = c(1:12)) +
                        scale_color_manual(values = pal) +
                        scale_fill_manual(values = pal) +
                        theme(
                                legend.position = "none",
                                axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                axis.title.y = element_text(size = 12, margin = margin(r=10)),
                                panel.grid.minor = element_blank()
                        ) +
                        guides(fill = F) +
                        guides(color=guide_legend(override.aes=list(fill=pal[1:3])))
                g1
        
        #SWE (mm)
        
                #Final mean SWE values
                g2 <- ggplot(final_data, aes(x = Month, y = SWE_mm, color = Region)) +
                        geom_ribbon(aes(ymin = (SWE_mm - SWE_SE), ymax = (SWE_mm + SWE_SE), color = Region, fill = Region), alpha = 0.5, lwd = 0.1) +
                        geom_point(size = 0.5) +
                        labs(x = "Month", y = "Mean SWE (mm)") + 
                        theme_bw() +
                        scale_x_continuous(limits = c(1, 12), breaks = c(1:12)) +
                        scale_color_manual(values = pal) +
                        scale_fill_manual(values = pal) +
                        theme(
                                legend.position = "none",
                                axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                axis.title.y = element_text(size = 12, margin = margin(r=10)),
                                panel.grid.minor = element_blank()
                                
                        ) +
                        guides(fill = F) +
                        guides(color=guide_legend(override.aes=list(fill=pal[1:3])))
                g2
                
                
        #COMBINED PLOT (Temp. and SWE)
                
                #Legend function
                extract_legend <- function(my_ggp) {
                        step1 <- ggplot_gtable(ggplot_build(my_ggp))
                        step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
                        step3 <- step1$grobs[[step2]]
                        return(step3)
                }
                
                #Sample plot for universal legend
                g1_l <- ggplot(final_data, aes(x = Month, y = Temperature_K, color = Region, fill = Region)) +
                                geom_ribbon(aes(ymin = (Temperature_K - Temperature_SE), ymax = (Temperature_K + Temperature_SE)), alpha = 0.5, lwd = 0.1) +
                                geom_point(size = 0.5) +
                                labs(x = "Month", y = "Mean Temperature (K)") + 
                                theme_bw() +
                                scale_x_continuous(limits = c(1, 12), breaks = c(1:12)) +
                                scale_color_manual(values = pal) +
                                scale_fill_manual(values = pal) +
                                theme(
                                        legend.position = "bottom",
                                        legend.background = element_rect(fill = "#fafafa", color = "#e6e6e6")
                                ) +
                                guides(fill = F) +
                                guides(color=guide_legend(override.aes=list(fill=pal[1:3])))
                
                #Extract legend
                shared_legend <- extract_legend(g1_l)
                
                top_row <- plot_grid(g1, NULL, g2, ncol = 3, rel_widths = c(0.475, 0.05, 0.475))
                complex_plot <- plot_grid(top_row, shared_legend, ncol = 1, rel_heights = c(0.65, 0.1))
                complex_plot
                

                
                
        
#END GENERATE PLOTS -----------------------------
        
        

#////////////////////////////////////////////



#WRITE FINAL CSV -----------------------------
        
        #Write CSV
        write.csv(final_data, '1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Averages/average_climate_data.csv')

        
#END WRITE FINAL CSV -----------------------------
        