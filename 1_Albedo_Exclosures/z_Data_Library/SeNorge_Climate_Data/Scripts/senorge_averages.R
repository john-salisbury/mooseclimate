#This script computes average temperature and SWE from SeNorge data for all Sustherb sites
#Averages are computed for each month using all years of available data AT EACH SITE

#Thus, this script produces one set of averaged climate values for each site in the study

#The final output is one CSV containing all climate data

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
        
                #48 LocalityNames (but only tree data for 47 sites) - need to filter to 47 used sites
        
        #Load site data for SustHerb sites used in project analysis
        site_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/all_sites_data.csv', header = T)
        

        
#END LOAD DATA -----------------------------------------------------------------------------------------------------
        
        
        
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



#FORMAT/FILTER DATA -----------------------------------------------------------------------------------------------

        #Filter SeNorge data to sites that are used in rest of analysis (as they have full data from Snøan's project)
        #(Note: These are the 94 sites/47 locality names in the 'site_data' CSV)
        
                #Use 'LocalityCode' variable to filter out FIDs that correspond to unused SustHerb sites
                        
                        #Get vector of LocalityCodes for used sites
                        site_data$LocalityCode <- as.factor(site_data$LocalityCode)
                        used_sites <- levels(site_data$LocalityCode)
                        
                        #Filter down sites in 'senorge_locs' df
                        senorge_locs$LocalityCode <- as.factor(senorge_locs$LocalityCode)
                        senorge_locs <- senorge_locs[senorge_locs$LocalityCode %in% used_sites,]
                        
                                #Filters to 47 localities, which matches up with used sites (looks good)
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
                                 'Temperature_K' = numeric(),
                                 'Temperature_SE' = numeric(),
                                 'SWE_mm' = numeric(),
                                 'SWE_SE' = numeric())
                
        #Loop through each LocalityName and calculate average SWE & Temp for all months
        
                #Create vector of 'used' LocalityNames (in correct format)
                used_locs <- levels(as.factor(site_data$LocalityName))
                
                #Loop
                for(i in 1:length(used_locs)){
                        
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
                        br <- as.character(site_data$LocalityCode[site_data$LocalityName == loc & site_data$Treatment == "B"])
                        
                        
                        #Loop through months 1-12 and calculate averages for each LocalityName
                        for(j in 1:12){
                                
                                #Temp df
                                temp <- data.frame("Region" = character(),
                                                   "LocalityName" = character(),
                                                   "Month" = integer(),
                                                   'Temperature_K' = numeric(),
                                                   'Temperature_SE' = numeric(),
                                                   'SWE_mm' = numeric(),
                                                   'SWE_SE' = numeric())
                                
                                temp[nrow(temp)+1,] <- NA
                                
                                #Calculate monthly average (month j) of SWE and Temp for site i
                                
                                #Temperature (K)
                                mean_temp <- mean(climate_filt$Temperature_K[climate_filt$X_Month == j &
                                                                                     climate_filt$LocalityCode == br])
                                
                                #SWE (mm)
                                mean_swe <- mean(climate_filt$swe_mm[climate_filt$X_Month == j &
                                                                             climate_filt$LocalityCode == br])
                                
                                #Calculate SE for monthly averages
                                
                                #Temperature SE
                                temp_se <- std(climate_filt$Temperature_K[climate_filt$X_Month == j &
                                                                                  climate_filt$LocalityCode == br])
                                
                                #SWE SE
                                swe_se <- std(climate_filt$swe_mm[climate_filt$X_Month == j &
                                                                          climate_filt$LocalityCode == br])
                                
                                #Add values to temp df
                                
                                temp$Region <- reg
                                temp$LocalityName <- loc
                                temp$Month <- j
                                temp$Temperature_K <- mean_temp
                                temp$Temperature_SE <- temp_se
                                temp$SWE_mm <- mean_swe
                                temp$SWE_SE <- swe_se
                                
                                
                                #Append temp df to final df
                                final_data <- rbind(final_data, temp)
                                
                                
                        }
                        
                        
                        
                }
                
                beep(8)
                
                
        #WRITE CSV OF SITE-SPECIFIC CLIMATE AVERAGES -----------
        write.csv(final_data, '1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Averages/average_climate_data_by_site.csv')
                
        
        #GENERATE PLOTS (Colored by region) -------------
                
                #Start here if loading data -----------
                final_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Averages/average_climate_data_by_site.csv', header = T)
                
        
                #Set strip text labels
                months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
                month_labs <- function(variable,value){
                        return(months[value])
                }
                
                #Position dodge
                pd <- position_dodge(0.5)
                
                #Palette
                pal <- wes_palette("Darjeeling1")
                
                #TEMPERATURE
                g1 <- ggplot(final_data, aes(x = Month, y = Temperature_K, color = Region, group = LocalityName)) +
                        #geom_ribbon(aes(ymin = (Temperature_K - Temperature_SE), ymax = (Temperature_K + Temperature_SE)), alpha = 0.5, lwd = 0.1) +
                        geom_line() +
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
                
                
                #SWE
                g2 <- ggplot(final_data, aes(x = Month, y = SWE_mm, color = Region, group = LocalityName)) +
                        #geom_ribbon(aes(ymin = (Temperature_K - Temperature_SE), ymax = (Temperature_K + Temperature_SE)), alpha = 0.5, lwd = 0.1) +
                        geom_line() +
                        geom_point(size = 0.5) +
                        labs(x = "Month", y = "SWE (mm)") + 
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
                        gl <- ggplot(final_data, aes(x = Month, y = SWE_mm, color = Region, group = LocalityName)) +
                                #geom_ribbon(aes(ymin = (Temperature_K - Temperature_SE), ymax = (Temperature_K + Temperature_SE)), alpha = 0.5, lwd = 0.1) +
                                geom_line() +
                                geom_point(size = 0.5) +
                                labs(x = "Month", y = "SWE (mm)") + 
                                theme_bw() +
                                scale_x_continuous(limits = c(1, 12), breaks = c(1:12)) +
                                scale_color_manual(values = pal) +
                                scale_fill_manual(values = pal) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10)),
                                        panel.grid.minor = element_blank()
                                ) +
                                guides(fill = F) +
                                guides(color=guide_legend(override.aes=list(fill=pal[1:3])))
                        gl
                        
                        #Extract legend
                        shared_legend <- extract_legend(gl)
                        
                        top_row <- plot_grid(g1, NULL, g2, ncol = 3, rel_widths = c(0.475, 0.05, 0.475))
                        complex_plot <- plot_grid(top_row, shared_legend, ncol = 1, rel_heights = c(0.65, 0.1))
                        complex_plot
        
        
                
#END COMPUTE AVERAGE T & SWE **BY SITE** ----------------------------------------

                
        
        
#////////////////////////////////////////////////////////////////////////////
        
        
        
        
#COMPUTE AVERAGE T & SWE **BY REGION** ---------------------------------------------------
        

        #MONTHLY AVERAGES (FOR EACH REGION) USING ALL YEARS OF DATA  -------
        #Note: this is what will be used in final analysis
        
                #Construct blank final df
                reg_means <- data.frame("Region" = character(),
                                        "Month" = integer(),
                                        'Temperature_K' = numeric(),
                                        'Temperature_SE' = numeric(),
                                        'SWE_mm' = numeric(),
                                        'SWE_SE' = numeric())  
                        
                #Add 12 blank rows for each region (36 total)
                reg_means[nrow(reg_means)+36,] <- NA
                
                #Add months (3 sets, one for each region)
                reg_means$Month <- c(1:12, 1:12, 1:12)
                
                #Add regions
                reg_means$Region <- c(rep("Trøndelag", 12), rep("Hedmark", 12), rep("Telemark", 12))
                
                #Compute average temperature (K) and average SWE (mm) for each month (using all years of data)
                for(i in 1:nrow(reg_means)){
                        
                        #Get month
                        mt <- reg_means[i, "Month"]
                        
                        #Get region
                        reg <- reg_means[i, "Region"]

                        #Avg. Temp ----
                        
                                #Mean temp (K)
                                mean_temp_k <- mean(climate_filt$Temperature_K[climate_filt$X_Month == mt &
                                                                                       climate_filt$Region == reg])
                                
                                #Temp SE
                                temp_se <- std(climate_filt$Temperature_K[climate_filt$X_Month == mt &
                                                                                  climate_filt$Region == reg])
                                
                                #Add to df
                                reg_means[i, "Temperature_K"] <- mean_temp_k
                                reg_means[i, "Temperature_SE"] <- temp_se
                                
                        #Avg. SWE ----
                                
                                #Mean SWE (mm)
                                mean_swe_mm <- mean(climate_filt$swe_mm[climate_filt$X_Month == mt &
                                                                                climate_filt$Region == reg])
                                
                                #SWE SE
                                swe_se <- std(climate_filt$swe_mm[climate_filt$X_Month == mt &
                                                                          climate_filt$Region == reg])
                                
                                #Add to df
                                reg_means[i, "SWE_mm"] <- mean_swe_mm
                                reg_means[i, "SWE_SE"] <- swe_se
                
                }
                
                beep(8)
                
                
        #WRITE CSV OF REGION-SPECIFIC CLIMATE AVERAGES -----------
        write.csv(reg_means, '1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Averages/average_climate_data_by_region.csv')
        
        
                
        #GENERATE PLOTS --------------
                
                #START HERE IF LOADING -----
                reg_means <- read.csv('1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Averages/average_climate_data_by_region.csv', header = T)
                
                #Define discrete color-blind-friendly palette
                pal <- c("#009E73", "#56B4e9", "#e69f00")
                
                #UPDATED LABELS FOR REGIONS -> COUNTIES -----
                reg_means$Counties[reg_means$Region == "Trøndelag"] <- "Trøndelag"
                reg_means$Counties[reg_means$Region == "Hedmark"] <- "Innlandet and Viken"
                reg_means$Counties[reg_means$Region == "Telemark"] <- "Telemark and Vestfold"
                
                
                
                #TEMPERATURE
                g1 <- ggplot(reg_means, aes(x = Month, y = Temperature_K, color = Region, fill = Counties)) +
                        geom_ribbon(aes(ymin = (Temperature_K - Temperature_SE), ymax = (Temperature_K + Temperature_SE)), alpha = 0.5, lwd = 0.1) +
                        geom_line() +
                        geom_point(size = 1) +
                        labs(x = "Month", y = "Temperature (K)") + 
                        theme_bw() +
                        scale_x_continuous(limits = c(1, 12), breaks = c(1:12)) +
                        scale_color_manual(values = c(pal[1], pal[2], pal[3])) +
                        scale_fill_manual(values = c(pal[1], pal[2], pal[3])) +
                        ggtitle("(a)") +
                        theme(
                                legend.position = "none",
                                axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                axis.title.y = element_text(size = 12, margin = margin(r=10)),
                                panel.grid.minor = element_blank(),
                                panel.grid.major.x = element_blank(),
                                plot.title = element_text(face = "bold")
                        ) +
                        guides(fill = F) +
                        guides(color=guide_legend(override.aes=list(fill=pal[1:3])))
                g1
                
                
                #SWE
                g2 <- ggplot(reg_means, aes(x = Month, y = SWE_mm, color = Region, fill = Counties)) +
                        geom_ribbon(aes(ymin = (SWE_mm - SWE_SE), ymax = (SWE_mm + SWE_SE)), alpha = 0.5, lwd = 0.1) +
                        geom_line() +
                        geom_point(size = 1) +
                        labs(x = "Month", y = "SWE (mm)") + 
                        theme_bw() +
                        ggtitle("(b)") +
                        scale_x_continuous(limits = c(1, 12), breaks = c(1:12)) +
                        scale_color_manual(values = c(pal[1], pal[2], pal[3])) +
                        scale_fill_manual(values = c(pal[1], pal[2], pal[3])) +
                        theme(
                                legend.position = "none",
                                axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                axis.title.y = element_text(size = 12, margin = margin(r=10)),
                                panel.grid.minor = element_blank(),
                                panel.grid.major.x = element_blank(),
                                plot.title = element_text(face = "bold")
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
                        gl <- ggplot(reg_means, aes(x = Month, y = SWE_mm, color = Counties)) +
                                geom_ribbon(aes(ymin = (Temperature_K - Temperature_SE), ymax = (Temperature_K + Temperature_SE)), alpha = 0.5, lwd = 0.1) +
                                geom_line() +
                                geom_point(size = 1.1) +
                                labs(x = "Month", y = "SWE (mm)") + 
                                theme_bw() +
                                scale_x_continuous(limits = c(1, 12), breaks = c(1:12)) +
                                scale_color_manual(values = c(pal[1], pal[2], pal[3])) +
                                scale_fill_manual(values = c(pal[1], pal[2], pal[3])) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10)),
                                        panel.grid.minor = element_blank(),
                                        legend.background = element_rect(fill="#fafafa",
                                                                         size=0.1, linetype="solid", 
                                                                         colour ="#666666"),
                                        legend.title = element_blank()
                                ) +
                                guides(color=guide_legend(override.aes=list(fill=pal[1:3]))) +
                                guides(fill=guide_legend(override.aes=list(fill=pal[1:3])))
                        
                        gl
                        
                        #Extract legend
                        shared_legend <- extract_legend(gl)
                        
                        top_row <- plot_grid(g1, NULL, g2, ncol = 3, rel_widths = c(0.475, 0.05, 0.475))
                        complex_plot <- plot_grid(top_row, NULL, shared_legend, ncol = 1, rel_heights = c(0.75, 0.04, 0.1))
                        complex_plot
                        
                        complex_stacked <- plot_grid(g1, NULL, g2, NULL, shared_legend, ncol = 1, rel_heights = c(0.45, 0.025, 0.45, 0.025, 0.05))
                        complex_stacked

#END COMPUTE AVERAGE T & SWE **BY REGION** -----------------------------

                        
                        