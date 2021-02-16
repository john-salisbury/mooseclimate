## Script to calculate albedo estimates for all 37 'used sites' (Trøndelag, Hedmark, Telemark)
## across all years of available volume data.

## Note: volume (m3/ha) and albedo estimates are on the subplot level
## Albedo estimates are calculated for each 'group' within each subplot, and then a weighted average 
## (based on tree species proportions within each subplot) is used to calculate a 'composite'
## subplot albedo value

## Note: albedo estimates are produced using average T and SWE data for each of the study sites
## (produced by taking mean of climate data across all years of available SustHerb tree data)

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
        library(lme4)
        library(lmerTest)
        library(beepr)
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        ## SITE DATA
        
                #Get 'cleaned' site data from adjacent 'Sites' folder
                site_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/cleaned_data.csv', header = TRUE)


        #SUBPLOT STAND VOLUMES (m3/ha) (Trondelag/Hedmark/Telemark: 2009-2019) - circular subplots within each plot/LocalityCode

                #Load subplot volumes
                plot_volumes <- read.csv('1_Albedo_Exclosures/1_Data_Processing/3_Volume_Estimates/Output/subplot_tree_volumes.csv', header = TRUE)
        
        
        #AVG. SENORGE SWE (mm) and TEMP (K) DATA (one set of values, averaged from data across 2009-2019) 
                
                clim <- read.csv('1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Averages/average_climate_data.csv', header = TRUE)
                
                
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
                        
                        
        ## SUSTHERB TREE SPECIES PROPORTIONS (SUBPLOT LEVEL - 2009-2019)
                        
                #Tree density data for Trøndelag, Telemark, and Hedmark (2009-2019)
                tree_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/Tree_Data/Usable_Data/tree_species_proportions_subplot_level.csv', header = TRUE)
                tree_data <- tree_data[tree_data$Year <= 2019, 2:8]
                
                        #Filter out unused SustHerb sites
                        
                        #Convert site codes to factors
                        tree_data$LocalityCode <- as.factor(tree_data$LocalityCode)
                        site_data$LocalityCode <- as.factor(site_data$LocalityCode)
                        
                        #Get vector of levels for sites that will be used (n = 74)
                        used_sites <- levels(site_data$LocalityCode)
                        
                        #Filter tree data to used sites
                        tree_data <- tree_data[tree_data$LocalityCode %in% used_sites,]
                        tree_data$LocalityCode <- as.factor(as.character(tree_data$LocalityCode))

#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#CALCULATE ALBEDO --------------------------------------------------------------------------------------

   
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

                        
                #For each row, add climate data (SWE/Temp)
                        
                        for(i in 1:nrow(albedo)){
                                
                                print(i)
                                
                                #Site info
                                
                                        #LocalityCode
                                        a <- albedo[i, "LocalityCode"]
                                        
                                        #LocalityName
                                        b <- albedo[i, "LocalityName"]
                                
                                        #Treatment
                                        tr <- albedo[i, "Treatment"]
                                        
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
                                
                                #Get climate data for corresponding month ------
                                        
                                        #Use average values from across study period (2009-2019) for all years
                                        
                                                #Temperature (K)
                                                albedo[i, "Temp_K"] <- clim$Temperature_K[clim$Month == mt]
                                        
                                                #SWE (mm)
                                                albedo[i, "SWE_mm"] <- clim$SWE_mm[clim$Month == mt]
                                
                                    
                                #Calculate albedo (with species-specific equation)
                                        
                                        if(spec == "Birch") {
                                                
                                                #Birch-specific equation
                                                albedo[i, "Albedo"] <- 0.085+0.089*(1-1/(1+exp(-2.414*(albedo[i, "Temp_K"] - 273.393))))+0.169*(1/(1+exp(-0.107*(albedo[i, "SWE_mm"] - 37.672))))+0.245*exp(-0.023 * albedo[i, "Volume_m3ha"])*(1-0.7*exp(-0.004 * albedo[i, "SWE_mm"]))

                                        } else if (spec == "Pine") {
                                                
                                                #Pine-specific equation
                                                albedo[i, "Albedo"] <- 0.069+0.084*(1-1/(1+exp(-1.965*(albedo[i, "Temp_K"] - 273.519))))+0.106*(1/(1+exp(-0.134*(albedo[i, "SWE_mm"] - 30.125))))+0.251*exp(-0.016 * albedo[i, "Volume_m3ha"])*(1-0.7*exp(-0.008 * albedo[i, "SWE_mm"]))
                                                
                                        } else if (spec == "Spruce"){
                                                
                                                #Spruce-specific equation
                                                albedo[i, "Albedo"] <- 0.077+0.072*(1-1/(1+exp(-2.354*(albedo[i, "Temp_K"] - 273.433))))+0.074*(1/(1+exp(-0.191*(albedo[i, "SWE_mm"] - 33.093))))+0.252*exp(-0.023 * albedo[i, "Volume_m3ha"])*(1-0.7*exp(-0.011 * albedo[i, "SWE_mm"]))

                                        }
                                        
                        }
                        
                #Remove arbitrary index column
                albedo <- albedo[,c(2:15)]
                
                #Remove "Volume_m3" column
                albedo <- albedo[,c(1:7,9:14)]
                        
                        
        #CALCULATE SINGLE COMPOSITE ALBEDO VALUE IN SEPARATE DATAFRAME
                        
                #Construct placeholder df w/ same structure as albedo df
                comp <- albedo[0,]
        
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
                        
                                #Treatment
                                if(site_data$Treatment[site_data$LocalityCode == used_sites[i]] == "open"){
                                        tr <- "B"
                                } else if (site_data$Treatment[site_data$LocalityCode == used_sites[i]] == "exclosure"){
                                        tr <- "UB"
                                }
                                
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
                                                
                                                #SWE
                                                swe <- albedo$SWE_mm[albedo$LocalityCode == used_sites[i] & albedo$Year == j & albedo$Month == l][1]
                                                
                                                #Temp
                                                temps <- albedo$Temp_K[albedo$LocalityCode == used_sites[i] & albedo$Year == j & albedo$Month == l][1]
                                                
                                                #CALCULATE COMPOSITE ALBEDO (WEIGHTED AVG BASED ON SPECIES PROPORTIONS)
                                                
                                                        #Birch----
                                                
                                                                #Birch prop
                                                                if(length(tree_data$Prop_birch[tree_data$LocalityCode == used_sites[i] & tree_data$Year == j & tree_data$Plot == subplots[k]]) == 0){
                                                                        b_prop <- 0
                                                                } else {
                                                                        b_prop <- tree_data$Prop_birch[tree_data$LocalityCode == used_sites[i] & tree_data$Year == j & tree_data$Plot == subplots[k]]
                                                                }
                                                                
                                                                #Birch albedo
                                                                if(length(albedo$Albedo[albedo$LocalityCode == used_sites[i] & albedo$Subplot == subplots[k] & albedo$Year == j & albedo$Month == l & albedo$Group == "Birch"]) == 0){
                                                                        b_alb <- 0
                                                                } else {
                                                                        b_alb <- albedo$Albedo[albedo$LocalityCode == used_sites[i] & albedo$Subplot == subplots[k] & albedo$Year == j & albedo$Month == l & albedo$Group == "Birch"]
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
        write.csv(final_albedo, "1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/subplot_albedo_estimates.csv")

        
#END CALCULATE ALBEDO ---------------------------------------------------------------------------------- 
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



                
#CALCULATE MEAN ALBEDO AT SUBPLOT LEVEL -------------------------------------------------------------------------------------
        
        #START HERE IF ALBEDO ALREADY SAVED TO CSV ------------------
        final_albedo <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/subplot_albedo_estimates.csv", header = T)
                
        
        #Aggregate means for each group, month, and year since exclosure
        #Note: not doing anything with 'year' variable, since we're using a single set of
        #average climate values
        
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
                
        
        
                
                
#END CALCULATE MEAN ALBEDO AT SUBPLOT LEVEL ---------------------------------------------------------------------------------- 
                



#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#PLOT ALBEDO AT SUBPLOT LEVEL ---------------------------------------------------------------------------------
        
                
        #COMPOSITE ALBEDO -----------
                
                #Set strip text labels
                months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
                month_labs <- function(variable,value){
                        return(months[value])
                }
                
                #All Sites
                pd <- position_dodge(0.1)
                
                
                #COMPLEX PLOT  -------------
                
                        #Winter
                        g1 <- ggplot(subset(albedo_means, Group == "Composite" & Season == "Winter"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.325, 0.525)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Winter") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g1
                                
                        #Spring
                        g2 <- ggplot(subset(albedo_means, Group == "Composite" & Season == "Spring"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.1425, 0.35)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Spring") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g2
                        
                        #Summer
                        g3 <- ggplot(subset(albedo_means, Group == "Composite" & Season == "Summer"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.05, 0.25)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Summer") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g3
                        
                        #Autumn
                        g4 <- ggplot(subset(albedo_means, Group == "Composite" & Season == "Autumn"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.1425, 0.35)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Autumn") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g4
                                
                                
                        #Common legend
                        
                                #Legend function
                                extract_legend <- function(my_ggp) {
                                        step1 <- ggplot_gtable(ggplot_build(my_ggp))
                                        step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
                                        step3 <- step1$grobs[[step2]]
                                        return(step3)
                                }
                        
                                g1_l <- ggplot(subset(albedo_means, Group == "Composite" & Season == "Winter"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                        geom_errorbar(position = pd, aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE)), colour="#666666", width=0.7) +
                                        geom_line(position = pd, aes(linetype = Treatment), lwd = 0.5) +
                                        geom_point(position = pd, size = 1.4) +
                                        theme_bw() +
                                        facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                        labs(x = "Years Since Exclosure", y = "Mean Albedo") +
                                        scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                        scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                        ggtitle("Winter") +
                                        theme(
                                                legend.position = "top",
                                                legend.background = element_rect(fill = "#fafafa", color = "#e6e6e6")
                                        ) +
                                        guides(shape = F) +
                                        guides(linetype = F)

                                #Extract legend
                                shared_legend <- extract_legend(g1_l)
                                
                        #Build complex plot
                        middle_rows <- plot_grid(g1, NULL, g2, NULL, NULL, NULL, g3, NULL, g4, ncol = 3, nrow = 3, rel_widths = c(0.475,0.05,0.475), rel_heights = c(0.475,0.05, 0.475))
                        final_plot <- plot_grid(shared_legend, middle_rows, ncol = 1, rel_heights = c(0.1,0.9) )
                        
                        #Save as SVG    
                        ggsave('albedo_comp_subplots_complex.svg',
                               final_plot,
                               "svg",
                               '1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots/Subplot_Resolution/Composite',
                               scale = 1.25)
                        
        
                
                #"STANDARD" FACETED PLOTS
                                
                        #ERROR BARS FOR SE
                        ggplot(subset(albedo_means, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment)) +
                                geom_errorbar(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE)), colour="#666666", width=0.55, position = pd) +
                                geom_line(position = pd, aes(color = Treatment), lwd = 0.5) +
                                geom_point(position = pd, aes(shape = Treatment, color = Treatment), size = 1) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                scale_y_continuous(limits = c(0.14, 0.457), breaks = c(0.15,0.2,0.25,0.3,0.35,0.4,0.45)) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        
                                #Export manually
                        
                        #SHADING FOR SE
                        ggplot(subset(albedo_means, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.3, lwd = 0) +
                                geom_point(position = pd, aes(shape = Treatment, color = Treatment), size = 0.6) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                scale_y_continuous(limits = c(0.14, 0.457), breaks = c(0.15,0.2,0.25,0.3,0.35,0.4,0.45)) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                ) +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(fill = F)
                        
                        
                        
                        
                        
                        
                        
        #DECIDUOUS SUBPLOT ALBEDO --------------------------------
                        
                        
                #COMPLEX PLOT  -------------
                        
                        #Winter
                        g1 <- ggplot(subset(albedo_means, Group == "Birch" & Season == "Winter"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Deciduous)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.325, 0.525)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Winter") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g1
                        
                        #Spring
                        g2 <- ggplot(subset(albedo_means, Group == "Birch" & Season == "Spring"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Deciduous)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.1425, 0.35)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Spring") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g2
                        
                        #Summer
                        g3 <- ggplot(subset(albedo_means, Group == "Birch" & Season == "Summer"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Deciduous)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.05, 0.25)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Summer") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g3
                        
                        #Autumn
                        g4 <- ggplot(subset(albedo_means, Group == "Birch" & Season == "Autumn"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Deciduous)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.1425, 0.35)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Autumn") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g4
                        
                        
                        #Common legend
                        
                        #Legend function
                        extract_legend <- function(my_ggp) {
                                step1 <- ggplot_gtable(ggplot_build(my_ggp))
                                step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
                                step3 <- step1$grobs[[step2]]
                                return(step3)
                        }
                        
                        g1_l <- ggplot(subset(albedo_means, Group == "Birch" & Season == "Winter"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_errorbar(position = pd, aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE)), colour="#666666", width=0.7) +
                                geom_line(position = pd, aes(linetype = Treatment), lwd = 0.5) +
                                geom_point(position = pd, size = 1.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Mean Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Winter") +
                                theme(
                                        legend.position = "top",
                                        legend.background = element_rect(fill = "#fafafa", color = "#e6e6e6")
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        
                        #Extract legend
                        shared_legend <- extract_legend(g1_l)
                        
                        #Build complex plot
                        middle_rows <- plot_grid(g1, NULL, g2, NULL, NULL, NULL, g3, NULL, g4, ncol = 3, nrow = 3, rel_widths = c(0.475,0.05,0.475), rel_heights = c(0.475,0.05, 0.475))
                        final_plot <- plot_grid(shared_legend, middle_rows, ncol = 1, rel_heights = c(0.1,0.9) )
                        
                        #Save as SVG    
                        ggsave('albedo_decid_subplots_complex.svg',
                               final_plot,
                               "svg",
                               '1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots/Subplot_Resolution/Deciduous',
                               scale = 1.25)
                        
                        
                        
                #"STANDARD" FACETED PLOTS
                        
                        #ERROR BARS FOR SE
                        ggplot(subset(albedo_means, Group == "Birch"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment)) +
                                geom_errorbar(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE)), colour="#666666", width=0.55, position = pd) +
                                geom_line(position = pd, aes(color = Treatment), lwd = 0.5) +
                                geom_point(position = pd, aes(shape = Treatment, color = Treatment), size = 1) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Deciduous)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                scale_y_continuous(limits = c(0.14, 0.48), breaks = c(0.15,0.2,0.25,0.3,0.35,0.4,0.45, 0.50)) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        
                                #Export manually
                        
                        #SHADING FOR SE
                        ggplot(subset(albedo_means, Group == "Birch"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.3, lwd = 0) +
                                geom_point(position = pd, aes(shape = Treatment, color = Treatment), size = 0.6) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Deciduous)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                scale_y_continuous(limits = c(0.14, 0.48), breaks = c(0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.50)) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                ) +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(fill = F)
                        
                        

                        
                        
                        
                        
                        
                        

        #PINE SUBPLOT ALBEDO --------------
                        
                        
                #COMPLEX PLOT  -------------
                        
                        #Winter
                        g1 <- ggplot(subset(albedo_means, Group == "Pine" & Season == "Winter"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Pine)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.325, 0.525)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Winter") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g1
                        
                        #Spring
                        g2 <- ggplot(subset(albedo_means, Group == "Pine" & Season == "Spring"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Pine)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.13, 0.35)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Spring") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g2
                        
                        #Summer
                        g3 <- ggplot(subset(albedo_means, Group == "Pine" & Season == "Summer"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Pine)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.05, 0.25)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Summer") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g3
                        
                        #Autumn
                        g4 <- ggplot(subset(albedo_means, Group == "Pine" & Season == "Autumn"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Pine)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.13, 0.35)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Autumn") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g4
                        
                        
                        #Common legend
                        
                        #Legend function
                        extract_legend <- function(my_ggp) {
                                step1 <- ggplot_gtable(ggplot_build(my_ggp))
                                step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
                                step3 <- step1$grobs[[step2]]
                                return(step3)
                        }
                        
                        g1_l <- ggplot(subset(albedo_means, Group == "Pine" & Season == "Winter"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_errorbar(position = pd, aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE)), colour="#666666", width=0.7) +
                                geom_line(position = pd, aes(linetype = Treatment), lwd = 0.5) +
                                geom_point(position = pd, size = 1.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Mean Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Winter") +
                                theme(
                                        legend.position = "top",
                                        legend.background = element_rect(fill = "#fafafa", color = "#e6e6e6")
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        
                        #Extract legend
                        shared_legend <- extract_legend(g1_l)
                        
                        #Build complex plot
                        middle_rows <- plot_grid(g1, NULL, g2, NULL, NULL, NULL, g3, NULL, g4, ncol = 3, nrow = 3, rel_widths = c(0.475,0.05,0.475), rel_heights = c(0.475,0.05, 0.475))
                        final_plot <- plot_grid(shared_legend, middle_rows, ncol = 1, rel_heights = c(0.1,0.9) )
                        
                        #Save as SVG    
                        ggsave('albedo_pine_subplots_complex.svg',
                               final_plot,
                               "svg",
                               '1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots/Subplot_Resolution/Pine',
                               scale = 1.25)
                        
                        
                        
                #"STANDARD" FACETED PLOTS
                        
                        #ERROR BARS FOR SE
                        ggplot(subset(albedo_means, Group == "Pine"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment)) +
                                geom_errorbar(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE)), colour="#666666", width=0.55, position = pd) +
                                geom_line(position = pd, aes(color = Treatment), lwd = 0.5) +
                                geom_point(position = pd, aes(shape = Treatment, color = Treatment), size = 1) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Pine)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                scale_y_continuous(limits = c(0.14, 0.45), breaks = c(0.15,0.2,0.25,0.3,0.35,0.4,0.45)) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        
                                #Export manually
                        
                        #SHADING FOR SE
                        ggplot(subset(albedo_means, Group == "Pine"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.3, lwd = 0) +
                                geom_point(position = pd, aes(shape = Treatment, color = Treatment), size = 0.6) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Pine)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                scale_y_continuous(limits = c(0.14, 0.45), breaks = c(0.15,0.2,0.25,0.3,0.35,0.4,0.45)) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                ) +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(fill = F)

                        
                        
                        
                        
                        
                        
                        
        #SPRUCE SUBPLOT ALBEDO --------------
                        
                        
                #COMPLEX PLOT  -------------
                        
                        #Winter
                        g1 <- ggplot(subset(albedo_means, Group == "Spruce" & Season == "Winter"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Spruce)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.325, 0.525)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Winter") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g1
                        
                        #Spring
                        g2 <- ggplot(subset(albedo_means, Group == "Spruce" & Season == "Spring"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Spruce)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.13, 0.35)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Spring") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g2
                        
                        #Summer
                        g3 <- ggplot(subset(albedo_means, Group == "Spruce" & Season == "Summer"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Spruce)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.05, 0.25)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Summer") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g3
                        
                        #Autumn
                        g4 <- ggplot(subset(albedo_means, Group == "Spruce" & Season == "Autumn"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.35, lwd = 0) +
                                geom_point(size = 0.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Spruce)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_y_continuous(limits = c(0.13, 0.35)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Autumn") +
                                theme(
                                        legend.position = "none",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(hjust = 0.5, size = 14)
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        g4
                        
                        
                        #Common legend
                        
                        #Legend function
                        extract_legend <- function(my_ggp) {
                                step1 <- ggplot_gtable(ggplot_build(my_ggp))
                                step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
                                step3 <- step1$grobs[[step2]]
                                return(step3)
                        }
                        
                        g1_l <- ggplot(subset(albedo_means, Group == "Spruce" & Season == "Winter"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_errorbar(position = pd, aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE)), colour="#666666", width=0.7) +
                                geom_line(position = pd, aes(linetype = Treatment), lwd = 0.5) +
                                geom_point(position = pd, size = 1.4) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 12, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Mean Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                ggtitle("Winter") +
                                theme(
                                        legend.position = "top",
                                        legend.background = element_rect(fill = "#fafafa", color = "#e6e6e6")
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        
                        #Extract legend
                        shared_legend <- extract_legend(g1_l)
                        
                        #Build complex plot
                        middle_rows <- plot_grid(g1, NULL, g2, NULL, NULL, NULL, g3, NULL, g4, ncol = 3, nrow = 3, rel_widths = c(0.475,0.05,0.475), rel_heights = c(0.475,0.05, 0.475))
                        final_plot <- plot_grid(shared_legend, middle_rows, ncol = 1, rel_heights = c(0.1,0.9) )
                        
                        #Save as SVG    
                        ggsave('albedo_spruce_subplots_complex.svg',
                               final_plot,
                               "svg",
                               '1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plots/Subplot_Resolution/Spruce',
                               scale = 1.25)
                        
                        
                        
                #"STANDARD" FACETED PLOTS
                        
                        #ERROR BARS FOR SE
                        ggplot(subset(albedo_means, Group == "Spruce"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_errorbar(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE)), colour="#666666", width=0.55, position = pd) +
                                geom_line(position = pd, aes(color = Treatment), lwd = 0.5) +
                                geom_point(position = pd, aes(shape = Treatment, color = Treatment), size = 1) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Spruce)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                scale_y_continuous(limits = c(0.13, 0.425), breaks = c(0.15,0.2,0.25,0.3,0.35,0.4)) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        
                                #Export manually
                        
                        #SHADING FOR SE
                        ggplot(subset(albedo_means, Group == "Spruce"), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, color = Treatment, group = Treatment)) +
                                geom_ribbon(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE), fill = Treatment), alpha = 0.3, lwd = 0) +
                                geom_point(position = pd, aes(shape = Treatment, color = Treatment), size = 0.6) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Albedo (Spruce)") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                scale_y_continuous(limits = c(0.13, 0.425), breaks = c(0.15,0.2,0.25,0.3,0.35,0.4)) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                ) +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(fill = F)
                        
                        
                        
                        
                        
                        
                        
        #EXPLORE/FACET BY REGION (AS A PROXY FOR MOOSE DENSITY) --------------  
                        
                #Calculate means within regions
                        
                        #Aggregate means for each region, group, month, and year since exclosure

                        albedo_reg <- aggregate(final_albedo$Albedo, by = list("Treatment" = final_albedo$Treatment,
                                                                               "Region" = final_albedo$Region,
                                                                                 "Years_Since_Exclosure" = final_albedo$Years_Since_Exclosure,
                                                                                 "Group" = final_albedo$Group,
                                                                                 "Month" = final_albedo$Month), FUN = mean)
                        
                        colnames(albedo_reg)[6] <- "Mean_Subplot_Albedo"
                        
                        #Calculate standard error for each mean
                        
                                #Add placeholder columns
                                albedo_reg$SE <- as.numeric('')
                                
                                #Calculate SEs for each species/group, month, and year
                                for(i in 1:nrow(albedo_reg)){
                                        
                                        #Get variables
                                        tr <- albedo_reg[i, "Treatment"]
                                        reg <- albedo_reg[i, "Region"]
                                        yse <- albedo_reg[i, "Years_Since_Exclosure"]
                                        mt <- albedo_reg[i, "Month"]
                                        gr <- albedo_reg[i, "Group"]
                                        
                                        #Calculate SE for albedo
                                        se <- std(final_albedo$Albedo[final_albedo$Treatment == tr &
                                                                              final_albedo$Region == reg &
                                                                              final_albedo$Group == gr &
                                                                              final_albedo$Years_Since_Exclosure == yse &
                                                                              final_albedo$Month == mt])
                                        
                                        #Add to df
                                        albedo_reg[i, "SE"] <- se
                                        
                                }
                                
                        #Add a 'season' variable for further grouping
                        
                        #Placeholder column
                        albedo_reg$Season <- as.character('')
                        
                        #Conditions
                        albedo_reg$Season[albedo_reg$Month %in% c(1:3)] <- "Winter"
                        albedo_reg$Season[albedo_reg$Month %in% c(4:6)] <- "Spring"
                        albedo_reg$Season[albedo_reg$Month %in% c(7:9)] <- "Summer"
                        albedo_reg$Season[albedo_reg$Month %in% c(10:12)] <- "Autumn"
                        
                        #Set as factor
                        albedo_reg$Region <- as.factor(albedo_reg$Region)
                        albedo_reg$Season <- as.factor(albedo_reg$Season)
                        
                        
        
                #Winter months, faceted by region ------------
                        
                        ggplot(data = subset(albedo_reg, Group == "Composite" & Month %in% c(1:3)), aes(x = Years_Since_Exclosure, y = Mean_Subplot_Albedo, group = Treatment)) +
                                geom_errorbar(aes(ymin = (Mean_Subplot_Albedo - SE), ymax = (Mean_Subplot_Albedo + SE)), colour="#666666", width=0.55, position = pd) +
                                geom_line(position = pd, aes(color = Treatment), lwd = 0.5) +
                                geom_point(position = pd, aes(shape = Treatment, color = Treatment), size = 1) +
                                theme_bw() +
                                facet_grid(Region~Month, labeller = labeller(month_labs)) +
                                labs(x = "Years Since Exclosure", y = "Albedo") +
                                scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                                #scale_y_continuous(limits = c(0.13, 0.425), breaks = c(0.15,0.2,0.25,0.3,0.35,0.4)) +
                                theme(
                                        legend.position = "bottom",
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                ) +
                                guides(shape = F) +
                                guides(linetype = F)
                        
                        
                                        #WHY DOES HEDMARK BROWSED HAVE ALBEDO THAT IS MUCH LOWER??
                                
                        
                        
                        
                #All months -------------
                        
                        
                        
                        
                        
                        
                        
                        
                                
#END PLOT ALBEDO AT SUBPLOT LEVEL ---------------------------------------------------------------------------------

                        
                        
                        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                        
                        
                        
                        
#CALCULATE MEAN ALBEDO + SE AT PLOT LEVEL ---------------------------------------------------------------------------------
                        
        # NOTE: This section has five steps:
                        
                #(1) Calculate a 'mean albedo' value for each plot (i.e. LocalityCode) - for all groups (composite, pine
                # deciduous, spruce) across all months and years of data.
                        
                #(2) At each study location (i.e. LocalityName), calculate DELTA ALBEDO between exclosure and open plots. 
                        
                #(3) Calculate mean values across all "years since exclosure" and months for DELTA ALBEDO (between excl./open plots)
                        
                #(4) Calculate standard error for each of these mean values
                        
                #(5) Plot mean values (also include plots of delta albedo vs herbivore densities)
                        
        # The figure resulting from this data should look similar to Figure 6 in Cherubini et al. (2017) - delta albedo on Y-axis,
        # and years since exclosure on X-axis (faceted by month).
                        
        # The goal here is to produce a figure that allows us to nicely visualize the treatment effect of exclosure on albedo (since
        # the other albedo plots don't show the treatment effect well - small difference between treatments but range of values makes
        # plotting difficult). This will also allow us to visualize the individual species-specific albedos together with the composite albedos.
                        
        # Similar to the earlier steps, climate data is ONE SET OF VALUES (averages from climate data at all
        # sites across the entire study period - 2009-2019)
        
                        
        #STEP 1 -------------
        
                #CALCULATE AVERAGE ALBEDO PER SPECIES PER PLOT
                plot_means <- aggregate(final_albedo$Albedo, by = list(final_albedo$LocalityName, final_albedo$LocalityCode, final_albedo$Treatment, final_albedo$Group, final_albedo$Years_Since_Exclosure, final_albedo$Month), FUN = mean)
                colnames(plot_means) <- c("LocalityName", "LocalityCode", "Treatment", "Group", "Years_Since_Exclosure", "Month", "Avg_Plot_Albedo")               
                
                        
                        
                        
                        
#END CALCULATE MEAN ALBEDO + SE AT PLOT LEVEL ---------------------------------------------------------------------------------
                        
##ALBEDO DIFFERENCES APPEAR TO BE QUITE SMALL - ALBEDO VS TIME isn't particularly interesting (lots of variation
#due to climate). Confirms that I need to look at treatment difference in a given site - how do I compare multiple
#subplots?
                
                
        #CALCULATE AVERAGE ALBEDO PER SPECIES PER PLOT
        plot_means <- aggregate(final_albedo$Albedo, by = list(final_albedo$LocalityName, final_albedo$LocalityCode, final_albedo$Treatment, final_albedo$Group, final_albedo$Years_Since_Exclosure, final_albedo$Month), FUN = mean)
        colnames(plot_means) <- c("LocalityName", "LocalityCode", "Treatment", "Group", "Years_Since_Exclosure", "Month", "Avg_Plot_Albedo")
        
        #Calculate difference between average plot albedos for each species
        
                #Aggregate with sum function (i.e. calculate difference between open and exclosure for each LocalityName)
                plot_diff <- aggregate(plot_means$Avg_Plot_Albedo, by = list(plot_means$LocalityName, plot_means$Group, plot_means$Years_Since_Exclosure, plot_means$Month), FUN = diff)
                colnames(plot_diff) <- c("LocalityName", "Group", "Years_Since_Exclosure", "Month", "Mean_Albedo_Diff")
                
                #Fix numeric(0) list error
                plot_diff$Mean_Albedo_Diff <- as.numeric(plot_diff$Mean_Albedo_Diff)
                
        #CALCULATE MEANS + SE OF MEAN ALBEDO DIFFERENCES
                
                #Remove NA values to allow aggregation
                plot_diff <- plot_diff[!is.na(plot_diff$Mean_Albedo_Diff),]
                
                #Aggregate means
                diff_means <- aggregate(plot_diff$Mean_Albedo_Diff, by = list(plot_diff$Group, plot_diff$Years_Since_Exclosure, plot_diff$Month), FUN = mean)
                colnames(diff_means) <- c("Group", "Years_Since_Exclosure", "Month", "Mean_Albedo_Diff")
                
                #Add placeholder columns
                diff_means$SE <- as.numeric('')
                
                #Calculate SEs for each species, month, and year
                for(i in min(diff_means$Years_Since_Exclosure):max(diff_means$Years_Since_Exclosure)){
                        
                        #Loop through each month
                        for(j in 1:12){
                                
                                #Birch mean albedo diff SE
                                                         
                                        diff_means$SE[diff_means$Group == "Birch" & diff_means$Years_Since_Exclosure == i & diff_means$Month == j] <- std(plot_diff$Mean_Albedo_Diff[plot_diff$Years_Since_Exclosure == i & plot_diff$Group == "Birch" & plot_diff$Month == j])
                                        
                                
                                #Spruce mean albedo diff SE
                                
                                        diff_means$SE[diff_means$Group == "Spruce" & diff_means$Years_Since_Exclosure == i & diff_means$Month == j] <- std(plot_diff$Mean_Albedo_Diff[plot_diff$Years_Since_Exclosure == i & plot_diff$Group == "Spruce" & plot_diff$Month == j])
                                        
                                
                                #Pine mean albedo diff SE
                                
                                        diff_means$SE[diff_means$Group == "Pine" & diff_means$Years_Since_Exclosure == i & diff_means$Month == j] <- std(plot_diff$Mean_Albedo_Diff[plot_diff$Years_Since_Exclosure == i & plot_diff$Group == "Pine" & plot_diff$Month == j])
                                
                                        
                                #Composite mean albedo diff SE
                                        
                                        diff_means$SE[diff_means$Group == "Composite" & diff_means$Years_Since_Exclosure == i & diff_means$Month == j] <- std(plot_diff$Mean_Albedo_Diff[plot_diff$Years_Since_Exclosure == i & plot_diff$Group == "Composite" & plot_diff$Month == j])
                                        
                                        
                        }
                        
                }
                
                

#PLOTS START HERE -------------------------------------------
                
        
        #WITH COMPOSITE ALBEDO -------------------------
                
                #Version A
                png(filename = "1_Albedo_Exclosures/Approach_1B/Output/albedo_estimates/means/mean_albedo_differences_comp_a.png",
                    width = 2200,
                    height = 1200,
                    bg = "white")
                
                ggplot(subset(diff_means, Group %in% c("Spruce", "Pine", "Birch")), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff, color = Group, group = Group)) +
                        geom_hline(lwd = 1, color = "#e6e6e6", yintercept = 0) +
                        geom_errorbar(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), colour="black", width=0.7, position = pd, alpha = 0.4) +
                        geom_line(lwd = 1.2, position = pd, aes(linetype = Group), alpha = 0.4) +
                        geom_point(size = 2.2, position = pd, aes(shape = Group), alpha = 0.4) +
                        theme_bw() +
                        facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                        labs(x = "Years Since Exclosure", y = "Mean Albedo Difference (Excl. - Open)", color = "Tree Species:") +
                        scale_x_continuous(limits = c(0,12), breaks = c(1,3,5,7,9,11)) +
                        scale_y_continuous(limits = c(-0.018, 0.012), breaks = c(-0.01, 0.00, 0.01)) +
                        scale_color_discrete(labels = c("Deciduous", "Pine", "Spruce")) +
                        geom_point(data = subset(diff_means, Group == "Composite"), size = 2.2, position = pd, color = "#333333") +
                        geom_line(data = subset(diff_means, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff), color = "#333333", linetype = 1, size = 1.3) +
                        theme(plot.title = element_text(hjust = 0.5, size = 50, margin = margin(t = 40, b = 40)),
                              axis.text.x = element_text(size = 20, margin = margin(t=16)),
                              axis.text.y = element_text(size = 22, margin = margin(r=16)),
                              axis.title.x = element_text(size = 34, margin = margin(t=40, b = 24)),
                              axis.title.y = element_text(size = 34, margin = margin(r=40)),
                              strip.text.x = element_text(size = 22),
                              legend.title = element_text(size = 28),
                              legend.text = element_text(size = 26, margin = margin(t=10)),
                              legend.position = "bottom") +
                        guides(shape = F) +
                        guides(linetype = F) +
                        guides(color = guide_legend(override.aes = list(size = 5) ) )
                
                dev.off()
                
                
                #Version B
                png(filename = "1_Albedo_Exclosures/Approach_1B/Output/albedo_estimates/means/mean_albedo_differences_comp_b.png",
                    width = 2200,
                    height = 1200,
                    bg = "white")
                
                ggplot(subset(diff_means, Group %in% c("Spruce", "Pine", "Birch")), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff, color = Group, group = Group)) +
                        geom_hline(lwd = 1, color = "#e6e6e6", yintercept = 0) +
                        #geom_errorbar(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), colour="black", width=0.7, position = pd, alpha = 0.4) +
                        geom_line(lwd = 1.2, position = pd, aes(linetype = Group), alpha = 0.4) +
                        geom_point(size = 2.2, position = pd, aes(shape = Group), alpha = 0.4) +
                        theme_bw() +
                        facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                        labs(x = "Years Since Exclosure", y = "Mean Albedo Difference (Excl. - Open)", color = "Tree Species:") +
                        scale_x_continuous(limits = c(0,12), breaks = c(1,3,5,7,9,11)) +
                        scale_y_continuous(limits = c(-0.018, 0.012), breaks = c(-0.01, 0.00, 0.01)) +
                        scale_color_discrete(labels = c("Deciduous", "Pine", "Spruce")) +
                        geom_errorbar(data = subset(diff_means, Group == "Composite"), aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), colour="black", width=0.7, position = pd, alpha = 1) +
                        geom_point(data = subset(diff_means, Group == "Composite"), size = 2.2, position = pd, color = "#333333") +
                        geom_line(data = subset(diff_means, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff), color = "#333333", linetype = 1, size = 1.3) +
                        theme(plot.title = element_text(hjust = 0.5, size = 50, margin = margin(t = 40, b = 40)),
                              axis.text.x = element_text(size = 20, margin = margin(t=16)),
                              axis.text.y = element_text(size = 22, margin = margin(r=16)),
                              axis.title.x = element_text(size = 34, margin = margin(t=40, b = 24)),
                              axis.title.y = element_text(size = 34, margin = margin(r=40)),
                              strip.text.x = element_text(size = 22),
                              legend.title = element_text(size = 28),
                              legend.text = element_text(size = 26, margin = margin(t=10)),
                              legend.position = "bottom") +
                        guides(shape = F) +
                        guides(linetype = F) +
                        guides(color = guide_legend(override.aes = list(size = 5) ) )
                
                dev.off()
                
        #WITHOUT COMPOSITE ALBEDO -------------------------   
                
                #Plot Means + SE
                
                        #All Species Together
                        png(filename = "1_Albedo_Exclosures/Approach_1B/Output/albedo_estimates/means/mean_albedo_differences.png",
                            width = 2200,
                            height = 1200,
                            bg = "white")
                        
                        ggplot(subset(diff_means, Group %in% c("Spruce", "Pine", "Birch")), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff, color = Group, group = Group)) +
                                geom_hline(lwd = 1, color = "#e6e6e6", yintercept = 0) +
                                geom_errorbar(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), colour="black", width=0.7, position = pd) +
                                geom_line(lwd = 1.2, position = pd, aes(linetype = Group)) +
                                geom_point(size = 2, position = pd, aes(shape = Group)) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Mean Albedo Difference (Excl. - Open)", color = "Tree Species:") +
                                scale_x_continuous(limits = c(0,12), breaks = c(1,3,5,7,9,11)) +
                                scale_y_continuous(limits = c(-0.018, 0.012), breaks = c(-0.01, 0.00, 0.01)) +
                                scale_color_discrete(labels = c("Deciduous", "Pine", "Spruce")) +
                                theme(plot.title = element_text(hjust = 0.5, size = 50, margin = margin(t = 40, b = 40)),
                                      axis.text.x = element_text(size = 20, margin = margin(t=16)),
                                      axis.text.y = element_text(size = 22, margin = margin(r=16)),
                                      axis.title.x = element_text(size = 34, margin = margin(t=40, b = 24)),
                                      axis.title.y = element_text(size = 34, margin = margin(r=40)),
                                      strip.text.x = element_text(size = 22),
                                      legend.title = element_text(size = 28),
                                      legend.text = element_text(size = 26, margin = margin(t=10)),
                                      legend.position = "bottom") +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(color = guide_legend(override.aes = list(size = 5) ) )
                        
                        dev.off()
                        
        #COMPOSITE ALBEDO ONLY -------------------------   
                        
                #Plot Means + SE
                        
                        #All Species Together
                        png(filename = "1_Albedo_Exclosures/Approach_1B/Output/albedo_estimates/means/mean_albedo_differences_comp_only.png",
                            width = 2200,
                            height = 1200,
                            bg = "white")
                        
                        ggplot(subset(diff_means, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Mean_Albedo_Diff)) +
                                geom_hline(lwd = 1, color = "#e6e6e6", yintercept = 0) +
                                geom_errorbar(aes(ymin = (Mean_Albedo_Diff - SE), ymax = (Mean_Albedo_Diff + SE)), colour="black", width=0.7, position = pd) +
                                geom_line(lwd = 1.2, position = pd, linetype = 2, color = "#666666") +
                                geom_point(size = 2, position = pd) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Mean Albedo Difference (Excl. - Open)") +
                                scale_x_continuous(limits = c(0,12), breaks = c(1,3,5,7,9,11)) +
                                scale_y_continuous(limits = c(-0.018, 0.012), breaks = c(-0.01, 0.00, 0.01)) +
                                theme(plot.title = element_text(hjust = 0.5, size = 50, margin = margin(t = 40, b = 40)),
                                      axis.text.x = element_text(size = 20, margin = margin(t=16)),
                                      axis.text.y = element_text(size = 22, margin = margin(r=16)),
                                      axis.title.x = element_text(size = 34, margin = margin(t=40, b = 24)),
                                      axis.title.y = element_text(size = 34, margin = margin(r=40)),
                                      strip.text.x = element_text(size = 22),
                                      legend.title = element_text(size = 28),
                                      legend.text = element_text(size = 26, margin = margin(t=10)),
                                      legend.position = "bottom") +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(color = guide_legend(override.aes = list(size = 5) ) )
                        
                        dev.off()

#RESULTS SEEM SIMILAR TO OTHER APPROACH 1A, BUT MORE VARIATION
                        
        #TRY PLOTTING MOVING MEANS ACROSS YSE FOR EACH SPECIES (JUST TO VISUALIZE CURVES)
                        
                #Create placeholder/copy df
                rolling <- diff_means
                
                #Add rolling means column
                rolling$Rolling_Mean_Albedo_Diff <- as.numeric('')
                
                
                #Loop through each month
                for(i in 1:12){
                        
                        #Loop through each group
                        spec <- c("Birch", "Pine", "Spruce", "Composite")
                        
                        for(j in 1:length(spec)){
                                
                                #Get frame of albedo diff values to calculate rolling means from (month i and species j)
                                frame <- diff_means$Mean_Albedo_Diff[diff_means$Group == spec[j] & diff_means$Month == i]
                                roll <- rollapplyr(frame, 3, mean, partial = TRUE)
                                
                                #Add frame of rolling means to "rolling" df
                                for(k in 1:length(roll)){
                                        rolling$Rolling_Mean_Albedo_Diff[rolling$Group == spec[j] & rolling$Month == i & rolling$Years_Since_Exclosure == k] <- roll[k]
                                }
                                
                        }
                          
                }
                
                
        #Plot rolling means
                
                #WITH COMPOSITE
                
                        #All Species Together
                        png(filename = "1_Albedo_Exclosures/Approach_1B/Output/albedo_estimates/means/rolling_mean_albedo_differences_comp.png",
                            width = 2200,
                            height = 1200,
                            bg = "white")
                        
                        ggplot(subset(rolling, Group %in% c("Birch", "Spruce", "Pine")), aes(x = Years_Since_Exclosure, y = Rolling_Mean_Albedo_Diff, color = Group)) +
                                geom_hline(lwd = 1, color = "#e6e6e6", yintercept = 0) +
                                geom_line(lwd = 1.2, position = pd) +
                                geom_point(size = 2, position = pd) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Rolling Mean Albedo Difference (Excl. - Open)", color = "Tree Species:") +
                                scale_x_continuous(limits = c(0,12), breaks = c(1,3,5,7,9,11)) +
                                scale_y_continuous(limits = c(-0.018, 0.012), breaks = c(-0.01, 0.00, 0.01)) +
                                scale_color_discrete(labels = c("Deciduous", "Pine", "Spruce")) +
                                geom_line(data = subset(rolling, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Rolling_Mean_Albedo_Diff), color = "#666666", linetype = 4, size = 1) +
                                theme(plot.title = element_text(hjust = 0.5, size = 50, margin = margin(t = 40, b = 40)),
                                      axis.text.x = element_text(size = 20, margin = margin(t=16)),
                                      axis.text.y = element_text(size = 22, margin = margin(r=16)),
                                      axis.title.x = element_text(size = 34, margin = margin(t=40, b = 24)),
                                      axis.title.y = element_text(size = 34, margin = margin(r=40)),
                                      strip.text.x = element_text(size = 22),
                                      legend.title = element_text(size = 28),
                                      legend.text = element_text(size = 26, margin = margin(t=10)),
                                      legend.position = "bottom") +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(group = F) +
                                guides(color = guide_legend(override.aes = list(size = 5) ) )
                        
                        dev.off()
                        
                
                
                #WITHOUT COMPOSITE
                
                        #All Species Together
                        png(filename = "1_Albedo_Exclosures/Approach_1B/Output/albedo_estimates/means/rolling_mean_albedo_differences.png",
                            width = 2200,
                            height = 1200,
                            bg = "white")
                        
                        ggplot(subset(rolling, Group %in% c("Birch", "Spruce", "Pine")), aes(x = Years_Since_Exclosure, y = Rolling_Mean_Albedo_Diff, color = Group)) +
                                geom_hline(lwd = 1, color = "#e6e6e6", yintercept = 0) +
                                geom_line(lwd = 1.2, position = pd, aes(linetype = Group)) +
                                geom_point(size = 2, position = pd, aes(shape = Group)) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Rolling Mean Albedo Difference (Excl. - Open)", color = "Tree Species:") +
                                scale_x_continuous(limits = c(0,12), breaks = c(1,3,5,7,9,11)) +
                                scale_y_continuous(limits = c(-0.018, 0.012), breaks = c(-0.01, 0.00, 0.01)) +
                                scale_color_discrete(labels = c("Deciduous", "Pine", "Spruce")) +
                                theme(plot.title = element_text(hjust = 0.5, size = 50, margin = margin(t = 40, b = 40)),
                                      axis.text.x = element_text(size = 20, margin = margin(t=16)),
                                      axis.text.y = element_text(size = 22, margin = margin(r=16)),
                                      axis.title.x = element_text(size = 34, margin = margin(t=40, b = 24)),
                                      axis.title.y = element_text(size = 34, margin = margin(r=40)),
                                      strip.text.x = element_text(size = 22),
                                      legend.title = element_text(size = 28),
                                      legend.text = element_text(size = 26, margin = margin(t=10)),
                                      legend.position = "bottom") +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(group = F) +
                                guides(color = guide_legend(override.aes = list(size = 5) ) )
                        
                        dev.off()
        
                        
                        
        #COMPOSITE ALBEDO ONLY -------------------------   
                        
                #Plot Means + SE
                        
                        #All Species Together
                        png(filename = "1_Albedo_Exclosures/Approach_1B/Output/albedo_estimates/means/rolling_mean_albedo_differences_comp_only.png",
                            width = 2200,
                            height = 1200,
                            bg = "white")
                        
                        ggplot(subset(rolling, Group == "Composite"), aes(x = Years_Since_Exclosure, y = Rolling_Mean_Albedo_Diff)) +
                                geom_hline(lwd = 1, color = "#e6e6e6", yintercept = 0) +
                                geom_line(lwd = 1.2, position = pd, linetype = 2, color = "#666666") +
                                geom_point(size = 2, position = pd) +
                                theme_bw() +
                                facet_wrap(~ Month, ncol = 6, labeller = month_labs) +
                                labs(x = "Years Since Exclosure", y = "Rolling Mean Albedo Difference (Excl. - Open)") +
                                scale_x_continuous(limits = c(0,12), breaks = c(1,3,5,7,9,11)) +
                                scale_y_continuous(limits = c(-0.018, 0.012), breaks = c(-0.01, 0.00, 0.01)) +
                                theme(plot.title = element_text(hjust = 0.5, size = 50, margin = margin(t = 40, b = 40)),
                                      axis.text.x = element_text(size = 20, margin = margin(t=16)),
                                      axis.text.y = element_text(size = 22, margin = margin(r=16)),
                                      axis.title.x = element_text(size = 34, margin = margin(t=40, b = 24)),
                                      axis.title.y = element_text(size = 34, margin = margin(r=40)),
                                      strip.text.x = element_text(size = 22),
                                      legend.title = element_text(size = 28),
                                      legend.text = element_text(size = 26, margin = margin(t=10)),
                                      legend.position = "bottom") +
                                guides(shape = F) +
                                guides(linetype = F) +
                                guides(color = guide_legend(override.aes = list(size = 5) ) )
                        
                        dev.off()
                        
                
                
                

                        
                        
                        
#MODEL ---------------------------------------------------------------------------------
                        
                        
        #HYPOTHESES TO TEST:
                
                #Overall albedo will be significantly lower in exclosures during winter months (due to greater stand volume)
                        
                #In particular, birch albedo will be significantly lower in exclosures during winter months
                #Spruce albedo and pine albedo will not be significantly lower 
                        
        #Histogram of albedo (response variable)
        hist(final_albedo$Albedo)
                        
                #Very skewed - maybe a GLME would make sense? Need to account for random effects
                        
        #Correlation matrix?
                        
        #Try a LMM --------
                        
                #Treat "Treatment", "Group/Species", "Years Since Exclosure", and "Month" as factors
                final_albedo$Month <- as.factor(final_albedo$Month)
                final_albedo$Treatment <- as.factor(final_albedo$Treatment)
                final_albedo$Group <- as.factor(final_albedo$Group)
                final_albedo$Years_Since_Exclosure <- as.factor(final_albedo$Years_Since_Exclosure)
                
                #Specify complicated model w/ random effects
                model <- lmer(Albedo ~ Treatment +
                                      Group +
                                      Month +
                                      Years_Since_Exclosure + 
                                      Treatment*Group +
                                      Treatment*Month +
                                      Treatment*Years_Since_Exclosure +
                                      Group*Month +
                                      Group*Years_Since_Exclosure +
                                      Years_Since_Exclosure*Month +
                                      Treatment*Group*Month +
                                      (1|Year) +
                                      (1|LocalityName/LocalityCode), data = final_albedo)
                
                #Summarize model
                summary(model)
                
                #Examine model residuals
                plot(model, alpha = 0.2)
                
                        #KEY POINT: Residuals have a very strange distribution - might need to try a GLME
                
        
        #Try
        
                        
        
#END MODEL ----------------------------------------------------------------------------- 