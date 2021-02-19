## Script to calculate radiative forcing using albedo and delta biomass (kg/m2)


##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(gridExtra)
        library(cowplot)
        library(beepr)
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT + FORMATTING ----------------------------------------------------------------------------------------------

        ##SUBPLOT ALBEDO DATA ----

                #Read CSV
                albedo <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/subplot_albedo_estimates.csv", header = T)
        
                #Filter to "composite" albedo values
                albedo_comp <- albedo[albedo$Group == "Composite",]
        
        ##SUBPLOT BIOMASS DATA ----
        
                #Read CSV
                biomass <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/tree_biomass.csv", header = T)
        
                #Sum total biomass (g) for each subplot
                subplot_df <- aggregate(biomass$Biomass_g, by = list("Years_Since_Exclosure" = biomass$Years_Since_Exclosure,
                                                               "Region" = biomass$Region,
                                                               "LocalityName" = biomass$LocalityName,
                                                               "LocalityCode" = biomass$LocalityCode,
                                                               "Treatment" = biomass$Treatment,
                                                               "Plot" = biomass$Plot), FUN = sum)
                
                colnames(subplot_df)[7] <- "Total_Biomass_kg"
                
                #Convert summed biomass g to kg
                subplot_df$Total_Biomass_kg <- subplot_df$Total_Biomass_kg / 1000
                
                #Convert biomass to kg/m2
                
                        #Each circular subplot has a radius of 2m - A = pi*r^2
                        subplot_area <- pi*(2^2) #12.57m2
                        
                        #Divide each value for kg by plot_area
                        subplot_df$Total_Biomass_kg_m2 <- subplot_df$Total_Biomass_kg / subplot_area 
                        
                        
                                #Now we have TOTAL BIOMASS per unit area (kg/m2) for EACH CIRCULAR
                                #SUBPLOT in a given plot
                
                
                
#END INITIAL DATA IMPORT + FORMATTING --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#CREATE DATAFRAMES FOR RF CALCULATIONS --------------------------------------------------------------------------------------

        #LONG VERSION OF DATAFRAME ---------
        
                #Make copy of albedo w/o arbitrary index column
                long <- albedo_comp[,c(2:7,11,14)]
                        
                #Add placeholder columns
                long$Total_Biomass_kg <- as.numeric('')
                long$Total_Biomass_kg_m2 <- as.numeric('')
                
                #Add corresponding biomass data for each subplot
                for(i in 1:nrow(long)){
                        
                        #Region
                        reg <- long[i, "Region"]
                        locCode <- long[i, "LocalityCode"]
                        tr <- long[i, "Treatment"]
                        sub <- long[i, "Subplot"]
                        yse <- long[i, "Years_Since_Exclosure"]
                        
                        #Get biomass values
                        
                                #Total biomass (kg)
                                bio <- subplot_df$Total_Biomass_kg[subplot_df$Region == reg &
                                                                           subplot_df$LocalityCode == locCode &
                                                                           subplot_df$Treatment == tr &
                                                                           subplot_df$Plot == sub &
                                                                           subplot_df$Years_Since_Exclosure == yse]
                                
                                #Biomass per unit area (kg/m2)
                                bio_area <- subplot_df$Total_Biomass_kg_m2[subplot_df$Region == reg &
                                                                                subplot_df$LocalityCode == locCode &
                                                                                subplot_df$Treatment == tr &
                                                                                subplot_df$Plot == sub &
                                                                                subplot_df$Years_Since_Exclosure == yse]
                        
                        #Add to df
                        long[i, "Total_Biomass_kg"] <- bio
                        long[i, "Total_Biomass_kg_m2"] <- bio_area

                }
                
                #Calculate annual delta in biomass for each subplot
                
                        #Placeholder Columns
                        long$Annual_Delta_Total_Biomass_kg <- as.numeric('')
                        long$Annual_Delta_Total_Biomass_kg_m2 <- as.numeric('')
                        
                        #Calculate delta in biomass for EACH SUBPLOT in each year
                        #Note: first year subplots may not be included - each plot likely had
                        #a starting biomass before the experiment was initiated, as there was a time lag
                        #of several years between clear-cut and experiment start for some sites
                        #(we can't just assume that biomass started at 0 for these sites)
                        
                        #Tricky part here - not all data/plots start at YSE = 1 (some start at 2 or 3)
                        
                        for(i in 1:nrow(long)){
                                
                                #Get YSE - if first year, assume starting biomass = 0 and delta biomass = biomass in year 1
                                yse <- long[i, "Years_Since_Exclosure"]
                                
                                #Get other variables
                                locCode <- long[i, "LocalityCode"]
                                tr <- long[i, "Treatment"]
                                sub <- long[i, "Subplot"]
                                
                                #Get minimum YSE (since data for some plots doesn't start at YSE = 1)
                                min_yr <- min(long$Years_Since_Exclosure[long$LocalityCode == locCode &
                                                                                 long$Treatment == tr &
                                                                                 long$Subplot == sub])

                                #If YSE = first year of data, then we don't actually know delta biomass
                                #(since we didn't have starting values). Assign as NA if YSE = first year of data.
                                
                                if( yse == min_yr ){
                                        
                                        #Total Biomass
                                        delta_b <- NA
                                        
                                        #Biomass per unit area
                                        delta_ba <- NA
                                        
                                } else {
                                        
                                        #Set years
                                        curr_yr <- yse
                                        past_yr <- yse - 1
                                        
                                        #Total Biomass
                                        delta_b <- long$Total_Biomass_kg[long$Years_Since_Exclosure == curr_yr &
                                                                                 long$LocalityCode == locCode &
                                                                                 long$Treatment == tr &
                                                                                 long$Subplot == sub][1] - long$Total_Biomass_kg[long$Years_Since_Exclosure == past_yr &
                                                                                                                                         long$LocalityCode == locCode &
                                                                                                                                         long$Treatment == tr &
                                                                                                                                         long$Subplot == sub][1]
                                        
                                        #Biomass per unit area
                                        delta_ba <- long$Total_Biomass_kg_m2[long$Years_Since_Exclosure == curr_yr &
                                                                                  long$LocalityCode == locCode &
                                                                                  long$Treatment == tr &
                                                                                  long$Subplot == sub][1] - long$Total_Biomass_kg_m2[long$Years_Since_Exclosure == past_yr &
                                                                                                                                          long$LocalityCode == locCode &
                                                                                                                                          long$Treatment == tr &
                                                                                                                                          long$Subplot == sub][1]
                                }
                                
                                #Add to dfs
                                long[i, "Annual_Delta_Total_Biomass_kg"] <- delta_b
                                long[i, "Annual_Delta_Total_Biomass_kg_m2"] <- delta_ba
                                
                                
                        }
                        
                        
        #Write dataframe to CSV
        write.csv(long, "1_Albedo_Exclosures/1_Data_Processing/6_Radiative_Forcing/Output/delta_biomass_albedo.csv")
                        
        
        
#END CREATE DATAFRAMES FOR RF CALCULATIONS------------------------------------------------------------------------
                        
                        
                        
                        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                        
                        
                        
                        
#CALCULATE/PLOT MEAN DELTA BIOMASS PER YEAR & TREATMENT ---------------------------------------------------------------------

        #Calculate means w/ SEs for each year (for biomass per unit area - kg/km2)
        
                #Only calculate w/ first month of data (since biomass values are repeated 12x - once for each month)
                long_1 <- long[long$Month == 1,]
        
                #Remove NAs to allow for aggregate function
                long_1 <- long_1[!is.na(long_1$Annual_Delta_Total_Biomass_kg_m2),]
        
                #Aggregate means
                mean_delta <- aggregate(long_1$Annual_Delta_Total_Biomass_kg_m2, by = list("Treatment" = long_1$Treatment,
                                                                                           "Years_Since_Exclosure" = long_1$Years_Since_Exclosure), FUN = mean)
                colnames(mean_delta)[3] <- 'Mean_Delta_Biomass_kg_m2'
                
                #Calculate SEs
                
                        #Define function
                        std <- function(x) sd(x)/sqrt(length(x))
                
                        #Placeholder column
                        mean_delta$SE <- as.numeric('')
                        
                        for(i in 1:nrow(mean_delta)){
                                
                                #Get variables for row i
                                yse <- mean_delta[i, "Years_Since_Exclosure"]
                                tr <- mean_delta[i, "Treatment"]
                                
                                #Calculate SE
                                se <- std(long_1$Annual_Delta_Total_Biomass_kg_m2[long_1$Treatment == tr &
                                                                                    long_1$Years_Since_Exclosure == yse])
                                
                                #Add to df
                                mean_delta[i, "SE"] <- se
                        }
        
        #Plot delta biomass per unit area for each subplot across all years
        pd = position_dodge(0.5)
        
        ggplot(data = mean_delta, aes(x = Years_Since_Exclosure, y = Mean_Delta_Biomass_kg_m2, group = Treatment)) +
                        geom_errorbar(aes(ymin = (Mean_Delta_Biomass_kg_m2 - SE), ymax = (Mean_Delta_Biomass_kg_m2 + SE)), colour="#666666", width=0.55, position = pd) +
                        geom_line(position = pd, aes(linetype = Treatment), lwd = 0.5) +
                        geom_point(position = pd, aes(shape = Treatment)) +
                        theme_bw() +
                        geom_hline(mapping = aes(yintercept = 0), linetype = 2, lwd = 0.5, color = "#999999") +
                        labs(x = "Years Since Exclosure", y = Delta*' Biomass  ' ~(m^3/ha)) +
                        scale_x_continuous(breaks = c(2,4,6,8,10)) +
                        scale_color_discrete(labels = c("Browsed", "Unbrowsed")) +
                        #scale_y_continuous(limits = c(0.14, 0.457), breaks = c(0.15,0.2,0.25,0.3,0.35,0.4,0.45)) +
                        theme(
                                legend.position = "right",
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10))
                        ) 
                        
#END CALCULATE/PLOT MEAN DELTA BIOMASS PER YEAR & TREATMENT ---------------------------------------------------------------------
                        
   
       