## Script to calculate radiative forcing using albedo and delta biomass (kg/m2)


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
                albedo <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/subplot_albedo_estimates.csv", header = T)
        
                #Filter to "composite" albedo values
                albedo_comp <- albedo[albedo$Group == "Composite",]
                
                #Calculate mean albedo per plot (average subplot values)
                albedo_plot_means <- aggregate(albedo_comp$Albedo, by = list("Region" = albedo_comp$Region,
                                                                             "LocalityName" = albedo_comp$LocalityName,
                                                                             "LocalityCode" = albedo_comp$LocalityCode,
                                                                             "Treatment" = albedo_comp$Treatment,
                                                                             "Years_Since_Exclosure" = albedo_comp$Years_Since_Exclosure,
                                                                             "Month" = albedo_comp$Month), FUN = mean)
                colnames(albedo_plot_means)[7] <- "Mean_Plot_Albedo"

                
        ##PLOT-LEVEL BIOMASS DATA ----
        
                #Read CSV
                biomass <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/tree_biomass.csv", header = T)
        
                #(1) Sum total biomass (g) for EACH SUBPLOT (doing this first to calculate kg/m2 biomass for each subplot
                #    will take an average of subplot biomass later)
                
                        #Sum biomass in each subplot
                        subplot_df <- aggregate(biomass$Subplot_Total_Biomass_g, by = list("Region" = biomass$Region,
                                                                           "LocalityName" = biomass$LocalityName,
                                                                           "LocalityCode" = biomass$LocalityCode,
                                                                           "Treatment" = biomass$Treatment,
                                                                           "Plot" = biomass$Plot,
                                                                           "Years_Since_Exclosure" = biomass$Years_Since_Exclosure), FUN = sum)
                        
                        colnames(subplot_df)[7] <- "Total_Biomass_kg"
                        
                        #Convert summed biomass g to kg
                        subplot_df$Total_Biomass_kg <- subplot_df$Total_Biomass_kg / 1000
                        
                        #Convert biomass to kg/m2
                        
                                #Each circular subplot has a radius of 2m - A = pi*r^2
                        
                                        #Square meters
                                        subplot_area <- pi*(2^2) #12.57m2
                                        
                                        #Hectares (divide m2 by 10000)
                                        subplot_area_ha <- subplot_area/10000
                                
                                
                                #Divide each value for kg by plot_area
                                        
                                        #Square meters
                                        subplot_df$Total_Biomass_kg_m2 <- subplot_df$Total_Biomass_kg / subplot_area 
                                        
                                        #Hectares
                                        subplot_df$Total_Biomass_kg_ha <- subplot_df$Total_Biomass_kg / subplot_area_ha
                                
                                        #Now we have TOTAL BIOMASS per unit area (both in kg/m2 and kg/ha) for EACH CIRCULAR
                                        #SUBPLOT in a given plot
                                
                                        #We can then take the average of biomass within each subplot for each PLOT
                        
                #(2) Get average biomass (kg/m2) for each PLOT (take mean of all subplots in a given plot)
                
                        #Aggregate means by plot
                        biomass_plot_means <- aggregate(subplot_df$Total_Biomass_kg_m2, by = list("Region" = subplot_df$Region,
                                                                                                  "LocalityName" = subplot_df$LocalityName,
                                                                                                  "LocalityCode" = subplot_df$LocalityCode,
                                                                                                  "Treatment" = subplot_df$Treatment,
                                                                                                  "Years_Since_Exclosure" = subplot_df$Years_Since_Exclosure), FUN = mean)
                        colnames(biomass_plot_means)[6] <- "Mean_Plot_Biomass_kg_m2"
                        
                        #Add column w/ kg/ha
                        biomass_means_kg_ha <- aggregate(subplot_df$Total_Biomass_kg_ha, by = list("Region" = subplot_df$Region,
                                                                                                  "LocalityName" = subplot_df$LocalityName,
                                                                                                  "LocalityCode" = subplot_df$LocalityCode,
                                                                                                  "Treatment" = subplot_df$Treatment,
                                                                                                  "Years_Since_Exclosure" = subplot_df$Years_Since_Exclosure), FUN = mean)
                        colnames(biomass_means_kg_ha)[6] <- "Mean_Plot_Biomass_kg_ha"
                        
                        #Join column w/ first biomass df
                        biomass_plot_means <- cbind(biomass_plot_means, biomass_means_kg_ha$Mean_Plot_Biomass_kg_ha)
                        colnames(biomass_plot_means)[7] <- "Mean_Plot_Biomass_kg_ha"
                        
                #Fix "stangeskovene eidskog " space issue
                biomass_plot_means$LocalityName[biomass_plot_means$LocalityName == 'stangeskovene eidskog '] <- 'stangeskovene_eidskog'
                        
                
                
                
#END INITIAL DATA IMPORT + FORMATTING --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#CREATE DATAFRAMES FOR RF CALCULATIONS --------------------------------------------------------------------------------------

        #OBJECTIVE: Create a dataframe that has:
        #  (1) Albedo for each plot (across all months and years)
        #  (2) Delta albedo (from the month before) for each plot
        #  (3) Biomass for each plot (at an annual scale)
        #  (4) Delta biomass for each plot (from the year before)
        
                        
        #STEP 1 (Albedo for each plot) ---------
        
                #Copy albedo df as starting point  
                final <- albedo_plot_means
                
        #STEP 2 (Monthly delta albedo for each plot) ---------
                
                #Add placeholder column for delta albedo
                final$Delta_Albedo <- as.numeric('')
                
                #Get DELTA ALBEDO for each month (i.e. subtract albedo of month i-1 from albedo of month i)
                for(i in 1:nrow(final)){
                        
                        #Get variables
                        reg <- final[i, "Region"]
                        loc <- final[i, "LocalityName"]
                        plot <- final[i, "LocalityCode"]
                        tr <- final[i, "Treatment"]
                        yse <- final[i, "Years_Since_Exclosure"]
                        mt <- final[i, "Month"]
                        
                        #Get min year of data for current LocalityCode
                        min_yr <- min(final$Years_Since_Exclosure[final$LocalityCode == plot])
                        
                        #Get current albedo
                        curr_alb <- final[i, "Mean_Plot_Albedo"]
                        
                        #Get previous albedo (to calculate monthly delta albedo)
                        
                                #If month = Feb:Dec, get previous month from year == yse
                                if(mt %in% c(2:12)){
                                
                                        #Get albedo from previous month in year == yse
                                        prev_alb <- final$Mean_Plot_Albedo[final$Region == reg &
                                                                                   final$LocalityName == loc &
                                                                                   final$LocalityCode == plot &
                                                                                   final$Treatment == tr &
                                                                                   final$Years_Since_Exclosure == yse &
                                                                                   final$Month == (mt - 1)]
                                        
                                        #Calculate delta albedo
                                        d_alb <- curr_alb - prev_alb
                                        
                                        #Add to df
                                        final[i, "Delta_Albedo"] <- d_alb
                                        
                                } else if (mt == 1){
                                        
                                        #If month == Jan, need to get albedo from Dec in previous year (yse - 1)
                                        
                                                #If current yse == min_yr, then add NA value (no delta to calculate)
                                                if( yse - min_yr == 0 ){
                                                        
                                                        final[i, "Delta_Albedo"] <- NA
                                                        
                                                } else {
                                                        
                                                        #Data in Dec of previous year exists - grab this value
                                                        prev_alb <- final$Mean_Plot_Albedo[final$Region == reg &
                                                                                                   final$LocalityName == loc &
                                                                                                   final$LocalityCode == plot &
                                                                                                   final$Treatment == tr &
                                                                                                   final$Years_Since_Exclosure == (yse - 1) &
                                                                                                   final$Month == 12]
                                                        #Calculate delta albedo
                                                        d_alb <- curr_alb - prev_alb
                                                        
                                                        #Add to df
                                                        final[i, "Delta_Albedo"] <- d_alb
                                                        
                                                }
                                        
                                        
                                }
                        
                        
                }
                
                        #NOW, for every plot, there should be delta albedo values (from the previous month), 
                        #EXCEPT for the first month of the first year of data (since we don't know the 
                        #starting albedo)
                
                
        #STEP 3 (Biomass for each plot) --------
                
                #Add placeholder columns
                final$Mean_Plot_Biomass_kg_m2 <- as.numeric('')
                final$Mean_Plot_Biomass_kg_ha <- as.numeric('')
                
                #Add corresponding biomass to each row
                #NOTE: biomass values will be repeated 12x (once for each month of albedo)
                for(i in 1:nrow(final)){
                        
                        #Get variables
                        reg <- final[i, "Region"]
                        loc <- final[i, "LocalityName"]
                        plot <- final[i, "LocalityCode"]
                        tr <- final[i, "Treatment"]
                        yse <- final[i, "Years_Since_Exclosure"]
                        
                        #Grab biomass where variables match
                        
                                #kg/m2
                                bio_1 <- biomass_plot_means$Mean_Plot_Biomass_kg_m2[biomass_plot_means$Region == reg &
                                                                                          biomass_plot_means$LocalityName == loc &
                                                                                          biomass_plot_means$LocalityCode == plot &
                                                                                          biomass_plot_means$Treatment == tr &
                                                                                          biomass_plot_means$Years_Since_Exclosure == yse]
                                
                                #kg/ha
                                bio_2 <- biomass_plot_means$Mean_Plot_Biomass_kg_ha[biomass_plot_means$Region == reg &
                                                                                            biomass_plot_means$LocalityName == loc &
                                                                                            biomass_plot_means$LocalityCode == plot &
                                                                                            biomass_plot_means$Treatment == tr &
                                                                                            biomass_plot_means$Years_Since_Exclosure == yse]
                        
                        #Add biomass values to final dataframe
                        final[i, "Mean_Plot_Biomass_kg_m2"] <- bio_1
                        final[i, "Mean_Plot_Biomass_kg_ha"] <- bio_2
                        
                }
                
                
                #NOTE: REMOVING FIRST YEAR OF DRANGEDAL3 DATA (WHERE YSE == 1)
                #Only "UB" data available in year 1, which doesn't allow for delta calculation
                final <- final[!(final$LocalityName == 'drangedal3' & final$Years_Since_Exclosure == 1),]
                
                
        #STEP 4 (Delta biomass for each plot) --------
                
                #Add placeholder column for delta biomass values
                final$Delta_Biomass_kg_m2 <- as.numeric('')
                final$Delta_Biomass_kg_ha <- as.numeric('')
                
                
                #For each plot, calculate annual delta biomass
                #NOTE: plots will have a delta biomass of NA in first year of available data
                #(since we don't know starting biomass)
                for(i in 1:nrow(final)){
                        
                        #Get variables
                        reg <- final[i, "Region"]
                        loc <- final[i, "LocalityName"]
                        plot <- final[i, "LocalityCode"]
                        tr <- final[i, "Treatment"]
                        yse <- final[i, "Years_Since_Exclosure"]
                        
                        #Get min year of data for current LocalityCode
                        min_yr <- min(final$Years_Since_Exclosure[final$LocalityCode == plot])
                        
                        #Get current biomass values
                        
                                #kg/m2
                                curr_bio_1 <- final[i, "Mean_Plot_Biomass_kg_m2"]
                                curr_bio_2 <- final[i, "Mean_Plot_Biomass_kg_ha"]
                        
                        #If first year of available data, delta biomass is NA
                        if( yse - min_yr == 0 ){
                                
                                #No prior biomass to compute delta biomass with
                                final[i, "Delta_Biomass_kg_m2"] <- NA
                                final[i, "Delta_Biomass_kg_ha"] <- NA
                                
                                
                        } else {
                                
                                #Data in Dec of previous year exists - grab this value
                                
                                        #kg/m2
                                        prev_bio_1 <- final$Mean_Plot_Biomass_kg_m2[final$Region == reg &
                                                                                   final$LocalityName == loc &
                                                                                   final$LocalityCode == plot &
                                                                                   final$Treatment == tr &
                                                                                   final$Years_Since_Exclosure == (yse - 1)][1]
                                
                                        #kg/ha
                                        prev_bio_2 <- final$Mean_Plot_Biomass_kg_ha[final$Region == reg &
                                                                                            final$LocalityName == loc &
                                                                                            final$LocalityCode == plot &
                                                                                            final$Treatment == tr &
                                                                                            final$Years_Since_Exclosure == (yse - 1)][1]
                                #Calculate delta albedo
                                        
                                        #kg/m2
                                        d_bio_1 <- curr_bio_1 - prev_bio_1
                                        
                                        #kg/ha
                                        d_bio_2 <- curr_bio_2 - prev_bio_2
                                
                                #Add to df
                                final[i, "Delta_Biomass_kg_m2"] <- d_bio_1
                                final[i, "Delta_Biomass_kg_ha"] <- d_bio_2
                                
                        }
                        
                }
                
                
                                
                                
        #WRITE FINAL CSV --------
                
                #Write CSV
                write.csv(final, "1_Albedo_Exclosures/1_Data_Processing/6_Radiative_Forcing/Output/plot_delta_biomass_albedo.csv")                       
                

#END CREATE DATAFRAMES FOR RF CALCULATIONS------------------------------------------------------------------------
                        
                        
                        
                        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                        
                        
                        
                        
#CALCULATE/PLOT MEAN DELTA BIOMASS PER YEAR & TREATMENT ---------------------------------------------------------------------

        #NOTE: Calculating mean delta biomass for initial visualization purposes
        #Calculate means w/ SEs for each year (for biomass per unit area - kg/km2)
        
        #NOTE: Add more visualizations of RF once calculated
        
                #Calculate mean delta biomass per year (in each region)
                
                        #Only calculate w/ first month of data (since biomass values are repeated 12x - once for each month)
                        base <- final[final$Month == 1,]
                
                        #Remove NA rows to allow for aggregation function
                        base <- base[!is.na(base$Delta_Biomass_kg_m2),]
        
                        #Aggregate means (by REGION)
                        mean_d_bio <- aggregate(base$Delta_Biomass_kg_m2, by = list("Region" = base$Region,
                                                                                    "Years_Since_Exclosure" = base$Years_Since_Exclosure,
                                                                                    "Treatment" = base$Treatment), FUN = mean)
                        colnames(mean_d_bio)[4] <- 'Mean_Delta_Biomass_kg_m2'
                
                #Calculate SEs
                
                        #Define function
                        std <- function(x) sd(x)/sqrt(length(x))
                
                        #Placeholder column
                        mean_d_bio$SE <- as.numeric('')
                        
                        for(i in 1:nrow(mean_d_bio)){
                                
                                #Get variables for row i
                                reg <- mean_d_bio[i, "Region"]
                                yse <- mean_d_bio[i, "Years_Since_Exclosure"]
                                tr <- mean_d_bio[i, "Treatment"]
                                

                                #Calculate SE
                                se <- std(base$Delta_Biomass_kg_m2[base$Years_Since_Exclosure == yse &
                                                                           base$Treatment == tr &
                                                                           base$Region == reg])
                                
                                #Add to df
                                mean_d_bio[i, "SE"] <- se
                        }
        
        #Plot mean delta biomass per unit area (by region)
                
        pd <- position_dodge(0.25)
        label1 <- expression(Delta*' Biomass  ' ~(kg/m^2))
        pal <- wes_palette("Darjeeling1")
        
        #Treatment nice name
        mean_d_bio$TNN[mean_d_bio$Treatment == "B"] <- "Browsed"
        mean_d_bio$TNN[mean_d_bio$Treatment == "UB"] <- "Unbrowsed"
        
        ggplot(data = mean_d_bio, aes(x = Years_Since_Exclosure, y = Mean_Delta_Biomass_kg_m2, group = TNN, color = TNN)) +
                        geom_errorbar(aes(ymin = (Mean_Delta_Biomass_kg_m2 - SE), ymax = (Mean_Delta_Biomass_kg_m2 + SE)), colour="#666666", width=0.25, position = pd) +
                        geom_line(lwd = 0.5, position = pd, aes(linetype = TNN)) +
                        geom_point(position = pd, aes(shape = TNN)) +
                        theme_bw() +
                        geom_hline(mapping = aes(yintercept = 0), linetype = 2, lwd = 0.5, color = "#999999") +
                        facet_wrap(~Region, ncol = 1) +
                        labs(x = "Years Since Exclosure", y = label1, color = "Treatment:", linetype = "Treatment:", shape = "Treatment:") +
                        scale_x_continuous(breaks = c(1:11)) +
                        scale_color_manual(values = pal) +
                        theme(
                                legend.position = "right",
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10, l = 10)),
                                panel.grid.minor = element_blank()
                        ) 
                        
#END CALCULATE/PLOT MEAN DELTA BIOMASS PER YEAR & TREATMENT ---------------------------------------------------------------------
                        
   
       