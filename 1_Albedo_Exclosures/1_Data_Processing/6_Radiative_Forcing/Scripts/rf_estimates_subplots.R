## Script to calculate radiative forcing using albedo and delta biomass (kg/m2)

## Note - delta biomass (annual timescale) and delta albedo (monthly and annual timescale)
## are calculated for EACH SUBPLOT (thus, the RF estimates are on a subplot resolution)


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

        ##SUBPLOT-LEVEL ALBEDO DATA ----

                #Read CSV
                albedo <- read.csv("1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/subplot_albedo_estimates.csv", header = T)
        
                #Filter to "composite" albedo values
                albedo_comp <- albedo[albedo$Group == "Composite",]
                
                #Filter to relevant columns (use this for delta calculations)
                albedo_comp <- albedo_comp[,c(2:7,11,14)]
                

        ##SUBPLOT-LEVEL BIOMASS DATA ----
        
                #Read CSV
                biomass <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/tree_biomass.csv", header = T)
        
                #(1) Sum TOTAL biomass (g) for EACH SUBPLOT 
                
                        #Sum biomass in each subplot
                        subplot_df <- aggregate(biomass$Subplot_Total_Biomass_g, by = list("Region" = biomass$Region,
                                                                           "LocalityName" = biomass$LocalityName,
                                                                           "LocalityCode" = biomass$LocalityCode,
                                                                           "Treatment" = biomass$Treatment,
                                                                           "Plot" = biomass$Plot,
                                                                           "Years_Since_Exclosure" = biomass$Years_Since_Exclosure), FUN = sum)
                        
                        colnames(subplot_df)[7] <- "Total_Biomass_kg"
                        
                #(2) Convert g biomass to kg/m2 biomass
                        
                        #Convert summed biomass g to kg
                        subplot_df$Total_Biomass_kg <- subplot_df$Total_Biomass_kg / 1000
                        
                        #Convert biomass to kg/m2
                        
                                #Each circular subplot has a radius of 2m - A = pi*r^2
                        
                                        #Square meters
                                        subplot_area <- pi*(2^2) #12.57m2
                                        
                                #Divide each value for kg by plot_area
                                        
                                        #Square meters
                                        subplot_df$Total_Biomass_kg_m2 <- subplot_df$Total_Biomass_kg / subplot_area 
                                        
                                                #Now we have TOTAL BIOMASS per unit area (both in kg/m2 and kg/ha) for EACH CIRCULAR
                                                #SUBPLOT in a given plot
                                        
                        #Fix 'stangeskovene eidskog' space error
                        subplot_df$LocalityName[subplot_df$LocalityName == "stangeskovene eidskog "] <- 'stangeskovene_eidskog'
                                
                
                
                
#END INITIAL DATA IMPORT + FORMATTING --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#CREATE DATAFRAMES FOR RF CALCULATIONS --------------------------------------------------------------------------------------

        #OBJECTIVE: Create a dataframe that has:
        #  (1) Albedo for each subplot (across all months and years)
        #  (2) Delta albedo (from the month before) for each subplot
        #  (3) Biomass for each subplot (at an annual scale)
        #  (4) Delta biomass for each subplot (from the year before)
        
                        
        #STEP 1 (Albedo for each subplot) ---------
        
                #Copy albedo df as starting point  
                final <- albedo_comp
                
        #STEP 2 (Monthly delta albedo for each subplot) ---------
                
                #Add placeholder column for delta albedo
                final$Delta_Albedo <- as.numeric('')
                
                #Get DELTA ALBEDO for each month (i.e. subtract albedo of month i-1 from albedo of month i)
                for(i in 1:nrow(final)){
                        
                        #Get variables
                        reg <- final[i, "Region"]
                        loc <- final[i, "LocalityName"]
                        plot <- final[i, "LocalityCode"]
                        subplot <- final[i, "Subplot"]
                        tr <- final[i, "Treatment"]
                        yse <- final[i, "Years_Since_Exclosure"]
                        mt <- final[i, "Month"]
                        
                        #Get min year of data for current LocalityCode
                        min_yr <- min(final$Years_Since_Exclosure[final$LocalityCode == plot])
                        
                        #Get current albedo
                        curr_alb <- final[i, "Albedo"]
                        
                        #Get previous albedo (to calculate monthly delta albedo)
                        
                                #If month = Feb:Dec, get previous month from year == yse
                                if(mt %in% c(2:12)){
                                
                                        #Get albedo from previous month in year == yse
                                        prev_alb <- final$Albedo[final$Region == reg &
                                                                         final$LocalityName == loc &
                                                                         final$LocalityCode == plot &
                                                                         final$Subplot == subplot &
                                                                         final$Treatment == tr &
                                                                         final$Years_Since_Exclosure == yse &
                                                                         final$Month == (mt - 1)]
                                        
                                        #Calculate delta albedo
                                        d_alb <- curr_alb - prev_alb
                                        
                                        #Error catch (some subplots are missing data)
                                        if ( length(d_alb) == 0 ){
                                                d_alb <- NA
                                        }
                                        
                                        #Add to df
                                        final[i, "Delta_Albedo"] <- d_alb
                                        
                                } else if (mt == 1){
                                        
                                        #If month == Jan, need to get albedo from Dec in previous year (yse - 1)
                                        
                                                #If current yse == min_yr, then add NA value (no delta to calculate)
                                                if( yse - min_yr == 0 ){
                                                        
                                                        final[i, "Delta_Albedo"] <- NA
                                                        
                                                } else {
                                                        
                                                        #Data in Dec of previous year exists - grab this value
                                                        prev_alb <- final$Albedo[final$Region == reg &
                                                                                           final$LocalityName == loc &
                                                                                           final$LocalityCode == plot &
                                                                                           final$Subplot == subplot &
                                                                                           final$Treatment == tr &
                                                                                           final$Years_Since_Exclosure == (yse - 1) &
                                                                                           final$Month == 12]
                                                        #Calculate delta albedo
                                                        d_alb <- curr_alb - prev_alb
                                                        
                                                        #Error catch (some subplots are missing data)
                                                        if ( length(d_alb) == 0 ){
                                                                d_alb <- NA
                                                        }
                                                        
                                                        #Add to df
                                                        final[i, "Delta_Albedo"] <- d_alb
                                                        
                                                }
                                        
                                        
                                }
                        
                        
                }
                
                beep(8)
                
                        #NOW, for every plot, there should be delta albedo values (from the previous month), 
                        #EXCEPT for the first month of the first year of data (since we don't know the 
                        #starting albedo)
                
                        #NOTE: some subplots are missing data - these have been added as NAs
                
                
        #STEP 3 (Biomass for each subplot) --------
                
                #Add placeholder columns
                final$Total_Biomass_kg <- as.numeric('')
                final$Total_Biomass_kg_m2 <- as.numeric('')

                #Add corresponding biomass to each row
                #NOTE: biomass values will be repeated 12x (once for each month of albedo)
                for(i in 1:nrow(final)){
                        
                        #Get variables
                        reg <- final[i, "Region"]
                        loc <- final[i, "LocalityName"]
                        plot <- final[i, "LocalityCode"]
                        subplot <- final[i, "Subplot"]
                        tr <- final[i, "Treatment"]
                        yse <- final[i, "Years_Since_Exclosure"]
                        
                        #Grab biomass where variables match
                        
                                #kg/m2
                                bio_1 <- subplot_df$Total_Biomass_kg[subplot_df$Region == reg &
                                                                             subplot_df$LocalityName == loc &
                                                                             subplot_df$LocalityCode == plot &
                                                                             subplot_df$Plot == subplot &
                                                                             subplot_df$Treatment == tr &
                                                                             subplot_df$Years_Since_Exclosure == yse]
                                
                        
                                #kg/m2
                                bio_2 <- subplot_df$Total_Biomass_kg_m2[subplot_df$Region == reg &
                                                                                          subplot_df$LocalityName == loc &
                                                                                          subplot_df$LocalityCode == plot &
                                                                                          subplot_df$Plot == subplot &
                                                                                          subplot_df$Treatment == tr &
                                                                                          subplot_df$Years_Since_Exclosure == yse]
                                
                                
                        #Add biomass values to final dataframe
                        final[i, "Total_Biomass_kg"] <- bio_1
                        final[i, "Total_Biomass_kg_m2"] <- bio_2
                        
                }
                
                beep(8)
                
                
                #NOTE: REMOVING FIRST YEAR OF DRANGEDAL3 DATA (WHERE YSE == 1)
                #Only "UB" data available in year 1, which doesn't allow for delta calculation
                final <- final[!(final$LocalityName == 'drangedal3' & final$Years_Since_Exclosure == 1),]
                
                
        #STEP 4 (Delta biomass for each plot) --------
                
                #Add placeholder column for delta biomass values
                final$Delta_Biomass_kg <- as.numeric('')
                final$Delta_Biomass_kg_m2 <- as.numeric('')

                
                #For each plot, calculate annual delta biomass
                #NOTE: plots will have a delta biomass of NA in first year of available data
                #(since we don't know starting biomass)
                for(i in 1:nrow(final)){
                        
                        #Get variables
                        reg <- final[i, "Region"]
                        loc <- final[i, "LocalityName"]
                        plot <- final[i, "LocalityCode"]
                        subplot <- final[i, "Subplot"]
                        tr <- final[i, "Treatment"]
                        yse <- final[i, "Years_Since_Exclosure"]
                        
                        #Get min year of data for current LocalityCode
                        min_yr <- min(final$Years_Since_Exclosure[final$LocalityCode == plot])
                        
                        #Get current biomass values
                        
                                #kg
                                curr_bio_1 <- final[i, "Total_Biomass_kg"]
                                
                                #kg/m2
                                curr_bio_2 <- final[i, "Total_Biomass_kg_m2"]
                                
                        
                        #If first year of available data, delta biomass is NA
                        if( yse - min_yr == 0 ){
                                
                                #No prior biomass to compute delta biomass with
                                final[i, "Delta_Biomass_kg"] <- NA
                                final[i, "Delta_Biomass_kg_m2"] <- NA

                                
                        } else {
                                
                                #Data in Dec of previous year exists - grab this value
                                
                                        #kg
                                        prev_bio_1 <- final$Total_Biomass_kg[final$Region == reg &
                                                                                   final$LocalityName == loc &
                                                                                   final$LocalityCode == plot &
                                                                                   final$Subplot == subplot &
                                                                                   final$Treatment == tr &
                                                                                   final$Years_Since_Exclosure == (yse - 1)][1]
                                
                                        #kg/m2
                                        prev_bio_2 <- final$Total_Biomass_kg_m2[final$Region == reg &
                                                                                            final$LocalityName == loc &
                                                                                            final$LocalityCode == plot &
                                                                                            final$Subplot == subplot &
                                                                                            final$Treatment == tr &
                                                                                            final$Years_Since_Exclosure == (yse - 1)][1]
                                
                                #Calculate delta biomass values
                                        
                                        #kg
                                        d_bio_1 <- curr_bio_1 - prev_bio_1
                                        
                                                #Error catch (for missing data)
                                                if(length(d_bio_1) == 0){
                                                       d_bio_1 <- NA 
                                                }
                                        
                                        #kg/m2
                                        d_bio_2 <- curr_bio_2 - prev_bio_2
                                        
                                                #Error catch (for missing data)
                                                if(length(d_bio_2) == 0){
                                                        d_bio_2 <- NA 
                                                }
                                
                                #Add to df
                                final[i, "Delta_Biomass_kg"] <- d_bio_1
                                final[i, "Delta_Biomass_kg_m2"] <- d_bio_2
                                
                        }
                        
                }
                
                beep(8)
                
                                
        #WRITE FINAL CSV OF SUBPLOT ESTIMATES --------
                
                #Write CSV
                write.csv(final, "1_Albedo_Exclosures/1_Data_Processing/6_Radiative_Forcing/Output/subplot_delta_biomass_albedo.csv")                       
                

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
                        
   
       