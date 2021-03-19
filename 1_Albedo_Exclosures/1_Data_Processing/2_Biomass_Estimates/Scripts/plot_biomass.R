## Script to calculate:
##  (1) Total biomass
##  (2) Species-specific biomass (pine, spruce, deciduous)
## for each PLOT (i.e. LocalityCode)

# Tree-specific biomass is summed for each subplot - subplot biomasses are then averaged to produce a 
# PLOT-level biomass

##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(wesanderson)
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        #Import tree-specific biomass (calculated in 'tree_biomass.R')
        biomass <- read.csv('1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/tree_biomass.csv', header = TRUE)

#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#CREATE PLOT-LEVEL AVERAGES ------------------------------------------------------------------
        
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
        
                        #Divide each value for kg by plot_area
        
                                #Square meters
                                subplot_df$Total_Biomass_kg_m2 <- subplot_df$Total_Biomass_kg / subplot_area 
        
                                        #Now we have TOTAL BIOMASS per unit area (both in kg/m2) for EACH CIRCULAR
                                        #SUBPLOT in a given plot
        
                                        #We can then take the average of biomass within each subplot for each PLOT
        
        #(2) Get average biomass (kg & kg/m2) for each PLOT (take mean of all subplots in a given plot)
        
                #Aggregate means by plot
        
                        #kg
                        biomass_plot_means <- aggregate(subplot_df$Total_Biomass_kg, by = list("Region" = subplot_df$Region,
                                                                                               "LocalityName" = subplot_df$LocalityName,
                                                                                               "LocalityCode" = subplot_df$LocalityCode,
                                                                                               "Treatment" = subplot_df$Treatment,
                                                                                               "Years_Since_Exclosure" = subplot_df$Years_Since_Exclosure), FUN = mean)
                        colnames(biomass_plot_means)[6] <- "Mean_Plot_Biomass_kg"
                        
                        #kg/m2
                        biomass_plot_means_m2 <- aggregate(subplot_df$Total_Biomass_kg_m2, by = list("Region" = subplot_df$Region,
                                                                                                     "LocalityName" = subplot_df$LocalityName,
                                                                                                     "LocalityCode" = subplot_df$LocalityCode,
                                                                                                     "Treatment" = subplot_df$Treatment,
                                                                                                     "Years_Since_Exclosure" = subplot_df$Years_Since_Exclosure), FUN = mean)
                        colnames(biomass_plot_means_m2)[6] <- "Mean_Plot_Biomass_kg_m2"
                        
                        #Join columns into one df
                        biomass_plot_means <- cbind(biomass_plot_means, biomass_plot_means_m2$Mean_Plot_Biomass_kg_m2)
                        colnames(biomass_plot_means)[7] <- "Mean_Plot_Biomass_kg_m2"
                        
                        #Fix "stangeskovene eidskog " space issue
                        biomass_plot_means$LocalityName[biomass_plot_means$LocalityName == 'stangeskovene eidskog '] <- 'stangeskovene_eidskog'
        
                        
        #WRITE CSV OF PLOT-LEVEL BIOMASS
        write.csv(biomass_plot_means, '1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_biomass.csv')
        
#END CREATE PLOT-LEVEL AVERAGES --------------------------------------------------------------
        