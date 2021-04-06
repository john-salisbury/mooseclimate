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




#CREATE PLOT-LEVEL AVERAGES OF TOTAL BIOMASS ------------------------------------------------------------------
        
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
        
#END CREATE PLOT-LEVEL AVERAGES OF TOTAL BIOMASS --------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#CREATE PLOT-LEVEL AVERAGES OF DECIDUOUS BIOMASS ------------------------------------------------------------------
        
        #(1) Sum deciduous biomass (g) for EACH SUBPLOT (doing this first to calculate kg/m2 biomass for each subplot
        #    will take an average of subplot biomass later)
        
                #Get vector of all taxa
                all_taxa <- levels(as.factor(biomass$Taxa))
        
                #Isolate to deciduous taxa
                decid_taxa <- all_taxa[c(1,2,6,7)]
        
                #Isolate 'deciduous' trees to aggregate
                decid <- biomass[biomass$Taxa %in% decid_taxa,]
        
                #Sum biomass in each subplot
                subplot_decid <- aggregate(decid$Subplot_Total_Biomass_g, by = list("Region" = decid$Region,
                                                                                   "LocalityName" = decid$LocalityName,
                                                                                   "LocalityCode" = decid$LocalityCode,
                                                                                   "Treatment" = decid$Treatment,
                                                                                   "Plot" = decid$Plot,
                                                                                   "Years_Since_Exclosure" = decid$Years_Since_Exclosure), FUN = sum)
        
                colnames(subplot_decid)[7] <- "Total_Decid_Biomass_kg"
        
                #Convert summed biomass g to kg
                subplot_decid$Total_Decid_Biomass_kg <- subplot_decid$Total_Decid_Biomass_kg / 1000
        
                #Convert biomass to kg/m2
        
                        #Each circular subplot has a radius of 2m - A = pi*r^2
        
                                #Square meters
                                subplot_area <- pi*(2^2) #12.57m2
        
                        #Divide each value for kg by plot_area
        
                        #Square meters
                        subplot_decid$Total_Decid_Biomass_kg_m2 <- subplot_decid$Total_Decid_Biomass_kg / subplot_area 
        
                                #Now we have TOTAL DECIDUOUS BIOMASS per unit area (both in kg/m2) for EACH CIRCULAR
                                #SUBPLOT in a given plot
        
                                #We can then take the average of biomass within each subplot for each PLOT
        
        #(2) Get average deciduous biomass (kg & kg/m2) for each PLOT (take mean of all subplots in a given plot)
        
                #Aggregate means by plot
        
                        #kg
                        biomass_d_plot_means <- aggregate(subplot_decid$Total_Decid_Biomass_kg, by = list("Region" = subplot_decid$Region,
                                                                                               "LocalityName" = subplot_decid$LocalityName,
                                                                                               "LocalityCode" = subplot_decid$LocalityCode,
                                                                                               "Treatment" = subplot_decid$Treatment,
                                                                                               "Years_Since_Exclosure" = subplot_decid$Years_Since_Exclosure), FUN = mean)
                        colnames(biomass_d_plot_means)[6] <- "Mean_Plot_Decid_Biomass_kg"
        
                        #kg/m2
                        biomass_d_plot_means_m2 <- aggregate(subplot_decid$Total_Decid_Biomass_kg_m2, by = list("Region" = subplot_decid$Region,
                                                                                                     "LocalityName" = subplot_decid$LocalityName,
                                                                                                     "LocalityCode" = subplot_decid$LocalityCode,
                                                                                                     "Treatment" = subplot_decid$Treatment,
                                                                                                     "Years_Since_Exclosure" = subplot_decid$Years_Since_Exclosure), FUN = mean)
                        colnames(biomass_d_plot_means_m2)[6] <- "Mean_Plot_Decid_Biomass_kg_m2"
                        
                #Join columns into one df
                biomass_d_plot_means <- cbind(biomass_d_plot_means, biomass_d_plot_means_m2$Mean_Plot_Decid_Biomass_kg_m2)
                colnames(biomass_d_plot_means)[7] <- "Mean_Plot_Decid_Biomass_kg_m2"
        
        #Fix "stangeskovene eidskog " space issue
        biomass_d_plot_means$LocalityName[biomass_d_plot_means$LocalityName == 'stangeskovene eidskog '] <- 'stangeskovene_eidskog'
        
        
        #WRITE CSV OF PLOT-LEVEL BIOMASS
        write.csv(biomass_d_plot_means, '1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_decid_biomass.csv')
        
        
#END CREATE PLOT-LEVEL AVERAGES OF DECIDUOUS BIOMASS ------------------------------------------------------------------        
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#CREATE PLOT-LEVEL AVERAGES OF PINE BIOMASS ------------------------------------------------------------------
        
        #(1) Sum pine biomass (g) for EACH SUBPLOT (doing this first to calculate kg/m2 biomass for each subplot
        #    will take an average of subplot biomass later)
        
                #Isolate to deciduous taxa
                pine_taxa <- all_taxa[5]
                
                #Isolate pine trees to aggregate
                pine <- biomass[biomass$Taxa %in% pine_taxa,]
                
                #Sum biomass in each subplot
                subplot_pine <- aggregate(pine$Subplot_Total_Biomass_g, by = list("Region" = pine$Region,
                                                                                    "LocalityName" = pine$LocalityName,
                                                                                    "LocalityCode" = pine$LocalityCode,
                                                                                    "Treatment" = pine$Treatment,
                                                                                    "Plot" = pine$Plot,
                                                                                    "Years_Since_Exclosure" = pine$Years_Since_Exclosure), FUN = sum)
                
                colnames(subplot_pine)[7] <- "Total_Pine_Biomass_kg"
                
                #Convert summed biomass g to kg
                subplot_pine$Total_Pine_Biomass_kg <- subplot_pine$Total_Pine_Biomass_kg / 1000
                
                #Convert biomass to kg/m2
        
                        #Each circular subplot has a radius of 2m - A = pi*r^2
                        
                                #Square meters
                                subplot_area <- pi*(2^2) #12.57m2
                        
                        #Divide each value for kg by plot_area
                        
                                #Square meters
                                subplot_pine$Total_Pine_Biomass_kg_m2 <- subplot_pine$Total_Pine_Biomass_kg / subplot_area 
                        
                                #Now we have TOTAL PINE BIOMASS per unit area (both in kg/m2) for EACH CIRCULAR
                                #SUBPLOT in a given plot
        
                                #We can then take the average of biomass within each subplot for each PLOT
        
        #(2) Get average pine biomass (kg & kg/m2) for each PLOT (take mean of all subplots in a given plot)
        
                #Aggregate means by plot
                
                        #kg
                        biomass_p_plot_means <- aggregate(subplot_pine$Total_Pine_Biomass_kg, by = list("Region" = subplot_pine$Region,
                                                                                                          "LocalityName" = subplot_pine$LocalityName,
                                                                                                          "LocalityCode" = subplot_pine$LocalityCode,
                                                                                                          "Treatment" = subplot_pine$Treatment,
                                                                                                          "Years_Since_Exclosure" = subplot_pine$Years_Since_Exclosure), FUN = mean)
                        colnames(biomass_p_plot_means)[6] <- "Mean_Plot_Pine_Biomass_kg"
                        
                        #kg/m2
                        biomass_p_plot_means_m2 <- aggregate(subplot_pine$Total_Pine_Biomass_kg_m2, by = list("Region" = subplot_pine$Region,
                                                                                                                "LocalityName" = subplot_pine$LocalityName,
                                                                                                                "LocalityCode" = subplot_pine$LocalityCode,
                                                                                                                "Treatment" = subplot_pine$Treatment,
                                                                                                                "Years_Since_Exclosure" = subplot_pine$Years_Since_Exclosure), FUN = mean)
                        colnames(biomass_p_plot_means_m2)[6] <- "Mean_Plot_Pine_Biomass_kg_m2"
        
                #Join columns into one df
                biomass_p_plot_means <- cbind(biomass_p_plot_means, biomass_p_plot_means_m2$Mean_Plot_Pine_Biomass_kg_m2)
                colnames(biomass_p_plot_means)[7] <- "Mean_Plot_Pine_Biomass_kg_m2"
                
                #Fix "stangeskovene eidskog " space issue
                biomass_p_plot_means$LocalityName[biomass_p_plot_means$LocalityName == 'stangeskovene eidskog '] <- 'stangeskovene_eidskog'
                
                
        #WRITE CSV OF PLOT-LEVEL BIOMASS
        write.csv(biomass_p_plot_means, '1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_pine_biomass.csv')
        
        
#END CREATE PLOT-LEVEL AVERAGES OF PINE BIOMASS ------------------------------------------------------------------        
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#CREATE PLOT-LEVEL AVERAGES OF SPRUCE BIOMASS ------------------------------------------------------------------
        
        #(1) Sum spruce biomass (g) for EACH SUBPLOT (doing this first to calculate kg/m2 biomass for each subplot
        #    will take an average of subplot biomass later)
        
                #Isolate to deciduous taxa
                spruce_taxa <- all_taxa[3:4]
                
                #Isolate spruce trees to aggregate
                spruce <- biomass[biomass$Taxa %in% spruce_taxa,]
                
                #Sum biomass in each subplot
                subplot_spruce <- aggregate(spruce$Subplot_Total_Biomass_g, by = list("Region" = spruce$Region,
                                                                                  "LocalityName" = spruce$LocalityName,
                                                                                  "LocalityCode" = spruce$LocalityCode,
                                                                                  "Treatment" = spruce$Treatment,
                                                                                  "Plot" = spruce$Plot,
                                                                                  "Years_Since_Exclosure" = spruce$Years_Since_Exclosure), FUN = sum)
                
                colnames(subplot_spruce)[7] <- "Total_Spruce_Biomass_kg"
        
                #Convert summed biomass g to kg
                subplot_spruce$Total_Spruce_Biomass_kg <- subplot_spruce$Total_Spruce_Biomass_kg / 1000
        
                #Convert biomass to kg/m2
        
                        #Each circular subplot has a radius of 2m - A = pi*r^2
                
                                #Square meters
                                subplot_area <- pi*(2^2) #12.57m2
        
                        #Divide each value for kg by plot_area
        
                                #Square meters
                                subplot_spruce$Total_Spruce_Biomass_kg_m2 <- subplot_spruce$Total_Spruce_Biomass_kg / subplot_area 
        
                                #Now we have TOTAL SPRUCE BIOMASS per unit area (both in kg/m2) for EACH CIRCULAR
                                #SUBPLOT in a given plot
                                
                                #We can then take the average of biomass within each subplot for each PLOT
        
        #(2) Get average spruce biomass (kg & kg/m2) for each PLOT (take mean of all subplots in a given plot)
        
                #Aggregate means by plot
                
                #kg
                biomass_s_plot_means <- aggregate(subplot_spruce$Total_Spruce_Biomass_kg, by = list("Region" = subplot_spruce$Region,
                                                                                                "LocalityName" = subplot_spruce$LocalityName,
                                                                                                "LocalityCode" = subplot_spruce$LocalityCode,
                                                                                                "Treatment" = subplot_spruce$Treatment,
                                                                                                "Years_Since_Exclosure" = subplot_spruce$Years_Since_Exclosure), FUN = mean)
                colnames(biomass_s_plot_means)[6] <- "Mean_Plot_Spruce_Biomass_kg"
                
                #kg/m2
                biomass_s_plot_means_m2 <- aggregate(subplot_spruce$Total_Spruce_Biomass_kg_m2, by = list("Region" = subplot_spruce$Region,
                                                                                                      "LocalityName" = subplot_spruce$LocalityName,
                                                                                                      "LocalityCode" = subplot_spruce$LocalityCode,
                                                                                                      "Treatment" = subplot_spruce$Treatment,
                                                                                                      "Years_Since_Exclosure" = subplot_spruce$Years_Since_Exclosure), FUN = mean)
                colnames(biomass_s_plot_means_m2)[6] <- "Mean_Plot_Spruce_Biomass_kg_m2"
        
                #Join columns into one df
                biomass_s_plot_means <- cbind(biomass_s_plot_means, biomass_s_plot_means_m2$Mean_Plot_Spruce_Biomass_kg_m2)
                colnames(biomass_s_plot_means)[7] <- "Mean_Plot_Spruce_Biomass_kg_m2"
                
                #Fix "stangeskovene eidskog " space issue
                biomass_s_plot_means$LocalityName[biomass_s_plot_means$LocalityName == 'stangeskovene eidskog '] <- 'stangeskovene_eidskog'
                
        
        #WRITE CSV OF PLOT-LEVEL BIOMASS
        write.csv(biomass_s_plot_means, '1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_spruce_biomass.csv')
        
        
#END CREATE PLOT-LEVEL AVERAGES OF SPRUCE BIOMASS ------------------------------------------------------------------        
        