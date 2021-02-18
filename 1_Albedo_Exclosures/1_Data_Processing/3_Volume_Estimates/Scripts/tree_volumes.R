## Script to calculate (1) individual tree volumes (m3), and (2) tree volume by 'group' (deciduous, pine, spruce)
## for each subplot across all years of data.

## Note: this uses data for all 'used sites' (Trøndelag, Hedmark, and Telemark) through 2019.
## Volume is calculated using biomass estimates calculated from the backfitted 
## "height-only" allometric models and height class only

##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(grid)
        library(wesanderson)
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        #Import CSV to dataframe
        data <- read.csv('1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/tree_biomass.csv', header = TRUE)

#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#CALCULATE VOLUMES FOR EACH SUBPLOT ---------------------------------------------------
        
        #Calculate volume for each tree
        
                #SPECIFIC WOOD DENSITIES TO CONVERT BIOMASS TO VOLUME
                ##Note: These are average wood densities for spruce, pine, and birch - provided by Repola (2006)
                
                        #This will likely be a major source of error - according to Repola (2006), density varies widely vertically
                
                        #Spruce density (kg/m3 converted to g/cm3)
                        s_density <- 385.3 / 1000
                        
                        #Pine density (kg/m3 converted to g/cm3)
                        p_density <- 412.6 / 1000
                        
                        #Birch density (kg/m3 converted to g/cm3)
                        b_density <- 475 / 1000
                        
                #Based on species, calculate volume & assign to either birch, pine, or spruce
                        
                        #Add blank column for volume
                        data$Volume_m3 <- ''
                        
                        #Add blank column for group
                        data$Group <- ''
                        
                        #Birch -----
                        
                                #Calculate volume
                                data$Volume_m3[data$Taxa == "Betula pubescens (Bjørk)" |
                                                       data$Taxa == "Betula pendula (Lavlandbjørk)" |
                                                       data$Taxa == "Salix caprea (Selje)"] <- (data$Biomass_g[data$Taxa == "Betula pubescens (Bjørk)" |
                                                                                                                       data$Taxa == "Betula pendula (Lavlandbjørk)" |
                                                                                                                       data$Taxa == "Salix caprea (Selje)"] / b_density) / 1e06
                                #Assign group to birch
                                data$Group[data$Taxa == "Betula pubescens (Bjørk)" |
                                                       data$Taxa == "Betula pendula (Lavlandbjørk)" |
                                                       data$Taxa == "Salix caprea (Selje)"] <- "Birch"
                                
                        
                        #Spruce ------
                                
                                #Calculate volume
                                data$Volume_m3[data$Taxa == "Picea abies (Gran)" |
                                                data$Taxa == "Juniperus communis (Einer)"] <- (data$Biomass_g[data$Taxa == "Picea abies (Gran)" |
                                                                                                                      data$Taxa == "Juniperus communis (Einer)"] / s_density) / 1e06
                                
                                #Assign group to spruce
                                data$Group[data$Taxa == "Picea abies (Gran)" |
                                                       data$Taxa == "Juniperus communis (Einer)"] <- "Spruce"
                                
                                
                        #Pine -------
                                
                                #Calculate volume
                                data$Volume_m3[data$Taxa == "Pinus sylvestris (Furu)"] <- (data$Biomass_g[data$Taxa == "Pinus sylvestris (Furu)"] / p_density) / 1e06
                                
                                #Assign group to pine
                                data$Group[data$Taxa == "Pinus sylvestris (Furu)"] <- "Pine"
                                
                                
                        #Rowan (NOTE: using birch-specific density due to lack of rowan-specific density) ------
                                
                                #Calculate volume
                                data$Volume_m3[data$Taxa == "Sorbus aucuparia (Rogn)"] <- (data$Biomass_g[data$Taxa == "Sorbus aucuparia (Rogn)"] / b_density) / 1e06
                        
                                #Assign group
                                data$Group[data$Taxa == "Sorbus aucuparia (Rogn)"] <- "Birch"
        
                                
                #Ensure that 'Volume' column is numeric
                data$Volume_m3 <- as.numeric(data$Volume_m3)
                
                #Ensure that "Group' column is factor
                data$Group <- as.factor(data$Group)
                
        

        #Calculate plot sampling area in hectares (used to convert m3 to m3/ha) -------
                
                #Each subplot has a radius of 2m - A = pi*r^2
                subplot_area <- pi*(2^2) #12.57m2
                
                #Convert m2 to hectares (ha) - 1m2 = 0.0001 ha (divide by 10,000)
                subplot_area_ha <- subplot_area/10000
                
        
        #Aggregate volume (m3) for each species by subplots within main plots
                
                vol <- aggregate(data$Volume_m3, by = list(data$Region,
                                                                 data$LocalityName,
                                                                 data$LocalityCode,
                                                                 data$Treatment,
                                                                 data$Plot,
                                                                 data$Years_Since_Exclosure,
                                                                 data$Group), FUN = sum)
                
                colnames(vol) <- c("Region", "LocalityName", "LocalityCode", "Treatment", "Subplot", "Years_Since_Exclosure", "Group", "Volume_m3")
                
        #Create volume/area (m3/ha) column - divide m3 by subplot area 
                
                vol$Volume_m3ha <- vol$Volume_m3 / subplot_area_ha
                
                
#END CALCULATE VOLUMES FOR EACH SUBPLOT ---------------------------------------------------
                
                
                
                
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                
                
                
                
#CALCULATE MEAN VOLUMES & SE ------------------------------------------------------
                
        #Define SE function
        std <- function(x) sd(x)/sqrt(length(x))
                
                
                
        #TOTAL MEAN SUBPLOT VOLUME ------------
                
                #Aggregate means 
                tot_means <- aggregate(vol$Volume_m3ha, by = list("Treatment" = vol$Treatment,
                                                                  "Years_Since_Exclosure" = vol$Years_Since_Exclosure), FUN = mean)
                
                colnames(tot_means)[3] <- "Average_vol_m3_ha"
                
                #Calculate SE
                
                        #Add placeholder columns
                        tot_means$SE <- as.numeric('')
                        
                        #Calculate SEs for each year
                        
                        for(i in 1:nrow(tot_means)){
                                
                                #Get variables
                                tr <- tot_means[i, "Treatment"]
                                yr <- tot_means[i, "Years_Since_Exclosure"]
                                
                                #Calculate SE
                                se <- std(vol$Volume_m3ha[vol$Treatment == tr & vol$Years_Since_Exclosure == yr])
                                
                                #Add to df
                                tot_means[i, "SE"] <- se
                        }
                        
                        
        #MEAN SUBPLOT VOLUME BY GROUP (PINE, SPRUCE, DECIDUOUS) ---------
                        
                #Aggregate means
                group_means <- aggregate(vol$Volume_m3ha, by = list("Treatment" = vol$Treatment,
                                                                    "Years_Since_Exclosure" = vol$Years_Since_Exclosure,
                                                                    "Group" = vol$Group), FUN = mean)
                colnames(group_means)[4] <- "Average_vol_m3_ha"
                
                #Calculate SE
                
                        #Add placeholder columns
                        group_means$SE <- as.numeric('')
                
                        for(i in 1:nrow(group_means)){
                                
                                #Get variables
                                tr <- group_means[i, "Treatment"]
                                yr <- group_means[i, "Years_Since_Exclosure"]
                                gr <- as.character(group_means[i, "Group"])
                                
                                #Calculate SE
                                se <- std(vol$Volume_m3ha[vol$Treatment == tr &
                                                                  vol$Years_Since_Exclosure == yr &
                                                                  vol$Group == gr])
                                
                                #Add to df
                                group_means[i, "SE"] <- se
                        }
                
                        
        #TOTAL MEAN SUBPLOT VOLUME BY REGION
                        
                #Aggregate means
                tot_reg_means <- aggregate(vol$Volume_m3ha, by = list("Treatment" = vol$Treatment,
                                                                  "Years_Since_Exclosure" = vol$Years_Since_Exclosure,
                                                                  "Region" = vol$Region), FUN = mean)
                colnames(tot_reg_means)[4] <- "Average_vol_m3_ha"
                
                #Calculate SE
                
                        #Add placeholder columns
                        tot_reg_means$SE <- as.numeric('')
                        
                        for(i in 1:nrow(tot_reg_means)){
                                
                                #Get variables
                                tr <- tot_reg_means[i, "Treatment"]
                                yr <- tot_reg_means[i, "Years_Since_Exclosure"]
                                re <- tot_reg_means[i, "Region"]
                                
                                #Calculate SE
                                se <- std(vol$Volume_m3ha[vol$Treatment == tr &
                                                                  vol$Years_Since_Exclosure == yr &
                                                                  vol$Region == re])
                                
                                #Add to df
                                tot_reg_means[i, "SE"] <- se
                        }
                
                        
        
        #MEAN SUBPLOT VOLUME BY GROUP AND REGION -----------------------
                
                #Aggregate means
                reg_means <- aggregate(vol$Volume_m3ha, by = list("Treatment" = vol$Treatment,
                                                                  "Years_Since_Exclosure" = vol$Years_Since_Exclosure,
                                                                  "Group" = vol$Group,
                                                                  "Region" = vol$Region), FUN = mean)
                colnames(reg_means)[5] <- "Average_vol_m3_ha"
                        
                #Calculate SE
                        
                        #Add placeholder columns
                        reg_means$SE <- as.numeric('')
                        
                        for(i in 1:nrow(reg_means)){
                                
                                #Get variables
                                tr <- reg_means[i, "Treatment"]
                                yr <- reg_means[i, "Years_Since_Exclosure"]
                                gr <- as.character(reg_means[i, "Group"])
                                re <- reg_means[i, "Region"]
                                
                                #Calculate SE
                                se <- std(vol$Volume_m3ha[vol$Treatment == tr &
                                                                  vol$Years_Since_Exclosure == yr &
                                                                  vol$Group == gr &
                                                                  vol$Region == re])
                                
                                #Add to df
                                reg_means[i, "SE"] <- se
                        }
                        

                        
                        
                
#END CALCULATE MEAN VOLUMES & SE ---------------------------------------------------

        


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#DATA VISUALIZATION ------------------------------------------------------------------------------------------
        
        
        #Plots ------
        plot_count_label <- c("0", "1\nn=36", "2\nn=37", "3\nn=37", "4\nn=37", "5\nn=37", "6\nn=37", "7\nn=29", "8\nn=29", "9\nn=28", "10\nn=15")
                
                #MEAN SUBPLOT VOLUME (TOTAL VOLUME) BY TREATMENT --------
                        
                        ggplot(data = tot_means, aes(x = Years_Since_Exclosure, y = Average_vol_m3_ha, group = Treatment)) +
                                geom_errorbar(aes(ymin = (Average_vol_m3_ha - SE), ymax = (Average_vol_m3_ha + SE)), colour="black", width=0.5, position = position_dodge(0.3)) +
                                geom_point(aes(shape = Treatment), size = 1.75, position = position_dodge(0.3)) +
                                geom_line(aes(linetype = Treatment)) +
                                labs(x = "Years Since Exclosure", y = "Mean Subplot Volume "~(m^3/ha)) +
                                scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                                
                                
                #MEAN SUBPLOT VOLUME BY GROUP & TREATMENT -------
                        
                        ggplot(data = group_means, aes(x = Years_Since_Exclosure, y = Average_vol_m3_ha, group = Treatment)) +
                                geom_errorbar(aes(ymin = (Average_vol_m3_ha - SE), ymax = (Average_vol_m3_ha + SE)), colour="black", width=0.5, position = position_dodge(0.3)) +
                                geom_point(aes(shape = Treatment), size = 1.75, position = position_dodge(0.3)) +
                                geom_line(aes(linetype = Treatment)) +
                                facet_wrap(~Group, nrow = 3) +
                                labs(x = "Years Since Exclosure", y = "Mean Subplot Volume "~(m^3/ha)) +
                                scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                        
                        
                #MEAN SUBPLOT VOLUME BY REGION & TREATMENT -------
                
                ggplot(data = tot_reg_means, aes(x = Years_Since_Exclosure, y = Average_vol_m3_ha, group = Treatment)) +
                        geom_errorbar(aes(ymin = (Average_vol_m3_ha - SE), ymax = (Average_vol_m3_ha + SE)), colour="black", width=0.5, position = position_dodge(0.3)) +
                        geom_point(aes(shape = Treatment), size = 1.75, position = position_dodge(0.3)) +
                        geom_line(aes(linetype = Treatment)) +
                        facet_wrap(~Region, ncol = 1) +
                        labs(x = "Years Since Exclosure", y = "Mean Subplot Volume "~(m^3/ha)) +
                        scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
                        theme_bw() +
                        theme(
                                axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                axis.title.y = element_text(size = 12, margin = margin(r=10))
                        )
                        
                        
                #MEAN SUBPLOT VOLUME BY GROUP, REGION, & TREATMENT -------
                        
                        ggplot(data = reg_means, aes(x = Years_Since_Exclosure, y = Average_vol_m3_ha, group = Treatment)) +
                                geom_errorbar(aes(ymin = (Average_vol_m3_ha - SE), ymax = (Average_vol_m3_ha + SE)), colour="black", width=0.5, position = position_dodge(0.3)) +
                                geom_point(aes(shape = Treatment), size = 1.75, position = position_dodge(0.3)) +
                                geom_line(aes(linetype = Treatment)) +
                                facet_grid(Region~Group) +
                                labs(x = "Years Since Exclosure", y = "Mean Subplot Volume "~(m^3/ha)) +
                                scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                        

                
#END DATA VISUALIZATION ----------------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                
                
                
                
#EXPORT PLOTS --------------------------------------------------------------------------------------

        #WRITE CSVs
                        
                #Main Volumes Dataset
                write.csv(vol, "1_Albedo_Exclosures/1_Data_Processing/3_Volume_Estimates/Output/subplot_tree_volumes.csv")
                        

        

#END EXPORT PLOTS ---------------------------------------------------------------------------------- 
