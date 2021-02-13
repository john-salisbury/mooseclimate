## Script to provide proportions of the 3 'classes' of trees used in the albedo model (spruce, pine,
## and birch/deciduous). These proportions are necessary to compute a weighted average and 'composite'
## albedo value for each subplot.

## NOTE: These proportions are on the SUBPLOT LEVEL - NOT on the plot level

##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for general data manipulation + visualization
        library(ggplot2)
        library(dplyr)

###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT + FILTERING --------------------------------------------------------------------------

        #Import SustHerb tree density data
        data <- read.csv('1_Albedo_Exclosures/z_Data_Library/Tree_Data/Usable_Data/sustherb_tree_data.csv', header = TRUE)
        
        #Remove rows that aren't identified or have no trees present
        data <- data[data$Taxa != 'Not identified (Ukjent)' & data$Taxa != 'No occurrence (Ingen)' & data$Quantity != 0,]
        data$Taxa <- as.factor(as.character(data$Taxa))
        
        #Convert date column to 'date' class
        data$X_Date <- as.Date(data$X_Date, format = "%m/%d/%y")
        
        #Remove trailing space in 'Stangeskovene Eidskog' LocalityName
        data$LocalityName <- as.character(data$LocalityName)
        data$LocalityName[data$LocalityName == 'Stangeskovene Eidskog '] <- 'Stangeskovene Eidskog'

#END INITIAL DATA IMPORT + FILTERING ----------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#CALCULATE SPECIES PROPORTIONS BY YEAR -----------------------------------------------------------------------
        
        #Add a year column to make aggregation step below easier
                
                #Add a blank placeholder column
                data$Year <- ''
        
                #For each row, add year to 'Year' column
                for(i in 1:nrow(data)){
                        data[i, "Year"] <- format(data[i, "X_Date"], "%Y")
                }
        
        #Aggregate at subplot level (within each plot/LocalityCode)
        agg <- aggregate(data$Quantity, by = list(Region = data$Region, LocalityName = data$LocalityName, LocalityCode = data$LocalityCode, Plot = data$Plot, Year = data$Year, Treatment = data$Treatment, Taxa = data$Taxa), FUN = sum)
        names(agg)[8] <- "Quantity"
        
        #Calculate species proportions in new dataframe
        
                #Get list of site codes in a vector
                agg$LocalityCode <- as.factor(agg$LocalityCode)
                site_codes <- levels(agg$LocalityCode)
                
                #Create placeholder column for totals by site and year
                agg$Total_trees_in_subplot <- ''
                
                #Temp df
                totals <- agg[0,]
                
                #For each subplot within each plot (for each year), calculate total # of trees
                for(i in 1:length(site_codes)){
                        
                        print(site_codes[i])
                        
                        #Get min and max years for study site 'i'
                        min_year <- min(agg$Year[agg$LocalityCode == site_codes[i]])
                        max_year <- max(agg$Year[agg$LocalityCode == site_codes[i]])
                        
                        #For each year, sum total trees and add to 'totals' column
                        for(j in min_year:max_year){
                                
                                print(j)
                                
                                #Get df for all subplots at site i in year j
                                temp <- agg[agg$LocalityCode == as.character(site_codes[i]) & agg$Year == j,]
                                
                                #Get all subplots present at site i in year j
                                subs <- levels(as.factor(temp$Plot))
                                
                                #Loop through subplots
                                for(k in 1:length(subs)){
                                        
                                        #Add total trees for subplot k
                                        temp$Total_trees_in_subplot[temp$Plot == as.character(subs[k])] <- sum(temp$Quantity[temp$Plot == as.character(subs[k])])
                                        
                                }
                                
                                #Bind to temp df
                                totals <- rbind(totals, temp)

                        }
                        
                }
                
                #Ensure 'Total trees' column is numeric
                totals$Total_trees_in_subplot <- as.numeric(totals$Total_trees_in_subplot)
                
                #Create new dataframe to hold final proportions for 3 'classes' of trees used in albedo model
                #Note: this is on a "subplot" level
                final <- data.frame("LocalityName" = character(),
                                    "LocalityCode" = character(),
                                    "Plot" = character(),
                                    "Year" = integer(),
                                    "Prop_spruce" = numeric(),
                                    "Prop_pine" = numeric(),
                                    "Prop_birch" = numeric())
        
                #For each site code (for each year), calculate proportions of each class of tree
                for(i in 1:length(site_codes)){
                        
                        print(site_codes[i])
                        
                        #Get min and max years for study site 'i'
                        min_year <- min(agg$Year[agg$LocalityCode == site_codes[i]])
                        max_year <- max(agg$Year[agg$LocalityCode == site_codes[i]])
                        
                        #Get LocalityName
                        local <- totals$LocalityName[totals$LocalityCode == site_codes[i]][1]
                        
                        #Loop through years j at site i
                        for(j in min_year:max_year){
                                
                                print(j)
                                
                                #Get df for all subplots at site i in year j
                                temp <- totals[totals$LocalityCode == as.character(site_codes[i]) & totals$Year == j,]
                                
                                #Get all subplots present at site i in year j
                                subs <- levels(as.factor(temp$Plot))
                                
                                #Loop through subplots
                                for(k in 1:length(subs)){
                                        
                                        #Spruce proportion (NOTE: Includes Juniperus communis)
                                        spruce_prop <- sum(temp$Quantity[temp$Plot == subs[k] & (temp$Taxa == "Picea abies (Gran)" | temp$Taxa == "Juniperus communis (Einer)")]) / temp$Total_trees_in_subplot[temp$Plot == subs[k]][1]

                                        #Pine proportion
                                        pine_prop <- sum(temp$Quantity[temp$Plot == subs[k] & temp$Taxa == "Pinus sylvestris (Furu)"]) / temp$Total_trees_in_subplot[temp$Plot == subs[k]][1]
                                        
                                        #Birch/Deciduous proportion (i.e. all other species)
                                        decid_prop <- sum(temp$Quantity[temp$Plot == subs[k] & temp$Taxa != "Picea abies (Gran)" & temp$Taxa != "Juniperus communis (Einer)" & temp$Taxa != "Pinus sylvestris (Furu)"]) / temp$Total_trees_in_subplot[temp$Plot == subs[k]][1]
                                        
                                        #Create temp row
                                        row <- data.frame("LocalityName" = local,
                                                          "LocalityCode" = site_codes[i],
                                                          "Plot" = subs[k],
                                                          "Year" = j,
                                                          "Prop_spruce" = spruce_prop,
                                                          "Prop_pine" = pine_prop,
                                                          "Prop_birch" = decid_prop)
                                        
                                        #Bind row to final df
                                        final <- rbind(final, row)
                                        
                                }
                                
                        }
                                
                                
                }

                
        #Final Data Cleanup
        final$Prop_spruce <- as.numeric(final$Prop_spruce)
        final$Prop_pine <- as.numeric(final$Prop_pine)                
        final$Prop_birch <- as.numeric(final$Prop_birch)

#END CALCULATE SPECIES PROPORTIONS BY YEAR ----------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#WRITE TO OUTPUT --------------------------------------------------------------------------
        
        #Write CSV to Output Folder
        write.csv(final, file = '1_Albedo_Exclosures/z_Data_Library/Tree_Data/Usable_Data/tree_species_proportions_subplot_level.csv', row.names = TRUE)
        
#END WRITE TO OUTPUT ----------------------------------------------------------------------
        