##Script to provide proportions of the 3 classes of trees used in the albedo model (spruce, pine, and birch/deciduous)
##NOTE: These proportions are on the PLOT LEVEL (i.e. LocalityCode) - NOT on the subplot level

##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for general data manipulation + visualization
        library(ggplot2)
        library(dplyr)
        library(RColorBrewer)
        library(wesanderson)

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
        
        #Load site data
        site_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/cleaned_data.csv', header = TRUE)
        

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
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#GENERATE PLOTS OF PROPORTIONS --------------------------------------------------------------------------
        
        #Start here if loading data
        final <- read.csv(file = '1_Albedo_Exclosures/z_Data_Library/Tree_Data/Usable_Data/tree_species_proportions_subplot_level.csv', header = TRUE)
        
        #FORMAT FOR PLOTTING -----
        
                #Add region column
                        
                        #Placeholder
                        final$Region <- as.character('')
                        
                        #Loop through
                        for(i in 1:nrow(final)){
                                site <- final[i, "LocalityCode"]
                                reg <- data$Region[data$LocalityCode == site][1]
                                final[i, "Region"] <- reg
                        }
        
                #Add 'Years Since Exclosure' & Treatment variables
        
                        #Filter to 'used sites' (i.e. LocalityCodes present in site_data)
                        used_sites <- levels(as.factor(site_data$LocalityCode))
                        final <- final[final$LocalityCode%in% used_sites,]
                        
                        #Add placeholder columns
                        final$Years_Since_Exclosure <- as.integer('')
                        final$Treatment <- as.character('')
                        
                        #Loop through
                        for(i in 1:nrow(final)){
                                
                                #Get site
                                loc <- final[i, "LocalityCode"]
                                
                                #Get year of row i
                                yr <- final[i, "Year"]
                                
                                #Get starting year of site (from site_data)
                                start <- site_data$Year.initiated[site_data$LocalityCode == loc]
                                
                                #Calculate YSE
                                yse <- yr - start
                                
                                #Get treatment of LocalityCode
                                tr <- site_data$Treatment[site_data$LocalityCode == loc]
                                
                                #Add to df
                                final[i, "Years_Since_Exclosure"] <- yse
                                final[i, "Treatment"] <- tr
                                
                        }
                        
                        #Clean up treatment name
                        final$Treatment[final$Treatment == "open"] <- "Browsed"
                        final$Treatment[final$Treatment == "exclosure"] <- "Unbrowsed"
                        
                #Add treatment variable
        
                #Expand dataset (species column and prop column instead of 3 prop columns)
        
                        #Placeholder columns
                        final$Prop <- as.numeric('')
                        final$Species <- as.character('')
                        
                        #Multiply each row x3 (once for each prop)
                        final <- final[rep(seq_len(nrow(final)), each = 3), ]

                        #Loop through sites and process
                        for(i in 1:length(sites)){
                                
                                print(i)
                                
                                #Get site
                                site <- sites[i]
                                
                                #Get min YSE
                                min_yr <- min(final$Years_Since_Exclosure[final$LocalityCode == site])
                                
                                #Get max YSE
                                max_yr <- max(final$Years_Since_Exclosure[final$LocalityCode == site])
                                
                                #loop through min/max years
                                for(j in min_yr:max_yr){
                                        
                                        plots <- levels(as.factor(final$Plot[final$LocalityCode == site & final$Years_Since_Exclosure == j]))
                                        
                                        for(k in 1:length(plots)){
                                                
                                                plot <- plots[k]
                                                
                                                #Add Spruce prop & label
                                                final$Species[final$LocalityCode == site & final$Years_Since_Exclosure == j & final$Plot == plot][1] <- "Spruce"
                                                final$Prop[final$LocalityCode == site & final$Years_Since_Exclosure == j & final$Plot == plot][1] <- final$Prop_spruce[final$LocalityCode == site & final$Years_Since_Exclosure == j & final$Plot == plot][1]
                                                
                                                #Add Pine prop & label
                                                final$Species[final$LocalityCode == site & final$Years_Since_Exclosure == j & final$Plot == plot][2] <- "Pine"
                                                final$Prop[final$LocalityCode == site & final$Years_Since_Exclosure == j & final$Plot == plot][2] <- final$Prop_pine[final$LocalityCode == site & final$Years_Since_Exclosure == j & final$Plot == plot][2]
                                                
                                                #Add Spruce prop & label
                                                final$Species[final$LocalityCode == site & final$Years_Since_Exclosure == j & final$Plot == plot][3] <- "Deciduous"
                                                final$Prop[final$LocalityCode == site & final$Years_Since_Exclosure == j & final$Plot == plot][3] <- final$Prop_birch[final$LocalityCode == site & final$Years_Since_Exclosure == j & final$Plot == plot][3]
                                        }
                                        
                                }
                                
                        }
                        
        #AVG SPECIES PROPORTIONS BY REGION & TREATMENT -----
        
                reg_prop <- aggregate(final$Prop, by = list("Region" = final$Region,
                                                            "Treatment" = final$Treatment,
                                                            "Years_Since_Exclosure" = final$Years_Since_Exclosure,
                                                            "Species" = final$Species), FUN = mean)
                colnames(reg_prop)[5] <- "Mean_Prop"
                
                #Remove Hedmark @ 2 YSE, since only 2 sites are in this year
                reg_prop <- reg_prop[!(reg_prop$Region == "Hedmark" & reg_prop$Years_Since_Exclosure == 2),]
                
                #Calculate SE for each mean value
                
                        #Define function
                        std <- function(x) sd(x)/sqrt(length(x))
                        
                        #Add placeholder columns
                        reg_prop$SE <- as.numeric('')
                        
                        #Calculate SEs for each species/group, month, and year
                        for(i in 1:nrow(reg_prop)){
                                
                                #Get variables
                                reg <- reg_prop[i, "Region"]
                                tr <- reg_prop[i, "Treatment"]
                                yse <- reg_prop[i, "Years_Since_Exclosure"]
                                gr <- reg_prop[i, "Species"]
                                
                                #Calculate SE for albedo
                                
                                se <- std(final$Prop[final$Region == reg &
                                                             final$Treatment == tr &
                                                             final$Species == gr &
                                                             final$Years_Since_Exclosure == yse])
                                
                                #Add to df
                                reg_prop[i, "SE"] <- se
                                
                        }

                #Palette
                pal <- wes_palette("Darjeeling1")
                        
                ggplot(data = reg_prop, aes(x = Years_Since_Exclosure, y = Mean_Prop, color = Species, fill = Species, linetype = Treatment)) +
                        geom_ribbon(aes(ymin = (Mean_Prop - SE), ymax = (Mean_Prop + SE), fill = Species), alpha = 0.15, lwd = 0) +
                        geom_point() +
                        geom_line() +
                        facet_wrap(~Region, ncol = 3) +
                        labs(x = "Years Since Exclosure", y = "Mean Proportion") +
                        scale_x_continuous(breaks = c(2,4,6,8,10)) +
                        scale_color_manual(values = pal) +
                        scale_fill_manual(values = pal) +
                        theme_bw() +
                        theme(
                                legend.position = "bottom",
                                axis.title.x = element_text(margin = margin(t = 4)),
                                axis.title.y = element_text(margin = margin(r = 4))
                                
                        )

                
                
        #AVG SPECIES PROPORTIONS BY SITE & TREATMENT -----
                
        
        
#END WRITE TO OUTPUT ----------------------------------------------------------------------
        