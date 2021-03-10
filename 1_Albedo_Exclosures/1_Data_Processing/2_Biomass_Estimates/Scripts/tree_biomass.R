## Script to calculate tree biomass for all sites in all regions
## using the backfitted "height-only" allometric models

#NOTE: This script uses TWO SETS of allometric models:

        #One set for UNBROWSED trees (backfitted by Kolstad et al. (2018))
        #One set for BROWSED trees (which I backfitted)

#This script also uses HEIGHT CLASSES (not specific height in cm) to calculate biomass
#This was done to ensure consistency in biomass across years

##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(wesanderson)
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        #Import CSV to dataframe
        data <- read.csv('1_Albedo_Exclosures/z_Data_Library/Tree_Data/Usable_Data/sustherb_tree_data.csv', header = TRUE)

#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#DATA FILTERING ----------------------------------------------------------------------------------------------

        #Summary: 
        ##  The code below filters data to trees under 6m in height, for
        ##  use with the Trøndelag-specific height-only biomass models that have been backfit

        #Format date column properly (to R date format)
        data$X_Date <- as.Date(as.character(data$X_Date), format = "%m/%d/%y")
        
        #Format LocalityName and LocalityCode as factors
        data$LocalityCode <- as.factor(data$LocalityCode)
        data$LocalityName <- as.factor(data$LocalityName)
        
        #LocalityName to lowercase
        data$LocalityName <- tolower(data$LocalityName)
        
        #Remove rows that are missing height class data (necessary for biomass models)
        data <- data[! is.na(data$Height_class_50cm),] #1 removed

        #Exclude trees > 6m (600cm)
        data <- data %>% filter(Height_cm <= 600 | is.na(Height_cm))

        #Filter to "Used Sites" with available productivity data (n = 37)
        
                #Get 'cleaned' site data from adjacent 'Sites' folder
                site_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/cleaned_data.csv', header = TRUE)
                site_data$LocalityCode <- as.factor(site_data$LocalityCode)
                
                #Get vector of levels for sites that will be used (n = 74)
                used_sites <- levels(site_data$LocalityCode)
                
                #Standardize LocalityNames in "data" df
                
                        #Didrik Holmsen
                        data$LocalityName[data$LocalityName == "didrik holmsen"] <- "didrik_holmsen"
                        
                        #Fet 3
                        data$LocalityName[data$LocalityName == "fet 3"] <- "fet_3"
                        
                        #Fritsøe 1
                        data$LocalityName[data$LocalityName == "fritsøe1"] <- "fritsoe1"
                        
                        #Fritsøe 2
                        data$LocalityName[data$LocalityName == "fritsøe2"] <- "fritsoe2"
                        
                        #Halvard Pramhus
                        data$LocalityName[data$LocalityName == "halvard pramhus"] <- "halvard_pramhus"
                        
                        #Singsaas
                        data$LocalityName[data$LocalityName == "singsås"] <- "singsaas"
                        
                        #Stangeskovene Aurskog
                        data$LocalityName[data$LocalityName == "stangeskovene aurskog"] <- "stangeskovene_aurskog"
                        
                        #Stangeskovene Eidskog
                        data$LocalityName[data$LocalityName == "stangeskovene eidskog"] <- "stangeskovene_eidskog"
                        
                        #Stig Dahlen
                        data$LocalityName[data$LocalityName == "stig dæhlen"] <- "stig_dahlen"
                        
                        #Truls Holm
                        data$LocalityName[data$LocalityName == "truls holm"] <- "truls_holm"
                        
                #Filter to used sites
                data <- data[data$LocalityCode %in% used_sites,] #Brings us to 37 LocalityNames - looks good
                
        #Add year column
                
                #Set dates
                dates <- as.POSIXct(data$X_Date, format = "%m/%d/%Y %H:%M:%S")
                attr(dates, "tzone") <- "Europe/Oslo"
                
                        # Make sure to set timezone to CET here - was running into issues with measurements in Jan 2016 being considered
                        # as Dec 2015
                
                #Double-check this (DON'T USE as.POS)
                dates_ver <- data.frame("LocalityCode" = data$LocalityCode,
                                    "Original_Date" = data$X_Date,
                                    "Formatted_Date" = dates,
                                    "Final_Year" = format(dates, format="%Y"))
                
                dates <- format(dates, format="%Y")
                data$Year <- dates
                
                        #FIXED YEAR ISSUE MENTIONED ABOVE - DATES SHOULD NOW BE CORRECT
                

#END DATA FILTERING -------------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#BIOMASS CALCULATION FOR EACH SET OF TREES ---------------------------------------------------------------
        
        #Add a blank column to hold biomass
        data$Subplot_Total_Biomass_g <- as.numeric('')
        
        # Loop through each row of the dataframe (2016, Trøndelag, <6m height, browsed)
        # and produce a volume estimate
                
        #NOTE: Each row represents observations of a specific species in a specific height class in a specific subplot
        ## The 'quantity' variable indicates how many trees of a species in a height class in a subplot were observed
        ## in a given year. Thus, to calculate biomass/unit area, the height class is multiplied by the quantity
        
        for(i in 1:nrow(data)){
                
                print(i)
                
                #Grab necessary parameters
                
                        #Get tree species
                        species <- as.character(data[i, "Taxa"])
                
                        #Get height class (in 50cm increments)
                        h <- as.numeric(data[i, "Height_class_50cm"])
                        h <- h*50
                        
                        #Get treatment
                        treatment <- data[i, "Treatment"]
                
                #CALCULATE BIOMASS FOR EACH TREE (OR OBSERVATION OF MULTIPLE TREES) USING HEIGHT-ONLY ALLOMETRIC EQUATIONS
                
                        #Betula pubescens model (Birch)
                        if( species == "Betula pubescens (Bjørk)" | species == "Betula pendula (Lavlandbjørk)" | species == "Salix caprea (Selje)" ){
                                
                                if(treatment == "B"){
                                        
                                        y <- 0.2072*h + 0.009974*(h^2)
                                        
                                } else if(treatment == "UB"){
                                        
                                        y <- 0.170274*h + 0.010018*(h^2)
                                        
                                }
                                
                        }
                        
                        #Picea abies model (Spruce)
                        else if( species == "Picea abies (Gran)" | species == "Juniperus communis (Einer)"){
                                
                                if(treatment == "B"){
                                        
                                        y <- 0.02014*(h^2) + 0.00006086*(h^3)
                                        
                                } else if(treatment == "UB"){
                                        
                                        y <- 0.038068*(h^2)
                                        
                                }
                                
                        }
                        
                        #Pinus sylvestris model (Pine)
                        else if( species == "Pinus sylvestris (Furu)" ){
                                
                                if(treatment == "B"){
                                        
                                        y <- 0.027464*(h^2)
                                        
                                } else if(treatment == "UB"){
                                        
                                        y <- 0.0149667*(h^2)
                                        
                                }
                              
                        }
                        
                        #Sorbus aucuparia model (Rowan)
                        else if( species == "Sorbus aucuparia (Rogn)" ){
                                
                                if(treatment == "B"){
                                        
                                        y <- 0.2620264*h + 0.005844*(h^2)
                                        
                                } else if(treatment == "UB"){
                                        
                                        y <- 0.0053962*(h^2)
                                        
                                }       
        
                        } else {
                                
                                y <- NA
                                
                        }
                        
                        #IMPORTANT - MULTIPLY CALCULATED BIOMASS x QUANTITY IN HEIGHT CLASS OBSERVED
                        
                                #Get quantity
                                q <- data[i, "Quantity"]
                                final_bio <- y*q
                        
                        #Add calculated biomass to 'Biomass' column
                        data[i, "Subplot_Total_Biomass_g"] <- final_bio
                
        }
        
                
        #Add "Years Since Exclosure" variable
                
                #Prep columns
                data$Year <- as.numeric(data$Year)
                data$Years_Since_Exclosure <- as.numeric('')
                
                #Loop through all observations and add correct year since exclosure
                for(i in 1:nrow(data)){
                        
                        print(i)
                        loc <- as.character(data[i, "LocalityCode"])
                        start <- as.numeric(site_data$Year.initiated[site_data$LocalityCode == loc])
                        
                        data[i, "Years_Since_Exclosure"] <- data[i, "Year"] - start
                        
                }
                
                
        #Filter out observations w/ NA biomass & reset Taxa factor levels
                
                data <- data[!is.na(data$Subplot_Total_Biomass_g),]
                data$Taxa <- as.factor(as.character(data$Taxa))
                
                
                
        #DECISION MADE TO REMOVE TWO SITES FROM HEDMARK IN 2nd year of exclosure
        data <- data[!(data$Region == "Hedmark" & data$Years_Since_Exclosure == 2),]
                

        
#END BIOMASS CALCULATION USING EXISTING MODELS -----------------------------------------------------------
                
                
                
                
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#WRITE FINAL BIOMASS CSV --------------------------------------------------------------------------------------   
                
        #WRITE CSV W/ INDIVIDUAL TREE BIOMASS -------------
        write.csv(data, "1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/tree_biomass.csv")
                
#END WRITE FINAL BIOMASS CSV --------------------------------------------------------------------------------------        
                
                
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#EXPORT PLOTS --------------------------------------------------------------------------------------

        #Load data if starting here
        data <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/tree_biomass.csv", header = T)
        
        #TOTAL SUBPLOT BIOMASS BY TREATMENT --------------
                
                #Sum total biomass (g) for each plot
                plot_df <- aggregate(data$Subplot_Total_Biomass_g, by = list("Years_Since_Exclosure" = data$Years_Since_Exclosure,
                                                                               "Region" = data$Region,
                                                                               "LocalityName" = data$LocalityName,
                                                                               "Treatment" = data$Treatment,
                                                                               "Plot" = data$Plot), FUN = sum)
                
                colnames(plot_df)[6] <- "Summed_biomass_subplot_kg"
                
                #Convert summed biomass g to kg
                plot_df$Summed_biomass_subplot_kg <- plot_df$Summed_biomass_subplot_kg / 1000
                
                #Convert biomass to kg/m2
                
                        #Each circular subplot has a radius of 2m - A = pi*r^2
                        subplot_area <- pi*(2^2) #12.57m2
                        
                        #Divide each value for kg by plot_area
                        plot_df$Subplot_biomass_kg_m2 <- plot_df$Summed_biomass_subplot_kg / subplot_area 
                
                        
                                #Now we have TOTAL BIOMASS per unit area (kg/m2) for EACH CIRCULAR
                                #SUBPLOT in a given plot
                        
                        
                        
                        
                #Calculate mean biomass values + SEs for each 'year since exclosure' and treatment
                        
                        #Calculate average biomass (kg/m2) for each treatment in each year
                        avg_biomass_df <- aggregate(plot_df$Subplot_biomass_kg_m2, by = list("Years_Since_Exclosure" = plot_df$Years_Since_Exclosure,
                                                                                             "Treatment" = plot_df$Treatment), FUN = mean)
                        colnames(avg_biomass_df)[3] <- "Mean_subplot_biomass_kg_m2"
                        
                        #Calculate SE for each year
                        
                                #Define function to calculate SE
                                std <- function(x) sd(x)/sqrt(length(x))
                                
                                #Add SE placeholder column
                                avg_biomass_df$SE <- as.numeric('')
                                
                                #Loop through years and calculate SE
                                for(i in min(plot_df$Years_Since_Exclosure):max(plot_df$Years_Since_Exclosure)){
                                        
                                        #For year i, calculate SE for each treatment
                                        
                                                #Browsed
                                                b_se <- std(plot_df$Subplot_biomass_kg_m2[plot_df$Years_Since_Exclosure == i & plot_df$Treatment == "B"])
                                                
                                                #Unbrowsed
                                                ub_se <- std(plot_df$Subplot_biomass_kg_m2[plot_df$Years_Since_Exclosure == i & plot_df$Treatment == "UB"])
                                                
                                        #Add SEs to avg biomass df for year i
                                        avg_biomass_df$SE[avg_biomass_df$Years_Since_Exclosure == i & avg_biomass_df$Treatment == "B"] <- b_se
                                        avg_biomass_df$SE[avg_biomass_df$Years_Since_Exclosure == i & avg_biomass_df$Treatment == "UB"] <- ub_se
                                        
                                }
                        
                #Plot mean subplot biomass by treatment and year (w/ SE)
                        
                        #Palette
                        pal <- wes_palette("Darjeeling1")
                        
                        #Treatment Nice Name (for plotting)
                        avg_biomass_df$TNN[avg_biomass_df$Treatment == "B"] <- "Browsed"
                        avg_biomass_df$TNN[avg_biomass_df$Treatment == "UB"] <- "Unbrowsed"

                        #Plot
                        ggplot(data = avg_biomass_df, aes(x = Years_Since_Exclosure, y = Mean_subplot_biomass_kg_m2, color = TNN, group = TNN)) +
                                geom_errorbar(aes(ymin = (Mean_subplot_biomass_kg_m2 - SE), ymax = (Mean_subplot_biomass_kg_m2 + SE)), colour="black", width=0.5, position = position_dodge(0.2)) +
                                geom_point(aes(shape = TNN), size = 1.75, position = position_dodge(0.2)) +
                                geom_line(aes(linetype = TNN)) +
                                labs(x = "Years Since Exclosure", y = "Aboveground Biomass "~(kg/m^2), color = "Treatment:", shape = "Treatment:", linetype = "Treatment:") +
                                scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
                                scale_color_manual(values = pal) +
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                        
                        
                        
                        
                        
                        
        #SPECIES-SPECIFIC SUBPLOT BIOMASS BY TREATMENT --------------
        
                #Sum species-specific biomass (g) for each subplot
                spec_df <- aggregate(data$Subplot_Total_Biomass_g, by = list("Years_Since_Exclosure" = data$Years_Since_Exclosure,
                                                               "Region" = data$Region,
                                                               "LocalityName" = data$LocalityName,
                                                               "Treatment" = data$Treatment,
                                                               "Plot" = data$Plot,
                                                               "Taxa" = data$Taxa), FUN = sum)
                
                colnames(spec_df)[7] <- "Summed_biomass_subplot_kg"
                
                #Convert summed species-specific biomass (g) to kg
                spec_df$Summed_biomass_subplot_kg <- spec_df$Summed_biomass_subplot_kg / 1000
                
                #Convert species-specific biomass to kg/m2
                
                        #Divide each value for kg by plot_area
                        spec_df$Subplot_biomass_kg_m2 <- spec_df$Summed_biomass_subplot_kg / subplot_area 
                
                                #Now we have biomass per unit area (kg/m2) for EACH SPECIES WITHIN
                                #EACH CIRCULAR SUBPLOT in a given plot
                        
                        
                        
                #Calculate mean biomass values + SEs for each 'year since exclosure' and treatment
                        
                        #Calculate average biomass (kg/m2) for each species in each treatment in each year
                        spec_biomass_df <- aggregate(spec_df$Subplot_biomass_kg_m2, by = list("Years_Since_Exclosure" = spec_df$Years_Since_Exclosure,
                                                                                             "Treatment" = spec_df$Treatment,
                                                                                             "Taxa" = spec_df$Taxa), FUN = mean)
                        colnames(spec_biomass_df)[4] <- "Mean_subplot_biomass_kg_m2"
                        
                        #Calculate SE for each year
                        
                                #Add SE placeholder column
                                spec_biomass_df$SE <- as.numeric('')
                                
                                for(i in 1:nrow(spec_biomass_df)){
                                        
                                        #Get variables from current row
                                        yse <- spec_biomass_df[i, "Years_Since_Exclosure"]
                                        tr <- spec_biomass_df[i, "Treatment"]
                                        tax <- as.character(spec_biomass_df[i, "Taxa"])
                                        
                                        #Calculate SE
                                        se <- std(spec_df$Subplot_biomass_kg_m2[spec_df$Years_Since_Exclosure == yse &
                                                                                        spec_df$Treatment == tr &
                                                                                        spec_df$Taxa == tax])
                                        
                                        #Add to row
                                        spec_biomass_df[i, "SE"] <- se
                                        
                                }
                                
                #Plot mean subplot biomass by species, treatment and year (w/ SE)
                                
                        #Plot
                        
                                #Add simplified Taxa column w/o Norwegian name
                                spec_biomass_df$simple_taxa <- sub("^(\\S*\\s+\\S+).*", "\\1", spec_biomass_df$Taxa)
                                
                                #Treatment Nice Name (for plotting)
                                spec_biomass_df$TNN[spec_biomass_df$Treatment == "B"] <- "Browsed"
                                spec_biomass_df$TNN[spec_biomass_df$Treatment == "UB"] <- "Unbrowsed"
                                
                                ggplot(data = spec_biomass_df, aes(x = Years_Since_Exclosure, y = Mean_subplot_biomass_kg_m2, color = TNN, group = TNN)) +
                                        geom_errorbar(aes(ymin = (Mean_subplot_biomass_kg_m2 - SE), ymax = (Mean_subplot_biomass_kg_m2 + SE)), colour="black", width=0.5, position = position_dodge(0.3)) +
                                        geom_point(aes(shape = TNN), size = 1.75, position = position_dodge(0.3)) +
                                        geom_line(aes(linetype = TNN)) +
                                        facet_wrap(~simple_taxa, nrow = 2) +
                                        labs(x = "Years Since Exclosure", y = "Aboveground Biomass "~(kg/m^2), color = "Treatment:", shape = "Treatment:", linetype = "Treatment:") +
                                        scale_x_continuous(breaks = c(2,4,6,8,10)) +
                                        scale_color_manual(values = pal) +
                                        theme_bw() +
                                        theme(
                                                axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                                axis.title.y = element_text(size = 12, margin = margin(r=10))
                                        )
                
                
        
                                
                        
        #SIMPLIFIED SPECIES: DECIDUOUS, PINE, & SPRUCE ---------------
        
                #Assign 'group' to original data
                        
                        #Placeholder column
                        data$Group <- as.character('')
                        
                        #Deciduous
                        decid <- c("Betula pendula (Lavlandbjørk)", "Betula pubescens (Bjørk)", "Salix caprea (Selje)", "Sorbus aucuparia (Rogn)")
                        data$Group[data$Taxa %in% decid] <- 'Deciduous'
                        
                        #Pine
                        data$Group[data$Taxa == "Pinus sylvestris (Furu)"] <- 'Pine'
                        
                        #Spruce
                        data$Group[data$Taxa == "Picea abies (Gran)" | data$Taxa == "Juniperus communis (Einer)"] <- 'Spruce'
                     
                           
                #Sum group-specific biomass (g) in each subplot (instead of summing by taxa)
                spec_simp <- aggregate(data$Subplot_Total_Biomass_g, by = list("Years_Since_Exclosure" = data$Years_Since_Exclosure,
                                                               "Region" = data$Region,
                                                               "LocalityName" = data$LocalityName,
                                                               "Treatment" = data$Treatment,
                                                               "Plot" = data$Plot,
                                                               "Group" = data$Group), FUN = sum)
                                
                colnames(spec_simp)[7] <- "Summed_biomass_subplot_kg"
                
                #Convert summed group-specific biomass (g) to kg
                spec_simp$Summed_biomass_subplot_kg <- spec_simp$Summed_biomass_subplot_kg / 1000
                
                #Convert to kg/m2
                
                        #Divide each value for kg by plot_area
                        spec_simp$Subplot_biomass_kg_m2 <- spec_simp$Summed_biomass_subplot_kg / subplot_area 
                        
                                #Now we have biomass per unit area (kg/m2) for EACH GROUP (DECIDUOUS, PINE, OR SPRUCE) WITHIN
                                #EACH CIRCULAR SUBPLOT in a given plot
                        
                        
                #Calculate mean biomass values + SEs for each 'year since exclosure' and treatment
                        
                        #Calculate average biomass (kg/m2) for each treatment in each year
                        spec_simp_bio <- aggregate(spec_simp$Subplot_biomass_kg_m2, by = list("Years_Since_Exclosure" = spec_simp$Years_Since_Exclosure,
                                                                                              "Treatment" = spec_simp$Treatment,
                                                                                              "Group" = spec_simp$Group), FUN = mean)
                        colnames(spec_simp_bio)[4] <- "Mean_subplot_biomass_kg_m2"
                        
                        #Calculate SE for each year
                        
                                #Add SE placeholder column
                                spec_simp_bio$SE <- as.numeric('')
                                
                                for(i in 1:nrow(spec_simp_bio)){
                                        
                                        #Get variables from current row
                                        yse <- spec_simp_bio[i, "Years_Since_Exclosure"]
                                        tr <- spec_simp_bio[i, "Treatment"]
                                        group <- as.character(spec_simp_bio[i, "Group"])
                                        
                                        #Calculate SE
                                        se <- std(spec_simp$Subplot_biomass_kg_m2[spec_simp$Years_Since_Exclosure == yse &
                                                                                        spec_simp$Treatment == tr &
                                                                                        spec_simp$Group == group])
                                        
                                        #Add to row
                                        spec_simp_bio[i, "SE"] <- se
                                        
                                }
                                
                #Plot mean subplot biomass by species, treatment and year (w/ SE)
                                
                        #Treatment Nice Name (for plotting)
                        spec_simp_bio$TNN[spec_simp_bio$Treatment == "B"] <- "Browsed"
                        spec_simp_bio$TNN[spec_simp_bio$Treatment == "UB"] <- "Unbrowsed"
                        
                        #Plot
                        ggplot(data = spec_simp_bio, aes(x = Years_Since_Exclosure, y = Mean_subplot_biomass_kg_m2, color = TNN, group = TNN)) +
                                geom_errorbar(aes(ymin = (Mean_subplot_biomass_kg_m2 - SE), ymax = (Mean_subplot_biomass_kg_m2 + SE)), colour="black", width=0.5, position = position_dodge(0.3)) +
                                geom_point(aes(shape = TNN), size = 1.75, position = position_dodge(0.3)) +
                                geom_line(aes(linetype = TNN)) +
                                facet_wrap(~Group, nrow = 3) +
                                labs(x = "Years Since Exclosure", y = "Aboveground Biomass "~(kg/m^2), color = "Treatment:", shape = "Treatment:", linetype = "Treatment:") +
                                scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
                                scale_color_manual(values = pal) +
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                        
                        
                        
        #TOTAL SUBPLOT BIOMASS BY REGION --------
                
                # 'plot_df' already has summed TOTAL biomass (kg/m2) for each subplot (and includes region variable)
                # We can use this to calculate means
                        
                        #Calculate mean biomass values + SEs for each 'year since exclosure' and treatment
                        
                                #Calculate average biomass (kg/m2) for each treatment in each year
                                total_reg <- aggregate(plot_df$Subplot_biomass_kg_m2, by = list("Years_Since_Exclosure" = plot_df$Years_Since_Exclosure,
                                                                                                      "Treatment" = plot_df$Treatment,
                                                                                                      "Region" = plot_df$Region), FUN = mean)
                                colnames(total_reg)[4] <- "Mean_subplot_biomass_kg_m2"
                                
                                #Calculate SE for each year
                                
                                        #Add SE placeholder column
                                        total_reg$SE <- as.numeric('')
                                        
                                        for(i in 1:nrow(total_reg)){
                                                
                                                #Get variables from current row
                                                yse <- total_reg[i, "Years_Since_Exclosure"]
                                                tr <- total_reg[i, "Treatment"]
                                                reg <- as.character(total_reg[i, "Region"])
                                                
                                                #Calculate SE
                                                se <- std(plot_df$Subplot_biomass_kg_m2[plot_df$Years_Since_Exclosure == yse &
                                                                                                plot_df$Treatment == tr &
                                                                                                plot_df$Region == reg])
                                                
                                                #Add to row
                                                total_reg[i, "SE"] <- se
                                                
                                        }
                        
                        
                        #Plot mean subplot biomass by species, treatment and year (w/ SE)
                                        
                                #Treatment nice names (for plotting)
                                total_reg$TNN[total_reg$Treatment == "B"] <- "Browsed"
                                total_reg$TNN[total_reg$Treatment == "UB"] <- "Unbrowsed"

                                        
                                #Plot
                                ggplot(data = total_reg, aes(x = Years_Since_Exclosure, y = Mean_subplot_biomass_kg_m2, color = TNN, group = TNN)) +
                                        geom_errorbar(aes(ymin = (Mean_subplot_biomass_kg_m2 - SE), ymax = (Mean_subplot_biomass_kg_m2 + SE)), colour="black", width=0.5, position = position_dodge(0.3)) +
                                        geom_point(aes(shape = TNN), size = 1.75, position = position_dodge(0.3)) +
                                        geom_line(aes(linetype = TNN)) +
                                        facet_wrap(~Region, nrow = 3) +
                                        labs(x = "Years Since Exclosure", y = "Aboveground Biomass "~(kg/m^2), color = "Treatment:", shape = "Treatment:", linetype = "Treatment:") +
                                        scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
                                        scale_color_manual(values = pal) +
                                        theme_bw() +
                                        theme(
                                                axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                                axis.title.y = element_text(size = 12, margin = margin(r=10))
                                        )
                        
                        
                        
        #SIMPLIFIED GROUP BY REGION (I.E. DIFFERENT MOOSE DENSITIES) ------
        
                #Use 'spec_simp' df
                
                        #Calculate mean biomass values + SEs for each 'year since exclosure' and treatment
                        
                                #Calculate average biomass (kg/m2) for each treatment in each year
                                spec_simp_region <- aggregate(spec_simp$Subplot_biomass_kg_m2, by = list("Years_Since_Exclosure" = spec_simp$Years_Since_Exclosure,
                                                                                                         "Region" = spec_simp$Region,
                                                                                                      "Treatment" = spec_simp$Treatment,
                                                                                                      "Group" = spec_simp$Group), FUN = mean)
                                colnames(spec_simp_region)[5] <- "Mean_subplot_biomass_kg_m2"
                                
                                #Calculate SE for each year
                                
                                #Add SE placeholder column
                                spec_simp_region$SE <- as.numeric('')
                                
                                for(i in 1:nrow(spec_simp_region)){
                                        
                                        #Get variables from current row
                                        yse <- spec_simp_region[i, "Years_Since_Exclosure"]
                                        tr <- spec_simp_region[i, "Treatment"]
                                        reg <- spec_simp_region[i, "Region"]
                                        group <- as.character(spec_simp_region[i, "Group"])
                                        
                                        #Calculate SE
                                        se <- std(spec_simp$Subplot_biomass_kg_m2[spec_simp$Years_Since_Exclosure == yse &
                                                                                          spec_simp$Region == reg &
                                                                                          spec_simp$Treatment == tr &
                                                                                          spec_simp$Group == group])
                                        
                                        #Add to row
                                        spec_simp_region[i, "SE"] <- se
                                        
                                }
                                
                #Plot mean subplot biomass by group, region, treatment and year (w/ SE)
                                
                        #Treatment nice names (for plotting)
                        spec_simp_region$TNN[spec_simp_region$Treatment == "B"] <- "Browsed"
                        spec_simp_region$TNN[spec_simp_region$Treatment == "UB"] <- "Unbrowsed"       
                                
                        #Plot
                        ggplot(data = spec_simp_region, aes(x = Years_Since_Exclosure, y = Mean_subplot_biomass_kg_m2, color = TNN, group = TNN)) +
                                geom_errorbar(aes(ymin = (Mean_subplot_biomass_kg_m2 - SE), ymax = (Mean_subplot_biomass_kg_m2 + SE)), colour="black", width=0.5, position = position_dodge(0.3)) +
                                geom_point(aes(shape = TNN), size = 1.75, position = position_dodge(0.3)) +
                                geom_line(aes(linetype = TNN)) +
                                facet_grid(Region~Group) +
                                labs(x = "Years Since Exclosure", y = "Aboveground Biomass "~(kg/m^2), color = "Treatment:", shape = "Treatment:", linetype = "Treatment:") +
                                scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
                                scale_color_manual(values = pal) +
                                theme_bw() +
                                theme(
                                        axis.title.x = element_text(size = 12, margin = margin(t=10)),
                                        axis.title.y = element_text(size = 12, margin = margin(r=10))
                                )
                        

                
#END EXPORT PLOTS ---------------------------------------------------------------------------------- 
