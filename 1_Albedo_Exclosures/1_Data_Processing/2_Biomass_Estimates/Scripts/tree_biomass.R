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
        dates <- as.POSIXct(data$X_Date, format = "%m/%d/%Y %H:%M:%S")
        dates <- format(dates, format="%Y")
        data$Year <- dates

#END DATA FILTERING -------------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#BIOMASS CALCULATION FOR EACH SET OF TREES ---------------------------------------------------------------
        
        #Add a blank column to hold biomass
        data$Biomass_g <- as.numeric('')
        
        # Loop through each row of the dataframe (2016, Trøndelag, <6m height, browsed)
        # and produce a volume estimate
        for(row in 1:nrow(data)){
                
                print(row)
                
                #Grab necessary parameters
                
                        #Get tree species
                        species <- as.character(data[row, "Taxa"])
                
                        #Get height class (in 50cm increments)
                        h <- as.numeric(data[row, "Height_class_50cm"])
                        h <- h*50
                        
                        #Get treatment
                        treatment <- data[row, "Treatment"]
                
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
                        
                        #Add calculated biomass to 'Biomass' column
                        data[row, "Biomass_g"] <- y
                
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
                
                data <- data[!is.na(data$Biomass_g),]
                data$Taxa <- as.factor(as.character(data$Taxa))
                

        
#END BIOMASS CALCULATION USING EXISTING MODELS -----------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#EXPORT PLOTS --------------------------------------------------------------------------------------
        
        #WRITE CSV
        write.csv(data, "1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/tree_biomass.csv")
        
        #Aggregate and plot
        
        
                #Aggregate
                plot_df <- aggregate(data$Biomass_g, by = list(data$Years_Since_Exclosure, data$Region, data$LocalityName, data$Treatment), FUN = sum)
                
                #Convert g to kg
                plot_df$x <- plot_df$x / 1000
                
                #Convert to kg/m2
                
                        #Each subplot has a radius of 2m - A = pi*r^2
                        subplot_area <- pi*(2^2) #12.57m2
                        
                        #Sum 4 subplots together to get total plot area in m2
                        plot_area <- 4*subplot_area #50.26548m2
                        
                        #Divide each value for kg by plot_area
                        plot_df$kg_m2 <- plot_df$x / plot_area
                        
                                #These look like they mesh well with the values produced by Kolstad et al. (2018) in 2016
                        
                #Calculate excl-open plot biomass for each LocalityName
                diff_df <- aggregate(plot_df$kg_m2, by = list(plot_df$Group.1, plot_df$Group.2, plot_df$Group.3), FUN = diff)
                
                #Add "Years Since Exclosure" variable & filter to used sites (n=37)
                diff_df$Group.1 <- as.numeric(diff_df$Group.1)
                diff_df$x <- as.numeric(diff_df$x)
                diff_df$Group.3 <- as.factor(diff_df$Group.3)

                

                #Plots ---------
                
                plot_theme <- theme(plot.title = element_text(hjust = 0.5, size = 50, margin = margin(t = 40, b = 40)),
                                    legend.position = "none",
                                    axis.text.x = element_text(size = 22, margin = margin(t=16)),
                                    axis.text.y = element_text(size = 22, margin = margin(r=16)),
                                    axis.title.x = element_text(size = 34, margin = margin(t=40, b = 40)),
                                    axis.title.y = element_text(size = 34, margin = margin(r=40)),
                                    strip.text.x = element_text(size = 22))
                
                        #Total Standing Biomass ------------
                
                                #Faceted by site
                                png(filename = "1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/Plots/biomass_diff_all_faceted.png",
                                    width = 1600,
                                    height = 1600,
                                    bg = "white")
                        
                                ggplot(data = diff_df, aes(x = Group.1, y = x))+
                                        geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                                        geom_point(size = 3) +
                                        geom_smooth(span = 100, color = "black", lwd = 0.8) + 
                                        facet_wrap(~ Group.3) +
                                        theme_bw() +
                                        labs(x = "Years Since Exclosure", y = expression(atop("Difference in Total Standing Biomass "~(kg/m^2), "(Excl. - Open)")) ) +
                                        scale_x_continuous(limits = c(1, 11), breaks = c(1, 3, 5, 7, 9, 11)) +
                                        plot_theme
                                
                                dev.off()
                        
                                #All sites
                                png(filename = "1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/Plots/biomass_diff_all.png",
                                    width = 1300,
                                    height = 1300,
                                    bg = "white")
                                
                                ggplot(data = diff_df, aes(x = Group.1, y = x))+
                                        geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                                        geom_point(size = 3) +
                                        geom_smooth(span = 20, color = "black", lwd = 0.8) + 
                                        theme_bw() +
                                        labs(x = "Years Since Exclosure", y = expression(atop("Difference in Total Standing Biomass "~(kg/m^2), "(Excl. - Open)")) ) +
                                        scale_x_continuous(limits = c(1, 11), breaks = c(1, 3, 5, 7, 9, 11)) +
                                        plot_theme
                                
                                dev.off()
                        
                                
                                
                        #Species-specific Biomass ------------
                        
                                #Aggregate by species --------
                                        
                                        #Aggregate
                                        spec_df <- aggregate(data$Biomass_g, by = list(data$Years_Since_Exclosure, data$Region, data$LocalityName, data$Treatment, data$Taxa), FUN = sum)
                                        
                                        #Convert g to kg
                                        spec_df$x <- spec_df$x / 1000
                                        
                                        #Convert to kg/m2
                                        
                                                #Divide each value for kg by plot_area
                                                spec_df$kg_m2 <- spec_df$x / plot_area
                                                
                                        #Calculate excl-open plot biomass for each LocalityName
                                        spec_diff_df <- aggregate(spec_df$kg_m2, by = list(spec_df$Group.1, spec_df$Group.2, spec_df$Group.3, spec_df$Group.5), FUN = diff)
                                        
                                        #Prep columns
                                        spec_diff_df$Group.1 <- as.numeric(spec_diff_df$Group.1)
                                        spec_diff_df$x <- as.numeric(as.character(spec_diff_df$x))
                                        spec_diff_df$Group.2 <- as.factor(spec_diff_df$Group.2)
                                        spec_diff_df$Group.3 <- as.factor(spec_diff_df$Group.3)
                                        spec_diff_df$Group.4 <- as.factor(spec_diff_df$Group.4)
                                        
                                #Plot ---------
                                        
                                        #Faceted by site
                                        png(filename = "1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/Plots/biomass_diff_spec_faceted.png",
                                            width = 1600,
                                            height = 1600,
                                            bg = "white")
                                        
                                        ggplot(data = subset(spec_diff_df, Group.3 != "No occurrence (Ingen)"), aes(x = Group.1, y = x, color = Group.4))+
                                                geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                                                geom_point(size = 3) +
                                                geom_smooth(span = 100, lwd = 0.8) + 
                                                facet_wrap(~ Group.3) +
                                                theme_bw() +
                                                labs(x = "Years Since Exclosure", y = expression(atop("Difference in Standing Biomass "~(kg/m^2), "(Excl. - Open)")), color = "Species") +
                                                scale_x_continuous(limits = c(1, 11), breaks = c(1, 3, 5, 7, 9, 11)) +
                                                plot_theme +
                                                theme(
                                                        legend.position = "bottom",
                                                        legend.title = element_text(size = 30, margin = margin(t=16)),
                                                        legend.text = element_text(size = 22, margin = margin(t=16))
                                                )
                                                
                                        
                                        dev.off()
                                        
                                        #All sites
                                        png(filename = "1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/Plots/biomass_spec_diff_all.png",
                                            width = 1800,
                                            height = 1300,
                                            bg = "white")
                                        
                                        ggplot(data = subset(spec_diff_df, Group.3 != "No occurrence (Ingen)"), aes(x = Group.1, y = x, color = Group.4))+
                                                geom_hline(yintercept = 0, color = "gray", linetype = 2) +
                                                geom_point(size = 3) +
                                                geom_smooth(span = 100, lwd = 0.8, se = F) + 
                                                theme_bw() +
                                                labs(x = "Years Since Exclosure", y = expression(atop("Difference in Standing Biomass "~(kg/m^2), "(Excl. - Open)")), color = "Species") +
                                                scale_x_continuous(limits = c(1, 11), breaks = c(1, 3, 5, 7, 9, 11)) +
                                                plot_theme +
                                                theme(
                                                        legend.position = "bottom",
                                                        legend.title = element_text(size = 30, margin = margin(t=16)),
                                                        legend.text = element_text(size = 22, margin = margin(t=16))
                                                )
                                        
                                        dev.off()
                                        
                                        
                                                
                                                
                        
                        

                
#END EXPORT PLOTS ---------------------------------------------------------------------------------- 
