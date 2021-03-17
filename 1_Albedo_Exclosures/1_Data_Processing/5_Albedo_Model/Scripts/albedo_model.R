## Script for albedo model


##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(gridExtra)
        library(cowplot)
        library(sf)
        library(raster)
        library(zoo)
        library(lme4)
        library(lmerTest)
        library(beepr)
        library(GGally)

        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        ## SITE DATA
        
                #Get 'cleaned' site data from adjacent 'Sites' folder
                site_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/cleaned_data.csv', header = TRUE)
                
        #HERBIVORE DENSITY DATA (2015 - MUNICIPALITY RESOLUTION)
                        
                #Read in herbivore biomass data (2015) from SpatialPolygons object (isolate dataframe)
                hbiomass_shp <- shapefile("1_Albedo_Exclosures/z_Data_Library/Herbivore_Density_Data/Usable_Data/NorwayLargeHerbivores")
                
                #Pull out dataframe
                hbiomass <- hbiomass_shp@data
                
                #Isolate to 2015 moose, roe deer, and red deer density data
                hbiomass2015 <- cbind(hbiomass[,c(1:10)], hbiomass$Ms_2015, hbiomass$Rd__2015, hbiomass$R_d_2015)
                
                
        ## SUSTHERB SITE PRODUCTIVITY INDICES
                
                #Productivity Data
                productivity <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/productivity_all_sites.csv', header = TRUE)
                productivity$LocalityName <- tolower(productivity$LocalityName)
                
                #Correct LocalityName items in productivity CSV
                
                        #Didrik Holmsen
                        productivity$LocalityName[productivity$LocalityName == "didrik holmsen"] <- "didrik_holmsen"
                        
                        #Fet 3
                        productivity$LocalityName[productivity$LocalityName == "fet 3"] <- "fet_3"
                        
                        #Fritsøe 1
                        productivity$LocalityName[productivity$LocalityName == "fritsøe1"] <- "fritsoe1"
                        
                        #Fritsøe 2
                        productivity$LocalityName[productivity$LocalityName == "fritsøe2"] <- "fritsoe2"
                        
                        #Halvard Pramhus
                        productivity$LocalityName[productivity$LocalityName == "halvard pramhus"] <- "halvard_pramhus"
                        
                        #Singsaas
                        productivity$LocalityName[productivity$LocalityName == "singsås"] <- "singsaas"
                        
                        #Stangeskovene Aurskog
                        productivity$LocalityName[productivity$LocalityName == "stangeskovene aurskog"] <- "stangeskovene_aurskog"
                        
                        #Stangeskovene Eidskog
                        productivity$LocalityName[productivity$LocalityName == "stangeskovene eidskog"] <- "stangeskovene_eidskog"
                        
                        #Stig Dahlen
                        productivity$LocalityName[productivity$LocalityName == "stig dæhlen"] <- "stig_dahlen"
                        
                        #Truls Holm
                        productivity$LocalityName[productivity$LocalityName == "truls holm"] <- "truls_holm"
                        
                        
        ## ALBEDO DATA (ON A SUBPLOT LEVEL)
                        
                #Load CSV
                albedo <- read.csv('1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/subplot_albedo_estimates.csv', header = T)


                
#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#DATA JOIN + EXPORT -----------------------------------------------------------------------------------
                
        #Join albedo data together with herbivore densities, productivity indices, and canopy roughness data
                
                #Make a copy of albedo data
                model_data <- albedo
                
                #Add productivity index data ---
                
                        #Placeholder column
                        model_data$Productivity_Index <- as.numeric('')
                        
                        #Loop through sites (LocalityName) in model_data
                        sites <- as.character(levels(as.factor(model_data$LocalityName)))
                        for(i in 1:length(sites)){
                                s <- sites[i]
                                model_data$Productivity_Index[model_data$LocalityName == s] <- productivity$Productivity[productivity$LocalityName == s]
                        }
                        
                #Add herbivore density data (and district id) ---
                
                        #Placeholder columns
                        model_data$DistrictID <- as.character('')
                        model_data$Moose_Density <- as.numeric('')
                        model_data$Red_Deer_Density <- as.numeric('')
                        model_data$Roe_Deer_Density <- as.numeric('')
                        
                        for(i in 1:length(sites)){
                                
                                s <- sites[i]
                                
                                #Get district ID from site_data
                                id <- as.character(site_data$DistrictID[site_data$LocalityName == s][1])
                                
                                #Format to match hbiomass2015 df (3-digit codes need 0 in front)
                                if(nchar(id) == 3){
                                        id <- paste("0", id, sep = "")
                                }
                                
                                model_data$DistrictID[model_data$LocalityName == s] <- id
                                
                                #Herbivore Densities ---
                                
                                model_data$Moose_Density[model_data$LocalityName == s] <- hbiomass2015$`hbiomass$Ms_2015`[hbiomass2015$kommnnr == id]
                                model_data$Red_Deer_Density[model_data$LocalityName == s] <- hbiomass2015$`hbiomass$Rd__2015`[hbiomass2015$kommnnr == id]
                                model_data$Roe_Deer_Density[model_data$LocalityName == s] <- hbiomass2015$`hbiomass$R_d_2015`[hbiomass2015$kommnnr == id]
                                
                        }
                        
                #Add canopy height data -----
                        
                        #Placeholder Columns
                        model_data$Mean_Canopy_Height <- as.numeric('')

                        for(i in 1:length(sites)){
                                
                                s <- sites[i]
                                
                                model_data$Mean_Canopy_Height[model_data$LocalityName == s] <- site_data$Mean[site_data$LocalityName == s]

                        }
                        
                #Remove arbitrary column 
                model_data <- model_data[,c(2:ncol(model_data))]
                
                
        #Write data for statistical model to CSV
        write.csv(model_data, "1_Albedo_Exclosures/1_Data_Processing/5_Albedo_Model/Output/albedo_model_data.csv")
                        
                        

#END DATA JOIN + EXPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#EXPLORATORY DIAGNOSTIC PLOTS -----------------------------------------------------------------------------------

        #START HERE IF ALREADY WRITTEN MODEL DATA ---
        model_data <- read.csv("1_Albedo_Exclosures/1_Data_Processing/5_Albedo_Model/Output/albedo_model_data.csv", header = T)
        
        #Assess multicollinearity between continuous numerical variables ------
        
                #Grab numerical variables (for "composite" albedo only)
                numerical <- model_data[model_data$Group == "Composite",c(9,11,14,16:19)]
                
                #Correlation matrix of numerical variables
                png(filename = "1_Albedo_Exclosures/1_Data_Processing/5_Albedo_Model/Output/Plots/correlation_matrix.png",
                    width = 1000,
                    height = 1000,
                    bg = "white")
                
                ggpairs(numerical)
                
                dev.off()
                
                #Correlogram of numerical variables
                png(filename = "1_Albedo_Exclosures/1_Data_Processing/5_Albedo_Model/Output/Plots/correlation_blocks.png",
                    width = 1000,
                    height = 1000,
                    bg = "white")
                
                ggcorr(numerical, label = T, label_size = 8, size = 6)
                
                dev.off()
                
                
                
                        #RESULTS:
                
                        # Productivity Index is moderately correlated with Mean Canopy Height (0.405), and slightly correlated with Roe Deer Density (0.208)
                        # and Red Deer Density (-0.171) - recommend by supervisors to choose productivity index instead of canopy height
                
                        # Moose Density is moderately correlated with Roe Deer Density (0.547) and slightly correlated with Red Deer Density (-0.226)
                                #Also recommend to include solely moose density (instead of other deer densities) for simplicity/interpretability
                
                       
        
        #Assess assumptions to use LMM ---------
        
                
                
        #TWO LMM MODELS:
                
                #Model A (Presence/Absence of Moose)
                
                        #Data:
                                #All data
                
                        #Fixed Effects:
                                #Treatment
                                #Years_Since_Exclosure
                                #SWE
                                #Productivity Index
                
                        #Random Effects:
                                #Region
                                #LocalityName
                                #LocalityCode
                
                #Model B (Moose Density (only looking at open plots))
                
                        #Data:
                                #Only exclosures
                
                        #Fixed Effects:
                                #Treatment
                                #Years_Since_Exclosure
                                #SWE
                                #Productivity Index
                        
                        #Random Effects:
                                #Region
                                #LocalityName
                                #LocalityCode

        
#END EXPLORATORY DIAGNOSTIC PLOTS -----------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#STATISTICAL MODELING --------------------------------------------------------------------------------------
                
        #Prepare final data for model with selected variables (only "Composite" albedo)
        final_model_data <- model_data[model_data$Group == "Composite",c(1:4,6,11,13,14,16)]
        
                #Save final model data
                write.csv(final_model_data, "1_Albedo_Exclosures/1_Data_Processing/5_Albedo_Model/Output/albedo_FINAL_model_data.csv")
        
                
        #START HERE:
        final_model_data <- read.csv("1_Albedo_Exclosures/1_Data_Processing/5_Albedo_Model/Output/albedo_FINAL_model_data.csv", header = T)
                
                #Treating "Years Since Exclosure" as a factor
                final_model_data$Years_Since_Exclosure <- as.factor(final_model_data$Years_Since_Exclosure)
        
        #MODEL A:
                
                model_a <- lmer(Albedo ~ Treatment +
                                        SWE_mm +
                                        SWE_mm^2 +
                                        Years_Since_Exclosure +
                                        Productivity_Index +
                                        Treatment*SWE_mm +
                                        Treatment*Years_Since_Exclosure +
                                        Treatment*SWE_mm*Years_Since_Exclosure +
                                        (1 | Region/LocalityName/LocalityCode), data = final_model_data)
                plot(resid(model_a))
                summary(model_a)
                
                
        #MODEL B:
                
                model_b <- lmer(Albedo ~ Moose_Density*SWE_mm*Years_Since_Exclosure + Productivity_Index + (1 | Region/LocalityName/LocalityCode), data = subset(final_model_data, Treatment == "B"))
                plot(resid(model_b))
                summary(model_b)

        
#END STATISTICAL MODELING -----------------------------------------------------------------------------------
        

