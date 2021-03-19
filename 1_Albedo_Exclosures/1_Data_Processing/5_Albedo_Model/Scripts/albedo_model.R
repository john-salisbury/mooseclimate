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
        library(lattice)

        
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
        
        ## MONTHLY AVERAGE PROPORTION OF DAYS WITH SNOW (SITE-SPECIFIC)
        ## Note: this is used instead of SWE
                        
                snow <- read.csv('1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Snow_Proportions/average_prop_day_with_snow.csv', header = T)
                        
        ## ALBEDO DATA (ON A PLOT LEVEL)
                        
                #Load CSV
                albedo <- read.csv('1_Albedo_Exclosures/1_Data_Processing/4_Albedo_Estimates/Output/Plot_Resolution/mean_plot_albedo.csv', header = T)


                
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
                        
                #Remove arbitrary column -----
                model_data <- model_data[,c(2:ncol(model_data))]
                
        
                #Add snow proportions
                
                        #Placeholder column
                        model_data$Snow_Prop <- as.numeric('')
                        
                        #Loop through each row
                        for(i in 1:nrow(model_data)){
                                
                                print(i)
                                
                                #Get variables
                                loc <- model_data[i, "LocalityName"]
                                mt <- model_data[i, "Month"]
                                
                                #Get snow prop
                                prop <- snow$Avg_Prop_Days_Snow[snow$LocalityName == loc &
                                                                        snow$Month == mt]
                                
                                #Add to row
                                model_data[i, "Snow_Prop"] <- prop

                        }
                        
        #Limit to "Composite" albedo group
        model_data <- model_data[model_data$Group == "Composite",]
                
                
        #Write data for statistical model to CSV
        write.csv(model_data, "1_Albedo_Exclosures/1_Data_Processing/5_Albedo_Model/Output/albedo_model_data.csv")
                        
                        

#END DATA JOIN + EXPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#EXPLORATORY DIAGNOSTIC PLOTS -----------------------------------------------------------------------------------

        #START HERE IF ALREADY WRITTEN MODEL DATA ---
        model_data <- read.csv("1_Albedo_Exclosures/1_Data_Processing/5_Albedo_Model/Output/albedo_model_data.csv", header = T)
        
        #Assess multicollinearity between continuous numerical variables ------
        
                #Grab numerical variables (for "composite" albedo only)
                numerical <- model_data[,c(8,10,12:14,15:16)]
                
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

                
        #Albedo within region
        ggplot(data = model_data, aes(x = Region, y = Plot_Albedo)) +
                geom_boxplot() +
                theme_bw() +
                labs(x = "Region", y = "Albedo") +
                theme(
                        axis.title.x = element_text(margin = margin(t = 6)),
                        axis.title.y = element_text(margin = margin(r = 6))
                )
        
                #Similar means but differing ranges - thus, region will be important to account for
        
        #Albedo within study site (LocalityName)
        ggplot(data = model_data, aes(x = LocalityName, y = Plot_Albedo, fill = Region)) +
                geom_boxplot() +
                theme_bw() +
                labs(x = "Region", y = "Albedo") +
                theme(
                        axis.title.x = element_text(margin = margin(t = 6), size = 14),
                        axis.title.y = element_text(margin = margin(r = 6), size = 14),
                        axis.text.x = element_text(angle = 90, hjust = 1)
                )
        
                #Variation between ranges - locality name also important to account for
        
        
        #Distribution of albedo (outcome)
        ggplot(data = model_data, aes(x = Plot_Albedo)) +
                geom_histogram() +
                theme_bw() +
                labs(x = "Albedo", y = "Frequency") +
                theme(
                        axis.title.x = element_text(margin = margin(t = 6), size = 14),
                        axis.title.y = element_text(margin = margin(r = 6), size = 14),
                        axis.text.x = element_text(angle = 90, hjust = 1)
                )
        
                #Very skewed data w/ proportions
        
                #Does transformation help?
                ggplot(data = model_data, aes(x = log(Plot_Albedo))) +
                        geom_histogram() +
                        theme_bw() +
                        labs(x = "log(Albedo)", y = "Frequency") +
                        theme(
                                axis.title.x = element_text(margin = margin(t = 6), size = 14),
                                axis.title.y = element_text(margin = margin(r = 6), size = 14),
                                axis.text.x = element_text(angle = 90, hjust = 1)
                        )
                
                        #Log transformation doesn't help (sqrt transformation doesn't help either)
                
                        #This might indicate that a GLMM is necessary (to handle proportion data)
        
#END EXPLORATORY DIAGNOSTIC PLOTS -----------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#MODEL 1: LMM --------------------------------------------------------------------------------------
     
        #Going to investigate using an LMM
        
        #MODEL 1A:

                #Model form
                model_a <- lmer(Plot_Albedo ~ Treatment +
                                        Years_Since_Exclosure +
                                        Snow_Prop +
                                        Productivity_Index +
                                        Region +
                                        Treatment*Years_Since_Exclosure +
                                        Treatment*Snow_Prop +
                                        Treatment*Region +
                                        (1 | LocalityName), data = model_data)
                
                #Assess assumptions of LMM
        
                        #(1) Linearity of relationship between response and predictor
                        plot(model_a, type = c("p", "smooth"))
                        
                                #Strange residual plot
                
                        #(2) Linearity of relationship between response and predictor (within treatment)
                        plot(model_a, resid(., scaled=TRUE) ~ fitted(.) | Treatment,
                             abline = 0, type = c("p", "smooth"), layout = c(2,1))
                        
                                #Strange residual plots within both treatments
                        
                        #(3) Linearity of relationship between response and predictor (within LocalityName)
                        plot(model_a, resid(., scaled=TRUE) ~ fitted(.) | LocalityName)
                        
                                #Strange residual plots at every site
                        
                        #(4) Homogeneity of residual variance
                        plot(model_a, sqrt(abs(resid(., scaled=TRUE))) ~ fitted(.),
                             type = c("p", "smooth"))
                        
                                #Looks horrible
                        
                        #(5) Within-group errors are independent with normal distribution
                        qqmath(model_a) #Not straight
                        hist(resid(model_a)) #Looks slightly normal?
                        

                        #Plot predictors vs residuals
                        
                                #Years_Since_Exclosure
                                df <- data.frame(x = model_data$Years_Since_Exclosure, y = resid(model_a))
                                ggplot(data = df, aes(x = x, y = y)) +
                                        geom_point(shape = 1)
                                
                                        #Looks reasonably evenly distributed
                                
                                
                                #Snow prop
                                df <- data.frame(x = model_data$Snow_Prop, y = resid(model_a))
                                ggplot(data = df, aes(x = x, y = y)) +
                                        geom_point(shape = 1) +
                                        labs(x = "Monthly Avg. Proportion of Days w/ Snow", y = "Model A Residuals") +
                                        theme_bw()
                                
                                        #SNOW PROP HAS NON-LINEAR RELATIONSHIP W/ RESIDUALS (probably responsible for weird trends
                                        #in residual plots)
                                
                                #Productivity Index
                                df <- data.frame(x = model_data$Productivity_Index, y = resid(model_a))
                                ggplot(data = df, aes(x = x, y = y)) +
                                        geom_point()
                                
                                        #Looks evenly distributed
                                
                                
                        #Key point - snow prop is the problematic factor - how do I handle this?
                                
                                
        #Let's explore snow vs albedo a bit more
                                
                #Snow vs albedo
                ggplot(data = model_data, aes(x = Snow_Prop, y = Plot_Albedo)) +
                        geom_point(shape = 1) +
                        geom_smooth(method = lm) +
                        theme_bw() +
                        labs(x = "Monthly Avg. Proportion of Days w/ Snow", y = "Albedo") +
                        theme(
                                axis.title.x = element_text(margin = margin(t = 6), size = 14),
                                axis.title.y = element_text(margin = margin(r = 6), size = 14),
                                axis.text.x = element_text(angle = 90, hjust = 1)
                        )
                
                #Transformed
                ggplot(data = model_data, aes(x = sqrt(Snow_Prop), y = Plot_Albedo)) +
                        geom_point(shape = 1) +
                        geom_smooth(method = lm) +
                        theme_bw() +
                        labs(x = "Monthly Avg. Proportion of Days w/ Snow", y = "Albedo") +
                        theme(
                                axis.title.x = element_text(margin = margin(t = 6), size = 14),
                                axis.title.y = element_text(margin = margin(r = 6), size = 14),
                                axis.text.x = element_text(angle = 90, hjust = 1)
                        )

                                
                                
                        
#END MODEL A: LMM --------------------------------------------------------------------------------------

                
        #MODEL B:
                
                model_b <- lmer(log(Albedo) ~ Moose_Density +
                                        sqrt(SWE_mm) +
                                        Years_Since_Exclosure +
                                        Productivity_Index +
                                        Moose_Density*Years_Since_Exclosure +
                                        Moose_Density*sqrt(SWE_mm) +
                                        (1 | Region), data = subset(model_data, Treatment == "B"))
                
                #Look at residuals
                hist(resid(model_b))
                qqnorm(resid(model_b))
                qqline(resid(model_b))
                plot(model_b)
                summary(model_b)
                
                        #Residuals also don't look great
                
                
        #SUMMARY OF TWO MODELS:
                
                # Both models have similar residual plots and don't look great - I'm going to 
                # try James's suggestion and use proportion of days with snow in place of SWE
                # to see if that helps reduce weird trends

        
#END MODEL B -----------------------------------------------------------------------------------
        

