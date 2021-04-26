## Script for LMM of forest biomass (at the PLOT resolution)

## Model A - Effect of presence/bbsence of moose, for:

        ## Total biomass (kg/m2) at the plot level


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
        library(nlme)
        library(beepr)
        library(GGally)
        library(lattice)

        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        ## SITE DATA
        
                #Get 'cleaned' site data from adjacent 'Sites' folder
                site_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/all_sites_data.csv', header = TRUE)
                
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
                        
                        #Kongsvinger 1 & 2
                        productivity$LocalityName[productivity$LocalityName == "kongsvinger 1"] <- "kongsvinger_1"
                        productivity$LocalityName[productivity$LocalityName == "kongsvinger 2"] <- "kongsvinger_2"
                        
                        #Maarud 1,2,3
                        productivity$LocalityName[productivity$LocalityName == "maarud 1"] <- "maarud_1"
                        productivity$LocalityName[productivity$LocalityName == "maarud 2"] <- "maarud_2"
                        productivity$LocalityName[productivity$LocalityName == "maarud 3"] <- "maarud_3"
                        
                        #Nes 1,2
                        productivity$LocalityName[productivity$LocalityName == "nes 1"] <- "nes_1"
                        productivity$LocalityName[productivity$LocalityName == "nes 2"] <- "nes_2"
                        
                        #Sørum 1
                        productivity$LocalityName[productivity$LocalityName == "sørum 1"] <- "sorum_1"
                        
                        
       
        #Biomass (total biomass)
                
                #Import biomass CSVs (total and species-specific)
                total_bio <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_biomass.csv", header = T)
                decid_bio <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_decid_biomass.csv", header = T)
                pine_bio <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_pine_biomass.csv", header = T)
                spruce_bio <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_spruce_biomass.csv", header = T)
                
        #FIX BIOMASS LOCALITYNAME ERRORS
                
                #Kongsvinger 1
                total_bio$LocalityName[total_bio$LocalityName == "kongsvinger 1"] <- "kongsvinger_1"
                decid_bio$LocalityName[decid_bio$LocalityName == "kongsvinger 1"] <- "kongsvinger_1"
                pine_bio$LocalityName[pine_bio$LocalityName == "kongsvinger 1"] <- "kongsvinger_1"
                spruce_bio$LocalityName[spruce_bio$LocalityName == "kongsvinger 1"] <- "kongsvinger_1"
                
                #Kongsvinger 2
                total_bio$LocalityName[total_bio$LocalityName == "kongsvinger 2"] <- "kongsvinger_2"
                decid_bio$LocalityName[decid_bio$LocalityName == "kongsvinger 2"] <- "kongsvinger_2"
                pine_bio$LocalityName[pine_bio$LocalityName == "kongsvinger 2"] <- "kongsvinger_2"
                spruce_bio$LocalityName[spruce_bio$LocalityName == "kongsvinger 2"] <- "kongsvinger_2"
                
                #Maarud 1
                total_bio$LocalityName[total_bio$LocalityName == "maarud 1"] <- "maarud_1"
                decid_bio$LocalityName[decid_bio$LocalityName == "maarud 1"] <- "maarud_1"
                pine_bio$LocalityName[pine_bio$LocalityName == "maarud 1"] <- "maarud_1"
                spruce_bio$LocalityName[spruce_bio$LocalityName == "maarud 1"] <- "maarud_1"
                
                #Maarud 2
                total_bio$LocalityName[total_bio$LocalityName == "maarud 2"] <- "maarud_2"
                decid_bio$LocalityName[decid_bio$LocalityName == "maarud 2"] <- "maarud_2"
                pine_bio$LocalityName[pine_bio$LocalityName == "maarud 2"] <- "maarud_2"
                spruce_bio$LocalityName[spruce_bio$LocalityName == "maarud 2"] <- "maarud_2"
                
                #Maarud 3
                total_bio$LocalityName[total_bio$LocalityName == "maarud 3"] <- "maarud_3"
                decid_bio$LocalityName[decid_bio$LocalityName == "maarud 3"] <- "maarud_3"
                pine_bio$LocalityName[pine_bio$LocalityName == "maarud 3"] <- "maarud_3"
                spruce_bio$LocalityName[spruce_bio$LocalityName == "maarud 3"] <- "maarud_3"
                
                #Nes 1
                total_bio$LocalityName[total_bio$LocalityName == "nes 1"] <- "nes_1"
                decid_bio$LocalityName[decid_bio$LocalityName == "nes 1"] <- "nes_1"
                pine_bio$LocalityName[pine_bio$LocalityName == "nes 1"] <- "nes_1"
                spruce_bio$LocalityName[spruce_bio$LocalityName == "nes 1"] <- "nes_1"
                
                #Nes 2
                total_bio$LocalityName[total_bio$LocalityName == "nes 2"] <- "nes_2"
                decid_bio$LocalityName[decid_bio$LocalityName == "nes 2"] <- "nes_2"
                pine_bio$LocalityName[pine_bio$LocalityName == "nes 2"] <- "nes_2"
                spruce_bio$LocalityName[spruce_bio$LocalityName == "nes 2"] <- "nes_2"
                
                #Sørum 1
                total_bio$LocalityName[total_bio$LocalityName == "sørum 1"] <- "sorum_1"
                decid_bio$LocalityName[decid_bio$LocalityName == "sørum 1"] <- "sorum_1"
                pine_bio$LocalityName[pine_bio$LocalityName == "sørum 1"] <- "sorum_1"
                spruce_bio$LocalityName[spruce_bio$LocalityName == "sørum 1"] <- "sorum_1"
                
                
                
                
                
#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#DATA JOIN + EXPORT -----------------------------------------------------------------------------------
                
        #Join biomass data together with herbivore densities, productivity indices, and canopy roughness data
                
                #Make a copy of albedo data (using 'total biomass' df as a base)
                model_data <- total_bio
                
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
                        
                        
                #Add species-specific biomass -----
                        
                        #Add placeholder columns
                        model_data$Decid_Bio <- as.numeric('')
                        model_data$Pine_Bio <- as.numeric('')
                        model_data$Spruce_Bio <- as.numeric('')
                        
                        #Loop through and add
                        for(i in 1:nrow(model_data)){
                                
                                print(i)
                                
                                #Get locality code and yr
                                loc <- model_data[i, "LocalityCode"]
                                yr <- model_data[i, "Years_Since_Exclosure"]
                                
                                #Get matching values
                                dbio <- decid_bio$Mean_Plot_Decid_Biomass_kg_m2[decid_bio$LocalityCode == loc &
                                                                                                              decid_bio$Years_Since_Exclosure == yr]
                                if(length(dbio) == 0){
                                        dbio <- 0
                                }
                                
                                pbio <- pine_bio$Mean_Plot_Pine_Biomass_kg_m2[pine_bio$LocalityCode == loc &
                                                                                                              pine_bio$Years_Since_Exclosure == yr]
                                if(length(pbio) == 0){
                                        pbio <- 0
                                }
                                
                                sbio <- spruce_bio$Mean_Plot_Spruce_Biomass_kg_m2[spruce_bio$LocalityCode == loc &
                                                                                                              spruce_bio$Years_Since_Exclosure == yr]
                                if(length(sbio) == 0){
                                        sbio <- 0
                                }
                                
                                #Add to df
                                model_data[i, "Decid_Bio"] <- dbio
                                model_data[i, "Pine_Bio"] <- pbio
                                model_data[i, "Spruce_Bio"] <- sbio
                                
                        }
                        
                        

                #Remove arbitrary column -----
                model_data <- model_data[,c(2:ncol(model_data))]
                
                
                
                
        #Write data for statistical model to CSV
        write.csv(model_data, "1_Albedo_Exclosures/1_Data_Processing/7_Biomass_Model/Output/biomass_model_data.csv")
                        
                        

#END DATA JOIN + EXPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#EXPLORATORY DIAGNOSTIC PLOTS -----------------------------------------------------------------------------------

        #START HERE IF ALREADY WRITTEN MODEL DATA ---
        model_data <- read.csv("1_Albedo_Exclosures/1_Data_Processing/7_Biomass_Model/Output/biomass_model_data.csv", header = T)
        model_data$Treatment <- as.factor(model_data$Treatment)
        
        #Assess multicollinearity between continuous numerical variables ------
        
                #Grab numerical variables (for "composite" albedo only)
                numerical <- model_data[,c(6,9,11:13)]
                
                #Correlation matrix of numerical variables
                png(filename = "1_Albedo_Exclosures/1_Data_Processing/7_Biomass_Model/Output/Plots/correlation_matrix.png",
                    width = 1000,
                    height = 1000,
                    bg = "white")
                
                ggpairs(numerical)
                
                dev.off()
                
                #Correlogram of numerical variables
                png(filename = "1_Albedo_Exclosures/1_Data_Processing/7_Biomass_Model/Output/Plots/correlation_blocks.png",
                    width = 1000,
                    height = 1000,
                    bg = "white")
                
                ggcorr(numerical, label = T, label_size = 8, size = 6)
                
                dev.off()
                
                
                        #RESULTS:
                        
                                # Moderate correlation between roe deer density and moose density
                                #  (should probably stick to moose density alone and presence/absence of moose alone)
                
                
        #TWO LMM MODELS:
                
                #Model A (Presence/Absence of Moose)
                
                        #Data:
                                #All data
                
                        #Fixed Effects:
                                #Treatment
                                #Years_Since_Exclosure
                                #Productivity Index
                                #Region
                
                        #Interactions
                                #Treatment*Years_Since_Exclosure
                                #Treatment*Region
                
                        #Random Effects:
                                #LocalityName

                
        #Biomass within region
        ggplot(data = model_data, aes(x = Region, y = Mean_Plot_Biomass_kg_m2)) +
                geom_boxplot() +
                theme_bw() +
                labs(x = "Region", y = "Biomass") +
                theme(
                        axis.title.x = element_text(margin = margin(t = 6)),
                        axis.title.y = element_text(margin = margin(r = 6))
                )
        
                #Similar means but differing ranges - thus, region will be important to account for
        
        #Biomass within study site (LocalityName)
        ggplot(data = model_data, aes(x = LocalityName, y = Mean_Plot_Biomass_kg_m2, fill = Region)) +
                geom_boxplot() +
                theme_bw() +
                labs(x = "Region", y = "Biomass") +
                theme(
                        axis.title.x = element_text(margin = margin(t = 6), size = 14),
                        axis.title.y = element_text(margin = margin(r = 6), size = 14),
                        axis.text.x = element_text(angle = 90, hjust = 1)
                )
        
                #Variation between ranges - locality name also important to account for
        
        
        #Distribution of biomass (outcome)
        ggplot(data = model_data, aes(x = Mean_Plot_Biomass_kg_m2)) +
                geom_histogram() +
                theme_bw() +
                labs(x = "Biomass", y = "Frequency") +
                theme(
                        axis.title.x = element_text(margin = margin(t = 6), size = 14),
                        axis.title.y = element_text(margin = margin(r = 6), size = 14),
                        axis.text.x = element_text(angle = 90, hjust = 1)
                )
        
        
#END EXPLORATORY DIAGNOSTIC PLOTS -----------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#TOTAL BIOMASS MODEL --------------------------------------------------------------------------------------
     
#Using the model-building procedures recommended by Zuur et al.

        #TREATING REGION AS A RANDOM EFFECT

        #STEP 1: Define the 'beyond optimal' model ---------
        
                #Define model of interest
                model_a <- lmer(Mean_Plot_Biomass_kg_m2 ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                        (1 | Region/LocalityName),
                                data = model_data)
        
                                #This model allows the slope of treatment to differ between region
        
                        #Quick check of residuals
                        plot(model_a) #Heteroskedasticity
                        hist(resid(model_a)) #Not normal distribution
                        qqmath(resid(model_a)) #Not normal
                        
                                #Let's investigate residuals vs explanatory variables for trends
                                plot(resid(model_a) ~ model_data$Years_Since_Exclosure) #Spread of residuals is slightly greater after several years 
                                plot(resid(model_a) ~ model_data$Productivity_Index) #Seems to increase as productivity increases

                ##KEY POINT: without any transformations, model has heteroskedasticity and non-normal residuals
                ##Some type of transformation is necessary
                      
                                          
        #STEP 2: Transform the 'beyond optimal' model to address heteroskedasticity --------
                                
                #Try a log-transformation
                model_a <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                        (1 | Region/LocalityName),
                                 data = model_data,
                                 REML = T)
                                
                        #Quick check of residuals
                        plot(model_a) #Looks much more homoskedastic
                        hist(resid(model_a)) #Normally distributed residuals
                        qqmath(resid(model_a)) #Looks very good
                        
                        #Summary
                        summary(model_a)
                        
                        
        #STEP 3: Identify best random effects structure using AIC ---------
                        
                #NOTE: using REML to compare random effects
                        
                        #RANDOM INTERCEPT MODELS
                        
                                #LocalityName nested within region
                                model_a1 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                                        (1 | Region/LocalityName),
                                                data = model_data,
                                                REML = T)
        
                                #LocalityName only
                                model_a2 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                                         (1 | LocalityName),
                                                 data = model_data,
                                                 REML = T)
                                        
                                #Region only
                                model_a3 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                                         (1 | Region),
                                                 data = model_data,
                                                 REML = T)
                        
                                #AIC Comparison of random effects
                                AIC(model_a1, model_a2, model_a3)
                                
                                        #AIC indicates that model_a2 is the best - thus, incorporating Region into random effects does not improve fit
                                
                                
                        #RANDOM SLOPE AND INTERCEPT MODELS (Does 'years since exclosure' vs biomass differ between LocalityNames? Probably)
                                
                                #Add random slope and intercept to model_a1 above
                                model_a4 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                                         (1 + Years_Since_Exclosure | LocalityName),
                                                 data = model_data,
                                                 REML = T)
                                
                                #Compare two models - did adding random slope improve AIC?
                                AIC(model_a1, model_a4) #Yes, substantially
                                
                                        #RANDOM STRUCTURE OF MODEL A4 IS "BEST" 

                        
        #STEP 4: Compare nested models with AIC ---------
                        
                #Re-fit model from Step 2 w/ ML (for comparison of fixed effects)
                        
                model_a1 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                         (1 + Years_Since_Exclosure | LocalityName),
                                 data = model_data,
                                REML = F)
                
                #Nested Models (keeping terms/interactions of interest)
                model_a2 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                         Productivity_Index +
                                         Years_Since_Exclosure +
                                         Treatment*Productivity_Index +
                                         Treatment*Years_Since_Exclosure +
                                         Productivity_Index*Years_Since_Exclosure +
                                         (1 + Years_Since_Exclosure | LocalityName),
                                 data = model_data,
                                 REML = F)
                        
                model_a3 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                         Productivity_Index +
                                         Years_Since_Exclosure +
                                         Treatment*Productivity_Index +
                                         Treatment*Years_Since_Exclosure +
                                         (1 + Years_Since_Exclosure | LocalityName),
                                 data = model_data,
                                 REML = F)
                        
                model_a4 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                         Productivity_Index +
                                         Years_Since_Exclosure +
                                         Treatment*Years_Since_Exclosure +
                                         Productivity_Index*Years_Since_Exclosure +
                                         (1 + Years_Since_Exclosure | LocalityName),
                                 data = model_data,
                                 REML = F)
                        
                        
                model_a5 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                         Productivity_Index +
                                         Years_Since_Exclosure +
                                         Treatment*Years_Since_Exclosure +
                                         (1 + Years_Since_Exclosure | LocalityName),
                                 data = model_data,
                                 REML = F)
                        
                model_a6 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                         Years_Since_Exclosure +
                                         Treatment*Years_Since_Exclosure +
                                         (1 + Years_Since_Exclosure | LocalityName),
                                 data = model_data,
                                 REML = F)

                        
                        
                                
        #STEP 5: AIC comparison of models -----------
                
                AIC(model_a1, model_a2, model_a3, model_a4, model_a5, model_a6) 
                        
                        #Model A2 has the lowest AIC value and is "best fitting" model

                
        #STEP 6: Re-run model with REML for most accurate parameter estimates --------
                
                final_model_a <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                              Productivity_Index +
                                              Years_Since_Exclosure +
                                              Treatment*Productivity_Index +
                                              Treatment*Years_Since_Exclosure +
                                              Productivity_Index*Years_Since_Exclosure +
                                              (1 + Years_Since_Exclosure | LocalityName),
                                      data = model_data,
                                      REML = T)
                                
                                
        #STEP 7: Double-check residual plots to validate ----------
                
                #Put model residuals and fitted values into df        
                resid <- data.frame("Residuals" = resid(final_model_a),
                                    "Fitted" = fitted(final_model_a))
                        
                #Normality of model residuals (histogram)
                p1 <- ggplot(data = resid, aes(x = Residuals)) +
                        geom_histogram(fill = "#b5b5b5", bins = 10) +
                        theme_bw() +
                        labs(x = "Model Residuals", y = "Frequency") +
                        theme(
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10))
                        ) +
                        geom_vline(xintercept = mean(resid$Residuals), linetype = 2)
                
                #Homoskedasticity of model residuals (residuals vs. model-fitted values)
                p2 <- qqmath(resid(final_model_a))
                p3 <- plot(final_model_a)
                p4 <- plot(final_model_a, sqrt(abs(resid(., scaled=TRUE))) ~ fitted(.),
                           type = c("p", "smooth"))
                p5 <- plot(final_model_a, resid(., scaled=TRUE) ~ fitted(.) | Treatment,
                     abline = 0, type = c("p", "smooth"), layout = c(2,1))
                
                #Assemble complex plot
                top_row <- plot_grid(p1, NULL, p2, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))
                middle_row <- plot_grid(p3, NULL, p4, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))
                bottom_row <- plot_grid(p5, NULL, NULL, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))
                
                
                complex_plot <- plot_grid(top_row, NULL, middle_row, NULL, bottom_row, ncol = 1, rel_heights = c(0.31, 0.035, 0.31, 0.035, 0.31))
                complex_plot
                
                
                                
        #STEP 8: Back-transform log for estimates -----------
        
                #UNTRANSFORMED MODEL W/ BROWSED AS REFERENCE
                
                        #Summarize model
                        summary(final_model_a)
                
                        #Confidence intervals
                        m_b_ci <- confint(final_model_a)
                
                
                #UNTRANSFORMED MODEL W/ UNBROWSED AS REFERENCE
                
                        #Switch reference group to unbrowsed (look at Years_Since_Exclosure)
                        levels(model_data$Treatment)
                        model_data$Treatment <- factor(model_data$Treatment, levels = c("UB", "B"))
                        
                        #Re-run model
                        rerun <-  lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                               Productivity_Index +
                                               Years_Since_Exclosure +
                                               Treatment*Productivity_Index +
                                               Treatment*Years_Since_Exclosure +
                                               Productivity_Index*Years_Since_Exclosure +
                                               (1 + Years_Since_Exclosure | LocalityName),
                                       data = model_data,
                                       REML = T)
                        #New summary
                        summary(rerun)
                        
                        #Confidence intervals
                        m_ub_ci <- confint(rerun)
                        
                
        
#END TOTAL BIOMASS MODEL --------------------------------------------------------------------------------------
                        
                        
                        
                        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#DECIDUOUS BIOMASS MODEL ------------------------------------------------------------------------------------------
                        
                        
        #Re-load model data
        model_data <- read.csv("1_Albedo_Exclosures/1_Data_Processing/7_Biomass_Model/Output/biomass_model_data.csv", header = T)
        model_data$Treatment <- as.factor(model_data$Treatment)
                        
        #Remove zero values of decid biomass (only 2 observations)
        decid_df <- model_data[model_data$Decid_Bio != 0,]
        
        
        #STEP 1: Define 'beyond optimal' model -----------------
        
                #Define model of interest
                model_decid <- lmer(Decid_Bio ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                                (1 | Region/LocalityName),
                                        data = model_data,
                                        REML = T)
                
                        #Quick check of residuals
                        plot(model_decid) #Heteroskedasticity
                        
              
                        ##KEY POINT: same as total biomass - need to transform
        

        #STEP 2: Transformation to address heterskedasticity ------
        
                #Try a log transformation of response
                model_decid <- lmer(log(Decid_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                            (1 | Region/LocalityName),
                                    data = decid_df,
                                    REML = T)
                        
                        #Investigate
                        plot(model_decid) #Looks slightly better, but still a bit heteroskedastic
        
                        #Plot residuals vs individual covariates
                        plot(resid(model_decid) ~ decid_df$Years_Since_Exclosure) #Looks reasonably uniform
                        plot(resid(model_decid) ~ decid_df$Productivity_Index) #No major pattern
                        decid_diag <- cbind(resid(model_decid), decid_df)
                        ggplot(data = decid_diag, aes(x = decid_diag$Treatment, y = decid_diag$`resid(model_decid)`)) +
                                geom_boxplot() #Similar variation between treatment
                        ggplot(data = decid_diag, aes(x = decid_diag$Region, y = decid_diag$`resid(model_decid)`)) +
                                geom_boxplot() #Hedmark has wider spread of residuals 

                        
        #STEP 3: Use AIC to identify best random effects structure
                        
                #RANDOM INTERCEPT MODELS
                        
                        #Region/LocalityName
                        model_d1 <- lmer(log(Decid_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                                 (1 | Region/LocalityName),
                                         data = decid_df,
                                         REML = T)
                        
                        #LocalityName
                        model_d2 <- lmer(log(Decid_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                                 (1 | LocalityName),
                                         data = decid_df,
                                         REML = T)
                        
                        #Region
                        model_d3 <- lmer(log(Decid_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                                 (1 | Region),
                                         data = decid_df,
                                         REML = T)
                        
                        #AIC Comparison
                        AIC(model_d1, model_d2, model_d3)
                        
                                #Model D2 is the "best" fitting (similar to total biomass model)
                        
                #RANDOM INTERCEPT SLOPE MODELS
                        
                        #Carried over
                        model_d4 <- lmer(log(Decid_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                                 (1 + Years_Since_Exclosure | LocalityName),
                                         data = decid_df,
                                         REML = T)
                        
                        #AIC
                        AIC(model_d1, model_d4) #Very similar AIC values
                        
                                #Going with model_d1 due to simpler structure
                
                        
        #STEP 4: AIC comparison of 'best model' with model w/ defined variance structure -------
                
                #Try allowing different variance by region
                md_o <- lme(log(Decid_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure,
                            random = ~ 1 | LocalityName,
                            weights = varIdent(form = ~ 1 | Region),
                            data = decid_df,
                            method = "REML")
                        
                        plot(md_o)
                        
                #AIC Comparison
                AIC(md_o, model_d4) #Very slight decrease in AIC (nothing major)
                
                        #Going to go with original model (model_d4), due to simplicity
                        
        
        #STEP 5: Compare nested models ------
                
                #Re-run 'winning' model with ML (for nested comparison)
                model_d1 <- lmer(log(Decid_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                         (1 | LocalityName),
                                 data = decid_df,
                                 REML = F)
                
                #Nested models
                model_d2 <- lmer(log(Decid_Bio) ~ Treatment +
                                         Productivity_Index +
                                         Years_Since_Exclosure +
                                         Treatment*Productivity_Index +
                                         Treatment*Years_Since_Exclosure +
                                         Productivity_Index*Years_Since_Exclosure +
                                         (1 | LocalityName),
                                 data = decid_df,
                                 REML = F)
                
                model_d3 <- lmer(log(Decid_Bio) ~ Treatment +
                                         Productivity_Index +
                                         Years_Since_Exclosure +
                                         Treatment*Productivity_Index +
                                         Treatment*Years_Since_Exclosure +
                                         (1 | LocalityName),
                                 data = decid_df,
                                 REML = F)
                
                model_d4 <- lmer(log(Decid_Bio) ~ Treatment +
                                         Productivity_Index +
                                         Years_Since_Exclosure +
                                         Treatment*Years_Since_Exclosure +
                                         Productivity_Index*Years_Since_Exclosure +
                                         (1 | LocalityName),
                                 data = decid_df,
                                 REML = F)
                
                model_d5 <- lmer(log(Decid_Bio) ~ Treatment +
                                         Productivity_Index +
                                         Years_Since_Exclosure +
                                         Treatment*Years_Since_Exclosure +
                                         (1 | LocalityName),
                                 data = decid_df,
                                 REML = F)
                
                model_d6 <- lmer(log(Decid_Bio) ~ Treatment +
                                         Years_Since_Exclosure +
                                         Treatment*Years_Since_Exclosure +
                                         (1 | LocalityName),
                                 data = decid_df,
                                 REML = F)
                
                
        #STEP 6: AIC comparison of nested models ------------
                        
                AIC(model_d1, model_d2, model_d3, model_d4, model_d5, model_d6)
                
                        #Model D1, D2, and D3 are very similar - D3 has the lowest AIC, so going with that
               
                
                
        #STEP 7: Re-run model with REML for most accurate parameter estimates --------
                
                final_model_d <- lmer(log(Decid_Bio) ~ Treatment +
                                              Productivity_Index +
                                              Years_Since_Exclosure +
                                              Treatment*Productivity_Index +
                                              Treatment*Years_Since_Exclosure +
                                              (1 | LocalityName),
                                      data = decid_df,
                                      REML = T)
                
                
        #STEP 8: Double-check residual plots to validate ----------
                
                #Put model residuals and fitted values into df        
                resid <- data.frame("Residuals" = resid(final_model_d),
                                    "Fitted" = fitted(final_model_d))
                
                #Normality of model residuals (histogram)
                p1 <- ggplot(data = resid, aes(x = Residuals)) +
                        geom_histogram(fill = "#b5b5b5", bins = 10) +
                        theme_bw() +
                        labs(x = "Model Residuals", y = "Frequency") +
                        theme(
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10))
                        ) +
                        geom_vline(xintercept = mean(resid$Residuals), linetype = 2)
                
                #Homoskedasticity of model residuals (residuals vs. model-fitted values)
                p2 <- qqmath(resid(final_model_d))
                p3 <- plot(final_model_d)
                p4 <- plot(final_model_d, sqrt(abs(resid(., scaled=TRUE))) ~ fitted(.),
                           type = c("p", "smooth"))
                p5 <- plot(final_model_d, resid(., scaled=TRUE) ~ fitted(.) | Treatment,
                           abline = 0, type = c("p", "smooth"), layout = c(2,1))
                
                #Assemble complex plot
                top_row <- plot_grid(p1, NULL, p2, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))
                middle_row <- plot_grid(p3, NULL, p4, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))
                bottom_row <- plot_grid(p5, NULL, NULL, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))
                
                
                complex_plot <- plot_grid(top_row, NULL, middle_row, NULL, bottom_row, ncol = 1, rel_heights = c(0.31, 0.035, 0.31, 0.035, 0.31))
                complex_plot
                

                
        #STEP 9: Back-transform log for estimates -----------
                
                #UNTRANSFORMED MODEL W/ BROWSED AS REFERENCE
                
                        #Summarize model
                        summary(final_model_d)
                
                        #Confidence intervals
                        m_b_decid_ci <- confint(final_model_d)

                
                #UNTRANSFORMED MODEL W/ UNBROWSED AS REFERENCE
                
                #Switch reference group to unbrowsed (look at Years_Since_Exclosure)
                levels(decid_df$Treatment)
                decid_df$Treatment <- factor(decid_df$Treatment, levels = c("UB", "B"))
                
                #Re-run model
                rerun <- lmer(log(Decid_Bio) ~ Treatment +
                                      Productivity_Index +
                                      Years_Since_Exclosure +
                                      Treatment*Productivity_Index +
                                      Treatment*Years_Since_Exclosure +
                                      (1 | LocalityName),
                              data = decid_df,
                              REML = T)
                
                #New summary
                summary(rerun)
                
                #Confidence intervals
                m_ub_ci <- confint(rerun)      
                        
                
#END DECIDUOUS BIOMASS MODEL --------------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#CONIFEROUS BIOMASS MODEL ------------------------------------------------------------------------------------------

#NOTE: Pine has many zero values, which makes LMM's difficult. I'm going to try to condense into 
#"coniferous biomass" (both pine and spruce biomass)

        #Create a column in df w/ 'coniferous biomass'
        model_data$Conif_Bio <- model_data$Pine_Bio + model_data$Spruce_Bio
        
        #Remove zero values of conif biomass (only 2 observations)
        model_data$Conif_Bio[model_data$Conif_Bio == 0] <- NA
        conif_df <- model_data[!is.na(model_data$Conif_Bio),]
        
        #Exploratory plots
        ggplot(data = conif_df, aes(x = Years_Since_Exclosure, y = Conif_Bio, color = Treatment)) +
                geom_point() +
                geom_smooth() +
                facet_wrap(~ Region)
        
        #STEP 1: Define 'beyond optimal' model -----------------
        
                #Define model
                model_conif <- lmer(Conif_Bio ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                            (1 | Region/LocalityName),
                                    data = conif_df)
                
                        #Quick investigation
                        plot(model_conif) #Heteroskedastic and (maybe) non-linearity
                        hist(resid(model_conif))
                        qqmath(resid(model_conif)) 
                
        #STEP 2: Transformation to address heterskedasticity ------
                        
                #Try a log transformation
                model_conif <- lmer(log(Conif_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                            (1 | Region/LocalityName),
                                    data = conif_df)
                
                        #Investigate
                        plot(model_conif) #Looks pretty good
                        
                        #Plot residuals vs individual covariates
                        plot(resid(model_conif) ~ conif_df$Years_Since_Exclosure) #Lots of variation in early years
                        #Maybe a defined variance structure would be useful?
                        plot(resid(model_conif) ~ conif_df$Productivity_Index) #No major pattern
        

        #STEP 3: AIC comparison to identify best random effects structure --------
                        
                #Use REML to compare random effects
                        
                #RANDOM INTERCEPT MODELS
                        
                        #Region/LocalityName
                        model_c1 <- lmer(log(Conif_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                                 (1 | Region/LocalityName),
                                         data = conif_df,
                                         REML = T)
                        
                        #LocalityName
                        model_c2 <- lmer(log(Conif_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                                 (1 | LocalityName),
                                         data = conif_df,
                                         REML = T)
                        
                        #Region
                        model_c3 <- lmer(log(Conif_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                                 (1 | Region),
                                         data = conif_df,
                                         REML = T)
                        
                        #AIC Comparison
                        AIC(model_c1, model_c2, model_c3)
                        
                                #Model C2 has lowest AIC 
                        
                        
                #RANDOM SLOPE INTERCEPT MODELS
                        
                        #Carried over
                        model_c4 <- lmer(log(Conif_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure +
                                                 (1 + Years_Since_Exclosure | LocalityName),
                                         data = conif_df,
                                         REML = T)
                        
                        #AIC Comparison
                        AIC(model_c2, model_c4) #Adding random slope substantially reduces AIC
                        
                                #Model C4 is the "best fitting" model
                        
                        
                        

                        
        #STEP 3: Try a defined variance structure to reduce heteroskedasticity --------
                        
                #Note: Using Zuur et al. (2009) as a reference
                #Use 'nlme' package
                library(nlme)
                
                #Visual
                boxplot(resid(model_c4) ~ conif_df$Treatment)
                plot(resid(model_c4) ~ conif_df$Years_Since_Exclosure)
                        
                #varIdent variance structure for different variance between regions
                md1 <- lme(log(Conif_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure,
                           random = ~ 1 | LocalityName,
                           weights = varIdent(form = ~ 1 | Region),
                           data = conif_df,
                           method = "REML")
                

        #STEP 4: AIC comparison of original model and model with defined variance structure ------
                        
                        #Re-define model with lme function
                        md_o <- lme(log(Conif_Bio) ~ Treatment*Years_Since_Exclosure*Productivity_Index,
                                    random = ~ 1 | LocalityName,
                                    data = conif_df,
                                    method = "REML")
                        
                        AIC(md_o, md1) #Adding varComb structure substantially reduced AIC value & improved residual plots
                        
                                #MD1 is "best fitting" plot
                        
                        
        #STEP 5: Compare nested models with AIC ---------
                        
                #Re-fit model from Step 4 w/ ML (for comparison of fixed effects)
                model_conif <- lme(log(Conif_Bio) ~ Productivity_Index*Treatment*Years_Since_Exclosure,
                                   random = ~ 1 | LocalityName,
                                   weights = varIdent(form = ~ 1 | Region),
                                   data = conif_df,
                                   method = "ML")
                
                #Nested models
                        
                        #MC1
                        mc1 <- lme(log(Conif_Bio) ~ Treatment +
                                           Productivity_Index +
                                           Years_Since_Exclosure +
                                           Treatment*Productivity_Index +
                                           Treatment*Years_Since_Exclosure +
                                           Productivity_Index*Years_Since_Exclosure,
                                   random = ~ 1 | LocalityName,
                                   weights = varIdent(form = ~ 1 | Region),
                                   data = conif_df,
                                   method = "ML")
                        
                        mc2 <- lme(log(Conif_Bio) ~ Treatment +
                                           Productivity_Index +
                                           Years_Since_Exclosure +
                                           Treatment*Productivity_Index +
                                           Treatment*Years_Since_Exclosure,
                                   random = ~ 1 | LocalityName,
                                   weights = varIdent(form = ~ 1 | Region),
                                   data = conif_df,
                                   method = "ML")
                        
                        mc3 <- lme(log(Conif_Bio) ~ Treatment +
                                           Productivity_Index +
                                           Years_Since_Exclosure +
                                           Treatment*Years_Since_Exclosure +
                                           Productivity_Index*Years_Since_Exclosure,
                                   random = ~ 1 | LocalityName,
                                   weights = varIdent(form = ~ 1 | Region),
                                   data = conif_df,
                                   method = "ML")
                        
                        mc4 <- lme(log(Conif_Bio) ~ Treatment +
                                           Productivity_Index +
                                           Years_Since_Exclosure +
                                           Treatment*Years_Since_Exclosure,
                                   random = ~ 1 | LocalityName,
                                   weights = varIdent(form = ~ 1 | Region),
                                   data = conif_df,
                                   method = "ML")
                        
                        mc5 <- lme(log(Conif_Bio) ~ Treatment +
                                           Years_Since_Exclosure +
                                           Treatment*Years_Since_Exclosure,
                                   random = ~ 1 | LocalityName,
                                   weights = varIdent(form = ~ 1 | Region),
                                   data = conif_df,
                                   method = "ML")
                        
                        
        #STEP 6: AIC comparison of nested models --------
                        
                AIC(model_conif, mc1, mc2, mc3, mc4, mc5)
                
                        #MC3 has the lowest AIC value
                        
                        
                        
        #STEP 7: Re-run model with REML for most accurate parameter estimates --------
        
                final_model_c <-  lme(log(Conif_Bio) ~ Treatment +
                                              Productivity_Index +
                                              Years_Since_Exclosure +
                                              Treatment*Years_Since_Exclosure +
                                              Productivity_Index*Years_Since_Exclosure,
                                      random = ~ 1 | LocalityName,
                                      weights = varIdent(form = ~ 1 | Region),
                                      data = conif_df,
                                      method = "REML")
        

                        
        #STEP 8: Double-check residual plots to validate ----------
                        
                        #Put model residuals and fitted values into df        
                        resid <- data.frame("Residuals" = resid(final_model_c),
                                            "Fitted" = fitted(final_model_c))
                        
                        #Normality of model residuals (histogram)
                        p1 <- ggplot(data = resid, aes(x = Residuals)) +
                                geom_histogram(fill = "#b5b5b5", bins = 10) +
                                theme_bw() +
                                labs(x = "Model Residuals", y = "Frequency") +
                                theme(
                                        axis.title.x = element_text(margin = margin(t = 10)),
                                        axis.title.y = element_text(margin = margin(r = 10))
                                ) +
                                geom_vline(xintercept = mean(resid$Residuals), linetype = 2)
                        
                        #Homoskedasticity of model residuals (residuals vs. model-fitted values)
                        p2 <- qqmath(resid(final_model_c))
                        p3 <- plot(final_model_c)
                        p4 <- plot(final_model_c, sqrt(abs(resid(., scaled=TRUE))) ~ fitted(.),
                                   type = c("p", "smooth"))
                        p5 <- plot(final_model_c, resid(., scaled=TRUE) ~ fitted(.) | Treatment,
                                   abline = 0, type = c("p", "smooth"), layout = c(2,1))
                        
                        #Assemble complex plot
                        top_row <- plot_grid(p1, NULL, p2, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))
                        middle_row <- plot_grid(p3, NULL, p4, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))
                        bottom_row <- plot_grid(p5, NULL, NULL, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))
                        
                        
                        complex_plot <- plot_grid(top_row, NULL, middle_row, NULL, bottom_row, ncol = 1, rel_heights = c(0.31, 0.035, 0.31, 0.035, 0.31))
                        complex_plot
                        
                        
                        
                        
        #STEP 9: Back-transform log for estimates -----------
                        
                #UNTRANSFORMED MODEL W/ BROWSED AS REFERENCE
                        
                        #Summarize model
                        summary(final_model_c)
                        
                        #Confidence intervals
                        intervals(final_model_c)
                        
                        
                #UNTRANSFORMED MODEL W/ UNBROWSED AS REFERENCE
                        
                        #Switch reference group to unbrowsed (look at Years_Since_Exclosure)
                        levels(conif_df$Treatment)
                        conif_df$Treatment <- factor(conif_df$Treatment, levels = c("UB", "B"))
                        
                        #Re-run model
                        rerun <- lme(log(Conif_Bio) ~ Treatment +
                                             Productivity_Index +
                                             Years_Since_Exclosure +
                                             Treatment*Years_Since_Exclosure +
                                             Productivity_Index*Years_Since_Exclosure,
                                     random = ~ 1 | LocalityName,
                                     weights = varIdent(form = ~ 1 | Region),
                                     data = conif_df,
                                     method = "REML")
                        
                        #New summary
                        summary(rerun)
                        
                        #Confidence intervals
                        intervals(rerun) 

                
        
#END CONIFEROUS BIOMASS MODEL --------------------------------------------------------------------------------------
                        
                        
                        
                        
                        
                        
                        
        #All total       
        ggplot(data = model_data, aes(x = Years_Since_Exclosure, y = Mean_Plot_Biomass_kg_m2, color = Treatment)) +
                geom_point() +
                geom_line() +
                facet_wrap(~LocalityName) +
                theme_bw()
                        
        weird <- c("drangedal1", "eidskog", "fet_3", "halvard_pramhus",
                   "kviteseid1", "kviteseid2", "nes_2", "notodden3",
                   "notodden6", "nsb_verdal", "selbu_kl", "singsaas",
                   "stangeskovene_eidskog", "steinkjer_2bbb", "verdal_2vb")
        
        isolated <- model_data[model_data$LocalityName %in% weird,]
        
        #Deciduous Isolated
        ggplot(data = isolated, aes(x = Years_Since_Exclosure, y = Decid_Bio, color = Treatment)) +
                geom_point() +
                geom_line() +
                scale_y_continuous(limits = c(0,3.6)) +
                facet_wrap(~LocalityName, nrow = 2) +
                theme_bw()
        
        #Add coniferous
        isolated$Conif_Bio <- isolated$Pine_Bio + isolated$Spruce_Bio
        
        ggplot(data = isolated, aes(x = Years_Since_Exclosure, y = Conif_Bio, color = Treatment)) +
                geom_point() +
                geom_line() +
                scale_y_continuous(limits = c(0,3.6)) +
                facet_wrap(~LocalityName, nrow = 2) +
                theme_bw()
        
        #Regions
        
        #Productivity
        model_data$In_Weird <- as.character('')
        model_data$In_Weird[model_data$LocalityName %in% weird] <- "Yes"
        model_data$In_Weird[!(model_data$LocalityName %in% weird)] <- "No"
        
        ggplot(data = subset(model_data, Years_Since_Exclosure == 6 & Treatment == "UB"), aes(x = Productivity_Index, y = Decid_Bio, color = In_Weird)) +
                geom_point() +
                theme_bw() +
                scale_color_manual(values = c("grey", "red")) +
                labs(x = "Productivity Index", y = "Deciduous Biomass (6 Yrs)") +
                theme(
                        legend.position = "none"
                )
        
                        #These sites have a range of productivity values but still low biomass growth
        
        #Moose Density
        ggplot(data = subset(model_data, Years_Since_Exclosure == 6 & Treatment == "UB"), aes(x = Roe_Deer_Density, y = Decid_Bio, color = In_Weird)) +
                geom_point() +
                theme_bw() +
                scale_color_manual(values = c("grey", "red")) +
                labs(x = "Roe Deer Density (kg/km2)", y = "Deciduous Biomass (6 Yrs)") +
                theme(
                        legend.position = "none"
                )
                        
                        
        