## Script for LMM of forest biomass (at the PLOT resolution)

## Model A - Effect of presence/bbsence of moose, for:

        ## Total biomass (kg/m2)
        ## Total species-specific biomass (kg/m2)


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
        
       
        #Biomass
                
                #Import biomass CSVs (total and species-specific)
                total_bio <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_biomass.csv", header = T)
                decid_bio <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_decid_biomass.csv", header = T)
                pine_bio <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_pine_biomass.csv", header = T)
                spruce_bio <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_spruce_biomass.csv", header = T)
                
                
#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#DATA JOIN + EXPORT -----------------------------------------------------------------------------------
                
        #Join biomass data together with herbivore densities, productivity indices, and canopy roughness data
                
                #Make a copy of albedo data (using 'total biomass' df as a base)
                model_data <- total_bio
                
                #Add plot species-specific biomass values
                
                        #Placeholder columns
                        model_data$Mean_Plot_Decid_Biomass_kg_m2 <- as.numeric('')
                        model_data$Mean_Plot_Pine_Biomass_kg_m2 <- as.numeric('')
                        model_data$Mean_Plot_Spruce_Biomass_kg_m2 <- as.numeric('')
                        
                        #Loop through
                        for(i in 1:nrow(model_data)){
                                
                                print(i)
                                
                                #Get variables
                                loc <- model_data[i, "LocalityCode"]
                                yr <- model_data[i, "Years_Since_Exclosure"]
                                
                                #Get corresponding species-specific biomass values
                                #Error catch for missing values
                                
                                        #Deciduous
                                        d_bio <- decid_bio$Mean_Plot_Decid_Biomass_kg_m2[decid_bio$LocalityCode == loc &
                                                                                                 decid_bio$Years_Since_Exclosure == yr]
                                        if(length(d_bio) == 0){
                                                d_bio <- 0
                                        }
                                        
                                        #Pine
                                        p_bio <- pine_bio$Mean_Plot_Pine_Biomass_kg_m2[pine_bio$LocalityCode == loc &
                                                                                                 pine_bio$Years_Since_Exclosure == yr]
                                        
                                        if(length(p_bio) == 0){
                                                p_bio <- 0
                                        }
                                        
                                        #Spruce
                                        s_bio <- spruce_bio$Mean_Plot_Spruce_Biomass_kg_m2[spruce_bio$LocalityCode == loc &
                                                                                                 spruce_bio$Years_Since_Exclosure == yr]
                                        
                                        if(length(s_bio) == 0){
                                                s_bio <- 0
                                        }
                                        
                                #Add to df
                                model_data[i, "Mean_Plot_Decid_Biomass_kg_m2"] <- d_bio
                                model_data[i, "Mean_Plot_Pine_Biomass_kg_m2"] <- p_bio
                                model_data[i, "Mean_Plot_Spruce_Biomass_kg_m2"] <- s_bio
                                        
                                
                        }
                        
                
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
                        

                #Remove arbitrary column -----
                model_data <- model_data[,c(2:ncol(model_data))]
                
                
                
                
        #Write data for statistical model to CSV
        write.csv(model_data, "1_Albedo_Exclosures/1_Data_Processing/7_Biomass_Model/Output/biomass_model_data.csv")
                        
                        

#END DATA JOIN + EXPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#EXPLORATORY DIAGNOSTIC PLOTS -----------------------------------------------------------------------------------

        #START HERE IF ALREADY WRITTEN MODEL DATA ---
        model_data <- read.csv("1_Albedo_Exclosures/1_Data_Processing/7_Biomass_Model/Output/biomass_model_data.csv", header = T)
        
        #Assess multicollinearity between continuous numerical variables ------
        
                #Grab numerical variables (for "composite" albedo only)
                numerical <- model_data[,c(6,12,14:16)]
                
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

                
                
                #Model B (Moose Density (only looking at open plots))
                
                        #Data:
                                #Only exclosures
                
                        #Fixed Effects:
                                #Moose Density
                                #Years_Since_Exclosure
                                #Productivity Index
                                #Region
                
                        #Interactions
                                #Moose Density*Years_Since_Exclosure
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
        
        
        #Distribution of albedo (outcome)
        ggplot(data = model_data, aes(x = Mean_Plot_Biomass_kg_m2)) +
                geom_histogram() +
                theme_bw() +
                labs(x = "Biomass", y = "Frequency") +
                theme(
                        axis.title.x = element_text(margin = margin(t = 6), size = 14),
                        axis.title.y = element_text(margin = margin(r = 6), size = 14),
                        axis.text.x = element_text(angle = 90, hjust = 1)
                )
        
                #Very skewed data w/ proportions
        
#END EXPLORATORY DIAGNOSTIC PLOTS -----------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#TOTAL BIOMASS MODEL --------------------------------------------------------------------------------------
     
#Using the general model-building procedures recommended by Heinze et al. (2018)

        #STEP 1: Define the most interesting model ---------
        
                #Define model
                model_a <- lmer(Mean_Plot_Biomass_kg_m2 ~ Treatment +
                                        Years_Since_Exclosure +
                                        Productivity_Index +
                                        Region +
                                        Treatment*Years_Since_Exclosure +
                                        Treatment*Region +
                                        (1 | LocalityName),
                                data = model_data)
        
                        #Quick check of residuals
                        plot(model_a) #Heteroskedasticity
                        hist(resid(model_a)) #Normally distributed residuals
                        qqmath(resid(model_a)) #Relatively normal residuals
                        
                                #Let's investigate residuals vs explanatory variables for trends
                                plot(resid(model_a) ~ model_data$Years_Since_Exclosure) #Residuals are greater after several years 
                                plot(resid(model_a) ~ model_data$Productivity_Index) #No clear trends
                                
                ##KEY POINT: without any transformations, model has normally distributed residuals but heteroskedasticity
                ##Some type of transformation is necessary
                      
                                          
        #STEP 2: Transform model to address heteroskedasticity --------
                                
                #Try a log-transformation
                model_a1 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                        Years_Since_Exclosure +
                                        Productivity_Index +
                                        Region +
                                        Treatment*Years_Since_Exclosure +
                                        Treatment*Region +
                                        (1 | LocalityName),
                                 data = model_data)
                
                        #Quick check of residuals
                        plot(model_a1) #Looks much more homoskedastic
                        hist(resid(model_a1)) #Normally distributed residuals
                        qqnorm(resid(model_a1)) #Looks very good

                        
        #STEP 3: Compare nested models with AIC ---------
                        
                #Re-fit model from Step 2 w/ ML (for comparison of fixed effects)
                model_a1 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                         Years_Since_Exclosure +
                                         Productivity_Index +
                                         Region +
                                         Treatment*Years_Since_Exclosure +
                                         Treatment*Region +
                                         (1 | LocalityName),
                                 data = model_data,
                                 REML = F)
                        
                        summary(model_a1) 
                        
                #Nested model without Productivity Index
                        
                        #Model A2
                        model_a2 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                                 Years_Since_Exclosure +
                                                 Region +
                                                 Treatment*Years_Since_Exclosure +
                                                 Treatment*Region +
                                                 (1 | LocalityName),
                                         data = model_data,
                                         REML = F)
                        
                                #Investigate summary
                                summary(model_a2) #Looks similar
                                
                                #Quick check of residual plots
                                plot(model_a2) #Looks very similar
                                hist(resid(model_a2)) #Relatively ormally distributed residuals
                                qqnorm(resid(model_a2)) #Looks great
                        
                        
                #Simple model version w/ temporal correlation structure (using nlme)
                library(nlme)
                model_a_temp <- lme(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                            Years_Since_Exclosure +
                                            Productivity_Index +
                                            Region +
                                            Treatment*Years_Since_Exclosure +
                                            Treatment*Region,
                                    random = ~1 | LocalityName,
                                    data = model_data,
                                    correlation = corCompSymm(form =~ Years_Since_Exclosure | LocalityName),
                                    method = "ML")
                
                plot(model_a_temp)
                
                #Summarize
                summary(model_a_temp)
                                
                                
        #STEP 4: AIC comparison of three models -----------
                
                AIC(model_a1, model_a2, model_a_temp) 
                        
                        #Model A1 has the lowest AIC value (may indicate that keeping productivity index improved
                        #fit quite a bit)
                        
                                
        
        #STEP 5: Re-run model with REML for most accurate parameter estimates --------
                
                final_model_a <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                              Years_Since_Exclosure +
                                              Productivity_Index +
                                              Region +
                                              Treatment*Years_Since_Exclosure +
                                              Treatment*Region +
                                              (1 | LocalityName),
                                      data = model_data,
                                      REML = T)
                

                                
        #STEP 6: Double-check residual plots to validate ----------
                
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
                
                
                                
        #STEP 7: Back-transform log for estimates -----------
        
                #Summarize model
                summary(final_model_a)

        
#END TOTAL BIOMASS MODEL --------------------------------------------------------------------------------------
                
                
                
                
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



                
#DECIDUOUS BIOMASS MODEL --------------------------------------------------------------------------------------                
  
        #STEP 1: Define the most interesting model ---------
                
                #Define model
                model_b <- lmer(Mean_Plot_Decid_Biomass_kg_m2 ~ Treatment +
                                        Years_Since_Exclosure +
                                        Productivity_Index +
                                        Region +
                                        Treatment*Years_Since_Exclosure +
                                        Treatment*Region +
                                        (1 | LocalityName),
                                data = model_data)
                
                #Quick check of residuals
                plot(model_b) #Heteroskedasticity (same as before)
                hist(resid(model_b)) #Weird distribution of residuals
                qqmath(resid(model_b)) #Looks terrible
                
                #Let's investigate residuals vs explanatory variables for trends
                plot(resid(model_b) ~ model_data$Years_Since_Exclosure) #Looks relatively linear 
                plot(resid(model_b) ~ model_data$Productivity_Index) #No clear trends
                
                ##KEY POINT: without any transformations, model has normally distributed residuals but heteroskedasticity
                ##Some type of transformation is necessary
                
                
        #STEP 2: Transform model to address heteroskedasticity --------
                
                #Try a log-transformation
                subset_decid <- model_data[model_data$Mean_Plot_Decid_Biomass_kg_m2 > 0,]
                model_b1 <- lmer(log(Mean_Plot_Decid_Biomass_kg_m2) ~ Treatment +
                                         Years_Since_Exclosure +
                                         Productivity_Index +
                                         Region +
                                         Moose_Density +
                                         Treatment*Years_Since_Exclosure +
                                         Treatment*Region +
                                         (1 | LocalityName),
                                 data = subset_decid)
                
                summary(model_b1)
                
                #Quick check of residuals
                plot(model_b1) #Looks much more homoskedastic, but still slightly heteroskedastic
                hist(resid(model_b1)) #Normally distributed residuals
                
                qqnorm(resid(model_b1)) #Looks OK, not great
                
                
        #STEP 3: Investigate Heteroskedasticity  ---------
                
                #Plot model residuals vs explanatory variables
                plot(resid(model_b1) ~ subset_decid$Years_Since_Exclosure) #Looks linear
                plot(resid(model_b1) ~ subset_decid$Productivity_Index) #No idea
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
        #STEP 3: Compare nested models with AIC ---------
                
                #Re-fit model from Step 2 w/ ML (for comparison of fixed effects)
                model_a1 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                         Years_Since_Exclosure +
                                         Productivity_Index +
                                         Region +
                                         Treatment*Years_Since_Exclosure +
                                         Treatment*Region +
                                         (1 | LocalityName),
                                 data = model_data,
                                 REML = F)
                
                summary(model_a1) 
                
                #Nested model without Productivity Index
                
                #Model A2
                model_a2 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                         Years_Since_Exclosure +
                                         Region +
                                         Treatment*Years_Since_Exclosure +
                                         Treatment*Region +
                                         (1 | LocalityName),
                                 data = model_data,
                                 REML = F)
                
                #Investigate summary
                summary(model_a2) #Looks similar
                
                #Quick check of residual plots
                plot(model_a2) #Looks very similar
                hist(resid(model_a2)) #Relatively ormally distributed residuals
                qqnorm(resid(model_a2)) #Looks great
                
                
                #Simple model version w/ temporal correlation structure (using nlme)
                library(nlme)
                model_a_temp <- lme(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                            Years_Since_Exclosure +
                                            Productivity_Index +
                                            Region +
                                            Treatment*Years_Since_Exclosure +
                                            Treatment*Region,
                                    random = ~1 | LocalityName,
                                    data = model_data,
                                    correlation = corCompSymm(form =~ Years_Since_Exclosure | LocalityName),
                                    method = "ML")
                
                plot(model_a_temp)
                
                #Summarize
                summary(model_a_temp)
                
                
                #STEP 4: AIC comparison of three models -----------
                
                AIC(model_a1, model_a2, model_a_temp) 
                
                #Model A1 has the lowest AIC value (may indicate that keeping productivity index improved
                #fit quite a bit)
                
                
                
                #STEP 5: Re-run model with REML for most accurate parameter estimates --------
                
                final_model_a <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Treatment +
                                              Years_Since_Exclosure +
                                              Productivity_Index +
                                              Region +
                                              Treatment*Years_Since_Exclosure +
                                              Treatment*Region +
                                              (1 | LocalityName),
                                      data = model_data,
                                      REML = T)
                
                
                              
#DECIDUOUS BIOMASS MODEL --------------------------------------------------------------------------------------                
                
                
                
                