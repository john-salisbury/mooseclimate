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
                
        
                #Add snow proportions -----
                
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
                        
                        
                #Add species proportions -----
                        
                        #Load species proportions
                        props <- read.csv('1_Albedo_Exclosures/z_Data_Library/Tree_Data/Usable_Data/tree_species_proportions_subplot_level.csv', header = T)
                        
                        #Isolate one year 
                        test <- props[props$Year == 2009,]
                        
                        #Are props correlated?
                        cor.test(test$Prop_spruce, test$Prop_birch) #Strongly correlated
                        
                        #Only calc plot-level avg. of deciduous prop
                        plot_props <- aggregate(props$Prop_birch, by = list("LocalityName" = props$LocalityName,
                                                                            "LocalityCode" = props$LocalityCode,
                                                                            "Year" = props$Year), FUN = mean)
                        colnames(plot_props)[4] <- "Avg_Decid_Prop"
                        
                        #Add to model data
                        model_data$Avg_Decid_Prop <- as.numeric('')
                        for(i in 1:nrow(model_data)){
                                
                                #Variables
                                loc <- model_data[i, "LocalityCode"]
                                yse <- model_data[i, "Years_Since_Exclosure"]
                                
                                #Translate YSE to Year for site loc
                                yr <- site_data$Year.initiated[site_data$LocalityCode == loc] + yse
                                
                                #Grab prop
                                p <-plot_props$Avg_Decid_Prop[plot_props$LocalityCode == loc & plot_props$Year == yr]
                                
                                #Add to model data
                                model_data[i, "Avg_Decid_Prop"] <- p
                                
                                
                        }
                        
                        
                #Add monthly SWE (mm) ----
                
                        #Load SWE
                        clim <- read.csv('1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Averages/average_climate_data_by_site.csv', header = T)
                        
                        model_data$SWE_mm <- as.numeric('')
                        model_data$Temp_K <- as.numeric('')
                        
                        
                        #Add SWE to model data
                        for(i in 1:nrow(model_data)){
                                loc <- model_data[i, "LocalityName"]
                                mt <- model_data[i, "Month"]
                                
                                swe <- clim$SWE_mm[clim$LocalityName == loc & clim$Month == mt]
                                temp <- clim$Temperature_K[clim$LocalityName == loc & clim$Month == mt]
                                
                                model_data[i, "SWE_mm"] <- swe
                                model_data[i, "Temp_K"] <- temp
                                
                        }
                        

                        
        #Limit to "Composite" albedo group
        model_data <- model_data[model_data$Group == "Composite",]
        
        #Add season variable
        model_data$Season[model_data$Month %in% c(1:3)] <- "Season_1"
        model_data$Season[model_data$Month %in% c(4:6)] <- "Season_2"
        model_data$Season[model_data$Month %in% c(7:9)] <- "Season_3"
        model_data$Season[model_data$Month %in% c(10:12)] <- "Season_4"
        
                
                
        #Write data for statistical model to CSV
        write.csv(model_data, "1_Albedo_Exclosures/1_Data_Processing/5_Albedo_Model/Output/albedo_model_data.csv")
                        
                        

#END DATA JOIN + EXPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#EXPLORATORY DIAGNOSTIC PLOTS -----------------------------------------------------------------------------------

        #START HERE IF ALREADY WRITTEN MODEL DATA ---
        model_data <- read.csv("1_Albedo_Exclosures/1_Data_Processing/5_Albedo_Model/Output/albedo_model_data.csv", header = T)
        
        #Assess multicollinearity between continuous numerical variables ------
        
                #Grab numerical variables (for "composite" albedo only)
                numerical <- model_data[,c(8,10,12:14,16)]
                
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
                
                
        
                
                #Model A (Presence/Absence of Moose)
                
                        #Data:
                                #All data
                
                        #Fixed Effects:
                                #Treatment
                                #Years_Since_Exclosure
                                #SWE
                                #Productivity Index
                                #Region
                
                        #Random Effects:
                                #LocalityName

               
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
                


        
#END EXPLORATORY DIAGNOSTIC PLOTS -----------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#MODEL A --------------------------------------------------------------------------------------
     
#Using the model-building protocol by Zuur et al.
                
                
        #STEP 1: Define the 'beyond optimal' model ---------
                
                #NOTE: Zuur suggests that, if there are many covariates, it is appropriate to build model according
                #to system knowledge (but include all interactions that might be relevant)
                
                #Ensure that Month is a factor
                model_data$Month <- as.factor(model_data$Month)
                
                #Define model 
                model_a <- lmer(Plot_Albedo ~ Productivity_Index +
                                        Treatment*Snow_Prop*Years_Since_Exclosure +
                                        (1 | Region/LocalityName) + (1 | Month),
                                data = model_data)
               
                #Quick check of residuals
                plot(model_a) #Not great
                hist(resid(model_a)) #Skewed
                qqmath(resid(model_a)) #Not great
                
                        #KEY POINT - Transformation of outcome might be necessary to address
                        #heteroskedasticity in residual plots
                
                

        #STEP 2: Log-transform outcome to address heteroskedasticity -------
                
                #Define model 
                model_a <- lmer(log(Plot_Albedo) ~ Productivity_Index*Treatment*Snow_Prop*Years_Since_Exclosure +
                                        (1 | Region/LocalityName) + (1 | Month),
                                data = model_data)
                
                #Quick check of residuals
                plot(model_a) #Looks much better
                hist(resid(model_a)) #Slightly more normal
                qqmath(resid(model_a)) #Not great
                
                   
                     
        #STEP 3: Identify best random effects structure using AIC ---------
                
                #Fit with REML (to compare random effects structure)
                #NOTE: adding month as a crossed random effect is necessary to make residual plots
                #look OK
                
                #Region/LocalityName + Month
                m1 <- lmer(log(Plot_Albedo) ~ Productivity_Index*Treatment*Snow_Prop*Years_Since_Exclosure +
                                   (1 | Region/LocalityName) + (1 | Month),
                           data = model_data,
                           REML = T)
                
                #LocalityName + Month
                m2 <- lmer(log(Plot_Albedo) ~ Productivity_Index*Treatment*Snow_Prop*Years_Since_Exclosure +
                                   (1 | LocalityName) + (1 | Month),
                           data = model_data,
                           REML = T)

                #AIC Comparison
                AIC(m1, m2)
                
                        #Very similar AIC values (within 2 points) - going to use the model
                        #with Region/LocalityName to account for variation due to region

               
        #STEP 4: Compare nested models with AIC ---------
                
                #Re-fit model from Step 3 w/ ML (for comparison of fixed effects)
                m1 <- lmer(log(Plot_Albedo) ~ Productivity_Index*Treatment*Snow_Prop*Years_Since_Exclosure +
                                   (1 | Region/LocalityName) + (1 | Month),
                           data = model_data,
                           REML = F)
                
                #Nested models (of interest, since there are many possible interaction combos)
                m2 <- lmer(log(Plot_Albedo) ~ Treatment +
                                   Years_Since_Exclosure +
                                   Snow_Prop +
                                   Productivity_Index +
                                   Treatment*Years_Since_Exclosure +
                                   Treatment*Snow_Prop +
                                   Treatment*Productivity_Index +
                                   Treatment*Snow_Prop*Years_Since_Exclosure +
                                   (1 | Region/LocalityName) + (1 | Month),
                           data = model_data,
                           REML = F)
                
                m3 <- lmer(log(Plot_Albedo) ~ Treatment +
                                   Years_Since_Exclosure +
                                   Snow_Prop +
                                   Productivity_Index +
                                   Treatment*Years_Since_Exclosure +
                                   Treatment*Snow_Prop +
                                   Treatment*Snow_Prop*Years_Since_Exclosure +
                                   (1 | Region/LocalityName) + (1 | Month),
                           data = model_data,
                           REML = F)
                
                m4 <- lmer(log(Plot_Albedo) ~ Treatment +
                                   Years_Since_Exclosure +
                                   Snow_Prop +
                                   Productivity_Index +
                                   Treatment*Years_Since_Exclosure +
                                   Treatment*Snow_Prop +
                                   Treatment*Productivity_Index +
                                   (1 | Region/LocalityName) + (1 | Month),
                           data = model_data,
                           REML = F)
                
                m5 <- lmer(log(Plot_Albedo) ~ Treatment +
                                   Years_Since_Exclosure +
                                   Snow_Prop +
                                   Productivity_Index +
                                   Treatment*Years_Since_Exclosure +
                                   Treatment*Snow_Prop +
                                   (1 | Region/LocalityName) + (1 | Month),
                           data = model_data,
                           REML = F)
                
                m6 <- lmer(log(Plot_Albedo) ~ Treatment +
                                   Years_Since_Exclosure +
                                   Snow_Prop +
                                   Treatment*Years_Since_Exclosure +
                                   Treatment*Snow_Prop +
                                   (1 | Region/LocalityName) + (1 | Month),
                           data = model_data,
                           REML = F)
                
                
                #AIC Comparison
                AIC(m1, m2, m3, m4, m5, m6)
                
                        #Model 6 is the "best-fitting" model
                
                
                
        #STEP 5: Re-run model with REML for most accurate parameter estimates --------
                
                final_model_a <- lmer(log(Plot_Albedo) ~ Treatment +
                                              Years_Since_Exclosure +
                                              Snow_Prop +
                                              Treatment*Years_Since_Exclosure +
                                              Treatment*Snow_Prop +
                                              (1 | Region/LocalityName) + (1 | Month),
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
                rerun <-  lmer(log(Plot_Albedo) ~ Treatment +
                                       Years_Since_Exclosure +
                                       Snow_Prop +
                                       Treatment*Years_Since_Exclosure +
                                       Treatment*Snow_Prop +
                                       (1 | Region/LocalityName) + (1 | Month),
                               data = model_data,
                               REML = T)
                
                #New summary
                summary(rerun)
                
                #Confidence intervals
                m_ub_ci <- confint(rerun)
                
                

                
        
                
                
#END MODEL A --------------------------------------------------------------------------------------
 
      