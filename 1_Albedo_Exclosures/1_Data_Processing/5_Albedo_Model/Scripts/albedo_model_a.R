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
        library(sjPlot)
        library(mgcv)

        
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
                        
                        #Add SWE to model data
                        for(i in 1:nrow(model_data)){
                                loc <- model_data[i, "LocalityName"]
                                mt <- model_data[i, "Month"]
                                
                                swe <- clim$SWE_mm[clim$LocalityName == loc & clim$Month == mt]
                                
                                model_data[i, "SWE_mm"] <- swe
                                
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
                numerical <- model_data[,c(8,10,12:16)]
                
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
     
#Using the general model-building procedures recommended by Heinze et al. (2018)
                
        #STEP 1: Define the most interesting model ---------
                
                #Ensure that Month is a factor
                model_data$Month <- as.factor(model_data$Month)
                
                #Define model (LOG TRANSFORMATION MAKES RESIDUAL PLOTS LOOK BETTER)
                model_a <- lmer(log(Plot_Albedo) ~ Treatment +
                                        Years_Since_Exclosure +
                                        Productivity_Index +
                                        Snow_Prop +
                                        Region +
                                        Treatment*Years_Since_Exclosure +
                                        Treatment*Snow_Prop +
                                        Treatment*Region +
                                        (1 | LocalityName) + (1 | Month),
                                data = model_data)
                
                summary(model_a)
               
                #Quick check of residuals
                plot(model_a) #Looks OK - some weird variation at intermediate values
                hist(resid(model_a)) #Normally distributed residuals
                qqmath(resid(model_a)) #Relatively normal residuals
                
                #Let's investigate residuals vs explanatory variables for trends
                plot(resid(model_a) ~ model_data$Years_Since_Exclosure) #Looks linear 
                plot(resid(model_a) ~ model_data$Productivity_Index) #No clear trends
                plot(resid(model_a) ~ model_data$Snow_Prop) #Looks somewhat linear, but lots of variation at intermediate values
                
                test <- cbind(resid(model_a), model_data)
                test <- test[test$Years_Since_Exclosure == 5,]
                ggplot(data = test, aes(x = Snow_Prop, y = test$`resid(model_a)`)) +
                        geom_point() +
                        facet_wrap(~ LocalityCode) #When looking at each LocalityCode, looks pretty linear
                
                ggplot(data = model_data, aes(x = Treatment, y = resid(model_a))) +
                        geom_boxplot() +
                        theme_bw() #Very similar residuals between treatments
                ggplot(data = model_data, aes(x = Region, y = resid(model_a))) +
                        geom_boxplot() +
                        theme_bw() #Telemark has more positive residuals, while Hedmark & Trøndelag have more neg's
                ggplot(data = model_data, aes(x = LocalityName, y = resid(model_a))) +
                        geom_boxplot() +
                        theme_bw()
                ggplot(data = model_data, aes(x = Month, y = resid(model_a))) +
                        geom_boxplot() +
                        theme_bw() #Pretty similar residuals across months (largest spread in April)
                
                
                ##KEY POINTS:
                
                        #Log transformation is necessary - lots of variation in residuals at intermediate values
                        ##of avg. snow prop
                
                        #Heterogenous variance (larger residuals for intermediate fitted values)
                
                
        

        #NOT SURE WHAT TO DO NEXT - POSSIBLY FIT SOME TYPE OF CUSTOM VARIANCE STRUCTURE (NOT SURE HOW TO DO THIS)
                
                
                
                
        #DIAGNOSTIC PLOTS
                
                #Put model residuals and fitted values into df        
                resid <- data.frame("Residuals" = resid(model_a),
                                    "Fitted" = fitted(model_a))
                
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
                p2 <- qqmath(resid(model_a))
                p3 <- plot(model_a)
                p4 <- plot(model_a, sqrt(abs(resid(., scaled=TRUE))) ~ fitted(.),
                           type = c("p", "smooth"))
                p5 <- plot(model_a, resid(., scaled=TRUE) ~ fitted(.) | Treatment,
                           abline = 0, type = c("p", "smooth"), layout = c(2,1))
                
                #Assemble complex plot
                top_row <- plot_grid(p1, NULL, p2, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))
                middle_row <- plot_grid(p3, NULL, p4, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))
                bottom_row <- plot_grid(p5, NULL, NULL, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))
                
                
                complex_plot <- plot_grid(top_row, NULL, middle_row, NULL, bottom_row, ncol = 1, rel_heights = c(0.31, 0.035, 0.31, 0.035, 0.31))
                complex_plot
                
                
                
        
                
                
#END MODEL A --------------------------------------------------------------------------------------
 
      