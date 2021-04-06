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
                


        
#END EXPLORATORY DIAGNOSTIC PLOTS -----------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#MODEL A --------------------------------------------------------------------------------------
     
        #Going to investigate using an LMM
        #Ref: Zuur et al. (2009)
        
        #STEP 1: Beyond optimal model w/ ML
                
                #Model form (FIT WITH REML TO ALLOW COMPARISON OF RANDOM STRUCTURE)
                m1 <- lmer(Plot_Albedo ~ Treatment*Years_Since_Exclosure*Snow_Prop*Productivity_Index*Region +
                                (1 | LocalityName), data = model_data, REML = F)
                
                        #Residuals check
                        plot(m1)
                        
                                #Very bad heteroskedasticity - need to try some transformations
                        
                
                
        #STEP 2: Transform 'beyond optimal' model to address heteroskedasticity --------
                
                #Log-transformation of outcome
                m1_trans <- lmer(log(Plot_Albedo) ~ Treatment*Years_Since_Exclosure*Snow_Prop*Productivity_Index*Region +
                                         (1 | LocalityName), data = model_data, REML = F)
                        
                        #Residuals check
                        plot(m1_trans) #Still not great
                        
                        
                #Add polynomial term for snow_prop (which is not linear)
                m1_trans <- lmer(Plot_Albedo ~ Treatment*Years_Since_Exclosure*Snow_Prop*I(Snow_Prop^2)*Productivity_Index*Region +
                                         (1 | LocalityName), data = model_data, REML = F)
                
                        #Residuals check
                        plot(m1_trans) #Slightly better
                        
                
                #Add log-transformation and polynomial for snow_prop
                m1_trans <- lmer(log(Plot_Albedo) ~ Treatment*Years_Since_Exclosure*Snow_Prop*I(Snow_Prop^2)*Productivity_Index*Region +
                                         (1 | LocalityName), data = model_data, REML = F)
                
                        #Residuals check
                        plot(m1_trans) #Slightly better
                
                
                
        
                
                
#END MODEL A --------------------------------------------------------------------------------------
                
                
                
        model_data$Snow_Level[model_data$Snow_Prop > 0.5] <- "High"
        model_data$Snow_Level[model_data$Snow_Prop <= 0.5] <- "Low"
        
        model_data$Month <- as.factor(model_data$Month)

#ONE BELOW HAD GREAT RESIDUALS - GO WITH THIS
        m2 <- lmer(log(Plot_Albedo) ~ Treatment*Years_Since_Exclosure*Snow_Prop*I(Snow_Prop^2)*I(Snow_Prop^3)*Productivity_Index*Region*Avg_Decid_Prop +
                           (1 | LocalityName) + (1 | Month),
                   data = model_data)
        
        m2 <- lmer(Plot_Albedo ~ Treatment*Years_Since_Exclosure +
                           Snow_Prop +
                           I(Snow_Prop^2) +
                           I(Snow_Prop^3) +
                           I(Snow_Prop^4) +
                           Productivity_Index +
                           Region +
                           Avg_Decid_Prop +
                           (1 | LocalityName) + (1 | Month),
                   data = model_data)
        
        plot(m2)
        summary(m2)            
                        
                        

                #Assess assumptions of LMM
        
                        #(1) Linearity of relationship between response and predictor
                        plot(m2, type = c("p", "smooth"))
                        
                                #Strange residual plot
                
                        #(2) Linearity of relationship between response and predictor (within treatment)
                        plot(m2, resid(., scaled=TRUE) ~ fitted(.) | Treatment,
                             abline = 0, type = c("p", "smooth"), layout = c(2,1))
                        
                                #Strange residual plots within both treatments
                        
                        #(3) Linearity of relationship between response and predictor (within LocalityName)
                        plot(m2, resid(., scaled=TRUE) ~ fitted(.) | LocalityName)
                        
                                #Strange residual plots at every site
                        
                        #(4) Homogeneity of residual variance
                        plot(m2, sqrt(abs(resid(., scaled=TRUE))) ~ fitted(.),
                             type = c("p", "smooth"))
                        
                                #Looks horrible
                        
                        #(5) Within-group errors are independent with normal distribution
                        qqmath(resid(m2)) #Not straight
                        hist(resid(m2)) #Looks slightly normal?
                        

                        #Plot predictors vs residuals
                        
                                #Years_Since_Exclosure
                                df <- data.frame(x = model_data$Years_Since_Exclosure, y = resid(m1))
                                ggplot(data = df, aes(x = x, y = y)) +
                                        geom_point(shape = 1)
                                
                                        #Looks reasonably evenly distributed
                                
                                
                                #Snow prop
                                df <- data.frame(x = model_data$Snow_Prop, y = resid(m1))
                                ggplot(data = df, aes(x = x, y = y)) +
                                        geom_point(shape = 1) +
                                        labs(x = "Monthly Avg. Proportion of Days w/ Snow", y = "Model A Residuals") +
                                        theme_bw()
                                
                                        #SNOW PROP HAS NON-LINEAR RELATIONSHIP W/ RESIDUALS (probably responsible for weird trends
                                        #in residual plots)
                                
                                #Productivity Index
                                df <- data.frame(x = model_data$Productivity_Index, y = resid(m1))
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
                        ggplot(data = model_data, aes(x = I(Snow_Prop^2), y = Plot_Albedo)) +
                                geom_point(shape = 1) +
                                geom_smooth(method = lm) +
                                theme_bw() +
                                labs(x = "(Monthly Avg. Proportion of Days w/ Snow)^2", y = "Albedo") +
                                theme(
                                        axis.title.x = element_text(margin = margin(t = 6), size = 14),
                                        axis.title.y = element_text(margin = margin(r = 6), size = 14),
                                        axis.text.x = element_text(angle = 90, hjust = 1)
                                )
                        
        
                #Save parameter estimates as table
                tab_model(m1, digits = 4)
                
                
                
                
                
#TRY MODEL WITH SWE INSTEAD OF SNOW PROP ---------------
                
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
                
                
                #Plot transformations of SWE vs albedo
                plot(model_data$Plot_Albedo ~ model_data$SWE_mm) #logistic
                plot(model_data$Plot_Albedo ~ log(model_data$SWE_mm)) #Worse
                plot(model_data$Plot_Albedo ~ sqrt(model_data$SWE_mm)) #Still nonlinear
                plot(log(model_data$Plot_Albedo) ~ I(model_data$SWE_mm^2)) #Much worse
                plot(log(model_data$Plot_Albedo) ~ model_data$SWE_mm) #Same (still bad)

                
                        #SWE DOES NOT WORK WELL IN MIXED EFFECTS MODEL (IMPOSSIBLE TO GET LINEAR TRANSFORMATION)
                        #SNOW PROP SEEMS TO BE BETTER COVARIATE
                
                
#TRY ADDING TREE PROPORTIONS (DECIDUOUS PROP, IN PARTICULAR) ---------------------
        
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
        

        #Try model
        m2 <- lmer(log(Plot_Albedo) ~ Treatment*Years_Since_Exclosure*Snow_Prop*I(Snow_Prop^2)*Productivity_Index*Avg_Decid_Prop*Month +
                           (1 | LocalityName), data = model_data, REML = T)
        
                plot(m2)
        
        model_data$Month <- as.factor(model_data$Month)
        m2 <- lme(Plot_Albedo ~ Treatment +
                          Years_Since_Exclosure +
                          Month +
                          Snow_Prop +
                          I(Snow_Prop^2) +
                          Productivity_Index +
                          Region +
                          Treatment*Years_Since_Exclosure +
                          Treatment*Month +
                          Month*Years_Since_Exclosure,
                    random = ~1 | LocalityName,
                    data = model_data,
                    correlation = corCompSymm(form =~ Years_Since_Exclosure | LocalityName), method = "ML")
        
        plot(m2)
        summary(m2)
        
                #DID NOT HELP
        

        
        
        
                
                
                
        #KEY FINDINGS FROM MODEL #1 - SNOW PROP HAS A NON-LINEAR RELATIONSHIP WITH ALBEDO 
        
                #NEXT STEP - TRY TRANSFORMATIONS
                
                        #Which transformation is appropriate?
                        plot(model_data$Plot_Albedo ~ model_data$Snow_Prop) #Not linear
                        plot(model_data$Plot_Albedo ~ I(model_data$Snow_Prop^2)) #Somewhat linear
                        plot(model_data$Plot_Albedo ~ I(model_data$Snow_Prop^3)) #Not as good as I^2
                        plot(model_data$Plot_Albedo ~ log(model_data$Snow_Prop)) #Terrible
                        plot(log(model_data$Plot_Albedo) ~ model_data$Snow_Prop) #Reasonable?
                        
                        
                        #TRY LOG TRANSFORMATION OF ALBEDO IN 'BEYOND OPTIMAL' MODEL
                        
                                #Model form (FIT WITH REML TO ALLOW COMPARISON OF RANDOM STRUCTURE)
                                m2 <- lmer(Plot_Albedo ~ Treatment*Years_Since_Exclosure*Snow_Prop*I(Snow_Prop^2)*Productivity_Index +
                                                   (1 | LocalityName), data = model_data, REML = T)
                                
                                
                                #Assess assumptions of LMM
                                
                                #(1) Linearity of relationship between response and predictor
                                plot(m2, type = c("p", "smooth"))
                                summary(m2)
                                
                                
                        
                        
        #WHAT MISSING VARIABLE COULD BETTER ACCOUNT FOR VARIATION IN ALBEDO
                
                #HIGH VARIATION AT MODERATE & HIGH SNOW, LOW VARIATION AT LOW SNOW (Which makes sense)
                        
                        #INCLUDING avg. PROPORTION of deciduous trees could help with variation
                                
                                
                                
                                
                m3 <- lme(Plot_Albedo ~ Treatment*Years_Since_Exclosure*Productivity_Index*Region,
                          random = ~ 1|LocalityName,
                          correlation = corCompSymm(form = ~Years_Since_Exclosure|LocalityName),
                          data = model_data)
         
                plot(m3, type = c("p", "smooth"))
                
                plot(model_data$Plot_Albedo ~ model_data$Avg_Decid_Prop)
                plot(model_data$Plot_Albedo ~ model_data$Productivity_Index)
                

                
#GAMM ----------
                
        m_gamm <- gamm(Plot_Albedo ~ Treatment +
                               Years_Since_Exclosure +
                               Productivity_Index +
                               Treatment*Years_Since_Exclosure +
                               s(Snow_Prop),
                       data = model_data,
                       random = )
                

                plot(m_res)
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                       
        #MODEL B: Include (snow)^2
                
                #Model form
                m2 <- lmer(Plot_Albedo ~ Treatment +
                                        Years_Since_Exclosure +
                                        Snow_Prop +
                                        I(Snow_Prop^2) +
                                        Productivity_Index +
                                        Region +
                                        Treatment*Years_Since_Exclosure +
                                        Treatment*Snow_Prop +
                                        Treatment*I(Snow_Prop^2) +
                                        Treatment*Region +
                                        (1 | LocalityName), data = model_data)
                
                #Assess assumptions of LMM
                
                        #(1) Linearity of relationship between response and predictor
                        plot(model_b, type = c("p", "smooth"))
                        
                        #Residuals look slightly better
                        
                        #(2) Linearity of relationship between response and predictor (within treatment)
                        plot(model_b, resid(., scaled=TRUE) ~ fitted(.) | Treatment,
                             abline = 0, type = c("p", "smooth"), layout = c(2,1))
                        
                        #Strange residual plots within both treatments
                        
                        #(3) Linearity of relationship between response and predictor (within LocalityName)
                        plot(model_b, resid(., scaled=TRUE) ~ fitted(.) | LocalityName)
                        
                        #Strange residual plots at every site
                        
                        #(4) Homogeneity of residual variance
                        plot(model_b, sqrt(abs(resid(., scaled=TRUE))) ~ fitted(.),
                             type = c("p", "smooth"))
                        
                        #Looks horrible
                        
                        #(5) Within-group errors are independent with normal distribution
                        qqmath(model_b) #Not straight
                        hist(resid(model_b)) #Looks slightly normal?
                        
                #Generate model table
                tab_model(model_b, digits = 4)
                        
                plot(Plot_Albedo~I(Snow_Prop^2), data = model_data)
                
                
        
                
        #AIC Comparison of two models:
                
                #Run classical AIC w/ penalty = 2
                AIC(model_a, model_b)
                
                
        
                
#END MODEL 1: LMM --------------------------------------------------------------------------------------
                
                
                
                
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#TRY MODEL W/ BIOMASS OR VOLUME -------------------------------------------------------------------------------------------------
                
        #Import biomass CSV
        bio <- read.csv("1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_biomass.csv", header = T)
                
        #Add placeholder column to model_data
        model_data$Biomass_kg_m2 <- as.numeric('')
        
        #Loop through model data and add
        for(i in 1:nrow(model_data)){
                
                print(i)
                
                #Get variables
                loc <- model_data[i, "LocalityCode"]
                yr <- model_data[i, "Years_Since_Exclosure"]
                
                #Get biomass
                bio_val <- bio$Mean_Plot_Biomass_kg_m2[bio$LocalityCode == loc & bio$Years_Since_Exclosure == yr]
                
                #Add biomass to row
                model_data[i, "Biomass_kg_m2"] <- bio_val
        }
        
        #Test correlations
        cor.test(model_data$Biomass_kg_m2, model_data$Years_Since_Exclosure) #Biomass is highly correlated w/ Years Since Exclosure
        cor.test(model_data$Biomass_kg_m2, model_data$Productivity_Index) #Biomass is strongly correlated w/ productivity index
        cor.test(model_data$Biomass_kg_m2, model_data$Snow_Prop) #NOT correlated
        
        #Build model
        model_c <- lmer(Plot_Albedo ~ Treatment +
                                Snow_Prop +
                                I(Snow_Prop^2) +
                                Biomass_kg_m2 +
                                Region +
                                Treatment*Snow_Prop +
                                Treatment*I(Snow_Prop^2) +
                                Treatment*Biomass_kg_m2 + 
                                Treatment*Region +
                                (1 | LocalityName), data = model_data)
        
        plot(model_c, type = c("p", "smooth"))
        summary(model_c)
        
        
                #INCLUDING BIOMASS DOES NOT SOLVE TREND - specifically w/ Snow
                
                
#END TRY MODEL W/ BIOMASS OR VOLUME ----------------------------------------------------------------------------------------------
                
