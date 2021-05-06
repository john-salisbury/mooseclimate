## Script to backfit height-only biomass models from already established 
## biomass models (FOR BROWSED PLOTS ONLY) - Kolstad et al. (2018)


##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(cowplot)
        
        #Packages for models
        library(lme4)

###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        #Import CSV to dataframe
        data <- read.csv('1_Albedo_Exclosures/z_Data_Library/Tree_Data/Usable_Data/sustherb_tree_data.csv', header = TRUE)

#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#DATA FILTERING ----------------------------------------------------------------------------------------------

        #Summary: 
        ##  The code below filters data to that is from Trøndelag, the year 2016, and trees under 6m in height.
        ##  Data from 2016 browsed plots in Trøndelag will be used to backfit/generate "height-only" biomass models
        ##  This is done since 2016 is the only year with 'Diameter at Ground Level' (DGL) measurements for all trees
        ##  Additionally, Trøndelag is the only region with these complete measurements
        ##  Also, height-only biomass models already exist in Kolstad et al. (2018)
        ##  Finally, as in Kolstad et al., trees over 6m in 2016 are likely leftover from forestry operations - thus they
        ##  are excluded.

        #Filter to 'Trøndelag' region only
        ##NOTE: This reduces the number of study sites (LocalityName) to 15 (w/ 30 total plots)
        trondelag <- data[data$Region == 'Trøndelag',]
        
        #Format date column properly (to R date format)
        trondelag$X_Date <- as.Date(as.character(trondelag$X_Date), format = "%m/%d/%y")
        
        #Format LocalityName and LocalityCode as factors
        trondelag$LocalityCode <- as.factor(trondelag$LocalityCode)
        trondelag$LocalityName <- as.factor(trondelag$LocalityName)
        
        #LocalityName to lowercase
        trondelag$LocalityName <- tolower(trondelag$LocalityName)
        
        #Filter to data from year 2016 only (and remove other df's)
        t2016 <- trondelag[trondelag$X_Date >= "2016-01-01" & trondelag$X_Date <= "2016-12-31",]
        rm(data)
        rm(trondelag)
        
        #Remove rows that are missing diameter-at-base data (3 total)
        t2016 <- t2016[! is.na(t2016$Diameter_at_base_mm), ]
        
        #Exclude trees > 6m (600cm)
        t2016 <- t2016[t2016$Height_cm <= 600,]
        
        #Filter to browsed plots
        t2016 <- t2016[t2016$Treatment == "B",]

#END DATA FILTERING -------------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#BIOMASS CALCULATION USING EXISTING MODELS ---------------------------------------------------------------
        
        #Add a blank column to hold biomass
        t2016$Biomass_g <- as.numeric('')
        
        # Loop through each row of the dataframe (2016, Trøndelag, <6m height, browsed)
        # and produce a volume estimate
        for(row in 1:nrow(t2016)){
                
                #IDENTIFY SPECIES + HEIGHT
                
                #Get tree species
                species <- as.character(t2016[row, "Taxa"])
                
                #Get 'Diameter at Ground Level' (mm)
                dgl <- as.numeric(t2016[row, "Diameter_at_base_mm"])
                
                #Get height (cm)
                height <- as.numeric(t2016[row, "Height_cm"])
                
                #CALCULATE BIOMASS FOR EACH TREE USING SUSTHERB ALLOMETRIC EQUATIONS
                
                #Betula pubescens model (Birch)
                if( species == "Betula pubescens (Bjørk)" || species == "Betula pendula (Lavlandbjørk)" || species == "Salix caprea (Selje)" ){
                        
                        #Biomass (g)
                        biomass <- (0.078843)*(dgl^2) + (0.009197)*(height^2)
                        
                }
                
                #Picea abies model (Spruce)
                else if( species == "Picea abies (Gran)" || species == "Juniperus communis (Einer)"){
                        
                        #Biomass (g)
                        biomass <- (0.020293)*(height^2) + (0.006092)*(dgl^3)
                        
                }
                
                #Pinus sylvestris model (Pine)
                else if( species == "Pinus sylvestris (Furu)" ){
                        
                        #Biomass (g)
                        biomass <- (0.325839)*(dgl^2) + (0.0007434)*(dgl^3)
                      
                }
                
                #Sorbus aucuparia model (Rowan)
                else if( species == "Sorbus aucuparia (Rogn)" ){
                                
                        #Biomass (g)
                        biomass <- (0.006664)*(height^2) + (0.082983)*(dgl^2)

                }
                
                #Add calculated biomass to 'Biomass' column
                t2016[row, "Biomass_g"] <- biomass
                
        }
        
        
#END BIOMASS CALCULATION USING EXISTING MODELS -----------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#MODEL BACKFITTING --------------------------------------------------------------------------------------
        
        #For each species, explore the relationship between height and modeled biomass ----------------
        
                #Spruce ------
                spruce_plot <- ggplot(data = subset(t2016, Taxa == "Picea abies (Gran)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth()
                spruce_plot
        
                        #Decent fit from first glance
        
                #Pine -------
                pine_plot <- ggplot(data = subset(t2016, Taxa == "Pinus sylvestris (Furu)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth()
                pine_plot
                
                        #Not so great fit
        
                #Birch --------
                birch_plot <- ggplot(data = subset(t2016, Taxa == "Betula pubescens (Bjørk)"), aes(x = Height_cm, y = Biomass_g)) +
                        geom_point() +
                        geom_smooth()
                birch_plot
                
                        #Excellent fit
        
                #Rowan --------
                rowan_plot <- ggplot(data = subset(t2016, Taxa == "Sorbus aucuparia (Rogn)"), aes(x = Height_cm, y = Biomass_g)) +
                        geom_point() +
                        geom_smooth()
                rowan_plot
                
                        #OK Fit
                
        #MODEL FIT + SELECTION ---------------------
                
        ##Same approach as Kolstad et al:
        ## "Biomass models were obtained for each species by starting with a saturated model including cubic,
        ## quadratic and linear terms for both height and DGL, and performing backwards elimination by removing
        ## the least significant terms first, and ending with the model with the highest adjusted R2-value
                
        ##NOTE: USING ADJUSTED R2 TO SELECT MODEL

                #Spruce ------------
                
                        #Fully saturated model ----
                
                                #Define model
                                s_full <- lm(Biomass_g ~ -1 + poly(Height_cm, 3, raw = T), data = subset(t2016, Taxa == "Picea abies (Gran)"))
                                summary(s_full)
                        
                                        #Only cubic term is significant
                                        #Adjusted R2 = 0.9038
                                
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Picea abies (Gran)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + poly(x, 3, raw = T))
                                
                                        #No negative values - seems to be a great fit
                                
                        #Cubic model without linear term ----
                                
                                #Define model
                                s_cub <- lm(Biomass_g ~ -1 + I(Height_cm^2) + I(Height_cm^3), data = subset(t2016, Taxa == "Picea abies (Gran)"))
                                summary(s_cub)
                                
                                        #Both quadratic and cubic terms significant
                                        #Same R2 as with linear term = 0.9045
                                
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Picea abies (Gran)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + I(x^2) + I(x^3))
                                
                        #Cubic term alone -----
                                
                                s_cub_only <- lm(Biomass_g ~ -1 + I(Height_cm^3), data = subset(t2016, Taxa == "Picea abies (Gran)"))
                                summary(s_cub_only)
                                
                                        #Lower adj. R2 (0.8921) - looks like cubic model with quadratic term is best
                        
                        #Quadratic model ----
                                
                                #Define model
                                s_quad <- lm(Biomass_g ~ -1 + poly(Height_cm, 2, raw = T), data = subset(t2016, Taxa == "Picea abies (Gran)"))
                                summary(s_quad)
                        
                                        #Looks like both terms are significant, so this form may be best
                                        #R2 = 0.9
                                
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Picea abies (Gran)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + poly(x, 2, raw = T))
                                
                                        #Appear to be negative values - don't think this model will work
                        
                        #Quadratic model without linear term ----
                                
                                #Define model
                                s_simple <- lm(Biomass_g ~ -1 + I(Height_cm^2), data = subset(t2016, Taxa == "Picea abies (Gran)"))
                                summary(s_simple)
                        
                                        #Worse R2 (0.8863) - looks like quadratic model without linear term is best
                                
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Picea abies (Gran)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + I(x^2))
                        
                                
                                
                        #MODEL CHOICE:
                        
                                #The cubic model with quadratic term and no linear term appears to have the best fit (highest R2 and fits the data
                                #well visually). Also, doesn't have negative coefficients (unlike quadratic)
                        
                                #SELECTED MODEL:  y = 0.02014x^2 + 0.00006086x^3
                                ggplot(data = subset(t2016, Taxa == "Picea abies (Gran)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + I(x^2) + I(x^3))
                                
                
                                
                #PINE ----------------------------------
                                
                        #Fully saturated model ----
                                
                                #Define model
                                p_full <- lm(Biomass_g ~ poly(Height_cm, 3, raw = T), data = subset(t2016, Taxa == "Pinus sylvestris (Furu)"))
                                summary(p_full)
                                
                                        #All terms are significant, but poor R2 value
                                        #R2 = 0.6776
                                
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Pinus sylvestris (Furu)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + poly(x, 3, raw = T))
                                
                                                #Don't expect a decrease in biomass at tall heights, so this model
                                                #doesn't make sense
                                
                        #Cubic model without linear term ----
                        
                                #Define model
                                p_cub <- lm(Biomass_g ~ I(Height_cm^2) + I(Height_cm^3), data = subset(t2016, Taxa == "Pinus sylvestris (Furu)"))
                                summary(p_cub)
                                
                                        #Worse R2 = 0.6688
                        
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Pinus sylvestris (Furu)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + I(x^2) + I(x^3))
                                
                                        #Model doesn't make biological sense (same as above)
                        
                        #Quadratic model ----
                        
                                #Define model
                                p_quad <- lm(Biomass_g ~ -1 + poly(Height_cm, 2, raw = T), data = subset(t2016, Taxa == "Pinus sylvestris (Furu)"))
                                summary(p_quad)
                                
                                        #Looks like both terms are significant, so this form may be best
                                        #R2 = 0.6611
                                
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Pinus sylvestris (Furu)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + poly(x, 2, raw = T))
                                
                                        #Looks OK, but not great
                                
                        #Quadratic model without linear term ----
                        
                                #Define model
                                p_simple <- lm(Biomass_g ~ -1 + I(Height_cm^2), data = subset(t2016, Taxa == "Pinus sylvestris (Furu)"))
                                summary(p_simple)
                        
                                        #Very poor R2 (0.5168) - looks like quadratic model with linear term is best
                        
                        #Plot model on data
                        ggplot(data = subset(t2016, Taxa == "Pinus sylvestris (Furu)"), aes(x = Height_cm, y = Biomass_g)) +
                                geom_point() +
                                geom_smooth(method = "lm", formula = y ~ -1 + I(x^2))
                        
                                #Doesn't look good
                        
                        
                        #Model choice:
                        
                        #The quadratic model w/ linear term has one of the highest R2 values,
                        #makes biological sense, and has two significant terms
                        
                                #What if I filter out height > 150? (Looks like the points over 200
                                #are dragging the model over)
                        
                                p_alt <- lm(Biomass_g ~ -1 + poly(Height_cm, 2, raw = T), data = subset(t2016, Taxa == "Pinus sylvestris (Furu)" & Height_cm < 150))
                                summary(p_alt)
                                
                                        #Much better R2 (0.6997), but linear term isn't significant - let's try removing it
                                
                                p_alt_2 <- lm(Biomass_g ~ -1 + I(Height_cm^2), data = subset(t2016, Taxa == "Pinus sylvestris (Furu)" & Height_cm < 150))
                                summary(p_alt_2)
                                
                                        #Almost identical R2 (0.7003) - let's go with this one
                                
                        #SELECTED MODEL:  y = 0.027464x^2  (R2 = 0.7015)
                        ggplot(data = subset(t2016, Taxa == "Pinus sylvestris (Furu)" & Height_cm < 150), aes(x = Height_cm, y = Biomass_g)) +
                                geom_point() +
                                geom_smooth(method = "lm", formula = y ~ -1 + I(x^2))
                        

                        
                
                #ROWAN ---------------------
                        
                        #Fully saturated model ----
                        
                                #Define model
                                r_full <- lm(Biomass_g ~ poly(Height_cm, 3, raw = T), data = subset(t2016, Taxa == "Sorbus aucuparia (Rogn)"))
                                summary(r_full)
                                
                                #No terms significant, poor R2 value
                                #R2 = 0.6418
                                
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Sorbus aucuparia (Rogn)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + poly(x, 3, raw = T))
                                
                                        #Looks reasonable
                        
                        #Cubic model without linear term ----
                        
                                #Define model
                                r_cub <- lm(Biomass_g ~ I(Height_cm^2) + I(Height_cm^3), data = subset(t2016, Taxa == "Sorbus aucuparia (Rogn)"))
                                summary(r_cub)
                                
                                        #Same R2 = 0.6404
                                
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Sorbus aucuparia (Rogn)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + I(x^2) + I(x^3))
                                
                                        #Model doesn't make biological sense 
                                
                        #Quadratic model ----
                        
                                #Define model
                                r_quad <- lm(Biomass_g ~ -1 + poly(Height_cm, 2, raw = T), data = subset(t2016, Taxa == "Sorbus aucuparia (Rogn)"))
                                summary(r_quad)
                                
                                        #Looks like both terms are significant, so this form may be best
                                        #Much higher R2 = 0.834
                                
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Sorbus aucuparia (Rogn)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + poly(x, 2, raw = T))
                                
                                        #Looks OK, but seems to be dragged upwards a bit
                        
                        #Quadratic model without linear term ----
                        
                                #Define model
                                r_simple <- lm(Biomass_g ~ -1 + I(Height_cm^2), data = subset(t2016, Taxa == "Sorbus aucuparia (Rogn)"))
                                summary(r_simple)
                                
                                #Worse R2 - looks like quadratic model with linear term is best
                                
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Sorbus aucuparia (Rogn)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + I(x^2))
                        
                                        #Looks OK
                        
                        
                        #MODEL CHOICE:
                        
                                #The quadratic model w/ linear term has one of the highest R2 values,
                                #makes biological sense, and has two significant terms
                        
                                #SELECTED MODEL:  y = 0.2620264x + 0.005844x^2  (R2 = 0.8345)
                                ggplot(data = subset(t2016, Taxa == "Sorbus aucuparia (Rogn)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + poly(x, 2, raw = T))
                                
                                
                                
                
                #BIRCH ---------------------
                        
                        #Fully saturated model ----
                        
                                #Define model
                                b_full <- lm(Biomass_g ~ poly(Height_cm, 3, raw = T), data = subset(t2016, Taxa == "Betula pubescens (Bjørk)"))
                                summary(b_full)
                                
                                        #Very good R value, quadratic term not significant
                                        #R2 = 0.9873
                        
                        #Plot model on data
                        ggplot(data = subset(t2016, Taxa == "Betula pubescens (Bjørk)"), aes(x = Height_cm, y = Biomass_g)) +
                                geom_point() +
                                geom_smooth(method = "lm", formula = y ~ -1 + poly(x, 3, raw = T))
                        
                                #Looks excellent
                        
                        #Cubic model without linear term ----
                        
                                #Define model
                                b_cub <- lm(Biomass_g ~ I(Height_cm^2) + I(Height_cm^3), data = subset(t2016, Taxa == "Betula pubescens (Bjørk)"))
                                summary(b_cub)
                                
                                        #Slightly worse R2 = 0.9869
                                
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Betula pubescens (Bjørk)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + I(x^2) + I(x^3))
                                
                                        #Model OK
                        
                        #Quadratic model ----
                        
                                #Define model
                                b_quad <- lm(Biomass_g ~ -1 + poly(Height_cm, 2, raw = T), data = subset(t2016, Taxa == "Betula pubescens (Bjørk)"))
                                summary(b_quad)
                        
                                        #Much higher R2 = 0.9898
                        
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Betula pubescens (Bjørk)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + poly(x, 2, raw = T))
                                
                                        #Excellent fit
                        
                        #Quadratic model without linear term ----
                        
                                #Define model
                                b_simple <- lm(Biomass_g ~ -1 + I(Height_cm^2), data = subset(t2016, Taxa == "Betula pubescens (Bjørk)"))
                                summary(b_simple)
                        
                                        #Slightly worse R2: 0.9879
                        
                                #Plot model on data
                                ggplot(data = subset(t2016, Taxa == "Betula pubescens (Bjørk)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + I(x^2))
                        
                                        #Looks good
                        
                        
                        #MODEL CHOICE:
                        
                                #The quadratic model w/ linear term has the highest R2 value,
                                #makes biological sense, and has two significant terms
                                
                                #SELECTED MODEL:  y = 0.2072x + 0.009974x^2  (R2 = 0.9898)
                                ggplot(data = subset(t2016, Taxa == "Betula pubescens (Bjørk)"), aes(x = Height_cm, y = Biomass_g)) +
                                        geom_point() +
                                        geom_smooth(method = "lm", formula = y ~ -1 + poly(x, 2, raw = T))
                                
                        
        
        #SO, FINAL MODELS ARE BELOW:
                
                #Spruce (browsed) ------
                
                        #Equation:  y = 0.02014x^2 + 0.00006086x^3
                        #Model in R:  lm(Biomass_g ~ -1 + I(Height_cm^2) + I(Height_cm^3), data = subset(t2016, Taxa == "Picea abies (Gran)"))
                        #R2 value: 0.9059
                        
                #Pine (browsed) ------
                                
                        #Equation:  y = 0.027464x^2
                        #Model in R:  lm(Biomass_g ~ -1 + I(Height_cm^2), data = subset(t2016, Taxa == "Pinus sylvestris (Furu)" & Height_cm < 150))
                        #R2 value: 0.7015
                                
                #Birch (browsed) ------
                                
                        #Equation:  y = 0.2072x + 0.009974x^2
                        #Model in R:  lm(Biomass_g ~ -1 + poly(Height_cm, 2, raw = T), data = subset(t2016, Taxa == "Betula pubescens (Bjørk)"))
                        #R2 value: 0.9898
                                
                #Rowan (browsed) ------
                                
                        #Equation:  y = 0.2620264x + 0.005844x^2
                        #Model in R:  lm(Biomass_g ~ -1 + poly(Height_cm, 2, raw = T), data = subset(t2016, Taxa == "Sorbus aucuparia (Rogn)"))
                        #R2 value: R2 = 0.8345
                
                                


#END MODEL BACKFITTING ----------------------------------------------------------------------------------        
        
        
   
                                     
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#EXPORT PLOTS --------------------------------------------------------------------------------------
                                
        #Birch ------
        
                #Define equation:
                be <- paste("y == ", "0.2072*X + 0.009974*X^2", sep ="")  
                
                #Define R2 value:
                br <- paste("R^2 == ", "0.9898", sep ="")
                
                g1 <- ggplot(data = subset(t2016, Taxa == "Betula pubescens (Bjørk)"), aes(x = Height_cm, y = Biomass_g)) +
                        geom_point(shape = 1) +
                        geom_smooth(method = "lm", formula = y ~ -1 + poly(x, 2, raw = T), se = F, color = "black") +
                        labs(x = "Height (cm)", y = "Modelled Biomass (g)") +
                        ggtitle("(a)") +
                        theme_bw() +
                        theme(
                                legend.position = "none",
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10)),
                                plot.title = element_text(face = "bold"),
                                panel.grid = element_blank()
                        ) +
                        annotate("text", x = 20, y = 3000, hjust = 0, vjust = 1, size = 3, label = paste(be, br, sep = "\n"), parse=TRUE) +
                        annotate("text", x = 20, y = 2800, hjust = 0, vjust = 1, size = 3, label = br, parse=TRUE)
                g1
                
                
                
        #Rowan ------
                
                #Define equation:
                re <- paste("y == ", "0.2620264*X + 0.005844*X^2", sep ="")  
                
                #Define R2 value:
                rr <- paste("R^2 == ", "0.8345", sep ="")
                
                g2 <- ggplot(data = subset(t2016, Taxa == "Sorbus aucuparia (Rogn)"), aes(x = Height_cm, y = Biomass_g)) +
                        geom_point(shape = 1) +
                        geom_smooth(method = "lm", formula = y ~ -1 + poly(x, 2, raw = T), se = F, color = "black") +
                        labs(x = "Height (cm)", y = "Modelled Biomass (g)") +
                        ggtitle("(b)") +
                        theme_bw() +
                        theme(
                                legend.position = "none",
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10)),
                                plot.title = element_text(face = "bold"),
                                panel.grid = element_blank()
                        ) +
                        annotate("text", x = 5, y = 124, hjust = 0, vjust = 1, size = 3, label = re, parse=TRUE) +
                        annotate("text", x = 5, y = 115, hjust = 0, vjust = 1, size = 3, label = rr, parse=TRUE)
                g2
                

        #Pine ------
                
                #Define equation:
                pe <- paste("y == ", "0.027464*X^2", sep ="")  
                
                #Define R2 value:
                pr <- paste("R^2 == ", "0.7015", sep ="")
                
                g3 <- ggplot(data = subset(t2016, Taxa == "Pinus sylvestris (Furu)" & Height_cm < 150), aes(x = Height_cm, y = Biomass_g)) +
                        geom_point(shape = 1) +
                        geom_smooth(method = "lm", formula = y ~ -1 + I(x^2), se = F, color = "black") +
                        labs(x = "Height (cm)", y = "Modelled Biomass (g)") +
                        ggtitle("(c)") +
                        theme_bw() +
                        theme(
                              legend.position = "none",
                              axis.title.x = element_text(margin = margin(t = 10)),
                              axis.title.y = element_text(margin = margin(r = 10)),
                              plot.title = element_text(face = "bold"),
                              panel.grid = element_blank()
                              ) +
                        annotate("text", x = 5, y = 580, hjust = 0, vjust = 1, size = 3, label = pe, parse=TRUE) +
                        annotate("text", x = 5, y = 540, hjust = 0, vjust = 1, size = 3, label = pr, parse=TRUE)
                g3
                        
                        
        #Spruce -----
                
                #Define equation:
                se <- paste("y == ", "0.02014*X^2 + 0.00006086*X^3", sep ="")  
                
                #Define R2 value:
                sr <- paste("R^2 == ", "0.9059", sep ="")
                
                g4 <- ggplot(data = subset(t2016, Taxa == "Picea abies (Gran)"), aes(x = Height_cm, y = Biomass_g)) +
                        geom_point(shape = 1) +
                        geom_smooth(method = "lm", formula = y ~ -1 + I(x^2) + I(x^3), se = F, color = "black") +
                        labs(x = "Height (cm)", y = "Modelled Biomass (g)") +
                        ggtitle("(d)") +
                        theme_bw() +
                        theme(
                                legend.position = "none",
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10)),
                                plot.title = element_text(face = "bold"),
                                panel.grid = element_blank()
                        ) +
                        annotate("text", x = 15, y = 18000, hjust = 0, vjust = 1, size = 3, label = se, parse=TRUE) +
                        annotate("text", x = 15, y = 16500, hjust = 0, vjust = 1, size = 3, label = sr, parse=TRUE)
                
                g4
                
                
        #COMPLEX/PANELED PLOT -----
                top <- plot_grid(g1, NULL, g2, ncol = 3, rel_widths = c(0.475, 0.05, 0.475))
                bottom <- plot_grid(g3, NULL, g4, ncol = 3, rel_widths = c(0.475, 0.05, 0.475))
                complex <- plot_grid(top, NULL, bottom, ncol = 1, rel_heights = c(0.475, 0.05, 0.475))
                complex
                
                
                
        
        

#END EXPORT PLOTS ---------------------------------------------------------------------------------- 
