## Script for LMM of forest biomass (at the PLOT resolution)

## Model B - Effect of moose density, for:

        ## Total biomass (kg/m2) at the plot level

##NOTE - this model only uses data from browsed sites


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

        ## MODEL DATA (FROM MODEL A)

                model_data <- read.csv("1_Albedo_Exclosures/1_Data_Processing/7_Biomass_Model/Output/biomass_model_data.csv", header = T)

                
#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#ISOLATE DATA TO BROWSED PLOTS -----------------------------------------------------------------------------------

        #Filter to plots w/ browsed treatment
        browsed <- model_data[model_data$Treatment == "B",]
                
#END ISOLATE DATA TO BROWSED PLOTS -----------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

        
        
        
#TOTAL BIOMASS MODEL --------------------------------------------------------------------------------------
     
#Using the general model-building procedures recommended by Heinze et al. (2018)

        #STEP 1: Define the most interesting model ---------
        
                #Define model
                model_b <- lmer(Mean_Plot_Biomass_kg_m2 ~ Moose_Density +
                                        Years_Since_Exclosure +
                                        Productivity_Index +
                                        Region +
                                        Moose_Density*Years_Since_Exclosure +
                                        Moose_Density*Region +
                                        (1 | LocalityName),
                                data = browsed)
        
                        #Quick check of residuals
                        plot(model_b) #Heteroskedasticity (like Model A)
                        hist(resid(model_b)) #Skewed w/ right tail
                        qqmath(resid(model_b)) #Not great
                        
                                #Let's investigate residuals vs explanatory variables for trends
                                plot(resid(model_b) ~ browsed$Years_Since_Exclosure) #Looks relatively linear, but with weird variation
                                plot(resid(model_b) ~ browsed$Productivity_Index) #No clear trends
                                plot(resid(model_b) ~ browsed$Moose_Density) #Relatively linear
                                
                ##KEY POINT: Some type of transformation is probably necessary
                      
                                          
        #STEP 2: Transform model to address heteroskedasticity --------
                                
                #Try a log-transformation
                model_b1 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Moose_Density +
                                             Years_Since_Exclosure +
                                             Productivity_Index +
                                             Region +
                                             Moose_Density*Years_Since_Exclosure +
                                             Moose_Density*Region +
                                        (1 | LocalityName),
                                 data = browsed)
                
                        #Quick check of residuals
                        plot(model_b1) #Looks much more homoskedastic
                        hist(resid(model_b1)) #Normally distributed residuals
                        qqnorm(resid(model_b1)) #Looks better

                        
        #STEP 3: Compare nested models with AIC ---------
                        
                #Re-fit model from Step 2 w/ ML (for comparison of fixed effects)
                model_b1 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Moose_Density +
                                         Years_Since_Exclosure +
                                         Productivity_Index +
                                         Region +
                                         Moose_Density*Years_Since_Exclosure +
                                         Moose_Density*Region +
                                         (1 | LocalityName),
                                 data = browsed,
                                 REML = F)
                        
                        summary(model_b1) 
                        
                #Nested model without Productivity Index
                        
                        #Model A2
                        model_b2 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Moose_Density +
                                                 Years_Since_Exclosure +
                                                 Region +
                                                 Moose_Density*Years_Since_Exclosure +
                                                 Moose_Density*Region +
                                                 (1 | LocalityName),
                                         data = browsed,
                                         REML = F)
                        
                                #Investigate summary
                                summary(model_b2) #Looks similar
                                
                                #Quick check of residual plots
                                plot(model_b2) #Looks very similar
                                hist(resid(model_b2)) #Relatively ormally distributed residuals
                                qqnorm(resid(model_b2)) #Looks great
                        
                        
                #Simple model version w/ temporal correlation structure (using nlme)
                library(nlme)
                model_b_temp <- lme(log(Mean_Plot_Biomass_kg_m2) ~ Moose_Density +
                                            Years_Since_Exclosure +
                                            Productivity_Index +
                                            Region +
                                            Moose_Density*Years_Since_Exclosure +
                                            Moose_Density*Region,
                                    random = ~1 | LocalityName,
                                    data = browsed,
                                    correlation = corCompSymm(form =~ Years_Since_Exclosure | LocalityName),
                                    method = "ML")
                
                plot(model_b_temp)
                
                #Summarize
                summary(model_b_temp)
                
                
                #Simple model without Region variable
                        
                        #Model B3
                        model_b3 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Moose_Density +
                                                 Years_Since_Exclosure +
                                                 Productivity_Index +
                                                 Moose_Density*Years_Since_Exclosure +
                                                 (1 | LocalityName),
                                         data = browsed,
                                         REML = F)
                        
                        #Investigate summary
                        summary(model_b3) #Looks similar
                        
                        #Quick check of residual plots
                        plot(model_b3) #Looks very similar
                        hist(resid(model_b3)) #Relatively ormally distributed residuals
                        qqnorm(resid(model_b3)) 
                        
                        
                        
                #Simplest model with only moose density and years since exclosure
                        
                        #Model B4
                        model_b4 <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Moose_Density*Years_Since_Exclosure +
                                                 (1 | LocalityName),
                                         data = browsed,
                                         REML = F)
                        
                        #Investigate summary
                        summary(model_b4) #Looks similar
                        
                        #Quick check of residual plots
                        plot(model_b4) #Looks very similar
                        hist(resid(model_b4)) #Relatively normally distributed residuals
                        qqnorm(resid(model_b4)) #Looks OK
                        
                                
                                
        #STEP 4: AIC comparison of models -----------
                
                AIC(model_b1, model_b2, model_b3, model_b4, model_b_temp) 
                        
                        #Model B3 has the lowest AIC value (may indicate that keeping productivity index improved
                        #fit)
                        
                                
        
        #STEP 5: Re-run model with REML for most accurate parameter estimates --------
                
                final_model_b <- lmer(log(Mean_Plot_Biomass_kg_m2) ~ Moose_Density +
                                              Years_Since_Exclosure +
                                              Productivity_Index +
                                              Moose_Density*Years_Since_Exclosure +
                                              (1 | LocalityName),
                                      data = browsed,
                                      REML = T)
                        
                plot(final_model_b)
                

                                
        #STEP 6: Double-check residual plots to validate ----------
                
                #Put model residuals and fitted values into df        
                resid <- data.frame("Residuals" = resid(final_model_b),
                                    "Fitted" = fitted(final_model_b))
                        
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
                p2 <- qqmath(resid(final_model_b))
                p3 <- plot(final_model_b)
                p4 <- plot(final_model_b, sqrt(abs(resid(., scaled=TRUE))) ~ fitted(.),
                           type = c("p", "smooth"))

                #Assemble complex plot
                top_row <- plot_grid(p1, NULL, p2, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))
                middle_row <- plot_grid(p3, NULL, p4, ncol = 3, rel_widths = (c(0.45,0.1,0.45)))

                
                complex_plot <- plot_grid(top_row, NULL, middle_row, ncol = 1, rel_heights = c(0.475, 0.05, 0.475))
                complex_plot
                
                
                                
        #STEP 7: Back-transform log for estimates -----------
        
                #Summarize model
                summary(final_model_b)

        
#END TOTAL BIOMASS MODEL --------------------------------------------------------------------------------------