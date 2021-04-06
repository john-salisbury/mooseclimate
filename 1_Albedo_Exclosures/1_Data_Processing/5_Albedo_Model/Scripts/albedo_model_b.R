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

        ## Load model data (from model A)
        model_data <- read.csv("1_Albedo_Exclosures/1_Data_Processing/5_Albedo_Model/Output/albedo_model_data.csv", header = T)


                
#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#FILTER TO BROWSED SITES -----------------------------------------------------------------------------------
        
        #Filter to "B" treatment
        model_data <- model_data[model_data$Treatment == "B",]
      
#END FILTER TO BROWSED SITES --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#MODEL B --------------------------------------------------------------------------------------
     
#Using the general model-building procedures recommended by Heinze et al. (2018)
                
        #STEP 1: Define the most interesting model ---------
                
                #Ensure that Month is a factor
                model_data$Month <- as.factor(model_data$Month)
                
                #Define model (LOG TRANSFORMATION MAKES RESIDUAL PLOTS LOOK BETTER)
                model_b <- lmer(log(Plot_Albedo) ~ Moose_Density +
                                        Years_Since_Exclosure +
                                        Productivity_Index +
                                        Snow_Prop +
                                        Region +
                                        Moose_Density*Years_Since_Exclosure +
                                        Moose_Density*Snow_Prop +
                                        Moose_Density*Region +
                                        (1 | LocalityName) + (1 | Month),
                                data = model_data)
                
                summary(model_b)
               
                #Quick check of residuals
                plot(model_b) #Looks OK - some weird variation at intermediate values
                hist(resid(model_b)) #Normally distributed residuals
                qqmath(resid(model_b)) #Not great
                
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
 
      