## Script to diagnose "weird" sites with more biomass/carbon in open plots vs exclosures

## James suggested two distinct mechanisms here:

        ## (1) At low productivity sites, higher biomass in open plots may be due to chance (i.e. 
        ## a few remaining, large trees)

        ## (2) At high productivity sites, higher biomass in open plots may be due to moose exclosure 
        ## (we're seeing coniferous facilitation, but no increase in deciduous species - moose may remove
        ## competition from field layer species to coniferous species - any increase in field layer species
        ## would not be seen in our records)

                #Plot delta biomass (total and species-specific) for sites across productivity



##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(ggpubr)
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

        ## USE MODEL DATA
        model_data <- read.csv("1_Albedo_Exclosures/1_Data_Processing/7_Biomass_Model/Output/biomass_model_data.csv", header = T)
        model_data$Treatment <- as.factor(model_data$Treatment)
                
                
#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        

        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#CALCULATE DELTA BIOMASS -----------------------------------------------------------------------------------

        #Add "coniferous biomass" column to model data
        model_data$Conif_Bio <- model_data$Pine_Bio + model_data$Spruce_Bio
        
        #Get list of sites to loop through
        sites <- levels(as.factor(model_data$LocalityName)) #44 total
        
        #Limit to 7 years since exclosure
        dat <- model_data[model_data$Years_Since_Exclosure == "7",]
        
        #Placeholder df
        new <- data.frame("LocalityName" = as.character(),
                          "Delta_Total_Bio" = as.numeric(),
                          "Delta_Decid_Bio" = as.numeric(),
                          "Delta_Conif_Bio" = as.numeric(),
                          "Productivity_Index" = as.numeric())
        
        #Loop through
        for(i in 1:length(sites)){
                
                #Get browsed & unbrowsed plots
                b <- dat$LocalityCode[dat$LocalityName == sites[i] & dat$Treatment == "B"]
                ub <- dat$LocalityCode[dat$LocalityName == sites[i] & dat$Treatment == "UB"]
                
                #Calculate delta (excl - open)
                
                        #Total
                        td <- dat$Mean_Plot_Biomass_kg_m2[dat$LocalityCode == ub] - dat$Mean_Plot_Biomass_kg_m2[dat$LocalityCode == b]
                        
                        #Deciduous
                        dd <- dat$Decid_Bio[dat$LocalityCode == ub] - dat$Decid_Bio[dat$LocalityCode == b]

                        #Coniferous
                        cd <- dat$Conif_Bio[dat$LocalityCode == ub] - dat$Conif_Bio[dat$LocalityCode == b]
                        
                #Add productivity
                        
                        pr <- dat$Productivity_Index[dat$LocalityCode == b]
                        
                #Add to df
                tmp <- data.frame("LocalityName" = sites[i],
                                  "Delta_Total_Bio" = td,
                                  "Delta_Decid_Bio" = dd,
                                  "Delta_Conif_Bio" = cd,
                                  "Productivity_Index" = pr)
                
                #Rbind
                new <- rbind(new, tmp)
                

        }
        
        #Add binary variable indicating "weird" sites
        
                #Vector of weird sites
                weird <- c("drangedal1", "eidskog", "fet_3", "halvard_pramhus",
                           "kviteseid1", "kviteseid2", "nes_2", "notodden3",
                           "notodden6", "nsb_verdal", "selbu_kl", "singsaas",
                           "stangeskovene_eidskog", "steinkjer_2bbb", "verdal_2vb")
                
                #Add binary variable
                new$Is_Weird[new$LocalityName %in% weird] <- "Yes"
                new$Is_Weird[!(new$LocalityName %in% weird)] <- "No"
                

#END CALCULATE DELTA BIOMASS -----------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#EXPLORATORY PLOTS -----------------------------------------------------------------------------------

        #Productivity
        
                #Complex plot labels
                delta_sym <- '\U0394'
                first <- paste("*Positive values of ", delta_sym, " biomass indicate higher biomass in", sep = "")
                sec <- "exclosures relative to corresponding open plots"
                lab <- paste(first, sec, sep = "\n")
                text_annotation <- text_grob(lab, face = "italic", color = "#333333", size = 9)
                
                #Graph labels
                units <- expression(paste('\U0394', " Total Biomass (", "kg/m"^2, ")"))
                units_2 <- expression(paste('\U0394', " Deciduous Biomass (", "kg/m"^2, ")"))
                units_3 <- expression(paste('\U0394', " Coniferous Biomass (", "kg/m"^2, ")"))
                
                #Delta Total Bio
                g1 <- ggplot(data = new, aes(x = Productivity_Index, y = Delta_Total_Bio, color = Is_Weird)) +
                        geom_smooth(method = "lm") +
                        geom_point() +
                        theme_bw() +
                        scale_y_continuous(limits = c(-2.2, 3.2)) +
                        labs(x = "Productivity Index", y = units, color = "Site of Interest:") +
                        theme(
                                legend.position = "none"
                        )
                g1
        
                #Deciduous Bio
                g2 <- ggplot(data = new, aes(x = Productivity_Index, y = Delta_Decid_Bio, color = Is_Weird)) +
                        geom_smooth(method = "lm") +
                        geom_point() +
                        theme_bw() +
                        scale_y_continuous(limits = c(-2.2, 3.2)) +
                        labs(x = "Productivity Index", y = units_2, color = "Site of Interest:") +
                        theme(
                                legend.position = "none"
                        )
                g2
                
                #Coniferous Bio
                g3 <- ggplot(data = new, aes(x = Productivity_Index, y = Delta_Conif_Bio, color = Is_Weird)) +
                        geom_smooth(method = "lm") +
                        geom_point() +
                        theme_bw() +
                        scale_y_continuous(limits = c(-2.2, 3.2)) +
                        labs(x = "Productivity Index", y = units_3, color = "Site of Interest:") +
                        theme(
                                legend.position = "none"
                        )
                g3
                
                #Coniferous Bio
                gl <- ggplot(data = new, aes(x = Productivity_Index, y = Delta_Conif_Bio, color = Is_Weird)) +
                        geom_smooth(method = "lm") +
                        geom_point() +
                        theme_bw() +
                        scale_y_continuous(limits = c(-2.2, 3.2)) +
                        labs(x = "Productivity Index", y = units_3, color = "Site of Interest:") +
                        theme(
                                legend.position = "bottom"
                        )
                
                #Legend function
                extract_legend <- function(my_ggp) {
                        step1 <- ggplot_gtable(ggplot_build(my_ggp))
                        step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
                        step3 <- step1$grobs[[step2]]
                        return(step3)
                }
                
                shared_legend <- extract_legend(gl)
                
                top <- plot_grid(text_annotation, ncol = 1)
                middle <- plot_grid(g1, NULL, g2, NULL, g3, ncol = 5, rel_widths = c(0.31666, 0.025, 0.31666, 0.025, 0.31666))
                bottom <- plot_grid(shared_legend, ncol = 1)
                complex <- plot_grid(top, NULL, middle, NULL, top, NULL, bottom, ncol = 1, rel_heights = c(0.05, 0.025, 0.5, 0.005, 0.1))
                complex
                
        
#END EXPLORATORY PLOTS -----------------------------------------------------------------------------------
        

                        
        