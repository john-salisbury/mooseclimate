## Script to calculate and visualize plot-level averages of tree volume by group and region

##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(grid)
        library(cowplot)
        library(zoo)
        library(sf)
        library(wesanderson)
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        #Import CSV to dataframe
        vol <- read.csv('1_Albedo_Exclosures/1_Data_Processing/3_Volume_Estimates/Output/subplot_tree_volumes.csv', header = TRUE)

        
#END INITIAL DATA IMPORT --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



        
#CALCULATE TOTAL VOLUME PER SUBPLOT (PER YEAR) ----------------------------------------------------------------------------------------------

        #Aggregate/sum volume for all groups in a given subplot (in a given year)
        vol_tot <- aggregate(vol$Volume_m3ha, by = list("Region" = vol$Region,
                                                        "LocalityName" = vol$LocalityName,
                                                        "LocalityCode" = vol$LocalityCode,
                                                        "Treatment" = vol$Treatment,
                                                        "Subplot" = vol$Subplot,
                                                        "Years_Since_Exclosure" = vol$Years_Since_Exclosure),
                             FUN = sum)
        colnames(vol_tot)[7] <- "Tot_Vol"
        
        
        #Aggregate plot-level averages for each PLOT (in a given year)
        vol_plot <- aggregate(vol_tot$Tot_Vol, by = list("Region" = vol_tot$Region,
                                                         "LocalityName" = vol_tot$LocalityName,
                                                         "LocalityCode" = vol_tot$LocalityCode,
                                                         "Treatment" = vol_tot$Treatment,
                                                         "Years_Since_Exclosure" = vol_tot$Years_Since_Exclosure),
                              FUN = mean)
        colnames(vol_plot)[6] <- "Plot_Tot_Vol"
        
        
        #Create region-level averages for plotting
        vol_reg <- aggregate(vol_plot$Plot_Tot_Vol, by = list("Region" = vol_plot$Region,
                                                              "Treatment" = vol_plot$Treatment,
                                                              "Years_Since_Exclosure" = vol_plot$Years_Since_Exclosure),
                             FUN = mean)
        colnames(vol_reg)[4] <- "Avg_Vol"
        
        #Add SE to df
        
                #SE Function
                std <- function(x) sd(x)/sqrt(length(x))
                
                #Placeholder column
                vol_reg$SE <- as.numeric('')
                
                #Loop through
                for(i in 1:nrow(vol_reg)){
                        
                        #Get variables
                        tr <- vol_reg[i, "Treatment"]
                        reg <- vol_reg[i, "Region"]
                        yse <- vol_reg[i, "Years_Since_Exclosure"]
                        
                        #Calculate SE for albedo
                        se <- std(vol_plot$Plot_Tot_Vol[vol_plot$Region == reg &
                                                                       vol_plot$Treatment == tr &
                                                                       vol_plot$Years_Since_Exclosure == yse])
                        
                        #Add to df
                        vol_reg[i, "SE"] <- se
                        
                }


#END CALCULATE TOTAL VOLUME PER SUBPLOT (PER YEAR) ----------------------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        
#GENERATE PLOTS ---------------------------------------------------

                #PLOT TOTAL BIO BY REGION (ALL REGIONS) -----------      
                
                #Treatment nice names (for plotting)
                vol_reg$TNN[vol_reg$Treatment == "B"] <- "Browsed"
                vol_reg$TNN[vol_reg$Treatment == "UB"] <- "Unbrowsed"
                
                #REPLACE REGION LABELS WITH COUNTY LABELS -----
                vol_reg$Counties[vol_reg$Region == "Trøndelag"] <- "Trøndelag"
                vol_reg$Counties[vol_reg$Region == "Hedmark"] <- "Innlandet and Viken"
                vol_reg$Counties[vol_reg$Region == "Telemark"] <- "Vestfold and Telemark"
                
                #Label for Hedmark facet
                ann_text_1 <- data.frame(Years_Since_Exclosure = 7.52, Avg_Vol = 31, lab = "||",
                                         Counties = "Innlandet and Viken", TNN = "Browsed")
                ann_text_2 <- data.frame(Years_Since_Exclosure = 7.52, Avg_Vol = 22.3, lab = "||",
                                         Counties = "Innlandet and Viken", TNN = "Browsed")
                
                #Position
                pd <- position_dodge(0.3)
                
                
                
                
                
                #Generate plot
                ggplot(data = vol_reg, aes(x = Years_Since_Exclosure, y = Avg_Vol, color = TNN, group = TNN)) +
                        geom_errorbar(aes(ymin = (Avg_Vol - SE), ymax = (Avg_Vol + SE)), colour="#666666", width=0.5, position = pd) +
                        geom_point(aes(shape = TNN), position = pd) +
                        geom_line(aes(linetype = TNN), position = pd) +
                        facet_wrap(~ Counties, ncol = 2) +
                        labs(x = "Years Since Exclosure", y = "Volume "~(m^3/ha), color = "Treatment:", shape = "Treatment:", linetype = "Treatment:") +
                        scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
                        scale_color_manual(values = viridis(n = 2, alpha = 1, begin = 0, end = 0.6)) +
                        theme_bw() +
                        theme(
                                panel.grid.minor = element_blank(),
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10)),
                                legend.position = c(0.75,0.24),
                                legend.background = element_rect(fill="#fafafa",
                                                                 size=0.1, linetype="solid", 
                                                                 colour ="#666666"),
                                legend.margin = margin(10,10,10,10)
                        ) +
                        geom_label(data = ann_text_1,
                                   label = "||",
                                   angle = 0.5,
                                   color = "#22A884FF",
                                   size = 3,
                                   label.padding = unit(0, "lines"),
                                   label.r = unit(0, "lines"),
                                   label.size = NA) +
                        geom_label(data = ann_text_2,
                                   label = "||",
                                   angle = 0.5,
                                   color = "#440154FF",
                                   size = 3,
                                   label.padding = unit(-0.04, "lines"),
                                   label.r = unit(0, "lines"),
                                   label.size = NA)
                
                

#END GENERATE PLOTS ---------------------------------------------------
        
     