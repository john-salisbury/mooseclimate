## Script to generate PLOT-LEVEL plots of biomass (for use in paper)

##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(viridis)
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT ----------------------------------------------------------------------------------------------

        #Import plot-level biomass

                #Total biomass
                total_bio <- read.csv('1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_biomass.csv', header = TRUE)

                #Deciduous biomass
                decid_bio <- read.csv('1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_decid_biomass.csv', header = TRUE)
                
                #Pine biomass
                pine_bio <- read.csv('1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_pine_biomass.csv', header = TRUE)
                
                #Spruce biomass
                spruce_bio <- read.csv('1_Albedo_Exclosures/1_Data_Processing/2_Biomass_Estimates/Output/plot_spruce_biomass.csv', header = TRUE)
                
                
        #Fix LocalityName errors
                
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




#PREP DATA FOR PLOTTING ------------------------------------------------------------------
                
        #Get coniferous biomass column -----
                
                #Make copy
                data <- total_bio
                
                #Placeholder df's
                data$Decid_Bio <- as.numeric('')
                data$Pine_Bio <- as.numeric('')
                data$Spruce_Bio <- as.numeric('')
                
                #Loop and add
                for(i in 1:nrow(data)){
                        
                        #Get variables
                        loc <- data[i, "LocalityCode"]
                        yr <- data[i, "Years_Since_Exclosure"]
                        
                        #Get additional bio values
                        
                        #Deciduous bio (w/ error catcher)
                        d <- decid_bio$Mean_Plot_Decid_Biomass_kg_m2[decid_bio$LocalityCode == loc & decid_bio$Years_Since_Exclosure == yr]
                        if(length(d) == 0){
                                d <- 0
                        }
                        
                        #Pine bio
                        p <- pine_bio$Mean_Plot_Pine_Biomass_kg_m2[pine_bio$LocalityCode == loc & pine_bio$Years_Since_Exclosure == yr]
                        if(length(p) == 0){
                                p <- 0
                        }
                        
                        #Spruce bio
                        s <- spruce_bio$Mean_Plot_Spruce_Biomass_kg_m2[spruce_bio$LocalityCode == loc & spruce_bio$Years_Since_Exclosure == yr]
                        if(length(s) == 0){
                                s <- 0
                        }
                        
                        #Add to df
                        data[i, "Decid_Bio"] <- d
                        data[i, "Pine_Bio"] <- p
                        data[i, "Spruce_Bio"] <- s
                        
                }
                
                #Add pine + spruce
                data$Conif_Bio <- data$Pine_Bio + data$Spruce_Bio
                
                
        #Average + SE by treatment & year -----
                
                #TOTAL BIO -------
                
                        #Average -----
                        tot_avg <- aggregate(data$Mean_Plot_Biomass_kg_m2, by = list("Treatment" = data$Treatment,
                                                                                          "Years_Since_Exclosure" = data$Years_Since_Exclosure),
                                             FUN = mean)
                        colnames(tot_avg)[3] <- "Avg_Bio"
                        
                        #Bio Type -----
                        tot_avg$Bio_Type <- "Total"
                        
                        #SE -----
                                
                                std <- function(x) sd(x)/sqrt(length(x))
                                
                                #Add placeholder columns
                                tot_avg$SE <- as.numeric('')
                                
                                #Calculate SEs for each species/group, month, and year
                                for(i in 1:nrow(tot_avg)){
                                        
                                        #Get variables
                                        tr <- tot_avg[i, "Treatment"]
                                        yse <- tot_avg[i, "Years_Since_Exclosure"]
                                        
                                        #Calculate SE for albedo
                                        se <- std(data$Mean_Plot_Biomass_kg_m2[data$Treatment == tr &
                                                                              data$Years_Since_Exclosure == yse])
                                        
                                        #Add to df
                                        tot_avg[i, "SE"] <- se
                                        
                                }
                                
                                
                                
                #DECID BIO -------
                                
                        #Average -----
                        d_avg <- aggregate(data$Decid_Bio, by = list("Treatment" = data$Treatment,
                                                                                          "Years_Since_Exclosure" = data$Years_Since_Exclosure),
                                             FUN = mean)
                        colnames(d_avg)[3] <- "Avg_Bio"
                        
                        #Bio Type -----
                        d_avg$Bio_Type <- "Deciduous"
                        
                        #SE -----
                        
                                std <- function(x) sd(x)/sqrt(length(x))
                                
                                #Add placeholder columns
                                d_avg$SE <- as.numeric('')
                                
                                #Calculate SEs for each species/group, month, and year
                                for(i in 1:nrow(d_avg)){
                                        
                                        #Get variables
                                        tr <- d_avg[i, "Treatment"]
                                        yse <- d_avg[i, "Years_Since_Exclosure"]
                                        
                                        #Calculate SE for albedo
                                        se <- std(data$Decid_Bio[data$Treatment == tr &
                                                                         data$Years_Since_Exclosure == yse])
                                        
                                        #Add to df
                                        d_avg[i, "SE"] <- se
                                        
                                }
                                
                                
                #CONIF BIO -------
                                
                        #Average -----
                        c_avg <- aggregate(data$Conif_Bio, by = list("Treatment" = data$Treatment,
                                                                     "Years_Since_Exclosure" = data$Years_Since_Exclosure),
                                           FUN = mean)
                        colnames(c_avg)[3] <- "Avg_Bio"
                        
                        #Bio Type -----
                        c_avg$Bio_Type <- "Coniferous"
                        
                        #SE -----
                        
                                std <- function(x) sd(x)/sqrt(length(x))
                                
                                #Add placeholder columns
                                c_avg$SE <- as.numeric('')
                                
                                #Calculate SEs for each species/group, month, and year
                                for(i in 1:nrow(c_avg)){
                                        
                                        #Get variables
                                        tr <- c_avg[i, "Treatment"]
                                        yse <- c_avg[i, "Years_Since_Exclosure"]
                                        
                                        #Calculate SE for albedo
                                        se <- std(data$Conif_Bio[data$Treatment == tr &
                                                                         data$Years_Since_Exclosure == yse])
                                        
                                        #Add to df
                                        c_avg[i, "SE"] <- se
                                        
                                }
                                
                                
                #BIND TOGETHER INTO SINGLE DF --------
                final <- rbind(tot_avg, d_avg, c_avg)
                                
                #Reorder factor
                final$Bio_Type <- factor(final$Bio_Type, levels = c("Total", "Deciduous", "Coniferous"))
                
                
                
                
                
        #Average (total bio only) + SE by treatment, region, & year -----
                
                #Average -----
                tot_avg_reg <- aggregate(data$Mean_Plot_Biomass_kg_m2, by = list("Region" = data$Region,
                                                                                 "Treatment" = data$Treatment,
                                                                                 "Years_Since_Exclosure" = data$Years_Since_Exclosure),
                                     FUN = mean)
                colnames(tot_avg_reg)[4] <- "Avg_Tot_Bio"
             
                #SE -----
                std <- function(x) sd(x)/sqrt(length(x))
                
                        #Add placeholder columns
                        tot_avg_reg$SE <- as.numeric('')
                        
                        #Calculate SEs for each species/group, month, and year
                        for(i in 1:nrow(tot_avg_reg)){
                                
                                #Get variables
                                tr <- tot_avg_reg[i, "Treatment"]
                                reg <- tot_avg_reg[i, "Region"]
                                yse <- tot_avg_reg[i, "Years_Since_Exclosure"]
                                
                                #Calculate SE for albedo
                                se <- std(data$Mean_Plot_Biomass_kg_m2[data$Region == reg &
                                                                               data$Treatment == tr &
                                                                               data$Years_Since_Exclosure == yse])
                                
                                #Add to df
                                tot_avg_reg[i, "SE"] <- se
                                
                        }
                
                
                
              
                
                
#END PREP DATA FOR PLOTTING ------------------------------------------------------------------
                
                
                
                
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#GENERATE FINAL PLOTS ----------------------------------------------------------------------
        
        #Position dodge
        pd <- position_dodge(0.3)
        
        #PLOT AVERAGES BY BIO TYPE (ALL REGIONS) -------- 
                        
                #Treatment nice names (for plotting)
                final$TNN[final$Treatment == "B"] <- "Browsed"
                final$TNN[final$Treatment == "UB"] <- "Unbrowsed"
                
                #Generate plot
                ggplot(data = final, aes(x = Years_Since_Exclosure, y = Avg_Bio, color = TNN, group = TNN)) +
                        geom_errorbar(aes(ymin = (Avg_Bio - SE), ymax = (Avg_Bio + SE)), colour="#666666", width=0.5, position = pd) +
                        geom_point(aes(shape = TNN), position = pd) +
                        geom_line(aes(linetype = TNN), position = pd) +
                        facet_wrap(~ Bio_Type, ncol = 2) +
                        labs(x = "Years Since Exclosure", y = "Biomass "~(kg/m^2), color = "Treatment:", shape = "Treatment:", linetype = "Treatment:") +
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
                        )
        
        #PLOT TOTAL BIO BY REGION (ALL REGIONS) -----------      
        
                #Treatment nice names (for plotting)
                tot_avg_reg$TNN[tot_avg_reg$Treatment == "B"] <- "Browsed"
                tot_avg_reg$TNN[tot_avg_reg$Treatment == "UB"] <- "Unbrowsed"
                
                #Label for Hedmark facet
                ann_text_1 <- data.frame(Years_Since_Exclosure = 7.52, Avg_Tot_Bio = 1.33, lab = "||",
                                         Region = "Hedmark", TNN = "Browsed")
                ann_text_2 <- data.frame(Years_Since_Exclosure = 7.52, Avg_Tot_Bio = 0.92, lab = "||",
                                         Region = "Hedmark", TNN = "Browsed")
        
                #Generate plot
                ggplot(data = tot_avg_reg, aes(x = Years_Since_Exclosure, y = Avg_Tot_Bio, color = TNN, group = TNN)) +
                        geom_errorbar(aes(ymin = (Avg_Tot_Bio - SE), ymax = (Avg_Tot_Bio + SE)), colour="#666666", width=0.5, position = pd) +
                        geom_point(aes(shape = TNN), position = pd) +
                        geom_line(aes(linetype = TNN), position = pd) +
                        facet_wrap(~ Region, ncol = 2) +
                        labs(x = "Years Since Exclosure", y = "Biomass "~(kg/m^2), color = "Treatment:", shape = "Treatment:", linetype = "Treatment:") +
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
        

                

                
#END GENERATE FINAL PLOTS ------------------------------------------------------------------
                
 