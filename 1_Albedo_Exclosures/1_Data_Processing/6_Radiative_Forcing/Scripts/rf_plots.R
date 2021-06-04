## Script to generate FINAL PLOTS of RF/Carbon equivalent effect of moose browsing

##PACKAGES ----------------------------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(gridExtra)
        library(cowplot)
        library(beepr)
        library(viridis)
        
###END PACKAGES ----------------------------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INITIAL DATA IMPORT + FORMATTING ----------------------------------------------------------------------------------------------

        #LOAD CLEANED RF DATASET
        data <- read.csv("1_Albedo_Exclosures/z_Data_Library/Radiative_Forcing_Estimates/Usable_Data/full_rf_data.csv", header = T)
        
        #FILTER OUT THINNED SITES
        bad_sites <- c("malvik", "selbu_flub", "hi_tydal")
        data <- data[!data$LocalityName %in% bad_sites,] 
        data$LocalityName <- as.factor(data$LocalityName) #Brings us to 44 LocalityNames (looks good)
              
        
#END INITIAL DATA IMPORT + FORMATTING --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


        
        
#CALCULATE AVERAGES + SE BY REGION & PLOT ----------------------------------------------------------------------------------------------

        #CALCULATE AVERAGES & SE -------------
        
                #Define function to calculate SE
                std <- function(x) sd(x)/sqrt(length(x))
                
                #Define vector of regions (for SE loop)
                regs <- levels(as.factor(data$Region))
                
                #Delta carbon - CO2 equiv
                
                        #Averages
                        avg_c <- aggregate(data$C_Eq_Delta_C, by = list("Region" = data$Region,
                                                                        "Years_Since_Exclosure" = data$Years_Since_Exclosure),
                                           FUN = mean)
                        colnames(avg_c)[3] <- "Avg_C_Eq_Delta_C"
                        
                        #Standard error
                        
                                #Add SE placeholder column
                                avg_c$SE <- as.numeric('')
                                
                                #Loop through regions
                                for(i in 1:length(regs)){
                                        
                                        #Identify region
                                        reg <- regs[i]
                                        
                                        #Min & max year
                                        min <- min(data$Years_Since_Exclosure[data$Region == reg])
                                        max <- max(data$Years_Since_Exclosure[data$Region == reg])
                                        
                                        for(j in min:max){
                                                
                                                #Calculate SE in year j
                                                se <- std(data$C_Eq_Delta_C[data$Region == reg & data$Years_Since_Exclosure == j])
                                                
                                                #Add to df
                                                avg_c$SE[avg_c$Region == reg & avg_c$Years_Since_Exclosure == j] <- se
                                        }
                                        
                                }
                                
                                
                #Delta albedo - CO2 equiv
                        
                        #Averages
                        avg_a <- aggregate(data$C_Eq_Delta_A, by = list("Region" = data$Region,
                                                                        "Years_Since_Exclosure" = data$Years_Since_Exclosure),
                                           FUN = mean)
                        colnames(avg_a)[3] <- "Avg_C_Eq_Delta_A"
                        
                        #Standard error
                        
                                #Add SE placeholder column
                                avg_a$SE <- as.numeric('')
                                
                                #Loop through regions
                                for(i in 1:length(regs)){
                                        
                                        #Identify region
                                        reg <- regs[i]
                                        
                                        #Min & max year
                                        min <- min(data$Years_Since_Exclosure[data$Region == reg])
                                        max <- max(data$Years_Since_Exclosure[data$Region == reg])
                                        
                                        for(j in min:max){
                                                
                                                #Calculate SE in year j
                                                se <- std(data$C_Eq_Delta_A[data$Region == reg & data$Years_Since_Exclosure == j])
                                                
                                                #Add to df
                                                avg_a$SE[avg_a$Region == reg & avg_a$Years_Since_Exclosure == j] <- se
                                        }
                                        
                                }
                
                #Net CO2 equiv
                        
                        #Averages
                        avg_net <- aggregate(data$Net_C_Eq, by = list("Region" = data$Region,
                                                                        "Years_Since_Exclosure" = data$Years_Since_Exclosure),
                                           FUN = mean)
                        colnames(avg_net)[3] <- "Avg_C_Eq_Net"
                        
                        #Standard error
                        
                                #Add SE placeholder column
                                avg_net$SE <- as.numeric('')
                                
                                #Loop through regions
                                for(i in 1:length(regs)){
                                        
                                        #Identify region
                                        reg <- regs[i]
                                        
                                        #Min & max year
                                        min <- min(data$Years_Since_Exclosure[data$Region == reg])
                                        max <- max(data$Years_Since_Exclosure[data$Region == reg])
                                        
                                        for(j in min:max){
                                                
                                                #Calculate SE in year j
                                                se <- std(data$Net_C_Eq[data$Region == reg & data$Years_Since_Exclosure == j])
                                                
                                                #Add to df
                                                avg_net$SE[avg_net$Region == reg & avg_net$Years_Since_Exclosure == j] <- se
                                        }
                                        
                                }
                                
                                
                                
        #GENERATE PLOTS ----------------------------------
                
                #Define discrete color-blind-friendly palette
                pal <- c("#009E73", "#56B4e9", "#e69f00")
                
                #Y-axis label
                lab <- expression(paste("CO"[2], "-equivalent  (", "kg/m"^2, ")"))
                
                #REPLACE REGION LABELS WITH COUNTY LABELS -----
                
                        #Avg C
                        avg_c$Counties[avg_c$Region == "Trøndelag"] <- "Trøndelag"
                        avg_c$Counties[avg_c$Region == "Hedmark"] <- "Innlandet and Viken"
                        avg_c$Counties[avg_c$Region == "Telemark"] <- "Telemark and Vestfold"
                        
                        #Avg A
                        avg_a$Counties[avg_a$Region == "Trøndelag"] <- "Trøndelag"
                        avg_a$Counties[avg_a$Region == "Hedmark"] <- "Innlandet and Viken"
                        avg_a$Counties[avg_a$Region == "Telemark"] <- "Telemark and Vestfold"
                        
                        #Net
                        avg_net$Counties[avg_net$Region == "Trøndelag"] <- "Trøndelag"
                        avg_net$Counties[avg_net$Region == "Hedmark"] <- "Innlandet and Viken"
                        avg_net$Counties[avg_net$Region == "Telemark"] <- "Telemark and Vestfold"
                
                #Plot
                ggplot() +
                        geom_hline(yintercept = 0, linetype = 2, color = "#c6c6c6") +
                        geom_ribbon(data = avg_c, aes(x = Years_Since_Exclosure, y = Avg_C_Eq_Delta_C, ymin = (Avg_C_Eq_Delta_C - SE), ymax = (Avg_C_Eq_Delta_C + SE), fill = paste("CO2-eq. ", "\U0394", "C", sep = "")),  position = position_dodge(0.2), alpha = 0.18, lwd = 0) +
                        geom_line(data = avg_c, aes(x = Years_Since_Exclosure, Avg_C_Eq_Delta_C, color = paste("CO2-eq. ", "\U0394", "C", sep = "")), lwd = 0.65) +
                        geom_ribbon(data = avg_a, aes(x = Years_Since_Exclosure, y = Avg_C_Eq_Delta_A, ymin = (Avg_C_Eq_Delta_A - SE), ymax = (Avg_C_Eq_Delta_A + SE), fill = paste("CO2-eq. ", "\U0394", "\U03B1", sep = "")), position = position_dodge(0.2), alpha = 0.2, lwd = 0) +
                        geom_line(data = avg_a, aes(x = Years_Since_Exclosure, Avg_C_Eq_Delta_A, color = paste("CO2-eq. ", "\U0394", "\U03B1", sep = "")), lwd = 0.65) +
                        geom_ribbon(data = avg_net, aes(x = Years_Since_Exclosure, y = Avg_C_Eq_Net, ymin = (Avg_C_Eq_Net - SE), ymax = (Avg_C_Eq_Net + SE), fill = "Net CO2-eq."), position = position_dodge(0.2), alpha = 0.5, lwd = 0) +
                        geom_line(data = avg_net, aes(x = Years_Since_Exclosure, y = Avg_C_Eq_Net, color = "Net CO2-eq."), lwd = 0.65) +
                        facet_wrap(~ Counties, ncol = 2) +
                        scale_x_continuous(limits = c(1,11), breaks = c(1,3,5,7,9,11)) +
                        scale_color_manual(values = c(pal[1], pal[2], pal[3])) +
                        scale_fill_manual(values = c(pal[1], pal[2], pal[3])) +
                        labs(x = "Years Since Exclosure", y = lab, fill = "", color = "") +
                        theme_bw() +
                        theme(
                                panel.grid.minor = element_blank(),
                                panel.grid.major.x = element_blank(),
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10)),
                                legend.position = c(0.75,0.24),
                                legend.background = element_rect(fill="#fafafa",
                                                                 size=0.1, linetype="solid", 
                                                                 colour ="#666666"),
                                legend.title = element_blank(),
                                legend.margin = margin(0,4,4,4)
                        )
                
                                #EXPORT @ 450x400px
                
                
                
        
                

        
#CALCULATE AVERAGES + SE BY REGION & PLOT ----------------------------------------------------------------------------------------------
                        
                        
                        
                        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#PLOT VS PRODUCTIVITY ----------------------------------------------------------------------------------------------
         
        #PLOT 8-YR CO2 vs Productivity ----------------
                
                #Load productivity
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
                        
                        #Nes 1
                        productivity$LocalityName[productivity$LocalityName == "nes 1"] <- "nes_1"
                        
                        #Nes 2
                        productivity$LocalityName[productivity$LocalityName == "nes 2"] <- "nes_2"
                        
                        #Kongsvinger 1
                        productivity$LocalityName[productivity$LocalityName == "kongsvinger 1"] <- "kongsvinger_1"
                        
                        #Kongsvinger 2
                        productivity$LocalityName[productivity$LocalityName == "kongsvinger 2"] <- "kongsvinger_2"
                        
                        #Maarud 1-3
                        productivity$LocalityName[productivity$LocalityName == "maarud 1"] <- "maarud_1"
                        productivity$LocalityName[productivity$LocalityName == "maarud 2"] <- "maarud_2"
                        productivity$LocalityName[productivity$LocalityName == "maarud 3"] <- "maarud_3"
                        
                        #Sørum 1
                        productivity$LocalityName[productivity$LocalityName == "sørum 1"] <- "sorum_1"
                        
                #Add productivity to data frame
                        
                        #Placeholder column
                        data$Productivity_Index <- as.numeric('')
                        
                        #Loop through
                        for(i in 1:nrow(data)){
                                
                                loc <- as.character(data[i, "LocalityName"])
                                prod <- productivity$Productivity[productivity$LocalityName == loc]
                                data[i, "Productivity_Index"] <- prod
                                
                        }
                        
                #Isolate 7 YSE data
                iso <- data[data$Years_Since_Exclosure == 7,] #All 44 sites have data for 7 YSE
                
                #Generate plot
                ggplot(data = iso) +
                        geom_point(aes(x = Productivity_Index, y = C_Eq_Delta_C,  color = paste("CO2-eq. ", "\U0394", "C", sep = ""))) +
                        geom_point(aes(x = Productivity_Index, y = C_Eq_Delta_A,  color = paste("CO2-eq. ", "\U0394", "\U03B1", sep = ""))) +
                        geom_point(aes(x = Productivity_Index, y = Net_C_Eq, color = "Net CO2-eq.")) +
                        facet_wrap(~Region, ncol = 2) +
                        labs(x = "Productivity Index", y = lab, fill = "", color = "") +
                        theme_bw() +
                        scale_color_manual(values = c(pal[1], pal[2], pal[3])) +
                        scale_fill_manual(values = c(pal[1], pal[2], pal[3])) +
                        theme(
                                panel.grid.minor = element_blank(),
                                panel.grid.major.x = element_blank(),
                                axis.title.x = element_text(margin = margin(t = 10)),
                                axis.title.y = element_text(margin = margin(r = 10)),
                                legend.position = c(0.75,0.24),
                                legend.background = element_rect(fill="#fafafa",
                                                                 size=0.1, linetype="solid", 
                                                                 colour ="#666666"),
                                legend.title = element_blank(),
                                legend.margin = margin(0,4,4,4)
                        )
                
                                #EXPORT @ 450x400px
                        
                        
                                       
#END PLOT VS PRODUCTIVITY ----------------------------------------------------------------------------------------------
                        