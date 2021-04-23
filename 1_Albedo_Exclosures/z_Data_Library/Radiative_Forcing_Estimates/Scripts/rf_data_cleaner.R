#This script cleans up the radiative forcing CSV (adds regions) for final plotting

        #Load CSV
        data <- read.csv("1_Albedo_Exclosures/z_Data_Library/Radiative_Forcing_Estimates/Original_Data/rf_data_cleaned.csv", header = T)

        #Load site data
        sites <- read.csv("1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/all_sites_data.csv", header = T)
        
        #Add regions
        data$Region <- as.character('')
        
        for(i in 1:nrow(data)){
                
                site <- data[i, "LocalityName"]
                reg <- sites$Region[sites$LocalityName == site][1]
                data[i, "Region"] <- reg
                
        }
        
        #Make region first column
        data <- data[, c(11, 1:10)]
        
        #Write CSV
        write.csv(data, "1_Albedo_Exclosures/z_Data_Library/Radiative_Forcing_Estimates/Usable_Data/full_rf_data.csv")
        
        