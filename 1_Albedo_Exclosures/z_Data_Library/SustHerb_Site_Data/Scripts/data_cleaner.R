#This script cleans up the original dataset from Snøan 2019 (located in 'original_data'), joins it with the
#FULL SITE DATA csv (47 sites in total), and stores it in 'cleaned_data'


        #Import original CSV into data frame
        #data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Original_Data/snøan_site_data.csv', header = TRUE)
        full <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Original_Data/sustherb_all_sites.csv', header = TRUE)

#DATA CLEANING
        #Clean site names (to lowercase and replace spaces w/ underscores)
        #data$LocalityName <- tolower( sub(' ', '_', data$LocalityName) )
        full$LocalityName <- tolower( sub(' ', '_', full$LocalityName) )
        
        #Clean region names (to lowercase)
        #data$Region <- tolower( data$Region )
        full$Region <- tolower( full$Region )
        
        #Clean treatment names (either 'open' or 'exclosure')
        #data$Treatment <- tolower( sub(' plot', '', data$Treatment) )
        
        #Standardize region names (remove special characters)
        
                #Fritsøe 1
                full$LocalityName[full$LocalityName == "fritsøe1"] <- "fritsoe1"
                
                #Fritsøe 2
                full$LocalityName[full$LocalityName == "fritsøe2"] <- "fritsoe2"
                
                #Singsås
                full$LocalityName[full$LocalityName == "singsås"] <- "singsaas"
                
                #Sørum 1
                full$LocalityName[full$LocalityName == "sørum_1"] <- "sorum_1"
                
                #Stangeskovene Eigskog (space issue)
                full$LocalityName[full$LocalityName == "stangeskovene_eidskog "] <- "stangeskovene_eidskog"
                
                #Stig Dahlen
                full$LocalityName[full$LocalityName == "stig_dæhlen"] <- "stig_dahlen"
                


#WRITE TO CSV
        #write.csv(data, '1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/cleaned_data.csv', row.names = TRUE)
        write.csv(full, '1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/all_sites_data.csv', row.names = TRUE)
   