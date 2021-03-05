## This is a script to filter the large, unified SatSkog data product down to Trøndelag & Hedmark
## that will allow for further visualization of species-specific albedo between the two regions


#PACKAGES ----------------------------------------------------------------------

        #Spatial Data Packages
        library(sf)
        
        #Data Manipulation + Visualization
        library(ggplot2)
        library(raster)
        library(lattice)
        library(dplyr)
        library(wesanderson)
        library(beepr)
        library(zoo)

#END PACKAGES ----------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#IMPORT & FORMAT DATA --------------------------------------------------------------------------------------------------

        #Load unified & FILTERED shapefile w/ all variables (from all years)
        data <- st_read("2_Albedo_Regional/z_Data_Library/SatSkog_Data_Product/Usable_Data/Unified_Shapefile/unified_satskog_shapefile.shp")
        beep(8)
        
        
#END IMPORT & FORMAT DATA --------------------------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#FILTER SHAPEFILE TO TRØNDELAG & HEDMARK --------------------------------------------------------------------------------
        
        
        #Get all kommune names
        data$kommnvn <- as.factor(data$kommnvn)
        kommunes <- levels(data$kommnvn)
        
        #HEDMARK ONLY SHAPEFILE
        hedmark_kommunes <- c("Ringsaker",
                              "Hamar",
                              "Elverum",
                              "Stange",
                              "Kongsvinger",
                              "Sør-Odal",
                              "Løten",
                              "Åsnes",
                              "Trysil",
                              "Eidskog",
                              "Tynset",
                              "Nord-Odal",
                              "Grue",
                              "Åmot",
                              "Våler",
                              "Stor-Elvdal",
                              "Alvdal",
                              "Os",
                              "Rendalen",
                              "Folldal",
                              "Tolga",
                              "Engerdal")
        
        hedmark <- subset(data, kommnvn %in% hedmark_kommunes)
        
        
        #TRØNDELAG ONLY SHAPEFILE
        trondelag_kommunes <- c("Trondheim",
                                "Steinkjer",
                                "Namsos",
                                "Frøya",
                                "Osen",
                                "Oppdal",
                                "Rennebu",
                                "Røros",
                                "Holtålen",
                                "Midtre Gauldal",
                                "Melhus",
                                "Skaun",
                                "Malvik",
                                "Selbu",
                                "Tydal",
                                "Meråker",
                                "Stjørdal",
                                "Frosta",
                                "Levanger",
                                "Verdal",
                                "Snåsa",
                                "Lierne",
                                "Røyrvik",
                                "Namsskogan",
                                "Grong",
                                "Overhalla",
                                "Flatanger",
                                "Leka",
                                "Inderøy",
                                "Indre Fosen",
                                "Heim",
                                "Hitra",
                                "Ørland",
                                "Åfjord",
                                "Orkland",
                                "Nærøysund",
                                "Rindal")
        
        trondelag <- subset(data, kommnvn %in% trondelag_kommunes)
        
        #Remove initial dataset to free up memory
        rm(data)
        
        
        
#END FILTER SHAPEFILE TO TRØNDELAG & HEDMARK --------------------------------------------------------------------------------
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        

#UNIFY TRØNDELAG & HEDMARK SHAPEFILES --------------------------------------------------------------------------------
        
        #Add "cluster" variable to each cropped df
        hedmark$Plot <- "Hedmark"
        trondelag$Plot <- "Trondelag"
        
        #Row bind three plots
        unified <- rbind(hedmark, trondelag)
        
        #Write final ESRI shapefile
        st_write(unified, "2_Albedo_Regional/1_Data_Processing/1_Filter_Regions/Output/regions.shp", driver = "ESRI Shapefile", append = FALSE)
        beep(8)
        
        
#END UNIFY TRØNDELAG & HEDMARK SHAPEFILES --------------------------------------------------------------------------------
        
        


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#GENERATE MAP PLOT OF SELECTED REGIONS --------------------------------------------------------------------------------

        #START HERE if already written shapefile
        unified <- st_read("2_Albedo_Regional/1_Data_Processing/1_Filter_Regions/Output/regions.shp")
        beep(8)
        
        #Load herbivore density data shapefile to plot outlines of kommunes
        hd <- st_read("2_Albedo_Regional/z_Data_Library/Herbivore_Density_Data/NorwayLargeHerbivores.shp") #UTM33N, same as SatSkog
        
                #Add 'Region' variable to Hedmark and Trondelag kommunes
        
                        #Placeholder column
                        hd$Region <- as.character('')
        
                        #Trøndelag
                        hd$Region[hd$kommnvn %in% trondelag_kommunes] <- "Trøndelag"
                        
                        #Hedmark
                        hd$Region[hd$kommnvn %in% hedmark_kommunes] <- "Hedmark"
                
        plot(hd$geometry, lwd = 0.3)
        plot(hd["Region"])
        
#END GENERATE MAP PLOT OF SELECTED REGIONS --------------------------------------------------------------------------------
        
