#This script examines herbivore densities within each region of the exclosure study
#(Trøndelag, Hedmark, and Telemark). It is meant to identify whether or not we can group
#albedo estimates by region in further plots.

#It specifically looks at estimated herbivore densities in 2015 (the year of data which will
#be used in final analysis)

#LOAD PACKAGES -----------------------------------------------------------------------

        #Packages for data processing + visualization
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(gridExtra)
        library(cowplot)
        library(sf)
        library(raster)
        library(zoo)
        library(beepr)
        library(wesanderson)


#END LOAD PACKAGES -----------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#LOAD DATA --------------------------------------------------------------------------

        ## SITE DATA

                #Get 'cleaned' site data from adjacent 'Sites' folder
                site_data <- read.csv('1_Albedo_Exclosures/z_Data_Library/SustHerb_Site_Data/Usable_Data/all_sites_data.csv', header = TRUE)
                
        #HERBIVORE DENSITY DATA (2015 - MUNICIPALITY RESOLUTION)
        
                #Read in herbivore biomass data (2015) from SpatialPolygons object (isolate dataframe)
                hbiomass_shp <- shapefile("1_Albedo_Exclosures/z_Data_Library/Herbivore_Density_Data/Usable_Data/NorwayLargeHerbivores")
                
                #Pull out dataframe
                hbiomass <- hbiomass_shp@data
                
                #Isolate to 2015 moose, roe deer, and red deer density data
                hbiomass2015 <- cbind(hbiomass[,c(1:10)], hbiomass$Ms_2015, hbiomass$Rd__2015, hbiomass$R_d_2015)
        

#END LOAD DATA -----------------------------------------------------------------------
        
        
                
                
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#FILTER DATA --------------------------------------------------------------------------
                
        ##Get vector of kommune IDs from site data
        #For 3-digit codes, add 0 to front (to match herb. density df format)
        for(i in 1:nrow(site_data)){
                
                site <- site_data[i, "DistrictID"]
                
                if( nchar(site) == 3 ){
                        site_data[i, "DistrictID"] <- paste(0, site, sep = "")
                }
        }
                
        #Remove 3 thinned sites
        bad_sites <- c("MAB", "MAUB", "FLB", "FLUB", "HIB", "HIUB")
        site_data <- site_data[!site_data$LocalityCode %in% bad_sites,]
        sites <- levels(as.factor(site_data$DistrictID))

        #Filter to herbivore densities in corresponding kommunes
        hbio_filt <- hbiomass2015[hbiomass2015$kommnnr %in% sites,]
        
        #Fix column names
        colnames(hbio_filt)[11:13] <- c("Moose_Density", "Red_Deer_Density", "Roe_Deer_Density")
        
        #Add "Region" column for grouping
        hbio_filt$Region <- as.character('')
        
        for(i in 1:nrow(hbio_filt)){
                
                kom <- hbio_filt[i, "kommnr"]
                reg <- site_data$Region[site_data$DistrictID == kom][1]
                hbio_filt[i, "Region"] <- reg
        }
        
        #Fix formatting for plotting
        hbio_filt$Region[hbio_filt$Region == "trondelag"] <- "Trøndelag"
        hbio_filt$Region[hbio_filt$Region == "hedmark"] <- "Hedmark"
        hbio_filt$Region[hbio_filt$Region == "telemark"] <- "Telemark"
        
        #Expand df for plotting
        
                #Multiply each row x3 (once for each herbivore type)
                hbio_exp <- hbio_filt[rep(seq_len(nrow(hbio_filt)), each = 3), ]
                
                #Add placeholder columns
                hbio_exp$Herb_Density <- as.character('')
                hbio_exp$Species <- as.character('')
                
                #Loop through and move densities for each kommune
                for(i in 1:length(sites)){
                        
                        site <- sites[i]
                        
                        #Add moose density
                        hbio_exp$Herb_Density[hbio_exp$kommnnr == site][1] <- hbio_exp$Moose_Density[hbio_exp$kommnnr == site][1]
                        hbio_exp$Species[hbio_exp$kommnnr == site][1] <- "Moose"
                        
                        #Add red deer density
                        hbio_exp$Herb_Density[hbio_exp$kommnnr == site][2] <- hbio_exp$Red_Deer_Density[hbio_exp$kommnnr == site][1]
                        hbio_exp$Species[hbio_exp$kommnnr == site][2] <- "Red Deer"
                        
                        #Add roe deer density
                        hbio_exp$Herb_Density[hbio_exp$kommnnr == site][3] <- hbio_exp$Roe_Deer_Density[hbio_exp$kommnnr == site][1]
                        hbio_exp$Species[hbio_exp$kommnnr == site][3] <- "Roe Deer"
                        
                }
                
                #Remove columns
                hbio_exp <- hbio_exp[,c(1:10, 14:16)]
                
                #Set correct class
                hbio_exp$Herb_Density <- as.numeric(hbio_exp$Herb_Density)
                hbio_exp$Species <- as.factor(hbio_exp$Species)
                
#END FILTER DATA --------------------------------------------------------------------------        
        
        
        
        
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#GENERATE PLOTS -----------------------------------------------------------------------------
        
        #Define discrete color-blind-friendly palette ----
        pal <- c("#009E73", "#56B4e9", "#e69f00")
                
        #UPDATED LABELS FOR REGIONS -> COUNTIES -----
        hbio_exp$Counties[hbio_exp$Region == "Trøndelag"] <- "Trøndelag"
        hbio_exp$Counties[hbio_exp$Region == "Hedmark"] <- "Innlandet and Viken"
        hbio_exp$Counties[hbio_exp$Region == "Telemark"] <- "Telemark and Vestfold"
                
        #Boxplot
        ggplot(data = hbio_exp, aes(x = Species, y = Herb_Density, fill = Counties)) + 
                geom_boxplot(outlier.shape = NA) +
                labs(x = "Herbivore", y = "Metabolic Biomass " ~(kg/m^2)) +
                theme_bw() +
                scale_fill_manual(values = pal) +
                theme(
                        axis.title.x = element_text(margin = margin(t = 10), size = 14),
                        axis.title.y = element_text(margin = margin(r = 10)),
                        legend.position = c(1,1),
                        legend.justification = c(1,1),
                        legend.background = element_rect(fill="#fafafa",
                                                         size=0.1, linetype="solid", 
                                                         colour ="#666666"),
                        legend.text = element_text(size = 10),
                        legend.title = element_blank(),
                        legend.margin = margin(0,4,4,4),
                        axis.text.x = element_text(size = 11),
                        axis.text.y = element_text(size = 11)
                )

        
#END GENERATE PLOTS --------------------------------------------------------------------------
        
        
                