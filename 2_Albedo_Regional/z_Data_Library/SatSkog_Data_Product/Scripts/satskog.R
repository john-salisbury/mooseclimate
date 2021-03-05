## This is a script to process spatial raster data from the SatSkog data product, large herbivore density data,
## and seNorge temp + SWE data (for ALL of Norway)

#It uses SatSkog files for a subset of kommunes (meant to diagnose/re-test issues that main set experienced)


#PACKAGES ----------------------------------------------------------------------

        #Spatial Data Packages
        library(sf)
        library(tmap)
        library(broom)
        
        #Packages for netCDF files
        library(ncdf4)
        library(chron)
        library(reshape2)
        
        #Data Manipulation + Visualization
        library(ggplot2)
        library(raster)
        library(dplyr)
        library(beepr)
        
        #Parallelization
        library(foreach)
        library(exactextractr)
        library(snow)

#END PACKAGES ----------------------------------------------------------------------




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#INDIVIDUAL SHAPEFILE PROCESSING --------------------------------------------------------------------------------------------------

        #PRE-PROCESSING ------------------

        #SENORGE CLIMATE AVERAGES (produced through exclosure component of project)

                #Load climate df
                clim <- read.csv('1_Albedo_Exclosures/z_Data_Library/SeNorge_Climate_Data/Averages/average_climate_data.csv', header = T)
                

        #SATSKOG DATA
        
                #Set age limit of 30 (max age of forest in polygons)
                age_limit <- 30

        #HERBIVORE DENSITY DATA
        
                #Read in vector data of herbivore densities w/ sf package
                hd_shp <- st_read("2_Albedo_Regional/z_Data_Library/Herbivore_Density_Data/NorwayLargeHerbivores.shp")
                
                #Eliminate columns from older dates (to speed up processing)
                
                        #Start w/ W__1949
                        start <- grep("W__1949", colnames(hd_shp))
                        end <- grep("WP_1989", colnames(hd_shp))
                        
                        #Filter out subset
                        hd_shp <- hd_shp[,c(2:(start - 1), (end + 1):ncol(hd_shp))]
                        
                        #Fix 'S__' variables (last digit dropped)
                        names(hd_shp)[grep("S___199", colnames(hd_shp))] <- "S___1999"
                        names(hd_shp)[grep("S___200", colnames(hd_shp))] <- "S___2009"
                        names(hd_shp)[grep("S___201", colnames(hd_shp))] <- "S___2015"
                        
                        #Predefine df's of herbivore densities (1999, 2009, 2015) to speed up processing
                        hd1999 <- hd_shp[,c(1:9, grep("W__1999", colnames(hd_shp)):grep("WP_1999", colnames(hd_shp)))]
                        hd2009 <- hd_shp[,c(1:9, grep("W__2009", colnames(hd_shp)):grep("WP_2009", colnames(hd_shp)))]
                        hd2015 <- hd_shp[,c(1:9, grep("W__2015", colnames(hd_shp)):grep("WP_2015", colnames(hd_shp)))]
                        
                        #Predefine vector of potential years (to speed up processing)
                        herb_years <- c(1999, 2009, 2015)
                        
                        #Predefine vector of column names (these will be used to remove '_Year' from existing
                        #column names, in order to row bind densities from different years)
                        herb_cols <- c('W_Density',
                                       'S_Density',
                                       'Ms_Density',
                                       'Rd_Density',
                                       'R_d_Density',
                                       'M_Density',
                                       'Sh_Density',
                                       'Cw_Density',
                                       'Hf_Density',
                                       'Gt_Density',
                                       'Hr_Density',
                                       'Tt_Density',
                                       'Ct_Density',
                                       'al_Density',
                                       'Lv_Density',
                                       'Wl_Density',
                                       'WP_Density')

        #MISC 
                
                #Define function to get FIRST n characters of a string (used in loop below)
                substrLeft <- function(x, n){
                        substr(x, 1, n)
                }
                
                #Define function to get LAST n characters of a string (used in loop below)
                substrRight <- function(x, n){
                        substr(x, nchar(x)-n+1, nchar(x))
                }




#PROCESS DATA ------------------  

        #Define list of subdirectories to loop through (which contains all shapefiles)
        dirs <- list.dirs(path="2_Albedo_Regional/z_Data_Library/SatSkog_Data_Product/Original_Data", full.names=TRUE, recursive=FALSE)

        #For each directory, process shapefile within
        for(dir in dirs){
                        
                        
                #Reset iterator 
                k <- 1
                
                #Construct directory path
                
                #Get name of direct parent directory (to write files later on in loop)
                dir_base <- strsplit(dir, split = "/")
                dir_base <- dir_base[[1]][length(dir_base[[1]])]
                
                #Get base working directory (to reset to later on in loop)
                wd <- getwd()
                
                #Define output path
                output_path <- paste("2_Albedo_Regional/z_Data_Library/SatSkog_Data_Product/Usable_Data/Processed_Shapefiles/",
                                     dir_base,
                                     '/',
                                     sep = '')
                
                #If output directory already exists, skip loop below
                if( dir.exists(output_path) ){
                        
                        print(paste(dir_base, " Already exists", sep = ""))
                        
                } else {
                        
                        
                        #Create relevant directory
                        dir.create(path = output_path)
                        
                        #SATSKOG SHAPEFILE ------------
                        
                        #Load shapefile
                        
                        #Grab shapefile from subdirectory (w/ .shp extension)
                        file <- list.files(path = dir, pattern="*.shp", full.names=TRUE, recursive=FALSE)
                        
                        #Read shapefile w/ st_read()
                        satskog <- st_read(file)
                        
                        #ERROR HANDLER - if nrow(SatSkog) >0, continue
                        if( nrow(satskog) > 0 ){
                                
                                #Filter SatSkog forest polygons to intended polygons
                                
                                #Filters:
                                #Remove rows w/ age less than 1 (likely invalid observations)
                                #Remove observations with 'reduced' quality images (which may affect accuracy of data)
                                #Filter to 'Clear Sky' observations ('ikke_sky') if possible
                                #Filter pixels with NDVI of 0 (which might indicate non-vegetative pixels)
                                #Filter to observations between age 1 and 'age limit' (younger forest)
                                satskog <- satskog[satskog$alder > 0 &
                                                           (satskog$kvalitet != "redusert" | is.na(satskog$kvalitet)) &
                                                           (satskog$class_sky == 'ikke_sky' | satskog$class_sky == "1.#INF") &
                                                           satskog$ndvi > 0 &
                                                           satskog$alder <= age_limit,]
                                
                                
                                ##ERROR HANDLER - Does satskog still have rows after filtering?
                                if( nrow(satskog) > 0 ){
                                        
                                        
                                        #Determine years of SatSkog pictures (which will be used to select closest herb. data + seNorge data)
                                        
                                        #Convert 'bildedato' to factor
                                        satskog$bildedato <- as.factor(satskog$bildedato)
                                        
                                        #Get 'bildedato' dates in vector
                                        pic_dates <- levels(satskog$bildedato)
                                        
                                        #Project all spatial data on to UTM33 CRS (Same as SatSkog product)
                                        
                                        #Set desired CRS as variable (using the SatSkog CRS as a base)
                                        st_crs_ver <- st_crs(satskog)
                                        crs_ver <- crs(satskog)
                                        
                                        ##FOR EACH PICTURE DATE, PROCESS ACCORDINGLY (USE CORRESPONDING SENORGE DATA + HERB. DENSITY DATA)
                                        
                                        for(date in pic_dates){
                                                
                                                #GET FULL YEAR FOR SATSKOG PICTURE ----------------
                                                
                                                #Isolate last two characters (i.e 2-digit year)
                                                year <- substrRight(date, 2)
                                                
                                                #Convert to full 4 digit year code
                                                ifelse(year > 20, year <- paste("19", year, sep = ""), year <- paste("20", year, sep = ""))
                                                
                                                
                                                
                                                #HERBIVORE DENSITIES ------------
                                                
                                                #Define df w/ herbivore densities from closest year
                                                
                                                        #Chose closest year from pre-defined vector
                                                        herb_year <- herb_years[which.min(abs(herb_years - as.numeric(year)))]
                                                        
                                                        #Choose relevant pre-defined df from global environment
                                                        herb <- get(paste("hd", herb_year, sep=""), envir = .GlobalEnv)
                                                        
                                                        #Rename/generalize herbivore density columns
                                                        # (to allow row binding of densities from different years)
                                                        colnames(herb)[10:26] <- herb_cols
                                                        
                                                        #Add 'herb year' as a new variable/column
                                                        herb$Herb_year <- as.integer(herb_year)
                                                
                                                
                                                #CRS PROJECTION ------------
                                                
                                                        #Change CRS to same as SatSkog
                                                        
                                                        #Herbivore density data
                                                        herb <- st_transform(herb, crs = st_crs_ver)

                                                
                                                #CREATE SATSKOG SUBSET FOR CORRESPONDING DATE ------------
                                                
                                                        #Create subset of polygons from corresponding bildedato
                                                        satskog_sub <- satskog[satskog$bildedato == date,]
                                                        
                                                        #Convert sf object to sp object (required for use w/ raster extract function)
                                                        
                                                                #SatSkog shapefile to sp object
                                                                satskog_sub_sp <- as(satskog_sub, Class = "Spatial")
                                                
                                                #FINAL CHECK:
                                                ## Remove all rows from Satskog file that have NAs
                                                ## (Since it isn't possible to calculate albedo for these plots)
                                                
                                                        #Omit rows w/ NA values
                                                        satskog_sub <- satskog_sub[!is.na(satskog_sub$alder) &
                                                                                           !is.na(satskog_sub$vuprha),]
                                                
                                                
                                                #CALCULATE ALBEDO FOR EACH POLYGON ------------
                                                ##ERROR HANDLER: Only run this step if there are >0 polygons left
                                                        
                                                if( nrow(satskog_sub) > 0 ){
                                                        
                                                        
                                                        #DUPLICATE EACH POLYGON 12x (once for each month)
                                                        #Note: this is a 'long' version of the df
                                                        satskog_sub_exp <- satskog_sub[rep(seq_len(nrow(satskog_sub)), each = 12), ]
                                                        
                                                        #Add vector of months to df
                                                        
                                                                #Expand vector of 1:12 to match # of rows in original/filtered satskog_sub
                                                                #NOTE: Each polygon should have 12x observations of albedo
                                                                months <- rep(c(1:12), times = nrow(satskog_sub))
                                                                
                                                                #Add months to expanded df
                                                                satskog_sub_exp$Month <- months
                                                                
                                                        #Create placeholder column for albedo
                                                        
                                                                #Spruce
                                                                satskog_sub_exp$Spruce_Albedo <- as.numeric('')
                                                                
                                                                #Pine
                                                                satskog_sub_exp$Pine_Albedo <- as.numeric('')
                                                                
                                                                #Deciduous
                                                                satskog_sub_exp$Decid_Albedo <- as.numeric('')
                                                                
                                                        
                                                                
                                                        #Calculate albedo for each polygon ------
                                                                
                                                                #Format area column properly
                                                                satskog_sub_exp$areal_hekt <- as.numeric(gsub(',', '.', satskog_sub_exp$areal_hekt))
                                                                
                                                                       
                                                                #Calculate albedo using equations from Hu et al. (2017)
                                                                ## NOTE: Doing this in a vectorized manner - GREATLY REDUCES TIME
                                                                        
                                                                        for(j in 1:12){
                                                                                
                                                                                #Climate variables
                                                                                temp_k <- clim$Temperature_K[clim$Month == j]
                                                                                swe_mm <- clim$SWE_mm[clim$Month == j]
                                                                                
                                                                                #Spruce albedo
                                                                                satskog_sub_exp$Spruce_Albedo[satskog_sub_exp$Month == j] <- 0.077+0.072*(1-1/(1+exp(-2.354*((temp_k)-273.433))))+0.074*(1/(1+exp(-0.191*((swe_mm)-33.093))))+0.252*exp(-0.023*(satskog_sub_exp$vuprha[satskog_sub_exp$Month == j]))*(1-0.7*exp(-0.011*(swe_mm)))
                                                                                
                                                                                #Pine albedo
                                                                                satskog_sub_exp$Pine_Albedo[satskog_sub_exp$Month == j] <- 0.069+0.084*(1-1/(1+exp(-1.965*((temp_k)-273.519))))+0.106*(1/(1+exp(-0.134*((swe_mm)-30.125))))+0.251*exp(-0.016*(satskog_sub_exp$vuprha[satskog_sub_exp$Month == j]))*(1-0.7*exp(-0.008*(swe_mm)))
                                                                                
                                                                                #Deciduous albedo
                                                                                satskog_sub_exp$Decid_Albedo[satskog_sub_exp$Month == j] <- 0.085+0.089*(1-1/(1+exp(-2.414*((temp_k)-273.393))))+0.169*(1/(1+exp(-0.107*((swe_mm)-37.672))))+0.245*exp(-0.023*(satskog_sub_exp$vuprha[satskog_sub_exp$Month == j]))*(1-0.7*exp(-0.004*(swe_mm)))
                                                                                
                                                                        }
                                                                        

                                                        #FINAL CHANGES TO DF ------------
                                                        

                                                                #JOIN herbivore density data w/ satskog using st_join
                                                                #Use st_join() to get satskog and herbivore densities together
                                                                satskog_sub_exp <- st_join(satskog_sub_exp, herb)
                                                        
                                                        
                                                        #JOIN SATSKOG SUBSETS ------------
                                                        
                                                        #Check iterator (is this date #2, #3, etc?)
                                                        if(k == 1){
                                                                
                                                                satskog_final <- satskog_sub_exp
                                                                
                                                        } else if (k > 1){
                                                                
                                                                satskog_final <- rbind(satskog_final, satskog_sub_exp)
                                                                
                                                        }
                                                        
                                                }               
                                                
                                                #Iterate
                                                k <- k + 1
                                                
                                        } #END LOOP FOR EACH BILDODATE
                                        
                                } else {
                                        
                                        #After filtering, SatSkog has 0 rows - set up for conditional below
                                        satskog_final <- satskog
                                        
                                } #End conditional loop
                                
                                
                                #FINAL CHECK
                                
                                        #Remove rows/polygons where any Moose Density is missing (since this will be removed anyways in final plots)
                                        satskog_final <- satskog_final[!is.na(satskog_final$Ms_Density),]
                                
                                
                                #WRITE FINAL SHAPEFILE TO CORRESPONDING SUBDIRECTORY ------------
                                
                                #Set working directory
                                setwd(output_path)
                                
                                #Write an sf object to output directory (using 'dir_base')
                                #ERROR HANDLER: Only if final sf object >0 rows
                                if( nrow(satskog_final) > 0 ){
                                        
                                        #Write ESRI shapefile
                                        st_write(satskog_final, paste(dir_base, "_processed.shp", sep = ""), driver = "ESRI Shapefile")
                                        
                                } else {
                                        
                                        #Write error txt file
                                        error_msg <- "No polygons meet filtering criteria in this kommune"
                                        write.table(error_msg, file = "message.txt",
                                                    row.names = TRUE, col.names = NA)
                                        
                                }
                                
                                #Reset working directory
                                setwd(wd)
                                
                                
                        } else {
                                
                                #ERROR - No SatSkog data available (rows in initial SatSkog df = 0)
                                
                                #Write error txt file
                                error_msg2 <- "No SatSkog data available for this kommune"
                                write.table(error_msg2, file = paste(output_path, "message.txt", sep = ""),
                                            row.names = TRUE, col.names = NA)
                                
                                
                        } #END 'IF SATSKOG ROWS > 0' LOOP
                        
                        
                } #END 'IF DIRECTORY EXISTS' LOOP
                
        } #END MAIN LOOP
        
        beep(8)
        
#END INDIVIDUAL SHAPEFIILE PROCESSING ----------------------------------------------------------------------------------------------
                
                
                
                
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#SHAPEFILE UNIFICATION -------------------------------------------------------------------------
                
                #Create blank list to store shapefiles
                files <- vector()
                
                #Load all directories w/ individual processed shapefiles
                dirs <- list.dirs(path="2_Albedo_Regional/z_Data_Library/SatSkog_Data_Product/Usable_Data/Processed_Shapefiles/", full.names=TRUE, recursive=FALSE)
                
                #Loop through each directory and load shapefiles into list
                for( dir in dirs ){
                        
                        #Grab shapefile from subdirectory (w/ .shp extension)
                        file <- list.files(path = dir, pattern="*.shp", full.names=TRUE, recursive=FALSE)
                        
                        #Append to file list
                        files <- append(files, file)
                        
                }
                
                #Read all shapefiles in vector and store as list
                shapefile_list <- lapply(files, st_read)
                
                #Bind all individual shapefiles into single shapefile
                
                #Set up final shapefile w/ rest of list items
                #NOTE: This step takes several hours (due to progressive 'slowness' of 'rbind' function)
                
                final_shp <- shapefile_list[[1]]
                
                for(i in 2:length(shapefile_list)){
                        
                        print(i)
                        
                        #Rbind w/ existing df (using rbind instead of bind_rows due to index error)
                        final_shp <- rbind(final_shp, shapefile_list[[i]])
                        
                }
                
                beep(8)
                
        ##ADDITIONAL FILTERING STEPS
                
                
                #Format data correctly
                final_shp$bon1_n <- as.factor(final_shp$bon1_n)
                final_shp$treslag <- as.factor(final_shp$treslag)
                
                # Only 'bonitet' between 11 (unproductive) and 15 (highly productive) - according to SatSkog
                # metadata, 11 should be minimum value
                final_shp$bonitet[final_shp$bonitet < 11] <- NA
                
                #Replace rows where 'bon1_n' is 'Null' w/ NA
                final_shp$bon1_n[final_shp$bon1_n == "Null"] <- NA
                

                #Fix weird variations in SWE, Temp, and Albedo labels
                colnames(final_shp)[43] <- "Spruce_Albedo"
                colnames(final_shp)[44] <- "Pine_Albedo"
                colnames(final_shp)[45] <- "Decid_Albedo"

                #Looks like there are some null values for elevation (-9999) - reassign as NA
                final_shp$dem[final_shp$dem < 0] <- NA
                
                #Add column w/ arbitrary IDs (for use in spatial analysis below)
                ids <- as.vector(1:nrow(final_shp))
                final_shp <- cbind(final_shp, ids)
                rm(ids)

        
                
#END SHAPEFILE UNIFICATION -------------------------------------------------------------------------
                
                
                
                
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\




#WRITE OUTPUT ----------------------------------------------------------------------------
                
        #Write full ESRI shapefile to relevant output directory
        st_write(final_shp, "2_Albedo_Regional/z_Data_Library/SatSkog_Data_Product/Usable_Data/Unified_Shapefile/unified_satskog_shapefile.shp", driver = "ESRI Shapefile", append = FALSE)

       
        beep(8)
                
#END WRITE OUTPUT -------------------------------------------------------------------------                
                
