############################################################################
# Maximum entropy and GIS: An approach to Assessing the Settlement Pattern #
# of Ninevite 5 Culture in Northern Mesopotamia (3000-2500 BCE) ############
# Garcia-Ramis, X.; Soriano-Elias, B. & Molist Montañà, M. #################
############################################################################
# Author of code: Soriano-Elias, Biel ######################################

### Function to compute Totalviewshed of a set of points #

### OUTPUTS: Raster (.tiff) with the values of each cell-point ###

### INPUTS: ###

  # x = Observers points in sf
## CAUTION: HEAVY COMPUTATION LOAD, PARTITION OF THE DATA ADVISED ##
x <- st_read("Inputs/Points/POINTS_TOTAL_UZGAR_17_KM.shp") 

  # y = DEM raster with a projected CRS
y <- rast("Inputs/Varaibles/MDe_UZGAR_PROJECTED.tif")

  # q = Distance of radius of the viewshed in meters
q <- 3000

  # dir = direction where the files should be created
dir <- "XXXXXX/YYYYY/XXXXX"

# r = Re-scaling of the values. Either TRUE or FALSE (TRUE by default)

Visual_basin_percent <- function(x,y,q,dir,r = TRUE) {
  
  #Loading of the DEM raster
  elevacio <- y
  
  #Loading of the observer points
  observadors <- x 
  
  #Creation of an empty raster to store values
  ext_rast <- ext(elevacio)
  res_rast <- res(elevacio)
  raster_buit <- rast(ext = ext_rast, res = res_rast, vals = NA, crs = crs(elevacio))
  
  ncores <- detectCores() #identification of number of pc cores
  
  viewshed <- compute_viewshed(elevacio, observadors, r = 3000 , parallel = TRUE, workers = ncores, raster = TRUE)
  
  #Loop to calculate the viewshed of each point
  for (i in 1:nrow(observadors)) {
    
    cell <- cellFromXY(elevacio, xy= st_coordinates(observadors[i, ]))
    
    raster_buit[cell] <- sum(viewshed[[i]]@visible)
    
  } 
  
  #Re-scaling of the values
  if(r) { 
    
    rast_min <- terra::minmax(raster_buit)[1]
    rast_max <- terra::minmax(raster_buit)[2]
    raster_buit <- ((raster_buit - rast_min)/(rast_max - rast_min)) 
    
  }

#Substituting NA cells of the DEM
raster_buit <- mask(raster_buit, elevacio, maskvalues = NA)

#saving of the raster's file  
nom_arxiu <- paste(substitute(x),"total_viewshed.tif") #creation of file name
dir <- dir #setting up file directory
writeRaster(raster_buit, filename = file.path(dir,nom_arxiu)) #saving of the raster of viewshed

print("END")

}

### RUNNING EXAMPLE###

Visual_basin_percent(x,y,q, dir)