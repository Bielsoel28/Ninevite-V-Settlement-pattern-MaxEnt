############################################################################
# Maximum entropy and GIS: An approach to Assessing the Settlement Pattern #
# of Ninevite 5 Culture in Northern Mesopotamia (3000-2500 BCE) ############
# Garcia-Ramis, X.; Soriano-Elias, B. & Molist Montañà, M. #################
############################################################################
# Author of code: Soriano-Elias, Biel ######################################

### Function to compute calculate how many sites are seen from a series of cells ###

### OUTPUTS: 5 Raster (.tiff) with the values of each cell (1 general and 4 fr the 4 sizes of sites) ###

### INPUTS:

  # x = Observers points in sf
## CAUTION: HEAVY COMPUTATION LOAD, PARTITION OF THE DATA ADVISED ##
x <- st_read("Inputs/Points/POINTS_TOTAL_UZGAR_17_KM.shp") 

  # y = DEM of the area
y <- rast("Inputs/Varaibles/MDe_UZGAR_PROJECTED.tif")

  # jac = Sites to be observed in sf
jac <- st_read("Inputs/Points/jaciments_UZGAR.shp") 

  # dir = direction where the files should be created
dir <- "XXXXXX/YYYYY/XXXXX"

### FUCTION SET UP ###

SITES_SIGHT <- function(x, y, jac, dir) {
  
  # Loading of DEM
  elevacio <- y
  
  # Loading of observer points
  observadors <- x
  
  # Loading of site points
  jaciments <- jac
  
  # Creation of empty storage raster
  raster_buit <- rast(elevacio)
  values(raster_buit) <- 0
    
    # One for each size
    raster_comptatge <- raster_buit
    raster_t1 <- raster_buit
    raster_t2 <- raster_buit
    raster_t3 <- raster_buit
    raster_t4 <- raster_buit
  
  ncores <- detectCores() #identification of number of pc cores
  
  # Splitting site by size
  punts_1 <- jaciments[jaciments$Mida == 1, ]
  punts_2 <- jaciments[jaciments$Mida == 2, ]
  punts_3 <- jaciments[jaciments$Mida == 3, ]
  punts_4 <- jaciments[jaciments$Mida == 4, ]
  
  print("Final 0")
  
  # Loop for observes
  for (i in 1:nrow(observadors)) {
    
    # Getting observer's coordinates
    cell_desti <- cellFromXY(elevacio, xy= st_coordinates(observadors[i, ]))
    
    # Computing visual basin
    outViewshed <- compute_viewshed(elevacio, observadors[i, ], offset_viewpoint = 1.75, r=3000, workers = ncores, parallel = TRUE, raster = TRUE)
    
    cell_origen <- cellFromXY(outViewshed, xy= st_coordinates(observadors[i, ]))
    
    # Reclassification and polygonize of visual basin
    
      # Reclass
      conca_raster <- classify(outViewshed, cbind(-Inf, 0, NA))
      conca_raster[cell_origen] <- ifelse(!is.na(conca_raster[cell_origen]), NA, conca_raster[cell_origen])
    
      rm(outViewshed)
    
      # Polygonize
      poligon_conca <- st_as_sf(as.polygons(conca_raster, dissolve = TRUE)) 
    
    # Calcultaion of number of sites at sight
    raster_comptatge[cell_desti] <- sum(as.numeric(st_intersects(jaciments, poligon_conca)), na.rm = TRUE) #general
    raster_t1[cell_desti] <- sum(as.numeric(st_intersects(punts_1, poligon_conca)), na.rm = TRUE) #punts_1
    raster_t2[cell_desti] <- sum(as.numeric(st_intersects(punts_2, poligon_conca)), na.rm = TRUE) #punts_2
    raster_t3[cell_desti] <- sum(as.numeric(st_intersects(punts_3, poligon_conca)), na.rm = TRUE) #punts_3
    raster_t4[cell_desti] <- sum(as.numeric(st_intersects(punts_4, poligon_conca)), na.rm = TRUE) #punts_4
    
    print(paste("Final",i))
    
  }
  
  dir <- dir #setting up file directory
  
  writeRaster(raster_comptatge, file.path(dir,"C_general.tif"))
  writeRaster(raster_t1, file.path(dir,"C_S1.tif"))
  writeRaster(raster_t2, file.path(dir,"C_S2.tif"))
  writeRaster(raster_t3, file.path(dir,"C_S3.tif"))
  writeRaster(raster_t4, file.path(dir,"C_S4.tif"))
  
  print("END")
  
}

### RUNNING EXAMPLE###

SITES_SIGHT(x,y, jac, dir)