############################################################################
# Maximum entropy and GIS: An approach to Assessing the Settlement Pattern #
# of Ninevite 5 Culture in Northern Mesopotamia (3000-2500 BCE) ############
# Garcia-Ramis, X.; Soriano-Elias, B. & Molist Montañà, M. #################
############################################################################
# Author of code: Soriano-Elias, Biel ######################################

### Classification of TPIs (One big and one small) to 10 Landform categories ###

### OUTPUT = Raster (.tiff) of the Classified Landforms ###

### INPUTS: ###
  # TPIB = TPI raster of a big radius 
TPIB <- rast("Inputs/Varaibles/TPI_3000_UZGAR.tif")

  # TPIS = TPI raster of a small radius
TPIS <- rast("Inputs/Varaibles/TPI_300_UZGAR.tif")

  # SlopeDegrees = Slope raster in degrees
SlopeDegrees <- rast("Inputs/Varaibles/SLOPE_UZGAR_PROJECTED.tif")

  # Name = Vector with the name of the area
name <- UZGAR

### FUCTION SET UP ###

Class10 <- function(TPIB,TPIS,SlopeDegrees,name) {
  
  # Extract the values from the rasters as a numeric vectors
  slope <- values(SlopeDegrees, na.rm = TRUE)
  tpi_values_B <- values(TPIB, na.rm = TRUE)
  tpi_values_S <- values(TPIS, na.rm = TRUE)
  
  # Calculate the mean and standard deviation
  mean_TPI_B <- mean(tpi_values_B)
  std_TPI_B <- sd(tpi_values_B)
  
  mean_TPI_S <- mean(tpi_values_S)
  std_TPI_S <- sd(tpi_values_S)
  
  # Normalize the TPI values
  TPI_B_normalized <- as.integer((tpi_values_B - mean_TPI_B) / std_TPI_B)
  TPI_S_normalized <- as.integer((tpi_values_S - mean_TPI_S) / std_TPI_S)
  
  # Initialize an empty vector for classification results
  classification <- vector("character", length = length(TPI_S_normalized))
  
print("Final 1")
  
for (i in 1:length(TPI_S_normalized)) {
  TPI_SN <- TPI_S_normalized[i]   # Extract SN (TPIst) value
  TPI_LN <- TPI_B_normalized[i]   # Extract LN (TPIst) value
  slope_ind <- slope[i]               # Extract slope value
  
  # Classification logic based on TPI (SN and LN) and slope conditions
  if (TPI_SN <= -1) {
    if (TPI_LN <= -1) {
      classification[i] <- "Canyons, V-shaped valleys"
    } else if (-1 < TPI_LN && TPI_LN < 1) {
      classification[i] <- "Mid-slope drainage, shallow valley"
    } else if (TPI_LN >= 1) {
      classification[i] <- "Highland drainage, headwaters"
    }
  } else if (-1 < TPI_SN && TPI_SN < 1) {
    if (TPI_LN <= -1) {
      classification[i] <- "U-shaped valleys"
    } else if (-1 < TPI_LN && TPI_LN < 1) {
      # Check if slope is NA
      if (is.na(slope_ind)) {
        classification[i] <- "Undefined plain area"
      } else {
        if (slope_ind <= 5) {
          classification[i] <- "Plains"
        } else {
          classification[i] <- "Open slopes"
        }
      }
    } else if (TPI_LN >= 1) {
      classification[i] <- "Upper slopes"
    }
  } else if (TPI_SN >= 1) {
    if (TPI_LN <= -1) {
      classification[i] <- "Local ridges/Hills in valleys"
    } else if (-1 < TPI_LN && TPI_LN < 1) {
      classification[i] <- "Middle ridges, small hills in plains"
    } else if (TPI_LN >= 1) {
      classification[i] <- "Peaks, high ridges"
    }
  }
}
print("Final 2")

  # Convert classification to a raster or data frame if needed
  classification_raster <- TPIS
  values(classification_raster)[!is.na(values(classification_raster))] <- classification
  
  # Save the classification raster (if it's a raster object)
  dir <- "Inputs/Variables" #setting up file directory
  writeRaster(classification_raster, file.path(dir,paste0(name,"_classified_TPI_raster.tif")))
  
print("End")
  
}

### RUNNING EXAMPLE###

Class10(TPIB, TPIS, SlopeDegrees, name)