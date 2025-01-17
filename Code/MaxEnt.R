############################################################################
# Maximum entropy and GIS: An approach to Assessing the Settlement Pattern #
# of Ninevite 5 Culture in Northern Mesopotamia (3000-2500 BCE) ############
# Garcia-Ramis, X.; Soriano-Elias, B. & Molist Montañà, M. #################
############################################################################
# Author of code: Soriano-Elias, Biel ######################################

# Code to elaborate Maximum Entropy models #

### GETTING STARTED ###

# LOADING OF NESCESSARRY PACKAGES 
Sys.setenv(JAVA_HOME = "C:/Program Files/OpenJDK/jdk-23.0.1")
paquetes <- c("terra", "sf", "dplyr", "viewscape", "spatstat", "corrplot","dismo","rJava")

for (paquete in paquetes) {
  library(paquete, character.only = TRUE)
}


### LOADING DATA ###

# POINTS

PUNTS_UZGAR <- st_read("Inputs/Points/POINTS_TOTAL_UZGAR_17_KM.shp")
sample <- PUNTS_UZGAR[PUNTS_UZGAR$Id == 0, ] # SPLITING INTO SAMPLE
jac <- PUNTS_UZGAR[PUNTS_UZGAR$Id == 1, ] # SPLITING INTO SITES

# RASTERS

  # Define the folder path containing the raster files
  folder_path <- "Inputs/Variables"

  # List all raster files in the folder
  # Specify the file extensions (.tif)
  raster_files <- list.files(path = folder_path,pattern = "\\.(tif|TIF)$", full.names = TRUE)

  # Extract filenames without extensions
  raster_names <- tools::file_path_sans_ext(basename(raster_files))

  # Load rasters into a list
  raster_list <- lapply(raster_files, rast)

  # Combine rasters into a single SpatRaster stack
  predictors <- rast(raster_list)

  # Stablisch and assign wanted names
  NAMES_2 <- c("AGRICULTURE SUITABILITY", "NUMBER OF ANYSIZE SITES IN SIGHT", "NUMBER OF SIZE 1 SITES IN SIGHT", "NUMBER OF SIZE 2 SITES IN SIGHT", "NUMBER OF SIZE 3 SITES IN SIGHT", "NUMBER OF SIZE 4 SITES IN SIGHT", "COST TIME FROM RIVERS", "HEIGHT AT SITES", "MEAN HEIGHT IN A 10KM BUFFER", "MEAN SLOPE IN A 10KM BUFFER", "MEAN SOLAR IRRADIATION IN A 10KM BUFFER", "SLOPE AT SITE", "SOLAR IRRADIATION AT SITE", "% OF VIEWVED AREA IN 3KM BUFFER", "TPI 300M", "TPI 3000M", "CLASSIFIED TPI")
  names(predictors) <- NAMES_2
  
### EXTRACTING VALUES FRON VARIABLES ###

sample_pred_1 <- extract(predictors[[2:6]], sample,  method = "bilinear", na.rm = TRUE) 
sample_pred_1 <- round(sample_pred_1 + 0.5) #EXTRACTING SITES AT SIGHT VARAIBLES 

sample_pred_2 <- extract(predictors[[14]], sample, method = "bilinear", na.rm = TRUE) # EXTRACTING VIEWSHED PERCENTATGE

sample_pred <- extract(predictors, sample) #EXTRACTING THE OTHER VARAIBLES

sample_pred[ ,3:7] <- sample_pred_1[ ,-1] # UNIFYING VALUES
sample_pred[ ,15] <- sample_pred_2[ ,-1]
sample_pred <- sample_pred[ ,-1]

jac_pred_1 <- extract(predictors[[2:6]], jac,  method = "bilinear", na.rm = TRUE)
jac_pred_1 <- round(jac_pred_1 + 0.5)
jac_pred_2 <- extract(predictors[[14]], jac, method = "bilinear", na.rm = TRUE)
jac_pred <- extract(predictors, jac)
jac_pred[ ,3:7] <- jac_pred_1[ ,-1]
jac_pred[ ,15] <- jac_pred_2[ ,-1]
jac_pred <- jac_pred[ ,-1]


### PEARSONS CORRELATION TEST ###

sample_pred_cor <- cor(sample_pred)

corrplot.mixed(sample_pred_cor,lower.col = "black", number.cex = 1,tl.pos="lt", tl.cex= 1)

# FILTERING OF CORRELATED VARIABLES #

predictors[[1]] <- as.factor(predictors[[1]]) # SETTING AGRICULTURE SUITABILITY RASTER AS FACTOR
jac_pred[1] <- extract(predictors[[1]], jac, ID = FALSE)
sample_pred[1] <- extract(predictors[[1]], sample,  ID = FALSE)

predictors[[17]] <- as.factor(predictors[[17]]) # SETTING TPI CLASSIFIED RASTER AS FACTOR
jac_pred[17] <-  extract(predictors[[17]], jac,  ID = FALSE)
sample_pred[17] <- extract(predictors[[17]], sample,  ID = FALSE)

predictors_final <- predictors[[-c(1,3,4,5,6,9,10,11,12)]]

sample_values <- sample_pred[ ,-c(1,3,4,5,6,9,10,11,12)]
jac_values <- jac_pred[ ,-c(1,3,4,5,6,9,10,11,12)]


### PREPARING DATA FOR MAXENT ### (Results may change!)

pa<- c(rep(1, nrow(jac_values)), rep(0, nrow(sample_values)))
sdmdata <- data.frame(cbind(pa, rbind(jac_values, sample_values)))

train_group <- kfold(sdmdata, 4) #STABLISH THE FOUR PARTITIONS OF DATA


### PARTITION 1 ###
train <- sdmdata[train_group != 1, ] #model training groUp
pres_train<-train[train$pa==1,] #all the presence points in the training group
abs_train<-train[train$pa==0,] # all the absence poings in the training group
test <- sdmdata[train_group == 1, ] #model test groUp
pres_test<-test[test$pa==1,] #all presence in test data
abs_test<-test[test$pa==0,] #all absence in test data

maxent_train<-data.frame(rbind(pres_train,abs_train))
me_pa<-maxent_train[,1]
maxent_train<-maxent_train[,-1]
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if(file.exists(jar)){
  FR.maxent_1<-maxent(x=maxent_train,p=as.factor(me_pa),path=".MaxEnt/1")
}

# Creating and saving the AUC graphic
tiff("AUC_1.tiff", width = 4*300, height = 5*300, res = 300) # Width and height in pixels
plot(evaluate(pres_test, abs_test, FR.maxent_1),'ROC',type='l')
dev.off()

### PARTITION 2 ###
train <- sdmdata[train_group != 2, ] 
pres_train<-train[train$pa==1,] 
abs_train<-train[train$pa==0,] 
test <- sdmdata[train_group == 2, ] 
pres_test<-test[test$pa==1,] 
abs_test<-test[test$pa==0,] 

maxent_train<-data.frame(rbind(pres_train,abs_train))
me_pa<-maxent_train[,1]
maxent_train<-maxent_train[,-1]
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if(file.exists(jar)){
  FR.maxent_2<-maxent(x=maxent_train,p=as.factor(me_pa),path=".MaxEnt/2")
}

tiff("AUC_2.tiff", width = 4*300, height = 5*300, res = 300) # Width and height in pixels
plot(evaluate(pres_test, abs_test, FR.maxent_2),'ROC',type='l')
dev.off()


### PARTITION 3 ###
train <- sdmdata[train_group != 3, ] #model training gropu, not equal to i
pres_train<-train[train$pa==1,] #all the presence points in the training group
abs_train<-train[train$pa==0,] # all the absence poings in the training group
test <- sdmdata[train_group == 3, ] #model test gropu, equal to i
pres_test<-test[test$pa==1,] #all presence in test data
abs_test<-test[test$pa==0,] #all absence in test data

maxent_train<-data.frame(rbind(pres_train,abs_train))
me_pa<-maxent_train[,1]
maxent_train<-maxent_train[,-1]
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if(file.exists(jar)){
  FR.maxent_3<-maxent(x=maxent_train,p=as.factor(me_pa),path=".MaxEnt/3")
}

tiff("AUC_3.tiff", width = 4*300, height = 5*300, res = 300) # Width and height in pixels
plot(evaluate(pres_test, abs_test, FR.maxent_3),'ROC',type='l')
dev.off()

### PARTITION 4 ###
train <- sdmdata[train_group != 4, ] #model training gropu, not equal to i
pres_train<-train[train$pa==1,] #all the presence points in the training group
abs_train<-train[train$pa==0,] # all the absence poings in the training group
test <- sdmdata[train_group == 4, ] #model test gropu, equal to i
pres_test<-test[test$pa==1,] #all presence in test data
abs_test<-test[test$pa==0,] #all absence in test data

maxent_train<-data.frame(rbind(pres_train,abs_train))
me_pa<-maxent_train[,1]
maxent_train<-maxent_train[,-1]
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if(file.exists(jar)){
  FR.maxent_4<-maxent(x=maxent_train,p=me_pa,path=".MaxEnt/4")
}

tiff("AUC_4.tiff", width = 4*300, height = 5*300, res = 300) # Width and height in pixels
plot(evaluate(pres_test, abs_test, FR.maxent_4),'ROC',type='l')
dev.off()


### SAVING RESULTS ###

# VARIABLES INFLUENCE 
tiff("FR.maxent_1.tiff", width = 6*300, height = 4*300, res = 300) # Width and height in pixels
plot(FR.maxent_1)
dev.off()

tiff("FR.maxent_2.tiff", width = 6*300, height = 4*300, res = 300) # Width and height in pixels
plot(FR.maxent_2)
dev.off()

tiff("FR.maxent_3.tiff", width = 6*300, height = 4*300, res = 300) # Width and height in pixels
plot(FR.maxent_3)
dev.off()

tiff("FR.maxent_4.tiff", width = 6*300, height = 4*300, res = 300) # Width and height in pixels
plot(FR.maxent_4)
dev.off()

# VARIABLES RESPONSE

tiff("FR.maxent_1_response.tiff", width = 10*300, height = 4*300, res = 300) # Width and height in pixels
dismo::response(FR.maxent_1, expand = 0.5, cex = 0.1)
dev.off()

tiff("FR.maxent_2_response.tiff", width = 10*300, height = 4*300, res = 300) # Width and height in pixels
dismo::response(FR.maxent_2, expand = 0.5)
dev.off()

tiff("FR.maxent_3_response.tiff", width = 10*300, height = 4*300, res = 300) # Width and height in pixels
dismo::response(FR.maxent_3, expand = 0.5)
dev.off()

tiff("FR.maxent_4_response.tiff", width = 10*300, height = 4*300, res = 300) # Width and height in pixels
dismo::response(FR.maxent_4, expand = 0.5)
dev.off()




