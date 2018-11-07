
#############################################################
# Pixel-based classification using original pixel values ####
#############################################################

# Add required packages
install.packages("randomForest")
install.packages("sp")
install.packages("rgdal")
install.packages("raster")

require(randomForest)
require(sp)
require(rgdal)
require(raster)


#Notes:

# Classes used the study: 1 forest, 2 water, 3 urban, 4 plantation-palm, 5 plantation-acacia, 6 fern, 7 plantation-bare

#the order of the layers in the stack (r) is:
#Layers 1:10  bands 2-8, 8A, 11-12 from the Sentinel-2 composite image for 2017 
#Layers 11:12 VV and VH from Sentinel-1 12th of March 2017 


# A) PREPARE TRAINING AND TEST DATA -------------------------------------------------------------------------------------

getwd()
setwd("D:/mydir") #Location of the reference data set shapefile


# Read point shapefile with training observations ("cl" field contains a number representing the class)

train_data <- readOGR("tr_200_comp_proportional","tr_200")

# # Read point shapefile with test observations ("cl" field contains a number representing the class)
test_data <- readOGR("te_400_comp_proportional","te_400")

# Read the scene I want to classify
r <-stack( paste(getwd(),"sentinel_stack.tif", sep="/") ) #load in the stack containing Sentinel-2 and Sentinel-1 bands.

#Reminder - the order of the layers in the stack (r) is:
#Layers 1:10  bands 2-8, 8A, 11-12 from the Sentinel-2 composite image for 2017 
#Layers 11:12 VV and VH from Sentinel-1 12th of March 2017 


# Extract raster data (spectral information from the bands) to points

train_data <- data.frame(train_data, extract(r, train_data))   #dataframe with the class labels for each reference point in one column and a columns with the different band values associated with the it.

test_data <- data.frame(test_data, extract(r, test_data))  

# Because the extract function is time-consuming I saved this so I can use it again wihtout re-running:

# save(train_data,file="train_data_200.Rda") 
# save(test_data,file="test_data_400.Rda")

# load("train_data_200.Rda")
# load("test_data_400.Rda")

#write.table(train_data, file= "train_data_200.txt", row.names = FALSE, col.names = TRUE)
#write.table(test_data, file= "test_data_400.txt", row.names = FALSE, col.names = TRUE)




# B) RANDOM FOREST CLASSIFICATIONS --------------------------------------------------------------------------------------


##############################################
# Run: OPTICAL Sentinel-2 composite only #####
##############################################

# r <-stack( paste(getwd(),"sentinel_stack.tif", sep="/") ) #load in the stack containing Sentinel-2 and Sentinel-1 bands.

# Load data
# load("train_data_200.Rda")
# load("test_data_400.Rda")


# CREATE RANDOM FOREST MODEL

# x is the spectral information. Select the bands you want to include. The first few columns do not contrain spectral information. Column 6 is the first optical band in r stack. Hence 6:15 are the 10 optical bands we use in the study.

# y is the class of the training data
# ntree parameter is set to 500 following Belgiu and Drăguţ 2016
# mtry parameter is set to default
# keep.forest means that the model you build is kept for the next stage, prediction


rf.mdl.optical_0312 <- randomForest(x=train_data[,6:15], y=as.factor(train_data[,2]), xtest= test_data[,6:15], ytest= as.factor(test_data[,2]),ntree=500, keep.forest = TRUE, proximity=TRUE, importance=TRUE)

save(rf.mdl.optical_0312, file = "mydir/rf_mdl_optical_0312.RData" ) #Save th model results for later use/reference

dimnum = 7
confmatrix <- as.matrix(rf.mdl.optical_0312$test$confusion[,1:dimnum])

write.csv(confmatrix, file= "mydir/optical0312_confmatrix.csv") #Save confusion matrix

# PREDICT SINGLE CLASSIFIED RASTER 

# The model built before (rf.mdl) is used to classify the image (r). 

map <- predict( r[[1:10]], rf.mdl.optical_0312, type="response") #Bands 1 to 10 are the optical bands in the r stack

writeRaster(map, "ClassPred_optical_0312.tif", format= 'GTiff') 


###################################################
# Run: RADAR Sentinel-1 12/03/2017 only ###########
###################################################


# r <-stack( paste(getwd(),"sentinel_stack.tif", sep="/") ) #load in the stack containing Sentinel-2 and Sentinel-1 bands.

# Load data
# load("train_data_200.Rda")
# load("test_data_400.Rda")


# CREATE RANDOM FOREST MODEL

# x is the spectral information. Select the bands you want to include. The first few columns do not contrain spectral information. Column 6 is the first optical band in r stack. Hence 6:15 are the 10 optical bands we use in the study. Columns 16:17 are Sentinel-1 VV and VH.

# y is the class of the training data
# ntree parameter is set to 500 following Belgiu and Drăguţ 2016
# mtry parameter is set to default
# keep.forest means that the model you build is kept for the next stage, prediction


rf.mdl.radar_0312 <- randomForest(x=train_data[,16:17], y=as.factor(train_data[,2]), xtest= test_data[,16:17] ,ytest= as.factor(test_data[,2]),ntree=500, keep.forest = TRUE, proximity=TRUE, importance=TRUE)

save(rf.mdl.radar_0312, file = "mydir/rf_mdl_radar_0312.RData" ) #Save th model results for later use/reference

dimnum = 7
confmatrix <- as.matrix(rf.mdl.radar_0312$test$confusion[,1:dimnum])

write.csv(confmatrix, file= "cl_S1_0312/radar0312_confmatrix.csv")

# PREDICT SINGLE CLASSIFIED RASTER  
# The model built before (rf.mdl) is used to classify the image (r). 

map <- predict( r[[11:12]], rf.mdl.radar_0312, type="response") #Bands 11 to 12 are radar VV and VH in the r stack

writeRaster(map, "ClassPred_radar_0312.tif", format= 'GTiff')

########################################################
# Run: FUSION Sentinel-2 and Sentinel-1 ################
########################################################



# r <-stack( paste(getwd(),"sentinel_stack.tif", sep="/") ) #load in the stack containing Sentinel-2 and Sentinel-1 bands.

# Load data
# load("train_data_200.Rda")
# load("test_data_400.Rda")


# CREATE RANDOM FOREST MODEL

# x is the spectral information. Select the bands you want to include. The first few columns do not contrain spectral information. Column 6 is the first optical band in r stack. Hence 6:15 are the 10 optical bands we use in the study. Columns 16:17 are Sentinel-1 VV and VH.

# y is the class of the training data
# ntree parameter is set to 500 following Belgiu and Drăguţ 2016
# mtry parameter is set to default
# keep.forest means that the model you build is kept for the next stage, prediction

# SET WORKING DIRECTORY
getwd()
setwd("D:/RS_sungai_b/Classifications/comp_fusion_proportional")

r <-stack( paste(getwd(),"S2_composite20180320_S1_6products.tif", sep="/") )

# Load data
load("train_data_200.Rda")
load("test_data_400.Rda")


# CREATE RF MODEL
# x is the spectral information. Select the bands you want to include.
# y is the class of the training data
# ntree can be varied to see how it effects results
# keep.forest means that the model you build is kept for the next stage, prediction

rf.mdl.fusion_Mar <- randomForest(x=train_data[,6:17], y=as.factor(train_data[,2]), xtest= test_data[,6:17] ,ytest= as.factor(test_data[,2]),ntree=500, keep.forest = TRUE, proximity=TRUE, importance=TRUE)

save(rf.mdl.fusion_Mar, file = "mydir/rf_mdl_fusion_12Mar.RData" ) #Save the model results for later use/reference

dimnum = 7
confmatrix <- as.matrix(rf.mdl.fusion_Mar$test$confusion[,1:dimnum])

write.csv(confmatrix, file= "mydir/fusion_12Mar_confmatrix.csv")

# PREDICT SINGLE CLASSIFIED RASTER  

# The model built before (rf.mdl) is used to classify the image (r).

map <- predict( r[[1:12]], rf.mdl.fusion_Mar, type="response")

writeRaster(map, "ClassPred_fusion_12Mar.tif", format= 'GTiff')


# ----------------------------------------- The End ----------------------------------------------