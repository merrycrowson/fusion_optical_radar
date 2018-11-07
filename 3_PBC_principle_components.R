
############################################################
# Pixel-based classification using principle components ####
############################################################

# Add required packages
install.packages("randomForest")
install.packages("sp")
install.packages("rgdal")
install.packages("raster")

require(randomForest)
require(sp)
require(rgdal)
require(raster)
library("RStoolbox")

#Notes:

# Classes used the study: 1 forest, 2 water, 3 urban, 4 plantation-palm, 5 plantation-acacia, 6 fern, 7 plantation-bare

#the order of the layers in the stack (r) is:
#Layers 1:10  bands 2-8, 8A, 11-12 from the Sentinel-2 composite image for 2017 
#Layers 11:12 VV and VH from Sentinel-1 12th of March 2017 


##############################################
# Run: OPTICAL Sentinel-2 composite only #####
##############################################


# Run PCA on optical Sentinel-2 only --------------------------------------------------------------------------------


setwd('D:/mydir')
r <- stack("sentinel_stack.tif") 
r <- scene[[1:10]] #Keep only the optical bands


output.dir<- "D:/place_I_want_to_save_my_PCs/PCA10_optical" # where results of PCA will be stored


# Run PCA:

pca_optical<- rasterPCA(r, nSamples= NULL, spca = TRUE, maskCheck=TRUE, filename=paste(output.dir,'PC1PC10_optical.tif',  sep= "/"))
save(pca_optical, file= paste(output.dir,'pca_optical_10PCAs.Rdata',  sep= "/" ))


summary(pca_optical$model)
pca_optical$model$loadings

# spca=TRUE means PCA is based on the correlation matrix instead of the covariance matrix, which is appropriate for layers with different dynamic ranges


# Investigate the model and save components loadings:

loadings_optical<-loadings(pca_optical$model) # need to save this to see how much each band is weighted in the PCA
ev.mat<-matrix(nrow=10, ncol=10, data=loadings_optical[1:10,]) #Check the number of columns
row.names(ev.mat)<-names(r)
write.csv2(ev.mat, file=paste(output.dir, "loadings_PCA_optical.csv", sep= "/"), quote=FALSE)
write.table(ev.mat, file=paste(output.dir, "loadings_PCA_optical.txt", sep= "/"))

writeRaster(pca_optical$map, "PCA_10_optical0312.tif", format= 'GTiff')

writeRaster(pca_optical$map[[1]], "PCA_10_optical_PCA_1.tif", format= 'GTiff') #Save PCs so I can look at them outside R
writeRaster(pca_optical$map[[2]], "PCA_10_optical_PCA_2.tif", format= 'GTiff')
writeRaster(pca_optical$map[[3]], "PCA_10_optical_PCA_3.tif", format= 'GTiff')
writeRaster(pca_optical$map[[4]], "PCA_10_optical_PCA_4.tif", format= 'GTiff')
writeRaster(pca_optical$map[[5]], "PCA_10_optical_PCA_5.tif", format= 'GTiff')
writeRaster(pca_optical$map[[6]], "PCA_10_optical_PCA_6.tif", format= 'GTiff')
writeRaster(pca_optical$map[[7]], "PCA_10_optical_PCA_7.tif", format= 'GTiff')
writeRaster(pca_optical$map[[8]], "PCA_10_optical_PCA_8.tif", format= 'GTiff')
writeRaster(pca_optical$map[[9]], "PCA_10_optical_PCA_9.tif", format= 'GTiff')
writeRaster(pca_optical$map[[10]],"PCA_10_optical_PCA_10.tif", format= 'GTiff')



# Optical only Random Forest based on PCs -------------------------------------------------------------------


# SET WORKING DIRECTORY

getwd()
setwd("D:/place_I_want_to_save_my_PCs/PCA10_optical")
load("pca_optical_10PCAs.RData")


r <- pca_optical$map


# Read point shapefile (tr_200) with training observations ("cl" field contains a number representing the class)

train_data <- readOGR("tr_200_comp_proportional","tr_200")

test_data <- readOGR("te_400_comp_proportional","te_400")


# EXTRACT RASTER DATA (spectral information from the bands) TO POINTS

train_data <- data.frame(train_data, extract(r, train_data))    #dataframe with the class labels for each reference point in one column, and various columns with the different band values (in this case from the PCs)

test_data <- data.frame(test_data, extract(r, test_data))  


# save(train_data,file="train_data_200_10PCA_optical.Rda") #Purely so I can load it later if needed, as the extract step is computationally intensive
# save(test_data,file="test_data_400_10PCA_optical.Rda")
# 
# write.table(train_data, file= "train_data_PCA_optical_200.txt", row.names = FALSE, col.names = TRUE)
# write.table(test_data, file= "test_data_PCA_optical400.txt", row.names = FALSE, col.names = TRUE)


# CREATE RF MODEL


# x is the spectral information. Select the bands you want to include. The first 5 columns do not contrain spectral information. we visualised the PCs to decide which ones to include
# y is the class of the training data
# ntree parameter is set to 500 following Belgiu and Drăguţ 2016
# mtry parameter is set as default
# keep.forest means that the model you build is kept for the next stage, prediction


# 
# rf.mdl.optical <- randomForest(x=train_data[,6:ncol(train_data)], y=as.factor(train_data[,2]), xtest= test_data[,6:ncol(test_data)] ,ytest= as.factor(test_data[,2]),ntree=500,
#                                  keep.forest = TRUE, proximity=TRUE, importance=TRUE) #This would include all of the PCs

rf.mdl.optical <- randomForest(x=train_data[,6:12], y=as.factor(train_data[,2]), xtest= test_data[,6:12] ,ytest= as.factor(test_data[,2]),ntree=500, keep.forest = TRUE, proximity=TRUE, importance=TRUE) #Includes the first 7 PCs

save(rf.mdl.optical, file = "rf_mdl_PCA_optical.RData" ) #Save the model results for later use

dimnum = 7
confmatrix <- as.matrix(rf.mdl.optical$test$confusion[,1:dimnum])

write.csv(confmatrix, file= "PCA_optical_confmatrix.csv")


# PREDICT SINGLE CLASSIFIED RASTER  

# The model built before (rf.mdl) is used to classify the stack (r).

map <- predict( r[[1:7]], rf.mdl.optical, type="response")

writeRaster(map, "ClassPred0312_PCA_optical.tif", format= 'GTiff')


###################################################
# Run: RADAR Sentinel-1 12/03/2017 only ###########
###################################################


# PCA on radar Sentinel-1 from the 12th of March 2017 #

setwd('D:/mydir')
scene <- stack("sentinel_stack.tif") 
r <- scene[[11:12]] #corresponds to Sentinel-1 VV and VH

output.dir<- "D:/place_I_want_to_save_my_PCs/PCA_radar" # where results of PCA will be stored



# Run PCA: 

pca_radar_1prod<- rasterPCA(r, nSamples= NULL, spca = TRUE, maskCheck=TRUE, filename=paste(output.dir,'PCA_radar_1prod.tif',  sep= "/")) 

save(pca_radar_1prod, file= paste(output.dir,'pca_radar_2PCAs_1prod.Rdata',  sep= "/" ))

# spca=TRUE means PCA is based on the correlation matrix instead of the covariance matrix, which is appropriate for layers with different dynamic ranges


### Investigate the model and save loadings:

names<-names(r)
loadings_radar1<-loadings(pca_radar_1prod$model) # need to save this to see how much each band is weighted in the PCA
ev.mat<-matrix(nrow=2, ncol=2, data=loadings_radar1[1:2,]) #Check the number of columns
row.names(ev.mat)<-names(r)
write.csv2(ev.mat, file=paste(output.dir, "loadings_PCA_radar_1prod.csv", sep= "/"), quote=FALSE)
write.table(ev.mat, file=paste(output.dir, "loadings_PCA_radar_1prod.txt", sep= "/"))

writeRaster(pca_radar_1prod$map, "PCA_2_radar_0312_1prod.tif", format= 'GTiff')

writeRaster(pca_radar_1prod$map[[1]], "PCA_2_radar_0312_PCA1.tif", format= 'GTiff') #Save PCs individually
writeRaster(pca_radar_1prod$map[[2]], "PCA_2_radar_0312_PCA2.tif", format= 'GTiff')



# Radar only Random Forest based on PCs ----------------------------------------------------------------------------------

setwd('D:/place_I_want_to_save_my_PCs/PCA_radar')
load("pca_radar_2PCAs_1prod.Rdata")

summary(pca_radar_1prod$model)
pca_radar_1prod$model$loadings

r <- pca_radar_1prod$map

# Read point shapefile with training observations ("cl" field contains a number representing the class)

train_data <- readOGR("tr_200_comp_proportional","tr_200")

test_data <- readOGR("te_400_comp_proportional","te_400")


# EXTRACT RASTER DATA TO POINTS

train_data <- data.frame(train_data, extract(r, train_data))    #dataframe with the class labels for each reference point in one column, and various columns with the different band values

test_data <- data.frame(test_data, extract(r, test_data))  


# save(train_data,file="train_data_200_2PCA_radar0312.Rda") #Purely so I can load it later if needed, as the extract step is computationally intensive
# save(test_data,file="test_data_400_2PCA_radar0312.Rda")
# 
# write.table(train_data, file= "train_data_PCA_radar_1prod_0312_200.txt", row.names = FALSE, col.names = TRUE)
# write.table(test_data, file= "test_data_PCA_radar_1prod_0312_400.txt", row.names = FALSE, col.names = TRUE)

#load("train_data_200_2PCA_radar0312.Rda") #Purely so I can load it later if needed, as the extract step is computationally intensive
#load("test_data_400_2PCA_radar0312.Rda")



# CREATE RF MODEL

# x is the spectral information. Select the bands you want to include. The first 5 columns do not contrain spectral information. we visualised the PCs to decide which ones to include
# y is the class of the training data
# ntree parameter is set to 500 following Belgiu and Drăguţ 2016
# mtry parameter is set as default
# keep.forest means that the model you build is kept for the next stage, prediction



rf.mdl.radar1 <- randomForest(x=train_data[,6:ncol(train_data)], y=as.factor(train_data[,2]), xtest= test_data[,6:ncol(test_data)], ytest= as.factor(test_data[,2]), ntree=500, keep.forest = TRUE, proximity=TRUE, importance=TRUE)


save(rf.mdl.radar1, file = "rf_mdl_PCA_radar_1prod_0312.RData" ) #Save th model results for later use

dimnum = 7
confmatrix <- as.matrix(rf.mdl.radar1$test$confusion[,1:dimnum])

write.csv(confmatrix, file= "PCA_radar_1prod_0312_confmatrix.csv")

# PREDICT SINGLE CLASSIFIED RASTER  
# The model built before (rf.mdl) is used to classify the image (r).


map <- predict( r, rf.mdl.radar1, type="response")

writeRaster(map, "ClassPred0312_PCA_radar_1prod_0312.tif", format= 'GTiff')


########################################################
# Run: FUSION Sentinel-2 and Sentinel-1 ################
########################################################


setwd('D:/mydir')
scene <- stack("sentinel_stack.tif") 
r <- scene[[1:12]] #all of the bands so that I have both Sentinel-1 and Sentinel-2


output.dir<- "D:/place_I_want_to_save_my_PCs/PCA12_2_products" # where results of PCA will be stored


# Run PCA on Sentinel-2 composite and Sentinel-1 12/03/2017 --------------------------------------------------------------

pca_2products_12 <- rasterPCA(r, nSamples= NULL, nComp=12, spca = TRUE, maskCheck=TRUE, filename=paste(output.dir,'PC1PC12_2products.tif',  sep= "/")) #one raster, one optical
# spca=TRUE means PCA is based on the correlation matrix instead of the covariance matrix, which is appropriate for layers with different dynamic ranges


save(pca_2products_12, file= paste(output.dir,'pca_2products_12PCAs.Rdata',  sep= "/" ))



### Investigate the model and save components loadings:

loadings_2<-loadings(pca_2products_12$model) # need to save this to see how much each band is weighted in the PCA
ev.mat<-matrix(nrow=12, ncol=12, data=loadings_2[1:12,]) #Check the number of columns
row.names(ev.mat)<-names(r)
write.csv2(ev.mat, file=paste(output.dir, "loadings_PCA_PCA12_2products.csv", sep= "/"), quote=FALSE)
write.table(ev.mat, file=paste(output.dir, "loadings_PCA_PCA12_2products.txt", sep= "/"))

writeRaster(pca_2products_12$map, "PCA_12_2products.tif", format= 'GTiff')

writeRaster(pca_2products_12$map[[1]], "PCA_12_2products_PCA_1.tif", format= 'GTiff') #Save individual PCs so that I can look at them
writeRaster(pca_2products_12$map[[2]], "PCA_12_2products_PCA_2.tif", format= 'GTiff')
writeRaster(pca_2products_12$map[[3]], "PCA_12_2products_PCA_3.tif", format= 'GTiff')
writeRaster(pca_2products_12$map[[4]], "PCA_12_2products_PCA_4.tif", format= 'GTiff')
writeRaster(pca_2products_12$map[[5]], "PCA_12_2products_PCA_5.tif", format= 'GTiff')
writeRaster(pca_2products_12$map[[6]], "PCA_12_2products_PCA_6.tif", format= 'GTiff')
writeRaster(pca_2products_12$map[[7]], "PCA_12_2products_PCA_7.tif", format= 'GTiff')
writeRaster(pca_2products_12$map[[8]], "PCA_12_2products_PCA_8.tif", format= 'GTiff')
writeRaster(pca_2products_12$map[[9]], "PCA_12_2products_PCA_9.tif", format= 'GTiff')
writeRaster(pca_2products_12$map[[10]], "PCA_12_2products_PCA_10.tif", format= 'GTiff')
writeRaster(pca_2products_12$map[[11]], "PCA_12_2products_PCA_11.tif", format= 'GTiff')
writeRaster(pca_2products_12$map[[12]], "PCA_12_2products_PCA_12.tif", format= 'GTiff')





# Fusion Random Forest classification based on PCs

# SET WORKING DIRECTORY
getwd()
setwd("D:/place_I_want_to_save_my_PCs/PCA12_2_products")


r <- stack("PC1PC12_2products.tif") #If pca object is not loaded


# Read point shapefile with training observations ("cl" field contains a number representing the class)

train_data <- readOGR("tr_200_comp_proportional","tr_200")

test_data <- readOGR("te_400_comp_proportional","te_400")


# EXTRACT RASTER DATA (spectral information from the bands) TO POINTS

train_data <- data.frame(train_data, extract(r, train_data))    #dataframe with the class labels for each reference point in one column, and various columns with the different band values 

test_data <- data.frame(test_data, extract(r, test_data))  


#save(train_data,file="train_data_200_12PCA_2prod.Rda") #Save for later use as the extract function takes some time
#save(test_data,file="test_data_400_12PCA_2prod.Rda")

#write.table(train_data, file= "train_data_200_12PCA_2prod.txt", row.names = FALSE, col.names = TRUE)
#write.table(test_data, file= "test_data_400_12PCA_2prod.txt", row.names = FALSE, col.names = TRUE)

# load("train_data_200_12PCA_2prod.Rda")
# load("test_data_400_12PCA_2prod.Rda")



# CREATE RF MODEL


# x is the spectral information. Select the bands you want to include. The first 5 columns do not contrain spectral information. we visualised the PCs to decide which ones to include
# y is the class of the training data
# parameters are ntree (set to 500 based on Belgiu and Drăguţ 2016) and mtry (set to default)
# keep.forest means that the model you build is kept for the next stage, prediction



# rf.mdl.fusion <- randomForest(x=train_data[,6:ncol(train_data)], y=as.factor(train_data[,2]), xtest= test_data[,6:ncol(test_data)] ,ytest= as.factor(test_data[,2]),ntree=500,
#     keep.forest = TRUE, proximity=TRUE, importance=TRUE) #This would include all of the PCs

rf.mdl.fusion <- randomForest(x=train_data[,6:14], y=as.factor(train_data[,2]), xtest= test_data[,6:14] ,ytest= as.factor(test_data[,2]),ntree=500, keep.forest = TRUE, proximity=TRUE, importance=TRUE) #I chose the bands to include based on visual inspection  of PCA images as well as model loadings/cumulative 

save(rf.mdl.fusion, file = "rf_mdl_PCA_9PCAs_2products.RData" ) #Save th model results for later use  

dimnum = 7
confmatrix <- as.matrix(rf.mdl.fusion$test$confusion[,1:dimnum])

write.csv(confmatrix, file= "9PCA_2products_confmatrix.csv")

# PREDICT SINGLE CLASSIFIED RASTER  
# The model built before (rf.mdl) is used to classify the image (r).


map <- predict( r[[1:9]], rf.mdl.fusion, type="response") #r is subset to only include the PCs we have chose to include

writeRaster(map, "ClassPred0312_PCA9_2products.tif", format= 'GTiff')


# ----------------------------------------------------- The End -----------------------------------------------------------



