
############################################################
# Object-based classification  #############################
############################################################

install.packages("randomForest")
install.packages("rgdal")
install.packages("sp")

library("randomForest")
library(raster)
library(rgdal)
library(sp)


#Note: This code starts with segments created in SAGA, with associated features (statistics) also created in SAGA.
#Note: Within Random Forest the column names have to match between training, test and the data frame you want to predict.

# SET WORKING DIRECTORY

getwd()
setwd("D:/mydir")



########################################################
# Run: FUSION Sentinel-2 and Sentinel-1 ################
########################################################




# LOAD DATA  ----------------------------------------------------------------------------------------------------


# Load in the segments with summary stats already calculated in GRASS using "i.segment" and "i.segment.stats".

options(stringsAsFactors = FALSE) #This prevents the ID column becoming a factor

seg <- readOGR("GRASS_seg_vect","s_fusion_2p_001_min5") ## Segments with summary statistics created in GRASS. Takes a while to load. The statistics I use are mean and standard deivation for each segment and each layer in the stack.

#check <- is.na(seg@data) #Check that all of the polygons have stats associated with them as Random Forest won't accept missing values
#table(check)
#summary(seg)
#rm(check)

polys <- seg #Because the polygons take a while to load into R I use this "duplicate variable" throughout 

polys$DN <- 1:length(polys$DN) #this means that ID now starts with 1. I change the IDs to start at 1 because this makes subsetting/indexing easier when selecting polygons for training based on the output from over()

#head(polys)

#polys <- polys[,-c(2,3)] #remove uneccesary columns that have been imported but look messy
#head(polys)

#Load in my training points

tr_pts <- readOGR("tr_200_comp_proportional","tr_200") #Training points
#head(tr_pts)

tr_pts$id <- 1:length(tr_pts$id) #this means that ID now starts with 1, to match the polygons
#tr_pts <- tr_pts[, -3] #Remove unecessary column
#head(tr_pts)

#Load in test points

te_pts <- readOGR("te_400_comp_proportional","te_400") #Test points
#head(te_pts)
te_pts$id <- 1:length(tr_pts$id) #this means that ID now starts with 1, to match the polygons
#tr_pts <- tr_pts[, -3]
#head(tr_pts)



# PREP DATA FOR RANDOM FOREST --------------------------------------------------------------------------------------------


# Get training polygons based on training points

o = over(tr_pts,polys) # This returns the features (including ID) of all of the polygons that have training points inside them
#Note: If there are multiple points in a polygon, its features (including ID) will be returned multiple times.

class(o)  #it is a dataframe and not a spatial object sadly, so I have to link this information with my original polygons
p_indx <- o$DN #Create index based on the output from o
class(p_indx)

#p_indx <- unique(p_indx) #This would remove the duplicate polygons, but I decided not to do this as it would change the number of training and test "points". 


tr_polys <- polys[p_indx,] # Use index to select all the polygons that have training points inside them. 
length(tr_polys$DN) 

tr_polys$cl <- tr_pts$cl #It must automatically order by ID because this works. If not I would have to use merge()


#writeOGR(tr_polys, dsn= "R_training_segments" ,layer= "training_polygons_check2",driver="ESRI Shapefile") #Create a shapefile containing the training polygons for visual inspection


# Because the random forest takes a dataframe as input I export the dataframe from the polygons

tr_data <- tr_polys@data 
#tr_data <- tr_data[!duplicated(tr_data),] #this remove polygons that are repeated... but I didn't do this as I think a few duplicates are not a problem and removing test samples would be a problem when comparing classifications.

#table(tr_data$cl)

# Get test polygons beased on test points

q = over(te_pts,polys) # This returns the features of all of the polygons that have test points inside them 
class(q)  #it is a dataframe
te_p_indx <- q$DN 
class(te_p_indx)  

te_polys <- polys[te_p_indx,]
length(te_polys$DN)

te_polys$cl <- te_pts$cl 

#writeOGR(te_polys, dsn= "R_test_segments" ,layer= "test_polygons_check",driver="ESRI Shapefile") #This creates a shapefile with the test polygons for visualization.

te_data <- te_polys@data 

#te_data <- te_data[!duplicated(te_data),] #this remove polygons that are repeated due to having multiple test points inside them. I chose not to do this (it was not very many anyway)


## All polygons ####

mydata <- polys@data #This extracts the dataframe from the polygons



######################################
### Random Forest Classification ####
######################################



# CREATE RF MODEL

# x is the spectral information as a dataframe (it doesn't accept shapefiles)
# y is the class of the training data (as a factor, so that it works as a classifier and doesn't try regression on my data)
# ntree parameter is set to 500 following Belgiu and Drăguţ 2016
# mtry parameter is set as default
# keep.forest means that the model you build is kept for the next stage: prediction



# head(tr_data) #Check data looks right
# length(tr_data)
# head(te_data)
# length(te_data)


( rf.mdl.fusion <- randomForest(x=tr_data[,3:26], y=as.factor(tr_data[,27]), xtest= te_data[,3:26] ,ytest= as.factor(te_data[,27]), ntree=500, keep.forest = TRUE, proximity=TRUE, importance=TRUE) ) #Rows 3 to 26 are selected because there are 12 layers in the Sentinel-1 + Sentinel-2 stack, and two summary statistics for each (mean + sd).

save(rf.mdl.fusion, file = "rf_mdl_OBIS_fusion2_001_min5.RData" ) #Save th model results for later use

dimnum = 7
confmatrix <- as.matrix(rf.mdl.fusion$test$confusion[,1:dimnum]) 

write.csv(confmatrix, file= "OBIS_fusion2_001_min5.csv") #Save just the confusion matrix for easy access

# PREDICT ENTIRE DATAFRAME

# The model built before (rf.mdl) is used to classify all the polygons. 

map <- predict( rf.mdl.fusion, mydata,  type="response") #Output is a large factor

write.csv(map, "ClassPred0312_OBIS_fusion2_001_min5.csv") 

table(map) #Gives me an initial idea of how much of each class I have

#A large factor is not much use to me, as it has no spatial information. I want the factors (predicted classes in "map") attached to my polygons ("my data")

sapply(mydata, class) #Check the class and column names of my data. For shorter column names...

#colnames(mydata) <- c("ID", "S2_compo_1", "S2_compo_2", "S2_compo_3", "S2_compo_4", "S2_compo_5", "S2_compo_6", "S2_compo_7", "S2_compo_8", "S2_compo_9","S2_compo_10","S1_compo_11", "S1_compo_12")
#head(mydata)


ob_class <- cbind(mydata, map) # This join gives me a dataframe, with no spatial info
head(ob_class)

write.csv(ob_class, "RF_grass_classifications/at_table_cl_OBIS_fusion2_001_min5.csv") 

## Attach dataframe to my polygons

cl_map <- polys
head(cl_map)
# cl_map <- cl_map[, -c(2:26)] #I remove the features to reduce the size of the file
head(cl_map)
cl_map$cl <- ob_class$map #Attach the class label to the polygons
head(cl_map)

writeOGR(cl_map, dsn= "RF_grass_classifications" ,layer= "cl_fusion2_001_min5",driver="ESRI Shapefile") #Save land cover map






#########################################################
# Run: OPTICAL Sentinel-2 composite only ################
#########################################################



# LOAD DATA  ----------------------------------------------------------------------------------------------------


# Load in the segments with summary stats already calculated in GRASS using "i.segment" and "i.segment.stats".


options(stringsAsFactors = FALSE) #This prevents the ID column becoming a factor

seg <- readOGR("GRASS_seg_vect","s_optical_1p_March_001_min5") ## Segments with statistics created in GRASS. Takes a while to load.The statistics I use are mean and standard deivation for each segment and each layer in the stack.

#check <- is.na(seg@data) #Check that all of the polygons have stats associated with them as Random Forest won't accept missing values
#table(check)
#summary(seg)
#rm(check)

polys <- seg #Because the polygons take a while to load I use this "duplicate variable" throughout in case I make a mistake

polys$cat <- 1:length(polys$cat) #cat referes to the polygon ID. this means that ID now starts with 1 #Note: I change the IDs to start at 1 because this makes subsetting/indexing easier when selecting polygons for training based on the output from over()

#head(polys)
#polys <- polys[,-c(2,3)] #remove uneccesary columns that have been imported but look messy
#head(polys)

#Load in my training points

tr_pts <- readOGR("tr_200_comp_proportional","tr_200") #Training points
head(tr_pts)
tr_pts$id <- 1:length(tr_pts$id) #this means that ID now starts with 1, to match the polygons
tr_pts <- tr_pts[, -3] #Remove unecessary column
head(tr_pts)

#Load in test points

te_pts <- readOGR("te_400_comp_proportional","te_400") #Test points
head(te_pts)
te_pts$id <- 1:length(tr_pts$id) #this means that ID now starts with 1, to match the polygons
#tr_pts <- tr_pts[, -3]
head(tr_pts)



# PREP DATA FOR RANDOM FOREST --------------------------------------------------------------------------------------------


# Get training polygons based on training points


o = over(tr_pts,polys) # This returns the features (including ID) of all of the polygons that have training points inside them
#Note: If there are multiple points in a polygon, its features (including ID) will be returned multiple times.

head(polys)
class(o)#it is a dataframe and not a spatial object sadly, so I have to link this information with my original polygons
head(o)
p_indx <- o$cat #Create index based on the output from o
class(p_indx)  
#p_indx <- unique(p_indx)


tr_polys <- polys[p_indx,] # Use index to select all the polygons that have training points inside them. 
length(tr_polys$cat) 

tr_polys$cl <- tr_pts$cl #It must automatically order by ID because this works. If not I would have to use merge()


#writeOGR(tr_polys, dsn= "R_training_segments" ,layer= "training_polygons_check2",driver="ESRI Shapefile") #This has duplicate polygons (where more than one point is in a polygon)


# Because the random forest takes a dataframe as input I export the dataframe from the polygons

tr_data <- tr_polys@data 
#tr_data <- tr_data[!duplicated(tr_data),] #this remove polygons that are repeated... I didn't do this
#table(tr_data$cl) #Gives an idea of how much of each class there is


# Get test polygons beased on test points


q = over(te_pts,polys) # This returns the features of all of the polygons that have points inside them 
class(q)  #it is a dataframe
te_p_indx <- q$cat 
class(te_p_indx)  

te_polys <- polys[te_p_indx,]
length(te_polys$cat)

te_polys$cl <- te_pts$cl 

#writeOGR(te_polys, dsn= "R_test_segments" ,layer= "test_polygons_check",driver="ESRI Shapefile") 

te_data <- te_polys@data 
#te_data <- te_data[!duplicated(te_data),] #this remove polygons that are repeated. I didn't use it


## All polygons ####

mydata <- polys@data #This extracts the dataframe from the polygons I imported earlier (The random forest predicts this later)



######################################
### Random Forest Classification ####
######################################



# CREATE RF MODEL

# x is the spectral information as a dataframe (it doesn't accept shapefiles)
# y is the class of the training data (as a factor, so that it works as a classifier and doesn't try regression on my data)
# ntree parameter is set to 500 following Belgiu and Drăguţ 2016
# mtry parameter is set as default
# keep.forest means that the model you build is kept for the next stage: prediction



# head(tr_data) #Check data looks right
# length(tr_data)
# head(te_data)
# length(te_data)


rf.mdl.optical <- randomForest(x=tr_data[,3:22], y=as.factor(tr_data[,27]), xtest= te_data[,3:22] ,ytest= as.factor(te_data[,27]), ntree=500, keep.forest = TRUE, proximity=TRUE, importance=TRUE) 

save(rf.mdl.optical, file = "rf_mdl_OBIS_optical_001_min5.RData" ) #Save the model results for later use

dimnum = 7
confmatrix <- as.matrix(rf.mdl.optical$test$confusion[,1:dimnum]) 

write.csv(confmatrix, file= "OBIS_optical_001_min5.csv") #Save just the confusion matrix for easy access

# PREDICT ENTIRE DATAFRAME

# The model built before (rf.mdl) is used to classify all the polygons. 


map <- predict(rf.mdl.optical, mydata,  type="response") #Output is a large factor

write.csv(map, "ClassPred0312_OBIS_optical_001_min5.csv") 

#table(map) #Gives me an initial idea of how much of each class I have

#A large factor is not much use to me, as it has no spatial information. I want the factors (predicted classes in "map") attached to my polygons ("my data")

#sapply(mydata, class) #Check the class and column names of my data. For shorter column names...

#colnames(mydata) <- c("ID", "S2_compo_1", "S2_compo_2", "S2_compo_3", "S2_compo_4", "S2_compo_5", "S2_compo_6", "S2_compo_7", "S2_compo_8", "S2_compo_9","S2_compo_10","S1_compo_11", "S1_compo_12")
#head(mydata)

ob_class <- cbind(mydata, map) # This join gives me a dataframe, with no spatial info
head(ob_class)

write.csv(ob_class, "at_table_cl_OBIS_optical_001_min5.csv") #I save it in case it is useful later

## Attach dataframe to my polygons

cl_map <- polys
head(cl_map)
cl_map <- cl_map[, -c(2:26)] #I remove the features to reduce the size of the file
head(cl_map)
cl_map$cl <- ob_class$map #Attach the class label to the polygons
head(cl_map)

writeOGR(cl_map, dsn= "RF_grass_classifications" ,layer= "cl_optical_001_min5",driver="ESRI Shapefile") #Save land cover map


#########################################################
# Run: FUSION Sentinel-1 12/03/2017 only ################
#########################################################



# LOAD DATA  ----------------------------------------------------------------------------------------------------


# Load in the segments with summary stats already calculated in GRASS using "i.segment" and "i.segment.stats".


seg <- readOGR("GRASS_seg_vect","s_radar_1p_001_min5") ## Segments with statistics created in GRASS.

check <- is.na(seg@data) #Check that all of the polygons have stats associated with them as Random Forest won't accept missing values
#table(check)
#summary(seg)
#rm(check)

polys <- seg #Because the polygons take a while to load I use this "duplicate variable" throughout in case I make a mistake


polys$cat <- 1:length(polys$cat) #this means that ID now starts with 1 #Note: I change the IDs to start at 1 because this makes subsetting/indexing easier when selecting polygons for training based on the output from over()

#head(polys)

#polys <- polys[,-c(2,3)] #remove uneccesary columns that have been imported but look messy
#head(polys)

#Load in my training points

tr_pts <- readOGR("tr_200_comp_proportional","tr_200") #Training points
#head(tr_pts)
tr_pts$id <- 1:length(tr_pts$id) #this means that ID now starts with 1, to match the polygons
#tr_pts <- tr_pts[, -3] #Remove unecessary column
#head(tr_pts)

#Load in test points

te_pts <- readOGR("te_400_comp_proportional","te_400") #Test points
#head(te_pts)
te_pts$id <- 1:length(tr_pts$id) #this means that ID now starts with 1, to match the polygons
#tr_pts <- tr_pts[, -3]
#head(tr_pts)




# PREP DATA FOR RANDOM FOREST --------------------------------------------------------------------------------------------


# Get training polygons based on training points


o = over(tr_pts,polys) # This returns the features (including ID) of all of the polygons that have training points inside them
#Note: If there are multiple points in a polygon, its features (including ID) will be returned multiple times.

class(o)  #it is a dataframe and not a spatial object sadly, so I have to link this information with my original polygons
#head(o)
p_indx <- o$cat #Create index based on the output from o
#class(p_indx)  



tr_polys <- polys[p_indx,] # Use index to select all the polygons that have training points inside them. 
length(tr_polys$cat) 

tr_polys$cl <- tr_pts$cl #It must automatically order by ID because this works. If not I would have to use merge()


#writeOGR(tr_polys, dsn= "R_training_segments" ,layer= "training_polygons_check2",driver="ESRI Shapefile") 


# Because the random forest takes a dataframe as input I export the dataframe from the polygons

tr_data <- tr_polys@data 
#tr_data <- tr_data[!duplicated(tr_data),] #this remove polygons that are repeated... I didn't do this
#table(tr_data$cl)


# Get test polygons beased on test points

q = over(te_pts,polys) # This returns the features of all of the polygons that have points inside them 
class(q)  #it is a dataframe
te_p_indx <- q$cat 
class(te_p_indx)  

te_polys <- polys[te_p_indx,]
length(te_polys$cat)

te_polys$cl <- te_pts$cl #The It must automatically order by ID because this works. 

#writeOGR(te_polys, dsn= "R_test_segments" ,layer= "test_polygons_check",driver="ESRI Shapefile") 

te_data <- te_polys@data 
#te_data <- te_data[!duplicated(te_data),] #this remove polygons that are repeated. I didn't use it int he end



## All polygons ####


mydata <- polys@data #This extracts the dataframe from the polygons I imported earlier 


######################################
### Random Forest Classification ####
######################################



# CREATE RF MODEL

# x is the spectral information as a dataframe (it doesn't accept shapefiles)
# y is the class of the training data (as a factor, so that it works as a classifier and doesn't try regression on my data)
# ntree parameter is set to 500 following Belgiu and Drăguţ 2016
# mtry parameter is set as default
# keep.forest means that the model you build is kept for the next stage: prediction



# head(tr_data) #Check data looks right
# length(tr_data)
# head(te_data)
# length(te_data)



rf.mdl.radar <- randomForest(x=tr_data[,3:6], y=as.factor(tr_data[,7]), xtest= te_data[,3:6] ,ytest= as.factor(te_data[,7]), ntree=500, keep.forest = TRUE, proximity=TRUE, importance=TRUE)

save(rf.mdl.radar, file = "rf_mdl_OBIS_radar1_001_min5.RData" ) #Save th model results for later use

dimnum = 7
confmatrix <- as.matrix(rf.mdl.radar$test$confusion[,1:dimnum]) 

write.csv(confmatrix, file= "OBIS_radar1_001_min5.csv") #Save just the confusion matrix for easy access


# PREDICT ENTIRE DATAFRAME
# The model built before (rf.mdl) is used to classify all the polygons. 


map <- predict(rf.mdl.radar, mydata,  type="response") 

write.csv(map, "ClassPred0312_OBIS_radar1_001_min5.csv") 

table(map) #Gives me an initial idea of how much of each class I have

#A large factor is not much use to me, as it has no spatial information. I want the factors (predicted classes in "map") attached to my polygons ("my data")

sapply(mydata, class) #Check the class and column names of my data. For shorter column names...

#colnames(mydata) <- c("ID", "S2_compo_1", "S2_compo_2", "S2_compo_3", "S2_compo_4", "S2_compo_5", "S2_compo_6", "S2_compo_7", "S2_compo_8", "S2_compo_9","S2_compo_10","S1_compo_11", "S1_compo_12")
#head(mydata)


ob_class <- cbind(mydata, map) # This join gives me a dataframe, with no spatial info
head(ob_class)

write.csv(ob_class, "at_table_cl_OBIS_radar1_001_min5.csv") #I save it in case it is useful later

## Attach dataframe to my polygons

cl_map <- polys
head(cl_map)
cl_map <- cl_map[, -c(2:6)] #I remove the features to reduce the size of the file
head(cl_map)
cl_map$cl <- ob_class$map #Attach the class label to the polygons
head(cl_map)

writeOGR(cl_map, dsn= "RF_grass_classifications" ,layer= "cl_radar_001_min5",driver="ESRI Shapefile") #Save land cover map


# --------------------------------------------------- The End ------------------------------------------------------------
