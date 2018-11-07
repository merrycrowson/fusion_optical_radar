
### Creates a composite (cloud filled) Sentinel-2 scene for 2017 ##############

#install.packages("raster")
#install.packages("rgdal")

library("raster")
library("rgdal")


# This work is based on the following as a starting point:
# 1) level 2A Sentinel 2 scenes from 2017, pre-processed and cropped to the study area. The 12th of March 2017 was the starting point for the rest of the method as it has the lowest cloud cover of all the dates in 2017.
# 2) A series of "patches" in the form of shapefiles (e.g. "patches_20171018"), that select areas from other dates in 2017 that can fill the areas of cloud in the scene from the 12th of March 2017 (created in QGIS through visual interpretation).



# Patch the cloud-filled areas of the main scene (3rd of March 2017) using scenes from other dates in 2017 ####
# For each scene I creates shapefiles of the cloud-free areas that could be used to fill the maps in the 3rd of Match 2017

setwd("D:/RS_sungai_b/S2/Processed_S2_sb/S2_sb_masked")
masked_scene <- stack("crop_Sentinel2_20170312_sb.tif")

#All scenes are from 2017

# 1217 #### This is the 17th of December 2017.

setwd("D:/RS_sungai_b/S2/Processed_S2_sb/S2_sb_masked") #Location of my patches
# 
#list.files()
patch_1217 <- readOGR(".", "patches_20171217") #This is a shapefile of areas that can be used to fill cloud cover in 03 March scene
target_raster <- raster("D://RS_sungai_b/masks_and_shapes/B2_M.img")#This is band 2 of my master Sentinel 2 image
scene_1217 <- stack("D:/RS_sungai_b/S2/Processed_S2_sb/S2_crop/crop_Sentinel2_20171217_sb.tif")
# 
r_patch_1217 <- rasterize(patch_1217, target_raster, field = 1) #create a raster version of the mask
indx <- r_patch_1217 == 1 
# 
composite_scene <- masked_scene
# 
compareRaster(composite_scene, scene_1217)#Just checks that they have the smae extent, pixel size etc
composite_scene[indx] <- scene_1217[indx] #The relevant areas with clouds in 3rd of March 2017 (defined by the mask) are replaced by the pixels from the 17th of December

# 1122 ####


#list.files()
patch_1122 <- readOGR(".", "patches_20171122")
target_raster <- raster("D://RS_sungai_b/masks_and_shapes/B2_M.img")#This is band 2 of my master Sentinel 2 image
scene_1122 <- stack("D:/RS_sungai_b/S2/Processed_S2_sb/S2_crop/crop_Sentinel2_20171122_sb.tif")

r_patch_1122 <- rasterize(patch_1122, target_raster, field = 1)
indx <- r_patch_1122 == 1

compareRaster(composite_scene, scene_1122)
composite_scene[indx] <- scene_1122[indx]


# 1018 ####


#list.files()
patch_1018 <- readOGR(".", "patches_20171018")
target_raster <- raster("D://RS_sungai_b/masks_and_shapes/B2_M.img")#This is band 2 of my master Sentinel 2 image
scene_1018 <- stack("D:/RS_sungai_b/S2/Processed_S2_sb/S2_crop/crop_Sentinel2_20171018_sb.tif")

r_patch_1018 <- rasterize(patch_1018, target_raster, field = 1)
indx <- r_patch_1018 == 1

compareRaster(composite_scene, scene_1018)
composite_scene[indx] <- scene_1018[indx]


# 0819 ####


#list.files()
patch_0819 <- readOGR(".", "patches_20170819")
target_raster <- raster("D://RS_sungai_b/masks_and_shapes/B2_M.img")#This is band 2 of my master Sentinel 2 image
scene_0819 <- stack("D:/RS_sungai_b/S2/Processed_S2_sb/S2_crop/crop_Sentinel2_20170819_sb.tif")

r_patch_0819 <- rasterize(patch_0819, target_raster, field = 1)
indx <- r_patch_0819 == 1

compareRaster(composite_scene, scene_0819)
composite_scene[indx] <- scene_0819[indx]

# 0730 ####


#list.files()
patch_0730 <- readOGR(".", "patches_20170730")
target_raster <- raster("D://RS_sungai_b/masks_and_shapes/B2_M.img")#This is band 2 of my master Sentinel 2 image
scene_0730 <- stack("D:/RS_sungai_b/S2/Processed_S2_sb/S2_crop/crop_Sentinel2_20170730_sb.tif")

r_patch_0730 <- rasterize(patch_0730, target_raster, field = 1) 
indx <- r_patch_0730 == 1

compareRaster(composite_scene, scene_0730)
composite_scene[indx] <- scene_0730[indx]

# 0725 ####


#list.files()
patch_0725 <- readOGR(".", "patches_20170725")
target_raster <- raster("D://RS_sungai_b/masks_and_shapes/B2_M.img")#This is band 2 of my master Sentinel 2 image
scene_0725 <- stack("D:/RS_sungai_b/S2/Processed_S2_sb/S2_crop/crop_Sentinel2_20170725_sb.tif")

r_patch_0725 <- rasterize(patch_0725, target_raster, field = 1)
indx <- r_patch_0725 == 1


compareRaster(composite_scene, scene_0725)
composite_scene[indx] <- scene_0725[indx]

# 0710 ####


patch_0710 <- readOGR(".", "patches_20170710")
target_raster <- raster("D://RS_sungai_b/masks_and_shapes/B2_M.img")#This is band 2 of my master Sentinel 2 image
scene_0710 <- stack("D:/RS_sungai_b/S2/Processed_S2_sb/S2_crop/crop_Sentinel2_20170710_sb.tif")

r_patch_0710 <- rasterize(patch_0710, target_raster, field = 1)
indx <- r_patch_0710 == 1

composite_scene[indx] <- scene_0710[indx]

# 0531 ####


patch_0531 <- readOGR(".", "patches_20170531")
target_raster <- raster("D://RS_sungai_b/masks_and_shapes/B2_M.img")#This is band 2 of my master Sentinel 2 image
scene_0531 <- stack("D:/RS_sungai_b/S2/Processed_S2_sb/S2_crop/crop_Sentinel2_20170531_sb.tif")

r_patch_0531 <- rasterize(patch_0531, target_raster, field = 1)
indx <- r_patch_0531 == 1

composite_scene[indx] <- scene_0531[indx]

# 0220 ####

patch_0220 <- readOGR(".", "patches_20170220")
target_raster <- raster("D://RS_sungai_b/masks_and_shapes/B2_M.img")#This is band 2 of my master Sentinel 2 image
scene_0220 <- stack("D:/RS_sungai_b/S2/Processed_S2_sb/S2_crop/crop_Sentinel2_20170220_sb.tif")

r_patch_0220 <- rasterize(patch_0220, target_raster, field = 1)
indx <- r_patch_0220 == 1

composite_scene[indx] <- scene_0220[indx]


# 0210 ####


patch_0210 <- readOGR(".", "patches_20170210")
target_raster <- raster("D://RS_sungai_b/masks_and_shapes/B2_M.img")#This is band 2 of my master Sentinel 2 image
scene_0210 <- stack("D:/RS_sungai_b/S2/Processed_S2_sb/S2_crop/crop_Sentinel2_20170210_sb.tif")

r_patch_0210 <- rasterize(patch_0210, target_raster, field = 1)
indx <- r_patch_0210 == 1

composite_scene[indx] <- scene_0210[indx]

#0101


#list.files()
patch_0101 <- readOGR(".", "patches_20170101")
target_raster <- raster("D://RS_sungai_b/masks_and_shapes/B2_M.img")#This is band 2 of my master Sentinel 2 image
scene_0101 <- stack("D:/RS_sungai_b/S2/Processed_S2_sb/S2_crop/crop_Sentinel2_20170101_sb.tif")

r_patch_0101 <- rasterize(patch_0101, target_raster, field = 1)
indx <- r_patch_0101 == 1

compareRaster(composite_scene, scene_0101)
composite_scene[indx] <- scene_0101[indx]


# Write raster to file ####


writeRaster(composite_scene, "composite_optical_20032018.tif", format= "GTiff", overwrite= T) 

## There is still some cloud left in this final product in areas of persistant cloud throughout all of the images available for 2017.



# --------------------------------------------------- The End ------------------------------------------------------------
