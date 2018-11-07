
# Pixel-based classification using OPVs the original pixel values
# Object-based classification
# Pixel-based classification using principal components (PCs)


####################################################################################################################
############## McNemar's test comparing the results from the three different approaches to data fusion #############
####################################################################################################################

install.packages("rgdal")

library("rgdal")

setwd("D:/mydir/Fusion_comparison")


### Fusion in a pixel-based classification using the original pixel values vrs fusion in an Object-based classificatio ----

#Data prep

test_ref <- readOGR("my_path", "te_400" ) #Load test data used in both of the classifications
#head(test_ref)

load("rf_mdl_fusion_12Mar.RData") #Classification A (random forest model)
predicted_A <- as.numeric(rf.mdl.fusion_Mar$test$predicted) #Predicted labels for test data, classification A

load("rf_mdl_OBIS_fusion2_001_min5.RData") #Classification B
predicted_B <- as.numeric(rf.mdl.fusion$test$predicted) #Predicted labels for test data, classification B

# Collapse the confusion matrices

collapse_conf_matrix <- dget("fun_collapse_matrix.R")

dl_ol_fusion_matrix <- collapse_conf_matrix(test_ref, 2800, predicted_A, predicted_B) 
dl_ol_fusion_matrix
save(dl_ol_fusion_matrix, file="McNemar_tests_3approaches/dl_ol_fusion_matrix.RData")

# Apply McNemar's statistic to compare the two classifications

dl_ol_fusion_mcnemar<- mcnemar.test(dl_ol_fusion_matrix, y = NULL, correct = FALSE)
dl_ol_fusion_mcnemar
save(dl_ol_fusion_mcnemar, file="McNemar_tests_3approaches/dl_ol_fusion_mcnemar.RData")



### fusion in an Object-based classificatio vrs Fusion in a pixel-based classification using principle components  ----


test_ref <- readOGR("my_path", "te_400" ) #Load test data used in both of the classifications
#head(test_ref)

load("rf_mdl_OBIS_fusion2_001_min5.RData") #Classification A (random forest model)
predicted_A <- as.numeric(rf.mdl.fusion$test$predicted) #Predicted labels for test data, classification A

load("rf_mdl_PCA_9PCAs_2products.RData") #Classification B (random forest model)
predicted_B <- as.numeric(rf.mdl.fusion$test$predicted) #Predicted labels for test data, classification B


# Collapse the confusion matrices

collapse_conf_matrix <- dget("fun_collapse_matrix.R")

ol_pl_fusion_matrix <- collapse_conf_matrix(test_ref, 2800, predicted_A, predicted_B)
ol_pl_fusion_matrix
save(ol_pl_fusion_matrix, file="McNemar_tests_3approaches/ol_pl_fusion_matrix.RData")


# Apply McNemar's statistic to compare the two classifications

ol_pl_fusion_mcnemar<- mcnemar.test(ol_pl_fusion_matrix, y = NULL, correct = FALSE)
ol_pl_fusion_mcnemar
save(ol_pl_fusion_mcnemar, file="McNemar_tests_3approaches/ol_pl_fusion_mcnemar.RData")


### Fusion in a pixel-based classification using principle components vrs Fusion in a pixel-based classification using the original pixel values ----


test_ref <- readOGR("my_path", "te_400" )
#head(test_ref)

load("rf_mdl_PCA_9PCAs_2products.RData") # Classification A (random forest model)
predicted_A <- as.numeric(rf.mdl.fusion$test$predicted) # Predicted labels for test data, classification A

load("rf_mdl_fusion_12Mar.RData") # Classification B (random forest model)
predicted_B <- as.numeric(rf.mdl.fusion_Mar$test$predicted) # Predicted labels for test data, classification B


# Collapse the confusion matrices

collapse_conf_matrix <- dget("fun_collapse_matrix.R")

pl_dl_fusion_matrix <- collapse_conf_matrix(test_ref, 2800, predicted_A, predicted_B)
pl_dl_fusion_matrix
save(pl_dl_fusion_matrix, file="McNemar_tests_3approaches/pl_dl_fusion_matrix.RData")

# Apply McNemar's statistic to compare the two classifications

pl_dl_fusion_mcnemar<- mcnemar.test(pl_dl_fusion_matrix, y = NULL, correct = FALSE)
pl_dl_fusion_mcnemar
save(pl_dl_fusion_mcnemar, file="McNemar_tests_3approaches/pl_dl_fusion_mcnemar.RData")



#########################################  Collate results ##############################################

setwd("D:/mydir/McNemar_tests_3approaches")

#mcnemar_3approaches <- list.files() #This loads the results calculated earlier, if they are not still in the environment
#load(mcnemar_3approaches[2])
#load(mcnemar_3approaches[4])
#load(mcnemar_3approaches[6])
#str(dl_ol_fusion_mcnemar)

dl_ol_mcnemar <- as.data.frame(dl_ol_fusion_mcnemar$data.name)
dl_ol_mcnemar$statistic <- dl_ol_fusion_mcnemar$statistic
dl_ol_mcnemar$p <- dl_ol_fusion_mcnemar$p.value
names(dl_ol_mcnemar)[1] <- "McNemar's chi-squared"
dl_ol_mcnemar


ol_pl_mcnemar <- as.data.frame(ol_pl_fusion_mcnemar$data.name)
ol_pl_mcnemar$statistic <- ol_pl_fusion_mcnemar$statistic
ol_pl_mcnemar$p <- ol_pl_fusion_mcnemar$p.value
names(ol_pl_mcnemar)[1] <- "McNemar's chi-squared"
ol_pl_mcnemar

pl_dl_mcnemar <- as.data.frame(pl_dl_fusion_mcnemar$data.name)
pl_dl_mcnemar$statistic <- pl_dl_fusion_mcnemar$statistic
pl_dl_mcnemar$p <- pl_dl_fusion_mcnemar$p.value
names(pl_dl_mcnemar)[1] <- "McNemar's chi-squared"
pl_dl_mcnemar


mcnemar_3approaches <- rbind(dl_ol_mcnemar, ol_pl_mcnemar, pl_dl_mcnemar)
mcnemar_3approaches

write.csv(mcnemar_3approaches, "mcnemar_3approaches_comparison.csv" )



##########################################################################################################################
############ McNemar's test comparing fusion vrs optical only in the three different classification approaches ###########
##########################################################################################################################

library("rgdal")


### Fusion vrs optical only in a pixel-based classification using the original pixel values -----------------------------

setwd("D:/mydir/Fusion_comparison")


test_ref <- readOGR("my_path", "te_400" ) #Load test data
#head(test_ref)

load("rf_mdl_fusion_12Mar.RData") #Classification A (random forest model)
load("rf_mdl_optical_0312.RData") #Classification B (random forest model)



predicted_A <- as.numeric(rf.mdl.fusion_Mar$test$predicted) #Predicted labels for test data, classification A
predicted_B <- as.numeric(rf.mdl.optical_0312$test$predicted) #Predicted labels for test data, classification B


collapse_conf_matrix <- dget("fun_collapse_matrix.R")

dl_fusion_optical_matrix <- collapse_conf_matrix(test_ref, 2800, predicted_A, predicted_B)
save(dl_fusion_optical_matrix, file="McNemar_tests_fusion_optical/dl_fusion_optical_matrix.RData")

dl_fusion_optical_mcnemar<- mcnemar.test(dl_fusion_optical_matrix, y = NULL, correct = FALSE)
dl_fusion_optical_mcnemar
save(dl_fusion_optical_mcnemar, file="McNemar_tests_fusion_optical/dl_fusion_optical_mcnemar.RData")


### Fusion vrs optical only in an object-based classification ------------------------------------------------------------


test_ref <- readOGR("my_path", "te_400" )
#head(test_ref)

load("rf_mdl_OBIS_fusion2_001_min5.RData") #Classification A (random forest model)
load("rf_mdl_OBIS_optical_001_min5.RData") #Classification B (random forest model)



predicted_A <- as.numeric(rf.mdl.fusion$test$predicted) #Predicted labels for test data, classification A
predicted_B <- as.numeric(rf.mdl.optical$test$predicted) #Predicted labels for test data, classification B



collapse_conf_matrix <- dget("fun_collapse_matrix.R")

ol_fusion_optical_matrix <- collapse_conf_matrix(test_ref, 2800, predicted_A, predicted_B)
ol_fusion_optical_matrix
save(ol_fusion_optical_matrix, file="ol_McNemar_tests_fusion_optical/ol_fusion_optical_matrix.RData")

ol_fusion_optical_mcnemar<- mcnemar.test(ol_fusion_optical_matrix, y = NULL, correct = FALSE)
ol_fusion_optical_mcnemar
save(ol_fusion_optical_mcnemar, file="ol_McNemar_tests_fusion_optical/ol_fusion_optical_mcnemar.RData")



### Fusion vrs optical only in a pixel-based classification using principle components -----------------------------


test_ref <- readOGR("my_path", "te_400" )
#head(test_ref)

load("rf_mdl_PCA_9PCAs_2products.RData") #Classification A (random forest model)
load("rf_mdl_PCA_optical.RData") #Classification B (random forest model)



predicted_A <- as.numeric(rf.mdl.fusion$test$predicted) #Predicted labels for test dara, classification A
predicted_B <- as.numeric(rf.mdl.optical$test$predicted) #Predicted labels for test dara, classification A


collapse_conf_matrix <- dget("fun_collapse_matrix.R")

pl_fusion_optical_matrix <- collapse_conf_matrix(test_ref, 2800, predicted_A, predicted_B)
pl_fusion_optical_matrix
save(pl_fusion_optical_matrix, file="pl_McNemar_tests_fusion_optical/pl_fusion_optical_matrix.RData")

pl_fusion_optical_mcnemar<- mcnemar.test(pl_fusion_optical_matrix, y = NULL, correct = FALSE)
pl_fusion_optical_mcnemar
save(pl_fusion_optical_mcnemar, file="pl_McNemar_tests_fusion_optical/pl_fusion_optical_mcnemar.RData")



################### Collate results ################################


#load("comp_fusion_proportional/McNemar_tests_fusion_optical/dl_fusion_optical_mcnemar.RData") #Load the McNemar results into the environment if they are not already there
#load("OBIS_classification_fusion/ol_McNemar_tests_fusion_optical/ol_fusion_optical_mcnemar.RData")
#load("Pixel_level_fusion/pl_McNemar_tests_fusion_optical/pl_fusion_optical_mcnemar.RData")


dl_mcnemar <- as.data.frame(dl_fusion_optical_mcnemar$data.name)
dl_mcnemar$statistic <- dl_fusion_optical_mcnemar$statistic
dl_mcnemar$p <- dl_fusion_optical_mcnemar$p.value
names(dl_mcnemar)[1] <- "McNemar's chi-squared"
dl_mcnemar


ol_mcnemar <- as.data.frame(ol_fusion_optical_mcnemar$data.name)
ol_mcnemar$statistic <- ol_fusion_optical_mcnemar$statistic
ol_mcnemar$p <- ol_fusion_optical_mcnemar$p.value
names(ol_mcnemar)[1] <- "McNemar's chi-squared"
ol_mcnemar

pl_mcnemar <- as.data.frame(pl_fusion_optical_mcnemar$data.name)
pl_mcnemar$statistic <- pl_fusion_optical_mcnemar$statistic
pl_mcnemar$p <- pl_fusion_optical_mcnemar$p.value
names(pl_mcnemar)[1] <- "McNemar's chi-squared"
pl_mcnemar


mcnemar_fusion_optical <- rbind(dl_mcnemar, ol_mcnemar, pl_mcnemar)
mcnemar_fusion_optical

write.csv(mcnemar_fusion_optical, "mcnemar_fusion_optical_comparison.csv" )


### -------------------------------------------------- The End ----------------------------------------------------------
