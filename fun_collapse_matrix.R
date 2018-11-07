
### this function collapses two matrices (from classifications A and B) to one binary correct/incorrect confusion matrix. 
#See Table 2A in Foody (2009).

#Function arguments:

# test_ref is the test data shapefile, with a column called "cl" containing the reference label (this is the same test data used in both classifications A and B)
# n_test_points is the number of test points used
# preicted_A is a vector of predicted class labels from the random forest classification.
# preicted_B is a vector of predicted class labels from the random forest classification.


collapse_conf_matrix <- function(test_ref, n_test_points, predicted_A, predicted_B) {
  
  #Classification A: comaprison ref and predicted ####
  
  
  df_A <- as.data.frame(predicted_A)
  test_A <- test_ref
  test_A@data <- cbind(test_A@data, df_A)
  
  c <- test_A@data$cl *10 + as.numeric(test_A@data$predicted) #the unit is the predicted class label, the 10s are the reference class
  
  test_A@data$comparison <- c
  test_A@data$corr <- rep(2, n_test_points)
  test_A@data$corr[test_A@data$comparison==11 | test_A@data$comparison==22 |test_A@data$comparison==33 |test_A@data$comparison==44 |test_A@data$comparison==55 | test_A@data$comparison==66 |test_A@data$comparison==77]<-1 
  #1 is correct
  #2 is incorrect
  
  
  # Classification B: comaprison ref and predicted ####
  
  
  df_B <- as.data.frame(predicted_B)
  test_B <- test_ref
  test_B@data <- cbind(test_B@data, df_B)

  
  c <- test_B@data$cl *10 + as.numeric(test_B@data$predicted)
  
  test_B@data$comparison <- c
  test_B@data$corr <- rep(2, n_test_points)
  test_B@data$corr[test_B@data$comparison==11 | test_B@data$comparison==22 |test_B@data$comparison==33 |test_B@data$comparison==44 |test_B@data$comparison==55 | test_B@data$comparison==66 |test_B@data$comparison==77]<-1 
  #1 is correct
  #2 is incorrect

  
  binary_conf <-  test_A@data$corr *10 + test_B@data$corr #The tens represent classification A, the units classification B
  binary_conf
  t_AB <- table(binary_conf)
  confusion_matrix_AB <-
    matrix(c(t_AB[1],t_AB[3], t_AB[2], t_AB[4]),
           nrow = 2,
           dimnames = list("Classification_A" = c("Correct", "Incorrect"),
                           "Classification_B" = c("Correct", "Incorrect")))
  return(confusion_matrix_AB)
  
}
