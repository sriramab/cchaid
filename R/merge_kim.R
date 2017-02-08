
## THIS FILE IS RESTRICTED TO MERGING OF PREDICTORS,
# F test is the square of t test when compairing a pair
# this function ccmerge gets the DATAFRAME from the user, merges insignificant levels
#' This function merges insignificant categories(levels) of each column within a dataframe
#'
#' @param x a dataframe from user, as constructed using cc_read_file()
#' @return Not yet, but should return a list of lists of dataframes.
#' @export
#' @examples
#' devtools::install_github("sriramab/cchaid")
#' library(cchaid)
#' a=cc_dataset
#' ccmerge(a)

##--------------------------------------------------------------------------##
#                                                                            #
#                       MAIN FUNCTION CALLED BY THE USER                     #
#                                                                            #
##--------------------------------------------------------------------------##
ccmerge <- function(data = x,
                    alpha_merge = 0.05,
                    alpha_split = 0.05) {
  #print(paste(names(data), " -- ", alpha_merge))
  if (!is.data.frame(data))
    stop("data is not a dataframe", call. = TRUE)
  if (alpha_merge > 1)
    warning("p value provided for alpha_merge should be <=1", call. = TRUE)
  
  # Detect each column on its data_type -> function to be used is class;
  # and send it to its appropriate function for merging
  n <- colnames(data[, -c(1, ncol(data))])
  print(n)
  print(str(n))
  
  for (i in n) {
    typeColumn <- class(data[, i])[1]
    switch(typeColumn,
           #IF FACTOR: GO TO NOMINAL_MERGE, IF ORDERED: GO TO ORDINAL_MERGE
           "factor" = {
             
             nominal_merge(data, data[, i], i, alpha_merge, alpha_split)
           }
           , "ordered" = {
             nominal_merge(data, data[, i], i, alpha_merge, alpha_split)
           })
  }
}

##--------------------------------------------------------------------------##
# FUNCTION BELOW MERGES INSIGNIFICANT CATEGORIES WITHIN EACH nominal COLUMN y 
# OF DATAFRAME data
##--------------------------------------------------------------------------##

nominal_merge <- function(data, y, i, alpha_merge, alpha_split) {
  
  l = length(levels(data[,i]))
  
  print(paste(i, " has ", l, " levels"))
  
  if (l <= 2){
    
    print("this is bonferroni correction")
    p_aov = as.matrix(summary((aov(data[, ncol(data)]~data[,i],data)))[[1]][,5])
    print(p_aov)
    
  }else{
     p = (pairwise.t.test(data[, ncol(data)],data[,i],p.adjust.method = "none",paired = FALSE,pool.sd = FALSE,var.equal = TRUE))$p.value
     p_max_value = max(p, na.rm = TRUE) # This picks the max p value, which means the least significant pair.
     
    while(p_max_value < alpha_merge){ #Do merging while p < alpha_merge
      
      p = (pairwise.t.test(data[, ncol(data)],data[,i],p.adjust.method = "none",paired = FALSE,pool.sd = FALSE,var.equal = TRUE))$p.value
      p_max_value = max(p, na.rm = TRUE) # This picks the max p value, which means the least significant pair.
      
      p_max = which(p == max(p, na.rm = TRUE), arr.ind = TRUE) 
      r <- rownames(p)[p_max[, 1]]
      c <- colnames(p)[p_max[, 2]]
      
      nameofMergedCategory <- paste(r, c, sep = "-") #------- MERGED HERE WITH A HYPHEN
      print(paste("merge these categories", nameofMergedCategory))
      print(nameofMergedCategory)
      cat("\n\n")
      
      # CALL MERGING FUNCTION merging_function()
      merging_function(data, y, r, c, nameofMergedCategory, i)
     
    }
  
  print("this is bonferroni correction")
  p_aov = as.matrix(summary((aov(data[, ncol(data)]~data[,i],data)))[[1]][,5])
  
  print(p_aov)
  
  cat("\n\n")
  
}
}

merging_function <- function(data, y, r, c, nameofMergedCategory, i) {
  
  list_of_levels = levels(data[,i])
  
  for (q in seq_along(list_of_levels)) {
    if (list_of_levels[q] == r || list_of_levels[q] == c) {
      list_of_levels[q] = nameofMergedCategory
      
      #print(list_of_levels)
      
    }
  }
  levels(data[,i]) <- list_of_levels
 
  print((data[,i]))
  #print(str(data[,i]))
  cat("\n")
  
  #y=combine_factor(y, list_of_levels)
  return(data)
}


ccmerge(a)
