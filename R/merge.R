## THIS FILE IS RESTRICTED TO MERGING OF PREDICTORS,
# F test is the square of t test when compairing a pair
# this function ccmerge gets the DATAFRAME from the user, merges insignificant levels
#' This function merges insignificant categories(levels) of each column within a dataframe
#' @importFrom rockchalk combineLevels
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
  #print(n)
  
  for (i in n) {
    typeColumn <- class(data[, i])[1]
    switch(typeColumn,
           #IF FACTOR: GO TO NOMINAL_MERGE, IF ORDERED: GO TO ORDINAL_MERGE
           "factor" = {
             nominal_merge(data, i, alpha_merge, alpha_split)
           }
           , "ordered" = {
             ordinal_merge(data,  i, alpha_merge, alpha_split)
           })
  }
  
}

##--------------------------------------------------------------------------##
# FUNCTION BELOW MERGES INSIGNIFICANT CATEGORIES WITHIN EACH nominal COLUMN y 
# OF DATAFRAME data
##--------------------------------------------------------------------------##

nominal_merge <- function(data, i, alpha_merge, alpha_split) {
  l = length(levels(data[,i]))
  
  print(paste(i, " has ", l, " levels"))
  
  p = (
    pairwise.t.test(
      data[, ncol(data)],
      data[,i],
      p.adjust.method = "none",
      paired = FALSE,
      pool.sd = FALSE,
      var.equal = TRUE
    )
  )$p.value
  
  print(p)
  
  if (l > 2) {
    p_max_value = max(p, na.rm = TRUE) # this picks the max p value or the least significant p
    
    ### CHECK FOR ALPHA MERGE
    if (p_max_value < alpha_merge) {
      print("call bonforeni")
      cat("\n\n")
      cat("\n\n")
    } else {
      p_max = which(p == max(p, na.rm = TRUE), arr.ind = TRUE) 
      r <- rownames(p)[p_max[, 1]]
      c <- colnames(p)[p_max[, 2]]
      
      #print(p_max)
      nameofMergedCategory <- paste(r, c, sep = "-") #------- MERGED HERE WITH A HYPHEN
      print(paste("merge these categories", nameofMergedCategory))
      #data[,i] <- rockchalk::combineLevels(data[,i], levs = c(r, c), newLabel = nameofMergedCategory)
      #combineLevels(fac, levs, newLabel)

      
      cat("\n\n")
      
      
      # CALL MERGING FUNCTION merging_function()
      merging_function(data, r, c, nameofMergedCategory, i)
      
    }
    
    
    
  } else{
    print("call bonforeni")
    cat("\n\n")
    cat("\n\n")
  }
  
  
  
}

##--------------------------------------------------------------------------##
# FUNCTION BELOW MERGES INSIGNIFICANT CATEGORIES WITHIN EACH ordinal COLUMN y 
# OF DATAFRAME data
##--------------------------------------------------------------------------##


ordinal_merge <- function(data, i, alpha_merge, alpha_split) {
  #print(paste("inside ordinal merge=", a10))
  
  l = length(levels(data[,i]))
  print(paste(i, "  has ", l, " levels"))
  p = (
    pairwise.t.test(
      data[, ncol(data)],
      data[,i],
      p.adjust.method = "none",
      paired = FALSE,
      pool.sd = FALSE,
      var.equal = TRUE
    )
  )$p.value
  print(p)
  
  if (l > 2) {
    p_max_value <- max(diag(p), na.rm = TRUE) # this picks the max p value or the least significant p
    
    ### CHECK FOR ALPHA MERGE
    if (p_max_value < alpha_merge) {
      print("call bonforeni")
      cat("\n\n")
      cat("\n\n")
    } else {
      p_max = which(p == max(diag(p), na.rm = TRUE), arr.ind = TRUE) #CHECK
      r <- rownames(p)[p_max[, 1]]
      c <- colnames(p)[p_max[, 2]]
      #print(class(r))
      
      #print(p_max)
      nameofMergedCategory <- paste(r, c, sep = "-") #------- MERGED HERE WITH A HYPHEN
      print(paste("merge these categories", nameofMergedCategory))
      cat("\n\n")
      #data[,i] <-rockchalk::combineLevels(data[,i], levs = c(r, c), newLabel = nameofMergedCategory)
     
    }
    
  } else{
    print("call correction")
    cat("\n\n")
    cat("\n\n")
  }
  
  
}


##--------------------------------------------------------------------------##
# THIS FUNCTION BELOW "SHOULD" MERGE INSIGNIFICANT LEVELS BY GIVING THEM A NEW LABEL
# THE IDEA IS TO USE reshape:combine_factor 
# IF r OR c IS FOUND IN PREVIOUS LEVELS (list_of_levels) OF THE FACTOR COLUMN y,
# REPLACE THEM WITH nameofMergedCategory: MEANS THAT THESE TWO LEVELS ARE MERGED FOR BETTER PREDICTION
##--------------------------------------------------------------------------##

merging_function <- function(data, r, c, nameofMergedCategory, i) {
 
   list_of_levels = levels(data[,i])
  
  
  for (q in seq_along(list_of_levels)) {
    if (list_of_levels[q] == r | list_of_levels[q] == c) {
      list_of_levels[q] = nameofMergedCategory
      
    }
  }
  
  cat("\n\n\n")
  
  
  #y=combine_factor(y, list_of_levels)
  
}



