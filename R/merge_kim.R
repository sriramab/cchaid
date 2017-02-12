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
  
  z<-c(0)
  p_aov_val <- list("numeric") # Initialization of a list to store p-values for respective predictor.
  
  for (i in n) {
    p_aov<-merging_loop(data, data[, i], i, alpha_merge, alpha_split)[1] #Get adj. p-values with Bonferroni correction, which is an output of "merging_loop" function
    #print(p_aov)
    
    z<-z+1
    p_aov_val[z]<-p_aov # List of p-value for respective predictors.
   
   cat("\n")
   
  }
  
  names(p_aov_val)<-n
  print(p_aov_val)
  
  t<-(which.min(p_aov_val)) # Find the predictor with most significant split.
  
  return(t)
}

##--------------------------------------------------------------------------##
# FUNCTION BELOW MERGES INSIGNIFICANT CATEGORIES WITHIN EACH nominal COLUMN data[,i]  
# OF DATAFRAME data AND SPITS OUT ADJ. P-VALUE WITH BONFERRONI CORRECTION
##--------------------------------------------------------------------------##

merging_loop <- function(data, y, i, alpha_merge, alpha_split) {
  
  l = length(levels(data[,i]))  # of categories in a predictor data[,i]
  
  print(paste(colnames(data[i]), " has ", l, " levels"))
  cat("\n")
  
  if (l <= 2){
    p_aov = as.matrix(summary((aov(data[, ncol(data)]~data[,i],data)))[[1]][,5])[1]
    print(paste("========================Finished Merging loop=========================="))
    
    #p_adj = p_aov * Bonferroni_correction(type,c,r)
    
    print(paste("Adusted p-value with bonferroni correction:",p_aov))
    cat("\n\n")
  }else{
    k<-c(0)    #index k for loop
    repeat{     #Do merging while l=>2 or p < alpha_merge
     
      k<-k+1   
      
      p = (pairwise.t.test(data[, ncol(data)],data[,i],p.adjust.method = "none",paired = FALSE,pool.sd = FALSE,var.equal = TRUE))$p.value
      
      if(class(data[,i])[1]=="factor"){
        p_max_value = max(p, na.rm = TRUE) # This picks the max p value for nominal predictor, which means the least significant pair.
      }else if(class(data[,i])[1]=="ordered"){
        p_max_value = max(diag(p), na.rm = TRUE) # This picks the max p value ordinal predictor, which means the least significant pair.
      }
      
      if(l==2|(p_max_value < alpha_merge)) break
      
      p_max = which(p == max(p, na.rm = TRUE), arr.ind = TRUE) 
      r <- rownames(p)[p_max[, 1]]
      c <- colnames(p)[p_max[, 2]]
      nameofMergedCategory <- paste(r, c, sep = "-") #------- MERGED HERE WITH A HYPHEN
      
      print(paste("Merged categories in",k,"-th Merging loop:", nameofMergedCategory))
      
      # CALL MERGING FUNCTION merging_function()
      data[,i]<-merging_function(data, y, r, c, nameofMergedCategory, i)
      
      l = length(levels(data[,i]))
      print(paste("Total number of categories after",k,"-th Merging loop:", l))
      
      print(paste("========================end of",k,"-th Merging loop=========================="))
      cat("\n")
      
    }
    
    p_aov = as.matrix(summary((aov(data[, ncol(data)]~data[,i],data)))[[1]][,5])[1]
    print(paste("========================Finished Merging loop=========================="))
    
    #p_adj = p_aov * Bonferroni_correction(type,c,r)
    
    print(paste("Adusted p-value with bonferroni correction:",p_aov))
    cat("\n\n")
    
  }
  return(p_aov)
}


merging_function <- function(data, y, r, c, nameofMergedCategory, i) {
  
  list_of_levels = levels(data[,i])
  
  for (q in seq_along(list_of_levels)) {
    if (list_of_levels[q] == r || list_of_levels[q] == c) {
      list_of_levels[q] = nameofMergedCategory
    }
  }
  levels(data[,i]) <- list_of_levels
  
  return(data[,i])
}

bonferroni_correction <- function(type,c,r){
  
}
  

ccmerge(a)
