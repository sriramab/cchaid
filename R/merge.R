## THIS FILE IS RESTRICTED TO MERGING OF PREDICTORS
# this function gets the DATAFRAME from the user, merges insignificant levels
#' This function reads user file
#' 
#' @param x a dataframe from user, as constructed using cc_read_file()
#' @return A dataframe with user selected fields in a tab-delimited text file.
#' @export
#' @examples 
#' my.df<-cc_read_file("filename.txt")



ccmerge<-function(data=x, alpha_merge=0.05){
  #print(paste(names(data), " -- ", alpha_merge))
  if(!is.data.frame(data)) stop("data is not a dataframe", call. = TRUE)
  if(alpha_merge>1) warning("p value provided for alpha_merge is higher than 1", call. = TRUE)
  
  # Detect each column on its data_type = function to be used is class; 
  # and send it to its appropriate function for merging
  
  sapply( data, function(x) if(class(x)[1]=="factor"){ 
    # merge for nominal values
    nominal_merge()
    
  } else if(class(x)[1]=="ordered") { 
    # merge for ordinal values
    a10=10
    ordinal_merge(10) 
    
    } else{}
  )
  
  
  
  
  
}

nominal_merge<-function(){
  print(paste("inside nomial merge"))
}

ordinal_merge<-function(a10){
  print(paste("inside ordinal merge=", a10))
}
ccmerge(a)
