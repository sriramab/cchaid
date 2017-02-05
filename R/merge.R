## THIS FILE IS RESTRICTED TO MERGING OF PREDICTORS
# this function gets the DATAFRAME from the user, merges insignificant levels
#' This function reads user file
#' 
#' @param x a dataframe from user, as constructed using cc_read_file()
#' @return A dataframe with user selected fields in a tab-delimited text file.
#' @export
#' @examples 
#' my.df<-cc_read_file("filename.txt")



ccmerge1<-function(data=x, alpha_merge=0.05){
  #print(paste(names(data), " -- ", alpha_merge))
  if(!is.data.frame(data)) stop("data is not a dataframe", call. = TRUE)
  if(alpha_merge>1) warning("p value provided for alpha_merge should be <=1", call. = TRUE)
  
  # Detect each column on its data_type -> function to be used is class; 
  # and send it to its appropriate function for merging
  
  sapply( data, function(x) if(class(x)[1]=="factor"){
    # merge for nominal values
    nominal_merge(x)

  } else if(class(x)[1]=="ordered") {
    # merge for ordinal values

    ordinal_merge(x)

  } #else{
      #print(paste("x"))
    #}
  )
  
  
  
  
  
}

nominal_merge<-function(y){
  
  l=length(levels(y))
  print(paste("Predictor has ", l, " levels"))
}

ordinal_merge<-function(y){
  #print(paste("inside ordinal merge=", a10))
  l=length(levels(y))
  print(paste("Predictor has ", l, " levels"))
}


ccmerge2<-function(data=d, alpha_merge=0.05){
  #print(paste(names(data), " -- ", alpha_merge))
  if(!is.data.frame(data)) stop("data is not a dataframe", call. = TRUE)
  if(alpha_merge>1) stop("p value provided for alpha_merge should be <=1", call. = TRUE)
  
  # Detect each column on its data_type -> function to be used is class; 
  # and send it to its appropriate function for merging
  
  sapply( colnames(data), function(x) if(class(data[,x])[1]=="factor"){
    # merge for nominal values
    nominal_merge(data[,x])
    
  } else if(class(data[,x])[1]=="ordered") {
    # merge for ordinal values
    
    ordinal_merge(data[,x])
    
  } #else{
  #print(paste("x"))
  #}
  )
  
  
  
  
  
}

ccmerge<-function(data=x, alpha_merge=0.05){
  #print(paste(names(data), " -- ", alpha_merge))
  if(!is.data.frame(data)) stop("data is not a dataframe", call. = TRUE)
  if(alpha_merge>1) warning("p value provided for alpha_merge should be <=1", call. = TRUE)
  
  # Detect each column on its data_type -> function to be used is class; 
  # and send it to its appropriate function for merging
  n<-colnames(data)
  #print(n)
  
  for (i in n){
    typeColumn<-class(data[,i])[1]
    switch(typeColumn, 
            "factor" ={
              n_merge(data[,i])
            }
            ,"ordered"={
              o_merge(data[,i])
            }
          )
  }
  
  
  
  
}

n_merge<-function(y){
  
  l=length(levels(y))
  print(paste("Predictor has ", l, " levels"))
}

o_merge<-function(y){
  #print(paste("inside ordinal merge=", a10))
  l=length(levels(y))
  print(paste("Predictor has ", l, " levels"))
}
ccmerge3(a)
ccmerge(a)
