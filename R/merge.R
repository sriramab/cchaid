## THIS FILE IS RESTRICTED TO MERGING OF PREDICTORS, F test is the square of t test when compairing a pair
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
  if(alpha_merge>1) warning("p value provided for alpha_merge should be <=1", call. = TRUE)
  
  # Detect each column on its data_type -> function to be used is class; 
  # and send it to its appropriate function for merging
  n<-colnames(data)
  #print(n)
  
  for (i in n){
    typeColumn<-class(data[,i])[1]
    switch(typeColumn, 
            "factor" ={
              nominal_merge(data, data[,i],i)
            }
            ,"ordered"={
              ordinal_merge(data, data[,i],i)
            }
          )
  }
  
}

nominal_merge<-function(data, y,i){
  
  l=length(levels(y))
  
  print(paste("Predictor has ", l, " levels"))
  p=(pairwise.t.test(data[,ncol(data)],y, p.adjust.method = "bonf"))$p.value
  p_max=which(p==max(p,na.rm = TRUE), arr.ind=TRUE)
  print(p)
  print(p_max)
  print(rownames(p))
  print(colnames(p))
}

ordinal_merge<-function(data, y,i){
  #print(paste("inside ordinal merge=", a10))
  l=length(levels(y))
  print(paste("Predictor has ", l, " levels"))
}

ccmerge(a)
#which(matrix_values==max(matrix_values,na.rm = TRUE), arr.ind=TRUE)