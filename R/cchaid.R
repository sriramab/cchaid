## THIS FILE IS RESTRICTED TO READING INPUT FILES, ALGORITHM IS IN ANOTHER FILE
#this function gets the file name from the user
#' This function reads user file
#' 
#' @param x A file name
#' @return A dataframe with user selected fields
#' @export
#' @examples 
#' cc_define_file("filename.txt")
cc_define_file<-function(x) {
  read_file<-x
  cc_read_file_name(read_file)
  
}

# this function reads the files
cc_read_file_name<-function(y){
  #read file data into R
  f_read_with_Header<-read.csv(y, header=F, sep="\t")
  f_read_without_Header<-read.csv(y, header=T, sep="\t", skip = 1)
  f_inclusionAs<-t(f_read_with_Header[1,])
  f_columnsAs<-t(f_read_with_Header[2,])
  ignored<-table(f_inclusionAs)
  
  print(paste0("Total variables read: ", length(f_inclusionAs), " ignored: ", ignored[["x"]][1]))
  #cc_dataset<<-data.frame(1:length(f_read_without_Header[[1]]))
  nr<-seq(1,length(f_read_without_Header[[1]]))
  cc_dataset<-data.frame(nr)
  cc_dataset_names<-vector()
  cc_list<-list()
  for (i in 1:22){
    #print(i)
    data_type_by_user<-f_inclusionAs[i]
    #print(data_type_by_user)
    cc_dataset_names[1] <-"Nr"
    switch(data_type_by_user,
           
           "n"={
             #create nominal variables
             aa<-f_columnsAs[i]
             #print(paste0("   ",aa))
             cc_list[[i]]<-as.factor(f_read_without_Header[[aa]])
             cc_dataset<-cbind(cc_dataset,cc_list[[i]])
             cc_dataset_names[i+1]<-aa
           },
           "o"={
             #create ordered variables
             aa<-f_columnsAs[i]
             #print(paste0("   ",aa))
             cc_list[[i]]<-as.ordered(f_read_without_Header[[aa]])
             # cc_dataset<<-data.frame(as.ordered(f_read_without_Header[[aa]]))
             cc_dataset<-cbind(cc_dataset,cc_list[[i]])
             cc_dataset_names[i+1]<-aa
           },
           "c"={
             #create continuous variables
             aa<-f_columnsAs[i]
             #print(paste0("   ",aa))
             cc_list[[i]]<-as.numeric(f_read_without_Header[[aa]])
             # cc_dataset<<-data.frame(as.ordered(f_read_without_Header[[aa]]))
             cc_dataset<-cbind(cc_dataset,cc_list[[i]])
             cc_dataset_names[i+1]<-aa
             #cc_dataset_names<-c(cc_dataset_names,aa)
           },
           "x"={
             #list deleted variables
             aa<-f_columnsAs[i]
             #print(paste0(" to be deleted  ",aa))
             #cc_dataset<<-data.frame(as.factor(f_read_without_Header[[aa]]))
           },
           {
             #print("default")
           }
           
    )
    
  }
  
  
  for (i in 1:19){
    # print(paste0(i,"--", length(cc_list[[i]])))
  }
  #cc_dataset<<-cc_dataset
  cc_prepare_data_frame(cc_dataset_names,cc_dataset)
}

cc_prepare_data_frame<-function(x,y){
  #print("hello")
  cc_names<-na.omit(x)
  cc_names<-c( cc_names)
  colnames(y)<-cc_names
  return(y)
  
  
}
#mydata<-cc_define_file("aidwork3.txt") # release this line when testing


