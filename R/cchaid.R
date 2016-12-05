
#this function gets the file name from the user
cc_define_file<-function(x) {
  read_file<-x
  cc_read_file_name(read_file)

}

# this function reads the files
cc_read_file_name<-function(y){
  #read file data into R
  f_read_with_Header<<-read.csv(y, header=F, sep="\t")
  f_read_without_Header<<-read.csv(y, header=T, sep="\t", skip = 1)
  f_inclusionAs<<-t(f_read_with_Header[1,])
  f_columnsAs<<-t(f_read_with_Header[2,])
  ignored<-table(f_inclusionAs)
  #ignored<-sum(unlist(f_datatypeList), "x")
  #class(f_datatypeList)
  print(paste0("Total variables read: ", length(f_inclusionAs), " ignored: ", ignored[["x"]][1]))
  cc_dataset<<-data.frame(1:length(f_read_without_Header[[1]]))
  
  cc_list<<-list()
  for (i in 1:22){
    print(i)
    data_type_by_user<<-f_inclusionAs[i]
    print(data_type_by_user)
    switch(data_type_by_user,
           
           "n"={
             #create nominal variable
             aa<-f_columnsAs[i]
             print(paste0("   ",aa))
             cc_list[[i]]<<-as.factor(f_read_without_Header[[aa]])
             cc_dataset<<-cbind(cc_dataset,cc_list[[i]])
           },
           "o"={
             #create nominal variable
             aa<-f_columnsAs[i]
             print(paste0("   ",aa))
             cc_list[[i]]<<-as.ordered(f_read_without_Header[[aa]])
            # cc_dataset<<-data.frame(as.ordered(f_read_without_Header[[aa]]))
           },
           "x"={
             #create nominal variable
             aa<-f_columnsAs[i]
             print(paste0(" to be deleted  ",aa))
             #cc_dataset<<-data.frame(as.factor(f_read_without_Header[[aa]]))
           },
           {
             print("default")
           }
      
    )
  }
 # cc_dataset<-data.frame(cc_list)
  cc_dataset  <-  as.data.frame(matrix(unlist(cc_list), nrow=length(unlist(cc_list[[1]]))))
  cc_prepare_data_frame()
  }

cc_prepare_data_frame<-function(){
  print("hello")
  #cc_imported_data
  
}
a<-cc_define_file("aidwork3.txt")


