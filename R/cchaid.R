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
  cc_dataset<<-data.frame()
  cc_prepare_data_frame()
  }

cc_prepare_data_frame<-function(){
  print("hello")
}
a<-cc_define_file("aidwork3.txt")


