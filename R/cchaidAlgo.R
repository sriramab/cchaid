## algorithm for cchaid
library(cchaid)
attach(cchaid::cc_dataset)


# FIT AN ANOVA MODEL AND CHECK F STATISTIC
do_ftest <-function(dF){ #should get a data.frame as dF
 if(missing(dF)) stop("A dataframe with continuous variable in the last column is necesary")
 #exists(df, inherits = FALSE)
 #print(df)
 dF<<-dF
 
 #ftest<<-apply(subset(dF,select=-1), 2, function(x) summary(aov(x~dF[[ncol(dF)]])))#[[1]]$`F value`[1])
 ftest<<-apply(subset(dF,select=-c(1, ncol(dF))), 2, function(x) summary(aov(x~dF[[ncol(dF)]])))
 
 f_values<<-apply(subset(dF,select=-c(1, ncol(dF))), 2, function(x) summary(aov(x~dF[[ncol(dF)]]))[[1]]$`F value`[1])
 
 p_values<<-apply(subset(dF,select=-c(1, ncol(dF))), 2, function(x) summary(aov(x~dF[[ncol(dF)]]))[[1]]$`Pr(>F)`[1])
 
 dianostics<<-data.frame(f_values, p_values)
 selectedVariable<<-names(subset(dF,select=-c(1, ncol(dF))))[which.max(f_values)]
 print(paste0("Variable explaining the most: ",selectedVariable))
 
 # split for selected variable on table
 
 split_groups<<-split(dF[,-c(which(colnames(dF)==selectedVariable[1]))], dF[[selectedVariable[1]]])
 sapply(split_groups,nrow )
 return(split_groups)
 }
#a<-cc_dataset
#do_ftest(a)


cc_f_test<-function (inputdf, depth=1, minbucket=100){
  returned.mylist<<-list()
  if (depth>3 | depth<1) {
    #if (!is.integer(depth)) warning("value rounded down as -> ", round(depth)) 
    warning(" max value of 3 adopted")
    depth<-3
    
  }
  if (!is.integer(depth)) warning("value rounded down as -> ", round(depth)) 
  #print("good")
  my.list<<-inputdf
  depths<-1
  returned.mylist[depths]<<-do_ftest(my.list) #working
  # while (depths <= depth){
  # 
  # 
  # for (i in 1:length(returned.mylist)){
  #   aabbccdd<-do_ftest
  # }
  #my.list<<-lapply(my.list, do_ftest())
  depths<-depths+1
  }
  

a<-cc_dataset
cc_f_test(a, depth=2)

for (i in 1:length(returned.mylist)){
  print(names(returned.mylist[i]))
  
}

# ######TRIAL SPACE
# #FIND WHICH VARIABLE IS MOST EXPLANING THE RESPONSE
# f<-apply(cc_dataset, 2, function(x) summary(aov(x~continuous))[[1]]$`F value`[1])
# 
# table(cc_dataset$Dpop)
# #str(fit)
# fit
# 
# fit[[1]]$`F value`[1]
# fit[[1]]$`Pr(>F)`[1]
# 
# continuous<-cc_dataset$Dur
# 
# anovatable<-cc_dataset[,1:21]
# str(anovatable)
# fvalues <- apply(anovatable, 2, function(x) summary(aov(x~continuous))[[1]]$`F value`[1])
# selectedVariable<-names(anovatable)[which.max(fvalues)]
# print(paste0("Variable explaining: ",selectedVariable))
# selectedVariable
# 
# ##### selected variable and its levels
# groups<-split(anovatable, anovatable[[selectedVariable]])
# selectedVariable.data<-data.frame(anovatable[,selectedVariable])
# selectedVariable.data  
# table(selectedVariable.data)
# is.factor(anovatable$wstat)
# class(selectedVariable)
# 
# 
# #get index of a column with name
# which( colnames(anovatable)=="wstat" )
# 
# #get name of a column based on its index
# names(anovatable)[10]
# 
# # remove a column or a few columsn
# #df[ , -which(names(df) %in% c("z","u"))]
# # remove a column or a few columns with subset
# # subset(df, select=-c(z,u))
# deletedtable<-subset(anovatable, select = - wstat)
# 
# #### TRIAL SPACE