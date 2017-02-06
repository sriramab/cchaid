
ram<-new.env()
assign("k", 10, envir = ram)
f<-function(a,b,dept=5){
  if (b<dept){
    a= a+1
    #print(ram$k)
    assign(paste0("DF", a), data.frame(A=rnorm(10), B=rnorm(10)), envir=ram)
    print(dept)
    b=b+1
    f(a, b, dept)
  }
}

f(1, runif(1))
ff()
ff<-function(){
  print(ram$DF2)
  a=5
  a
}

