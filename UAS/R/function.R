#Mean
ratarata <- function(data){
  n <- length(data)
  rata2<-sum(data)/n
  return(rata2)
}

#Median
calculatemedian<- function(data){
  return(median(data))
}


#Modus
calculatemodus <- function(data){
  freqtable <- table(data)
  maxfreq <- max(freqtable)
  modes <- as.numeric(names(freqtable[freqtable == maxfreq]))
  return(modes)
}


#Range
calculaterange <- function(data){
  jangkauan<-max(data)-min(data)
  return(jangkauan)}

#Variansi
varians <- function(data){
  n <- length(data)
  sum=0
  summ=0
  for(i in 1:n){
    sum=sum+data[i]
  }
  rata=sum/n
  for(i in 1:n){
    sel <- data[i]-rata
    pang <- sel^2
    summ <- summ+pang
  }
  varians <- summ/(n-1)
  return(varians)
}

#Standar Deviasi
strdeviasi <- function(data){
  n <- length(data)
  sum=0
  summ=0
  for(i in 1:n){
    sum=sum+data[i]
  }
  rata=sum/n
  for(i in 1:n){
    sel <- data[i]-rata
    pang <- sel^2
    summ <- summ+pang
  }
  stdev <- sqrt(summ/(n-1))
  return(stdev)
}

