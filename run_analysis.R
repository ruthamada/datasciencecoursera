datafolder <- "UCI HAR Dataset"

gettables <- function (filename,cols = NULL){
  print(paste("Getting table:", filename))
  f <- paste(datafolder,filename,sep="/")
  data <- data.frame()
  if(is.null(cols)){
    data <- read.table(f,sep="",stringsAsFactors=F)
  } else {
    data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
  }
  data
}

getdata <- function(type, features){
  print(paste("Getting data", type))
  subject_data <- gettables(paste(type,"/","subject_",type,".txt",sep=""),"id")
  y_data <- gettables(paste(type,"/","y_",type,".txt",sep=""),"activity")
  x_data <- gettables(paste(type,"/","X_",type,".txt",sep=""),features$V2)
  return (cbind(subject_data,y_data,x_data))
}

#Obteniendo los datos
test <- getdata("test", features)
train <- getdata("train", features)

#Actividad 1
library(plyr)
data <- rbind(train, test)
data <- arrange(data, id)

#Actividad 2
mean_and_std <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]

#Actividad 3
activity_labels <- gettables("activity_labels.txt")

#Actividad 4
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

#Actividad 5
tidy_dataset <- ddply(mean_and_std, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidy_dataset)[-c(1:2)] <- paste(colnames(tidy_dataset)[-c(1:2)], "_mean", sep="")
