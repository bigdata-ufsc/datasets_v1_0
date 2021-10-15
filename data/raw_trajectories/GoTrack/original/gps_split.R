library(data.table)
library(ggplot2)
library(stringr)
library(Hmisc)

from.data.create.zip.file <- function (dt.list, mainDir, subDir, ext, zipfilename){
  
  dir.create(file.path(mainDir, subDir))
  n <- length(dt.list)
  lapply(1:length(dt.list),function(xi){
    x <- dt.list[[xi]]
    filename <- paste0(str_pad(xi, nchar(n), pad = "0")," ","s",unique(x$tid)," ",unique(x$label))
    filepath <- paste0(mainDir, "/", subDir, "/", filename, ext)
    cat(filepath,"/n")
    write.table(subset(x, select = -c(tid,label)), file = filepath, quote=FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
  })
  
}

dt.to.list <- function (dt){
  lapply(unique(dt$tid), function(x){
    subset(dt, tid == x)
  })
}

get.stratified <- function (tids, pTrain){
  
  set.seed(1)
  
  tids.train <- unlist(lapply(unique(tids$label), function(x){
    y <- subset(tids, label == x)
    sample(y$tid, pTrain * length(y$tid))
  }))
  
  tids.test <- subset(tids, tid %nin% tids.train)$tid
  
  list(train = tids.train, test = tids.test)
}

time.to.minutes.in.a.day <- function(x){
  
  x.time <- as.ITime(x)
  
  hour(x.time) * 60 + minute(x.time)
  
}

# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------

setwd("C:/Users/camil/Desktop/GPS Trajectory/")
dt<-fread("go_track_tracks.CSV")
dt2<-fread("go_track_trackspoints.CSV")

labels<- lapply(1:length(dt2$id),function(i){
  aux<-dt$car_or_bus[dt$id == dt2$track_id[[i]]]
  label <- if(aux == 1) "car" else "bus"

})
dt2$label<-unlist(labels)

lonlat <- lapply(1:length(dt2$id),function(i){
  lon <- dt2$longitude[[i]]
  lat <- dt2$latitude[[i]]
  x <- paste(lat, " ", lon, sep = "")
})
dt2$latlon<-unlist(lonlat)

new_time <- lapply(1:length(dt2$id),function(i){
  split_time <- strsplit(dt2$time[[i]], " ")
  split_time <- strsplit(split_time[[1]][2], ":")
  hour = as.integer(split_time[[1]][1]) * 60
  minute = as.integer(split_time[[1]][2]) + hour
})

dt2$new_time<-unlist(new_time)

dt1 <- dt2[,.(tid=track_id,label,latlon,time=new_time)]

dt1[,label := as.factor(paste0('c',dt1$label) )]

all.tids <- unique(subset(dt1, select=c(tid,label) ))

tids <- get.stratified(all.tids,0.7)

stop()
from.data.create.zip.file(dt.to.list(subset(dt1, tid %in% tids$train) ), getwd(), "train", ".r2", "train.zip")
from.data.create.zip.file(dt.to.list(subset(dt1, tid %in% tids$test ) ), getwd(), "test", ".r2", "test.zip")

