library(readxl)
library(ggplot2)
library(scales)
library(colormap)

#necessary formatting in ebook: 
#series ID = 'Series Id'
#field ID = 'Field Id'

data_columns <- function(plotdata) {
  #extract only those parameters that contain drone data, ie not time, X/Y etc
  cols <- colnames(plotdata)
  splitted <- t(data.frame(strsplit(cols[grep('_', cols)], '_'))) #grep for only cols with '_'
  names <- levels(as.factor(splitted[,1]))
  types <- levels(as.factor(splitted[,2]))
  return(list(names, types))
}

prepare_data <- function(febook, fdata) {
  febook$seedname <- droplevels(as.factor(febook$`Seed Name`))
  ddata <- merge(febook, fdata, by=c('X', 'Y'), all.y=T)
  ddata$xy <- interaction(ddata$X, ddata$Y)
  ddata$time_f <- as.factor(ddata$time)
  ddata$Xf <- as.factor(ddata$X)
  ddata$Yf <- as.factor(ddata$Y)
  ddata$seedname[is.na(ddata$seedname)] <- "NA"
  ddata$Seed <- as.factor(ddata$seedname)
  ddata$standard <- as.factor(ddata$'Standard Ind')
  return(ddata)
}