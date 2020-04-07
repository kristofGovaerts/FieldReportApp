library(readxl)
library(ggplot2)
library(scales)
library(colormap)
library(ggpubr)

colors <- c(colormap()[1], colormap()[36], colormap()[72]) #the lowest, middle and highest colors of the viridis colormap

#necessary formatting in ebook: 
#series ID = 'Series Id'
#field ID = 'Field Id'

plot_fieldmap <- function (febook, include='ALL'){
  febt <- data.frame(febook)
  febt$series <- as.factor(febt$Series.Id)
  series_stats <- aggregate(. ~ series, data = febt[,c('series','X','Y')], mean)
  series_stats$X <- as.integer(series_stats$X)
  series_stats$Y <- as.integer(series_stats$Y)
  febt$alpha <- 1.0
  if (include != 'ALL') {
    febt[!(febt$series %in% include), "alpha"] <- 0.1
  }
  
  p <- ggplot(data=febt, aes(x=X, y=Y, fill=series)) + geom_tile(colour='grey100', aes(alpha=alpha)) + 
    ggtitle(paste(levels(as.factor(febt$Field.Id))[1], 'Fieldmap')) + 
    geom_label(data=series_stats, aes(x=X, y=Y, label=series)) +
    scale_y_continuous(breaks=pretty_breaks(n = 10)) + scale_x_continuous(breaks=pretty_breaks(n = 10)) +
    theme(panel.background = element_blank(), legend.position = 'none')
  return(p)
}

plot_checks <- function(ddata, par) {
  cd <- subset(ddata, standard == 'Y')
  cd$seedname <- droplevels(cd$seedname)
  cd <- cd[!is.na(cd[par]),] #remove empties
  print(ggline(data=cd, x='time', y=par, add = c("median_iqr"), color = "seedname", palette = "jco", numeric.x.axis=T))
}

plot_data_column <- function (data, column, timepoint) {
  pd <- subset(data, time==timepoint)
  limits = c(min(pd[par]), max(pd[par]))
  midpoint = mean(limits)
  p <- ggplot(data=pd, aes_string(x='X', y='Y', fill=column)) + geom_tile() + ggtitle(column) + 
    scale_fill_gradient2(midpoint=midpoint, limits=limits, low=colors[1], mid=colors[2], high=colors[3], na.value='white') +
    theme(panel.background = element_blank())
  print(p)
}
