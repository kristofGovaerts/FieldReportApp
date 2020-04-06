library(readxl)
library(ggplot2)
library(scales)
library(colormap)

#necessary formatting in ebook: 
#series ID = 'Series Id'
#field ID = 'Field Id'

fefile <- "C:\\Users\\Kristof\\Desktop\\SESVanderhave_DroneData\\LEC901\\F_LEC901 CERCO.xlsx" #field ebook
fe <- read_excel(fefile)

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
