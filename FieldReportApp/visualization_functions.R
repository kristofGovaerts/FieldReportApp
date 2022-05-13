library(ggplot2)
library(scales)
library(colormap)
library(ggpubr)
library(rlang)

colors <- c(colormap()[1], colormap()[36], colormap()[72]) #the lowest, middle and highest colors of the viridis colormap

#necessary formatting in ebook: 
#series ID = 'Series Id'
#field ID = 'Field Id'

plot_fieldmap <- function (febook, include='ALL', show_seedname=FALSE){
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
  
  if (show_seedname) {
    febt$seed <- febt$`Seed Name`
    p <- p + geom_text(aes(x=X, y=Y, label=seed), angle=90, size=3)
  }
  return(p)
}

plot_checks <- function(ddata, par) {
  ddata$seedname <- as.factor(ddata$seedname)
  cd <- subset(ddata, standard == 'Y')
  cd$seedname <- droplevels(cd$seedname)
  cd <- cd[!is.na(cd[par]),] #remove empties
  print(ggline(data=cd, x='time', y=par, add = c("median_iqr"), color = "seedname", palette = "jco", numeric.x.axis=T))
}

plot_check_discrimination <- function(df, pars, time_col='time', check_col='standard', group_col='seedname') {
  l <- list()
  tps <- sort(unique(df[,time_col]))
  
  for (tpn in 1:length(tps)) {
    tp <- tps[tpn]
    
    dft <- filter(df, !!sym(time_col)==tp)
    
    dft %>%
      filter(!!sym(check_col)=='Y') %>%
      dplyr::group_by(!!sym(group_col)) %>%
      summarize_at(pars, mean) %>%
      ungroup() %>% dplyr::select(pars) %>% summarize_all(var) -> vars_1
    
    dft2 <- data.frame(dft)
    gc <- dft2[,c(group_col, check_col)]
    set.seed(15) 
    gc2 <- gc[sample(nrow(gc)),]
    dft2[,c(group_col, check_col)] <- gc2
    
    dft2 %>%
      filter(!!sym(check_col)=='Y') %>%
      dplyr::group_by(!!sym(group_col)) %>%
      summarize_at(pars, mean) %>%
      ungroup() %>% dplyr::select(pars) %>% summarize_all(var) -> vars_2
    
    l[[tpn]] <- vars_1/vars_2
  }
  ldf <- data.frame(matrix(unlist(l), nrow=length(l), byrow=TRUE))
  colnames(ldf) <- pars
  ldf$time <- as.numeric(tps)
  ldf <- pivot_longer(ldf, pars)
  
  p <- ggplot(ldf, aes(x=time, y=log10(value))) + geom_line(aes(color=name), size=2) + 
    geom_hline(yintercept=0, linetype="dashed", color = "blue") +
    theme_bw() + ylab("log10(Discrimination)") + theme(legend.position='bottom')
  print(p)
}

plot_data_column <- function (data, column, timepoint) {
  pd <- subset(data, time==timepoint)
  limits = c(min(pd[column]), max(pd[column]))
  midpoint = mean(limits)
  p <- ggplot(data=pd, aes_string(x='X', y='Y', fill=column)) + geom_tile() + ggtitle(paste("Parameter:", column, "Timepoint:", toString(timepoint))) + 
    scale_fill_gradient2(midpoint=midpoint, limits=limits, low=colors[1], mid=colors[2], high=colors[3], na.value='white') +
    theme(panel.background = element_blank())
  print(p)
}
