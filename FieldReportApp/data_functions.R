library(SpATS)
library(dplyr)
library(tidyr)
library(purrr)
library(scales)

#necessary formatting in ebook: 
#series ID = 'Series Id'
#field ID = 'Field Id'
reverse <- function(r) {
  #reverse a range, for inverting axes
  return(1 - (r - max(r)))
}

data_columns <- function(plotdata) {
  #extract only those parameters that contain drone data, ie not time, X/Y etc
  cols <- colnames(plotdata)
  splitted <- t(data.frame(strsplit(cols[grep('_', cols)], '_'))) #grep for only cols with '_'
  names <- levels(as.factor(splitted[,1]))
  types <- levels(as.factor(splitted[,2]))
  return(list(names, types))
}

prepare_data <- function(febook, fdata) {
  febook$seedname <- as.factor(febook$`Seed Name`)
  ddata <- merge(febook, fdata, by=c('X', 'Y'), all=T)
  ddata$seedname <- droplevels(ddata$seedname)
  ddata <- ddata[!is.na(ddata$seedname),]
  ddata$xy <- interaction(ddata$X, ddata$Y)
  ddata$time_f <- as.factor(ddata$time)
  ddata$Xf <- as.factor(ddata$X)
  ddata$Yf <- as.factor(ddata$Y)
  ddata$seedname[is.na(ddata$seedname)] <- "NA"
  ddata$Seed <- as.factor(ddata$seedname)
  ddata$standard <- as.factor(ddata$'Standard Ind')
  ddata$series <- ddata$'Series Id'
  return(ddata)
}

spats_field <- function(df, tps, par, genotype='Seed', X='X', Y='Y', Xf='Xf', Yf='Yf', gar=TRUE, nseg=c(15,15)) {
  cov_spats  <- list() #empty list
  
  for (i in 1:length(tps)) {
    d <- subset(df, time==tps[i])
    b <-SpATS(response = par, genotype = genotype, spatial = ~PSANOVA(X,Y,nseg=nseg), 
              random = ~Xf + Yf, data=d, genotype.as.random=gar)
    cov_spats[[tps[i]]] <- b
  }
  return(cov_spats)
}

spats_all <- function(df, tps, pars, genotype='Seed', X='X', Y='Y', Xf='Xf', Yf='Yf', gar=TRUE, nseg=c(15,15)) {
  asl <- list()
  for (i in 1:length(pars)) {
    par <- pars[i]
    timepoints <- unique(df[!is.na(df[[par]]),]$time)
    print(paste(c("Parameter:", par)))
    print(paste(c("Timepoints found:", timepoints)))
    timepoints <- intersect(timepoints, tps)
    sf <- spats_field(df, timepoints, par, genotype=genotype, X=X, Y=Y, Xf=Xf, Yf=Yf, gar=gar, nseg=nseg)
    asl[[par]] <- sf
  }
  return(asl)
}

fspats_to_BLUE <- function(fspats) {
  df <- data.frame(levels(as.factor(fspats[[1]]$data$Seed)))
  colnames(df) <- c('seedname')
  resp <- fspats[[1]]$model$response
  gar <- fspats[[1]]$model$geno$as.random
  
  for (i in 1:length(fspats)){
    t <- fspats[[i]]$data$time[1]
    str <- paste(resp, t, 'BLUE', sep='_')
    blue <- data.frame(fspats[[i]]$coeff[df$seedname])
    
    blue$seedname <- rownames(blue)
    colnames(blue) <- c(str, 'seedname')
    df <- merge(df, blue, by='seedname', all.x=T)
  }
  return(df)
}

fspats_to_pred <- function(fspats, par=NA) {
  for (i in 1:length(fspats)){
    spats <- fspats[[i]]
    tp <- as.character(spats$data$time[1]) #get tp - normally just one identical for each cell
    if (is.na(par)) {par <- spats$model$response}
    
    p <- predict(spats, which='Seed')[,c('Seed', 'predicted.values','standard.errors')]
    colnames(p) <- c('seedname', paste(par, tp, 'pred', sep='_'), paste(par, tp, 'se', sep='_'))
    if (i != 1){
      out <- merge(out, p, by='seedname', all=T)
    } else {out <- p}
  }
  return(out)
}

consolidate_spatslist <- function(spatslist, type="pred") {
  #type = "pred" or "blu*". note that blu* is BLUE if genotype != random, BLUP otherwise
  outdf <- data.frame(levels(spatslist[[1]][[1]]$data$Seed)) #data frame should be the same for all spats
  colnames(outdf) <- c('seedname')
  for (i in 1:length(spatslist)) {
    if (type == "pred") {
    outdf <- merge(outdf, fspats_to_pred(spatslist[[i]]), by='seedname', all.y=T)
    } else if (type == "blu*") {
      outdf <- merge(outdf, fspats_to_BLUE(spatslist[[i]]), by='seedname', all.y=T)
    } else {print("Error: type not understood.")}
  }
  return(outdf)
}

#next = generate AUCs
norm_AUC <- function(time, par) {
  auc <- DescTools::AUC(time, par, method="trapezoid")
  t <- max(time) - min(time)
  return(auc/t)
}

to_long <- function(spatsres) {
  inds <- grep(paste('seedname|pred|BLUE', sep=''), colnames(spatsres))
  
  #Here we pivot the input data frame in two steps: 
  #1. Into LONG format (names = seedname, par, time, type, value)
  #2. Widen column 'par' so we have a column per parameter
  spatsres[,inds] %>%
    pivot_longer(-seedname, names_to=c("par", "time", "type"), names_pattern=c("(.*_*)_(.*)_(.*)"), values_to='value') %>%
    pivot_wider(names_from='par') -> spats_long
  return(spats_long)
  }

to_aucs <- function(spatsres) {
  inds <- grep(paste('seedname|pred|BLUE', sep=''), colnames(spatsres))
  
  #here we generate AUCs
  spatsres[,inds] %>%
    pivot_longer(-seedname, names_to=c("par", "time", "type"), names_pattern=c("(.*_*)_(.*)_(.*)"), values_to='value') %>%
    group_by(par, seedname) %>%
    dplyr::summarize(AUC = norm_AUC(as.numeric(time), value)) %>%
    ungroup %>%
    pivot_wider(names_from='par', values_from='AUC') -> spats_auc
  return(spats_auc)
  }

rescale_col <- function(col, r=c(1,9)) {
  fn <- function(x) (((max(r, na.rm=T)-min(r, na.rm=T))/(max(col, na.rm=T)-min(col, na.rm=T)))*((x-max(col, na.rm=T))))+max(r, na.rm=T)
  return(sapply(col, fn))
}

rescale_pars <- function(df, r=c(1,9)) {
  nc <- sapply(df, function(x) rescale_col(x, r=r))
  colnames(nc) <- paste(colnames(nc), '_scaled', sep='')
  return(nc)
}
