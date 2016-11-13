# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
devtools::use_package("reshape2")
devtools::use_package("ggplot2")
devtools::use_package("dplyr")

clean_names=function(df){
  #names(df)
  names(df)=gsub(" ","_",names(df))
  names(df)=gsub("\\(","",names(df))
  names(df)=gsub("\\)","",names(df))
  names(df)=gsub("/","_",names(df))
  names(df)=gsub("\\.","_",names(df))
  names(df)=gsub("\\-","_",names(df))
  names(df)=gsub("\\?","",names(df))
  names(df)=gsub("\\%","",names(df))
  names(df)=gsub("\u20AC","Euro",names(df))
  #add x if starts with a digit
  names(df)[grep("^\\d",names(df))]=paste0("x",names(df[grep("^\\d",names(df))]))
  #head(df)
  return(df)
}

#excel int time to posix
as.POSIXct.excel=function(x){
  x=as.POSIXct(x * (60*60*24), origin="1899-12-30", tz="GMT")
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#univariate plots of data
PLOT_ALL=function(DF){

  library(tidyr)
  library(dplyr)
  library(reshape2)
  library(ggplot2)
  par(mfrow=c(1,2), las=1)
  figs=1
  for(n in seq(length(DF))){
    if(is.numeric(DF[,n])){
      try(hist(DF[,n],main=names(DF[n]),breaks=20, col="grey"),T)
    }
    else if(is.POSIXct(DF[,n])){
      print(qplot(DF[,n],main = names(DF[n])))
    }
    else if(!is.numeric(DF[,n])){
      if(length(levels(DF[,n]))<21){
        par(mfrow=c(1,1), las=1)
        figs=1
        #ggplot takes too long to make chart with large levels
        print(qplot(DF[,n],main = names(DF[n]))+coord_flip())}
      else{
        if(figs==1){
          par(mfrow=c(1,2), las=1)
          plot(table(DF[,n]),main=names(DF[n]))
          figs=2}
        else{
          plot(table(DF[,n]),main=names(DF[n]))
          figs=1}
      }

    }
    else{print("unkonw type")}
  }
  par(mfrow=c(1,1), las=1)
}




#help on text function

right = function (string, char){
  substr(string,nchar(string)-(char-1),nchar(string))
}

left = function (string,char){
  substr(string,1,char)
}

#split data by numeric/catagorical
only.numeric= function(DF){
  DF[,sapply(DF, is.numeric)]
}


only.catagorical=function(DF){
  DF[,!sapply(DF, is.numeric)]
}

only.factor=function(DF){
  DF[,sapply(DF, is.factor)]
}

only.date=function(DF){
  DF[,sapply(DF, is.POSIXt)]
}

as.hour.decimal=function(x){
  x <- as.numeric(x)
  x[1]+x[2]/60
}

#visual of missing data
ggplot_missing <- function(x){
  #library(reshape2)
  percent.missing=colMeans(is.na(x))
  missrank = rev(order(percent.missing))
  na_cols=apply(sapply(x[missrank],is.na),any,MARGIN = 2)
  z=x[missrank][na_cols] %>% is.na %>% melt
  ggplot(data = z,
         aes(x = z[,1],
             y = z[,2])) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
    labs(y = "Variables in Dataset with missing/NA",
         x = "Rows / observations")
}

split.fun <- function(x, labs, digits, varlen, faclen){
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width=25), collapse="\n")
  }
  labs
}



treeplot_pres=function(x,extra=0,tweak=1){
  library(rattle)
  library(rpart.plot)
  rpart.plot(x,
             split.fun = split.fun,
             varlen = 0,
             tweak = tweak,
             box.col = c("red","palegreen3")[x$frame$yval],
             type = 2,
             branch = 1,
             extra = extra,
             lwd=2)
}


as.numeric.factor = function(x) {as.numeric(levels(x))[x]}

