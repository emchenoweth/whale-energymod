
setwd("F:/Analysis/Tag processing/JuliaTagData/mn12_262a")
setwd("F:/Tags")
setwd("F:/Analysis/RawTagsPRH/mn13_109a")

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

is.even <- function(x) x %% 2 == 0

#install.packages("R.matlab")
install.packages("ggplot2")
library("R.matlab", lib.loc="~/R/win-library/3.1")


data <- as.data.frame(readMat("mn13_109aprh.mat"))
speed <- as.data.frame(readMat("mn13_109awhalespeed.mat"))
data$speedFN<-speed$speedFN
data$speedFN<-speed$speedSP
data$speedFN<-speed$speedr2

write.csv(data, "F:/Mn13_109a_prhs.csv")
