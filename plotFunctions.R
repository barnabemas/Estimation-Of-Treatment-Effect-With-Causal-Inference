################################# BALANCE PLOTTING ################################# 

#The function bellow plots for X1,X3, X5 and X7 the difference in means 
#in each matching method

#data_scenario is the return of gloabl_simulation

balance_ploting<-function(data_scenario){
  
  plot_X1 <- (ggplot(stack(data_scenario$info_X1$sdiff), aes(x=ind, y=values,color=ind)) 
              + geom_boxplot()
              + coord_flip()
              +stat_summary(fun.y=mean, geom="point", shape=23, size=4)
              +geom_hline(yintercept=10, linetype="dashed", color = "red") 
              +theme(axis.text.y = element_text(face="bold", size= 10),
                     legend.position="none",
                     plot.title = element_text(size=13, face="bold"))
              +labs(title="X1 balance",x="", y = "Abs. Stand. Diff. in Mean (in %)"))
  
  plot_X3 <- (ggplot(stack(data_scenario$info_X3$sdiff), aes(x=ind, y=values,color=ind)) 
              + geom_boxplot()
              + coord_flip()
              +stat_summary(fun.y=mean, geom="point", shape=23, size=4)
              +geom_hline(yintercept=10, linetype="dashed", color = "red") 
              +theme(axis.text.y = element_text(face="bold", size=10),
                     legend.position="none",
                     plot.title = element_text(size=13, face="bold"))
              +labs(title="X3 balance",x="", y = "Abs. Stand. Diff. in Mean (in %)"))
  
  plot_X5 <- (ggplot(stack(data_scenario$info_X5$sdiff),aes(x=ind, y=values,color=ind)) 
              + geom_boxplot()
              + coord_flip()
              +stat_summary(fun.y=mean, geom="point", shape=23, size=4)
              +geom_hline(yintercept=10, linetype="dashed", color = "red") 
              +theme(axis.text.y = element_text(face="bold", size=10),
                     legend.position="none",
                     plot.title = element_text(size=13, face="bold"))
              +labs(title="X5 balance",x="", y = "Abs. Stand. Diff. in Mean (in %)"))
  
  plot_X7 <- (ggplot(stack(data_scenario$info_X7$sdiff), aes(x=ind, y=values,color=ind)) 
              + geom_boxplot()
              + coord_flip()
              +stat_summary(fun.y=mean, geom="point", shape=23, size=4)
              +geom_hline(yintercept=10, linetype="dashed", color = "red") 
              +theme(axis.text.y = element_text(face="bold", size=10),
                     legend.position="none",
                     plot.title = element_text(size=13, face="bold"))
              +labs(title="X7 balance",x="", y = "Abs. Stand. Diff. in Mean (in %)"))
  
  layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
  multiplot(plot_X1, plot_X3, plot_X5, plot_X7, layout=layout)
}

################################### ATT PLOTTING ################################### 

#The function bellow plots fthe estimated ATT for each matching method
#data_scenario is the return of gloabl_simulation

att_ploting <- function(data_scenario){
plot_att <- ggplot(stack(data_scenario$info_gen$ATT), aes(x=ind, y=values,color=ind)) + 
  geom_boxplot()
plot_att <- (plot_att + coord_flip()
             +stat_summary(fun.y=mean, geom="point", shape=23, size=4)
             +geom_hline(yintercept=1, linetype="dashed", color = "red") 
             +theme(axis.text.y = element_text(face="bold", size=10),
                    legend.position="none",
                    plot.title = element_text(size=13, face="bold"))
             +labs(title="ATT estimation, scenario 3 - 3 (missing val)",x="", y = "ATT"))

p <- (ggplot(stack(data_scenario$info_gen$ATT), aes(x=values, color=ind)) 
      + geom_histogram(aes(y=..density..),fill = "white", position="dodge") 
      + theme(axis.text.y = element_text(face="bold", size=14), plot.title = element_text(size=14, face="bold")) 
      + labs("ATT"))

multiplot(plot_att, p)
}

################################### MULTIPLOTTING ################################### 

# Multiple plot function imported from github 
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

