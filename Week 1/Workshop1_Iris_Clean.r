https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
#######################################################################
#######################################################################
#######################################################################
######                                                           ######
######  STAT0023 Workshop 1: graphics for exploratory analysis   ######
######                                                           ######
######        Visualising the Fisher/Anderson iris data          ######
######                                                           ######
######  This is a "clean" version of Workshop1_Iris.r,           ######
######  which is suitable for running using source().            ######
######  See the original script for detailed comments.           ######
######                                                           ######
#######################################################################
#######################################################################
#######################################################################
##
##	Load the required libraries. If you get an error when running 
##  the line below, you will need to install the relevant library / 
##  libraries. Instructions for doing this can be found in the 
##  "Introduction to R" document provided on the Moodle page for 
##  Week 1 of the course. For some points to be aware of when 
##  installing in the UCL cluster rooms or on Desktop@UCLAnywhere,
##  see the comments in file Workshop1_Iris.r.
##
library(ggplot2); library(rgl); library(RColorBrewer)
##
##  Load the data and take a preliminary look
##
data(iris)
cat("\nSUMMARY OF IRIS DATA SET:\n")
cat("========================\n")
cat("\nVariables in data set:\n")
cat("----------------------\n")
print(str(iris))
cat("\nSummary statistics:\n")
cat("-------------------\n")
print(summary(iris))
##
##  Summary statistics for each species, first the slow way:
##
cat("\nMean sepal lengths for each species:\n")
cat("------------------------------------\n")
cat("Setosa:\n"); print(mean(iris$Sepal.Length[iris$Species=="setosa"]))
cat("Versicolor:\n"); print(mean(iris$Sepal.Length[iris$Species=="versicolor"]))
cat("Virginica:\n"); print(mean(iris$Sepal.Length[iris$Species=="virginica"]))
##
##  and then using tapply():
##
print(tapply(iris$Sepal.Length, INDEX=iris$Species, FUN=mean))
##
##  Other summary statistics. NB not all of the statistics from
##  the original script are included here. NB also rounding of
##  variances to three decimal places for display purposes. 
##
cat("\nVariances in sepal lengths for each species:\n")
cat("--------------------------------------------\n")
print(round(tapply(iris$Sepal.Length, INDEX=iris$Species, FUN=var),3))
cat("\nMedian sepal lengths for each species:\n")
cat("--------------------------------------\n")
print(tapply(iris$Sepal.Length, INDEX=iris$Species, FUN=median))
cat("\nLower quartiles of sepal length distributions for each species:\n")
cat("---------------------------------------------------------------\n")
print(tapply(iris$Sepal.Length, INDEX=iris$Species, FUN=quantile, prob=0.25))
##
##  Start plotting, and use par(ask=TRUE) to give the user a chance
##  to inspect each plot before proceeding to the next. 
##
if (!(names(dev.cur()) %in% c("windows","X11cairo"))) x11(width=8,height=6)
par(ask=TRUE)
plot(iris)
##
##  Boxplots of sepal length for each species.
##
boxplot(Sepal.Length ~ Species, data=iris,xlab="Species",
        ylab="Sepal length (cm)",
        main="Distributions of sepal lengths by species",
        col="salmon")
##
##  Formal comparisons of sepal lengths between the versicolor
##  and virginica species. 
##
cat("\nCOMPARISON OF SEPAL LENGTHS FOR VERSICOLOR AND VIRGINICA SPECIES:\n")
cat("=================================================================\n")
cat("\nTest for equal variances:\n")
cat("-------------------------\n")
print(var.test(iris$Sepal.Length[iris$Species=="versicolor"],
               iris$Sepal.Length[iris$Species=="virginica"]))
cat("\nTest for equal means:\n")
cat("---------------------\n")
print(t.test(iris$Sepal.Length[iris$Species=="versicolor"],
       iris$Sepal.Length[iris$Species=="virginica"],
       var.equal=TRUE))
##
##  Now some plots showing relationships between pairs of 
##  variables for each species. Either show the species in 
##  separate plots but on the same scales ...
##
print(
  ggplot(data=iris, mapping=aes(x=Petal.Length, y=Sepal.Length) ) +
    geom_point() +
    facet_wrap(~ Species) +
    labs(x="Petal length (cm)", y="Sepal length (cm)",
         title="Sepal length versus petal length for specimens of three iris species") +
    theme(plot.title=element_text(hjust=0.5))
  )
##
##  ... or all together on the same set of axes. 
##
plot.colours <- brewer.pal(3,"Dark2") # see help for brewer.pal ...
plot(iris$Petal.Length,iris$Sepal.Length,type="p",
     xlab="Petal length (cm)",ylab="Sepal length (cm)",
     col=plot.colours[iris$Species],  # Plotting colours
     pch=(15:17)[iris$Species],       # Plotting symbols (work it out!)
     cex=1.2,                         # Make points 20% bigger than default
     main="Sepal length versus petal length\nfor specimens of three iris species")
legend("topleft",pch=15:17,col=plot.colours,legend=levels(iris$Species),
       cex=1.2,title="Species",title.col=grey(0.4))
box(lwd=2)  # Draw a frame around the plot, with double line width
##
##  Save the plot to a PNG file (don't forget dev.off()!)
##
dev.copy(png,"IrisPlot1.png",width=8*72,height=6*72)
dev.off()
##
##  And finally an interactive 3-D scatterplot to play with 
##  (omitting the 'rgl ::' part of the command which isn't
##  needed here)
##
plot3d(iris$Sepal.Length,iris$Sepal.Width,iris$Petal.Length,
       col=plot.colours[iris$Species],size=10,
       xlab="Sepal length (cm)",
       ylab="Sepal width (cm)",
       zlab="Petal length (cm)")