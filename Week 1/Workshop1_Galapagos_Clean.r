https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
#######################################################################
#######################################################################
#######################################################################
######                                                           ######
######            STAT0023 Workshop 1: revision of R             ######
######                                                           ######
######              Analysis of Galapagos Islands data           ######
######                                                           ######
######  This script is a "clean" version of the file             ######
######  Workshop1_Galapagos.r: it is designed to be run using    ######
######  source(), in which case it writes all results to the     ######
######  file Galapagos_Results.txt. See the header to file       ######
######  Workshop1_Galapagos.r for more details.                  ######
######                                                           ######
#######################################################################
#######################################################################
#######################################################################
##
##	Divert output to file 
##
sink("Galapagos_Results.txt")
##
##	First step: read the data, find out what's available and 
##  produce summaries of each variable
##
species.data <- read.table("galapagos.dat",header=TRUE)
cat("\nSUMMARY OF DATA SET:\n")
cat("====================\n")
cat("\nVariables in data set:\n")
cat("----------------------\n")
print(str(species.data))
cat("\nSummary statistics:\n")
cat("-------------------\n")
print(summary(species.data))
##
##	Plot the data ...
##
plot(species.data)
##
##	... and identify the outlying data point
##
big.island <- (species.data$Area > 3000)
cat("\nOUTLYING DATA POINT:\n")
cat("====================\n")
print(species.data[big.island,])
##
##	Plot Endemics vs Elevation (publication-quality), and output to PDF
##	
plot(species.data$Elevation,species.data$Endemics,
     xlab="Elevation (m)",ylab="No. of species",
     main="Variation of endemic species numbers with\nisland elevation",
     pch=15,col="blue")
box(lwd=2)
dev.copy(pdf,"endemics.pdf",width=6,height=6)
dev.off()
#
#	Fit a linear regression model and output results
#
endemics.model <- lm(Endemics ~ Elevation, data=species.data)
cat("\nREGRESSION OF ENDEMICS UPON ELEVATION:\n")
cat("======================================\n")
print(summary(endemics.model))
#
#	Produce the previous plot again, with the fitted regression 
#	line superimposed
#
plot(species.data$Elevation,species.data$Endemics,
     xlab="Elevation (m)",ylab="No. of species",
     main="Variation of endemic species numbers with\nisland elevation",
     pch=15,col="blue")
box(lwd=2)
abline(endemics.model, col="red", lty=2, lwd=2)
#
#	And plot some diagnostics (R produces 4 plots by default, 
#	so make space for them)
#
par(mfrow=c(2,2))
plot(endemics.model)
##
##	Return output to Console, and reset graphics window for 1 plot 
##	per page
##
sink(); par(mfrow=c(1,1))
