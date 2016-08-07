
priceQuantMat <- read.csv("~/svn/micEcon/pkg/micEconIndex/tests/testthat/priceQuantMat.txt")

outputPriceMat <- priceQuantMat[, substr(colnames(priceQuantMat), 1, 1) == "P"]
outputQuantMat <- priceQuantMat[, substr(colnames(priceQuantMat), 1, 1) == "Q"]

# Adding this because TFPIP also wants to calc an input index
outputPriceMat <- cbind(data.frame(XPRICE = rep(1, nrow(outputPriceMat))), outputPriceMat)
outputQuantMat <- cbind(data.frame(XQUANT = rep(1, nrow(outputQuantMat))), outputQuantMat)

write.table(cbind(outputQuantMat, outputPriceMat),
  "~/svn/micEcon/TFPIP/pkg-dta.txt",
  quote = FALSE, row.names = FALSE, col.names = FALSE)


#eg1-dta.txt        DATA FILE NAME
#eg1-out.txt        OUTPUT FILE NAME
#5              NUMBER OF OBSERVATIONS
#2              NUMBER OF OUTPUTS
#3              NUMBER OF INPUTS
#0              0=TORNQVIST AND 1=FISHER
#0	         0=NON-TRANSITIVE AND 1=TRANSITIVE"
instrFile <- c(
  "pkg-dta.txt", "pkg-out.txt", nrow(outputQuantMat),
  1, ncol(outputQuantMat) - 1,
  1, 0
)

#TFPIP is a DOS computer program.  In DOS all file names must satisfy certain
#restrictions:
#	- no more than 12 characters
#	- no more than 3 characters after the period (".")
#	- no more than 8 characters before the period
#That is, the largest possible file name has the form:
#	XXXXXXXX.XXX

#cat(instrFile,
#  file = "~/svn/micEcon/TFPIP/pkg-ins.txt",
#  sep = "\n")

#system(c("/svn/micEcon/TFPIP/TFPIP.EXE /svn/micEcon/TFPIP/pkg-ins.txt",

# It seems there is no easy way to make this work from the shell, so I will do it manually
# to get the comparison calcs
