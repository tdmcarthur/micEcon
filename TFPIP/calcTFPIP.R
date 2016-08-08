
#priceQuantMat <- read.csv("~/svn/micEcon/pkg/micEconIndex/tests/testthat/priceQuantMat.txt")
load("~/svn/micEcon/pkg/micEconIndex/R/sysdata.rda", verbose = TRUE)
# Chaneg this to a load statement, then get the TFPIP stuff


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
  1, 1
)

cat(instrFile,
  file = "~/svn/micEcon/TFPIP/pkg-ins.txt",
  sep = "\n")


# Then run TFPIP manually in Windows

# Then this:

TFPIPlines <- readLines("~/svn/micEcon/TFPIP/pkg-out.txt")
# TFPIPlines <- readLines("~/svn/micEcon/TFPIP/EG1-out.txt")
startLines <- grep("obsn", TFPIPlines)

#TFPIPresult <- read.table("~/svn/micEcon/TFPIP/pkg-out.txt",
#  header = TRUE, sep = "",
#  skip = startLines[1] - 1,
#  nrows = startLines[2] - startLines[1] - 5)
# Above was necessary for non-transitive indices
TFPIPresult <- read.table("~/svn/micEcon/TFPIP/pkg-out.txt",
  header = TRUE, sep = "",
  skip = startLines[1] - 1)

#TFPIPresult <- c(1, TFPIPresult$input)
# Above was necessary for non-transitive indices
TFPIPresult <- TFPIPresult$input

save(TFPIPresult, file = "~/svn/micEcon/pkg/micEconIndex/data-raw/TFPIPcheck.Rdata")








#TFPIP is a DOS computer program.  In DOS all file names must satisfy certain
#restrictions:
#	- no more than 12 characters
#	- no more than 3 characters after the period (".")
#	- no more than 8 characters before the period
#That is, the largest possible file name has the form:
#	XXXXXXXX.XXX



#system(c("/svn/micEcon/TFPIP/TFPIP.EXE /svn/micEcon/TFPIP/pkg-ins.txt",

# It seems there is no easy way to make this work from the shell, so I will do it manually
# to get the comparison calcs
