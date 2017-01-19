## micEconIndex R package

### Tools for calculating price and quantity indices. Not yet published to CRAN.

### Part of micEcon project: Tools for microeconomic analysis and microeconomic modelling

#### Available functions:
* priceIndex()	  Calculates a Laspeyres, Paasche or Fisher price index.
* quantityIndex()	Calculates a Laspeyres, Paasche or Fisher quantity index.

#### New features (2016/08/08):
* quantityIndex() can now calculate the Elteto-Koves-Szulc (EKS) transitivity correction for the Fisher index. The implementation is in Rcpp, so it can handle a large numbers of firms.
* The results of Fisher quantity index with EKS correction is checked against the results of Tim Coelli's TFPIP software (http://www.uq.edu.au/economics/cepa/tfpip.php)

#### Future work (2017/01/01):
* Extend EKS correction to Laspeyres and Paasche quantity indices


Package DESCRIPTION:

Package: micEconIndex

Version: 0.1-2

Date: 2016/08/08

Title: Price and Quantity Indices

Author: Arne Henningsen

Maintainer: Arne Henningsen <arne.henningsen@gmail.com>

Depends:

    R (>= 2.10)

Imports:

    miscTools (>= 0.6-1),
    data.table (>= 1.9.6),
    Rcpp

SystemRequirements:

    C++11

Suggests:

    Ecdat (>= 0.1-5),
    micEcon (>= 0.6-12),
    testthat (>= 1.0.2)

LinkingTo: Rcpp, RcppArmadillo, RcppProgress

Description: Tools for calculating various price and quantity indices.

License: GPL (>= 2)

URL: http://www.micEcon.org
