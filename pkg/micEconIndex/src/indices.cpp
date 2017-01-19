
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppProgress)]]
// #include <progress.hpp>
using namespace Rcpp;
using namespace arma;
using namespace std;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppProgress)]]
// #include <progress.hpp>
// [[Rcpp::export]]
arma::mat fisherEKSsparse(const arma::sp_mat  Q_consol,  const arma::sp_mat  P_consol,
                             arma::mat     Q_freq,          arma::mat     P_freq,
                             arma::uvec    Q_ind,           arma::uvec    P_ind) {
 
  uword base_period = 0 ;
  double M_dbl = Q_ind.n_elem ;
  uword split_interval = ceil( as_scalar(2e+12) / (P_consol.n_rows * Q_consol.n_rows)) ;
  // The matrix chunk size is designed to keep the Q_split_x_P under one gig of memory

  uvec spl = regspace<uvec>(1, split_interval, Q_consol.n_rows - 1) - 1L  ;
  // subtracting 1 from P_consol.n_rows so that I can append it later
  // without running into a bug. Using the useful behavior of regspace here
  spl.resize(spl.n_elem + 1) ;
  spl(spl.n_elem - 1) = Q_consol.n_rows  ;
  // preventing off-by-one error.

  arma::vec Q_x_P_vec(Q_consol.n_rows) ;
  Q_x_P_vec.zeros() ;
  arma::rowvec P_x_Q_vec(P_consol.n_rows) ;
  P_x_Q_vec.zeros() ;
  arma::vec revenue(Q_ind.n_elem) ;
  revenue.zeros() ;
  inplace_trans( Q_freq );

  for (uword spl_i=0; spl_i < (spl.n_elem - 1); spl_i++) {
    // Note that col < split.n_elem is strictly less than, so
    // it won't run over the last element.

    arma::mat Q_split_x_P = log( conv_to<mat>::from(Q_consol.rows(
      spl(spl_i), spl(spl_i + 1) - 1) * P_consol.t() )) ;

    Q_x_P_vec(span( spl(spl_i), spl(spl_i + 1) - 1 )) = Q_split_x_P * P_freq ;
    // P_freq is a column vec.
    // In a sense I am appending these values
  
     P_x_Q_vec += Q_freq.cols(spl(spl_i), spl(spl_i + 1) - 1 ) * Q_split_x_P ;

    // Q_freq originally comes in as a column vector
    // P_x_Q_vec is a row vector. Add in place

      uvec logic_1 = Q_ind >= spl(spl_i) ;
      uvec logic_2 = Q_ind <= (spl(spl_i + 1) - 1) ;
      uvec Q_split_supra_ind = find( (logic_1 + logic_2) == 2) ;

   // Jumping through a bunch of hoops to get compound logical expressions

    umat Q_P_joined = join_rows(Q_ind(Q_split_supra_ind) - spl(spl_i),
                          P_ind(Q_split_supra_ind) );

    revenue(Q_split_supra_ind) =
       Q_split_x_P( arma::sub2ind( size(Q_split_x_P), Q_P_joined.t() )) ;

  }


  double sum_revenue = sum(revenue) ;


   double top_row  = (-1) * M_dbl * arma::as_scalar(revenue(base_period))  +
        sum_revenue -
        Q_x_P_vec(base_period) +
        P_x_Q_vec(base_period) ;

  arma::vec ret = (1 / (2 * M_dbl)) * (
        top_row +
        M_dbl * revenue  -
        sum_revenue  +
        Q_x_P_vec(Q_ind) -
        P_x_Q_vec(P_ind)
    ) ;
    // "For matrix M, return the sum of elements in each column (dim=0), or each row (dim=1) "

  return(exp(ret)) ;
}

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppProgress)]]
// #include <progress.hpp>
// [[Rcpp::export]]
arma::mat fisherEKSdense(const arma::mat  Q_consol,  const arma::mat  P_consol,
                             arma::mat     Q_freq,          arma::mat     P_freq,
                             arma::uvec    Q_ind,           arma::uvec    P_ind) {

  uword base_period = 0 ;
  double M_dbl = Q_ind.n_elem ;
  uword split_interval = ceil( as_scalar(2e+12) / (P_consol.n_rows * Q_consol.n_rows)) ;
  // The matrix chunk size is designed to keep the Q_split_x_P under one gig of memory
  uvec spl = regspace<uvec>(1, split_interval, Q_consol.n_rows - 1) - 1L  ;
  // subtracting 1 from P_consol.n_rows so that I can append it later
  // without running into a bug. Using the useful behavior of regspace here

  spl.resize(spl.n_elem + 1) ;
  spl(spl.n_elem - 1) = Q_consol.n_rows  ;
  // preventing off-by-one error.

  arma::vec Q_x_P_vec(Q_consol.n_rows) ;
  Q_x_P_vec.zeros() ;
  arma::rowvec P_x_Q_vec(P_consol.n_rows) ;
  P_x_Q_vec.zeros() ;
  arma::vec revenue(Q_ind.n_elem) ;
  revenue.zeros() ;
  inplace_trans( Q_freq );

  for (uword spl_i=0; spl_i < (spl.n_elem - 1); spl_i++) {
    // Note that col < split.n_elem is strictly less than, so
    // it won't run over the last element.

    arma::mat Q_split_x_P = log( Q_consol.rows(
      spl(spl_i), spl(spl_i + 1) - 1) * P_consol.t() ) ;

    Q_x_P_vec(span( spl(spl_i), spl(spl_i + 1) - 1 )) = Q_split_x_P * P_freq ;
    // P_freq is a column vec.
    // In a sense I am appending these values

     P_x_Q_vec += Q_freq.cols(spl(spl_i), spl(spl_i + 1) - 1 ) * Q_split_x_P ;
    // Q_freq originally comes in as a column vector
    // P_x_Q_vec is a row vector. Add in place

      uvec logic_1 = Q_ind >= spl(spl_i) ;
      uvec logic_2 = Q_ind <= (spl(spl_i + 1) - 1) ;
      uvec Q_split_supra_ind = find( (logic_1 + logic_2) == 2) ;

   // Jumping through a bunch of hoops to get compound logical expressions

    umat Q_P_joined = join_rows(Q_ind(Q_split_supra_ind) - spl(spl_i),
                          P_ind(Q_split_supra_ind) );

    revenue(Q_split_supra_ind) =
       Q_split_x_P( arma::sub2ind( size(Q_split_x_P), Q_P_joined.t() )) ;

  }


  double sum_revenue = sum(revenue) ;

   double top_row  = (-1) * M_dbl * arma::as_scalar(revenue(base_period))  +
        sum_revenue -
        Q_x_P_vec(base_period) +
        P_x_Q_vec(base_period) ;

  arma::vec ret = (1 / (2 * M_dbl)) * (
        top_row +
        M_dbl * revenue  -
        sum_revenue  +
        Q_x_P_vec(Q_ind) -
        P_x_Q_vec(P_ind)
    ) ;
    // "For matrix M, return the sum of elements in each column (dim=0), or each row (dim=1) "

  return(exp(ret)) ;
}




// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat fisherInd (arma::mat Q, arma::mat P, int base_period) {

  base_period = base_period - 1 ;
  // Since C++ is zero-indexed
  int M = Q.n_rows ;
  arma::vec I_row(M);
  arma::vec I_col(M);
  arma::vec ret(M);
  I_row.ones() ;
  I_col.ones() ;
  ret.ones() ;

  for (int col=0; col<M; col++) {
    double Q_ind_L = dot(P.row(base_period), Q.row(col)) / dot(P.row(base_period), Q.row(base_period)) ;
    double Q_ind_P = dot(P.row(col), Q.row(col)) / dot(P.row(col), Q.row(base_period)) ;
    I_row(col) = sqrt(Q_ind_L * Q_ind_P) ;
  }

  for (int col=0; col<M; col++) {

    for (int row=0; row<M; row++) {
      double Q_ind_L = dot(P.row(row), Q.row(col)) / dot(P.row(row), Q.row(row)) ;
      double Q_ind_P = dot(P.row(col), Q.row(col)) / dot(P.row(col), Q.row(row)) ;
      I_col(row) = sqrt(Q_ind_L * Q_ind_P) ;
    }

    arma::vec interm_vec_prod = I_row % I_col ;

      if ( ! arma::is_finite(interm_vec_prod) ) {
        // Note that arma::is_finite checks the whole vector for any non-finite values
        stop("NaNs produced in quantity index. Check the quantity and price matrix inputs. Quantity indices must be positive, so the product of quantities and prices must be positive in all cases.") ;
        // Throws error in the case of zeros in the computed quantity index, which
        // will only occur in the case of all zeros in quantity
        // Thanks to explanation here: http://gallery.rcpp.org/articles/intro-to-exceptions/
      }
      ret(col) = exp(mean(log(interm_vec_prod))) ;
  }

  return(ret) ;
}




// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
if (T) {

library(data.table)
library(Matrix)

consol.matrix <- function(x) {
  if (!is.data.table(x)) x <- as.data.table(x)
  x.ret <- x[, .(.N), by = names(x)]
  N.ret <- matrix(x.ret$N, ncol = 1)
  x.ret[, N := NULL]
  list(mat = as.matrix(x.ret), freq = N.ret)
}



set.seed(100)
# n.col <- 100; n.row = 40000
# With these params, fastest index fn get 77 secs. Faster index fn gets 320 secs (4 times faster):
#  n.row.fact <- 100000 ; real.rows.factor = 2 ; n.col <- 400;
# n.row.fact <- 100000 ; real.rows.factor = 2 ; n.col <- 10;
# With the below, I have fastest 0.13; faster 0.185; naive 18.4 secs :
# n.row.fact <- 1000 ; real.rows.factor = 5 ; n.col <- 300;
# With below, I get fastest 0.013; faster 0.014; naive 112.533:
# n.row.fact <- 100 ; real.rows.factor = 100 ; n.col <- 100;
 n.row.fact <- 10 ; real.rows.factor = 2 ; n.col <- 4;
#n.row.fact <- 1000 ; real.rows.factor = 1 ; n.col <- 400;
n.row = real.rows.factor; n.row = n.row * n.row.fact
n.real.rows = n.row / real.rows.factor
P.mat <- matrix(runif(n.real.rows*n.col), ncol = n.col, nrow = n.row, byrow = TRUE )
P.mat <- rbind(P.mat[-1, ], P.mat[1, ])
#P.mat <- rbind(P.mat[1, ], P.mat[1, ], P.mat[2, ], P.mat[2, ])
#P.mat <- matrix(runif(n.col*n.row), nrow = n.row )
# Q.mat <- matrix(runif(n.col*n.row), ncol = n.col)
Q.mat <- matrix(runif(n.real.rows*n.col), ncol = n.col, nrow = n.row, byrow = TRUE )
Q.mat[, 4:ncol(Q.mat)] <- 0
# Making the matrix sparse


Q.mat.consol <- consol.matrix(Q.mat)
P.mat.consol <- consol.matrix(P.mat)
#rm(Q.mat)
#rm(P.mat)


fisherEKSdense.ret <- micEconIndex:::fisherEKSdense(
                Q_consol = Q.mat.consol$mat,
                P_consol = P.mat.consol$mat,
                Q_freq = Q.mat.consol$freq,
                P_freq = P.mat.consol$freq,
                Q_ind = rep((1:n.real.rows) - 1, real.rows.factor),
                P_ind = rep((1:n.real.rows) - 1, real.rows.factor))

fisherInd.ret <- micEconIndex:::fisherInd(Q.mat, P.mat, 1)

summary(fisherEKSdense.ret - fisherInd.ret )

fisherEKSsparse.ret <- micEconIndex:::fisherEKSsparse(
                Q_consol = Matrix(Q.mat.consol$mat, sparse=TRUE),
                P_consol = Matrix(P.mat.consol$mat, sparse=TRUE),
                Q_freq = Q.mat.consol$freq,
                P_freq = P.mat.consol$freq,
                Q_ind = rep((1:n.real.rows) - 1, real.rows.factor),
                P_ind = rep((1:n.real.rows) - 1, real.rows.factor))

summary(fisherEKSsparse.ret - fisherInd.ret )

}
*/
