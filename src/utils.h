#pragma once

#include <fstream>
#include <cstdlib> // atof
#include <map>

#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

constexpr int INFINITE_INT =  std::numeric_limits <int>::max ();
constexpr double INFINITE_DBL =  std::numeric_limits <double>::max ();

const double rcpp_matrix_max (Rcpp::NumericMatrix dmat);
