#pragma once

#include <cstdlib> // atof
#include <unordered_map>

#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppParallel,RcppThread)]]
#include <RcppThread.h>
#include <RcppParallel.h>

constexpr int INFINITE_INT =  std::numeric_limits <int>::max ();
constexpr double INFINITE_DBL =  std::numeric_limits <double>::max ();

Rcpp::IntegerMatrix rcpp_add_net_to_gtfs (Rcpp::IntegerMatrix net_times,
        Rcpp::IntegerMatrix gtfs_times, Rcpp::List gtfs_to_net_index,
        Rcpp::List gtfs_to_net_dist, const int nverts);

Rcpp::IntegerMatrix rcpp_min_from_two_matrices (Rcpp::IntegerMatrix mat1,
        Rcpp::IntegerMatrix mat2);
