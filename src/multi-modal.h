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

Rcpp::NumericMatrix rcpp_add_net_to_gtfs (Rcpp::NumericMatrix net_times,
        Rcpp::NumericMatrix gtfs_times);
