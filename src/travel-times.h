#pragma once

#include <fstream>
#include <cstdlib> // atof
#include <map>

#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppParallel,RcppThread)]]
#include <RcppThread.h>
#include <RcppParallel.h>

constexpr int INFINITE_INT =  std::numeric_limits <int>::max ();
constexpr double INFINITE_DBL =  std::numeric_limits <double>::max ();
constexpr int XY_PRECISION = 100000;

const bool is_na (const int &x);

std::vector <int> get_closest_gtfs_stns (Rcpp::IntegerMatrix &times_to_gtfs_stops,
        const int &i, const int &n_closest);

Rcpp::List rcpp_closest_gtfs (Rcpp::DataFrame vxy,
        Rcpp::DataFrame stopxy, const int n_closest);

Rcpp::IntegerMatrix rcpp_closest_pts (Rcpp::NumericMatrix dmat,
        const int n_closest, const double maxd);

Rcpp::IntegerMatrix rcpp_net_gtfs_travel_times (Rcpp::IntegerMatrix &t_net_to_gtfs,
        Rcpp::IntegerMatrix t_gtfs_to_gtfs,
        Rcpp::IntegerMatrix t_gtfs_to_net,
        Rcpp::List closest_gtfs_to_from,
        Rcpp::IntegerMatrix closest_gtfs_to_net);
