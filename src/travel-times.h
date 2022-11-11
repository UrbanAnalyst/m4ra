#pragma once

#include <cstdlib> // atof
#include <unordered_set>
#include <unordered_map>

#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppParallel,RcppThread)]]
#include <RcppThread.h>
#include <RcppParallel.h>

constexpr int INFINITE_INT =  std::numeric_limits <int>::max ();
constexpr double INFINITE_DBL =  std::numeric_limits <double>::max ();
constexpr int XY_PRECISION = 100000;

// The following typedefs and structs are for `expand_closest_index`. The hold
// unordered maps between each GTFS station and one `int_dbl_pair_set`, holding
// a set of paired indices and distances to all closest network vertices.
typedef std::pair <int, double> int_dbl_pair;
     
struct int_dbl_pair_hash
{
    std::size_t operator() (const std::pair <int, double> &pair) const {
        return std::hash <int> () (pair.first) ^ std::hash <double> () (pair.second);
    }
};

typedef std::unordered_set <int_dbl_pair, int_dbl_pair_hash> int_dbl_pair_set;

const bool is_na (const int &x);

std::vector <int> get_closest_gtfs_stns (Rcpp::IntegerMatrix &times_to_gtfs_stops,
        const int &i, const int &n_closest);

Rcpp::List rcpp_closest_gtfs (Rcpp::DataFrame vxy,
        Rcpp::DataFrame stopxy, const int n_closest);

Rcpp::NumericMatrix rcpp_closest_pts (Rcpp::NumericMatrix dmat,
        const int n_closest, const double maxd);

Rcpp::List rcpp_expand_closest_index (Rcpp::NumericMatrix closest);

Rcpp::IntegerMatrix rcpp_net_gtfs_travel_times (Rcpp::IntegerMatrix &t_net_to_gtfs,
        Rcpp::IntegerMatrix t_gtfs_to_gtfs,
        Rcpp::IntegerMatrix t_gtfs_to_net,
        Rcpp::List closest_gtfs_to_from,
        Rcpp::IntegerMatrix closest_gtfs_to_net);

Rcpp::List rcpp_remap_verts_to_stops (Rcpp::NumericMatrix &dmat,
        Rcpp::IntegerVector &index_out);
