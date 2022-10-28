#pragma once

#include <math.h> // isnan

#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

constexpr int INFINITE_INT =  std::numeric_limits <int>::max ();
constexpr double INFINITE_DBL =  std::numeric_limits <double>::max ();

// https://stackoverflow.com/a/35890119
template <typename T>
struct NaNAwareLess
{
    bool operator () (T a, T b) const
    {
        if (std::isnan (b))
        {
            return false; // Assume NaN is less than *any* non-NaN value.
        }
        if (std::isnan (a))
        {
            return true; // Assume *any* non-NaN value is greater than NaN.
        }
        return (a < b);
    }
};

const double rcpp_matrix_max (Rcpp::NumericMatrix dmat);
