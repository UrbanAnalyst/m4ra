#pragma once

#include <fstream>
#include <cstdlib> // atof

#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppParallel,RcppThread)]]
#include <RcppThread.h>
#include <RcppParallel.h>

void rcpp_scan_time_files (const Rcpp::DataFrame tt,
        Rcpp::NumericVector &times,
        const std::string &path,
        const double t0);
