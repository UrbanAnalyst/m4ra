#pragma once

#include <memory>
#include <vector>
#include <algorithm> // std::fill, std::reverse
#include <iostream>
#include <fstream>

#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppParallel,RcppThread)]]
#include <RcppThread.h>
#include <RcppParallel.h>

#include "pathfinders.h"

class DGraph;
class PathFinder;

//----------------------------
//----- functions in run_sp.cpp
//----------------------------

namespace run_sp {

std::shared_ptr <HeapDesc> getHeapImpl(const std::string& heap_type);

size_t make_vert_map (const Rcpp::DataFrame &vert_map_in,
        const std::vector <std::string> &vert_map_id,
        const std::vector <size_t> &vert_map_n,
        std::map <std::string, size_t> &vert_map);

void make_vert_to_edge_maps (const std::vector <std::string> &from,
        const std::vector <std::string> &to, const std::vector <double> &wt,
        std::unordered_map <std::string, size_t> &verts_to_edge_map,
        std::unordered_map <std::string, double> &verts_to_dist_map);

size_t get_chunk_size (const size_t nfrom);
} // end namespace run_sp


Rcpp::NumericMatrix rcpp_get_sp_dists_par (const Rcpp::DataFrame graph,
        const Rcpp::DataFrame vert_map_in,
        Rcpp::IntegerVector fromi,
        Rcpp::IntegerVector toi_in);